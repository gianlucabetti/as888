000100050302     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000110170928     h dftactgrp(*no) bnddir('UBBNDDIR': 'TIO7')
000120170927      ****************************************************************
000130170927      *                                                               *
000140170928      *       IMMISSIONE ORM MANUALI ?                                *
000150170927      *                                                               *
000160170927      *****************************************************************
000170170927      *
000180010208      *  ATTENZIONE: località cliente ordinante e destinatario a
000190010208      *              video sono lunghe 20 nel file 35
000200170927      *
000210010115      *              i campi che non sono gestiti a video sono hidden
000220170927      *
000230001010      ****************************************************************
000240001010      *  RIEPILOGO INDICATORI
000250001010      ****************************************************************
000260001009      * 01 - IMMISSIONE
000270001009      * 02 - MANUTENZIONE
000280001109      * 03 - COPIA
000290150615      * 04 - Immissione da menù (no conferma)
000300060117      * 05 - Tipo Orm protetto
000310011113      * 06 - Dati cliente destinatario protetti
000320011113      * 07 - Dati cliente mittente a protetti
000330001109      * 08 - Dati cliente ordinante protetti
000340001109      * 09 - P.O.ritiro protetto
000350010202      * 10 - CHIUSURA/ANNULLAMENTO
000360050502      * 11 - ORM con ricevuta di ritiro
000370160310      * 12 - Protegge dati Alert ORM Commissionato
000380010309      * 13 - Visualizza causale
000390010515      * 14 - ORM con p.o. ritiro Euroexpress
000400010515      * 15 - ORM con p.o. ritiro DPD
000410011114      * 16 - Protezione codice destinatario
000420020523      * 17 - Chiusura ORM con genrazione bolla addebito
000430061023      * 18 - P.O. emissione o P.O. ritiro DPD F2=Dati DPD
000440040419      * 19 - Abilita F21 x dirottamento
000450160311      * 20 - Protegge dati Alert Conferma Prenotazione Ritiro
000460061003      * 21 - P.o. ritiro estero in manutenzione
000470150310      * 22 - Filiale ritiro abilitato a PDA
000480150924      * 23 - Visualizza filiale emissione
000490070913      * 24 - Protezione Giro di ritiro
000500070925      * 25 - Emissione window per proposta con mittente e/o qtà diverse da ORM
000510081127      * 26 - Orm import DPD automatico solo alcuni campi modificabili
000520001010      * 28 - ERRORE GENERICO DSPF
000530040628      * 29 - Protezione data ritiro quando dirottamento ORM
000540001010      * 30 - Comodo
000550001115      * 32 - Comodo x controllo cap
000560010522      * 33 - Videata ORM allocato
000570040916      * 34 - Protezione orm commissionato
000580050324      * 35 - errore orm commissionato
000590040426      * 37 - Comodo x msg info orm già inserito con stesso mittente
000600011114      * 38/39 - Comodo x msg info blocco destinatario
000610010305      * 40 - Data ritiro cade in festivo
000620011113      * 41 - Cliente mittente obbligatorio o non valido
000630001010      * 42 - Cliente ordinante non valido
000640001010      * 43 - Colli obbligatori
000650001010      * 44 - Peso obbligatorio
000660001010      * 45 - Volume obbligatorio
000670001010      * 46 - Bancali obbligatori
000680001010      * 47 - Bilico obbligatorio
000690001010      * 48 - Autotreno obbligatorio
000700001010      * 49 - Motrice obbligatoria
000710011113      * 50 - Comodo x dati mittente
000720130412      * 51 - Giro errato
000730161019      * 52 - Posizione cursore Data Pronta Merce
000740010213      * 53 - Chi paga errato
000750001010      * 54 - Tassazione e tariffa errati
000760001010      * 55 - P.O. Consegna errato
000770011113      * 56 - Cliente destinatario non valido
000780001020      * 57 - Visualizza tasto funzione per esistenza PROPOSTE
000790001012      * 58 - Ora errata
000800001016      * 59 - P.O. Ritiro errato
000810001010      * 60 - Posizione cursore Ragione Sociale
000820001010      * 61 - Posizione cursore Indirizzo
000830001010      * 62 - Posizione cursore CAP
000840001010      * 63 - Posizione cursore Località
000850001010      * 64 - Posizione cursore Provincia
000860001010      * 65 - Posizione cursore Nazione
000870001012      * 66 - Enter x forzare colli
000880001012      * 67 - Enter x forzare peso
000890001012      * 68 - Enter x forzare volume
000900001012      * 69 - Enter x forzare bancali
000910001012      * 70 - Enter x forzare bilico
000920001012      * 71 - Enter x forzare autotreno
000930001012      * 72 - Enter x forzare motrice
000940010202      * 74 - Comodo x dati ordinante
000950010202      * 75 - Comodo x dati destinatario
000960001025      * 76 - Visualizza ORM fisso
000970001215      * 77 - Data immissione ORM errata
000980130920      * 78 - Protezione campo riferimento
000990100811      * 79 - Interrogazione cappario DPD x nazione ritiro
001000131211      * 80 - Tasto 'F5=Altri Dati' in HI
001010110420      * 81 - Richiamato per conferma proposta di variazione
001020010122      * 82 - Note obbligatorie per orm superiore ai 5 q.
001030131211      * 83 - Tasto 'F4=Alert' in HI
001040010202      * 84 - Priorità non in tabella
001050010202      * 85 - Codice tariffa errato
001060010214      * 86 - Referente obbligatorio per prepagato
001070010214      * 87 - Telefono obbligatorio per prepagato
001080160406      * 88 - Abilita F13=Conferma Prenotazione
001090010301      * 89 - Comodo per msg info p.o. ritiro
001100090205      * 90 - Riemissione videata
001110061003      * 91 - Il tasto f18 viene visualizzato in HI se esistono note
001120061003      * 92 - errore QCMDEXEC
001130090206      * 93 - errore persona contattata
001140090206      * 94 - errore annotazioni
001150131024      * 95 - visualizza orari standard servizio
001160140130      * 96 - Il tasto f17 viene visualizzato in HI se esistono Note AUT
001170140130      * 97 - Visualizza tasto F17 Note AUT
001180140918      * 98 - Visualizza e abilita F16 - Chiusura ORM
001190001010      ****************************************************************
001200001010
001210001016     FFNORM01L  UF A E           K DISK
001220040531     fFnorm18l  if   e           k disk    rename(Fnorm000:Fnorm18)
001230040426     f                                     prefix(f_)
001240010829     FFNORF01L  UF A E           K DISK
001250001019     FFNORN01L  UF A E           K DISK
001260001019     FFNORV00F  O    E             DISK
001270131024     FFNORVE0F  O    E             DISK
001280140227     FFNORP01L  uF A E           K DISK
001290131029     FFNORPE1L  if a E           k DISK
001300140227     fFNORPT1L  if   e           k disk
001310070706     ffnorg01l  uf a e           k disk
001320090206     ffnora01l  uf a e           k disk
001330100217     ffnore01l  uf a e           k disk
001340001011     FAZORG01L  IF   E           K DISK
001350151109     FFNACR01L  uF   E           K DISK
001360071031     ffnacr13l  if   e           k disk
001370151109     fFNACRE1L  uf a e           k disk
001380070904     ffnors01l  if   e           k disk
001390070806     ffnors11l  if   e           k disk
001400140130     fFNORT01L  if   e           k disk
001410010118     FTABEL00F  IF   E           K DISK
001420001010     FTNTAM01L  IF   E           K DISK
001430001215     FAZCLN01L  IF   E           K DISK
001440040930     FCNACO00F  IF   E           K DISK
001450140318     ffidst01l  if   e           k disk
001460100217     ffnvaoe1l  if   e           k disk
001470130117     fTNTBE01L  if   e           k disk
001480140422     fFISAO00F  o    e             disk
001490160713     fTIORI01L  if   e           k disk    usropn extfile(wFLib)
001500150713     FFIOR05D   CF   E             WORKSTN usropn
001510161021     f                                     infds(InfDspF)
001520001010
001530131011      *------------------------------------------------------------------------*
001540131011      *   C O S T A N T I
001550131011      *------------------------------------------------------------------------*
001560131011     d cf2413          c                   const('F24=AlTasti(1/3)')
001570131011     d cf2423          c                   const('F24=AlTasti(2/3)')
001580131011     d cf2433          c                   const('F24=AlTasti(3/3)')
001590131011     d cf2412          c                   const('F24=AlTasti(1/2)')
001600131011     d cf2422          c                   const('F24=AlTasti(2/2)')
001610140120
001620140120     d c_Digits        c                   const('0123456789')
001630140506
001640140506     d cNoTelefona     c                   const('NON TELEFONARE')
001650140506     d cGiaAvvisato    c                   const('Il mittente è già stato -
001660140506     d                                     avvisato via mail, NON TELEFONARE')
001670140506
001680140506     d cCommAlert      c                   const('ORM COMMISSIONATO CON -
001690140506     d                                     PREVVISO MAIL/SMS')
001700140506
001710140506     d cDirottaNoComm  c                   const('ORM "NON COMMISSIONATO" -
001720140506     d                                     DIROTTATO')
001730140506
001740140506     d cDirottaPreavv  c                   const('ORM CON PREAVVISO VIA MAIL -
001750140506     d                                     DIROTTATO')
001760161021
001770161021      // - Tasti funzionali a video
001780161021     d c_F01           c                   const(x'31')
001790161021     d c_F02           c                   const(x'32')
001800161021     d c_F03           c                   const(x'33')
001810161021     d c_F04           c                   const(x'34')
001820161021     d c_F05           c                   const(x'35')
001830161021     d c_F06           c                   const(x'36')
001840161021     d c_F07           c                   const(x'37')
001850161021     d c_F08           c                   const(x'38')
001860161021     d c_F09           c                   const(x'39')
001870161021     d c_F10           c                   const(x'3A')
001880161021     d c_F11           c                   const(x'3B')
001890161021     d c_F12           c                   const(x'3C')
001900161021     d c_F13           c                   const(x'B1')
001910161021     d c_F14           c                   const(x'B2')
001920161021     d c_F15           c                   const(x'B3')
001930161021     d c_F16           c                   const(x'B4')
001940161021     d c_F17           c                   const(x'B5')
001950161021     d c_F18           c                   const(x'B6')
001960161021     d c_F19           c                   const(x'B7')
001970161021     d c_F20           c                   const(x'B8')
001980161021     d c_F21           c                   const(x'B9')
001990161021     d c_F22           c                   const(x'BA')
002000161021     d c_F23           c                   const(x'BB')
002010161021     d c_F24           c                   const(x'BC')
002020161021     d c_Enter         c                   const(x'F1')
002030161021     d c_PageDown      c                   const(x'F4')
002040161021     d c_PageUp        c                   const(x'F5')
002050161021
002060161021      *  titolo videata (lunghezza massima 34)
002070161021     D TIT_A           C                   CONST('*** G E S T I O N E  O. R. M.-
002080161021     D                                       ***')
002090131011
002100131011      *------------------------------------------------------------------------*
002110131011      *   V A R I A B I L I
002120001010      *------------------------------------------------------------------------*
002130001011     D kpoe            S                   LIKE(V1cpoe)
002140001016     D knsr            S                   LIKE(V1nsr)
002150001006     D knor            S                   LIKE(V1nor)
002160001016     D knrv            S                   LIKE(V1nrv)
002170070806     d knrvorig        s                   like(knrv)
002180070806     d knrvsav         s                   like(knrv)
002190010116
002200001026     D kdai            S                   LIKE(ORNdai)
002210001026     D kori            S                   LIKE(ORNori)
002220010116     D kfar            S                   LIKE(ORNfar)
002230001018     D kprg            S                   LIKE(ORNprg)
002240001026
002250001011     D kfnacr          S                   LIKE(ACRcro)
002260001026     D kazorg          S                   LIKE(ORGfil)
002270001026
002280001010     D kksc            S                   LIKE(TAMksc)
002290001010     D kctr            S                   LIKE(TAMctr)
002300001215
002310001215     D ktfp            S                   LIKE(CLNtfp)
002320001215     D ktfa            S                   LIKE(CLNtfa)
002330001215     D kann            S                   LIKE(CLNann)
002340001215     D kmes            S                   LIKE(CLNmes)
002350010118
002360010118     D kcod            S                   LIKE(TBLcod)
002370010118     D kkey            S                   LIKE(TBLkey)
002380010118     D kdes            S             30
002390040930
002400040930     D kci             s                   like(acokcc) inz(151)
002410070123
002420120612     d kdstnpg         s                   like(dstnpg)
002430120612     d kdstfgs         s                   like(dstfgs)
002440120612     d kdstnfv         s                   like(dstnfv)
002450130117
002460140422     d kTBEcod         s                   like(TBEcod)
002470001006
002480001011     D wmsg            S                   LIKE(O13msg)
002490001011     D wfzv            S                   LIKE(E13fzv)
002500001011     D wfz1            S                   LIKE(E13fz1)
002510001011     D wfz2            S                   LIKE(E13fz2)
002520001011     D wfz3            S                   LIKE(E13fz3)
002530001023
002540010117     D sav_acrfcl      S                   LIKE(ACRfcl)
002550010117     D sav_acrfpk      S                   LIKE(ACRfpk)
002560010117     D sav_acrfmc      S                   LIKE(ACRfmc)
002570010117     D sav_acrfbn      S                   LIKE(ACRfbn)
002580010117     D sav_acrfbl      S                   LIKE(ACRfbl)
002590010117     D sav_acrfat      S                   LIKE(ACRfat)
002600010117     D sav_acrfmt      S                   LIKE(ACRfmt)
002610010302     D inv_v1dao       S                   LIKE(ORMdao)
002620010116     D sav_v1ccra      S                   LIKE(ds_v1ccra)
002630131024     d sav_dsV1Ccra    s                   like(ds_V1Ccra)
002640010116     D sav_v1ccor      S                   LIKE(ds_v1ccor)
002650170208     d sav_dsV1Ccor    s                   like(ds_V1Ccor)
002660010116     D sav_v1ccrc      S                   LIKE(ds_v1ccrc)
002670010118     D sav_v1car       S                   LIKE(v1car)
002680010118     D sav_v1lor       S                   LIKE(v1lor)
002690010118     D sav_v1prr       S                   LIKE(v1prr)
002700080910     D sav_v1cao       S                   LIKE(v1cao)
002710010118     D sav_v1loo       S                   LIKE(v1loo)
002720030922     D sav_v1loo35     S                   LIKE(v1lor)
002730010118     D sav_v1pro       S                   LIKE(v1pro)
002740080910     D sav_v1cac       S                   LIKE(v1cac)
002750010118     D sav_v1loc       S                   LIKE(v1loc)
002760030922     D sav_v1loc35     S                   LIKE(v1lor)
002770010118     D sav_v1prc       S                   LIKE(v1prc)
002780010116     D sav_v1cpor      S                   LIKE(v1cpor)
002790010206     D sav_ACRpoa      S                   LIKE(ACRpoa)
002800131022     D sav_ACRorr      S                   LIKE(ACRorr)
002810010116     D sav_v1fao       S                   LIKE(v1fao)
002820010116     D sav_v1dfo       S                   LIKE(v1dfo)
002830010116     D sav_v1ofo       S                   LIKE(v1ofo)
002840010202     D priorita        S                   LIKE(ORMsto)
002850010308     D sav_v1pag       S                   LIKE(ORMpag)
002860150612     D network         S                   LIKE(§OGntw)
002870010402     D sav_v1pkg       S                   LIKE(v1pkg)
002880150921     D sav_v1vlm       S                   LIKE(v1vlm)
002890010507     D sav_v1ksc       S                   LIKE(v1ksc)
002900020522     d sav_lpksc       s                   like(ormksc)
002910030606     d AcrKscCor       s                   Like(AcrKsc)
002920050202     d AcrCccCor       s                   Like(AcrCcc)
002930040419     d sav_wdcpor      s                   like(wdcpor)
002940041014     d savndc          s                   like(ormndc) inz
002950041014     d savddc          s                   like(ormddc) inz
002960060516     d savfgs          s                   like(orffgs) inz
002970041130     d wfase200        s              1    inz('0')
002980050322     d sav_v1com       s                   like(v1com)
002990070124     d savorfndc       s                   like(ormndc) inz
003000070124     d savorffgs       s                   like(orffgs) inz
003010070125     d savorfddc       s                   like(orfddc) inz
003020070125     d savorffar       s                   like(orffar) inz
003030070920     d sav_acrcgi      s                   like(acr1cgi) inz
003040070913     d sav_v1ccgi      s                   like(v1ccgi) inz
003050071031     d kacr1ain        s                   like(acr1ain)
003060071031     d savpkg          s                   like(v1pkg)
003070071031     d savvlm          s                   like(v1vlm)
003080100217     d savbnc          s                   like(v1bnc)
003090100217     d sav_vaocor      s                   like(ormcor)
003100100217     d sav_vaopoe      s                   like(ormpoe)
003110100217     d sav_vaonsr      s                   like(ormnsr)
003120100217     d sav_vaonor      s                   like(ormnor)
003130100217     d sav_vaonrv      s                   like(ormnrv)
003140131030     d ntw_v1cpor      s                   like(§OGntw)
003150131107     d sav_V1rsc       s                   like(V1rsc)
003160131108     d sav_O95gf2      s                   like(O95gf2)
003170131111     d sav_O95lna      s                   like(O95lna)
003180131112     d sav_ORGfl1      s                   like(ORGfl1)
003190131202     d sav_v1nac       s                   like(v1nac)
003200151109     d sav_mailconf    s                   like(VAOEdati)
003210160318     d sav_smsconf     s             16a   inz
003220161028     d sav_v1dpm       s                   like(v1dpm)
003230010116
003240010116     D alf_ormpor      S                   LIKE(v1cpor)
003250010116     D alf_acrpoa      S                   LIKE(v1cpor)
003260010116     D num_v1cpor      S                   LIKE(ORMpor)
003270160921
003280160921     d wold_V1ncl      s                   like(V1ncl)
003290160921     d wold_V1pkg      s                   like(V1pkg)
003300160921     d wold_V1vlm      s                   like(V1vlm)
003310160922     d wold_V1bnc      s                   like(V1bnc)
003320160922     d wold_V1blc      s                   like(V1blc)
003330160922     d wold_V1att      s                   like(V1att)
003340160922     d wold_V1mtc      s                   like(V1mtc)
003350010116
003360010116     D oggi_aammgg     S                   LIKE(ORMdao)
003370010116     D oggi_ggmmaa     S                   LIKE(v1dao)
003380010907
003390010907     D oggi_anno       S              4  0
003400010907     D v1dar_anno      S              4  0
003410010116
003420161115     D old_invv1dpm    S                   LIKE(v1dpm)
003430010305     D old_invv1dar    S                   LIKE(v1dar)
003440010907     D old_newv1dar    S                   LIKE(v1dar)
003450010123     D new_v1dar       S                   LIKE(v1dar)
003460010123     D inv_newv1dar    S                   LIKE(v1dar)
003470050304     d dtmaxinfo       s                   like(OrmDar)
003480050304     d dtmaxblocco     s                   like(OrmDar)
003490140530     d wdata           s                   like(ORMdar)
003500150306     d wabi            s                   like(UTEaut)
003510161019     D inv_v1dpm       S                   LIKE(ORMdao)
003520170524     d DarCalcolata    s                   like(ORMdar)
003530001117
003540001025     D §kpjbu          S                   LIKE(KPJBU)
003550001012
003560010123     D alleore         S                   LIKE(ORMoao)
003570040301     d UL36vol         s              8  3 inz
003580010117     D ore             S              2  0
003590010117     D minuti          S              2  0
003600001020
003610001023     D werr            S              1
003620010704     D conta           S              2  0
003630010704     D okbolla         S              1
003640020524     d wbolla          S              1    inz('0')
003650081125     d wnobolla        S              1    inz('0')
003660010704     D lung            S             15  5
003670010704     D comman          S            110
003680030307     d wdatibolla      s              9
003690030623     d wdatibolA4      s              9
003700030623     d wdatibolA5      s              9
003710030610     d whocor          s              1    Inz
003720040426     d w002a           s              2
003730030609     d w004a           s              4
003740030609     d w009a           s              9
003750050304     d w0020           s              2  0
003760050323     d w0110           s             11  0
003770030623     d wtrulvid        s              1    Inz
003780040615     d wokdirotta      S              1    inz('0')
003790050304     d wforzadar1      s              1    inz('0')
003800040806     d wsecondi        s              2  0
003810050420     d wproposte       s              1    inz('0')
003820050502     d wricevuta       s              1    inz('0')
003830060109     d wforzapor       s              1    inz('0')
003840060109     d xx              s              3  0
003850070124     d wforzadir       s              1    inz(*off)
003860070228     d wforzachi       s              1    inz(*off)
003870071112     d $noagr          s              1    inz(*off)
003880080221     d wforzaqta       s               n
003890090128     d $notam          s               n
003900100223     d $errpre         s               n
003910100218     d $endpre         s               n
003920100223     d $okF6pre        s               n
003930100225     d $immetti        s               n
003940100218     d wmail           s                   like(vaoedati)
003950100222     d wcap            s                   like(v1car)
003960100811     d wNazione        s                   like(v1nar)
003970100811     d wCapDPD         s                   like(v1car)
003980110610     d $cacgdd         s               n
003990130121     d wORMcor         s              7s 0 inz
004000130507     d wintcap         s               n   inz(*off)
004010131024     d wokestensione   s               n   inz(*off)
004020140422     d walert          s               n   inz(*off)
004030151008     d wconferma       s               n   inz(*off)
004040140428     d werralert       s               n   inz(*off)
004050160406     d wErrConferma    s               n   inz(*off)
004060140506     d wpos            s              5i 0 inz
004070140515     d wdar            s                   like(ORMdar) inz
004080140515     d wOKalert        s               n   inz(*off)
004090140516     d wOKrcdg         s               n   inz(*off)
004100140711     d wOKF04          s               n   inz(*off)
004110160406     d wOKF13          s               n   inz(*off)
004120160426     d WebNPR          s               n   inz(*off)
004130170524     d wOkPosticipa    s               n   inz(*off)
004140170524     d wAnticipa       s               n   inz(*off)
004150170524     d Anticipato      s               n   inz(*off)
004160170524     d ggAnticipo      s              2s 0 inz
004170001009
004180001006     D W0140           S             14  0
004190060726
004200060726     d wormconf        s              1    inz('0')
004210070710     d wpkg            s             10  1
004220070706     d wvlm            s             10  3
004230070918     d wvlmaut         s             10  3
004240070918     d wvlmmtc         s             10  3
004250070918     d wvlmblc         s             10  3
004260070918     d wpkgaut         s             10  1
004270070918     d wpkgmtc         s             10  1
004280070918     d wpkgblc         s             10  1
004290070918     d wpkb            s             10  1
004300071031     d $varqta         s              1    inz(*off)
004310071106     d $proposte       s              1    inz(*off)
004320071108     d $modifica       s              1    inz(*off)
004330110121     d $copia          s              1
004340140122     d wVarDati        s                   like(OR56var)
004350001009
004360010116     D v1dar_eur       S               D   DATFMT(*eur)
004370050304     d dataeur         s               d   datfmt(*eur)
004380050302     d dataiso         s               d   datfmt(*iso)
004390051220     d wora            s               t
004400090211     d data_app        s              8  0 inz(20091231)
004410090407     d wokapp          s               n
004420091007     d wormnoman       s               n
004430100204     d ok_pvo          s               n
004440161027     d OrariEstesi     s               n   inz
004450100812     d $NoApp          s               n
004460111222     d w_V1Cpoe        s                   like(V1Cpoe)
004470130918     d wNOTEL          s               n
004480131107     d wokOR97         s               n
004490131202     d wForzaNAC       s               n
004500140226     d wF17write       s               n
004510140320     d wOkOrariSer     s               n
004520140509     d wModLocRit      s               n   inz(*off)
004530140508     d wCalDtRit       s               n   inz(*off)
004540140508     d wRicAlert       s               n   inz(*off)
004550160405     d wRicConf        s               n   inz(*off)
004560140509     d wokCapr         s               n   inz(*off)
004570140918     d OrmFase420      s               n   inz(*off)
004580150713     d ConfAuto        s               n   inz(*off)
004590160208     d SoloUnaVolta    s               n   inz
004600160217     d parametroNPR    s             10a   inz
004610160217     d wnpr            s             10a   inz
004620160317     d wCodAcre        s                   like(ACREcro)
004630160325     d wModGiroAut     s               n   inz
004640160325     d wModCodCra      s               n   inz
004650160713     d wLib            s             10a   inz
004660160713     d wFLib           s             21a   inz
004670161024     d Peso            s              7s 1 inz
004680161024     d old_wpkg        s                   like(v1pkg)
004690161025     d wModPeso        s               n   inz
004700161028     d wModDpm         s               n   inz
004710161115     d DataCalcolo     s                   like(ORMdao)
004720161115     d wDay_Of_Week    s              2  0 inz
004730161116     d wModPor         s               n   inz
004740131011
004750131030     d wEndF04         s               n
004760131030     d wEndF05         s               n
004770160405     d wEndF13         s               n
004780131030     d wErrF04         s               n
004790131030     d wErrF05         s               n
004800160405     d wErrF13         s               n
004810131025     d wOkF6F04        s               n
004820131011     d wOkF6F05        s               n
004830160405     d wOkF6F13        s               n
004840131011
004850131011     d rigatf1         s              1    dim(78)
004860131011     d rigatf2         s              1    dim(62)
004870131011     d rigatf3         s              1    dim(62)
004880131011     d $loop           s              1  0
004890131011     d $rig            s              1  0
004900131011     d $h              s              3  0
004910131011     d $j              s              3  0
004920131011     d $k              s              3  0
004930131011     d $x              s              3  0
004940131011     d $y              s              3  0
004950131011     d $z              s              3  0
004960001006
004970001010      *   S C H I E R E
004980110610     D MSG             S             78    DIM(100) CTDATA PERRCD(1)              MSG VIDEO
004990090211     D MOD             S             15    DIM(7)  CTDATA PERRCD(1)              MODALITA'
005000010201     D L6              S              3  0 DIM(30)                               P.O. Gestiti £6
005010030307     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
005020030623      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
005030030623     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
005040030623     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
005050050323     d skfig           s             11  0 Dim(500) inz(*Zeros)
005060061023     d skpodpd         s              3  0 dim(250)                             p.o. DPD
005070150305     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
005080131011     d tf02            s              1    dim(25) ctdata perrcd(25)
005090131011     d tf06            s              1    dim(25) ctdata perrcd(25)
005100131011     d tf11            s              1    dim(25) ctdata perrcd(25)
005110160405     d tf13            s              1    dim(25) ctdata perrcd(25)
005120131011     d tf14            s              1    dim(25) ctdata perrcd(25)
005130131011     d tf16            s              1    dim(25) ctdata perrcd(25)
005140131011     d tf19            s              1    dim(25) ctdata perrcd(25)
005150131011     d tf20            s              1    dim(25) ctdata perrcd(25)
005160131011     d tf21            s              1    dim(25) ctdata perrcd(25)
005170131011     d $tf             s              1    dim(25)
005180140422
005190140422     d skTRCvao        s              2    dim(99)
005200001006
005210001010      *   D S   I N T E R N E / E S T E R N E
005220161021      // - InfDS
005230161021     d InfDspF         ds
005240161021     d  dsp_aid              369    369a
005250161021     d  sfl_rrn              376    377i 0
005260161021     d  min_nrr              378    379i 0
005270161021     d  num_rcds             380    381i 0
005280001009
005290001009     D WLBDAT          DS                  INZ
005300001009     D  G02DAT                 1      8  0
005310001009     D  G02INV                 9     16  0
005320001009     D  G02ERR                17     17
005330001009     D  G02TGI                18     22  0
005340170525
005350170525     d wdat8           ds
005360170525     d  data1                  1      8  0
005370170525     d  data2                  9     16  0
005380170525     d  giolav                17     21  0
005390170525
005400170525     d tfptfa          ds
005410170525     d  p_tfp                  1      3  0
005420170525     d  p_tfa                  4      6  0
005430001020
005440010116     D                 DS
005450010116     D  ds_v1cra1              1      3  0
005460010116     D  ds_v1cra2              4      7  0
005470030606     d  Ds_V1cra12             1      7  0
005480010116     D  ds_v1cra3              8     10  0
005490010116     D  ds_v1ccra              1     10  0
005500001020
005510001023     D                 DS
005520010116     D  ds_v1cor1              1      3  0
005530010116     D  ds_v1cor2              4      7  0
005540030606     d  Ds_V1cor12             1      7  0
005550010116     D  ds_v1cor3              8     10  0
005560010116     D  ds_v1ccor              1     10  0
005570010116
005580001023     D                 DS
005590010116     D  ds_v1crc1              1      3  0
005600010116     D  ds_v1crc2              4      7  0
005610050120     d  ds_V1crc12             1      7  0
005620010116     D  ds_v1crc3              8     10  0
005630010116     D  ds_v1ccrc              1     10  0
005640001215
005650010702     D                 DS
005660010702     D  v1ofoo                 1      2  0
005670010702     D  v1ofom                 3      4  0
005680010702     D  v1ofos                 5      6  0
005690010702     D  ds_v1ofo               1      6  0
005700001215     D                 DS
005710010117     D  ds_giorno              1      2  0
005720010117     D  ds_mese                3      4  0
005730010117     D  ds_anno                5      8  0
005740010117     D  ds_data                1      8  0
005750010201
005760010201     D                 DS
005770010201     D  ds_poe                 1      3  0
005780010201     D  ds_nsr                 4      5  0
005790010201     D  ds_nor                 6     12  0
005800010201     D  ds_nrv                13     14  0
005810010201     D  ds_keyorm              1     14  0
005820010118
005830001215     D clnmat          DS
005840010117     D  mat                    1     31    dim(31)
005850001215     D clnpom          DS
005860001215     D  pom                    1     31    dim(31)
005870001011
005880010118     D Ds_tnsd         DS
005890010117     D  ds_cod                        3
005900010117     D  ds_tip                        1
005910010117     D  ds_des                       25
005920010117     D  ds_pra                        3
005930010122
005940001009     D Parm01          DS
005950001016     D  ppoe                          3  0
005960001016     D  pnor                          7  0
005970001016     D  pnsr                          2  0
005980001016     D  pnrv                          2  0
005990001016     D  psce                          1
006000001018     D  pfgs                          3  0
006010001108     D  ppor                          3  0
006020001108     D  pdtr                          8  0
006030010314     D  pmdo                         10
006040010314     D  ppro                         10
006050001108     D  pdts                          8  0
006060010123     D  prmp                          1
006070010123     D  pbrc                          1
006080010123     D  pref                          2
006090010202     D  pmio                          1
006100041014     d  pndc                          6  0
006110041014     d  pddc                          8  0
006120060516     d  pfvvfgs                       3  0
006130070119
006140070119      * ds per dati sensibili PDA (dati iniziali)
006150140122      * serve anche per le telefonate AUT
006160070119     d datipdasav      ds
006170070119     d  pdasavrsr                    35
006180070119     d  pdasavinr                    35
006190070119     d  pdasavlor                    35
006200070119     d  pdasavprr                     2
006210070119     d  pdasavrer                    35
006220070119     d  pdasavter                    16
006230070119     d  pdasavorr                     4  0
006240070119     d  pdasavncl                     5  0
006250070119     d  pdasavpkg                     7  1
006260070119     d  pdasavvlm                     5  3
006270070119     d  pdasavbnc                     5  0
006280070119     d  pdasavno1                    35
006290070119     d  pdasavno2                    35
006300131107     d  pdasavora1                    4  0
006310131107     d  pdasavora2                    4  0
006320131107     d  pdasavora3                    4  0
006330131107     d  pdasavora4                    4  0
006340070119
006350070119      * ds per dati sensibili PDA (dati modificati)
006360140122      * serve anche per le telefonate AUT
006370070119     d datipdamod      ds
006380070119     d  pdamodrsr                    35
006390070119     d  pdamodinr                    35
006400070119     d  pdamodlor                    35
006410070119     d  pdamodprr                     2
006420070119     d  pdamodrer                    35
006430070119     d  pdamodter                    16
006440070119     d  pdamodorr                     4  0
006450070119     d  pdamodncl                     5  0
006460070119     d  pdamodpkg                     7  1
006470070119     d  pdamodvlm                     5  3
006480070119     d  pdamodbnc                     5  0
006490070119     d  pdamodno1                    35
006500070119     d  pdamodno2                    35
006510131107     d  pdamodora1                    4  0
006520131107     d  pdamodora2                    4  0
006530131107     d  pdamodora3                    4  0
006540131107     d  pdamodora4                    4  0
006550031125      *
006560031125     d FIOR16DS      e ds                  inz
006570001009
006580001116     D TISI92DS      E DS
006590030623     D TRUL90DS      E DS
006600001023     D FIOR05DS      E DS
006610001010     D TIBS02DS      E DS
006620001010     D FNLV14DS      E DS
006630001010     D FNLV13DS      E DS
006640001010     D TISI95DS      E DS
006650111206     D TISI95DS1     E DS                  extname(tisi95ds)
006660111206     D                                     prefix(a)
006670060929     d tisi97ds      e ds
006680130916     d FIOR81DS      e ds
006690001025     D FIOR06DS      E DS
006700010411     D FIOR30DS      E DS
006710010704     D DTASV         E DS
006720010704     D DBLP          E DS
006730090202     d ds5P          e ds                  inz
006740090202     d   §5Ppgm      e                     inz('FIOR18R   ')
006750010206     D OG148         E DS
006760010515     D OG143         E DS
006770060929     d c_og143       e ds                  extname(og143)
006780060929     d                                     prefix(c_)
006790140422     d m_OG143       e ds                  extname(OG143)
006800140422     d                                     prefix(m_)
006810010202     D DDFT          E DS
006820010301     D DCMR          E DS
006830081028     d a_dcmr        e ds                  extname(dcmr) prefix(a_)
006840010314     D DFAR          E DS
006850111221     d dFFC          e ds
006860011113     D DORM01        E DS
006870081125     d dorf01        e ds
006880150609     D TRUL06DS      E DS
006890150609     D  LIN                    1     90  0 DIM(30)
006900030922     d Trul33ds      e ds
006910040419     d dOsr          e ds
006920050323     d Tibs10ds      e ds
006930050323     d  skfigli               21   5520  0 dim(500)
006940060109     d fnlv55ds      e ds
006950061023     d fieu55ds      e ds
006960070122     d fior56ds      e ds
006970070913     d fidg09ds      e ds
006980071031     d dagr          e ds
006990081126     d ds3idp        e ds
007000171016     d Tisid4ds      e ds
007010171016     d Tisid8ds      e ds
007020100222     d ds15          e ds
007030110613     d FIOR95ds      e ds
007040111206     d FIOR96ds      e ds
007050130910     d FIOR97ds      e ds
007060161027     d FIOR971ds     e ds
007070140317     d TRULORSds     e ds
007080140317     d TRULOR2ds     e ds
007090131112     d TRUL03ds      e ds
007100150306     d dUTE01        e ds
007110140130
007120140130      // ds per note AUT
007130140130     d FIORT1DS      e ds
007140140318
007150140318      // Ds per recupero ora presunta ritiro da PDA
007160140318     d FIOR01DS      e ds
007170060203
007180080812      * passaggio parametri per controllo se ritiro all'estero fattibile
007190080812     d fnlv12ds      e ds                  inz
007200090330      * ritorno dati cliente
007210090330     d ds3k          e ds                  inz
007220100812     d ds3q          e ds                  inz
007230100204      * personalizzazione VAO per cliente
007240100204     d dpvo          e ds
007250100224      * Invio e-mail di preavviso
007260100224     d FIOR52DS      e ds
007270150305
007280150305      // Carico filiali gestite dall'utente
007290150305     d TRUL31DS      e ds
007300160526
007310160526      // Ritorna orari servizio ritiri estero
007320160526     d FIORE00DS     e ds
007330160920
007340160920      // Controllo limiti quantità
007350160920     d TRUL73DS      e ds
007360160922     d TRUL731DS     e ds
007370161021
007380161021      // - Aggiunge/Toglie gg/mm dalla data
007390161021     d XGIOLAVDS     e ds                  inz
007400100204
007410001103      * Ds per controllo se dati variati
007420131107     D Vidold        E DS                  extname(fior05d:fior051)
007430130320     D                                     prefix(Old_)
007440071108     D                                     INZ
007450131107     D Vidnew        E DS                  extname(fior05d:fior051)
007460071108     D                                     INZ
007470131018
007480131018      * Ds per controllo se dati variati videata 2
007490131107     D Vidold2       E DS                  extname(fior05d:fior052)
007500131018     D                                     prefix(Old_)
007510131018     D                                     INZ
007520131212     D Vidoldd2      E DS                  extname(fior05d:fior052)
007530131212     D                                     prefix(Oldd_)
007540131212     D                                     INZ
007550131107     D Vidnew2       E DS                  extname(fior05d:fior052)
007560131018     D                                     INZ
007570131018
007580131018      * Ds per controllo se dati variati videata Alert
007590131107     D VidoldA       E DS                  extname(fior05d:fior053)
007600131018     D                                     prefix(Old_)
007610131018     D                                     INZ
007620131212     D VidolddA      E DS                  extname(fior05d:fior053)
007630131212     D                                     prefix(Oldd_)
007640131212     D                                     INZ
007650131107     D VidnewA       E DS                  extname(fior05d:fior053)
007660131018     D                                     INZ
007670160405
007680160405      * Ds per controllo se dati variati videata Conferma Prenotazione
007690160405     D VidoldC       E DS                  extname(fior05d:fior054)
007700160405     D                                     prefix(Old_)
007710160405     D                                     INZ
007720160405     D VidolddC      E DS                  extname(fior05d:fior054)
007730160405     D                                     prefix(Oldd_)
007740160405     D                                     INZ
007750160405     D VidnewC       E DS                  extname(fior05d:fior054)
007760160405     D                                     INZ
007770001010
007780001010      *
007790001010     D KPJBA         E DS
007800060203      * - Parametri x Controllo profilo utenti
007810060203     d TIBS34ds      e ds
007820060203      * - Ds di riferimento al file esterno AZUTE00F
007830060203     d AZUTEds       e ds                  extname(AZUTE00F)
007840060203      * - Ds per dati organigramma
007850060203     d DDatiUte      e ds
007860100218
007870100218      // ds per controllo email
007880100218     d dsemail       e ds
007890140422
007900140422      // ds per tab ORE
007910140422     d dORE          e ds
007920131018
007930131018      // ds per rcd 'O' FNORE - orari apertura
007940131018     d dOREorari     e ds
007950140422
007960140422      // ds per rcd 'G' FNORE - generale
007970140422     d dOREgen       e ds
007980140422
007990140422      // ds per rcd 'S' FNORE - SMS
008000140422     d dOREsms       e ds
008010160314
008020160314      // ds per rcd 'OR' FNORE - orari indicativi ritiro
008030160314     d dOREor        e ds
008040140124
008050140124      // ds per rcd 'O' FNORE - orari apertura di work
008060140124     d wOREorari     e ds                  extname(dOREorari) prefix(w)
008070160712
008080160712      // ds per rcd 'AA' FNORE - Variazione dati alert conferma ritiro
008090160712     d dOREana       e ds
008100161027
008110161027      // ds per rcd 'DT' FNORE - Data Pronta Merce
008120161027     d dOREdt        e ds
008130161026
008140161026      // ds per campo IOREflo - TRULORSDS
008150161026     d dIORE01       e ds
008160140120
008170140120      // ds per campo ACRmai file FNACR
008180140120     d dACR01        e ds
008190161115
008200161115      // - Tabella GPD - Giorni Posticipo Data Ritiro
008210161115     d dGPD          e ds                  inz
008220161115     d  skGiorni               1     14s 0 dim(7)
008230001010      *
008240001010     D                SDS
008250001010     D  VTCPGM                 1     10
008260100219
008270100219      //---------------------------------------------------------------
008280100219      //?Definizione procedure utilizzate.
008290100219      //---------------------------------------------------------------
008300160526      // Ritorna Orari Servizio Ritiro Estero
008310160526     d fiore00r        pr                  extpgm('FIORE00R')
008320160526     d  fiore00ds                          likeds(FIORE00DS)
008330100224
008340100224      // -Invio e-mail di preavviso
008350100224     d fior52r         pr                  extpgm('FIOR52R')
008360100224     d  kpjba                              likeds(KPJBA)
008370100224     d  fior52ds                           likeds(FIOR52ds)
008380110613
008390110613      // Controllo destinatario per ORM Export DPD
008400110613     d fior95r         pr                  extpgm('FIOR95R')
008410110613     d  kpjba                              likeds(KPJBA)
008420110613     d  fior95ds                           likeds(FIOR95ds)
008430111206
008440111206      // Forzature su Filiale Ritiro
008450111206     d fior96r         pr                  extpgm('FIOR96R')
008460111206     d  kpjba                              likeds(KPJBA)
008470111206     d  fior96ds                           likeds(FIOR96ds)
008480130910
008490130910      // Calcolo data ritiro
008500131108     d fior97r         pr                  extpgm('FIOR97R')
008510130910     d  kpjba                              likeds(KPJBA)
008520130910     d  fior97ds                           likeds(FIOR97ds)
008530161027     d  fior971ds                          likeds(FIOR971ds)
008540161027     d                                     options (*nopass)
008550140130
008560140130      // Gestione note AUT
008570140130     d fiort1r         pr                  extpgm('FIORT1R')
008580140130     d  kpjba                              likeds(KPJBA)
008590140130     d  fiort1ds                           likeds(FIORT1DS)
008600131024
008610131024      // Calcola terminal
008620131024     d fnlv55r         pr                  extpgm('FNLV55R')
008630131024     d  fnlv55ds                           likeds(FNLV55DS)
008640100907
008650100907      // - Controllo ritirabilità all'estero
008660100907     d fnlv12r         pr                  extpgm('FNLV12R')
008670100907     d  fnlv12ds                           likeds(FNLV12DS)
008680111206     d  tisi95ds1                          likeds(TISI95DS1)
008690100907     d  tisi97ds                           likeds(TISI97DS)
008700160217
008710160217       // -?Recupero nuovo Numero Prenotazione Ritiro
008720160217     d GetNPR          pr            10
008730160217     d  ParametroNPR                 10
008740100219
008750100219      // -Controllo cappario DPD
008760171016     d tisid4r         pr                  extpgm('TISID4R')
008770171016     d  tisid4ds                           likeds(TISID4DS)
008780100222
008790100222      // -Interrogazione cappario DPD
008800171016     d tisid8r         pr                  extpgm('TISID8R')
008810100222     d  kpjba                              likeds(KPJBA)
008820171016     d  tisid8ds                           likeds(TISID8DS)
008830130912
008840131112      // - Interrogazione orari di servizio
008850140317     d trulorsr        pr                  extpgm('TRULORSR')
008860130912     d  kpjba                              likeds(KPJBA)
008870140317     d  trulorsds                          likeds(TRULORSDS)
008880140317     d  trulor2ds                          likeds(TRULOR2DS)
008890131024     d                                     options (*nopass)
008900131112
008910131112      // - Controlla orari apertura + note 1/2
008920131112     d trul03r         pr                  extpgm('TRUL03R')
008930131112     d  trul03ds                           likeds(TRUL03DS)
008940160920
008950160920      // - Controlla limiti quantità
008960160920     d trul73r         pr                  extpgm('TRUL73R')
008970160920     d  trul73ds                           likeds(TRUL73DS)
008980160922     d trul731r        pr                  extpgm('TRUL731R')
008990160922     d  trul731ds                          likeds(TRUL731DS)
009000140318
009010140318      // - Recupero ora presunta ritiro
009020140318     d fior01r         pr                  extpgm('FIOR01R')
009030140318     d  fior01ds                           likeds(FIOR01DS)
009040161021
009050161021      // - Aggiunge/Toglie gg/mm dalla data
009060161021     d XGIOLAV         pr                  extpgm('XGIOLAV')
009070161021     d  xgiolavds                          likeds(xgiolavds)
009080100218
009090100218      //---------------------------------------------------------------
009100100218      //?Definizione prototipi utilizzati.
009110100218      //---------------------------------------------------------------
009120100218      /copy gaitrasrc/srcprotopr,xemail
009130161020      /copy gaitrasrc/srcprotopr,XSRDA8
009140170525      /copy gaitrasrc/srcprotopr,XSRLAV8
009150111221      /copy gaitrasrc/srcprotopr,tibs02r
009160131017      /copy gaitrasrc/srcprotopr,ubchkcel
009170131017      /copy gaitrasrc/srcprotopi,ubchkcel
009180100218
009190100218      //---------------------------------------------------------------
009200100218      //?Definizione key-list.
009210100218      //---------------------------------------------------------------
009220100218
009230100218       // - File FNVAOE1L
009240100218     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
009250100218     d                                     prefix(k_)
009260100218     d                                     inz
009270131018
009280131018       // - File FNORE01L
009290131029     d k05fnore      e ds                  extname(FNORE01L:*key)
009300131018     d                                     prefix(k_)
009310131018     d                                     inz
009320131029
009330131029       // - File FNORPE1L
009340131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
009350131029     d                                     prefix(k_)
009360131029     d                                     inz
009370001010
009380001006      *------------------------------------------------------------------------*
009390001009
009400010123     C     inizio        tag
009410010329
009420150713     c                   IF        not ConfAuto
009430010329     C                   clear                   fior06DS
009440010329     C                   movel     'C'           i06tla
009450010405     C                   movel(p)  kpjbu         §kpjbu
009460010405     C                   movel(p)  parm01        kpjbu
009470010329     C                   call      'FIOR06R'
009480010329     C                   parm                    kpjba
009490010329     C                   parm                    fior06ds
009500010405     C                   movel     §kpjbu        kpjbu
009510150713     c                   ENDIF
009520010329
009530010413      * Spengo gli indicatori di comodo
009540010507     C                   setoff                                       060708
009550010515     C                   setoff                                       131415
009560070913     c                   setoff                                       172124
009570081106     c                   setoff                                       252629
009580040426     C                   setoff                                       373839
009590010522     C                   setoff                                       334057
009600010507     C                   setoff                                       666768
009610010507     C                   setoff                                       697071
009620010507     C                   setoff                                       727678
009630160316     C                   setoff                                       122089
009640061023     c                   setoff                                       3418
009650140130     c                   setoff                                       9697
009660040615     c                   Eval      wokdirotta = *Off
009670050304     c                   Eval      wforzadar1 = *Off
009680050426     c                   Clear                   v3cau
009690050502     c                   Eval      wricevuta = *Off
009700060109     c                   Eval      wforzapor  = *Off
009710070124     c                   Eval      wforzadir = *Off
009720070228     c                   eval      wforzachi = *off
009730071031     c                   eval      $varqta = *off
009740080221     c                   eval      wforzaqta = *off
009750100317     c                   clear                   v1este
009760100906     c                   clear                   dpvo
009770100906     c                   eval      ok_pvo = *off
009780131202     c                   Eval      wForzaNac = *Off
009790140226     c                   eval      wF17write = *off
009800140320     c                   eval      wOKorariSer = *off
009810140508     c                   eval      wRicAlert = *off
009820160405     c                   eval      wRicConf = *off
009830160406     c                   clear                   sav_mailconf
009840160406     c                   clear                   sav_smsconf
009850160426     c                   eval      WebNPR = *off
009860160516     c                   eval      $modifica = *off
009870160921     c                   clear                   wold_V1ncl
009880160921     c                   clear                   wold_V1pkg
009890160921     c                   clear                   wold_V1vlm
009900170524       wOkPosticipa = *off;
009910170524       wAnticipa = *off;
009920170524       clear DarCalcolata;
009930170524       Anticipato = *off;
009940170524       clear ggAnticipo;
009950071108
009960071108      *------------------
009970010123
009980071108     ?* Manutenzione/Copia ?
009990010115     C                   if        *in02 = *on or *in03 = *on
010000150924     c                   eval      *in23 = *off
010010010115     C                   exsr      Sr_Aggiorna
010020050502      * Richiamato da procedura conferma ORM commissionati
010030090205      * il richiamo fatto con F6 - Conferma tutti dal subfile del fior04r
010040090205      * passa sempre §RMTLA = '2' perchè l'ORM è sempre da visualizzare
010050010124     C                   if        §RMfpr = 'O'
010060010124      * Esce dal programma se richiesta la conferma totale e ci sono degli errori
010070010124     C                   if        §RMtla = '0'
010080010124     C                   goto      endsrciclo
010090010124     C                   endif
010100010124      * Esce dal programma se richiesta la conferma e non ci sono degli errori
010110010124     C                   if        §RMtla = '1' and *in28 = *off
010120010124     C                   goto      endsrciclo
010130010124     C                   endif
010140140506      * Esce dal programma se conferma manuale ORM con alert non andata a buon fine
010150140506      * per conferma bolla
010160140506     c                   if        §RMtla = '2' and walert and *in28 and
010170140506     c                             §RMerr = '5'
010180140506     c                   goto      endsrciclo
010190140506     c                   endif
010200050502      * Esce dal programma se l'orm è allocato
010210010522     C                   if        *in33 = *on
010220010522     C                   goto      endsrciclo
010230010522     C                   endif
010240060726      * Esce dal programma se l'orm è già confermato
010250060726     C                   if        wormconf = *on
010260060726     C                   goto      endsrciclo
010270060726     C                   endif
010280091007      * Esce dal programma se l'orm non è in fase modificabile
010290091007     c                   if        wormnoman
010300091007     c                   goto      endsrciclo
010310091007     c                   endif
010320010124     C                   else
010330010115     C   28              goto      endsrciclo
010340010124     C                   endif
010350001023     C                   endif
010360071108
010370071108     ?* Immissione ?
010380010115     C                   if        *in01 = *on
010390010115     C                   exsr      Sr_Immetti
010400010117      * Esce dal programma se richiesta la conferma totale e ci sono degli errori
010410010124     C                   if        §RMtla = '0'
010420010117     C                   goto      endsrciclo
010430010117     C                   endif
010440010118      * Esce dal programma se richiesta la conferma e non ci sono degli errori
010450010118     C                   if        §RMtla = '1' and *in28 = *off
010460100812     c                             and not $errpre
010470010118     C                   goto      endsrciclo
010480010118     C                   endif
010490150924      /free
010500150924       //?Controllo se visualizzare o no la filiale emissione
010510150924       IF  DUTpou <> V1Cpoe and V1Cpoe > 0;
010520150924         *in23 = *on;
010530150924       ELSE;
010540150924         *in23 = *off;
010550150924       ENDIF;
010560150924      /end-free
010570010115     C                   endif
010580010202
010590040419      * Abilito o meno il tasto F21 x dirottamento
010600040419      * --> quando è modifica ORM
010610040419      * --> il p.o. ritiro è protetto quindi non ho altre possibilità x dirottare l'ORM
010620150305      * --> il p.o. ritiro deve essere gestito dall'utente
010630040419      * --> non devo essere in chiusura ORM
010640050321      * --> l'ORM deve essere in fase < 600
010650050315      * --> non deve essere un ORM prepagato
010660150305     c                   IF        %lookup(%editc(num_v1cpor:'X'):skpog) > 0
010670150305     c                             and *in02 and *in09 and not *in10 and
010680150305     c                             ORMfao < 600 and ORMtor <> 'P'
010690150305     c                   Eval      *In19 = *on
010700150305     c                   ENDIF
010710071108
010720140918      * se ho abilitato il tasto F21 lo disabilito se
010730130412      * ORM in fase 400
010740080314      * e se ORM in fase 410
010750140918      * e se ORM in fase 420
010760130412     c                   if        ormfao = 400 or ormfao = 410
010770140918     c                             or ORMfao = 420
010780071108     c                   eval      *in19 = *off
010790071108     c                   endif
010800001010
010810071107      * salvo i dati dell'ORM appena caricati a video
010820090211     C                   eval      Vidold = Vidnew
010830131018     C                   eval      Vidold2 = Vidnew2
010840131018     C                   eval      VidoldA = VidnewA
010850160406     c                   eval      VidoldC = VidnewC
010860070119
010870070119      * salvo i dati sensibili per PDA
010880140122      * o per telefonate AUT
010890070123      * se p.o. abilitato
010900070119      * se sono in modifica
010910150609      * p.o. ritiro in gestione all'utente vuol dire che modifico e non proposta di variazione
010920070119      * se ORM in fase 400 o 410
010930070119      * negli altri casi non serve
010940140122     c                   if        *in02 and
010950140317     c                             *in22 and
010960150305     c                             %lookup(%editc(ormpor:'X'):skpog) > 0 and
010970071210     c                             (v1fao = 400 or v1fao = 410)
010980070119     c                   exsr      sr_datipdasav
010990070119     c                   endif
011000100317
011010100317      /free
011020100317       //?Imposto costante se ORM con preavviso
011030100317       IF  §ormpre = 'M';
011040100317         v1este = 'Preavviso: '+
011050100317                  'Mail';
011060100317       ENDIF;
011070140506       IF  walert and not *in01;
011080140506         v1este = 'Preavviso: Mail/SMS';
011090140506       ENDIF;
011100131014
011110131014       //?Imposto la riga dei tasti funzione
011120131014       ExSr Sr_Tastifun;
011130100317      /end-free
011140050502
011150050502      * Emissione 1° Videata
011160001006     C                   do        *hival
011170010208
011180150305      * Se immissione ricalcola la data
011190170615      * o se copia ORM
011200150305     C                   if        *in01
011210170615     c                             or *in03
011220010208     C                   exsr      Sr_Calcolaoggi
011230010208     C                   endif
011240030606
011250030606      * Se sto riemettendo la videata xchè ORM commissionato prima devo
011260030606      * impostare l'ordinante in base a quello che è stato impostato
011270030606      * nella videata
011280030606If  2c                   If        whocor <> *Blanks
011290030610Sel 3c                   Select
011300030606      * l'ordinante è il mittente
011310030610     c                   When      wdcor = 'M'
011320030606     c                   Eval      v1cor1 = v1cra1
011330030606     c                   Eval      v1cor2 = v1cra2
011340030606     c                   Eval      v1cor3 = v1cra3
011350030606     c                   Eval      v1rso = v1rsr
011360030606     c                   Eval      v1ino = v1inr
011370030606     c                   Eval      v1cao = v1car
011380030606     c                   Eval      v1loo = v1lor
011390030922     c                   Eval      sav_v1loo35 = v1lor
011400030606     c                   Eval      v1pro = v1prr
011410030606     c                   Eval      v1nao = v1nar
011420131106      /free
011430131106       //?se ordinante = mittente l'ORM è NON commissionato
011440131106         V1com = 'N';
011450131112       //?mi devo posizionare sulle note
011460131112         *in82 = *on;
011470131106      /end-free
011480030606      * l'ordinante è il destinatario
011490030610     c                   When      wdcor = 'D'
011500030609     c                   Eval      v1cor1 = v1crc1
011510030609     c                   Eval      v1cor2 = v1crc2
011520030609     c                   Eval      v1cor3 = v1crc3
011530030609     c                   Eval      v1rso = v1rsc
011540030609     c                   Eval      v1ino = v1inc
011550030609     c                   Eval      v1cao = v1cac
011560030609     c                   Eval      v1loo = v1loc
011570030922     c                   Eval      sav_v1loo35 = sav_v1loc35
011580030609     c                   Eval      v1pro = v1prc
011590030609     c                   Eval      v1nao = v1nac
011600131112      /free
011610131112       //?mi devo posizionare sulle note se ok il destinatario
011620131112         IF  V1rso <> *blanks;
011630131112           *in82 = *on;
011640131112         ELSE;
011650131112       //?mi posiziono sul destinatario così lo possono immettere
011660131112           *in56 = *on;
011670131112         ENDIF;
011680131112      /end-free
011690030610     c                   Other
011700030606     c                   Eval      *In42 = *On
011710030610    3c                   EndSl
011720030606     c                   Clear                   whocor
011730030610    2c                   EndIf
011740030609      * se sto riemettendo la videata x ritorno da immissione bolla e ho il 28 acceso
011750030609      * lo devo spegnere
011760030609     c                   If        V1com = 'S' and Okbolla = '1' and *In28
011770030609     c                   Eval      *In28 = *Off
011780030609     c                   EndIf
011790050502
011800050502      * Prima di andare avanti con i controlli della videata
011810050502      * se conferma ORM commissionati e ORM con ricevuta di ritiro
011820050502      * devo prima emettere una finestra con un promemoria
011830050502     c                   If        *In11 and §rmfpr = 'O' and
011840050502     c                             wricevuta = *Off
011850090211     C                   write     fior051
011860050502Do  2c                   Do        *Hival
011870050502     c                   Exfmt     Fior05r
011880050502     c                   Eval      wricevuta = *On
011890050502If  3C                   If        *Inkl
011900050502     c                   Leave
011910050502    3c                   EndIf
011920050502    2c                   EndDo
011930050502     c                   EndIf
011940070925
011950070925      * emetto Window per avvisare se mittente e/o qtà diverse tra proposta ORM e
011960070925      * dati memorizzati nell'ORM nel caso di giro impostato a video
011970070925     c                   if        *in25 and v1ccgi <> *blanks
011980090211     C                   write     fior051
011990070925Do  2c                   do        *hival
012000070925     c                   exfmt     fior05orp
012010070925     c                   eval      *in25 = *off
012020070925     c   kl              leave
012030070925    2c                   enddo
012040070925     c                   endif
012050100223
012060100223      /free
012070100223       //?se errore relativo al preavviso
012080100223       //?devo emettere una win per la richiesta dei dati corretti
012090100223         DOW  $errpre;
012100100223           exsr sr_DatiPre;
012110100223         ENDDO;
012120140506
012130140506       //?se conferma ORM commissionato con alert
012140140506       //?di un orm che in conferma automatica ha avuto un errore
012150140506       //?informo l'utente che non deve telefonare
012160140506         IF  §RMfpr = 'O' and walert and not wNOTEL;
012170140506           *in28 = *off;
012180140506           write FIOR051;
012190140506           exsr CommissionatoAlert;
012200140506Do  2      DOW  not *inkl;
012210140506             exfmt  FIOR05WTEL;
012220140506           ENDDO;
012230140506           wNOTEL = *on;
012240140506         ENDIF;
012250100812
012260130919       //?se conferma ORM commissionato e dirottato
012270130919       //?per ORM con prevviso via mail o per ORM NON COMMISSIONATI
012280130919       //?informo l'utente che non deve telefonare
012290130919       //?mi posiziono qua tanto se dirottato sono già stati fatti i controlli
012300130919       //?relativi al prevviso
012310150206       //?NON faccio niente se ORM estero/DPD
012320131212         IF  §RMfpr = 'O' and $NoApp and not wNOTEL and
012330150206            (§ORMpre = 'M' or V1com = 'N') and
012340150206            not *in15 and not *in14;
012350130919             SELECT;
012360130919               WHEN  §ORMpre = 'M';
012370140506                 exsr DirottaPreavviso;
012380130919               WHEN  V1com = 'N';
012390140506                 exsr DirottaNoComm;
012400130919             ENDSL;
012410140506           write FIOR051;
012420140506Do  2      DOW  not *inkl;
012430140506             exfmt  FIOR05WTEL;
012440100812           ENDDO;
012450130918           wNOTEL = *on;
012460100812         ENDIF;
012470131030
012480160406       //?se devo riemettere la videata ed ho errore in una delle 3 WIN
012490160406       //?emetto il messaggio di errore in sospeso su altre videate
012500160405         IF  wErrF04 or wErrF05 or wErrF13;
012510131030           *in28 = *on;
012520140424           V1Cmsg = msg(73);
012530131030         ENDIF;
012540140423
012550160406       //?se errore dovuto a controllo Alert emetto win per sistemare i dati
012560160406       //?il controllo è fatto al momento del F6
012570140423         IF  wErrAlert;
012580140423           exsr F04D01;
012590140423         ENDIF;
012600160406
012610160406       //?se errore dovuto a controllo dati Conferma Prenotazione
012620160406       //?il controllo è fatto nella sr_contr richiamata dopo il
012630160406       //?caricamento dati da VAS
012640160406       //?emetto la win per sistemarli
012650160406         IF  wErrConferma;
012660160406           exsr F13D01;
012670160406         ENDIF;
012680130918
012690100223      /end-free
012700001010
012710071107     ?* emetto la videata
012720071107      *                  __________________
012730090211     C                   exfmt     fior051
012740071107      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
012750071108     C                   setoff                                       28  90
012760131030     c                   eval      wErrF04 = *off
012770131030     c                   eval      wErrF05 = *off
012780160405     c                   eval      wErrF13 = *off
012790131030     c                   clear                   v1cmsg
012800131011
012810131011      /free
012820131014       //?F4=Dati Avviso Ritiro
012830131014       IF  *inkd;
012840131202         exsr sr_contr;
012850131202         IF  *in28 or *in90;
012860131202           iter;
012870131202         ENDIF;
012880131014         exsr F04D01;
012890140509         IF  not wCalDtRit;
012900140509           iter;
012910140509         ENDIF;
012920131014       ENDIF;
012930131011
012940131011       //?F5=Altri dati
012950131011       IF  *inke;
012960131210         exsr sr_contr;
012970131202         IF  *in28 or *in90;
012980131202           iter;
012990131202         ENDIF;
013000131011         exsr F05D01;
013010131011         iter;
013020131011       ENDIF;
013030161021
013040161027       //?- PageDown --> devo calcolare 1 gg in meno nella data ritiro
013050161027       //?- PageUp   --> devo calcolare 1 gg in più nella data ritiro
013060161027         IF  dsp_aid = c_PageDown or
013070161027             dsp_aid = c_PageUp;
013080161026           exsr AddSubUnoDar;
013090161021         ENDIF;
013100131011      /end-free
013110001011
013120001011      * F6=Conferma
013130001025     C                   if        *inkf = *on
013140010202     ?* Controlla ?
013150081126     C                   exsr      Sr_Contr
013160030610     C   28
013170030610     Cor 90              iter
013180131030
013190131030      /free
013200160405       //?Controllo anche i dati della videata F04 e F05 e F13
013210160316       //?F04 Alert
013220160316       //?solo se sono in immissione o copia, negli altri casi non serve.
013230160406       //?o in conferma ORM commissionati
013240160316         IF  (*in01 or *in03 or §RMfpr = 'O');
013250150305         //?F04 Alert
013260140429             wErrF04 = *off;
013270140429             exsr CtrDatiF04;
013280140429             IF  *in28;
013290140429               wErrF04 = *on;
013300160316           //?se conferma ORM commissionati
013310160316           //?per In12 acceso lo spengo (il 12 protegge i campi della win)
013320160406           //?in questo modo do la possibilità di variare i dati sbagliati
013330160316               IF  *in12 and §RMfpr = 'O' and §RMtla = '2';
013340160316                 *in12 = *off;
013350160316               ENDIF;
013360140429               iter;
013370140429             ENDIF;
013380160316         ENDIF;
013390160316         //?F05 Altri dati
013400160316         //?solo se NON sono in chiusura ORM
013410160316         IF  not *in10;
013420140429           wErrF05 = *off;
013430140429           exsr CtrDatiF05;
013440140429           IF  *in28;
013450140429             wErrF05 = *on;
013460140429             iter;
013470140429           ENDIF;
013480131216         ENDIF;
013490160405       //?Se ORM Commissionato ed ho emesso la WIN per avere i dati dell'Alert
013500160405       //?ma i dati non sono stati inseriti devo riemettere la videata
013510160405         IF  V1com = 'S' and wRicAlert and
013520160405             w03sms = *blanks and w03mail = *blanks and not wOKF04;
013530140711           exsr F04D01;
013540160316           wOKF04 = *on;
013550140711           iter;
013560140711         ENDIF;
013570160405       //?F13 Conferma Prenotazione Ritiro
013580160406       //?solo se sono in immissione o copia, negli altri casi non serve.
013590160405         IF  *in01 or *in03;
013600160405         //?F13 Conferma Prenotazione Ritiro
013610160405           wErrF13 = *off;
013620160405           exsr CtrDatiF13;
013630160405           IF  *in28;
013640160405             wErrF13 = *on;
013650160405             iter;
013660160405           ENDIF;
013670160405         ENDIF;
013680160406       //?Se ho emesso in precedenza la WIN per avere i dati relativi alla Conferma
013690160406       //?prenotazione ma i dati non sono stati inseriti devo riemettere la WIN
013700160406       //?questo capita solo in immissione manuale con mittene non codificato
013710160406         IF  wRicConf and
013720160406             W04sms = *blanks and W04mail = *blanks and not wOKF13;
013730160406           exsr F13D01;
013740160406           wOKF13 = *on;
013750160406           iter;
013760160406         ENDIF;
013770131030      /end-free
013780030606
013790010507      * Capita che ci sono degli orm con po ritiro a zero e di conseguenza il po di
013800010507      * trasmissione e in questo caso va in errore la trasmissione
013810010507      * così aggiungo anche qua il controllo
013820010507     C                   if        v1cpor = *blanks or v1cpor = '000'
013830010507     C                   movel     msg(11)       v1cmsg
013840010507     C                   seton                                        59  28
013850010507     C                   iter
013860010507     C                   endif
013870010507      * Se il ksc è cambiato devo riemettere la videata
013880010906      * può capitare se variano il codice di chi paga o il flag chi paga
013890010906      * e poi danno subito F6 senza enter
013900070920      * stessa cosa per il mittente è variato devo riemettere la videata
013910070920      * è importante per il giro
013920070920     C                   if        (v1ksc <> sav_v1ksc and
013930070920     C                              sav_v1ksc <> *zeros) or
013940070920     c                             (ds_v1ccra <> sav_v1ccra and
013950070920     c                              sav_v1ccra <> *zeros)
013960070921     c                   eval      v1cmsg = msg(70)
013970070920     C                   eval      sav_v1ksc = v1ksc
013980070919     c                   eval      sav_v1ccra = ds_v1ccra
013990070920     c                   eval      *in28 = *on
014000070919     c                   iter
014010070919     c                   endif
014020010301      * Se Chiusura orm e note obbligatorie richiamo il programma
014030010301     C                   if        d§cmrnot = 'S' and *in10 = *on
014040030731     C                             and (v1fao = 999 or v1fao = 910)
014050010301     C                   do        *hival
014060010301     C                   exsr      sr_note
014070010301     C                   if        o06f03 = 'S' or o06f12 = 'S'
014080010301     C                   iter
014090010301     C                   endif
014100010301     C                   if        o06dai <> v1dfo
014110010301     C                   iter
014120010301     C                   endif
014130010301     C                   if        o06ori < v1ofo
014140010301     C                   iter
014150010301     C                   endif
014160010301     C                   if        o06prg = 0
014170010301     C                   iter
014180010301     C                   endif
014190010301     C                   leave
014200010301     C                   enddo
014210010301     C                   endif
014220040402      * se è stata modificata la data di ritiro devo richiedere le note
014230040402      * obbligatorie
014240040405if  1c                   If        Ormdar <> Inv_V1Dar and *In02
014250040914      * e non è un cambio di p.o.
014260040914     c                             and num_v1cpor = ormpor
014270050420      * e non è una comferma di una proposta
014280050420     c                             and §rmfpr <> 'P'
014290040402do  2c                   Do        *Hival
014300040402     c                   ExSr      Sr_Note
014310040402     C                   if        o06f03 = 'S' or o06f12 = 'S'
014320040402     c                             and i06flm = 'D'
014330040402     C                   iter
014340040402     C                   endif
014350040402     C                   movel     w0140         I06ori
014360040402     C                   if        o06dai <> I06dai and i06flm = 'D'
014370040402     C                   iter
014380040402     C                   endif
014390040402     C                   if        o06ori < i06ori and i06flm = 'D'
014400040402     C                   iter
014410040402     C                   endif
014420040402     C                   if        o06prg = 0 and i06flm = 'D'
014430040402     C                   iter
014440040402     C                   endif
014450040402     C                   leave
014460040402e   2c                   EndDo
014470040402e   1c                   EndIf
014480090205      * se conferma ORM commissionati prima di confermare il ritiro devo emettere
014490090205      * un window per richiedere i dati dell'appuntamento
014500090206      * non lo faccio se sto dirottando l'ORM
014510090407      * e se ORM export
014520130918      * e se l'orm è già stato dirottato prima ed ho avvisato l'utente di NON telefonare
014530140429      * e se per l'orm sono previsti gli alert
014540090407     c                   eval      wokapp = *off
014550090206     c                   if        §rmfpr = 'O' and not *in29
014560090407     c                             and not *in14 and not *in15
014570130918     c                             and not wNOTEL
014580140429     c                             and not walert
014590090205     c                   exsr      sr_ormapp
014600090205      * se non è stato confermato l'appuntamento (F6) devo riemettere la videata
014610090205      * dell'ORM
014620090205     c   90              iter
014630090407      * imposto flag per ok info inserite
014640090407     c                   eval      wokapp = *on
014650090205     c                   endif
014660010202     ?* Confermo ?
014670001103     C                   exsr      Sr_Conferma
014680030611     C   28
014690030611     Cor 90              iter
014700010117      * Orm già stampato si vuole ristampare?
014710150305      * solo se il p.o. ritiro è in gestione all'utente
014720080225      * e se non ho fatto una proposta di variazione
014730010117     C                   if        v1dst > *zeros  and
014740150305     C                             d§farstp = 'S' and
014750150305     c                             %lookup(%editc(num_v1cpor:'X'):skpog) > 0
014760080225     c                             and $proposte = *off
014770001108     C                   exsr      Sr_Contrstp
014780001108     C                   endif
014790010124     C                   if        §RMtla = *blanks and *in01 = *on
014800010123     C                   goto      inizio
014810010124     C                   else
014820010124     C                   leave
014830010124     C                   endif
014840001011     C                   endif
014850001009
014860001009      * F7=Int.Cod.Ritiro
014870001026     C   kg              exsr      Sr_RicRitiro
014880001106     C   90              iter
014890001009
014900001009      * F8=Int.Cod.Ordinante
014910001011     C   kh              exsr      Sr_RicOrdin
014920001106     C   90              iter
014930001009
014940011113      * F9=Int.Cod.Destinatario
014950001011     C   ki              exsr      Sr_RicCons
014960001106     C   90              iter
014970001222
014980001222      * F11=Calcola volume
014990001222     C                   if        *inkk = *on
015000001222     C                   exsr      Sr_Volume
015010001222     C                   iter
015020001222     C                   endif
015030001011
015040001011      * F12=Ritorno
015050001023     C                   if        *inkl = *on
015060010209      * Cambiato idea dopo F16 di chiusura
015070050322     C                   if        *in10 = *on and (§RMtla = *blanks
015080050322     c                             or §rmfpr = 'O')
015090001107     C                             and *in02 = *on
015100001109     C                   eval      *in10 = *off
015110010302     C                   clear                   dcmr
015120001107     C                   movel     mod(2)        v1mod
015130090211     c                   if        §rmfpr = 'O'
015140090211     c                   eval      v1mod = mod(7)
015150090211     c                   endif
015160010116     C                   eval      v1fao = sav_v1fao
015170010116     C                   eval      v1dfo = sav_v1dfo
015180010116     C                   eval      v1ofo = sav_v1ofo
015190010202     C                   exsr      Sr_Contrfao
015200010202     C                   clear                   v3cau
015210010202     C                   clear                   sav_v1fao
015220010202     C                   clear                   sav_v1dfo
015230010202     C                   clear                   sav_v1ofo
015240131025      /free
015250131025       //?Imposto la riga dei tasti funzione
015260131025       ExSr Sr_Tastifun;
015270131025      /end-free
015280001107     C                   iter
015290001107     C                   endif
015300010307      * Se sto confermando un orm pepagato commissionato non posso uscire
015310010307     C                   if        v1ctor = 'P' and §rmfpr = 'O'
015320010307     C                   eval      *in28 = *on
015330010307     C                   movel     msg(67)       v1cmsg
015340010307     C                   iter
015350010307     C                   endif
015360010117
015370001026     C                   unlock    fnorm01l
015380160902     C                   unlock    fnorg01l
015390010202     C                   eval      §RMerr = '3'
015400001023     C                   leave
015410001023     C                   endif
015420100222
015430100222      /free
015440160406       //?F13=Conferma Prenotazione
015450160405       IF  *inkm;
015460160405         exsr sr_contr;
015470160405         IF  *in28 or *in90;
015480160405           iter;
015490160405         ENDIF;
015500160405         exsr F13D01;
015510160405         iter;
015520160405       ENDIF;
015530160405
015540130912       //?F14=Interrogazione oraro di servizio
015550130912       IF  *inkn;
015560130913         exsr F14D01;
015570130912         iter;
015580130912       ENDIF;
015590130912
015600100222       //?F15=Interrogazione cappario DPD
015610100907       IF  *inkp and *in79;
015620100222       //?Se è network DPD ok interrogo e passo cap fittizio
015630131030         IF  v1ntwd <> *blanks or ntw_V1Cpor = 'DPD';
015640100907           TBLkey = v1nar;
015650100222           chain (CodUt : '15' : TBLkey ) TABEL;
015660100222           IF  %found( TABEL00F );
015670100222             ds15 = TBLuni;
015680100222           ENDIF;
015690100907           %subst(wcap:(§15pcf):(%len(v1car)-§15pcf)) =
015700100907             %subst(v1car:(§15pcf):(%len(v1car)-§15pcf));
015710100907           %subst(wcap:(§15pcf+§15ecf):(%len(v1car)-§15ecf)) = *blanks;
015720171016           clear tisid8ds;
015730171016           isid8dri = oggi_aammgg;
015740171016           isid8op0 = 'D01';
015750171016           isid8nar = v1nar;
015760171016           isid8cap = wcap;
015770171016           tisid8r (kpjba : tisid8ds);
015780100222       //?Per errore riemetto la videata con errore
015790171016           IF  osid8err <> *blanks;
015800100222             *in28 = *on;
015810100907             *in50 = *on;
015820100222             *in62 = *on;
015830171016             v1cmsg = osid8msg;
015840100222             iter;
015850100222           ENDIF;
015860100222       //?Selezionato un cap lo imposto e riemetto la videata
015870171016           IF  osid8capb <> *blanks;
015880171016             v1car = osid8capb;
015890100907             *in50 = *on;
015900100907             *in79 = *off;
015910100222             *in62 = *on;
015920100222             iter;
015930100222           ENDIF;
015940100222       //?Se non è stato selezionato un cap riemetto la videata
015950171016           IF  osid8capb = *blanks;
015960100907             *in50 = *on;
015970100222             *in62 = *on;
015980100222             iter;
015990100222           ENDIF;
016000100222         ENDIF;
016010100222       //?Se non è più network DPD spengo indicatore di comodo
016020131030         IF  v1ntwd = *blanks or ntw_V1Cpor <> 'DPD';
016030100222           *in79 = *off;
016040100222         ENDIF;
016050100222       ENDIF;
016060100222      /end-free
016070010202
016080040908      * F16=Chiusura
016090001020     C                   if        *inkq = *on and *in01 = *off
016100010202     C                   exsr      Sr_Chiusura
016110010202     C                   if        *in10 = *on
016120010202     C                   movel     mod(5)        v1mod
016130010202     C                   endif
016140010202     C                   iter
016150010202     C                   endif
016160140130
016170140130      /free
016180140130       //?F17=Note AUT
016190140130       IF  *inkr;
016200140130         exsr F17D01;
016210140130         iter;
016220140130       ENDIF;
016230140130      /end-free
016240010202
016250001025      * F18=Note
016260001025     C                   if        *inks = *on
016270001025     C                   exsr      Sr_Note
016280001106     C                   iter
016290001025     C                   endif
016300010202
016310001020      * F19=Proposte
016320001020     C                   if        *inkt = *on and *in57 = *on
016330001103     C                   exsr      Sr_Proposte
016340001106     C                   iter
016350001020     C                   endif
016360010202
016370001102      * F20=Simulazione
016380001116     C                   if        *inku = *on
016390001102     C                   exsr      Sr_Simula
016400001106     C                   iter
016410001102     C                   endif
016420061023
016430061023      * F2=Dati DPD
016440061023     c                   if        *inkb
016450061023     c                   exsr      sr_datidpd
016460061023     c                   endif
016470131011      * F24=Altri tasti  ?
016480131011     c                   if        *inky and $loop > 1
016490131011     c                   exsr      sr_f24
016500131014     c                   iter
016510131011     c                   endif
016520001010
016530001011      * Controlli
016540010202     C                   if        *in28 = *off
016550081126     C                   exsr      Sr_Contr
016560001020     C                   endif
016570010202
016580010202      * Ritorna ad emetter la videata se ci sono errori o se sono state fatte delle ricerche
016590001010     C   28
016600001010     Cor 90              iter
016610040420      * F21=Dirottamento
016620040420      * lo faccio dopo i controlli xchè nella videata del dirottamento mi servono dei dati
016630040420      * che vengono generati nella routine sr_contr
016640040420     c                   If        *InKv = *on
016650050420      * prima di dare l'ok al dirottamento però devo controllare che non ci siano delle proposte
016660050420      * di variazione ancora da elaborare
016670050420      * in questo caso blocco il dirottamento xchè prima sono da elaborare le proposte di
016680050420      * variazione altrimenti poi vengono fuori dei problemi con il p.o. ritiro
016690050420     c                   ExSr      Sr_ContrProp
016700050420  b6 c                   If        wproposte = *On
016710050420     c                   Eval      *In28 = *On
016720050420     c                   Eval      v1cmsg = msg(88)
016730050420     c                   Iter
016740050420     c                   EndIf
016750040420      * per dirottare l'ORM
016760040420      *  ---> se ha la serie deve essere abilitato da tabella OSR
016770040420     c                   If        d§Osrdir <> *Blanks
016780040420     c                   Eval      *In28 = *On
016790040420     c                   Eval      v1cmsg = msg(77)
016800040420     c                   Iter
016810040420     c                   EndIf
016820040420     c                   ExSr      Sr_Dirotta
016830040628      * se ok il dirottamento aggiorno l'ORM
016840040628     c                   If        wokdirotta = *On
016850040628     c                   Eval      v1dpor = wddpor
016860040628     c                   Eval      v1mod = mod(6)
016870040628     c                   Eval      *In29 = *On
016880070924      * prima di riemettere se devo imposto il giro dal cliente
016890070924     c                   if        ds_v1ccra <> *zeros
016900070924     c                   movel     v1cpor        num_v1cpor
016910071031      * recupero la tabella AGR per il nuovo po ritiro
016920071031     c                   exsr      sr_caragr
016930071031      * recupero anche il nuovo giro
016940070924     c                   exsr      sr_carcgi
016950071023     c                   if        sav_acrcgi <> *blanks
016960070924     c                   eval      v1tgi = 'A'
016970070924     c                   eval      v1ccgi = sav_acrcgi
016980070924     c                   eval      sav_v1ccgi = sav_acrcgi
016990070924     c                   else
017000070924     c                   clear                   v1tgi
017010070924     c                   clear                   v1ccgi
017020070924     c                   clear                   sav_v1ccgi
017030070924     c                   endif
017040070924     c                   exsr      ctrgiro
017050070924     c                   if        d09ocgi <> ' '
017060070924     c                   eval      v1dcgi = d09odes
017070070924     c                   endif
017080070924     c                   endif
017090040628     c                   EndIf
017100040420     c                   EndIf
017110001010
017120001006     C                   enddo
017130001023
017140001023     C     endsrciclo    tag
017150001026
017160001106      * Se richiamato e allocato ritorna al pgm chiamante con codice di ritorno
017170010117     C                   if        §RMfpr = 'P' and *in28 = *on
017180001106     C                   eval      §RMerr = '3'
017190001106     C                   endif
017200001106
017210030922     C                   movel(p)  kpjbu         §kpjbu
017220030922
017230150713     c                   IF        not ConfAuto
017240001026     C                   clear                   fior06DS
017250001026     C                   movel     'C'           i06tla
017260010405     C                   movel(p)  parm01        kpjbu
017270001026     C                   call      'FIOR06R'
017280001026     C                   parm                    kpjba
017290001027     C                   parm                    fior06ds
017300150713     c                   ENDIF
017310030922
017320030922     c                   clear                   TRUL33DS
017330030922     c                   eval      I33tla = 'C'
017340030922     c                   movel(p)  TRUL33DS      KPJBU
017350030922     c                   call      'TRUL33R'
017360030922     c                   parm                    KPJBA
017370031125
017380031201      * E' meglio lasciare l'eventuale chiusura del prtf laser degli
017390031201      *   ORM al pgm. chiamante, perchè altrimenti verrebbe stampato
017400031201      *   1 ORM ogni pagina per 4 ORM
017410040302     c                   clear                   FIOR16ds
017420040302     c                   eval      R16tla = 'C'
017430040302     c                   movel(p)  FIOR16DS      KPJBU
017440090202     c                   call      §5Ppgm
017450040302     c                   parm                    KPJBA
017460080812
017470080812      * chiudo FNLV12
017480080812     c                   clear                   fnlv12ds
017490111206     c                   clear                   tisi95ds1
017500080812     c                   clear                   tisi97ds
017510080812     c                   eval      i95tla = 'C'
017520080812     c                   call      'FNLV12R'
017530080812     c                   parm                    fnlv12ds
017540111206     c                   parm                    tisi95ds1
017550080812     c                   parm                    tisi97ds
017560030922
017570030922     C                   movel     §kpjbu        kpjbu
017580001010
017590001006     C                   eval      *inlr = *on
017600010115      **********************************************************************
017610010115      * MANUTENZIONE - COPIA
017620010115      **********************************************************************
017630010115     C     Sr_Aggiorna   begsr
017640060726
017650060726     c                   eval      wormconf = *off
017660091007     c                   eval      wormnoman = *off
017670100812     c                   eval      $NoApp = *off
017680130918     c                   eval      wNOTEL = *off
017690140508      /free
017700140512       //?Richiamo la routine delle abilitazioni ai tasti funzione
017710140512       //?mi serve in questo punto perchè nella routina uso gli indicatori
017720140512       //?che vengono accesi o spenti nella abifxx
017730140512         exsr Abi_fxx;
017740140508       //?Accendo indicatore per proteggere i campi alert su F04
017750160208       //?non lo faccio se sono in copia
017760160405       //?anche per F13
017770160208         IF  not *in03;
017780160208           *in12 = *on;
017790160311           *in20 = *on;
017800160208         ENDIF;
017810140508      /end-free
017820010115
017830010115     C     kfnor         chain(e)  fnorm01L
017840010117      * Allocato
017850010115     C                   if        %error
017860160516     c                   IF        §RMtla <> '0'
017870010115     C                   exsr      Sr_Allocato
017880160516     c                   ENDIF
017890010115     C                   eval      *in28 = *on
017900010522     C                   eval      *in33 = *on
017910010115     C                   goto      endsrmanut
017920010115     C                   endif
017930010117      * Non trovato
017940010117     C                   if        not%found(fnorm01l)
017950010117     C                   eval      *in28 = *on
017960010117     C                   goto      endsrmanut
017970010117     C                   endif
017980071030      * alloco anche fnorg
017990071030     c     kfnor         chain(e)  fnorg01L
018000071030      * Allocato
018010071030     c                   if        %error
018020160516     c                   IF        §RMtla <> '0'
018030071030     c                   exsr      Sr_Allocato
018040160516     c                   ENDIF
018050071030     c                   eval      *in28 = *on
018060071030     c                   eval      *in33 = *on
018070071030     c                   goto      endsrmanut
018080071030     c                   endif
018090151016      * Non trovato
018100071030     c                   if        not%found(fnorg01l)
018110151016     C                   eval      *in28 = *on
018120151016     C                   goto      endsrmanut
018130151016     C                   endif
018140140918      /free
018150140918       //?Imposto se ORM in fase 420
018160140918         IF  ORMfao = 420;
018170140918           OrmFase420 = *on;
018180140918         ELSE;
018190140918           OrmFase420 = *off;
018200140918         ENDIF;
018210140918      /end-free
018220130117
018230130117      * se ORM con serie cerco la tabella OSR
018240130117     c                   IF        ORmnsr > 0
018250130117     c                   exsr      sr_tabOSR
018260130117     c                   ENDIF
018270071030
018280060726      * se sono in conferma ORM commissionati e l'ORM non è più in fase 50 non devo confermarlo
018290060726      * esco dalla routine e non confermo l'ORM
018300060726     c                   if        §rmfpr = 'O' and ormfao <> 50
018310160516     c                   IF        §RMtla <> '0'
018320060726     c                   exsr      sr_ormconf
018330160516     c                   ENDIF
018340060726     c                   eval      *in28 = *on
018350060726     c                   eval      wormconf = *on
018360060726     c                   goto      endsrmanut
018370060726     c                   endif
018380091007      * controllo se la fase dell'ORM permette la manutenzione
018390091007     c                   clear                   dfar
018400091007     c                   clear                   tibs02ds
018410091007     c                   eval      t02mod = 'C'
018420091007     c                   eval      t02sif = knsif
018430091007     c                   eval      t02cod = 'FAR'
018440091007     c                   eval      t02ke1 = %editc(ormfao:'X')
018450091007     c                   call      'TIBS02R'
018460091007     c                   parm                    kpjba
018470091007     c                   parm                    tibs02ds
018480091007     c                   eval      dfar = t02uni
018490091007     c                   if        d§farman <> 'S' and *in02
018500091007     c                   eval      wormnoman = *on
018510091007     c                   eval      *in28 = *on
018520091007     c                   goto      endsrmanut
018530091007     c                   endif
018540081127      * se ORM Import DPD automatico non posso manutenzionare
018550081127      * gli unici campi possibili sono il mittente, la data e ora ritiro
018560081127      * quindi emetto un video diverso
018570081127     c                   if        %lookup(ormpoe:skpodpd) > *zeros and
018580081106     c                             ormtco = 'F'
018590081106     c                   eval      *in26 = *on
018600081106     c                   endif
018610010228      * Annullato
018620010228     C                   if        ORMatb <> *blanks
018630010228     C                   eval      *in02 = *off
018640010228     C                   eval      *in10 = *on
018650010228     C                   movel     mod(4)        v1mod
018660010228     C                   endif
018670010213      * Chiuso
018680030731     C                   if        ORMfao = 999 and OrmFao = 910
018690030731     C                                          and *in03 = *off
018700010222     C                   setoff                                       02
018710010213     C                   eval      *in10 = *on
018720010213     C                   movel     mod(5)        v1mod
018730010309     C                   endif
018740010309      * Cerco la causale
018750010305     C                   exsr      Sr_Contrcau
018760010115      * Se richiamato da gestione proposte carica i dati dalla ds esterna
018770010117     C                   if        §RMfpr = 'P'
018780010115     C                   exsr      Sr_Cards
018790070925      * e controllo se devo emettere prima una window per avvisare (nel caso di giro
018800070925      * immesso) se mittente e/o quantità modificate
018810070925     c                   exsr      sr_controrp
018820161108      /free
018830161108       //?Calcolo il peso/volume se no in caso di conferma proposta si azzerano
018840161108        exsr sr_pesvol;
018850161108      /end-free
018860010117     C                   goto      endsrmanut
018870010117     C                   endif
018880010117      * altrimenti carica i dati dal file
018890010115     C                   exsr      Sr_Carvid
018900010117      * e controlla se esistono proposte di variazione
018910010117     C                   exsr      Sr_Orp
018920010117      * Se copia ricerca l'ultimo viaggio dell'ORM
018930010115     C                   if        *in03 = *on
018940010115     C                   exsr      Sr_Copia
018950010117      * Controlla se ok la ricerca dell'ultimo viaggio
018960010115     C   28              exsr      Sr_ErrCopia
018970010115     C   28              goto      endsrmanut
018980010115     C                   endif
018990010124      * Controlla dati caricati se richiamto per conferma da altri p.o.
019000010124     C                   if        §RMfpr = 'O'
019010010124     C                   exsr      Sr_Contr
019020010124      * Richiesta la conferma totale dal pgm chiamante
019030010124     C                   if        §RMtla = '0'
019040010124      * se ci sono errori ritorna al pgm chiamante
019050010124     C   28              eval      §RMerr = '5'
019060010124     C   28              goto      endsrmanut
019070010124      * se non ci sono errori conferma orm
019080010124     C                   exsr      Sr_Conferma
019090010507     C   28              eval      §RMerr = '5'
019100010507     C   28              goto      endsrmanut
019110010124     C                   endif
019120010124      * Richiesta la conferma senza visualizzare l'orm
019130010124      * conferma solo se non ci sono errori, altrimenti visualizza l'orm con gli errori
019140010124     C                   if        §RMtla = '1' and *in28 = *off
019150010124     C                   exsr      Sr_Conferma
019160010124     C                   endif
019170100812      /free
019180140506       //?Se ORM con Alert e richiesta §RMtla = '2'
019190140506       //?vuol dire che la conferma automatica dell'orm è andata in errore
019200170206       //?e ora sto facendo la conferma manuale
019210170206       //?oppure mentre sto facendo le conferme arrivano degli ORM nuovi da confermare
019220170206       //?ma ormai la conferma automatica è già stata fatta quindi il pgm me li propone
019230140506         IF  §RMtla = '2' and walert;
019240140506         //?se ci sono errori dovuti ai controlli esco dalla routine ed emetto la videata ORM
019250140506           IF  *in28;
019260140508           //?spengo indicatore di errore su F04 potrebbero essere errati proprio quelli
019270140508             *in12 = *off;
019280140506             leavesr;
019290140506           ENDIF;
019300140506         //?se non ci sono errori procedo con la conferma dell'ORM in questo modo
019310140506         //?se l'errore è sulla generazione della bolla viene emessa solo la videta
019320140506         //?della bolla
019330140506           exsr sr_conferma;
019340140506           IF  *in28;
019350140506             §RMerr = '5';
019360140506             leavesr;
019370140506           ENDIF;
019380170206         //?se arrivo qua vuol dire che sono riuscita a confemare l'ORM
019390170206         //?quindi devo uscira dal FIOR05R per poi confermare un altro ORM
019400170206         //?per fare questo forzo §RMTLA =  '0'
019410170206           §RMtla = '0';
019420170206           leavesr;
019430140506         ENDIF;
019440140506
019450130918       //?Controllo se ORM ha già avuto un dirottamento
019460100812       //?mi posiziono qua tanto in conferma orm commissionati il richiamo è sempre
019470100812       //?§RMtla = '2'
019480100812       //?cerco la fase precedente con data ora della fase attuale meno un secondo e fase 200
019490100812       //?se la trovo vuol dire che è sto confermando un ORM appena dirottato
019500100812       //?la chiave della fase attuale già impsotata in routine contrcau
019510100812           chain(n)  (kpoe:knsr:knor:knrv:kdai:kori-1:200) FNORF01L;
019520100812           IF  %found(FNORF01L);
019530100812         //?msg che il mittente è già stato avvisato via MAIL non telefonare x appuntamento
019540100812             $NoApp = *on;
019550100812           ENDIF;
019560100812      /end-free
019570010124     C                   endif
019580010115
019590010115     C     endsrmanut    endsr
019600010115      **********************************************************************
019610010115      * IMMISSIONE
019620010115      **********************************************************************
019630010115     C     Sr_Immetti    begsr
019640010115
019650071108     c                   clear                   fnorm000
019660071108     c                   clear                   fnorg000
019670100225     c                   eval      $immetti = *off
019680071108
019690010117      * Carica i dati di default
019700010115     C                   exsr      Sr_Cardft
019710160406      /free
019720160406       //?Abilito F13-Conferma Prenotazione
019730160413       //?Se immissione ORM manuale o copia
019740160413         IF  (*in01 or *in03) and (V1Ctco = 'M' or V1Ctco = 'E');
019750160406           *in88 = *on;
019760160406         ENDIF;
019770160406      /end-free
019780010117      * Se non richiamato da procedura esterna esce dalla routine
019790010117     C                   if        §RMtla = *blanks
019800010117     C                   goto      endsrimm
019810010115     C                   endif
019820010117      * Carica i dati dalla ds esterna
019830010202     C                   exsr      Sr_Cards
019840170525      * Ricalcolo la data
019850170525     C                   exsr      Sr_Calcolaoggi
019860010117      * Controlla dati caricati
019870010117     C                   exsr      Sr_Contr
019880100223      /free
019890100223       //?Controlli per preavviso ORM in caso di conferma ORM da VAO
019900100223         $errpre = *off;
019910100223         IF  §rmfpr = 'C';
019920100225           $immetti = *on;
019930100223           exsr sr_preavviso;
019940100322           IF  $errpre;
019950100225         //?imposto i dati per la window di modifica dati preavviso
019960100225             IF  §ormpre = 'M';
019970100225               wpresino = 'SI';
019980100225             ELSE;
019990100225               wpresino = 'NO';
020000100225             ENDIF;
020010100225             wpremail = vaoedati;
020020100225           ENDIF;
020030100223         ENDIF;
020040100223      /end-free
020050100223
020060010117      * Richiesta la conferma totale dal pgm chiamante
020070010115     C                   if        §RMtla = '0'
020080010117      * se ci sono errori ritorna al pgm chiamante
020090100322     c                   if        *in28 or $errpre
020100100322     C                   eval      §RMerr = '5'
020110100322     C                   goto      endsrimm
020120100322     c                   endif
020130010117      * se non ci sono errori conferma orm
020140010117     C                   exsr      Sr_Conferma
020150100322     c                   if        *in28 or $errpre
020160100322     C                   eval      §RMerr = '5'
020170100322     C                   goto      endsrimm
020180100322     C                   endif
020190010115     C                   endif
020200010117      * Richiesta la conferma senza visualizzare l'orm
020210010117      * conferma solo se non ci sono errori, altrimenti visualizza l'orm con gli errori
020220100812     c                   IF        §RMtla = '1' and not *in28 and
020230100812     c                             not $errpre
020240010115     C                   exsr      Sr_Conferma
020250010115     C                   endif
020260010115
020270010115     C     endsrimm      endsr
020280130117
020290130121      /free
020300130121       //--------------------------------------------------------------
020310130121       //?Cerco tabella OSR per codice ordinante
020320130121       //--------------------------------------------------------------
020330130121       BEGSR sr_TABosr;
020340130117
020350130121         clear dOSR;
020360130121         wORMcor = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
020370130121       //?leggo tutta la tabella OSR
020380140422         kTBEcod = 'OSR';
020390130121         setll kTBEcod tntbe01l;
020400130121         reade kTBEcod tntbe01l;
020410130121
020420130121         DOW not %eof(tntbe01l);
020430130122           IF TBEatb = *blanks;
020440130121           dosr = tbeuni;
020450130121           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
020460130121              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
020470130121              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
020480130121              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
020490130121              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
020500130121              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
020510130121              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
020520130121              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
020530130121             leavesr;
020540130121           ENDIF;
020550130122           ENDIF;
020560130121           reade kTBEcod tntbe01l;
020570130121         ENDDO;
020580130121
020590130121       ENDSR;
020600130121      /end-free
020610130117
020620010208      **********************************************************************
020630010208      * CALCOLA LA DATA E L'ORA DEL GIORNO
020640010208      **********************************************************************
020650010208     C     Sr_Calcolaoggibegsr
020660010208
020670131211      /free
020680131211       //?Imposto oggi in formato aaaammgg
020690131211         oggi_aammgg = %dec(%date());
020700131211       //?Imposto oggi in formato ggmmaaaa
020710131211         dataISO = %date(oggi_aammgg:*iso);
020720131211         dataEUR = dataISO;
020730131211         oggi_ggmmaa = %dec(dataEUR);
020740160411       //?Imposto ora attuale
020750160411       //?se NON è conferma automatica da VAS imposto anche la data
020760160411         IF  not ConfAuto;
020770131211         V1oao = %dec(%time());
020780160411         V1dao = oggi_ggmmaa;
020790161026         inv_v1dao = oggi_aammgg;
020800160411         ENDIF;
020810131211      /end-free
020820010208
020830010208     C                   endsr
020840001027      **********************************************************************
020850001027      * VIDEATA PER ORM ALLOCATO AD ALTRO UTENTE
020860001027      **********************************************************************
020870001027     C     Sr_Allocato   begsr
020880001027
020890001027     C                   eval      v3cpoe = ppoe
020900001027     C                   eval      v3nsr  = pnsr
020910001027     C                   eval      v3nor  = pnor
020920001027     C                   eval      v3nrv  = pnrv
020930001027
020940001027     C                   exfmt     fior05v
020950001027
020960001027     C                   endsr
020970060726      *--------------------------------------------------------------------*
020980060726      * Videata per informare l'utente che l'ORM è già stato confermato
020990060726      *--------------------------------------------------------------------*
021000060726     c     sr_ormconf    begsr
021010060726
021020060726     c                   eval      v3cpoe = ppoe
021030060726     c                   eval      v3nsr  = pnsr
021040060726     c                   eval      v3nor  = pnor
021050060726     c                   eval      v3nrv  = pnrv
021060060726
021070060726     c                   exfmt     fior05va
021080060726
021090060726     c                   endsr
021100060726
021110001016      **********************************************************************
021120001023      * CARICA DATI DI DEFAULT NELLA VIDEATA
021130001016      **********************************************************************
021140001016     C     Sr_Cardft     begsr
021150131211
021160010124      * Pulisco il video
021170071108     C                   clear                   vidnew
021180131018     C                   clear                   vidnew2
021190131018     C                   clear                   vidnewA
021200160407     C                   clear                   vidnewC
021210010205      * Pulisco i campi di solo output e hidden
021220010205     C                   clear                   v1fao
021230010205     C                   clear                   v1dfao
021240010205     C                   clear                   v1dfo
021250010205     C                   clear                   v1ofo
021260010205     C                   clear                   v1cpoe
021270010205     C                   clear                   v1nsr
021280010205     C                   clear                   v1nor
021290010205     C                   clear                   v1nrv
021300010205     C                   clear                   v1ctco
021310010205     C                   clear                   v1dtco
021320010205     C                   clear                   v1cpos
021330010205     C                   clear                   v1ors
021340010205     C                   clear                   v1dsto
021350010205     C                   clear                   v1dpor
021360070913     c                   clear                   v1dcgi
021370010205     C                   clear                   v1dtor
021380010205     C                   clear                   v1dpoc
021390010205     C                   clear                   v1dst
021400010205     C                   clear                   v1cst
021410010205     C                   clear                   v1vcs
021420010205     C                   clear                   v1fcs
021430010205     C                   clear                   v1npg
021440010205     C                   clear                   v1ndc
021450010205     C                   clear                   v1ddc
021460010205     C                   clear                   v1stp
021470010205     C                   clear                   v1tap
021480010205     C                   clear                   v1eti
021490070913     c                   clear                   v1tgi
021500081215     c                   clear                   v1ntwd
021510161024     C                   clear                   v1dar
021520010305      * Pulisco i campi di salvataggio
021530010305     C                   clear                   sav_acrpoa
021540131022     C                   clear                   sav_acrorr
021550010305     C                   clear                   sav_acrfcl
021560010305     C                   clear                   sav_acrfpk
021570010305     C                   clear                   sav_acrfmc
021580010305     C                   clear                   sav_acrfbn
021590010305     C                   clear                   sav_acrfbl
021600010305     C                   clear                   sav_acrfat
021610010305     C                   clear                   sav_acrfmt
021620010305     C                   clear                   sav_v1cpor
021630010305     C                   clear                   sav_v1ccra
021640131024     c                   clear                   sav_dsV1Ccra
021650010305     C                   clear                   sav_v1car
021660010305     C                   clear                   sav_v1lor
021670010305     C                   clear                   sav_v1prr
021680010305     C                   clear                   sav_v1ccor
021690170208     c                   clear                   sav_dsV1Ccor
021700080910     C                   clear                   sav_v1cao
021710010305     C                   clear                   sav_v1loo
021720030922     C                   clear                   sav_v1loo35
021730010305     C                   clear                   sav_v1pro
021740010305     C                   clear                   sav_v1ccrc
021750131107     C                   clear                   sav_v1rsc
021760080910     C                   clear                   sav_v1cac
021770010305     C                   clear                   sav_v1loc
021780030922     C                   clear                   sav_v1loc35
021790010305     C                   clear                   sav_v1prc
021800131202     C                   clear                   sav_v1nac
021810010906     C                   clear                   sav_v1pag
021820010507     C                   clear                   sav_v1ksc
021830030606     c                   Clear                   AcrKscCor
021840050202     c                   Clear                   AcrCccCor
021850050322     c                   Clear                   sav_v1com
021860050323     c                   Clear                   old_newv1dar
021870070920     c                   clear                   sav_acrcgi
021880070913     c                   clear                   sav_v1ccgi
021890071031     c                   clear                   savpkg
021900071031     c                   clear                   savvlm
021910071031     c                   clear                   savbnc
021920131022     c                   clear                   v1anticipa
021930161028     c                   clear                   sav_v1dpm
021940161103     c                   clear                   inv_v1dpm
021950160310
021960160310     c                   clear                   §OREfsco
021970160310     c                   clear                   §OREfmco
021980140109
021990140422     c                   clear                   m_OG143
022000140109     c                   clear                   okbolla
022010140109     c                   eval      wintcap = *off
022020140422     c                   eval      walert = *off
022030140423     c                   eval      werralert = *off
022040160406     c                   eval      wErrConferma = *off
022050140515     c                   eval      wOKalert = *off
022060140711     c                   eval      wOKF04 = *off
022070160406     c                   eval      wOKF13 = *off
022080160310     c                   eval      wconferma = *off
022090131022
022100010305      * Imposto l'ultimo orm creato
022110010305     C                   eval      v1poeo = §rmpoeo
022120010305     C                   eval      v1nsro = §rmnsro
022130010305     C                   eval      v1noro = §rmnoro
022140010305     C                   eval      v1nrvo = §rmnrvo
022150010123
022160010201      * Imposto la data e ora chiamata/immissione
022170010117     C                   eval      v1dao = oggi_ggmmaa
022180131212     C                   clear                   wlbdat
022190131212      * giro la data
022200131212     C                   z-add     v1dao         G02dat
022210131212     C                   movel     *blanks       G02err
022220131212     C                   call      'XSRDA8'
022230131212     C                   parm                    wlbdat
022240131212     C                   z-add     G02inv        inv_v1dao
022250131211     C                   eval      V1oao = %dec(%time())
022260010201      * Pulisco i dati dei codici
022270010116     C                   clear                   ds_v1ccra
022280010116     C                   eval      v1cra1 = ds_v1cra1
022290010116     C                   eval      v1cra2 = ds_v1cra2
022300010116     C                   eval      v1cra3 = ds_v1cra3
022310010116     C                   clear                   ds_v1ccor
022320010116     C                   eval      v1cor1 = ds_v1cor1
022330010116     C                   eval      v1cor2 = ds_v1cor2
022340010116     C                   eval      v1cor3 = ds_v1cor3
022350010116     C                   clear                   ds_v1ccrc
022360010116     C                   eval      v1crc1 = ds_v1crc1
022370010116     C                   eval      v1crc2 = ds_v1crc2
022380010116     C                   eval      v1crc3 = ds_v1crc3
022390010205      * Imposto alcuni dati di DFT solo se non provengo da procedura esterna
022400010205     C                   if        §RMtla = *blanks
022410010205      * Imposta il tipo orm da DFT
022420010205     C                   eval      v1ctor = d§dfttor
022430010205     C                   exsr      Sr_Contrtor
022440010205      * Imposta il tipo comunicazione orm da DFT
022450010205     C                   eval      v1ctco = d§dfttco
022460141212      * se richiamato per immissione orm MAIL imposto 'E'
022470141212     c                   IF        psce = 'E'
022480141212     c                   eval      v1ctco = 'E'
022490141211     c                   ENDIF
022500010205     C                   exsr      Sr_Contrtco
022510010205      * Imposta il chi paga da DFT
022520010205     C                   eval      v1pag = d§dftpag
022530021119      * Imposta il flag ddt da DFT
022540021119     C                   eval      v1ddt = d§dftddt
022550010205     C                   endif
022560010201
022570010201     C                   eval      *in41 = *on                                  pos.curs.cli.ritiro
022580131024
022590131024     c                   eval      *in95 = *off
022600150923      /free
022610150923       //?Imposto la filiale emissione
022620150923       V1Cpoe = ppoe;
022630150923       V1poei = ppoe;
022640150923       clear V1poeid;
022650150923       chain (V1poei) AZORG01L;
022660150923       IF  %found(AZORG01L);
022670150923         V1poeid = ORGdes;
022680150923       ENDIF;
022690161103       //?Imposto Data e ora Pronta merce di dft in immissione/copia ORM su AS
022700161103       IF  (*in01 or *in03) and §RMtla = *blanks;
022710161103         V1dpm = V1dao;
022720161103         sav_V1dpm = V1dpm;
022730161103         inv_V1dpm = inv_V1dao;
022740161103         V1orr = V1oao/100;
022750161103       ENDIF;
022760150923      /end-free
022770150923
022780001016
022790001016     C                   endsr
022800001009      **********************************************************************
022810001023      * CARICA DATI NELLA VIDEATA DAL FILE
022820001009      **********************************************************************
022830001009     C     Sr_Carvid     begsr
022840131024
022850131024     c                   eval      wokestensione = *off
022860001009
022870010117     C                   eval      v1atb  = ORMatb
022880010117     C                   eval      v1ctco = ORMtco
022890010117     C                   eval      v1ctor = ORMtor
022900001023      * Inversione data emissione ORM
022910001016     C                   clear                   wlbdat
022920001017     C                   z-add     ORMdao        G02inv
022930001017     C                   movel     '3'           G02err
022940001016     C                   call      'XSRDA8'
022950001016     C                   parm                    wlbdat
022960001025     C                   z-add     G02dat        v1dao
022970130919     C                   z-add     ORMdao        inv_v1dao
022980001215      * Ora emissione ORM
022990131211     C                   eval      v1oao = ORMoao
023000001016      * Dati Ordinante
023010010605     C                   eval      sav_v1ccor = ORMcor
023020010605     C                   eval      ds_v1ccor = ORMcor
023030170208     C                   eval      sav_dsv1ccor = ORMcor
023040010116     C                   eval      v1cor1 = ds_v1cor1
023050010116     C                   eval      v1cor2 = ds_v1cor2
023060010116     C                   eval      v1cor3 = ds_v1cor3
023070010116     C                   eval      *in08 = (ORMcor <> *zeros)
023080010117     C                   eval      v1rso = ORMrso
023090010117     C                   eval      v1ino = ORMino
023100010117     C                   eval      v1cao = ORMcao
023110080910     C                   eval      sav_v1cao = ORMcao
023120001025     C                   movel     ORMloo        v1loo
023130010118     C                   movel     ORMloo        sav_v1loo
023140030922     C                   movel     ORMloo        sav_v1loo35
023150010117     C                   eval      v1pro = ORMpro
023160010118     C                   eval      sav_v1pro = ORMpro
023170010117     C                   eval      v1nao = ORMnao
023180010702      * Dati Mittente
023190010117     C                   eval      sav_v1ccra = ORMcra
023200010117     C                   eval      ds_v1ccra = ORMcra
023210161028     C                   eval      sav_dsv1ccra = ORMcra
023220010116     C                   eval      v1cra1 = ds_v1cra1
023230010116     C                   eval      v1cra2 = ds_v1cra2
023240010116     C                   eval      v1cra3 = ds_v1cra3
023250010116     C                   eval      *in07 = (ORMcra <> *zeros)
023260010117     C                   eval      v1rsr = ORMrsr
023270010117     C                   eval      v1inr = ORMinr
023280010117     C                   eval      v1car = ORMcar
023290010118     C                   eval      sav_v1car = ORMcar
023300001025     C                   movel     ORMlor        v1lor
023310010118     C                   movel     ORMlor        sav_v1lor
023320010117     C                   eval      v1prr = ORMprr
023330010118     C                   eval      sav_v1prr = ORMprr
023340010117     C                   eval      v1nar = ORMnar
023350010117     C                   eval      v1rer = ORMrer
023360010117     C                   eval      v1ter = ORMter
023370010702      * Dati Destinatario
023380010605     C                   eval      sav_v1ccrc = ORMcrc
023390010605     C                   eval      ds_v1ccrc = ORMcrc
023400010116     C                   eval      v1crc1 = ds_v1crc1
023410010116     C                   eval      v1crc2 = ds_v1crc2
023420010116     C                   eval      v1crc3 = ds_v1crc3
023430010116     C                   eval      *in06 = (ORMcrc <> *zeros)
023440010117     C                   eval      v1rsc = ORMrsc
023450131107     c                   eval      sav_V1rsc = V1rsc
023460010117     C                   eval      v1inc = ORMinc
023470010219     C                   movel     ORMloc        v1loc
023480010208     C                   movel     ORMloc        sav_v1loc
023490030922     C                   movel     ORMloc        sav_v1loc35
023500010117     C                   eval      v1cac = ORMcac
023510080910     C                   eval      sav_v1cac = ORMcac
023520010117     C                   eval      v1prc = ORMprc
023530010118     C                   eval      sav_v1prc = ORMprc
023540010117     C                   eval      v1nac = ORMnac
023550131202     c                   eval      sav_v1nac = ORMnac
023560001016
023570010117     C                   eval      v1ffd = ORMffd
023580010117     C                   eval      v1nam = ORMnam
023590001023      * Inversione data ritiro
023600161110     C                   clear                   wlbdat
023610040402     C                   z-add     ORMdar        inv_v1dar
023620001017     C                   z-add     ORMdar        G02inv
023630001017     C                   movel     '3'           G02err
023640001016     C                   call      'XSRDA8'
023650001016     C                   parm                    wlbdat
023660001025     C                   z-add     G02dat        v1dar
023670001016
023680010117     C                   eval      v1orr = ORMorr
023690010117     C                   eval      v1ncl = ORMncl
023700010117     C                   eval      v1pkg = ORMpkg
023710071031     c                   eval      savpkg = ormpkg
023720161115      * mi devo salvare anche il campo ORGpkg per visualizzazione orari servizio con F14
023730161115     c                   eval      wpkg = ORGpkg
023740170605     c                   eval      old_wpkg = ORGpkg
023750010117     C                   eval      v1vlm = ORMvlm
023760161024     c                   eval      savvlm = ormvlm
023770010117     C                   eval      v1bnc = ORMbnc
023780071031     c                   eval      savbnc = ormbnc
023790010117     C                   eval      v1blc = ORMblc
023800010117     C                   eval      v1att = ORMatt
023810010117     C                   eval      v1mtc = ORMmtc
023820010702      * Chi paga
023830010117     C                   eval      v1pag = ORMpag
023840010308     C                   eval      sav_v1pag = ORMpag
023850010117     C                   eval      v1ksc = ORMksc
023860010507     C                   eval      sav_v1ksc = ORMksc
023870010702
023880010202     C                   eval      v1ctr = ORMctr
023890040630
023900010117     C                   eval      v1ddt = ORMddt
023910010202
023920001016     C                   if        ORMpor <> *zeros
023930001016     C                   movel     ORMpor        v1cpor
023940010116     C                   movel     ORMpor        sav_v1cpor
023950001016     C                   endif
023960070913
023970001016     C                   if        ORMpoc <> *zeros
023980001016     C                   movel     ORMpoc        v1cpoc
023990001011     C                   endif
024000010117     C                   eval      v1dst = ORMdst
024010010117     C                   eval      v1not1 = ORMno1
024020010117     C                   eval      v1not2 = ORMno2
024030010117     C                   eval      v1fao = ORMfao
024040010117     C                   movel     ORMpos        v1cpos
024050010117     C                   eval      v1ors = ORMors
024060010117     C                   eval      v1rfa = ORMrfa
024070001221     C                   move      ORMsto        v1sto
024080010119     C                   eval      v1spi = ORMspi
024090010115      * Campi hidden
024100010117     C                   eval      v1cst = ORMcst
024110010117     C                   eval      v1vcs = ORMvcs
024120010117     C                   eval      v1fcs = ORMfcs
024130010117     C                   eval      v1dfo = ORMdfo
024140010117     C                   eval      v1ofo = ORMofo
024150010117     C                   eval      v1npg = ORMnpg
024160010117     C                   eval      v1ndc = ORMndc
024170010117     C                   eval      v1ddc = ORMddc
024180010117     C                   eval      v1stp = ORMstp
024190010117     C                   eval      v1tap = ORMtap
024200010117     C                   eval      v1eti = ORMeti
024210011113     C                   eval      dorm01 = ORMflo
024220071107
024230071107      * imposto codice giro presente su FNORG
024240070920     c                   eval      v1tgi = orgtgi
024250070920     c                   eval      v1ccgi = orgcgi
024260070920     c                   eval      sav_v1ccgi = orgcgi
024270071106     c                   eval      sav_acrcgi = orgcgi
024280071106
024290071106      * imposto anche il volume o il peso già memorizzati su ORG per confronti sucessivi
024300071106     c                   eval      savvlm = orgvlm
024310071106     c                   eval      savpkg = orgpkg
024320070920
024330001025      * Protegge P.O.ritiro
024340050321      * SEMPRE!!!!! (in manutenzione)
024350050321      * e se orm prepagato da bolla  (tipo comunicazione)
024360050321     c                   Eval      *In09 =  *In02
024370011012     C                             or      v1ctco = 'P'
024380001027      * Protegge riferimento alfanumerico
024390050422      * se tipo comunicazione ORM da File o Internet
024400050323     C                   eval      *in78 = (ORMtco ='F'
024410011012     C                                   or ORMtco = 'I')
024420010124      * Se richiamato da gestione conferma orm da altri p.o. metto già la fase a 100
024430010124     C                   if        §RMfpr = 'O'
024440010124     C                   eval      v1fao = 100
024450010124     C                   eval      v1dfo = oggi_aammgg
024460050422     c                   time                    w0140
024470010124     C                   movel     w0140         v1ofo
024480010124     C                   endif
024490050321      * Orm commissionato
024500050322      * sempre protetto in manutenzione
024510030606     c                   Eval      V1com = §orCom
024520130918     c                   Eval      sav_V1com = §orCom
024530050420     c                   If        *In02
024540050322     c                   Eval      *In34 = *On
024550050420     c                   EndIf
024560060117
024570060117      * se il tipo ORM è 'P' prepagato proteggo il campo di tipo ORM in modo da non poterlo
024580060117      * + variare
024590060117     c                   Eval      *In05 = (v1ctor = 'P')
024600131018
024610131018      /free
024620131018       //?Recupero dati da estensione FNORE
024630131018       //?Rcd 'O ' --> Orari apertura
024640131018         k_OREpoe = ORMpoe;
024650131018         k_OREnsr = ORMnsr;
024660131018         k_OREnor = ORMnor;
024670131018         k_OREnrv = ORMnrv;
024680131018         k_OREtrc = 'O ';
024690131018         clear dOREorari;
024700131018       //?Aggancio il rcd
024710131029         chain(n)  %kds(k05fnore) FNORE01L;
024720131018         IF  %found(FNORE01L);
024730131018           dOREorari = OREdati;
024740131024           wokestensione = *on;
024750131018         ENDIF;
024760131018         v1OraAMda = §OREoramda;
024770131018         v1OraAMa  = §OREorama;
024780131018         v1OraPMda = §OREorapda;
024790131018         v1OraPMa  = §OREorapa;
024800161020       //?Rcd 'DT' --> Data Pronta Merce
024810170524       //?         --> Data Ritiro Calcolata e Anticipo
024820161020         k_OREtrc = 'DT';
024830161027         clear dOREdt;
024840161020         chain(n)  %kds(k05fnore) FNORE01L;
024850161020         IF  %found(FNORE01L);
024860161027           dOREdt = OREdati;
024870161020           wokestensione = *on;
024880161020         ENDIF;
024890161027         IF  §OREdpm > *zeros;
024900161027           inv_V1dpm = §OREdpm;
024910161110           clear wlbdat;
024920161020           G02inv = inv_V1dpm;
024930161020           G02err = '3';
024940161020           xsrda8 (wlbdat);
024950161020           V1dpm = G02dat;
024960161028           sav_V1dpm = V1dpm;
024970161020         ENDIF;
024980170524         IF  §OREdar > *zeros and §OREdar <> *blanks;
024990170524           DarCalcolata = %int(§OREdar);
025000170524           wOkPosticipa = (§OREposd = 'S');
025010170524           wAnticipa = (§OREant = 'S');
025020170524           Anticipato = (§OREmod = 'S');
025030170524           IF  §OREgga > *zeros and §OREgga <> *blanks;
025040170524             ggAnticipo = %int(§OREgga);
025050170524           ENDIF;
025060170524         ENDIF;
025070140429
025080140429       //?Controllo da rcd GEN se ORM con Alert
025090160405       //?e/o conferma prenotazione ritiro
025100140429         walert = *off;
025110160406         *in88 = *off;
025120140429         clear dOREgen;
025130140429         k_OREtrc = 'G ';
025140140429         clear dOREgen;
025150140429         chain(n) %kds(K05fnore) FNORE01L;
025160140429         IF  %found(FNORE01L);
025170140429           dOREgen = OREdati;
025180140429         ENDIF;
025190140429         IF  §OREfiso = 'S' or §OREfimo = 'S';
025200140429           walert = *on;
025210140429         ENDIF;
025220160405         IF  §OREfmco = 'S' or §OREfsco = 'S';
025230160406           *in88 = *on;
025240160405         ENDIF;
025250140429
025260160316       //?Carico i dati degli alert per ORM COMMISSIONATO
025270160513       //?se è stato inviato l'alert o se sono in conferma ORM commissionati
025280160513         IF  walert or §RMfpr = 'O';
025290140429         //?Rcd 'S ' --> SMS per invio alert
025300140429           k_OREtrc = 'S ';
025310140505           clear dOREsms;
025320140505           clear w03sms;
025330140429         //?Aggancio il rcd
025340140429           chain(n)  %kds(k05fnore) FNORE01L;
025350140429           IF  %found(FNORE01L);
025360140505             dOREsms = OREdati;
025370140429           ENDIF;
025380140505           IF  §OREsms <> *blanks;
025390140505             w03sms = §OREsms;
025400140505           ENDIF;
025410140429         //?Rcd 'MA' --> Mail per invio alert
025420140429           k_OREtrc = 'MA';
025430140429         //?Aggancio il rcd
025440140429           chain(n)  %kds(k05fnore) FNORE01L;
025450140429           IF  %found(FNORE01L);
025460140429             w03mail = OREdati;
025470140429           ENDIF;
025480160513         ENDIF;
025490160406
025500160406       //?Carico i dati relativi alla CONFERMA PRENOTAZIONE
025510160513       //?solo se l'alert è stato inviato
025520160513         IF  *in88;
025530160406         //?Rcd 'SC' --> SMS
025540160406         k_OREtrc = 'SC';
025550160406         clear w04sms;
025560160406         //?Aggancio il rcd
025570160406         chain(n)  %kds(k05fnore) FNORE01L;
025580160406         IF  %found(FNORE01L);
025590160406           w04sms = OREdati;
025600160406         ENDIF;
025610160406         //?Rcd 'MC' --> Mail
025620160406         k_OREtrc = 'MC';
025630160406         clear w04mail;
025640160406         //?Aggancio il rcd
025650160406         chain(n)  %kds(k05fnore) FNORE01L;
025660160406         IF  %found(FNORE01L);
025670160406           w04mail = OREdati;
025680160406         ENDIF;
025690160406       //?Se non ci sono disattivo F13
025700160406         IF  W04sms = *blanks and W04mail = *blanks;
025710160406           *in88 = *off;
025720160406         ENDIF;
025730160513         ENDIF;
025740160224
025750160406       //?Carico NPR
025760160224         k_OREtrc = 'NP';
025770160224         clear OREdati;
025780160224       //?Aggancio il rcd
025790160224         chain  %kds(k05fnore) FNORE01L;
025800160224         IF  %found(FNORE01L);
025810160310           V1npr = OREdati;
025820160224         ENDIF;
025830140122
025840131018      /end-free
025850170524
025860170524     C                   exsr      Sr_Decodifica
025870170524
025880170524      * se filiale ritiro non in gestione all'utente proteggo il giro
025890170524     c                   IF        %lookup(%editc(num_v1cpor:'X'):skpog) = 0
025900170524     c                   eval      *in24 = *on
025910170524     c                   ENDIF
025920170524
025930170524      * se ORM in fase 400 proteggo sempre il giro o 410
025940170524     c                   if        v1fao = 400 or v1fao = 410
025950170524     c                   eval      *in24 = *on
025960170524     c                   endif
025970170524
025980170524     c                   clear                   v1ntwd
025990001009
026000001009     C                   endsr
026010001023      **********************************************************************
026020001023      * CARICA DATI DALLA DS
026030001023      **********************************************************************
026040001023     C     Sr_Cards      begsr
026050001106
026060010118     C                   eval      v1cpoe = §RMpoe
026070010118     C                   eval      v1ctco = §RMtco
026080010201     C                   if        v1ctco = *blanks
026090010201     C                   eval      v1ctco = 'F'
026100010201     C                   endif
026110010118     C                   eval      v1ctor = §RMtor
026120100217
026130100218      /free
026140100218       //?Mi salvo i dati necessari per agganciare l'estensione del vao
026150100218       IF  §RMFPR = 'C';
026160100218         sav_vaocor = §rmcor;
026170100218         sav_vaopoe = §rmpoe;
026180100218         sav_vaonsr = §rmnsr;
026190100218         sav_vaonor = §rmnor;
026200100218         sav_vaonrv = §rmnrv;
026210100218       ENDIF;
026220100218      /end-free
026230100217
026240001023      * Dati Ordinante
026250010726      * se conferma ORM da ritiri fissi salvare i dati del cliente ordinante
026260050322      * anche per conferma ORM da IT/FILE
026270061011      * o se richiamato per proposta chiusura ORM
026280010726     C                   if        §RMtco = 'S'
026290010903     C                             or §RMfpr = 'C'
026300061011     c                             or §rmtla = 'C'
026310010726     C                   eval      sav_v1ccor = §RMcor
026320010726     C                   endif
026330010605     C                   eval      ds_v1ccor = §RMcor
026340010116     C                   eval      v1cor1 = ds_v1cor1
026350010116     C                   eval      v1cor2 = ds_v1cor2
026360010116     C                   eval      v1cor3 = ds_v1cor3
026370010116     C                   eval      *in08 = (§RMcor <> *zeros)
026380010118     C                   eval      v1rso = §RMRSO
026390010118     C                   eval      v1ino = §RMINO
026400010118     C                   eval      v1cao = §RMCAO
026410080910     C                   eval      sav_v1cao = §RMCAO
026420010118     C                   movel     §RMloo        v1loo
026430010118     C                   movel     §RMloo        sav_v1loo
026440030922     C                   movel     §RMloo        sav_v1loo35
026450010118     C                   eval      v1pro = §RMPRO
026460010118     C                   eval      sav_v1pro = §RMpro
026470010118     C                   eval      v1nao = §RMNAO
026480010702      * Dati Mittente
026490010726      * se conferma ORM da ritiri fissi salvare i dati del cliente mittente
026500010903      * anche per cnferma ORM da IT/CALL CENTER/FILE
026510010726     C                   if        §RMtco = 'S'
026520010903     C                             or §RMfpr = 'C'
026530040914      * o se conferma proposte
026540040914     c                             or §rmfpr = 'P'
026550010726     C                   eval      sav_v1ccra = §RMcra
026560140414      /free
026570140414       //?Se conferma ORM da VAO (orm internet) e cliente mittente codificato
026580140414       //?salvo ORA pronta merce
026590150921       //?e filiale ritiro forzata
026600140414         IF  §RMfpr = 'C' and §RMtco = 'I' and §RMcra > 0;
026610151109           chain(n) (§RMcra) FNACR01L;
026620140414           IF  %found(FNACR01L);
026630140414             sav_ACRorr = ACRorr;
026640150921             sav_ACRpoa = ACRpoa;
026650140414           ENDIF;
026660140414         ENDIF;
026670140414      /end-free
026680010726     C                   endif
026690010118     C                   eval      ds_v1ccra = §RMcra
026700010116     C                   eval      v1cra1 = ds_v1cra1
026710010116     C                   eval      v1cra2 = ds_v1cra2
026720010116     C                   eval      v1cra3 = ds_v1cra3
026730010116     C                   eval      *in07 = (§RMcra <> *zeros)
026740010118     C                   eval      v1rsr = §RMRSR
026750010118     C                   eval      v1inr = §RMINR
026760010118     C                   eval      v1car = §RMCAR
026770010118     C                   eval      sav_v1car = §RMcar
026780001023     C                   movel     §RMLOR        v1lor
026790010118     C                   movel     §RMlor        sav_v1lor
026800010118     C                   eval      v1prr = §RMPRR
026810010118     C                   eval      sav_v1prr = §RMprr
026820010118     C                   eval      v1nar = §RMNAR
026830010118     C                   eval      v1rer = §RMrer
026840010118     C                   eval      v1ter = §RMter
026850010117      * Dati Destinatario
026860010726      * se conferma ORM da ritiri fissi salvare i dati del cliente destinatario
026870050322      * anche per conferma ORM da IT/FILE
026880010726     C                   if        §RMtco = 'S'
026890010903     C                             or §RMfpr = 'C'
026900010726     C                   eval      sav_v1ccrc = §RMcrc
026910131107     C                   eval      sav_V1rsc = V1rsc
026920010726     C                   endif
026930010605     C                   eval      ds_v1ccrc = §RMcrc
026940010116     C                   eval      v1crc1 = ds_v1crc1
026950010116     C                   eval      v1crc2 = ds_v1crc2
026960010116     C                   eval      v1crc3 = ds_v1crc3
026970010116     C                   eval      *in06 = (§RMcrc <> *zeros)
026980010118     C                   eval      v1rsc = §RMrsc
026990010118     C                   eval      v1inc = §RMinc
027000010208     C                   movel     §RMloc        v1loc
027010010208     C                   movel     §RMloc        sav_v1loc
027020030922     C                   movel     §RMloc        sav_v1loc35
027030010118     C                   eval      v1cac = §RMcac
027040080910     C                   eval      sav_v1cac = §RMcac
027050010118     C                   eval      v1prc = §RMprc
027060010118     C                   eval      sav_v1prc = §RMprc
027070010118     C                   eval      v1nac = §RMnac
027080131202     c                   eval      sav_v1nac = §RMnac
027090010118     C                   eval      v1ffd = §RMffd
027100010118     C                   eval      v1nam = §RMnam
027110001023      * Inversione data ritiro
027120010117     C                   if        §rmdar > *zeros
027130140529     C                   z-add     §RMdar        old_invv1dar
027140161110     c                   clear                   wlbdat
027150001023     C                   z-add     §RMdar        G02inv
027160001023     C                   movel     '3'           G02err
027170001023     C                   call      'XSRDA8'
027180001023     C                   parm                    wlbdat
027190160830     c                   IF        G02err = '1'
027200160830     c                   z-add     *zeros        v1dar
027210160830     c                   ELSE
027220001025     C                   z-add     G02dat        v1dar
027230160830     c                   ENDIF
027240010202     C                   else
027250010202     C                   z-add     *zeros        v1dar
027260010117     C                   endif
027270160307      /free
027280160307         //?è capitato che da File arrivasse una data non corretta
027290160307         //?se ho anno o mese o giorno a 0 pulisco la data
027300160307           IF  V1dar > 0;
027310160307             IF  %subst(%editc(V1dar:'X'):1:2) = '00' or
027320160307                 %subst(%editc(V1dar:'X'):3:2) = '00' or
027330160307                 %subst(%editc(V1dar:'X'):5:4) = '0000';
027340160307               clear v1dar;
027350160307             ENDIF;
027360160307           ENDIF;
027370160307      /end-free
027380001023
027390010118     C                   eval      v1orr = §RMorr
027400131022      * se conferma ORM fissi salvo ORA pronta merce
027410131022     c                   IF        §RMtco = 'S'
027420131022     c                   eval      sav_ACRorr = §RMorr
027430131022     c                   ENDIF
027440010118     C                   eval      v1ncl = §RMncl
027450010118     C                   eval      v1pkg = §RMpkg
027460071031     c                   eval      savpkg = §rmpkg
027470010118     C                   eval      v1vlm = §RMvlm
027480071031     c                   eval      savvlm = §rmvlm
027490010118     C                   eval      v1bnc = §RMbnc
027500071031     c                   eval      savbnc = §rmbnc
027510010118     C                   eval      v1blc = §RMblc
027520010118     C                   eval      v1att = §RMatt
027530010118     C                   eval      v1mtc = §RMmtc
027540010702      * Chi paga
027550010118     C                   eval      v1pag = §RMpag
027560010308     C                   eval      sav_v1pag = §RMpag
027570010118     C                   eval      v1ksc = §RMksc
027580010507     C                   eval      sav_v1ksc = §RMksc
027590010514     C                   eval      v1ctr = §RMctr
027600010702     C                   eval      v1ddt = §RMddt
027610140109      * Imposta il flag ddt da DFT
027620140109     c                   IF        v1ddt = *blanks
027630140109     c                   eval      v1ddt = d§dftddt
027640140109     c                   ENDIF
027650001023     C                   if        §RMpor <> *zeros
027660001023     C                   movel     §RMpor        v1cpor
027670010116     C                   movel     §RMpor        sav_v1cpor
027680001023     C                   endif
027690001023     C                   if        §RMpoc <> *zeros
027700001023     C                   movel     §RMpoc        v1cpoc
027710001023     C                   endif
027720010118     C                   eval      v1not1 = §RMno1
027730010118     C                   eval      v1not2 = §RMno2
027740010118     C                   eval      v1cpos = §RMpos
027750010118     C                   eval      v1ors = §RMors
027760010118     C                   eval      v1rfa = §RMrfa
027770001221     C                   move      §RMsto        v1sto
027780010119     C                   eval      v1spi = §RMspi
027790010115      * Campi hidden
027800010118     C                   eval      v1cst = §RMcst
027810010118     C                   eval      v1vcs = §RMvcs
027820010118     C                   eval      v1fcs = §RMfcs
027830010118     C                   eval      v1tap = §RMtap
027840010118     C                   eval      v1eti = §RMeti
027850011113     C                   eval      dorm01 = §rmflo
027860010117     C                   clear                   v1dst
027870010117     C                   clear                   v1npg
027880010117     C                   clear                   v1ndc
027890010117     C                   clear                   v1ddc
027900010117     C                   clear                   v1stp
027910081215     c                   clear                   v1ntwd
027920001025
027930010117      * Dati da ORM se richiamato da gestione proposte
027940001106     C                   if        §RMfpr = 'P'
027950001106      * Inversione data assegnazione ORM
027960161110     c                   clear                   wlbdat
027970001106     C                   z-add     ORMdao        G02inv
027980001106     C                   movel     '3'           G02err
027990001106     C                   call      'XSRDA8'
028000001106     C                   parm                    wlbdat
028010001106     C                   z-add     G02dat        v1dao
028020131212     C                   z-add     ORMdao        inv_v1dao
028030010117
028040131211     C                   eval      v1oao = ORMoao
028050010118     C                   eval      v1ctco = ORMtco
028060010118     C                   eval      v1fao = ORMfao
028070010118     C                   eval      v1dfo = ORMdfo
028080010118     C                   eval      v1ofo = ORMofo
028090010118     C                   eval      v1cpos = ORMpos
028100010118     C                   eval      v1ors = ORMors
028110010118     C                   eval      v1dst = ORMdst
028120010118     C                   eval      v1npg = ORMnpg
028130010118     C                   eval      v1ndc = ORMndc
028140010118     C                   eval      v1ddc = ORMddc
028150010118     C                   eval      v1stp = ORMstp
028160050422      * Se è una proposta di chiusura
028170010117     C                   if        §RMtla = 'C'
028180070228      * salvo la fase anche per aggiornare FIPDO
028190070228     c                   eval      savorffar = orffar
028200070228      * e salvo anche la distinta per passarla al pgm che aggiorna FIPDO
028210070228     c                   eval      savorffgs = orffgs
028220070228     c                   eval      savorfndc = orfndc
028230070228     c                   eval      savorfddc = orfddc
028240050422      * imposto i dati per la fase
028250010117     C                   z-add     oggi_aammgg   v1dfo
028260050422     c                   time                    w0140
028270010117     C                   movel     w0140         v1ofo
028280010117     C                   eval      *in10 = *on
028290010117     C                   movel     mod(5)        v1mod
028300010209     C                   eval      v3cau = §RMcau
028310030801      * metto la causale anche in prima videata
028320030801     C                   eval      v1cau = §RMcau
028330030801     C                   eval      *in13 = (v1cau <> *blanks)
028340050422      * imposto la fase che c'è sulla causale
028350030801     C                   clear                   dcmr
028360030801     C                   clear                   TIBS02DS
028370030801     C                   movel     'C'           T02mod
028380030801     C                   movel     knsif         t02sif
028390030801     C                   movel     'CMR'         t02cod
028400030801     C                   movel(p)  v1cau         T02ke1
028410030801     C                   call      'TIBS02R'
028420030801     C                   parm                    KPJBA
028430030801     C                   parm                    TIBS02DS
028440030801     C                   movel     t02uni        dcmr
028450030801     C                   movel     d§cmrdes      v1dcau
028460030801     c                   Movel     d§CmrFar      V1Fao
028470020524      * se la proposta di chiusura ha generato una bolla non permetto F12
028480020524     c                   if        %subst(§rmflv:1:1) = 'B'
028490020524     c                   eval      *in17 = *on
028500020524     c                   endif
028510010117     C                   endif
028520050322      * Proteggo sempre il campo dell'orm commissionato
028530050322     c                   Eval      *In34 = *On
028540001106     C                   endif
028550010117
028560050422      * Se Conferma ORM da VAO
028570050422     C                   if        §RMfpr = 'C'
028580050422      * data/ora immissione ORM da VAO
028590161110     c                   clear                   wlbdat
028600050422     c                   z-add     §rmdao        G02inv
028610050422     c                   movel     '3'           G02err
028620050422     c                   call      'XSRDA8'
028630050422     c                   parm                    wlbdat
028640050422     c                   z-add     G02dat        v1dao
028650131212     C                   z-add     §RMdao        inv_v1dao
028660131211     c                   eval      v1oao = §RMoao
028670010117      * Protegge riferimento alfanumerico
028680010117     C                   eval      *in78 = *on
028690010123      * Se non è impostato il riferimento mette il n.orm che c'è nella ds
028700010712     C                   if        §RMrfa = *blanks   and
028710010712     C                             §RMnsr <> *zeros
028720010201     C                   clear                   ds_keyorm
028730010201     C                   eval      ds_poe = §RMpoe
028740010201     C                   eval      ds_nsr = §RMnsr
028750010201     C                   eval      ds_nor = §RMnor
028760010201     C                   eval      ds_nrv = §RMnrv
028770010201     C                   movel     ds_keyorm     v1rfa
028780010123     C                   endif
028790001114     C                   endif
028800010322      * Protegge P.O.ritiro
028810050321      * SEMPRE!!!!! (in manutenzione)
028820050321      * e se orm prepagato da bolla  (tipo comunicazione)
028830050321     c                   Eval      *In09 =  *In02
028840050315     C                             or      v1ctco = 'P'
028850050321      * Orm commissionato
028860030606     c                   Eval      V1com = §orCom
028870170313      /free
028880170313       //?salvo codice mittente e codice ordinante per ricalcolo della data ritiro
028890170505       //?nel caso di variazione del dato
028900170505         sav_dsV1Ccra = ds_V1Ccra;
028910170505         sav_dsV1Ccor = ds_V1Ccor;
028920170313      /end-free
028930100205
028940100205      * Personalizzazioni ORMFLO da tavella PVO
028950100205      * solo in immissione e se conferma da VAO
028960100205     c                   if        *in01 and §rmfpr = 'C'
028970100205     c                   exsr      sr_pvo
028980100205     c                   endif
028990100205
029000050502      * se sto confermando da VAO
029010040714     c                   If        §RMfpr = 'C'
029020050502      * prendo quello passato dal cliente
029030040714     c                   Eval      V1com = §orComc
029040050502      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
029050050502      * di variazione
029060050502     c                   If        §orsrm = 'S'
029070050502     c                   Eval      v1com = 'S'
029080050502     c                   Eval      *In34 = *On
029090050511      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
029100050511     c                   Clear                   v1dar
029110050502     c                   EndIf
029120100325
029130100325      /free
029140150921       //?Calcolo il peso/volume
029150150921        exsr sr_pesvol;
029160131111       //?Richiamo la routine CALPOR
029170131111       //?per avere la filiale di ritiro calcolata da cappario e per
029180131111       //?ulteriori controlli che vengono fatti SOLO in questa routine
029190131111        exsr  CALPOR;
029200161019        IF  *in28;
029210161019          *in28 = *off;
029220161019        ENDIF;
029230161108       //?Cerco orari servizio
029240161108        exsr CercaOrari;
029250140109       //?Recupero gli ambiti per il recupero del giro da ACR
029260140109        exsr sr_caragr;
029270100325       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
029280100325       //?di variazione
029290100325        IF  §ORMpre = 'M';
029300100325          V1com = 'N';
029310100325          *in34 = *on;
029320100325        ENDIF;
029330131018
029340131018       //?Recupero dati da estensione VAOE
029350131018       //?Rcd 'O ' --> Orari apertura
029360131018         k_vaoecor = sav_vaocor;
029370131018         k_vaoepoe = sav_vaopoe;
029380131018         k_vaoensr = sav_vaonsr;
029390131018         k_vaoenor = sav_vaonor;
029400131018         k_vaoenrv = sav_vaonrv;
029410131018         k_vaoetrc = 'O ';
029420131018         clear dOREorari;
029430131018       //?Aggancio il rcd
029440131018         chain  %kds(k06fnvaoe) FNVAOE1L;
029450131018         IF  %found(FNVAOE1L);
029460140415           dOREorari = VAOEdati;
029470131018         ENDIF;
029480131018         v1OraAMda = §OREoramda;
029490131018         v1OraAMa  = §OREorama;
029500131018         v1OraPMda = §OREorapda;
029510131018         v1OraPMa  = §OREorapa;
029520161019       //?Rcd 'DT' --> Data Pronta Merce
029530170524       //?         --> Data Ritiro Calcolata e Anticipo
029540161019         k_vaoetrc = 'DT';
029550161027         clear dOREdt;
029560161019         chain  %kds(k06fnvaoe) FNVAOE1L;
029570161019         IF  %found(FNVAOE1L);
029580161027           dOREdt = VAOEdati;
029590161019         ENDIF;
029600161027         IF  §OREdpm > *zeros;
029610161110           clear wlbdat;
029620161027           inv_V1dpm = §OREdpm;
029630161020           G02inv = inv_V1dpm;
029640161020           G02err = '3';
029650161020           xsrda8 (wlbdat);
029660161020           V1dpm = G02dat;
029670161028           sav_V1dpm = V1dpm;
029680161020         ENDIF;
029690170524         IF  §OREdar > *zeros and §OREdar <> *blanks;
029700170524           DarCalcolata = %int(§OREdar);
029710170524           wOkPosticipa = (§OREposd = 'S');
029720170524           wAnticipa = (§OREant = 'S');
029730170524           Anticipato = (§OREmod = 'S');
029740170524           IF  §OREgga > *zeros and §OREgga <> *blanks;
029750170524             ggAnticipo = %int(§OREgga);
029760170524           ENDIF;
029770170524         ENDIF;
029780131018       //?Rcd 'S ' --> SMS x invio alert
029790131018         k_vaoetrc = 'S ';
029800140505         clear dOREsms;
029810131018       //?Aggancio il rcd
029820131018         chain  %kds(k06fnvaoe) FNVAOE1L;
029830131018         IF  %found(FNVAOE1L);
029840161020           dOREsms = VAOEdati;
029850131018         ENDIF;
029860161020         w03sms = §OREsms;
029870131018       //?Rcd 'MA' --> Mail x invio alert
029880131018         k_vaoetrc = 'MA';
029890131018       //?Aggancio il rcd
029900131018         chain  %kds(k06fnvaoe) FNVAOE1L;
029910131018         IF  %found(FNVAOE1L);
029920131025           w03mail = VAOEdati;
029930131018         ENDIF;
029940140422       //?Rcd 'G' --> Generale
029950140422         k_vaoetrc = 'G';
029960140422         clear dOREgen;
029970140422       //?Aggancio il rcd
029980140422         chain  %kds(k06fnvaoe) FNVAOE1L;
029990140422         IF  %found(FNVAOE1L);
030000140422           dOREgen = VAOEdati;
030010140422         ENDIF;
030020160406       //?controllo se c'è l'invio conferma accettazione ORM
030030160406         wconferma = *off;
030040160406         IF  §OREfmco = 'S' or §OREfsco = 'S';
030050160406           wconferma = *on;
030060160406           *in88 = *on;
030070160406         ENDIF;
030080160406
030090160406       //?Se ho il flag di invio mail conferma cerco i dati
030100160406         IF  wconferma;
030110160406         //?Rcd 'MC' --> Mail x Conferma Prenotazione
030120160406           k_vaoetrc = 'MC';
030130160406           clear w04mail;
030140160406         //?Aggancio il rcd
030150160406           chain  %kds(k06fnvaoe) FNVAOE1L;
030160160406           IF  %found(FNVAOE1L);
030170160406             w04mail = VAOEdati;
030180160406           ENDIF;
030190160406         //?Rcd 'SC' --> Sms x Conferma Prenotazione
030200160406           k_vaoetrc = 'SC';
030210160406           clear w04sms;
030220160406         //?Aggancio il rcd
030230160406           chain  %kds(k06fnvaoe) FNVAOE1L;
030240160406           IF  %found(FNVAOE1L);
030250160406             w04sms = VAOEdati;
030260160406           ENDIF;
030270160406           sav_mailconf = W04mail;
030280160406           sav_smsconf = W04sms;
030290160406         ENDIF;
030300150526
030310150526       //?Controllo i dati MAIL e SMS
030320150526       //?se sono errati non li devo caricare
030330160316         IF  W03sms <> *blanks or W03mail <> *blanks;
030340150526           exsr CtrDatiF04;
030350150526         ENDIF;
030360160426
030370160426       //?Cerco se c'è già il NPR
030380160426         k_vaoetrc = 'NP';
030390160426         WebNPR = *off;
030400160426       //?Aggancio il rcd
030410160426         chain  %kds(k06fnvaoe) FNVAOE1L;
030420160426         IF  %found(FNVAOE1L) and VAOEdati <> *blanks;
030430160426           WebNPR = *on;
030440160426         ENDIF;
030450131212
030460131212       //?Se mittente codificato cerco il giro
030470140109         IF  ds_V1Ccra > 0;
030480131212           exsr sr_pesvol;
030490131212           exsr sr_carcgi;
030500131212           IF  sav_acrcgi <> *blanks;
030510131212             v1tgi = 'A';
030520131212             v1ccgi = sav_acrcgi;
030530131212             sav_v1ccgi = sav_acrcgi;
030540131212           ENDIF;
030550131212         ENDIF;
030560140505
030570140516       //?Se ORM commissionato verifico
030580140516         IF  V1com = 'S';
030590140516         //?verifico se ci sono i presupposti per Alert
030600140516           exsr VerificaAlert;
030610140516         //?se data ritiro già impostata controllo se è da ricalcolare
030620140516           IF  v1dar > 0;
030630140516             exsr  DataRitiroAlert;
030640140516           ENDIF;
030650140505         ENDIF;
030660140428
030670140428       //?Se data ritiro immessa dal cliente inferiore a oggi (giorno di conferma)
030680140429       //?imposto la data di ritiro = oggi
030690140428         IF  §RMdar > 0 and §RMdar < oggi_aammgg;
030700140429           V1dar = oggi_ggmmaa;
030710140428         ENDIF;
030720131212
030730100325      /end-free
030740100325
030750040714     c                   EndIf
030760170505       //?Salvo il commissionato
030770170505         sav_v1com = V1com;
030780070919
030790071107      * se sto confermando una proposta di variazione
030800071107     c                   if        §rmfpr = 'P' and §rmtla <> 'C'
030810080314      * devo ricalcolare sempre il giro se ORM in fase 400 o 410
030820080314     c                   if        v1fao = 400 or v1fao = 410
030830071107      * recupero la tabella AGR per il po ritiro
030840071107     c                   movel     v1cpor        num_v1cpor
030850071107     c                   exsr      sr_caragr
030860071107      * calcolo peso o volume
030870071107     c                   exsr      sr_pesvol
030880071107      * cerca il giro
030890071107     c                   exsr      sr_carcgi
030900071107     c                   if        sav_acrcgi <> *blanks
030910071107     c                   eval      v1tgi = 'A'
030920071107     c                   eval      v1ccgi = sav_acrcgi
030930071107     c                   eval      sav_v1ccgi = sav_acrcgi
030940071107     c                   else
030950071107     c                   clear                   v1tgi
030960071107     c                   clear                   v1ccgi
030970071107     c                   clear                   sav_v1ccgi
030980071107     c                   endif
030990071107     c                   endif
031000080314      * devo impostare il giro presente su ORG se ORM NON in fase 400 o 410
031010080314     c                   if        v1fao <> 400 and v1fao <> 410
031020071107     c                   eval      v1tgi = orgtgi
031030071107     c                   eval      v1ccgi = orgcgi
031040071107     c                   eval      sav_v1ccgi = orgcgi
031050071107     c                   eval      sav_acrcgi = orgcgi
031060160406     c                   eval      savvlm = ORGvlm
031070160406     c                   eval      savpkg = ORGpkg
031080071107     c                   endif
031090131029      /free
031100131029       //?Recupero dati da estensione FNORPE
031110160316       //?Non carico gli Alert, dopo l'immissione sono inutili
031120160406       //?così come i dati per conferma prenotazione
031130131029       //?Rcd 'O ' --> Orari apertura
031140131029         k_ORPEpoe = §RMpoe;
031150131029         k_ORPEnsr = §RMnsr;
031160131029         k_ORPEnor = §RMnor;
031170131029         k_ORPEnrv = §RMnrv;
031180131029         k_ORPEdtv = §RMdtv;
031190131029         k_ORPEorv = §RMorv;
031200131029         k_ORPEtrc = 'O ';
031210131029         clear dOREorari;
031220131029       //?Aggancio il rcd
031230131029         chain  %kds(k07fnorpe) FNORPE1L;
031240131029         IF  %found(FNORPE1L);
031250131029           dOREorari = ORPEdati;
031260140123           wokestensione = *on;
031270131029         ENDIF;
031280131029         v1OraAMda = §OREoramda;
031290131029         v1OraAMa  = §OREorama;
031300131029         v1OraPMda = §OREorapda;
031310131029         v1OraPMa  = §OREorapa;
031320161019       //?Rcd 'DT' --> Data Pronta Merce
031330170524       //?         --> Data Ritiro Calcolata e Anticipo
031340161019         k_ORPEtrc = 'DT';
031350161027         clear dOREdt;
031360161019         chain  %kds(k07fnorpe) FNORPE1L;
031370161019         IF  %found(FNORPE1L);
031380161027           dOREdt = ORPEdati;
031390161019           wokestensione = *on;
031400161019         ENDIF;
031410161027         IF  §OREdpm > *zeros;
031420161110           clear wlbdat;
031430161027           inv_V1dpm = §OREdpm;
031440161020           G02inv = inv_V1dpm;
031450161020           G02err = '3';
031460161020           xsrda8 (wlbdat);
031470161020           V1dpm = G02dat;
031480161028           sav_V1dpm = V1dpm;
031490161020         ENDIF;
031500170524         IF  §OREdar > *zeros and §OREdar <> *blanks;
031510170524           DarCalcolata = %int(§OREdar);
031520170524           wOkPosticipa = (§OREposd = 'S');
031530170524           wAnticipa = (§OREant = 'S');
031540170524           Anticipato = (§OREmod = 'S');
031550170524           IF  §OREgga > *zeros and §OREgga <> *blanks;
031560170524             ggAnticipo = %int(§OREgga);
031570170524           ENDIF;
031580170524         ENDIF;
031590140124       //?Visto che è una conferma di proposta di variazione devo caricare anche
031600140124       //?i dati dell'ORM, almeno per gli orari in modo da poter confrontare le
031610140124       //?variazioni fatte
031620140124         clear wOREorari;
031630140124       //?Recupero dati da estensione FNORE
031640140124       //?Rcd 'O ' --> Orari apertura
031650140124         k_OREpoe = §RMpoe;
031660140124         k_OREnsr = §RMnsr;
031670140124         k_OREnor = §RMnor;
031680140124         k_OREnrv = §RMnrv;
031690140124         k_OREtrc = 'O ';
031700140124       //?Aggancio il rcd
031710140124         chain  %kds(k05fnore) FNORE01L;
031720140124         IF  %found(FNORE01L);
031730140124           wOREorari = OREdati;
031740140124         ENDIF;
031750131029      /end-free
031760070920     c                   endif
031770131024
031780131024      /free
031790140320       //?Se conferma ORM fissi o immissione prepagato
031800140320         IF  §RMtco = 'S' or §RMtco = 'P';
031810150921       //?Richiamo calcolo peso volume
031820150921           exsr sr_pesvol;
031830131211       //?Richiamo la routine CALPOR
031840131211       //?per avere la filiale di ritiro calcolata da cappario e per
031850131211       //?ulteriori controlli che vengono fatti SOLO in questa routine
031860131211           exsr  CALPOR;
031870140109       //?Recupero gli ambiti per il recupero del giro da ACR
031880140109           exsr sr_caragr;
031890131024         ENDIF;
031900140606
031910140606       //?Se conferma di una proposta di chiusura ORM
031920140606       //?devo portarmi dietro anche i dati delle estensioni
031930140606         IF  §RMfpr = 'P' and §RMtla = 'C';
031940140606         //?Recupero dati da estensione FNORE
031950140606         //?Rcd 'O ' --> Orari apertura
031960140606           k_OREpoe = §RMpoe;
031970140606           k_OREnsr = §RMnsr;
031980140606           k_OREnor = §RMnor;
031990140606           k_OREnrv = §RMnrv;
032000140606           k_OREtrc = 'O ';
032010140606           clear dOREorari;
032020140606         //?Aggancio il rcd
032030140606           chain(n)  %kds(k05fnore) FNORE01L;
032040140606           IF  %found(FNORE01L);
032050140606             dOREorari = OREdati;
032060140606             wokestensione = *on;
032070140606           ENDIF;
032080140606           v1OraAMda = §OREoramda;
032090140606           v1OraAMa  = §OREorama;
032100140606           v1OraPMda = §OREorapda;
032110140606           v1OraPMa  = §OREorapa;
032120161020       //?Rcd 'DT' --> Data Pronta Merce
032130170524       //?         --> Data Ritiro Calcolata e Anticipo
032140161020           k_OREtrc = 'DT';
032150161027           clear dOREdt;
032160161020           chain(n)  %kds(k05fnore) FNORE01L;
032170161020           IF  %found(FNORE01L);
032180161027             dOREdt = OREdati;
032190161020             wokestensione = *on;
032200161020           ENDIF;
032210161027           IF  §OREdpm > *zeros;
032220161027             inv_V1dpm = §OREdpm;
032230161110             clear wlbdat;
032240161020             G02inv = inv_V1dpm;
032250161020             G02err = '3';
032260161020             xsrda8 (wlbdat);
032270161020             V1dpm = G02dat;
032280161028             sav_V1dpm = V1dpm;
032290161020           ENDIF;
032300170524           IF  §OREdar > *zeros and §OREdar <> *blanks;
032310170524             DarCalcolata = %int(§OREdar);
032320170524             wOkPosticipa = (§OREposd = 'S');
032330170524             wAnticipa = (§OREant = 'S');
032340170524             Anticipato = (§OREmod = 'S');
032350170524             IF  §OREgga > *zeros and §OREgga <> *blanks;
032360170524               ggAnticipo = %int(§OREgga);
032370170524             ENDIF;
032380170524           ENDIF;
032390140606
032400140606         //?Controllo da rcd GEN se ORM con Alert
032410160316         //?Per visualizzare che è stato inviato l'Alert per ORM commissionato
032420140606           walert = *off;
032430140606           clear dOREgen;
032440140606           k_OREtrc = 'G ';
032450140606           clear dOREgen;
032460140606           chain(n) %kds(K05fnore) FNORE01L;
032470140606           IF  %found(FNORE01L);
032480140606             dOREgen = OREdati;
032490140606           ENDIF;
032500140606           IF  §OREfiso = 'S' or §OREfimo = 'S';
032510140606            walert = *on;
032520140606           ENDIF;
032530160316         //?non carico i dati degli alert, sono in chiusura e non servono
032540160316         //?e soprattutto sono in conferma proposta quindi no F04
032550160405         //?stessa cosa per F13
032560140606         ENDIF;
032570140606
032580131024      /end-free
032590060117
032600060117      * se il tipo ORM è 'P' prepagato proteggo il campo di tipo ORM in modo da non poterlo
032610060117      * + variare
032620060117     c                   Eval      *In05 = (v1ctor = 'P')
032630131209
032640001106     C                   exsr      Sr_Decodifica
032650070919
032660150305      * se filiale ritiro non in gestione all'utente proteggo il giro
032670070919      * se ho già la filiale ritiro impostata
032680071108      * e se giro non è già protetto
032690071108     c                   if        num_v1cpor > *zeros and not *in24
032700150305     c                             and %lookup(%editc(num_v1cpor:'X'):skpog) = 0
032710150305     c                   eval      *in24 = *on
032720070919     c                   endif
032730001023
032740001023     C                   endsr
032750070925
032760070925      *--------------------------------------------------------------------*
032770070925      * Controllo i dati della proposta con quelli dell'ORM
032780070925      *--------------------------------------------------------------------*
032790070925     c     sr_controrp   begsr
032800070925
032810070925     c                   if        ds_v1ccra <> ormcra or
032820070925     c                             v1rsr <> ormrsr or
032830070925     c                             v1pkg <> ormpkg or v1ncl <> ormncl or
032840070925     c                             v1vlm <> ormvlm or v1bnc <> ormbnc or
032850070925     c                             v1blc <> ormblc or v1att <> ormatt or
032860070925     c                             v1mtc <> ormmtc or v1spi <> ormspi
032870070925     c                   eval      *in25 = *on
032880070925     c                   endif
032890160329
032900160329      /free
032910160329       //?Se è stato tolto il codice cliente
032920160329       //?quindi si tratta di un mittente non codificato
032930160329       //?devo togliere il giro
032940160329         IF  ORMcra > 0 and ds_V1Ccra = 0;
032950160329           clear V1Ccgi;
032960160329           clear V1tgi;
032970160329           *in25 = *off;
032980160329         ENDIF;
032990160329      /end-free
033000070925
033010070925     c                   endsr
033020070925
033030050302      *--------------------------------------------------------------------*
033040050302      * Calcolo le date massime per controllo data ritiro
033050050302      *--------------------------------------------------------------------*
033060050302     c     Sr_dtmax      BegSr
033070050304
033080050304      * controllo che i campi siano effettivamente numerici
033090050304     c                   If        d§dftgg1 = *Blanks
033100050304     c                   Eval      d§dftgg1 = '00'
033110050304     c                   Else
033120050304     c                   If        %subst(d§dftgg1:1:1) < '0'
033130050304     c                   Eval      %subst(d§dftgg1:1:1) = '0'
033140050304     c                   EndIf
033150050304     c                   If        %subst(d§dftgg1:2:1) < '0'
033160050304     c                   Eval      %subst(d§dftgg1:2:1) = '0'
033170050304     c                   EndIf
033180050304     c                   EndIf
033190050304     c                   If        d§dftgg2 = *Blanks
033200050304     c                   Eval      d§dftgg2 = '00'
033210050304     c                   Else
033220050304     c                   If        %subst(d§dftgg2:1:1) < '0'
033230050304     c                   Eval      %subst(d§dftgg2:1:1) = '0'
033240050304     c                   EndIf
033250050304     c                   If        %subst(d§dftgg2:2:1) < '0'
033260050304     c                   Eval      %subst(d§dftgg2:2:1) = '0'
033270050304     c                   EndIf
033280050304     c                   EndIf
033290050304
033300050304      * Calcolo la data max x msg info
033310050304     c                   Move      d§dftgg1      w0020
033320050304     c                   Move      v1dao         dataeur
033330050304     c                   Move      dataeur       dataiso
033340050304     c                   Adddur    w0020:*D      dataiso
033350050304     c                   Move      dataiso       dtmaxinfo
033360050304
033370050304      * Calcolo la data max x msg bloccante
033380050304     c                   Move      d§dftgg2      w0020
033390050304     c                   Move      dataeur       dataiso
033400050304     c                   Adddur    w0020:*D      dataiso
033410050304     c                   Move      dataiso       dtmaxblocco
033420050302
033430050302     c                   EndSr
033440001020      **********************************************************************
033450001023      * VERIFICA SE CI SONO PROPOSTE DI VARIAZIONE
033460001020      **********************************************************************
033470001020     C     Sr_Orp        begsr
033480001020
033490001020     C                   eval      *in57 = *off
033500001020
033510001020     C     kfnor         setll     fnorp01l
033520001020     C                   do        *hival
033530140227     C     kfnor         reade(n)  fnorp01l
033540001020     C                   if        %eof(fnorp01l)
033550001020     C                   leave
033560001020     C                   endif
033570001103     C                   if        ORPfev <> *blanks
033580001023     C                   iter
033590001023     C                   endif
033600001020     C                   eval      *in57 = *on
033610001023     C                   leave
033620001020     C                   enddo
033630001020
033640001020     C                   endsr
033650061003
033660061003      *--------------------------------------------------------------------*
033670061003      * verifico se ci sono delle note per l'ORM
033680061003      *--------------------------------------------------------------------*
033690061003     c     sr_orn        begsr
033700061003
033710061003     c                   eval      *in91 = *off
033720061003
033730061003     c     kfnor         setll     fnorn01l
033740061003     c                   do        *hival
033750061003     c     kfnor         reade(n)  fnorn01l
033760061003     c                   if        %eof(fnorn01l)
033770061003     c                   leave
033780061003     c                   endif
033790061003     c                   eval      *in91 = *on
033800061003     c                   leave
033810061003     c                   enddo
033820061003
033830061003     c                   endsr
033840061003
033850061003      *--------------------------------------------------------------------*
033860061003      * controllo se p.o. ritiro estero
033870061003      *--------------------------------------------------------------------*
033880061003     c     sr_por        begsr
033890061003
033900061003     c                   eval      *in21 = *off
033910061003     c                   clear                   v1dfcs
033920061003
033930061003     c                   Clear                   Og143
033940131030     c                   clear                   ntw_V1Cpor
033950131112     c                   clear                   sav_ORGfl1
033960131209     c     num_v1cpor    chain     azorg01l
033970061003     c                   if        %found(azorg01l)
033980061003     c                   eval      og143 = orgde3
033990061003     c                   endif
034000131030     c                   eval      ntw_V1Cpor = §OGntw
034010131112     c                   eval      sav_ORGfl1 = ORGfl1
034020061003      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
034030061003      * europolitan invece ormai è obsoleta
034040131030     c                   if        ntw_V1Cpor = 'DPD' or ntw_V1Cpor = 'EEX'
034050061003      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
034060061003      * partener se caricata
034070131209     c                   if        v1fcs <> *blanks
034080061003     c                   clear                   tibs02ds
034090061003     c                   eval      t02mod = 'C'
034100061003     c                   eval      t02sif = knsif
034110061003     c                   eval      t02cod = 'MIP'
034120131209     c                   eval      t02ke1 = v1fcs
034130061003     c                   call      'TIBS02R'
034140061003     c                   parm                    kpjba
034150061003     c                   parm                    tibs02ds
034160061003     c                   eval      v1dfcs = t02uni
034170061003     c                   eval      *in21 = *on
034180061003     c                   endif
034190061003     c                   endif
034200061003
034210061003     c                   endsr
034220061003
034230001109      **********************************************************************
034240001109      * COPIA ORM CON NUOVO NUMERO VIAGGIO
034250001109      **********************************************************************
034260010906     C     Sr_Copia      begsr
034270001109
034280001109      * Disalloca ORM
034290001109     C                   unlock    fnorm01l
034300071030     C                   unlock    fnorg01l
034310001109      * Cerca ultimo viaggio
034320001109     C                   eval      knrv = 99
034330001109     C     kfnor         setgt     fnorm01l
034340001109     C     kfnor1        readpe(n) fnorm01l
034350001109     C                   if        %eof(fnorm01l)
034360001109     C                   eval      *in28 = *on
034370001109     C                   else
034380010117     C                   if        ORMnrv = 99
034390010117     C                   eval      *in28 = *on
034400010117     C                   else
034410001109     C                   eval      v1nrv = (ORMnrv +1)
034420001109     C                   eval      knrv  = v1nrv
034430010618
034440010618      * Imposto la data e ora chiamata/immissione
034450010618     C                   eval      v1dao = oggi_ggmmaa
034460131211     C                   eval      v1oao = %dec(%time())
034470010911     C                   clear                   wlbdat
034480010911      * giro la data per controllarla con l'inizio della pikup area
034490010911     C                   z-add     v1dao         G02dat
034500010911     C                   movel     *blanks       G02err
034510010911     C                   call      'XSRDA8'
034520010911     C                   parm                    wlbdat
034530010911     C                   z-add     G02inv        inv_v1dao
034540121119      /free
034550170323       //?Pulisco la data pronta merce, in questo modo
034560170323       //?o la imposta l'utente o in automatico il pgm la imposta = data immissione
034570170323       clear V1dpm;
034580170323       clear sav_V1dpm;
034590170323       clear inv_V1dpm;
034600121119       //?Se filiale ritiro NTW DPD imposto il flag del NTW DPD
034610121119       //?nella routine sr_por cerco il ntw della filiale ritiro
034620131030       IF  ntw_V1Cpor = 'DPD';
034630121119         V1ntwd = 'D';
034640121119       ENDIF;
034650121119      /end-free
034660010618
034670130918      * imposto flag per non far fare dei controlli nella sr_contrcapr
034680130918      * controlli inutili oper il calcolo della data, ora mi serve solo la tisi95ds
034690130918      * e la filiale ritiro
034700110121     c                   eval      $copia='S'
034710040615     C                   exsr      Sr_Contrcapr
034720110121     c                   eval      $copia=' '
034730140320      /free
034740140604       //?I dati mail e SMS non sono da duplicare
034750160406       //?l'alert non va riportato sull'orm copiato
034760140604         clear w03sms;
034770140604         clear w03mail;
034780160406         clear w04sms;
034790160406         clear w04mail;
034800160405         clear dOREgen;
034810160406       //?ma attivo F13 nel caso l'utente voglia immettere i dati per la conferma
034820160406         *in88 = *on;
034830140320      /end-free
034840140320
034850130918      * calcolo la data di ritiro
034860040615     c                   movel     v1cpor        num_v1cpor
034870040615     C                   ExSr      Sr_CalPikup
034880040615     C                   eval      v1dar = new_v1dar
034890010618
034900010518      * Azzero la data di stampa e i dati relativi alla distinta
034910010207     C                   z-add     *zeros        v1dst
034920010518     C                   z-add     *zeros        v1npg
034930010518     C                   z-add     *zeros        v1ndc
034940010518     C                   z-add     *zeros        v1ddc
034950140709      * pulisco anche il dato relativo al tipo comunicazione partner
034960140709     c                   clear                   v1fcs
034970140709     c                   clear                   v1dfcs
034980140709     c                   eval      *in21 = *off
034990140709
035000010417     C                   eval      *in09 = *off
035010031106
035020141211      * imposto che il tipo comunicazione di default
035030150220       //?ma solo l'orm che sto copiando NON è un ORM MAIL
035040150220       //?in questo caso devo lasciare MAIL
035050150220     c                   IF        V1Ctco <> 'E'
035060141211     c                   Eval      V1cTco = d§dfttco
035070031106     C                   exsr      Sr_Contrtco
035080150220     c                   ENDIF
035090010117     C                   endif
035100071023
035110071023      * pulisco il giro
035120071023     c                   clear                   v1tgi
035130071023     c                   clear                   v1ccgi
035140071023     c                   clear                   sav_v1ccgi
035150001109     C                   endif
035160001109
035170001109     C                   endsr
035180001109      **********************************************************************
035190001109      * VIDEATA PER ORM ERRATO IN COPIA
035200001109      **********************************************************************
035210001109     C     Sr_ErrCopia   begsr
035220001109
035230001109     C                   eval      v4cpoe = ppoe
035240001109     C                   eval      v4nsr  = pnsr
035250001109     C                   eval      v4nor  = pnor
035260001109     C                   eval      v4nrv  = pnrv
035270001109     C                   eval      v4poe1 = ppoe
035280001109     C                   eval      v4nsr1 = pnsr
035290001109     C                   eval      v4nor1 = pnor
035300001109     C                   eval      v4nrv1 = knrv
035310001109
035320001109     C                   exfmt     fior05e
035330001109
035340001109     C                   endsr
035350001109      **********************************************************************
035360001109      * DECODIFICHE
035370001109      **********************************************************************
035380001109     C     Sr_Decodifica begsr
035390001109
035400010223      * Fase
035410010223     C                   if        v1fao <> *zeros
035420010223     C                   clear                   TIBS02DS
035430010223     C                   movel     'C'           T02mod
035440010223     C                   movel     knsif         t02sif
035450010223     C                   movel     'FAR'         t02cod
035460010223     C                   movel(p)  v1fao         T02ke1
035470010223     C                   call      'TIBS02R'
035480010223     C                   parm                    KPJBA
035490010223     C                   parm                    TIBS02DS
035500010314     C                   movel     t02uni        dfar
035510010314     C                   movel     d§fardes      v1dfao
035520010223     C                   endif
035530010223      * Tipo Ordine
035540010223     C                   if        v1ctor <> *blanks
035550010223     C                   clear                   TIBS02DS
035560010223     C                   movel     'C'           T02mod
035570010223     C                   movel     knsif         t02sif
035580010223     C                   movel     'TOR'         t02cod
035590010223     C                   movel(p)  v1ctor        T02ke1
035600010223     C                   call      'TIBS02R'
035610010223     C                   parm                    KPJBA
035620010223     C                   parm                    TIBS02DS
035630010223     C                   movel     t02uni        v1dtor
035640010223     C                   endif
035650010223      * Tipo comunicazione
035660010223     C                   if        v1ctco <> *blanks
035670010223     C                   clear                   TIBS02DS
035680010223     C                   movel     'C'           T02mod
035690010223     C                   movel     knsif         t02sif
035700010223     C                   movel     'TCO'         t02cod
035710010223     C                   movel(p)  v1ctco        T02ke1
035720010223     C                   call      'TIBS02R'
035730010223     C                   parm                    KPJBA
035740010223     C                   parm                    TIBS02DS
035750010223     C                   movel     t02uni        v1dtco
035760010223     C                   endif
035770161027      /free
035780161027       //?Se tipo comunicazione fisso imposto anche il n. del fisso
035790161027       IF  V1Ctco = 'S';
035800161027         V1Dtco = %trim(V1Dtco) + ' ' + %editc(V1cpos:'X') + '-' +
035810161027                                  %trim(%editc(V1ors:'4'));
035820161027       ENDIF;
035830010223      * Priorità
035840010223     C                   if        v1sto <> *zeros
035850010223     C                   clear                   TIBS02DS
035860010223     C                   move      *all'0'       priorita
035870010223     C                   movel     'C'           T02mod
035880010223     C                   movel     knsif         t02sif
035890010223     C                   movel     'STO'         t02cod
035900010223     C                   move      v1sto         priorita
035910010223     C                   movel(p)  priorita      T02ke1
035920010223     C                   call      'TIBS02R'
035930010223     C                   parm                    KPJBA
035940010223     C                   parm                    TIBS02DS
035950010223     C                   movel     t02uni        v1dsto
035960010223     C                   endif
035970010202      * P.O. ritiro
035980010202     C                   movel     v1cpor        num_v1cpor
035990010202     C                   movel     v1cpor        kazorg
036000150310     c                   clear                   og148
036010010202     C     kazorg        chain     azorg01L
036020010202     C                   if        %found(azorg01l)
036030010202     C                   movel     ORGdes        v1dpor
036040150310     C                   movel     ORGde8        og148
036050010202     C                   endif
036060150310      * filiale ritiro abilitata allo scarico PDA
036070150310     c                   eval      *in22 = (§ogpdaorm <> *blanks)
036080070913      * giro
036090070913     c                   exsr      ctrgiro
036100070913      *     imposto la descrizione del giro
036110070913     c                   if        d09ocgi <> ' '
036120070913     c                   eval      v1dcgi = d09odes
036130070913     c                   endif
036140010223      * P.O. consegna
036150010223     C                   movel     v1cpoc        kazorg
036160010223     C     kazorg        chain     azorg01L
036170010223     C                   if        %found(azorg01l)
036180010223     C                   movel     ORGdes        v1dpoc
036190010223     C                   endif
036200010202      * Orm fisso emetto il numero ritiro fisso
036210001109     C                   eval      *in76 = (v1ors <> *zeros)
036220050502
036230050502      * Visualizzo che ORM con ricevuta di ritiro
036240050502     c                   Eval      *In11 = (§orsrm = 'S')
036250070806
036260070806      * controllo anche se ci sono note
036270131210     c                   exsr      sr_orn
036280140130
036290140130      /free
036300140130         *in96 = *off;
036310140130       //?Controllo se ci sono Note AUT
036320140130         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
036330140130         IF  %found(FNORT01L);
036340140130           *in96 = *on;
036350140130         ENDIF;
036360140130      /end-free
036370140130
036380070806      * controllo anche se p.o. ritiro estero
036390070806     c                   exsr      sr_por
036400070806      * controllo se p.o. ritiro DPD o se p.o. emissione DPD per abilitare F2=Dati DPD
036410131209     c     V1Cpoe        lookup    skpodpd                                30
036420131209     c  n30num_V1Cpor    lookup    skpodpd                                30
036430070806     c                   eval      *in18 = (*in30)
036440131003
036450131003      * Richiamo calcolo della data
036460131003      * per avere le date max blocco e max info
036470131003     c                   ExSr      Sr_CalPikup
036480161103
036490161103      /free
036500161103       //?Se non ho la data pronta merce imposto la data ritiro
036510161103         IF  V1dpm = 0;
036520161103           V1dpm = V1dar;
036530161103           sav_V1dpm = V1dpm;
036540161103           inv_v1dpm = old_invv1dar;
036550161103         ENDIF;
036560161103      /end-free
036570070806
036580070806     C                   endsr
036590070119
036600070119      *--------------------------------------------------------------------*
036610070119      * Carico dati sensibili per PDA (salvo)
036620070119      *--------------------------------------------------------------------*
036630070119     c     sr_datipdasav begsr
036640070119
036650070119     c                   clear                   datipdasav
036660080321
036670080321      * se non è una conferma proposta di variazione salvo i dati del video
036680080321     c                   if        §rmfpr <> 'P'
036690070119     c                   eval      pdasavrsr = v1rsr
036700070119     c                   eval      pdasavinr = v1inr
036710070119     c                   eval      pdasavlor = v1lor
036720070119     c                   eval      pdasavprr = v1prr
036730070119     c                   eval      pdasavrer = v1rer
036740070119     c                   eval      pdasavter = v1ter
036750070119     c                   eval      pdasavorr = v1orr
036760070119     c                   eval      pdasavncl = v1ncl
036770070119     c                   eval      pdasavpkg = v1pkg
036780070119     c                   eval      pdasavvlm = v1vlm
036790070119     c                   eval      pdasavbnc = v1bnc
036800070119     c                   eval      pdasavno1 = v1not1
036810070119     c                   eval      pdasavno2 = v1not2
036820131107      /free
036830131107        pdasavora1 = V1OraAMda;
036840131107        pdasavora2 = V1OraAMa;
036850131107        pdasavora3 = V1OraPMda;
036860131107        pdasavora4 = V1OraPMa;
036870131107      /end-free
036880080321     c                   endif
036890080321
036900080321      * se è una conferma proposta di variazione salvo i dati dal file
036910140122     c                   if        §rmfpr = 'P'
036920080321     c                   eval      pdasavrsr = ormrsr
036930080321     c                   eval      pdasavinr = orminr
036940080321     c                   eval      pdasavlor = ormlor
036950080321     c                   eval      pdasavprr = ormprr
036960080321     c                   eval      pdasavrer = ormrer
036970080321     c                   eval      pdasavter = ormter
036980080321     c                   eval      pdasavorr = ormorr
036990080321     c                   eval      pdasavncl = ormncl
037000080321     c                   eval      pdasavpkg = ormpkg
037010080321     c                   eval      pdasavvlm = ormvlm
037020080321     c                   eval      pdasavbnc = ormbnc
037030080321     c                   eval      pdasavno1 = ormno1
037040080321     c                   eval      pdasavno2 = ormno2
037050131107      /free
037060140124        pdasavora1 = w§OREoramda;
037070140124        pdasavora2 = w§OREorama;
037080140124        pdasavora3 = w§OREorapda;
037090140124        pdasavora4 = w§OREorapa;
037100131107      /end-free
037110080321     c                   endif
037120070119
037130070119     c                   endsr
037140070119
037150001010      **********************************************************************
037160001010      * CONTROLLI
037170001010      **********************************************************************
037180001010     C     Sr_Contr      begsr
037190001006
037200001009      * Reset indicatori
037210001010     C                   setoff                                       414243
037220001010     C                   setoff                                       444546
037230001010     C                   setoff                                       474849
037240161020     C                   setoff                                       505152
037250001010     C                   setoff                                       535455
037260001023     C                   setoff                                       565859
037270001011     C                   setoff                                       606162
037280001011     C                   setoff                                       636465
037290161108     C                   setoff                                         7475
037300131211     C                   setoff                                       778284
037310010214     C                   setoff                                       858687
037320160406     C                   setoff                                       35
037330130611
037340130611     c                   eval      wintcap = *off
037350071106
037360071126     c  n10              eval      $proposte = *off
037370071108     c                   eval      $modifica = *off
037380131018
037390131018     c                   clear                   h1riga
037400131018     c                   clear                   h1colo
037410010223
037420011113      * Tipo ORM
037430081126     C                   exsr      Sr_Contrtor
037440010223     C   90
037450010223     Cor 28              goto      endsrcontr
037460010223
037470011113      * Tipo comunicazione ORM
037480081126     C                   exsr      Sr_Contrtco
037490150615     C   90
037500150615     Cor 28              goto      endsrcontr
037510001009
037520011113      * Cliente mittente
037530010117     C                   exsr      Sr_Contrclir
037540001011     C   28              goto      endsrcontr
037550131211
037560130507      * se richiamato il pgm di interrogazione cappario riemetto la videata
037570130507      * e mi posiziono sui colli
037580130507     c                   IF        wintcap
037590130507     c                   eval      *in43 = *on
037600140109     c                   eval      *in90 = *on
037610130507     c                   leavesr
037620130507     c                   ENDIF
037630001006
037640130508      * Colli - Peso - Bancali - Volume
037650001010     C                   exsr      Sr_Contrqta
037660001020     C   28              goto      endsrcontr
037670150921
037680150921      /free
037690161025       //?Se immissione/copia
037700161025       //?e non c'è la filiale ritiro
037710161025       //?calcolo peso/volume
037720161025       //?calcolo filiale ritiro
037730161025         IF  (*in01 or *in03) and
037740161025              V1Cpor = *blanks;
037750161025           exsr sr_pesvol;
037760161025           exsr CALPOR;
037770161025       //?Recupero gli ambiti per il recupero del giro da ACR
037780161025           exsr sr_caragr;
037790161025           IF  *in28;
037800161025             leavesr;
037810161025           ENDIF;
037820150921         ENDIF;
037830161019
037840161019       //?Controllo la data pronta merce
037850161020         exsr ContrDataProntaMerce;
037860161019         IF  *in28;
037870161020           leavesr;
037880161019         ENDIF;
037890161019
037900150921      /end-free
037910010223
037920010223      * Referente e Telefono
037930010223     C                   exsr      Sr_Contrref
037940010223     C   28              goto      endsrcontr
037950131014
037960131014      /free
037970131112       //?Controllo gli orari di apertura
037980131112         exsr OrariApertura;
037990131014         IF  *in28;
038000131014           leavesr;
038010131014         ENDIF;
038020131014      /end-free
038030001010
038040010202      * Pagamento
038050001011     C                   exsr      Sr_Contrpag
038060001020     C   28              goto      endsrcontr
038070001010
038080010117      * Cliente destinatario
038090010117     C                   exsr      Sr_Contrclid
038100001011     C   28              goto      endsrcontr
038110010208
038120010208      * Cliente ordinante
038130010208     C                   exsr      Sr_Contrclio
038140010208     C   28              goto      endsrcontr
038150050323
038160050323      * se ORM da FILE il codice del cliente ritiro deve essere un figlio del codice ordinante
038170050323      * metto il controllo in questo punto xchè ho già tutti i clienti decodificati
038180140319      * faccio il controllo se i 2 codici sono <> come ksc lungo 7
038190050323     c                   If        v1ctco = 'F' and ds_v1ccor <> *Zeros and
038200050323     c                             ds_v1ccra <> *Zeros
038210140319     c                             and ds_v1cra12 <> ds_v1cor12
038220050323     c                   ExSr      Sr_Figli
038230050323     c                   Move      ds_v1cra12    w0110
038240050323     c     w0110         Lookup    Skfigli                                30
038250050323     c                   If        Not *In30
038260050323     c                   Eval      v1cmsg = msg(86)
038270050323     c                   Eval      *In28 = *On
038280050323     c                   Eval      *In41 = *On
038290050323     c                   LeaveSr
038300050323     c                   EndIf
038310050323     c                   EndIf
038320130508
038330130508      * P.O. Consegna
038340130508     C                   exsr      Sr_Contrpoc
038350130508     C   90
038360130508     Cor 28              goto      endsrcontr
038370040630
038380040630      * Cod.Cliente - Cod.Tariffa
038390040630     C                   exsr      Sr_Contrksc
038400040630     C   28              goto      endsrcontr
038410010201
038420010201      * P.O. ritiro
038430010201     C                   exsr      Sr_Contrpor
038440130412      * controllo se devo proteggere il giro
038450150305      * se non è già protetto
038460150305     c                   if        %lookup(%editc(num_v1cpor:'X'):skpog) = 0
038470071108     c                             and not *in24
038480070924     c                   eval      *in24 = *on
038490070924     c                   endif
038500131202      /free
038510150305       //?Se il giro è protetto ma la ritiro è una filiale gestita dall'utente
038520131202       //?devo sproteggere il campo del giro
038530131202       //?se sono in immissione
038540150305         IF  *in24 and *in01 and
038550150305           %lookup(%editc(num_v1cpor:'X'):skpog) > 0;
038560131202           *in24 = *off;
038570131202         ENDIF;
038580131202      /end-free
038590010201     C   90
038600010201     Cor 28              goto      endsrcontr
038610140122
038620140122      /free
038630110610       //?Controlli per ORM Export e destinatario merce
038640110610       //?Se Export EEX non posso inserire destinatari esteri
038650110610         IF  *in14 and V1nac <> *blanks;
038660110610           V1Cmsg = msg(95);
038670110610           *in28 = *on;
038680110610           *in65 = *on;
038690110613           *in75 = *on;
038700110610           *in90 = *on;
038710110610           leavesr;
038720110610         ENDIF;
038730110610       //?Se Export DPD non posso inserire destinatari di San Marino
038740110610       //?                                                Città del Vaticano
038750110610       //?                                                Livigno
038760110613       //?Richiamo pgm esterno
038770110613         IF  *in15;
038780110613           clear FIOR95ds;
038790110613           IOR95cap = V1cac;
038800110613           IOR95loc = V1loc;
038810110613           IOR95prv = V1prc;
038820110613           IOR95naz = V1nac;
038830110613           fior95r(kpjba : fior95ds);
038840110613           IF  OOR95err <> *blanks;
038850110613             V1Cmsg = OOR95msg;
038860110613             *in28 = *on;
038870110613             *in63 = *on;
038880110613             *in75 = *on;
038890110613             *in90 = *on;
038900110613             leavesr;
038910110613           ENDIF;
038920110613           IF  OOR95ok = 'N';
038930110613             V1Cmsg = OOR95msg;
038940110613             *in28 = *on;
038950110613             *in63 = *on;
038960110613             *in75 = *on;
038970110613             *in90 = *on;
038980110613             leavesr;
038990110613           ENDIF;
039000110613         ENDIF;
039010110610      /end-free
039020071031
039030071031      * calcolo peso o volume se non impostati a video
039040071031      * considerando anche le cubature degli automezzi eventualmente immessi
039050071031      * faccio ora questo calcolo perchè mi serve per recuperare il giro
039060140109      * ma solo se non è chiusura ORM e non ho fatto F5-Altri dati
039070131210     c                   IF        not *inke and not *in10
039080131209     c                   exsr      sr_pesvol
039090131209     c                   exsr      sr_contrcgi
039100160325     c                   IF        wModGiroAut
039110160325     c                   exsr      Sr_Tastifun
039120160325     c                   leavesr
039130160325     c                   ENDIF
039140131212     c                   exsr      Sr_Tastifun
039150161109     c   28              eval      v1cmsg = msg(73)
039160161109     c   28              leavesr
039170131209     c                   ENDIF
039180161024
039190100204      * Personalizzazioni ORMFLO da tavella PVO
039200100205      * solo in immissione ORM da menù
039210100204      * faccio la personalizzazione in questo punto perchè così nelle specifiche dopo
039220100204      * faccio i controlli del commissionato e della data ritiro che vengono
039230100204      * modificati da questa routine
039240100205     c                   if        *in01 and §rmtla = *blanks
039250100204     c                   exsr      sr_pvo
039260100205      * Se blocco destinatario devo ricontrollare i dati del destinatario
039270100205     c                   if        §ormfd = 'S'
039280100205     c                   exsr      sr_contrclid
039290100205     c   28              leavesr
039300100205     c                   endif
039310100205      * devo sbloccare il campo di ORM commissinato nel caso in cui lo avessi bloccato
039320100205      * perchè orm con ricevuta ritiro
039330100208     c                   if        §orsrm = 'S'
039340100205     c                   eval      *in34 = *off
039350100205     c                   endif
039360100205      * devo pulire il campo di orm commissionato così il pgm rifà il calcolo in automatico
039370100208     c                   if        §orcomc <> *blanks
039380100205     c                   clear                   v1com
039390100205     c                   endif
039400100204     c                   endif
039410050322
039420050322      * ORM COMMISSIONATO
039430050322     c                   If        *In01 or *In03
039440050322     c                   Exsr      Sr_ormcom
039450050322      * se è un orm commissionato controllo se ok tutti i dati
039460050322     c                   Exsr      Sr_Contrcom
039470050322     c   28
039480050322     cor 90              goto      endsrcontr
039490050324      * se ORM prepagato non locale deve essere un ORM COMMISSIONATO
039500050324     c                   If        v1com = 'N' and v1ctor = 'P' and
039510150305     c                             num_v1cpor <> DUTpou
039520050324     c                   Eval      v1cmsg = msg(87)
039530050324     c                   Eval      *In28 = *On
039540050324     c                   Eval      *In35 = *On
039550050324     c                   Leavesr
039560050324     c                   EndIf
039570050322     c                   EndIf
039580160405
039590160405      /free
039600160405       //?Se sono in immissione manuale (telefonica/mail)
039610160405       //?O copia di un ORM manuale (telefonica/mail)
039620160405       //?e non è un mittente estero
039630160405       //?e l'ordinante è un codificato
039640160405       //?e le quantità non superano i limiti impostati in tabella
039650160405         IF  ((*in01 or *in03) and
039660160405              (V1Ctco = 'M' or V1Ctco = 'E')) and
039670160405             ((ds_V1Ccra > 0 and
039680160405               m_§OGntw <> 'EEX' and m_§OGntw <> 'DPD' and
039690160405               m_§OGntw <> 'FED') or
039700160405              (ds_V1Ccra = 0 and V1nar = *blanks)) and
039710160405              (ds_V1Ccor > *zeros and
039720160405               ds_V1cor2 <> 9999 and ds_V1cor2 <> 8888) and
039730160405              (V1pkg <= d§DFTpkga and V1vlm <= d§DFTvlma and
039740160405               V1bnc <= d§DFTbnca);
039750160405       //?Prima del calcolo della data di ritiro se ORM commissionato e non ho
039760160405       //?ancora inserito i dati dell'alert devo richiederli
039770160405       //?se non è un ORM prepagato
039780160405           IF  V1com = 'S' and not wRicAlert and
039790160405               w03sms = *blanks and w03mail = *blanks and
039800160405               V1Ctor <> 'P';
039810160405             exsr F04D01;
039820160405             wRicAlert = *on;
039830160405         //?Imposto che devo ricalcolare la data di ritiro se = oggi
039840160405             IF  V1dar > 0 and V1dar = oggi_ggmmaa;
039850160405               wCalDtRit = *on;
039860160405             ENDIF;
039870160405           ENDIF;
039880160405         ENDIF;
039890160405      /end-free
039900050322
039910050322      * calcolo la data di ritiro
039920050322     C                   If           v1dar = *zeros
039930050322      * calcolo la data di ritiro sempre se è una conferma da VAO
039940130918      * anche richiesta dall'ordinante sul VAO
039950130919     c                             or §rmFpr = 'C'
039960050322      * calcolo la data di ritiro sempre se variato il flag di ORM COMMISSIONATO
039970050322     c                             or sav_v1com <> v1com
039980131024      * calcolo la data di ritiro sempre se variato il mittente
039990131211      * solo come codice
040000131024     c                             or sav_dsV1Ccra <> ds_V1Ccra
040010170208      * calcolo la data di ritiro sempre se variato l'ordinante
040020170208      * solo come codice
040030170208     c                             or sav_dsV1Ccor <> ds_V1Ccor
040040140508      * calcolo la data di ritiro sempre se variato cap/località/provincia
040050140509     c                             or wModLocRit
040060140509      * o se ORM con Alert
040070140508     c                             or wCalDtRit
040080161025      * o se variato il peso
040090161025     c                             or wModPeso
040100161028      * o variata la data pronta merce
040110161028     c                             or wModDpm
040120161116      * o variata la filiale di ritiro
040130161116     c                             or wModPor
040140050322     c                   movel     v1cpor        num_v1cpor
040150050322     c                   ExSr      Sr_CalPikup
040160130918      * imposto la data di ritiro
040170050322     C                   if        v1dar = *zeros
040180050322     C                             or (v1com <> sav_v1com and
040190050322     C                                 v1dar = old_newv1dar)
040200161026     c                             or sav_dsV1Ccra <> ds_V1Ccra
040210170208     c                             or sav_dsV1Ccor <> ds_V1Ccor
040220140509     c                             or wModLocRit
040230140509     c                             or wCalDtRit
040240161025     c                             or wModPeso
040250161028     c                             or wModDpm
040260161116     c                             or wModPor
040270050322     C                   eval      v1dar = new_v1dar
040280140529      /free
040290140529       //?se conferma ORM da VAO e la data ricalcolata è inferiore a oggi
040300140529       //?imposto oggi
040310140530        IF  §RMfpr = 'C';
040320140530           clear wdata;
040330140530           dataeur = %date(v1dar:*eur);
040340140530           dataiso = dataeur;
040350140530           wdata = %dec(dataiso);
040360140530           IF  wdata < oggi_aammgg;
040370140530             v1dar = oggi_ggmmaa;
040380140530          ENDIF;
040390140529        ENDIF;
040400140529      /end-free
040410050322     C                   endif
040420050322     c                   eval      sav_v1com = v1com
040430131024     c                   eval      sav_dsV1Ccra = ds_V1Ccra
040440170208     c                   eval      sav_dsV1Ccor = ds_V1Ccor
040450140509      /free
040460140509        wModLocRit = *off;
040470140509        wCalDtRit = *off;
040480161026        wModPeso = *off;
040490161028        wModDpm = *off;
040500161116        wModPor = *off;
040510140509      /end-free
040520050322     C                   endif
040530160330
040540160330      /free
040550160330       //?Se immissione Telefonica o Mail/Fax e mittente NON codificato
040560160413       //?obbligo del n. di telefono e del referente
040570160330         IF  (*in01 or *in03) and
040580160413             (V1Ctco = 'M' or V1Ctco = 'E') and ds_V1Ccra = 0;
040590160413           IF  V1rer = *blanks;
040600160413             V1Cmsg = msg(57);
040610160413             *in28 = *on;
040620160413             *in86 = *on;
040630160413             leavesr;
040640160413           ENDIF;
040650160413           IF  V1ter = *blanks;
040660160330           V1Cmsg = msg(58);
040670160330           *in28 = *on;
040680160330           *in87 = *on;
040690160330           leavesr;
040700160413           ENDIF;
040710160330         ENDIF;
040720160330      /end-free
040730010906
040740010906      * Data ritiro
040750130918      * la controllo per ultima
040760130918      * e solo se non sto dirottanto l'ORM
040770040628     c                   If        wokdirotta = *Off
040780010906     C                   exsr      Sr_Contrdar
040790010906     C   28              goto      endsrcontr
040800040628     c                   endif
040810161109
040820161109      /free
040830161109       //?Cerco orari servizio
040840161109         exsr CercaOrari;
040850161109      /end-free
040860131024
040870131024      * Ora
040880131024     c                   exsr      Sr_Controrr
040890131024     c   28              goto      endsrcontr
040900130917
040910130918      * controllo ora pronta merce con nuovi orari di servizio
040920131022      * solo in immissione ORM
040930140317      * e copia ORM
040940140317     c                   IF        *in01 or *in03
040950130917     C                   exsr      contrORRmax
040960130917     C   28              goto      endsrcontr
040970131022     c                   ENDIF
040980040426
040990040426      * invio msg info nel caso di ORM già inserito con stesso mittente e ritiro lo stesso giorno
041000120227      * faccio il controllo anche mentre sto dirottando
041010120227     c                   If        (*In01 or *In03 or wokdirotta = *on)
041020120227     c                              and ds_v1ccra > *Zeros
041030120306     c     kOrm08        Setll     Fnorm18l
041040120306     c                   do        *hival
041050120306     c     kOrm08        reade     Fnorm18l
041060120306     c                   if        %eof(Fnorm18l)
041070120306     c                   leave
041080120306     c                   endif
041090120306     c                   if        wokdirotta = *on and
041100120306     c                             f_ormpoe = v1cpoe and f_ormnsr = v1nsr and
041110120306     c                             f_ormnor = v1nor  and f_ormnrv = v1nrv
041120120306     c                   iter
041130120306     c                   endif
041140120306     c                   If        Not *In37
041150040628     c                   Eval      V1cMsg = Msg(80)
041160040426     c                   Eval      %subst(v1cmsg:5:3) = %editc(f_OrmPoe:'4')
041170040426     c                   Move      f_OrmNsr      w002a
041180040426     c                   Eval      %subst(v1cmsg:9:2) = w002a
041190040426     c                   Eval      %subst(v1cmsg:12:7) = %editc(f_OrmNor:'4')
041200040426     c                   Move      f_OrmNrv      w002a
041210040426     c                   Eval      %subst(v1cmsg:20:2) = w002a
041220040426     c                   Eval      *In28 = *On
041230040426     c                   Eval      *In37 = *On
041240040426     c                   Eval      *In41 = *On
041250040426     c                   GoTo      endsrcontr
041260120306     c                   EndIf
041270120306     c                   enddo
041280040426     c                   EndIf
041290160405
041300160405      /free
041310160406       //?Alla fine di tutti i controlli
041320160406       //?se Conferma da VAS controllo i dati della conferma prenotazione
041330160406       //?se richiesta da chi ha inserito il ritiro
041340160406         IF  §RMfpr = 'C' and wConferma;
041350160406           exsr CtrDatiF13;
041360160406           IF  *in28;
041370160406             *in28= *off;
041380160406             wErrConferma = *on;
041390160406             leavesr;
041400160406           ENDIF;
041410160406         ENDIF;
041420160406
041430160413       //?Immissione Manuale o copia
041440160406       //?ORM con mittente NON codificato e ordinante NON codificato
041450160408       //?(sono un perfetto sconosciuto e quindi sono costretta a chiedere
041460160406       //? se vogliono la conferma della prenotazione)
041470160406       //?richiedo i dati per conferma prenotazione
041480160406       //?se non sono già impostati
041490160413         IF  (*in01 or *in03) and (V1Ctco = 'M' or V1Ctco = 'E') and
041500160406             ds_V1Ccra = 0 and ds_v1Ccor = 0 and
041510160406             W04sms = *blanks and W04mail = *blanks and
041520160405             not wRicConf;
041530160405           exsr F13D01;
041540160405           wRicConf = *on;
041550160405         ENDIF;
041560160405      /end-free
041570010223
041580010223      * Fase
041590010223     C                   exsr      Sr_Contrfao
041600071108
041610071108      * controllo se ho modificato qualcosa
041620071126      * sempre se conferma proposta di variazione
041630090211     c                   if        vidold <> vidnew or
041640131018     c                             vidold2 <> vidnew2 or
041650131018     c                             vidoldA <> vidnewA or
041660160406     c                             vidoldC <> vidnewC or
041670090206     c                             §rmfpr = 'P'
041680071108     c                   eval      $modifica = *on
041690071108     c                   endif
041700071106
041710071106      * alla fine di tutti i controlli
041720071106      * se non ho fatto un dirottamento
041730071106     c                   if        wokdirotta = *off and
041740071106      * se è manutenzione
041750071106     c                             *in02 and not *in10 and §rmtla = *blanks and
041760071106      * se è stato variato qualcosa
041770071108     c                             $modifica = *on and
041780071107      * ma non solo il giro
041790071108     c                             v1ccgi = sav_v1ccgi
041800080314      * devo --> ricalcolare il giro se ORM NON in fase 400 (assegnato) e 410
041810080314      *      --> generare proposta di variazione se ORM in fase 400 e410
041820071106      * ORM NON assegnato
041830071106      * ricalcolo il giro
041840071106     c                   if        v1fao < 400
041850071106     c                   exsr      sr_carcgi
041860071106     c                   if        sav_acrcgi <> *blanks
041870160325     c                   IF        V1Ccgi <> sav_ACRcgi
041880071106     c                   eval      v1tgi = 'A'
041890071106     c                   eval      v1ccgi = sav_acrcgi
041900071106     c                   eval      sav_v1ccgi = sav_acrcgi
041910160325      * informo l'utente che il giro è stato variato
041920161117     c                   eval      *in28 = *on
041930161117     c                   eval      *in90 = *on
041940161117     c                   eval      V1Cmsg = msg(98)
041950160325     c                   leavesr
041960160325     c                   ENDIF
041970071106     c                   else
041980160325     c                   IF        V1Ccgi <> *blanks
041990071106     c                   clear                   v1tgi
042000071106     c                   clear                   v1ccgi
042010071106     c                   clear                   sav_v1ccgi
042020160325      * informo l'utente che il giro è stato variato
042030161117     c                   eval      *in28 = *on
042040161117     c                   eval      *in90 = *on
042050161117     c                   eval      V1Cmsg = msg(98)
042060160325     c                   leavesr
042070160325     c                   ENDIF
042080071106     c                   endif
042090071106     c                   endif
042100071106      * ORM assegnato
042110071106      * scrivo proposta variazione
042120080314     c                   if        v1fao = 400 or v1fao = 410
042130071106     c                   eval      $proposte = *on
042140071106     c                   endif
042150150309      * ORM in fase 50 e NON sono in conferma ORM commissionati vuol dire
042160150309      * che sono una filiale abilitata alla manutenzione ma NON sono la filiale
042170150309      * ritiro quindi
042180150309      * scrivo proposta variazione
042190150309     c                   if        v1fao = 050 and §rmfpr <> 'O'
042200150309     c                   eval      $proposte = *on
042210150309     c                   endif
042220071106     c                   endif
042230100225
042240100224      /free
042250100224       //?Controlli per preavviso ORM in caso di conferma ORM da VAO
042260100225       //?lo faccio solo se non sono nel momento dei controlli fatti prima
042270100225       //?di confermare i dati da VAO
042280100225         IF  $immetti;
042290100225           $errpre = *off;
042300100225           IF  §rmfpr = 'C' and *in01;
042310100225             exsr sr_CtrDatiPre;
042320100225             $errpre = *in28;
042330100225           ENDIF;
042340100224         ENDIF;
042350131112
042360131112       //?se sono in immissione/copia/conferma ORM commissionati
042370131112       //?e non ho nessun errore mi devo posizionare sempre sul campo delle note
042380131112         IF  (*in01 or *in03 or §RMfpr = 'O') and not *in28;
042390131112           *in82 = *on;
042400131112         ENDIF;
042410131106
042420100224      /end-free
042430001006
042440001011     C     endsrcontr    endsr
042450010906
042460010223      **********************************************************************
042470010223      * CONTROLLI     * tipo ordine *
042480010223      **********************************************************************
042490010223     C     Sr_Contrtor   begsr
042500010223
042510010223      * Ricerca Tipo Ordine con "?"
042520010223     C     '?'           scan      v1ctor                                 30
042530010223     C     *in30         ifeq      *on
042540160316     c                   eval      H1riga = 05
042550160316     c                   eval      H1colo = 11
042560010223     C                   eval      *in90 = *on
042570010223     C                   clear                   TIBS02DS
042580010223     C                   movel     'R'           t02mod
042590010223     C                   movel     knsif         t02sif
042600010223     C                   movel     'TOR'         t02cod
042610010223     C                   call      'TIBS02R'
042620010223     C                   parm                    KPJBA
042630010223     C                   parm                    TIBS02DS
042640010223     C                   movel     T02ke1        v1ctor
042650010223     C                   movel     T02uni        v1dtor
042660010223     C                   goto      endsrctor
042670010223     C                   endif
042680010223      * Se non immesso devo mettere quello di dft
042690010223     C                   if        v1ctor = *blanks
042700010223     C                   eval      v1ctor = d§dfttor
042710010223     C                   endif
042720010223      * Decodifico
042730010223     C                   clear                   TIBS02DS
042740010223     C                   movel     'C'           T02mod
042750010223     C                   movel     knsif         t02sif
042760010223     C                   movel     'TOR'         t02cod
042770010223     C                   movel(p)  v1ctor        T02ke1
042780010223     C                   call      'TIBS02R'
042790010223     C                   parm                    KPJBA
042800010223     C                   parm                    TIBS02DS
042810010223     C                   if        t02err <> *blanks
042820010223     C                   movel     msg(13)       v1cmsg
042830160316     c                   eval      H1riga = 05
042840160316     c                   eval      H1colo = 11
042850160316     C                   seton                                            28
042860010223     C                   goto      endsrctor
042870010223     C                   endif
042880010223     C                   movel     t02ke1        v1ctor
042890010223     C                   movel     t02uni        v1dtor
042900010223
042910010223     C     endsrctor     endsr
042920010223      **********************************************************************
042930010223      * CONTROLLI     * tipo comunicazione ordine *
042940010223      **********************************************************************
042950010223     C     Sr_Contrtco   begsr
042960150615
042970150615      * Ricerca Tipo Ordine con "?"
042980150615     C     '?'           scan      v1ctco                                 30
042990150615     C     *in30         ifeq      *on
043000160316     c                   eval      H1riga = 04
043010160316     c                   eval      H1colo = 16
043020150615     C                   eval      *in90 = *on
043030150615     C                   clear                   TIBS02DS
043040150615     C                   movel     'R'           t02mod
043050150615     C                   movel     knsif         t02sif
043060150615     C                   movel     'TCO'         t02cod
043070150615     C                   call      'TIBS02R'
043080150615     C                   parm                    KPJBA
043090150615     C                   parm                    TIBS02DS
043100150615     C                   movel     T02ke1        v1ctco
043110150615     C                   movel     T02uni        v1dtco
043120150615     C                   leavesr
043130150615     C                   endif
043140010223
043150150615      * Se non immesso devo mettere quello di dft
043160010223     C                   if        v1ctco = *blanks
043170010223     C                   eval      v1ctco = d§dfttco
043180010223     C                   endif
043190010223
043200010223     C                   if        v1ctco <> *blanks
043210010223     C                   clear                   TIBS02DS
043220010223     C                   movel     'C'           T02mod
043230010223     C                   movel     knsif         t02sif
043240010223     C                   movel     'TCO'         t02cod
043250010223     C                   movel(p)  v1ctco        T02ke1
043260010223     C                   call      'TIBS02R'
043270010223     C                   parm                    KPJBA
043280010223     C                   parm                    TIBS02DS
043290150615     C                   if        t02err <> *blanks
043300150615     C                   movel     msg(97)       v1cmsg
043310160316     c                   eval      H1riga = 04
043320160316     c                   eval      H1colo = 16
043330160316     C                   seton                                            28
043340150615     C                   leavesr
043350150615     C                   endif
043360010223     C                   movel     t02uni        v1dtco
043370161028      /free
043380161028       //?Se tipo comunicazione fisso imposto anche il n. del fisso
043390161028       IF  V1Ctco = 'S';
043400161028         V1Dtco = %trim(V1Dtco) + ' ' + %editc(V1cpos:'X') + '-' +
043410161028                                  %trim(%editc(V1ors:'4'));
043420161028       ENDIF;
043430150615     c                   IF        *in04 and V1Ctco <> 'M' and
043440150615     c                             V1Ctco <> 'E'
043450150615     C                   movel     msg(97)       v1cmsg
043460150615     C                   eval      V1Cmsg = %trim(V1Cmsg) +
043470160405     C                             ' Ammesso solo MAIL/FAX o TELEFONICO'
043480160316     c                   eval      H1riga = 04
043490160316     c                   eval      H1colo = 16
043500160316     C                   seton                                            28
043510150615     C                   leavesr
043520150615     C                   endif
043530010223     C                   endif
043540010223
043550010223     C                   endsr
043560010117      **********************************************************************
043570010117      * CONTROLLI     * cliente ritiro *
043580010117      **********************************************************************
043590010117     C     Sr_Contrclir  begsr
043600010117
043610161026     c                   eval      wModCodCra = *off
043620010117     C                   eval      ds_v1cra1 = v1cra1
043630010117     C                   eval      ds_v1cra2 = v1cra2
043640010117     C                   eval      ds_v1cra3 = v1cra3
043650010117      * Obbligatorio
043660010117     C                   if        ds_v1ccra = *zeros
043670010117     C                             and v1rsr = *blanks
043680010117     C                   movel     msg(1)        v1cmsg
043690010117     C                   seton                                        41  28
043700130712     c                   eval      *in07 = *off
043710010117     C                   goto      endsrclir
043720010117     C                   endif
043730010117      * Inserito codice
043740010117     C                   if        ds_v1ccra <> *zeros
043750010117     C                   eval      *in07 = *on
043760010117     C                   exsr      Sr_Contrcra
043770010117     C   28              goto      endsrclir
043780131114      * Se variato codice recupero i dati da anagrafica clienti ritiro
043790010117     C                   if        ds_v1ccra <> sav_v1ccra
043800160325     c                   eval      wModCodCra = *on
043810160208     c                   eval      SoloUnaVolta = *off
043820010117     C                   exsr      Sr_Caranacr
043830070920     C  nkf              eval      sav_v1ccra = ds_v1ccra
043840010417     C                   eval      *in89 = *off
043850131114      /free
043860131114       //?Se NON variato codice e sto facendo conferma ORM fissi
043870131114       //?devo recuperare da anagrafica clienti ritiro gli orari di apertura
043880160208       //?ma solo una volta, poi l'utente può fare ciò che vuole
043890131114         ELSE;
043900160208           IF  §RMtco = 'S' and not SoloUnaVolta;
043910131125             exsr CaricaOrari;
043920160208             SoloUnaVolta = *on;
043930131125           ENDIF;
043940131114      /end-free
043950010117     C                   endif
043960010117     C                   else
043970160325     c                   IF        sav_v1ccra <> 0
043980160325     c                   eval      wModCodCra = *on
043990160325     c                   ENDIF
044000010117      * Inserita ragione sociale
044010010117     C                   clear                   sav_v1ccra
044020010305     C                   clear                   sav_acrpoa
044030131022     C                   clear                   sav_acrorr
044040010413     C                   clear                   sav_v1pkg
044050150921     C                   clear                   sav_v1vlm
044060070920     C                   clear                   sav_acrcgi
044070161025     c                   clear                   sav_acrfcl
044080161025     c                   clear                   sav_acrfpk
044090161025     c                   clear                   sav_acrfmc
044100161025     c                   clear                   sav_acrfbn
044110161025     c                   clear                   sav_acrfbl
044120161025     c                   clear                   sav_acrfat
044130161025     c                   clear                   sav_acrfmt
044140010305     C                   exsr      Sr_Contrrsr
044150010117     C                   eval      *in07 = *off
044160010117     C                   endif
044170010117
044180010117     C     endsrclir     endsr
044190010201      **********************************************************************
044200010201      * CONTROLLI     * cliente ritiro *
044210010201      **********************************************************************
044220010201     C     Sr_Contrcra   begsr
044230010201
044240010201     C                   z-add     ds_v1ccra     kfnacr
044250151109     C     kfnacr        chain(n)  fnacr01l
044260010201     C                   if        %found(fnacr01l)
044270010530     C                   if        ACRatb <> *blanks
044280010201     C                   movel     msg(2)        v1cmsg
044290010201     C                   seton                                        41  28
044300010201     C                   goto      endsrccra
044310010201     C                   endif
044320010502     C                   if        ACRtcr = 'M'
044330010502     C                   movel     msg(69)       v1cmsg
044340010502     C                   seton                                        41  28
044350010502     C                   goto      endsrccra
044360010502     C                   endif
044370140120
044380140120      /free
044390140120         dACR01 = ACRmai;
044400140120      /end-free
044410140120
044420010201      * Controllo se posso modificare il cliente ritiro
044430010201     C                   movel     ACRpoa        alf_acrpoa
044440010201     C                   if        alf_acrpoa <> v1cpor
044450010201     C                             and ds_v1ccra <> sav_v1ccra
044460010201     C                             and *in09 = *on
044470010201     C                   seton                                        41  28
044480010201     C                   movel     msg(40)       v1cmsg
044490010201     C                   eval      ds_v1ccra = sav_v1ccra
044500010201     C                   eval      v1cra1 = ds_v1cra1
044510010201     C                   eval      v1cra2 = ds_v1cra2
044520010201     C                   eval      v1cra3 = ds_v1cra3
044530010201     C                   goto      endsrccra
044540010201     C                   endif
044550010201
044560010906      * Se modificato il cliente pulisco il ksc
044570010906     C                   if        v1pag = 'M'
044580010906     C                             and (ds_v1ccra <> sav_v1ccra
044590030902     C                             or   v1pag <> sav_v1pag
044600040630     c                             or   v1ksc = *Zeros
044610040630     c                             or   v1ctr = *Blanks)
044620030902      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocra mi carica il ksc
044630040630     c                   If        v1ksc = *zeros or v1ctr = *Blanks
044640030902     c                   Clear                   sav_V1pag
044650030902     c                   EndIf
044660010906     C                   clear                   v1ksc
044670010906     C                   clear                   v1ctr
044680010906     C                   endif
044690010201     C                   exsr      Sr_Decocra
044700010201     C                   else
044710050426     c                   If        v1rsr = *Blanks
044720050426     c                   Eval      *In07 = *Off
044730050426     c                   EndIf
044740010201     C                   movel     msg(2)        v1cmsg
044750010201     C                   seton                                        41  28
044760010201     C                   endif
044770140422
044780140422      /free
044790140422       //?Recupero il ntw del mittente
044800140422         clear m_OG143;
044810140422         chain  v1cra1 AZORG01L;
044820140422         IF  %found(AZORG01L) and ORGfva = *blanks;
044830140422           m_OG143 = ORGde3;
044840140422         ENDIF;
044850140422      /end-free
044860010201
044870010201     C     endsrccra     endsr
044880010201      **********************************************************************
044890010201      * DECODIFICHE     * cliente ritiro *
044900010201      **********************************************************************
044910010201     C     Sr_Decocra    begsr
044920010201
044930010201     C                   movel     ACRrsr        v1rsr
044940010201     C                   movel     ACRinr        v1inr
044950010201     C                   movel     ACRcar        v1car
044960010201     C                   movel     ACRlor        v1lor
044970010201     C                   movel     ACRprr        v1prr
044980010201     C                   movel     ACRnar        v1nar
044990010308      * Se paga mittente memorizzo il codice piano dei conti
045000010308     C                   if        v1pag = 'M'
045010010906     C                             and (ds_v1ccra <> sav_v1ccra
045020010906     C                             or   v1pag <> sav_v1pag)
045030010906     C                             and  v1ksc = *zeros
045040010308     C                   eval      v1ksc = ACRksc
045050010507     C  nkf              eval      sav_v1ksc = ACRksc
045060010309     C                   eval      sav_v1pag = v1pag
045070040629      * memorizzo anche il codice tariffa
045080040629     c                   If        AcrCcc = 999
045090040629     c                   Clear                   V1Ctr
045100040629     c                   Else
045110040629     c                   Move      AcrCcc        V1Ctr
045120040629     c                   EndIf
045130010514     C                   endif
045140010201
045150010201     C                   endsr
045160010122      **********************************************************************
045170010122      * CARICO DATI DA ANAGRAFICO RITIRI
045180010122      **********************************************************************
045190010122     C     Sr_Caranacr   begsr
045200010122
045210010122      * Se non è richiamato carica i dati dall'anagrafico clienti
045220010723     C                   if        §RMtla = *blanks
045230010122     C                   movel     ACRper        v1rer
045240010122     C                   movel     ACRtel        v1ter
045250010122     C                   movel     ACRntm        v1nam
045260010207     C                   movel     ACRspi        v1spi
045270050128      * se note immesse dall'utente ora le sostituisco con quelle dell'anagrafica
045280050128      * msg. info
045290050128     c                   If        acrno1 <> *Blanks
045300050128     c                   If        v1not1 <> *Blanks
045310050128     c                   Eval      v1cmsg = msg(85)
045320050128     c                   Eval      *In28 = *On
045330050128     c                   Eval      *In82 = *On
045340050128     c                   EndIf
045350050128     c                   movel     ACRno1        v1not1
045360050128     c                   EndIf
045370050128     c                   If        acrno2 <> *Blanks
045380050128     c                   If        v1not2 <> *Blanks
045390050128     c                   Eval      v1cmsg = msg(85)
045400050128     c                   Eval      *In28 = *On
045410050128     c                   Eval      *In82 = *On
045420050128     c                   EndIf
045430050128     c                   movel     ACRno2        v1not2
045440050128     c                   EndIf
045450010723     C                   else
045460010723      * Se è richiamato carico i dati non immessi
045470010723     C                   if        v1rer = *blanks
045480010723     C                   movel     ACRper        v1rer
045490010723     C                   endif
045500010723     C                   if        v1ter = *blanks
045510010723     C                   movel     ACRtel        v1ter
045520010723     C                   endif
045530010723     C                   if        v1nam = *blanks
045540010723     C                   movel     ACRntm        v1nam
045550010723     C                   endif
045560010723     C                   if        v1not1 = *blanks
045570010723     C                   movel     ACRno1        v1not1
045580010723     C                   endif
045590010723     C                   if        v1not2 = *blanks
045600010723     C                   movel     ACRno2        v1not2
045610010723     C                   endif
045620010723     C                   if        v1spi = *blanks
045630010723     C                   movel     ACRspi        v1spi
045640010723     C                   endif
045650010723     C                   endif
045660010723
045670010723     C                   movel     ACRpoa        sav_ACRpoa
045680131113      /free
045690131113       //?Recupero gli orari di apertura
045700131114         exsr CaricaOrari;
045710131113      /end-free
045720010122
045730010122      * Recupera i dati del cliente ordinante se inserito
045740010122     C                   if        ACRcco <> *zeros
045750010122     C                   z-add     ACRcco        ds_v1ccor
045760010122     C                   eval      v1cor1 = ds_v1cor1
045770010122     C                   eval      v1cor2 = ds_v1cor2
045780010122     C                   eval      v1cor3 = ds_v1cor3
045790010122     C                   exsr      Sr_Contrcor
045800010122     C                   endif
045810010227
045820010227      * Recupera l'ora del ritiro se non già inserita
045830161114     C*********          if        v1orr = *zeros
045840161114     c                   IF        ACRorr > *zeros
045850010227     C                   z-add     ACRorr        v1orr
045860131022     C                   z-add     ACRorr        sav_ACRorr
045870161114     c                   ENDIF
045880161114     C*********          endif
045890010122
045900010122      * Controlla i flag per posizionamento cursore
045910010122     C                   eval      *in43 = (ACRfcl = 'O')
045920010122     C   43              goto      endflg
045930010122     C                   eval      *in44 = (ACRfpk = 'O')
045940010122     C   44              goto      endflg
045950010122     C                   eval      *in45 = (ACRfmc = 'O')
045960010122     C   45              goto      endflg
045970010122     C                   eval      *in46 = (ACRfbn = 'O')
045980010122     C   46              goto      endflg
045990010122     C                   eval      *in47 = (ACRfbl = 'O')
046000010122     C   47              goto      endflg
046010010122     C                   eval      *in48 = (ACRfat = 'O')
046020010122     C   48              goto      endflg
046030010122     C                   eval      *in49 = (ACRfmt = 'O')
046040010122     C   49              goto      endflg
046050010122     C                   eval      *in43 = (ACRfcl = 'F')
046060010122     C   43              goto      endflg
046070010122     C                   eval      *in44 = (ACRfpk = 'F')
046080010122     C   44              goto      endflg
046090010122     C                   eval      *in45 = (ACRfmc = 'F')
046100010122     C   45              goto      endflg
046110010122     C                   eval      *in46 = (ACRfbn = 'F')
046120010122     C   46              goto      endflg
046130010122     C                   eval      *in47 = (ACRfbl = 'F')
046140010122     C   47              goto      endflg
046150010122     C                   eval      *in48 = (ACRfat = 'F')
046160010122     C   48              goto      endflg
046170010122     C                   eval      *in49 = (ACRfmt = 'F')
046180010122     C   49              goto      endflg
046190010122     C     endflg        tag
046200010122      * Carico i flag in campi di comodo
046210010122     C                   exsr      Sr_Carflag
046220070913
046230070913      * codice giro
046240070925     c                   clear                   v1ccgi
046250071031     c                   clear                   sav_acrcgi
046260010122
046270010702     C                   endsr
046280131114      /free
046290131114       //--------------------------------------------------------------
046300131114       //?Carico gli orari di apertura da anagrafica clienti ritiro.
046310131114       //--------------------------------------------------------------
046320131114       BEGSR CaricaOrari;
046330131114
046340131114         IF  %subst(ACRmai:1:16) <> *blanks;
046350140120           IF  %check(c_Digits:%subst(ACRmai:1:
046360140120               %len(§ACRoa1))) = *zeros;
046370140120             v1OraAMda = %int(§ACRoa1);
046380131114           ENDIF;
046390140120           IF  %check(c_Digits:%subst(ACRmai:%len(§ACRoa1)+1:
046400140120             %len(§ACRoa2))) = *zeros;
046410140120             v1OraAMa  = %int(§ACRoa2);
046420131114           ENDIF;
046430140120           IF  %check(c_Digits:%subst(ACRmai:%len(§ACRoa1)+
046440140120               %len(§ACRoa2)+1:
046450140120               %len(§ACRoa3))) = *zeros;
046460140120             v1OraPMda = %int(§ACRoa3);
046470131114           ENDIF;
046480140120           IF  %check(c_Digits:%subst(ACRmai:%len(§ACRoa1)+
046490140120               %len(§ACRoa2)+
046500140120               %len(§ACRoa3)+1:
046510140120               %len(§ACRoa4))) = *zeros;
046520140120             v1OraPMa  = %int(§ACRoa4);
046530131114           ENDIF;
046540131114         ENDIF;
046550131114
046560131114       ENDSR;
046570131114      /end-free
046580131114
046590010122      **********************************************************************
046600010122      * CARICA FLAG COLLI - PESO - VOLUME - ETC ETC
046610010122      **********************************************************************
046620010122     C     Sr_Carflag    begsr
046630010122
046640010122     C                   eval      sav_acrfcl = ACRfcl
046650010122     C                   eval      sav_acrfpk = ACRfpk
046660010122     C                   eval      sav_acrfmc = ACRfmc
046670010122     C                   eval      sav_acrfbn = ACRfbn
046680010122     C                   eval      sav_acrfbl = ACRfbl
046690010122     C                   eval      sav_acrfat = ACRfat
046700010122     C                   eval      sav_acrfmt = ACRfmt
046710010122
046720010122     C                   endsr
046730001016      **********************************************************************
046740001016      * CONTROLLI     * dati cliente ritiro *
046750001016      **********************************************************************
046760001016     C     Sr_Contrrsr   begsr
046770001016
046780001016     C                   clear                   FNLV14DS
046790001016     C                   z-add     *date         I14dri
046800001016     C                   movel     v1rsr         I14rsc
046810001016     C                   movel     v1inr         I14ind
046820001103     C                   movel(p)  v1lor         E14loc
046830001016     C                   movel     v1prr         E14prv
046840001016     C                   movel     v1car         E14cap
046850001016     C                   movel     v1nar         E14nar
046860130507
046870130507      * mi memorizzo che non ho passato il cap e la provincia
046880161108     c                   IF        §rmfpr <> 'C'
046890130507     c                   IF        E14cap = *blanks and E14prv = *blanks
046900130507     c                   eval      wintcap = *on
046910130507     c                   ELSE
046920130507     c                   eval      wintcap = *off
046930130507     c                   ENDIF
046940161108     c                   ENDIF
046950161109      /free
046960161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
046970161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
046980161109       //?il pgm di interrogazione cappario
046990161109         IF  ConfAuto;
047000161109           I14batch = 'S';
047010161109         ENDIF;
047020161109      /end-free
047030130507
047040001016     C                   call      'FNLV14R'
047050001016     C                   parm                    KPJBA
047060001016     C                   parm                    FNLV14DS
047070001016
047080001016     C                   movel     I14ind        v1inr
047090001016     C                   movel     E14loc        v1lor
047100001016     C                   movel     E14prv        v1prr
047110001016     C                   movel     E14cap        v1car
047120001016     C                   movel     E14nar        v1nar
047130001016
047140001016     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
047150001016     C                   eval      *in61 = (O14err = '2')                       Indirizzo
047160001016     C                   eval      *in62 = (O14err = '5')                       CAP
047170001016     C                   eval      *in63 = (O14err = '3')                       Località
047180001016     C                   eval      *in64 = (O14err = '4')                       Provincia
047190001016     C                   eval      *in65 = (O14err = '6')                       Nazione
047200001016
047210001016     C                   if        O14msg <> *blanks
047220001016     C                   movel(p)  O14msg        v1cmsg
047230001016     C                   eval      *in28 = *on
047240001023     C                   eval      *in50 = *on
047250001016     C                   goto      endsrcrsr
047260001023     C                   endif
047270001023      * Controlla indirizzo
047280001023     C                   if        v1car <> *blanks or v1lor <> *blanks or
047290001023     C                             v1prr <> *blanks or v1nar <> *blanks
047300150921     c                   eval      sav_v1pkg = wpkg
047310150921     c                   eval      sav_v1vlm = wvlm
047320001023     C                   exsr      Sr_Contrcapr
047330001023     C                   eval      *in50 = (*in28 = *on)
047340001016     C   28              goto      endsrcrsr
047350010202     C                   endif
047360010207      * Se paga il mittente devo pulire il codice cliente che paga
047370010329     C                   if        v1pag = 'M'
047380010507     C                             and (v1pag <> sav_v1pag
047390010906     C                             or   sav_v1ccra <> ds_v1ccra)
047400010207     C                   clear                   v1ksc
047410010906     C                   clear                   v1ctr
047420010903     C  nkf              clear                   sav_v1ksc
047430010329     C                   eval      sav_v1pag = v1pag
047440010207     C                   endif
047450001016
047460001016     C     endsrcrsr     endsr
047470001023      **********************************************************************
047480001023      * CONTROLLI     * cap - località - provincia *
047490001023      **********************************************************************
047500001023     C     Sr_Contrcapr  begsr
047510001115
047520001115     C                   eval      *in32 = *off
047530131108     c                   clear                   sav_O95gf2
047540131111     c                   clear                   sav_O95lna
047550001023
047560001023     C                   clear                   TISI95DS
047570001023     C                   clear                   FNLV13DS
047580001023     C                   movel     ' '           I95tla
047590001023     C                   movel     '7'           I95tcn
047600001023     C                   movel     v1car         I95cap
047610001023     C                   movel     v1nar         I95nar
047620001023     C                   movel     v1inr         I95ind
047630001103     C                   movel(p)  v1lor         I95loc
047640001023     C                   movel     v1prr         I95prv
047650010413     C                   z-add     sav_v1pkg     I95lkg
047660150921     C                   z-add     sav_v1vlm     I95lmc
047670001023     C                   movel     'S'           I13af0
047680001023     C                   movel     'S'           I13af1
047690001023     C                   movel     'S'           I13sz2
047700001023     C                   movel     'S'           I13la3
047710001023     C                   movel     'S'           I13sz3
047720001023     C                   movel     'S'           I13cnv
047730081215      * se impostato network DPD lo passo
047740081215     c                   if        v1ntwd <> *blanks
047750081216     c                   eval      i95fi1 = v1ntwd
047760081215     c                   else
047770081215     c                   clear                   i95fi1
047780081215     c                   endif
047790001023
047800001023      * Gestione errori forzabili: se cambiato CAP/Località
047810001023      * riazzera i valori di errori forzabili
047820001023
047830010223      * CAP
047840010417     C     v1car         ifne      sav_v1car
047850010417     C                   eval      *in89 = *off
047860010417     C                   movel     v1car         sav_v1car
047870001023     C                   clear                   wfzv
047880001023     C                   clear                   wfz1
047890001023     C                   clear                   wfz2
047900001023     C                   clear                   wfz3
047910001115     C                   eval      *in32 = *on
047920061222     C                   endif
047930001023
047940001023      * Modifica località
047950001023
047960010118     C     v1lor         ifne      sav_v1lor
047970001023     C                   clear                   wfz1
047980001023     C                   clear                   wfz2
047990001023     C                   clear                   wfz3
048000010118     C                   movel     v1lor         sav_v1lor
048010001115     C                   eval      *in32 = *on
048020001023     C                   endif
048030001023
048040001023      * Modifica provincia
048050001023
048060010118     C     v1prr         ifne      sav_v1prr
048070001023     C                   clear                   wfz3
048080010118     C                   movel     v1prr         sav_v1prr
048090001115     C                   eval      *in32 = *on
048100001023     C                   endif
048110001023
048120001023     C                   movel     wfzv          E13fzv
048130001023     C                   movel     wfz1          E13fz1
048140001023     C                   movel     wfz2          E13fz2
048150001023     C                   movel     wfz3          E13fz3
048160040415      * se il pgm è richiamato e non deve far vedere le videate imposto wfv1 = '1'
048170040415     c                   If        §rmtla = '0'
048180040415     c                   Eval      E13fz1 = 1
048190040415     c                   EndIf
048200001025
048210001023     C                   call      'FNLV13R'
048220001023     C                   parm                    KPJBA
048230001023     C                   parm                    FNLV13DS
048240001023     C                   parm                    TISI95DS
048250131108
048260131108     c                   eval      sav_O95gf2 = O95gf2
048270131111     c                   eval      sav_O95lna = O95lna
048280131108
048290110121     c* Se sto copiando da altro orm per emettere la videata non
048300110121     c* gestisco eventuali errori: sarà poi nella seconda fase,
048310110121     c* dopo emissione della videata, che verrano effettuati
048320110121     c* i controlli.
048330110121     c                   if        $copia='S'
048340110121     c                   leavesr
048350110121     c                   endif
048360001023
048370001023      * Ricontrolla errori
048380001023
048390001023     C                   movel     O13err        werr
048400001023     C                   movel     O13msg        wmsg
048410001023     C                   movel     E13fzv        wfzv
048420001023     C                   movel     E13fz1        wfz1
048430001023     C                   movel     E13fz2        wfz2
048440001023     C                   movel     E13fz3        wfz3
048450001023
048460001023      * Se il livello di affidabilità non è inferiore a 2 e
048470001023      * il livello di reperimento dati non è inferiore a 3
048480001023      * reimposta i dati da TISI95DS
048490110120      * I dati non vengono sostituiti se i dati provengono da anagrafica
048500001023
048510110119     c                   if        ds_v1ccra=0
048520001023     C     O13ron        ifeq      'S'
048530110119     C                   movel     O95nar        v1nar
048540110119     c                   endif
048550110119     C     O13roc        ifeq      'S'
048560110119     C                   movel     O95cap        v1car
048570110119     c                   endif
048580110119     C     O13rop        ifeq      'S'
048590110119     C                   movel     O95prv        v1prr
048600110119     c                   endif
048610110119     C     O13rol        ifeq      'S'
048620001023     C                   movel     O95loc        v1lor
048630001023     C                   end
048640001129
048650140508      /free
048660140529       //?Se variato cap/località/provincia
048670140529       //?e sono in conferma ORM da VAO (dati inviati dal cliente errati)
048680140529       //?e se la data NON è maggiore di oggi
048690140529       //?e ci sono i presupposti per inviare l'alert
048700140529       //?imposto il flag così da ricalcolare la data ritiro
048710140529         IF  *in32 and §RMfpr = 'C' and
048720140529             old_invv1dar <= oggi_aammgg and wOKalert and
048730140529             werr = *blanks and wmsg = *blanks;
048740140529           wModLocRit = *on;
048750140529         ENDIF;
048760161116       //?Se variato cap/località/provincia
048770161116       //?e sono in immissione manuale
048780161116       //?imposto il flag così da ricalcolare la data ritiro
048790161116         IF  *in32 and (*in01 or *in03) and
048800161116            (V1Ctco = 'M' or V1Ctco = 'E') and
048810161116             werr = *blanks and wmsg = *blanks;
048820161116           wModLocRit = *on;
048830161116         ENDIF;
048840161109
048850161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
048860161109       //?conferma automatica batch non devo dare errore
048870161109         IF  ConfAuto and O13roc = 'S';
048880161109           clear werr;
048890161109           clear O13roc;
048900161109         ENDIF;
048910140508      /end-free
048920001023
048930001023     C     werr          ifne      ' '
048940001023     C     wmsg          andne     *blanks
048950061222     C     *in32         oreq      *on
048960001023
048970001023     C                   select
048980001023
048990001023     C     werr          wheneq    '3'
049000001023     C                   eval      *in63 = *on
049010001023     C                   eval      *in28 = *on
049020001023
049030001023     C     werr          wheneq    '5'
049040001023     C                   eval      *in62 = *on
049050001023     C                   eval      *in28 = *on
049060001023
049070001023     C     werr          wheneq    '4'
049080001023     C                   eval      *in64 = *on
049090001023     C                   eval      *in28 = *on
049100001023
049110001023     C     werr          wheneq    '6'
049120001023     C                   eval      *in65 = *on
049130001023     C                   eval      *in28 = *on
049140001023     C                   endsl
049150010702
049160110120     C                   movel(p)  wmsg          v1cmsg
049170001023     C                   end
049180110120     c                   else
049190110120     C     werr          ifne      ' '
049200110120     C     wmsg          andne     *blanks
049210110120     C                   eval      *in41 = *on
049220110120     C                   eval      *in28 = *on
049230110120     C                   movel(p)  msg(94)       v1cmsg
049240110120     c                   endif
049250110120     c                   endif
049260001023
049270001023     C   28              goto      endsrcapr
049280001023
049290001023      * Controlla se reimpostata località
049300001023
049310001023     C     O13rol        ifeq      'S'
049320001023     C                   eval      *in63 = *on
049330001023     C                   eval      *in28 = *on
049340001023     C                   movel     msg(19)       v1cmsg
049350001023     C                   goto      endsrcapr
049360001023     C                   end
049370001023
049380001023      * Controlla se reimpostata Provincia
049390001023
049400001023     C     O13rop        ifeq      'S'
049410001023     C                   eval      *in64 = *on
049420001023     C                   eval      *in28 = *on
049430001023     C                   movel     msg(21)       v1cmsg
049440001023     C                   goto      endsrcapr
049450001023     C                   end
049460001023
049470001023      * Controlla se reimpostato cap
049480001023
049490001023     C     O13roc        ifeq      'S'
049500001023     C                   eval      *in62 = *on
049510001023     C                   eval      *in28 = *on
049520001023     C                   movel     msg(20)       v1cmsg
049530001023     C                   goto      endsrcapr
049540001023     C                   end
049550001023
049560001023      * Controlla se reimpostata Nazione
049570001023
049580001023     C     O13ron        ifeq      'S'
049590001023     C                   eval      *in65 = *on
049600001023     C                   eval      *in28 = *on
049610001023     C                   movel     msg(22)       v1cmsg
049620001023     C                   goto      endsrcapr
049630001023     C                   end
049640001023
049650001023     C     endsrcapr     endsr
049660131024
049670131024      /free
049680131024       //--------------------------------------------------------------
049690131024       //?Calcolo la filiale ritiro.
049700131024       //--------------------------------------------------------------
049710131024       BEGSR calPOR;
049720131024
049730131024       //?Da ritiro fisso
049740131024         IF  V1Cpos <> 0 and V1Cpor = *blanks;
049750131024           V1Cpor = %editc(V1Cpos:'X');
049760131024         ENDIF;
049770131024
049780131024       //?Da Anagrafica clienti ritiro
049790131024         IF  sav_ACRpoa <> 0 and V1Cpor = *blanks;
049800131024           V1Cpor = %editc(sav_ACRpoa:'X');
049810131024         ENDIF;
049820131024
049830150922       //?Calcolo la filiale ritiro in base al peso/volume
049840150921         sav_V1pkg = wpkg;
049850150921         sav_V1vlm = wvlm;
049860150921         exsr sr_contrcapr;
049870131024
049880131024       //?Se c'è stato un errore nel controllo del cappario esco
049890131024         IF  O13err <> *blanks or *in28;
049900131024           leavesr;
049910131024         ENDIF;
049920131024
049930131024       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
049940131024         clear FIOR96ds;
049950150305       //?se la filiale emissione non è ancora stata impostata passo la filiale utente
049960131024         IF  V1Cpoe = *zeros;
049970150305           IOR96poe = DUTpou;
049980131024         ELSE;
049990131024           IOR96poe = V1Cpoe;
050000131024         ENDIF;
050010131111         IOR96por = sav_O95lna;
050020131024         fior96r(kpjba : fior96ds);
050030131024         IF  OOR96err = *blanks and OOR96por <> *zeros;
050040131111           sav_O95lna = OOR96por;
050050160209         //?se la filiale di ritiro è da forzare
050060160209         //?pulisco il campo V1Cpor così imposto per forza la nuova filiale forzata
050070160209         //?in questo modo forzo SEMPRE
050080160209           IF  IOR96por <> OOR96por;
050090131024             clear v1cpor;
050100131024           ENDIF;
050110131024         ENDIF;
050120131024
050130131024       //?Imposto la filiale ritiro a video
050140131024         IF V1Cpor = *blanks;
050150131111           V1Cpor = %editc(sav_O95lna:'X');
050160131024         ENDIF;
050170131024
050180131024       //?Se immissione/copia ORM e filiale ritiro è una filiale gestita
050190131024       //?imposto la filiale 'mamma' come filiale ritiro
050200131024         IF  *in01 or *in03 and wforzapor = *off;
050210131024           clear FNLV55DS;
050220131024           D55tpt = '6';
050230131024           D55lin = %dec(V1Cpor:3:0);
050240131024           D55drf = oggi_aammgg;
050250131024           fnlv55r (fnlv55ds);
050260131024           IF  D55err = *blanks;
050270131024             V1Cpor = %editc(D55tfa:'X');
050280131024             wforzapor = *on;
050290131024           ENDIF;
050300131024         ENDIF;
050310131024
050320131024       //?Decodifico filiale ritiro
050330131030         clear OG143;
050340131030         clear OG148;
050350131030         clear ntw_V1Cpor;
050360131024         kazorg = %dec(V1Cpor:3:0);
050370131024         chain kazorg AZORG01L;
050380131024         IF  not %found(AZORG01L);
050390131024           V1Cmsg = msg(33);
050400131024           *in28 = *on;
050410131024           *in59 = *on;
050420131024           leavesr;
050430131024         ENDIF;
050440131024         IF  ORGfva <> *blanks or
050450131024            (ORGfag <> 'A' and ORGfag <> 'F');
050460131024           V1Cmsg = msg(33);
050470131024           *in28 = *on;
050480131024           *in59 = *on;
050490131024           leavesr;
050500131024         ENDIF;
050510131024
050520131024         OG143 = ORGde3;
050530131024         OG148 = ORGde8;
050540131030         ntw_V1Cpor = §OGntw;
050550131024
050560131024       //?Se filiale ritiro non ha la procedura ORM attiva blocco
050570131024         IF  §OGorm <> 'S';
050580131024           V1Cmsg = msg(36);
050590131024           *in28 = *on;
050600131024           *in59 = *on;
050610131024           leavesr;
050620131024         ENDIF;
050630150310
050640150310       //?Imposto se filiale ritiro ha la procedura PDA attiva
050650150310         *in22 = (§ogpdaorm <> *blanks);
050660131024
050670131024         V1Dpor = ORGdes;
050680131024         num_V1Cpor = %dec(V1Cpor:3:0);
050690161116         IF  sav_V1Cpor <> *zeros and sav_V1Cpor <> *blanks and
050700161116             V1Cpor <> sav_V1Cpor;
050710161116           wModPor = *on;
050720161116         ENDIF;
050730131024         sav_V1Cpor = V1Cpor;
050740131024
050750131024       ENDSR;
050760131024
050770131024      /end-free
050780131024
050790001010      **********************************************************************
050800001010      * CONTROLLI     * colli - peso - volume - etc etc *
050810001010      **********************************************************************
050820001010     C     Sr_Contrqta   begsr
050830001010
050840001023      * Se il flag è F può non essere inserita la qtà
050850001023      * Se il flag è O la qtà deve essere inserita
050860010118     C                   if        sav_acrfcl <> *blanks and v1ncl = *zeros
050870010118     C                   if        sav_acrfcl = 'F' and *in66 = *off
050880001012     C                   movel     msg(25)       v1cmsg
050890001012     C                   seton                                        436628
050900001012     C                   goto      endsrcqta
050910001012     C                   endif
050920010118     C                   if        sav_acrfcl = 'O'
050930001010     C                   movel     msg(4)        v1cmsg
050940001010     C                   seton                                        43  28
050950001012     C                   goto      endsrcqta
050960001012     C                   endif
050970010903     C                   else
050980010903     C                   setoff                                       43
050990001011     C                   endif
051000001010
051010010118     C                   if        sav_acrfpk <> *blanks and v1pkg = *zeros
051020010118     C                   if        sav_acrfpk = 'F' and *in67 = *off
051030001012     C                   movel     msg(26)       v1cmsg
051040001012     C                   seton                                        446728
051050001012     C                   goto      endsrcqta
051060001012     C                   endif
051070010118     C                   if        sav_acrfpk = 'O'
051080001010     C                   movel     msg(5)        v1cmsg
051090001010     C                   seton                                        44  28
051100001012     C                   goto      endsrcqta
051110001012     C                   endif
051120010903     C                   else
051130010903     C                   setoff                                       44
051140001010     C                   endif
051150130508
051160130508     C                   if        sav_acrfbn <> *blanks and v1bnc = *zeros
051170130508     C                   if        sav_acrfbn = 'F' and *in69 = *off
051180130508     C                   movel     msg(28)       v1cmsg
051190130508     C                   seton                                        466928
051200130508     C                   goto      endsrcqta
051210130508     C                   endif
051220130508     C                   if        sav_acrfbn = 'O'
051230130508     C                   movel     msg(7)        v1cmsg
051240130508     C                   seton                                        46  28
051250130508     C                   goto      endsrcqta
051260130508     C                   endif
051270130508     C                   else
051280130508     C                   setoff                                       46
051290130508     C                   endif
051300001010
051310010118     C                   if        sav_acrfmc <> *blanks and v1vlm = *zeros
051320010118     C                   if        sav_acrfmc = 'F' and *in68 = *off
051330001012     C                   movel     msg(27)       v1cmsg
051340001012     C                   seton                                        456828
051350001012     C                   goto      endsrcqta
051360001012     C                   endif
051370010118     C                   if        sav_acrfmc = 'O'
051380001010     C                   movel     msg(6)        v1cmsg
051390001010     C                   seton                                        45  28
051400001012     C                   goto      endsrcqta
051410001012     C                   endif
051420010903     C                   else
051430010903     C                   setoff                                       45
051440001010     C                   endif
051450010903
051460010213      * Se prepagato obbligatorio colli e peso
051470010213     C                   if        v1ctor = 'P' and
051480010213     C                             (v1ncl = *zeros or v1pkg = *zeros)
051490010213     C                   movel     msg(54)       v1cmsg
051500010213     C                   seton                                        44  28
051510010213     C                   goto      endsrcqta
051520010213     C                   endif
051530130320
051540130320      * Immissione ORM manuale voglio i colli
051550130321      * o i bancali
051560141211     c                   IF        *in01 and
051570141212     c                             (V1Ctco = 'M' or V1Ctco = 'E') and
051580130321     c                             V1ncl = *zeros and V1bnc = *zeros
051590130320     c                   eval      V1Cmsg = msg(4)
051600130321     c                   eval      V1Cmsg = %trim(V1Cmsg) + ' o i bancali.'
051610130320     c                   eval      *in28 = *on
051620130320     c                   eval      *in43 = *on
051630130320     c                   leavesr
051640130320     c                   ENDIF
051650010109
051660010109      * Almeno 1 tra volume peso e bancali deve essere inserito
051670010417     C                   if        v1pkg = *zeros and v1vlm = *zeros
051680010417     C                             and v1bnc = *zeros
051690010109     C                   movel     msg(37)       v1cmsg
051700010109     C                   seton                                        44  28
051710010109     C                   goto      endsrcqta
051720010205     C                   else
051730010205     C                   setoff                                       44
051740010109     C                   endif
051750081126
051760160707      /free
051770160921       //?Controllo i limiti previsti per le quantità
051780160920       //?Richiamo pgm controllo limiti quantità x colli/peso/volume
051790160920         clear TRUL73DS;
051800160920         I73tsp = 'C';
051810160920         I73ncl = V1ncl;
051820160920         I73psk = V1pkg;
051830160920         I73vmc = V1vlm;
051840160920         trul73r (trul73ds);
051850160920       //?Limite colli bloccante
051860160921         SELECT;
051870160921         WHEN  O73fclm = '1';
051880160920           *in28 = *on;
051890160920           *in43 = *on;
051900160920           V1Cmsg = 'Il numero colli massimo consentito è';
051910160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lclm:'4'));
051920160920           leavesr;
051930160920       //?Limite colli forzabile
051940160921         WHEN  O73fclf = '1' and V1ncl <> wold_V1ncl;
051950160920           *in28 = *on;
051960160920           *in43 = *on;
051970160920           V1Cmsg = 'Il numero colli massimo consentito è';
051980160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lclf:'4')) +
051990160921                    '. Enter x forzare.';
052000160921           wold_V1ncl = V1ncl;
052010160920           leavesr;
052020160920       //?Limite peso bloccante
052030160921         WHEN  O73fkgm = '1';
052040160921           *in28 = *on;
052050160921           *in44 = *on;
052060160921           V1Cmsg = 'Il peso massimo consentito è';
052070160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lkgm:'4'));
052080160921           leavesr;
052090160921       //?Limite peso forzabile
052100160921         WHEN  O73fkgf = '1' and V1pkg <> wold_V1pkg;
052110160921           *in28 = *on;
052120160921           *in44 = *on;
052130160921           V1Cmsg = 'Il peso massimo consentito è';
052140160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lkgf:'4')) +
052150160921                    '. Enter x forzare.';
052160160921           wold_V1pkg = V1pkg;
052170160921           leavesr;
052180160920       //?Limite volume bloccante
052190160921         WHEN  O73fmcm = '1';
052200160921           *in28 = *on;
052210160921           *in45 = *on;
052220160921           V1Cmsg = 'Il volume massimo consentito è';
052230160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lmcm:'4'));
052240160921           leavesr;
052250160921       //?Limite volume forzabile
052260160921         WHEN  O73fmcf = '1' and V1vlm <> wold_V1vlm;
052270160921           *in28 = *on;
052280160921           *in45 = *on;
052290160921           V1Cmsg = 'Il volume massimo consentito è';
052300160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lmcf:'4')) +
052310160921                    '. Enter x forzare.';
052320160921           wold_V1vlm = V1vlm;
052330160921           leavesr;
052340160920         ENDSL;
052350160922       //?Controllo i limiti previsti per le quantità
052360160922       //?Richiamo pgm controllo limiti quantità x bancali/bilico/autoreno/motrice
052370160922         clear TRUL731DS;
052380160922         I731bnc = V1bnc;
052390160922         I731blc = V1blc;
052400160922         I731att = V1att;
052410160922         I731mtc = V1mtc;
052420160922         trul731r (trul731ds);
052430160922       //?Limite bancali bloccante
052440160922         SELECT;
052450160922         WHEN  O731fbncm = '1';
052460160922           *in28 = *on;
052470160922           *in46 = *on;
052480160922           V1Cmsg = 'Il numero bancali massimo consentito è';
052490160922           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O731lbncm:'4'));
052500160922           leavesr;
052510160922       //?Limite bancali forzabile
052520160922         WHEN  O731fbncf = '1' and V1bnc <> wold_V1bnc;
052530160922           *in28 = *on;
052540160922           *in46 = *on;
052550160922           V1Cmsg = 'Il numero bancali massimo consentito è';
052560160922           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O731lbncf:'4')) +
052570160922                    '. Enter x forzare.';
052580160922           wold_V1bnc = V1bnc;
052590160922           leavesr;
052600160922         ENDSL;
052610160921
052620160707      /end-free
052630001010
052640001012     C     endsrcqta     endsr
052650130508      **********************************************************************
052660130508      * CONTROLLI     * bilico - motrice - autotreno  *
052670130508      **********************************************************************
052680130508     C     Sr_Contrqta1  begsr
052690130508
052700130508      * Se il flag è F può non essere inserita la qtà
052710130508      * Se il flag è O la qtà deve essere inserita
052720130508     C                   if        sav_acrfbl <> *blanks and v1blc = *zeros
052730130508     C                   if        sav_acrfbl = 'F' and *in70 = *off
052740131210     C                   movel     msg(29)       w2cmsg
052750130508     C                   seton                                        477028
052760130508     C                   leavesr
052770130508     C                   endif
052780130508     C                   if        sav_acrfbl = 'O'
052790131030     C                   movel     msg(8)        w2cmsg
052800130508     C                   seton                                        47  28
052810130508     C                   leavesr
052820130508     C                   endif
052830130508     C                   else
052840130508     C                   setoff                                       47
052850130508     C                   endif
052860130508
052870130508     C                   if        sav_acrfat <> *blanks and v1att = *zeros
052880130508     C                   if        sav_acrfat = 'F' and *in71 = *off
052890131030     C                   movel     msg(30)       w2cmsg
052900130508     C                   seton                                        487128
052910130508     C                   leavesr
052920130508     C                   endif
052930130508     C                   if        sav_acrfat = 'O'
052940131030     C                   movel     msg(9)        w2cmsg
052950130508     C                   seton                                        48  28
052960130508     C                   leavesr
052970130508     C                   endif
052980130508     C                   else
052990130508     C                   setoff                                       48
053000130508     C                   endif
053010130508
053020130508     C                   if        sav_acrfmt <> *blanks and v1mtc = *zeros
053030130508     C                   if        sav_acrfmt = 'F' and *in72 = *off
053040131030     C                   movel     msg(31)       w2cmsg
053050130508     C                   seton                                        497228
053060130508     C                   leavesr
053070130508     C                   endif
053080130508     C                   if        sav_acrfmt = 'O'
053090131030     C                   movel     msg(10)       w2cmsg
053100130508     C                   seton                                        49  28
053110130508     C                   leavesr
053120130508     C                   endif
053130130508     C                   else
053140130508     C                   setoff                                       49
053150130508     C                   endif
053160130508
053170130508      * se conferma ORM da clienti msg info se inseriti almeno 1 autotreno o
053180130508      * 1 bilico o 1 motrice
053190130508     c                   if        §rmfpr = 'C' and
053200130508     c                             (v1blc > 0 or v1att > 0 or v1mtc > 0) and
053210130508     c                             wforzaqta = *off
053220130508     c                   eval      *in28 = *on
053230131030     c                   eval      w2cmsg = 'ATTENZIONE! Inserito'
053240130508     c                   select
053250130508     c                   when      v1blc > 0
053260130508     c                   eval      *in47 = *on
053270131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' bilico.'
053280130508     c                   when      v1att > 0
053290130508     c                   eval      *in48 = *on
053300131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' autotreno.'
053310130508     c                   when      v1mtc > 0
053320130508     c                   eval      *in49 = *on
053330131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' motrice.'
053340130508     c                   endsl
053350131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' Enter per forzare'
053360130508     c                   eval      wforzaqta = *on
053370130508     c                   leavesr
053380130508     c                   endif
053390160922      /free
053400160922       //?Controllo i limiti previsti per le quantità
053410160922       //?Richiamo pgm controllo limiti quantità x bancali/bilico/autoreno/motrice
053420160922         clear TRUL731DS;
053430160922         I731bnc = V1bnc;
053440160922         I731blc = V1blc;
053450160922         I731att = V1att;
053460160922         I731mtc = V1mtc;
053470160922         trul731r (trul731ds);
053480160922       //?Limite bilico bloccante
053490160922         SELECT;
053500160922         WHEN  O731fblcm = '1';
053510160922           *in28 = *on;
053520160922           *in47 = *on;
053530160922           W2Cmsg = 'Il numero bilici massimo consentito è';
053540160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lblcm:'4'));
053550160922           leavesr;
053560160922       //?Limite bilico forzabile
053570160922         WHEN  O731fblcf = '1' and V1blc <> wold_V1blc;
053580160922           *in28 = *on;
053590160922           *in47 = *on;
053600160922           W2Cmsg = 'Il numero bilici massimo consentito è';
053610160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lblcf:'4')) +
053620160922                    '. Enter x forzare.';
053630160922           wold_V1blc = V1blc;
053640160922           leavesr;
053650160922       //?Limite autotreno bloccante
053660160922         WHEN  O731fattm = '1';
053670160922           *in28 = *on;
053680160922           *in48 = *on;
053690160922           W2Cmsg = 'Il numero autrotreni massimo consentito è';
053700160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lattm:'4'));
053710160922           leavesr;
053720160922       //?Limite autotreno forzabile
053730160922         WHEN  O731fattf = '1' and V1att <> wold_V1att;
053740160922           *in28 = *on;
053750160922           *in48 = *on;
053760160922           W2Cmsg = 'Il numero autotreni massimo consentito è';
053770160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lattf:'4')) +
053780160922                    '. Enter x forzare.';
053790160922           wold_V1att = V1att;
053800160922           leavesr;
053810160922       //?Limite motrice bloccante
053820160922         WHEN  O731fmtcm = '1';
053830160922           *in28 = *on;
053840160922           *in49 = *on;
053850160922           W2Cmsg = 'Il numero motrici massimo consentito è';
053860160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lmtcm:'4'));
053870160922           leavesr;
053880160922       //?Limite motrice forzabile
053890160922         WHEN  O731fmtcf = '1' and V1mtc <> wold_V1mtc;
053900160922           *in28 = *on;
053910160922           *in49 = *on;
053920160922           W2Cmsg = 'Il numero motrice massimo consentito è';
053930160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lmtcf:'4')) +
053940160922                    '. Enter x forzare.';
053950160922           wold_V1mtc = V1mtc;
053960160922           leavesr;
053970160922         ENDSL;
053980160922      /end-free
053990130508
054000130508     C                   endsr
054010001012      **********************************************************************
054020001016      * CONTROLLI     * data ritiro *
054030001012      **********************************************************************
054040001016     C     Sr_Contrdar   begsr
054050131107
054060131107      /free
054070131107       //?Se non ho mai richiamato il calcolo della data ritiro lo faccio ora
054080131107       //?in questo modo evito errori di dati decimali nel caso in cui
054090131107       //?l'utente riempe tutta la videata senza mai dare enter ed imposta anche
054100131107       //?la data di ritiro
054110131107         IF  not wokOR97;
054120131107           ExSr Sr_CalPikup;
054130131107           wokOR97 = *on;
054140131107         ENDIF;
054150161108       //?Se non ho ancora visualizzato gli orari devo farlo, ma solo se conferma da VAO
054160161108       //?potrei emettere un errore sulla data e averla ancora visualizzata
054170161108         IF  §RMfpr = 'C' and V1dar > *zeros and *in95 = *off;
054180161108           *in95 = *on;
054190161108         ENDIF;
054200131107      /end-free
054210010201
054220010305      * Se non impostata mette quella calcolata
054230010302     C                   if        v1dar = *zeros
054240010201     C                   eval      v1dar = new_v1dar
054250010202     C                   endif
054260161108
054270161108       //?Alcuni controlli che seguono sono superflui dato che la data ritiro è di solo output
054280161108       //?ma preferisco lasciarli che non si sa mai
054290001012
054300010202      * Data obbligatoria
054310010202     C                   if        v1dar = *zeros
054320161108     C                   seton                                        28
054330010202     C                   movel     msg(34)       v1cmsg
054340010202     C                   goto      endsrcdar
054350010202     C                   endif
054360010202
054370010202      * Controlla
054380010116     C                   clear                   inv_v1dar
054390001016     C                   clear                   wlbdat
054400001016     C                   z-add     v1dar         G02dat
054410001016     C                   call      'XSRDA8'
054420001016     C                   parm                    wlbdat
054430001016     C                   if        G02err = '1'
054440161108     C                   seton                                        28
054450001016     C                   movel     msg(35)       v1cmsg
054460010202     C                   goto      endsrcdar
054470010202     C                   endif
054480001025     C                   z-add     G02dat        v1dar
054490010116     C                   z-add     G02inv        inv_v1dar
054500130919
054510130919      * Controllo se modificata dall'utente spengo gli indicatori di comodo
054520130919      * per i msg info
054530130919     c                   IF        old_invv1dar <> inv_v1dar
054540130919     c                   eval      *in40 = *off
054550130919     c                   eval      wforzadar1 = *off
054560130919     c                   eval      old_invv1dar = inv_v1dar
054570130919     c                   ENDIF
054580050304
054590050304      * controllo se supera la data max x errore bloccante
054600130918     c                   If        inv_v1dar > OOR97dmaxb
054610050304     c                   Eval      *In28 = *On
054620050304     c                   Eval      v1cmsg = msg(35)
054630050304     c                   Eval      v1cmsg = %trim(v1cmsg) + ' supera ' +
054640050304     c                             d§dftgg2 + ' gg. dalla data immissione ORM'
054650050304     c                   Goto      endsrcdar
054660050304     c                   EndIf
054670050304
054680050304      * controllo se supera la data max x errore info
054690130918     c                   If        inv_v1dar > OOR97dmaxf and wforzadar1 = *Off
054700050304     c                   Eval      *In28 = *On
054710050304     c                   Eval      v1cmsg = msg(35)
054720050304     c                   Eval      v1cmsg = %trim(v1cmsg) + ' supera ' +
054730050304     c                             d§dftgg1 + ' gg. dalla data immissione ORM.-
054740050304     c                              ENTER X FORZARE!'
054750050304     c                   Eval      wforzadar1 = *On
054760050304     c                   Goto      endsrcdar
054770050304     c                   EndIf
054780010302
054790010302      * Non può essere inferiore alla data immissione orm
054800010302     C                   if        inv_v1dar < inv_v1dao
054810161108     C                   seton                                        28
054820010305     C                   movel     msg(65)       v1cmsg
054830010302     C                   goto      endsrcdar
054840010302     C                   endif
054850161020      /free
054860161020       //?Non può essere inferiore alla Data Pronta Merce
054870161020         IF  inv_v1dar < inv_v1dpm;
054880161020           *in28 = *on;
054890161020           V1Cmsg = 'La Data Ritiro non può essere inferiore +
054900161020                     alla Data Pronta Merce';
054910161020           leavesr;
054920161020         ENDIF;
054930161020      /end-free
054940010906
054950010906      * Se la data è stata ricalcolata emetto un messaggio
054960010906      * se non è stata modificata dall'utente
054970010907     C                   if        old_newv1dar = *zeros
054980010907     C                   eval      old_newv1dar = new_v1dar
054990010907     C                   endif
055000010907     C                   if        v1dar = new_v1dar
055010010907     C                             and new_v1dar <> old_newv1dar
055020010907     C                   eval      old_newv1dar = new_v1dar
055030161108     C                   seton                                        28
055040010906     C                   movel     msg(63)       v1cmsg
055050161026      /free
055060161026       //?Anche se c'è l'errore ricalcolo gli orari servizio
055070161026       //?in questo modo, se viene emesso il msg info del ricalcolo della data a causa
055080161026       //?della variazione di peso, facco già vedere i nuovi orari servizio, altrimenti
055090161026       //?verrebero visualizzati dopo ripristino tastiera ed enter sucessivi.
055100161026         exsr CercaOrari;
055110161026      /end-free
055120010906     C                   goto      endsrcdar
055130010906     C                   endif
055140130918
055150130918      /free
055160140319       //?Nuovi controlli per immissione ORM
055170170526         IF  *in01;
055180131015       //?Imposto 'A' a video se si può anticipare
055190140424           IF  OOR97dmin < OOR97dar;
055200140319             v1anticipa = 'A';
055210140319           ENDIF;
055220131015       //?se la data ritiro è uguale alla data minima tolgo la 'A'
055230140319           IF  inv_v1dar = OOR97dmin;
055240140319             clear v1anticipa;
055250140319           ENDIF;
055260140512       //?se la data ritiro è maggiore della data ritiro calcolata
055270140512       //?tolgo la 'A'
055280140512           IF  inv_V1dar > OOR97dar;
055290140512             clear v1anticipa;
055300140512           ENDIF;
055310140319       //?blocco se data ritiro < data minima e < data ritiro calcolata
055320140319           IF  inv_v1dar < OOR97dar and
055330140319               inv_V1dar < OOR97dmin;
055340130918             *in28 = *on;
055350130918             V1Cmsg = msg(51);
055360130918             leavesr;
055370130918           ENDIF;
055380140319         ENDIF;
055390130918      /end-free
055400010906
055410010305      * Msg info se trovo che è una festività
055420130918     C                   eval      ktfa = num_v1cpor
055430010305     C                   move      v1dar         ds_data
055440010305     C                   eval      kann = ds_anno
055450010305     C                   eval      kmes = ds_mese
055460010305     C     kazcln        chain     azcln01l
055470010305     C                   if        %found(azcln01l)
055480010305     C                   if        (MAT(ds_giorno) =  'F'
055490010305     C                             or  POM(ds_giorno) =  'F')
055500010305     C                             and *in40 = *off
055510161108     C                   seton                                        4028
055520010305     C                   movel     msg(66)       v1cmsg
055530010305     C                   goto      endsrcdar
055540010305     C                   endif
055550010305     C                   endif
055560001012
055570010202     C     endsrcdar     endsr
055580040615      **********************************************************************
055590040615      * CONTROLLI     * calcola data ritiro in base alla pikup area *
055600040615      **********************************************************************
055610040615     c     Sr_Calpikup   BegSr
055620130910
055630130910      /free
055640130910       //?Richiamo pgm per calcolo data ritiro
055650130910         clear FIOR97ds;
055660161027         clear FIOR971DS;
055670130910       //?se la filiale emissione non è ancora stata impostata passo la filiale in gestione
055680130910         IF  V1Cpoe = *zeros;
055690150305           IOR97poe = DUTpou;
055700130910         ELSE;
055710130910           IOR97poe = V1Cpoe;
055720130910         ENDIF;
055730130910         IOR97por = num_V1Cpor;
055740131010         IOR97cap = V1car;
055750131010         IOR97loc = V1lor;
055760160526         IOR97naz = V1nar;
055770130910         IOR97dao = inv_V1dao;
055780131211         IOR97oao = V1oao;
055790130910         IF  *in11;
055800130910           IOR97srm = 'S';
055810130910         ENDIF;
055820130910         IOR97com = V1com;
055830130910         IOR97tor = V1Ctor;
055840130918         IF  ds_V1Ccra > 0;
055850130918           IOR97mcod = 'S';
055860130918         ENDIF;
055870150305       //?se mail/sms inseriti e ORM commissionato
055880140515       //?se ORM manuale
055890140515       //?passo che ORM con alert
055900150305         IF  (w03mail <> *blanks or w03sms <> *blanks) and
055910140515             v1com = 'S' and §RMtla = *blanks;
055920140507           IOR97alert = 'S';
055930140507         ENDIF;
055940140515       //?se conferma ORM da VAO, mail/sms e ci sono i presupposti per alert
055950170308       //?e i flag impostati indicano che devo inviare l'alert
055960140515       //?passo che ORM con alert
055970170308         //IF  §RMfpr = 'C' and (w03mail <> *blanks or w03sms <> *blanks) and
055980170308         //    wOKalert;
055990170308         IF  §RMfpr = 'C' and wOKalert and
056000170308            ((§OREfisv <> 'X' and §OREfisv <> 'N' and w03sms <> *blanks) or
056010170308             (§OREfimv <> 'X' and §OREfimv <> 'N' and w03mail <> *blanks));
056020140515           IOR97alert = 'S';
056030140515         ENDIF;
056040161024       //?passo il peso 'esploso'
056050161027         IOR97pkg = wpkg;
056060161027       //?Se Ordinante codificato
056070161027         IF  ds_V1Ccor > 0;
056080161027           IOR97ocod = 'S';
056090161027         ENDIF;
056100170927       //?Se Ordinante codificato YOOX e ORM da WEB non devo passare il codice ordinante
056110170927         IF  ds_V1Ccor > 0 and ds_V1Ccor = 0017732000 and V1ctco = 'I';
056120170927           clear IOR97ocod;
056130170927         ENDIF;
056140161028       //?Passo Data Pronta Merce
056150161028         IOR97dpm = inv_V1dpm;
056160161024
056170161027         fior97r(kpjba : fior97ds : fior971ds);
056180130910         IF  OOR97err = *blanks and OOR97dar <> *zeros;
056190130910           inv_newV1dar = OOR97dar;
056200130910           dataiso = %date(inv_newV1dar:*iso);
056210130910           dataeur = dataiso;
056220130910           new_V1dar = %dec(dataeur);
056230130910         ENDIF;
056240131015
056250131015       //?Imposto 'A' a video se si può anticipare
056260131015         clear v1anticipa;
056270170524         IF  (*in01 or *in03) and OOR97dmin < OOR97dar;
056280131015           v1anticipa = 'A';
056290131015         ENDIF;
056300170524       //?In immissione o copia mi salvo anche la data ritiro calcolata
056310170524       //?e se ORM da posticipare
056320170525       //?e se posso posticipare
056330170524         IF  *in01 or *in03;
056340170524           wOkPosticipa = *off;
056350170525           wAnticipa = *off;
056360170524           IF  OOR97postd = 'S';
056370170524             wOkPosticipa = *on;
056380170524           ENDIF;
056390170524           DarCalcolata = OOR97dar;
056400170525           wAnticipa = (OOR97dmin < OOR97dar);
056410170524         ENDIF;
056420131107
056430131107       //?Mi memorizzo che ho già richiamato il calcolo della data
056440131107         wokOR97 = *on;
056450161026
056460161026       //?Se ho data pronta merce maggiore data ritiro e sto confermando da VAS
056470161026       //?metto la data pronta merce come data ritiro
056480170524       //?da VAS non si anticipa
056490170616       //?ma solo se tipo comunicazione da FILE, da internet facciamo anticipare
056500170616         IF  §RMfpr = 'C' and §RMtco = 'F' and
056510170616             inv_newv1dar < inv_v1dpm;
056520161026           old_invV1dar = inv_v1dpm;
056530161026           inv_newV1dar = inv_v1dpm;
056540161026           new_V1dar = v1dpm;
056550170524           clear v1anticipa;
056560161026         ENDIF;
056570130910
056580130910      /end-free
056590130910
056600040615     c                   EndSr
056610001017      **********************************************************************
056620001017      * CONTROLLI     * ora ritiro *
056630001017      **********************************************************************
056640001017     C     Sr_Controrr   begsr
056650001017
056660010117     C                   if        v1orr <> *zeros
056670010117     C                   movel     v1orr         ore
056680010117     C                   move      v1orr         minuti
056690001017
056700071107     C                   if        ore > 23 or minuti > 59
056710010220     C                             or ore < 1
056720001017     C                   seton                                        58  28
056730001017     C                   movel     msg(24)       v1cmsg
056740130917     c                   leavesr
056750001017     C                   endif
056760131111     C                   ELSE
056770140320
056780140320      /free
056790131106       //?Se immissione/copia ORM commissionato non c'è obbligo dell'ora
056800131106         IF  (*in01 or *in03) and V1com = 'S';
056810131106         ELSE;
056820131106           *in28 = *on;
056830131106           *in58 = *on;
056840131106           V1Cmsg = msg(52);
056850131106           leavesr;
056860131106         ENDIF;
056870131106      /end-free
056880131111
056890131111     c                   ENDIF
056900001017
056910001017     C                   endsr
056920001103      **********************************************************************
056930070913      * CONTROLLI     * priorità
056940001103      **********************************************************************
056950001103     C     Sr_Contrsto   begsr
056960001103
056970010123     C                   clear                   v1dsto
056980010123
056990010123      * Decodifica priorità ORM
057000010202     C                   if        v1sto <> *zeros
057010010202     C                   clear                   TIBS02DS
057020010202     C                   move      *all'0'       priorita
057030010202     C                   movel     'C'           T02mod
057040010202     C                   movel     knsif         t02sif
057050010202     C                   movel     'STO'         t02cod
057060010202     C                   move      v1sto         priorita
057070010202     C                   movel(p)  priorita      T02ke1
057080010202     C                   call      'TIBS02R'
057090010202     C                   parm                    KPJBA
057100010202     C                   parm                    TIBS02DS
057110010202     C                   if        t02err <> *blanks
057120131030     C                   movel     msg(53)       w2cmsg
057130010202     C                   seton                                        84  28
057140010202     C                   goto      endsrsto
057150010202     C                   endif
057160010202     C                   movel     t02ke1        priorita
057170010202     C                   move      priorita      v1sto
057180010202     C                   movel     t02uni        v1dsto
057190010202     C                   endif
057200001103
057210001103     C     endsrsto      endsr
057220010223      **********************************************************************
057230010223      * CONTROLLI     * Referente e telefono *
057240010223      **********************************************************************
057250010223     C     Sr_Contrref   begsr
057260010223
057270131106      * Per orm prepagato obbligatori tel. e ref.
057280131106     C                   if        v1ctor = 'P'
057290030610      * anche per ORM commissionati
057300030610     c                             or V1com = 'S'
057310010223     C                   if        v1rer = *blanks
057320010223     C                   movel     msg(57)       v1cmsg
057330010223     C                   seton                                        86  28
057340010223     C                   goto      endsrcref
057350010223     C                   endif
057360010223     C                   if        v1ter = *blanks
057370010223     C                   movel     msg(58)       v1cmsg
057380010223     C                   seton                                        87  28
057390010223     C                   goto      endsrcref
057400010223     C                   endif
057410010223     C                   endif
057420010223
057430010223     C     endsrcref     endsr
057440010202      **********************************************************************
057450010202      * CONTROLLI     * pagamento *
057460010202      **********************************************************************
057470010202     C     Sr_Contrpag   begsr
057480010202
057490010202      * Se non inserito prendo di dft
057500010202     C                   if        v1pag = *blanks
057510010202     C                   eval      v1pag = d§dftpag
057520010308     C                   eval      sav_v1pag = d§dftpag
057530010514     C                   endif
057540070912      * controllo che sia valido
057550070912      * nel caso di dato sbagliato che arriva da FNAVO e non viene toccato il campo
057560070912     c                   if        v1pag <> 'M' and v1pag <> 'O' and
057570070912     c                             v1pag <> 'D'
057580070912     c                   eval      v1cmsg = 'Pagamento errato'
057590070912     c                   eval      *in28 = *on
057600070912     c                   eval      *in53 = *on
057610070912     c                   leavesr
057620070912     c                   endif
057630011113      * Se prepagato obbligatorio che paghi il mittente
057640081216      * se non è un ritiro all'estero
057650010213     C                   if        v1ctor = 'P' and v1pag <> 'M'
057660081216     c                             and v1nar = *blanks
057670010213     C                   movel     msg(56)       v1cmsg
057680010213     C                   seton                                        53  28
057690010213     C                   goto      endsrcpag
057700010213     C                   endif
057710010223      * Se singolo e paga il destinatario quest'ultimo è obbligatorio
057720010223     C                   if        v1ctor = 'S' and v1pag = 'D'
057730010223     C                   eval      ds_v1crc1 = v1crc1
057740010223     C                   eval      ds_v1crc2 = v1crc2
057750010223     C                   eval      ds_v1crc3 = v1crc3
057760010223     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
057770010223     C                   movel     msg(61)       v1cmsg
057780010223     C                   seton                                        56  28
057790010223     C                   goto      endsrcpag
057800010223     C                   endif
057810050121      * il codice destinatario non può essere un 8888 o 9999
057820050121     C                   If        ds_v1ccrc > *zeros and
057830050121     c                             (ds_v1crc2 = 9999 or ds_v1crc2 = 8888)
057840050120     C                   movel     msg(84)       v1cmsg
057850050120     C                   seton                                        56  28
057860050120     C                   goto      endsrcpag
057870050120     C                   endif
057880010223     C                   endif
057890030808      * Se inserito che paga l'ordinante
057900030808      * l'ordinante deve essere codificato e non può esserre un 8888 o 9999
057910030808      * se mittente e ordinante sono uguali obbligatorio che paghi mittente
057920010202     C                   eval      ds_v1cor1 = v1cor1
057930010202     C                   eval      ds_v1cor2 = v1cor2
057940010202     C                   eval      ds_v1cor3 = v1cor3
057950030808     c                   Eval      ds_V1Cra1 = V1Cra1
057960030808     c                   eval      ds_V1Cra2 = V1Cra2
057970030808     c                   eval      ds_V1Cra3 = V1Cra3
057980030808     C                   if        v1pag = 'O'
057990030808     C                   If        (ds_v1ccor = *zeros or
058000030606     c                             (ds_v1cor2 = 9999 or ds_v1cor2 = 8888))
058010010202     C                   movel     msg(23)       v1cmsg
058020010202     C                   seton                                        42  28
058030010202     C                   goto      endsrcpag
058040010202     C                   endif
058050030808     c                   EndIF
058060010507
058070010507     C  nkf              eval      sav_v1ksc = v1ksc
058080010202
058090010202     C     endsrcpag     endsr
058100010202      **********************************************************************
058110010202      * CONTROLLI     * cod.cliente e tariffa *
058120010202      **********************************************************************
058130010202     C     Sr_Contrksc   begsr
058140010202
058150040930      * Controllo se il codice cliente esiste su CNACO
058160040930if  1c                   If        v1ksc <> *Zeros
058170040930     c                   z-add     v1ksc         kksc
058180040930     c     kCnaco        Chain     Cnaco00f
058190040930if  2c                   If        Not %Found(Cnaco00f) or
058200040930     c                             AcoFlg <> *Blanks
058210040930     c                   Eval      v1cmsg = msg(16)
058220040930     c                   Seton                                        54  28
058230040930     c                   Goto      endsrcksc
058240040930    2c                   EndIf
058250040930      * Controllo il network del p.o. del cliente non deve essere LED o SEDE
058260040930     c                   Movel     v1ksc         kazorg
058270040930     c                   Clear                   Og143
058280040930     c     kazorg        Chain     Azorg01l
058290040930     c                   If        %Found(Azorg01l)
058300040930     c                   Eval      Og143 = Orgde3
058310040930     c                   EndIf
058320040930if  1c                   If        §OgNtw = 'LOG' or §OgNtw = 'XXX'
058330040930     c                   Eval      v1cmsg = msg(16)
058340040930     c                   Seton                                        54  28
058350040930     c                   Goto      endsrcksc
058360040930e   2c                   EndIf
058370040930e   1c                   EndIf
058380010202      * Se codice tariffa inserito deve essere un numero
058390010202     C                   if        v1ctr <> *blanks  and
058400010320     C                             (%subst(v1ctr:1:1) < '0'  and
058410010320     C                              %subst(v1ctr:1:1) <> ' ') or
058420010320     C                             (%subst(v1ctr:2:1) < '0'  and
058430010320     C                              %subst(v1ctr:2:1) <> ' ') or
058440010320     C                             (%subst(v1ctr:3:1) < '0'   and
058450010320     C                              %subst(v1ctr:3:1) <> ' ')
058460010202     C                   movel     msg(16)       v1cmsg
058470010202     C                   seton                                        85  28
058480010202     C                   goto      endsrcksc
058490010202     C                   endif
058500170728         IF  V1ctr <> *blanks and
058510170728             (%subst(v1ctr:1:1) = ' ' or %subst(V1ctr:2:1) = ' ' or
058520170728              %subst(V1ctr:3:1) = ' ');
058530170728           V1Cmsg = msg(16);
058540170728           *in28 = *on;
058550170728           *in85 = *on;
058560170728           leavesr;
058570170728         ENDIF;
058580040630      * non posso inserire solo la tariffa senza il codice cliente
058590040630     C                   if        v1ksc = *zeros and v1ctr <> *blanks
058600040630     C                   movel     msg(59)       v1cmsg
058610040630     C                   seton                                        85  28
058620040630     C                   goto      endsrcksc
058630040630     C                   endif
058640010202      * Controlla validità codice + tariffa
058650010202     C                   if        v1ksc <> *zeros and v1ctr <> *blanks
058660010202     C                   z-add     v1ksc         kksc
058670010202     C                   movel     v1ctr         kctr
058680090128     c                   eval      $notam = *off
058690090128     c     ktntam        setll     tntam01l
058700090128      * se non trovo errore
058710090128     c                   if        not %equal(tntam01l)
058720090128     c                   eval      v1cmsg = msg(16)
058730090128     c                   seton                                        54  28
058740090128     c                   leavesr
058750090128     c                   endif
058760090128     c                   do        *hival
058770090128     c     ktntam        Reade     Tntam01l
058780040713     C                   if        %Eof(tntam01l)
058790090128     C                   leavesr
058800010202     C                   endif
058810090128      * se annullato leggo altro record
058820010202     C                   if        TAMatb <> *blanks
058830090128     C                   iter
058840010202     C                   endif
058850090128      * trovato almeno 1 rcd ok
058860090128     c                   eval      $notam = *on
058870090128     c                   enddo
058880090128      * se non ho trovato nessuna tariffa errore
058890090128     c                   if        not $notam
058900090128     c                   eval      v1cmsg = msg(16)
058910090128     c                   seton                                        54  28
058920090128     c                   leavesr
058930090128     c                   endif
058940010202     C                   endif
058950010202
058960010202     C     endsrcksc     endsr
058970010223      **********************************************************************
058980010223      * CONTROLLI     * flag ddt *
058990010223      **********************************************************************
059000010223     C     Sr_Contrddt   begsr
059010010223
059020021119      * Da default immissione ORM
059030010223     C                   if        v1ddt = *blanks
059040010223     C                   eval      v1ddt = d§dftddt
059050010223     C                   endif
059060010223
059070010223     C                   endsr
059080010202      **********************************************************************
059090010202      * CONTROLLI     * P.O. consegna *
059100010202      **********************************************************************
059110010202     C     Sr_Contrpoc   begsr
059120010202
059130010320     C                   clear                   v1dpoc
059140060929     c                   clear                   c_og143
059150010320
059160010202     C                   if        v1cpoc <> *blanks
059170010202      * Ricerca P.O.Consegna con "?"
059180010202     C     '?'           scan      v1cpoc                                 30
059190010202     C     *in30         ifeq      *on
059200010202     C                   eval      *in55 = *on
059210010202     C                   eval      *in90 = *on
059220010202     C                   clear                   ds_tnsd
059230010202     C                   call      'TNSD24R'
059240010202     C                   parm                    ds_cod
059250010202     C                   parm                    ds_tip
059260010202     C                   parm                    ds_des
059270010202     C                   parm                    ds_pra
059280010202     C                   movel     ds_cod        v1cpoc
059290010202     C                   movel     ds_des        v1dpoc
059300010202     C                   endif
059310010316     C                   endif
059320010316      * Solo campi numerico
059330010320     C                   if        v1cpoc <> *blanks and
059340010320     C                             (%subst(v1cpoc:1:1) < '0' and
059350010320     C                              %subst(v1cpoc:1:1) <> ' ') or
059360010320     C                             (%subst(v1cpoc:2:1) < '0' and
059370010320     C                              %subst(v1cpoc:2:1) <> ' ') or
059380010320     C                             (%subst(v1cpoc:3:1) < '0' and
059390010320     C                              %subst(v1cpoc:3:1) <> ' ')
059400010316     C                   seton                                        55  28
059410010316     C                   movel     msg(17)       v1cmsg
059420010316     C                   goto      endsrcpoc
059430010316     C                   endif
059440010316      * Controllo
059450010202     C                   if        v1cpoc <> *blanks
059460010202     C                   movel     v1cpoc        kazorg
059470010202     C     kazorg        chain     azorg01L
059480010316     C                   if        not%found(azorg01l)
059490010316     C                   movel     msg(17)       v1cmsg
059500010316     C                   seton                                        55  28
059510010316     C                   goto      endsrcpoc
059520010316     C                   endif
059530010202     C                   if        ORGfva <> *blanks or
059540010202     C                             (ORGfag <> 'A' and ORGfag <> 'F')
059550010202     C                   movel     msg(17)       v1cmsg
059560010202     C                   seton                                        55  28
059570010202     C                   goto      endsrcpoc
059580010202     C                   endif
059590010202     C                   movel     ORGdes        v1dpoc
059600060929     c                   eval      c_og143 = orgde3
059610010202     C                   endif
059620111221
059630111221      /free
059640111221       //?Forzo la filiale di consegna in base alla tabella FFC
059650120116       //?se il destinatario è estero
059660150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
059670120116       IF  V1nac <> *blanks;
059680120116         IF  V1Cpoe = *zeros;
059690150305           w_V1Cpoe = DUTpou;
059700120116         ELSE;
059710120116           w_V1Cpoe = V1Cpoe;
059720120116         ENDIF;
059730120116         //?Provo con la filiale dell'ordinante se presente
059740120116         IF  ds_V1Ccor > 0;
059750120116           clear TIBS02ds;
059760120116           clear dFFC;
059770120116           T02mod = 'C';
059780120116           T02cod = 'FFC';
059790120116           T02ke1 = %editc(ds_V1Cor1:'X');
059800120116           T02sif = KNSIF;
059810120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
059820120116           IF  T02err = *blanks;
059830120116             dFFC = T02uni;
059840120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
059850120116               V1Cpoc = %editc(§FFCpoc:'X');
059860120116             ENDIF;
059870120116             leavesr;
059880120116           ENDIF;
059890120116         ENDIF;
059900120116         //?Se non trovo con Filiale Ordinante provo con Filiale Emissione
059910120116         //?se diversa da Filiale Ordinante
059920120116         IF  w_V1Cpoe <> ds_V1Cor1;
059930120116           clear TIBS02ds;
059940120116           clear dFFC;
059950120116           T02mod = 'C';
059960120116           T02cod = 'FFC';
059970120116           T02ke1 = %editc(w_V1Cpoe:'X');
059980120116           T02sif = KNSIF;
059990120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
060000120116           IF  T02err = *blanks;
060010120116             dFFC = T02uni;
060020120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
060030120116               V1Cpoc = %editc(§FFCpoc:'X');
060040120116             ENDIF;
060050120116             leavesr;
060060120116           ENDIF;
060070120116         ENDIF;
060080111221       ENDIF;
060090111221      /end-free
060100010202
060110010202     C     endsrcpoc     endsr
060120010202      **********************************************************************
060130010202      * CONTROLLI     * cliente destinatario *
060140010202      **********************************************************************
060150010202     C     Sr_Contrclid  begsr
060160110610
060170110610     c                   eval      $cacgdd = *off
060180010202
060190010202     C                   eval      ds_v1crc1 = v1crc1
060200010202     C                   eval      ds_v1crc2 = v1crc2
060210010202     C                   eval      ds_v1crc3 = v1crc3
060220010223
060230010223      * Se orm prepagato deve esserci il destinatario
060240010223     C                   if        v1ctor = 'P' and
060250010223     C                             (ds_v1ccrc = *zeros and v1rsc = *blanks)
060260010223     C                   movel     msg(50)       v1cmsg
060270010223     C                   seton                                        56  28
060280010223     C                   goto      endsrclid
060290010223     C                   endif
060300131107      /free
060310131107       //?Se sono in immissione/copia e sto inserendo il destinatario
060320131107       //?se il tipo ORM è impostato a Multiplo lo aggiorno in automatico
060330131107       //?in Singolo in modo da non dare il messaggio di errore
060340131107         IF  (*in01 or *in03) and
060350131107             ((sav_V1Ccrc <> ds_V1Ccrc and sav_V1Ccrc = 0) or
060360131107             (sav_V1rsc <> V1rsc and sav_V1rsc = *blanks)) and
060370131107             V1Ctor = 'M';
060380131107           V1Ctor = 'S';
060390131107         ENDIF;
060400131107      /end-free
060410131107
060420010223      * Se orm multiplo non deve esserci il destinatario
060430010223     C                   if        v1ctor = 'M' and
060440010223     C                             (ds_v1ccrc > *zeros or v1rsc <> *blanks)
060450010223     C                   movel     msg(38)       v1cmsg
060460160316     c                   eval      H1riga = 05
060470160316     c                   eval      H1colo = 11
060480160316     C                   seton                                            28
060490011114     C                   setoff                                       0616
060500010223     C                   goto      endsrclid
060510010223     C                   endif
060520011114
060530011114      * Se blocco destinatario l'ORM deve essere singolo e i dati del
060540011114      * destinatario devono essere completi
060550011114     C                   if        §ormfd = 'S'
060560011114     C                   if        v1ctor <> 'S' and *in38 = *off
060570011114     C                   movel     msg(74)       v1cmsg
060580160316     c                   eval      H1riga = 05
060590160316     c                   eval      H1colo = 11
060600160316     C                   seton                                          3828
060610011114     C                   setoff                                       0616
060620011114     C                   goto      endsrclid
060630011114     C                   endif
060640011114     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
060650011114     C                             and *in39 = *off
060660011114     C                   movel     msg(75)       v1cmsg
060670011114     C                   seton                                        563928
060680011114     C                   setoff                                       0616
060690011114     C                   goto      endsrclid
060700011114     C                   endif
060710011114     C                   endif
060720010223
060730010202      * Inserito codice
060740010202     C                   if        ds_v1ccrc <> *zeros
060750010202     C                   eval      *in06 = *on
060760010202     C                   exsr      Sr_Contrcrc
060770010202     C                   eval      sav_v1ccrc = ds_v1ccrc
060780010202     C                   else
060790010202      * Inserita ragione sociale
060800010202     C                   clear                   sav_v1ccrc
060810010202     C                   exsr      Sr_Contrrsc
060820131107     c                   eval      sav_V1rsc = V1rsc
060830010202     C                   eval      *in06 = *off
060840010202     C                   endif
060850110120     c   28              leavesr
060860011119      * Se ORM con blocco destinatario faccio già i controlli
060870011119     C                   if        §ormfd = 'S' and
060880011119     C                             (ds_v1ccrc <> *zeros or v1rsc <> *blanks)
060890011119     C                   eval      *in16 = *on
060900011119     C                   endif
060910060929
060920060929      * se oltre al destinatario è stato inserito anche il p.o. di consegna
060930060929      * devo fare un ulteriore controllo
060940060929      * ai fini dell'orm non serve, ma serve per impedire l'instradamento sbagliato
060950060929      * al momento della creazione della bolla legata a questo ORM
060960110120     c                   if        v1cpoc<> *blank and v1rsc <> *blanks
060970060929     c                   exsr      sr_contrcapc
060980060929     c                   endif
060990100811
061000100811      /free
061010100811       //?Se ORM import DPD devo controllare che la nazione di destino sia
061020100907       //?servita da DPD --> chiodo DPD come ntw.
061030150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
061040130107         IF  V1Cpoe = *zeros;
061050150305           w_V1Cpoe = DUTpou;
061060130107         ELSE;
061070130107           w_V1Cpoe = V1Cpoe;
061080130107         ENDIF;
061090130107         IF  %lookup(w_V1Cpoe:skpodpd) > 0 and v1nac <> *blanks and
061100101102             O14lad <= 0;
061110100909           v1cmsg = 'Nazione destino non servita da DPD. +
061120100909                     Richiedere al Depot l''annullamento';
061130100907           *in28 = *on;
061140100907           *in65 = *on;
061150100907           *in75 = *on;
061160100811         ENDIF;
061170160208
061180160209       //?Se inserito destinatario
061190160209         IF  ds_V1Ccrc > 0 or V1rsc <> *blanks;
061200160209         //?Se c'è la filiale di consegna uso quella
061210160209           IF  V1Cpoc > *zeros;
061220160209             O95lna = %dec(V1Cpoc:3:0);
061230160209           ELSE;
061240160209         //?Se non c'è la filiale di consegna
061250160209         //?richiamo routine sr_contrcapc per il calcolo della LNA
061260160209             exsr sr_contrcapc;
061270160209           ENDIF;
061280160209         //?Se ORM Prepagato e consegna a lna 340-341-345 ORM non possibile vedi DV 1540
061290160209           IF  (O95lna = 340 or O95lna = 341 or O95lna = 345) and
061300160209                V1Ctor = 'P';
061310160209             TBLcod = '15';
061320160209             TBLkey = v1nac;
061330160209             chain (CodUt:TBLcod:TBLkey) TABEL00F;
061340160209             IF  %found(TABEL00F);
061350160209               ds15 = TBLuni;
061360160209             ENDIF;
061370160209             V1Cmsg = 'Per consegne in';
061380160209             V1Cmsg = %trim(V1Cmsg) + ' ' + §15des;
061390160209             V1Cmsg = %trim(V1Cmsg) +
061400160209                      ' no pagamento a carico del Mittente';
061410160209             *in28 = *on;
061420160209             *in65 = *on;
061430160209             *in75 = *on;
061440160209           ENDIF;
061450160209         ENDIF;
061460160208
061470100811      /end-free
061480010202
061490010202     C     endsrclid     endsr
061500010202      **********************************************************************
061510010202      * CONTROLLI     * cliente destinatario *
061520010202      **********************************************************************
061530010202     C     Sr_Contrcrc   begsr
061540010202
061550010202     C                   z-add     ds_v1ccrc     kfnacr
061560151109     C     kfnacr        chain(n)  fnacr01l
061570010202     C                   if        %found(fnacr01l)
061580010202     C                   if        ACRatb <> *blanks
061590010202     C                   movel     msg(18)       v1cmsg
061600010202     C                   seton                                        56  28
061610010202     C                   goto      endsrccrc
061620010202     C                   endif
061630040727     C                   if        ACRtcr = 'M'
061640040727     C                   movel     msg(83)       v1cmsg
061650040727     C                   seton                                        56  28
061660040727     C                   goto      endsrccrc
061670040727     C                   endif
061680010906
061690010906      * Se modificato il cliente pulisco il ksc
061700010906     C                   if        v1pag = 'D'
061710010906     C                             and (ds_v1ccrc <> sav_v1ccrc
061720031010     C                             or v1pag <> sav_v1pag
061730040630     c                             or v1ksc = *Zeros
061740040630     c                             or V1Ctr = *Blanks)
061750031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocrc mi carica il ksc
061760040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
061770031010     c                   Clear                   sav_V1pag
061780031010     c                   EndIf
061790010906     C                   clear                   v1ksc
061800010906     C                   clear                   v1ctr
061810010906     C                   endif
061820010202     C                   exsr      Sr_Decocrc
061830010202     C                   else
061840010202     C                   movel     msg(18)       v1cmsg
061850010202     C                   seton                                        56  28
061860010202     C                   endif
061870010202
061880010202     C     endsrccrc     endsr
061890010202      **********************************************************************
061900011113      * DECODIFICHE     * cliente destinatario *
061910010202      **********************************************************************
061920010202     C     Sr_Decocrc    begsr
061930010202
061940010202     C                   movel     ACRrsr        v1rsc
061950010202     C                   movel     ACRinr        v1inc
061960010202     C                   movel     ACRcar        v1cac
061970010202     C                   movel     ACRlor        v1loc
061980030922     C                   movel     ACRlor        sav_v1loc35
061990010202     C                   movel     ACRprr        v1prc
062000010202     C                   movel     ACRnar        v1nac
062010010207      * Se paga destinatario memorizzo il codice piano dei conti
062020010207     C                   if        v1pag = 'D'
062030010906     C                             and (ds_v1ccrc <> sav_v1ccrc
062040010906     C                             or v1pag <> sav_v1pag)
062050010906     C                             and  v1ksc = *zeros
062060050120     c                   If        AcrKsc > *Zeros
062070010207     C                   eval      v1ksc = ACRksc
062080010903     C  nkf              eval      sav_v1ksc  = ACRksc
062090010309     C                   eval      sav_v1pag = v1pag
062100040629      * memorizzo anche il codice tariffa
062110040629     c                   If        AcrCcc = 999
062120040629     c                   Clear                   V1Ctr
062130040629     c                   Else
062140040629     c                   Move      AcrCcc        V1Ctr
062150040629     c                   EndIf
062160050120     c                   Else
062170050120     c                   Eval      V1Ksc = ds_v1crc12
062180050120     c                   EndIf
062190010207     C                   endif
062200010202
062210010202     C                   endsr
062220010202      **********************************************************************
062230011113      * CONTROLLI     * dati cliente destinatario *
062240010202      **********************************************************************
062250010202     C     Sr_Contrrsc   begsr
062260030922
062270030930     c                   If        V1loc = *Blanks or
062280030930     c                             (v1loc <> sav_v1loc)
062290030929     c                   Clear                   sav_v1loc35
062300030929     c                   EndIf
062310030922     c                   If        sav_v1loc35 = *blanks
062320030922     c                   Movel(p)  v1loc         sav_v1loc35
062330030922     c                   EndIf
062340131202
062350131202      /free
062360131202         IF  V1loc <> sav_V1loc or V1cac <> sav_V1cac or V1nac <> sav_V1nac;
062370131202           wForzaNAC = *off;
062380131202         ENDIF;
062390131202      /end-free
062400010202
062410010202     C                   clear                   FNLV14DS
062420010202     C                   z-add     *date         I14dri
062430010202     C                   movel     v1rsc         I14rsc
062440010202     C                   movel     v1inc         I14ind
062450030922     C                   movel(p)  sav_v1loc35   E14loc
062460010202     C                   movel     v1prc         E14prv
062470010202     C                   movel     v1cac         E14cap
062480010202     C                   movel     v1nac         E14nar
062490161109      /free
062500161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
062510161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
062520161109       //?il pgm di interrogazione cappario
062530161109         IF  ConfAuto;
062540161109           I14batch = 'S';
062550161109         ENDIF;
062560161109      /end-free
062570010202
062580010202     C                   call      'FNLV14R'
062590010202     C                   parm                    KPJBA
062600010202     C                   parm                    FNLV14DS
062610010202
062620010202     C                   movel     I14ind        v1inc
062630010202     C                   movel     E14loc        v1loc
062640030922     C                   movel     E14loc        sav_v1loc35
062650010202     C                   movel     E14prv        v1prc
062660010202     C                   movel     E14cap        v1cac
062670010202     C                   movel     E14nar        v1nac
062680010202
062690010202     C                   if        v1rsc <> *blanks or v1inc <> *blanks
062700010202     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
062710010202     C                   eval      *in61 = (O14err = '2')                       Indirizzo
062720010202     C                   eval      *in62 = (O14err = '5')                       CAP
062730010202     C                   eval      *in63 = (O14err = '3')                       Località
062740010202     C                   eval      *in64 = (O14err = '4')                       Provincia
062750010202     C                   eval      *in65 = (O14err = '6')                       Nazione
062760010202
062770010202     C                   if        O14msg <> *blanks
062780010202     C                   movel(p)  O14msg        v1cmsg
062790010202     C                   eval      *IN28 = *ON
062800010202     C                   eval      *in75 = *on
062810010202     C                   goto      endsrcrsc
062820010202     C                   endif
062830010202     C                   endif
062840010202
062850010202      * Controlla indirizzo
062860010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062870010202     C                             v1prc <> *blanks or v1nac <> *blanks
062880010202     C                   exsr      Sr_Contrcapc
062890010202     C                   eval      *in75 = (*in28 = *on)
062900010202     C   28              goto      endsrcrsc
062910010202     C                   endif
062920010202
062930010202      * Controlla se dati completi o parziali
062940010202     C                   if        v1rsc = *blanks
062950010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062960010202     C                             v1prc <> *blanks or v1nac <> *blanks
062970010202     C                   movel     msg(39)       v1cmsg
062980010202     C                   eval      *IN28 = *ON
062990010202     C                   eval      *in56 = *on
063000010202     C                   goto      endsrcrsc
063010010202     C                   endif
063020010202     C                   endif
063030131202      /free
063040131202       //?Se inserita la nazione e questa è lunga 2 byte e presente in tabella PR-provincia
063050131202       //?devo avvisare l'utente se è giusta la nazione o se per caso voleva inserire la
063060131202       //?provincia. Caso di nazione 'VE' accettata in immissione ORM perchè esiste la
063070131202       //?nazione VE Venezuale ma l'utente voleva inserire VE Venezia, ha sbagliato campo
063080131202         IF  V1rsc <> *blanks and V1nac <> *blanks and
063090131202            ((%subst(V1nac:1:1) = *blanks and %subst(V1nac:2:2) <> *blanks) or
063100131202             (%subst(V1nac:1:2) <> *blanks and %subst(V1nac:3:1) = *blanks));
063110131202           TBLcod = 'PR';
063120131202           TBLkey = v1nac;
063130131202           chain (CodUt:TBLcod:TBLkey) TABEL00F;
063140131202           IF  %found(TABEL00F) and TBLflg = *blanks and not wForzaNAC;
063150131202             TBLcod = '15';
063160131202             TBLkey = v1nac;
063170131202             chain (CodUt:TBLcod:TBLkey) TABEL00F;
063180131202             IF  %found(TABEL00F);
063190131202               ds15 = TBLuni;
063200131202             ENDIF;
063210131202             wForzaNAC = *on;
063220131202             sav_V1nac = V1nac;
063230131202             *in28 = *on;
063240131202             *in65 = *on;
063250131202             *in75 = *on;
063260131202             V1Cmsg = 'Verificare la nazione del destinatario';
063270131202             V1Cmsg = %trim(V1Cmsg) + ' ' +
063280131202                      %trim(V1nac) + '-' +
063290131202                      %trim(§15des);
063300131202             leavesr;
063310131202           ENDIF;
063320131202         ENDIF;
063330131202      /end-free
063340010207
063350010207      * Se paga il destinatario devo pulire il codice cliente che paga
063360010329     C                   if        v1pag = 'D'
063370010507     C                             and (v1pag <> sav_v1pag
063380010917     C                             or   sav_v1ccrc <> ds_v1ccrc)
063390010207     C                   clear                   v1ksc
063400010906     C                   clear                   v1ctr
063410010903     C  nkf              clear                   sav_v1ksc
063420010329     C                   eval      sav_v1pag = v1pag
063430010207     C                   endif
063440010202
063450010202     C     endsrcrsc     endsr
063460010202      **********************************************************************
063470010202      * CONTROLLI     * cap - località - provincia *
063480010202      **********************************************************************
063490010202     C     Sr_Contrcapc  begsr
063500010202
063510010202     C                   eval      *in32 = *off
063520010202
063530010202     C                   clear                   TISI95DS
063540010202     C                   clear                   FNLV13DS
063550080423     c                   clear                   tisi97ds
063560010202     C                   movel     ' '           I95tla
063570010202     C                   movel     '7'           I95tcn
063580010202     C                   movel     v1cac         I95cap
063590010202     C                   movel     v1nac         I95nar
063600010202     C                   movel     v1inc         I95ind
063610030922     C                   movel(p)  sav_v1loc35   I95loc
063620010202     C                   movel     v1prc         I95prv
063630010202     C                   movel     'S'           I13af0
063640010202     C                   movel     'S'           I13af1
063650010202     C                   movel     'S'           I13sz2
063660010202     C                   movel     'S'           I13la3
063670010202     C                   movel     'S'           I13sz3
063680010202     C                   movel     'S'           I13cnv
063690101215     c                   if        v1pag<>'M'
063700101214     c                   eval      i95tpo='A'
063710101214     c                   endif
063720010202
063730010202      * Gestione errori forzabili: se cambiato CAP/Località
063740010202      * riazzera i valori di errori forzabili
063750080910
063760080910      * CAP
063770080910     C     v1cac         ifne      sav_v1cac
063780080910     C                   movel     v1cac         sav_v1cac
063790080910     C                   clear                   wfzv
063800080910     C                   clear                   wfz1
063810080910     C                   clear                   wfz2
063820080910     C                   clear                   wfz3
063830080910     C                   eval      *in32 = *on
063840080910     C                   endif
063850010202
063860010202      * Modifica località
063870010202
063880010202     C     v1loc         ifne      sav_v1loc
063890010202     C                   clear                   wfz1
063900010202     C                   clear                   wfz2
063910010202     C                   clear                   wfz3
063920010202     C                   movel     v1loc         sav_v1loc
063930010202     C                   eval      *in32 = *on
063940010202     C                   endif
063950010202
063960010202      * Modifica provincia
063970010202
063980010202     C     v1prc         ifne      sav_v1prc
063990010202     C                   clear                   wfz3
064000010202     C                   movel     v1prc         sav_v1prc
064010010202     C                   eval      *in32 = *on
064020010202     C                   endif
064030010202
064040010202     C                   movel     wfzv          E13fzv
064050010202     C                   movel     wfz1          E13fz1
064060010202     C                   movel     wfz2          E13fz2
064070010202     C                   movel     wfz3          E13fz3
064080060929
064090060929     c                   eval      i97ntw = c_§ogntw
064100010202
064110010202     C                   call      'FNLV13R'
064120010202     C                   parm                    KPJBA
064130010202     C                   parm                    FNLV13DS
064140010202     C                   parm                    TISI95DS
064150060929     c                   parm      ' '           flgbac            1
064160060929     c                   parm                    tisi97ds
064170010202
064180010202      * Ricontrolla errori
064190010202
064200010202     C                   movel     O13err        werr
064210010202     C                   movel     O13msg        wmsg
064220010202     C                   movel     E13fzv        wfzv
064230010202     C                   movel     E13fz1        wfz1
064240010202     C                   movel     E13fz2        wfz2
064250010202     C                   movel     E13fz3        wfz3
064260010202
064270010202      * Se il livello di affidabilità non è inferiore a 2 e
064280010202      * il livello di reperimento dati non è inferiore a 3
064290010202      * reimposta i dati da TISI95DS
064300110120      * I dati non vengono sostituiti se i dati provengono da anagrafica
064310010202
064320110120     c                   if        ds_v1ccrc=0
064330010202     C     O13ron        ifeq      'S'
064340110120     C                   movel     O95nar        v1nac
064350110120     c                   endif
064360110120     C     O13roc        ifeq      'S'
064370110120     C                   movel     O95cap        v1cac
064380110120     c                   endif
064390110120     C     O13rop        ifeq      'S'
064400110120     C                   movel     O95prv        v1prc
064410110120     c                   endif
064420110120     C     O13rol        ifeq      'S'
064430010202     C                   movel     O95loc        v1loc
064440030922     C                   movel     O95loc        sav_v1loc35
064450010202     C                   end
064460161109
064470161109      /free
064480161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
064490161109       //?conferma automatica batch non devo dare errore
064500161109         IF  ConfAuto and O13roc = 'S';
064510161109           clear werr;
064520161109           clear O13roc;
064530161109         ENDIF;
064540161109      /end-free
064550010202
064560010202     C     werr          ifne      ' '
064570010202     C     wmsg          andne     *blanks
064580080910     C     *in32         oreq      *on
064590010202
064600010202     C                   select
064610010202
064620010202     C     werr          wheneq    '3'
064630010202     C                   eval      *in63 = *on
064640010202     C                   eval      *in28 = *on
064650010202
064660010202     C     werr          wheneq    '5'
064670010202     C                   eval      *in62 = *on
064680010202     C                   eval      *in28 = *on
064690010202
064700010202     C     werr          wheneq    '4'
064710010202     C                   eval      *in64 = *on
064720010202     C                   eval      *in28 = *on
064730010202
064740010202     C     werr          wheneq    '6'
064750010202     C                   eval      *in65 = *on
064760010202     C                   eval      *in28 = *on
064770010202     C                   endsl
064780010202
064790010202     C                   movel(p)  wmsg          v1cmsg
064800010202     C                   end
064810110120     c                   else
064820110120     C     werr          ifne      ' '
064830110120     C     wmsg          andne     *blanks
064840110120     C                   eval      *in56 = *on
064850110120     C                   eval      *in28 = *on
064860110120     C                   movel(p)  msg(94)       v1cmsg
064870110120     c                   endif
064880110120     c                   endif
064890010202
064900010202     C   28              goto      endsrcapc
064910010202
064920010202      * Controlla se reimpostata località
064930010202
064940010202     C     O13rol        ifeq      'S'
064950010202     C                   eval      *in63 = *on
064960010202     C                   eval      *in28 = *on
064970010202     C                   movel     msg(19)       v1cmsg
064980010202     C                   goto      endsrcapc
064990010202     C                   end
065000010202
065010010202      * Controlla se reimpostata Provincia
065020010202
065030010202     C     O13rop        ifeq      'S'
065040010202     C                   eval      *in64 = *on
065050010202     C                   eval      *in28 = *on
065060010202     C                   movel     msg(21)       v1cmsg
065070010202     C                   goto      endsrcapc
065080010202     C                   end
065090010202
065100010202      * Controlla se reimpostato cap
065110010202
065120010202     C     O13roc        ifeq      'S'
065130010202     C                   eval      *in62 = *on
065140010202     C                   eval      *in28 = *on
065150010202     C                   movel     msg(20)       v1cmsg
065160010202     C                   goto      endsrcapc
065170010202     C                   end
065180010202
065190010202      * Controlla se reimpostata Nazione
065200010202
065210010202     C     O13ron        ifeq      'S'
065220010202     C                   eval      *in65 = *on
065230010202     C                   eval      *in28 = *on
065240010202     C                   movel     msg(22)       v1cmsg
065250010202     C                   goto      endsrcapc
065260010202     C                   end
065270110610
065280110610      /free
065290110610       //?Mi salvo se il cap del destinatario prevede l'obbligo DDT
065300110610         $cacgdd = (O95gdd = 'S');
065310110610      /end-free
065320110610
065330010202
065340010202     C     endsrcapc     endsr
065350010208      **********************************************************************
065360010208      * CONTROLLI     * cliente ordinante *
065370010208      **********************************************************************
065380010208     C     Sr_Contrclio  begsr
065390010208
065400010208     C                   eval      ds_v1cor1 = v1cor1
065410010208     C                   eval      ds_v1cor2 = v1cor2
065420010208     C                   eval      ds_v1cor3 = v1cor3
065430160413      /free
065440160413       //?Se è stato variato il codice ordinante
065450160413       //?in caso di immissione ORM manuale a mittente non codificato
065460160413       //?devo reimpostare i dati per la conferma prenotazione
065470160413         IF  sav_V1Ccor <> ds_V1Ccor and
065480160413             (*in01 or *in03) and
065490160413             (V1Ctco = 'M' or V1Ctco = 'E') and
065500160413             ds_V1Ccra = 0;
065510160413           wOkF6F13 = *off;
065520160413           wRicConf = *off;
065530160413           clear W04sms;
065540160413           clear W04mail;
065550160413         ENDIF;
065560160413      /end-free
065570010208      * Inserito codice
065580010208     C                   if        ds_v1ccor <> *zeros
065590010208     C                   eval      *in08 = *on
065600010208     C                   exsr      Sr_Contrcor
065610010208     C                   eval      sav_v1ccor = ds_v1ccor
065620010208     C                   else
065630010208      * Inserita ragione sociale
065640010208     C                   clear                   sav_v1ccor
065650010208     C                   exsr      Sr_Contrrso
065660010208     C                   eval      *in08 = *off
065670010208     C                   endif
065680010208
065690010702     C                   endsr
065700010208      **********************************************************************
065710010208      * CONTROLLI     * cliente ordinante *
065720010208      **********************************************************************
065730010208     C     Sr_Contrcor   begsr
065740010208
065750010208     C                   z-add     ds_v1ccor     kfnacr
065760151109     C     kfnacr        chain(n)  fnacr01l
065770010208     C                   if        not%found(fnacr01l)
065780010208     C                   movel     msg(3)        v1cmsg
065790010208     C                   seton                                        42  28
065800010208     C                   goto      endsrccor
065810010208     C                   endif
065820010208     C                   if        ACRatb <> *blanks
065830010208     C                   movel     msg(3)        v1cmsg
065840010208     C                   seton                                        42  28
065850010208     C                   goto      endsrccor
065860010208     C                   endif
065870040727     C                   if        ACRtcr = 'M'
065880040727     C                   movel     msg(82)       v1cmsg
065890040727     C                   seton                                        42  28
065900040727     C                   goto      endsrccor
065910040727     C                   endif
065920010906      * Se modificato il cliente pulisco il ksc
065930010906     C                   if        v1pag = 'O'
065940010906     C                             and (ds_v1ccor <> sav_v1ccor
065950031010     C                             or v1pag <> sav_v1pag
065960040630     c                             or   v1ksc = *Zeros
065970040630     c                             or   V1Ctr = *Blanks)
065980031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocro mi carica il ksc
065990040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
066000031010     c                   Clear                   sav_V1pag
066010031010     c                   EndIf
066020010906     C                   clear                   v1ksc
066030010906     C                   clear                   v1ctr
066040010906     C                   endif
066050010208     C                   exsr      Sr_Decocor
066060010208
066070010208     C     endsrccor     endsr
066080010208      **********************************************************************
066090010208      * DECODIFICHE     * cliente ordinante *
066100010208      **********************************************************************
066110010208     C     Sr_Decocor    begsr
066120010208
066130010208     C                   movel     ACRrsr        v1rso
066140010208     C                   movel     ACRinr        v1ino
066150010208     C                   movel     ACRcar        v1cao
066160010208     C                   movel     ACRlor        v1loo
066170030922     C                   movel     ACRlor        sav_v1loo35
066180010208     C                   movel     ACRprr        v1pro
066190010208     C                   movel     ACRnar        v1nao
066200010208      * Se paga ordinante memorizzo il codice piano dei conti
066210030902      * se non c'è il ksc memorizzo i primi 7 byte del codice anagrafica ritiro
066220010208     C                   if        v1pag = 'O'
066230010906     C                             and (ds_v1ccor <> sav_v1ccor
066240010906     C                             or v1pag <> sav_v1pag)
066250010906     C                             and  v1ksc = *zeros
066260030902     c                   If        AcrKsc > *Zeros
066270010208     C                   eval      v1ksc = ACRksc
066280040629      * memorizzo anche il codice tariffa
066290040629     c                   If        AcrCcc = 999
066300040629     c                   Clear                   V1Ctr
066310040629     c                   Else
066320040629     c                   Move      AcrCcc        V1Ctr
066330040629     c                   EndIf
066340030902     c                   Else
066350030902     c                   Eval      V1Ksc = Ds_V1cor12
066360030902     c                   EndIf
066370030902     C  nkf              eval      sav_v1ksc  = V1ksc
066380010309     C                   eval      sav_v1pag = v1pag
066390010208     C                   endif
066400030606      * lo salvo comunque xchè mi può servire nel caso di creazione
066410030606      * bolla x orm commissionato
066420030606     c                   Eval      AcrKscCor = AcrKsc
066430050202     c                   Eval      AcrCccCor = AcrCcc
066440040405
066450040405      * recupero le note del cliente ordinante
066460040405      * se il cliente ritiro non è codificato
066470040405if  1c                   if        ds_v1ccor <> sav_v1ccor and ds_v1ccra = *Zeros
066480040726      * solo se non sono già state immesse
066490040405     c                   If        v1not1 = *Blanks
066500040405     c                   Eval      V1not1 = AcrNo1
066510040405     c                   EndIf
066520040405     c                   If        v1not2 = *Blanks
066530040405     c                   Eval      V1not2 = AcrNo2
066540040405     c                   EndIf
066550040405e   1c                   EndIf
066560050321
066570050321      * imposto se ORM commissionato o meno
066580050323     c                   If        *In01
066590050322     c                             and (sav_v1ccor <> ds_v1ccor or
066600050322     c                                  v1com = *blanks)
066610050321     c                   Eval      v1com = AcrFcc
066620050322     c                   EndIf
066630010208
066640010208     C                   endsr
066650010208      **********************************************************************
066660010208      * CONTROLLI     * dati cliente ordinante *
066670010208      **********************************************************************
066680010208     C     Sr_Contrrso   begsr
066690030922
066700030930     c                   If        V1loo = *Blanks or
066710030930     c                             (v1loo <> sav_v1loo)
066720030929     c                   Clear                   sav_v1loo35
066730030929     c                   EndIf
066740030922     c                   If        sav_v1loo35 = *blanks
066750030922     c                   Movel(p)  v1loo         sav_v1loo35
066760030922     c                   EndIf
066770010208
066780010208     C                   clear                   FNLV14DS
066790010208     C                   z-add     *date         I14dri
066800010208     C                   movel     v1rso         I14rsc
066810010208     C                   movel     v1ino         I14ind
066820030922     C                   movel(p)  sav_v1loo35   E14loc
066830010208     C                   movel     v1pro         E14prv
066840010208     C                   movel     v1cao         E14cap
066850010208     C                   movel     v1nao         E14nar
066860161109      /free
066870161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
066880161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
066890161109       //?il pgm di interrogazione cappario
066900161109         IF  ConfAuto;
066910161109           I14batch = 'S';
066920161109         ENDIF;
066930161109      /end-free
066940010208
066950010208     C                   call      'FNLV14R'
066960010208     C                   parm                    KPJBA
066970010208     C                   parm                    FNLV14DS
066980010208
066990010208     C                   movel     I14ind        v1ino
067000010208     C                   movel     E14loc        v1loo
067010030922     C                   movel     E14loc        sav_v1loo35
067020010208     C                   movel     E14prv        v1pro
067030010208     C                   movel     E14cap        v1cao
067040010208     C                   movel     E14nar        v1nao
067050010220
067060010220     C                   if        v1rso <> *blanks or v1ino <> *blanks
067070010220     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
067080010220     C                   eval      *in61 = (O14err = '2')                       Indirizzo
067090010220     C                   eval      *in62 = (O14err = '5')                       CAP
067100010220     C                   eval      *in63 = (O14err = '3')                       Località
067110010220     C                   eval      *in64 = (O14err = '4')                       Provincia
067120010220     C                   eval      *in65 = (O14err = '6')                       Nazione
067130010220
067140010220     C                   if        O14msg <> *blanks
067150010220     C                   movel(p)  O14msg        v1cmsg
067160010220     C                   eval      *IN28 = *ON
067170010220     C                   eval      *in74 = *on
067180010220     C                   goto      endsrcrso
067190010220     C                   endif
067200010220     C                   endif
067210010208
067220010208      * Controlla indirizzo
067230010208     C                   if        v1cao <> *blanks or v1loo <> *blanks or
067240010208     C                             v1pro <> *blanks or v1nao <> *blanks
067250010208     C                   exsr      Sr_Contrcapo
067260010208     C                   eval      *in74 = (*in28 = *on)
067270010208     C   28              goto      endsrcrso
067280010208     C                   endif
067290010208      * Se paga l'ordinante devo pulire il codice cliente che paga
067300010329     C                   if        v1pag = 'O'
067310010507     C                             and (v1pag <> sav_v1pag
067320010917     C                             or   sav_v1ccor <> ds_v1ccor)
067330010208     C                   clear                   v1ksc
067340010906     C                   clear                   v1ctr
067350010507     C  nkf              clear                   sav_v1ksc
067360010329     C                   eval      sav_v1pag = v1pag
067370010208     C                   endif
067380010208
067390010208     C     endsrcrso     endsr
067400010208      **********************************************************************
067410010208      * CONTROLLI     * cap - località - provincia *
067420010208      **********************************************************************
067430010208     C     Sr_Contrcapo  begsr
067440010208
067450010208     C                   eval      *in32 = *off
067460010208
067470010208     C                   clear                   TISI95DS
067480010208     C                   clear                   FNLV13DS
067490010208     C                   movel     ' '           I95tla
067500010208     C                   movel     '7'           I95tcn
067510010208     C                   movel     v1cao         I95cap
067520010208     C                   movel     v1nao         I95nar
067530010208     C                   movel     v1ino         I95ind
067540030922     C                   movel(p)  sav_v1loo35   I95loc
067550010208     C                   movel     v1pro         I95prv
067560010208     C                   movel     'S'           I13af0
067570010208     C                   movel     'S'           I13af1
067580010208     C                   movel     'S'           I13sz2
067590010208     C                   movel     'S'           I13la3
067600010208     C                   movel     'S'           I13sz3
067610010208     C                   movel     'S'           I13cnv
067620010208
067630010906      * Gestione errori forzabili: se cambiato CAP/Località
067640010208      * riazzera i valori di errori forzabili
067650010208
067660080910      * CAP
067670080910     C     v1cao         ifne      sav_v1cao
067680080910     C                   movel     v1cao         sav_v1cao
067690080910     C                   clear                   wfzv
067700080910     C                   clear                   wfz1
067710080910     C                   clear                   wfz2
067720080910     C                   clear                   wfz3
067730080910     C                   eval      *in32 = *on
067740080910     C                   endif
067750010208
067760010208      * Modifica località
067770010208
067780010208     C     v1loo         ifne      sav_v1loo
067790010208     C                   clear                   wfz1
067800010208     C                   clear                   wfz2
067810010208     C                   clear                   wfz3
067820010208     C                   movel     v1loo         sav_v1loo
067830010208     C                   eval      *in32 = *on
067840010208     C                   endif
067850010208
067860010208      * Modifica provincia
067870010208
067880010208     C     v1pro         ifne      sav_v1pro
067890010208     C                   clear                   wfz3
067900010208     C                   movel     v1pro         sav_v1pro
067910010208     C                   eval      *in32 = *on
067920010208     C                   endif
067930010208
067940010208     C                   movel     wfzv          E13fzv
067950010208     C                   movel     wfz1          E13fz1
067960010208     C                   movel     wfz2          E13fz2
067970010208     C                   movel     wfz3          E13fz3
067980010208
067990010208     C                   call      'FNLV13R'
068000010208     C                   parm                    KPJBA
068010010208     C                   parm                    FNLV13DS
068020010208     C                   parm                    TISI95DS
068030010208
068040010208      * Ricontrolla errori
068050010208
068060010208     C                   movel     O13err        werr
068070010208     C                   movel     O13msg        wmsg
068080010208     C                   movel     E13fzv        wfzv
068090010208     C                   movel     E13fz1        wfz1
068100010208     C                   movel     E13fz2        wfz2
068110010208     C                   movel     E13fz3        wfz3
068120010208
068130010208      * Se il livello di affidabilità non è inferiore a 2 e
068140010208      * il livello di reperimento dati non è inferiore a 3
068150010208      * reimposta i dati da TISI95DS
068160010208
068170010208     C     O13ron        ifeq      'S'
068180110120     C                   movel     O95nar        v1nao
068190110120     c                   endif
068200110120     C     O13roc        ifeq      'S'
068210110120     C                   movel     O95cap        v1cao
068220110120     c                   endif
068230110120     C     O13rop        ifeq      'S'
068240110120     C                   movel     O95prv        v1pro
068250110120     c                   endif
068260110120     C     O13rol        ifeq      'S'
068270010208     C                   movel     O95loc        v1loo
068280030922     C                   movel     O95loc        sav_v1loo35
068290010208     C                   end
068300161109
068310161109      /free
068320161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
068330161109       //?conferma automatica batch non devo dare errore
068340161109         IF  ConfAuto and O13roc = 'S';
068350161109           clear werr;
068360161109           clear O13roc;
068370161109         ENDIF;
068380161109      /end-free
068390010208
068400010208     C     werr          ifne      ' '
068410010208     C     wmsg          andne     *blanks
068420080910     C     *in32         oreq      *on
068430010208
068440010208     C                   select
068450010208
068460010208     C     werr          wheneq    '3'
068470010208     C                   eval      *in63 = *on
068480010208     C                   eval      *in28 = *on
068490010208
068500010208     C     werr          wheneq    '5'
068510010208     C                   eval      *in62 = *on
068520010208     C                   eval      *in28 = *on
068530010208
068540010208     C     werr          wheneq    '4'
068550010208     C                   eval      *in64 = *on
068560010208     C                   eval      *in28 = *on
068570010208
068580010208     C     werr          wheneq    '6'
068590010208     C                   eval      *in65 = *on
068600010208     C                   eval      *in28 = *on
068610010208     C                   endsl
068620010208      *
068630010208     C                   movel(p)  wmsg          v1cmsg
068640010208     C                   end
068650010208
068660010208     C   28              goto      endsrcapo
068670010208
068680010208      * Controlla se reimpostata località
068690010208
068700010208     C     O13rol        ifeq      'S'
068710010208     C                   eval      *in63 = *on
068720010208     C                   eval      *in28 = *on
068730010208     C                   movel     msg(19)       v1cmsg
068740010208     C                   goto      endsrcapo
068750010208     C                   end
068760010208
068770010208      * Controlla se reimpostata Provincia
068780010208
068790010208     C     O13rop        ifeq      'S'
068800010208     C                   eval      *in64 = *on
068810010208     C                   eval      *in28 = *on
068820010208     C                   movel     msg(21)       v1cmsg
068830010208     C                   goto      endsrcapo
068840010208     C                   end
068850010208
068860010208      * Controlla se reimpostato cap
068870010208
068880010208     C     O13roc        ifeq      'S'
068890010208     C                   eval      *in62 = *on
068900010208     C                   eval      *in28 = *on
068910010208     C                   movel     msg(20)       v1cmsg
068920010208     C                   goto      endsrcapo
068930010208     C                   end
068940010208
068950010208      * Controlla se reimpostata Nazione
068960010208
068970010208     C     O13ron        ifeq      'S'
068980010208     C                   eval      *in65 = *on
068990010208     C                   eval      *in28 = *on
069000010208     C                   movel     msg(22)       v1cmsg
069010010208     C                   goto      endsrcapo
069020010208     C                   end
069030010208
069040010208     C     endsrcapo     endsr
069050050323
069060050323      *--------------------------------------------------------------------*
069070050323      * CONTROLLO PADRE/FIGLI ordinante con codice ritiro
069080050323      *--------------------------------------------------------------------*
069090050323     c     Sr_Figli      BegSr
069100050323
069110050323      * controllo se l'ordinante è un papà
069120050323     c                   Clear                   Tibs10ds
069130050323     c                   Eval      d10tle = 'WW'
069140050323     c                   Eval      d10paf = 'F'
069150050323     c                   Move      ds_v1cor12    d10cod
069160050323     c                   Eval      d10drf = *date
069170050323     c                   Call      'TIBS10R'
069180050323     c                   Parm                    Tibs10ds
069190050323if  1c                   If        d10err <> *Blanks
069200050323      * controllo se il codice inserito è un figlio
069210050323     c                   Clear                   Tibs10ds
069220050323     c                   Eval      d10tle = 'WW'
069230050323     c                   Eval      d10paf = 'P'
069240050323     c                   Move      ds_v1cor12    d10cod
069250050323     c                   Eval      d10drf = *date
069260050323     c                   Call      'TIBS10R'
069270050323     c                   Parm                    Tibs10ds
069280050323if  2c                   If        d10err <> *Blanks
069290050323     c                   Clear                   skfigli
069300050323e   2c                   EndIf
069310050323e   1c                   EndIf
069320050323
069330050323     c                   EndSr
069340050323
069350001016      **********************************************************************
069360001016      * CONTROLLI     * P.O. ritiro *
069370001016      **********************************************************************
069380001016     C     Sr_Contrpor   begsr
069390131024
069400010413      * Controllo il p.o. di ritiro
069410010413     C                   clear                   v1dpor
069420010202
069430010413      * Ricerca P.O.Ritiro  con "?"
069440010413     C     '?'           scan      v1cpor                                 30
069450010413     C     *in30         ifeq      *on
069460010413     C                   eval      *in59 = *on
069470010413     C                   eval      *in90 = *on
069480010413     C                   clear                   ds_tnsd
069490010413     C                   call      'TNSD24R'
069500010413     C                   parm                    ds_cod
069510010413     C                   parm                    ds_tip
069520010413     C                   parm                    ds_des
069530010413     C                   parm                    ds_pra
069540010413     C                   movel     ds_cod        v1cpor
069550010413     C                   movel     ds_des        v1dpor
069560010413     C                   endif
069570001023
069580010316      * Solo campi numerico
069590010320     C                   if        v1cpor = *blanks and
069600010320     C                             (%subst(v1cpor:1:1) < '0' and
069610010320     C                              %subst(v1cpor:1:1) <> ' ') or
069620010320     C                             (%subst(v1cpor:2:1) < '0' and
069630010320     C                              %subst(v1cpor:2:1) <> ' ') or
069640010320     C                             (%subst(v1cpor:3:1) < '0' and
069650010320     C                              %subst(v1cpor:3:1) <> ' ')
069660010316     C                   seton                                        59  28
069670010316     C                   movel     msg(33)       v1cmsg
069680010316     C                   goto      endsrcpor
069690010316     C                   endif
069700010202      * Obbligatorio
069710010315     C                   if        v1cpor = *blanks
069720010202     C                   movel     msg(11)       v1cmsg
069730010202     C                   seton                                        59  28
069740010202     C                   goto      endsrcpor
069750010202     C                   endif
069760060109
069770060109      * Se immissione ORM e p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
069780060109      * come p.o. ritiro OMR
069790060110     c                   If        (*In01 or *In03 ) and wforzapor = *Off
069800060109     c                   Clear                   fnlv55ds
069810060109     c                   Eval      d55tpt = '6'
069820060109     c                   Move      v1cpor        d55lin
069830060109     c                   Eval      d55drf = oggi_aammgg
069840060109     c                   Call      'FNLV55R'
069850060109     c                   Parm                    Fnlv55ds
069860060109     c                   If        d55err = *Blanks
069870060109     c                   Move      d55tfa        v1cpor
069880060109     c                   Eval      wforzapor = *On
069890060109     c                   EndIf
069900060109     c                   EndIf
069910151203
069920151203      /free
069930151203       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
069940151203       //?solo se immissione/copia ma no conferma ritiri fissi
069950151203       //?o manutenzione dopo dirottamento
069960151203         IF  ((*in01 or *in03) and §RMtco <> 'S') or
069970151203              (*in09 and wokdirotta = *on);
069980151203           clear FIOR96ds;
069990151203       //?se la filiale emissione non è ancora stata impostata passo la filiale utente
070000151203           IF  V1Cpoe = *zeros;
070010151203             IOR96poe = DUTpou;
070020151203           ELSE;
070030151203             IOR96poe = V1Cpoe;
070040151203           ENDIF;
070050151203           IOR96por = %dec(V1Cpor:3:0);
070060151203           fior96r(kpjba : fior96ds);
070070151203           IF  OOR96err = *blanks and OOR96por <> *zeros;
070080151203             V1Cpor = %editc(OOR96por:'X');
070090151203           ENDIF;
070100151203         ENDIF;
070110151203      /end-free
070120060109
070130010202      * Controllo
070140131112     c                   clear                   sav_ORGfl1
070150131210     c                   clear                   ntw_V1Cpor
070160001016     C                   movel     v1cpor        kazorg
070170001016     C     kazorg        chain     azorg01L
070180010202     C                   if        not%found(azorg01l)
070190010202     C                   movel     msg(33)       v1cmsg
070200010202     C                   seton                                        59  28
070210010202     C                   goto      endsrcpor
070220010202     C                   endif
070230001204     C                   if        ORGfva <> *blanks  or
070240001204     C                             (ORGfag <> 'A' and ORGfag <> 'F')
070250001017     C                   movel     msg(33)       v1cmsg
070260001016     C                   seton                                        59  28
070270001019     C                   goto      endsrcpor
070280001019     C                   endif
070290131112     c                   eval      sav_ORGfl1 = ORGfl1
070300080812     C                   movel     orgde3        og143
070310010206     C                   movel     ORGde8        og148
070320131210     c                   eval      ntw_V1Cpor = §OGntw
070330010206      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
070340010206     C                   if        §ogorm <> 'S'
070350010206     C                   movel     msg(36)       v1cmsg
070360010206     C                   seton                                        59  28
070370010206     C                   goto      endsrcpor
070380010206     C                   endif
070390131024
070400131024     C                   movel     ORGdes        v1dpor
070410131024     C                   movel     v1cpor        num_v1cpor
070420131024
070430131024      /free
070440150310       //?Imposto se filiale ritiro ha la procedura PDA attiva
070450150310         *in22 = (§ogpdaorm <> *blanks);
070460131024      /end-free
070470131024
070480060110      * Se il p.o. immesso a video è un p.o. gestito
070490060110      * errore per p.o. non gestibile
070500060110     c                   Clear                   fnlv55ds
070510060110     c                   Eval      d55tpt = '6'
070520060110     c                   Move      v1cpor        d55lin
070530060110     c                   Eval      d55drf = oggi_aammgg
070540060110     c                   Call      'FNLV55R'
070550060110     c                   Parm                    Fnlv55ds
070560060110     c                   If        d55err = *Blanks and d55tfa <> d55lin
070570060110     c                   movel     msg(89)       v1cmsg
070580060110     c                   seton                                        59  28
070590060110     c                   goto      endsrcpor
070600060110     c                   EndIf
070610080812
070620080812      * controllo se ritiro all'estero possibile
070630131030     c                   if        ntw_V1Cpor = 'DPD' or ntw_V1Cpor = 'EEX' or
070640131030     c                             ntw_V1Cpor = 'FED'
070650080812     c                   clear                   fnlv12ds
070660111206     c                   clear                   tisi95ds1
070670080812     c                   clear                   tisi97ds
070680111206     c                   eval      ai95nar = v1nar
070690111206     c                   eval      ai95cap = v1car
070700111206     c                   eval      ai95dat = oggi_aammgg
070710131030     c                   eval      i97ntw = ntw_V1Cpor
070720081215      * se impostato network DPD lo passo
070730081215     c                   if        v1ntwd <> *blanks
070740111206     c                   eval      ai95fi1 = v1ntwd
070750081215     c                   else
070760111206     c                   clear                   ai95fi1
070770081215     c                   endif
070780080812     c                   call      'FNLV12R'
070790080812     c                   parm                    fnlv12ds
070800111206     c                   parm                    tisi95ds1
070810080812     c                   parm                    tisi97ds
070820080812     c                   if        o12err <> *blanks
070830080812     c                   eval      v1cmsg = o12msg
070840080812     c                   seton                                        59  28
070850080812     c                   goto      endsrcpor
070860080812     c                   endif
070870080812     c                   endif
070880081215      * controllo congruenza tra network DPD e po ritiro
070890081215      * in immissione o copia
070900131030     c                   if        v1ntwd <> *blanks and ntw_V1Cpor <> 'DPD'
070910081215     c                             and (*in01 or *in03)
070920081215     c                   eval      *in28 = *on
070930081215     c                   eval      *in59 = *on
070940081215     c                   eval      v1cmsg = 'Indicato Network DPD. Incongruenza-
070950081215     c                              con la filiale ritiro'
070960081215     c                   goto      endsrcpor
070970081215     c                   endif
070980100219
070990100219      /free
071000100219       //?Se inserito ntw DPD richiamo il controllo del cappario DPD
071010110216       //?oppure filiale ritiro è DPD
071020131030       IF  v1ntwd <> *blanks or ntw_V1Cpor = 'DPD';
071030100811         wNazione = v1nar;
071040100811         wCapDPD  = v1car;
071050100811         exsr CtrCapDPD;
071060100811         IF  *in28;
071070100811           *in50 = *on;
071080100811           *in79 = *on;
071090100811           v1cmsg = %trim(v1cmsg) + ' F15 x interrogazione Cappario DPD';
071100100811           leavesr;
071110100811         ENDIF;
071120100219       ENDIF;
071130100219      /end-free
071140080812
071150010515      * Se p.o. estero controllare se DPD o EUROEXPRESS
071160010515     C                   eval      *in14 = *off
071170010515     C                   eval      *in15 = *off
071180131112     C                   if        sav_ORGfl1 = 'E'
071190010515     C                   eval      *in14 = *on
071200010515     C                   endif
071210131030     C                   if        ntw_V1Cpor = 'DPD'
071220010515     C                   eval      *in15 = *on
071230010515     C                   endif
071240010417
071250010213      * Se il p.o. ritiro è della mia £6 l'ORM non può essere prepagato
071260010213      * a meno che non sia richiamato da programma immissione prepagati
071270150609      * in immissione ORM
071280010704     C                   if        v1ctor = 'P' and §RMfpr <> 'B'
071290150609     C                             and §RMfpr <> 'C'
071300010307     C     num_v1cpor    lookup    l6                                     30
071310010307     C                   if        *in30 = *on
071320150609     C                             and *in01
071330010307     C                   movel     msg(48)       v1cmsg
071340160316     c                   eval      H1riga = 05
071350160316     c                   eval      H1colo = 11
071360160316     C                   seton                                            28
071370010307     C                   goto      endsrcpor
071380010307     C                   endif
071390010307     C                   endif
071400010417
071410010417     C                   if        %subst(v1cpor:1:1) = ' '  and
071420010417     C                             %subst(v1cpor:2:1) = ' '
071430010417     C                   movel     '00'          v1cpor
071440010417     C                   endif
071450010417     C                   if        %subst(v1cpor:1:1) = ' '
071460010417     C                   movel     '0'           v1cpor
071470010417     C                   endif
071480010417
071490010417     C                   if        v1cpor <> sav_v1cpor
071500010417     C                   eval      *in89 = *off
071510161116     c                   IF        sav_v1cpor <> *zeros and
071520161116     c                             sav_v1cpor <> *blanks
071530170613     c                             and wokdirotta = *off
071540161116     c                   eval      wModPor = *on
071550161116     c                   ENDIF
071560010417     C                   movel     v1cpor        sav_v1cpor
071570071023     c                   clear                   v1ccgi
071580071023     c                   clear                   sav_v1ccgi
071590071023     c                   clear                   sav_acrcgi
071600010417     C                   endif
071610010515
071620010521      * Se p.o. ritiro Euroexpress o DPD obbligatorio il peso e
071630010521      * i colli
071640010521     C                   if        *in14 = *on or *in15 = *on
071650010521     C                   if        v1pkg = *zeros
071660010515     C                   movel     msg(5)        v1cmsg
071670010515     C                   seton                                        44  28
071680010515     C                   goto      endsrcpor
071690010521     C                   endif
071700010521     C                   if        v1ncl = *zeros
071710010521     C                   movel     msg(4)        v1cmsg
071720010521     C                   seton                                        43  28
071730010521     C                   goto      endsrcpor
071740010521     C                   endif
071750081216      * no prepagato e no pagamento a carico del mittente
071760081216     c                   if        v1ctor = 'P'
071770160316     c                   eval      H1riga = 05
071780160316     c                   eval      H1colo = 11
071790081216     c                   eval      *in28 = *on
071800081216     c                   eval      v1cmsg = 'Per ORM Export non si può fare un -
071810081216     c                             prepagato'
071820081216     c                   goto      endsrcpor
071830081216     c                   endif
071840081216     c                   if        v1pag = 'M'
071850081216     c                   eval      *in28 = *on
071860081216     c                   eval      *in53 = *on
071870081216     c                   eval      v1cmsg = 'Per ORM Export non può pagare il -
071880081216     c                             mittente'
071890081216     c                   goto      endsrcpor
071900081216     c                   endif
071910010515     C                   endif
071920080422
071930080422      * se filiale ritiro DPD controllo i colli
071940080422      * deve essere obbligatoriamente 1
071950080422     c                   if        *in15 = *on and v1ncl > 1
071960080422     c                   eval      v1cmsg = msg(90)
071970080422     c                   eval      *in28 = *on
071980080422     c                   eval      *in43 = *on
071990080422     c                   leavesr
072000080422     c                   endif
072010081126      * se orm export DPD il peso non può essere maggiore del limite stabilito
072020081126     c                   if        *in15 and v1pkg > §3ipkd
072030081126     c                   eval      v1cmsg = msg(92)
072040081126     c                   eval      *in28 = *on
072050081126     c                   eval      *in44 = *on
072060081126     c                   leavesr
072070081126     c                   endif
072080081126      * se orm export DPD obbligatorio il destinatario
072090081127     c                   if        *in15 and ds_v1ccrc = *zeros and
072100081126     c                             v1rsc = *blanks
072110081126     c                   eval      v1cmsg = msg(93)
072120081126     c                   eval      *in28 = *on
072130081211     c                   eval      *in56 = *on
072140081126     c                   leavesr
072150081126     c                   endif
072160010417
072170010417      * Controllo p.o. ritiro del video con quello calcolato
072180020523      * non in chiusura orm
072190050711      * non se p.o. ritiro protetto o se è richiamato da programma
072200050711      * immissione prepagati
072210060109      * se non forzato da me
072220150921      * non controllo se conferma automatica VAS
072230150921     c                   IF        not ConfAuto
072240050711     c                   If        Not *In09 or §rmfpr = 'B'
072250060109     c                   if        wforzapor = *Off
072260010417     C                   if        v1cpos <> *zeros     and
072270010417     C                             v1cpos <> num_v1cpor and
072280020523     C                             *in89 = *off and *in10 = *off
072290010417     C                   movel     msg(62)       v1cmsg
072300010417     C                   seton                                        895928
072310010417     C                   goto      endsrcpor
072320010417     C                   endif
072330010417     C                   if        sav_acrpoa <> *zeros     and
072340010417     C                             sav_acrpoa <> num_v1cpor and
072350150921     C                             *in89 = *off and not *in10
072360010417     C                   movel     msg(62)       v1cmsg
072370010417     C                   seton                                        895928
072380010417     C                   goto      endsrcpor
072390010417     C                   endif
072400131111     C                   if        sav_O95lna <> *zeros       and
072410131111     C                             sav_O95lna <> num_v1cpor   and
072420010417     C                             sav_acrpoa = *zeros    and
072430150921     C                             V1Cpos = *zeros    and
072440150921     C                             *in89 = *off and not *in10
072450010417     C                   movel     msg(62)       v1cmsg
072460010417     C                   seton                                        895928
072470010417     C                   goto      endsrcpor
072480010417     C                   endif
072490060109     c                   EndIf
072500040420     c                   EndIf
072510150921     c                   ENDIF
072520010417
072530010417     C                   movel     v1cpor        sav_v1cpor
072540071031
072550071031      * recupero gli ambiti per il recupero del giro da ACR
072560071031     c                   exsr      sr_caragr
072570010301
072580001016     C     endsrcpor     endsr
072590070920
072600070913      *------------------------------------------------------------------------*
072610070913      * CONTROLLI     * giro
072620070913      *------------------------------------------------------------------------*
072630070913     c     sr_contrcgi   begsr
072640070919
072650070919     c                   clear                   v1dcgi
072660160325     c                   eval      wModGiroAut = *off
072670071105
072680071105      * ricerca giro
072690071105     c                   if        %scan('?':v1ccgi) > *zeros
072700071105     c                   clear                   fidg09ds
072710071105     c                   eval      d09iop0 = 'P01'
072720071105     c                   eval      d09ifgs = num_v1cpor
072730071105     c                   eval      d09idat = *date
072740071105     c                   eval      d09itug = 'R'
072750071105     c                   eval      kpjbu = fidg09ds
072760071105     c                   call      'FIDG09R'
072770071105     c                   parm                    kpjba
072780071105     c                   eval      fidg09ds = kpjbu
072790071105      *     imposto il giro scelto
072800071105     c                   if        d09ocgi <> *blanks
072810071105     c                   eval      v1ccgi = d09ocgi
072820071105     c                   endif
072830071105     c                   endif
072840070919
072850071023      * Aggancio ACR e recupero il giro
072860071031     c                   if        (sav_acrcgi = *blanks and
072870071031     c                              ds_v1ccra <> *zeros)
072880131212     c                             or ($varqta = *on and
072890131212     c                                 sav_acrcgi = v1ccgi)
072900071023     c                   exsr      sr_carcgi
072910071023     c                   endif
072920071023
072930071023      * se giro non immesso da utente imposto il giro recuperato
072940071031      * oppure se variate le quantità
072950071031     c                   if        (sav_v1ccgi = *blanks and
072960071031     c                              sav_acrcgi <> *blanks)
072970131018     c                             or (sav_v1ccgi <> sav_acrcgi
072980131018     c                                 and sav_acrcgi <> *blanks)
072990131212     c                             or ($varqta = *on and
073000131212     c                                (sav_acrcgi = v1ccgi or
073010131212     c                                 sav_v1ccgi = v1ccgi))
073020070920     c                   eval      v1tgi = 'A'
073030070920     c                   eval      v1ccgi = sav_acrcgi
073040070920     c                   eval      sav_v1ccgi = sav_acrcgi
073050070920     c                   endif
073060160325      /free
073070160325       //?Se immissione o copia ORM
073080160325       //?Se variato il mittente e ora è un NON codificato
073090160429       //?Se il giro è rimasto lo stesso di prima
073100160429         IF  (*in01 or *in03) and
073110160429             wModCodCra and ds_V1Ccra = *zeros and
073120160429             V1Ccgi = sav_V1Ccgi and sav_ACRcgi = *blanks;
073130160429           clear V1tgi;
073140160429           clear V1Ccgi;
073150160429           clear sav_V1Ccgi;
073160160429           wModGiroAut = *on;
073170160429           *in28 = *on;
073180160429           *in90 = *on;
073190160429           V1Cmsg = msg(98);
073200160429           leavesr;
073210160325         ENDIF;
073220160325      /end-free
073230070913      *     controllo
073240070913     c                   if        v1ccgi <> *blank
073250070913     c                   exsr      ctrgiro
073260070913      *     giro errato
073270070924     c                   if        d09oerr = '1' and not *in24
073280070913     c                   eval      *in28 = *on
073290070913     c                   eval      *in51 = *on
073300131030     c                   eval      w2cmsg = msg(43)
073310070913     c                   leavesr
073320070913     c                   endif
073330070913      *     imposto la descrizione del giro
073340071011     c                   if        d09ocgi <> *blanks
073350070913     c                   eval      v1dcgi = d09odes
073360070913     c                   endif
073370070913      * se giro diverso da quello salvato imposto che giro manuale
073380070919     c                   if        v1ccgi <> sav_v1ccgi
073390070913     c                   eval      v1tgi = 'M'
073400070913     c                   endif
073410070913     c                   endif
073420070913
073430070913     c                   endsr
073440070913
073450070913      *------------------------------------------------------------------------*
073460070913      *   Controllo e decodifica giro
073470070913      *------------------------------------------------------------------------*
073480070913     c     ctrgiro       begsr
073490070913
073500070913     c                   clear                   fidg09ds
073510070913     c                   eval      d09iop0 = '001'
073520071011     c                   eval      d09ifgs = num_v1cpor
073530070913     c                   eval      d09idat = *date
073540070913     c                   eval      d09icgi = v1ccgi
073550070913     c                   eval      d09itug = 'R'
073560070913     c                   eval      kpjbu = fidg09ds
073570070913     c                   call      'FIDG09R'
073580070913     c                   parm                    kpjba
073590070913     c                   eval      fidg09ds = kpjbu
073600070913
073610070913     c                   endsr
073620070913
073630010202      **********************************************************************
073640010202      * CONTROLLI     * fase *
073650010202      **********************************************************************
073660010202     C     Sr_Contrfao   begsr
073670010202
073680010723      * Se non ho la data e ora fase caricata le imposto con data e ora del sistema
073690140107     c                   IF        *in01
073700050422     C                   if        v1dfo = *zeros
073710050422     C                   eval      v1dfo = oggi_aammgg
073720010723     C                   endif
073730050422     C                   if        v1ofo = *zeros
073740050422     C                   time                    w0140
073750010723     C                   movel     w0140         v1ofo
073760010202     C                   endif
073770010202     C                   endif
073780010202      * Decodifica la fase
073790010202     C                   if        v1fao <> *zeros
073800010202     C                   clear                   TIBS02DS
073810010202     C                   movel     'C'           T02mod
073820010202     C                   movel     knsif         t02sif
073830010202     C                   movel     'FAR'         t02cod
073840010202     C                   movel(p)  v1fao         T02ke1
073850010202     C                   call      'TIBS02R'
073860010202     C                   parm                    KPJBA
073870010202     C                   parm                    TIBS02DS
073880010314     C                   movel     t02uni        dfar
073890010314     C                   movel     d§fardes      v1dfao
073900070706     c                   eval      v1eti = d§farass
073910010202     C                   endif
073920010202
073930010202     C                   endsr
073940130917      /free
073950130917       //--------------------------------------------------------------
073960130917       //?Controllo ora ritiro merce con ora massima per il ritiro.
073970130917       //--------------------------------------------------------------
073980130917       BEGSR contrORRmax;
073990130917
074000131024       //?Controllo ora pronta merce se non è presa da anagrafica clienti
074010131022         SELECT;
074020140320         //?Se ora ritiro a 0 = commissionato
074030140320         //?Se ora fine servizio a 0 = ORM EXPORT no orari servizio
074040140320         //?per ora ritiro a 0 ci pensa la routine CONTRORR
074050140320         //?se non ci sono orari servizio non devo controllare
074060161104         //?non controllo se la data ritiro è maggiore della data pronta merce
074070161028           WHEN  V1orr = 0 or V1ostfs = 0;
074080140414           WHEN  sav_ACRorr <> *zeros and V1orr <= sav_ACRorr;
074090161104           WHEN  inv_v1dar > inv_v1dpm;
074100131022           OTHER;
074110131022       //?Controllo ora pronta merce con ora fine standard
074120161028       //?o con ora max
074130161028           IF  V1orr > V1ostfs;
074140131022             V1Cmsg = msg(12);
074150131022             V1Cmsg = %trim(V1Cmsg) + ' (' +
074160161028                      %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074170161028                      %subst(%editc(V1ostfs:'X'):3:2) + ')';
074180131022             *in28 = *on;
074190131022             *in58 = *on;
074200131022           ENDIF;
074210131022         ENDSL;
074220130917
074230140317       //?Controllo ora immissione ORM con ora fine standard
074240161028       //?o con ora max
074250161028         IF  V1ostfs > 0 and
074260161028             %dec(%subst(%editc(v1oao:'X'):1:4):4:0) > V1ostfs and
074270161028             inv_V1dao = inv_V1dar;
074280131016           V1Cmsg = msg(32);
074290130920           V1Cmsg = %trim(V1Cmsg) + ' (' +
074300161028                    %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074310161028                    %subst(%editc(V1ostfs:'X'):3:2) + ')';
074320130917           *in28 = *on;
074330130917         ENDIF;
074340130917
074350130917       ENDSR;
074360131014
074370131014       //--------------------------------------------------------------
074380131112       //?Controllo orari di apertura.
074390131014       //--------------------------------------------------------------
074400131112       BEGSR OrariApertura;
074410131017
074420131112       //?Richiamo pgm per controllo orari ritiro
074430131112       //?per ora il pgm deve controllare anche che ci sia abbastanza
074440131112       //?spazio nella nota1 e nella nota2 per poterli inserire in
074450131112       //?questi campi per poi inviarli a PDA
074460131112         IF  V1OraAMda > 0 or V1OraAMa > 0 or
074470131112             V1OraPMda > 0 or V1OraPma > 0;
074480131112           clear TRUL03DS;
074490131112           I03hm1 = V1OraAMda;
074500131112           I03hm2 = V1OraAMa;
074510131112           I03hm3 = V1OraPMda;
074520131112           I03hm4 = V1OraPMa;
074530131112           trul03r (TRUL03DS);
074540131112           IF  O03err <> *off;
074550131112             V1Cmsg = O03msg;
074560131112             *in28 = *on;
074570131112             SELECT;
074580131112               WHEN  O03errpos = 1;
074590131112                 H1riga = 12;
074600131112                 H1colo = 21;
074610131112               WHEN  O03errpos = 2;
074620131112                 H1riga = 12;
074630131112                 H1colo = 28;
074640131112               WHEN  O03errpos = 3;
074650131112                 H1riga = 12;
074660131112                 H1colo = 38;
074670131112               WHEN  O03errpos = 4;
074680131112                 H1riga = 12;
074690131112                 H1colo = 45;
074700131112               WHEN  O03errpos = 5;
074710131112                 *in82 = *on;
074720131112               WHEN  O03errpos = 6;
074730131112                 H1riga = 14;
074740131112                 H1colo = 43;
074750131112             ENDSL;
074760131112           ENDIF;
074770131112         ENDIF;
074780131107
074790131014       ENDSR;
074800100811
074810100811       //--------------------------------------------------------------
074820100811       //?Controllo cappario DPD.
074830100811       //--------------------------------------------------------------
074840100811       BEGSR CtrCapDPD;
074850100811
074860171016         clear tisid4ds;
074870171016         isid4dri = oggi_aammgg;
074880100811         IF  inv_v1dar = 0;
074890171016           isid4dri = oggi_aammgg;
074900100811         ELSE;
074910171016           isid4dri = inv_v1dar;
074920100811         ENDIF;
074940171016         isid4nzd = wNazione;
074950171016         isid4cad = wCapDPD;
074960100811         IF  v1pkg > §3ilmp;
074970171016           isid4sorc = 101;
074980100811         ELSE;
074990171016           isid4sorc = 136;
075000100811         ENDIF;
075010100811         IF  v1cpoe = *zeros;
075020171016           isid4lnp = DUTpou;
075030100811         ELSE;
075040171016           isid4lnp = v1cpoe;
075050100811         ENDIF;
075060171016         tisid4r (tisid4ds);
075070171016         IF  osid4err <> *blanks;
075080100811           *in28 = *on;
075090100811           *in62 = *on;
075100100811           v1cmsg = 'CAP non trovato su cappario DPD.';
075110100811           leavesr;
075120100811         ENDIF;
075130100811
075140100811       ENDSR;
075150161019
075160161019       //--------------------------------------------------------------
075170161019       //?Controllo Data Pronta Merce.
075180161019       //--------------------------------------------------------------
075190161019       BEGSR ContrDataProntaMerce;
075200161020
075210161020       //?Se sono in immissione o copia e la data è a 0 la imposto = oggi
075220161020         IF  (*in01 or *in03) and V1dpm = 0;
075230161020           V1dpm = oggi_ggmmaa;
075240161028           IF  sav_v1dpm > 0;
075250161028             clear sav_v1dpm;
075260161028           ELSE;
075270161028             sav_V1dpm = V1dpm;
075280161028           ENDIF;
075290161020         ENDIF;
075300161019
075310161019       //?La data è obbligatoria
075320161019         IF  V1dpm = 0;
075330161019           *in28 = *on;
075340161019           *in52 = *on;
075350161020           V1Cmsg = 'Immettere la Data Pronta Merce';
075360161019           leavesr;
075370161019         ENDIF;
075380161019
075390161019       //?Data Valida
075400161019         clear inv_V1dpm;
075410161019         clear wlbdat;
075420161019         G02dat = V1dpm;
075430161019         xsrda8 (wlbdat);
075440161019         IF  G02err = '1';
075450161019           *in28 = *on;
075460161019           *in52 = *on;
075470161020           V1Cmsg = 'Data Pronta Merce errata';
075480161019           leavesr;
075490161019         ENDIF;
075500161019         V1dpm = G02dat;
075510161019         inv_V1dpm = G02inv;
075520170303
075530170303       //?Data non più vecchia di 7 gg da oggi
075540170427       //?No se sono in chiusura ORM
075550170427         IF  not *in10;
075560170303         clear wdata;
075570170303         dataiso = %date(oggi_aammgg:*iso);
075580170303         wdata = %dec(dataiso - %days(7));
075590170303         IF  inv_V1dpm < wdata;
075600170303           *in28 = *on;
075610170303           *in52 = *on;
075620170303           V1Cmsg = 'Data Pronta Merce errata. Inferiore a 7 gg da oggi';
075630170303           leavesr;
075640170303         ENDIF;
075650170427         ENDIF;
075660161115
075670161115       //?Se sono in manutenzione non devo ricalcolare la data ritiro
075680161115       //?se la data pronta merce immessa è inferiore alla data pronta merce dell'ORM
075690161115         IF  *in02 and §OREdpm > 0 and inv_v1dpm <= §OREdpm;
075700161115           leavesr;
075710161115         ENDIF;
075720161028
075730161028         IF  V1dpm <> sav_V1dpm;
075740161028           wModDpm = *on;
075750161028           sav_V1dpm = V1dpm;
075760161028         ENDIF;
075770161019
075780161019       ENDSR;
075790100811      /end-free
075800100204
075810100204      *--------------------------------------------------------------------*
075820100204      * Imposto le personalizzazioni di ORMFLO da tabella PVO
075830100204      *--------------------------------------------------------------------*
075840100204     c     sr_pvo        begsr
075850100204
075860100204     c                   eval      ok_pvo = *off
075870100204
075880100204      * aggancio la tabella per:
075890100204      * --> ordinante lungo 10
075900100205     c                   clear                   dpvo
075910100204     c                   clear                   tibs02ds
075920100204     c                   eval      t02mod = 'C'
075930100204     c                   eval      t02sif = knsif
075940100204     c                   eval      t02cod = 'PVO'
075950100205     c                   eval      t02ke1 = %editc(ds_v1ccor:'X')
075960100204     c                   eval      t02ke2 = 'O'
075970100204     c                   call      'TIBS02R'
075980100204     c                   parm                    kpjba
075990100204     c                   parm                    tibs02ds
076000100204     c                   if        t02err = *blanks
076010100204     c                   eval      dpvo = t02uni
076020100204     c                   eval      OK_pvo = *on
076030100204     c                   endif
076040100204      * --> ordinante lungo 7
076050100204     c                   if        not OK_pvo
076060100204     c                   clear                   dpvo
076070100204     c                   clear                   tibs02ds
076080100204     c                   eval      t02mod = 'C'
076090100204     c                   eval      t02sif = knsif
076100100204     c                   eval      t02cod = 'PVO'
076110100204     c                   eval      t02ke1 = %editc(ds_v1cor12:'X')
076120100204     c                   eval      t02ke2 = 'O'
076130100204     c                   call      'TIBS02R'
076140100204     c                   parm                    kpjba
076150100204     c                   parm                    tibs02ds
076160100204     c                   if        t02err = *blanks
076170100204     c                   eval      dpvo = t02uni
076180100204     c                   eval      OK_pvo = *on
076190100204     c                   endif
076200100204     c                   endif
076210100204      * --> ksc
076220100204     c                   if        not OK_pvo
076230100204     c                   clear                   dpvo
076240100204     c                   clear                   tibs02ds
076250100204     c                   eval      t02mod = 'C'
076260100204     c                   eval      t02sif = knsif
076270100204     c                   eval      t02cod = 'PVO'
076280100204     c                   eval      t02ke1 = %editc(v1ksc:'X')
076290100204     c                   eval      t02ke2 = 'K'
076300100204     c                   call      'TIBS02R'
076310100204     c                   parm                    kpjba
076320100204     c                   parm                    tibs02ds
076330100204     c                   if        t02err = *blanks
076340100204     c                   eval      dpvo = t02uni
076350100204     c                   eval      OK_pvo = *on
076360100204     c                   endif
076370100204     c                   endif
076380100204
076390100204      * se ho trovato uno dei 3 codici in tabella devo personalizzare ORMFLO
076400100204     c                   if        OK_pvo
076410100205
076420100205     c                   if        d§pvocb = 'S'
076430100205     c                   eval      §ormcb = d§pvocb
076440100204     c                   endif
076450100205     c                   if        d§pvofr = 'S'
076460100205     c                   eval      §ormfr = d§pvofr
076470100204     c                   endif
076480100205     c                   if        d§pvoks = 'S'
076490100205     c                   eval      §ormks = d§pvoks
076500100204     c                   endif
076510100205     c                   if        d§pvofd = 'S'
076520100205     c                   eval      §ormfd = d§pvofd
076530100204     c                   endif
076540100205     c                   if        d§pvoic <> *blanks
076550100205     c                   eval      §ormic = d§pvoic
076560100204     c                   endif
076570100204      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
076580100204      * di variazione
076590100204      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
076600100205     c                   if        d§pvosrm = 'S'
076610100205     c                   eval      §orsrm = d§pvosrm
076620100204     c                   eval      v1com = 'S'
076630100204     c                   eval      *in34 = *on
076640100204     c                   clear                   v1dar
076650100204     c                   endif
076660100208     c                   if        *in01 and §rmtla = *blanks
076670100208     c                   eval      v1com = §orcomc
076680100208     c                   endif
076690100812      /free
076700100812       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
076710100812       //?di variazione
076720100812        IF  D§PVOpre = 'M';
076730100812          §ORMpre = D§PVOpre;
076740100812          V1com = 'N';
076750100812          *in34 = *on;
076760100812        ENDIF;
076770100812      /end-free
076780100204
076790100204     c                   endif
076800100208
076810100208     c                   if        *in01 and §rmtla = *blanks and
076820100208     c                             not ok_pvo
076830100208      * devo pulire tutti i dati del campo ormflo
076840100208     c                   clear                   dorm01
076850100208     c                   endif
076860100204
076870100204     c                   endsr
076880071031
076890071031      *--------------------------------------------------------------------*
076900071031      * Carico tabella AGR del po ritiro
076910071031      *--------------------------------------------------------------------*
076920071031     c     sr_caragr     begsr
076930071105
076940071031     c                   clear                   tibs02ds
076950071031     c                   clear                   dagr
076960071112     c                   eval      $noagr = *off
076970071031     c                   eval      t02mod = 'C'
076980071031     c                   eval      t02sif = knsif
076990071031     c                   eval      t02cod = 'AGR'
077000071031     c                   movel(p)  v1cpor        t02ke1
077010071031     c                   call      'TIBS02R'
077020071031     c                   parm                    kpjba
077030071031     c                   parm                    tibs02ds
077040071105     c                   if        t02err = *blanks
077050071031     c                   eval      dagr = t02uni
077060071112     c                   else
077070071112     c                   eval      $noagr = *on
077080071105     c                   endif
077090071031
077100071031     c                   endsr
077110071023
077120071023      *--------------------------------------------------------------------*
077130071023      * Carico il giro da FNACR o da FNORS
077140071023      *--------------------------------------------------------------------*
077150071023     c     sr_carcgi     begsr
077160071023
077170071023     c                   clear                   sav_acrcgi
077180071031
077190071106     c                   eval      kacr1ain = '='
077200071112      * se tabella AGR inserita per filiale ritiro
077210071112     c                   if        $noagr = *off
077220071031      * controllo se devo recuperare il giro Oltre, il giro Sotto o lo Standard
077230071031     c                   if        wpkg > §agrpkgo or wvlm > §agrvlmo or
077240071031     c                             v1bnc > §agrbnco
077250071031     c                   eval      kacr1ain = '>'
077260071031     c                   endif
077270071031     c                   if        wpkg < §agrpkgs and wvlm < §agrvlms and
077280071031     c                             v1bnc < §agrbncs
077290071031     c                   eval      kacr1ain = '<'
077300071031     c                   endif
077310071112     c                   endif
077320071023
077330071023      * da anagrafica clienti ritiro
077340071031     c     kacr1         chain     fnacr13l
077350071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077360071023     c                   eval      sav_acrcgi = acr1cgi
077370071031     c                   else
077380071106     c                   if        kacr1ain <> '='
077390071106     c                   eval      kacr1ain = '='
077400071031     c     kacr1         chain     fnacr13l
077410071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077420071031     c                   eval      sav_acrcgi = acr1cgi
077430071031     c                   endif
077440071031     c                   endif
077450071031     c                   endif
077460071031
077470071023      * se conferma ORM fisso recupero il giro dal fisso se impostato
077480071031      * quindi solo immissione
077490161117     c                   if        §rmfpr = 'S'
077500071023     c     kors          chain     fnors11l
077510071023     c                   if        %found(fnors11l) and ors1cgi <> *blanks
077520071023     c                   eval      sav_acrcgi = ors1cgi
077530071023     c                   endif
077540071023     c                   endif
077550071023
077560071023     c                   endsr
077570100217
077580100218      /free
077590100218       //--------------------------------------------------------------------
077600100218       //?Controllo se OK tutti i dati per il preavviso.
077610100218       //--------------------------------------------------------------------
077620100218       BEGSR sr_Preavviso;
077630100217
077640100218       //?Imposto la chiave per il FNVAOE rcd 'M '
077650100218         k_vaoecor = sav_vaocor;
077660100218         k_vaoepoe = sav_vaopoe;
077670100218         k_vaoensr = sav_vaonsr;
077680100218         k_vaoenor = sav_vaonor;
077690100218         k_vaoenrv = sav_vaonrv;
077700100218         k_vaoetrc = 'M ';
077710100218       //?Aggancio il rcd
077720100812         chain  %kds(k06fnvaoe) FNVAOE1L;
077730100218
077740100218         SELECT;
077750100218       //?Se non trovo il rcd e non c'è il flag di preavviso mail OK
077760100218           WHEN  not %found(FNVAOE1L) and §ormpre <> 'M';
077770100218             clear vaoedati;
077780100218       //?Se trovo il rcd e c'è il flag di preavviso mail OK e controllo la mail
077790100218           WHEN  %found(FNVAOE1L) and §ormpre = 'M';
077800100218             wmail = vaoedati;
077810100218             exsr sr_CtrMail;
077820100223             vaoedati = wmail;
077830100223             IF  $errpre;
077840100223               wpremsg = §emlmsgo;
077850100225               *in94 = *on;
077860100223             ENDIF;
077870100218       //?Se non trovo il rcd e c'è il flag di preavviso mail errore
077880100218           WHEN  not %found(FNVAOE1L) and §ormpre = 'M';
077890100218             clear vaoedati;
077900100223             $errpre = *on;
077910100225             wpremsg = 'Dati incongruenti. ' +
077920100225                       'ORM con preavviso senza indirizzo mail';
077930100218       //?Se trovo il rcd e non c'è il flag di preavviso mail errore
077940100218           WHEN  %found(FNVAOE1L) and §ormpre <> 'M';
077950100223             $errpre = *on;
077960100225             wpremsg = 'Dati incongruenti. ' +
077970100225                       'ORM senza preavviso con indirizzo mail';
077980100218         ENDSL;
077990100218
078000100218       ENDSR;
078010100218
078020100218       //--------------------------------------------------------------------
078030100218       //?Controllo validità indirizzo mail.
078040100218       //--------------------------------------------------------------------
078050100218       BEGSR sr_CtrMail;
078060100218
078070100218         clear dsemail;
078080100223         §emlindi = wmail;
078090100218         xemail (dsemail);
078100100223         $errpre = (§emlerro <> '0');
078110100223         IF  not $errpre;
078120100223           wmail = §emlindo;
078130100223         ENDIF;
078140100218
078150100218       ENDSR;
078160100218
078170100218       //--------------------------------------------------------------------
078180100218       //?Richiesta dati preavviso con window.
078190100218       //--------------------------------------------------------------------
078200100218       BEGSR sr_DatiPre;
078210100218
078220100218         $endpre = *off;
078230100223         *in90 = *off;
078240100223         $okf6pre = *off;
078250100224         *in28 = *off;
078260100218
078270100218       //?emetto la videata con i dati dell'ORM
078280100218         write fior051;
078290100322         IF  $errpre;
078300100322           *in28 = *on;
078310100322         ENDIF;
078320100218       //?emetto la window dove richiedo i dati del preavviso
078330100218         DOW not $endpre;
078340100218           exfmt fior05pre;
078350100218           clear  wpremsg;
078360100218           *in28 = *off;
078370100225           *in94 = *off;
078380100223         SELECT;
078390100223       //?F12-Ritorno
078400100223           WHEN  *inkl;
078410100218             $endpre = *on;
078420100223             *in28 = *off;
078430100223             *in90 = *on;
078440100223             $errpre = *off;
078450100223       //?F6-Conferma
078460100223           WHEN  *inkf;
078470100225             exsr sr_CtrDatiPre;
078480100223             IF  not *in28;
078490100223               IF  wpresino = 'NO';
078500100223                 clear §ormpre;
078510100325           //?Se non è più ORM con preavviso MAIL pulisco il campo di ORM commissionato
078520100325           //?e lo sblocco, così il pgm ricalcola se commissionato o meno e l'utente può
078530100325           //?variarlo
078540100325                 clear V1com;
078550100325                 *in34 = *off;
078560100223               ENDIF;
078570100223               IF  wpresino = 'SI';
078580100223                 §ormpre = 'M';
078590100325           //?Se è ORM con preavviso MAIL imposto che ORM NON commissionato
078600100325           //?e sblocco il campo così l'utente non può variarlo
078610100325                 V1com = 'N';
078620100325                 *in34 = *on;
078630100223               ENDIF;
078640100223               vaoedati = wpremail;
078650100223               $endpre = *on;
078660100223               $okf6pre = *on;
078670100223               $errpre = *off;
078680100223             ENDIF;
078690100223           OTHER;
078700100218       //?controllo i dati
078710100225             exsr sr_CtrDatiPre;
078720100223           ENDSL;
078730100218         ENDDO;
078740100218
078750100218       ENDSR;
078760100218
078770100218       //--------------------------------------------------------------------
078780100218       //?Controlli dati preavviso.
078790100218       //--------------------------------------------------------------------
078800100225       BEGSR sr_CtrDatiPre;
078810100218
078820100218         *in28 = *off;
078830100218
078840100218       //?se preavviso = 'SI' deve esserci la mail
078850100218         IF  wpresino = 'SI' and wpremail = *blanks;
078860100218           *in28 = *on;
078870100225           wpremsg = 'Dati incongruenti. ' +
078880100225                     'ORM con preavviso senza indirizzo mail';
078890100218           leavesr;
078900100218         ENDIF;
078910100218       //?se preavviso = 'NO' NON deve esserci la mail
078920100218         IF  wpresino = 'NO' and wpremail <> *blanks;
078930100218           *in28 = *on;
078940100225           wpremsg = 'Dati incongruenti. ' +
078950100225                     'ORM senza preavviso con indirizzo mail';
078960100218           leavesr;
078970100218         ENDIF;
078980100218       //?la mail deve essere valida
078990100218         IF  wpremail <> *blanks;
079000100223           wmail = wpremail;
079010100218           exsr sr_CtrMail;
079020100223           IF  $errpre;
079030100223             $errpre = *off;
079040100218             *in28 = *on;
079050100225             *in94 = *on;
079060100218             wpremsg = §emlmsgo;
079070100218             leavesr;
079080100218           ENDIF;
079090100223           wpremail = wmail;
079100100218         ENDIF;
079110100218
079120100218       ENDSR;
079130100218
079140100218      /end-free
079150010305
079160030606      **********************************************************************
079170030611      * Imposta flag x Orm commissionato *
079180030606      **********************************************************************
079190030611     c     Sr_OrmCom     BegSr
079200030611
079210030611      * Se p.o. emissione a 0 vuol dire che sono in immissione
079220150305      * in questo caso lo imposto già uguale al p.o. utente
079230030611      * per poter fare il controllo tra p.o. ritiro e p.o. emissione
079240030611     c                   If        v1cpoe = *zeros
079250150305     c                   z-add     DUTpou        v1cpoe
079260030611     c                   EndIf
079270050323      * RICORDA!!!!
079280050324      * Un orm da INTERNET/FILE con codice ordinante e p.o. emissione = a p.o. ritiro
079290050323      * non è un ORM commissionato
079300060103      * Un orm FISSO con codice ordinate e p.o. emissione = a p.o. ritiro (x forza)
079310060103      * non è un ORM commissionato (in automatico l'utente può variarlo)
079320100205
079330100205      * come prima cosa imposto il campo di ORM commissionato se lo recupero dalla
079340100205      * personalizzazione del cliente
079350100205     c                   if        v1com = *blanks
079360100205     c                             and d§pvocomc <> *blanks
079370100205     c                   eval      v1com = d§pvocomc
079380100205     c                   leavesr
079390100205     c                   endif
079400030606
079410030606      * Imposto che è un orm commissionato se:
079420030606      * - Cod.mittente <> Cod.ordinante
079430030606      * - Ordinante presente ma non codificato o 8888 o 9999
079440130911      * - P.o. ritiro <> p.o. emissione
079450130911      *   ma solo se mittente NON codificato
079460030606
079470050323If  1c                   If        V1Com = *Blanks  and
079480030902     c                                             ((Ds_V1Cor12 > *Zeros and
079490040707     c                                              Ds_V1cra12 <> Ds_V1Cor12 and
079500050323     c                                              V1ctco <> 'I' and
079510060103     c                                              V1ctco <> 'F' and
079520060103     c                                              V1ctco <> 'S')
079530030902     c                                              or
079540030902     c                                             (V1rso <> *Blanks   and
079550030902     c                                              (Ds_V1ccor = *Zeros or
079560030902     c                                               Ds_V1cor2 = 8888   or
079570050323     c                                               Ds_V1cor2 = 9999)))
079580030606     c                   Eval      V1Com = 'S'
079590030902     c                   Goto      Endormcom
079600030606    1c                   EndIf
079610050323If  1c                   If        V1Com = *Blanks  and
079620030902     c                                              Num_V1cPor <> V1cpoe
079630130911     c                                         and  ds_V1Ccra = 0
079640030902     c                   Eval      V1Com = 'S'
079650030902     c                   Goto      Endormcom
079660030902    1c                   EndIf
079670030611
079680030902     c     endormcom     EndSr
079690030611      **********************************************************************
079700030611      * Controllo se Ok tutti i dati
079710030611      **********************************************************************
079720030611     c     Sr_ContrCom   BegSr
079730030606
079740030606      * Se è un orm commissionato l'ordinante è obbligatorio
079750030606      * se l'ordinante non c'è devo emettere una window dove chiedo chi è l'ordinante
079760030606If  1c                   If        V1Com = 'S' and
079770030606     c                             V1rso = *Blanks and Ds_V1ccor = *Zeros
079780030610     c                   Eval      whocor = 'S'
079790030610     c                   Eval      *In90 = *On
079800131106      /free
079810131106       //?pulisco il campo del commissionato così viene rifatto il calcolo
079820131106       //?se ORM commissionato o meno
079830131106         clear V1com;
079840131106      /end-free
079850030611      * se non sto facendo una conferma di massa emetto la window
079860030611If 1ac                   If        §rmtla <> '0' and §rmtla <> '1'
079870030610Do  2c                   Do        *Hival
079880030610     c                   Clear                   wdcor
079890030606     c                   Exfmt     Fior05o
079900030610If  3C                   If        *Inkl
079910030606     c                   Leave
079920030610    3c                   EndIf
079930030610If  3c                   If        wdcor <> *Blanks
079940030609     c                   Leave
079950030610    3c                   EndIf
079960030610    2c                   EndDo
079970030611      * se sto facendo una conferma di massa errore
079980030611  x1ac                   Else
079990030611     C                   seton                                        28
080000030611   1ac                   EndIf
080010030606    1c                   EndIf
080020030610
080030030610      * devo controllare il referente e il n. telefono
080040030612If  1c                   If        V1Com = 'S' and Not *In28
080050030610     C                   exsr      Sr_Contrref
080060030610    1c                   EndIf
080070030606
080080030606     c                   EndSr
080090070706
080100070706      *--------------------------------------------------------------------*
080110070706      * Calcolo peso e volume
080120070706      *--------------------------------------------------------------------*
080130070706     c     sr_pesvol     begsr
080140071108
080150071108     c                   clear                   wvlmaut
080160071108     c                   clear                   wpkgaut
080170071108     c                   clear                   wvlmmtc
080180071108     c                   clear                   wpkgmtc
080190071108     c                   clear                   wvlmblc
080200071108     c                   clear                   wpkgblc
080210070706
080220070706     c                   eval      wpkg = v1pkg
080230070706     c                   eval      wvlm = v1vlm
080240070706
080250070706      * se non inserito peso a video lo calcolo per memorizzarlo su fnorg00f
080260070706     c                   if        wpkg = *zeros
080270070706     c                   select
080280070918 b3  c                   when      v1bnc <> *zeros
080290070918     c                   eval      wpkg = v1bnc / d§dftbnc * d§dftpkg
080300070706 b2  c                   when      v1vlm <> *zeros
080310070918     c                   eval      wpkg = v1vlm * d§dftpkg
080320070706 e3  c                   endsl
080330070706      * se troppo alto il peso lo imposto al massimo
080340070706     c                   if        wpkg > 999999,9
080350070706     c                   eval      wpkg = 999999,9
080360070706     c                   endif
080370070706     c                   endif
080380070706
080390070706      * se non inserito volume a video lo calcolo per memorizzarlo su fnorg00f
080400070706     c                   if        wvlm = *zeros
080410070706     c                   select
080420070918 b3  c                   when      v1bnc <> *zeros
080430070918     c                   eval(h)   wvlm = v1bnc / d§dftbnc
080440070918 b2  c                   when      v1pkg <> *zeros
080450070918     c                   eval(h)   wvlm = v1pkg / d§dftpkg
080460070706 e3  c                   endsl
080470070706      * se troppo alto il volume lo imposto al massimo
080480070706     c                   if        wvlm > 99,999
080490070706     c                   eval      wvlm = 99,990
080500070706     c                   endif
080510070706     c                   endif
080520070918
080530070918      * se inseriti gli automezzi devo convertirli in m³ per poi memorizzare
080540070918      * il dato più alto su FNORG
080550070918      * AUTOTRENO
080560070918     c                   if        v1att > *zeros
080570070918     c                   eval      wvlmaut = v1att * d§dftaut
080580070918     c                   eval      wpkgaut = wvlmaut * d§dftpkg
080590070918     c                   if        wvlmaut > 99,999
080600070918     c                   eval      wvlmaut = 99,999
080610070918     c                   endif
080620070918     c                   if        wpkgaut > 999999,9
080630070918     c                   eval      wpkgaut = 999999,9
080640070918     c                   endif
080650070918     c                   endif
080660070918      * MOTRICE
080670070918     c                   if        v1mtc > *zeros
080680070918     c                   eval      wvlmmtc = v1mtc * d§dftmtc
080690070918     c                   eval      wpkgmtc = wvlmmtc * d§dftpkg
080700070918     c                   if        wvlmmtc > 99,999
080710070918     c                   eval      wvlmmtc = 99,999
080720070918     c                   endif
080730070918     c                   if        wpkgmtc > 999999,9
080740070918     c                   eval      wpkgmtc = 999999,9
080750070918     c                   endif
080760070918     c                   endif
080770070918      * BILICO
080780070918     c                   if        v1blc > *zeros
080790070918     c                   eval      wvlmblc = v1blc * d§dftblc
080800070918     c                   eval      wpkgblc = wvlmblc * d§dftpkg
080810070918     c                   if        wvlmblc > 99,999
080820070918     c                   eval      wvlmblc = 99,999
080830070918     c                   endif
080840070918     c                   if        wpkgblc > 999999,9
080850070918     c                   eval      wpkgblc = 999999,9
080860070918     c                   endif
080870070918     c                   endif
080880070918
080890070918      * cerco il volume più alto
080900070918     c                   if        wvlmaut > wvlm
080910070918     c                   eval      wvlm = wvlmaut
080920070918     c                   endif
080930070918     c                   if        wvlmmtc > wvlm
080940070918     c                   eval      wvlm = wvlmmtc
080950070918     c                   endif
080960070918     c                   if        wvlmblc > wvlm
080970070918     c                   eval      wvlm = wvlmblc
080980070918     c                   endif
080990070918
081000070918      * cerco il peso più alto
081010070918     c                   if        wpkgaut > wpkg
081020070918     c                   eval      wpkg = wpkgaut
081030070918     c                   endif
081040070918     c                   if        wpkgmtc > wpkg
081050070918     c                   eval      wpkg = wpkgmtc
081060070918     c                   endif
081070070918     c                   if        wpkgblc > wpkg
081080070918     c                   eval      wpkg = wpkgblc
081090070918     c                   endif
081100071031
081110071108      * se sono in modifica e non devo fare una proposta di variazione
081120071031      * controllo se variati peso/volume/bancali per ricalcolare il giro in base gli ambiti
081130071031      * per il peso e il volume devo utilizzare i valori sviluppati dal pgm
081140071031     c                   eval      $varqta = *off
081150150609     c     v1cpor        Lookup    skpog                                  30
081160071108     c                   select
081170080314     c                   when      *in30 and *in02 and (v1fao = 400
081180080314     c                             or v1fao = 410)
081190071108     c                   when      not *in30 and *in02
081200071108     c                   other
081210131202     c                   if        (savpkg <> wpkg or savvlm <> wvlm or
081220131202     c                              savbnc <> v1bnc) and
081230131202     c                             (savpkg > 0 or savvlm > 0 or
081240131202     c                              savbnc > 0)
081250071031     c                   eval      $varqta = *on
081260071031     c                   eval      savpkg = wpkg
081270071031     c                   eval      savvlm = wvlm
081280071031     c                   eval      savbnc = v1bnc
081290071031     c                   endif
081300071108     c                   endsl
081310161025      /free
081320161025       //?Controllo se ho variato il peso, serve per ricalcolare la data ritiro
081330161025       //?ma solo se data ritiro a video non impostata da utente
081340161025         IF  old_wpkg <> wpkg;
081350161026           IF  old_wpkg > 0 and inv_newv1dar >= inv_v1dar;
081360161025             wModPeso = *on;
081370161025           ENDIF;
081380161025           old_wpkg = wpkg;
081390161025         ENDIF;
081400161025      /end-free
081410070706
081420070706     c                   endsr
081430070706
081440010305      **********************************************************************
081450010305      * CONTROLLI     * cerco causale chiusura e decodifico *
081460010305      **********************************************************************
081470010305     C     Sr_Contrcau   begsr
081480010305
081490010305     C                   eval      kpoe = ORMpoe
081500010305     C                   eval      knsr = ORMnsr
081510010305     C                   eval      knor = ORMnor
081520010305     C                   eval      knrv = ORMnrv
081530010305     C                   eval      kdai = ORMdfo
081540010305     C                   eval      kori = ORMofo
081550010305     C                   eval      kfar = ORMfao
081560110418     c     kfnorn1       setll     fnorf01l
081570110418     c                   do        *hival
081580110418     c     kfnorn1       reade(n)  fnorf01L
081590110418     c                   if        %eof(fnorf01L)
081600110418     c                   leave
081610110418     c                   endif
081620010305     C                   eval      v1cau = orfcar
081630110418     C                   enddo
081640010305
081650010305     C                   clear                   dcmr
081660010305     C                   clear                   TIBS02DS
081670010305     C                   movel     'C'           T02mod
081680010305     C                   movel     knsif         t02sif
081690010305     C                   movel     'CMR'         t02cod
081700010305     C                   movel(p)  v1cau         T02ke1
081710010305     C                   call      'TIBS02R'
081720010305     C                   parm                    KPJBA
081730010305     C                   parm                    TIBS02DS
081740010305     C                   movel     t02uni        dcmr
081750010305     C                   movel     d§cmrdes      v1dcau
081760010309
081770010309     C                   eval      *in13 = (v1cau <> *blanks)
081780070123
081790070123      * mi salvo il p.o. e il n. distinta
081800080226     c                   eval      kdstfgs = orffgs
081810080226     c                   eval      kdstnfv = orfndc
081820010305
081830010305     C                   endsr
081840001019      **********************************************************************
081850001023      * CONTROLLI     * se già stampato deve confermare *
081860001019      **********************************************************************
081870001019     C     Sr_Contrstp   begsr
081880001019
081890031201     C                   if        v1dst  >  *zeros
081900001019     C                   movel     'NO'          wfstp
081910010221     C     emetto        tag
081920001019     C                   exfmt     fior05w
081930001107     C                   if        wfstp = 'SI'
081940031125     C                   clear                   FIOR16ds
081950041013     C                   eval      R16ris = 'R'
081960030623     C                   clear                   trul90ds
081970030623     C                   movel     'S'           D90rso
081980030623     C                   call      'TRUL90R'
081990010314     C                   parm                    kpjba
082000030623     C                   parm                    trul90ds
082010001108
082020001108      * F3=Fine
082030030623     C                   if        D90f3 = '1'
082040010314     C                   goto      endsrstampa
082050010314     C                   endif
082060031125
082070090202      /free
082080090202         reset ds5P;
082090090202         if  %subst(knmus : 1 : 3) = 'EDP';
082100090202           TBLkey = 'O' + 'EDP';
082110090202         else;
082120090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
082130090202         endif;
082140090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
082150090202         if  %found( TABEL00F );
082160090202           ds5P = TBLuni;
082170090202         endif;
082180090202      /end-free
082190031201
082200031125     C                   movel     D90mdo        R16mdo
082210031125     C                   movel     D90pro        R16oqo
082220001108
082230031201     c                   eval      R16poe = Ppoe
082240031201     c                   eval      R16nsr = Pnsr
082250031201     c                   eval      R16nor = Pnor
082260031201     c                   eval      R16nrv = Pnrv
082270031125     C                   movel     v1cpor        R16por
082280031125     C                   eval      R16dst = v1dst
082290031125     C                   eval      R16dar = inv_v1dar
082300001108
082310001108     C                   movel     kpjbu         §kpjbu
082320031125     C                   movel(p)  FIOR16ds      kpjbu
082330090202     C                   call      §5Ppgm
082340031125     C                   parm                    KPJBA
082350031125     C                   movel     kpjbu         FIOR16DS
082360001108     C                   movel     §kpjbu        kpjbu
082370001019     C                   endif
082380001019     C                   endif
082390001019
082400001108     C     endsrstampa   endsr
082410001011      **********************************************************************
082420001011      * F6 - Conferma
082430001011      **********************************************************************
082440001011     C     Sr_Conferma   BEGSR
082450140422
082460140422      /free
082470140423        werralert = *off;
082480140422       //?Se conferma ORM da clienti come prima cosa verifico gli alert
082490140507        IF  §rmfpr = 'C' or
082500150305       //?oppure se sono in immissione ORM
082510140507       //?e non è stato richiamato (quindi immissione ORM MANUALE)
082520160208       //?o copia ORM
082530160208            ((*in01 or *in03 )and §RMtla = *blanks);
082540140423          exsr AlertORM;
082550140423          IF  *in28;
082560140423            werralert = *on;
082570140423            leavesr;
082580140423          ENDIF;
082590140422        ENDIF;
082600160406
082610160413       //?Se immissione ORM manuale o copia
082620160406         IF  (*in01 or *in03) and
082630160406             (V1Ctco = 'M' or V1Ctco = 'E');
082640160406         //?se ho almeno 1 dei 2 codici (ordinante - mittente) e non ho emesso
082650160406         //?la WIN (non ho mai dati un F6 sulla WIN)
082660160406         //?e non ho i dati mail e/o sms provo a recuperarli
082670160406         //?dall'anagrafica clienti
082680160406           IF  (ds_V1Ccor > 0 or ds_V1Ccra > 0) and
082690160406                W04mail = *blanks and W04sms = *blanks and
082700160406                not wOkF6F13;
082710160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
082720160406             exsr RecDatiConf;
082730160406           ENDIF;
082740160406       //?se ho i dati relativi alla conferma prenotazione ritiro
082750160405       //?imposto i dati per scrivere il rcd 'G' di fnore e il file spia
082760160406       //?da conferma VAS ho già tutto impostato
082770160406           IF  (w04sms <> *blanks or w04mail <> *blanks);
082780160406             SELECT;
082790160406             WHEN  W04sms <> *blanks and W04mail <> *blanks;
082800160406               §OREfsco = 'S';
082810160406               §OREfmco = 'S';
082820160406             WHEN  W04sms <> *blanks;
082830160406               §OREfsco = 'S';
082840160406               §OREfmco = 'N';
082850160406             WHEN  W04mail <> *blanks;
082860160406               §OREfmco = 'S';
082870160406               §OREfsco = 'N';
082880160406             ENDSL;
082890160406             wconferma = *on;
082900160406           ENDIF;
082910160310         ENDIF;
082920140422      /end-free
082930001011
082940071108     ?* Scrive
082950001109     C                   if        *in01 = *on or *in03 = *on
082960001117     C                   clear                   fnorm000
082970050422     C  n03              exsr      Sr_Numor
082980050422     C   28              goto      endsrconf
082990001011     C                   exsr      Sr_Carfile
083000001108      * Scrive la fase
083010001017     C                   exsr      Sr_Carfilef
083020001108      * Scrive le note
083030150713     c                   IF        not ConfAuto
083040001026     C                   exsr      Sr_Carnote
083050150713     c                   ENDIF
083060070919      * scrivo fnorg (GIRO)
083070151016     c                   exsr      sr_wrtorg
083080100218      /free
083090160310
083100170524       //?Se dovevo posticipare
083110170524        Anticipato = *off;
083120170524        clear ggAnticipo;
083130170524       //?Imposto se l'utente ha anticipato la data ritiro
083140170615       //?se c'era la 'A' di anticipa
083150170524        IF  wAnticipa and ORMdar < DarCalcolata;
083160170524          Anticipato = *on;
083170170615        ENDIF;
083180170525       //?e di quanti gg lavorativi ha anticipato
083190170615       //?con o senza 'A' di anticipa, in alcuni casi possono anticipare alla
083200170615       //?data immissione senza che sia visualizzata la 'A', questo soprattutto in
083210170615       //?copia ORM oppure se immissione prepagato
083220170525        clear wdat8;
083230170525        clear tfptfa;
083240170525        p_tfa = ORMpor;
083250170525        data1 = DarCalcolata;
083260170525        data2 = ORMdar;
083270170525        xsrlav8(wdat8);
083280170525       //?dal pgm di calcolo differenza tra le 2 date torna sempre almeno 1, io lo tolgo
083290170525        ggAnticipo = giolav -1;
083300100225       //?Se conferma ORM da clienti scrivo eventuale estensione inviata dal cliente
083310100225        IF  §rmfpr = 'C';
083320100218          exsr sr_fnore;
083330100218        ENDIF;
083340131014       //?Se presenti i dati scrivo fnore
083350140422        IF  w03mail <> *blanks;
083360140422          clear old_w03mail;
083370140422        ENDIF;
083380140422        IF  w03sms <> *blanks;
083390140422          clear old_w03sms;
083400140422        ENDIF;
083410160405        IF  w04mail <> *blanks;
083420160405          clear old_w04mail;
083430160310        ENDIF;
083440160405        IF  w04sms <> *blanks;
083450160405          clear old_w04sms;
083460160310        ENDIF;
083470140422        exsr aggFNORE;
083480160322
083490160322       //?Se conferma ORM da clienti per orm da internet
083500160322       //?memorizzo mail sull'anagrafica clienti ritiro, così anche il relativo flag
083510160322       //?anche sms
083520160712       //?Solo però se richiesto da Ordinante
083530160712        IF  §rmfpr = 'C' and ORMtco = 'I' and ORMcor > 0 and §OREmemo = 'S';
083540160712          exsr aggACR;
083550160322        ENDIF;
083560160314
083570160314       //?Se ORM con Conferma Prenotazione Ritiro e sono in immissione manuale
083580160314       //?cioè ORM Telefonico o Mail/Fax
083590160314       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
083600160314       //?servono nell'invio dell'alert
083610160318       //?ma solo se devo creare l'alert conferma prenotazione ritiro
083620160318        IF  §RMfpr <> 'C' and (ORMtco = 'M' or ORMtco = 'E') and
083630160318            wconferma;
083640160314         //?imposto orario ritiro standard
083650160314          clear dOREor;
083660160314          §OREori = V1ostis;
083670160314          §OREorf = V1ostfs;
083680170609         //?Se la data ritiro è uguale a data immissione
083690170609          IF  ORMdar = ORMdao;
083700160314         //?Se l'ora pronta merce è maggiore
083710160314         //?dell'ora inizio servizio
083720160314         //?e minore dell'ora fine
083730160314         //?la imposto come orario inzio
083740160314          IF  V1orr > V1ostis and
083750160314              V1orr < V1ostfs;
083760160314            §OREori = V1orr;
083770160314          ENDIF;
083780160314         //?Se l'ora pronta merce è maggiore
083790160314         //?dell'ora fine servizio
083800160314         //?la imposto come orario fine
083810160314          IF  V1orr > V1ostfs;
083820160314            §OREorf = V1orr;
083830160314          ENDIF;
083840170609          ENDIF;
083850160314          clear FNORE000;
083860160314          OREpoe = ORMpoe;
083870160314          OREnsr = ORMnsr;
083880160314          OREnor = ORMnor;
083890160314          OREnrv = ORMnrv;
083900160314          OREtrc = 'OR';
083910160314          OREdati = dOREor;
083920160314          write FNORE000;
083930160517          feod  FNORE01L;
083940160314        ENDIF;
083950160217
083960160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
083970160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
083980160224       //?il NPR lo recupero solo se non sto facendo conferma ORM da INTERNET
083990160426       //?e se non è già stato calcolato
084000160426        IF  §rmfpr <> 'C' or
084010160426           (§rmfpr = 'C' and ORMtco <> 'I') or
084020160426           (§rmfpr = 'C' and ORMtco = 'I' and not WebNPR);
084030160217          wnpr = GetNPR(parametroNPR);
084040160217       //?Se il rif. ORM è vuoto imposto il NPR
084050160217          IF  ORMrfa = *blanks;
084060160317            ORMrfa = wnpr;
084070160217          ENDIF;
084080160314       //?scrivo FNORE00F rcd NP
084090160314       //?per memorizzare il NPR
084100160314          clear FNORE000;
084110160314          OREpoe = ORMpoe;
084120160314          OREnsr = ORMnsr;
084130160314          OREnor = ORMnor;
084140160314          OREnrv = ORMnrv;
084150160314          OREtrc = 'NP';
084160160314          OREdati = wnpr;
084170160314          write FNORE000;
084180160517          feod  FNORE01L;
084190160217        ENDIF;
084200100218      /end-free
084210100218
084220001108     C                   write     fnorm000
084230160509     C                   feod      fnorm01L
084240100218
084250100218      /free
084260100218       //?dopo aver scritto l'ORM se previsto il preavviso richiamo programma
084270100218       //?per invio mail di preavviso
084280100218        IF  §ormpre = 'M';
084290100224         clear fior52ds;
084300100224         I52poe = ORMpoe;
084310100224         I52nsr = ORMnsr;
084320100224         I52nor = ORMnor;
084330100224         I52nrv = ORMnrv;
084340100224         fior52r (kpjba:fior52ds);
084350100218        ENDIF;
084360140422
084370140428       //?dopo aver scritto l'ORM se previsto Alert
084380151008       //?e/o previsto invio mail x conferma accettazione ORM
084390140428       //?scrivo file spia
084400151008       IF  walert or wconferma;
084410140422         exsr wrtspia;
084420140422       ENDIF;
084430160217
084440100218      /end-free
084450100218
084460010202     C                   eval      §RMpoeo = ormpoe
084470010202     C                   eval      §RMnsro = ormnsr
084480010202     C                   eval      §RMnoro = ormnor
084490010202     C                   eval      §RMnrvo = ormnrv
084500010704
084510010704      * Richiamo il programma immissione prepagati
084520050324      * solo se sono da conferma ORM VAO, se l'orm è di tipo 'P'
084530050324      * e se è un ORM locale
084540050324      * se l'ORM non è locale il prepagato lo creo al momento della conferma ORM
084550050324      * commissionati (Un ORM prepagato non locale è SEMPRE commissionato)
084560010704     C                   if        §RMfpr = 'C' and
084570011012     C                             ORMtor = 'P'
084580080407      * se po emissione diverso da po ritiro controllo se il po ritiro è la capofila
084590080407      * del po emissione
084600080407     c                   if        ormpoe <> ormpor
084610080407     c                   clear                   fnlv55ds
084620080407     c                   eval      d55tpt = '6'
084630080407     c                   eval      d55lin = ormpoe
084640080407     c                   eval      d55drf = oggi_aammgg
084650080407     c                   call      'FNLV55R'
084660080407     c                   parm                    fnlv55ds
084670080407     c                   if        d55err <> *blanks
084680080407     c                   clear                   d55tfa
084690080407     c                   endif
084700080407     c                   endif
084710080407      * questo per parare gli errori dovuti a orm emessi da filiali in £6 con ritiro
084720080407      * presso la capofila
084730080407     c                   if        ormpoe = ormpor or ormpor = d55tfa
084740010704     C                   exsr      Sr_Immprep
084750010704
084760010704      * Non è stato creato il prepagato
084770010704      * devo annullare l'ORM appena creato
084780010704     C                   if        okbolla = *blanks
084790010704     C                   eval      §RMerr = '5'
084800010704      * Deleto l'ORM
084810010704     C                   clear                   fior30ds
084820010704     C                   eval      i30mod = 'D'
084830010704     C                   eval      i30poe = ormpoe
084840010704     C                   eval      i30nsr = ormnsr
084850010704     C                   eval      i30nor = ormnor
084860010704     C                   eval      i30nrv = ormnrv
084870010704     C                   call      'FIOR30R'
084880010704     C                   parm                    kpjba
084890010704     C                   parm                    fior30ds
084900010704
084910010704     C                   endif
084920080407     c                   endif
084930010704     C                   endif
084940010704
084950001026     C                   endif
084960001011
084970071108     ?* Aggiorna
084980010919   1bC                   if        *in02 = *on
084990071108     c                   select
085000071126      * sono in conferma proposte di variazione
085010071126     C                   when      §rmfpr = 'P'
085020071126     c                   eval      $modifica = *on
085030071126     C  n10              exsr      Sr_Storico
085040140227      /free
085050151023       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
085060151023       //?da rcd già vincolato da questo lavoro
085070151023         unlock FNORG01L;
085080140227       //?Richiamo pgm di gestione note per convalidare la proposta di variazione
085090140227       //?della nota
085100140227         chain (§RMpoe:§RMnsr:§RMnor:§RMnrv:§RMdtv:§RMorv) FNORPT1L;
085110140227         IF  %found(FNORPT1L);
085120140227           clear fiort1ds;
085130140227           IORT1mod = 'P';
085140140227           IORT1poe = ORMpoe;
085150140227           IORT1nsr = ORMnsr;
085160140227           IORT1nor = ORMnor;
085170140227           IORT1nrv = ORMnrv;
085180140227           IORT1dim = §RMdtv;
085190140227           IORT1oim = §RMorv;
085200140227           IORT1pgm = 'FIOR05R';
085210140227           §kpjbu = kpjbu;
085220140227           fiort1r (kpjba:fiort1ds);
085230140227           kpjbu = §kpjbu;
085240140227         ENDIF;
085250151023       //?Rialloco FNORG
085260151023         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
085270140227      /end-free
085280140227
085290010205      * Non sono in conferma orm da altri p.o.
085300071108     C                   when      §rmfpr <> 'O'
085310071108      * è stato modificato qualcosa
085320131018     c                             and (vidnew <> vidold
085330160406     c                               or vidnew2 <> vidold2
085340160406     c                               or vidnewA <> vidoldA
085350160406     c                               or vidnewC <> vidoldC)
085360071108      * non è un orm dirottato
085370071108     c                             and wokdirotta = *off
085380071108     c                   eval      $modifica = *on
085390071106      * se devo scrivere una proposta lo faccio subito
085400071106      * e poi esco dalla routine
085410140918     c                   if        $proposte = *on and not OrmFase420
085420071106     c                   exsr      sr_carproposte
085430080225     c                   unlock    fnorm01l
085440151023     c                   unlock    fnorg01l
085450071106     c                   goto      endsrconf
085460071106     c                   endif
085470071108      * controllo se devo scrivere lo Storico o la proposta di variazione
085480150305    2C                   if        %lookup(%editc(ormpor:'X'):skpog) > 0
085490071108      * scrivo lo storico
085500170605    3C*******            if        num_v1cpor <> v1cpoe
085510010222     C  n10              exsr      Sr_Storico
085520170605   3xC*******            else
085530170605    4C*******            if        v1fao >=300
085540170605     C**n10              exsr      Sr_Storico
085550170605    4C*******            endif
085560170605    3C*******            endif
085570071108      * scrivo la proposta
085580071108   2xC                   else
085590010202     C                   exsr      Sr_CarProposte
085600080225     c                   unlock    fnorm01l
085610151023     c                   unlock    fnorg01l
085620010919     C                   goto      endsrconf
085630071108    2C                   endif
085640071108      * Non sono in conferma orm da altri p.o.
085650071108     C                   when      §rmfpr <> 'O'
085660071108      * è un orm dirottato
085670071108     c                             and wokdirotta = *on
085680071108     c                   eval      $modifica = *on
085690040615      * Storico
085700040615     c                   ExSr      Sr_Storico
085710040618      * scrivo la fase 200
085720040615     C                   clear                   fnorf000
085730040615     C                   eval      ORFpoe = v1cpoe
085740040615     C                   eval      ORFnsr = v1nsr
085750040615     C                   eval      ORFnor = v1nor
085760040615     C                   eval      ORFnrv = v1nrv
085770040615     C                   eval      ORFpog = OrmPor
085780040615     C                   eval      ORFdae = v1dfo
085790040615     C                   eval      ORFore = v1ofo
085800051220     c                   Move      v1ofo         wora
085810051220     c                   subdur    1:*s          wora
085820051220     c                   move      wora          orfore
085830040618     C                   eval      ORFfar = 200
085840040615     C                   eval      ORFpue = knmus
085850040615     C                   write     fnorf000
085860040615      * scrivo fase 50
085870040615     C                   clear                   fnorf000
085880040615     C                   eval      ORFpoe = v1cpoe
085890040615     C                   eval      ORFnsr = v1nsr
085900040615     C                   eval      ORFnor = v1nor
085910040615     C                   eval      ORFnrv = v1nrv
085920040615     C                   eval      ORFpog = num_v1cpor
085930040615     C                   eval      ORFdae = v1dfo
085940040615     C                   eval      ORFore = v1ofo
085950040615     C                   eval      ORFfar = v1fao
085960040615     C                   eval      ORFpue = knmus
085970040615     C                   write     fnorf000
085980160517     C                   feod      fnorf01l
085990040615      * pulisco i campi che non devo reimpostare
086000040615     c                   Clear                   v1npg
086010040615     c                   Clear                   v1ndc
086020040615     c                   Clear                   v1ddc
086030040615     c                   Clear                   v1dst
086040140918
086050010919      * se sono in conferma ORM da altri p.o. devo storicizzare
086060010919      * se sono state fatte delle variazioni
086070071108     C                   when      §rmfpr = 'O'
086080071108     c                   eval      $modifica = *on
086090030609      * Prima di confermare la fase 100 dell'orm genero la bolla 'FT' di orm commissionato
086100030623      * se cod.ordinante codificato e se non è un ORM prepagato
086110071108    2c                   If        v1com = 'S' and
086120030610     c                             ds_V1ccor > *Zeros and
086130030610     c                             ds_V1cor2 <> 8888 and ds_V1cor2 <> 9999
086140030623     c                             and v1ctor <> 'P'
086150041130      * non deve esserci già una fase 200
086160041130     c                   Eval      wfase200 = *Off
086170041130     C     kfnor         setll     fnorf01l
086180071108    3c                   Do        *Hival
086190041130     C     kfnor         reade(n)  fnorf01l
086200041130     C                   if        %eof (fnorf01l)
086210041130     c                   Leave
086220041130     c                   EndIf
086230071108    4C                   If        orffar = 200
086240041130     c                   eval      wfase200 = *On
086250041130     c                   Leave
086260071108    4c                   EndIF
086270071108    3c                   EndDo
086280071108    3c                   If        wfase200 = *Off
086290030609     c                   Exsr      Sr_BollaFt
086300030609      * se f12 o f03 da immissione bolla riemetto la videata
086310071108    4c                   If        okbolla = '1'
086320030609     c                   Eval      *in28 = *On
086330030609     c                   GoTo      endsrconf
086340071108    4c                   EndIf
086350140506      /free
086360140506       //?se OK bolle e sto confermando un ORM commissionato con Alert
086370140506       //?imposto già che l'ORM è confermato così poi esce dal FIOR05R
086380140506         IF  walert;
086390140506           wormconf = *on;
086400140506         ENDIF;
086410140506      /end-free
086420071108    3c                   EndIf
086430071108    2c                   EndIf
086440030612      * storicizzo se ho fatto vedere la videata
086450131018     C                   if        (Vidnew <> Vidold
086460131211     c                              or vidnew2 <> vidold2
086470160406     c                              or vidnewA <> vidoldA
086480160406     c                              or vidnewC <> vidoldC)
086490030612     c                             and §rmtla = '2'
086500010919     C                   exsr      Sr_Storico
086510071108     C                   endif
086520050322      * sto dirottatando l'orm invece di confermarlo
086530071108    2c                   if        wokdirotta = *On
086540050322      * scrivo la fase 200
086550050322     C                   clear                   fnorf000
086560050322     C                   eval      ORFpoe = v1cpoe
086570050322     C                   eval      ORFnsr = v1nsr
086580050322     C                   eval      ORFnor = v1nor
086590050322     C                   eval      ORFnrv = v1nrv
086600050322     C                   eval      ORFpog = OrmPor
086610050322     C                   eval      ORFdae = v1dfo
086620050322     C                   eval      ORFore = v1ofo
086630070125     c                   Move      v1ofo         wora
086640070125     c                   subdur    1:*s          wora
086650070125     c                   move      wora          orfore
086660050322     C                   eval      ORFfar = 200
086670050322     C                   eval      ORFpue = knmus
086680050322     C                   write     fnorf000
086690160517     c                   feod      fnorf01l
086700071108    2c                   EndIf
086710071108     c                   endsl
086720140918
086730010202      * Scrive fase per chisura o conferma orm da altri p.o.
086740010202     C                   if        *in10 = *on or §RMfpr = 'O'
086750010202     C                   exsr      Sr_Carfilef
086760010202     C                   endif
086770140918
086780090211      * memorizzo i dati dell'appuntamento
086790150305     c                   if        §RMfpr = 'O'
086800090330     c                             and wokdirotta = *off
086810090407     c                             and wokapp = *on
086820090407      * l'appuntamento va inserito in fase 100 quindi se sto chiudendo
086830090407      * l'ORM prima scrivo la fase 100 poi scrivo i dati dell'appuntamento
086840090407     c                   if        *in10
086850090407     C                   clear                   fnorf000
086860090407     C                   eval      ORFpoe = ormpoe
086870090407     C                   eval      ORFnsr = ormnsr
086880090407     C                   eval      ORFnor = ormnor
086890090407     C                   eval      ORFnrv = ormnrv
086900090407     C                   eval      ORFpog = ormpor
086910090407     C                   eval      ORFdae = v1dfo
086920090407     C                   eval      ORFore = v1ofo
086930090407     c                   Move      v1ofo         wora
086940090407     c                   subdur    1:*s          wora
086950090407     c                   move      wora          orfore
086960090407     C                   eval      ORFfar = 100
086970090407     C                   eval      ORFpue = knmus
086980090407     C                   write     fnorf000
086990160517     c                   feod      fnorf01l
087000090407     c                   endif
087010090211     c                   exsr      sr_wrtapp
087020090211     c                   endif
087030070119
087040070119      * salvo i dati modificati sensibili per PDA
087050070123      * se p.o. abilitato al PDA
087060140122      * o se filiale ritiro abilitata alle telefonate AUT
087070070119      * se sono in modifica (se proposta variazione non arriva qua)
087080070119      * se ORM in fase 400 o 410
087090070119      * negli altri casi non serve
087100140122     c                   if        *in02 and
087110140317     c                             *in22 and
087120071210     c                             (v1fao = 400 or v1fao = 410)
087130070119     c                   exsr      sr_datipdamod
087140070119     c                   endif
087150070119
087160071108      * se aggiornato qualcosa
087170071108     c                   if        $modifica = *on
087180001023      * Aggiorna ORM
087190001011     C                   exsr      Sr_Carfile
087200151016      * aggiorno fnorg (GIRO)
087210151016     c                   exsr      sr_aggorg
087220131018      /free
087230131018       //?Se modificati i dati win alert o dati orari apertura
087240161028       //?o se modificata data pronta merce
087250131029       //?o se conferma proposta di variazione
087260131018       //?aggiorno FNORE
087270131018        IF  old_v1OraAMda <> v1OraAMda or
087280131018            old_v1OraAMa  <> v1OraAMa  or
087290131018            old_v1OraPMda <> v1OraPMda or
087300131018            old_v1OraPMa  <> v1OraPMa  or
087310161028            old_v1dpm <> v1dpm or
087320131029            vidoldA <>  vidnewA or
087330131029            §RMfpr = 'P';
087340131018          exsr aggFNORE;
087350131018        ENDIF;
087360131018      /end-free
087370001108     C                   update    fnorm000
087380090202     c                   else
087390090202     c                   UNLOCK    FNORM01L
087400151023     c                   unlock    fnorg01l
087410071108     c                   endif
087420070119
087430070123      * se il p.o. è abilitato al PDA
087440140122      * o abilitato alle telefonate AUT
087450140317     c                   if         *in22  and
087460070228      * non è un dirottamento o chiusura ORM
087470070228     c                             (wokdirotta = *off or not *in10) and
087480070119      * l'orm è in fase 400 o 410
087490080314     c                             (ormfao = 400 or ormfao = 410) and
087500070119      * almeno uno dei dati sensibili per PDA è stato variato
087510070119     c                             datipdasav <> datipdamod
087520140123       //?Verifico cosa è stato variato
087530140122     c                   exsr      ctrvardati
087540070122      * aggiorno i dati inviati al PDA
087550070124     c                   clear                   fior56ds
087560070124     c                   eval      or56tla = 'O'
087570080121     c                   eval      or56fgs = orffgs
087580080121     c                   eval      or56ndcd = orfndc
087590080121     c                   eval      or56ndca = orfndc
087600140120     c                   eval      OR56cmd = 'V'
087610140122     c                   eval      OR56var = wVardati
087620070119     c                   exsr      sr_aggpdo
087630070119     c                   endif
087640070119
087650010919   1eC                   endif
087660001106
087670001109     C                   eval      *in10 = *off
087680001011
087690001019     C     endsrconf     endsr
087700010202      **********************************************************************
087710010202      * F7 - Ricerca cliente ritiro
087720010202      **********************************************************************
087730010202     C     Sr_RicRitiro  BEGSR
087740010202
087750010202     C                   eval      *in90 = *on
087760010202
087770130916     c                   clear                   FIOR81DS
087780130916     c                   IF        V1cra1 > 0
087790130916     c                   eval      I81fil = V1cra1
087800130916     c                   ENDIF
087810130916     c                   IF        V1rsr <> *blanks
087820130916     c                   eval      I81rag = V1rsr
087830130916     c                   ENDIF
087840150907     c                   IF        V1prr <> *blanks
087850150907     c                   eval      I81prv = V1prr
087860150907     c                   ENDIF
087870130916     c                   call      'FIOR81R'
087880130916     c                   parm                    kpjba
087890130916     c                   parm                    FIOR81DS
087900130916     c                   IF        O81err <> *blanks
087910130916     c                   eval      V1Cmsg = O81msg
087920130916     c                   seton                                        41  28
087930130916     c                   leavesr
087940130916     c                   ENDIF
087950130916     c                   IF        O81ret = '1'
087960130916     c                   leavesr
087970130916     c                   ENDIF
087980130916
087990130916     c                   eval      ds_v1ccra = O81cro
088000070905     c                   eval      v1cra1 = ds_v1cra1
088010070905     c                   eval      v1cra2 = ds_v1cra2
088020070905     c                   eval      v1cra3 = ds_v1cra3
088030070905     c                   exsr      sr_contrclir
088040010202
088050010202     C                   endsr
088060010202      **********************************************************************
088070010202      * F8 - Ricerca cliente ordinante
088080010202      **********************************************************************
088090010202     C     Sr_RicOrdin   BEGSR
088100010202
088110010202     C                   eval      *in90 = *on
088120070905
088130130916     c                   clear                   FIOR81DS
088140130916     c                   IF        V1cor1 > 0
088150130916     c                   eval      I81fil = V1cor1
088160130916     c                   ENDIF
088170130916     c                   IF        V1rso <> *blanks
088180130916     c                   eval      I81rag = V1rso
088190130916     c                   ENDIF
088200150907     c                   IF        V1pro <> *blanks
088210150907     c                   eval      I81prv = V1pro
088220150907     c                   ENDIF
088230130916     c                   call      'FIOR81R'
088240130916     c                   parm                    kpjba
088250130916     c                   parm                    FIOR81DS
088260130916     c                   IF        O81err <> *blanks
088270130916     c                   eval      V1Cmsg = O81msg
088280130916     c                   seton                                        42  28
088290130916     c                   leavesr
088300130916     c                   ENDIF
088310130916     c                   IF        O81ret = '1'
088320130916     c                   leavesr
088330130916     c                   ENDIF
088340130916
088350130916     c                   eval      ds_v1ccor = O81cro
088360070905     c                   eval      v1cor1 = ds_v1cor1
088370070905     c                   eval      v1cor2 = ds_v1cor2
088380070905     c                   eval      v1cor3 = ds_v1cor3
088390070905     c                   exsr      sr_contrclio
088400010202
088410010202     C                   endsr
088420010202      **********************************************************************
088430011113      * F9 - Ricerca cliente destinatario
088440010202      **********************************************************************
088450010202     C     Sr_RicCons    BEGSR
088460010202
088470010202     C                   eval      *in90 = *on
088480070905
088490130916     c                   clear                   FIOR81DS
088500130916     c                   IF        V1crc1 > 0
088510130916     c                   eval      I81fil = V1crc1
088520130916     c                   ENDIF
088530130916     c                   IF        V1rsc <> *blanks
088540130916     c                   eval      I81rag = V1rsc
088550130916     c                   ENDIF
088560150907     c                   IF        V1prc <> *blanks
088570150907     c                   eval      I81prv = V1prc
088580150907     c                   ENDIF
088590130916     c                   call      'FIOR81R'
088600130916     c                   parm                    kpjba
088610130916     c                   parm                    FIOR81DS
088620130916     c                   IF        O81err <> *blanks
088630130916     c                   eval      V1Cmsg = O81msg
088640130916     c                   seton                                        56  28
088650130916     c                   leavesr
088660130916     c                   ENDIF
088670130916     c                   IF        O81ret = '1'
088680130916     c                   leavesr
088690130916     c                   ENDIF
088700130916
088710130916     c                   eval      ds_v1ccrc = O81cro
088720070905     c                   eval      v1crc1 = ds_v1crc1
088730070905     c                   eval      v1crc2 = ds_v1crc2
088740070905     c                   eval      v1crc3 = ds_v1crc3
088750070905     c                   exsr      sr_contrclid
088760010202
088770010202     C                   endsr
088780010202      **********************************************************************
088790010202      * F11 - Calcola Volume
088800010202      **********************************************************************
088810010202     C     Sr_Volume     BEGSR
088820010202
088830040301     c                   clear                   UL36vol
088840040301     c                   call      'TRUL36R'
088850040301     c                   parm                    UL36vol
088860040301     c                   add       UL36vol       V1vlm
088870010202
088880010202     C                   endsr
088890010202      **********************************************************************
088900010202      * F16 - Chiusura
088910010202      **********************************************************************
088920010202     C     Sr_Chiusura   BEGSR
088930010202
088940081125     c                   eval      wnobolla =  *off
088950020523     c                   eval      *in17 = *off
088960010202     C                   clear                   v3cau
088970010202     C                   clear                   v3dcau
088980010202     C                   eval      v3cpoe = ppoe
088990010202     C                   eval      v3nsr  = pnsr
089000010202     C                   eval      v3nor  = pnor
089010010202     C                   eval      v3nrv  = pnrv
089020071126
089030071126      * se è manutenzione
089040130412     c                   if        *in02 and §rmtla = *blanks and
089050071126      * ORM assegnato
089060071126      * scrivo proposta variazione
089070080314     c                             (v1fao = 400 or v1fao = 410)
089080071126     c                   eval      $proposte = *on
089090071126     c                   endif
089100071126
089110010202      * Videata con richiesta causale di chiusura (obbligatoria)
089120010227     C                   do        *hival
089130010202     C                   exfmt     fior05c
089140010301     C                   setoff                                       28
089150010202      * F12=Ritorno e non chiudo
089160050609     c   kl              clear                   v3cau
089170010202     C   kl              goto      endsrchiuso
089180070228
089190070228      * se il p.o. che chiude è abilitato al PDA
089200070228      * e fase 400 o 410
089210071210     c                   if        *in22 and
089220070228     c                             (ormfao = 400 or ormfao = 410)
089230070228      * devo avvisare che ho già avuto un esito da AUT
089240070228      * se l'orm è in fase 410
089250070228     c                   if        ormfao = 410 and wforzachi = *off
089260070228     c                   eval      v3cmsg = msg(41)
089270070228     c                   seton                                        28
089280070228     c                   eval      wforzachi = *on
089290070228     c                   iter
089300070228     c                   endif
089310070228     c                   endif
089320070228
089330010202      * Causale obbligatoria
089340010202     C                   if        v3cau = *blanks
089350010202     C                   movel     msg(14)       v3cmsg
089360010202     C                   clear                   v3dcau
089370010202     C                   eval      *in28 = *on
089380010202     C                   iter
089390010202     C                   endif
089400010302
089410010302     C                   clear                   v3dcau
089420010202      * Ricerco causale
089430010202     C     '?'           scan      v3cau                                  30
089440010202     C     *in30         ifeq      *on
089450010202     C                   clear                   TIBS02DS
089460010301     C                   clear                   dcmr
089470010202     C                   movel     'R'           t02mod
089480010202     C                   movel     knsif         t02sif
089490010227     C                   movel     'CMR'         t02cod
089500010202     C                   call      'TIBS02R'
089510010202     C                   parm                    KPJBA
089520010202     C                   parm                    TIBS02DS
089530010202     C                   movel     T02ke1        v3cau
089540010301     C                   movel     T02uni        dcmr
089550010202     C                   iter
089560010202     C                   endif
089570010202      * Decodifico
089580010301     C                   clear                   dcmr
089590010202     C                   clear                   TIBS02DS
089600010202     C                   movel     'C'           T02mod
089610010202     C                   movel     knsif         t02sif
089620010227     C                   movel     'CMR'         t02cod
089630010202     C                   movel(p)  v3cau         T02ke1
089640010202     C                   call      'TIBS02R'
089650010202     C                   parm                    KPJBA
089660010202     C                   parm                    TIBS02DS
089670010202     C                   if        t02err <> *blanks
089680010202     C                   movel     msg(15)       v3cmsg
089690010202     C                   eval      *in28 = *on
089700010202     C                   iter
089710010202     C                   endif
089720010202     C                   movel     t02ke1        v3cau
089730010301     C                   movel     t02uni        dcmr
089740010622
089750010301     C                   movel     d§cmrdes      v3dcau
089760030912
089770030912      * nei casi in cui il flag sia impostato a 'E' o a 'R'
089780030912      * se p.o. ritiro diverso da p.o. emissione controllo se posso usare la causale
089790030912     c                   If        Ormpoe <> OrmPor
089800030912      * se impostato a 'E' il p.o. emissione non può usare la causale
089810151014     c                   If        d§CmrNoc = 'E' and  ORMpoe = DUTpou and
089820150622     c                             %lookup(%editc(ORMpoe:'X'):skpog) > 0
089830030806     C                   movel     msg(64)       v3cmsg
089840030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089850070418     c                             'dalla filiale emissione'
089860030806     C                   eval      *in28 = *on
089870030806     C                   iter
089880030806     c                   EndIf
089890030912      * se impostato a 'R' il p.o. ritiro non può usare la causale
089900151014     c                   If        d§CmrNoc = 'R' and  ORMpor = DUTpou and
089910150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
089920030806     C                   movel     msg(64)       v3cmsg
089930030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089940070418     c                             'dalla filiale ritiro'
089950030806     C                   eval      *in28 = *on
089960030806     C                   iter
089970030806     c                   EndIf
089980030912     c                   EndIf
089990030912      * se impostato a 'S' non si può usare e basta
090000030806     C                   if        d§cmrnoc = 'S'
090010010305     C                   movel     msg(64)       v3cmsg
090020010302     C                   eval      *in28 = *on
090030010302     C                   iter
090040010302     C                   endif
090050100218
090060100218      /free
090070100218       //?Se ORM NON prevede il preavviso non posso utilizzare la nuova causale '98'
090080100218       IF  §ormpre <> 'M' and v3cau = '98';
090090100218         v3cmsg = msg(64);
090100100218         v3cmsg = %trim(v3cmsg) + '. Il ritiro non prevede il PREAVVISO';
090110100218         *in28 = *on;
090120100218         iter;
090130100218       ENDIF;
090140100218      /end-free
090150081028
090160081124      * se causale inserita genera bolla di addebito (colpa cliente)
090170081028      * controllo che l'ORM non abbia già avuto una causale che genera addebito
090180081125      * in questo caso faccio solo un addebito
090190081124     c                   if        d§cmradd = 'S'
090200081028     c     kfnor         setll     fnorf01l
090210081028     c                   do        *hival
090220081028     c     kfnor         reade(n)  fnorf01l
090230081028     c                   if        %eof (fnorf01l)
090240081028     c                   leave
090250081028     c                   endif
090260081029      * senza causale rileggo
090270081028     c                   if        orfcar = *blanks
090280081028     c                   iter
090290081028     c                   endif
090300081029      * fase da PDA rileggo
090310081029     c                   if        orffar = 410
090320081029     c                   iter
090330081029     c                   endif
090340081028     c                   clear                   a_dcmr
090350081028     c                   clear                   tibs02ds
090360081028     c                   eval      t02mod = 'C'
090370081028     c                   eval      t02sif = knsif
090380081028     c                   eval      t02cod = 'CMR'
090390081028     c                   eval      t02ke1 = orfcar
090400081028     c                   call      'TIBS02R'
090410081028     c                   parm                    kpjba
090420081028     c                   parm                    tibs02ds
090430081028     c                   eval      a_dcmr = t02uni
090440081124      * se causale che genera bolla vado avanti senza generare l'addebito per la seconda volta
090450081028     c                   if        a_d§cmradd = 'S'
090460081125     c                   eval      wnobolla = *on
090470081028     c                   leave
090480081028     c                   endif
090490081028     c                   enddo
090500081028     c                   endif
090510081007
090520081007      * se ORM automatico DPD non si può chiudere l'ORM con la causale 80
090530081007      * l'unica eccezione è per la generazione automatica da parte dei pgm di AB
090540081007      * che creano la proposta di chiusura con causale 80
090550081007     c     ormpoe        lookup    skpodpd                                30
090560081007     c                   if        §rmfpr <> 'P' and ormtco = 'F' and
090570081007     c                             *in30 and v3cau = '80'
090580081007     c                   eval      v3cmsg = msg(49)
090590081007     c                   eval      *in28 = *on
090600081007     c                   iter
090610081007     c                   endif
090620081007
090630010227      * Conferma chiusura
090640010227     C                   if        *inkf = *on
090650071126      * se non è una proposta di variazione
090660071126     c                   if        $proposte = *off
090670020524      * procedo con i controlli per creare l'addebito solo se l'orm è mio
090680150306     c                   if        ormpoe = ormpor or
090690150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
090700020523     c                   if        d§cmradd <> *blanks
090710080314     c                             and (ormfao = 400 or ormfao = 410)
090720020523     c                   exsr      sr_contradd
090730020523      * se f12 o f03 da immissione bolla riemetto la videata
090740020523     c                   if        okbolla = '1'
090750020523     c                   iter
090760020523     c                   endif
090770020524     c                   if        wbolla = *on
090780020523     c                   eval      *in17 = *on
090790020524     c                   endif
090800020523     c                   endif
090810020524     c                   endif
090820071126     c                   endif
090830010202
090840010202      * Salva dati della fase nel caso l'utente cambi idea (f12)
090850010202     C                   eval      sav_v1fao = v1fao
090860010202     C                   eval      sav_v1dfo = v1dfo
090870010202     C                   eval      sav_v1ofo = v1ofo
090880070228      * salvo la fase anche per aggiornare FIPDO
090890070228     c                   eval      savorffar = orffar
090900070228      * e salvo anche la distinta per passarla al pgm che aggiorna FIPDO
090910070228     c                   eval      savorffgs = orffgs
090920070228     c                   eval      savorfndc = orfndc
090930070228     c                   eval      savorfddc = orfddc
090940150922      * imposto peso/volume di ORG in modo da non azzerarlo al momento della conferma
090950150922      * della chiusura ORM
090960150922     c                   eval      wpkg = ORGpkg
090970150922     c                   eval      wvlm = ORGvlm
090980010202
090990010301     C                   movel     d§cmrfar      v1fao                          chiuso
091000050422     C                   z-add     oggi_aammgg   v1dfo
091010050422     C                   time                    w0140
091020010202     C                   movel     w0140         v1ofo
091030010202     C                   eval      *in10 = *on
091040010305
091050010305     C                   eval      v1cau = v3cau
091060010305     C                   eval      v1dcau = v3dcau
091070010301
091080010227     C                   leave
091090010227     C                   endif
091100010227     C
091110010227     C                   enddo
091120010202      * Decodifica la fase
091130010202     C                   exsr      Sr_Contrfao
091140131025
091150131025      /free
091160131025       //?Imposto la riga dei tasti funzione
091170131025       ExSr Sr_Tastifun;
091180131025      /end-free
091190010202
091200010202     C     endsrchiuso   endsr
091210020522      **********************************************************************
091220020522      * Controllo se devo generare la bolla di addebito
091230020522      **********************************************************************
091240020522     c     sr_contradd   begsr
091250020522
091260020522     c                   clear                   sav_lpksc
091270020523     c                   clear                   okbolla
091280020524     c                   eval      wbolla = *off
091290081125
091300081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
091310081125      * come se avessi fatto la bolla
091320081125     c                   if        wnobolla = *on
091330081125     c                   eval      wbolla = *on
091340081125     c                   leavesr
091350081125     c                   endif
091360020522
091370020522      * se cod.ordinante codificato addebito
091380020522 b1  c                   if        ds_v1ccor <> *zeros and v1cor2 <> 8888
091390020522     c                             and v1cor2 <> 9999
091400020523     c                   clear                   dblp
091410090504     c                   movel     ormcor        §lpksca
091420020702      * se c'è imposto il codice bollettazione
091430151109     c     ormcor        chain(n)  fnacr01l
091440020702     c                   if        %found(fnacr01l) and acratb = *blanks
091450020702     c                             and acrksc <> *zeros
091460090504     c                   move      acrksc        §lpksca
091470090505     c                   if        acrccc<>999
091480090504     c                   move      acrccc        §lpctra
091490090505     c                   endif
091500020702     c                   endif
091510020522     c                   eval      §lprsd = ormrso
091520020522     c                   eval      §lpind = ormino
091530020522     c                   eval      §lpcad = ormcao
091540020522     c                   eval      §lplod = ormloo
091550020522     c                   eval      §lpprd = ormpro
091560020522     c                   eval      §lpnzd = ormnao
091570020522     c                   exsr      sr_wrtbolla
091580020522 x1  c                   else
091590020522      * se non inserito l'ordinante
091600020522 b2  c                   if        v1rso = *blanks
091610050323      * orm locale
091620050323 b3  c                   if        v1cpoe = num_v1cpor
091630020522      * se cod.mittente codificato addebito
091640020522 b4  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
091650020522     c                             and v1cra2 <> 9999
091660020523     c                   clear                   dblp
091670090504     c                   movel     ormcra        §lpksca
091680020702      * se c'è imposto il codice bollettazione
091690151109     c     ormcra        chain(n)  fnacr01l
091700020702     c                   if        %found(fnacr01l) and acratb = *blanks
091710020702     c                             and acrksc <> *zeros
091720090504     c                   move      acrksc        §lpksca
091730090505     c                   if        acrccc<>999
091740090504     c                   move      acrccc        §lpctra
091750090505     c                   endif
091760020702     c                   endif
091770020522     c                   eval      §lprsd = ormrsr
091780020522     c                   eval      §lpind = orminr
091790020522     c                   eval      §lpcad = ormcar
091800020522     c                   eval      §lplod = ormlor
091810020522     c                   eval      §lpprd = ormprr
091820020522     c                   eval      §lpnzd = ormnar
091830020522     c                   exsr      sr_wrtbolla
091840020522      * se cod.mittente non codificato
091850020522 x4  c                   else
091860020522      * se cod.destinatario codificato addebito
091870020522 b5  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
091880020522     c                             and v1crc2 <> 9999
091890020523     c                   clear                   dblp
091900090504     c                   movel     ormcrc        §lpksca
091910020702      * se c'è imposto il codice bollettazione
091920151109     c     ormcrc        chain(n)  fnacr01l
091930020702     c                   if        %found(fnacr01l) and acratb = *blanks
091940020702     c                             and acrksc <> *zeros
091950090504     c                   move      acrksc        §lpksca
091960090505     c                   if        acrccc<>999
091970090504     c                   move      acrccc        §lpctra
091980090505     c                   endif
091990020702     c                   endif
092000020522     c                   eval      §lprsd = ormrsc
092010020522     c                   eval      §lpind = orminc
092020020522     c                   eval      §lpcad = ormcac
092030020522     c                   eval      §lplod = ormloc
092040020522     c                   eval      §lpprd = ormprc
092050020522     c                   eval      §lpnzd = ormnac
092060020522     c                   exsr      sr_wrtbolla
092070020522 e5  c                   endif
092080020522 e4  c                   endif
092090050323      * orm non locale
092100020522 x3  c                   else
092110020522      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
092120020522 b4  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
092130020522     c                             and v1crc2 <> 9999  and v1crc1 = v1cpoe
092140020523     c                   clear                   dblp
092150090504     c                   movel     ormcrc        §lpksca
092160020702      * se c'è imposto il codice bollettazione
092170151109     c     ormcrc        chain(n)  fnacr01l
092180020702     c                   if        %found(fnacr01l) and acratb = *blanks
092190020702     c                             and acrksc <> *zeros
092200090504     c                   move      acrksc        §lpksca
092210090505     c                   if        acrccc<>999
092220090504     c                   move      acrccc        §lpctra
092230090505     c                   endif
092240020702     c                   endif
092250020522     c                   eval      §lprsd = ormrsc
092260020522     c                   eval      §lpind = orminc
092270020522     c                   eval      §lpcad = ormcac
092280020522     c                   eval      §lplod = ormloc
092290020522     c                   eval      §lpprd = ormprc
092300020522     c                   eval      §lpnzd = ormnac
092310020522     c                   exsr      sr_wrtbolla
092320020522      * se non codificato o p.o. differenti
092330020522 x4  c                   else
092340020522      * se cod.mittente codificato addebito
092350020522 b5  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
092360020522     c                             and v1cra2 <> 9999
092370020523     c                   clear                   dblp
092380090504     c                   movel     ormcra        §lpksca
092390020702      * se c'è imposto il codice bollettazione
092400151109     c     ormcra        chain(n)  fnacr01l
092410020702     c                   if        %found(fnacr01l) and acratb = *blanks
092420020702     c                             and acrksc <> *zeros
092430090504     c                   move      acrksc        §lpksca
092440090505     c                   if        acrccc<>999
092450090504     c                   move      acrccc        §lpctra
092460090505     c                   endif
092470020702     c                   endif
092480020522     c                   eval      §lprsd = ormrsr
092490020522     c                   eval      §lpind = orminr
092500020522     c                   eval      §lpcad = ormcar
092510020522     c                   eval      §lplod = ormlor
092520020522     c                   eval      §lpprd = ormprr
092530020522     c                   eval      §lpnzd = ormnar
092540020522     c                   exsr      sr_wrtbolla
092550020522 e5  c                   endif
092560020522 e4  c                   endif
092570020522 e3  c                   endif
092580020522 e2  c                   endif
092590020522 e1  c                   endif
092600020522
092610020522     c                   endsr
092620020522      **********************************************************************
092630020522      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
092640020522      **********************************************************************
092650020522     c     sr_wrtbolla   begsr
092660020522
092670030623     c                   Clear                   wtrulvid
092680020522      * richiamo il programma per la scelta delle stampanti
092690030307     c                   Clear                   wdatibolla
092700030307     c                   Eval      wdatibolla = 'BollaAdd.'
092710030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
092720030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
092730020522     c                   exsr      sr_scestp
092740020524     c   92              eval      okbolla = '1'
092750020522     c   92              goto      endwrtbolla
092760020522
092770020522     c                   clear                   kpjbu
092780020522     c                   clear                   dtasv
092790020522
092800020522     c                   eval      §lpfpr = 'T'
092810020522     c                   movel     ormpor        §lpccn
092820020522     c                   move      9999          §lpccn
092830020523     c                   movel     oggi_aammgg   §lpaas
092840020523     c                   move      oggi_aammgg   §lpmgs
092850020522     c                   eval      §lplnp = ormpor
092860020522     c                   eval      §lpcbo = 'FY'
092870020522     c                   eval      §lpsop = '+'
092880020530     c                   eval      §lplna = ormpoe
092890020522     c                   eval      §lprsm = ormrsr
092900020522     c                   eval      §lpinm = orminr
092910020522     c                   eval      §lpcam = ormcar
092920020522     c                   eval      §lplom = ormlor
092930020522     c                   eval      §lpprm = ormprr
092940020522     c                   eval      §lpnzm = ormnar
092950020522
092960020522 b1  c                   if        ormncl <> 0
092970020522     c                   eval      §lpncl = ormncl
092980020522 x1  c                   else
092990020522     c                   eval      §lpncl = 1
093000020522 e1  c                   endif
093010020522 b1  c                   if        ormpkg <> 0
093020020522     c                   eval      §lppkb = ormpkg
093030020522 x1  c                   else
093040070918 b3  c                   if        ormbnc <> 0
093050070918     c                   eval      wpkb = ormbnc / d§dftbnc * d§dftpkg
093060070918     c                   if        wpkb > 999999,9
093070070918     c                   eval      wpkb = 999999,9
093080070918     c                   endif
093090070918     c                   eval      §lppkb = wpkb
093100070918 x2  c                   else
093110020522 b2  c                   if        ormvlm <> 0
093120070918     c                   eval      wpkb = ormvlm * d§dftpkg
093130070918     c                   if        wpkb > 999999,9
093140070918     c                   eval      wpkb = 999999,9
093150070918     c                   endif
093160070918     c                   eval      §lppkb = wpkb
093170020522 e3  c                   endif
093180020522 e2  c                   endif
093190020522 e1  c                   endif
093200020522
093210020522     c                   clear                   ds_keyorm
093220020522     c                   eval      ds_poe = ORMpoe
093230020522     c                   eval      ds_nsr = ORMnsr
093240020522     c                   eval      ds_nor = ORMnor
093250020522     c                   eval      ds_nrv = ORMnrv
093260030716     c                   move      ds_keyorm     §lprmn
093270020522
093280020522     c                   eval      §lprma = ormrfa
093290020523     c                   eval      §lprmo = v3cau + '-' + d§cmrdes1
093300020522     c                   eval      §lpctm = '4F'
093310020523     c                   movel     oggi_aammgg   §lpdrt
093320020522
093330020522      * Cerca codice autotrasportatore e flag mattino/pomeriggio
093340020522 b1  c                   if        ormndc <> 0
093350120612      /free
093360120612        DSTnpg = ORMnpg;
093370120612        DSTnfv = ORMndc;
093380120612        DSTfgs = ORMpor;
093390120612        chain (DSTnpg:DSTnfv:DSTfgs) FIDST01L;
093400120612      /end-free
093410120612 b2  c                   if        %found(fidst01l)
093420020522     c                   eval      §lppdr = DSTpdr
093430120612     c                   eval      §lpfpp = DSTfpp
093440020523 x2  c                   else
093450020523     c                   movel     §lplnp        §lppdr
093460020523     c                   move      0999          §lppdr
093470020523     c                   eval      §lpfpp = 'M'
093480020523 e2  c                   endif
093490020522 x1  c                   else
093500020522     c                   movel     §lplnp        §lppdr
093510020522     c                   move      0999          §lppdr
093520020522     c                   eval      §lpfpp = 'M'
093530020522 e1  c                   endif
093540020522
093550020522     C                   call      'FNLS01R'
093560020522     C                   parm                    kpjba
093570020522     C                   parm                    dblp
093580020522     C                   parm                    dtasv
093590030623     c                   Parm                    Trul90ds
093600020522
093610020522     C                   if        §lpfpr = '4' or §lpfpr = '3'
093620020522     C                             or §lpfpr = '5'
093630020522     C                   eval      okbolla = '1'
093640020524     c                   else
093650020524     c                   eval      wbolla = *on
093660020522     C                   endif
093670020522
093680020522     c     endwrtbolla   endsr
093690030606      **********************************************************************
093700030606      * Scrivo bolla addebito "FT"
093710030606      **********************************************************************
093720030606     c     Sr_BollaFt    BegSr
093730030609
093740030609     c                   Clear                   okbolla
093750030623     c                   Eval      wtrulvid = 'S'
093760030623
093770030623      * richiamo il programma per la scelta delle stampanti
093780030623     c                   Clear                   wdatibolla
093790030623     c                   Eval      wdatibolla = 'BollaAdd.'
093800030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
093810030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
093820030623     c                   exsr      sr_scestp
093830030623     c   92              eval      okbolla = '1'
093840030623     c   92              GoTo      endbollaft
093850030606
093860030606     c                   Clear                   Kpjbu
093870030606     c                   Clear                   dtasv
093880030606     c                   Clear                   dblp
093890030606
093900030606     c                   Eval      §lpFpr = 'T'
093910030606      * Cod.Cliente mittente
093920030606      * Ksc dell'ordinante da Fnacr
093930030606     c                   If        AcrKscCor <> *Zeros
093940090504     c                   Eval      §lpksca = %editc(AcrKscCor:'X')
093950090505     c                   if        ACrCCCCor<>999
093960090505     c                   move      AcrCccCor     §lpctra
093970090505     c                   endif
093980030606     c                   Else
093990030606      * Primi 7 byte dell'ordinante
094000090504     c                   Eval      §lpksca = %editc(Ds_V1cor12:'X')
094010030606     c                   EndIf
094020030606
094030050324     c                   Movel     OrmPor        §lpCcn
094040030606     c                   Move      9999          §lpCcn
094050030606     c                   Movel     oggi_aammgg   §lpAas
094060030606     c                   Move      oggi_aammgg   §lpMgs
094070050324     c                   Eval      §lpLnp = OrmPor
094080030606     c                   Eval      §lpCbo = 'FT'
094090030606     c                   Eval      §lpSop = '+'
094100090504     c                   Movel     §lpksca       §lpLna
094110030606      * Mittente bolla = mittente ORM
094120030606     c                   Eval      §lpRsm = V1rsr
094130030606     c                   Eval      §lpInm = V1inr
094140030606     c                   Eval      §lpCam = V1car
094150030606     c                   Eval      §lpLom = V1lor
094160030606     c                   Eval      §lpPrm = V1prr
094170030606     c                   Eval      §lpNzm = V1nar
094180030606      * Destinatario bolla = ordinante ORM
094190030606     c                   Eval      §lpRsd = V1rso
094200030606     c                   Eval      §lpInd = V1ino
094210030606     c                   Eval      §lpCad = V1cao
094220030922     c                   Eval      §lpLod = sav_V1loo35
094230030606     c                   Eval      §lpPrd = V1pro
094240030606     c                   Eval      §lpNzd = V1nao
094250030606      * colli = colli ORM - se a 0 imposto 1
094260030609 b1  c                   if        V1ncl <> 0
094270030609     c                   eval      §lpncl = V1ncl
094280030606 x1  c                   else
094290030606     c                   eval      §lpncl = 1
094300030606 e1  c                   endif
094310030606      * peso = peso ORM - se a 0 ricavo dal volume o dai bancali dell'ORM
094320030609 b1  c                   if        V1pkg <> 0
094330030609     c                   eval      §lppkb = V1pkg
094340030606 x1  c                   else
094350070918 b3  c                   if        V1bnc <> 0
094360070918     c                   eval      wpkb = v1bnc / d§dftbnc * d§dftpkg
094370070918     c                   if        wpkb > 999999,9
094380070918     c                   eval      wpkb = 999999,9
094390070918     c                   endif
094400070918     c                   eval      §lppkb = wpkb
094410070918 x2  c                   else
094420030609 b2  c                   if        V1vlm <> 0
094430070918     c                   eval      wpkb = v1vlm * d§dftpkg
094440070918     c                   if        wpkb > 999999,9
094450070918     c                   eval      wpkb = 999999,9
094460070918     c                   endif
094470070918     c                   eval      §lppkb = wpkb
094480030606 e3  c                   endif
094490030606 e2  c                   endif
094500030606 e1  c                   endif
094510030606      * rif. mittente numerico = n.ORM
094520030606     c                   clear                   ds_keyorm
094530030609     c                   eval      ds_poe = V1cpoe
094540030609     c                   eval      ds_nsr = V1nsr
094550030609     c                   eval      ds_nor = V1nor
094560030609     c                   eval      ds_nrv = V1nrv
094570030716     c                   move      ds_keyorm     §lprmn
094580030606      * rif. mittente alfabetico = rif. alfabetico ORM
094590030609     c                   eval      §lprma = V1rfa
094600030606      * trattamento merce
094610030606     c                   eval      §lpctm = '4F'
094620030606      * data ritiro merce
094630030606     c                   movel     oggi_aammgg   §lpdrt
094640030606
094650030606      * codice autotrasportatore e flag mattino/pomeriggio
094660030606     c                   Eval      §lpFpp = 'M'
094670030606     c                   Movel     §lpLnp        §lpPdr
094680030606     c                   Move      0999          §lpPdr
094690030606
094700030606     c                   Call      'FNLS01R'
094710030606     c                   Parm                    Kpjba
094720030606     c                   Parm                    dblp
094730030606     c                   Parm                    dtasv
094740030623     c                   Parm                    Trul90ds
094750030606
094760030606     c                   If        §lpFpr = '4' or §lpFpr = '3'
094770030606     c                             or §lpFpr = '5'
094780030606     c                   Eval      okbolla = '1'
094790030606     c                   EndIf
094800030606
094810030606     c     endbollaft    EndSr
094820140130
094830140130      /free
094840140130       //--------------------------------------------------------------------
094850140130       //?F17 - Note AUT.
094860140130       //--------------------------------------------------------------------
094870140130       BEGSR F17D01;
094880140130
094890140130       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
094900140130       //?da rcd già vincolato da questo lavoro
094910140130         unlock FNORG01L;
094920140130
094930140130         clear fiort1ds;
094940140226         IORT1mod = 'G';
094950140130         IORT1poe = ORMpoe;
094960140130         IORT1nsr = ORMnsr;
094970140130         IORT1nor = ORMnor;
094980140130         IORT1nrv = ORMnrv;
094990140130         IORT1pgm = 'FIOR05R';
095000140130         §kpjbu = kpjbu;
095010140130
095020140130         fiort1r (kpjba:fiort1ds);
095030140130         kpjbu = §kpjbu;
095040140226
095050140226       //?Se ho scritto note ho la data e ora di immissione
095060140226         IF  OORT1dim > 0 and OORT1oim > 0;
095070140226           wF17write = *on;
095080140226         ENDIF;
095090140130
095100140130       //?Rialloco FNORG
095110140130         chain (kpoe:knsr:knor:knrv) FNORG01L;
095120140314
095130140314       //?Controllo se da F17 sono state scritte delle proposte
095140140314         exsr sr_orp;
095150140130
095160140130       //?Imposto la riga dei tasti funzione
095170140130         ExSr Sr_Tastifun;
095180140130
095190140130       ENDSR;
095200140130
095210140130      /end-free
095220140130
095230001025      **********************************************************************
095240001025      * F18 - Note
095250001025      **********************************************************************
095260001025     C     Sr_Note       BEGSR
095270001025
095280001025     C                   clear                   fior06DS
095290010118     C                   eval      i06dta = v1dao
095300010118     C                   eval      i06poe = v1cpoe
095310010118     C                   eval      i06nor = v1nor
095320010118     C                   eval      i06nsr = v1nsr
095330010118     C                   eval      i06nrv = v1nrv
095340010118     C                   eval      i06far = v1fao
095350010301     C                   eval      i06dai = oggi_aammgg
095360010301     C                   movel     w0140         I06ori
095370010220     C                   if        v1cpoe = *zeros
095380150305     C                   eval      i06poe = DUTpou
095390010220     C                   endif
095400001026
095410001025     C                   select
095420001026     C                   when      *in01 = *on
095430001026     C                   movel     'M'           i06flm
095440001026
095450001109     C                   when      *in03 = *on
095460001109     C                   movel     'M'           i06flm
095470001026
095480001026     C                   when      *in02 = *on
095490040402      * richiamo particolare nel caso di orm commissionato e variazione data ritiro
095500040402     c                   If        ormdar <> inv_v1dar and v1Com = 'S'
095510040419     c                   Eval      I06Flm = 'D'
095520040402     c                   Else
095530001026     C                   movel     'V'           i06flm
095540050421      * se p.o. ritiro diverso da p.o. emissione e da p.o. gestione e non è chiusura ORM
095550050421      * il pgm va in interrogazione note non da la possibilità di inserirne.....
095560050421      * aggiungo che la data ritiro deve essere uguale
095570050421      * xchè per data ritiro diversa e non è un ORM commissionato devo dare la possibilità
095580050421      * di inserire la nota
095590010116     C                   if        v1cpoe <> num_v1cpor  and
095600150305     C                             %lookup(%editc(num_v1cpor:'X'):skpog) = 0
095610150305     C                             and *in10 = *off
095620050421     c                             and ormdar = inv_v1dar
095630001027     C                   movel     'I'           i06flm
095640001027     C                   endif
095650050420     c                   EndIf
095660050128      * se sto facendo una proposta di chiusura ORM la fase deve essere la fase ORM e non la nuova
095670150305     c                   If        *In10 and
095680150305     c                             %lookup(%editc(ORMpor:'X'):skpog) = 0
095690050128     c                   Eval      i06far = OrmFao
095700050128     c                   EndIf
095710050322      * se sto confermando un ORM la fase deve essere la fase ORM e non la nuova
095720050324      * se non è chiusura ORM
095730050324     c                   If        §rmfpr = 'O' and Not *In10
095740050322     c                   Eval      i06far = OrmFao
095750050322     c                   EndIf
095760001026     C                   endsl
095770001026
095780001110     C                   movel(p)  kpjbu         §kpjbu
095790001110     C                   movel(p)  parm01        kpjbu
095800001025     C                   CALL      'FIOR06R'
095810001025     C                   PARM                    kpjba
095820001027     C                   parm                    fior06ds
095830001110     C                   movel     §kpjbu        kpjbu
095840131212      /free
095850131212       IF not *in01;
095860131212         exsr sr_orn;
095870131212       ENDIF;
095880131212       //?Imposto la riga dei tasti funzione
095890131212       ExSr Sr_Tastifun;
095900131212      /end-free
095910001025
095920001025     C                   endsr
095930001103      **********************************************************************
095940001103      * F19 - Proposte
095950001103      **********************************************************************
095960001103     C     Sr_Proposte   BEGSR
095970001103
095980001103     C                   movel     'P'           psce
095990001103     C                   movel     kpjbu         §kpjbu
096000001103     C                   clear                   kpjbu
096010001103     C                   movel     parm01        kpjbu
096020001103     C                   call      'FIOR07R'
096030001103     C                   parm                    kpjba
096040001103     C                   movel     §kpjbu        kpjbu
096050001103
096060001103     C                   endsr
096070001102      **********************************************************************
096080001102      * F20 - Simulazione delivery-tassazione
096090001102      **********************************************************************
096100001102     C     Sr_Simula     BEGSR
096110001116
096120001116     C                   clear                   TISI92DS
096130001116     C                   movel     'ORM'         D92op0
096140001116     C                   movel     v1cpor        D92lnp
096150001116     C                   movel     v1nac         D92nar
096160001116     C                   movel     v1cac         D92cad
096170030922     C                   movel(p)  sav_v1loc35   D92lod
096180001116     C                   movel     v1ffd         D92ffd
096190001116     C                   movel     v1ksc         D92ksc
096200001116     C                   movel     v1ctr         D92ctr
096210001116     C                   z-add     v1ncl         D92ncl
096220001116     C                   z-add     v1pkg         D92pkb
096230001116     C                   z-add     v1vlm         D92vlf
096240001116     C                   movel     v1ddt         D92ddt
096250001102
096260001102     C                   movel     kpjbu         §kpjbu
096270001116     C                   movel(p)  tisi92ds      kpjbu
096280001102     C                   call      'TISI92R'
096290001102     C                   parm                    kpjba
096300001102     C                   movel     §kpjbu        kpjbu
096310001102
096320001102     C                   endsr
096330061023
096340061023      *--------------------------------------------------------------------*
096350061023      * F2 - Dati DPD
096360061023      *--------------------------------------------------------------------*
096370061023     c     sr_datidpd    begsr
096380061023
096390080430     c                   clear                   fieu55ds
096400080430
096410080430      * se ORM commissionato da DPD passo il n. ORM DPD
096420080430     c     ormpoe        lookup    skpodpd                                30
096430080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
096440080430     c                   eval      i55dep = %subst(ormrfa:4:4)
096450080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
096460081015     c                   eval      i55ie = 'I'
096470080430     c                   eval      kpjbu = fieu55ds
096480080430     c                   call      'FIEU55C'
096490080430     c                   parm                    kpjba
096500080430     c                   endif
096510080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
096520080430     c     ormpor        lookup    skpodpd                                30
096530080430     c                   if        *in30
096540061023     c                   eval      i55poe = v1cpoe
096550061023     c                   eval      i55nrs = v1nsr
096560061023     c                   eval      i55nro = v1nor
096570061023     c                   eval      i55nrv = v1nrv
096580081015     c                   eval      i55ie = 'E'
096590061023     c                   eval      kpjbu = fieu55ds
096600061206     c                   call      'FIEU55C'
096610061023     c                   parm                    kpjba
096620080430     c                   endif
096630061023
096640061023     c                   endsr
096650061023
096660050422      *--------------------------------------------------------------------*
096670050420      * CONTROLLO PROPOSTE
096680050422      *--------------------------------------------------------------------*
096690050420     c     Sr_ContrProp  BegSr
096700050420
096710050420     c                   Eval      wproposte = *Off
096720050420     c                   Movel     v1cpoe        kpoe
096730050420     c                   Movel     v1nsr         knsr
096740050420     c                   Movel     v1nor         knor
096750050420     c                   Movel     v1nrv         knrv
096760050420
096770050420     c     kfnor         Setll     Fnorp01l
096780050420     c                   Do        *Hival
096790140227     c     kfnor         Reade(n)  Fnorp01l
096800050420     c                   If        %Eof(Fnorp01l)
096810050420     c                   Leave
096820050420     c                   EndIf
096830050420     c                   If        OrpFev <> *Blanks
096840050420     c                   Iter
096850050420     c                   EndIf
096860050420     c                   Eval      wproposte = *On
096870050420     c                   Leave
096880050420     c                   EndDo
096890050420
096900050420     c                   EndSr
096910040419      **********************************************************************
096920040419      * F21 - Dirottamento
096930040419      **********************************************************************
096940040419     c     Sr_Dirotta    BegSr
096950040419
096960040419     c                   Clear                   wdcpor
096970040419     c                   Clear                   wddpor
096980040419     c                   Move      v1cpor        sav_wdcpor
096990040420     c                   Eval      wdcdar = v1dar
097000040419     c                   Do        *Hival
097010040419     c                   Exfmt     Fior05dw
097020040420     c                   Eval      *In28 = *Off
097030040419      * F12=Ritorno
097040040419     c   kl              Leave
097050040419     c                   ExSr      Sr_ContrWd
097060040420     c   28              Iter
097070040420      * F6=Conferma
097080040420     c                   If        *InKf
097090040615     c                   movel     wdcpor        v1cpor
097100040615     c                   movel     wdcdar        v1dar
097110040615     c                   Eval      v1fao = 050
097120050422     c                   Eval      v1dfo = oggi_aammgg
097130040615     c                   Time                    v1ofo
097140170411      * devo pulire anche i dati relativi al giro
097150170411     c                   clear                   V1Ccgi
097160170411     c                   clear                   V1tgi
097170170411     c                   clear                   sav_V1Ccgi
097180040615     c                   Eval      wokdirotta = *On
097190040420     c                   Leave
097200040420     c                   EndIf
097210040419     c                   EndDo
097220040419
097230040419     c                   EndSr
097240090205
097250090205      *--------------------------------------------------------------------*
097260090205      * Window per immissione dati appuntamento ORM
097270090205      *--------------------------------------------------------------------*
097280090205     c     sr_ormapp     begsr
097290090205
097300090206     c                   eval      *in90 = *off
097310090206     c                   eval      *in93 = *off
097320090206     c                   eval      *in94 = *off
097330090205     c                   do        *hival
097340090206     c                   exfmt     fior05app
097350090206     c                   eval      *in28 = *off
097360090206     c                   eval      *in93 = *off
097370090206     c                   eval      *in94 = *off
097380090206     c                   clear                   wappmsg
097390090206      * f12-ritorno
097400090206     c                   if        *inkl
097410090206     c                   eval      *in90 = *on
097420090206     c                   leave
097430090206     c                   endif
097440090206      * controllo
097450090206     c                   if        wappref = *blanks
097460090206     c                   eval      wappmsg = 'Inserire la persona da -
097470090206     c                             contattare'
097480090206     c                   eval      *in28 = *on
097490090206     c                   eval      *in93 = *on
097500090206     c                   iter
097510090206     c                   endif
097520090206      * almeno 5 caratteri
097530090206     c                   if        %subst(wappref:4:31) = *blanks
097540090206     c                   eval      wappmsg = 'Completare la persona -
097550090206     c                             da contattare'
097560090206     c                   eval      *in28 = *on
097570090206     c                   eval      *in93 = *on
097580090206     c                   iter
097590090206     c                   endif
097600090206      * controllo
097610090206     c                   if        wappnota1 = *blanks
097620090206     c                   eval      wappmsg = 'Inserire le annotazioni'
097630090206     c                   eval      *in28 = *on
097640090206     c                   eval      *in94 = *on
097650090206     c                   iter
097660090206     c                   endif
097670090206      * almeno 5 caratteri
097680090206     c                   if        %subst(wappnota1:4:31) = *blanks
097690090206     c                   eval      wappmsg = 'Completare le -
097700090206     c                             annotazioni'
097710090206     c                   eval      *in28 = *on
097720090206     c                   eval      *in94 = *on
097730090206     c                   iter
097740090206     c                   endif
097750090206      * f6-conferma
097760090206     c   kf              leave
097770090205     c                   enddo
097780090205
097790090205     c                   endsr
097800090205
097810040419      **********************************************************************
097820040419      * Controllo i dati per il dirottamento
097830040419      **********************************************************************
097840040419     c     Sr_ContrWd    BegSr
097850040419
097860040419      * --> P.O. ritiro nuovo
097870040419     c                   Clear                   wddpor
097880040419      * Obbligatorio
097890040419     c                   If        wdcpor = *Zeros
097900040419     c                   Eval      wdcmsg = msg(11)
097910070124     c                   Seton                                        28
097920040419     c                   Goto      endctrwd
097930040419     c                   EndIf
097940040419      * Non può essere lo stesso di prima
097950040419     c                   If        wdcpor = sav_wdcpor
097960040419     c                   Eval      wdcmsg = msg(78)
097970070124     c                   Seton                                        28
097980040419     c                   Goto      endctrwd
097990040419     c                   EndIf
098000040419      * Deve esistere
098010040419     c     wdcpor        Chain     Azorg01L
098020040419     c                   If        Not %Found(Azorg01l)
098030040419     c                   Eval      wdcmsg = msg(33)
098040070124     c                   Seton                                        28
098050040419     c                   Goto      endctrwd
098060040419     c                   EndIf
098070040419      * Non deve essere annullato
098080040419     c                   If        OrgFva <> *Blanks  or
098090040419     c                             (OrgFag <> 'A' and OrgFag <> 'F')
098100040419     c                   Eval      wdcmsg = msg(33)
098110070124     c                   Seton                                        28
098120040419     c                   Goto      endctrwd
098130040419     c                   EndIf
098140040419      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
098150040419     c                   Eval      og148 = OrgDe8
098160040419     c                   If        §ogorm <> 'S'
098170040628     c                   Eval      wdcmsg = msg(79)
098180070124     c                   Seton                                        28
098190040419     c                   Goto      endctrwd
098200040419     c                   EndIf
098210040419      * Decodifico
098220040419     c                   Eval      wddpor = OrgDes
098230070124
098240070124      * se il p.o. che dirotta è abilitato al PDA
098250070124      * e fase 400 o 410
098260071210     c                   if        *in22 and
098270070124     c                             (ormfao = 400 or ormfao = 410)
098280070124      * devo avvisare che ho già avuto un esito da AUT
098290070124      * se l'orm è in fase 410
098300070124     c                   if        ormfao = 410 and wforzadir = *off
098310070124     c                   eval      wdcmsg = msg(41)
098320070124     c                   seton                                        28
098330070124     c                   eval      wforzadir = *on
098340070124     c                   goto      endctrwd
098350070124     c                   endif
098360070124      * in ogni caso devo salvarmi la distinta per passarla al pgm che aggiorna
098370070124      * fipdo
098380070124     c                   eval      savorffgs = orffgs
098390070124     c                   eval      savorfndc = orfndc
098400070125     c                   eval      savorfddc = orfddc
098410070125      * mi salvo anche la fase che mi servirà poi nel caso di ORM dirottato
098420070125     c                   eval      savorffar = orffar
098430070124     c                   endif
098440040419
098450040615      * è sempre un orm commissionato controllo se ok tutti i dati
098460130920      * ma solo se era già un ORM COMMISSIONATO
098470130920     c                   IF        §ORcom = 'S'
098480040615     c                   Eval      V1com = 'S'
098490130918     c                   ENDIF
098500040419
098510040419     c     endctrwd      EndSr
098520001011      **********************************************************************
098530001023      * PRENDE IL NUOVO NUMERO ORDINE
098540001011      **********************************************************************
098550001011     C     Sr_Numor      BEGSR
098560001011
098570010222     C                   if        v1cpoe = *zeros
098580150305     C                   z-add     DUTpou        v1cpoe
098590010222     C                   endif
098600030922
098610030922     c                   clear                   TRUL33DS
098620030922     c                   Eval      I33cnu = 300
098630030922     c                   Eval      I33po1 = V1cpoe
098640030922     c                   Eval      I33num = 1
098650030922     c                   movel(p)  TRUL33DS      KPJBU
098660030922     c                   call      'TRUL33R'
098670030922     c                   parm                    KPJBA
098680030922     c                   movel     KPJBU         TRUL33DS
098690030922
098700030922    1c                   Select
098710030922      * Numeratore non trovato
098720030922     C                   when      O33err =  01
098730030922     C                   eval      *in28 = *on
098740030922     C                   movel     msg(72)       v1cmsg
098750030922     C                   goto      endsrnumor
098760030922      * Numeratore allocato
098770030922     C                   when      O33err =  02
098780030922     C                   eval      *in28 = *on
098790030922     C                   movel     msg(71)       v1cmsg
098800030922     C                   goto      endsrnumor
098810030922      * altro errore
098820030922     C                   when      O33err <> *zeros
098830030922     C                   eval      *in28 = *on
098840030922     C                   movel     O33msg        v1cmsg
098850030922     C                   goto      endsrnumor
098860030922    1C                   endsl
098870030922
098880030922     C                   z-add     O33nrf        v1nor
098890001011
098900010507     C     endsrnumor    endsr
098910070119
098920070119      *--------------------------------------------------------------------*
098930070119      * Carico dati sensibili per PDA modificati
098940070119      *--------------------------------------------------------------------*
098950070119     c     sr_datipdamod begsr
098960070119
098970070119     c                   clear                   datipdamod
098980070119     c                   eval      pdamodrsr = v1rsr
098990070119     c                   eval      pdamodinr = v1inr
099000070119     c                   eval      pdamodlor = v1lor
099010070119     c                   eval      pdamodprr = v1prr
099020070119     c                   eval      pdamodrer = v1rer
099030070119     c                   eval      pdamodter = v1ter
099040070119     c                   eval      pdamodorr = v1orr
099050070119     c                   eval      pdamodncl = v1ncl
099060070119     c                   eval      pdamodpkg = v1pkg
099070070119     c                   eval      pdamodvlm = v1vlm
099080070119     c                   eval      pdamodbnc = v1bnc
099090070119     c                   eval      pdamodno1 = v1not1
099100070119     c                   eval      pdamodno2 = v1not2
099110131107      /free
099120140123        pdamodora1 = V1OraAMda;
099130140123        pdamodora2 = V1OraAMa;
099140140123        pdamodora3 = V1OraPMda;
099150140123        pdamodora4 = V1OraPMa;
099160131107      /end-free
099170070119
099180070119     c                   endsr
099190070119
099200001011      **********************************************************************
099210001026      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
099220001011      **********************************************************************
099230001011     C     Sr_Carfile    BEGSR
099240001011
099250010116     C                   eval      ORMatb = v1atb
099260010116     C                   eval      ORMpoe = v1cpoe
099270010116     C                   eval      ORMnsr = v1nsr
099280010116     C                   eval      ORMnor = v1nor
099290010116     C                   eval      ORMnrv = v1nrv
099300010116     C                   eval      ORMtor = v1ctor
099310010116     C                   eval      ORMtco = v1ctco
099320010116      * Inversione data emissione ORM
099330010116     C                   clear                   wlbdat
099340010116     C                   z-add     v1dao         G02dat
099350010116     C                   movel     *blanks       G02err
099360010116     C                   call      'XSRDA8'
099370010116     C                   parm                    wlbdat
099380010116     C                   z-add     G02inv        ORMdao
099390010116
099400131211     C                   eval      ORMoao = v1oao
099410010116
099420010116     C                   eval      ds_v1cor1 = v1cor1
099430010116     C                   eval      ds_v1cor2 = v1cor2
099440010116     C                   eval      ds_v1cor3 = v1cor3
099450010116     C                   eval      ORMcor = ds_v1ccor
099460010116     C                   eval      ORMrso = v1rso
099470010116     C                   eval      ORMino = v1ino
099480010116     C                   eval      ORMcao = v1cao
099490030922     C                   movel(p)  sav_v1loo35   ORMloo
099500010116     C                   eval      ORMpro = v1pro
099510010116     C                   eval      ORMnao = v1nao
099520010116
099530010116     C                   eval      ds_v1cra1 = v1cra1
099540010116     C                   eval      ds_v1cra2 = v1cra2
099550010116     C                   eval      ds_v1cra3 = v1cra3
099560010116     C                   eval      ORMcra = ds_v1ccra
099570010116     C                   eval      ORMrsr = v1rsr
099580010116     C                   eval      ORMinr = v1inr
099590010116     C                   eval      ORMcar = v1car
099600001103     C                   movel(p)  v1lor         ORMlor
099610010116     C                   eval      ORMprr = v1prr
099620010116     C                   eval      ORMnar = v1nar
099630010116     C                   eval      ORMrer = v1rer
099640010116     C                   eval      ORMter = v1ter
099650010116
099660010116     C                   eval      ds_v1crc1 = v1crc1
099670010116     C                   eval      ds_v1crc2 = v1crc2
099680010116     C                   eval      ds_v1crc3 = v1crc3
099690010116     C                   eval      ORMcrc = ds_v1ccrc
099700010116     C                   eval      ORMrsc = v1rsc
099710010116     C                   eval      ORMinc = v1inc
099720030922     C                   movel(p)  sav_v1loc35   ORMloc
099730010116     C                   eval      ORMcac = v1cac
099740010116     C                   eval      ORMprc = v1prc
099750010116     C                   eval      ORmnac = v1nac
099760001023      * Inversione data ritiro
099770001017     C                   clear                   wlbdat
099780001017     C                   z-add     v1dar         G02dat
099790001017     C                   movel     *blanks       G02err
099800001017     C                   call      'XSRDA8'
099810001017     C                   parm                    wlbdat
099820001025     C                   z-add     G02inv        ORMdar
099830010118     C                   eval      ORMorr = v1orr
099840010116
099850010116     C                   eval      ORMffd = v1ffd
099860010116     C                   eval      ORMnam = v1nam
099870010116     C                   eval      ORMncl = v1ncl
099880010116     C                   eval      ORMpkg = v1pkg
099890010116     C                   eval      ORMvlm = v1vlm
099900010116     C                   eval      ORMbnc = v1bnc
099910010116     C                   eval      ORMblc = v1blc
099920010116     C                   eval      ORMatt = v1att
099930010116     C                   eval      ORMmtc = v1mtc
099940010116     C                   eval      ORMddt = v1ddt
099950140331      * se codice ordinante = codice mittente e impostato che paga l'Ordinante
099960140331      * in automatico metto il 'Mittente' in questo modo elimino un msg di errore
099970140331     c                   IF        V1pag = 'O' and
099980140331     c                             ds_V1cCor = ds_V1cCra and ds_V1cCra > *Zeros
099990140331     c                   eval      v1pag = 'M'
100000140331     c                   ENDIF
100010140331
100020010116     C                   eval      ORMpag = v1pag
100030010116     C                   eval      ORMksc = v1ksc
100040010202     C                   eval      ORMctr = v1ctr
100050001017     C                   movel     v1cpor        ORMpor
100060001016     C                   movel     v1cpoc        ORMpoc
100070010116     C                   move      v1sto         ORMsto
100080010116
100090010116     C                   eval      ORMpos = v1cpos
100100010116     C                   eval      ORMors = v1ors
100110010116     C                   eval      ORMdst = v1dst
100120010116     C                   eval      ORMno1 = v1not1
100130010116     C                   eval      ORMno2 = v1not2
100140010116     C                   eval      ORMrfa = v1rfa
100150010712
100160010116     C                   eval      ORMcst = v1cst
100170010116     C                   eval      ORMvcs = v1vcs
100180010116     C                   eval      ORMfcs = v1fcs
100190050316      * se ORM commissionato e sono in immissione l'ORM deve essere creato in fase
100200050316      * 50 mentre se non è commissionato subito in fase 100
100210081211      * se ORM export DPD sempre in fase 50
100220050316     c                   If        *In01 or *In03
100230081127     c                   If        v1com = 'S' or *in15
100240050316     c                   Eval      v1fao = 50
100250050316     c                   Else
100260050316     c                   Eval      v1fao = 100
100270050316     c                   EndIf
100280151118      /free
100290151118       //?Se ORM da internet ed è un prepagato locale
100300151118       //?lo immetto subito in fase 100 anche se è commissionato
100310151118       //?la telefonata al mittente viene fatta al momento della conferma ORM
100320151118         IF  §RMfpr = 'C' and ORMtco = 'I' and ORMtor = 'P' and
100330151118             ORMpoe = ORMpor;
100340151118           V1fao = 100;
100350151118         ENDIF;
100360140428       //?Alert da inviare
100370140422         IF  walert;
100380140428         //?Se ora ritiro a 0 imposto inizio servizio o l'ora "tappo"
100390140422           IF  ORMorr = 0 and OOR2stis > 0;
100400140422             ORMorr = OOR2stis;
100410140422           ENDIF;
100420140422           IF  ORMorr = 0;
100430140422             ORMorr = d§DFTorrtp;
100440140422           ENDIF;
100450140422         //?Se ora telefono non impostato metto il cell.x sms
100460140422           IF  ORMter = *blanks;
100470140422             ORMter = W03sms;
100480140422           ENDIF;
100490140422         ENDIF;
100500140422      /end-free
100510070912      * devo aggiornare il flag di ORM assegnabile in base alla fase
100520070912     c                   exsr      sr_contrfao
100530050316     c                   EndIf
100540140429
100550140109     C                   eval      ORMdfo = v1dfo
100560140109     C                   eval      ORMofo = v1ofo
100570010116     C                   eval      ORMfao = v1fao
100580010116     C                   eval      ORMnpg = v1npg
100590010116     C                   eval      ORMndc = v1ndc
100600010116     C                   eval      ORMddc = v1ddc
100610010622     C                   eval      ORMstp = v1stp
100620010622     C   10              clear                   ORMnpg
100630010622     C   10              clear                   ORMndc
100640010622     C   10              clear                   ORMddc
100650010622     C   10              clear                   ORMtap
100660010116     C                   eval      ORMtap = v1tap
100670010116     C                   eval      ORMeti = v1eti
100680010119     C                   eval      ORMspi = v1spi
100690011114      * Se forzato blocco destinatario devo pulire il relativo flag
100700011114     C                   if        §ormfd = 'S'
100710011119     C                   if        v1ctor <> 'S'
100720011114     C                   clear                   §ormfd
100730011119     C                   else
100740070219     C                   if        v1rsc = *blanks and ds_v1ccrc = *zeros
100750011119     C                   clear                   §ormfd
100760011114     C                   endif
100770011119     C                   endif
100780011114     C                   endif
100790030606      * ORM commissionato
100800131106      /free
100810131106       //?Se il campo è vuoto imposto 'N' in questo modo in immissione fino a che
100820131106       //?non viene forzato dall'utente il pgm calcola se ORM commissionato o meno
100830131106       //?mentre nel file ho solo 'N' o 'S' (da ora in poi)
100840140424       //?gli unici ORM con il campo a blank sono gli ORM con serie (RC) scritti
100850140424       //?automaticamente dal traduttore
100860140424       //?e gli ORM commissionati da DPD chiusi in automatico da DPD
100870131106         IF  V1com = *blanks;
100880131106           V1com = 'N';
100890131106         ENDIF;
100900131106      /end-free
100910030606     c                   Eval      §orcom = v1com
100920030731
100930030731      * in base al tipo orm e a chi paga imposto il campo §orpfb
100940030731      * imposto 'F' se paga mittente per orm singolo o prepagato
100950030731      * imposto 'A' se NON paga mittente
100960030731     c                   Clear                   §orPfb
100970030731     c                   Select
100980030731     c                   When      OrmPag = 'M'
100990030731     c                   If        OrmTor = 'P' or OrmTor = 'S'
101000030731     c                   Eval      §orPfb = 'F'
101010030731     c                   EndIf
101020030731     c                   Other
101030030731     c                   Eval      §orPfb = 'A'
101040030731     c                   EndSl
101050090403
101060090403      * in immissione ORM
101070090403      * se orm da internet o manuale verifico se impostare il §ormks
101080100913      * solo però se il campo non è già stato impostato da tabella PVO
101090090403     c                   if         *in01 and
101100141211     c                             (ormtco = 'I' or ormtco = 'M' or
101110141212     c                              ORMtco = 'E')
101120100913     c                             and (not ok_pvo or
101130100913     c                                 (ok_pvo and d§pvoks = ' '))
101140090403     c                   exsr      sr_vac
101150090403     c                   endif
101160090403
101170090403      * imposto flag operativi
101180030731     C                   eval      ORMflo = dorm01
101190161115      /free
101200161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
101210161115         //?ipotetico ritiro da effettuare al mattino
101220161115         IF  Orariestesi;
101230161115           ORMrmp = 'S';
101240161115         ELSE;
101250161115           clear ORMrmp;
101260161115         ENDIF;
101270161116         //?Se ORM da Internet non lo memorizzo
101280161116         IF  ORMtco = 'I';
101290161116           clear ORMrmp;
101300161116         ENDIF;
101310161116
101320161115      /end-free
101330010228
101340040908     c                   Eval      OrmDtt = oggi_aammgg
101350001204      * Mette sempre in positivo i campi delle quantità
101360001204     C                   mllzo     1             ORMorr
101370001204     C                   mllzo     1             ORMncl
101380001204     C                   mllzo     1             ORMpkg
101390001204     C                   mllzo     1             ORMvlm
101400001204     C                   mllzo     1             ORMbnc
101410001204     C                   mllzo     1             ORMblc
101420001204     C                   mllzo     1             ORMatt
101430001204     C                   mllzo     1             ORMmtc
101440001109      * Dati di copia
101450001109     C                   if        *in03 = *on
101460001109     C                   z-add     knrv          ORMnrv
101470050316      * se ORM commissionato l'ORM deve essere creato in fase
101480050316      * 50 mentre se non è commissionato subito in fase 100
101490081211      * se ORM export DPD sempre in fase 50
101500081211     c                   If        v1com = 'S' or *in15
101510050316     c                   Eval      ORMfao = 50
101520050316     c                   Else
101530050316     c                   Eval      ORMfao = 100
101540050316     c                   EndIf
101550010118     C                   eval      ORMdfo = oggi_aammgg
101560010118     C                   movel     w0140         ORMofo
101570001130     C                   clear                   ORMnpg
101580001129     C                   z-add     *zeros        ORMndc
101590001129     C                   z-add     *zeros        ORMddc
101600001129     C                   movel     *blanks       ORMtap
101610010223     C                   z-add     *zeros        ORMdst
101620010209      * Aggiorno anche i dati che sono a video per poter scrivere la fase
101630010209     C                   movel     ORMnrv        v1nrv
101640010209     C                   movel     ORMfao        v1fao
101650010209     C                   movel     ORMdfo        v1dfo
101660010209     C                   movel     ORMofo        v1ofo
101670070706      * devo aggiornare il flag di ORM assegnabile in base alla fase
101680070706     c                   exsr      sr_contrfao
101690070706     c                   eval      ormeti = v1eti
101700001109     C                   endif
101710131126
101720131126      /free
101730131126       //?Immissione/Copia ORM
101740131126         IF  *in01 or *in03;
101750131126       //?imposto ora cut-off nel campo Stop
101760140318           ORMstp = OOR2lrnc;
101770160526       //?in teoria se ritiro estero dovrei mettere il nuovo limite per orm esteri
101780160526       //?ma dato che il campo ORMSTP non è usato non lo faccio
101790131126         ENDIF;
101800131126      /end-free
101810001017
101820001017     C                   endsr
101830090403
101840090403      *--------------------------------------------------------------------*
101850090403      * Se conferma orm da VAO controllo se impostare il ritorno VAC
101860090403      *--------------------------------------------------------------------*
101870090403     c     sr_vac        begsr
101880090403
101890090403      * se paga Ordinante
101900090403      * se ho il KSC
101910090403     c                   if        v1pag = 'O' and
101920090403     c                             v1ksc <> *zeros
101930090403      * in tabella 3K trovo il cliente e non devo escludere
101940090403     c                   clear                   ds3k
101950090403     c                   eval      kcod = '3K'
101960090403     c                   movel(p)  v1ksc         kkey
101970090403     c     ktab          chain     tabel00f
101980090403     c                   if        %found(tabel00f) and tblflg = *blanks
101990090403     c                   eval      ds3k = tbluni
102000090403      * ritorno VAC x triangolazione ORM
102010090403     c                   if        §3kvto = 'S'
102020090403      * imposto il falg per ritorno VAC
102030090403     c                   eval      §ormks = 'S'
102040100812     c                   leavesr
102050090403     c                   endif
102060090403     c                   endif
102070100812
102080100812      * se non ho trovato con 3K provo con 3Q
102090100812     c                   clear                   ds3q
102100100812     c                   eval      kcod = '3Q'
102110100812     c                   movel(p)  v1ksc         kkey
102120100812     c     ktab          chain     tabel00f
102130100812     c                   if        %found(tabel00f) and tblflg = *blanks
102140100812     c                   eval      ds3Q = tbluni
102150100812      * ritorno VAB x triangolazione ORM
102160100812     c                   if        §3Qvto = 'S'
102170100812      * imposto il flag per ritorno VAB
102180100812     c                   eval      §ormks = 'S'
102190100812     c                   endif
102200100812     c                   endif
102210100812
102220090403     c                   endif
102230090403
102240090403     c                   endsr
102250140122
102260140122      /free
102270140122       //--------------------------------------------------------------------
102280140122       //?Controllo i dati variati.
102290140122       //--------------------------------------------------------------------
102300140122       BEGSR ctrvardati;
102310140122
102320140122         clear wVardati;
102330140122
102340140122       //?Variazione dati mittente ==> M
102350140122         IF  pdasavrsr <> pdamodrsr or
102360140122             pdasavinr <> pdamodinr or
102370140122             pdasavlor <> pdamodlor or
102380140122             pdasavprr <> pdamodprr or
102390140122             pdasavrer <> pdamodrer or
102400140122             pdasavter <> pdamodter;
102410140122           wVardati = %trim(wVardati) + 'M';
102420140122         ENDIF;
102430140122       //?Variazione dati ora pronta merce ==> O
102440140122         IF  pdasavorr <> pdamodorr or
102450140122             pdasavora1 <> pdamodora1 or
102460140122             pdasavora2 <> pdamodora2 or
102470140122             pdasavora3 <> pdamodora3 or
102480140122             pdasavora4 <> pdamodora4;
102490140122           wVardati = %trim(wVardati) + 'O';
102500140122         ENDIF;
102510140122       //?Variazione dati quantità ==> Q
102520140122         IF  pdasavncl <> pdamodncl or
102530140122             pdasavpkg <> pdamodpkg or
102540140122             pdasavvlm <> pdamodvlm or
102550140122             pdasavbnc <> pdamodbnc;
102560140122           wVardati = %trim(wVardati) + 'Q';
102570140122         ENDIF;
102580140124       //?Variazione dati note ==> N
102590140122         IF  pdasavno1 <> pdamodno1 or
102600140122             pdasavno2 <> pdamodno2;
102610140124           wVardati = %trim(wVardati) + 'N';
102620140122         ENDIF;
102630140122
102640140122       ENDSR;
102650140122
102660140122      /end-free
102670070119
102680070119      *--------------------------------------------------------------------*
102690070119      * Aggiorno file PDO
102700070119      *--------------------------------------------------------------------*
102710070119     c     sr_aggpdo     begsr
102720070301
102730140122      * Richiamo sempre il FIOR56R, ci pensa poi lui a decidere se scrivere i dati
102740140122      * per il PDA e/o la telefonata AUT
102750070123
102760120612      * devo aggiornare FIPDO
102770070228     c                   eval      or56poe = ormpoe
102780070228     c                   eval      or56nsr = ormnsr
102790070228     c                   eval      or56nor = ormnor
102800070228     c                   eval      or56nrv = ormnrv
102810070228     c                   call      'FIOR56R'
102820140122     c                   parm                    kpjba
102830070228     c                   parm                    fior56ds
102840070228      * il pgm torna degli errori, ma dato che ho già fatto qua tutti i controlli
102850070228      * del caso, non me ne preoccupo
102860070119
102870070119     c                   endsr
102880070119
102890001017      **********************************************************************
102900001026      * CARICA IL FILE FNORF DAL FILE FNORM
102910001017      **********************************************************************
102920001017     C     Sr_Carfilef   BEGSR
102930001011
102940081125     c                   clear                   dorf01
102950001019     C                   clear                   fnorf000
102960010116     C                   eval      ORFpoe = v1cpoe
102970010116     C                   eval      ORFnsr = v1nsr
102980010116     C                   eval      ORFnor = v1nor
102990050322     C                   eval      ORFnrv = v1nrv
103000050322      * sono in conferma orm commissioni e sto dirottando l'orm imposto il p.o. ritiro
103010150305      * come po fase
103020050322     c                   If        §rmfpr = 'O' and wokdirotta = *On
103030050322     c                   movel     v1cpor        orfpog
103040050322     c                   Else
103050150305     C                   eval      ORFpog = DUTpou
103060050322     c                   EndIf
103070010116     C                   eval      ORFdae = v1dfo
103080010116     C                   eval      ORFore = v1ofo
103090010116     C                   eval      ORFfar = v1fao
103100001019     C                   eval      ORFpue = knmus
103110010202     C                   eval      ORFcar = v3cau
103120080523      * se sono in conferma proposta di chiusura ORM e l'orm non ha la distinta non la riporto dalla
103130080523      * conferma chiusura perchè ormai l'ORM non è più assegnato
103140080523      * caso di orm assegnato, proposta di chiusura, disassegnato, accetto porposta di chiusura
103150080523     c                   select
103160080523     c                   when      §rmfpr = 'P' and *in10 and ormndc = 0
103170080523     c                   clear                   orffgs
103180080523     c                   clear                   orfndc
103190080523     c                   clear                   orfddc
103200080523      * se c'è la distinta la riporto sulla fase
103210080523     c                   when      OrmNdc > *Zeros
103220040909     c                   Eval      OrfFgs = OrmPor
103230040909     c                   Eval      OrfNdc = OrmNdc
103240040909     c                   Eval      OrfDdc = OrmDdc
103250080523      * se non c'è la distinta riporto sulla fase quella salvata in precedenza
103260080523     c                   when      savndc > *Zeros
103270060516     c                   eval      orffgs = savfgs
103280041014     c                   Eval      OrfNdc = savndc
103290041014     c                   Eval      OrfDdc = savddc
103300080523     c                   endsl
103310081125      * imposto se non ho fatto l'ulteriore addebito
103320081125     c                   if        wnobolla = *on
103330081125     c                   eval      §orfadd = 'S'
103340081125     c                   endif
103350081125     c                   eval      orfflo = dorf01
103360001109
103370001019     C                   write     fnorf000
103380001017
103390001011     C                   endsr
103400001026      **********************************************************************
103410001026      * CARICA IL FILE DELLE NOTE FNORN RICHIAMANDO IL PROGRAMMA
103420001026      **********************************************************************
103430001026     C     Sr_Carnote    BEGSR
103440001026
103450001026     C                   if        *in01 = *on or *in02 = *on
103460001109     C                             or *in03 = *on
103470001026     C                   clear                   fior06ds
103480001026     C                   movel     'C'           i06flm
103490010118     C                   eval      i06dta = ORMdao
103500010118     C                   eval      i06poe = ORMpoe
103510010118     C                   eval      i06nor = ORMnor
103520010118     C                   eval      i06nsr = ORMnsr
103530010118     C                   eval      i06nrv = ORMnrv
103540010118     C                   eval      i06far = ORMfao
103550010301     C                   eval      i06dai = oggi_aammgg
103560010301     C                   movel     w0140         I06ori
103570001026
103580010405     C                   movel(p)  kpjbu         §kpjbu
103590010405     C                   movel(p)  parm01        kpjbu
103600001026     C                   call      'FIOR06R'
103610001026     C                   parm                    kpjba
103620001027     C                   parm                    fior06ds
103630010405     C                   movel     §kpjbu        kpjbu
103640001026
103650001026     C                   endif
103660001026
103670001026     C                   endsr
103680070903
103690070706      *--------------------------------------------------------------------*
103700151016      * File FNORG00F - Scrivo
103710070706      *--------------------------------------------------------------------*
103720151016     c     sr_wrtorg     begsr
103730151016
103740151016     c                   clear                   fnorg000
103750151016     c                   eval      orgpoe = ormpoe
103760151016     c                   eval      orgnsr = ormnsr
103770151016     c                   eval      orgnor = ormnor
103780151016     c                   eval      orgnrv = ormnrv
103790151016     c                   eval      orgpor = ormpor
103800151016     c                   eval      orgpkg = wpkg
103810151016     c                   eval      orgvlm = wvlm
103820151016      * giro immesso
103830151016     c                   if        v1ccgi <> *blanks
103840151016     c                   eval      orgcgi = v1ccgi
103850151016     c                   eval      orgtgi = v1tgi
103860151016     c                   eval      orgpocgi = num_v1cpor
103870151016     c                   time                    w0140
103880151016     c                   movel     w0140         orghvtel
103890151016     c                   move      w0140         orgdtvtel
103900151016     c     *eur          move      orgdtvtel     dataeur
103910151016     c                   move      dataeur       dataiso
103920151016     c                   move      dataiso       orgdtvtel
103930151016     c                   endif
103940151016      * scrivo
103950151016     c                   write     fnorg000
103960151016
103970151016     c                   endsr
103980151016
103990151016      *--------------------------------------------------------------------*
104000151016      * File FNORG00F - Aggiorno
104010151016      *--------------------------------------------------------------------*
104020151016     c     sr_aggorg     begsr
104030070706
104040070706     c                   eval      orgpor = ormpor
104050070706     c                   eval      orgpkg = wpkg
104060070706     c                   eval      orgvlm = wvlm
104070071031     c                   select
104080071031      * non c'è il giro ma prima c'era
104090071031     c                   when      v1ccgi = *blanks and orgcgi <> *blanks
104100071031     c                   clear                   orgcgi
104110071031     c                   clear                   orgtgi
104120071031     c                   clear                   orgpocgi
104130071031     c                   time                    w0140
104140071031     c                   movel     w0140         orghvtel
104150071031     c                   move      w0140         orgdtvtel
104160071031     c     *eur          move      orgdtvtel     dataeur
104170071031     c                   move      dataeur       dataiso
104180071031     c                   move      dataiso       orgdtvtel
104190071031      * non c'è il giro
104200071031     c                   when      v1ccgi = *blanks
104210071031     c                   clear                   orgcgi
104220071031     c                   clear                   orgtgi
104230071031     c                   clear                   orgpocgi
104240071031     c                   clear                   orghvtel
104250071031     c                   clear                   orgdtvtel
104260071031      * giro variato
104270080305      * o stesso giro ma dirottamento (caso di stesso giro codificato su due filiali diverse)
104280071031     c                   when      orgcgi <> v1ccgi or orgtgi <> v1tgi
104290080305     c                             or (orgcgi = v1ccgi and wokdirotta = *on)
104300070919     c                   eval      orgcgi = v1ccgi
104310070919     c                   eval      orgtgi = v1tgi
104320071023     c                   eval      orgpocgi = num_v1cpor
104330071003     c                   time                    w0140
104340071003     c                   movel     w0140         orghvtel
104350071003     c                   move      w0140         orgdtvtel
104360071003     c     *eur          move      orgdtvtel     dataeur
104370071003     c                   move      dataeur       dataiso
104380071003     c                   move      dataiso       orgdtvtel
104390071031     c                   endsl
104400080130      * se è un dirottamento devo pulire i dati relativi a NFTL e SLO
104410080130      * che potrebbero essere rimasti sporchi da un'assegnazione automatica
104420080130      * fatta in precedenza
104430080211      * pulisco anche quello che riguarda la distinta
104440080130     c                   if        wokdirotta = *on
104450080211     c                   clear                   orgpdc
104460080211     c                   clear                   orgfgs
104470080211     c                   clear                   orgndc
104480080211     c                   clear                   orgddc
104490080130     c                   clear                   orgnftl
104500080130     c                   clear                   orgslo
104510080211     c                   clear                   orgdtvdis
104520080211     c                   clear                   orghvdis
104530080130     c                   endif
104540071031      * aggiorno
104550070706     c                   update    fnorg000
104560070706
104570070706     c                   endsr
104580100218
104590100218      /free
104600100218       //--------------------------------------------------------------------
104610100218       //?Scrivo file FNORE00F.
104620100218       //--------------------------------------------------------------------
104630100218       BEGSR sr_Fnore;
104640140516
104650140516         wOKrcdg = *off;
104660100218
104670100225       //?Se previsto il preavviso scrivo l'estensione 'M ' per memorizzare
104680100225       //?l'indirizzo mail dove inviare il preavviso
104690100225       //?(non aggancio il FNVAOE rcd 'M ' in quanto l'indirizzo mail è già recuperato)
104700100225         IF  §ormpre = 'M';
104710100225           clear fnore000;
104720100225           OREpoe = ORMpoe;
104730100225           OREnsr = ORMnsr;
104740100225           OREnor = ORMnor;
104750100225           OREnrv = ORMnrv;
104760100225           OREtrc = 'M ';
104770100225           OREdati = VAOEdati;
104780100225           write fnore000;
104790100225         ENDIF;
104800100218
104810100318       //?Importo le estensioni da VAO a ORM
104820100218         k_vaoecor = sav_vaocor;
104830100218         k_vaoepoe = sav_vaopoe;
104840100218         k_vaoensr = sav_vaonsr;
104850100218         k_vaoenor = sav_vaonor;
104860100218         k_vaoenrv = sav_vaonrv;
104870100218       //?Aggancio il rcd
104880100318         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
104890100318         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104900100318         DOW not %eof(FNVAOE1l);
104910140422         //?ma solo dei rcd che non vengono variati da pgm
104920140422           IF  %lookup(VAOEtrc:skTRCvao) = 0;
104930100318             clear fnore000;
104940100318             OREpoe = ORMpoe;
104950100318             OREnsr = ORMnsr;
104960100318             OREnor = ORMnor;
104970100318             OREnrv = ORMnrv;
104980100318             OREtrc = VAOEtrc;
104990100318             OREdati = VAOEdati;
105000100318             write fnore000;
105010100318           ENDIF;
105020100318           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
105030100318         ENDDO;
105040140422
105050140422       //?Visto che questa routine viene richiamata da conferma VAO
105060140422       //?scrivo subito il rcd 'G', con i dati da VAO tranne i 2 flag ORM per alert
105070140422       //?che sono da impostare in base ai valori calcolati nel pgm
105080140422         k_vaoecor = sav_vaocor;
105090140422         k_vaoepoe = sav_vaopoe;
105100140422         k_vaoensr = sav_vaonsr;
105110140422         k_vaoenor = sav_vaonor;
105120140422         k_vaoenrv = sav_vaonrv;
105130140516         k_vaoetrc = 'G ';
105140140422       //?Scrivo il rcd solo se trovo su VAO
105150140422         chain  %kds(k06fnvaoe) FNVAOE1L;
105160140422         IF  %found(FNVAOE1L);
105170140422           clear fnore000;
105180140422           OREpoe = ORMpoe;
105190140422           OREnsr = ORMnsr;
105200140422           OREnor = ORMnor;
105210140422           OREnrv = ORMnrv;
105220140422           OREtrc = VAOEtrc;
105230140422           OREdati = dOREgen;
105240140422           write fnore000;
105250140516           wOKrcdg = *on;
105260140422         ENDIF;
105270100218
105280100218       ENDSR;
105290131018
105300131018       //--------------------------------------------------------------------
105310131018       //?Scrivo/Aggiorno file FNORE00F.
105320131018       //--------------------------------------------------------------------
105330131018       BEGSR aggFNORE;
105340131018
105350131018         k_OREpoe = ORMpoe;
105360131018         k_OREnsr = ORMnsr;
105370131018         k_OREnor = ORMnor;
105380131018         k_OREnrv = ORMnrv;
105390131018
105400131018       //?Rcd 'O ' - Orari apertura
105410131018        IF  v1OraAMda > 0 or v1OraAMa > 0 or
105420131018            v1OraPMda > 0 or v1OraPMa > 0;
105430131018          k_OREtrc = 'O ';
105440131018          §OREoramda = v1OraAMda;
105450131018          §OREorama  = v1OraAMa;
105460131018          §OREorapda = v1OraPMda;
105470131018          §OREorapa  = v1OraPMa;
105480131029          chain  %kds(k05fnore) FNORE01L;
105490131018          IF  %found(FNORE01L);
105500131125            OREdati = dOREorari;
105510131125            update fnore000;
105520131018          ELSE;
105530131018            clear fnore000;
105540131018            OREpoe = ORMpoe;
105550131018            OREnsr = ORMnsr;
105560131018            OREnor = ORMnor;
105570131018            OREnrv = ORMnrv;
105580131018            OREtrc = 'O ';
105590131018            OREdati = dOREorari;
105600131018            write fnore000;
105610131018          ENDIF;
105620131018        ENDIF;
105630131125       //?Rcd 'O ' - Orari apertura da cancellare
105640131125        IF  v1OraAMda = 0 and v1OraAMa = 0 and
105650131125            v1OraPMda = 0 and v1OraPMa = 0;
105660131125          k_OREtrc = 'O ';
105670131125          §OREoramda = v1OraAMda;
105680131125          §OREorama  = v1OraAMa;
105690131125          §OREorapda = v1OraPMda;
105700131125          §OREorapa  = v1OraPMa;
105710131125          chain  %kds(k05fnore) FNORE01L;
105720131125          IF  %found(FNORE01L);
105730131125            delete fnore000;
105740131125          ENDIF;
105750131125        ENDIF;
105760161020
105770161020       //?Rcd 'DT' - Dta Pronta Merce
105780170524       //?         --> Data Ritiro Calcolata e Anticipo
105790161020        k_OREtrc = 'DT';
105800161027        clear dOREdt;
105810161027        §OREdpm = inv_V1dpm;
105820170524       //?Per immissione o copia devo salvare i dati relativi
105830170524       //?alla data ritiro calcolata se potevo anticipare e se ho anticipato
105840170524       //?in variazione riporto quello che c'era già sul file
105850170524       //?anche se viene variata la data di ritiro
105860170524        §OREdar = %editc(DarCalcolata:'X');
105870170524        IF  wOkPosticipa;
105880170524          §OREposd = 'S';
105890170524        ENDIF;
105900170524        IF  wAnticipa;
105910170524          §OREant = 'S';
105920170524        ENDIF;
105930170524        IF  Anticipato;
105940170524          §OREmod = 'S';
105950170524        ENDIF;
105960170524        §OREgga = %editc(ggAnticipo:'X');
105970161020        chain  %kds(k05fnore) FNORE01L;
105980161020        IF  %found(FNORE01L);
105990161027          OREdati = dOREdt;
106000161020          update fnore000;
106010161020        ELSE;
106020161020          clear fnore000;
106030161020          OREpoe = ORMpoe;
106040161020          OREnsr = ORMnsr;
106050161020          OREnor = ORMnor;
106060161020          OREnrv = ORMnrv;
106070161020          OREtrc = 'DT';
106080161027          OREdati = dOREdt;
106090161020          write fnore000;
106100161020        ENDIF;
106110131018
106120131018       //?Rcd 'MA' - Mail x ALERT
106130131029        IF  old_w03mail <> w03mail or §RMfpr = 'P';
106140131018          k_OREtrc = 'MA';
106150131029          chain  %kds(k05fnore) FNORE01L;
106160131018          IF  %found(FNORE01L);
106170131029            IF  w03mail <> *blanks;
106180131029              OREdati = w03mail;
106190131029              update fnore000;
106200131029            ELSE;
106210131029              delete fnore000;
106220131029            ENDIF;
106230131018          ELSE;
106240131029            IF  w03mail <> *blanks;
106250131029              clear fnore000;
106260131029              OREpoe = ORMpoe;
106270131029              OREnsr = ORMnsr;
106280131029              OREnor = ORMnor;
106290131029              OREnrv = ORMnrv;
106300131029              OREtrc = 'MA';
106310131029              OREdati = w03mail;
106320131029              write fnore000;
106330131029            ENDIF;
106340131018          ENDIF;
106350131018        ENDIF;
106360131018
106370131018       //?Rcd 'S ' - SMS x ALERT
106380131029        IF  old_w03sms <> w03sms or §rmfpr = 'P';
106390131018          k_OREtrc = 'S ';
106400140422          §OREsms = w03sms;
106410131029          chain  %kds(k05fnore) FNORE01L;
106420131018          IF  %found(FNORE01L);
106430131029            IF  w03sms <> *blanks;
106440140422              OREdati = dOREsms;
106450131029              update fnore000;
106460131029            ELSE;
106470131029              delete fnore000;
106480131029            ENDIF;
106490131018          ELSE;
106500131029            IF  w03sms <> *blanks;
106510131029              clear fnore000;
106520131029              OREpoe = ORMpoe;
106530131029              OREnsr = ORMnsr;
106540131029              OREnor = ORMnor;
106550131029              OREnrv = ORMnrv;
106560131029              OREtrc = 'S ';
106570140422              OREdati = dOREsms;
106580131029              write fnore000;
106590131029            ENDIF;
106600131018          ENDIF;
106610131018        ENDIF;
106620160310
106630160406       //?Rcd 'MC' - Mail x Conferma Prenotazione Ritiro
106640160406        IF  old_w04mail <> w04mail;
106650160310          k_OREtrc = 'MC';
106660160310          chain  %kds(k05fnore) FNORE01L;
106670160310          IF  %found(FNORE01L);
106680160406            IF  w04mail <> *blanks;
106690160406              OREdati = w04mail;
106700160310              update fnore000;
106710160310            ELSE;
106720160310              delete fnore000;
106730160310            ENDIF;
106740160310          ELSE;
106750160406            IF  w04mail <> *blanks;
106760160310              clear fnore000;
106770160310              OREpoe = ORMpoe;
106780160310              OREnsr = ORMnsr;
106790160310              OREnor = ORMnor;
106800160310              OREnrv = ORMnrv;
106810160310              OREtrc = 'MC';
106820160406              OREdati = w04mail;
106830160310              write fnore000;
106840160310            ENDIF;
106850160310          ENDIF;
106860160310        ENDIF;
106870160310
106880160406       //?Rcd 'SC' - SMS x Conferma Prenotazione Ritiro
106890160406        IF  old_w04sms <> w04sms;
106900160310          k_OREtrc = 'SC';
106910160310          chain  %kds(k05fnore) FNORE01L;
106920160310          IF  %found(FNORE01L);
106930160406            IF  w04sms <> *blanks;
106940160406              OREdati = w04sms;
106950160310              update fnore000;
106960160310            ELSE;
106970160310              delete fnore000;
106980160310            ENDIF;
106990160310          ELSE;
107000160406            IF  w04sms <> *blanks;
107010160310              clear fnore000;
107020160310              OREpoe = ORMpoe;
107030160310              OREnsr = ORMnsr;
107040160310              OREnor = ORMnor;
107050160310              OREnrv = ORMnrv;
107060160310              OREtrc = 'SC';
107070160406              OREdati = w04sms;
107080160310              write fnore000;
107090160310            ENDIF;
107100160310          ENDIF;
107110160310        ENDIF;
107120140507
107130150305       //?Se sono in immissione
107140160208       //?o copia ORM
107150140603       //?oppure sono in conferma da VAO ma non ho scritto il rcd
107160160208           IF  *in01 or *in03 or
107170140603               (§RMfpr = 'C' and not wOKrcdg);
107180140507         //?Se flag impostati memorizzo rcd 'G'
107190160310           IF  §OREfiso <> *blanks or §OREfimo <> *blanks or
107200160310               §OREfmco <> *blanks or §OREfsco <> *blanks;
107210140507             k_OREtrc = 'G ';
107220140507             chain  %kds(k05fnore) FNORE01L;
107230140507             IF  %found(FNORE01L);
107240140507               OREdati = dOREgen;
107250140507               update fnore000;
107260140507             ELSE;
107270140507               clear fnore000;
107280140507               OREpoe = ORMpoe;
107290140507               OREnsr = ORMnsr;
107300140507               OREnor = ORMnor;
107310140507               OREnrv = ORMnrv;
107320140507               OREtrc = 'G ';
107330140507               OREdati = dOREgen;
107340140507               write fnore000;
107350140507             ENDIF;
107360140507           ENDIF;
107370140507         ENDIF;
107380131014
107390131014       ENDSR;
107400131014
107410131014       //--------------------------------------------------------------------
107420160406       //?F4 - Alert ORM Commissionato.
107430131014       //--------------------------------------------------------------------
107440131014       BEGSR F04D01;
107450131014
107460131025         wEndF04 = *off;
107470131025         wOkF6F04 = *off;
107480140423         IF  not wErrF04;
107490131030           *in28 = *off;
107500131030         ENDIF;
107510131212
107520131212       //?Salvo i dati della videata
107530131212         vidolddA = vidnewA;
107540131025
107550131025       //?emetto la videata relativa alla MAIL e SMS
107560131025         write fior051;
107570140423         *in28 = werralert;
107580140423         werralert = *off;
107590131025       //?emetto la window
107600131025         DOW not wEndF04;
107610131025           exfmt fior053;
107620131025           clear  w03msg;
107630131025           *in28 = *off;
107640131025         SELECT;
107650131025       //?F12-Ritorno
107660131025           WHEN  *inkl;
107670131025             wEndF04 = *on;
107680131025             *in28 = *off;
107690131025             *in90 = *on;
107700131212             vidnewA = vidolddA;
107710131025       //?F6-Conferma
107720131025           WHEN  *inkf;
107730131025             exsr CtrDatiF04;
107740131025             IF  not *in28;
107750131025               wEndF04 = *on;
107760131025               wOkF6F04 = *on;
107770131025             ENDIF;
107780131212             vidolddA = vidnewA;
107790131025       //?F10-Copia n.Telefono
107800131025           WHEN  *inkj;
107810160406             IF  w03sms = *blanks;
107820160310               w03sms = V1ter;
107830160310             ENDIF;
107840131025           OTHER;
107850131025       //?controllo i dati
107860131025             exsr CtrDatiF04;
107870131025           ENDSL;
107880131025         ENDDO;
107890131212       //?Imposto la riga dei tasti funzione
107900131212       ExSr Sr_Tastifun;
107910140509
107920140509       //?Se immissione/copia ORM commissionato e dati alert inseriti
107930140509       //?devo ricalcolare la data di ritiro
107940140512       //?se = ad oggi
107950140509         IF  (*in01 or *in03) and V1com = 'S' and
107960140512             (w03sms <> *blanks or w03mail <> *blanks) and
107970140512              V1dar > 0 and V1dar = oggi_ggmmaa;
107980140509           wCalDtRit = *on;
107990140509         ENDIF;
108000140509
108010140509         *inkf = *off;
108020140509         *inkl = *off;
108030140509         *inkj = *off;
108040131014
108050131014       ENDSR;
108060131011
108070131011       //--------------------------------------------------------------------
108080131011       //?F5 - Altri dati.
108090131011       //--------------------------------------------------------------------
108100131011       BEGSR F05D01;
108110131011
108120131011         wEndF05 = *off;
108130131011         wOkF6F05 = *off;
108140131030         IF  not wErrF05;
108150131030           *in28 = *off;
108160131030         ENDIF;
108170131212
108180131212       //?Salvo i dati della videata
108190131212         vidoldd2 = vidnew2;
108200131212
108210131212       //?Decodifico lo stato
108220131212         exsr sr_contrsto;
108230131212
108240131212       //?Decodifico il giro se impostato
108250131212         clear V1Dcgi;
108260131212         IF  V1Ccgi <> *blanks;
108270131212           exsr ctrgiro;
108280131212           IF  D09ocgi <> *blanks;
108290131212             V1Dcgi = D09odes;
108300131212           ENDIF;
108310131212         ENDIF;
108320131011
108330131011       //?emetto la videata relativa agli altri dati
108340131011         write fior051;
108350131011       //?emetto la window dove richiedo gli altri dati
108360131011         DOW not wEndF05;
108370131011           exfmt fior052;
108380131011           clear  w2cmsg;
108390131011           *in28 = *off;
108400131011         SELECT;
108410131011       //?F12-Ritorno
108420131011           WHEN  *inkl;
108430131011             wEndF05 = *on;
108440131011             *in28 = *off;
108450131011             *in90 = *on;
108460131212             vidnew2 = vidoldd2;
108470131212             wforzaqta = *off;
108480131011       //?F6-Conferma
108490131011           WHEN  *inkf;
108500131017             exsr CtrDatiF05;
108510131011             IF  not *in28;
108520131011               wEndF05 = *on;
108530131011               wOkF6F05 = *on;
108540131011             ENDIF;
108550131212             vidoldd2 = vidnew2;
108560131011           OTHER;
108570131011       //?controllo i dati
108580131017             exsr CtrDatiF05;
108590131011           ENDSL;
108600131011         ENDDO;
108610131212       //?Imposto la riga dei tasti funzione
108620131212       ExSr Sr_Tastifun;
108630131011
108640131011       ENDSR;
108650160406
108660160406       //--------------------------------------------------------------------
108670160406       //?F13 - Conferma Prenotazione.
108680160406       //--------------------------------------------------------------------
108690160406       BEGSR F13D01;
108700160406
108710160406         wEndF13 = *off;
108720160406         wOkF6F13 = *off;
108730160406         IF  not wErrF13;
108740160406           *in28 = *off;
108750160406         ENDIF;
108760160406
108770160413       //?Immissione ORM manuale o copia
108780160406       //?e non ci sono ancora i dati
108790160406         IF  (*in01 or *in03) and
108800160406             (V1Ctco = 'M' or V1Ctco = 'E') and
108810160406              W04sms = *blanks and W04mail = *blanks;
108820160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
108830160406           exsr RecDatiConf;
108840160406         ENDIF;
108850160406
108860160406       //?Se è richiamato da Conferma VAS
108870160406       //?proteggo i dati per Conferma Prenotazione
108880160406       //?a meno che non stia emettendo la videata per sistemare
108890160406       //?eventuali errori
108900160406         IF  §RMfpr = 'C' and not wErrConferma;
108910160406           *in20 = *on;
108920160406         ENDIF;
108930160406
108940160406       //?Salvo i dati della videata
108950160406         vidolddC = vidnewC;
108960160406
108970160406       //?emetto la videata relativa alla MAIL e SMS
108980160406         write fior051;
108990160406         *in28 = wErrConferma;
109000160406         wErrConferma = *off;
109010160406       //?emetto la window
109020160406         DOW  not wEndF13;
109030160406           exfmt fior054;
109040160406           clear w04msg;
109050160406           *in28 = *off;
109060160406           SELECT;
109070160406       //?F6-Conferma
109080160406           WHEN  *inkf;
109090160406             exsr CtrDatiF13;
109100160406             IF  not *in28;
109110160406               wEndF13 = *on;
109120160406               wOkF6F13 = *on;
109130160406             ENDIF;
109140160406             vidolddC = vidnewC;
109150160406       //?F10-Copia n.Telefono
109160160406           WHEN  *inkj;
109170160406             IF  w04sms = *blanks;
109180160406               w04sms = V1ter;
109190160406             ENDIF;
109200160406             HWriga = 06;
109210160406             HWcolo = 07;
109220160406       //?F12-Ritorno
109230160406           WHEN  *inkl;
109240160406             wEndF13 = *on;
109250160406             *in28 = *off;
109260160406             *in90 = *on;
109270160406             vidnewC = vidolddC;
109280160406       //?Enter
109290160406       //?controllo i dati
109300160406           OTHER;
109310160406             exsr CtrDatiF13;
109320160406           ENDSL;
109330160406         ENDDO;
109340160406
109350160406         *inkf = *off;
109360160406         *inkl = *off;
109370160406         *inkj = *off;
109380160406
109390160406       ENDSR;
109400130912
109410130912       //--------------------------------------------------------------------
109420130912       //?F14 - Interrogazione orari di servizio.
109430130912       //--------------------------------------------------------------------
109440130913       BEGSR F14D01;
109450150921
109460150921       //?Se non c'è la filiale ritiro e non ho peso/volume/bancali e non è
109470150921       //?un mittente codificato e siamo in immissione/copia
109480150921       //?non posso interrogare gli orari
109490150921         IF  num_V1Cpor = *zeros and V1pkg = 0 and V1vlm = 0 and
109500150921             V1bnc = 0 and (*in01 or *in03) and ds_v1ccra = 0;
109510150921           V1Cmsg = 'Per F14 serve la fil.ritiro e +
109520150921                     almeno 1 tra peso/vol./bancali';
109530150921           *in28 = *on;
109540150921           *in44 = *on;
109550150921           leavesr;
109560150921         ENDIF;
109570131024
109580131024       //?Se non presente calcolo la filiale ritiro
109590131024         IF  num_V1Cpor = *zeros and
109600131024            (*in01 or *in03);
109610131024           exsr  CALPOR;
109620140109         //?Recupero gli ambiti per il recupero del giro da ACR
109630140109           exsr sr_caragr;
109640131024           IF  *in28;
109650131024             leavesr;
109660131024           ENDIF;
109670131024         ENDIF;
109680130912
109690140319       //?Posso richiamare il pgm di int.orari di servizio solo se ho già
109700130912       //?la filiale di ritiro
109710130912         IF  num_V1Cpor = *zeros;
109720130912           V1Cmsg = 'Per interrogare gli orari serve la filiale ritiro';
109730130912           *in28 = *on;
109740130912           *in59 = *on;
109750130912           leavesr;
109760130912         ENDIF;
109770130912       //?l'indirizzo completo del mittente
109780130912         IF  V1car = *blanks or V1lor = *blanks;
109790130912           V1Cmsg = 'Per interrogare gli orari serve l''indirizzo del +
109800130912                     mittente';
109810130912           *in28 = *on;
109820130912           *in63 = *on;
109830130912           leavesr;
109840130912         ENDIF;
109850161116       //?il peso/volume/bancali
109860161116         IF  V1pkg = 0 and V1vlm = 0 and V1bnc = 0;
109870161116           V1Cmsg = 'Per interrogare gli orari occorre inserire +
109880161116                     almeno 1 tra peso/vol./bancali';
109890161116           *in28 = *on;
109900161116           *in44 = *on;
109910161116           leavesr;
109920161116         ENDIF;
109930161116       //?Calcolo il peso/volume se no in caso di conferma proposta si azzerano
109940161116          exsr sr_pesvol;
109950161116
109960161116       //?Cerco il giorno della settimana di "oggi"
109970161116         exsr DatiPerOrari;
109980140318
109990140318       //?Cerco eventuale ora presunta ritiro
110000140318         clear fior01ds;
110010140319       //?ma solo se ORM in distinta e NON quadrato
110020140318         IF  V1ndc > 0 and V1fao < 600;
110030140318           IOR01fgs = num_V1cpor;
110040140318           IOR01ndc = V1ndc;
110050140318           IOR01poe = V1cpoe;
110060140318           IOR01nsr = V1nsr;
110070140318           IOR01nor = V1nor;
110080140318           IOR01nrv = V1nrv;
110090140318           fior01r (fior01ds);
110100140318         ENDIF;
110110130912
110120140318         clear trulorsds;
110130130912         IOREfil = num_V1Cpor;
110140130912         IOREcap = V1car;
110150130912         IOREloc = V1lor;
110160130920         IOREnar = V1nar;
110170131024         IF  V1dar <> 0;
110180131024           IOREdta = inv_V1dar;
110190131024         ELSE;
110200131024           IOREdta = inv_V1dao;
110210131024         ENDIF;
110220130912         IOREtser = 'R';
110230131108         IOREgf2 = sav_O95gf2;
110240140318         IOREorp = OOR01ora;
110250161026
110260161116       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
110270161116       //?non è da fare per i ritiri esteri
110280161116         clear dIORE01;
110290170529         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks;
110300161116           §IOREr_mnx = 'S';
110310161116         ENDIF;
110320161026         IOREflo = dIORE01;
110330130912
110340140318         trulorsr (kpjba:trulorsds);
110350130912
110360130912       ENDSR;
110370131014
110380131014       //--------------------------------------------------------------------
110390131014       //?Controllo dati 'Altri Dati'.
110400131014       //--------------------------------------------------------------------
110410131014       BEGSR CtrDatiF05;
110420131030
110430131030         *in47 = *off;
110440131030         *in48 = *off;
110450131030         *in49 = *off;
110460131030         *in51 = *off;
110470131030         *in84 = *off;
110480131014
110490131014         exsr Sr_Contrsto;
110500131014         IF  *in28;
110510131014           leavesr;
110520131014         ENDIF;
110530131014
110540131211         exsr Sr_Contrddt;
110550131014         IF  *in28;
110560131014           leavesr;
110570131014         ENDIF;
110580131014
110590131014         exsr sr_Contrqta1;
110600131014         IF  *in28;
110610131014           leavesr;
110620131014         ENDIF;
110630131202       //?calcolo peso o volume se non impostati a video
110640131202       //?considerando anche le cubature degli automezzi eventualmente immessi
110650131202         exsr sr_pesvol;
110660131014
110670131014         exsr sr_contrcgi;
110680131014         IF  *in28 or *in90;
110690131014           leavesr;
110700131014         ENDIF;
110710131014
110720131014       ENDSR;
110730131014
110740131014       //--------------------------------------------------------------------
110750131017       //?Controllo dati Alert.
110760131014       //--------------------------------------------------------------------
110770131025       BEGSR CtrDatiF04;
110780131014
110790160310       //?Dati Alert ORM Commissionato
110800131017       //?controllo SMS;
110810131025         IF  w03sms <> *blanks;
110820131025           pInCell = %trim(w03sms);
110830131017           clear pOutErr;
110840140722           clear pOutCell;
110850131017           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
110860150526             IF  §RMfpr = 'C';
110870150526               clear W03sms;
110880150526               leavesr;
110890150526             ENDIF;
110900131017             *in28 = *on;
110910160406             HWriga = 04;
110920131017             HWcolo = 26;
110930140424             w03msg = msg(60);
110940131017             leavesr;
110950131017           ENDIF;
110960140722           IF  pOutCell <> *blanks;
110970140722             w03sms = pOutCell;
110980140722           ENDIF;
110990131014         ENDIF;
111000131014
111010131025         IF  w03mail <> *blanks;
111020131025           §emlindi = w03mail;
111030131014           xemail (dsemail);
111040131014           IF  §emlerro <> '0';
111050150526             IF  §RMfpr = 'C';
111060150526               clear W03mail;
111070150526               leavesr;
111080150526             ENDIF;
111090160406             HWriga = 06;
111100131017             HWcolo = 07;
111110131014             *in28 = *on;
111120140424             w03msg = msg(55);
111130131014             leavesr;
111140131014           ENDIF;
111150140722           IF  §emlindo <> *blanks;
111160140722             w03mail = §emlindo;
111170140722           ENDIF;
111180131014         ENDIF;
111190131014
111200131014       ENDSR;
111210160406
111220160406       //--------------------------------------------------------------------
111230160406       //?Controllo dati Conferma Prenotazione.
111240160406       //--------------------------------------------------------------------
111250160406       BEGSR CtrDatiF13;
111260160406
111270160406       //?Dati Conferma Prenotazione
111280160406         IF  w04sms <> *blanks;
111290160406           pInCell = %trim(w04sms);
111300160406           clear pOutErr;
111310160406           clear pOutCell;
111320160406           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
111330160406             *in28 = *on;
111340160406             H4riga = 04;
111350160406             H4colo = 26;
111360160406             w04msg = msg(60);
111370160406             leavesr;
111380160406           ENDIF;
111390160406           IF  pOutCell <> *blanks;
111400160406             w04sms = pOutCell;
111410160406           ENDIF;
111420160406         ENDIF;
111430160406
111440160406         IF  w04mail <> *blanks;
111450160406           §emlindi = w04mail;
111460160406           xemail (dsemail);
111470160406           IF  §emlerro <> '0';
111480160406             H4riga = 06;
111490160406             H4colo = 07;
111500160406             *in28 = *on;
111510160406             w04msg = msg(55);
111520160406             leavesr;
111530160406           ENDIF;
111540160406           IF  §emlindo <> *blanks;
111550160406             w04mail = §emlindo;
111560160406           ENDIF;
111570160406         ENDIF;
111580160406
111590160406       //?Dati Conferma VAS e avevo il flag di OK conferma ma non ho i dati
111600160406       //?pulisco il flag in modo da non scrivere il file spia
111610160406         IF  §RMfpr = 'C' and
111620160406             W04mail = *blanks and W04sms = *blanks;
111630160406           wconferma = *off;
111640160406         ENDIF;
111650160406
111660160406         sav_mailconf = w04mail;
111670160406         sav_smsconf  = w04sms;
111680160406
111690160406       ENDSR;
111700140320
111710140320       //--------------------------------------------------------------------
111720140320       //?Cerco orari servizio.
111730140320       //--------------------------------------------------------------------
111740140320       BEGSR CercaOrari;
111750161026
111760161026       //?Se immissione/copia o conferma ORM commissionati
111770161026       //?Visualizzo orari servizio
111780161026         IF  *in01 or *in03 or §RMfpr = 'O';
111790161026           *in95 = *on;
111800161026         ENDIF;
111810140320
111820160526       //?Richiamo il TRUL solo se non è ritiro Estero
111830140320         clear trulorsds;
111840140320         clear trulor2ds;
111850160526         IF  V1nar = *blanks;
111860140320         IOREfil = num_V1Cpor;
111870140320         IOREcap = V1car;
111880140320         IOREloc = V1lor;
111890140320         IOREnar = V1nar;
111900140320         IF  V1dar <> 0;
111910140320           IOREdta = inv_V1dar;
111920140320         ELSE;
111930140320           IOREdta = inv_V1dao;
111940140320         ENDIF;
111950140320         IOREtser = 'R';
111960140320         IOREgf2 = sav_O95gf2;
111970140320         trulorsr (kpjba:trulorsds:trulor2ds);
111980140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
111990140320           *in95 = *off;
112000140320           leavesr;
112010140320         ELSE;
112020140320           V1ostis = OOR2stis;
112030140320           V1ostfs = OOR2stfs;
112040140320         ENDIF;
112050160526         ENDIF;
112060160526       //?Se ritiro Estero chiamo nuovo FIORE00R
112070160526         IF  V1nar <> *blanks;
112080160526           clear FIORE00DS;
112090160526           IORE00por = num_V1Cpor;
112100160526           IORE00nar = V1nar;
112110160526           fiore00r (fiore00ds);
112120160526           IF  OORE00err <> *blanks;
112130160526             *in95 = *off;
112140160526             leavesr;
112150160526           ELSE;
112160160526             V1ostis = OORE00min;
112170160526             V1ostfs = OORE00max;
112180160526           ENDIF;
112190160526         ENDIF;
112200161115
112210161115         exsr DatiPerOrari;
112220161021
112230161021       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
112240161021       //?non è da fare per i ritiri esteri
112250161115       //?non è da fare se non ho gli orari minimo e massimo
112260161027         OrariEstesi = *off;
112270161115         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks and
112280161027             OOR2miis > *zeros and OOR2mxfs > *zeros;
112290161021           V1ostis = OOR2miis;
112300161021           V1ostfs = OOR2mxfs;
112310161027           OrariEstesi = *on;
112320161021         ENDIF;
112330140320
112340140320         wOKorariSer = *on;
112350140320
112360140320       ENDSR;
112370161021
112380161021       //--------------------------------------------------------------------
112390161026       //?Aggiungo/Sottraggo 1 gg lavorativo alla data ritiro
112400161021       //--------------------------------------------------------------------
112410161026       BEGSR AddSubUnoDar;
112420161027
112430161027       //?Non dovrebbe essere possibile, ma non si sa mai
112440161027         IF  V1dar = 0;
112450161027           *in28 = *on;
112460161027           V1Cmsg = 'PageUp/PageDown non possibile senza data ritiro';
112470161027           leavesr;
112480161027         ENDIF;
112490161027
112500161027         clear XGIOLAVDS;
112510161027         clear wlbdat;
112520161027         IXGLdata = inv_V1dar;
112530161027         IF  dsp_aid = c_PageDown;
112540161027           IXGLsub  = 'S';
112550161027         ENDIF;
112560161027         IF  dsp_aid = c_PageUp;
112570161027           IXGLadd  = 'S';
112580161027         ENDIF;
112590161027         IXGLgg   = 1;
112600161027         IXGLlav  = 'S';
112610161027         IXGLpa   = 'P';
112620161027         IXGLfil  = num_V1Cpor;
112630161027         XGIOLAV (xgiolavds);
112640161027         IF  OXGLerr <> *blanks;
112650161027           leavesr;
112660161027         ENDIF;
112670161027
112680161122       //?Solo se immissione
112690161122         IF  *in01;
112700161028         //?Se data ritiro inferiore alla data ritiro calcolata errore
112710161028           IF  OXGldata < OOR97dar and
112720161028               OXGLdata < OOR97dmin;
112730161028             *in28 = *on;
112740161028             V1Cmsg = msg(51);
112750161028             leavesr;
112760161028           ENDIF;
112770161028         ENDIF;
112780161027
112790161108       //?Supera la data max consentita
112800161108         IF  OXGLdata > OOR97dmaxb;
112810161108           *in28 = *on;
112820161108           V1Cmsg = msg(35);
112830161108           V1Cmsg = %trim(V1Cmsg) + ' supera '+
112840161108           d§dftgg2 + ' gg. dalla data immissione ORM';
112850161108           leavesr;
112860161108         ENDIF;
112870161108
112880161108       //?Non può essere inferiore alla data immissione ORM
112890161108         IF  OXGLdata < inv_V1dao;
112900161108           *in28 = *on;
112910161108           V1Cmsg = msg(65);
112920161108           leavesr;
112930161108         ENDIF;
112940161027
112950161110         clear wlbdat;
112960161027         G02inv = OXGLdata;
112970161027         G02err = '3';
112980161027         xsrda8 (wlbdat);
112990161027         V1dar = G02dat;
113000161027         inv_v1dar = OXGLdata;
113010161108         old_invv1dar = OXGLdata;
113020161021
113030161021       ENDSR;
113040130912
113050100218      /end-free
113060090206
113070090206      *--------------------------------------------------------------------*
113080090206      * File FNORA00F
113090090206      *--------------------------------------------------------------------*
113100090206     c     sr_wrtapp     begsr
113110090206
113120090206     c     kfnora        chain     fnora01l
113130090206     c                   if        not %found(fnora01l)
113140090206     c                   clear                   fnora000
113150090206     c                   eval      orapoe = ormpoe
113160090206     c                   eval      oransr = ormnsr
113170090206     c                   eval      oranor = ormnor
113180090206     c                   eval      oranrv = ormnrv
113190090407     c                   eval      orafar = 100
113200090216     c                   eval      oradae = v1dfo
113210090216     c                   eval      oraore = v1ofo
113220090206     c                   eval      oraref = wappref
113230090206     c                   eval      oranote = wappnota1 + wappnota2
113240090206     c                   eval      oradata = oggi_aammgg
113250090206     c                   time                    w0140
113260090206     c                   movel     w0140         oraora
113270090206     c                   eval      oraute = knmus
113280090206     c                   write     fnora000
113290090206     c                   endif
113300090206
113310090206     c                   if        %found(fnora01l)
113320090206     c                   eval      oraref = wappref
113330090206     c                   eval      oranote = wappnota1 + wappnota2
113340090206     c                   eval      oradata = oggi_aammgg
113350090206     c                   time                    w0140
113360090206     c                   movel     w0140         oraora
113370090206     c                   eval      oraute = knmus
113380090206     c                   update    fnora000
113390090206     c                   endif
113400090206
113410090206     c                   endsr
113420070903
113430010704      **********************************************************************
113440010704      * RICHIAMO IMMISSIONE PREPAGATI
113450010704      **********************************************************************
113460010704     C     Sr_Immprep    begsr
113470030623
113480030623     c                   Clear                   wtrulvid
113490010704
113500020522      * richiamo il programma per la scelta delle stampanti
113510030307     c                   Clear                   wdatibolla
113520030307     c                   Eval      wdatibolla = 'Prepagati'
113530030623     c                   Eval      wdatibolA4 = 'Prepag_A4'
113540030623     c                   Eval      wdatibolA5 = 'Prepag_A5'
113550020522     c                   exsr      sr_scestp
113560020522     c   92              goto      endsrimmprep
113570010704
113580010704      * Richiamo immissione prepagati
113590010704     C                   clear                   kpjbu
113600010704     C                   clear                   dtasv
113610010704     C                   clear                   dblp
113620010704
113630010704     C                   move      'O'           §LPfpr
113640010704     C                   movel     ORMpoe        §LPpoe
113650010704     C                   movel     ORMnsr        §LPnsr
113660010704     C                   movel     ORMnor        §LPnor
113670010704     C                   movel     ORMnrv        §LPnrv
113680010704
113690010704     C                   call      'FNLS01R'
113700010704     C                   parm                    kpjba
113710010704     C                   parm                    dblp
113720010704     C                   parm                    dtasv
113730030623     c                   Parm                    Trul90ds
113740010704
113750010704     C                   eval      okbolla = '1'
113760010704
113770010704     C                   if        §lpfpr = '4' or §lpfpr = '3'
113780010704     C                             or §lpfpr = '5'
113790010704     C                   eval      okbolla = ' '
113800010704     C                   endif
113810010704
113820010704     C     endsrimmprep  endsr
113830020522      **********************************************************************
113840020522      * scelta stampante per immissione bolla
113850020522      **********************************************************************
113860020522     c     sr_scestp     begsr
113870020522
113880020522     C                   clear                   kpjbu
113890020522     c                   setoff                                       92
113900020522
113910020522      * Richiesta stampanti
113920020522     C                   if        conta = *zeros
113930030623     C                   clear                   trul90ds
113940030623      * se non serve non faccio vedere il video della scelta stampanti
113950030623     c                   If        wtrulvid <> *Blanks
113960030623     C                   movel     'X'           D90rse
113970030623     C                   movel     'X'           D90rsb
113980030623     c                   Else
113990030623     C                   movel     'S'           D90rse
114000030623     C                   movel     'S'           D90rsb
114010030623     c                   EndIf
114020030623     C                   call      'TRUL90R'
114030020522     C                   parm                    kpjba
114040030623     C                   parm                    trul90ds
114050020522      * F3=Fine
114060030623     C                   if        D90f3 = '1'
114070020522     C                   eval      *in92 = *on
114080020522     C                   goto      endstp
114090020522     C                   endif
114100030307
114110030307     C                   z-add     103           lung
114120020522
114130020522      * Ovrprtf Segnacolli
114140030623     C                   movea     D90pre        cm3(28)
114150030623     C                   movea     D90mde        cm3(50)
114160020522     C                   clear                   comman
114170020522     C                   movea     cm3           comman
114180020522     C                   CALL      'QCMDEXC'                            92
114190020522     C                   PARM                    comman
114200020522     C                   PARM                    lung
114210020522     C   92              goto      endstp
114220030623
114230030623      * Bolle - Nuovo modulo
114240030623
114250030623     c                   Z-Add     110           lung
114260030623
114270030623      * Ovrprtf Bolle A4
114280030623     c                   Movea     D90prb4       cma4(30)
114290030623     c                   Movea     D90mdb4       cma4(52)
114300030623     c                   Movea     wdatibolA4    cma4(96)
114310030623     c                   Clear                   Comman
114320030623     c                   Movea     cmA4          Comman
114330030623     c                   Call      'QCMDEXC'                            92
114340030623     c                   Parm                    Comman
114350030623     c                   Parm                    Lung
114360030623     c   92              goto      endstp
114370030623
114380030623      * Ovrprtf Bolle A5
114390030623     c                   Movea     D90prb5       cmA5(30)
114400030623     c                   Movea     D90mdb5       cmA5(52)
114410030623     c                   Movea     wdatibolA5    cma5(96)
114420030623     c                   Clear                   Comman
114430030623     c                   Movea     cmA5          Comman
114440030623     c                   Call      'QCMDEXC'                            92
114450030623     c                   Parm                    Comman
114460030623     c                   Parm                    Lung
114470030623     c   92              goto      endstp
114480020522
114490030623      * se non faccio vedere il video non aggiorno il contatore
114500030623     c                   If        wtrulvid = *Blanks
114510020522     C                   eval      conta = conta +1
114520030623     c                   EndIf
114530020522     C                   endif
114540020522
114550020522     c     endstp        endsr
114560001019      **********************************************************************
114570001023      * SCRIVE storico variazioni
114580001019      **********************************************************************
114590001019     C     Sr_Storico    BEGSR
114600001019
114610001019     C                   clear                   fnorv000
114620001019     C                   eval      ORVpoe = ORMpoe
114630001019     C                   eval      ORVnsr = ORMnsr
114640001019     C                   eval      ORVnor = ORMnor
114650001019     C                   eval      ORVnrv = ORMnrv
114660001019     C                   eval      ORVtor = ORMtor
114670001019     C                   eval      ORVcor = ORMcor
114680001019     C                   eval      ORVrso = ORMrso
114690001019     C                   eval      ORVino = ORMino
114700001019     C                   eval      ORVcao = ORMcao
114710001019     C                   eval      ORVloo = ORMloo
114720001019     C                   eval      ORVpro = ORMpro
114730001019     C                   eval      ORVnao = ORMnao
114740001019     C                   eval      ORVcra = ORMcra
114750001019     C                   eval      ORVrsr = ORMrsr
114760001019     C                   eval      ORVinr = ORMinr
114770001019     C                   eval      ORVcar = ORMcar
114780001019     C                   eval      ORVlor = ORMlor
114790001019     C                   eval      ORVprr = ORMprr
114800001019     C                   eval      ORVnar = ORMnar
114810001019     C                   eval      ORVrer = ORMrer
114820001019     C                   eval      ORVter = ORMter
114830001019     C                   eval      ORVcrc = ORMcrc
114840001019     C                   eval      ORVrsc = ORMrsc
114850001019     C                   eval      ORVinc = ORMinc
114860001019     C                   eval      ORVloc = ORMloc
114870001019     C                   eval      ORVcac = ORMcac
114880001019     C                   eval      ORVprc = ORMprc
114890001019     C                   eval      ORVnac = ORMnac
114900001019     C                   eval      ORVffd = ORMffd
114910001019     C                   eval      ORVdar = ORMdar
114920001019     C                   eval      ORVorr = ORMorr
114930001019     C                   eval      ORVnam = ORMnam
114940001019     C                   eval      ORVncl = ORMncl
114950001019     C                   eval      ORVpkg = ORMpkg
114960001019     C                   eval      ORVvlm = ORMvlm
114970001019     C                   eval      ORVbnc = ORMbnc
114980001019     C                   eval      ORVblc = ORMblc
114990001019     C                   eval      ORVatt = ORMatt
115000001019     C                   eval      ORVmtc = ORMmtc
115010001019     C                   eval      ORVpag = ORMpag
115020001019     C                   eval      ORVksc = ORMksc
115030001019     C                   eval      ORVctr = ORMctr
115040001024     C                   eval      ORVddt = ORMddt
115050001019     C                   eval      ORVpor = ORMpor
115060130416     C                   eval      ORVzor = ORMzor
115070001019     C                   eval      ORVpoc = ORMpoc
115080001019     C                   eval      ORVno1 = ORMno1
115090001019     C                   eval      ORVno2 = ORMno2
115100001019     C                   eval      ORVcst = ORMcst
115110001019     C                   eval      ORVvcs = ORMvcs
115120001019     C                   eval      ORVfcs = ORMfcs
115130001019     C                   eval      ORVdfo = ORMdfo
115140001019     C                   eval      ORVofo = ORMofo
115150001019     C                   eval      ORVfao = ORMfao
115160001027     C                   eval      ORVsto = ORMsto
115170001019     C                   eval      ORVdst = ORMdst
115180001019     C                   eval      ORVnpg = ORMnpg
115190001019     C                   eval      ORVndc = ORMndc
115200001019     C                   eval      ORVddc = ORMddc
115210001019     C                   eval      ORVstp = ORMstp
115220001027     C                   eval      ORVrfa = ORMrfa
115230070706     C                   eval      ORVeti = ORMeti
115240001103     C                   eval      ORVsto = ORMsto
115250010119     C                   eval      ORVspi = ORMspi
115260001019     C                   eval      ORVflo = ORMflo
115270010116     C                   z-add     oggi_aammgg   ORVdtv
115280001019     C                   movel     w0140         ORVorv
115290001019     C                   movel     knmus         ORVutv
115300040908     c                   Eval      OrvDtt = oggi_aammgg
115310161115     c                   eval      ORVrmp = ORMrmp
115320001019     C                   write     fnorv000
115330131024
115340131024      /free
115350131024       //?Scrivo anche le estensioni
115360131024         IF  wOkestensione;
115370131024           setll    (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115380131024           reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115390140520           DOW  not %eof(FNORE01L);
115400131024             clear FNORVE00;
115410131024             ORVEpoe = OREpoe;
115420131024             ORVEnsr = OREnsr;
115430131024             ORVEnor = OREnor;
115440131024             ORVEnrv = OREnrv;
115450131024             ORVEdtv = ORVdtv;
115460131024             ORVEorv = ORVorv;
115470131024             ORVEutv = ORVutv;
115480131024             ORVEtrc = OREtrc;
115490131024             ORVEdati = OREdati;
115500131024             write FNORVE00;
115510140520             reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115520140520           ENDDO;
115530131024         ENDIF;
115540131024      /end-free
115550001019
115560001019     C                   endsr
115570001019      **********************************************************************
115580001023      * SCRIVE proposte variazioni
115590001019      **********************************************************************
115600001103     C     Sr_CarProposteBEGSR
115610140227
115620140227      /free
115630140227       //?Se è stata fatta una proposta di nota AUT devo aggiornare FNORP
115640140227       //?scritto da FIORT1R
115650140227         IF  wF17write;
115660140227           chain (kpoe:knsr:knor:knrv:OORT1dim:OORT1oim) FNORP01L;
115670140227           IF  %found(FNORP01L);
115680140227             exsr move_proposte;
115690140227             update fnorp000;
115700140227           ENDIF;
115710140227         ENDIF;
115720140227      /end-free
115730001019
115740140227     c                   IF        not wF17write
115750010116     C                   clear                   fnorp000
115760010116     C                   eval      ORPpoe = v1cpoe
115770010116     C                   eval      ORPnsr = v1nsr
115780010116     C                   eval      ORPnor = v1nor
115790010116     C                   eval      ORPnrv = v1nrv
115800140227     c                   exsr      move_proposte
115810140227     C                   z-add     oggi_aammgg   ORPdtv
115820140227     C                   movel     w0140         ORPorv
115830140227     C                   movel     knmus         ORPutv
115840001019     C                   write     fnorp000
115850140227     c                   ENDIF
115860131024
115870131024      /free
115880131024       //?Scrivo anche le estensioni
115890131024         IF  v1OraAMda <> 0 or v1OraAMa <> 0 or
115900131024             v1OraPMda <> 0 or v1OraPMa <> 0;
115910131024           §OREoramda = v1OraAMda;
115920131024           §OREorama  = v1OraAMa;
115930131024           §OREorapda = v1OraPMda;
115940131024           §OREorapa  = v1OraPMa;
115950131024           clear FNORPE00;
115960131024           ORPEpoe = ORPpoe;
115970131024           ORPEnsr = ORPnsr;
115980131024           ORPEnor = ORPnor;
115990131024           ORPEnrv = ORPnrv;
116000131024           ORPEdtv = ORPdtv;
116010131024           ORPEorv = ORPorv;
116020131024           ORPEutv = ORPutv;
116030131024           ORPEtrc = 'O ';
116040131024           ORPEdati = dOREorari;
116050131024           write FNORPE00;
116060131024         ENDIF;
116070161020         //?RCD DT - Data Pronta Merce
116080161027         clear dOREdt;
116090161027         §OREdpm = inv_v1dpm;
116100170605         §OREdar = %editc(DarCalcolata:'X');
116110170605         IF  wOkPosticipa;
116120170605           §OREposd = 'S';
116130170605         ENDIF;
116140170605         IF  wAnticipa;
116150170605           §OREant = 'S';
116160170605         ENDIF;
116170170605         IF  Anticipato;
116180170605           §OREmod = 'S';
116190170605         ENDIF;
116200170605         §OREgga = %editc(ggAnticipo:'X');
116210161020         clear fnorpe00;
116220161020         ORPEpoe = ORPpoe;
116230161020         ORPEnsr = ORPnsr;
116240161020         ORPEnor = ORPnor;
116250161020         ORPEnrv = ORPnrv;
116260161020         ORPEdtv = ORPdtv;
116270161020         ORPEorv = ORPorv;
116280161020         ORPEutv = ORPutv;
116290161020         ORPEtrc = 'DT';
116300161027         ORPEdati = dOREdt;
116310161020         write FNORPE00;
116320160406         IF  w04sms <> *blanks;
116330160311           clear FNORPE00;
116340160311           ORPEpoe = ORPpoe;
116350160311           ORPEnsr = ORPnsr;
116360160311           ORPEnor = ORPnor;
116370160311           ORPEnrv = ORPnrv;
116380160311           ORPEdtv = ORPdtv;
116390160311           ORPEorv = ORPorv;
116400160311           ORPEutv = ORPutv;
116410160311           ORPEtrc = 'SC';
116420160406           ORPEdati = w04sms;
116430160311           write FNORPE00;
116440160311         ENDIF;
116450160406         IF  w04mail <> *blanks;
116460160311           clear FNORPE00;
116470160311           ORPEpoe = ORPpoe;
116480160311           ORPEnsr = ORPnsr;
116490160311           ORPEnor = ORPnor;
116500160311           ORPEnrv = ORPnrv;
116510160311           ORPEdtv = ORPdtv;
116520160311           ORPEorv = ORPorv;
116530160311           ORPEutv = ORPutv;
116540160311           ORPEtrc = 'MC';
116550160406           ORPEdati = w04mail;
116560160311           write FNORPE00;
116570160311         ENDIF;
116580131025         IF  w03sms <> *blanks;
116590140505           §OREsms = w03sms;
116600131024           clear FNORPE00;
116610131024           ORPEpoe = ORPpoe;
116620131024           ORPEnsr = ORPnsr;
116630131024           ORPEnor = ORPnor;
116640131024           ORPEnrv = ORPnrv;
116650131024           ORPEdtv = ORPdtv;
116660131024           ORPEorv = ORPorv;
116670131024           ORPEutv = ORPutv;
116680131024           ORPEtrc = 'S ';
116690140505           ORPEdati = dOREsms;
116700131024           write FNORPE00;
116710131024         ENDIF;
116720131025         IF  w03mail <> *blanks;
116730131024           clear FNORPE00;
116740131024           ORPEpoe = ORPpoe;
116750131024           ORPEnsr = ORPnsr;
116760131024           ORPEnor = ORPnor;
116770131024           ORPEnrv = ORPnrv;
116780131024           ORPEdtv = ORPdtv;
116790131024           ORPEorv = ORPorv;
116800131024           ORPEutv = ORPutv;
116810131024           ORPEtrc = 'MA';
116820131025           ORPEdati = w03mail;
116830131024           write FNORPE00;
116840131024         ENDIF;
116850131024      /end-free
116860001019
116870001019     C                   endsr
116880140227
116890140227      *------------------------------------------------------------------------*
116900140227      *  Muovo i campi del video al file delle proposte
116910140227      *------------------------------------------------------------------------*
116920140227     c     move_proposte begsr
116930140227
116940140227     C                   eval      ORPtor = v1ctor
116950140227     C                   eval      ds_v1cor1 = v1cor1
116960140227     C                   eval      ds_v1cor2 = v1cor2
116970140227     C                   eval      ds_v1cor3 = v1cor3
116980140227     C                   eval      ORPcor = ds_v1ccor
116990140227     C                   eval      ORPrso = v1rso
117000140227     C                   eval      ORPino = v1ino
117010140227     C                   eval      ORPcao = v1cao
117020140227     C                   movel(p)  sav_v1loo35   ORPloo
117030140227     C                   eval      ORPpro = v1pro
117040140227     C                   eval      ORPnao = v1nao
117050140227     C                   eval      ds_v1cra1 = v1cra1
117060140227     C                   eval      ds_v1cra2 = v1cra2
117070140227     C                   eval      ds_v1cra3 = v1cra3
117080140227     C                   eval      ORPcra = ds_v1ccra
117090140227     C                   eval      ORPrsr = v1rsr
117100140227     C                   eval      ORPinr = v1inr
117110140227     C                   eval      ORPcar = v1car
117120140227     C                   movel(p)  v1lor         ORPlor
117130140227     C                   eval      ORPprr = v1prr
117140140227     C                   eval      ORPnar = v1nar
117150140227     C                   eval      ORPrer = v1rer
117160140227     C                   eval      ORPter = v1ter
117170140227     C                   eval      ds_v1crc1 = v1crc1
117180140227     C                   eval      ds_v1crc2 = v1crc2
117190140227     C                   eval      ds_v1crc3 = v1crc3
117200140227     C                   eval      ORPcrc = ds_v1ccrc
117210140227     C                   eval      ORPrsc = v1rsc
117220140227     C                   eval      ORPinc = v1inc
117230140227     C                   movel(p)  sav_v1loc35   ORPloc
117240140227     C                   eval      ORPcac = v1cac
117250140227     C                   eval      ORPprc = v1prc
117260140227     C                   eval      ORPnac = v1nac
117270140227     C                   eval      ORPffd = v1ffd
117280140227      * Inversione data ritiro
117290140227     C                   clear                   wlbdat
117300140227     C                   z-add     v1dar         G02dat
117310140227     C                   movel     *blanks       G02err
117320140227     C                   call      'XSRDA8'
117330140227     C                   parm                    wlbdat
117340140227     C                   z-add     G02inv        ORPdar
117350140227
117360140227     C                   eval      ORPorr = v1orr
117370140227     C                   eval      ORPnam = v1nam
117380140227     C                   eval      ORPncl = v1ncl
117390140227     C                   eval      ORPpkg = v1pkg
117400140227     C                   eval      ORPvlm = v1vlm
117410140227     C                   eval      ORPbnc = v1bnc
117420140227     C                   eval      ORPblc = v1blc
117430140227     C                   eval      ORPatt = v1att
117440140227     C                   eval      ORPmtc = v1mtc
117450140227     C                   eval      ORPpag = v1pag
117460140227     C                   eval      ORPksc = v1ksc
117470140227     C                   eval      ORPctr = v1ctr
117480140227     C                   eval      ORPddt = v1ddt
117490140227     C                   movel     v1cpor        ORPpor
117500140227     C                   movel     v1cpoc        ORPpoc
117510140227     C                   eval      ORPno1 = v1not1
117520140227     C                   eval      ORPno2 = v1not2
117530140227     C                   eval      ORPrfa = v1rfa
117540140227     C                   move      v1sto         ORPsto
117550140227     C                   eval      ORPcst = v1cst
117560140227     C                   eval      ORPvcs = v1vcs
117570140227     C                   eval      ORPfcs = v1fcs
117580140227     C                   eval      ORPdfo = v1dfo
117590140227     C                   eval      ORPofo = v1ofo
117600140227     C                   eval      ORPfao = v1fao
117610140227     C                   eval      ORPdst = v1dst
117620140227     C                   eval      ORPnpg = v1npg
117630140227     C                   eval      ORPndc = v1ndc
117640140227     C                   eval      ORPddc = v1ddc
117650140227     C                   eval      ORPstp = v1stp
117660140227     C                   eval      ORPtap = v1tap
117670140227     C                   eval      ORPeti = v1eti
117680140227     C                   eval      ORPspi = v1spi
117690140227     C                   eval      ORPcau = v3cau
117700140227      * Se forzato blocco destinatario devo pulire il relativo flag
117710140227     C                   if        §ormfd = 'S'
117720140227     C                   if        v1ctor <> 'S'
117730140227     C                   clear                   §ormfd
117740140227     C                   else
117750140227     C                   if        v1rsc = *blanks or ds_v1ccrc = *zeros
117760140227     C                   clear                   §ormfd
117770140227     C                   endif
117780140227     C                   endif
117790140227     C                   endif
117800140227      * ORM commissionato
117810140227     c                   Eval      §orcom = v1com
117820140227
117830140227      * in base al tipo orm e a chi paga imposto il campo §orpfb
117840140227      * imposto 'F' se paga mittente per orm singolo o prepagato
117850140227      * imposto 'A' se NON paga mittente
117860140227     c                   Clear                   §orPfb
117870140227     c                   Select
117880140227     c                   When      OrpPag = 'M'
117890140227     c                   If        OrpTor = 'P' or OrpTor = 'S'
117900140227     c                   Eval      §orPfb = 'F'
117910140227     c                   EndIf
117920140227     c                   Other
117930140227     c                   Eval      §orPfb = 'A'
117940140227     c                   EndSl
117950140227
117960140227     C                   eval      ORPflo = dorm01
117970140227
117980140227     c                   Eval      OrpDtt = oggi_aammgg
117990140227      * Mette sempre in positivo i campi delle quantità
118000140227     C                   mllzo     1             ORPorr
118010140227     C                   mllzo     1             ORPncl
118020140227     C                   mllzo     1             ORPpkg
118030140227     C                   mllzo     1             ORPvlm
118040140227     C                   mllzo     1             ORPbnc
118050140227     C                   mllzo     1             ORPblc
118060140227     C                   mllzo     1             ORPatt
118070140227     C                   mllzo     1             ORPmtc
118080161115      /free
118090161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
118100161115         //?ipotetico ritiro da effettuare al mattino
118110161115         IF  Orariestesi;
118120161115           ORPrmp = 'M';
118130161115         ENDIF;
118140161116         //?Se ORM da Internet non lo memorizzo
118150161116         IF  ORMtco = 'I';
118160161116           clear ORPrmp;
118170161116         ENDIF;
118180161115      /end-free
118190161115
118200140227     c                   endsr
118210131011
118220131011      *------------------------------------------------------------------------*
118230131011      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
118240131011      *------------------------------------------------------------------------*
118250131011     c     sr_f24        begsr
118260131011
118270131011     c                   Select
118280131011
118290131011     c                   When      $loop = 2 and $rig =2  or
118300131011     c                             $loop = 3 and $rig =3
118310131011     c                   Movea     rigatf1       vzfd01
118320131011     c                   Z-add     1             $rig
118330131011
118340131011     c                   If        $loop = 2
118350131011     c                   Eval      vzfd02= cf2412
118360131011     c                   Else
118370131011     c                   Eval      vzfd02= cf2413
118380131011     c                   EndIf
118390131011
118400131011     c                   When      $loop = 2 and $rig =1 or
118410131011     c                             $loop = 3 and $rig =1
118420131011     c                   Movea     rigatf2       vzfd01
118430131011     c                   Z-add     2             $rig
118440131011
118450131011     c                   If        $loop = 2
118460131011     c                   Eval      vzfd02 = cf2422
118470131011     c                   Else
118480131011     c                   Eval      vzfd02 = cf2423
118490131011     c                   EndIf
118500131011
118510131011     c                   When      $loop = 3 and $rig =2
118520131011     c                   Movea     rigatf3       vzfd01
118530131011     c                   Z-add     3             $rig
118540131011     c                   Eval      vzfd02 = cf2433
118550131011     c                   EndSl
118560131011
118570131011     c                   EndSr
118580131011
118590131011      *------------------------------------------------------------------------*
118600131011      *  Imposto descrizione tasti funzione
118610131011      *------------------------------------------------------------------------*
118620131011     c     Sr_Tastifun   BegSr
118630140130
118640140130      /free
118650140130       //?Routine per abilitazione tasti funzione riga fissa
118660140130         exsr Abi_fxx;
118670140130      /end-free
118680131011
118690131011      * Conta i caratteri riempiti dalle RigaTf1
118700131011     c                   Clear                   $z
118710131011      * Conta i caratteri riempiti dalle RigaTf2
118720131011     c                   Clear                   $k
118730131011      * Conta i caratteri riempiti dalle RigaTf3
118740131011     c                   Clear                   $j
118750131011      * Conta la posizione del campone da cui partire per impostare
118760131011      * la descrizione del tasto funzione
118770131011     c                   Clear                   $y
118780131011
118790131011      * Stringhe che contengono le descrizioni dei tasti funzione
118800131011     c                   Reset                   rigatf1
118810131011     c                   Reset                   rigatf2
118820131011     c                   Reset                   rigatf3
118830131011
118840131011      * F2 - Dati DPD
118850131011     c                   if        *in02 and not *in10 and
118860131011     c                             *in18 and not *in17
118870131011     c                   reset                   $tf
118880131011     c                   movea     tf02          $tf
118890131011     c                   exsr      rie_$tf
118900131011     c                   endif
118910131011      * F11 - Volume
118920131011     c                   if        not *in10 and not *in26
118930131011     c                   reset                   $tf
118940131011     c                   movea     tf11          $tf
118950131011     c                   exsr      rie_$tf
118960131011     c                   endif
118970131011      * F6 - Conferma
118980131011     c                   if        *in01 or *in02 or *in03
118990131011     c                   reset                   $tf
119000131011     c                   movea     tf06          $tf
119010131011     c                   exsr      rie_$tf
119020131011     c                   endif
119030160406      * F13 - Conferma Prenotazione Ritiro
119040160406     c                   if        *in88
119050160406     c                   reset                   $tf
119060160406     c                   movea     tf13          $tf
119070160406     c                   exsr      rie_$tf
119080160406     c                   endif
119090131011      * F14 - Orari Servizio
119100131108     c                   if        not *in10 and
119110131025     c                             not *in17
119120131011     c                   reset                   $tf
119130131011     c                   movea     tf14          $tf
119140131011     c                   exsr      rie_$tf
119150131011     c                   endif
119160131011      * F20 - Simulazione
119170131011     c                   if        *in01
119180131011     c                   reset                   $tf
119190131011     c                   movea     tf20          $tf
119200131011     c                   exsr      rie_$tf
119210131011     c                   endif
119220131011      * F16 - Chiusura
119230140918     c                   if        *in98
119240131011     c                   reset                   $tf
119250131011     c                   movea     tf16          $tf
119260131011     c                   exsr      rie_$tf
119270131011     c                   endif
119280131011      * F19 - Proposte variazione
119290131011     c                   if        *in57 and not *in10
119300131011     c                   reset                   $tf
119310131011     c                   movea     tf19          $tf
119320131011     c                   exsr      rie_$tf
119330131011     c                   endif
119340131011      * F21 - Dirottamento
119350131011     c                   if        *in19 and not *in10 and
119360131011     c                             not *in26
119370131011     c                   reset                   $tf
119380131011     c                   movea     tf21          $tf
119390131011     c                   exsr      rie_$tf
119400131011     c                   endif
119410131011
119420131011      * Pulisco la stringa
119430131011     c                   Eval      $h = 1
119440131011     c                   For       $h by 1 to 62
119450131011     c                   If        rigatf1($h) = '#'
119460131011     c                   Clear                   rigatf1($h)
119470131011     c                   EndIf
119480131011     c                   EndFor
119490131011     c                   Eval      $h = 1
119500131011     c                   For       $h by 1 to 62
119510131011     c                   If        rigatf2($h) = '#'
119520131011     c                   Clear                   rigatf2($h)
119530131011     c                   EndIf
119540131011     c                   EndFor
119550131011     c                   Eval      $h = 1
119560131011     c                   For       $h by 1 to 62
119570131011     c                   If        rigatf3($h) = '#'
119580131011     c                   Clear                   rigatf3($h)
119590131011     c                   EndIf
119600131011     c                   EndFor
119610131011
119620131011      * Imposto la descrizione del tasto funzione F24 e segnalo
119630131011      * quante righe ho riempito e quale devo emettere
119640131011     c                   Select
119650131011
119660131011     c                   When      $k <> *Zeros and $y <> *Zeros
119670131011     c                   Eval      $loop = 3
119680131011     c                   Eval      vzfd02 = cf2413
119690131011
119700131011     c                   When      $k <> *Zeros and $y = *Zeros
119710131011     c                   Eval      $loop = 2
119720131011     c                   Eval      vzfd02 = cf2412
119730131011
119740131011     c                   Other
119750131011
119760131011     c                   Eval      $loop = 1
119770131011     c                   Clear                   vzfd02
119780131011     c                   EndSl
119790131011
119800131011      * Prima riga di tasti funzione
119810131011     c                   Movea     rigatf1       vzfd01
119820131011     c                   Eval      $rig = 1
119830131212
119840131212      /free
119850131212        *in80 = *off;
119860131212        *in83 = *off;
119870131212       //?Imposto tasto funzione in HI se presente dati
119880131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
119890131216             V1mtc > 0 or V1sto > 0 or V1ffd <> *blanks;
119900131212           *in80 = *on;
119910131212         ENDIF;
119920160513         IF  ((*in01 or *in03) and
119930160513              (w03sms <> *blanks or W03mail <> *blanks)) or
119940160513               walert;
119950131212           *in83 = *on;
119960131212         ENDIF;
119970131212      /end-free
119980131011
119990131011     c                   EndSr
120000140130
120010140130      /free
120020140130       //--------------------------------------------------------------------
120030140130       //?Abilitazione tasti funzione riga fissa.
120040140130       //--------------------------------------------------------------------
120050140130       BEGSR Abi_Fxx;
120060140130
120070140130         *in97 = *off;
120080140130       //?Abilito il tasto F17-Note AUT solo se sono in manutenzione
120090140227       //?e se NON è conferma proposta di variazione
120100140317           IF  *in02 and §RMfpr <> 'P';
120110140204             *in97 = *on;
120120140204           ENDIF;
120130140918
120140140918         *in98 = *off;
120150140918       //?Attivo F16 Chiusura ORM se
120160140918       //?non è copia/immissione ORM
120170140918       //?non è già chiusura ORM
120180140918       //?non siamo in conferma proposta di variazione
120190140918       //?se ORM non è in fase 420
120200140918         IF  not *in10 and not *in03 and not *in01 and not *in81 and
120210140918             not OrmFase420;
120220140918           *in98 = *on;
120230140918         ENDIF;
120240140130
120250140130       ENDSR;
120260140130      /end-free
120270131011
120280131011      *------------------------------------------------------------------------*
120290131011      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
120300131011      *------------------------------------------------------------------------*
120310131011     c     Rie_$tf       BegSr
120320131011      *
120330131011     c                   Eval      $x = 1
120340131011     c     '#'           Lookup    $tf($x)                                30
120350131011     c                   Add       $x            $z
120360131011     c                   If        $Z <= 63
120370131011     c                   Eval      $j = $Z - $x + 1
120380131011     c                   Movea     $tf           rigatf1($j)
120390131011     c                   Else
120400131011     c                   Add       $x            $k
120410131011     c                   If        $k <= 63
120420131011     c                   Eval      $j = $K - $x + 1
120430131011     c                   Movea     $tf           rigatf2($j)
120440131011     c                   Else
120450131011     c                   Add       $x            $y
120460131011     c                   If        $y <= 63
120470131011     c                   Eval      $j = $y - $x + 1
120480131011     c                   Movea     $tf           rigatf3($j)
120490131011     c                   EndIf
120500131011     c                   EndIf
120510131011     c                   EndIf
120520131011
120530131011     c                   EndSr
120540140422
120550140422      /free
120560140422       //--------------------------------------------------------------------
120570140422       //?Controllo se Alert o meno.
120580140422       //--------------------------------------------------------------------
120590140422       BEGSR AlertORM;
120600140422
120610140424         walert = *off;
120620140422         clear §OREfimo;
120630140422         clear §OREfiso;
120640140515
120650140515         exsr VerificaAlert;
120660140422
120670140515       //?Se OK alert, sono in conferma da VAO e
120680140515       //?ci sono i dati dell'alert li controllo
120690140515         IF  wOKalert and §RMfpr = 'C' and
120700140515            (w03mail <> *blanks or w03sms <> *blanks);
120710140424           exsr CtrDatiF04;
120720140424           IF  *in28;
120730140424             leavesr;
120740140424           ENDIF;
120750140424         ENDIF;
120760140422
120770140422       //?Imposto il flag ORM alert via SMS
120780140515         IF  w03sms <> *blanks;
120790140515           IF  not wOKalert;
120800140515             §OREfiso = 'N';
120810140515           ELSE;
120820140515             SELECT;
120830140422         //?Imposto in base al flag VAO alert via SMS
120840140422         //?per 'N' no alert
120850140423             WHEN  §OREfisv = 'N';
120860140423               §OREfiso = 'N';
120870140422         //?per 'X' alert solo se data ritiro > di oggi
120880140423             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
120890140423               §OREfiso = 'S';
120900140423             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
120910140423               §OREfiso = 'N';
120920140422         //?no 'N' e no 'X' alert calcolato
120930140423             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
120940140515               IF  inv_V1dar > oggi_aammgg;
120950140515                 §OREfiso = 'S';
120960140515               ELSE;
120970140515                 §OREfiso = 'N';
120980140515               ENDIF;
120990140515             ENDSL;
121000140515           ENDIF;
121010140422         ENDIF;
121020140422
121030140422       //?Imposto il flag ORM alert via mail
121040140515         IF  w03mail<> *blanks;
121050140515           IF  not wOKalert;
121060140515             §OREfimo = 'N';
121070140515           ELSE;
121080140515             SELECT;
121090140422         //?Imposto in base al flag VAO alert via mail
121100140422         //?per 'N' no alert
121110140507             WHEN  §OREfimv = 'N';
121120140423               §OREfimo = 'N';
121130140422         //?per 'X' alert solo se data ritiro > di oggi
121140140423             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
121150140423               §OREfimo = 'S';
121160140423             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
121170140423               §OREfimo = 'N';
121180140422         //?no 'N' e no 'X' alert calcolato
121190140423             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
121200140515               IF  inv_V1dar > oggi_aammgg;
121210140515                 §OREfimo = 'S';
121220140515               ELSE;
121230140515                 §OREfimo = 'N';
121240140515               ENDIF;
121250140515             ENDSL;
121260140515           ENDIF;
121270140422         ENDIF;
121280140422
121290140422       //?imposto flag per OK alert da mandare
121300140422         IF  §OREfiso = 'S' or §OREfimo = 'S';
121310140422           walert = *on;
121320140422         ENDIF;
121330140422
121340140422       ENDSR;
121350140515
121360140515       //--------------------------------------------------------------------
121370140515       //?Verifico se OK alert.
121380140515       //--------------------------------------------------------------------
121390140515       BEGSR VerificaAlert;
121400140515
121410140515         wOKalert = *off;
121420140515
121430140515         SELECT;
121440140515
121450140515       //?Se mittente codificato e filiale mittente ha ntw estero
121460140515       //?o mittente NON codificato con nazione estera
121470140515       //?NO alert e NON memorizzo i dati su FNORE
121480140515         WHEN  (ds_V1Ccra > 0 and
121490140515               (m_§OGntw = 'DPD' or m_§OGntw = 'EEX' or m_§OGntw = 'FED')) or
121500140515               (ds_V1Ccra = 0 and V1nar <> *blanks);
121510140515           clear w03sms;
121520140515           clear w03mail;
121530140515
121540140515         //?Se ORM NON commissionato NO alert
121550140515         WHEN  V1com <> 'S';
121560140515         //?Se ORM prepagato NO alert
121570140515         WHEN  V1Ctor = 'P';
121580140515         //?Se Ordinante non codificato NO alert
121590140515         WHEN  ds_V1Ccor = *zeros or
121600140515               ds_V1cor2 = 9999 or ds_V1cor2 = 8888;
121610140515         //?Controllo se peso/volume/bancali superano i limit massimi da DFT
121620140515         //?NO alert
121630140515         WHEN  (d§DFTpkga > 0 and V1pkg > d§DFTpkga) or
121640140515               (d§DFTvlma > 0 and V1vlm > d§DFTvlma) or
121650140515               (d§DFTbnca > 0 and V1bnc > d§DFTbnca);
121660140515         OTHER;
121670140515           wOKalert = *on;
121680140515         ENDSL;
121690140515
121700140515       ENDSR;
121710140505
121720140505       //--------------------------------------------------------------------
121730140505       //?Calcolo data ritiro Alert.
121740140505       //--------------------------------------------------------------------
121750140505       BEGSR DataRitiroAlert;
121760140515
121770140515       //?data ritiro video > oggi non faccio niente
121780140515         dataEUR = %date(V1dar:*eur);
121790140515         dataISO = dataeur;
121800140515         wdar = %dec(dataISO);
121810140515         IF  wdar > oggi_aammgg;
121820140515           leavesr;
121830140515         ENDIF;
121840140515
121850140515       //?se non ci sono i presupposti per inviare alert non faccio niente
121860140515         IF  not wOKalert;
121870140515           leavesr;
121880140515         ENDIF;
121890140515
121900140515       //?se i presupposti ci sono allora devo ricalcolare la data ritiro
121910140515       //?ma solo se i flag da VAO non sono 'X' o 'N'
121920140710         IF  (§OREfisv <> 'X' and §OREfisv <> 'N' and w03sms <> *blanks) or
121930140710             (§OREfimv <> 'X' and §OREfimv <> 'N' and w03mail <> *blanks);
121940140519           IF  V1Cpor > *zeros and num_V1Cpor = *zeros;
121950140519             num_v1Cpor = %dec(V1Cpor:3:0);
121960140519           ENDIF;
121970140527         //?se conferma ORM da VAO e NON ho ancora il POR lo prendo da O95lna
121980140529         //?se impostato
121990140527         //?capita perchè c'è un errore nel controllo del cap del mittente
122000140529           IF  §RMfpr = 'C' and num_V1Cpor = 0 and sav_O95lna > 0;
122010140527             num_V1Cpor = sav_O95lna;
122020140527           ENDIF;
122030140529         //?Calcolo la data solo se ho già la filiale ritiro
122040140529           IF  num_V1Cpor > 0;
122050140515           exsr sr_calpikup;
122060140515           v1dar = new_v1dar;
122070140515           dataEUR = %date(V1dar:*eur);
122080140515           dataISO = dataeur;
122090140515           inv_v1dar = %dec(dataISO);
122100140529           ENDIF;
122110140515         ENDIF;
122120140505
122130140505       ENDSR;
122140160406
122150160406       //--------------------------------------------------------------------
122160160406       //?Controllo se Conferma Prenotazione o meno.
122170160406       //--------------------------------------------------------------------
122180160406       BEGSR ConfermaORM;
122190160406
122200160406         clear §OREfmco;
122210160406         clear §OREfsco;
122220160406
122230160406       //?Se sono in conferma da VAO ed è stata richiesta la conferma
122240160406       //?controllo i dati
122250160406         IF  wConferma and §RMfpr = 'C' and
122260160406            (w04mail <> *blanks or w04sms <> *blanks);
122270160406           exsr CtrDatiF04;
122280160406           IF  *in28;
122290160406             leavesr;
122300160406           ENDIF;
122310160406         ENDIF;
122320160406
122330160406       //?Imposto il flag ORM alert via SMS
122340160406         IF  w03sms <> *blanks;
122350160406           IF  not wOKalert;
122360160406             §OREfiso = 'N';
122370160406           ELSE;
122380160406             SELECT;
122390160406         //?Imposto in base al flag VAO alert via SMS
122400160406         //?per 'N' no alert
122410160406             WHEN  §OREfisv = 'N';
122420160406               §OREfiso = 'N';
122430160406         //?per 'X' alert solo se data ritiro > di oggi
122440160406             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
122450160406               §OREfiso = 'S';
122460160406             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
122470160406               §OREfiso = 'N';
122480160406         //?no 'N' e no 'X' alert calcolato
122490160406             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
122500160406               IF  inv_V1dar > oggi_aammgg;
122510160406                 §OREfiso = 'S';
122520160406               ELSE;
122530160406                 §OREfiso = 'N';
122540160406               ENDIF;
122550160406             ENDSL;
122560160406           ENDIF;
122570160406         ENDIF;
122580160406
122590160406       //?Imposto il flag ORM alert via mail
122600160406         IF  w03mail<> *blanks;
122610160406           IF  not wOKalert;
122620160406             §OREfimo = 'N';
122630160406           ELSE;
122640160406             SELECT;
122650160406         //?Imposto in base al flag VAO alert via mail
122660160406         //?per 'N' no alert
122670160406             WHEN  §OREfimv = 'N';
122680160406               §OREfimo = 'N';
122690160406         //?per 'X' alert solo se data ritiro > di oggi
122700160406             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
122710160406               §OREfimo = 'S';
122720160406             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
122730160406               §OREfimo = 'N';
122740160406         //?no 'N' e no 'X' alert calcolato
122750160406             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
122760160406               IF  inv_V1dar > oggi_aammgg;
122770160406                 §OREfimo = 'S';
122780160406               ELSE;
122790160406                 §OREfimo = 'N';
122800160406               ENDIF;
122810160406             ENDSL;
122820160406           ENDIF;
122830160406         ENDIF;
122840160406
122850160406       //?imposto flag per OK alert da mandare
122860160406         IF  §OREfiso = 'S' or §OREfimo = 'S';
122870160406           walert = *on;
122880160406         ENDIF;
122890160406
122900160406       ENDSR;
122910140422
122920140422       //--------------------------------------------------------------------
122930140422       //?Scrivo file spia Alert.
122940140422       //--------------------------------------------------------------------
122950140422       BEGSR wrtspia;
122960140422
122970151008         IF  walert;
122980140422         clear FISAO000;
122990140422         SAOdaori = %timestamp();
123000140422         SAOpoe = ORMpoe;
123010140422         SAOnsr = ORMnsr;
123020140422         SAOnor = ORMnor;
123030140422         SAOnrv = ORMnrv;
123040140422         SAOtipa = 'AVV';
123050140422         SAOsts = '0';
123060140422         write FISAO000;
123070151008         ENDIF;
123080151008
123090151008         IF  wconferma;
123100151021           clear FISAO000;
123110151021           SAOdaori = %timestamp();
123120151021           SAOpoe = ORMpoe;
123130151021           SAOnsr = ORMnsr;
123140151021           SAOnor = ORMnor;
123150151021           SAOnrv = ORMnrv;
123160151021           SAOtipa = 'CON';
123170151021           SAOsts = '0';
123180151021           write FISAO000;
123190151008         ENDIF;
123200140422
123210140422       ENDSR;
123220140506
123230140506       //--------------------------------------------------------------------
123240140506       //?Imposto la frase per NON TELEFONARE
123250140506       //?se ORM commissionato con Alert
123260140506       //--------------------------------------------------------------------
123270140506       BEGSR CommissionatoAlert;
123280140506
123290140506         clear wtel01;
123300140506         clear wpos;
123310140506         wpos = (%len(wtel01) - %len(cCommAlert)) / 2;
123320140506         %subst(wtel01:wpos:%len(cCommAlert)) = cCommAlert;
123330140506
123340140506         clear wtel02;
123350140506         clear wpos;
123360140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123370140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123380140506
123390140506       ENDSR;
123400140506
123410140506       //--------------------------------------------------------------------
123420140506       //?Imposto la frase per NON TELEFONARE
123430140506       //?se ORM NON commissionato dirottato
123440140506       //--------------------------------------------------------------------
123450140506       BEGSR DirottaNoComm;
123460140506
123470140506         clear wtel01;
123480140506         clear wpos;
123490140506         wpos = (%len(wtel01) - %len(cDirottaNoComm)) / 2;
123500140506         %subst(wtel01:wpos:%len(cDirottaNoComm)) = cDirottaNoComm;
123510140506
123520140506         clear wtel02;
123530140506         clear wpos;
123540140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123550140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123560140506
123570140506       ENDSR;
123580140506
123590140506       //--------------------------------------------------------------------
123600140506       //?Imposto la frase per NON TELEFONARE
123610140506       //?se ORM Con preavviso via mail (CEVA) dirottato
123620140506       //--------------------------------------------------------------------
123630140506       BEGSR DirottaPreavviso;
123640140506
123650140506         clear wtel01;
123660140506         clear wpos;
123670140506         wpos = (%len(wtel01) - %len(cDirottaPreavv)) / 2;
123680140506         %subst(wtel01:wpos:%len(cDirottaPreavv)) = cDirottaPreavv;
123690140506
123700140506         clear wtel02;
123710140506         clear wpos;
123720140506         wpos = (%len(wtel02) - %len(cGiaAvvisato)) / 2;
123730140506         %subst(wtel02:wpos:%len(cGiaAvvisato)) = cGiaAvvisato;
123740140506
123750140506       ENDSR;
123760151106
123770160406       //--------------------------------------------------------------------
123780160406       //?Recupero i dati per Conferma Prenotazione da anagrafica clienti.
123790160406       //--------------------------------------------------------------------
123800160406       BEGSR RecDatiConf;
123810160406
123820160406         clear wCodAcre;
123830160406       //?dall'ordinante se presente
123840160406         IF  ds_V1Ccor > 0;
123850160406           wCodAcre = ds_V1Ccor;
123860160406         ELSE;
123870160406         //?dal mittente se ordinante non presente
123880160406           IF  V1rso = *blanks and ds_V1Ccra > 0;
123890160406             wCodAcre = ds_V1Ccra;
123900160406           ENDIF;
123910160406         ENDIF;
123920160406       //?Ho recuperato un codice quindi aggancio l'angrafica
123930160406          IF  wCodAcre = 0;
123940160406            leavesr;
123950160406          ENDIF;
123960160406          clear dACR01;
123970160406          chain(n) (wCodAcre) FNACR01L;
123980160406          IF  %found(FNACR01L);
123990160406            dACR01 = ACRmai;
124000160406          ENDIF;
124010160406         //?Se il cliente ha il flag di invio conferma
124020160406         //?imposto i dati a video
124030160406          IF  §ACRfmar <> 'S';
124040160406            leavesr;
124050160406          ENDIF;
124060160406          IF  w04mail = *blanks;
124070160406            chain(n) (wCodAcre:'MC') FNACRE1L;
124080160406            IF  %found(FNACRE1L);
124090160406              w04mail = ACREdati;
124100160406            ENDIF;
124110160406          ENDIF;
124120160406          IF  w04sms = *blanks;
124130160406            chain(n) (wCodAcre:'SC') FNACRE1L;
124140160406            IF  %found(FNACRE1L);
124150160406              w04sms = ACREdati;
124160160406            ENDIF;
124170160406          ENDIF;
124180160406
124190160406       ENDSR;
124200160406
124210151106       //--------------------------------------------------------------------
124220151106       //?Aggiorno Anagrafica clienti ritiro Ordinante x mail conferma.
124230151106       //--------------------------------------------------------------------
124240151106       BEGSR aggACR;
124250160505
124260160505       //?Se ORM con Ordinante YOOX non aggiorno
124270160505         IF  ORMcor = 17732000;
124280160505           leavesr;
124290160505         ENDIF;
124300151106
124310151109         chain(e) ORMcor FNACR01L;
124320161118         IF  not  %error;
124330151106         IF  %found(FNACR01L);
124340151109           dACR01 = ACRmai;
124350160322           IF  §OREfmco = 'S' or §OREfsco = 'S';
124360160322             §ACRfmar = 'S';
124370160322           ELSE;
124380151109             clear §ACRfmar;
124390160322           ENDIF;
124400151109           ACRmai = dACR01;
124410151109           update FNACR000;
124420151106         ENDIF;
124430161118         ENDIF;
124440151109
124450151109         chain(e) (ORMcor:'MC') FNACRE1L;
124460161118         IF  not %error;
124470151109         IF  %found(FNACRE1L);
124480160322           IF  sav_mailconf <> *blanks and §ACRfmar = 'S';
124490151109             ACREdati = sav_mailconf;
124500151109             update FNACRE00;
124510151109           ELSE;
124520151109             delete FNACRE00;
124530151109           ENDIF;
124540151109         ENDIF;
124550160112         IF  not %found(FNACRE1L) and sav_mailconf <> *blanks and
124560160322             §ACRfmar = 'S';
124570151109           clear FNACRE00;
124580151109           ACREcro = ORMcor;
124590151109           ACREtrc = 'MC';
124600151109           ACREdati = sav_mailconf;
124610151109           write FNACRE00;
124620151109         ENDIF;
124630161118         ENDIF;
124640160318
124650160318         chain(e) (ORMcor:'SC') FNACRE1L;
124660161118         IF  not %error;
124670160318         IF  %found(FNACRE1L);
124680160322           IF  sav_smsconf <> *blanks and §ACRfmar = 'S';
124690160318             ACREdati = sav_smsconf;
124700160318             update FNACRE00;
124710160318           ELSE;
124720160318             delete FNACRE00;
124730160318           ENDIF;
124740160318         ENDIF;
124750160318         IF  not %found(FNACRE1L) and sav_smsconf <> *blanks and
124760160322             §ACRfmar = 'S';
124770160318           clear FNACRE00;
124780160318           ACREcro = ORMcor;
124790160318           ACREtrc = 'SC';
124800160318           ACREdati = sav_smsconf;
124810160318           write FNACRE00;
124820160318         ENDIF;
124830161118         ENDIF;
124840160712
124850161118       //?Cerco il codice utente da memorizzare su FNACRE
124860160712         clear ORIidc;
124870160712         IF  §ORMpg > *zeros;
124880160712           ORIprg = %dec(§ORMpg:7:0);
124890160712           chain (ORIprg) TIORI01L;
124900160712         ENDIF;
124910161118
124920161118       //?Memorizzo chi ha fatto la richiesta
124930161118         clear dOREana;
124940161118         §OREtipo = 'I';
124950161118         §OREjob = §OREorip;
124960161118         §OREute = ORIidc;
124970161118         §OREdtv = %dec(%date());
124980161118         §OREorv = %dec(%time());
124990161118         chain(e) (ORMcor:'AA') FNACRE1L;
125000161118         IF  not %error;
125010161118           IF  %found(FNACRE1L);
125020161118             ACREdati = dOREana;
125030161118             update FNACRE00;
125040161118           ENDIF;
125050161118           IF  not %found(FNACRE1L);
125060161118             clear FNACRE00;
125070161118             ACREcro = ORMcor;
125080161118             ACREtrc = 'AA';
125090161118             ACREdati = dOREana;
125100161118             write FNACRE00;
125110161118           ENDIF;
125120161118         ENDIF;
125130151106
125140151106       ENDSR;
125150161115
125160161115       //--------------------------------------------------------------------
125170161115       //?Imposto i dati necessari alla visualizzazione orari servizio.
125180161115       //--------------------------------------------------------------------
125190161115       BEGSR DatiPerOrari;
125200161115
125210161115       //?Imposto la data per calcolo data ritiro = oggi
125220161115         DataCalcolo = Oggi_aammgg;
125230161115       //?Se data pronta merce > oggi la data calcolo è la data pronta merce
125240161115         IF  inv_V1dpm > 0 and inv_V1dpm > Oggi_aammgg;
125250161115           DataCalcolo = inv_V1dpm;
125260161115         ENDIF;
125270161115       //?Cerco giorno della settimana della data calcolo data ritiro
125280161115         dataiso = %date(DataCalcolo);
125290161115         clear wDay_Of_Week;
125300161115         exec sql
125310161115         set :wDay_Of_Week = dayofweek_iso(:dataiso);
125320161115
125330161115       //?Aggancio la tabella GPD con filiale ritiro ORM
125340161115         clear TIBS02ds;
125350161115         clear dGPD;
125360161115         T02mod = 'C';
125370161115         T02cod = 'GPD';
125380161115         T02sif = KNSIF;
125390161115         T02ke1 = %editc(num_V1cpor:'X');
125400161115         TNTBE_RicercaControllo (kpjba : tibs02ds);
125410161115         IF  T02err = *blanks;
125420161115           dGPD = T02uni;
125430161115         ELSE;
125440161115         //?Non trovo GPD con filiale ritiro
125450161115         //?la cerco con filiale generica 999
125460161115           clear TIBS02ds;
125470161115           T02mod = 'C';
125480161115           T02cod = 'GPD';
125490161115           T02sif = KNSIF;
125500161115           T02ke1 = '999';
125510161115           TNTBE_RicercaControllo (kpjba : tibs02ds);
125520161115           IF  T02err = *blanks;
125530161115             dGPD = T02uni;
125540161115           ENDIF;
125550161115         ENDIF;
125560161115
125570161115       ENDSR;
125580140422
125590140422      /end-free
125600131011
125610001010      *****************************************************************
125620001010      * ROUTINE INIZIALE
125630001010      *****************************************************************
125640001010     C     *INZSR        BEGSR
125650001009
125660001010     C     *ENTRY        PLIST
125670001010     C                   PARM                    KPJBA
125680001023     C                   PARM                    FIOR05DS
125690001011
125700001011     C                   movel     TIT_A         VTCtit
125710001009
125720150306     c                   IF        kpjbu <> *blanks
125730001026     C                   movel(p)  kpjbu         parm01
125740150306     c                   ELSE
125750150306     c                   clear                   parm01
125760150306     c                   ENDIF
125770001016
125780060203     C                   Z-ADD     1             CODUT             1 0
125790060203
125800060203      * Reperisco dati job
125810150713     c                   IF        §RMfpr <> 'K'
125820060203     c                   exsr      DatiJob
125830150713     c                   open      FIOR05D
125840150713     c                   ENDIF
125850150713
125860150713      /free
125870160713
125880160713       //?Determino la libreria
125890160713         IF  %subst(knsif : 7 : 1) = 'P';
125900160713           wLib = 'GAITRAGRPS';
125910160713         ELSE;
125920160713           wLib = 'GAITRAGRU';
125930160713         ENDIF;
125940160713       //?Apro i file
125950160713         wFLib = %trim(wLib) + '/TIORI01L';
125960160713         open  TIORI01L;
125970160713
125980150713       //?Se richiamato da pgm in ascolto (conferma automatica ORM da file/internet)
125990150713         IF  §RMfpr = 'K';
126000150713       //?imposto DUTpou = filiale emissione
126010150713           DUTpou = §RMpoe;
126020150713       //?carico la SKpog con la filiale emissione
126030150713           SKpog(1) = %editc(§RMpoe:'X');
126040150713       //?il flag di tipo richiamo diventa 'C'
126050150713           §RMfpr = 'C';
126060150713       //?Conferm automatica
126070150713           ConfAuto = *on;
126080150713       //?Utente batch
126090150713           knmus = 'BATCH';
126100150713         ENDIF;
126110150713      /end-free
126120001018
126130041014      * salvo il n. di distinta quando è proposta di chiusura
126140041014     c                   If        §rmtla = 'C' and §rmfpr = 'P' and
126150041014     c                             pndc > *Zeros
126160041014     c                   Eval      savndc = pndc
126170041014     c                   Eval      savddc = pddc
126180060516     c                   Eval      savfgs = pfvvfgs
126190041014     c                   EndIf
126200150304      * imposto il n. ORM quando è proposta variazione
126210001106     C                   if        §RMfpr = 'P'
126220001106     C                   eval      ppoe = §RMpoe
126230001106     C                   eval      pnsr = §RMnsr
126240001106     C                   eval      pnor = §RMnor
126250001106     C                   eval      pnrv = §RMnrv
126260001106     C                   endif
126270110420
126280110420      /free
126290110420       //?Imposto flag per arrivo da conferma proposta di variazione
126300110420       *in81 = *off;
126310110420       IF  §rmfpr = 'P';
126320110420         *in81 = *on;
126330110420       ENDIF;
126340110420      /end-free
126350001023
126360150304     C                   movel     DUTpou        kazorg
126370150310     C                   clear                   network
126380150612     C                   clear                   og143
126390001019     C     kazorg        chain     azorg01L
126400001016     C                   if        %found(azorg01l)
126410150612     C                   movel     orgde3        OG143
126420150612     c                   eval      network = §OGntw
126430001016     C                   endif
126440150609      * Carico £6
126450150609     C                   CLEAR                   trul06ds
126460150609     C                   MOVE      '£6'          D06COD
126470150609     C                   MOVEL     DUTpou        D06KEY
126480150609     C                   MOVEL     trul06ds      KPJBU
126490150609     C                   CALL      'TRUL06R'
126500150609     C                   PARM                    KPJBA
126510150609     C                   MOVEL     KPJBU         trul06ds
126520150609     C                   MOVEA     LIN           L6
126530061023
126540150305      * carico sk dei p.o. DPD
126550061023     c                   clear                   xx
126560061023     c     *loval        setll     azorg01l
126570061023     c                   do        *hival
126580061023     c                   read      azorg01l
126590061023     c                   if        %eof(azorg01l)
126600061023     c                   leave
126610061023     c                   endif
126620061023     c                   if        orgfva <> *blanks  or
126630061023     c                             (orgfag <> 'F' and orgfag <> 'A')
126640061023     c                   iter
126650061023     c                   endif
126660061023     c                   eval      og143 = orgde3
126670061023     c                   if        §ogntw = 'DPD'
126680061023     c                   add       1             xx
126690061023     c                   eval      skpodpd(xx) = orgfil
126700061023     c                   endif
126710061023     c                   enddo
126720081126
126730081126      * tabella 3I x DPD
126740081126     c                   clear                   ds3idp
126750081126     c                   eval      kcod = '3I'
126760081126     c                   eval      kkey = 'DPD'
126770081126     c     ktab          chain     tabel00f
126780081126     c                   if        %found(tabel00f)
126790081126     c                             and tblflg = *blanks
126800081126     c                   eval      ds3idp = tbluni
126810081126     c                   endif
126820001009
126830001023      * reperisce data e ora
126840001010     C                   TIME                    W0140
126850001010      * UDATE IN GGMMAAAA
126860010116     C                   MOVE      W0140         oggi_ggmmaa
126870001010      * UDATE IN AAAAMMGG
126880010124     C                   Z-ADD     oggi_ggmmaa   G02DAT
126890001010     C                   MOVEL     *BLANK        G02ERR
126900001010     C                   CALL      'XSRDA8'
126910001215     C                   PARM                    WLBDAT
126920010116     C                   Z-ADD     G02INV        oggi_aammgg
126930010907
126940010907     C                   movel     oggi_aammgg   oggi_anno
126950010907     C                   add       1             oggi_anno
126960001016
126970001009     C     Kfnor         klist
126980001011     C                   kfld                    kpoe
126990001016     C                   kfld                    knsr
127000001009     C                   kfld                    knor
127010001016     C                   kfld                    knrv
127020001109
127030001109     C     Kfnor1        klist
127040001109     C                   kfld                    kpoe
127050001109     C                   kfld                    knsr
127060001109     C                   kfld                    knor
127070001018
127080001026     C     Kfnorn        klist
127090001018     C                   kfld                    kpoe
127100001018     C                   kfld                    knsr
127110001018     C                   kfld                    knor
127120001018     C                   kfld                    knrv
127130001026     C                   kfld                    kdai
127140001026     C                   kfld                    kori
127150001018     C                   kfld                    kfar
127160001018     C                   kfld                    kprg
127170001026
127180001026     C     Kfnorn1       klist
127190001026     C                   kfld                    kpoe
127200001026     C                   kfld                    knsr
127210001026     C                   kfld                    knor
127220001026     C                   kfld                    knrv
127230001026     C                   kfld                    kdai
127240001026     C                   kfld                    kori
127250001026     C                   kfld                    kfar
127260070806
127270070806     c     kors          klist
127280070806     c                   kfld                    v1cpos
127290070806     c                   kfld                    v1ors
127300001009
127310010118     C     Ktab          klist
127320010119     C                   kfld                    codut
127330010118     C                   kfld                    kcod
127340010118     C                   kfld                    kkey
127350001010
127360001010     C     Ktntam        klist
127370001010     C                   kfld                    kksc
127380001010     C                   kfld                    kctr
127390010122
127400010122     C     Ktnta1        klist
127410010122     C                   kfld                    kksc
127420010122     C                   kfld                    kctr
127430010122     C                   kfld                    kprg
127440001215
127450001215     C     kazcln        klist
127460001215     C                   kfld                    ktfp
127470001215     C                   kfld                    ktfa
127480001215     C                   kfld                    kann
127490001215     C                   kfld                    kmes
127500001009
127510040426     c     kOrm08        klist
127520040426     c                   kfld                    ds_v1ccra
127530040426     c                   kfld                    inv_v1dar
127540040930
127550040930     c     kCnaco        klist
127560040930     c                   kfld                    codut
127570040930     c                   kfld                    kci
127580040930     c                   kfld                    kksc
127590070123
127600070301     c     kfidst        klist
127610070301     c                   kfld                    kdstnpg
127620080226     c                   kfld                    kdstnfv
127630080226     c                   kfld                    kdstfgs
127640071023
127650071023     c     kacr1         klist
127660071023     c                   kfld                    ds_v1ccra
127670071023     c                   kfld                    num_v1cpor
127680071031     c                   kfld                    kacr1ain
127690090206
127700090206     c     kfnora        klist
127710090206     c                   kfld                    ormpoe
127720090206     c                   kfld                    ormnsr
127730090206     c                   kfld                    ormnor
127740090206     c                   kfld                    ormnrv
127750090206     c                   kfld                    ormfao
127760090206     c                   kfld                    ormdfo
127770090206     c                   kfld                    ormofo
127780150615
127790150615     c                   eval      *in04 = *off
127800040426
127810001009     C                   select
127820010124      * Immissione
127830141212     C                   when      psce = '1' or psce = 'E'
127840001009     C                   movel     mod(1)        v1mod
127850001009     C                   eval      *in01 = *on
127860150615     c                   IF        §RMtla = *blanks
127870150615     c                   eval      *in04 = *on
127880150615     c                   ENDIF
127890010124      * Manutenzione
127900001009     C                   when      psce = '2'
127910001009     C                   movel     mod(2)        v1mod
127920001009     C                   eval      *in02 = *on
127930010124      * Copia
127940001109     C                   when      psce = '3'
127950001109     C                   movel     mod(1)        v1mod
127960010124     C                   eval      *in03 = *on
127970010124      * Conferma
127980010124     C                   when      psce = 'C'
127990090211     C                   movel     mod(7)        v1mod
128000010124     C                   eval      *in02 = *on
128010010124      * Richiamato x immissione
128020001025     C                   when      §RMtla <> *blanks
128030010124     C                             and §RMfpr <> 'P' and §RMfpr <> 'O'
128040001023     C                   movel     mod(1)        v1mod
128050001025     C                   eval      *in01 = *on
128060010124      * Richiamato x manutenzione
128070001106     C                   when      §RMtla <> *blanks
128080001106     C                             and §RMfpr = 'P'
128090001106     C                   movel     mod(2)        v1mod
128100001106     C                   eval      *in02 = *on
128110001009     C                   endsl
128120001009
128130001011     C                   eval      kpoe = ppoe
128140001016     C                   eval      knsr = pnsr
128150001011     C                   eval      knor = pnor
128160001016     C                   eval      knrv = pnrv
128170001011
128180001025     C                   eval      v1cpoe = ppoe
128190001016     C                   eval      v1nsr  = pnsr
128200001025     C                   eval      v1nor  = pnor
128210001025     C                   eval      v1nrv  = pnrv
128220150305
128230150305      * Recupero i dati di default prima con la filiale emissione
128240150305     C                   clear                   ddft
128250150305     C                   clear                   TIBS02DS
128260150305     C                   movel     'C'           T02mod
128270150305     C                   movel     knsif         t02sif
128280150305     C                   movel     'DFT'         t02cod
128290150305     C                   movel(p)  v1cpoe        T02ke1
128300150305     C                   call      'TIBS02R'
128310150305     C                   parm                    KPJBA
128320150305     C                   parm                    TIBS02DS
128330150305     C                   if        t02err =  *blanks
128340150305     C                   movel     t02uni        ddft
128350150305     C                   else
128360150305      * se non tropo provo con 999 generico
128370150305     C                   clear                   TIBS02DS
128380150305     C                   movel     'C'           T02mod
128390150305     C                   movel     knsif         t02sif
128400150305     C                   movel     'DFT'         t02cod
128410150305     C                   movel(p)  '999'         T02ke1
128420150305     C                   call      'TIBS02R'
128430150305     C                   parm                    KPJBA
128440150305     C                   parm                    TIBS02DS
128450150305     C                   if        t02err =  *blanks
128460150305     C                   movel     t02uni        ddft
128470150305     C                   endif
128480150305     C                   endif
128490150305
128500150305      * se i dati relativi al peso e volume sono a zero li imposto a di dft io
128510150305      * caso che non dovrebbe capitare ma se capita nella routine 'pesvol' il
128520150305      * programma si spacca
128530150305     c                   if        d§dftpkg = *zeros
128540150305     c                   eval      d§dftpkg = 200
128550150305     c                   endif
128560150305     c                   if        d§dftbnc = *zeros
128570150305     c                   eval      d§dftbnc = 1
128580150305     c                   endif
128590140318
128600140318      /free
128610140422       //?Carico i Tipi rcd FNORE che NON sono da copiare pari pari da VAOE
128620140422         clear dORE;
128630140422         clear xx;
128640140422         kTBEcod = 'ORE';
128650140422         setll kTBEcod tntbe01l;
128660140422         reade kTBEcod tntbe01l;
128670140422         DOW not %eof(tntbe01l);
128680140422           dORE = TBEUni;
128690140422           IF  §OREvar = 'S';
128700140422             xx += 1;
128710140423             skTRCvao(xx) = %subst(TBEke1:1:2);
128720140422           ENDIF;
128730140422           reade kTBEcod tntbe01l;
128740140422         ENDDO;
128750131022      /end-free
128760001006
128770001006     C                   endsr
128780060203
128790060203      *---------------------------------------------------------------*
128800060203      * Reperimento Dati del job (Utente/Operativi)                   *
128810060203      *---------------------------------------------------------------*
128820060203     c     DatiJob       BEGSR
128830060203      *
128840060203     c     *dtaara       define    §azute        azuteds
128850060203     c     *dtaara       define    §datiute      ddatiute
128860060203      *
128870060203     c                   in(E)     *dtaara
128880060203     c                   IF        %ERROR or RSUT = *blanks
128890060203     c                   clear                   Tibs34Ds
128900060203     c                   call      'TIBS34R'
128910060203     c                   parm                    Tibs34Ds
128920060203     c                   in        *dtaara
128930060203     c                   ENDIF
128940150305
128950150305      * abilitazione utente
128960150305     c                   clear                   wabi
128970150305     c                   eval      dUTE01 = UTEfaf
128980150305     c                   SELECT
128990150305      * carica abilitazioni del profilo
129000150305     c                   WHEN      §UTEorm <> *blanks
129010150305     c                   eval      wabi = §UTEorm
129020150305     c                   WHEN      UTEaut <> *blanks
129030150305     c                   eval      wabi = UTEaut
129040150305      * se non c'è l'abilitazione
129050150305      * --> se 1° livello, abilitazioni al terminal
129060150305      *     se 2° livello, abilitazioni al punto operativo
129070150305      *     se sede no abilitazioni
129080150305     c                   OTHER
129090150305     c                   IF        DUTlpo = '1'
129100150305     c                   eval      wabi   = 'TP'
129110150305     c                   ENDIF
129120150305     c                   IF        DUTlpo = '2'
129130150305     c                   eval      wabi   = 'PO'
129140150305     c                   ENDIF
129150150305     c                   ENDSL
129160150305
129170150305      * Reperimento delle filiali gestibili dall'utente
129180150305     c                   clear                   TRUL31DS
129190150305     c                   eval      I31abi = wabi
129200150305     c                   eval      I31cdi = DUTdis
129210150305     c                   eval      I31car = DUTare
129220150305     c                   eval      I31cpo = DUTpou
129230150305     c                   call      'TRUL31R'
129240150305     c                   parm                    KPJBA
129250150305     c                   parm                    TRUL31DS
129260150305     c                   IF        O31pog > *zeros
129270150305     c                   movea     O31pog        SKpog
129280150305     c                   ENDIF
129290060203      *
129300060203     c                   ENDSR
129310001010      *---------------------------------------------------------------------------------------------
129320001010** MSG  Lungh. 78                                                            *
129330010420Inserire mittente                                                                   1
129340010420Codice mittente non valido                                                          2
129350010213Ordinante non valido                                                                3
129360001010Immettere i colli                                                                   4
129370001010Immettere il peso                                                                   5
129380001010Immettere il volume                                                                 6
129390001010Immettere i bancali                                                                 7
129400001010Immettere il bilico                                                                 8
129410001010Immettere l'autotreno                                                               9
129420001010Immettere la motrice                                                                10
129430070418Inserire Filiale di ritiro                                                          11
129440131016Ora pronta merce maggiore dell'orario di fine servizio                              12
129450001010Tipo ordine non valido                                                              13
129460010202Immettere la cusale di chiusura                                                     14
129470010202Causale chiusura errata                                                             15
129480001010Codice tassazione o codice tariffa errati                                           16
129490070418Filiale consegna errata                                                             17
129500010118Destinatario non valido                                                             18
129510001010Attenzione !! Modificata Località                                                   19
129520001010Attenzione !! Modificato CAP                                                        20
129530001010Attenzione !! Modificata Provincia                                                  21
129540001010Attenzione !! Modificata Nazione                                                    22
129550030609Pagamento a carico dell'ordinante...inserire un codice valido                       23
129560001012Ora errata                                                                          24
129570001012Immettere i colli ------> ENTER x forzare                                           25
129580001012Immettere il peso ------> ENTER x forzare                                           26
129590001012Immettere il volume ------> ENTER x forzare                                         27
129600001012Immettere i bancali ------> ENTER x forzare                                         28
129610001012Immettere il bilico ------> ENTER x forzare                                         29
129620001012Immettere l'autotreno ------> ENTER x forzare                                       30
129630001012Immettere la motrice ------> ENTER x forzare                                        31
129640131016Ritiro no in giornata, superato l'orario di fine servizio                           32
129650070418Filiale ritiro errata                                                               33
129660001016Immettere data ritiro                                                               34
129670001016Data ritiro errata                                                                  35
129680070418La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica        36
129690010205Immettere almeno una quantità (Kg. Volume Bancali)                                  37
129700131107ORM multiplo non possibile se presente il destinatario                              38
129710010118Dati del destinatario incompleti                                                    39
129720010420Non è possibile variare il mittente                                                 40
129730070130ATTENZIONE!! Già avuto esito da PDA. Enter x forzare                                41
129740001103Non è stato modificato nulla                                                        42
129750070913Giro Ritiro errato                                                                  43
129760001215Data chiamata errata                                                                44
129770001215Data chiamata obbligatoria                                                          45
129780131107Orari di apertura incongruenti                                                      46
129790001215Ora chiamata obbligatoria                                                           47
129800010213ORM prepagato non possibile, inserire prima il prepagato!                           48
129810081007ORM IMPORT DPD automatico, non si può chiudere con causale "80"                     49
129820010223Immettere destinatario se ORM prepagato                                             50
129830161108Data ritiro immessa inferiore a quella calcolata.                                   51
129840010126Immettere l'ora del ritiro                                                          52
129850010202Priorità non trovata in tabella                                                     53
129860070913Per ORM prepagato immettere colli e peso                                            54
129870140424Immettere un indirizzo e-mail valido                                                55
129880010420Per ORM prepagato deve pagare il mittente                                           56
129890010215Inserire il referente                                                               57
129900010215Inserire il telefono                                                                58
129910040630Non immettere il codice tariffa se non immesso il codice cliente                    59
129920140424Immettere un numero di telefono valido                                              60
129930010223Immettere il destinatario. Orm singolo con pagamento a carico destinatario          61
129940070418ATTENZIONE!! Filiale ritiro diversa da quella calcolata                             62
129950010906ATTENZIONE!! data ritiro ricalcolata!                                               63
129960030806Causale chiusura non gestibile                                                      64
129970010305Data ritiro inferiore alla data immissione ORM                                      65
129980170303ATTENZIONE!! La data ritiro immessa è un giorno festivo                             66
129990010413ATTENZIONE!! fare F6 x confermare                                                   67
130000010906ATTENZIONE!! Volume troppo alto. Ricalcolare.                                       68
130010010502ATTENZIONE!! Mittente BLOCCATO                                                      69
130020070921ATTENZIONE!! Modificato codice mittente e/o codice cliente tariffa                  70
130030010507ATTENZIONE!! Numeratore ORM impegnato riprovare                                     71
130040010508ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                            72
130050140424Messaggi in sospeso su altre videate                                                73
130060011114Richiesta forzatura del destinatario ma l'ORM non è singolo                         74
130070011114Richiesta forzatura del destinatario ma i dati sono incompleti o errati             75
130080030808Se Mittente e Ordinante sono uguali impostare che paga il Mittente                  76
130090040419Non è possibile dirottare l'ORM                                                     77
130100070418Modificare la filiale ritiro                                                        78
130110070418La filiale ritiro non ha la proc.ORM attiva                                         79
130120040628ORM xxx-xx-xxxxxxx-xx da ritirare lo stesso giorno per lo stesso mittente           80
130130131202ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!        81
130140040727ATTENZIONE!! Ordinante BLOCCATO                                                     82
130150040727ATTENZIONE!! Destinatario BLOCCATO                                                  83
130160050120Pagamento a carico del destinatario...inserire un codice valido                     84
130170050128ATTENZIONE!! Le note immesse sono state sovrascritte                                85
130180050323ATTENZIONE!! Non esiste legame tra il mittente e l'ordinante. VERIFICARE!!!         86
130190050324ATTENZIONE!! ORM PREPAGATO non locale deve essere un ORM commissionato              87
130200050420Prima confermare le proposte di variazione                                          88
130210070418ATTENZIONE!! Filiale ritiro non gestibile                                           89
130220080422ATTENZIONE!! Per ORM commissionati a DPD il ritiro deve essere di 1 collo           90
130230140331Immettere la natura della merce.                                                    91
130240081126ATTENZIONE!! ORM Export DPD peso maggiore del limite consentito                     92
130250081126ATTENZIONE!! ORM Export DPD destinatario obbligatorio                               93
130260110120ATTENZIONE!! Presenti incongurenze fra Cap, Prov, Loc: modificare l'anagrafica      94
130270110610Per ORM Export EEX non è possibile la consegna all'estero                           95
130280110610Per ORM Export DPD non è possibile la consegna a                                    96
130290150615Tipo Comunicazione ORM non valido.                                                  97
130300160329ATTENZIONE!! Il giro è stato variato a seguito delle modifiche effettuate           98
130310010508** MOD  Lungh. 15                                                            *
130320001013  IMMISSIONE                                                                        1
130330001013 MANUTENZIONE                                                                       2
130340001009VISUALIZZAZIONE                                                                     3
130350001013   ANNULLATO                                                                        4
130360001106   CHIUSURA                                                                         5
130370040628 DIROTTAMENTO                                                                       6
130380090211   CONFERMA                                                                         7
130390010704** CM3
130400030611OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('ETICH     ')
130410160229 ovrscope(*calllvl) SHARE(*YES)
130420030623** cmA4
130430030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130440030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130450030623**  cmA5
130460030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130470030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130480131011** TF02
130490131211F2=Dati DPD  #
130500131014** TF06
130510131211F6=Conferma  #
130520131011** TF11
130530131211F11=Volume  #
130540160405** TF13
130550160405F13=Conf.Prenotazione  #
130560131011** TF14
130570131211F14=OrariSr  #
130580131011** TF16
130590131211F16=Chiusura  #
130600131011** TF19
130610131211F19=Proposte  #
130620131011** TF20
130630131211F20=Simulazione  #
130640131011** TF21
130650131211F21=Dirottamento  #
