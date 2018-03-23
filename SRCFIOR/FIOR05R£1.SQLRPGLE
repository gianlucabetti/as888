000100050302     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000110160224     h dftactgrp(*no) bnddir('UBBNDDIR': 'TIO7')
000120001006      *****************************************************************
000130001006      *                                                               *
000140130320      *     
 IMMISSIONE ORM MANUALI ?                                *
000150001006      *                                                               *
000160001006      *****************************************************************
000170001103
000180010208      *  ATTENZIONE: località cliente ordinante e destinatario a
000190010208      *              video sono lunghe 20 nel file 35
000200010115
000210010115      *              i campi che non sono gestiti a video sono hidden
000220001006
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
007000100219     d Tisie3ds      e ds
007010100222     d Tisie8ds      e ds
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
008760100219     d tisie3r         pr                  extpgm('TISIE3R')
008770100219     d  tisie3ds                           likeds(TISIE3ds)
008780100222
008790100222      // -Interrogazione cappario DPD
008800100222     d tisie8r         pr                  extpgm('TISIE8R')
008810100222     d  kpjba                              likeds(KPJBA)
008820100222     d  tisie8ds                           likeds(TISIE8ds)
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
011190150305     C                   if        *in01
011200010208     C                   exsr      Sr_Calcolaoggi
011210010208     C                   endif
011220030606
011230030606      * Se sto riemettendo la videata xchè ORM commissionato prima devo
011240030606      * impostare l'ordinante in base a quello che è stato impostato
011250030606      * nella videata
011260030606If  2c                   If        whocor <> *Blanks
011270030610Sel 3c                   Select
011280030606      * l'ordinante è il mittente
011290030610     c                   When      wdcor = 'M'
011300030606     c                   Eval      v1cor1 = v1cra1
011310030606     c                   Eval      v1cor2 = v1cra2
011320030606     c                   Eval      v1cor3 = v1cra3
011330030606     c                   Eval      v1rso = v1rsr
011340030606     c                   Eval      v1ino = v1inr
011350030606     c                   Eval      v1cao = v1car
011360030606     c                   Eval      v1loo = v1lor
011370030922     c                   Eval      sav_v1loo35 = v1lor
011380030606     c                   Eval      v1pro = v1prr
011390030606     c                   Eval      v1nao = v1nar
011400131106      /free
011410131106       //?se ordinante = mittente l'ORM è NON commissionato
011420131106         V1com = 'N';
011430131112       //?mi devo posizionare sulle note
011440131112         *in82 = *on;
011450131106      /end-free
011460030606      * l'ordinante è il destinatario
011470030610     c                   When      wdcor = 'D'
011480030609     c                   Eval      v1cor1 = v1crc1
011490030609     c                   Eval      v1cor2 = v1crc2
011500030609     c                   Eval      v1cor3 = v1crc3
011510030609     c                   Eval      v1rso = v1rsc
011520030609     c                   Eval      v1ino = v1inc
011530030609     c                   Eval      v1cao = v1cac
011540030609     c                   Eval      v1loo = v1loc
011550030922     c                   Eval      sav_v1loo35 = sav_v1loc35
011560030609     c                   Eval      v1pro = v1prc
011570030609     c                   Eval      v1nao = v1nac
011580131112      /free
011590131112       //?mi devo posizionare sulle note se ok il destinatario
011600131112         IF  V1rso <> *blanks;
011610131112           *in82 = *on;
011620131112         ELSE;
011630131112       //?mi posiziono sul destinatario così lo possono immettere
011640131112           *in56 = *on;
011650131112         ENDIF;
011660131112      /end-free
011670030610     c                   Other
011680030606     c                   Eval      *In42 = *On
011690030610    3c                   EndSl
011700030606     c                   Clear                   whocor
011710030610    2c                   EndIf
011720030609      * se sto riemettendo la videata x ritorno da immissione bolla e ho il 28 acceso
011730030609      * lo devo spegnere
011740030609     c                   If        V1com = 'S' and Okbolla = '1' and *In28
011750030609     c                   Eval      *In28 = *Off
011760030609     c                   EndIf
011770050502
011780050502      * Prima di andare avanti con i controlli della videata
011790050502      * se conferma ORM commissionati e ORM con ricevuta di ritiro
011800050502      * devo prima emettere una finestra con un promemoria
011810050502     c                   If        *In11 and §rmfpr = 'O' and
011820050502     c                             wricevuta = *Off
011830090211     C                   write     fior051
011840050502Do  2c                   Do        *Hival
011850050502     c                   Exfmt     Fior05r
011860050502     c                   Eval      wricevuta = *On
011870050502If  3C                   If        *Inkl
011880050502     c                   Leave
011890050502    3c                   EndIf
011900050502    2c                   EndDo
011910050502     c                   EndIf
011920070925
011930070925      * emetto Window per avvisare se mittente e/o qtà diverse tra proposta ORM e
011940070925      * dati memorizzati nell'ORM nel caso di giro impostato a video
011950070925     c                   if        *in25 and v1ccgi <> *blanks
011960090211     C                   write     fior051
011970070925Do  2c                   do        *hival
011980070925     c                   exfmt     fior05orp
011990070925     c                   eval      *in25 = *off
012000070925     c   kl              leave
012010070925    2c                   enddo
012020070925     c                   endif
012030100223
012040100223      /free
012050100223       //?se errore relativo al preavviso
012060100223       //?devo emettere una win per la richiesta dei dati corretti
012070100223         DOW  $errpre;
012080100223           exsr sr_DatiPre;
012090100223         ENDDO;
012100140506
012110140506       //?se conferma ORM commissionato con alert
012120140506       //?di un orm che in conferma automatica ha avuto un errore
012130140506       //?informo l'utente che non deve telefonare
012140140506         IF  §RMfpr = 'O' and walert and not wNOTEL;
012150140506           *in28 = *off;
012160140506           write FIOR051;
012170140506           exsr CommissionatoAlert;
012180140506Do  2      DOW  not *inkl;
012190140506             exfmt  FIOR05WTEL;
012200140506           ENDDO;
012210140506           wNOTEL = *on;
012220140506         ENDIF;
012230100812
012240130919       //?se conferma ORM commissionato e dirottato
012250130919       //?per ORM con prevviso via mail o per ORM NON COMMISSIONATI
012260130919       //?informo l'utente che non deve telefonare
012270130919       //?mi posiziono qua tanto se dirottato sono già stati fatti i controlli
012280130919       //?relativi al prevviso
012290150206       //?NON faccio niente se ORM estero/DPD
012300131212         IF  §RMfpr = 'O' and $NoApp and not wNOTEL and
012310150206            (§ORMpre = 'M' or V1com = 'N') and
012320150206            not *in15 and not *in14;
012330130919             SELECT;
012340130919               WHEN  §ORMpre = 'M';
012350140506                 exsr DirottaPreavviso;
012360130919               WHEN  V1com = 'N';
012370140506                 exsr DirottaNoComm;
012380130919             ENDSL;
012390140506           write FIOR051;
012400140506Do  2      DOW  not *inkl;
012410140506             exfmt  FIOR05WTEL;
012420100812           ENDDO;
012430130918           wNOTEL = *on;
012440100812         ENDIF;
012450131030
012460160406       //?se devo riemettere la videata ed ho errore in una delle 3 WIN
012470160406       //?emetto il messaggio di errore in sospeso su altre videate
012480160405         IF  wErrF04 or wErrF05 or wErrF13;
012490131030           *in28 = *on;
012500140424           V1Cmsg = msg(73);
012510131030         ENDIF;
012520140423
012530160406       //?se errore dovuto a controllo Alert emetto win per sistemare i dati
012540160406       //?il controllo è fatto al momento del F6
012550140423         IF  wErrAlert;
012560140423           exsr F04D01;
012570140423         ENDIF;
012580160406
012590160406       //?se errore dovuto a controllo dati Conferma Prenotazione
012600160406       //?il controllo è fatto nella sr_contr richiamata dopo il
012610160406       //?caricamento dati da VAS
012620160406       //?emetto la win per sistemarli
012630160406         IF  wErrConferma;
012640160406           exsr F13D01;
012650160406         ENDIF;
012660130918
012670100223      /end-free
012680001010
012690071107     ?* emetto la videata
012700071107      *                  __________________
012710090211     C                   exfmt     fior051
012720071107      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
012730071108     C                   setoff                                       28  90
012740131030     c                   eval      wErrF04 = *off
012750131030     c                   eval      wErrF05 = *off
012760160405     c                   eval      wErrF13 = *off
012770131030     c                   clear                   v1cmsg
012780131011
012790131011      /free
012800131014       //?F4=Dati Avviso Ritiro
012810131014       IF  *inkd;
012820131202         exsr sr_contr;
012830131202         IF  *in28 or *in90;
012840131202           iter;
012850131202         ENDIF;
012860131014         exsr F04D01;
012870140509         IF  not wCalDtRit;
012880140509           iter;
012890140509         ENDIF;
012900131014       ENDIF;
012910131011
012920131011       //?F5=Altri dati
012930131011       IF  *inke;
012940131210         exsr sr_contr;
012950131202         IF  *in28 or *in90;
012960131202           iter;
012970131202         ENDIF;
012980131011         exsr F05D01;
012990131011         iter;
013000131011       ENDIF;
013010161021
013020161027       //?- PageDown --> devo calcolare 1 gg in meno nella data ritiro
013030161027       //?- PageUp   --> devo calcolare 1 gg in più nella data ritiro
013040161027         IF  dsp_aid = c_PageDown or
013050161027             dsp_aid = c_PageUp;
013060161026           exsr AddSubUnoDar;
013070161021         ENDIF;
013080131011      /end-free
013090001011
013100001011      * F6=Conferma
013110001025     C                   if        *inkf = *on
013120010202     ?* Controlla ?
013130081126     C                   exsr      Sr_Contr
013140030610     C   28
013150030610     Cor 90              iter
013160131030
013170131030      /free
013180160405       //?Controllo anche i dati della videata F04 e F05 e F13
013190160316       //?F04 Alert
013200160316       //?solo se sono in immissione o copia, negli altri casi non serve.
013210160406       //?o in conferma ORM commissionati
013220160316         IF  (*in01 or *in03 or §RMfpr = 'O');
013230150305         //?F04 Alert
013240140429             wErrF04 = *off;
013250140429             exsr CtrDatiF04;
013260140429             IF  *in28;
013270140429               wErrF04 = *on;
013280160316           //?se conferma ORM commissionati
013290160316           //?per In12 acceso lo spengo (il 12 protegge i campi della win)
013300160406           //?in questo modo do la possibilità di variare i dati sbagliati
013310160316               IF  *in12 and §RMfpr = 'O' and §RMtla = '2';
013320160316                 *in12 = *off;
013330160316               ENDIF;
013340140429               iter;
013350140429             ENDIF;
013360160316         ENDIF;
013370160316         //?F05 Altri dati
013380160316         //?solo se NON sono in chiusura ORM
013390160316         IF  not *in10;
013400140429           wErrF05 = *off;
013410140429           exsr CtrDatiF05;
013420140429           IF  *in28;
013430140429             wErrF05 = *on;
013440140429             iter;
013450140429           ENDIF;
013460131216         ENDIF;
013470160405       //?Se ORM Commissionato ed ho emesso la WIN per avere i dati dell'Alert
013480160405       //?ma i dati non sono stati inseriti devo riemettere la videata
013490160405         IF  V1com = 'S' and wRicAlert and
013500160405             w03sms = *blanks and w03mail = *blanks and not wOKF04;
013510140711           exsr F04D01;
013520160316           wOKF04 = *on;
013530140711           iter;
013540140711         ENDIF;
013550160405       //?F13 Conferma Prenotazione Ritiro
013560160406       //?solo se sono in immissione o copia, negli altri casi non serve.
013570160405         IF  *in01 or *in03;
013580160405         //?F13 Conferma Prenotazione Ritiro
013590160405           wErrF13 = *off;
013600160405           exsr CtrDatiF13;
013610160405           IF  *in28;
013620160405             wErrF13 = *on;
013630160405             iter;
013640160405           ENDIF;
013650160405         ENDIF;
013660160406       //?Se ho emesso in precedenza la WIN per avere i dati relativi alla Conferma
013670160406       //?prenotazione ma i dati non sono stati inseriti devo riemettere la WIN
013680160406       //?questo capita solo in immissione manuale con mittene non codificato
013690160406         IF  wRicConf and
013700160406             W04sms = *blanks and W04mail = *blanks and not wOKF13;
013710160406           exsr F13D01;
013720160406           wOKF13 = *on;
013730160406           iter;
013740160406         ENDIF;
013750131030      /end-free
013760030606
013770010507      * Capita che ci sono degli orm con po ritiro a zero e di conseguenza il po di
013780010507      * trasmissione e in questo caso va in errore la trasmissione
013790010507      * così aggiungo anche qua il controllo
013800010507     C                   if        v1cpor = *blanks or v1cpor = '000'
013810010507     C                   movel     msg(11)       v1cmsg
013820010507     C                   seton                                        59  28
013830010507     C                   iter
013840010507     C                   endif
013850010507      * Se il ksc è cambiato devo riemettere la videata
013860010906      * può capitare se variano il codice di chi paga o il flag chi paga
013870010906      * e poi danno subito F6 senza enter
013880070920      * stessa cosa per il mittente è variato devo riemettere la videata
013890070920      * è importante per il giro
013900070920     C                   if        (v1ksc <> sav_v1ksc and
013910070920     C                              sav_v1ksc <> *zeros) or
013920070920     c                             (ds_v1ccra <> sav_v1ccra and
013930070920     c                              sav_v1ccra <> *zeros)
013940070921     c                   eval      v1cmsg = msg(70)
013950070920     C                   eval      sav_v1ksc = v1ksc
013960070919     c                   eval      sav_v1ccra = ds_v1ccra
013970070920     c                   eval      *in28 = *on
013980070919     c                   iter
013990070919     c                   endif
014000010301      * Se Chiusura orm e note obbligatorie richiamo il programma
014010010301     C                   if        d§cmrnot = 'S' and *in10 = *on
014020030731     C                             and (v1fao = 999 or v1fao = 910)
014030010301     C                   do        *hival
014040010301     C                   exsr      sr_note
014050010301     C                   if        o06f03 = 'S' or o06f12 = 'S'
014060010301     C                   iter
014070010301     C                   endif
014080010301     C                   if        o06dai <> v1dfo
014090010301     C                   iter
014100010301     C                   endif
014110010301     C                   if        o06ori < v1ofo
014120010301     C                   iter
014130010301     C                   endif
014140010301     C                   if        o06prg = 0
014150010301     C                   iter
014160010301     C                   endif
014170010301     C                   leave
014180010301     C                   enddo
014190010301     C                   endif
014200040402      * se è stata modificata la data di ritiro devo richiedere le note
014210040402      * obbligatorie
014220040405if  1c                   If        Ormdar <> Inv_V1Dar and *In02
014230040914      * e non è un cambio di p.o.
014240040914     c                             and num_v1cpor = ormpor
014250050420      * e non è una comferma di una proposta
014260050420     c                             and §rmfpr <> 'P'
014270040402do  2c                   Do        *Hival
014280040402     c                   ExSr      Sr_Note
014290040402     C                   if        o06f03 = 'S' or o06f12 = 'S'
014300040402     c                             and i06flm = 'D'
014310040402     C                   iter
014320040402     C                   endif
014330040402     C                   movel     w0140         I06ori
014340040402     C                   if        o06dai <> I06dai and i06flm = 'D'
014350040402     C                   iter
014360040402     C                   endif
014370040402     C                   if        o06ori < i06ori and i06flm = 'D'
014380040402     C                   iter
014390040402     C                   endif
014400040402     C                   if        o06prg = 0 and i06flm = 'D'
014410040402     C                   iter
014420040402     C                   endif
014430040402     C                   leave
014440040402e   2c                   EndDo
014450040402e   1c                   EndIf
014460090205      * se conferma ORM commissionati prima di confermare il ritiro devo emettere
014470090205      * un window per richiedere i dati dell'appuntamento
014480090206      * non lo faccio se sto dirottando l'ORM
014490090407      * e se ORM export
014500130918      * e se l'orm è già stato dirottato prima ed ho avvisato l'utente di NON telefonare
014510140429      * e se per l'orm sono previsti gli alert
014520090407     c                   eval      wokapp = *off
014530090206     c                   if        §rmfpr = 'O' and not *in29
014540090407     c                             and not *in14 and not *in15
014550130918     c                             and not wNOTEL
014560140429     c                             and not walert
014570090205     c                   exsr      sr_ormapp
014580090205      * se non è stato confermato l'appuntamento (F6) devo riemettere la videata
014590090205      * dell'ORM
014600090205     c   90              iter
014610090407      * imposto flag per ok info inserite
014620090407     c                   eval      wokapp = *on
014630090205     c                   endif
014640010202     ?* Confermo ?
014650001103     C                   exsr      Sr_Conferma
014660030611     C   28
014670030611     Cor 90              iter
014680010117      * Orm già stampato si vuole ristampare?
014690150305      * solo se il p.o. ritiro è in gestione all'utente
014700080225      * e se non ho fatto una proposta di variazione
014710010117     C                   if        v1dst > *zeros  and
014720150305     C                             d§farstp = 'S' and
014730150305     c                             %lookup(%editc(num_v1cpor:'X'):skpog) > 0
014740080225     c                             and $proposte = *off
014750001108     C                   exsr      Sr_Contrstp
014760001108     C                   endif
014770010124     C                   if        §RMtla = *blanks and *in01 = *on
014780010123     C                   goto      inizio
014790010124     C                   else
014800010124     C                   leave
014810010124     C                   endif
014820001011     C                   endif
014830001009
014840001009      * F7=Int.Cod.Ritiro
014850001026     C   kg              exsr      Sr_RicRitiro
014860001106     C   90              iter
014870001009
014880001009      * F8=Int.Cod.Ordinante
014890001011     C   kh              exsr      Sr_RicOrdin
014900001106     C   90              iter
014910001009
014920011113      * F9=Int.Cod.Destinatario
014930001011     C   ki              exsr      Sr_RicCons
014940001106     C   90              iter
014950001222
014960001222      * F11=Calcola volume
014970001222     C                   if        *inkk = *on
014980001222     C                   exsr      Sr_Volume
014990001222     C                   iter
015000001222     C                   endif
015010001011
015020001011      * F12=Ritorno
015030001023     C                   if        *inkl = *on
015040010209      * Cambiato idea dopo F16 di chiusura
015050050322     C                   if        *in10 = *on and (§RMtla = *blanks
015060050322     c                             or §rmfpr = 'O')
015070001107     C                             and *in02 = *on
015080001109     C                   eval      *in10 = *off
015090010302     C                   clear                   dcmr
015100001107     C                   movel     mod(2)        v1mod
015110090211     c                   if        §rmfpr = 'O'
015120090211     c                   eval      v1mod = mod(7)
015130090211     c                   endif
015140010116     C                   eval      v1fao = sav_v1fao
015150010116     C                   eval      v1dfo = sav_v1dfo
015160010116     C                   eval      v1ofo = sav_v1ofo
015170010202     C                   exsr      Sr_Contrfao
015180010202     C                   clear                   v3cau
015190010202     C                   clear                   sav_v1fao
015200010202     C                   clear                   sav_v1dfo
015210010202     C                   clear                   sav_v1ofo
015220131025      /free
015230131025       //?Imposto la riga dei tasti funzione
015240131025       ExSr Sr_Tastifun;
015250131025      /end-free
015260001107     C                   iter
015270001107     C                   endif
015280010307      * Se sto confermando un orm pepagato commissionato non posso uscire
015290010307     C                   if        v1ctor = 'P' and §rmfpr = 'O'
015300010307     C                   eval      *in28 = *on
015310010307     C                   movel     msg(67)       v1cmsg
015320010307     C                   iter
015330010307     C                   endif
015340010117
015350001026     C                   unlock    fnorm01l
015360160902     C                   unlock    fnorg01l
015370010202     C                   eval      §RMerr = '3'
015380001023     C                   leave
015390001023     C                   endif
015400100222
015410100222      /free
015420160406       //?F13=Conferma Prenotazione
015430160405       IF  *inkm;
015440160405         exsr sr_contr;
015450160405         IF  *in28 or *in90;
015460160405           iter;
015470160405         ENDIF;
015480160405         exsr F13D01;
015490160405         iter;
015500160405       ENDIF;
015510160405
015520130912       //?F14=Interrogazione oraro di servizio
015530130912       IF  *inkn;
015540130913         exsr F14D01;
015550130912         iter;
015560130912       ENDIF;
015570130912
015580100222       //?F15=Interrogazione cappario DPD
015590100907       IF  *inkp and *in79;
015600100222       //?Se è network DPD ok interrogo e passo cap fittizio
015610131030         IF  v1ntwd <> *blanks or ntw_V1Cpor = 'DPD';
015620100907           TBLkey = v1nar;
015630100222           chain (CodUt : '15' : TBLkey ) TABEL;
015640100222           IF  %found( TABEL00F );
015650100222             ds15 = TBLuni;
015660100222           ENDIF;
015670100907           %subst(wcap:(§15pcf):(%len(v1car)-§15pcf)) =
015680100907             %subst(v1car:(§15pcf):(%len(v1car)-§15pcf));
015690100907           %subst(wcap:(§15pcf+§15ecf):(%len(v1car)-§15ecf)) = *blanks;
015700100222           clear tisie8ds;
015710100222           isie8dri = oggi_aammgg;
015720100222           isie8op0 = 'D01';
015730100907           isie8nar = v1nar;
015740100222           isie8cap = wcap;
015750100222           tisie8r (kpjba : tisie8ds);
015760100222       //?Per errore riemetto la videata con errore
015770100222           IF  osie8err <> *blanks;
015780100222             *in28 = *on;
015790100907             *in50 = *on;
015800100222             *in62 = *on;
015810100222             v1cmsg = osie8msg;
015820100222             iter;
015830100222           ENDIF;
015840100222       //?Selezionato un cap lo imposto e riemetto la videata
015850100222           IF  osie8capb <> *blanks;
015860100907             v1car = osie8capb;
015870100907             *in50 = *on;
015880100907             *in79 = *off;
015890100222             *in62 = *on;
015900100222             iter;
015910100222           ENDIF;
015920100222       //?Se non è stato selezionato un cap riemetto la videata
015930100222           IF  osie8capb = *blanks;
015940100907             *in50 = *on;
015950100222             *in62 = *on;
015960100222             iter;
015970100222           ENDIF;
015980100222         ENDIF;
015990100222       //?Se non è più network DPD spengo indicatore di comodo
016000131030         IF  v1ntwd = *blanks or ntw_V1Cpor <> 'DPD';
016010100222           *in79 = *off;
016020100222         ENDIF;
016030100222       ENDIF;
016040100222      /end-free
016050010202
016060040908      * F16=Chiusura
016070001020     C                   if        *inkq = *on and *in01 = *off
016080010202     C                   exsr      Sr_Chiusura
016090010202     C                   if        *in10 = *on
016100010202     C                   movel     mod(5)        v1mod
016110010202     C                   endif
016120010202     C                   iter
016130010202     C                   endif
016140140130
016150140130      /free
016160140130       //?F17=Note AUT
016170140130       IF  *inkr;
016180140130         exsr F17D01;
016190140130         iter;
016200140130       ENDIF;
016210140130      /end-free
016220010202
016230001025      * F18=Note
016240001025     C                   if        *inks = *on
016250001025     C                   exsr      Sr_Note
016260001106     C                   iter
016270001025     C                   endif
016280010202
016290001020      * F19=Proposte
016300001020     C                   if        *inkt = *on and *in57 = *on
016310001103     C                   exsr      Sr_Proposte
016320001106     C                   iter
016330001020     C                   endif
016340010202
016350001102      * F20=Simulazione
016360001116     C                   if        *inku = *on
016370001102     C                   exsr      Sr_Simula
016380001106     C                   iter
016390001102     C                   endif
016400061023
016410061023      * F2=Dati DPD
016420061023     c                   if        *inkb
016430061023     c                   exsr      sr_datidpd
016440061023     c                   endif
016450131011      * F24=Altri tasti  ?
016460131011     c                   if        *inky and $loop > 1
016470131011     c                   exsr      sr_f24
016480131014     c                   iter
016490131011     c                   endif
016500001010
016510001011      * Controlli
016520010202     C                   if        *in28 = *off
016530081126     C                   exsr      Sr_Contr
016540001020     C                   endif
016550010202
016560010202      * Ritorna ad emetter la videata se ci sono errori o se sono state fatte delle ricerche
016570001010     C   28
016580001010     Cor 90              iter
016590040420      * F21=Dirottamento
016600040420      * lo faccio dopo i controlli xchè nella videata del dirottamento mi servono dei dati
016610040420      * che vengono generati nella routine sr_contr
016620040420     c                   If        *InKv = *on
016630050420      * prima di dare l'ok al dirottamento però devo controllare che non ci siano delle proposte
016640050420      * di variazione ancora da elaborare
016650050420      * in questo caso blocco il dirottamento xchè prima sono da elaborare le proposte di
016660050420      * variazione altrimenti poi vengono fuori dei problemi con il p.o. ritiro
016670050420     c                   ExSr      Sr_ContrProp
016680050420  b6 c                   If        wproposte = *On
016690050420     c                   Eval      *In28 = *On
016700050420     c                   Eval      v1cmsg = msg(88)
016710050420     c                   Iter
016720050420     c                   EndIf
016730040420      * per dirottare l'ORM
016740040420      *  ---> se ha la serie deve essere abilitato da tabella OSR
016750040420     c                   If        d§Osrdir <> *Blanks
016760040420     c                   Eval      *In28 = *On
016770040420     c                   Eval      v1cmsg = msg(77)
016780040420     c                   Iter
016790040420     c                   EndIf
016800040420     c                   ExSr      Sr_Dirotta
016810040628      * se ok il dirottamento aggiorno l'ORM
016820040628     c                   If        wokdirotta = *On
016830040628     c                   Eval      v1dpor = wddpor
016840040628     c                   Eval      v1mod = mod(6)
016850040628     c                   Eval      *In29 = *On
016860070924      * prima di riemettere se devo imposto il giro dal cliente
016870070924     c                   if        ds_v1ccra <> *zeros
016880070924     c                   movel     v1cpor        num_v1cpor
016890071031      * recupero la tabella AGR per il nuovo po ritiro
016900071031     c                   exsr      sr_caragr
016910071031      * recupero anche il nuovo giro
016920070924     c                   exsr      sr_carcgi
016930071023     c                   if        sav_acrcgi <> *blanks
016940070924     c                   eval      v1tgi = 'A'
016950070924     c                   eval      v1ccgi = sav_acrcgi
016960070924     c                   eval      sav_v1ccgi = sav_acrcgi
016970070924     c                   else
016980070924     c                   clear                   v1tgi
016990070924     c                   clear                   v1ccgi
017000070924     c                   clear                   sav_v1ccgi
017010070924     c                   endif
017020070924     c                   exsr      ctrgiro
017030070924     c                   if        d09ocgi <> ' '
017040070924     c                   eval      v1dcgi = d09odes
017050070924     c                   endif
017060070924     c                   endif
017070040628     c                   EndIf
017080040420     c                   EndIf
017090001010
017100001006     C                   enddo
017110001023
017120001023     C     endsrciclo    tag
017130001026
017140001106      * Se richiamato e allocato ritorna al pgm chiamante con codice di ritorno
017150010117     C                   if        §RMfpr = 'P' and *in28 = *on
017160001106     C                   eval      §RMerr = '3'
017170001106     C                   endif
017180001106
017190030922     C                   movel(p)  kpjbu         §kpjbu
017200030922
017210150713     c                   IF        not ConfAuto
017220001026     C                   clear                   fior06DS
017230001026     C                   movel     'C'           i06tla
017240010405     C                   movel(p)  parm01        kpjbu
017250001026     C                   call      'FIOR06R'
017260001026     C                   parm                    kpjba
017270001027     C                   parm                    fior06ds
017280150713     c                   ENDIF
017290030922
017300030922     c                   clear                   TRUL33DS
017310030922     c                   eval      I33tla = 'C'
017320030922     c                   movel(p)  TRUL33DS      KPJBU
017330030922     c                   call      'TRUL33R'
017340030922     c                   parm                    KPJBA
017350031125
017360031201      * E' meglio lasciare l'eventuale chiusura del prtf laser degli
017370031201      *   ORM al pgm. chiamante, perchè altrimenti verrebbe stampato
017380031201      *   1 ORM ogni pagina per 4 ORM
017390040302     c                   clear                   FIOR16ds
017400040302     c                   eval      R16tla = 'C'
017410040302     c                   movel(p)  FIOR16DS      KPJBU
017420090202     c                   call      §5Ppgm
017430040302     c                   parm                    KPJBA
017440080812
017450080812      * chiudo FNLV12
017460080812     c                   clear                   fnlv12ds
017470111206     c                   clear                   tisi95ds1
017480080812     c                   clear                   tisi97ds
017490080812     c                   eval      i95tla = 'C'
017500080812     c                   call      'FNLV12R'
017510080812     c                   parm                    fnlv12ds
017520111206     c                   parm                    tisi95ds1
017530080812     c                   parm                    tisi97ds
017540030922
017550030922     C                   movel     §kpjbu        kpjbu
017560001010
017570001006     C                   eval      *inlr = *on
017580010115      **********************************************************************
017590010115      * MANUTENZIONE - COPIA
017600010115      **********************************************************************
017610010115     C     Sr_Aggiorna   begsr
017620060726
017630060726     c                   eval      wormconf = *off
017640091007     c                   eval      wormnoman = *off
017650100812     c                   eval      $NoApp = *off
017660130918     c                   eval      wNOTEL = *off
017670140508      /free
017680140512       //?Richiamo la routine delle abilitazioni ai tasti funzione
017690140512       //?mi serve in questo punto perchè nella routina uso gli indicatori
017700140512       //?che vengono accesi o spenti nella abifxx
017710140512         exsr Abi_fxx;
017720140508       //?Accendo indicatore per proteggere i campi alert su F04
017730160208       //?non lo faccio se sono in copia
017740160405       //?anche per F13
017750160208         IF  not *in03;
017760160208           *in12 = *on;
017770160311           *in20 = *on;
017780160208         ENDIF;
017790140508      /end-free
017800010115
017810010115     C     kfnor         chain(e)  fnorm01L
017820010117      * Allocato
017830010115     C                   if        %error
017840160516     c                   IF        §RMtla <> '0'
017850010115     C                   exsr      Sr_Allocato
017860160516     c                   ENDIF
017870010115     C                   eval      *in28 = *on
017880010522     C                   eval      *in33 = *on
017890010115     C                   goto      endsrmanut
017900010115     C                   endif
017910010117      * Non trovato
017920010117     C                   if        not%found(fnorm01l)
017930010117     C                   eval      *in28 = *on
017940010117     C                   goto      endsrmanut
017950010117     C                   endif
017960071030      * alloco anche fnorg
017970071030     c     kfnor         chain(e)  fnorg01L
017980071030      * Allocato
017990071030     c                   if        %error
018000160516     c                   IF        §RMtla <> '0'
018010071030     c                   exsr      Sr_Allocato
018020160516     c                   ENDIF
018030071030     c                   eval      *in28 = *on
018040071030     c                   eval      *in33 = *on
018050071030     c                   goto      endsrmanut
018060071030     c                   endif
018070151016      * Non trovato
018080071030     c                   if        not%found(fnorg01l)
018090151016     C                   eval      *in28 = *on
018100151016     C                   goto      endsrmanut
018110151016     C                   endif
018120140918      /free
018130140918       //?Imposto se ORM in fase 420
018140140918         IF  ORMfao = 420;
018150140918           OrmFase420 = *on;
018160140918         ELSE;
018170140918           OrmFase420 = *off;
018180140918         ENDIF;
018190140918      /end-free
018200130117
018210130117      * se ORM con serie cerco la tabella OSR
018220130117     c                   IF        ORmnsr > 0
018230130117     c                   exsr      sr_tabOSR
018240130117     c                   ENDIF
018250071030
018260060726      * se sono in conferma ORM commissionati e l'ORM non è più in fase 50 non devo confermarlo
018270060726      * esco dalla routine e non confermo l'ORM
018280060726     c                   if        §rmfpr = 'O' and ormfao <> 50
018290160516     c                   IF        §RMtla <> '0'
018300060726     c                   exsr      sr_ormconf
018310160516     c                   ENDIF
018320060726     c                   eval      *in28 = *on
018330060726     c                   eval      wormconf = *on
018340060726     c                   goto      endsrmanut
018350060726     c                   endif
018360091007      * controllo se la fase dell'ORM permette la manutenzione
018370091007     c                   clear                   dfar
018380091007     c                   clear                   tibs02ds
018390091007     c                   eval      t02mod = 'C'
018400091007     c                   eval      t02sif = knsif
018410091007     c                   eval      t02cod = 'FAR'
018420091007     c                   eval      t02ke1 = %editc(ormfao:'X')
018430091007     c                   call      'TIBS02R'
018440091007     c                   parm                    kpjba
018450091007     c                   parm                    tibs02ds
018460091007     c                   eval      dfar = t02uni
018470091007     c                   if        d§farman <> 'S' and *in02
018480091007     c                   eval      wormnoman = *on
018490091007     c                   eval      *in28 = *on
018500091007     c                   goto      endsrmanut
018510091007     c                   endif
018520081127      * se ORM Import DPD automatico non posso manutenzionare
018530081127      * gli unici campi possibili sono il mittente, la data e ora ritiro
018540081127      * quindi emetto un video diverso
018550081127     c                   if        %lookup(ormpoe:skpodpd) > *zeros and
018560081106     c                             ormtco = 'F'
018570081106     c                   eval      *in26 = *on
018580081106     c                   endif
018590010228      * Annullato
018600010228     C                   if        ORMatb <> *blanks
018610010228     C                   eval      *in02 = *off
018620010228     C                   eval      *in10 = *on
018630010228     C                   movel     mod(4)        v1mod
018640010228     C                   endif
018650010213      * Chiuso
018660030731     C                   if        ORMfao = 999 and OrmFao = 910
018670030731     C                                          and *in03 = *off
018680010222     C                   setoff                                       02
018690010213     C                   eval      *in10 = *on
018700010213     C                   movel     mod(5)        v1mod
018710010309     C                   endif
018720010309      * Cerco la causale
018730010305     C                   exsr      Sr_Contrcau
018740010115      * Se richiamato da gestione proposte carica i dati dalla ds esterna
018750010117     C                   if        §RMfpr = 'P'
018760010115     C                   exsr      Sr_Cards
018770070925      * e controllo se devo emettere prima una window per avvisare (nel caso di giro
018780070925      * immesso) se mittente e/o quantità modificate
018790070925     c                   exsr      sr_controrp
018800161108      /free
018810161108       //?Calcolo il peso/volume se no in caso di conferma proposta si azzerano
018820161108        exsr sr_pesvol;
018830161108      /end-free
018840010117     C                   goto      endsrmanut
018850010117     C                   endif
018860010117      * altrimenti carica i dati dal file
018870010115     C                   exsr      Sr_Carvid
018880010117      * e controlla se esistono proposte di variazione
018890010117     C                   exsr      Sr_Orp
018900010117      * Se copia ricerca l'ultimo viaggio dell'ORM
018910010115     C                   if        *in03 = *on
018920010115     C                   exsr      Sr_Copia
018930010117      * Controlla se ok la ricerca dell'ultimo viaggio
018940010115     C   28              exsr      Sr_ErrCopia
018950010115     C   28              goto      endsrmanut
018960010115     C                   endif
018970010124      * Controlla dati caricati se richiamto per conferma da altri p.o.
018980010124     C                   if        §RMfpr = 'O'
018990010124     C                   exsr      Sr_Contr
019000010124      * Richiesta la conferma totale dal pgm chiamante
019010010124     C                   if        §RMtla = '0'
019020010124      * se ci sono errori ritorna al pgm chiamante
019030010124     C   28              eval      §RMerr = '5'
019040010124     C   28              goto      endsrmanut
019050010124      * se non ci sono errori conferma orm
019060010124     C                   exsr      Sr_Conferma
019070010507     C   28              eval      §RMerr = '5'
019080010507     C   28              goto      endsrmanut
019090010124     C                   endif
019100010124      * Richiesta la conferma senza visualizzare l'orm
019110010124      * conferma solo se non ci sono errori, altrimenti visualizza l'orm con gli errori
019120010124     C                   if        §RMtla = '1' and *in28 = *off
019130010124     C                   exsr      Sr_Conferma
019140010124     C                   endif
019150100812      /free
019160140506       //?Se ORM con Alert e richiesta §RMtla = '2'
019170140506       //?vuol dire che la conferma automatica dell'orm è andata in errore
019180170206       //?e ora sto facendo la conferma manuale
019190170206       //?oppure mentre sto facendo le conferme arrivano degli ORM nuovi da confermare
019200170206       //?ma ormai la conferma automatica è già stata fatta quindi il pgm me li propone
019210140506         IF  §RMtla = '2' and walert;
019220140506         //?se ci sono errori dovuti ai controlli esco dalla routine ed emetto la videata ORM
019230140506           IF  *in28;
019240140508           //?spengo indicatore di errore su F04 potrebbero essere errati proprio quelli
019250140508             *in12 = *off;
019260140506             leavesr;
019270140506           ENDIF;
019280140506         //?se non ci sono errori procedo con la conferma dell'ORM in questo modo
019290140506         //?se l'errore è sulla generazione della bolla viene emessa solo la videta
019300140506         //?della bolla
019310140506           exsr sr_conferma;
019320140506           IF  *in28;
019330140506             §RMerr = '5';
019340140506             leavesr;
019350140506           ENDIF;
019360170206         //?se arrivo qua vuol dire che sono riuscita a confemare l'ORM
019370170206         //?quindi devo uscira dal FIOR05R per poi confermare un altro ORM
019380170206         //?per fare questo forzo §RMTLA =  '0'
019390170206           §RMtla = '0';
019400170206           leavesr;
019410140506         ENDIF;
019420140506
019430130918       //?Controllo se ORM ha già avuto un dirottamento
019440100812       //?mi posiziono qua tanto in conferma orm commissionati il richiamo è sempre
019450100812       //?§RMtla = '2'
019460100812       //?cerco la fase precedente con data ora della fase attuale meno un secondo e fase 200
019470100812       //?se la trovo vuol dire che è sto confermando un ORM appena dirottato
019480100812       //?la chiave della fase attuale già impsotata in routine contrcau
019490100812           chain(n)  (kpoe:knsr:knor:knrv:kdai:kori-1:200) FNORF01L;
019500100812           IF  %found(FNORF01L);
019510100812         //?msg che il mittente è già stato avvisato via MAIL non telefonare x appuntamento
019520100812             $NoApp = *on;
019530100812           ENDIF;
019540100812      /end-free
019550010124     C                   endif
019560010115
019570010115     C     endsrmanut    endsr
019580010115      **********************************************************************
019590010115      * IMMISSIONE
019600010115      **********************************************************************
019610010115     C     Sr_Immetti    begsr
019620010115
019630071108     c                   clear                   fnorm000
019640071108     c                   clear                   fnorg000
019650100225     c                   eval      $immetti = *off
019660071108
019670010117      * Carica i dati di default
019680010115     C                   exsr      Sr_Cardft
019690160406      /free
019700160406       //?Abilito F13-Conferma Prenotazione
019710160413       //?Se immissione ORM manuale o copia
019720160413         IF  (*in01 or *in03) and (V1Ctco = 'M' or V1Ctco = 'E');
019730160406           *in88 = *on;
019740160406         ENDIF;
019750160406      /end-free
019760010117      * Se non richiamato da procedura esterna esce dalla routine
019770010117     C                   if        §RMtla = *blanks
019780010117     C                   goto      endsrimm
019790010115     C                   endif
019800010117      * Carica i dati dalla ds esterna
019810010202     C                   exsr      Sr_Cards
019820170525      * Ricalcolo la data
019830170525     C                   exsr      Sr_Calcolaoggi
019840010117      * Controlla dati caricati
019850010117     C                   exsr      Sr_Contr
019860100223      /free
019870100223       //?Controlli per preavviso ORM in caso di conferma ORM da VAO
019880100223         $errpre = *off;
019890100223         IF  §rmfpr = 'C';
019900100225           $immetti = *on;
019910100223           exsr sr_preavviso;
019920100322           IF  $errpre;
019930100225         //?imposto i dati per la window di modifica dati preavviso
019940100225             IF  §ormpre = 'M';
019950100225               wpresino = 'SI';
019960100225             ELSE;
019970100225               wpresino = 'NO';
019980100225             ENDIF;
019990100225             wpremail = vaoedati;
020000100225           ENDIF;
020010100223         ENDIF;
020020100223      /end-free
020030100223
020040010117      * Richiesta la conferma totale dal pgm chiamante
020050010115     C                   if        §RMtla = '0'
020060010117      * se ci sono errori ritorna al pgm chiamante
020070100322     c                   if        *in28 or $errpre
020080100322     C                   eval      §RMerr = '5'
020090100322     C                   goto      endsrimm
020100100322     c                   endif
020110010117      * se non ci sono errori conferma orm
020120010117     C                   exsr      Sr_Conferma
020130100322     c                   if        *in28 or $errpre
020140100322     C                   eval      §RMerr = '5'
020150100322     C                   goto      endsrimm
020160100322     C                   endif
020170010115     C                   endif
020180010117      * Richiesta la conferma senza visualizzare l'orm
020190010117      * conferma solo se non ci sono errori, altrimenti visualizza l'orm con gli errori
020200100812     c                   IF        §RMtla = '1' and not *in28 and
020210100812     c                             not $errpre
020220010115     C                   exsr      Sr_Conferma
020230010115     C                   endif
020240010115
020250010115     C     endsrimm      endsr
020260130117
020270130121      /free
020280130121       //--------------------------------------------------------------
020290130121       //?Cerco tabella OSR per codice ordinante
020300130121       //--------------------------------------------------------------
020310130121       BEGSR sr_TABosr;
020320130117
020330130121         clear dOSR;
020340130121         wORMcor = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
020350130121       //?leggo tutta la tabella OSR
020360140422         kTBEcod = 'OSR';
020370130121         setll kTBEcod tntbe01l;
020380130121         reade kTBEcod tntbe01l;
020390130121
020400130121         DOW not %eof(tntbe01l);
020410130122           IF TBEatb = *blanks;
020420130121           dosr = tbeuni;
020430130121           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
020440130121              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
020450130121              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
020460130121              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
020470130121              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
020480130121              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
020490130121              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
020500130121              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
020510130121             leavesr;
020520130121           ENDIF;
020530130122           ENDIF;
020540130121           reade kTBEcod tntbe01l;
020550130121         ENDDO;
020560130121
020570130121       ENDSR;
020580130121      /end-free
020590130117
020600010208      **********************************************************************
020610010208      * CALCOLA LA DATA E L'ORA DEL GIORNO
020620010208      **********************************************************************
020630010208     C     Sr_Calcolaoggibegsr
020640010208
020650131211      /free
020660131211       //?Imposto oggi in formato aaaammgg
020670131211         oggi_aammgg = %dec(%date());
020680131211       //?Imposto oggi in formato ggmmaaaa
020690131211         dataISO = %date(oggi_aammgg:*iso);
020700131211         dataEUR = dataISO;
020710131211         oggi_ggmmaa = %dec(dataEUR);
020720160411       //?Imposto ora attuale
020730160411       //?se NON è conferma automatica da VAS imposto anche la data
020740160411         IF  not ConfAuto;
020750131211         V1oao = %dec(%time());
020760160411         V1dao = oggi_ggmmaa;
020770161026         inv_v1dao = oggi_aammgg;
020780160411         ENDIF;
020790131211      /end-free
020800010208
020810010208     C                   endsr
020820001027      **********************************************************************
020830001027      * VIDEATA PER ORM ALLOCATO AD ALTRO UTENTE
020840001027      **********************************************************************
020850001027     C     Sr_Allocato   begsr
020860001027
020870001027     C                   eval      v3cpoe = ppoe
020880001027     C                   eval      v3nsr  = pnsr
020890001027     C                   eval      v3nor  = pnor
020900001027     C                   eval      v3nrv  = pnrv
020910001027
020920001027     C                   exfmt     fior05v
020930001027
020940001027     C                   endsr
020950060726      *--------------------------------------------------------------------*
020960060726      * Videata per informare l'utente che l'ORM è già stato confermato
020970060726      *--------------------------------------------------------------------*
020980060726     c     sr_ormconf    begsr
020990060726
021000060726     c                   eval      v3cpoe = ppoe
021010060726     c                   eval      v3nsr  = pnsr
021020060726     c                   eval      v3nor  = pnor
021030060726     c                   eval      v3nrv  = pnrv
021040060726
021050060726     c                   exfmt     fior05va
021060060726
021070060726     c                   endsr
021080060726
021090001016      **********************************************************************
021100001023      * CARICA DATI DI DEFAULT NELLA VIDEATA
021110001016      **********************************************************************
021120001016     C     Sr_Cardft     begsr
021130131211
021140010124      * Pulisco il video
021150071108     C                   clear                   vidnew
021160131018     C                   clear                   vidnew2
021170131018     C                   clear                   vidnewA
021180160407     C                   clear                   vidnewC
021190010205      * Pulisco i campi di solo output e hidden
021200010205     C                   clear                   v1fao
021210010205     C                   clear                   v1dfao
021220010205     C                   clear                   v1dfo
021230010205     C                   clear                   v1ofo
021240010205     C                   clear                   v1cpoe
021250010205     C                   clear                   v1nsr
021260010205     C                   clear                   v1nor
021270010205     C                   clear                   v1nrv
021280010205     C                   clear                   v1ctco
021290010205     C                   clear                   v1dtco
021300010205     C                   clear                   v1cpos
021310010205     C                   clear                   v1ors
021320010205     C                   clear                   v1dsto
021330010205     C                   clear                   v1dpor
021340070913     c                   clear                   v1dcgi
021350010205     C                   clear                   v1dtor
021360010205     C                   clear                   v1dpoc
021370010205     C                   clear                   v1dst
021380010205     C                   clear                   v1cst
021390010205     C                   clear                   v1vcs
021400010205     C                   clear                   v1fcs
021410010205     C                   clear                   v1npg
021420010205     C                   clear                   v1ndc
021430010205     C                   clear                   v1ddc
021440010205     C                   clear                   v1stp
021450010205     C                   clear                   v1tap
021460010205     C                   clear                   v1eti
021470070913     c                   clear                   v1tgi
021480081215     c                   clear                   v1ntwd
021490161024     C                   clear                   v1dar
021500010305      * Pulisco i campi di salvataggio
021510010305     C                   clear                   sav_acrpoa
021520131022     C                   clear                   sav_acrorr
021530010305     C                   clear                   sav_acrfcl
021540010305     C                   clear                   sav_acrfpk
021550010305     C                   clear                   sav_acrfmc
021560010305     C                   clear                   sav_acrfbn
021570010305     C                   clear                   sav_acrfbl
021580010305     C                   clear                   sav_acrfat
021590010305     C                   clear                   sav_acrfmt
021600010305     C                   clear                   sav_v1cpor
021610010305     C                   clear                   sav_v1ccra
021620131024     c                   clear                   sav_dsV1Ccra
021630010305     C                   clear                   sav_v1car
021640010305     C                   clear                   sav_v1lor
021650010305     C                   clear                   sav_v1prr
021660010305     C                   clear                   sav_v1ccor
021670170208     c                   clear                   sav_dsV1Ccor
021680080910     C                   clear                   sav_v1cao
021690010305     C                   clear                   sav_v1loo
021700030922     C                   clear                   sav_v1loo35
021710010305     C                   clear                   sav_v1pro
021720010305     C                   clear                   sav_v1ccrc
021730131107     C                   clear                   sav_v1rsc
021740080910     C                   clear                   sav_v1cac
021750010305     C                   clear                   sav_v1loc
021760030922     C                   clear                   sav_v1loc35
021770010305     C                   clear                   sav_v1prc
021780131202     C                   clear                   sav_v1nac
021790010906     C                   clear                   sav_v1pag
021800010507     C                   clear                   sav_v1ksc
021810030606     c                   Clear                   AcrKscCor
021820050202     c                   Clear                   AcrCccCor
021830050322     c                   Clear                   sav_v1com
021840050323     c                   Clear                   old_newv1dar
021850070920     c                   clear                   sav_acrcgi
021860070913     c                   clear                   sav_v1ccgi
021870071031     c                   clear                   savpkg
021880071031     c                   clear                   savvlm
021890071031     c                   clear                   savbnc
021900131022     c                   clear                   v1anticipa
021910161028     c                   clear                   sav_v1dpm
021920161103     c                   clear                   inv_v1dpm
021930160310
021940160310     c                   clear                   §OREfsco
021950160310     c                   clear                   §OREfmco
021960140109
021970140422     c                   clear                   m_OG143
021980140109     c                   clear                   okbolla
021990140109     c                   eval      wintcap = *off
022000140422     c                   eval      walert = *off
022010140423     c                   eval      werralert = *off
022020160406     c                   eval      wErrConferma = *off
022030140515     c                   eval      wOKalert = *off
022040140711     c                   eval      wOKF04 = *off
022050160406     c                   eval      wOKF13 = *off
022060160310     c                   eval      wconferma = *off
022070131022
022080010305      * Imposto l'ultimo orm creato
022090010305     C                   eval      v1poeo = §rmpoeo
022100010305     C                   eval      v1nsro = §rmnsro
022110010305     C                   eval      v1noro = §rmnoro
022120010305     C                   eval      v1nrvo = §rmnrvo
022130010123
022140010201      * Imposto la data e ora chiamata/immissione
022150010117     C                   eval      v1dao = oggi_ggmmaa
022160131212     C                   clear                   wlbdat
022170131212      * giro la data
022180131212     C                   z-add     v1dao         G02dat
022190131212     C                   movel     *blanks       G02err
022200131212     C                   call      'XSRDA8'
022210131212     C                   parm                    wlbdat
022220131212     C                   z-add     G02inv        inv_v1dao
022230131211     C                   eval      V1oao = %dec(%time())
022240010201      * Pulisco i dati dei codici
022250010116     C                   clear                   ds_v1ccra
022260010116     C                   eval      v1cra1 = ds_v1cra1
022270010116     C                   eval      v1cra2 = ds_v1cra2
022280010116     C                   eval      v1cra3 = ds_v1cra3
022290010116     C                   clear                   ds_v1ccor
022300010116     C                   eval      v1cor1 = ds_v1cor1
022310010116     C                   eval      v1cor2 = ds_v1cor2
022320010116     C                   eval      v1cor3 = ds_v1cor3
022330010116     C                   clear                   ds_v1ccrc
022340010116     C                   eval      v1crc1 = ds_v1crc1
022350010116     C                   eval      v1crc2 = ds_v1crc2
022360010116     C                   eval      v1crc3 = ds_v1crc3
022370010205      * Imposto alcuni dati di DFT solo se non provengo da procedura esterna
022380010205     C                   if        §RMtla = *blanks
022390010205      * Imposta il tipo orm da DFT
022400010205     C                   eval      v1ctor = d§dfttor
022410010205     C                   exsr      Sr_Contrtor
022420010205      * Imposta il tipo comunicazione orm da DFT
022430010205     C                   eval      v1ctco = d§dfttco
022440141212      * se richiamato per immissione orm MAIL imposto 'E'
022450141212     c                   IF        psce = 'E'
022460141212     c                   eval      v1ctco = 'E'
022470141211     c                   ENDIF
022480010205     C                   exsr      Sr_Contrtco
022490010205      * Imposta il chi paga da DFT
022500010205     C                   eval      v1pag = d§dftpag
022510021119      * Imposta il flag ddt da DFT
022520021119     C                   eval      v1ddt = d§dftddt
022530010205     C                   endif
022540010201
022550010201     C                   eval      *in41 = *on                                  pos.curs.cli.ritiro
022560131024
022570131024     c                   eval      *in95 = *off
022580150923      /free
022590150923       //?Imposto la filiale emissione
022600150923       V1Cpoe = ppoe;
022610150923       V1poei = ppoe;
022620150923       clear V1poeid;
022630150923       chain (V1poei) AZORG01L;
022640150923       IF  %found(AZORG01L);
022650150923         V1poeid = ORGdes;
022660150923       ENDIF;
022670161103       //?Imposto Data e ora Pronta merce di dft in immissione/copia ORM su AS
022680161103       IF  (*in01 or *in03) and §RMtla = *blanks;
022690161103         V1dpm = V1dao;
022700161103         sav_V1dpm = V1dpm;
022710161103         inv_V1dpm = inv_V1dao;
022720161103         V1orr = V1oao/100;
022730161103       ENDIF;
022740150923      /end-free
022750150923
022760001016
022770001016     C                   endsr
022780001009      **********************************************************************
022790001023      * CARICA DATI NELLA VIDEATA DAL FILE
022800001009      **********************************************************************
022810001009     C     Sr_Carvid     begsr
022820131024
022830131024     c                   eval      wokestensione = *off
022840001009
022850010117     C                   eval      v1atb  = ORMatb
022860010117     C                   eval      v1ctco = ORMtco
022870010117     C                   eval      v1ctor = ORMtor
022880001023      * Inversione data emissione ORM
022890001016     C                   clear                   wlbdat
022900001017     C                   z-add     ORMdao        G02inv
022910001017     C                   movel     '3'           G02err
022920001016     C                   call      'XSRDA8'
022930001016     C                   parm                    wlbdat
022940001025     C                   z-add     G02dat        v1dao
022950130919     C                   z-add     ORMdao        inv_v1dao
022960001215      * Ora emissione ORM
022970131211     C                   eval      v1oao = ORMoao
022980001016      * Dati Ordinante
022990010605     C                   eval      sav_v1ccor = ORMcor
023000010605     C                   eval      ds_v1ccor = ORMcor
023010170208     C                   eval      sav_dsv1ccor = ORMcor
023020010116     C                   eval      v1cor1 = ds_v1cor1
023030010116     C                   eval      v1cor2 = ds_v1cor2
023040010116     C                   eval      v1cor3 = ds_v1cor3
023050010116     C                   eval      *in08 = (ORMcor <> *zeros)
023060010117     C                   eval      v1rso = ORMrso
023070010117     C                   eval      v1ino = ORMino
023080010117     C                   eval      v1cao = ORMcao
023090080910     C                   eval      sav_v1cao = ORMcao
023100001025     C                   movel     ORMloo        v1loo
023110010118     C                   movel     ORMloo        sav_v1loo
023120030922     C                   movel     ORMloo        sav_v1loo35
023130010117     C                   eval      v1pro = ORMpro
023140010118     C                   eval      sav_v1pro = ORMpro
023150010117     C                   eval      v1nao = ORMnao
023160010702      * Dati Mittente
023170010117     C                   eval      sav_v1ccra = ORMcra
023180010117     C                   eval      ds_v1ccra = ORMcra
023190161028     C                   eval      sav_dsv1ccra = ORMcra
023200010116     C                   eval      v1cra1 = ds_v1cra1
023210010116     C                   eval      v1cra2 = ds_v1cra2
023220010116     C                   eval      v1cra3 = ds_v1cra3
023230010116     C                   eval      *in07 = (ORMcra <> *zeros)
023240010117     C                   eval      v1rsr = ORMrsr
023250010117     C                   eval      v1inr = ORMinr
023260010117     C                   eval      v1car = ORMcar
023270010118     C                   eval      sav_v1car = ORMcar
023280001025     C                   movel     ORMlor        v1lor
023290010118     C                   movel     ORMlor        sav_v1lor
023300010117     C                   eval      v1prr = ORMprr
023310010118     C                   eval      sav_v1prr = ORMprr
023320010117     C                   eval      v1nar = ORMnar
023330010117     C                   eval      v1rer = ORMrer
023340010117     C                   eval      v1ter = ORMter
023350010702      * Dati Destinatario
023360010605     C                   eval      sav_v1ccrc = ORMcrc
023370010605     C                   eval      ds_v1ccrc = ORMcrc
023380010116     C                   eval      v1crc1 = ds_v1crc1
023390010116     C                   eval      v1crc2 = ds_v1crc2
023400010116     C                   eval      v1crc3 = ds_v1crc3
023410010116     C                   eval      *in06 = (ORMcrc <> *zeros)
023420010117     C                   eval      v1rsc = ORMrsc
023430131107     c                   eval      sav_V1rsc = V1rsc
023440010117     C                   eval      v1inc = ORMinc
023450010219     C                   movel     ORMloc        v1loc
023460010208     C                   movel     ORMloc        sav_v1loc
023470030922     C                   movel     ORMloc        sav_v1loc35
023480010117     C                   eval      v1cac = ORMcac
023490080910     C                   eval      sav_v1cac = ORMcac
023500010117     C                   eval      v1prc = ORMprc
023510010118     C                   eval      sav_v1prc = ORMprc
023520010117     C                   eval      v1nac = ORMnac
023530131202     c                   eval      sav_v1nac = ORMnac
023540001016
023550010117     C                   eval      v1ffd = ORMffd
023560010117     C                   eval      v1nam = ORMnam
023570001023      * Inversione data ritiro
023580161110     C                   clear                   wlbdat
023590040402     C                   z-add     ORMdar        inv_v1dar
023600001017     C                   z-add     ORMdar        G02inv
023610001017     C                   movel     '3'           G02err
023620001016     C                   call      'XSRDA8'
023630001016     C                   parm                    wlbdat
023640001025     C                   z-add     G02dat        v1dar
023650001016
023660010117     C                   eval      v1orr = ORMorr
023670010117     C                   eval      v1ncl = ORMncl
023680010117     C                   eval      v1pkg = ORMpkg
023690071031     c                   eval      savpkg = ormpkg
023700161115      * mi devo salvare anche il campo ORGpkg per visualizzazione orari servizio con F14
023710161115     c                   eval      wpkg = ORGpkg
023720170605     c                   eval      old_wpkg = ORGpkg
023730010117     C                   eval      v1vlm = ORMvlm
023740161024     c                   eval      savvlm = ormvlm
023750010117     C                   eval      v1bnc = ORMbnc
023760071031     c                   eval      savbnc = ormbnc
023770010117     C                   eval      v1blc = ORMblc
023780010117     C                   eval      v1att = ORMatt
023790010117     C                   eval      v1mtc = ORMmtc
023800010702      * Chi paga
023810010117     C                   eval      v1pag = ORMpag
023820010308     C                   eval      sav_v1pag = ORMpag
023830010117     C                   eval      v1ksc = ORMksc
023840010507     C                   eval      sav_v1ksc = ORMksc
023850010702
023860010202     C                   eval      v1ctr = ORMctr
023870040630
023880010117     C                   eval      v1ddt = ORMddt
023890010202
023900001016     C                   if        ORMpor <> *zeros
023910001016     C                   movel     ORMpor        v1cpor
023920010116     C                   movel     ORMpor        sav_v1cpor
023930001016     C                   endif
023940070913
023950001016     C                   if        ORMpoc <> *zeros
023960001016     C                   movel     ORMpoc        v1cpoc
023970001011     C                   endif
023980010117     C                   eval      v1dst = ORMdst
023990010117     C                   eval      v1not1 = ORMno1
024000010117     C                   eval      v1not2 = ORMno2
024010010117     C                   eval      v1fao = ORMfao
024020010117     C                   movel     ORMpos        v1cpos
024030010117     C                   eval      v1ors = ORMors
024040010117     C                   eval      v1rfa = ORMrfa
024050001221     C                   move      ORMsto        v1sto
024060010119     C                   eval      v1spi = ORMspi
024070010115      * Campi hidden
024080010117     C                   eval      v1cst = ORMcst
024090010117     C                   eval      v1vcs = ORMvcs
024100010117     C                   eval      v1fcs = ORMfcs
024110010117     C                   eval      v1dfo = ORMdfo
024120010117     C                   eval      v1ofo = ORMofo
024130010117     C                   eval      v1npg = ORMnpg
024140010117     C                   eval      v1ndc = ORMndc
024150010117     C                   eval      v1ddc = ORMddc
024160010117     C                   eval      v1stp = ORMstp
024170010117     C                   eval      v1tap = ORMtap
024180010117     C                   eval      v1eti = ORMeti
024190011113     C                   eval      dorm01 = ORMflo
024200071107
024210071107      * imposto codice giro presente su FNORG
024220070920     c                   eval      v1tgi = orgtgi
024230070920     c                   eval      v1ccgi = orgcgi
024240070920     c                   eval      sav_v1ccgi = orgcgi
024250071106     c                   eval      sav_acrcgi = orgcgi
024260071106
024270071106      * imposto anche il volume o il peso già memorizzati su ORG per confronti sucessivi
024280071106     c                   eval      savvlm = orgvlm
024290071106     c                   eval      savpkg = orgpkg
024300070920
024310001025      * Protegge P.O.ritiro
024320050321      * SEMPRE!!!!! (in manutenzione)
024330050321      * e se orm prepagato da bolla  (tipo comunicazione)
024340050321     c                   Eval      *In09 =  *In02
024350011012     C                             or      v1ctco = 'P'
024360001027      * Protegge riferimento alfanumerico
024370050422      * se tipo comunicazione ORM da File o Internet
024380050323     C                   eval      *in78 = (ORMtco ='F'
024390011012     C                                   or ORMtco = 'I')
024400010124      * Se richiamato da gestione conferma orm da altri p.o. metto già la fase a 100
024410010124     C                   if        §RMfpr = 'O'
024420010124     C                   eval      v1fao = 100
024430010124     C                   eval      v1dfo = oggi_aammgg
024440050422     c                   time                    w0140
024450010124     C                   movel     w0140         v1ofo
024460010124     C                   endif
024470050321      * Orm commissionato
024480050322      * sempre protetto in manutenzione
024490030606     c                   Eval      V1com = §orCom
024500130918     c                   Eval      sav_V1com = §orCom
024510050420     c                   If        *In02
024520050322     c                   Eval      *In34 = *On
024530050420     c                   EndIf
024540060117
024550060117      * se il tipo ORM è 'P' prepagato proteggo il campo di tipo ORM in modo da non poterlo
024560060117      * + variare
024570060117     c                   Eval      *In05 = (v1ctor = 'P')
024580131018
024590131018      /free
024600131018       //?Recupero dati da estensione FNORE
024610131018       //?Rcd 'O ' --> Orari apertura
024620131018         k_OREpoe = ORMpoe;
024630131018         k_OREnsr = ORMnsr;
024640131018         k_OREnor = ORMnor;
024650131018         k_OREnrv = ORMnrv;
024660131018         k_OREtrc = 'O ';
024670131018         clear dOREorari;
024680131018       //?Aggancio il rcd
024690131029         chain(n)  %kds(k05fnore) FNORE01L;
024700131018         IF  %found(FNORE01L);
024710131018           dOREorari = OREdati;
024720131024           wokestensione = *on;
024730131018         ENDIF;
024740131018         v1OraAMda = §OREoramda;
024750131018         v1OraAMa  = §OREorama;
024760131018         v1OraPMda = §OREorapda;
024770131018         v1OraPMa  = §OREorapa;
024780161020       //?Rcd 'DT' --> Data Pronta Merce
024790170524       //?         --> Data Ritiro Calcolata e Anticipo
024800161020         k_OREtrc = 'DT';
024810161027         clear dOREdt;
024820161020         chain(n)  %kds(k05fnore) FNORE01L;
024830161020         IF  %found(FNORE01L);
024840161027           dOREdt = OREdati;
024850161020           wokestensione = *on;
024860161020         ENDIF;
024870161027         IF  §OREdpm > *zeros;
024880161027           inv_V1dpm = §OREdpm;
024890161110           clear wlbdat;
024900161020           G02inv = inv_V1dpm;
024910161020           G02err = '3';
024920161020           xsrda8 (wlbdat);
024930161020           V1dpm = G02dat;
024940161028           sav_V1dpm = V1dpm;
024950161020         ENDIF;
024960170524         IF  §OREdar > *zeros and §OREdar <> *blanks;
024970170524           DarCalcolata = %int(§OREdar);
024980170524           wOkPosticipa = (§OREposd = 'S');
024990170524           wAnticipa = (§OREant = 'S');
025000170524           Anticipato = (§OREmod = 'S');
025010170524           IF  §OREgga > *zeros and §OREgga <> *blanks;
025020170524             ggAnticipo = %int(§OREgga);
025030170524           ENDIF;
025040170524         ENDIF;
025050140429
025060140429       //?Controllo da rcd GEN se ORM con Alert
025070160405       //?e/o conferma prenotazione ritiro
025080140429         walert = *off;
025090160406         *in88 = *off;
025100140429         clear dOREgen;
025110140429         k_OREtrc = 'G ';
025120140429         clear dOREgen;
025130140429         chain(n) %kds(K05fnore) FNORE01L;
025140140429         IF  %found(FNORE01L);
025150140429           dOREgen = OREdati;
025160140429         ENDIF;
025170140429         IF  §OREfiso = 'S' or §OREfimo = 'S';
025180140429           walert = *on;
025190140429         ENDIF;
025200160405         IF  §OREfmco = 'S' or §OREfsco = 'S';
025210160406           *in88 = *on;
025220160405         ENDIF;
025230140429
025240160316       //?Carico i dati degli alert per ORM COMMISSIONATO
025250160513       //?se è stato inviato l'alert o se sono in conferma ORM commissionati
025260160513         IF  walert or §RMfpr = 'O';
025270140429         //?Rcd 'S ' --> SMS per invio alert
025280140429           k_OREtrc = 'S ';
025290140505           clear dOREsms;
025300140505           clear w03sms;
025310140429         //?Aggancio il rcd
025320140429           chain(n)  %kds(k05fnore) FNORE01L;
025330140429           IF  %found(FNORE01L);
025340140505             dOREsms = OREdati;
025350140429           ENDIF;
025360140505           IF  §OREsms <> *blanks;
025370140505             w03sms = §OREsms;
025380140505           ENDIF;
025390140429         //?Rcd 'MA' --> Mail per invio alert
025400140429           k_OREtrc = 'MA';
025410140429         //?Aggancio il rcd
025420140429           chain(n)  %kds(k05fnore) FNORE01L;
025430140429           IF  %found(FNORE01L);
025440140429             w03mail = OREdati;
025450140429           ENDIF;
025460160513         ENDIF;
025470160406
025480160406       //?Carico i dati relativi alla CONFERMA PRENOTAZIONE
025490160513       //?solo se l'alert è stato inviato
025500160513         IF  *in88;
025510160406         //?Rcd 'SC' --> SMS
025520160406         k_OREtrc = 'SC';
025530160406         clear w04sms;
025540160406         //?Aggancio il rcd
025550160406         chain(n)  %kds(k05fnore) FNORE01L;
025560160406         IF  %found(FNORE01L);
025570160406           w04sms = OREdati;
025580160406         ENDIF;
025590160406         //?Rcd 'MC' --> Mail
025600160406         k_OREtrc = 'MC';
025610160406         clear w04mail;
025620160406         //?Aggancio il rcd
025630160406         chain(n)  %kds(k05fnore) FNORE01L;
025640160406         IF  %found(FNORE01L);
025650160406           w04mail = OREdati;
025660160406         ENDIF;
025670160406       //?Se non ci sono disattivo F13
025680160406         IF  W04sms = *blanks and W04mail = *blanks;
025690160406           *in88 = *off;
025700160406         ENDIF;
025710160513         ENDIF;
025720160224
025730160406       //?Carico NPR
025740160224         k_OREtrc = 'NP';
025750160224         clear OREdati;
025760160224       //?Aggancio il rcd
025770160224         chain  %kds(k05fnore) FNORE01L;
025780160224         IF  %found(FNORE01L);
025790160310           V1npr = OREdati;
025800160224         ENDIF;
025810140122
025820131018      /end-free
025830170524
025840170524     C                   exsr      Sr_Decodifica
025850170524
025860170524      * se filiale ritiro non in gestione all'utente proteggo il giro
025870170524     c                   IF        %lookup(%editc(num_v1cpor:'X'):skpog) = 0
025880170524     c                   eval      *in24 = *on
025890170524     c                   ENDIF
025900170524
025910170524      * se ORM in fase 400 proteggo sempre il giro o 410
025920170524     c                   if        v1fao = 400 or v1fao = 410
025930170524     c                   eval      *in24 = *on
025940170524     c                   endif
025950170524
025960170524     c                   clear                   v1ntwd
025970001009
025980001009     C                   endsr
025990001023      **********************************************************************
026000001023      * CARICA DATI DALLA DS
026010001023      **********************************************************************
026020001023     C     Sr_Cards      begsr
026030001106
026040010118     C                   eval      v1cpoe = §RMpoe
026050010118     C                   eval      v1ctco = §RMtco
026060010201     C                   if        v1ctco = *blanks
026070010201     C                   eval      v1ctco = 'F'
026080010201     C                   endif
026090010118     C                   eval      v1ctor = §RMtor
026100100217
026110100218      /free
026120100218       //?Mi salvo i dati necessari per agganciare l'estensione del vao
026130100218       IF  §RMFPR = 'C';
026140100218         sav_vaocor = §rmcor;
026150100218         sav_vaopoe = §rmpoe;
026160100218         sav_vaonsr = §rmnsr;
026170100218         sav_vaonor = §rmnor;
026180100218         sav_vaonrv = §rmnrv;
026190100218       ENDIF;
026200100218      /end-free
026210100217
026220001023      * Dati Ordinante
026230010726      * se conferma ORM da ritiri fissi salvare i dati del cliente ordinante
026240050322      * anche per conferma ORM da IT/FILE
026250061011      * o se richiamato per proposta chiusura ORM
026260010726     C                   if        §RMtco = 'S'
026270010903     C                             or §RMfpr = 'C'
026280061011     c                             or §rmtla = 'C'
026290010726     C                   eval      sav_v1ccor = §RMcor
026300010726     C                   endif
026310010605     C                   eval      ds_v1ccor = §RMcor
026320010116     C                   eval      v1cor1 = ds_v1cor1
026330010116     C                   eval      v1cor2 = ds_v1cor2
026340010116     C                   eval      v1cor3 = ds_v1cor3
026350010116     C                   eval      *in08 = (§RMcor <> *zeros)
026360010118     C                   eval      v1rso = §RMRSO
026370010118     C                   eval      v1ino = §RMINO
026380010118     C                   eval      v1cao = §RMCAO
026390080910     C                   eval      sav_v1cao = §RMCAO
026400010118     C                   movel     §RMloo        v1loo
026410010118     C                   movel     §RMloo        sav_v1loo
026420030922     C                   movel     §RMloo        sav_v1loo35
026430010118     C                   eval      v1pro = §RMPRO
026440010118     C                   eval      sav_v1pro = §RMpro
026450010118     C                   eval      v1nao = §RMNAO
026460010702      * Dati Mittente
026470010726      * se conferma ORM da ritiri fissi salvare i dati del cliente mittente
026480010903      * anche per cnferma ORM da IT/CALL CENTER/FILE
026490010726     C                   if        §RMtco = 'S'
026500010903     C                             or §RMfpr = 'C'
026510040914      * o se conferma proposte
026520040914     c                             or §rmfpr = 'P'
026530010726     C                   eval      sav_v1ccra = §RMcra
026540140414      /free
026550140414       //?Se conferma ORM da VAO (orm internet) e cliente mittente codificato
026560140414       //?salvo ORA pronta merce
026570150921       //?e filiale ritiro forzata
026580140414         IF  §RMfpr = 'C' and §RMtco = 'I' and §RMcra > 0;
026590151109           chain(n) (§RMcra) FNACR01L;
026600140414           IF  %found(FNACR01L);
026610140414             sav_ACRorr = ACRorr;
026620150921             sav_ACRpoa = ACRpoa;
026630140414           ENDIF;
026640140414         ENDIF;
026650140414      /end-free
026660010726     C                   endif
026670010118     C                   eval      ds_v1ccra = §RMcra
026680010116     C                   eval      v1cra1 = ds_v1cra1
026690010116     C                   eval      v1cra2 = ds_v1cra2
026700010116     C                   eval      v1cra3 = ds_v1cra3
026710010116     C                   eval      *in07 = (§RMcra <> *zeros)
026720010118     C                   eval      v1rsr = §RMRSR
026730010118     C                   eval      v1inr = §RMINR
026740010118     C                   eval      v1car = §RMCAR
026750010118     C                   eval      sav_v1car = §RMcar
026760001023     C                   movel     §RMLOR        v1lor
026770010118     C                   movel     §RMlor        sav_v1lor
026780010118     C                   eval      v1prr = §RMPRR
026790010118     C                   eval      sav_v1prr = §RMprr
026800010118     C                   eval      v1nar = §RMNAR
026810010118     C                   eval      v1rer = §RMrer
026820010118     C                   eval      v1ter = §RMter
026830010117      * Dati Destinatario
026840010726      * se conferma ORM da ritiri fissi salvare i dati del cliente destinatario
026850050322      * anche per conferma ORM da IT/FILE
026860010726     C                   if        §RMtco = 'S'
026870010903     C                             or §RMfpr = 'C'
026880010726     C                   eval      sav_v1ccrc = §RMcrc
026890131107     C                   eval      sav_V1rsc = V1rsc
026900010726     C                   endif
026910010605     C                   eval      ds_v1ccrc = §RMcrc
026920010116     C                   eval      v1crc1 = ds_v1crc1
026930010116     C                   eval      v1crc2 = ds_v1crc2
026940010116     C                   eval      v1crc3 = ds_v1crc3
026950010116     C                   eval      *in06 = (§RMcrc <> *zeros)
026960010118     C                   eval      v1rsc = §RMrsc
026970010118     C                   eval      v1inc = §RMinc
026980010208     C                   movel     §RMloc        v1loc
026990010208     C                   movel     §RMloc        sav_v1loc
027000030922     C                   movel     §RMloc        sav_v1loc35
027010010118     C                   eval      v1cac = §RMcac
027020080910     C                   eval      sav_v1cac = §RMcac
027030010118     C                   eval      v1prc = §RMprc
027040010118     C                   eval      sav_v1prc = §RMprc
027050010118     C                   eval      v1nac = §RMnac
027060131202     c                   eval      sav_v1nac = §RMnac
027070010118     C                   eval      v1ffd = §RMffd
027080010118     C                   eval      v1nam = §RMnam
027090001023      * Inversione data ritiro
027100010117     C                   if        §rmdar > *zeros
027110140529     C                   z-add     §RMdar        old_invv1dar
027120161110     c                   clear                   wlbdat
027130001023     C                   z-add     §RMdar        G02inv
027140001023     C                   movel     '3'           G02err
027150001023     C                   call      'XSRDA8'
027160001023     C                   parm                    wlbdat
027170160830     c                   IF        G02err = '1'
027180160830     c                   z-add     *zeros        v1dar
027190160830     c                   ELSE
027200001025     C                   z-add     G02dat        v1dar
027210160830     c                   ENDIF
027220010202     C                   else
027230010202     C                   z-add     *zeros        v1dar
027240010117     C                   endif
027250160307      /free
027260160307         //?è capitato che da File arrivasse una data non corretta
027270160307         //?se ho anno o mese o giorno a 0 pulisco la data
027280160307           IF  V1dar > 0;
027290160307             IF  %subst(%editc(V1dar:'X'):1:2) = '00' or
027300160307                 %subst(%editc(V1dar:'X'):3:2) = '00' or
027310160307                 %subst(%editc(V1dar:'X'):5:4) = '0000';
027320160307               clear v1dar;
027330160307             ENDIF;
027340160307           ENDIF;
027350160307      /end-free
027360001023
027370010118     C                   eval      v1orr = §RMorr
027380131022      * se conferma ORM fissi salvo ORA pronta merce
027390131022     c                   IF        §RMtco = 'S'
027400131022     c                   eval      sav_ACRorr = §RMorr
027410131022     c                   ENDIF
027420010118     C                   eval      v1ncl = §RMncl
027430010118     C                   eval      v1pkg = §RMpkg
027440071031     c                   eval      savpkg = §rmpkg
027450010118     C                   eval      v1vlm = §RMvlm
027460071031     c                   eval      savvlm = §rmvlm
027470010118     C                   eval      v1bnc = §RMbnc
027480071031     c                   eval      savbnc = §rmbnc
027490010118     C                   eval      v1blc = §RMblc
027500010118     C                   eval      v1att = §RMatt
027510010118     C                   eval      v1mtc = §RMmtc
027520010702      * Chi paga
027530010118     C                   eval      v1pag = §RMpag
027540010308     C                   eval      sav_v1pag = §RMpag
027550010118     C                   eval      v1ksc = §RMksc
027560010507     C                   eval      sav_v1ksc = §RMksc
027570010514     C                   eval      v1ctr = §RMctr
027580010702     C                   eval      v1ddt = §RMddt
027590140109      * Imposta il flag ddt da DFT
027600140109     c                   IF        v1ddt = *blanks
027610140109     c                   eval      v1ddt = d§dftddt
027620140109     c                   ENDIF
027630001023     C                   if        §RMpor <> *zeros
027640001023     C                   movel     §RMpor        v1cpor
027650010116     C                   movel     §RMpor        sav_v1cpor
027660001023     C                   endif
027670001023     C                   if        §RMpoc <> *zeros
027680001023     C                   movel     §RMpoc        v1cpoc
027690001023     C                   endif
027700010118     C                   eval      v1not1 = §RMno1
027710010118     C                   eval      v1not2 = §RMno2
027720010118     C                   eval      v1cpos = §RMpos
027730010118     C                   eval      v1ors = §RMors
027740010118     C                   eval      v1rfa = §RMrfa
027750001221     C                   move      §RMsto        v1sto
027760010119     C                   eval      v1spi = §RMspi
027770010115      * Campi hidden
027780010118     C                   eval      v1cst = §RMcst
027790010118     C                   eval      v1vcs = §RMvcs
027800010118     C                   eval      v1fcs = §RMfcs
027810010118     C                   eval      v1tap = §RMtap
027820010118     C                   eval      v1eti = §RMeti
027830011113     C                   eval      dorm01 = §rmflo
027840010117     C                   clear                   v1dst
027850010117     C                   clear                   v1npg
027860010117     C                   clear                   v1ndc
027870010117     C                   clear                   v1ddc
027880010117     C                   clear                   v1stp
027890081215     c                   clear                   v1ntwd
027900001025
027910010117      * Dati da ORM se richiamato da gestione proposte
027920001106     C                   if        §RMfpr = 'P'
027930001106      * Inversione data assegnazione ORM
027940161110     c                   clear                   wlbdat
027950001106     C                   z-add     ORMdao        G02inv
027960001106     C                   movel     '3'           G02err
027970001106     C                   call      'XSRDA8'
027980001106     C                   parm                    wlbdat
027990001106     C                   z-add     G02dat        v1dao
028000131212     C                   z-add     ORMdao        inv_v1dao
028010010117
028020131211     C                   eval      v1oao = ORMoao
028030010118     C                   eval      v1ctco = ORMtco
028040010118     C                   eval      v1fao = ORMfao
028050010118     C                   eval      v1dfo = ORMdfo
028060010118     C                   eval      v1ofo = ORMofo
028070010118     C                   eval      v1cpos = ORMpos
028080010118     C                   eval      v1ors = ORMors
028090010118     C                   eval      v1dst = ORMdst
028100010118     C                   eval      v1npg = ORMnpg
028110010118     C                   eval      v1ndc = ORMndc
028120010118     C                   eval      v1ddc = ORMddc
028130010118     C                   eval      v1stp = ORMstp
028140050422      * Se è una proposta di chiusura
028150010117     C                   if        §RMtla = 'C'
028160070228      * salvo la fase anche per aggiornare FIPDO
028170070228     c                   eval      savorffar = orffar
028180070228      * e salvo anche la distinta per passarla al pgm che aggiorna FIPDO
028190070228     c                   eval      savorffgs = orffgs
028200070228     c                   eval      savorfndc = orfndc
028210070228     c                   eval      savorfddc = orfddc
028220050422      * imposto i dati per la fase
028230010117     C                   z-add     oggi_aammgg   v1dfo
028240050422     c                   time                    w0140
028250010117     C                   movel     w0140         v1ofo
028260010117     C                   eval      *in10 = *on
028270010117     C                   movel     mod(5)        v1mod
028280010209     C                   eval      v3cau = §RMcau
028290030801      * metto la causale anche in prima videata
028300030801     C                   eval      v1cau = §RMcau
028310030801     C                   eval      *in13 = (v1cau <> *blanks)
028320050422      * imposto la fase che c'è sulla causale
028330030801     C                   clear                   dcmr
028340030801     C                   clear                   TIBS02DS
028350030801     C                   movel     'C'           T02mod
028360030801     C                   movel     knsif         t02sif
028370030801     C                   movel     'CMR'         t02cod
028380030801     C                   movel(p)  v1cau         T02ke1
028390030801     C                   call      'TIBS02R'
028400030801     C                   parm                    KPJBA
028410030801     C                   parm                    TIBS02DS
028420030801     C                   movel     t02uni        dcmr
028430030801     C                   movel     d§cmrdes      v1dcau
028440030801     c                   Movel     d§CmrFar      V1Fao
028450020524      * se la proposta di chiusura ha generato una bolla non permetto F12
028460020524     c                   if        %subst(§rmflv:1:1) = 'B'
028470020524     c                   eval      *in17 = *on
028480020524     c                   endif
028490010117     C                   endif
028500050322      * Proteggo sempre il campo dell'orm commissionato
028510050322     c                   Eval      *In34 = *On
028520001106     C                   endif
028530010117
028540050422      * Se Conferma ORM da VAO
028550050422     C                   if        §RMfpr = 'C'
028560050422      * data/ora immissione ORM da VAO
028570161110     c                   clear                   wlbdat
028580050422     c                   z-add     §rmdao        G02inv
028590050422     c                   movel     '3'           G02err
028600050422     c                   call      'XSRDA8'
028610050422     c                   parm                    wlbdat
028620050422     c                   z-add     G02dat        v1dao
028630131212     C                   z-add     §RMdao        inv_v1dao
028640131211     c                   eval      v1oao = §RMoao
028650010117      * Protegge riferimento alfanumerico
028660010117     C                   eval      *in78 = *on
028670010123      * Se non è impostato il riferimento mette il n.orm che c'è nella ds
028680010712     C                   if        §RMrfa = *blanks   and
028690010712     C                             §RMnsr <> *zeros
028700010201     C                   clear                   ds_keyorm
028710010201     C                   eval      ds_poe = §RMpoe
028720010201     C                   eval      ds_nsr = §RMnsr
028730010201     C                   eval      ds_nor = §RMnor
028740010201     C                   eval      ds_nrv = §RMnrv
028750010201     C                   movel     ds_keyorm     v1rfa
028760010123     C                   endif
028770001114     C                   endif
028780010322      * Protegge P.O.ritiro
028790050321      * SEMPRE!!!!! (in manutenzione)
028800050321      * e se orm prepagato da bolla  (tipo comunicazione)
028810050321     c                   Eval      *In09 =  *In02
028820050315     C                             or      v1ctco = 'P'
028830050321      * Orm commissionato
028840030606     c                   Eval      V1com = §orCom
028850170313      /free
028860170313       //?salvo codice mittente e codice ordinante per ricalcolo della data ritiro
028870170505       //?nel caso di variazione del dato
028880170505         sav_dsV1Ccra = ds_V1Ccra;
028890170505         sav_dsV1Ccor = ds_V1Ccor;
028900170313      /end-free
028910100205
028920100205      * Personalizzazioni ORMFLO da tavella PVO
028930100205      * solo in immissione e se conferma da VAO
028940100205     c                   if        *in01 and §rmfpr = 'C'
028950100205     c                   exsr      sr_pvo
028960100205     c                   endif
028970100205
028980050502      * se sto confermando da VAO
028990040714     c                   If        §RMfpr = 'C'
029000050502      * prendo quello passato dal cliente
029010040714     c                   Eval      V1com = §orComc
029020050502      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
029030050502      * di variazione
029040050502     c                   If        §orsrm = 'S'
029050050502     c                   Eval      v1com = 'S'
029060050502     c                   Eval      *In34 = *On
029070050511      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
029080050511     c                   Clear                   v1dar
029090050502     c                   EndIf
029100100325
029110100325      /free
029120150921       //?Calcolo il peso/volume
029130150921        exsr sr_pesvol;
029140131111       //?Richiamo la routine CALPOR
029150131111       //?per avere la filiale di ritiro calcolata da cappario e per
029160131111       //?ulteriori controlli che vengono fatti SOLO in questa routine
029170131111        exsr  CALPOR;
029180161019        IF  *in28;
029190161019          *in28 = *off;
029200161019        ENDIF;
029210161108       //?Cerco orari servizio
029220161108        exsr CercaOrari;
029230140109       //?Recupero gli ambiti per il recupero del giro da ACR
029240140109        exsr sr_caragr;
029250100325       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
029260100325       //?di variazione
029270100325        IF  §ORMpre = 'M';
029280100325          V1com = 'N';
029290100325          *in34 = *on;
029300100325        ENDIF;
029310131018
029320131018       //?Recupero dati da estensione VAOE
029330131018       //?Rcd 'O ' --> Orari apertura
029340131018         k_vaoecor = sav_vaocor;
029350131018         k_vaoepoe = sav_vaopoe;
029360131018         k_vaoensr = sav_vaonsr;
029370131018         k_vaoenor = sav_vaonor;
029380131018         k_vaoenrv = sav_vaonrv;
029390131018         k_vaoetrc = 'O ';
029400131018         clear dOREorari;
029410131018       //?Aggancio il rcd
029420131018         chain  %kds(k06fnvaoe) FNVAOE1L;
029430131018         IF  %found(FNVAOE1L);
029440140415           dOREorari = VAOEdati;
029450131018         ENDIF;
029460131018         v1OraAMda = §OREoramda;
029470131018         v1OraAMa  = §OREorama;
029480131018         v1OraPMda = §OREorapda;
029490131018         v1OraPMa  = §OREorapa;
029500161019       //?Rcd 'DT' --> Data Pronta Merce
029510170524       //?         --> Data Ritiro Calcolata e Anticipo
029520161019         k_vaoetrc = 'DT';
029530161027         clear dOREdt;
029540161019         chain  %kds(k06fnvaoe) FNVAOE1L;
029550161019         IF  %found(FNVAOE1L);
029560161027           dOREdt = VAOEdati;
029570161019         ENDIF;
029580161027         IF  §OREdpm > *zeros;
029590161110           clear wlbdat;
029600161027           inv_V1dpm = §OREdpm;
029610161020           G02inv = inv_V1dpm;
029620161020           G02err = '3';
029630161020           xsrda8 (wlbdat);
029640161020           V1dpm = G02dat;
029650161028           sav_V1dpm = V1dpm;
029660161020         ENDIF;
029670170524         IF  §OREdar > *zeros and §OREdar <> *blanks;
029680170524           DarCalcolata = %int(§OREdar);
029690170524           wOkPosticipa = (§OREposd = 'S');
029700170524           wAnticipa = (§OREant = 'S');
029710170524           Anticipato = (§OREmod = 'S');
029720170524           IF  §OREgga > *zeros and §OREgga <> *blanks;
029730170524             ggAnticipo = %int(§OREgga);
029740170524           ENDIF;
029750170524         ENDIF;
029760131018       //?Rcd 'S ' --> SMS x invio alert
029770131018         k_vaoetrc = 'S ';
029780140505         clear dOREsms;
029790131018       //?Aggancio il rcd
029800131018         chain  %kds(k06fnvaoe) FNVAOE1L;
029810131018         IF  %found(FNVAOE1L);
029820161020           dOREsms = VAOEdati;
029830131018         ENDIF;
029840161020         w03sms = §OREsms;
029850131018       //?Rcd 'MA' --> Mail x invio alert
029860131018         k_vaoetrc = 'MA';
029870131018       //?Aggancio il rcd
029880131018         chain  %kds(k06fnvaoe) FNVAOE1L;
029890131018         IF  %found(FNVAOE1L);
029900131025           w03mail = VAOEdati;
029910131018         ENDIF;
029920140422       //?Rcd 'G' --> Generale
029930140422         k_vaoetrc = 'G';
029940140422         clear dOREgen;
029950140422       //?Aggancio il rcd
029960140422         chain  %kds(k06fnvaoe) FNVAOE1L;
029970140422         IF  %found(FNVAOE1L);
029980140422           dOREgen = VAOEdati;
029990140422         ENDIF;
030000160406       //?controllo se c'è l'invio conferma accettazione ORM
030010160406         wconferma = *off;
030020160406         IF  §OREfmco = 'S' or §OREfsco = 'S';
030030160406           wconferma = *on;
030040160406           *in88 = *on;
030050160406         ENDIF;
030060160406
030070160406       //?Se ho il flag di invio mail conferma cerco i dati
030080160406         IF  wconferma;
030090160406         //?Rcd 'MC' --> Mail x Conferma Prenotazione
030100160406           k_vaoetrc = 'MC';
030110160406           clear w04mail;
030120160406         //?Aggancio il rcd
030130160406           chain  %kds(k06fnvaoe) FNVAOE1L;
030140160406           IF  %found(FNVAOE1L);
030150160406             w04mail = VAOEdati;
030160160406           ENDIF;
030170160406         //?Rcd 'SC' --> Sms x Conferma Prenotazione
030180160406           k_vaoetrc = 'SC';
030190160406           clear w04sms;
030200160406         //?Aggancio il rcd
030210160406           chain  %kds(k06fnvaoe) FNVAOE1L;
030220160406           IF  %found(FNVAOE1L);
030230160406             w04sms = VAOEdati;
030240160406           ENDIF;
030250160406           sav_mailconf = W04mail;
030260160406           sav_smsconf = W04sms;
030270160406         ENDIF;
030280150526
030290150526       //?Controllo i dati MAIL e SMS
030300150526       //?se sono errati non li devo caricare
030310160316         IF  W03sms <> *blanks or W03mail <> *blanks;
030320150526           exsr CtrDatiF04;
030330150526         ENDIF;
030340160426
030350160426       //?Cerco se c'è già il NPR
030360160426         k_vaoetrc = 'NP';
030370160426         WebNPR = *off;
030380160426       //?Aggancio il rcd
030390160426         chain  %kds(k06fnvaoe) FNVAOE1L;
030400160426         IF  %found(FNVAOE1L) and VAOEdati <> *blanks;
030410160426           WebNPR = *on;
030420160426         ENDIF;
030430131212
030440131212       //?Se mittente codificato cerco il giro
030450140109         IF  ds_V1Ccra > 0;
030460131212           exsr sr_pesvol;
030470131212           exsr sr_carcgi;
030480131212           IF  sav_acrcgi <> *blanks;
030490131212             v1tgi = 'A';
030500131212             v1ccgi = sav_acrcgi;
030510131212             sav_v1ccgi = sav_acrcgi;
030520131212           ENDIF;
030530131212         ENDIF;
030540140505
030550140516       //?Se ORM commissionato verifico
030560140516         IF  V1com = 'S';
030570140516         //?verifico se ci sono i presupposti per Alert
030580140516           exsr VerificaAlert;
030590140516         //?se data ritiro già impostata controllo se è da ricalcolare
030600140516           IF  v1dar > 0;
030610140516             exsr  DataRitiroAlert;
030620140516           ENDIF;
030630140505         ENDIF;
030640140428
030650140428       //?Se data ritiro immessa dal cliente inferiore a oggi (giorno di conferma)
030660140429       //?imposto la data di ritiro = oggi
030670140428         IF  §RMdar > 0 and §RMdar < oggi_aammgg;
030680140429           V1dar = oggi_ggmmaa;
030690140428         ENDIF;
030700131212
030710100325      /end-free
030720100325
030730040714     c                   EndIf
030740170505       //?Salvo il commissionato
030750170505         sav_v1com = V1com;
030760070919
030770071107      * se sto confermando una proposta di variazione
030780071107     c                   if        §rmfpr = 'P' and §rmtla <> 'C'
030790080314      * devo ricalcolare sempre il giro se ORM in fase 400 o 410
030800080314     c                   if        v1fao = 400 or v1fao = 410
030810071107      * recupero la tabella AGR per il po ritiro
030820071107     c                   movel     v1cpor        num_v1cpor
030830071107     c                   exsr      sr_caragr
030840071107      * calcolo peso o volume
030850071107     c                   exsr      sr_pesvol
030860071107      * cerca il giro
030870071107     c                   exsr      sr_carcgi
030880071107     c                   if        sav_acrcgi <> *blanks
030890071107     c                   eval      v1tgi = 'A'
030900071107     c                   eval      v1ccgi = sav_acrcgi
030910071107     c                   eval      sav_v1ccgi = sav_acrcgi
030920071107     c                   else
030930071107     c                   clear                   v1tgi
030940071107     c                   clear                   v1ccgi
030950071107     c                   clear                   sav_v1ccgi
030960071107     c                   endif
030970071107     c                   endif
030980080314      * devo impostare il giro presente su ORG se ORM NON in fase 400 o 410
030990080314     c                   if        v1fao <> 400 and v1fao <> 410
031000071107     c                   eval      v1tgi = orgtgi
031010071107     c                   eval      v1ccgi = orgcgi
031020071107     c                   eval      sav_v1ccgi = orgcgi
031030071107     c                   eval      sav_acrcgi = orgcgi
031040160406     c                   eval      savvlm = ORGvlm
031050160406     c                   eval      savpkg = ORGpkg
031060071107     c                   endif
031070131029      /free
031080131029       //?Recupero dati da estensione FNORPE
031090160316       //?Non carico gli Alert, dopo l'immissione sono inutili
031100160406       //?così come i dati per conferma prenotazione
031110131029       //?Rcd 'O ' --> Orari apertura
031120131029         k_ORPEpoe = §RMpoe;
031130131029         k_ORPEnsr = §RMnsr;
031140131029         k_ORPEnor = §RMnor;
031150131029         k_ORPEnrv = §RMnrv;
031160131029         k_ORPEdtv = §RMdtv;
031170131029         k_ORPEorv = §RMorv;
031180131029         k_ORPEtrc = 'O ';
031190131029         clear dOREorari;
031200131029       //?Aggancio il rcd
031210131029         chain  %kds(k07fnorpe) FNORPE1L;
031220131029         IF  %found(FNORPE1L);
031230131029           dOREorari = ORPEdati;
031240140123           wokestensione = *on;
031250131029         ENDIF;
031260131029         v1OraAMda = §OREoramda;
031270131029         v1OraAMa  = §OREorama;
031280131029         v1OraPMda = §OREorapda;
031290131029         v1OraPMa  = §OREorapa;
031300161019       //?Rcd 'DT' --> Data Pronta Merce
031310170524       //?         --> Data Ritiro Calcolata e Anticipo
031320161019         k_ORPEtrc = 'DT';
031330161027         clear dOREdt;
031340161019         chain  %kds(k07fnorpe) FNORPE1L;
031350161019         IF  %found(FNORPE1L);
031360161027           dOREdt = ORPEdati;
031370161019           wokestensione = *on;
031380161019         ENDIF;
031390161027         IF  §OREdpm > *zeros;
031400161110           clear wlbdat;
031410161027           inv_V1dpm = §OREdpm;
031420161020           G02inv = inv_V1dpm;
031430161020           G02err = '3';
031440161020           xsrda8 (wlbdat);
031450161020           V1dpm = G02dat;
031460161028           sav_V1dpm = V1dpm;
031470161020         ENDIF;
031480170524         IF  §OREdar > *zeros and §OREdar <> *blanks;
031490170524           DarCalcolata = %int(§OREdar);
031500170524           wOkPosticipa = (§OREposd = 'S');
031510170524           wAnticipa = (§OREant = 'S');
031520170524           Anticipato = (§OREmod = 'S');
031530170524           IF  §OREgga > *zeros and §OREgga <> *blanks;
031540170524             ggAnticipo = %int(§OREgga);
031550170524           ENDIF;
031560170524         ENDIF;
031570140124       //?Visto che è una conferma di proposta di variazione devo caricare anche
031580140124       //?i dati dell'ORM, almeno per gli orari in modo da poter confrontare le
031590140124       //?variazioni fatte
031600140124         clear wOREorari;
031610140124       //?Recupero dati da estensione FNORE
031620140124       //?Rcd 'O ' --> Orari apertura
031630140124         k_OREpoe = §RMpoe;
031640140124         k_OREnsr = §RMnsr;
031650140124         k_OREnor = §RMnor;
031660140124         k_OREnrv = §RMnrv;
031670140124         k_OREtrc = 'O ';
031680140124       //?Aggancio il rcd
031690140124         chain  %kds(k05fnore) FNORE01L;
031700140124         IF  %found(FNORE01L);
031710140124           wOREorari = OREdati;
031720140124         ENDIF;
031730131029      /end-free
031740070920     c                   endif
031750131024
031760131024      /free
031770140320       //?Se conferma ORM fissi o immissione prepagato
031780140320         IF  §RMtco = 'S' or §RMtco = 'P';
031790150921       //?Richiamo calcolo peso volume
031800150921           exsr sr_pesvol;
031810131211       //?Richiamo la routine CALPOR
031820131211       //?per avere la filiale di ritiro calcolata da cappario e per
031830131211       //?ulteriori controlli che vengono fatti SOLO in questa routine
031840131211           exsr  CALPOR;
031850140109       //?Recupero gli ambiti per il recupero del giro da ACR
031860140109           exsr sr_caragr;
031870131024         ENDIF;
031880140606
031890140606       //?Se conferma di una proposta di chiusura ORM
031900140606       //?devo portarmi dietro anche i dati delle estensioni
031910140606         IF  §RMfpr = 'P' and §RMtla = 'C';
031920140606         //?Recupero dati da estensione FNORE
031930140606         //?Rcd 'O ' --> Orari apertura
031940140606           k_OREpoe = §RMpoe;
031950140606           k_OREnsr = §RMnsr;
031960140606           k_OREnor = §RMnor;
031970140606           k_OREnrv = §RMnrv;
031980140606           k_OREtrc = 'O ';
031990140606           clear dOREorari;
032000140606         //?Aggancio il rcd
032010140606           chain(n)  %kds(k05fnore) FNORE01L;
032020140606           IF  %found(FNORE01L);
032030140606             dOREorari = OREdati;
032040140606             wokestensione = *on;
032050140606           ENDIF;
032060140606           v1OraAMda = §OREoramda;
032070140606           v1OraAMa  = §OREorama;
032080140606           v1OraPMda = §OREorapda;
032090140606           v1OraPMa  = §OREorapa;
032100161020       //?Rcd 'DT' --> Data Pronta Merce
032110170524       //?         --> Data Ritiro Calcolata e Anticipo
032120161020           k_OREtrc = 'DT';
032130161027           clear dOREdt;
032140161020           chain(n)  %kds(k05fnore) FNORE01L;
032150161020           IF  %found(FNORE01L);
032160161027             dOREdt = OREdati;
032170161020             wokestensione = *on;
032180161020           ENDIF;
032190161027           IF  §OREdpm > *zeros;
032200161027             inv_V1dpm = §OREdpm;
032210161110             clear wlbdat;
032220161020             G02inv = inv_V1dpm;
032230161020             G02err = '3';
032240161020             xsrda8 (wlbdat);
032250161020             V1dpm = G02dat;
032260161028             sav_V1dpm = V1dpm;
032270161020           ENDIF;
032280170524           IF  §OREdar > *zeros and §OREdar <> *blanks;
032290170524             DarCalcolata = %int(§OREdar);
032300170524             wOkPosticipa = (§OREposd = 'S');
032310170524             wAnticipa = (§OREant = 'S');
032320170524             Anticipato = (§OREmod = 'S');
032330170524             IF  §OREgga > *zeros and §OREgga <> *blanks;
032340170524               ggAnticipo = %int(§OREgga);
032350170524             ENDIF;
032360170524           ENDIF;
032370140606
032380140606         //?Controllo da rcd GEN se ORM con Alert
032390160316         //?Per visualizzare che è stato inviato l'Alert per ORM commissionato
032400140606           walert = *off;
032410140606           clear dOREgen;
032420140606           k_OREtrc = 'G ';
032430140606           clear dOREgen;
032440140606           chain(n) %kds(K05fnore) FNORE01L;
032450140606           IF  %found(FNORE01L);
032460140606             dOREgen = OREdati;
032470140606           ENDIF;
032480140606           IF  §OREfiso = 'S' or §OREfimo = 'S';
032490140606            walert = *on;
032500140606           ENDIF;
032510160316         //?non carico i dati degli alert, sono in chiusura e non servono
032520160316         //?e soprattutto sono in conferma proposta quindi no F04
032530160405         //?stessa cosa per F13
032540140606         ENDIF;
032550140606
032560131024      /end-free
032570060117
032580060117      * se il tipo ORM è 'P' prepagato proteggo il campo di tipo ORM in modo da non poterlo
032590060117      * + variare
032600060117     c                   Eval      *In05 = (v1ctor = 'P')
032610131209
032620001106     C                   exsr      Sr_Decodifica
032630070919
032640150305      * se filiale ritiro non in gestione all'utente proteggo il giro
032650070919      * se ho già la filiale ritiro impostata
032660071108      * e se giro non è già protetto
032670071108     c                   if        num_v1cpor > *zeros and not *in24
032680150305     c                             and %lookup(%editc(num_v1cpor:'X'):skpog) = 0
032690150305     c                   eval      *in24 = *on
032700070919     c                   endif
032710001023
032720001023     C                   endsr
032730070925
032740070925      *--------------------------------------------------------------------*
032750070925      * Controllo i dati della proposta con quelli dell'ORM
032760070925      *--------------------------------------------------------------------*
032770070925     c     sr_controrp   begsr
032780070925
032790070925     c                   if        ds_v1ccra <> ormcra or
032800070925     c                             v1rsr <> ormrsr or
032810070925     c                             v1pkg <> ormpkg or v1ncl <> ormncl or
032820070925     c                             v1vlm <> ormvlm or v1bnc <> ormbnc or
032830070925     c                             v1blc <> ormblc or v1att <> ormatt or
032840070925     c                             v1mtc <> ormmtc or v1spi <> ormspi
032850070925     c                   eval      *in25 = *on
032860070925     c                   endif
032870160329
032880160329      /free
032890160329       //?Se è stato tolto il codice cliente
032900160329       //?quindi si tratta di un mittente non codificato
032910160329       //?devo togliere il giro
032920160329         IF  ORMcra > 0 and ds_V1Ccra = 0;
032930160329           clear V1Ccgi;
032940160329           clear V1tgi;
032950160329           *in25 = *off;
032960160329         ENDIF;
032970160329      /end-free
032980070925
032990070925     c                   endsr
033000070925
033010050302      *--------------------------------------------------------------------*
033020050302      * Calcolo le date massime per controllo data ritiro
033030050302      *--------------------------------------------------------------------*
033040050302     c     Sr_dtmax      BegSr
033050050304
033060050304      * controllo che i campi siano effettivamente numerici
033070050304     c                   If        d§dftgg1 = *Blanks
033080050304     c                   Eval      d§dftgg1 = '00'
033090050304     c                   Else
033100050304     c                   If        %subst(d§dftgg1:1:1) < '0'
033110050304     c                   Eval      %subst(d§dftgg1:1:1) = '0'
033120050304     c                   EndIf
033130050304     c                   If        %subst(d§dftgg1:2:1) < '0'
033140050304     c                   Eval      %subst(d§dftgg1:2:1) = '0'
033150050304     c                   EndIf
033160050304     c                   EndIf
033170050304     c                   If        d§dftgg2 = *Blanks
033180050304     c                   Eval      d§dftgg2 = '00'
033190050304     c                   Else
033200050304     c                   If        %subst(d§dftgg2:1:1) < '0'
033210050304     c                   Eval      %subst(d§dftgg2:1:1) = '0'
033220050304     c                   EndIf
033230050304     c                   If        %subst(d§dftgg2:2:1) < '0'
033240050304     c                   Eval      %subst(d§dftgg2:2:1) = '0'
033250050304     c                   EndIf
033260050304     c                   EndIf
033270050304
033280050304      * Calcolo la data max x msg info
033290050304     c                   Move      d§dftgg1      w0020
033300050304     c                   Move      v1dao         dataeur
033310050304     c                   Move      dataeur       dataiso
033320050304     c                   Adddur    w0020:*D      dataiso
033330050304     c                   Move      dataiso       dtmaxinfo
033340050304
033350050304      * Calcolo la data max x msg bloccante
033360050304     c                   Move      d§dftgg2      w0020
033370050304     c                   Move      dataeur       dataiso
033380050304     c                   Adddur    w0020:*D      dataiso
033390050304     c                   Move      dataiso       dtmaxblocco
033400050302
033410050302     c                   EndSr
033420001020      **********************************************************************
033430001023      * VERIFICA SE CI SONO PROPOSTE DI VARIAZIONE
033440001020      **********************************************************************
033450001020     C     Sr_Orp        begsr
033460001020
033470001020     C                   eval      *in57 = *off
033480001020
033490001020     C     kfnor         setll     fnorp01l
033500001020     C                   do        *hival
033510140227     C     kfnor         reade(n)  fnorp01l
033520001020     C                   if        %eof(fnorp01l)
033530001020     C                   leave
033540001020     C                   endif
033550001103     C                   if        ORPfev <> *blanks
033560001023     C                   iter
033570001023     C                   endif
033580001020     C                   eval      *in57 = *on
033590001023     C                   leave
033600001020     C                   enddo
033610001020
033620001020     C                   endsr
033630061003
033640061003      *--------------------------------------------------------------------*
033650061003      * verifico se ci sono delle note per l'ORM
033660061003      *--------------------------------------------------------------------*
033670061003     c     sr_orn        begsr
033680061003
033690061003     c                   eval      *in91 = *off
033700061003
033710061003     c     kfnor         setll     fnorn01l
033720061003     c                   do        *hival
033730061003     c     kfnor         reade(n)  fnorn01l
033740061003     c                   if        %eof(fnorn01l)
033750061003     c                   leave
033760061003     c                   endif
033770061003     c                   eval      *in91 = *on
033780061003     c                   leave
033790061003     c                   enddo
033800061003
033810061003     c                   endsr
033820061003
033830061003      *--------------------------------------------------------------------*
033840061003      * controllo se p.o. ritiro estero
033850061003      *--------------------------------------------------------------------*
033860061003     c     sr_por        begsr
033870061003
033880061003     c                   eval      *in21 = *off
033890061003     c                   clear                   v1dfcs
033900061003
033910061003     c                   Clear                   Og143
033920131030     c                   clear                   ntw_V1Cpor
033930131112     c                   clear                   sav_ORGfl1
033940131209     c     num_v1cpor    chain     azorg01l
033950061003     c                   if        %found(azorg01l)
033960061003     c                   eval      og143 = orgde3
033970061003     c                   endif
033980131030     c                   eval      ntw_V1Cpor = §OGntw
033990131112     c                   eval      sav_ORGfl1 = ORGfl1
034000061003      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
034010061003      * europolitan invece ormai è obsoleta
034020131030     c                   if        ntw_V1Cpor = 'DPD' or ntw_V1Cpor = 'EEX'
034030061003      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
034040061003      * partener se caricata
034050131209     c                   if        v1fcs <> *blanks
034060061003     c                   clear                   tibs02ds
034070061003     c                   eval      t02mod = 'C'
034080061003     c                   eval      t02sif = knsif
034090061003     c                   eval      t02cod = 'MIP'
034100131209     c                   eval      t02ke1 = v1fcs
034110061003     c                   call      'TIBS02R'
034120061003     c                   parm                    kpjba
034130061003     c                   parm                    tibs02ds
034140061003     c                   eval      v1dfcs = t02uni
034150061003     c                   eval      *in21 = *on
034160061003     c                   endif
034170061003     c                   endif
034180061003
034190061003     c                   endsr
034200061003
034210001109      **********************************************************************
034220001109      * COPIA ORM CON NUOVO NUMERO VIAGGIO
034230001109      **********************************************************************
034240010906     C     Sr_Copia      begsr
034250001109
034260001109      * Disalloca ORM
034270001109     C                   unlock    fnorm01l
034280071030     C                   unlock    fnorg01l
034290001109      * Cerca ultimo viaggio
034300001109     C                   eval      knrv = 99
034310001109     C     kfnor         setgt     fnorm01l
034320001109     C     kfnor1        readpe(n) fnorm01l
034330001109     C                   if        %eof(fnorm01l)
034340001109     C                   eval      *in28 = *on
034350001109     C                   else
034360010117     C                   if        ORMnrv = 99
034370010117     C                   eval      *in28 = *on
034380010117     C                   else
034390001109     C                   eval      v1nrv = (ORMnrv +1)
034400001109     C                   eval      knrv  = v1nrv
034410010618
034420010618      * Imposto la data e ora chiamata/immissione
034430010618     C                   eval      v1dao = oggi_ggmmaa
034440131211     C                   eval      v1oao = %dec(%time())
034450010911     C                   clear                   wlbdat
034460010911      * giro la data per controllarla con l'inizio della pikup area
034470010911     C                   z-add     v1dao         G02dat
034480010911     C                   movel     *blanks       G02err
034490010911     C                   call      'XSRDA8'
034500010911     C                   parm                    wlbdat
034510010911     C                   z-add     G02inv        inv_v1dao
034520121119      /free
034530170323       //?Pulisco la data pronta merce, in questo modo
034540170323       //?o la imposta l'utente o in automatico il pgm la imposta = data immissione
034550170323       clear V1dpm;
034560170323       clear sav_V1dpm;
034570170323       clear inv_V1dpm;
034580121119       //?Se filiale ritiro NTW DPD imposto il flag del NTW DPD
034590121119       //?nella routine sr_por cerco il ntw della filiale ritiro
034600131030       IF  ntw_V1Cpor = 'DPD';
034610121119         V1ntwd = 'D';
034620121119       ENDIF;
034630121119      /end-free
034640010618
034650130918      * imposto flag per non far fare dei controlli nella sr_contrcapr
034660130918      * controlli inutili oper il calcolo della data, ora mi serve solo la tisi95ds
034670130918      * e la filiale ritiro
034680110121     c                   eval      $copia='S'
034690040615     C                   exsr      Sr_Contrcapr
034700110121     c                   eval      $copia=' '
034710140320      /free
034720140604       //?I dati mail e SMS non sono da duplicare
034730160406       //?l'alert non va riportato sull'orm copiato
034740140604         clear w03sms;
034750140604         clear w03mail;
034760160406         clear w04sms;
034770160406         clear w04mail;
034780160405         clear dOREgen;
034790160406       //?ma attivo F13 nel caso l'utente voglia immettere i dati per la conferma
034800160406         *in88 = *on;
034810140320      /end-free
034820140320
034830130918      * calcolo la data di ritiro
034840040615     c                   movel     v1cpor        num_v1cpor
034850040615     C                   ExSr      Sr_CalPikup
034860040615     C                   eval      v1dar = new_v1dar
034870010618
034880010518      * Azzero la data di stampa e i dati relativi alla distinta
034890010207     C                   z-add     *zeros        v1dst
034900010518     C                   z-add     *zeros        v1npg
034910010518     C                   z-add     *zeros        v1ndc
034920010518     C                   z-add     *zeros        v1ddc
034930140709      * pulisco anche il dato relativo al tipo comunicazione partner
034940140709     c                   clear                   v1fcs
034950140709     c                   clear                   v1dfcs
034960140709     c                   eval      *in21 = *off
034970140709
034980010417     C                   eval      *in09 = *off
034990031106
035000141211      * imposto che il tipo comunicazione di default
035010150220       //?ma solo l'orm che sto copiando NON è un ORM MAIL
035020150220       //?in questo caso devo lasciare MAIL
035030150220     c                   IF        V1Ctco <> 'E'
035040141211     c                   Eval      V1cTco = d§dfttco
035050031106     C                   exsr      Sr_Contrtco
035060150220     c                   ENDIF
035070010117     C                   endif
035080071023
035090071023      * pulisco il giro
035100071023     c                   clear                   v1tgi
035110071023     c                   clear                   v1ccgi
035120071023     c                   clear                   sav_v1ccgi
035130001109     C                   endif
035140001109
035150001109     C                   endsr
035160001109      **********************************************************************
035170001109      * VIDEATA PER ORM ERRATO IN COPIA
035180001109      **********************************************************************
035190001109     C     Sr_ErrCopia   begsr
035200001109
035210001109     C                   eval      v4cpoe = ppoe
035220001109     C                   eval      v4nsr  = pnsr
035230001109     C                   eval      v4nor  = pnor
035240001109     C                   eval      v4nrv  = pnrv
035250001109     C                   eval      v4poe1 = ppoe
035260001109     C                   eval      v4nsr1 = pnsr
035270001109     C                   eval      v4nor1 = pnor
035280001109     C                   eval      v4nrv1 = knrv
035290001109
035300001109     C                   exfmt     fior05e
035310001109
035320001109     C                   endsr
035330001109      **********************************************************************
035340001109      * DECODIFICHE
035350001109      **********************************************************************
035360001109     C     Sr_Decodifica begsr
035370001109
035380010223      * Fase
035390010223     C                   if        v1fao <> *zeros
035400010223     C                   clear                   TIBS02DS
035410010223     C                   movel     'C'           T02mod
035420010223     C                   movel     knsif         t02sif
035430010223     C                   movel     'FAR'         t02cod
035440010223     C                   movel(p)  v1fao         T02ke1
035450010223     C                   call      'TIBS02R'
035460010223     C                   parm                    KPJBA
035470010223     C                   parm                    TIBS02DS
035480010314     C                   movel     t02uni        dfar
035490010314     C                   movel     d§fardes      v1dfao
035500010223     C                   endif
035510010223      * Tipo Ordine
035520010223     C                   if        v1ctor <> *blanks
035530010223     C                   clear                   TIBS02DS
035540010223     C                   movel     'C'           T02mod
035550010223     C                   movel     knsif         t02sif
035560010223     C                   movel     'TOR'         t02cod
035570010223     C                   movel(p)  v1ctor        T02ke1
035580010223     C                   call      'TIBS02R'
035590010223     C                   parm                    KPJBA
035600010223     C                   parm                    TIBS02DS
035610010223     C                   movel     t02uni        v1dtor
035620010223     C                   endif
035630010223      * Tipo comunicazione
035640010223     C                   if        v1ctco <> *blanks
035650010223     C                   clear                   TIBS02DS
035660010223     C                   movel     'C'           T02mod
035670010223     C                   movel     knsif         t02sif
035680010223     C                   movel     'TCO'         t02cod
035690010223     C                   movel(p)  v1ctco        T02ke1
035700010223     C                   call      'TIBS02R'
035710010223     C                   parm                    KPJBA
035720010223     C                   parm                    TIBS02DS
035730010223     C                   movel     t02uni        v1dtco
035740010223     C                   endif
035750161027      /free
035760161027       //?Se tipo comunicazione fisso imposto anche il n. del fisso
035770161027       IF  V1Ctco = 'S';
035780161027         V1Dtco = %trim(V1Dtco) + ' ' + %editc(V1cpos:'X') + '-' +
035790161027                                  %trim(%editc(V1ors:'4'));
035800161027       ENDIF;
035810010223      * Priorità
035820010223     C                   if        v1sto <> *zeros
035830010223     C                   clear                   TIBS02DS
035840010223     C                   move      *all'0'       priorita
035850010223     C                   movel     'C'           T02mod
035860010223     C                   movel     knsif         t02sif
035870010223     C                   movel     'STO'         t02cod
035880010223     C                   move      v1sto         priorita
035890010223     C                   movel(p)  priorita      T02ke1
035900010223     C                   call      'TIBS02R'
035910010223     C                   parm                    KPJBA
035920010223     C                   parm                    TIBS02DS
035930010223     C                   movel     t02uni        v1dsto
035940010223     C                   endif
035950010202      * P.O. ritiro
035960010202     C                   movel     v1cpor        num_v1cpor
035970010202     C                   movel     v1cpor        kazorg
035980150310     c                   clear                   og148
035990010202     C     kazorg        chain     azorg01L
036000010202     C                   if        %found(azorg01l)
036010010202     C                   movel     ORGdes        v1dpor
036020150310     C                   movel     ORGde8        og148
036030010202     C                   endif
036040150310      * filiale ritiro abilitata allo scarico PDA
036050150310     c                   eval      *in22 = (§ogpdaorm <> *blanks)
036060070913      * giro
036070070913     c                   exsr      ctrgiro
036080070913      *     imposto la descrizione del giro
036090070913     c                   if        d09ocgi <> ' '
036100070913     c                   eval      v1dcgi = d09odes
036110070913     c                   endif
036120010223      * P.O. consegna
036130010223     C                   movel     v1cpoc        kazorg
036140010223     C     kazorg        chain     azorg01L
036150010223     C                   if        %found(azorg01l)
036160010223     C                   movel     ORGdes        v1dpoc
036170010223     C                   endif
036180010202      * Orm fisso emetto il numero ritiro fisso
036190001109     C                   eval      *in76 = (v1ors <> *zeros)
036200050502
036210050502      * Visualizzo che ORM con ricevuta di ritiro
036220050502     c                   Eval      *In11 = (§orsrm = 'S')
036230070806
036240070806      * controllo anche se ci sono note
036250131210     c                   exsr      sr_orn
036260140130
036270140130      /free
036280140130         *in96 = *off;
036290140130       //?Controllo se ci sono Note AUT
036300140130         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
036310140130         IF  %found(FNORT01L);
036320140130           *in96 = *on;
036330140130         ENDIF;
036340140130      /end-free
036350140130
036360070806      * controllo anche se p.o. ritiro estero
036370070806     c                   exsr      sr_por
036380070806      * controllo se p.o. ritiro DPD o se p.o. emissione DPD per abilitare F2=Dati DPD
036390131209     c     V1Cpoe        lookup    skpodpd                                30
036400131209     c  n30num_V1Cpor    lookup    skpodpd                                30
036410070806     c                   eval      *in18 = (*in30)
036420131003
036430131003      * Richiamo calcolo della data
036440131003      * per avere le date max blocco e max info
036450131003     c                   ExSr      Sr_CalPikup
036460161103
036470161103      /free
036480161103       //?Se non ho la data pronta merce imposto la data ritiro
036490161103         IF  V1dpm = 0;
036500161103           V1dpm = V1dar;
036510161103           sav_V1dpm = V1dpm;
036520161103           inv_v1dpm = old_invv1dar;
036530161103         ENDIF;
036540161103      /end-free
036550070806
036560070806     C                   endsr
036570070119
036580070119      *--------------------------------------------------------------------*
036590070119      * Carico dati sensibili per PDA (salvo)
036600070119      *--------------------------------------------------------------------*
036610070119     c     sr_datipdasav begsr
036620070119
036630070119     c                   clear                   datipdasav
036640080321
036650080321      * se non è una conferma proposta di variazione salvo i dati del video
036660080321     c                   if        §rmfpr <> 'P'
036670070119     c                   eval      pdasavrsr = v1rsr
036680070119     c                   eval      pdasavinr = v1inr
036690070119     c                   eval      pdasavlor = v1lor
036700070119     c                   eval      pdasavprr = v1prr
036710070119     c                   eval      pdasavrer = v1rer
036720070119     c                   eval      pdasavter = v1ter
036730070119     c                   eval      pdasavorr = v1orr
036740070119     c                   eval      pdasavncl = v1ncl
036750070119     c                   eval      pdasavpkg = v1pkg
036760070119     c                   eval      pdasavvlm = v1vlm
036770070119     c                   eval      pdasavbnc = v1bnc
036780070119     c                   eval      pdasavno1 = v1not1
036790070119     c                   eval      pdasavno2 = v1not2
036800131107      /free
036810131107        pdasavora1 = V1OraAMda;
036820131107        pdasavora2 = V1OraAMa;
036830131107        pdasavora3 = V1OraPMda;
036840131107        pdasavora4 = V1OraPMa;
036850131107      /end-free
036860080321     c                   endif
036870080321
036880080321      * se è una conferma proposta di variazione salvo i dati dal file
036890140122     c                   if        §rmfpr = 'P'
036900080321     c                   eval      pdasavrsr = ormrsr
036910080321     c                   eval      pdasavinr = orminr
036920080321     c                   eval      pdasavlor = ormlor
036930080321     c                   eval      pdasavprr = ormprr
036940080321     c                   eval      pdasavrer = ormrer
036950080321     c                   eval      pdasavter = ormter
036960080321     c                   eval      pdasavorr = ormorr
036970080321     c                   eval      pdasavncl = ormncl
036980080321     c                   eval      pdasavpkg = ormpkg
036990080321     c                   eval      pdasavvlm = ormvlm
037000080321     c                   eval      pdasavbnc = ormbnc
037010080321     c                   eval      pdasavno1 = ormno1
037020080321     c                   eval      pdasavno2 = ormno2
037030131107      /free
037040140124        pdasavora1 = w§OREoramda;
037050140124        pdasavora2 = w§OREorama;
037060140124        pdasavora3 = w§OREorapda;
037070140124        pdasavora4 = w§OREorapa;
037080131107      /end-free
037090080321     c                   endif
037100070119
037110070119     c                   endsr
037120070119
037130001010      **********************************************************************
037140001010      * CONTROLLI
037150001010      **********************************************************************
037160001010     C     Sr_Contr      begsr
037170001006
037180001009      * Reset indicatori
037190001010     C                   setoff                                       414243
037200001010     C                   setoff                                       444546
037210001010     C                   setoff                                       474849
037220161020     C                   setoff                                       505152
037230001010     C                   setoff                                       535455
037240001023     C                   setoff                                       565859
037250001011     C                   setoff                                       606162
037260001011     C                   setoff                                       636465
037270161108     C                   setoff                                         7475
037280131211     C                   setoff                                       778284
037290010214     C                   setoff                                       858687
037300160406     C                   setoff                                       35
037310130611
037320130611     c                   eval      wintcap = *off
037330071106
037340071126     c  n10              eval      $proposte = *off
037350071108     c                   eval      $modifica = *off
037360131018
037370131018     c                   clear                   h1riga
037380131018     c                   clear                   h1colo
037390010223
037400011113      * Tipo ORM
037410081126     C                   exsr      Sr_Contrtor
037420010223     C   90
037430010223     Cor 28              goto      endsrcontr
037440010223
037450011113      * Tipo comunicazione ORM
037460081126     C                   exsr      Sr_Contrtco
037470150615     C   90
037480150615     Cor 28              goto      endsrcontr
037490001009
037500011113      * Cliente mittente
037510010117     C                   exsr      Sr_Contrclir
037520001011     C   28              goto      endsrcontr
037530131211
037540130507      * se richiamato il pgm di interrogazione cappario riemetto la videata
037550130507      * e mi posiziono sui colli
037560130507     c                   IF        wintcap
037570130507     c                   eval      *in43 = *on
037580140109     c                   eval      *in90 = *on
037590130507     c                   leavesr
037600130507     c                   ENDIF
037610001006
037620130508      * Colli - Peso - Bancali - Volume
037630001010     C                   exsr      Sr_Contrqta
037640001020     C   28              goto      endsrcontr
037650150921
037660150921      /free
037670161025       //?Se immissione/copia
037680161025       //?e non c'è la filiale ritiro
037690161025       //?calcolo peso/volume
037700161025       //?calcolo filiale ritiro
037710161025         IF  (*in01 or *in03) and
037720161025              V1Cpor = *blanks;
037730161025           exsr sr_pesvol;
037740161025           exsr CALPOR;
037750161025       //?Recupero gli ambiti per il recupero del giro da ACR
037760161025           exsr sr_caragr;
037770161025           IF  *in28;
037780161025             leavesr;
037790161025           ENDIF;
037800150921         ENDIF;
037810161019
037820161019       //?Controllo la data pronta merce
037830161020         exsr ContrDataProntaMerce;
037840161019         IF  *in28;
037850161020           leavesr;
037860161019         ENDIF;
037870161019
037880150921      /end-free
037890010223
037900010223      * Referente e Telefono
037910010223     C                   exsr      Sr_Contrref
037920010223     C   28              goto      endsrcontr
037930131014
037940131014      /free
037950131112       //?Controllo gli orari di apertura
037960131112         exsr OrariApertura;
037970131014         IF  *in28;
037980131014           leavesr;
037990131014         ENDIF;
038000131014      /end-free
038010001010
038020010202      * Pagamento
038030001011     C                   exsr      Sr_Contrpag
038040001020     C   28              goto      endsrcontr
038050001010
038060010117      * Cliente destinatario
038070010117     C                   exsr      Sr_Contrclid
038080001011     C   28              goto      endsrcontr
038090010208
038100010208      * Cliente ordinante
038110010208     C                   exsr      Sr_Contrclio
038120010208     C   28              goto      endsrcontr
038130050323
038140050323      * se ORM da FILE il codice del cliente ritiro deve essere un figlio del codice ordinante
038150050323      * metto il controllo in questo punto xchè ho già tutti i clienti decodificati
038160140319      * faccio il controllo se i 2 codici sono <> come ksc lungo 7
038170050323     c                   If        v1ctco = 'F' and ds_v1ccor <> *Zeros and
038180050323     c                             ds_v1ccra <> *Zeros
038190140319     c                             and ds_v1cra12 <> ds_v1cor12
038200050323     c                   ExSr      Sr_Figli
038210050323     c                   Move      ds_v1cra12    w0110
038220050323     c     w0110         Lookup    Skfigli                                30
038230050323     c                   If        Not *In30
038240050323     c                   Eval      v1cmsg = msg(86)
038250050323     c                   Eval      *In28 = *On
038260050323     c                   Eval      *In41 = *On
038270050323     c                   LeaveSr
038280050323     c                   EndIf
038290050323     c                   EndIf
038300130508
038310130508      * P.O. Consegna
038320130508     C                   exsr      Sr_Contrpoc
038330130508     C   90
038340130508     Cor 28              goto      endsrcontr
038350040630
038360040630      * Cod.Cliente - Cod.Tariffa
038370040630     C                   exsr      Sr_Contrksc
038380040630     C   28              goto      endsrcontr
038390010201
038400010201      * P.O. ritiro
038410010201     C                   exsr      Sr_Contrpor
038420130412      * controllo se devo proteggere il giro
038430150305      * se non è già protetto
038440150305     c                   if        %lookup(%editc(num_v1cpor:'X'):skpog) = 0
038450071108     c                             and not *in24
038460070924     c                   eval      *in24 = *on
038470070924     c                   endif
038480131202      /free
038490150305       //?Se il giro è protetto ma la ritiro è una filiale gestita dall'utente
038500131202       //?devo sproteggere il campo del giro
038510131202       //?se sono in immissione
038520150305         IF  *in24 and *in01 and
038530150305           %lookup(%editc(num_v1cpor:'X'):skpog) > 0;
038540131202           *in24 = *off;
038550131202         ENDIF;
038560131202      /end-free
038570010201     C   90
038580010201     Cor 28              goto      endsrcontr
038590140122
038600140122      /free
038610110610       //?Controlli per ORM Export e destinatario merce
038620110610       //?Se Export EEX non posso inserire destinatari esteri
038630110610         IF  *in14 and V1nac <> *blanks;
038640110610           V1Cmsg = msg(95);
038650110610           *in28 = *on;
038660110610           *in65 = *on;
038670110613           *in75 = *on;
038680110610           *in90 = *on;
038690110610           leavesr;
038700110610         ENDIF;
038710110610       //?Se Export DPD non posso inserire destinatari di San Marino
038720110610       //?                                                Città del Vaticano
038730110610       //?                                                Livigno
038740110613       //?Richiamo pgm esterno
038750110613         IF  *in15;
038760110613           clear FIOR95ds;
038770110613           IOR95cap = V1cac;
038780110613           IOR95loc = V1loc;
038790110613           IOR95prv = V1prc;
038800110613           IOR95naz = V1nac;
038810110613           fior95r(kpjba : fior95ds);
038820110613           IF  OOR95err <> *blanks;
038830110613             V1Cmsg = OOR95msg;
038840110613             *in28 = *on;
038850110613             *in63 = *on;
038860110613             *in75 = *on;
038870110613             *in90 = *on;
038880110613             leavesr;
038890110613           ENDIF;
038900110613           IF  OOR95ok = 'N';
038910110613             V1Cmsg = OOR95msg;
038920110613             *in28 = *on;
038930110613             *in63 = *on;
038940110613             *in75 = *on;
038950110613             *in90 = *on;
038960110613             leavesr;
038970110613           ENDIF;
038980110613         ENDIF;
038990110610      /end-free
039000071031
039010071031      * calcolo peso o volume se non impostati a video
039020071031      * considerando anche le cubature degli automezzi eventualmente immessi
039030071031      * faccio ora questo calcolo perchè mi serve per recuperare il giro
039040140109      * ma solo se non è chiusura ORM e non ho fatto F5-Altri dati
039050131210     c                   IF        not *inke and not *in10
039060131209     c                   exsr      sr_pesvol
039070131209     c                   exsr      sr_contrcgi
039080160325     c                   IF        wModGiroAut
039090160325     c                   exsr      Sr_Tastifun
039100160325     c                   leavesr
039110160325     c                   ENDIF
039120131212     c                   exsr      Sr_Tastifun
039130161109     c   28              eval      v1cmsg = msg(73)
039140161109     c   28              leavesr
039150131209     c                   ENDIF
039160161024
039170100204      * Personalizzazioni ORMFLO da tavella PVO
039180100205      * solo in immissione ORM da menù
039190100204      * faccio la personalizzazione in questo punto perchè così nelle specifiche dopo
039200100204      * faccio i controlli del commissionato e della data ritiro che vengono
039210100204      * modificati da questa routine
039220100205     c                   if        *in01 and §rmtla = *blanks
039230100204     c                   exsr      sr_pvo
039240100205      * Se blocco destinatario devo ricontrollare i dati del destinatario
039250100205     c                   if        §ormfd = 'S'
039260100205     c                   exsr      sr_contrclid
039270100205     c   28              leavesr
039280100205     c                   endif
039290100205      * devo sbloccare il campo di ORM commissinato nel caso in cui lo avessi bloccato
039300100205      * perchè orm con ricevuta ritiro
039310100208     c                   if        §orsrm = 'S'
039320100205     c                   eval      *in34 = *off
039330100205     c                   endif
039340100205      * devo pulire il campo di orm commissionato così il pgm rifà il calcolo in automatico
039350100208     c                   if        §orcomc <> *blanks
039360100205     c                   clear                   v1com
039370100205     c                   endif
039380100204     c                   endif
039390050322
039400050322      * ORM COMMISSIONATO
039410050322     c                   If        *In01 or *In03
039420050322     c                   Exsr      Sr_ormcom
039430050322      * se è un orm commissionato controllo se ok tutti i dati
039440050322     c                   Exsr      Sr_Contrcom
039450050322     c   28
039460050322     cor 90              goto      endsrcontr
039470050324      * se ORM prepagato non locale deve essere un ORM COMMISSIONATO
039480050324     c                   If        v1com = 'N' and v1ctor = 'P' and
039490150305     c                             num_v1cpor <> DUTpou
039500050324     c                   Eval      v1cmsg = msg(87)
039510050324     c                   Eval      *In28 = *On
039520050324     c                   Eval      *In35 = *On
039530050324     c                   Leavesr
039540050324     c                   EndIf
039550050322     c                   EndIf
039560160405
039570160405      /free
039580160405       //?Se sono in immissione manuale (telefonica/mail)
039590160405       //?O copia di un ORM manuale (telefonica/mail)
039600160405       //?e non è un mittente estero
039610160405       //?e l'ordinante è un codificato
039620160405       //?e le quantità non superano i limiti impostati in tabella
039630160405         IF  ((*in01 or *in03) and
039640160405              (V1Ctco = 'M' or V1Ctco = 'E')) and
039650160405             ((ds_V1Ccra > 0 and
039660160405               m_§OGntw <> 'EEX' and m_§OGntw <> 'DPD' and
039670160405               m_§OGntw <> 'FED') or
039680160405              (ds_V1Ccra = 0 and V1nar = *blanks)) and
039690160405              (ds_V1Ccor > *zeros and
039700160405               ds_V1cor2 <> 9999 and ds_V1cor2 <> 8888) and
039710160405              (V1pkg <= d§DFTpkga and V1vlm <= d§DFTvlma and
039720160405               V1bnc <= d§DFTbnca);
039730160405       //?Prima del calcolo della data di ritiro se ORM commissionato e non ho
039740160405       //?ancora inserito i dati dell'alert devo richiederli
039750160405       //?se non è un ORM prepagato
039760160405           IF  V1com = 'S' and not wRicAlert and
039770160405               w03sms = *blanks and w03mail = *blanks and
039780160405               V1Ctor <> 'P';
039790160405             exsr F04D01;
039800160405             wRicAlert = *on;
039810160405         //?Imposto che devo ricalcolare la data di ritiro se = oggi
039820160405             IF  V1dar > 0 and V1dar = oggi_ggmmaa;
039830160405               wCalDtRit = *on;
039840160405             ENDIF;
039850160405           ENDIF;
039860160405         ENDIF;
039870160405      /end-free
039880050322
039890050322      * calcolo la data di ritiro
039900050322     C                   If           v1dar = *zeros
039910050322      * calcolo la data di ritiro sempre se è una conferma da VAO
039920130918      * anche richiesta dall'ordinante sul VAO
039930130919     c                             or §rmFpr = 'C'
039940050322      * calcolo la data di ritiro sempre se variato il flag di ORM COMMISSIONATO
039950050322     c                             or sav_v1com <> v1com
039960131024      * calcolo la data di ritiro sempre se variato il mittente
039970131211      * solo come codice
039980131024     c                             or sav_dsV1Ccra <> ds_V1Ccra
039990170208      * calcolo la data di ritiro sempre se variato l'ordinante
040000170208      * solo come codice
040010170208     c                             or sav_dsV1Ccor <> ds_V1Ccor
040020140508      * calcolo la data di ritiro sempre se variato cap/località/provincia
040030140509     c                             or wModLocRit
040040140509      * o se ORM con Alert
040050140508     c                             or wCalDtRit
040060161025      * o se variato il peso
040070161025     c                             or wModPeso
040080161028      * o variata la data pronta merce
040090161028     c                             or wModDpm
040100161116      * o variata la filiale di ritiro
040110161116     c                             or wModPor
040120050322     c                   movel     v1cpor        num_v1cpor
040130050322     c                   ExSr      Sr_CalPikup
040140130918      * imposto la data di ritiro
040150050322     C                   if        v1dar = *zeros
040160050322     C                             or (v1com <> sav_v1com and
040170050322     C                                 v1dar = old_newv1dar)
040180161026     c                             or sav_dsV1Ccra <> ds_V1Ccra
040190170208     c                             or sav_dsV1Ccor <> ds_V1Ccor
040200140509     c                             or wModLocRit
040210140509     c                             or wCalDtRit
040220161025     c                             or wModPeso
040230161028     c                             or wModDpm
040240161116     c                             or wModPor
040250050322     C                   eval      v1dar = new_v1dar
040260140529      /free
040270140529       //?se conferma ORM da VAO e la data ricalcolata è inferiore a oggi
040280140529       //?imposto oggi
040290140530        IF  §RMfpr = 'C';
040300140530           clear wdata;
040310140530           dataeur = %date(v1dar:*eur);
040320140530           dataiso = dataeur;
040330140530           wdata = %dec(dataiso);
040340140530           IF  wdata < oggi_aammgg;
040350140530             v1dar = oggi_ggmmaa;
040360140530          ENDIF;
040370140529        ENDIF;
040380140529      /end-free
040390050322     C                   endif
040400050322     c                   eval      sav_v1com = v1com
040410131024     c                   eval      sav_dsV1Ccra = ds_V1Ccra
040420170208     c                   eval      sav_dsV1Ccor = ds_V1Ccor
040430140509      /free
040440140509        wModLocRit = *off;
040450140509        wCalDtRit = *off;
040460161026        wModPeso = *off;
040470161028        wModDpm = *off;
040480161116        wModPor = *off;
040490140509      /end-free
040500050322     C                   endif
040510160330
040520160330      /free
040530160330       //?Se immissione Telefonica o Mail/Fax e mittente NON codificato
040540160413       //?obbligo del n. di telefono e del referente
040550160330         IF  (*in01 or *in03) and
040560160413             (V1Ctco = 'M' or V1Ctco = 'E') and ds_V1Ccra = 0;
040570160413           IF  V1rer = *blanks;
040580160413             V1Cmsg = msg(57);
040590160413             *in28 = *on;
040600160413             *in86 = *on;
040610160413             leavesr;
040620160413           ENDIF;
040630160413           IF  V1ter = *blanks;
040640160330           V1Cmsg = msg(58);
040650160330           *in28 = *on;
040660160330           *in87 = *on;
040670160330           leavesr;
040680160413           ENDIF;
040690160330         ENDIF;
040700160330      /end-free
040710010906
040720010906      * Data ritiro
040730130918      * la controllo per ultima
040740130918      * e solo se non sto dirottanto l'ORM
040750040628     c                   If        wokdirotta = *Off
040760010906     C                   exsr      Sr_Contrdar
040770010906     C   28              goto      endsrcontr
040780040628     c                   endif
040790161109
040800161109      /free
040810161109       //?Cerco orari servizio
040820161109         exsr CercaOrari;
040830161109      /end-free
040840131024
040850131024      * Ora
040860131024     c                   exsr      Sr_Controrr
040870131024     c   28              goto      endsrcontr
040880130917
040890130918      * controllo ora pronta merce con nuovi orari di servizio
040900131022      * solo in immissione ORM
040910140317      * e copia ORM
040920140317     c                   IF        *in01 or *in03
040930130917     C                   exsr      contrORRmax
040940130917     C   28              goto      endsrcontr
040950131022     c                   ENDIF
040960040426
040970040426      * invio msg info nel caso di ORM già inserito con stesso mittente e ritiro lo stesso giorno
040980120227      * faccio il controllo anche mentre sto dirottando
040990120227     c                   If        (*In01 or *In03 or wokdirotta = *on)
041000120227     c                              and ds_v1ccra > *Zeros
041010120306     c     kOrm08        Setll     Fnorm18l
041020120306     c                   do        *hival
041030120306     c     kOrm08        reade     Fnorm18l
041040120306     c                   if        %eof(Fnorm18l)
041050120306     c                   leave
041060120306     c                   endif
041070120306     c                   if        wokdirotta = *on and
041080120306     c                             f_ormpoe = v1cpoe and f_ormnsr = v1nsr and
041090120306     c                             f_ormnor = v1nor  and f_ormnrv = v1nrv
041100120306     c                   iter
041110120306     c                   endif
041120120306     c                   If        Not *In37
041130040628     c                   Eval      V1cMsg = Msg(80)
041140040426     c                   Eval      %subst(v1cmsg:5:3) = %editc(f_OrmPoe:'4')
041150040426     c                   Move      f_OrmNsr      w002a
041160040426     c                   Eval      %subst(v1cmsg:9:2) = w002a
041170040426     c                   Eval      %subst(v1cmsg:12:7) = %editc(f_OrmNor:'4')
041180040426     c                   Move      f_OrmNrv      w002a
041190040426     c                   Eval      %subst(v1cmsg:20:2) = w002a
041200040426     c                   Eval      *In28 = *On
041210040426     c                   Eval      *In37 = *On
041220040426     c                   Eval      *In41 = *On
041230040426     c                   GoTo      endsrcontr
041240120306     c                   EndIf
041250120306     c                   enddo
041260040426     c                   EndIf
041270160405
041280160405      /free
041290160406       //?Alla fine di tutti i controlli
041300160406       //?se Conferma da VAS controllo i dati della conferma prenotazione
041310160406       //?se richiesta da chi ha inserito il ritiro
041320160406         IF  §RMfpr = 'C' and wConferma;
041330160406           exsr CtrDatiF13;
041340160406           IF  *in28;
041350160406             *in28= *off;
041360160406             wErrConferma = *on;
041370160406             leavesr;
041380160406           ENDIF;
041390160406         ENDIF;
041400160406
041410160413       //?Immissione Manuale o copia
041420160406       //?ORM con mittente NON codificato e ordinante NON codificato
041430160408       //?(sono un perfetto sconosciuto e quindi sono costretta a chiedere
041440160406       //? se vogliono la conferma della prenotazione)
041450160406       //?richiedo i dati per conferma prenotazione
041460160406       //?se non sono già impostati
041470160413         IF  (*in01 or *in03) and (V1Ctco = 'M' or V1Ctco = 'E') and
041480160406             ds_V1Ccra = 0 and ds_v1Ccor = 0 and
041490160406             W04sms = *blanks and W04mail = *blanks and
041500160405             not wRicConf;
041510160405           exsr F13D01;
041520160405           wRicConf = *on;
041530160405         ENDIF;
041540160405      /end-free
041550010223
041560010223      * Fase
041570010223     C                   exsr      Sr_Contrfao
041580071108
041590071108      * controllo se ho modificato qualcosa
041600071126      * sempre se conferma proposta di variazione
041610090211     c                   if        vidold <> vidnew or
041620131018     c                             vidold2 <> vidnew2 or
041630131018     c                             vidoldA <> vidnewA or
041640160406     c                             vidoldC <> vidnewC or
041650090206     c                             §rmfpr = 'P'
041660071108     c                   eval      $modifica = *on
041670071108     c                   endif
041680071106
041690071106      * alla fine di tutti i controlli
041700071106      * se non ho fatto un dirottamento
041710071106     c                   if        wokdirotta = *off and
041720071106      * se è manutenzione
041730071106     c                             *in02 and not *in10 and §rmtla = *blanks and
041740071106      * se è stato variato qualcosa
041750071108     c                             $modifica = *on and
041760071107      * ma non solo il giro
041770071108     c                             v1ccgi = sav_v1ccgi
041780080314      * devo --> ricalcolare il giro se ORM NON in fase 400 (assegnato) e 410
041790080314      *      --> generare proposta di variazione se ORM in fase 400 e410
041800071106      * ORM NON assegnato
041810071106      * ricalcolo il giro
041820071106     c                   if        v1fao < 400
041830071106     c                   exsr      sr_carcgi
041840071106     c                   if        sav_acrcgi <> *blanks
041850160325     c                   IF        V1Ccgi <> sav_ACRcgi
041860071106     c                   eval      v1tgi = 'A'
041870071106     c                   eval      v1ccgi = sav_acrcgi
041880071106     c                   eval      sav_v1ccgi = sav_acrcgi
041890160325      * informo l'utente che il giro è stato variato
041900161117     c                   eval      *in28 = *on
041910161117     c                   eval      *in90 = *on
041920161117     c                   eval      V1Cmsg = msg(98)
041930160325     c                   leavesr
041940160325     c                   ENDIF
041950071106     c                   else
041960160325     c                   IF        V1Ccgi <> *blanks
041970071106     c                   clear                   v1tgi
041980071106     c                   clear                   v1ccgi
041990071106     c                   clear                   sav_v1ccgi
042000160325      * informo l'utente che il giro è stato variato
042010161117     c                   eval      *in28 = *on
042020161117     c                   eval      *in90 = *on
042030161117     c                   eval      V1Cmsg = msg(98)
042040160325     c                   leavesr
042050160325     c                   ENDIF
042060071106     c                   endif
042070071106     c                   endif
042080071106      * ORM assegnato
042090071106      * scrivo proposta variazione
042100080314     c                   if        v1fao = 400 or v1fao = 410
042110071106     c                   eval      $proposte = *on
042120071106     c                   endif
042130150309      * ORM in fase 50 e NON sono in conferma ORM commissionati vuol dire
042140150309      * che sono una filiale abilitata alla manutenzione ma NON sono la filiale
042150150309      * ritiro quindi
042160150309      * scrivo proposta variazione
042170150309     c                   if        v1fao = 050 and §rmfpr <> 'O'
042180150309     c                   eval      $proposte = *on
042190150309     c                   endif
042200071106     c                   endif
042210100225
042220100224      /free
042230100224       //?Controlli per preavviso ORM in caso di conferma ORM da VAO
042240100225       //?lo faccio solo se non sono nel momento dei controlli fatti prima
042250100225       //?di confermare i dati da VAO
042260100225         IF  $immetti;
042270100225           $errpre = *off;
042280100225           IF  §rmfpr = 'C' and *in01;
042290100225             exsr sr_CtrDatiPre;
042300100225             $errpre = *in28;
042310100225           ENDIF;
042320100224         ENDIF;
042330131112
042340131112       //?se sono in immissione/copia/conferma ORM commissionati
042350131112       //?e non ho nessun errore mi devo posizionare sempre sul campo delle note
042360131112         IF  (*in01 or *in03 or §RMfpr = 'O') and not *in28;
042370131112           *in82 = *on;
042380131112         ENDIF;
042390131106
042400100224      /end-free
042410001006
042420001011     C     endsrcontr    endsr
042430010906
042440010223      **********************************************************************
042450010223      * CONTROLLI     * tipo ordine *
042460010223      **********************************************************************
042470010223     C     Sr_Contrtor   begsr
042480010223
042490010223      * Ricerca Tipo Ordine con "?"
042500010223     C     '?'           scan      v1ctor                                 30
042510010223     C     *in30         ifeq      *on
042520160316     c                   eval      H1riga = 05
042530160316     c                   eval      H1colo = 11
042540010223     C                   eval      *in90 = *on
042550010223     C                   clear                   TIBS02DS
042560010223     C                   movel     'R'           t02mod
042570010223     C                   movel     knsif         t02sif
042580010223     C                   movel     'TOR'         t02cod
042590010223     C                   call      'TIBS02R'
042600010223     C                   parm                    KPJBA
042610010223     C                   parm                    TIBS02DS
042620010223     C                   movel     T02ke1        v1ctor
042630010223     C                   movel     T02uni        v1dtor
042640010223     C                   goto      endsrctor
042650010223     C                   endif
042660010223      * Se non immesso devo mettere quello di dft
042670010223     C                   if        v1ctor = *blanks
042680010223     C                   eval      v1ctor = d§dfttor
042690010223     C                   endif
042700010223      * Decodifico
042710010223     C                   clear                   TIBS02DS
042720010223     C                   movel     'C'           T02mod
042730010223     C                   movel     knsif         t02sif
042740010223     C                   movel     'TOR'         t02cod
042750010223     C                   movel(p)  v1ctor        T02ke1
042760010223     C                   call      'TIBS02R'
042770010223     C                   parm                    KPJBA
042780010223     C                   parm                    TIBS02DS
042790010223     C                   if        t02err <> *blanks
042800010223     C                   movel     msg(13)       v1cmsg
042810160316     c                   eval      H1riga = 05
042820160316     c                   eval      H1colo = 11
042830160316     C                   seton                                            28
042840010223     C                   goto      endsrctor
042850010223     C                   endif
042860010223     C                   movel     t02ke1        v1ctor
042870010223     C                   movel     t02uni        v1dtor
042880010223
042890010223     C     endsrctor     endsr
042900010223      **********************************************************************
042910010223      * CONTROLLI     * tipo comunicazione ordine *
042920010223      **********************************************************************
042930010223     C     Sr_Contrtco   begsr
042940150615
042950150615      * Ricerca Tipo Ordine con "?"
042960150615     C     '?'           scan      v1ctco                                 30
042970150615     C     *in30         ifeq      *on
042980160316     c                   eval      H1riga = 04
042990160316     c                   eval      H1colo = 16
043000150615     C                   eval      *in90 = *on
043010150615     C                   clear                   TIBS02DS
043020150615     C                   movel     'R'           t02mod
043030150615     C                   movel     knsif         t02sif
043040150615     C                   movel     'TCO'         t02cod
043050150615     C                   call      'TIBS02R'
043060150615     C                   parm                    KPJBA
043070150615     C                   parm                    TIBS02DS
043080150615     C                   movel     T02ke1        v1ctco
043090150615     C                   movel     T02uni        v1dtco
043100150615     C                   leavesr
043110150615     C                   endif
043120010223
043130150615      * Se non immesso devo mettere quello di dft
043140010223     C                   if        v1ctco = *blanks
043150010223     C                   eval      v1ctco = d§dfttco
043160010223     C                   endif
043170010223
043180010223     C                   if        v1ctco <> *blanks
043190010223     C                   clear                   TIBS02DS
043200010223     C                   movel     'C'           T02mod
043210010223     C                   movel     knsif         t02sif
043220010223     C                   movel     'TCO'         t02cod
043230010223     C                   movel(p)  v1ctco        T02ke1
043240010223     C                   call      'TIBS02R'
043250010223     C                   parm                    KPJBA
043260010223     C                   parm                    TIBS02DS
043270150615     C                   if        t02err <> *blanks
043280150615     C                   movel     msg(97)       v1cmsg
043290160316     c                   eval      H1riga = 04
043300160316     c                   eval      H1colo = 16
043310160316     C                   seton                                            28
043320150615     C                   leavesr
043330150615     C                   endif
043340010223     C                   movel     t02uni        v1dtco
043350161028      /free
043360161028       //?Se tipo comunicazione fisso imposto anche il n. del fisso
043370161028       IF  V1Ctco = 'S';
043380161028         V1Dtco = %trim(V1Dtco) + ' ' + %editc(V1cpos:'X') + '-' +
043390161028                                  %trim(%editc(V1ors:'4'));
043400161028       ENDIF;
043410150615     c                   IF        *in04 and V1Ctco <> 'M' and
043420150615     c                             V1Ctco <> 'E'
043430150615     C                   movel     msg(97)       v1cmsg
043440150615     C                   eval      V1Cmsg = %trim(V1Cmsg) +
043450160405     C                             ' Ammesso solo MAIL/FAX o TELEFONICO'
043460160316     c                   eval      H1riga = 04
043470160316     c                   eval      H1colo = 16
043480160316     C                   seton                                            28
043490150615     C                   leavesr
043500150615     C                   endif
043510010223     C                   endif
043520010223
043530010223     C                   endsr
043540010117      **********************************************************************
043550010117      * CONTROLLI     * cliente ritiro *
043560010117      **********************************************************************
043570010117     C     Sr_Contrclir  begsr
043580010117
043590161026     c                   eval      wModCodCra = *off
043600010117     C                   eval      ds_v1cra1 = v1cra1
043610010117     C                   eval      ds_v1cra2 = v1cra2
043620010117     C                   eval      ds_v1cra3 = v1cra3
043630010117      * Obbligatorio
043640010117     C                   if        ds_v1ccra = *zeros
043650010117     C                             and v1rsr = *blanks
043660010117     C                   movel     msg(1)        v1cmsg
043670010117     C                   seton                                        41  28
043680130712     c                   eval      *in07 = *off
043690010117     C                   goto      endsrclir
043700010117     C                   endif
043710010117      * Inserito codice
043720010117     C                   if        ds_v1ccra <> *zeros
043730010117     C                   eval      *in07 = *on
043740010117     C                   exsr      Sr_Contrcra
043750010117     C   28              goto      endsrclir
043760131114      * Se variato codice recupero i dati da anagrafica clienti ritiro
043770010117     C                   if        ds_v1ccra <> sav_v1ccra
043780160325     c                   eval      wModCodCra = *on
043790160208     c                   eval      SoloUnaVolta = *off
043800010117     C                   exsr      Sr_Caranacr
043810070920     C  nkf              eval      sav_v1ccra = ds_v1ccra
043820010417     C                   eval      *in89 = *off
043830131114      /free
043840131114       //?Se NON variato codice e sto facendo conferma ORM fissi
043850131114       //?devo recuperare da anagrafica clienti ritiro gli orari di apertura
043860160208       //?ma solo una volta, poi l'utente può fare ciò che vuole
043870131114         ELSE;
043880160208           IF  §RMtco = 'S' and not SoloUnaVolta;
043890131125             exsr CaricaOrari;
043900160208             SoloUnaVolta = *on;
043910131125           ENDIF;
043920131114      /end-free
043930010117     C                   endif
043940010117     C                   else
043950160325     c                   IF        sav_v1ccra <> 0
043960160325     c                   eval      wModCodCra = *on
043970160325     c                   ENDIF
043980010117      * Inserita ragione sociale
043990010117     C                   clear                   sav_v1ccra
044000010305     C                   clear                   sav_acrpoa
044010131022     C                   clear                   sav_acrorr
044020010413     C                   clear                   sav_v1pkg
044030150921     C                   clear                   sav_v1vlm
044040070920     C                   clear                   sav_acrcgi
044050161025     c                   clear                   sav_acrfcl
044060161025     c                   clear                   sav_acrfpk
044070161025     c                   clear                   sav_acrfmc
044080161025     c                   clear                   sav_acrfbn
044090161025     c                   clear                   sav_acrfbl
044100161025     c                   clear                   sav_acrfat
044110161025     c                   clear                   sav_acrfmt
044120010305     C                   exsr      Sr_Contrrsr
044130010117     C                   eval      *in07 = *off
044140010117     C                   endif
044150010117
044160010117     C     endsrclir     endsr
044170010201      **********************************************************************
044180010201      * CONTROLLI     * cliente ritiro *
044190010201      **********************************************************************
044200010201     C     Sr_Contrcra   begsr
044210010201
044220010201     C                   z-add     ds_v1ccra     kfnacr
044230151109     C     kfnacr        chain(n)  fnacr01l
044240010201     C                   if        %found(fnacr01l)
044250010530     C                   if        ACRatb <> *blanks
044260010201     C                   movel     msg(2)        v1cmsg
044270010201     C                   seton                                        41  28
044280010201     C                   goto      endsrccra
044290010201     C                   endif
044300010502     C                   if        ACRtcr = 'M'
044310010502     C                   movel     msg(69)       v1cmsg
044320010502     C                   seton                                        41  28
044330010502     C                   goto      endsrccra
044340010502     C                   endif
044350140120
044360140120      /free
044370140120         dACR01 = ACRmai;
044380140120      /end-free
044390140120
044400010201      * Controllo se posso modificare il cliente ritiro
044410010201     C                   movel     ACRpoa        alf_acrpoa
044420010201     C                   if        alf_acrpoa <> v1cpor
044430010201     C                             and ds_v1ccra <> sav_v1ccra
044440010201     C                             and *in09 = *on
044450010201     C                   seton                                        41  28
044460010201     C                   movel     msg(40)       v1cmsg
044470010201     C                   eval      ds_v1ccra = sav_v1ccra
044480010201     C                   eval      v1cra1 = ds_v1cra1
044490010201     C                   eval      v1cra2 = ds_v1cra2
044500010201     C                   eval      v1cra3 = ds_v1cra3
044510010201     C                   goto      endsrccra
044520010201     C                   endif
044530010201
044540010906      * Se modificato il cliente pulisco il ksc
044550010906     C                   if        v1pag = 'M'
044560010906     C                             and (ds_v1ccra <> sav_v1ccra
044570030902     C                             or   v1pag <> sav_v1pag
044580040630     c                             or   v1ksc = *Zeros
044590040630     c                             or   v1ctr = *Blanks)
044600030902      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocra mi carica il ksc
044610040630     c                   If        v1ksc = *zeros or v1ctr = *Blanks
044620030902     c                   Clear                   sav_V1pag
044630030902     c                   EndIf
044640010906     C                   clear                   v1ksc
044650010906     C                   clear                   v1ctr
044660010906     C                   endif
044670010201     C                   exsr      Sr_Decocra
044680010201     C                   else
044690050426     c                   If        v1rsr = *Blanks
044700050426     c                   Eval      *In07 = *Off
044710050426     c                   EndIf
044720010201     C                   movel     msg(2)        v1cmsg
044730010201     C                   seton                                        41  28
044740010201     C                   endif
044750140422
044760140422      /free
044770140422       //?Recupero il ntw del mittente
044780140422         clear m_OG143;
044790140422         chain  v1cra1 AZORG01L;
044800140422         IF  %found(AZORG01L) and ORGfva = *blanks;
044810140422           m_OG143 = ORGde3;
044820140422         ENDIF;
044830140422      /end-free
044840010201
044850010201     C     endsrccra     endsr
044860010201      **********************************************************************
044870010201      * DECODIFICHE     * cliente ritiro *
044880010201      **********************************************************************
044890010201     C     Sr_Decocra    begsr
044900010201
044910010201     C                   movel     ACRrsr        v1rsr
044920010201     C                   movel     ACRinr        v1inr
044930010201     C                   movel     ACRcar        v1car
044940010201     C                   movel     ACRlor        v1lor
044950010201     C                   movel     ACRprr        v1prr
044960010201     C                   movel     ACRnar        v1nar
044970010308      * Se paga mittente memorizzo il codice piano dei conti
044980010308     C                   if        v1pag = 'M'
044990010906     C                             and (ds_v1ccra <> sav_v1ccra
045000010906     C                             or   v1pag <> sav_v1pag)
045010010906     C                             and  v1ksc = *zeros
045020010308     C                   eval      v1ksc = ACRksc
045030010507     C  nkf              eval      sav_v1ksc = ACRksc
045040010309     C                   eval      sav_v1pag = v1pag
045050040629      * memorizzo anche il codice tariffa
045060040629     c                   If        AcrCcc = 999
045070040629     c                   Clear                   V1Ctr
045080040629     c                   Else
045090040629     c                   Move      AcrCcc        V1Ctr
045100040629     c                   EndIf
045110010514     C                   endif
045120010201
045130010201     C                   endsr
045140010122      **********************************************************************
045150010122      * CARICO DATI DA ANAGRAFICO RITIRI
045160010122      **********************************************************************
045170010122     C     Sr_Caranacr   begsr
045180010122
045190010122      * Se non è richiamato carica i dati dall'anagrafico clienti
045200010723     C                   if        §RMtla = *blanks
045210010122     C                   movel     ACRper        v1rer
045220010122     C                   movel     ACRtel        v1ter
045230010122     C                   movel     ACRntm        v1nam
045240010207     C                   movel     ACRspi        v1spi
045250050128      * se note immesse dall'utente ora le sostituisco con quelle dell'anagrafica
045260050128      * msg. info
045270050128     c                   If        acrno1 <> *Blanks
045280050128     c                   If        v1not1 <> *Blanks
045290050128     c                   Eval      v1cmsg = msg(85)
045300050128     c                   Eval      *In28 = *On
045310050128     c                   Eval      *In82 = *On
045320050128     c                   EndIf
045330050128     c                   movel     ACRno1        v1not1
045340050128     c                   EndIf
045350050128     c                   If        acrno2 <> *Blanks
045360050128     c                   If        v1not2 <> *Blanks
045370050128     c                   Eval      v1cmsg = msg(85)
045380050128     c                   Eval      *In28 = *On
045390050128     c                   Eval      *In82 = *On
045400050128     c                   EndIf
045410050128     c                   movel     ACRno2        v1not2
045420050128     c                   EndIf
045430010723     C                   else
045440010723      * Se è richiamato carico i dati non immessi
045450010723     C                   if        v1rer = *blanks
045460010723     C                   movel     ACRper        v1rer
045470010723     C                   endif
045480010723     C                   if        v1ter = *blanks
045490010723     C                   movel     ACRtel        v1ter
045500010723     C                   endif
045510010723     C                   if        v1nam = *blanks
045520010723     C                   movel     ACRntm        v1nam
045530010723     C                   endif
045540010723     C                   if        v1not1 = *blanks
045550010723     C                   movel     ACRno1        v1not1
045560010723     C                   endif
045570010723     C                   if        v1not2 = *blanks
045580010723     C                   movel     ACRno2        v1not2
045590010723     C                   endif
045600010723     C                   if        v1spi = *blanks
045610010723     C                   movel     ACRspi        v1spi
045620010723     C                   endif
045630010723     C                   endif
045640010723
045650010723     C                   movel     ACRpoa        sav_ACRpoa
045660131113      /free
045670131113       //?Recupero gli orari di apertura
045680131114         exsr CaricaOrari;
045690131113      /end-free
045700010122
045710010122      * Recupera i dati del cliente ordinante se inserito
045720010122     C                   if        ACRcco <> *zeros
045730010122     C                   z-add     ACRcco        ds_v1ccor
045740010122     C                   eval      v1cor1 = ds_v1cor1
045750010122     C                   eval      v1cor2 = ds_v1cor2
045760010122     C                   eval      v1cor3 = ds_v1cor3
045770010122     C                   exsr      Sr_Contrcor
045780010122     C                   endif
045790010227
045800010227      * Recupera l'ora del ritiro se non già inserita
045810161114     C*********          if        v1orr = *zeros
045820161114     c                   IF        ACRorr > *zeros
045830010227     C                   z-add     ACRorr        v1orr
045840131022     C                   z-add     ACRorr        sav_ACRorr
045850161114     c                   ENDIF
045860161114     C*********          endif
045870010122
045880010122      * Controlla i flag per posizionamento cursore
045890010122     C                   eval      *in43 = (ACRfcl = 'O')
045900010122     C   43              goto      endflg
045910010122     C                   eval      *in44 = (ACRfpk = 'O')
045920010122     C   44              goto      endflg
045930010122     C                   eval      *in45 = (ACRfmc = 'O')
045940010122     C   45              goto      endflg
045950010122     C                   eval      *in46 = (ACRfbn = 'O')
045960010122     C   46              goto      endflg
045970010122     C                   eval      *in47 = (ACRfbl = 'O')
045980010122     C   47              goto      endflg
045990010122     C                   eval      *in48 = (ACRfat = 'O')
046000010122     C   48              goto      endflg
046010010122     C                   eval      *in49 = (ACRfmt = 'O')
046020010122     C   49              goto      endflg
046030010122     C                   eval      *in43 = (ACRfcl = 'F')
046040010122     C   43              goto      endflg
046050010122     C                   eval      *in44 = (ACRfpk = 'F')
046060010122     C   44              goto      endflg
046070010122     C                   eval      *in45 = (ACRfmc = 'F')
046080010122     C   45              goto      endflg
046090010122     C                   eval      *in46 = (ACRfbn = 'F')
046100010122     C   46              goto      endflg
046110010122     C                   eval      *in47 = (ACRfbl = 'F')
046120010122     C   47              goto      endflg
046130010122     C                   eval      *in48 = (ACRfat = 'F')
046140010122     C   48              goto      endflg
046150010122     C                   eval      *in49 = (ACRfmt = 'F')
046160010122     C   49              goto      endflg
046170010122     C     endflg        tag
046180010122      * Carico i flag in campi di comodo
046190010122     C                   exsr      Sr_Carflag
046200070913
046210070913      * codice giro
046220070925     c                   clear                   v1ccgi
046230071031     c                   clear                   sav_acrcgi
046240010122
046250010702     C                   endsr
046260131114      /free
046270131114       //--------------------------------------------------------------
046280131114       //?Carico gli orari di apertura da anagrafica clienti ritiro.
046290131114       //--------------------------------------------------------------
046300131114       BEGSR CaricaOrari;
046310131114
046320131114         IF  %subst(ACRmai:1:16) <> *blanks;
046330140120           IF  %check(c_Digits:%subst(ACRmai:1:
046340140120               %len(§ACRoa1))) = *zeros;
046350140120             v1OraAMda = %int(§ACRoa1);
046360131114           ENDIF;
046370140120           IF  %check(c_Digits:%subst(ACRmai:%len(§ACRoa1)+1:
046380140120             %len(§ACRoa2))) = *zeros;
046390140120             v1OraAMa  = %int(§ACRoa2);
046400131114           ENDIF;
046410140120           IF  %check(c_Digits:%subst(ACRmai:%len(§ACRoa1)+
046420140120               %len(§ACRoa2)+1:
046430140120               %len(§ACRoa3))) = *zeros;
046440140120             v1OraPMda = %int(§ACRoa3);
046450131114           ENDIF;
046460140120           IF  %check(c_Digits:%subst(ACRmai:%len(§ACRoa1)+
046470140120               %len(§ACRoa2)+
046480140120               %len(§ACRoa3)+1:
046490140120               %len(§ACRoa4))) = *zeros;
046500140120             v1OraPMa  = %int(§ACRoa4);
046510131114           ENDIF;
046520131114         ENDIF;
046530131114
046540131114       ENDSR;
046550131114      /end-free
046560131114
046570010122      **********************************************************************
046580010122      * CARICA FLAG COLLI - PESO - VOLUME - ETC ETC
046590010122      **********************************************************************
046600010122     C     Sr_Carflag    begsr
046610010122
046620010122     C                   eval      sav_acrfcl = ACRfcl
046630010122     C                   eval      sav_acrfpk = ACRfpk
046640010122     C                   eval      sav_acrfmc = ACRfmc
046650010122     C                   eval      sav_acrfbn = ACRfbn
046660010122     C                   eval      sav_acrfbl = ACRfbl
046670010122     C                   eval      sav_acrfat = ACRfat
046680010122     C                   eval      sav_acrfmt = ACRfmt
046690010122
046700010122     C                   endsr
046710001016      **********************************************************************
046720001016      * CONTROLLI     * dati cliente ritiro *
046730001016      **********************************************************************
046740001016     C     Sr_Contrrsr   begsr
046750001016
046760001016     C                   clear                   FNLV14DS
046770001016     C                   z-add     *date         I14dri
046780001016     C                   movel     v1rsr         I14rsc
046790001016     C                   movel     v1inr         I14ind
046800001103     C                   movel(p)  v1lor         E14loc
046810001016     C                   movel     v1prr         E14prv
046820001016     C                   movel     v1car         E14cap
046830001016     C                   movel     v1nar         E14nar
046840130507
046850130507      * mi memorizzo che non ho passato il cap e la provincia
046860161108     c                   IF        §rmfpr <> 'C'
046870130507     c                   IF        E14cap = *blanks and E14prv = *blanks
046880130507     c                   eval      wintcap = *on
046890130507     c                   ELSE
046900130507     c                   eval      wintcap = *off
046910130507     c                   ENDIF
046920161108     c                   ENDIF
046930161109      /free
046940161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
046950161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
046960161109       //?il pgm di interrogazione cappario
046970161109         IF  ConfAuto;
046980161109           I14batch = 'S';
046990161109         ENDIF;
047000161109      /end-free
047010130507
047020001016     C                   call      'FNLV14R'
047030001016     C                   parm                    KPJBA
047040001016     C                   parm                    FNLV14DS
047050001016
047060001016     C                   movel     I14ind        v1inr
047070001016     C                   movel     E14loc        v1lor
047080001016     C                   movel     E14prv        v1prr
047090001016     C                   movel     E14cap        v1car
047100001016     C                   movel     E14nar        v1nar
047110001016
047120001016     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
047130001016     C                   eval      *in61 = (O14err = '2')                       Indirizzo
047140001016     C                   eval      *in62 = (O14err = '5')                       CAP
047150001016     C                   eval      *in63 = (O14err = '3')                       Località
047160001016     C                   eval      *in64 = (O14err = '4')                       Provincia
047170001016     C                   eval      *in65 = (O14err = '6')                       Nazione
047180001016
047190001016     C                   if        O14msg <> *blanks
047200001016     C                   movel(p)  O14msg        v1cmsg
047210001016     C                   eval      *in28 = *on
047220001023     C                   eval      *in50 = *on
047230001016     C                   goto      endsrcrsr
047240001023     C                   endif
047250001023      * Controlla indirizzo
047260001023     C                   if        v1car <> *blanks or v1lor <> *blanks or
047270001023     C                             v1prr <> *blanks or v1nar <> *blanks
047280150921     c                   eval      sav_v1pkg = wpkg
047290150921     c                   eval      sav_v1vlm = wvlm
047300001023     C                   exsr      Sr_Contrcapr
047310001023     C                   eval      *in50 = (*in28 = *on)
047320001016     C   28              goto      endsrcrsr
047330010202     C                   endif
047340010207      * Se paga il mittente devo pulire il codice cliente che paga
047350010329     C                   if        v1pag = 'M'
047360010507     C                             and (v1pag <> sav_v1pag
047370010906     C                             or   sav_v1ccra <> ds_v1ccra)
047380010207     C                   clear                   v1ksc
047390010906     C                   clear                   v1ctr
047400010903     C  nkf              clear                   sav_v1ksc
047410010329     C                   eval      sav_v1pag = v1pag
047420010207     C                   endif
047430001016
047440001016     C     endsrcrsr     endsr
047450001023      **********************************************************************
047460001023      * CONTROLLI     * cap - località - provincia *
047470001023      **********************************************************************
047480001023     C     Sr_Contrcapr  begsr
047490001115
047500001115     C                   eval      *in32 = *off
047510131108     c                   clear                   sav_O95gf2
047520131111     c                   clear                   sav_O95lna
047530001023
047540001023     C                   clear                   TISI95DS
047550001023     C                   clear                   FNLV13DS
047560001023     C                   movel     ' '           I95tla
047570001023     C                   movel     '7'           I95tcn
047580001023     C                   movel     v1car         I95cap
047590001023     C                   movel     v1nar         I95nar
047600001023     C                   movel     v1inr         I95ind
047610001103     C                   movel(p)  v1lor         I95loc
047620001023     C                   movel     v1prr         I95prv
047630010413     C                   z-add     sav_v1pkg     I95lkg
047640150921     C                   z-add     sav_v1vlm     I95lmc
047650001023     C                   movel     'S'           I13af0
047660001023     C                   movel     'S'           I13af1
047670001023     C                   movel     'S'           I13sz2
047680001023     C                   movel     'S'           I13la3
047690001023     C                   movel     'S'           I13sz3
047700001023     C                   movel     'S'           I13cnv
047710081215      * se impostato network DPD lo passo
047720081215     c                   if        v1ntwd <> *blanks
047730081216     c                   eval      i95fi1 = v1ntwd
047740081215     c                   else
047750081215     c                   clear                   i95fi1
047760081215     c                   endif
047770001023
047780001023      * Gestione errori forzabili: se cambiato CAP/Località
047790001023      * riazzera i valori di errori forzabili
047800001023
047810010223      * CAP
047820010417     C     v1car         ifne      sav_v1car
047830010417     C                   eval      *in89 = *off
047840010417     C                   movel     v1car         sav_v1car
047850001023     C                   clear                   wfzv
047860001023     C                   clear                   wfz1
047870001023     C                   clear                   wfz2
047880001023     C                   clear                   wfz3
047890001115     C                   eval      *in32 = *on
047900061222     C                   endif
047910001023
047920001023      * Modifica località
047930001023
047940010118     C     v1lor         ifne      sav_v1lor
047950001023     C                   clear                   wfz1
047960001023     C                   clear                   wfz2
047970001023     C                   clear                   wfz3
047980010118     C                   movel     v1lor         sav_v1lor
047990001115     C                   eval      *in32 = *on
048000001023     C                   endif
048010001023
048020001023      * Modifica provincia
048030001023
048040010118     C     v1prr         ifne      sav_v1prr
048050001023     C                   clear                   wfz3
048060010118     C                   movel     v1prr         sav_v1prr
048070001115     C                   eval      *in32 = *on
048080001023     C                   endif
048090001023
048100001023     C                   movel     wfzv          E13fzv
048110001023     C                   movel     wfz1          E13fz1
048120001023     C                   movel     wfz2          E13fz2
048130001023     C                   movel     wfz3          E13fz3
048140040415      * se il pgm è richiamato e non deve far vedere le videate imposto wfv1 = '1'
048150040415     c                   If        §rmtla = '0'
048160040415     c                   Eval      E13fz1 = 1
048170040415     c                   EndIf
048180001025
048190001023     C                   call      'FNLV13R'
048200001023     C                   parm                    KPJBA
048210001023     C                   parm                    FNLV13DS
048220001023     C                   parm                    TISI95DS
048230131108
048240131108     c                   eval      sav_O95gf2 = O95gf2
048250131111     c                   eval      sav_O95lna = O95lna
048260131108
048270110121     c* Se sto copiando da altro orm per emettere la videata non
048280110121     c* gestisco eventuali errori: sarà poi nella seconda fase,
048290110121     c* dopo emissione della videata, che verrano effettuati
048300110121     c* i controlli.
048310110121     c                   if        $copia='S'
048320110121     c                   leavesr
048330110121     c                   endif
048340001023
048350001023      * Ricontrolla errori
048360001023
048370001023     C                   movel     O13err        werr
048380001023     C                   movel     O13msg        wmsg
048390001023     C                   movel     E13fzv        wfzv
048400001023     C                   movel     E13fz1        wfz1
048410001023     C                   movel     E13fz2        wfz2
048420001023     C                   movel     E13fz3        wfz3
048430001023
048440001023      * Se il livello di affidabilità non è inferiore a 2 e
048450001023      * il livello di reperimento dati non è inferiore a 3
048460001023      * reimposta i dati da TISI95DS
048470110120      * I dati non vengono sostituiti se i dati provengono da anagrafica
048480001023
048490110119     c                   if        ds_v1ccra=0
048500001023     C     O13ron        ifeq      'S'
048510110119     C                   movel     O95nar        v1nar
048520110119     c                   endif
048530110119     C     O13roc        ifeq      'S'
048540110119     C                   movel     O95cap        v1car
048550110119     c                   endif
048560110119     C     O13rop        ifeq      'S'
048570110119     C                   movel     O95prv        v1prr
048580110119     c                   endif
048590110119     C     O13rol        ifeq      'S'
048600001023     C                   movel     O95loc        v1lor
048610001023     C                   end
048620001129
048630140508      /free
048640140529       //?Se variato cap/località/provincia
048650140529       //?e sono in conferma ORM da VAO (dati inviati dal cliente errati)
048660140529       //?e se la data NON è maggiore di oggi
048670140529       //?e ci sono i presupposti per inviare l'alert
048680140529       //?imposto il flag così da ricalcolare la data ritiro
048690140529         IF  *in32 and §RMfpr = 'C' and
048700140529             old_invv1dar <= oggi_aammgg and wOKalert and
048710140529             werr = *blanks and wmsg = *blanks;
048720140529           wModLocRit = *on;
048730140529         ENDIF;
048740161116       //?Se variato cap/località/provincia
048750161116       //?e sono in immissione manuale
048760161116       //?imposto il flag così da ricalcolare la data ritiro
048770161116         IF  *in32 and (*in01 or *in03) and
048780161116            (V1Ctco = 'M' or V1Ctco = 'E') and
048790161116             werr = *blanks and wmsg = *blanks;
048800161116           wModLocRit = *on;
048810161116         ENDIF;
048820161109
048830161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
048840161109       //?conferma automatica batch non devo dare errore
048850161109         IF  ConfAuto and O13roc = 'S';
048860161109           clear werr;
048870161109           clear O13roc;
048880161109         ENDIF;
048890140508      /end-free
048900001023
048910001023     C     werr          ifne      ' '
048920001023     C     wmsg          andne     *blanks
048930061222     C     *in32         oreq      *on
048940001023
048950001023     C                   select
048960001023
048970001023     C     werr          wheneq    '3'
048980001023     C                   eval      *in63 = *on
048990001023     C                   eval      *in28 = *on
049000001023
049010001023     C     werr          wheneq    '5'
049020001023     C                   eval      *in62 = *on
049030001023     C                   eval      *in28 = *on
049040001023
049050001023     C     werr          wheneq    '4'
049060001023     C                   eval      *in64 = *on
049070001023     C                   eval      *in28 = *on
049080001023
049090001023     C     werr          wheneq    '6'
049100001023     C                   eval      *in65 = *on
049110001023     C                   eval      *in28 = *on
049120001023     C                   endsl
049130010702
049140110120     C                   movel(p)  wmsg          v1cmsg
049150001023     C                   end
049160110120     c                   else
049170110120     C     werr          ifne      ' '
049180110120     C     wmsg          andne     *blanks
049190110120     C                   eval      *in41 = *on
049200110120     C                   eval      *in28 = *on
049210110120     C                   movel(p)  msg(94)       v1cmsg
049220110120     c                   endif
049230110120     c                   endif
049240001023
049250001023     C   28              goto      endsrcapr
049260001023
049270001023      * Controlla se reimpostata località
049280001023
049290001023     C     O13rol        ifeq      'S'
049300001023     C                   eval      *in63 = *on
049310001023     C                   eval      *in28 = *on
049320001023     C                   movel     msg(19)       v1cmsg
049330001023     C                   goto      endsrcapr
049340001023     C                   end
049350001023
049360001023      * Controlla se reimpostata Provincia
049370001023
049380001023     C     O13rop        ifeq      'S'
049390001023     C                   eval      *in64 = *on
049400001023     C                   eval      *in28 = *on
049410001023     C                   movel     msg(21)       v1cmsg
049420001023     C                   goto      endsrcapr
049430001023     C                   end
049440001023
049450001023      * Controlla se reimpostato cap
049460001023
049470001023     C     O13roc        ifeq      'S'
049480001023     C                   eval      *in62 = *on
049490001023     C                   eval      *in28 = *on
049500001023     C                   movel     msg(20)       v1cmsg
049510001023     C                   goto      endsrcapr
049520001023     C                   end
049530001023
049540001023      * Controlla se reimpostata Nazione
049550001023
049560001023     C     O13ron        ifeq      'S'
049570001023     C                   eval      *in65 = *on
049580001023     C                   eval      *in28 = *on
049590001023     C                   movel     msg(22)       v1cmsg
049600001023     C                   goto      endsrcapr
049610001023     C                   end
049620001023
049630001023     C     endsrcapr     endsr
049640131024
049650131024      /free
049660131024       //--------------------------------------------------------------
049670131024       //?Calcolo la filiale ritiro.
049680131024       //--------------------------------------------------------------
049690131024       BEGSR calPOR;
049700131024
049710131024       //?Da ritiro fisso
049720131024         IF  V1Cpos <> 0 and V1Cpor = *blanks;
049730131024           V1Cpor = %editc(V1Cpos:'X');
049740131024         ENDIF;
049750131024
049760131024       //?Da Anagrafica clienti ritiro
049770131024         IF  sav_ACRpoa <> 0 and V1Cpor = *blanks;
049780131024           V1Cpor = %editc(sav_ACRpoa:'X');
049790131024         ENDIF;
049800131024
049810150922       //?Calcolo la filiale ritiro in base al peso/volume
049820150921         sav_V1pkg = wpkg;
049830150921         sav_V1vlm = wvlm;
049840150921         exsr sr_contrcapr;
049850131024
049860131024       //?Se c'è stato un errore nel controllo del cappario esco
049870131024         IF  O13err <> *blanks or *in28;
049880131024           leavesr;
049890131024         ENDIF;
049900131024
049910131024       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
049920131024         clear FIOR96ds;
049930150305       //?se la filiale emissione non è ancora stata impostata passo la filiale utente
049940131024         IF  V1Cpoe = *zeros;
049950150305           IOR96poe = DUTpou;
049960131024         ELSE;
049970131024           IOR96poe = V1Cpoe;
049980131024         ENDIF;
049990131111         IOR96por = sav_O95lna;
050000131024         fior96r(kpjba : fior96ds);
050010131024         IF  OOR96err = *blanks and OOR96por <> *zeros;
050020131111           sav_O95lna = OOR96por;
050030160209         //?se la filiale di ritiro è da forzare
050040160209         //?pulisco il campo V1Cpor così imposto per forza la nuova filiale forzata
050050160209         //?in questo modo forzo SEMPRE
050060160209           IF  IOR96por <> OOR96por;
050070131024             clear v1cpor;
050080131024           ENDIF;
050090131024         ENDIF;
050100131024
050110131024       //?Imposto la filiale ritiro a video
050120131024         IF V1Cpor = *blanks;
050130131111           V1Cpor = %editc(sav_O95lna:'X');
050140131024         ENDIF;
050150131024
050160131024       //?Se immissione/copia ORM e filiale ritiro è una filiale gestita
050170131024       //?imposto la filiale 'mamma' come filiale ritiro
050180131024         IF  *in01 or *in03 and wforzapor = *off;
050190131024           clear FNLV55DS;
050200131024           D55tpt = '6';
050210131024           D55lin = %dec(V1Cpor:3:0);
050220131024           D55drf = oggi_aammgg;
050230131024           fnlv55r (fnlv55ds);
050240131024           IF  D55err = *blanks;
050250131024             V1Cpor = %editc(D55tfa:'X');
050260131024             wforzapor = *on;
050270131024           ENDIF;
050280131024         ENDIF;
050290131024
050300131024       //?Decodifico filiale ritiro
050310131030         clear OG143;
050320131030         clear OG148;
050330131030         clear ntw_V1Cpor;
050340131024         kazorg = %dec(V1Cpor:3:0);
050350131024         chain kazorg AZORG01L;
050360131024         IF  not %found(AZORG01L);
050370131024           V1Cmsg = msg(33);
050380131024           *in28 = *on;
050390131024           *in59 = *on;
050400131024           leavesr;
050410131024         ENDIF;
050420131024         IF  ORGfva <> *blanks or
050430131024            (ORGfag <> 'A' and ORGfag <> 'F');
050440131024           V1Cmsg = msg(33);
050450131024           *in28 = *on;
050460131024           *in59 = *on;
050470131024           leavesr;
050480131024         ENDIF;
050490131024
050500131024         OG143 = ORGde3;
050510131024         OG148 = ORGde8;
050520131030         ntw_V1Cpor = §OGntw;
050530131024
050540131024       //?Se filiale ritiro non ha la procedura ORM attiva blocco
050550131024         IF  §OGorm <> 'S';
050560131024           V1Cmsg = msg(36);
050570131024           *in28 = *on;
050580131024           *in59 = *on;
050590131024           leavesr;
050600131024         ENDIF;
050610150310
050620150310       //?Imposto se filiale ritiro ha la procedura PDA attiva
050630150310         *in22 = (§ogpdaorm <> *blanks);
050640131024
050650131024         V1Dpor = ORGdes;
050660131024         num_V1Cpor = %dec(V1Cpor:3:0);
050670161116         IF  sav_V1Cpor <> *zeros and sav_V1Cpor <> *blanks and
050680161116             V1Cpor <> sav_V1Cpor;
050690161116           wModPor = *on;
050700161116         ENDIF;
050710131024         sav_V1Cpor = V1Cpor;
050720131024
050730131024       ENDSR;
050740131024
050750131024      /end-free
050760131024
050770001010      **********************************************************************
050780001010      * CONTROLLI     * colli - peso - volume - etc etc *
050790001010      **********************************************************************
050800001010     C     Sr_Contrqta   begsr
050810001010
050820001023      * Se il flag è F può non essere inserita la qtà
050830001023      * Se il flag è O la qtà deve essere inserita
050840010118     C                   if        sav_acrfcl <> *blanks and v1ncl = *zeros
050850010118     C                   if        sav_acrfcl = 'F' and *in66 = *off
050860001012     C                   movel     msg(25)       v1cmsg
050870001012     C                   seton                                        436628
050880001012     C                   goto      endsrcqta
050890001012     C                   endif
050900010118     C                   if        sav_acrfcl = 'O'
050910001010     C                   movel     msg(4)        v1cmsg
050920001010     C                   seton                                        43  28
050930001012     C                   goto      endsrcqta
050940001012     C                   endif
050950010903     C                   else
050960010903     C                   setoff                                       43
050970001011     C                   endif
050980001010
050990010118     C                   if        sav_acrfpk <> *blanks and v1pkg = *zeros
051000010118     C                   if        sav_acrfpk = 'F' and *in67 = *off
051010001012     C                   movel     msg(26)       v1cmsg
051020001012     C                   seton                                        446728
051030001012     C                   goto      endsrcqta
051040001012     C                   endif
051050010118     C                   if        sav_acrfpk = 'O'
051060001010     C                   movel     msg(5)        v1cmsg
051070001010     C                   seton                                        44  28
051080001012     C                   goto      endsrcqta
051090001012     C                   endif
051100010903     C                   else
051110010903     C                   setoff                                       44
051120001010     C                   endif
051130130508
051140130508     C                   if        sav_acrfbn <> *blanks and v1bnc = *zeros
051150130508     C                   if        sav_acrfbn = 'F' and *in69 = *off
051160130508     C                   movel     msg(28)       v1cmsg
051170130508     C                   seton                                        466928
051180130508     C                   goto      endsrcqta
051190130508     C                   endif
051200130508     C                   if        sav_acrfbn = 'O'
051210130508     C                   movel     msg(7)        v1cmsg
051220130508     C                   seton                                        46  28
051230130508     C                   goto      endsrcqta
051240130508     C                   endif
051250130508     C                   else
051260130508     C                   setoff                                       46
051270130508     C                   endif
051280001010
051290010118     C                   if        sav_acrfmc <> *blanks and v1vlm = *zeros
051300010118     C                   if        sav_acrfmc = 'F' and *in68 = *off
051310001012     C                   movel     msg(27)       v1cmsg
051320001012     C                   seton                                        456828
051330001012     C                   goto      endsrcqta
051340001012     C                   endif
051350010118     C                   if        sav_acrfmc = 'O'
051360001010     C                   movel     msg(6)        v1cmsg
051370001010     C                   seton                                        45  28
051380001012     C                   goto      endsrcqta
051390001012     C                   endif
051400010903     C                   else
051410010903     C                   setoff                                       45
051420001010     C                   endif
051430010903
051440010213      * Se prepagato obbligatorio colli e peso
051450010213     C                   if        v1ctor = 'P' and
051460010213     C                             (v1ncl = *zeros or v1pkg = *zeros)
051470010213     C                   movel     msg(54)       v1cmsg
051480010213     C                   seton                                        44  28
051490010213     C                   goto      endsrcqta
051500010213     C                   endif
051510130320
051520130320      * Immissione ORM manuale voglio i colli
051530130321      * o i bancali
051540141211     c                   IF        *in01 and
051550141212     c                             (V1Ctco = 'M' or V1Ctco = 'E') and
051560130321     c                             V1ncl = *zeros and V1bnc = *zeros
051570130320     c                   eval      V1Cmsg = msg(4)
051580130321     c                   eval      V1Cmsg = %trim(V1Cmsg) + ' o i bancali.'
051590130320     c                   eval      *in28 = *on
051600130320     c                   eval      *in43 = *on
051610130320     c                   leavesr
051620130320     c                   ENDIF
051630010109
051640010109      * Almeno 1 tra volume peso e bancali deve essere inserito
051650010417     C                   if        v1pkg = *zeros and v1vlm = *zeros
051660010417     C                             and v1bnc = *zeros
051670010109     C                   movel     msg(37)       v1cmsg
051680010109     C                   seton                                        44  28
051690010109     C                   goto      endsrcqta
051700010205     C                   else
051710010205     C                   setoff                                       44
051720010109     C                   endif
051730081126
051740160707      /free
051750160921       //?Controllo i limiti previsti per le quantità
051760160920       //?Richiamo pgm controllo limiti quantità x colli/peso/volume
051770160920         clear TRUL73DS;
051780160920         I73tsp = 'C';
051790160920         I73ncl = V1ncl;
051800160920         I73psk = V1pkg;
051810160920         I73vmc = V1vlm;
051820160920         trul73r (trul73ds);
051830160920       //?Limite colli bloccante
051840160921         SELECT;
051850160921         WHEN  O73fclm = '1';
051860160920           *in28 = *on;
051870160920           *in43 = *on;
051880160920           V1Cmsg = 'Il numero colli massimo consentito è';
051890160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lclm:'4'));
051900160920           leavesr;
051910160920       //?Limite colli forzabile
051920160921         WHEN  O73fclf = '1' and V1ncl <> wold_V1ncl;
051930160920           *in28 = *on;
051940160920           *in43 = *on;
051950160920           V1Cmsg = 'Il numero colli massimo consentito è';
051960160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lclf:'4')) +
051970160921                    '. Enter x forzare.';
051980160921           wold_V1ncl = V1ncl;
051990160920           leavesr;
052000160920       //?Limite peso bloccante
052010160921         WHEN  O73fkgm = '1';
052020160921           *in28 = *on;
052030160921           *in44 = *on;
052040160921           V1Cmsg = 'Il peso massimo consentito è';
052050160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lkgm:'4'));
052060160921           leavesr;
052070160921       //?Limite peso forzabile
052080160921         WHEN  O73fkgf = '1' and V1pkg <> wold_V1pkg;
052090160921           *in28 = *on;
052100160921           *in44 = *on;
052110160921           V1Cmsg = 'Il peso massimo consentito è';
052120160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lkgf:'4')) +
052130160921                    '. Enter x forzare.';
052140160921           wold_V1pkg = V1pkg;
052150160921           leavesr;
052160160920       //?Limite volume bloccante
052170160921         WHEN  O73fmcm = '1';
052180160921           *in28 = *on;
052190160921           *in45 = *on;
052200160921           V1Cmsg = 'Il volume massimo consentito è';
052210160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lmcm:'4'));
052220160921           leavesr;
052230160921       //?Limite volume forzabile
052240160921         WHEN  O73fmcf = '1' and V1vlm <> wold_V1vlm;
052250160921           *in28 = *on;
052260160921           *in45 = *on;
052270160921           V1Cmsg = 'Il volume massimo consentito è';
052280160921           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O73lmcf:'4')) +
052290160921                    '. Enter x forzare.';
052300160921           wold_V1vlm = V1vlm;
052310160921           leavesr;
052320160920         ENDSL;
052330160922       //?Controllo i limiti previsti per le quantità
052340160922       //?Richiamo pgm controllo limiti quantità x bancali/bilico/autoreno/motrice
052350160922         clear TRUL731DS;
052360160922         I731bnc = V1bnc;
052370160922         I731blc = V1blc;
052380160922         I731att = V1att;
052390160922         I731mtc = V1mtc;
052400160922         trul731r (trul731ds);
052410160922       //?Limite bancali bloccante
052420160922         SELECT;
052430160922         WHEN  O731fbncm = '1';
052440160922           *in28 = *on;
052450160922           *in46 = *on;
052460160922           V1Cmsg = 'Il numero bancali massimo consentito è';
052470160922           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O731lbncm:'4'));
052480160922           leavesr;
052490160922       //?Limite bancali forzabile
052500160922         WHEN  O731fbncf = '1' and V1bnc <> wold_V1bnc;
052510160922           *in28 = *on;
052520160922           *in46 = *on;
052530160922           V1Cmsg = 'Il numero bancali massimo consentito è';
052540160922           V1Cmsg = %trim(V1Cmsg) + ' ' + %trim(%editc(O731lbncf:'4')) +
052550160922                    '. Enter x forzare.';
052560160922           wold_V1bnc = V1bnc;
052570160922           leavesr;
052580160922         ENDSL;
052590160921
052600160707      /end-free
052610001010
052620001012     C     endsrcqta     endsr
052630130508      **********************************************************************
052640130508      * CONTROLLI     * bilico - motrice - autotreno  *
052650130508      **********************************************************************
052660130508     C     Sr_Contrqta1  begsr
052670130508
052680130508      * Se il flag è F può non essere inserita la qtà
052690130508      * Se il flag è O la qtà deve essere inserita
052700130508     C                   if        sav_acrfbl <> *blanks and v1blc = *zeros
052710130508     C                   if        sav_acrfbl = 'F' and *in70 = *off
052720131210     C                   movel     msg(29)       w2cmsg
052730130508     C                   seton                                        477028
052740130508     C                   leavesr
052750130508     C                   endif
052760130508     C                   if        sav_acrfbl = 'O'
052770131030     C                   movel     msg(8)        w2cmsg
052780130508     C                   seton                                        47  28
052790130508     C                   leavesr
052800130508     C                   endif
052810130508     C                   else
052820130508     C                   setoff                                       47
052830130508     C                   endif
052840130508
052850130508     C                   if        sav_acrfat <> *blanks and v1att = *zeros
052860130508     C                   if        sav_acrfat = 'F' and *in71 = *off
052870131030     C                   movel     msg(30)       w2cmsg
052880130508     C                   seton                                        487128
052890130508     C                   leavesr
052900130508     C                   endif
052910130508     C                   if        sav_acrfat = 'O'
052920131030     C                   movel     msg(9)        w2cmsg
052930130508     C                   seton                                        48  28
052940130508     C                   leavesr
052950130508     C                   endif
052960130508     C                   else
052970130508     C                   setoff                                       48
052980130508     C                   endif
052990130508
053000130508     C                   if        sav_acrfmt <> *blanks and v1mtc = *zeros
053010130508     C                   if        sav_acrfmt = 'F' and *in72 = *off
053020131030     C                   movel     msg(31)       w2cmsg
053030130508     C                   seton                                        497228
053040130508     C                   leavesr
053050130508     C                   endif
053060130508     C                   if        sav_acrfmt = 'O'
053070131030     C                   movel     msg(10)       w2cmsg
053080130508     C                   seton                                        49  28
053090130508     C                   leavesr
053100130508     C                   endif
053110130508     C                   else
053120130508     C                   setoff                                       49
053130130508     C                   endif
053140130508
053150130508      * se conferma ORM da clienti msg info se inseriti almeno 1 autotreno o
053160130508      * 1 bilico o 1 motrice
053170130508     c                   if        §rmfpr = 'C' and
053180130508     c                             (v1blc > 0 or v1att > 0 or v1mtc > 0) and
053190130508     c                             wforzaqta = *off
053200130508     c                   eval      *in28 = *on
053210131030     c                   eval      w2cmsg = 'ATTENZIONE! Inserito'
053220130508     c                   select
053230130508     c                   when      v1blc > 0
053240130508     c                   eval      *in47 = *on
053250131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' bilico.'
053260130508     c                   when      v1att > 0
053270130508     c                   eval      *in48 = *on
053280131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' autotreno.'
053290130508     c                   when      v1mtc > 0
053300130508     c                   eval      *in49 = *on
053310131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' motrice.'
053320130508     c                   endsl
053330131212     c                   eval      w2cmsg = %trim(w2cmsg) + ' Enter per forzare'
053340130508     c                   eval      wforzaqta = *on
053350130508     c                   leavesr
053360130508     c                   endif
053370160922      /free
053380160922       //?Controllo i limiti previsti per le quantità
053390160922       //?Richiamo pgm controllo limiti quantità x bancali/bilico/autoreno/motrice
053400160922         clear TRUL731DS;
053410160922         I731bnc = V1bnc;
053420160922         I731blc = V1blc;
053430160922         I731att = V1att;
053440160922         I731mtc = V1mtc;
053450160922         trul731r (trul731ds);
053460160922       //?Limite bilico bloccante
053470160922         SELECT;
053480160922         WHEN  O731fblcm = '1';
053490160922           *in28 = *on;
053500160922           *in47 = *on;
053510160922           W2Cmsg = 'Il numero bilici massimo consentito è';
053520160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lblcm:'4'));
053530160922           leavesr;
053540160922       //?Limite bilico forzabile
053550160922         WHEN  O731fblcf = '1' and V1blc <> wold_V1blc;
053560160922           *in28 = *on;
053570160922           *in47 = *on;
053580160922           W2Cmsg = 'Il numero bilici massimo consentito è';
053590160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lblcf:'4')) +
053600160922                    '. Enter x forzare.';
053610160922           wold_V1blc = V1blc;
053620160922           leavesr;
053630160922       //?Limite autotreno bloccante
053640160922         WHEN  O731fattm = '1';
053650160922           *in28 = *on;
053660160922           *in48 = *on;
053670160922           W2Cmsg = 'Il numero autrotreni massimo consentito è';
053680160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lattm:'4'));
053690160922           leavesr;
053700160922       //?Limite autotreno forzabile
053710160922         WHEN  O731fattf = '1' and V1att <> wold_V1att;
053720160922           *in28 = *on;
053730160922           *in48 = *on;
053740160922           W2Cmsg = 'Il numero autotreni massimo consentito è';
053750160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lattf:'4')) +
053760160922                    '. Enter x forzare.';
053770160922           wold_V1att = V1att;
053780160922           leavesr;
053790160922       //?Limite motrice bloccante
053800160922         WHEN  O731fmtcm = '1';
053810160922           *in28 = *on;
053820160922           *in49 = *on;
053830160922           W2Cmsg = 'Il numero motrici massimo consentito è';
053840160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lmtcm:'4'));
053850160922           leavesr;
053860160922       //?Limite motrice forzabile
053870160922         WHEN  O731fmtcf = '1' and V1mtc <> wold_V1mtc;
053880160922           *in28 = *on;
053890160922           *in49 = *on;
053900160922           W2Cmsg = 'Il numero motrice massimo consentito è';
053910160922           W2Cmsg = %trim(W2Cmsg) + ' ' + %trim(%editc(O731lmtcf:'4')) +
053920160922                    '. Enter x forzare.';
053930160922           wold_V1mtc = V1mtc;
053940160922           leavesr;
053950160922         ENDSL;
053960160922      /end-free
053970130508
053980130508     C                   endsr
053990001012      **********************************************************************
054000001016      * CONTROLLI     * data ritiro *
054010001012      **********************************************************************
054020001016     C     Sr_Contrdar   begsr
054030131107
054040131107      /free
054050131107       //?Se non ho mai richiamato il calcolo della data ritiro lo faccio ora
054060131107       //?in questo modo evito errori di dati decimali nel caso in cui
054070131107       //?l'utente riempe tutta la videata senza mai dare enter ed imposta anche
054080131107       //?la data di ritiro
054090131107         IF  not wokOR97;
054100131107           ExSr Sr_CalPikup;
054110131107           wokOR97 = *on;
054120131107         ENDIF;
054130161108       //?Se non ho ancora visualizzato gli orari devo farlo, ma solo se conferma da VAO
054140161108       //?potrei emettere un errore sulla data e averla ancora visualizzata
054150161108         IF  §RMfpr = 'C' and V1dar > *zeros and *in95 = *off;
054160161108           *in95 = *on;
054170161108         ENDIF;
054180131107      /end-free
054190010201
054200010305      * Se non impostata mette quella calcolata
054210010302     C                   if        v1dar = *zeros
054220010201     C                   eval      v1dar = new_v1dar
054230010202     C                   endif
054240161108
054250161108       //?Alcuni controlli che seguono sono superflui dato che la data ritiro è di solo output
054260161108       //?ma preferisco lasciarli che non si sa mai
054270001012
054280010202      * Data obbligatoria
054290010202     C                   if        v1dar = *zeros
054300161108     C                   seton                                        28
054310010202     C                   movel     msg(34)       v1cmsg
054320010202     C                   goto      endsrcdar
054330010202     C                   endif
054340010202
054350010202      * Controlla
054360010116     C                   clear                   inv_v1dar
054370001016     C                   clear                   wlbdat
054380001016     C                   z-add     v1dar         G02dat
054390001016     C                   call      'XSRDA8'
054400001016     C                   parm                    wlbdat
054410001016     C                   if        G02err = '1'
054420161108     C                   seton                                        28
054430001016     C                   movel     msg(35)       v1cmsg
054440010202     C                   goto      endsrcdar
054450010202     C                   endif
054460001025     C                   z-add     G02dat        v1dar
054470010116     C                   z-add     G02inv        inv_v1dar
054480130919
054490130919      * Controllo se modificata dall'utente spengo gli indicatori di comodo
054500130919      * per i msg info
054510130919     c                   IF        old_invv1dar <> inv_v1dar
054520130919     c                   eval      *in40 = *off
054530130919     c                   eval      wforzadar1 = *off
054540130919     c                   eval      old_invv1dar = inv_v1dar
054550130919     c                   ENDIF
054560050304
054570050304      * controllo se supera la data max x errore bloccante
054580130918     c                   If        inv_v1dar > OOR97dmaxb
054590050304     c                   Eval      *In28 = *On
054600050304     c                   Eval      v1cmsg = msg(35)
054610050304     c                   Eval      v1cmsg = %trim(v1cmsg) + ' supera ' +
054620050304     c                             d§dftgg2 + ' gg. dalla data immissione ORM'
054630050304     c                   Goto      endsrcdar
054640050304     c                   EndIf
054650050304
054660050304      * controllo se supera la data max x errore info
054670130918     c                   If        inv_v1dar > OOR97dmaxf and wforzadar1 = *Off
054680050304     c                   Eval      *In28 = *On
054690050304     c                   Eval      v1cmsg = msg(35)
054700050304     c                   Eval      v1cmsg = %trim(v1cmsg) + ' supera ' +
054710050304     c                             d§dftgg1 + ' gg. dalla data immissione ORM.-
054720050304     c                              ENTER X FORZARE!'
054730050304     c                   Eval      wforzadar1 = *On
054740050304     c                   Goto      endsrcdar
054750050304     c                   EndIf
054760010302
054770010302      * Non può essere inferiore alla data immissione orm
054780010302     C                   if        inv_v1dar < inv_v1dao
054790161108     C                   seton                                        28
054800010305     C                   movel     msg(65)       v1cmsg
054810010302     C                   goto      endsrcdar
054820010302     C                   endif
054830161020      /free
054840161020       //?Non può essere inferiore alla Data Pronta Merce
054850161020         IF  inv_v1dar < inv_v1dpm;
054860161020           *in28 = *on;
054870161020           V1Cmsg = 'La Data Ritiro non può essere inferiore +
054880161020                     alla Data Pronta Merce';
054890161020           leavesr;
054900161020         ENDIF;
054910161020      /end-free
054920010906
054930010906      * Se la data è stata ricalcolata emetto un messaggio
054940010906      * se non è stata modificata dall'utente
054950010907     C                   if        old_newv1dar = *zeros
054960010907     C                   eval      old_newv1dar = new_v1dar
054970010907     C                   endif
054980010907     C                   if        v1dar = new_v1dar
054990010907     C                             and new_v1dar <> old_newv1dar
055000010907     C                   eval      old_newv1dar = new_v1dar
055010161108     C                   seton                                        28
055020010906     C                   movel     msg(63)       v1cmsg
055030161026      /free
055040161026       //?Anche se c'è l'errore ricalcolo gli orari servizio
055050161026       //?in questo modo, se viene emesso il msg info del ricalcolo della data a causa
055060161026       //?della variazione di peso, facco già vedere i nuovi orari servizio, altrimenti
055070161026       //?verrebero visualizzati dopo ripristino tastiera ed enter sucessivi.
055080161026         exsr CercaOrari;
055090161026      /end-free
055100010906     C                   goto      endsrcdar
055110010906     C                   endif
055120130918
055130130918      /free
055140140319       //?Nuovi controlli per immissione ORM
055150170526         IF  *in01;
055160131015       //?Imposto 'A' a video se si può anticipare
055170140424           IF  OOR97dmin < OOR97dar;
055180140319             v1anticipa = 'A';
055190140319           ENDIF;
055200131015       //?se la data ritiro è uguale alla data minima tolgo la 'A'
055210140319           IF  inv_v1dar = OOR97dmin;
055220140319             clear v1anticipa;
055230140319           ENDIF;
055240140512       //?se la data ritiro è maggiore della data ritiro calcolata
055250140512       //?tolgo la 'A'
055260140512           IF  inv_V1dar > OOR97dar;
055270140512             clear v1anticipa;
055280140512           ENDIF;
055290140319       //?blocco se data ritiro < data minima e < data ritiro calcolata
055300140319           IF  inv_v1dar < OOR97dar and
055310140319               inv_V1dar < OOR97dmin;
055320130918             *in28 = *on;
055330130918             V1Cmsg = msg(51);
055340130918             leavesr;
055350130918           ENDIF;
055360140319         ENDIF;
055370130918      /end-free
055380010906
055390010305      * Msg info se trovo che è una festività
055400130918     C                   eval      ktfa = num_v1cpor
055410010305     C                   move      v1dar         ds_data
055420010305     C                   eval      kann = ds_anno
055430010305     C                   eval      kmes = ds_mese
055440010305     C     kazcln        chain     azcln01l
055450010305     C                   if        %found(azcln01l)
055460010305     C                   if        (MAT(ds_giorno) =  'F'
055470010305     C                             or  POM(ds_giorno) =  'F')
055480010305     C                             and *in40 = *off
055490161108     C                   seton                                        4028
055500010305     C                   movel     msg(66)       v1cmsg
055510010305     C                   goto      endsrcdar
055520010305     C                   endif
055530010305     C                   endif
055540001012
055550010202     C     endsrcdar     endsr
055560040615      **********************************************************************
055570040615      * CONTROLLI     * calcola data ritiro in base alla pikup area *
055580040615      **********************************************************************
055590040615     c     Sr_Calpikup   BegSr
055600130910
055610130910      /free
055620130910       //?Richiamo pgm per calcolo data ritiro
055630130910         clear FIOR97ds;
055640161027         clear FIOR971DS;
055650130910       //?se la filiale emissione non è ancora stata impostata passo la filiale in gestione
055660130910         IF  V1Cpoe = *zeros;
055670150305           IOR97poe = DUTpou;
055680130910         ELSE;
055690130910           IOR97poe = V1Cpoe;
055700130910         ENDIF;
055710130910         IOR97por = num_V1Cpor;
055720131010         IOR97cap = V1car;
055730131010         IOR97loc = V1lor;
055740160526         IOR97naz = V1nar;
055750130910         IOR97dao = inv_V1dao;
055760131211         IOR97oao = V1oao;
055770130910         IF  *in11;
055780130910           IOR97srm = 'S';
055790130910         ENDIF;
055800130910         IOR97com = V1com;
055810130910         IOR97tor = V1Ctor;
055820130918         IF  ds_V1Ccra > 0;
055830130918           IOR97mcod = 'S';
055840130918         ENDIF;
055850150305       //?se mail/sms inseriti e ORM commissionato
055860140515       //?se ORM manuale
055870140515       //?passo che ORM con alert
055880150305         IF  (w03mail <> *blanks or w03sms <> *blanks) and
055890140515             v1com = 'S' and §RMtla = *blanks;
055900140507           IOR97alert = 'S';
055910140507         ENDIF;
055920140515       //?se conferma ORM da VAO, mail/sms e ci sono i presupposti per alert
055930170308       //?e i flag impostati indicano che devo inviare l'alert
055940140515       //?passo che ORM con alert
055950170308         //IF  §RMfpr = 'C' and (w03mail <> *blanks or w03sms <> *blanks) and
055960170308         //    wOKalert;
055970170308         IF  §RMfpr = 'C' and wOKalert and
055980170308            ((§OREfisv <> 'X' and §OREfisv <> 'N' and w03sms <> *blanks) or
055990170308             (§OREfimv <> 'X' and §OREfimv <> 'N' and w03mail <> *blanks));
056000140515           IOR97alert = 'S';
056010140515         ENDIF;
056020161024       //?passo il peso 'esploso'
056030161027         IOR97pkg = wpkg;
056040161027       //?Se Ordinante codificato
056050161027         IF  ds_V1Ccor > 0;
056060161027           IOR97ocod = 'S';
056070161027         ENDIF;
056080161028       //?Passo Data Pronta Merce
056090161028         IOR97dpm = inv_V1dpm;
056100161024
056110161027         fior97r(kpjba : fior97ds : fior971ds);
056120130910         IF  OOR97err = *blanks and OOR97dar <> *zeros;
056130130910           inv_newV1dar = OOR97dar;
056140130910           dataiso = %date(inv_newV1dar:*iso);
056150130910           dataeur = dataiso;
056160130910           new_V1dar = %dec(dataeur);
056170130910         ENDIF;
056180131015
056190131015       //?Imposto 'A' a video se si può anticipare
056200131015         clear v1anticipa;
056210170524         IF  (*in01 or *in03) and OOR97dmin < OOR97dar;
056220131015           v1anticipa = 'A';
056230131015         ENDIF;
056240170524       //?In immissione o copia mi salvo anche la data ritiro calcolata
056250170524       //?e se ORM da posticipare
056260170525       //?e se posso posticipare
056270170524         IF  *in01 or *in03;
056280170524           wOkPosticipa = *off;
056290170525           wAnticipa = *off;
056300170524           IF  OOR97postd = 'S';
056310170524             wOkPosticipa = *on;
056320170524           ENDIF;
056330170524           DarCalcolata = OOR97dar;
056340170525           wAnticipa = (OOR97dmin < OOR97dar);
056350170524         ENDIF;
056360131107
056370131107       //?Mi memorizzo che ho già richiamato il calcolo della data
056380131107         wokOR97 = *on;
056390161026
056400161026       //?Se ho data pronta merce maggiore data ritiro e sto confermando da VAS
056410161026       //?metto la data pronta merce come data ritiro
056420170524       //?da VAS non si anticipa
056430161026         IF  §RMfpr = 'C' and inv_newv1dar < inv_v1dpm;
056440161026           old_invV1dar = inv_v1dpm;
056450161026           inv_newV1dar = inv_v1dpm;
056460161026           new_V1dar = v1dpm;
056470170524           clear v1anticipa;
056480161026         ENDIF;
056490130910
056500130910      /end-free
056510130910
056520040615     c                   EndSr
056530001017      **********************************************************************
056540001017      * CONTROLLI     * ora ritiro *
056550001017      **********************************************************************
056560001017     C     Sr_Controrr   begsr
056570001017
056580010117     C                   if        v1orr <> *zeros
056590010117     C                   movel     v1orr         ore
056600010117     C                   move      v1orr         minuti
056610001017
056620071107     C                   if        ore > 23 or minuti > 59
056630010220     C                             or ore < 1
056640001017     C                   seton                                        58  28
056650001017     C                   movel     msg(24)       v1cmsg
056660130917     c                   leavesr
056670001017     C                   endif
056680131111     C                   ELSE
056690140320
056700140320      /free
056710131106       //?Se immissione/copia ORM commissionato non c'è obbligo dell'ora
056720131106         IF  (*in01 or *in03) and V1com = 'S';
056730131106         ELSE;
056740131106           *in28 = *on;
056750131106           *in58 = *on;
056760131106           V1Cmsg = msg(52);
056770131106           leavesr;
056780131106         ENDIF;
056790131106      /end-free
056800131111
056810131111     c                   ENDIF
056820001017
056830001017     C                   endsr
056840001103      **********************************************************************
056850070913      * CONTROLLI     * priorità
056860001103      **********************************************************************
056870001103     C     Sr_Contrsto   begsr
056880001103
056890010123     C                   clear                   v1dsto
056900010123
056910010123      * Decodifica priorità ORM
056920010202     C                   if        v1sto <> *zeros
056930010202     C                   clear                   TIBS02DS
056940010202     C                   move      *all'0'       priorita
056950010202     C                   movel     'C'           T02mod
056960010202     C                   movel     knsif         t02sif
056970010202     C                   movel     'STO'         t02cod
056980010202     C                   move      v1sto         priorita
056990010202     C                   movel(p)  priorita      T02ke1
057000010202     C                   call      'TIBS02R'
057010010202     C                   parm                    KPJBA
057020010202     C                   parm                    TIBS02DS
057030010202     C                   if        t02err <> *blanks
057040131030     C                   movel     msg(53)       w2cmsg
057050010202     C                   seton                                        84  28
057060010202     C                   goto      endsrsto
057070010202     C                   endif
057080010202     C                   movel     t02ke1        priorita
057090010202     C                   move      priorita      v1sto
057100010202     C                   movel     t02uni        v1dsto
057110010202     C                   endif
057120001103
057130001103     C     endsrsto      endsr
057140010223      **********************************************************************
057150010223      * CONTROLLI     * Referente e telefono *
057160010223      **********************************************************************
057170010223     C     Sr_Contrref   begsr
057180010223
057190131106      * Per orm prepagato obbligatori tel. e ref.
057200131106     C                   if        v1ctor = 'P'
057210030610      * anche per ORM commissionati
057220030610     c                             or V1com = 'S'
057230010223     C                   if        v1rer = *blanks
057240010223     C                   movel     msg(57)       v1cmsg
057250010223     C                   seton                                        86  28
057260010223     C                   goto      endsrcref
057270010223     C                   endif
057280010223     C                   if        v1ter = *blanks
057290010223     C                   movel     msg(58)       v1cmsg
057300010223     C                   seton                                        87  28
057310010223     C                   goto      endsrcref
057320010223     C                   endif
057330010223     C                   endif
057340010223
057350010223     C     endsrcref     endsr
057360010202      **********************************************************************
057370010202      * CONTROLLI     * pagamento *
057380010202      **********************************************************************
057390010202     C     Sr_Contrpag   begsr
057400010202
057410010202      * Se non inserito prendo di dft
057420010202     C                   if        v1pag = *blanks
057430010202     C                   eval      v1pag = d§dftpag
057440010308     C                   eval      sav_v1pag = d§dftpag
057450010514     C                   endif
057460070912      * controllo che sia valido
057470070912      * nel caso di dato sbagliato che arriva da FNAVO e non viene toccato il campo
057480070912     c                   if        v1pag <> 'M' and v1pag <> 'O' and
057490070912     c                             v1pag <> 'D'
057500070912     c                   eval      v1cmsg = 'Pagamento errato'
057510070912     c                   eval      *in28 = *on
057520070912     c                   eval      *in53 = *on
057530070912     c                   leavesr
057540070912     c                   endif
057550011113      * Se prepagato obbligatorio che paghi il mittente
057560081216      * se non è un ritiro all'estero
057570010213     C                   if        v1ctor = 'P' and v1pag <> 'M'
057580081216     c                             and v1nar = *blanks
057590010213     C                   movel     msg(56)       v1cmsg
057600010213     C                   seton                                        53  28
057610010213     C                   goto      endsrcpag
057620010213     C                   endif
057630010223      * Se singolo e paga il destinatario quest'ultimo è obbligatorio
057640010223     C                   if        v1ctor = 'S' and v1pag = 'D'
057650010223     C                   eval      ds_v1crc1 = v1crc1
057660010223     C                   eval      ds_v1crc2 = v1crc2
057670010223     C                   eval      ds_v1crc3 = v1crc3
057680010223     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
057690010223     C                   movel     msg(61)       v1cmsg
057700010223     C                   seton                                        56  28
057710010223     C                   goto      endsrcpag
057720010223     C                   endif
057730050121      * il codice destinatario non può essere un 8888 o 9999
057740050121     C                   If        ds_v1ccrc > *zeros and
057750050121     c                             (ds_v1crc2 = 9999 or ds_v1crc2 = 8888)
057760050120     C                   movel     msg(84)       v1cmsg
057770050120     C                   seton                                        56  28
057780050120     C                   goto      endsrcpag
057790050120     C                   endif
057800010223     C                   endif
057810030808      * Se inserito che paga l'ordinante
057820030808      * l'ordinante deve essere codificato e non può esserre un 8888 o 9999
057830030808      * se mittente e ordinante sono uguali obbligatorio che paghi mittente
057840010202     C                   eval      ds_v1cor1 = v1cor1
057850010202     C                   eval      ds_v1cor2 = v1cor2
057860010202     C                   eval      ds_v1cor3 = v1cor3
057870030808     c                   Eval      ds_V1Cra1 = V1Cra1
057880030808     c                   eval      ds_V1Cra2 = V1Cra2
057890030808     c                   eval      ds_V1Cra3 = V1Cra3
057900030808     C                   if        v1pag = 'O'
057910030808     C                   If        (ds_v1ccor = *zeros or
057920030606     c                             (ds_v1cor2 = 9999 or ds_v1cor2 = 8888))
057930010202     C                   movel     msg(23)       v1cmsg
057940010202     C                   seton                                        42  28
057950010202     C                   goto      endsrcpag
057960010202     C                   endif
057970030808     c                   EndIF
057980010507
057990010507     C  nkf              eval      sav_v1ksc = v1ksc
058000010202
058010010202     C     endsrcpag     endsr
058020010202      **********************************************************************
058030010202      * CONTROLLI     * cod.cliente e tariffa *
058040010202      **********************************************************************
058050010202     C     Sr_Contrksc   begsr
058060010202
058070040930      * Controllo se il codice cliente esiste su CNACO
058080040930if  1c                   If        v1ksc <> *Zeros
058090040930     c                   z-add     v1ksc         kksc
058100040930     c     kCnaco        Chain     Cnaco00f
058110040930if  2c                   If        Not %Found(Cnaco00f) or
058120040930     c                             AcoFlg <> *Blanks
058130040930     c                   Eval      v1cmsg = msg(16)
058140040930     c                   Seton                                        54  28
058150040930     c                   Goto      endsrcksc
058160040930    2c                   EndIf
058170040930      * Controllo il network del p.o. del cliente non deve essere LED o SEDE
058180040930     c                   Movel     v1ksc         kazorg
058190040930     c                   Clear                   Og143
058200040930     c     kazorg        Chain     Azorg01l
058210040930     c                   If        %Found(Azorg01l)
058220040930     c                   Eval      Og143 = Orgde3
058230040930     c                   EndIf
058240040930if  1c                   If        §OgNtw = 'LOG' or §OgNtw = 'XXX'
058250040930     c                   Eval      v1cmsg = msg(16)
058260040930     c                   Seton                                        54  28
058270040930     c                   Goto      endsrcksc
058280040930e   2c                   EndIf
058290040930e   1c                   EndIf
058300010202      * Se codice tariffa inserito deve essere un numero
058310010202     C                   if        v1ctr <> *blanks  and
058320010320     C                             (%subst(v1ctr:1:1) < '0'  and
058330010320     C                              %subst(v1ctr:1:1) <> ' ') or
058340010320     C                             (%subst(v1ctr:2:1) < '0'  and
058350010320     C                              %subst(v1ctr:2:1) <> ' ') or
058360010320     C                             (%subst(v1ctr:3:1) < '0'   and
058370010320     C                              %subst(v1ctr:3:1) <> ' ')
058380010202     C                   movel     msg(16)       v1cmsg
058390010202     C                   seton                                        85  28
058400010202     C                   goto      endsrcksc
058410010202     C                   endif
058420040630      * non posso inserire solo la tariffa senza il codice cliente
058430040630     C                   if        v1ksc = *zeros and v1ctr <> *blanks
058440040630     C                   movel     msg(59)       v1cmsg
058450040630     C                   seton                                        85  28
058460040630     C                   goto      endsrcksc
058470040630     C                   endif
058480010202      * Controlla validità codice + tariffa
058490010202     C                   if        v1ksc <> *zeros and v1ctr <> *blanks
058500010202     C                   z-add     v1ksc         kksc
058510010202     C                   movel     v1ctr         kctr
058520090128     c                   eval      $notam = *off
058530090128     c     ktntam        setll     tntam01l
058540090128      * se non trovo errore
058550090128     c                   if        not %equal(tntam01l)
058560090128     c                   eval      v1cmsg = msg(16)
058570090128     c                   seton                                        54  28
058580090128     c                   leavesr
058590090128     c                   endif
058600090128     c                   do        *hival
058610090128     c     ktntam        Reade     Tntam01l
058620040713     C                   if        %Eof(tntam01l)
058630090128     C                   leavesr
058640010202     C                   endif
058650090128      * se annullato leggo altro record
058660010202     C                   if        TAMatb <> *blanks
058670090128     C                   iter
058680010202     C                   endif
058690090128      * trovato almeno 1 rcd ok
058700090128     c                   eval      $notam = *on
058710090128     c                   enddo
058720090128      * se non ho trovato nessuna tariffa errore
058730090128     c                   if        not $notam
058740090128     c                   eval      v1cmsg = msg(16)
058750090128     c                   seton                                        54  28
058760090128     c                   leavesr
058770090128     c                   endif
058780010202     C                   endif
058790010202
058800010202     C     endsrcksc     endsr
058810010223      **********************************************************************
058820010223      * CONTROLLI     * flag ddt *
058830010223      **********************************************************************
058840010223     C     Sr_Contrddt   begsr
058850010223
058860021119      * Da default immissione ORM
058870010223     C                   if        v1ddt = *blanks
058880010223     C                   eval      v1ddt = d§dftddt
058890010223     C                   endif
058900010223
058910010223     C                   endsr
058920010202      **********************************************************************
058930010202      * CONTROLLI     * P.O. consegna *
058940010202      **********************************************************************
058950010202     C     Sr_Contrpoc   begsr
058960010202
058970010320     C                   clear                   v1dpoc
058980060929     c                   clear                   c_og143
058990010320
059000010202     C                   if        v1cpoc <> *blanks
059010010202      * Ricerca P.O.Consegna con "?"
059020010202     C     '?'           scan      v1cpoc                                 30
059030010202     C     *in30         ifeq      *on
059040010202     C                   eval      *in55 = *on
059050010202     C                   eval      *in90 = *on
059060010202     C                   clear                   ds_tnsd
059070010202     C                   call      'TNSD24R'
059080010202     C                   parm                    ds_cod
059090010202     C                   parm                    ds_tip
059100010202     C                   parm                    ds_des
059110010202     C                   parm                    ds_pra
059120010202     C                   movel     ds_cod        v1cpoc
059130010202     C                   movel     ds_des        v1dpoc
059140010202     C                   endif
059150010316     C                   endif
059160010316      * Solo campi numerico
059170010320     C                   if        v1cpoc <> *blanks and
059180010320     C                             (%subst(v1cpoc:1:1) < '0' and
059190010320     C                              %subst(v1cpoc:1:1) <> ' ') or
059200010320     C                             (%subst(v1cpoc:2:1) < '0' and
059210010320     C                              %subst(v1cpoc:2:1) <> ' ') or
059220010320     C                             (%subst(v1cpoc:3:1) < '0' and
059230010320     C                              %subst(v1cpoc:3:1) <> ' ')
059240010316     C                   seton                                        55  28
059250010316     C                   movel     msg(17)       v1cmsg
059260010316     C                   goto      endsrcpoc
059270010316     C                   endif
059280010316      * Controllo
059290010202     C                   if        v1cpoc <> *blanks
059300010202     C                   movel     v1cpoc        kazorg
059310010202     C     kazorg        chain     azorg01L
059320010316     C                   if        not%found(azorg01l)
059330010316     C                   movel     msg(17)       v1cmsg
059340010316     C                   seton                                        55  28
059350010316     C                   goto      endsrcpoc
059360010316     C                   endif
059370010202     C                   if        ORGfva <> *blanks or
059380010202     C                             (ORGfag <> 'A' and ORGfag <> 'F')
059390010202     C                   movel     msg(17)       v1cmsg
059400010202     C                   seton                                        55  28
059410010202     C                   goto      endsrcpoc
059420010202     C                   endif
059430010202     C                   movel     ORGdes        v1dpoc
059440060929     c                   eval      c_og143 = orgde3
059450010202     C                   endif
059460111221
059470111221      /free
059480111221       //?Forzo la filiale di consegna in base alla tabella FFC
059490120116       //?se il destinatario è estero
059500150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
059510120116       IF  V1nac <> *blanks;
059520120116         IF  V1Cpoe = *zeros;
059530150305           w_V1Cpoe = DUTpou;
059540120116         ELSE;
059550120116           w_V1Cpoe = V1Cpoe;
059560120116         ENDIF;
059570120116         //?Provo con la filiale dell'ordinante se presente
059580120116         IF  ds_V1Ccor > 0;
059590120116           clear TIBS02ds;
059600120116           clear dFFC;
059610120116           T02mod = 'C';
059620120116           T02cod = 'FFC';
059630120116           T02ke1 = %editc(ds_V1Cor1:'X');
059640120116           T02sif = KNSIF;
059650120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
059660120116           IF  T02err = *blanks;
059670120116             dFFC = T02uni;
059680120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
059690120116               V1Cpoc = %editc(§FFCpoc:'X');
059700120116             ENDIF;
059710120116             leavesr;
059720120116           ENDIF;
059730120116         ENDIF;
059740120116         //?Se non trovo con Filiale Ordinante provo con Filiale Emissione
059750120116         //?se diversa da Filiale Ordinante
059760120116         IF  w_V1Cpoe <> ds_V1Cor1;
059770120116           clear TIBS02ds;
059780120116           clear dFFC;
059790120116           T02mod = 'C';
059800120116           T02cod = 'FFC';
059810120116           T02ke1 = %editc(w_V1Cpoe:'X');
059820120116           T02sif = KNSIF;
059830120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
059840120116           IF  T02err = *blanks;
059850120116             dFFC = T02uni;
059860120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
059870120116               V1Cpoc = %editc(§FFCpoc:'X');
059880120116             ENDIF;
059890120116             leavesr;
059900120116           ENDIF;
059910120116         ENDIF;
059920111221       ENDIF;
059930111221      /end-free
059940010202
059950010202     C     endsrcpoc     endsr
059960010202      **********************************************************************
059970010202      * CONTROLLI     * cliente destinatario *
059980010202      **********************************************************************
059990010202     C     Sr_Contrclid  begsr
060000110610
060010110610     c                   eval      $cacgdd = *off
060020010202
060030010202     C                   eval      ds_v1crc1 = v1crc1
060040010202     C                   eval      ds_v1crc2 = v1crc2
060050010202     C                   eval      ds_v1crc3 = v1crc3
060060010223
060070010223      * Se orm prepagato deve esserci il destinatario
060080010223     C                   if        v1ctor = 'P' and
060090010223     C                             (ds_v1ccrc = *zeros and v1rsc = *blanks)
060100010223     C                   movel     msg(50)       v1cmsg
060110010223     C                   seton                                        56  28
060120010223     C                   goto      endsrclid
060130010223     C                   endif
060140131107      /free
060150131107       //?Se sono in immissione/copia e sto inserendo il destinatario
060160131107       //?se il tipo ORM è impostato a Multiplo lo aggiorno in automatico
060170131107       //?in Singolo in modo da non dare il messaggio di errore
060180131107         IF  (*in01 or *in03) and
060190131107             ((sav_V1Ccrc <> ds_V1Ccrc and sav_V1Ccrc = 0) or
060200131107             (sav_V1rsc <> V1rsc and sav_V1rsc = *blanks)) and
060210131107             V1Ctor = 'M';
060220131107           V1Ctor = 'S';
060230131107         ENDIF;
060240131107      /end-free
060250131107
060260010223      * Se orm multiplo non deve esserci il destinatario
060270010223     C                   if        v1ctor = 'M' and
060280010223     C                             (ds_v1ccrc > *zeros or v1rsc <> *blanks)
060290010223     C                   movel     msg(38)       v1cmsg
060300160316     c                   eval      H1riga = 05
060310160316     c                   eval      H1colo = 11
060320160316     C                   seton                                            28
060330011114     C                   setoff                                       0616
060340010223     C                   goto      endsrclid
060350010223     C                   endif
060360011114
060370011114      * Se blocco destinatario l'ORM deve essere singolo e i dati del
060380011114      * destinatario devono essere completi
060390011114     C                   if        §ormfd = 'S'
060400011114     C                   if        v1ctor <> 'S' and *in38 = *off
060410011114     C                   movel     msg(74)       v1cmsg
060420160316     c                   eval      H1riga = 05
060430160316     c                   eval      H1colo = 11
060440160316     C                   seton                                          3828
060450011114     C                   setoff                                       0616
060460011114     C                   goto      endsrclid
060470011114     C                   endif
060480011114     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
060490011114     C                             and *in39 = *off
060500011114     C                   movel     msg(75)       v1cmsg
060510011114     C                   seton                                        563928
060520011114     C                   setoff                                       0616
060530011114     C                   goto      endsrclid
060540011114     C                   endif
060550011114     C                   endif
060560010223
060570010202      * Inserito codice
060580010202     C                   if        ds_v1ccrc <> *zeros
060590010202     C                   eval      *in06 = *on
060600010202     C                   exsr      Sr_Contrcrc
060610010202     C                   eval      sav_v1ccrc = ds_v1ccrc
060620010202     C                   else
060630010202      * Inserita ragione sociale
060640010202     C                   clear                   sav_v1ccrc
060650010202     C                   exsr      Sr_Contrrsc
060660131107     c                   eval      sav_V1rsc = V1rsc
060670010202     C                   eval      *in06 = *off
060680010202     C                   endif
060690110120     c   28              leavesr
060700011119      * Se ORM con blocco destinatario faccio già i controlli
060710011119     C                   if        §ormfd = 'S' and
060720011119     C                             (ds_v1ccrc <> *zeros or v1rsc <> *blanks)
060730011119     C                   eval      *in16 = *on
060740011119     C                   endif
060750060929
060760060929      * se oltre al destinatario è stato inserito anche il p.o. di consegna
060770060929      * devo fare un ulteriore controllo
060780060929      * ai fini dell'orm non serve, ma serve per impedire l'instradamento sbagliato
060790060929      * al momento della creazione della bolla legata a questo ORM
060800110120     c                   if        v1cpoc<> *blank and v1rsc <> *blanks
060810060929     c                   exsr      sr_contrcapc
060820060929     c                   endif
060830100811
060840100811      /free
060850100811       //?Se ORM import DPD devo controllare che la nazione di destino sia
060860100907       //?servita da DPD --> chiodo DPD come ntw.
060870150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
060880130107         IF  V1Cpoe = *zeros;
060890150305           w_V1Cpoe = DUTpou;
060900130107         ELSE;
060910130107           w_V1Cpoe = V1Cpoe;
060920130107         ENDIF;
060930130107         IF  %lookup(w_V1Cpoe:skpodpd) > 0 and v1nac <> *blanks and
060940101102             O14lad <= 0;
060950100909           v1cmsg = 'Nazione destino non servita da DPD. +
060960100909                     Richiedere al Depot l''annullamento';
060970100907           *in28 = *on;
060980100907           *in65 = *on;
060990100907           *in75 = *on;
061000100811         ENDIF;
061010160208
061020160209       //?Se inserito destinatario
061030160209         IF  ds_V1Ccrc > 0 or V1rsc <> *blanks;
061040160209         //?Se c'è la filiale di consegna uso quella
061050160209           IF  V1Cpoc > *zeros;
061060160209             O95lna = %dec(V1Cpoc:3:0);
061070160209           ELSE;
061080160209         //?Se non c'è la filiale di consegna
061090160209         //?richiamo routine sr_contrcapc per il calcolo della LNA
061100160209             exsr sr_contrcapc;
061110160209           ENDIF;
061120160209         //?Se ORM Prepagato e consegna a lna 340-341-345 ORM non possibile vedi DV 1540
061130160209           IF  (O95lna = 340 or O95lna = 341 or O95lna = 345) and
061140160209                V1Ctor = 'P';
061150160209             TBLcod = '15';
061160160209             TBLkey = v1nac;
061170160209             chain (CodUt:TBLcod:TBLkey) TABEL00F;
061180160209             IF  %found(TABEL00F);
061190160209               ds15 = TBLuni;
061200160209             ENDIF;
061210160209             V1Cmsg = 'Per consegne in';
061220160209             V1Cmsg = %trim(V1Cmsg) + ' ' + §15des;
061230160209             V1Cmsg = %trim(V1Cmsg) +
061240160209                      ' no pagamento a carico del Mittente';
061250160209             *in28 = *on;
061260160209             *in65 = *on;
061270160209             *in75 = *on;
061280160209           ENDIF;
061290160209         ENDIF;
061300160208
061310100811      /end-free
061320010202
061330010202     C     endsrclid     endsr
061340010202      **********************************************************************
061350010202      * CONTROLLI     * cliente destinatario *
061360010202      **********************************************************************
061370010202     C     Sr_Contrcrc   begsr
061380010202
061390010202     C                   z-add     ds_v1ccrc     kfnacr
061400151109     C     kfnacr        chain(n)  fnacr01l
061410010202     C                   if        %found(fnacr01l)
061420010202     C                   if        ACRatb <> *blanks
061430010202     C                   movel     msg(18)       v1cmsg
061440010202     C                   seton                                        56  28
061450010202     C                   goto      endsrccrc
061460010202     C                   endif
061470040727     C                   if        ACRtcr = 'M'
061480040727     C                   movel     msg(83)       v1cmsg
061490040727     C                   seton                                        56  28
061500040727     C                   goto      endsrccrc
061510040727     C                   endif
061520010906
061530010906      * Se modificato il cliente pulisco il ksc
061540010906     C                   if        v1pag = 'D'
061550010906     C                             and (ds_v1ccrc <> sav_v1ccrc
061560031010     C                             or v1pag <> sav_v1pag
061570040630     c                             or v1ksc = *Zeros
061580040630     c                             or V1Ctr = *Blanks)
061590031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocrc mi carica il ksc
061600040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
061610031010     c                   Clear                   sav_V1pag
061620031010     c                   EndIf
061630010906     C                   clear                   v1ksc
061640010906     C                   clear                   v1ctr
061650010906     C                   endif
061660010202     C                   exsr      Sr_Decocrc
061670010202     C                   else
061680010202     C                   movel     msg(18)       v1cmsg
061690010202     C                   seton                                        56  28
061700010202     C                   endif
061710010202
061720010202     C     endsrccrc     endsr
061730010202      **********************************************************************
061740011113      * DECODIFICHE     * cliente destinatario *
061750010202      **********************************************************************
061760010202     C     Sr_Decocrc    begsr
061770010202
061780010202     C                   movel     ACRrsr        v1rsc
061790010202     C                   movel     ACRinr        v1inc
061800010202     C                   movel     ACRcar        v1cac
061810010202     C                   movel     ACRlor        v1loc
061820030922     C                   movel     ACRlor        sav_v1loc35
061830010202     C                   movel     ACRprr        v1prc
061840010202     C                   movel     ACRnar        v1nac
061850010207      * Se paga destinatario memorizzo il codice piano dei conti
061860010207     C                   if        v1pag = 'D'
061870010906     C                             and (ds_v1ccrc <> sav_v1ccrc
061880010906     C                             or v1pag <> sav_v1pag)
061890010906     C                             and  v1ksc = *zeros
061900050120     c                   If        AcrKsc > *Zeros
061910010207     C                   eval      v1ksc = ACRksc
061920010903     C  nkf              eval      sav_v1ksc  = ACRksc
061930010309     C                   eval      sav_v1pag = v1pag
061940040629      * memorizzo anche il codice tariffa
061950040629     c                   If        AcrCcc = 999
061960040629     c                   Clear                   V1Ctr
061970040629     c                   Else
061980040629     c                   Move      AcrCcc        V1Ctr
061990040629     c                   EndIf
062000050120     c                   Else
062010050120     c                   Eval      V1Ksc = ds_v1crc12
062020050120     c                   EndIf
062030010207     C                   endif
062040010202
062050010202     C                   endsr
062060010202      **********************************************************************
062070011113      * CONTROLLI     * dati cliente destinatario *
062080010202      **********************************************************************
062090010202     C     Sr_Contrrsc   begsr
062100030922
062110030930     c                   If        V1loc = *Blanks or
062120030930     c                             (v1loc <> sav_v1loc)
062130030929     c                   Clear                   sav_v1loc35
062140030929     c                   EndIf
062150030922     c                   If        sav_v1loc35 = *blanks
062160030922     c                   Movel(p)  v1loc         sav_v1loc35
062170030922     c                   EndIf
062180131202
062190131202      /free
062200131202         IF  V1loc <> sav_V1loc or V1cac <> sav_V1cac or V1nac <> sav_V1nac;
062210131202           wForzaNAC = *off;
062220131202         ENDIF;
062230131202      /end-free
062240010202
062250010202     C                   clear                   FNLV14DS
062260010202     C                   z-add     *date         I14dri
062270010202     C                   movel     v1rsc         I14rsc
062280010202     C                   movel     v1inc         I14ind
062290030922     C                   movel(p)  sav_v1loc35   E14loc
062300010202     C                   movel     v1prc         E14prv
062310010202     C                   movel     v1cac         E14cap
062320010202     C                   movel     v1nac         E14nar
062330161109      /free
062340161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
062350161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
062360161109       //?il pgm di interrogazione cappario
062370161109         IF  ConfAuto;
062380161109           I14batch = 'S';
062390161109         ENDIF;
062400161109      /end-free
062410010202
062420010202     C                   call      'FNLV14R'
062430010202     C                   parm                    KPJBA
062440010202     C                   parm                    FNLV14DS
062450010202
062460010202     C                   movel     I14ind        v1inc
062470010202     C                   movel     E14loc        v1loc
062480030922     C                   movel     E14loc        sav_v1loc35
062490010202     C                   movel     E14prv        v1prc
062500010202     C                   movel     E14cap        v1cac
062510010202     C                   movel     E14nar        v1nac
062520010202
062530010202     C                   if        v1rsc <> *blanks or v1inc <> *blanks
062540010202     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
062550010202     C                   eval      *in61 = (O14err = '2')                       Indirizzo
062560010202     C                   eval      *in62 = (O14err = '5')                       CAP
062570010202     C                   eval      *in63 = (O14err = '3')                       Località
062580010202     C                   eval      *in64 = (O14err = '4')                       Provincia
062590010202     C                   eval      *in65 = (O14err = '6')                       Nazione
062600010202
062610010202     C                   if        O14msg <> *blanks
062620010202     C                   movel(p)  O14msg        v1cmsg
062630010202     C                   eval      *IN28 = *ON
062640010202     C                   eval      *in75 = *on
062650010202     C                   goto      endsrcrsc
062660010202     C                   endif
062670010202     C                   endif
062680010202
062690010202      * Controlla indirizzo
062700010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062710010202     C                             v1prc <> *blanks or v1nac <> *blanks
062720010202     C                   exsr      Sr_Contrcapc
062730010202     C                   eval      *in75 = (*in28 = *on)
062740010202     C   28              goto      endsrcrsc
062750010202     C                   endif
062760010202
062770010202      * Controlla se dati completi o parziali
062780010202     C                   if        v1rsc = *blanks
062790010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062800010202     C                             v1prc <> *blanks or v1nac <> *blanks
062810010202     C                   movel     msg(39)       v1cmsg
062820010202     C                   eval      *IN28 = *ON
062830010202     C                   eval      *in56 = *on
062840010202     C                   goto      endsrcrsc
062850010202     C                   endif
062860010202     C                   endif
062870131202      /free
062880131202       //?Se inserita la nazione e questa è lunga 2 byte e presente in tabella PR-provincia
062890131202       //?devo avvisare l'utente se è giusta la nazione o se per caso voleva inserire la
062900131202       //?provincia. Caso di nazione 'VE' accettata in immissione ORM perchè esiste la
062910131202       //?nazione VE Venezuale ma l'utente voleva inserire VE Venezia, ha sbagliato campo
062920131202         IF  V1rsc <> *blanks and V1nac <> *blanks and
062930131202            ((%subst(V1nac:1:1) = *blanks and %subst(V1nac:2:2) <> *blanks) or
062940131202             (%subst(V1nac:1:2) <> *blanks and %subst(V1nac:3:1) = *blanks));
062950131202           TBLcod = 'PR';
062960131202           TBLkey = v1nac;
062970131202           chain (CodUt:TBLcod:TBLkey) TABEL00F;
062980131202           IF  %found(TABEL00F) and TBLflg = *blanks and not wForzaNAC;
062990131202             TBLcod = '15';
063000131202             TBLkey = v1nac;
063010131202             chain (CodUt:TBLcod:TBLkey) TABEL00F;
063020131202             IF  %found(TABEL00F);
063030131202               ds15 = TBLuni;
063040131202             ENDIF;
063050131202             wForzaNAC = *on;
063060131202             sav_V1nac = V1nac;
063070131202             *in28 = *on;
063080131202             *in65 = *on;
063090131202             *in75 = *on;
063100131202             V1Cmsg = 'Verificare la nazione del destinatario';
063110131202             V1Cmsg = %trim(V1Cmsg) + ' ' +
063120131202                      %trim(V1nac) + '-' +
063130131202                      %trim(§15des);
063140131202             leavesr;
063150131202           ENDIF;
063160131202         ENDIF;
063170131202      /end-free
063180010207
063190010207      * Se paga il destinatario devo pulire il codice cliente che paga
063200010329     C                   if        v1pag = 'D'
063210010507     C                             and (v1pag <> sav_v1pag
063220010917     C                             or   sav_v1ccrc <> ds_v1ccrc)
063230010207     C                   clear                   v1ksc
063240010906     C                   clear                   v1ctr
063250010903     C  nkf              clear                   sav_v1ksc
063260010329     C                   eval      sav_v1pag = v1pag
063270010207     C                   endif
063280010202
063290010202     C     endsrcrsc     endsr
063300010202      **********************************************************************
063310010202      * CONTROLLI     * cap - località - provincia *
063320010202      **********************************************************************
063330010202     C     Sr_Contrcapc  begsr
063340010202
063350010202     C                   eval      *in32 = *off
063360010202
063370010202     C                   clear                   TISI95DS
063380010202     C                   clear                   FNLV13DS
063390080423     c                   clear                   tisi97ds
063400010202     C                   movel     ' '           I95tla
063410010202     C                   movel     '7'           I95tcn
063420010202     C                   movel     v1cac         I95cap
063430010202     C                   movel     v1nac         I95nar
063440010202     C                   movel     v1inc         I95ind
063450030922     C                   movel(p)  sav_v1loc35   I95loc
063460010202     C                   movel     v1prc         I95prv
063470010202     C                   movel     'S'           I13af0
063480010202     C                   movel     'S'           I13af1
063490010202     C                   movel     'S'           I13sz2
063500010202     C                   movel     'S'           I13la3
063510010202     C                   movel     'S'           I13sz3
063520010202     C                   movel     'S'           I13cnv
063530101215     c                   if        v1pag<>'M'
063540101214     c                   eval      i95tpo='A'
063550101214     c                   endif
063560010202
063570010202      * Gestione errori forzabili: se cambiato CAP/Località
063580010202      * riazzera i valori di errori forzabili
063590080910
063600080910      * CAP
063610080910     C     v1cac         ifne      sav_v1cac
063620080910     C                   movel     v1cac         sav_v1cac
063630080910     C                   clear                   wfzv
063640080910     C                   clear                   wfz1
063650080910     C                   clear                   wfz2
063660080910     C                   clear                   wfz3
063670080910     C                   eval      *in32 = *on
063680080910     C                   endif
063690010202
063700010202      * Modifica località
063710010202
063720010202     C     v1loc         ifne      sav_v1loc
063730010202     C                   clear                   wfz1
063740010202     C                   clear                   wfz2
063750010202     C                   clear                   wfz3
063760010202     C                   movel     v1loc         sav_v1loc
063770010202     C                   eval      *in32 = *on
063780010202     C                   endif
063790010202
063800010202      * Modifica provincia
063810010202
063820010202     C     v1prc         ifne      sav_v1prc
063830010202     C                   clear                   wfz3
063840010202     C                   movel     v1prc         sav_v1prc
063850010202     C                   eval      *in32 = *on
063860010202     C                   endif
063870010202
063880010202     C                   movel     wfzv          E13fzv
063890010202     C                   movel     wfz1          E13fz1
063900010202     C                   movel     wfz2          E13fz2
063910010202     C                   movel     wfz3          E13fz3
063920060929
063930060929     c                   eval      i97ntw = c_§ogntw
063940010202
063950010202     C                   call      'FNLV13R'
063960010202     C                   parm                    KPJBA
063970010202     C                   parm                    FNLV13DS
063980010202     C                   parm                    TISI95DS
063990060929     c                   parm      ' '           flgbac            1
064000060929     c                   parm                    tisi97ds
064010010202
064020010202      * Ricontrolla errori
064030010202
064040010202     C                   movel     O13err        werr
064050010202     C                   movel     O13msg        wmsg
064060010202     C                   movel     E13fzv        wfzv
064070010202     C                   movel     E13fz1        wfz1
064080010202     C                   movel     E13fz2        wfz2
064090010202     C                   movel     E13fz3        wfz3
064100010202
064110010202      * Se il livello di affidabilità non è inferiore a 2 e
064120010202      * il livello di reperimento dati non è inferiore a 3
064130010202      * reimposta i dati da TISI95DS
064140110120      * I dati non vengono sostituiti se i dati provengono da anagrafica
064150010202
064160110120     c                   if        ds_v1ccrc=0
064170010202     C     O13ron        ifeq      'S'
064180110120     C                   movel     O95nar        v1nac
064190110120     c                   endif
064200110120     C     O13roc        ifeq      'S'
064210110120     C                   movel     O95cap        v1cac
064220110120     c                   endif
064230110120     C     O13rop        ifeq      'S'
064240110120     C                   movel     O95prv        v1prc
064250110120     c                   endif
064260110120     C     O13rol        ifeq      'S'
064270010202     C                   movel     O95loc        v1loc
064280030922     C                   movel     O95loc        sav_v1loc35
064290010202     C                   end
064300161109
064310161109      /free
064320161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
064330161109       //?conferma automatica batch non devo dare errore
064340161109         IF  ConfAuto and O13roc = 'S';
064350161109           clear werr;
064360161109           clear O13roc;
064370161109         ENDIF;
064380161109      /end-free
064390010202
064400010202     C     werr          ifne      ' '
064410010202     C     wmsg          andne     *blanks
064420080910     C     *in32         oreq      *on
064430010202
064440010202     C                   select
064450010202
064460010202     C     werr          wheneq    '3'
064470010202     C                   eval      *in63 = *on
064480010202     C                   eval      *in28 = *on
064490010202
064500010202     C     werr          wheneq    '5'
064510010202     C                   eval      *in62 = *on
064520010202     C                   eval      *in28 = *on
064530010202
064540010202     C     werr          wheneq    '4'
064550010202     C                   eval      *in64 = *on
064560010202     C                   eval      *in28 = *on
064570010202
064580010202     C     werr          wheneq    '6'
064590010202     C                   eval      *in65 = *on
064600010202     C                   eval      *in28 = *on
064610010202     C                   endsl
064620010202
064630010202     C                   movel(p)  wmsg          v1cmsg
064640010202     C                   end
064650110120     c                   else
064660110120     C     werr          ifne      ' '
064670110120     C     wmsg          andne     *blanks
064680110120     C                   eval      *in56 = *on
064690110120     C                   eval      *in28 = *on
064700110120     C                   movel(p)  msg(94)       v1cmsg
064710110120     c                   endif
064720110120     c                   endif
064730010202
064740010202     C   28              goto      endsrcapc
064750010202
064760010202      * Controlla se reimpostata località
064770010202
064780010202     C     O13rol        ifeq      'S'
064790010202     C                   eval      *in63 = *on
064800010202     C                   eval      *in28 = *on
064810010202     C                   movel     msg(19)       v1cmsg
064820010202     C                   goto      endsrcapc
064830010202     C                   end
064840010202
064850010202      * Controlla se reimpostata Provincia
064860010202
064870010202     C     O13rop        ifeq      'S'
064880010202     C                   eval      *in64 = *on
064890010202     C                   eval      *in28 = *on
064900010202     C                   movel     msg(21)       v1cmsg
064910010202     C                   goto      endsrcapc
064920010202     C                   end
064930010202
064940010202      * Controlla se reimpostato cap
064950010202
064960010202     C     O13roc        ifeq      'S'
064970010202     C                   eval      *in62 = *on
064980010202     C                   eval      *in28 = *on
064990010202     C                   movel     msg(20)       v1cmsg
065000010202     C                   goto      endsrcapc
065010010202     C                   end
065020010202
065030010202      * Controlla se reimpostata Nazione
065040010202
065050010202     C     O13ron        ifeq      'S'
065060010202     C                   eval      *in65 = *on
065070010202     C                   eval      *in28 = *on
065080010202     C                   movel     msg(22)       v1cmsg
065090010202     C                   goto      endsrcapc
065100010202     C                   end
065110110610
065120110610      /free
065130110610       //?Mi salvo se il cap del destinatario prevede l'obbligo DDT
065140110610         $cacgdd = (O95gdd = 'S');
065150110610      /end-free
065160110610
065170010202
065180010202     C     endsrcapc     endsr
065190010208      **********************************************************************
065200010208      * CONTROLLI     * cliente ordinante *
065210010208      **********************************************************************
065220010208     C     Sr_Contrclio  begsr
065230010208
065240010208     C                   eval      ds_v1cor1 = v1cor1
065250010208     C                   eval      ds_v1cor2 = v1cor2
065260010208     C                   eval      ds_v1cor3 = v1cor3
065270160413      /free
065280160413       //?Se è stato variato il codice ordinante
065290160413       //?in caso di immissione ORM manuale a mittente non codificato
065300160413       //?devo reimpostare i dati per la conferma prenotazione
065310160413         IF  sav_V1Ccor <> ds_V1Ccor and
065320160413             (*in01 or *in03) and
065330160413             (V1Ctco = 'M' or V1Ctco = 'E') and
065340160413             ds_V1Ccra = 0;
065350160413           wOkF6F13 = *off;
065360160413           wRicConf = *off;
065370160413           clear W04sms;
065380160413           clear W04mail;
065390160413         ENDIF;
065400160413      /end-free
065410010208      * Inserito codice
065420010208     C                   if        ds_v1ccor <> *zeros
065430010208     C                   eval      *in08 = *on
065440010208     C                   exsr      Sr_Contrcor
065450010208     C                   eval      sav_v1ccor = ds_v1ccor
065460010208     C                   else
065470010208      * Inserita ragione sociale
065480010208     C                   clear                   sav_v1ccor
065490010208     C                   exsr      Sr_Contrrso
065500010208     C                   eval      *in08 = *off
065510010208     C                   endif
065520010208
065530010702     C                   endsr
065540010208      **********************************************************************
065550010208      * CONTROLLI     * cliente ordinante *
065560010208      **********************************************************************
065570010208     C     Sr_Contrcor   begsr
065580010208
065590010208     C                   z-add     ds_v1ccor     kfnacr
065600151109     C     kfnacr        chain(n)  fnacr01l
065610010208     C                   if        not%found(fnacr01l)
065620010208     C                   movel     msg(3)        v1cmsg
065630010208     C                   seton                                        42  28
065640010208     C                   goto      endsrccor
065650010208     C                   endif
065660010208     C                   if        ACRatb <> *blanks
065670010208     C                   movel     msg(3)        v1cmsg
065680010208     C                   seton                                        42  28
065690010208     C                   goto      endsrccor
065700010208     C                   endif
065710040727     C                   if        ACRtcr = 'M'
065720040727     C                   movel     msg(82)       v1cmsg
065730040727     C                   seton                                        42  28
065740040727     C                   goto      endsrccor
065750040727     C                   endif
065760010906      * Se modificato il cliente pulisco il ksc
065770010906     C                   if        v1pag = 'O'
065780010906     C                             and (ds_v1ccor <> sav_v1ccor
065790031010     C                             or v1pag <> sav_v1pag
065800040630     c                             or   v1ksc = *Zeros
065810040630     c                             or   V1Ctr = *Blanks)
065820031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocro mi carica il ksc
065830040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
065840031010     c                   Clear                   sav_V1pag
065850031010     c                   EndIf
065860010906     C                   clear                   v1ksc
065870010906     C                   clear                   v1ctr
065880010906     C                   endif
065890010208     C                   exsr      Sr_Decocor
065900010208
065910010208     C     endsrccor     endsr
065920010208      **********************************************************************
065930010208      * DECODIFICHE     * cliente ordinante *
065940010208      **********************************************************************
065950010208     C     Sr_Decocor    begsr
065960010208
065970010208     C                   movel     ACRrsr        v1rso
065980010208     C                   movel     ACRinr        v1ino
065990010208     C                   movel     ACRcar        v1cao
066000010208     C                   movel     ACRlor        v1loo
066010030922     C                   movel     ACRlor        sav_v1loo35
066020010208     C                   movel     ACRprr        v1pro
066030010208     C                   movel     ACRnar        v1nao
066040010208      * Se paga ordinante memorizzo il codice piano dei conti
066050030902      * se non c'è il ksc memorizzo i primi 7 byte del codice anagrafica ritiro
066060010208     C                   if        v1pag = 'O'
066070010906     C                             and (ds_v1ccor <> sav_v1ccor
066080010906     C                             or v1pag <> sav_v1pag)
066090010906     C                             and  v1ksc = *zeros
066100030902     c                   If        AcrKsc > *Zeros
066110010208     C                   eval      v1ksc = ACRksc
066120040629      * memorizzo anche il codice tariffa
066130040629     c                   If        AcrCcc = 999
066140040629     c                   Clear                   V1Ctr
066150040629     c                   Else
066160040629     c                   Move      AcrCcc        V1Ctr
066170040629     c                   EndIf
066180030902     c                   Else
066190030902     c                   Eval      V1Ksc = Ds_V1cor12
066200030902     c                   EndIf
066210030902     C  nkf              eval      sav_v1ksc  = V1ksc
066220010309     C                   eval      sav_v1pag = v1pag
066230010208     C                   endif
066240030606      * lo salvo comunque xchè mi può servire nel caso di creazione
066250030606      * bolla x orm commissionato
066260030606     c                   Eval      AcrKscCor = AcrKsc
066270050202     c                   Eval      AcrCccCor = AcrCcc
066280040405
066290040405      * recupero le note del cliente ordinante
066300040405      * se il cliente ritiro non è codificato
066310040405if  1c                   if        ds_v1ccor <> sav_v1ccor and ds_v1ccra = *Zeros
066320040726      * solo se non sono già state immesse
066330040405     c                   If        v1not1 = *Blanks
066340040405     c                   Eval      V1not1 = AcrNo1
066350040405     c                   EndIf
066360040405     c                   If        v1not2 = *Blanks
066370040405     c                   Eval      V1not2 = AcrNo2
066380040405     c                   EndIf
066390040405e   1c                   EndIf
066400050321
066410050321      * imposto se ORM commissionato o meno
066420050323     c                   If        *In01
066430050322     c                             and (sav_v1ccor <> ds_v1ccor or
066440050322     c                                  v1com = *blanks)
066450050321     c                   Eval      v1com = AcrFcc
066460050322     c                   EndIf
066470010208
066480010208     C                   endsr
066490010208      **********************************************************************
066500010208      * CONTROLLI     * dati cliente ordinante *
066510010208      **********************************************************************
066520010208     C     Sr_Contrrso   begsr
066530030922
066540030930     c                   If        V1loo = *Blanks or
066550030930     c                             (v1loo <> sav_v1loo)
066560030929     c                   Clear                   sav_v1loo35
066570030929     c                   EndIf
066580030922     c                   If        sav_v1loo35 = *blanks
066590030922     c                   Movel(p)  v1loo         sav_v1loo35
066600030922     c                   EndIf
066610010208
066620010208     C                   clear                   FNLV14DS
066630010208     C                   z-add     *date         I14dri
066640010208     C                   movel     v1rso         I14rsc
066650010208     C                   movel     v1ino         I14ind
066660030922     C                   movel(p)  sav_v1loo35   E14loc
066670010208     C                   movel     v1pro         E14prv
066680010208     C                   movel     v1cao         E14cap
066690010208     C                   movel     v1nao         E14nar
066700161109      /free
066710161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
066720161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
066730161109       //?il pgm di interrogazione cappario
066740161109         IF  ConfAuto;
066750161109           I14batch = 'S';
066760161109         ENDIF;
066770161109      /end-free
066780010208
066790010208     C                   call      'FNLV14R'
066800010208     C                   parm                    KPJBA
066810010208     C                   parm                    FNLV14DS
066820010208
066830010208     C                   movel     I14ind        v1ino
066840010208     C                   movel     E14loc        v1loo
066850030922     C                   movel     E14loc        sav_v1loo35
066860010208     C                   movel     E14prv        v1pro
066870010208     C                   movel     E14cap        v1cao
066880010208     C                   movel     E14nar        v1nao
066890010220
066900010220     C                   if        v1rso <> *blanks or v1ino <> *blanks
066910010220     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
066920010220     C                   eval      *in61 = (O14err = '2')                       Indirizzo
066930010220     C                   eval      *in62 = (O14err = '5')                       CAP
066940010220     C                   eval      *in63 = (O14err = '3')                       Località
066950010220     C                   eval      *in64 = (O14err = '4')                       Provincia
066960010220     C                   eval      *in65 = (O14err = '6')                       Nazione
066970010220
066980010220     C                   if        O14msg <> *blanks
066990010220     C                   movel(p)  O14msg        v1cmsg
067000010220     C                   eval      *IN28 = *ON
067010010220     C                   eval      *in74 = *on
067020010220     C                   goto      endsrcrso
067030010220     C                   endif
067040010220     C                   endif
067050010208
067060010208      * Controlla indirizzo
067070010208     C                   if        v1cao <> *blanks or v1loo <> *blanks or
067080010208     C                             v1pro <> *blanks or v1nao <> *blanks
067090010208     C                   exsr      Sr_Contrcapo
067100010208     C                   eval      *in74 = (*in28 = *on)
067110010208     C   28              goto      endsrcrso
067120010208     C                   endif
067130010208      * Se paga l'ordinante devo pulire il codice cliente che paga
067140010329     C                   if        v1pag = 'O'
067150010507     C                             and (v1pag <> sav_v1pag
067160010917     C                             or   sav_v1ccor <> ds_v1ccor)
067170010208     C                   clear                   v1ksc
067180010906     C                   clear                   v1ctr
067190010507     C  nkf              clear                   sav_v1ksc
067200010329     C                   eval      sav_v1pag = v1pag
067210010208     C                   endif
067220010208
067230010208     C     endsrcrso     endsr
067240010208      **********************************************************************
067250010208      * CONTROLLI     * cap - località - provincia *
067260010208      **********************************************************************
067270010208     C     Sr_Contrcapo  begsr
067280010208
067290010208     C                   eval      *in32 = *off
067300010208
067310010208     C                   clear                   TISI95DS
067320010208     C                   clear                   FNLV13DS
067330010208     C                   movel     ' '           I95tla
067340010208     C                   movel     '7'           I95tcn
067350010208     C                   movel     v1cao         I95cap
067360010208     C                   movel     v1nao         I95nar
067370010208     C                   movel     v1ino         I95ind
067380030922     C                   movel(p)  sav_v1loo35   I95loc
067390010208     C                   movel     v1pro         I95prv
067400010208     C                   movel     'S'           I13af0
067410010208     C                   movel     'S'           I13af1
067420010208     C                   movel     'S'           I13sz2
067430010208     C                   movel     'S'           I13la3
067440010208     C                   movel     'S'           I13sz3
067450010208     C                   movel     'S'           I13cnv
067460010208
067470010906      * Gestione errori forzabili: se cambiato CAP/Località
067480010208      * riazzera i valori di errori forzabili
067490010208
067500080910      * CAP
067510080910     C     v1cao         ifne      sav_v1cao
067520080910     C                   movel     v1cao         sav_v1cao
067530080910     C                   clear                   wfzv
067540080910     C                   clear                   wfz1
067550080910     C                   clear                   wfz2
067560080910     C                   clear                   wfz3
067570080910     C                   eval      *in32 = *on
067580080910     C                   endif
067590010208
067600010208      * Modifica località
067610010208
067620010208     C     v1loo         ifne      sav_v1loo
067630010208     C                   clear                   wfz1
067640010208     C                   clear                   wfz2
067650010208     C                   clear                   wfz3
067660010208     C                   movel     v1loo         sav_v1loo
067670010208     C                   eval      *in32 = *on
067680010208     C                   endif
067690010208
067700010208      * Modifica provincia
067710010208
067720010208     C     v1pro         ifne      sav_v1pro
067730010208     C                   clear                   wfz3
067740010208     C                   movel     v1pro         sav_v1pro
067750010208     C                   eval      *in32 = *on
067760010208     C                   endif
067770010208
067780010208     C                   movel     wfzv          E13fzv
067790010208     C                   movel     wfz1          E13fz1
067800010208     C                   movel     wfz2          E13fz2
067810010208     C                   movel     wfz3          E13fz3
067820010208
067830010208     C                   call      'FNLV13R'
067840010208     C                   parm                    KPJBA
067850010208     C                   parm                    FNLV13DS
067860010208     C                   parm                    TISI95DS
067870010208
067880010208      * Ricontrolla errori
067890010208
067900010208     C                   movel     O13err        werr
067910010208     C                   movel     O13msg        wmsg
067920010208     C                   movel     E13fzv        wfzv
067930010208     C                   movel     E13fz1        wfz1
067940010208     C                   movel     E13fz2        wfz2
067950010208     C                   movel     E13fz3        wfz3
067960010208
067970010208      * Se il livello di affidabilità non è inferiore a 2 e
067980010208      * il livello di reperimento dati non è inferiore a 3
067990010208      * reimposta i dati da TISI95DS
068000010208
068010010208     C     O13ron        ifeq      'S'
068020110120     C                   movel     O95nar        v1nao
068030110120     c                   endif
068040110120     C     O13roc        ifeq      'S'
068050110120     C                   movel     O95cap        v1cao
068060110120     c                   endif
068070110120     C     O13rop        ifeq      'S'
068080110120     C                   movel     O95prv        v1pro
068090110120     c                   endif
068100110120     C     O13rol        ifeq      'S'
068110010208     C                   movel     O95loc        v1loo
068120030922     C                   movel     O95loc        sav_v1loo35
068130010208     C                   end
068140161109
068150161109      /free
068160161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
068170161109       //?conferma automatica batch non devo dare errore
068180161109         IF  ConfAuto and O13roc = 'S';
068190161109           clear werr;
068200161109           clear O13roc;
068210161109         ENDIF;
068220161109      /end-free
068230010208
068240010208     C     werr          ifne      ' '
068250010208     C     wmsg          andne     *blanks
068260080910     C     *in32         oreq      *on
068270010208
068280010208     C                   select
068290010208
068300010208     C     werr          wheneq    '3'
068310010208     C                   eval      *in63 = *on
068320010208     C                   eval      *in28 = *on
068330010208
068340010208     C     werr          wheneq    '5'
068350010208     C                   eval      *in62 = *on
068360010208     C                   eval      *in28 = *on
068370010208
068380010208     C     werr          wheneq    '4'
068390010208     C                   eval      *in64 = *on
068400010208     C                   eval      *in28 = *on
068410010208
068420010208     C     werr          wheneq    '6'
068430010208     C                   eval      *in65 = *on
068440010208     C                   eval      *in28 = *on
068450010208     C                   endsl
068460010208      *
068470010208     C                   movel(p)  wmsg          v1cmsg
068480010208     C                   end
068490010208
068500010208     C   28              goto      endsrcapo
068510010208
068520010208      * Controlla se reimpostata località
068530010208
068540010208     C     O13rol        ifeq      'S'
068550010208     C                   eval      *in63 = *on
068560010208     C                   eval      *in28 = *on
068570010208     C                   movel     msg(19)       v1cmsg
068580010208     C                   goto      endsrcapo
068590010208     C                   end
068600010208
068610010208      * Controlla se reimpostata Provincia
068620010208
068630010208     C     O13rop        ifeq      'S'
068640010208     C                   eval      *in64 = *on
068650010208     C                   eval      *in28 = *on
068660010208     C                   movel     msg(21)       v1cmsg
068670010208     C                   goto      endsrcapo
068680010208     C                   end
068690010208
068700010208      * Controlla se reimpostato cap
068710010208
068720010208     C     O13roc        ifeq      'S'
068730010208     C                   eval      *in62 = *on
068740010208     C                   eval      *in28 = *on
068750010208     C                   movel     msg(20)       v1cmsg
068760010208     C                   goto      endsrcapo
068770010208     C                   end
068780010208
068790010208      * Controlla se reimpostata Nazione
068800010208
068810010208     C     O13ron        ifeq      'S'
068820010208     C                   eval      *in65 = *on
068830010208     C                   eval      *in28 = *on
068840010208     C                   movel     msg(22)       v1cmsg
068850010208     C                   goto      endsrcapo
068860010208     C                   end
068870010208
068880010208     C     endsrcapo     endsr
068890050323
068900050323      *--------------------------------------------------------------------*
068910050323      * CONTROLLO PADRE/FIGLI ordinante con codice ritiro
068920050323      *--------------------------------------------------------------------*
068930050323     c     Sr_Figli      BegSr
068940050323
068950050323      * controllo se l'ordinante è un papà
068960050323     c                   Clear                   Tibs10ds
068970050323     c                   Eval      d10tle = 'WW'
068980050323     c                   Eval      d10paf = 'F'
068990050323     c                   Move      ds_v1cor12    d10cod
069000050323     c                   Eval      d10drf = *date
069010050323     c                   Call      'TIBS10R'
069020050323     c                   Parm                    Tibs10ds
069030050323if  1c                   If        d10err <> *Blanks
069040050323      * controllo se il codice inserito è un figlio
069050050323     c                   Clear                   Tibs10ds
069060050323     c                   Eval      d10tle = 'WW'
069070050323     c                   Eval      d10paf = 'P'
069080050323     c                   Move      ds_v1cor12    d10cod
069090050323     c                   Eval      d10drf = *date
069100050323     c                   Call      'TIBS10R'
069110050323     c                   Parm                    Tibs10ds
069120050323if  2c                   If        d10err <> *Blanks
069130050323     c                   Clear                   skfigli
069140050323e   2c                   EndIf
069150050323e   1c                   EndIf
069160050323
069170050323     c                   EndSr
069180050323
069190001016      **********************************************************************
069200001016      * CONTROLLI     * P.O. ritiro *
069210001016      **********************************************************************
069220001016     C     Sr_Contrpor   begsr
069230131024
069240010413      * Controllo il p.o. di ritiro
069250010413     C                   clear                   v1dpor
069260010202
069270010413      * Ricerca P.O.Ritiro  con "?"
069280010413     C     '?'           scan      v1cpor                                 30
069290010413     C     *in30         ifeq      *on
069300010413     C                   eval      *in59 = *on
069310010413     C                   eval      *in90 = *on
069320010413     C                   clear                   ds_tnsd
069330010413     C                   call      'TNSD24R'
069340010413     C                   parm                    ds_cod
069350010413     C                   parm                    ds_tip
069360010413     C                   parm                    ds_des
069370010413     C                   parm                    ds_pra
069380010413     C                   movel     ds_cod        v1cpor
069390010413     C                   movel     ds_des        v1dpor
069400010413     C                   endif
069410001023
069420010316      * Solo campi numerico
069430010320     C                   if        v1cpor = *blanks and
069440010320     C                             (%subst(v1cpor:1:1) < '0' and
069450010320     C                              %subst(v1cpor:1:1) <> ' ') or
069460010320     C                             (%subst(v1cpor:2:1) < '0' and
069470010320     C                              %subst(v1cpor:2:1) <> ' ') or
069480010320     C                             (%subst(v1cpor:3:1) < '0' and
069490010320     C                              %subst(v1cpor:3:1) <> ' ')
069500010316     C                   seton                                        59  28
069510010316     C                   movel     msg(33)       v1cmsg
069520010316     C                   goto      endsrcpor
069530010316     C                   endif
069540010202      * Obbligatorio
069550010315     C                   if        v1cpor = *blanks
069560010202     C                   movel     msg(11)       v1cmsg
069570010202     C                   seton                                        59  28
069580010202     C                   goto      endsrcpor
069590010202     C                   endif
069600060109
069610060109      * Se immissione ORM e p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
069620060109      * come p.o. ritiro OMR
069630060110     c                   If        (*In01 or *In03 ) and wforzapor = *Off
069640060109     c                   Clear                   fnlv55ds
069650060109     c                   Eval      d55tpt = '6'
069660060109     c                   Move      v1cpor        d55lin
069670060109     c                   Eval      d55drf = oggi_aammgg
069680060109     c                   Call      'FNLV55R'
069690060109     c                   Parm                    Fnlv55ds
069700060109     c                   If        d55err = *Blanks
069710060109     c                   Move      d55tfa        v1cpor
069720060109     c                   Eval      wforzapor = *On
069730060109     c                   EndIf
069740060109     c                   EndIf
069750151203
069760151203      /free
069770151203       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
069780151203       //?solo se immissione/copia ma no conferma ritiri fissi
069790151203       //?o manutenzione dopo dirottamento
069800151203         IF  ((*in01 or *in03) and §RMtco <> 'S') or
069810151203              (*in09 and wokdirotta = *on);
069820151203           clear FIOR96ds;
069830151203       //?se la filiale emissione non è ancora stata impostata passo la filiale utente
069840151203           IF  V1Cpoe = *zeros;
069850151203             IOR96poe = DUTpou;
069860151203           ELSE;
069870151203             IOR96poe = V1Cpoe;
069880151203           ENDIF;
069890151203           IOR96por = %dec(V1Cpor:3:0);
069900151203           fior96r(kpjba : fior96ds);
069910151203           IF  OOR96err = *blanks and OOR96por <> *zeros;
069920151203             V1Cpor = %editc(OOR96por:'X');
069930151203           ENDIF;
069940151203         ENDIF;
069950151203      /end-free
069960060109
069970010202      * Controllo
069980131112     c                   clear                   sav_ORGfl1
069990131210     c                   clear                   ntw_V1Cpor
070000001016     C                   movel     v1cpor        kazorg
070010001016     C     kazorg        chain     azorg01L
070020010202     C                   if        not%found(azorg01l)
070030010202     C                   movel     msg(33)       v1cmsg
070040010202     C                   seton                                        59  28
070050010202     C                   goto      endsrcpor
070060010202     C                   endif
070070001204     C                   if        ORGfva <> *blanks  or
070080001204     C                             (ORGfag <> 'A' and ORGfag <> 'F')
070090001017     C                   movel     msg(33)       v1cmsg
070100001016     C                   seton                                        59  28
070110001019     C                   goto      endsrcpor
070120001019     C                   endif
070130131112     c                   eval      sav_ORGfl1 = ORGfl1
070140080812     C                   movel     orgde3        og143
070150010206     C                   movel     ORGde8        og148
070160131210     c                   eval      ntw_V1Cpor = §OGntw
070170010206      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
070180010206     C                   if        §ogorm <> 'S'
070190010206     C                   movel     msg(36)       v1cmsg
070200010206     C                   seton                                        59  28
070210010206     C                   goto      endsrcpor
070220010206     C                   endif
070230131024
070240131024     C                   movel     ORGdes        v1dpor
070250131024     C                   movel     v1cpor        num_v1cpor
070260131024
070270131024      /free
070280150310       //?Imposto se filiale ritiro ha la procedura PDA attiva
070290150310         *in22 = (§ogpdaorm <> *blanks);
070300131024      /end-free
070310131024
070320060110      * Se il p.o. immesso a video è un p.o. gestito
070330060110      * errore per p.o. non gestibile
070340060110     c                   Clear                   fnlv55ds
070350060110     c                   Eval      d55tpt = '6'
070360060110     c                   Move      v1cpor        d55lin
070370060110     c                   Eval      d55drf = oggi_aammgg
070380060110     c                   Call      'FNLV55R'
070390060110     c                   Parm                    Fnlv55ds
070400060110     c                   If        d55err = *Blanks and d55tfa <> d55lin
070410060110     c                   movel     msg(89)       v1cmsg
070420060110     c                   seton                                        59  28
070430060110     c                   goto      endsrcpor
070440060110     c                   EndIf
070450080812
070460080812      * controllo se ritiro all'estero possibile
070470131030     c                   if        ntw_V1Cpor = 'DPD' or ntw_V1Cpor = 'EEX' or
070480131030     c                             ntw_V1Cpor = 'FED'
070490080812     c                   clear                   fnlv12ds
070500111206     c                   clear                   tisi95ds1
070510080812     c                   clear                   tisi97ds
070520111206     c                   eval      ai95nar = v1nar
070530111206     c                   eval      ai95cap = v1car
070540111206     c                   eval      ai95dat = oggi_aammgg
070550131030     c                   eval      i97ntw = ntw_V1Cpor
070560081215      * se impostato network DPD lo passo
070570081215     c                   if        v1ntwd <> *blanks
070580111206     c                   eval      ai95fi1 = v1ntwd
070590081215     c                   else
070600111206     c                   clear                   ai95fi1
070610081215     c                   endif
070620080812     c                   call      'FNLV12R'
070630080812     c                   parm                    fnlv12ds
070640111206     c                   parm                    tisi95ds1
070650080812     c                   parm                    tisi97ds
070660080812     c                   if        o12err <> *blanks
070670080812     c                   eval      v1cmsg = o12msg
070680080812     c                   seton                                        59  28
070690080812     c                   goto      endsrcpor
070700080812     c                   endif
070710080812     c                   endif
070720081215      * controllo congruenza tra network DPD e po ritiro
070730081215      * in immissione o copia
070740131030     c                   if        v1ntwd <> *blanks and ntw_V1Cpor <> 'DPD'
070750081215     c                             and (*in01 or *in03)
070760081215     c                   eval      *in28 = *on
070770081215     c                   eval      *in59 = *on
070780081215     c                   eval      v1cmsg = 'Indicato Network DPD. Incongruenza-
070790081215     c                              con la filiale ritiro'
070800081215     c                   goto      endsrcpor
070810081215     c                   endif
070820100219
070830100219      /free
070840100219       //?Se inserito ntw DPD richiamo il controllo del cappario DPD
070850110216       //?oppure filiale ritiro è DPD
070860131030       IF  v1ntwd <> *blanks or ntw_V1Cpor = 'DPD';
070870100811         wNazione = v1nar;
070880100811         wCapDPD  = v1car;
070890100811         exsr CtrCapDPD;
070900100811         IF  *in28;
070910100811           *in50 = *on;
070920100811           *in79 = *on;
070930100811           v1cmsg = %trim(v1cmsg) + ' F15 x interrogazione Cappario DPD';
070940100811           leavesr;
070950100811         ENDIF;
070960100219       ENDIF;
070970100219      /end-free
070980080812
070990010515      * Se p.o. estero controllare se DPD o EUROEXPRESS
071000010515     C                   eval      *in14 = *off
071010010515     C                   eval      *in15 = *off
071020131112     C                   if        sav_ORGfl1 = 'E'
071030010515     C                   eval      *in14 = *on
071040010515     C                   endif
071050131030     C                   if        ntw_V1Cpor = 'DPD'
071060010515     C                   eval      *in15 = *on
071070010515     C                   endif
071080010417
071090010213      * Se il p.o. ritiro è della mia £6 l'ORM non può essere prepagato
071100010213      * a meno che non sia richiamato da programma immissione prepagati
071110150609      * in immissione ORM
071120010704     C                   if        v1ctor = 'P' and §RMfpr <> 'B'
071130150609     C                             and §RMfpr <> 'C'
071140010307     C     num_v1cpor    lookup    l6                                     30
071150010307     C                   if        *in30 = *on
071160150609     C                             and *in01
071170010307     C                   movel     msg(48)       v1cmsg
071180160316     c                   eval      H1riga = 05
071190160316     c                   eval      H1colo = 11
071200160316     C                   seton                                            28
071210010307     C                   goto      endsrcpor
071220010307     C                   endif
071230010307     C                   endif
071240010417
071250010417     C                   if        %subst(v1cpor:1:1) = ' '  and
071260010417     C                             %subst(v1cpor:2:1) = ' '
071270010417     C                   movel     '00'          v1cpor
071280010417     C                   endif
071290010417     C                   if        %subst(v1cpor:1:1) = ' '
071300010417     C                   movel     '0'           v1cpor
071310010417     C                   endif
071320010417
071330010417     C                   if        v1cpor <> sav_v1cpor
071340010417     C                   eval      *in89 = *off
071350161116     c                   IF        sav_v1cpor <> *zeros and
071360161116     c                             sav_v1cpor <> *blanks
071370161116     c                   eval      wModPor = *on
071380161116     c                   ENDIF
071390010417     C                   movel     v1cpor        sav_v1cpor
071400071023     c                   clear                   v1ccgi
071410071023     c                   clear                   sav_v1ccgi
071420071023     c                   clear                   sav_acrcgi
071430010417     C                   endif
071440010515
071450010521      * Se p.o. ritiro Euroexpress o DPD obbligatorio il peso e
071460010521      * i colli
071470010521     C                   if        *in14 = *on or *in15 = *on
071480010521     C                   if        v1pkg = *zeros
071490010515     C                   movel     msg(5)        v1cmsg
071500010515     C                   seton                                        44  28
071510010515     C                   goto      endsrcpor
071520010521     C                   endif
071530010521     C                   if        v1ncl = *zeros
071540010521     C                   movel     msg(4)        v1cmsg
071550010521     C                   seton                                        43  28
071560010521     C                   goto      endsrcpor
071570010521     C                   endif
071580081216      * no prepagato e no pagamento a carico del mittente
071590081216     c                   if        v1ctor = 'P'
071600160316     c                   eval      H1riga = 05
071610160316     c                   eval      H1colo = 11
071620081216     c                   eval      *in28 = *on
071630081216     c                   eval      v1cmsg = 'Per ORM Export non si può fare un -
071640081216     c                             prepagato'
071650081216     c                   goto      endsrcpor
071660081216     c                   endif
071670081216     c                   if        v1pag = 'M'
071680081216     c                   eval      *in28 = *on
071690081216     c                   eval      *in53 = *on
071700081216     c                   eval      v1cmsg = 'Per ORM Export non può pagare il -
071710081216     c                             mittente'
071720081216     c                   goto      endsrcpor
071730081216     c                   endif
071740010515     C                   endif
071750080422
071760080422      * se filiale ritiro DPD controllo i colli
071770080422      * deve essere obbligatoriamente 1
071780080422     c                   if        *in15 = *on and v1ncl > 1
071790080422     c                   eval      v1cmsg = msg(90)
071800080422     c                   eval      *in28 = *on
071810080422     c                   eval      *in43 = *on
071820080422     c                   leavesr
071830080422     c                   endif
071840081126      * se orm export DPD il peso non può essere maggiore del limite stabilito
071850081126     c                   if        *in15 and v1pkg > §3ipkd
071860081126     c                   eval      v1cmsg = msg(92)
071870081126     c                   eval      *in28 = *on
071880081126     c                   eval      *in44 = *on
071890081126     c                   leavesr
071900081126     c                   endif
071910081126      * se orm export DPD obbligatorio il destinatario
071920081127     c                   if        *in15 and ds_v1ccrc = *zeros and
071930081126     c                             v1rsc = *blanks
071940081126     c                   eval      v1cmsg = msg(93)
071950081126     c                   eval      *in28 = *on
071960081211     c                   eval      *in56 = *on
071970081126     c                   leavesr
071980081126     c                   endif
071990010417
072000010417      * Controllo p.o. ritiro del video con quello calcolato
072010020523      * non in chiusura orm
072020050711      * non se p.o. ritiro protetto o se è richiamato da programma
072030050711      * immissione prepagati
072040060109      * se non forzato da me
072050150921      * non controllo se conferma automatica VAS
072060150921     c                   IF        not ConfAuto
072070050711     c                   If        Not *In09 or §rmfpr = 'B'
072080060109     c                   if        wforzapor = *Off
072090010417     C                   if        v1cpos <> *zeros     and
072100010417     C                             v1cpos <> num_v1cpor and
072110020523     C                             *in89 = *off and *in10 = *off
072120010417     C                   movel     msg(62)       v1cmsg
072130010417     C                   seton                                        895928
072140010417     C                   goto      endsrcpor
072150010417     C                   endif
072160010417     C                   if        sav_acrpoa <> *zeros     and
072170010417     C                             sav_acrpoa <> num_v1cpor and
072180150921     C                             *in89 = *off and not *in10
072190010417     C                   movel     msg(62)       v1cmsg
072200010417     C                   seton                                        895928
072210010417     C                   goto      endsrcpor
072220010417     C                   endif
072230131111     C                   if        sav_O95lna <> *zeros       and
072240131111     C                             sav_O95lna <> num_v1cpor   and
072250010417     C                             sav_acrpoa = *zeros    and
072260150921     C                             V1Cpos = *zeros    and
072270150921     C                             *in89 = *off and not *in10
072280010417     C                   movel     msg(62)       v1cmsg
072290010417     C                   seton                                        895928
072300010417     C                   goto      endsrcpor
072310010417     C                   endif
072320060109     c                   EndIf
072330040420     c                   EndIf
072340150921     c                   ENDIF
072350010417
072360010417     C                   movel     v1cpor        sav_v1cpor
072370071031
072380071031      * recupero gli ambiti per il recupero del giro da ACR
072390071031     c                   exsr      sr_caragr
072400010301
072410001016     C     endsrcpor     endsr
072420070920
072430070913      *------------------------------------------------------------------------*
072440070913      * CONTROLLI     * giro
072450070913      *------------------------------------------------------------------------*
072460070913     c     sr_contrcgi   begsr
072470070919
072480070919     c                   clear                   v1dcgi
072490160325     c                   eval      wModGiroAut = *off
072500071105
072510071105      * ricerca giro
072520071105     c                   if        %scan('?':v1ccgi) > *zeros
072530071105     c                   clear                   fidg09ds
072540071105     c                   eval      d09iop0 = 'P01'
072550071105     c                   eval      d09ifgs = num_v1cpor
072560071105     c                   eval      d09idat = *date
072570071105     c                   eval      d09itug = 'R'
072580071105     c                   eval      kpjbu = fidg09ds
072590071105     c                   call      'FIDG09R'
072600071105     c                   parm                    kpjba
072610071105     c                   eval      fidg09ds = kpjbu
072620071105      *     imposto il giro scelto
072630071105     c                   if        d09ocgi <> *blanks
072640071105     c                   eval      v1ccgi = d09ocgi
072650071105     c                   endif
072660071105     c                   endif
072670070919
072680071023      * Aggancio ACR e recupero il giro
072690071031     c                   if        (sav_acrcgi = *blanks and
072700071031     c                              ds_v1ccra <> *zeros)
072710131212     c                             or ($varqta = *on and
072720131212     c                                 sav_acrcgi = v1ccgi)
072730071023     c                   exsr      sr_carcgi
072740071023     c                   endif
072750071023
072760071023      * se giro non immesso da utente imposto il giro recuperato
072770071031      * oppure se variate le quantità
072780071031     c                   if        (sav_v1ccgi = *blanks and
072790071031     c                              sav_acrcgi <> *blanks)
072800131018     c                             or (sav_v1ccgi <> sav_acrcgi
072810131018     c                                 and sav_acrcgi <> *blanks)
072820131212     c                             or ($varqta = *on and
072830131212     c                                (sav_acrcgi = v1ccgi or
072840131212     c                                 sav_v1ccgi = v1ccgi))
072850070920     c                   eval      v1tgi = 'A'
072860070920     c                   eval      v1ccgi = sav_acrcgi
072870070920     c                   eval      sav_v1ccgi = sav_acrcgi
072880070920     c                   endif
072890160325      /free
072900160325       //?Se immissione o copia ORM
072910160325       //?Se variato il mittente e ora è un NON codificato
072920160429       //?Se il giro è rimasto lo stesso di prima
072930160429         IF  (*in01 or *in03) and
072940160429             wModCodCra and ds_V1Ccra = *zeros and
072950160429             V1Ccgi = sav_V1Ccgi and sav_ACRcgi = *blanks;
072960160429           clear V1tgi;
072970160429           clear V1Ccgi;
072980160429           clear sav_V1Ccgi;
072990160429           wModGiroAut = *on;
073000160429           *in28 = *on;
073010160429           *in90 = *on;
073020160429           V1Cmsg = msg(98);
073030160429           leavesr;
073040160325         ENDIF;
073050160325      /end-free
073060070913      *     controllo
073070070913     c                   if        v1ccgi <> *blank
073080070913     c                   exsr      ctrgiro
073090070913      *     giro errato
073100070924     c                   if        d09oerr = '1' and not *in24
073110070913     c                   eval      *in28 = *on
073120070913     c                   eval      *in51 = *on
073130131030     c                   eval      w2cmsg = msg(43)
073140070913     c                   leavesr
073150070913     c                   endif
073160070913      *     imposto la descrizione del giro
073170071011     c                   if        d09ocgi <> *blanks
073180070913     c                   eval      v1dcgi = d09odes
073190070913     c                   endif
073200070913      * se giro diverso da quello salvato imposto che giro manuale
073210070919     c                   if        v1ccgi <> sav_v1ccgi
073220070913     c                   eval      v1tgi = 'M'
073230070913     c                   endif
073240070913     c                   endif
073250070913
073260070913     c                   endsr
073270070913
073280070913      *------------------------------------------------------------------------*
073290070913      *   Controllo e decodifica giro
073300070913      *------------------------------------------------------------------------*
073310070913     c     ctrgiro       begsr
073320070913
073330070913     c                   clear                   fidg09ds
073340070913     c                   eval      d09iop0 = '001'
073350071011     c                   eval      d09ifgs = num_v1cpor
073360070913     c                   eval      d09idat = *date
073370070913     c                   eval      d09icgi = v1ccgi
073380070913     c                   eval      d09itug = 'R'
073390070913     c                   eval      kpjbu = fidg09ds
073400070913     c                   call      'FIDG09R'
073410070913     c                   parm                    kpjba
073420070913     c                   eval      fidg09ds = kpjbu
073430070913
073440070913     c                   endsr
073450070913
073460010202      **********************************************************************
073470010202      * CONTROLLI     * fase *
073480010202      **********************************************************************
073490010202     C     Sr_Contrfao   begsr
073500010202
073510010723      * Se non ho la data e ora fase caricata le imposto con data e ora del sistema
073520140107     c                   IF        *in01
073530050422     C                   if        v1dfo = *zeros
073540050422     C                   eval      v1dfo = oggi_aammgg
073550010723     C                   endif
073560050422     C                   if        v1ofo = *zeros
073570050422     C                   time                    w0140
073580010723     C                   movel     w0140         v1ofo
073590010202     C                   endif
073600010202     C                   endif
073610010202      * Decodifica la fase
073620010202     C                   if        v1fao <> *zeros
073630010202     C                   clear                   TIBS02DS
073640010202     C                   movel     'C'           T02mod
073650010202     C                   movel     knsif         t02sif
073660010202     C                   movel     'FAR'         t02cod
073670010202     C                   movel(p)  v1fao         T02ke1
073680010202     C                   call      'TIBS02R'
073690010202     C                   parm                    KPJBA
073700010202     C                   parm                    TIBS02DS
073710010314     C                   movel     t02uni        dfar
073720010314     C                   movel     d§fardes      v1dfao
073730070706     c                   eval      v1eti = d§farass
073740010202     C                   endif
073750010202
073760010202     C                   endsr
073770130917      /free
073780130917       //--------------------------------------------------------------
073790130917       //?Controllo ora ritiro merce con ora massima per il ritiro.
073800130917       //--------------------------------------------------------------
073810130917       BEGSR contrORRmax;
073820130917
073830131024       //?Controllo ora pronta merce se non è presa da anagrafica clienti
073840131022         SELECT;
073850140320         //?Se ora ritiro a 0 = commissionato
073860140320         //?Se ora fine servizio a 0 = ORM EXPORT no orari servizio
073870140320         //?per ora ritiro a 0 ci pensa la routine CONTRORR
073880140320         //?se non ci sono orari servizio non devo controllare
073890161104         //?non controllo se la data ritiro è maggiore della data pronta merce
073900161028           WHEN  V1orr = 0 or V1ostfs = 0;
073910140414           WHEN  sav_ACRorr <> *zeros and V1orr <= sav_ACRorr;
073920161104           WHEN  inv_v1dar > inv_v1dpm;
073930131022           OTHER;
073940131022       //?Controllo ora pronta merce con ora fine standard
073950161028       //?o con ora max
073960161028           IF  V1orr > V1ostfs;
073970131022             V1Cmsg = msg(12);
073980131022             V1Cmsg = %trim(V1Cmsg) + ' (' +
073990161028                      %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074000161028                      %subst(%editc(V1ostfs:'X'):3:2) + ')';
074010131022             *in28 = *on;
074020131022             *in58 = *on;
074030131022           ENDIF;
074040131022         ENDSL;
074050130917
074060140317       //?Controllo ora immissione ORM con ora fine standard
074070161028       //?o con ora max
074080161028         IF  V1ostfs > 0 and
074090161028             %dec(%subst(%editc(v1oao:'X'):1:4):4:0) > V1ostfs and
074100161028             inv_V1dao = inv_V1dar;
074110131016           V1Cmsg = msg(32);
074120130920           V1Cmsg = %trim(V1Cmsg) + ' (' +
074130161028                    %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074140161028                    %subst(%editc(V1ostfs:'X'):3:2) + ')';
074150130917           *in28 = *on;
074160130917         ENDIF;
074170130917
074180130917       ENDSR;
074190131014
074200131014       //--------------------------------------------------------------
074210131112       //?Controllo orari di apertura.
074220131014       //--------------------------------------------------------------
074230131112       BEGSR OrariApertura;
074240131017
074250131112       //?Richiamo pgm per controllo orari ritiro
074260131112       //?per ora il pgm deve controllare anche che ci sia abbastanza
074270131112       //?spazio nella nota1 e nella nota2 per poterli inserire in
074280131112       //?questi campi per poi inviarli a PDA
074290131112         IF  V1OraAMda > 0 or V1OraAMa > 0 or
074300131112             V1OraPMda > 0 or V1OraPma > 0;
074310131112           clear TRUL03DS;
074320131112           I03hm1 = V1OraAMda;
074330131112           I03hm2 = V1OraAMa;
074340131112           I03hm3 = V1OraPMda;
074350131112           I03hm4 = V1OraPMa;
074360131112           trul03r (TRUL03DS);
074370131112           IF  O03err <> *off;
074380131112             V1Cmsg = O03msg;
074390131112             *in28 = *on;
074400131112             SELECT;
074410131112               WHEN  O03errpos = 1;
074420131112                 H1riga = 12;
074430131112                 H1colo = 21;
074440131112               WHEN  O03errpos = 2;
074450131112                 H1riga = 12;
074460131112                 H1colo = 28;
074470131112               WHEN  O03errpos = 3;
074480131112                 H1riga = 12;
074490131112                 H1colo = 38;
074500131112               WHEN  O03errpos = 4;
074510131112                 H1riga = 12;
074520131112                 H1colo = 45;
074530131112               WHEN  O03errpos = 5;
074540131112                 *in82 = *on;
074550131112               WHEN  O03errpos = 6;
074560131112                 H1riga = 14;
074570131112                 H1colo = 43;
074580131112             ENDSL;
074590131112           ENDIF;
074600131112         ENDIF;
074610131107
074620131014       ENDSR;
074630100811
074640100811       //--------------------------------------------------------------
074650100811       //?Controllo cappario DPD.
074660100811       //--------------------------------------------------------------
074670100811       BEGSR CtrCapDPD;
074680100811
074690100811         clear tisie3ds;
074700100811         isie3dri = oggi_aammgg;
074710100811         IF  inv_v1dar = 0;
074720100811           isie3dsp = oggi_aammgg;
074730100811         ELSE;
074740100811           isie3dsp = inv_v1dar;
074750100811         ENDIF;
074760100811         isie3hsp = %dec(%time());
074770100811         isie3nzd = wNazione;
074780100811         isie3cad = wCapDPD;
074790100811         IF  v1pkg > §3ilmp;
074800100811           isie3srv = 101;
074810100811         ELSE;
074820100811           isie3srv = 136;
074830100811         ENDIF;
074840100811         IF  v1cpoe = *zeros;
074850150305           isie3lnp = DUTpou;
074860100811         ELSE;
074870100811           isie3lnp = v1cpoe;
074880100811         ENDIF;
074890100811         tisie3r (tisie3ds);
074900100811         IF  osie3err <> *blanks;
074910100811           *in28 = *on;
074920100811           *in62 = *on;
074930100811           v1cmsg = 'CAP non trovato su cappario DPD.';
074940100811           leavesr;
074950100811         ENDIF;
074960100811
074970100811       ENDSR;
074980161019
074990161019       //--------------------------------------------------------------
075000161019       //?Controllo Data Pronta Merce.
075010161019       //--------------------------------------------------------------
075020161019       BEGSR ContrDataProntaMerce;
075030161020
075040161020       //?Se sono in immissione o copia e la data è a 0 la imposto = oggi
075050161020         IF  (*in01 or *in03) and V1dpm = 0;
075060161020           V1dpm = oggi_ggmmaa;
075070161028           IF  sav_v1dpm > 0;
075080161028             clear sav_v1dpm;
075090161028           ELSE;
075100161028             sav_V1dpm = V1dpm;
075110161028           ENDIF;
075120161020         ENDIF;
075130161019
075140161019       //?La data è obbligatoria
075150161019         IF  V1dpm = 0;
075160161019           *in28 = *on;
075170161019           *in52 = *on;
075180161020           V1Cmsg = 'Immettere la Data Pronta Merce';
075190161019           leavesr;
075200161019         ENDIF;
075210161019
075220161019       //?Data Valida
075230161019         clear inv_V1dpm;
075240161019         clear wlbdat;
075250161019         G02dat = V1dpm;
075260161019         xsrda8 (wlbdat);
075270161019         IF  G02err = '1';
075280161019           *in28 = *on;
075290161019           *in52 = *on;
075300161020           V1Cmsg = 'Data Pronta Merce errata';
075310161019           leavesr;
075320161019         ENDIF;
075330161019         V1dpm = G02dat;
075340161019         inv_V1dpm = G02inv;
075350170303
075360170303       //?Data non più vecchia di 7 gg da oggi
075370170427       //?No se sono in chiusura ORM
075380170427         IF  not *in10;
075390170303         clear wdata;
075400170303         dataiso = %date(oggi_aammgg:*iso);
075410170303         wdata = %dec(dataiso - %days(7));
075420170303         IF  inv_V1dpm < wdata;
075430170303           *in28 = *on;
075440170303           *in52 = *on;
075450170303           V1Cmsg = 'Data Pronta Merce errata. Inferiore a 7 gg da oggi';
075460170303           leavesr;
075470170303         ENDIF;
075480170427         ENDIF;
075490161115
075500161115       //?Se sono in manutenzione non devo ricalcolare la data ritiro
075510161115       //?se la data pronta merce immessa è inferiore alla data pronta merce dell'ORM
075520161115         IF  *in02 and §OREdpm > 0 and inv_v1dpm <= §OREdpm;
075530161115           leavesr;
075540161115         ENDIF;
075550161028
075560161028         IF  V1dpm <> sav_V1dpm;
075570161028           wModDpm = *on;
075580161028           sav_V1dpm = V1dpm;
075590161028         ENDIF;
075600161019
075610161019       ENDSR;
075620100811      /end-free
075630100204
075640100204      *--------------------------------------------------------------------*
075650100204      * Imposto le personalizzazioni di ORMFLO da tabella PVO
075660100204      *--------------------------------------------------------------------*
075670100204     c     sr_pvo        begsr
075680100204
075690100204     c                   eval      ok_pvo = *off
075700100204
075710100204      * aggancio la tabella per:
075720100204      * --> ordinante lungo 10
075730100205     c                   clear                   dpvo
075740100204     c                   clear                   tibs02ds
075750100204     c                   eval      t02mod = 'C'
075760100204     c                   eval      t02sif = knsif
075770100204     c                   eval      t02cod = 'PVO'
075780100205     c                   eval      t02ke1 = %editc(ds_v1ccor:'X')
075790100204     c                   eval      t02ke2 = 'O'
075800100204     c                   call      'TIBS02R'
075810100204     c                   parm                    kpjba
075820100204     c                   parm                    tibs02ds
075830100204     c                   if        t02err = *blanks
075840100204     c                   eval      dpvo = t02uni
075850100204     c                   eval      OK_pvo = *on
075860100204     c                   endif
075870100204      * --> ordinante lungo 7
075880100204     c                   if        not OK_pvo
075890100204     c                   clear                   dpvo
075900100204     c                   clear                   tibs02ds
075910100204     c                   eval      t02mod = 'C'
075920100204     c                   eval      t02sif = knsif
075930100204     c                   eval      t02cod = 'PVO'
075940100204     c                   eval      t02ke1 = %editc(ds_v1cor12:'X')
075950100204     c                   eval      t02ke2 = 'O'
075960100204     c                   call      'TIBS02R'
075970100204     c                   parm                    kpjba
075980100204     c                   parm                    tibs02ds
075990100204     c                   if        t02err = *blanks
076000100204     c                   eval      dpvo = t02uni
076010100204     c                   eval      OK_pvo = *on
076020100204     c                   endif
076030100204     c                   endif
076040100204      * --> ksc
076050100204     c                   if        not OK_pvo
076060100204     c                   clear                   dpvo
076070100204     c                   clear                   tibs02ds
076080100204     c                   eval      t02mod = 'C'
076090100204     c                   eval      t02sif = knsif
076100100204     c                   eval      t02cod = 'PVO'
076110100204     c                   eval      t02ke1 = %editc(v1ksc:'X')
076120100204     c                   eval      t02ke2 = 'K'
076130100204     c                   call      'TIBS02R'
076140100204     c                   parm                    kpjba
076150100204     c                   parm                    tibs02ds
076160100204     c                   if        t02err = *blanks
076170100204     c                   eval      dpvo = t02uni
076180100204     c                   eval      OK_pvo = *on
076190100204     c                   endif
076200100204     c                   endif
076210100204
076220100204      * se ho trovato uno dei 3 codici in tabella devo personalizzare ORMFLO
076230100204     c                   if        OK_pvo
076240100205
076250100205     c                   if        d§pvocb = 'S'
076260100205     c                   eval      §ormcb = d§pvocb
076270100204     c                   endif
076280100205     c                   if        d§pvofr = 'S'
076290100205     c                   eval      §ormfr = d§pvofr
076300100204     c                   endif
076310100205     c                   if        d§pvoks = 'S'
076320100205     c                   eval      §ormks = d§pvoks
076330100204     c                   endif
076340100205     c                   if        d§pvofd = 'S'
076350100205     c                   eval      §ormfd = d§pvofd
076360100204     c                   endif
076370100205     c                   if        d§pvoic <> *blanks
076380100205     c                   eval      §ormic = d§pvoic
076390100204     c                   endif
076400100204      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
076410100204      * di variazione
076420100204      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
076430100205     c                   if        d§pvosrm = 'S'
076440100205     c                   eval      §orsrm = d§pvosrm
076450100204     c                   eval      v1com = 'S'
076460100204     c                   eval      *in34 = *on
076470100204     c                   clear                   v1dar
076480100204     c                   endif
076490100208     c                   if        *in01 and §rmtla = *blanks
076500100208     c                   eval      v1com = §orcomc
076510100208     c                   endif
076520100812      /free
076530100812       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
076540100812       //?di variazione
076550100812        IF  D§PVOpre = 'M';
076560100812          §ORMpre = D§PVOpre;
076570100812          V1com = 'N';
076580100812          *in34 = *on;
076590100812        ENDIF;
076600100812      /end-free
076610100204
076620100204     c                   endif
076630100208
076640100208     c                   if        *in01 and §rmtla = *blanks and
076650100208     c                             not ok_pvo
076660100208      * devo pulire tutti i dati del campo ormflo
076670100208     c                   clear                   dorm01
076680100208     c                   endif
076690100204
076700100204     c                   endsr
076710071031
076720071031      *--------------------------------------------------------------------*
076730071031      * Carico tabella AGR del po ritiro
076740071031      *--------------------------------------------------------------------*
076750071031     c     sr_caragr     begsr
076760071105
076770071031     c                   clear                   tibs02ds
076780071031     c                   clear                   dagr
076790071112     c                   eval      $noagr = *off
076800071031     c                   eval      t02mod = 'C'
076810071031     c                   eval      t02sif = knsif
076820071031     c                   eval      t02cod = 'AGR'
076830071031     c                   movel(p)  v1cpor        t02ke1
076840071031     c                   call      'TIBS02R'
076850071031     c                   parm                    kpjba
076860071031     c                   parm                    tibs02ds
076870071105     c                   if        t02err = *blanks
076880071031     c                   eval      dagr = t02uni
076890071112     c                   else
076900071112     c                   eval      $noagr = *on
076910071105     c                   endif
076920071031
076930071031     c                   endsr
076940071023
076950071023      *--------------------------------------------------------------------*
076960071023      * Carico il giro da FNACR o da FNORS
076970071023      *--------------------------------------------------------------------*
076980071023     c     sr_carcgi     begsr
076990071023
077000071023     c                   clear                   sav_acrcgi
077010071031
077020071106     c                   eval      kacr1ain = '='
077030071112      * se tabella AGR inserita per filiale ritiro
077040071112     c                   if        $noagr = *off
077050071031      * controllo se devo recuperare il giro Oltre, il giro Sotto o lo Standard
077060071031     c                   if        wpkg > §agrpkgo or wvlm > §agrvlmo or
077070071031     c                             v1bnc > §agrbnco
077080071031     c                   eval      kacr1ain = '>'
077090071031     c                   endif
077100071031     c                   if        wpkg < §agrpkgs and wvlm < §agrvlms and
077110071031     c                             v1bnc < §agrbncs
077120071031     c                   eval      kacr1ain = '<'
077130071031     c                   endif
077140071112     c                   endif
077150071023
077160071023      * da anagrafica clienti ritiro
077170071031     c     kacr1         chain     fnacr13l
077180071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077190071023     c                   eval      sav_acrcgi = acr1cgi
077200071031     c                   else
077210071106     c                   if        kacr1ain <> '='
077220071106     c                   eval      kacr1ain = '='
077230071031     c     kacr1         chain     fnacr13l
077240071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077250071031     c                   eval      sav_acrcgi = acr1cgi
077260071031     c                   endif
077270071031     c                   endif
077280071031     c                   endif
077290071031
077300071023      * se conferma ORM fisso recupero il giro dal fisso se impostato
077310071031      * quindi solo immissione
077320161117     c                   if        §rmfpr = 'S'
077330071023     c     kors          chain     fnors11l
077340071023     c                   if        %found(fnors11l) and ors1cgi <> *blanks
077350071023     c                   eval      sav_acrcgi = ors1cgi
077360071023     c                   endif
077370071023     c                   endif
077380071023
077390071023     c                   endsr
077400100217
077410100218      /free
077420100218       //--------------------------------------------------------------------
077430100218       //?Controllo se OK tutti i dati per il preavviso.
077440100218       //--------------------------------------------------------------------
077450100218       BEGSR sr_Preavviso;
077460100217
077470100218       //?Imposto la chiave per il FNVAOE rcd 'M '
077480100218         k_vaoecor = sav_vaocor;
077490100218         k_vaoepoe = sav_vaopoe;
077500100218         k_vaoensr = sav_vaonsr;
077510100218         k_vaoenor = sav_vaonor;
077520100218         k_vaoenrv = sav_vaonrv;
077530100218         k_vaoetrc = 'M ';
077540100218       //?Aggancio il rcd
077550100812         chain  %kds(k06fnvaoe) FNVAOE1L;
077560100218
077570100218         SELECT;
077580100218       //?Se non trovo il rcd e non c'è il flag di preavviso mail OK
077590100218           WHEN  not %found(FNVAOE1L) and §ormpre <> 'M';
077600100218             clear vaoedati;
077610100218       //?Se trovo il rcd e c'è il flag di preavviso mail OK e controllo la mail
077620100218           WHEN  %found(FNVAOE1L) and §ormpre = 'M';
077630100218             wmail = vaoedati;
077640100218             exsr sr_CtrMail;
077650100223             vaoedati = wmail;
077660100223             IF  $errpre;
077670100223               wpremsg = §emlmsgo;
077680100225               *in94 = *on;
077690100223             ENDIF;
077700100218       //?Se non trovo il rcd e c'è il flag di preavviso mail errore
077710100218           WHEN  not %found(FNVAOE1L) and §ormpre = 'M';
077720100218             clear vaoedati;
077730100223             $errpre = *on;
077740100225             wpremsg = 'Dati incongruenti. ' +
077750100225                       'ORM con preavviso senza indirizzo mail';
077760100218       //?Se trovo il rcd e non c'è il flag di preavviso mail errore
077770100218           WHEN  %found(FNVAOE1L) and §ormpre <> 'M';
077780100223             $errpre = *on;
077790100225             wpremsg = 'Dati incongruenti. ' +
077800100225                       'ORM senza preavviso con indirizzo mail';
077810100218         ENDSL;
077820100218
077830100218       ENDSR;
077840100218
077850100218       //--------------------------------------------------------------------
077860100218       //?Controllo validità indirizzo mail.
077870100218       //--------------------------------------------------------------------
077880100218       BEGSR sr_CtrMail;
077890100218
077900100218         clear dsemail;
077910100223         §emlindi = wmail;
077920100218         xemail (dsemail);
077930100223         $errpre = (§emlerro <> '0');
077940100223         IF  not $errpre;
077950100223           wmail = §emlindo;
077960100223         ENDIF;
077970100218
077980100218       ENDSR;
077990100218
078000100218       //--------------------------------------------------------------------
078010100218       //?Richiesta dati preavviso con window.
078020100218       //--------------------------------------------------------------------
078030100218       BEGSR sr_DatiPre;
078040100218
078050100218         $endpre = *off;
078060100223         *in90 = *off;
078070100223         $okf6pre = *off;
078080100224         *in28 = *off;
078090100218
078100100218       //?emetto la videata con i dati dell'ORM
078110100218         write fior051;
078120100322         IF  $errpre;
078130100322           *in28 = *on;
078140100322         ENDIF;
078150100218       //?emetto la window dove richiedo i dati del preavviso
078160100218         DOW not $endpre;
078170100218           exfmt fior05pre;
078180100218           clear  wpremsg;
078190100218           *in28 = *off;
078200100225           *in94 = *off;
078210100223         SELECT;
078220100223       //?F12-Ritorno
078230100223           WHEN  *inkl;
078240100218             $endpre = *on;
078250100223             *in28 = *off;
078260100223             *in90 = *on;
078270100223             $errpre = *off;
078280100223       //?F6-Conferma
078290100223           WHEN  *inkf;
078300100225             exsr sr_CtrDatiPre;
078310100223             IF  not *in28;
078320100223               IF  wpresino = 'NO';
078330100223                 clear §ormpre;
078340100325           //?Se non è più ORM con preavviso MAIL pulisco il campo di ORM commissionato
078350100325           //?e lo sblocco, così il pgm ricalcola se commissionato o meno e l'utente può
078360100325           //?variarlo
078370100325                 clear V1com;
078380100325                 *in34 = *off;
078390100223               ENDIF;
078400100223               IF  wpresino = 'SI';
078410100223                 §ormpre = 'M';
078420100325           //?Se è ORM con preavviso MAIL imposto che ORM NON commissionato
078430100325           //?e sblocco il campo così l'utente non può variarlo
078440100325                 V1com = 'N';
078450100325                 *in34 = *on;
078460100223               ENDIF;
078470100223               vaoedati = wpremail;
078480100223               $endpre = *on;
078490100223               $okf6pre = *on;
078500100223               $errpre = *off;
078510100223             ENDIF;
078520100223           OTHER;
078530100218       //?controllo i dati
078540100225             exsr sr_CtrDatiPre;
078550100223           ENDSL;
078560100218         ENDDO;
078570100218
078580100218       ENDSR;
078590100218
078600100218       //--------------------------------------------------------------------
078610100218       //?Controlli dati preavviso.
078620100218       //--------------------------------------------------------------------
078630100225       BEGSR sr_CtrDatiPre;
078640100218
078650100218         *in28 = *off;
078660100218
078670100218       //?se preavviso = 'SI' deve esserci la mail
078680100218         IF  wpresino = 'SI' and wpremail = *blanks;
078690100218           *in28 = *on;
078700100225           wpremsg = 'Dati incongruenti. ' +
078710100225                     'ORM con preavviso senza indirizzo mail';
078720100218           leavesr;
078730100218         ENDIF;
078740100218       //?se preavviso = 'NO' NON deve esserci la mail
078750100218         IF  wpresino = 'NO' and wpremail <> *blanks;
078760100218           *in28 = *on;
078770100225           wpremsg = 'Dati incongruenti. ' +
078780100225                     'ORM senza preavviso con indirizzo mail';
078790100218           leavesr;
078800100218         ENDIF;
078810100218       //?la mail deve essere valida
078820100218         IF  wpremail <> *blanks;
078830100223           wmail = wpremail;
078840100218           exsr sr_CtrMail;
078850100223           IF  $errpre;
078860100223             $errpre = *off;
078870100218             *in28 = *on;
078880100225             *in94 = *on;
078890100218             wpremsg = §emlmsgo;
078900100218             leavesr;
078910100218           ENDIF;
078920100223           wpremail = wmail;
078930100218         ENDIF;
078940100218
078950100218       ENDSR;
078960100218
078970100218      /end-free
078980010305
078990030606      **********************************************************************
079000030611      * Imposta flag x Orm commissionato *
079010030606      **********************************************************************
079020030611     c     Sr_OrmCom     BegSr
079030030611
079040030611      * Se p.o. emissione a 0 vuol dire che sono in immissione
079050150305      * in questo caso lo imposto già uguale al p.o. utente
079060030611      * per poter fare il controllo tra p.o. ritiro e p.o. emissione
079070030611     c                   If        v1cpoe = *zeros
079080150305     c                   z-add     DUTpou        v1cpoe
079090030611     c                   EndIf
079100050323      * RICORDA!!!!
079110050324      * Un orm da INTERNET/FILE con codice ordinante e p.o. emissione = a p.o. ritiro
079120050323      * non è un ORM commissionato
079130060103      * Un orm FISSO con codice ordinate e p.o. emissione = a p.o. ritiro (x forza)
079140060103      * non è un ORM commissionato (in automatico l'utente può variarlo)
079150100205
079160100205      * come prima cosa imposto il campo di ORM commissionato se lo recupero dalla
079170100205      * personalizzazione del cliente
079180100205     c                   if        v1com = *blanks
079190100205     c                             and d§pvocomc <> *blanks
079200100205     c                   eval      v1com = d§pvocomc
079210100205     c                   leavesr
079220100205     c                   endif
079230030606
079240030606      * Imposto che è un orm commissionato se:
079250030606      * - Cod.mittente <> Cod.ordinante
079260030606      * - Ordinante presente ma non codificato o 8888 o 9999
079270130911      * - P.o. ritiro <> p.o. emissione
079280130911      *   ma solo se mittente NON codificato
079290030606
079300050323If  1c                   If        V1Com = *Blanks  and
079310030902     c                                             ((Ds_V1Cor12 > *Zeros and
079320040707     c                                              Ds_V1cra12 <> Ds_V1Cor12 and
079330050323     c                                              V1ctco <> 'I' and
079340060103     c                                              V1ctco <> 'F' and
079350060103     c                                              V1ctco <> 'S')
079360030902     c                                              or
079370030902     c                                             (V1rso <> *Blanks   and
079380030902     c                                              (Ds_V1ccor = *Zeros or
079390030902     c                                               Ds_V1cor2 = 8888   or
079400050323     c                                               Ds_V1cor2 = 9999)))
079410030606     c                   Eval      V1Com = 'S'
079420030902     c                   Goto      Endormcom
079430030606    1c                   EndIf
079440050323If  1c                   If        V1Com = *Blanks  and
079450030902     c                                              Num_V1cPor <> V1cpoe
079460130911     c                                         and  ds_V1Ccra = 0
079470030902     c                   Eval      V1Com = 'S'
079480030902     c                   Goto      Endormcom
079490030902    1c                   EndIf
079500030611
079510030902     c     endormcom     EndSr
079520030611      **********************************************************************
079530030611      * Controllo se Ok tutti i dati
079540030611      **********************************************************************
079550030611     c     Sr_ContrCom   BegSr
079560030606
079570030606      * Se è un orm commissionato l'ordinante è obbligatorio
079580030606      * se l'ordinante non c'è devo emettere una window dove chiedo chi è l'ordinante
079590030606If  1c                   If        V1Com = 'S' and
079600030606     c                             V1rso = *Blanks and Ds_V1ccor = *Zeros
079610030610     c                   Eval      whocor = 'S'
079620030610     c                   Eval      *In90 = *On
079630131106      /free
079640131106       //?pulisco il campo del commissionato così viene rifatto il calcolo
079650131106       //?se ORM commissionato o meno
079660131106         clear V1com;
079670131106      /end-free
079680030611      * se non sto facendo una conferma di massa emetto la window
079690030611If 1ac                   If        §rmtla <> '0' and §rmtla <> '1'
079700030610Do  2c                   Do        *Hival
079710030610     c                   Clear                   wdcor
079720030606     c                   Exfmt     Fior05o
079730030610If  3C                   If        *Inkl
079740030606     c                   Leave
079750030610    3c                   EndIf
079760030610If  3c                   If        wdcor <> *Blanks
079770030609     c                   Leave
079780030610    3c                   EndIf
079790030610    2c                   EndDo
079800030611      * se sto facendo una conferma di massa errore
079810030611  x1ac                   Else
079820030611     C                   seton                                        28
079830030611   1ac                   EndIf
079840030606    1c                   EndIf
079850030610
079860030610      * devo controllare il referente e il n. telefono
079870030612If  1c                   If        V1Com = 'S' and Not *In28
079880030610     C                   exsr      Sr_Contrref
079890030610    1c                   EndIf
079900030606
079910030606     c                   EndSr
079920070706
079930070706      *--------------------------------------------------------------------*
079940070706      * Calcolo peso e volume
079950070706      *--------------------------------------------------------------------*
079960070706     c     sr_pesvol     begsr
079970071108
079980071108     c                   clear                   wvlmaut
079990071108     c                   clear                   wpkgaut
080000071108     c                   clear                   wvlmmtc
080010071108     c                   clear                   wpkgmtc
080020071108     c                   clear                   wvlmblc
080030071108     c                   clear                   wpkgblc
080040070706
080050070706     c                   eval      wpkg = v1pkg
080060070706     c                   eval      wvlm = v1vlm
080070070706
080080070706      * se non inserito peso a video lo calcolo per memorizzarlo su fnorg00f
080090070706     c                   if        wpkg = *zeros
080100070706     c                   select
080110070918 b3  c                   when      v1bnc <> *zeros
080120070918     c                   eval      wpkg = v1bnc / d§dftbnc * d§dftpkg
080130070706 b2  c                   when      v1vlm <> *zeros
080140070918     c                   eval      wpkg = v1vlm * d§dftpkg
080150070706 e3  c                   endsl
080160070706      * se troppo alto il peso lo imposto al massimo
080170070706     c                   if        wpkg > 999999,9
080180070706     c                   eval      wpkg = 999999,9
080190070706     c                   endif
080200070706     c                   endif
080210070706
080220070706      * se non inserito volume a video lo calcolo per memorizzarlo su fnorg00f
080230070706     c                   if        wvlm = *zeros
080240070706     c                   select
080250070918 b3  c                   when      v1bnc <> *zeros
080260070918     c                   eval(h)   wvlm = v1bnc / d§dftbnc
080270070918 b2  c                   when      v1pkg <> *zeros
080280070918     c                   eval(h)   wvlm = v1pkg / d§dftpkg
080290070706 e3  c                   endsl
080300070706      * se troppo alto il volume lo imposto al massimo
080310070706     c                   if        wvlm > 99,999
080320070706     c                   eval      wvlm = 99,990
080330070706     c                   endif
080340070706     c                   endif
080350070918
080360070918      * se inseriti gli automezzi devo convertirli in m³ per poi memorizzare
080370070918      * il dato più alto su FNORG
080380070918      * AUTOTRENO
080390070918     c                   if        v1att > *zeros
080400070918     c                   eval      wvlmaut = v1att * d§dftaut
080410070918     c                   eval      wpkgaut = wvlmaut * d§dftpkg
080420070918     c                   if        wvlmaut > 99,999
080430070918     c                   eval      wvlmaut = 99,999
080440070918     c                   endif
080450070918     c                   if        wpkgaut > 999999,9
080460070918     c                   eval      wpkgaut = 999999,9
080470070918     c                   endif
080480070918     c                   endif
080490070918      * MOTRICE
080500070918     c                   if        v1mtc > *zeros
080510070918     c                   eval      wvlmmtc = v1mtc * d§dftmtc
080520070918     c                   eval      wpkgmtc = wvlmmtc * d§dftpkg
080530070918     c                   if        wvlmmtc > 99,999
080540070918     c                   eval      wvlmmtc = 99,999
080550070918     c                   endif
080560070918     c                   if        wpkgmtc > 999999,9
080570070918     c                   eval      wpkgmtc = 999999,9
080580070918     c                   endif
080590070918     c                   endif
080600070918      * BILICO
080610070918     c                   if        v1blc > *zeros
080620070918     c                   eval      wvlmblc = v1blc * d§dftblc
080630070918     c                   eval      wpkgblc = wvlmblc * d§dftpkg
080640070918     c                   if        wvlmblc > 99,999
080650070918     c                   eval      wvlmblc = 99,999
080660070918     c                   endif
080670070918     c                   if        wpkgblc > 999999,9
080680070918     c                   eval      wpkgblc = 999999,9
080690070918     c                   endif
080700070918     c                   endif
080710070918
080720070918      * cerco il volume più alto
080730070918     c                   if        wvlmaut > wvlm
080740070918     c                   eval      wvlm = wvlmaut
080750070918     c                   endif
080760070918     c                   if        wvlmmtc > wvlm
080770070918     c                   eval      wvlm = wvlmmtc
080780070918     c                   endif
080790070918     c                   if        wvlmblc > wvlm
080800070918     c                   eval      wvlm = wvlmblc
080810070918     c                   endif
080820070918
080830070918      * cerco il peso più alto
080840070918     c                   if        wpkgaut > wpkg
080850070918     c                   eval      wpkg = wpkgaut
080860070918     c                   endif
080870070918     c                   if        wpkgmtc > wpkg
080880070918     c                   eval      wpkg = wpkgmtc
080890070918     c                   endif
080900070918     c                   if        wpkgblc > wpkg
080910070918     c                   eval      wpkg = wpkgblc
080920070918     c                   endif
080930071031
080940071108      * se sono in modifica e non devo fare una proposta di variazione
080950071031      * controllo se variati peso/volume/bancali per ricalcolare il giro in base gli ambiti
080960071031      * per il peso e il volume devo utilizzare i valori sviluppati dal pgm
080970071031     c                   eval      $varqta = *off
080980150609     c     v1cpor        Lookup    skpog                                  30
080990071108     c                   select
081000080314     c                   when      *in30 and *in02 and (v1fao = 400
081010080314     c                             or v1fao = 410)
081020071108     c                   when      not *in30 and *in02
081030071108     c                   other
081040131202     c                   if        (savpkg <> wpkg or savvlm <> wvlm or
081050131202     c                              savbnc <> v1bnc) and
081060131202     c                             (savpkg > 0 or savvlm > 0 or
081070131202     c                              savbnc > 0)
081080071031     c                   eval      $varqta = *on
081090071031     c                   eval      savpkg = wpkg
081100071031     c                   eval      savvlm = wvlm
081110071031     c                   eval      savbnc = v1bnc
081120071031     c                   endif
081130071108     c                   endsl
081140161025      /free
081150161025       //?Controllo se ho variato il peso, serve per ricalcolare la data ritiro
081160161025       //?ma solo se data ritiro a video non impostata da utente
081170161025         IF  old_wpkg <> wpkg;
081180161026           IF  old_wpkg > 0 and inv_newv1dar >= inv_v1dar;
081190161025             wModPeso = *on;
081200161025           ENDIF;
081210161025           old_wpkg = wpkg;
081220161025         ENDIF;
081230161025      /end-free
081240070706
081250070706     c                   endsr
081260070706
081270010305      **********************************************************************
081280010305      * CONTROLLI     * cerco causale chiusura e decodifico *
081290010305      **********************************************************************
081300010305     C     Sr_Contrcau   begsr
081310010305
081320010305     C                   eval      kpoe = ORMpoe
081330010305     C                   eval      knsr = ORMnsr
081340010305     C                   eval      knor = ORMnor
081350010305     C                   eval      knrv = ORMnrv
081360010305     C                   eval      kdai = ORMdfo
081370010305     C                   eval      kori = ORMofo
081380010305     C                   eval      kfar = ORMfao
081390110418     c     kfnorn1       setll     fnorf01l
081400110418     c                   do        *hival
081410110418     c     kfnorn1       reade(n)  fnorf01L
081420110418     c                   if        %eof(fnorf01L)
081430110418     c                   leave
081440110418     c                   endif
081450010305     C                   eval      v1cau = orfcar
081460110418     C                   enddo
081470010305
081480010305     C                   clear                   dcmr
081490010305     C                   clear                   TIBS02DS
081500010305     C                   movel     'C'           T02mod
081510010305     C                   movel     knsif         t02sif
081520010305     C                   movel     'CMR'         t02cod
081530010305     C                   movel(p)  v1cau         T02ke1
081540010305     C                   call      'TIBS02R'
081550010305     C                   parm                    KPJBA
081560010305     C                   parm                    TIBS02DS
081570010305     C                   movel     t02uni        dcmr
081580010305     C                   movel     d§cmrdes      v1dcau
081590010309
081600010309     C                   eval      *in13 = (v1cau <> *blanks)
081610070123
081620070123      * mi salvo il p.o. e il n. distinta
081630080226     c                   eval      kdstfgs = orffgs
081640080226     c                   eval      kdstnfv = orfndc
081650010305
081660010305     C                   endsr
081670001019      **********************************************************************
081680001023      * CONTROLLI     * se già stampato deve confermare *
081690001019      **********************************************************************
081700001019     C     Sr_Contrstp   begsr
081710001019
081720031201     C                   if        v1dst  >  *zeros
081730001019     C                   movel     'NO'          wfstp
081740010221     C     emetto        tag
081750001019     C                   exfmt     fior05w
081760001107     C                   if        wfstp = 'SI'
081770031125     C                   clear                   FIOR16ds
081780041013     C                   eval      R16ris = 'R'
081790030623     C                   clear                   trul90ds
081800030623     C                   movel     'S'           D90rso
081810030623     C                   call      'TRUL90R'
081820010314     C                   parm                    kpjba
081830030623     C                   parm                    trul90ds
081840001108
081850001108      * F3=Fine
081860030623     C                   if        D90f3 = '1'
081870010314     C                   goto      endsrstampa
081880010314     C                   endif
081890031125
081900090202      /free
081910090202         reset ds5P;
081920090202         if  %subst(knmus : 1 : 3) = 'EDP';
081930090202           TBLkey = 'O' + 'EDP';
081940090202         else;
081950090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
081960090202         endif;
081970090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
081980090202         if  %found( TABEL00F );
081990090202           ds5P = TBLuni;
082000090202         endif;
082010090202      /end-free
082020031201
082030031125     C                   movel     D90mdo        R16mdo
082040031125     C                   movel     D90pro        R16oqo
082050001108
082060031201     c                   eval      R16poe = Ppoe
082070031201     c                   eval      R16nsr = Pnsr
082080031201     c                   eval      R16nor = Pnor
082090031201     c                   eval      R16nrv = Pnrv
082100031125     C                   movel     v1cpor        R16por
082110031125     C                   eval      R16dst = v1dst
082120031125     C                   eval      R16dar = inv_v1dar
082130001108
082140001108     C                   movel     kpjbu         §kpjbu
082150031125     C                   movel(p)  FIOR16ds      kpjbu
082160090202     C                   call      §5Ppgm
082170031125     C                   parm                    KPJBA
082180031125     C                   movel     kpjbu         FIOR16DS
082190001108     C                   movel     §kpjbu        kpjbu
082200001019     C                   endif
082210001019     C                   endif
082220001019
082230001108     C     endsrstampa   endsr
082240001011      **********************************************************************
082250001011      * F6 - Conferma
082260001011      **********************************************************************
082270001011     C     Sr_Conferma   BEGSR
082280140422
082290140422      /free
082300140423        werralert = *off;
082310140422       //?Se conferma ORM da clienti come prima cosa verifico gli alert
082320140507        IF  §rmfpr = 'C' or
082330150305       //?oppure se sono in immissione ORM
082340140507       //?e non è stato richiamato (quindi immissione ORM MANUALE)
082350160208       //?o copia ORM
082360160208            ((*in01 or *in03 )and §RMtla = *blanks);
082370140423          exsr AlertORM;
082380140423          IF  *in28;
082390140423            werralert = *on;
082400140423            leavesr;
082410140423          ENDIF;
082420140422        ENDIF;
082430160406
082440160413       //?Se immissione ORM manuale o copia
082450160406         IF  (*in01 or *in03) and
082460160406             (V1Ctco = 'M' or V1Ctco = 'E');
082470160406         //?se ho almeno 1 dei 2 codici (ordinante - mittente) e non ho emesso
082480160406         //?la WIN (non ho mai dati un F6 sulla WIN)
082490160406         //?e non ho i dati mail e/o sms provo a recuperarli
082500160406         //?dall'anagrafica clienti
082510160406           IF  (ds_V1Ccor > 0 or ds_V1Ccra > 0) and
082520160406                W04mail = *blanks and W04sms = *blanks and
082530160406                not wOkF6F13;
082540160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
082550160406             exsr RecDatiConf;
082560160406           ENDIF;
082570160406       //?se ho i dati relativi alla conferma prenotazione ritiro
082580160405       //?imposto i dati per scrivere il rcd 'G' di fnore e il file spia
082590160406       //?da conferma VAS ho già tutto impostato
082600160406           IF  (w04sms <> *blanks or w04mail <> *blanks);
082610160406             SELECT;
082620160406             WHEN  W04sms <> *blanks and W04mail <> *blanks;
082630160406               §OREfsco = 'S';
082640160406               §OREfmco = 'S';
082650160406             WHEN  W04sms <> *blanks;
082660160406               §OREfsco = 'S';
082670160406               §OREfmco = 'N';
082680160406             WHEN  W04mail <> *blanks;
082690160406               §OREfmco = 'S';
082700160406               §OREfsco = 'N';
082710160406             ENDSL;
082720160406             wconferma = *on;
082730160406           ENDIF;
082740160310         ENDIF;
082750140422      /end-free
082760001011
082770071108     ?* Scrive
082780001109     C                   if        *in01 = *on or *in03 = *on
082790001117     C                   clear                   fnorm000
082800050422     C  n03              exsr      Sr_Numor
082810050422     C   28              goto      endsrconf
082820001011     C                   exsr      Sr_Carfile
082830001108      * Scrive la fase
082840001017     C                   exsr      Sr_Carfilef
082850001108      * Scrive le note
082860150713     c                   IF        not ConfAuto
082870001026     C                   exsr      Sr_Carnote
082880150713     c                   ENDIF
082890070919      * scrivo fnorg (GIRO)
082900151016     c                   exsr      sr_wrtorg
082910100218      /free
082920160310
082930170524       //?Se dovevo posticipare
082940170524        Anticipato = *off;
082950170524        clear ggAnticipo;
082960170524       //?Imposto se l'utente ha anticipato la data ritiro
082970170524        IF  wAnticipa and ORMdar < DarCalcolata;
082980170524          Anticipato = *on;
082990170525       //?e di quanti gg lavorativi ha anticipato
083000170525        clear wdat8;
083010170525        clear tfptfa;
083020170525        p_tfa = ORMpor;
083030170525        data1 = DarCalcolata;
083040170525        data2 = ORMdar;
083050170525        xsrlav8(wdat8);
083060170525       //?dal pgm di calcolo differenza tra le 2 date torna sempre almeno 1, io lo tolgo
083070170525        ggAnticipo = giolav -1;
083080170606        ENDIF;
083090100225       //?Se conferma ORM da clienti scrivo eventuale estensione inviata dal cliente
083100100225        IF  §rmfpr = 'C';
083110100218          exsr sr_fnore;
083120100218        ENDIF;
083130131014       //?Se presenti i dati scrivo fnore
083140140422        IF  w03mail <> *blanks;
083150140422          clear old_w03mail;
083160140422        ENDIF;
083170140422        IF  w03sms <> *blanks;
083180140422          clear old_w03sms;
083190140422        ENDIF;
083200160405        IF  w04mail <> *blanks;
083210160405          clear old_w04mail;
083220160310        ENDIF;
083230160405        IF  w04sms <> *blanks;
083240160405          clear old_w04sms;
083250160310        ENDIF;
083260140422        exsr aggFNORE;
083270160322
083280160322       //?Se conferma ORM da clienti per orm da internet
083290160322       //?memorizzo mail sull'anagrafica clienti ritiro, così anche il relativo flag
083300160322       //?anche sms
083310160712       //?Solo però se richiesto da Ordinante
083320160712        IF  §rmfpr = 'C' and ORMtco = 'I' and ORMcor > 0 and §OREmemo = 'S';
083330160712          exsr aggACR;
083340160322        ENDIF;
083350160314
083360160314       //?Se ORM con Conferma Prenotazione Ritiro e sono in immissione manuale
083370160314       //?cioè ORM Telefonico o Mail/Fax
083380160314       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
083390160314       //?servono nell'invio dell'alert
083400160318       //?ma solo se devo creare l'alert conferma prenotazione ritiro
083410160318        IF  §RMfpr <> 'C' and (ORMtco = 'M' or ORMtco = 'E') and
083420160318            wconferma;
083430160314         //?imposto orario ritiro standard
083440160314          clear dOREor;
083450160314          §OREori = V1ostis;
083460160314          §OREorf = V1ostfs;
083470170609         //?Se la data ritiro è uguale a data immissione
083480170609          IF  ORMdar = ORMdao;
083490160314         //?Se l'ora pronta merce è maggiore
083500160314         //?dell'ora inizio servizio
083510160314         //?e minore dell'ora fine
083520160314         //?la imposto come orario inzio
083530160314          IF  V1orr > V1ostis and
083540160314              V1orr < V1ostfs;
083550160314            §OREori = V1orr;
083560160314          ENDIF;
083570160314         //?Se l'ora pronta merce è maggiore
083580160314         //?dell'ora fine servizio
083590160314         //?la imposto come orario fine
083600160314          IF  V1orr > V1ostfs;
083610160314            §OREorf = V1orr;
083620160314          ENDIF;
083630170609          ENDIF;
083640160314          clear FNORE000;
083650160314          OREpoe = ORMpoe;
083660160314          OREnsr = ORMnsr;
083670160314          OREnor = ORMnor;
083680160314          OREnrv = ORMnrv;
083690160314          OREtrc = 'OR';
083700160314          OREdati = dOREor;
083710160314          write FNORE000;
083720160517          feod  FNORE01L;
083730160314        ENDIF;
083740160217
083750160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
083760160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
083770160224       //?il NPR lo recupero solo se non sto facendo conferma ORM da INTERNET
083780160426       //?e se non è già stato calcolato
083790160426        IF  §rmfpr <> 'C' or
083800160426           (§rmfpr = 'C' and ORMtco <> 'I') or
083810160426           (§rmfpr = 'C' and ORMtco = 'I' and not WebNPR);
083820160217          wnpr = GetNPR(parametroNPR);
083830160217       //?Se il rif. ORM è vuoto imposto il NPR
083840160217          IF  ORMrfa = *blanks;
083850160317            ORMrfa = wnpr;
083860160217          ENDIF;
083870160314       //?scrivo FNORE00F rcd NP
083880160314       //?per memorizzare il NPR
083890160314          clear FNORE000;
083900160314          OREpoe = ORMpoe;
083910160314          OREnsr = ORMnsr;
083920160314          OREnor = ORMnor;
083930160314          OREnrv = ORMnrv;
083940160314          OREtrc = 'NP';
083950160314          OREdati = wnpr;
083960160314          write FNORE000;
083970160517          feod  FNORE01L;
083980160217        ENDIF;
083990100218      /end-free
084000100218
084010001108     C                   write     fnorm000
084020160509     C                   feod      fnorm01L
084030100218
084040100218      /free
084050100218       //?dopo aver scritto l'ORM se previsto il preavviso richiamo programma
084060100218       //?per invio mail di preavviso
084070100218        IF  §ormpre = 'M';
084080100224         clear fior52ds;
084090100224         I52poe = ORMpoe;
084100100224         I52nsr = ORMnsr;
084110100224         I52nor = ORMnor;
084120100224         I52nrv = ORMnrv;
084130100224         fior52r (kpjba:fior52ds);
084140100218        ENDIF;
084150140422
084160140428       //?dopo aver scritto l'ORM se previsto Alert
084170151008       //?e/o previsto invio mail x conferma accettazione ORM
084180140428       //?scrivo file spia
084190151008       IF  walert or wconferma;
084200140422         exsr wrtspia;
084210140422       ENDIF;
084220160217
084230100218      /end-free
084240100218
084250010202     C                   eval      §RMpoeo = ormpoe
084260010202     C                   eval      §RMnsro = ormnsr
084270010202     C                   eval      §RMnoro = ormnor
084280010202     C                   eval      §RMnrvo = ormnrv
084290010704
084300010704      * Richiamo il programma immissione prepagati
084310050324      * solo se sono da conferma ORM VAO, se l'orm è di tipo 'P'
084320050324      * e se è un ORM locale
084330050324      * se l'ORM non è locale il prepagato lo creo al momento della conferma ORM
084340050324      * commissionati (Un ORM prepagato non locale è SEMPRE commissionato)
084350010704     C                   if        §RMfpr = 'C' and
084360011012     C                             ORMtor = 'P'
084370080407      * se po emissione diverso da po ritiro controllo se il po ritiro è la capofila
084380080407      * del po emissione
084390080407     c                   if        ormpoe <> ormpor
084400080407     c                   clear                   fnlv55ds
084410080407     c                   eval      d55tpt = '6'
084420080407     c                   eval      d55lin = ormpoe
084430080407     c                   eval      d55drf = oggi_aammgg
084440080407     c                   call      'FNLV55R'
084450080407     c                   parm                    fnlv55ds
084460080407     c                   if        d55err <> *blanks
084470080407     c                   clear                   d55tfa
084480080407     c                   endif
084490080407     c                   endif
084500080407      * questo per parare gli errori dovuti a orm emessi da filiali in £6 con ritiro
084510080407      * presso la capofila
084520080407     c                   if        ormpoe = ormpor or ormpor = d55tfa
084530010704     C                   exsr      Sr_Immprep
084540010704
084550010704      * Non è stato creato il prepagato
084560010704      * devo annullare l'ORM appena creato
084570010704     C                   if        okbolla = *blanks
084580010704     C                   eval      §RMerr = '5'
084590010704      * Deleto l'ORM
084600010704     C                   clear                   fior30ds
084610010704     C                   eval      i30mod = 'D'
084620010704     C                   eval      i30poe = ormpoe
084630010704     C                   eval      i30nsr = ormnsr
084640010704     C                   eval      i30nor = ormnor
084650010704     C                   eval      i30nrv = ormnrv
084660010704     C                   call      'FIOR30R'
084670010704     C                   parm                    kpjba
084680010704     C                   parm                    fior30ds
084690010704
084700010704     C                   endif
084710080407     c                   endif
084720010704     C                   endif
084730010704
084740001026     C                   endif
084750001011
084760071108     ?* Aggiorna
084770010919   1bC                   if        *in02 = *on
084780071108     c                   select
084790071126      * sono in conferma proposte di variazione
084800071126     C                   when      §rmfpr = 'P'
084810071126     c                   eval      $modifica = *on
084820071126     C  n10              exsr      Sr_Storico
084830140227      /free
084840151023       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
084850151023       //?da rcd già vincolato da questo lavoro
084860151023         unlock FNORG01L;
084870140227       //?Richiamo pgm di gestione note per convalidare la proposta di variazione
084880140227       //?della nota
084890140227         chain (§RMpoe:§RMnsr:§RMnor:§RMnrv:§RMdtv:§RMorv) FNORPT1L;
084900140227         IF  %found(FNORPT1L);
084910140227           clear fiort1ds;
084920140227           IORT1mod = 'P';
084930140227           IORT1poe = ORMpoe;
084940140227           IORT1nsr = ORMnsr;
084950140227           IORT1nor = ORMnor;
084960140227           IORT1nrv = ORMnrv;
084970140227           IORT1dim = §RMdtv;
084980140227           IORT1oim = §RMorv;
084990140227           IORT1pgm = 'FIOR05R';
085000140227           §kpjbu = kpjbu;
085010140227           fiort1r (kpjba:fiort1ds);
085020140227           kpjbu = §kpjbu;
085030140227         ENDIF;
085040151023       //?Rialloco FNORG
085050151023         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
085060140227      /end-free
085070140227
085080010205      * Non sono in conferma orm da altri p.o.
085090071108     C                   when      §rmfpr <> 'O'
085100071108      * è stato modificato qualcosa
085110131018     c                             and (vidnew <> vidold
085120160406     c                               or vidnew2 <> vidold2
085130160406     c                               or vidnewA <> vidoldA
085140160406     c                               or vidnewC <> vidoldC)
085150071108      * non è un orm dirottato
085160071108     c                             and wokdirotta = *off
085170071108     c                   eval      $modifica = *on
085180071106      * se devo scrivere una proposta lo faccio subito
085190071106      * e poi esco dalla routine
085200140918     c                   if        $proposte = *on and not OrmFase420
085210071106     c                   exsr      sr_carproposte
085220080225     c                   unlock    fnorm01l
085230151023     c                   unlock    fnorg01l
085240071106     c                   goto      endsrconf
085250071106     c                   endif
085260071108      * controllo se devo scrivere lo Storico o la proposta di variazione
085270150305    2C                   if        %lookup(%editc(ormpor:'X'):skpog) > 0
085280071108      * scrivo lo storico
085290170605    3C*******            if        num_v1cpor <> v1cpoe
085300010222     C  n10              exsr      Sr_Storico
085310170605   3xC*******            else
085320170605    4C*******            if        v1fao >=300
085330170605     C**n10              exsr      Sr_Storico
085340170605    4C*******            endif
085350170605    3C*******            endif
085360071108      * scrivo la proposta
085370071108   2xC                   else
085380010202     C                   exsr      Sr_CarProposte
085390080225     c                   unlock    fnorm01l
085400151023     c                   unlock    fnorg01l
085410010919     C                   goto      endsrconf
085420071108    2C                   endif
085430071108      * Non sono in conferma orm da altri p.o.
085440071108     C                   when      §rmfpr <> 'O'
085450071108      * è un orm dirottato
085460071108     c                             and wokdirotta = *on
085470071108     c                   eval      $modifica = *on
085480040615      * Storico
085490040615     c                   ExSr      Sr_Storico
085500040618      * scrivo la fase 200
085510040615     C                   clear                   fnorf000
085520040615     C                   eval      ORFpoe = v1cpoe
085530040615     C                   eval      ORFnsr = v1nsr
085540040615     C                   eval      ORFnor = v1nor
085550040615     C                   eval      ORFnrv = v1nrv
085560040615     C                   eval      ORFpog = OrmPor
085570040615     C                   eval      ORFdae = v1dfo
085580040615     C                   eval      ORFore = v1ofo
085590051220     c                   Move      v1ofo         wora
085600051220     c                   subdur    1:*s          wora
085610051220     c                   move      wora          orfore
085620040618     C                   eval      ORFfar = 200
085630040615     C                   eval      ORFpue = knmus
085640040615     C                   write     fnorf000
085650040615      * scrivo fase 50
085660040615     C                   clear                   fnorf000
085670040615     C                   eval      ORFpoe = v1cpoe
085680040615     C                   eval      ORFnsr = v1nsr
085690040615     C                   eval      ORFnor = v1nor
085700040615     C                   eval      ORFnrv = v1nrv
085710040615     C                   eval      ORFpog = num_v1cpor
085720040615     C                   eval      ORFdae = v1dfo
085730040615     C                   eval      ORFore = v1ofo
085740040615     C                   eval      ORFfar = v1fao
085750040615     C                   eval      ORFpue = knmus
085760040615     C                   write     fnorf000
085770160517     C                   feod      fnorf01l
085780040615      * pulisco i campi che non devo reimpostare
085790040615     c                   Clear                   v1npg
085800040615     c                   Clear                   v1ndc
085810040615     c                   Clear                   v1ddc
085820040615     c                   Clear                   v1dst
085830140918
085840010919      * se sono in conferma ORM da altri p.o. devo storicizzare
085850010919      * se sono state fatte delle variazioni
085860071108     C                   when      §rmfpr = 'O'
085870071108     c                   eval      $modifica = *on
085880030609      * Prima di confermare la fase 100 dell'orm genero la bolla 'FT' di orm commissionato
085890030623      * se cod.ordinante codificato e se non è un ORM prepagato
085900071108    2c                   If        v1com = 'S' and
085910030610     c                             ds_V1ccor > *Zeros and
085920030610     c                             ds_V1cor2 <> 8888 and ds_V1cor2 <> 9999
085930030623     c                             and v1ctor <> 'P'
085940041130      * non deve esserci già una fase 200
085950041130     c                   Eval      wfase200 = *Off
085960041130     C     kfnor         setll     fnorf01l
085970071108    3c                   Do        *Hival
085980041130     C     kfnor         reade(n)  fnorf01l
085990041130     C                   if        %eof (fnorf01l)
086000041130     c                   Leave
086010041130     c                   EndIf
086020071108    4C                   If        orffar = 200
086030041130     c                   eval      wfase200 = *On
086040041130     c                   Leave
086050071108    4c                   EndIF
086060071108    3c                   EndDo
086070071108    3c                   If        wfase200 = *Off
086080030609     c                   Exsr      Sr_BollaFt
086090030609      * se f12 o f03 da immissione bolla riemetto la videata
086100071108    4c                   If        okbolla = '1'
086110030609     c                   Eval      *in28 = *On
086120030609     c                   GoTo      endsrconf
086130071108    4c                   EndIf
086140140506      /free
086150140506       //?se OK bolle e sto confermando un ORM commissionato con Alert
086160140506       //?imposto già che l'ORM è confermato così poi esce dal FIOR05R
086170140506         IF  walert;
086180140506           wormconf = *on;
086190140506         ENDIF;
086200140506      /end-free
086210071108    3c                   EndIf
086220071108    2c                   EndIf
086230030612      * storicizzo se ho fatto vedere la videata
086240131018     C                   if        (Vidnew <> Vidold
086250131211     c                              or vidnew2 <> vidold2
086260160406     c                              or vidnewA <> vidoldA
086270160406     c                              or vidnewC <> vidoldC)
086280030612     c                             and §rmtla = '2'
086290010919     C                   exsr      Sr_Storico
086300071108     C                   endif
086310050322      * sto dirottatando l'orm invece di confermarlo
086320071108    2c                   if        wokdirotta = *On
086330050322      * scrivo la fase 200
086340050322     C                   clear                   fnorf000
086350050322     C                   eval      ORFpoe = v1cpoe
086360050322     C                   eval      ORFnsr = v1nsr
086370050322     C                   eval      ORFnor = v1nor
086380050322     C                   eval      ORFnrv = v1nrv
086390050322     C                   eval      ORFpog = OrmPor
086400050322     C                   eval      ORFdae = v1dfo
086410050322     C                   eval      ORFore = v1ofo
086420070125     c                   Move      v1ofo         wora
086430070125     c                   subdur    1:*s          wora
086440070125     c                   move      wora          orfore
086450050322     C                   eval      ORFfar = 200
086460050322     C                   eval      ORFpue = knmus
086470050322     C                   write     fnorf000
086480160517     c                   feod      fnorf01l
086490071108    2c                   EndIf
086500071108     c                   endsl
086510140918
086520010202      * Scrive fase per chisura o conferma orm da altri p.o.
086530010202     C                   if        *in10 = *on or §RMfpr = 'O'
086540010202     C                   exsr      Sr_Carfilef
086550010202     C                   endif
086560140918
086570090211      * memorizzo i dati dell'appuntamento
086580150305     c                   if        §RMfpr = 'O'
086590090330     c                             and wokdirotta = *off
086600090407     c                             and wokapp = *on
086610090407      * l'appuntamento va inserito in fase 100 quindi se sto chiudendo
086620090407      * l'ORM prima scrivo la fase 100 poi scrivo i dati dell'appuntamento
086630090407     c                   if        *in10
086640090407     C                   clear                   fnorf000
086650090407     C                   eval      ORFpoe = ormpoe
086660090407     C                   eval      ORFnsr = ormnsr
086670090407     C                   eval      ORFnor = ormnor
086680090407     C                   eval      ORFnrv = ormnrv
086690090407     C                   eval      ORFpog = ormpor
086700090407     C                   eval      ORFdae = v1dfo
086710090407     C                   eval      ORFore = v1ofo
086720090407     c                   Move      v1ofo         wora
086730090407     c                   subdur    1:*s          wora
086740090407     c                   move      wora          orfore
086750090407     C                   eval      ORFfar = 100
086760090407     C                   eval      ORFpue = knmus
086770090407     C                   write     fnorf000
086780160517     c                   feod      fnorf01l
086790090407     c                   endif
086800090211     c                   exsr      sr_wrtapp
086810090211     c                   endif
086820070119
086830070119      * salvo i dati modificati sensibili per PDA
086840070123      * se p.o. abilitato al PDA
086850140122      * o se filiale ritiro abilitata alle telefonate AUT
086860070119      * se sono in modifica (se proposta variazione non arriva qua)
086870070119      * se ORM in fase 400 o 410
086880070119      * negli altri casi non serve
086890140122     c                   if        *in02 and
086900140317     c                             *in22 and
086910071210     c                             (v1fao = 400 or v1fao = 410)
086920070119     c                   exsr      sr_datipdamod
086930070119     c                   endif
086940070119
086950071108      * se aggiornato qualcosa
086960071108     c                   if        $modifica = *on
086970001023      * Aggiorna ORM
086980001011     C                   exsr      Sr_Carfile
086990151016      * aggiorno fnorg (GIRO)
087000151016     c                   exsr      sr_aggorg
087010131018      /free
087020131018       //?Se modificati i dati win alert o dati orari apertura
087030161028       //?o se modificata data pronta merce
087040131029       //?o se conferma proposta di variazione
087050131018       //?aggiorno FNORE
087060131018        IF  old_v1OraAMda <> v1OraAMda or
087070131018            old_v1OraAMa  <> v1OraAMa  or
087080131018            old_v1OraPMda <> v1OraPMda or
087090131018            old_v1OraPMa  <> v1OraPMa  or
087100161028            old_v1dpm <> v1dpm or
087110131029            vidoldA <>  vidnewA or
087120131029            §RMfpr = 'P';
087130131018          exsr aggFNORE;
087140131018        ENDIF;
087150131018      /end-free
087160001108     C                   update    fnorm000
087170090202     c                   else
087180090202     c                   UNLOCK    FNORM01L
087190151023     c                   unlock    fnorg01l
087200071108     c                   endif
087210070119
087220070123      * se il p.o. è abilitato al PDA
087230140122      * o abilitato alle telefonate AUT
087240140317     c                   if         *in22  and
087250070228      * non è un dirottamento o chiusura ORM
087260070228     c                             (wokdirotta = *off or not *in10) and
087270070119      * l'orm è in fase 400 o 410
087280080314     c                             (ormfao = 400 or ormfao = 410) and
087290070119      * almeno uno dei dati sensibili per PDA è stato variato
087300070119     c                             datipdasav <> datipdamod
087310140123       //?Verifico cosa è stato variato
087320140122     c                   exsr      ctrvardati
087330070122      * aggiorno i dati inviati al PDA
087340070124     c                   clear                   fior56ds
087350070124     c                   eval      or56tla = 'O'
087360080121     c                   eval      or56fgs = orffgs
087370080121     c                   eval      or56ndcd = orfndc
087380080121     c                   eval      or56ndca = orfndc
087390140120     c                   eval      OR56cmd = 'V'
087400140122     c                   eval      OR56var = wVardati
087410070119     c                   exsr      sr_aggpdo
087420070119     c                   endif
087430070119
087440010919   1eC                   endif
087450001106
087460001109     C                   eval      *in10 = *off
087470001011
087480001019     C     endsrconf     endsr
087490010202      **********************************************************************
087500010202      * F7 - Ricerca cliente ritiro
087510010202      **********************************************************************
087520010202     C     Sr_RicRitiro  BEGSR
087530010202
087540010202     C                   eval      *in90 = *on
087550010202
087560130916     c                   clear                   FIOR81DS
087570130916     c                   IF        V1cra1 > 0
087580130916     c                   eval      I81fil = V1cra1
087590130916     c                   ENDIF
087600130916     c                   IF        V1rsr <> *blanks
087610130916     c                   eval      I81rag = V1rsr
087620130916     c                   ENDIF
087630150907     c                   IF        V1prr <> *blanks
087640150907     c                   eval      I81prv = V1prr
087650150907     c                   ENDIF
087660130916     c                   call      'FIOR81R'
087670130916     c                   parm                    kpjba
087680130916     c                   parm                    FIOR81DS
087690130916     c                   IF        O81err <> *blanks
087700130916     c                   eval      V1Cmsg = O81msg
087710130916     c                   seton                                        41  28
087720130916     c                   leavesr
087730130916     c                   ENDIF
087740130916     c                   IF        O81ret = '1'
087750130916     c                   leavesr
087760130916     c                   ENDIF
087770130916
087780130916     c                   eval      ds_v1ccra = O81cro
087790070905     c                   eval      v1cra1 = ds_v1cra1
087800070905     c                   eval      v1cra2 = ds_v1cra2
087810070905     c                   eval      v1cra3 = ds_v1cra3
087820070905     c                   exsr      sr_contrclir
087830010202
087840010202     C                   endsr
087850010202      **********************************************************************
087860010202      * F8 - Ricerca cliente ordinante
087870010202      **********************************************************************
087880010202     C     Sr_RicOrdin   BEGSR
087890010202
087900010202     C                   eval      *in90 = *on
087910070905
087920130916     c                   clear                   FIOR81DS
087930130916     c                   IF        V1cor1 > 0
087940130916     c                   eval      I81fil = V1cor1
087950130916     c                   ENDIF
087960130916     c                   IF        V1rso <> *blanks
087970130916     c                   eval      I81rag = V1rso
087980130916     c                   ENDIF
087990150907     c                   IF        V1pro <> *blanks
088000150907     c                   eval      I81prv = V1pro
088010150907     c                   ENDIF
088020130916     c                   call      'FIOR81R'
088030130916     c                   parm                    kpjba
088040130916     c                   parm                    FIOR81DS
088050130916     c                   IF        O81err <> *blanks
088060130916     c                   eval      V1Cmsg = O81msg
088070130916     c                   seton                                        42  28
088080130916     c                   leavesr
088090130916     c                   ENDIF
088100130916     c                   IF        O81ret = '1'
088110130916     c                   leavesr
088120130916     c                   ENDIF
088130130916
088140130916     c                   eval      ds_v1ccor = O81cro
088150070905     c                   eval      v1cor1 = ds_v1cor1
088160070905     c                   eval      v1cor2 = ds_v1cor2
088170070905     c                   eval      v1cor3 = ds_v1cor3
088180070905     c                   exsr      sr_contrclio
088190010202
088200010202     C                   endsr
088210010202      **********************************************************************
088220011113      * F9 - Ricerca cliente destinatario
088230010202      **********************************************************************
088240010202     C     Sr_RicCons    BEGSR
088250010202
088260010202     C                   eval      *in90 = *on
088270070905
088280130916     c                   clear                   FIOR81DS
088290130916     c                   IF        V1crc1 > 0
088300130916     c                   eval      I81fil = V1crc1
088310130916     c                   ENDIF
088320130916     c                   IF        V1rsc <> *blanks
088330130916     c                   eval      I81rag = V1rsc
088340130916     c                   ENDIF
088350150907     c                   IF        V1prc <> *blanks
088360150907     c                   eval      I81prv = V1prc
088370150907     c                   ENDIF
088380130916     c                   call      'FIOR81R'
088390130916     c                   parm                    kpjba
088400130916     c                   parm                    FIOR81DS
088410130916     c                   IF        O81err <> *blanks
088420130916     c                   eval      V1Cmsg = O81msg
088430130916     c                   seton                                        56  28
088440130916     c                   leavesr
088450130916     c                   ENDIF
088460130916     c                   IF        O81ret = '1'
088470130916     c                   leavesr
088480130916     c                   ENDIF
088490130916
088500130916     c                   eval      ds_v1ccrc = O81cro
088510070905     c                   eval      v1crc1 = ds_v1crc1
088520070905     c                   eval      v1crc2 = ds_v1crc2
088530070905     c                   eval      v1crc3 = ds_v1crc3
088540070905     c                   exsr      sr_contrclid
088550010202
088560010202     C                   endsr
088570010202      **********************************************************************
088580010202      * F11 - Calcola Volume
088590010202      **********************************************************************
088600010202     C     Sr_Volume     BEGSR
088610010202
088620040301     c                   clear                   UL36vol
088630040301     c                   call      'TRUL36R'
088640040301     c                   parm                    UL36vol
088650040301     c                   add       UL36vol       V1vlm
088660010202
088670010202     C                   endsr
088680010202      **********************************************************************
088690010202      * F16 - Chiusura
088700010202      **********************************************************************
088710010202     C     Sr_Chiusura   BEGSR
088720010202
088730081125     c                   eval      wnobolla =  *off
088740020523     c                   eval      *in17 = *off
088750010202     C                   clear                   v3cau
088760010202     C                   clear                   v3dcau
088770010202     C                   eval      v3cpoe = ppoe
088780010202     C                   eval      v3nsr  = pnsr
088790010202     C                   eval      v3nor  = pnor
088800010202     C                   eval      v3nrv  = pnrv
088810071126
088820071126      * se è manutenzione
088830130412     c                   if        *in02 and §rmtla = *blanks and
088840071126      * ORM assegnato
088850071126      * scrivo proposta variazione
088860080314     c                             (v1fao = 400 or v1fao = 410)
088870071126     c                   eval      $proposte = *on
088880071126     c                   endif
088890071126
088900010202      * Videata con richiesta causale di chiusura (obbligatoria)
088910010227     C                   do        *hival
088920010202     C                   exfmt     fior05c
088930010301     C                   setoff                                       28
088940010202      * F12=Ritorno e non chiudo
088950050609     c   kl              clear                   v3cau
088960010202     C   kl              goto      endsrchiuso
088970070228
088980070228      * se il p.o. che chiude è abilitato al PDA
088990070228      * e fase 400 o 410
089000071210     c                   if        *in22 and
089010070228     c                             (ormfao = 400 or ormfao = 410)
089020070228      * devo avvisare che ho già avuto un esito da AUT
089030070228      * se l'orm è in fase 410
089040070228     c                   if        ormfao = 410 and wforzachi = *off
089050070228     c                   eval      v3cmsg = msg(41)
089060070228     c                   seton                                        28
089070070228     c                   eval      wforzachi = *on
089080070228     c                   iter
089090070228     c                   endif
089100070228     c                   endif
089110070228
089120010202      * Causale obbligatoria
089130010202     C                   if        v3cau = *blanks
089140010202     C                   movel     msg(14)       v3cmsg
089150010202     C                   clear                   v3dcau
089160010202     C                   eval      *in28 = *on
089170010202     C                   iter
089180010202     C                   endif
089190010302
089200010302     C                   clear                   v3dcau
089210010202      * Ricerco causale
089220010202     C     '?'           scan      v3cau                                  30
089230010202     C     *in30         ifeq      *on
089240010202     C                   clear                   TIBS02DS
089250010301     C                   clear                   dcmr
089260010202     C                   movel     'R'           t02mod
089270010202     C                   movel     knsif         t02sif
089280010227     C                   movel     'CMR'         t02cod
089290010202     C                   call      'TIBS02R'
089300010202     C                   parm                    KPJBA
089310010202     C                   parm                    TIBS02DS
089320010202     C                   movel     T02ke1        v3cau
089330010301     C                   movel     T02uni        dcmr
089340010202     C                   iter
089350010202     C                   endif
089360010202      * Decodifico
089370010301     C                   clear                   dcmr
089380010202     C                   clear                   TIBS02DS
089390010202     C                   movel     'C'           T02mod
089400010202     C                   movel     knsif         t02sif
089410010227     C                   movel     'CMR'         t02cod
089420010202     C                   movel(p)  v3cau         T02ke1
089430010202     C                   call      'TIBS02R'
089440010202     C                   parm                    KPJBA
089450010202     C                   parm                    TIBS02DS
089460010202     C                   if        t02err <> *blanks
089470010202     C                   movel     msg(15)       v3cmsg
089480010202     C                   eval      *in28 = *on
089490010202     C                   iter
089500010202     C                   endif
089510010202     C                   movel     t02ke1        v3cau
089520010301     C                   movel     t02uni        dcmr
089530010622
089540010301     C                   movel     d§cmrdes      v3dcau
089550030912
089560030912      * nei casi in cui il flag sia impostato a 'E' o a 'R'
089570030912      * se p.o. ritiro diverso da p.o. emissione controllo se posso usare la causale
089580030912     c                   If        Ormpoe <> OrmPor
089590030912      * se impostato a 'E' il p.o. emissione non può usare la causale
089600151014     c                   If        d§CmrNoc = 'E' and  ORMpoe = DUTpou and
089610150622     c                             %lookup(%editc(ORMpoe:'X'):skpog) > 0
089620030806     C                   movel     msg(64)       v3cmsg
089630030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089640070418     c                             'dalla filiale emissione'
089650030806     C                   eval      *in28 = *on
089660030806     C                   iter
089670030806     c                   EndIf
089680030912      * se impostato a 'R' il p.o. ritiro non può usare la causale
089690151014     c                   If        d§CmrNoc = 'R' and  ORMpor = DUTpou and
089700150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
089710030806     C                   movel     msg(64)       v3cmsg
089720030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089730070418     c                             'dalla filiale ritiro'
089740030806     C                   eval      *in28 = *on
089750030806     C                   iter
089760030806     c                   EndIf
089770030912     c                   EndIf
089780030912      * se impostato a 'S' non si può usare e basta
089790030806     C                   if        d§cmrnoc = 'S'
089800010305     C                   movel     msg(64)       v3cmsg
089810010302     C                   eval      *in28 = *on
089820010302     C                   iter
089830010302     C                   endif
089840100218
089850100218      /free
089860100218       //?Se ORM NON prevede il preavviso non posso utilizzare la nuova causale '98'
089870100218       IF  §ormpre <> 'M' and v3cau = '98';
089880100218         v3cmsg = msg(64);
089890100218         v3cmsg = %trim(v3cmsg) + '. Il ritiro non prevede il PREAVVISO';
089900100218         *in28 = *on;
089910100218         iter;
089920100218       ENDIF;
089930100218      /end-free
089940081028
089950081124      * se causale inserita genera bolla di addebito (colpa cliente)
089960081028      * controllo che l'ORM non abbia già avuto una causale che genera addebito
089970081125      * in questo caso faccio solo un addebito
089980081124     c                   if        d§cmradd = 'S'
089990081028     c     kfnor         setll     fnorf01l
090000081028     c                   do        *hival
090010081028     c     kfnor         reade(n)  fnorf01l
090020081028     c                   if        %eof (fnorf01l)
090030081028     c                   leave
090040081028     c                   endif
090050081029      * senza causale rileggo
090060081028     c                   if        orfcar = *blanks
090070081028     c                   iter
090080081028     c                   endif
090090081029      * fase da PDA rileggo
090100081029     c                   if        orffar = 410
090110081029     c                   iter
090120081029     c                   endif
090130081028     c                   clear                   a_dcmr
090140081028     c                   clear                   tibs02ds
090150081028     c                   eval      t02mod = 'C'
090160081028     c                   eval      t02sif = knsif
090170081028     c                   eval      t02cod = 'CMR'
090180081028     c                   eval      t02ke1 = orfcar
090190081028     c                   call      'TIBS02R'
090200081028     c                   parm                    kpjba
090210081028     c                   parm                    tibs02ds
090220081028     c                   eval      a_dcmr = t02uni
090230081124      * se causale che genera bolla vado avanti senza generare l'addebito per la seconda volta
090240081028     c                   if        a_d§cmradd = 'S'
090250081125     c                   eval      wnobolla = *on
090260081028     c                   leave
090270081028     c                   endif
090280081028     c                   enddo
090290081028     c                   endif
090300081007
090310081007      * se ORM automatico DPD non si può chiudere l'ORM con la causale 80
090320081007      * l'unica eccezione è per la generazione automatica da parte dei pgm di AB
090330081007      * che creano la proposta di chiusura con causale 80
090340081007     c     ormpoe        lookup    skpodpd                                30
090350081007     c                   if        §rmfpr <> 'P' and ormtco = 'F' and
090360081007     c                             *in30 and v3cau = '80'
090370081007     c                   eval      v3cmsg = msg(49)
090380081007     c                   eval      *in28 = *on
090390081007     c                   iter
090400081007     c                   endif
090410081007
090420010227      * Conferma chiusura
090430010227     C                   if        *inkf = *on
090440071126      * se non è una proposta di variazione
090450071126     c                   if        $proposte = *off
090460020524      * procedo con i controlli per creare l'addebito solo se l'orm è mio
090470150306     c                   if        ormpoe = ormpor or
090480150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
090490020523     c                   if        d§cmradd <> *blanks
090500080314     c                             and (ormfao = 400 or ormfao = 410)
090510020523     c                   exsr      sr_contradd
090520020523      * se f12 o f03 da immissione bolla riemetto la videata
090530020523     c                   if        okbolla = '1'
090540020523     c                   iter
090550020523     c                   endif
090560020524     c                   if        wbolla = *on
090570020523     c                   eval      *in17 = *on
090580020524     c                   endif
090590020523     c                   endif
090600020524     c                   endif
090610071126     c                   endif
090620010202
090630010202      * Salva dati della fase nel caso l'utente cambi idea (f12)
090640010202     C                   eval      sav_v1fao = v1fao
090650010202     C                   eval      sav_v1dfo = v1dfo
090660010202     C                   eval      sav_v1ofo = v1ofo
090670070228      * salvo la fase anche per aggiornare FIPDO
090680070228     c                   eval      savorffar = orffar
090690070228      * e salvo anche la distinta per passarla al pgm che aggiorna FIPDO
090700070228     c                   eval      savorffgs = orffgs
090710070228     c                   eval      savorfndc = orfndc
090720070228     c                   eval      savorfddc = orfddc
090730150922      * imposto peso/volume di ORG in modo da non azzerarlo al momento della conferma
090740150922      * della chiusura ORM
090750150922     c                   eval      wpkg = ORGpkg
090760150922     c                   eval      wvlm = ORGvlm
090770010202
090780010301     C                   movel     d§cmrfar      v1fao                          chiuso
090790050422     C                   z-add     oggi_aammgg   v1dfo
090800050422     C                   time                    w0140
090810010202     C                   movel     w0140         v1ofo
090820010202     C                   eval      *in10 = *on
090830010305
090840010305     C                   eval      v1cau = v3cau
090850010305     C                   eval      v1dcau = v3dcau
090860010301
090870010227     C                   leave
090880010227     C                   endif
090890010227     C
090900010227     C                   enddo
090910010202      * Decodifica la fase
090920010202     C                   exsr      Sr_Contrfao
090930131025
090940131025      /free
090950131025       //?Imposto la riga dei tasti funzione
090960131025       ExSr Sr_Tastifun;
090970131025      /end-free
090980010202
090990010202     C     endsrchiuso   endsr
091000020522      **********************************************************************
091010020522      * Controllo se devo generare la bolla di addebito
091020020522      **********************************************************************
091030020522     c     sr_contradd   begsr
091040020522
091050020522     c                   clear                   sav_lpksc
091060020523     c                   clear                   okbolla
091070020524     c                   eval      wbolla = *off
091080081125
091090081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
091100081125      * come se avessi fatto la bolla
091110081125     c                   if        wnobolla = *on
091120081125     c                   eval      wbolla = *on
091130081125     c                   leavesr
091140081125     c                   endif
091150020522
091160020522      * se cod.ordinante codificato addebito
091170020522 b1  c                   if        ds_v1ccor <> *zeros and v1cor2 <> 8888
091180020522     c                             and v1cor2 <> 9999
091190020523     c                   clear                   dblp
091200090504     c                   movel     ormcor        §lpksca
091210020702      * se c'è imposto il codice bollettazione
091220151109     c     ormcor        chain(n)  fnacr01l
091230020702     c                   if        %found(fnacr01l) and acratb = *blanks
091240020702     c                             and acrksc <> *zeros
091250090504     c                   move      acrksc        §lpksca
091260090505     c                   if        acrccc<>999
091270090504     c                   move      acrccc        §lpctra
091280090505     c                   endif
091290020702     c                   endif
091300020522     c                   eval      §lprsd = ormrso
091310020522     c                   eval      §lpind = ormino
091320020522     c                   eval      §lpcad = ormcao
091330020522     c                   eval      §lplod = ormloo
091340020522     c                   eval      §lpprd = ormpro
091350020522     c                   eval      §lpnzd = ormnao
091360020522     c                   exsr      sr_wrtbolla
091370020522 x1  c                   else
091380020522      * se non inserito l'ordinante
091390020522 b2  c                   if        v1rso = *blanks
091400050323      * orm locale
091410050323 b3  c                   if        v1cpoe = num_v1cpor
091420020522      * se cod.mittente codificato addebito
091430020522 b4  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
091440020522     c                             and v1cra2 <> 9999
091450020523     c                   clear                   dblp
091460090504     c                   movel     ormcra        §lpksca
091470020702      * se c'è imposto il codice bollettazione
091480151109     c     ormcra        chain(n)  fnacr01l
091490020702     c                   if        %found(fnacr01l) and acratb = *blanks
091500020702     c                             and acrksc <> *zeros
091510090504     c                   move      acrksc        §lpksca
091520090505     c                   if        acrccc<>999
091530090504     c                   move      acrccc        §lpctra
091540090505     c                   endif
091550020702     c                   endif
091560020522     c                   eval      §lprsd = ormrsr
091570020522     c                   eval      §lpind = orminr
091580020522     c                   eval      §lpcad = ormcar
091590020522     c                   eval      §lplod = ormlor
091600020522     c                   eval      §lpprd = ormprr
091610020522     c                   eval      §lpnzd = ormnar
091620020522     c                   exsr      sr_wrtbolla
091630020522      * se cod.mittente non codificato
091640020522 x4  c                   else
091650020522      * se cod.destinatario codificato addebito
091660020522 b5  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
091670020522     c                             and v1crc2 <> 9999
091680020523     c                   clear                   dblp
091690090504     c                   movel     ormcrc        §lpksca
091700020702      * se c'è imposto il codice bollettazione
091710151109     c     ormcrc        chain(n)  fnacr01l
091720020702     c                   if        %found(fnacr01l) and acratb = *blanks
091730020702     c                             and acrksc <> *zeros
091740090504     c                   move      acrksc        §lpksca
091750090505     c                   if        acrccc<>999
091760090504     c                   move      acrccc        §lpctra
091770090505     c                   endif
091780020702     c                   endif
091790020522     c                   eval      §lprsd = ormrsc
091800020522     c                   eval      §lpind = orminc
091810020522     c                   eval      §lpcad = ormcac
091820020522     c                   eval      §lplod = ormloc
091830020522     c                   eval      §lpprd = ormprc
091840020522     c                   eval      §lpnzd = ormnac
091850020522     c                   exsr      sr_wrtbolla
091860020522 e5  c                   endif
091870020522 e4  c                   endif
091880050323      * orm non locale
091890020522 x3  c                   else
091900020522      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
091910020522 b4  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
091920020522     c                             and v1crc2 <> 9999  and v1crc1 = v1cpoe
091930020523     c                   clear                   dblp
091940090504     c                   movel     ormcrc        §lpksca
091950020702      * se c'è imposto il codice bollettazione
091960151109     c     ormcrc        chain(n)  fnacr01l
091970020702     c                   if        %found(fnacr01l) and acratb = *blanks
091980020702     c                             and acrksc <> *zeros
091990090504     c                   move      acrksc        §lpksca
092000090505     c                   if        acrccc<>999
092010090504     c                   move      acrccc        §lpctra
092020090505     c                   endif
092030020702     c                   endif
092040020522     c                   eval      §lprsd = ormrsc
092050020522     c                   eval      §lpind = orminc
092060020522     c                   eval      §lpcad = ormcac
092070020522     c                   eval      §lplod = ormloc
092080020522     c                   eval      §lpprd = ormprc
092090020522     c                   eval      §lpnzd = ormnac
092100020522     c                   exsr      sr_wrtbolla
092110020522      * se non codificato o p.o. differenti
092120020522 x4  c                   else
092130020522      * se cod.mittente codificato addebito
092140020522 b5  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
092150020522     c                             and v1cra2 <> 9999
092160020523     c                   clear                   dblp
092170090504     c                   movel     ormcra        §lpksca
092180020702      * se c'è imposto il codice bollettazione
092190151109     c     ormcra        chain(n)  fnacr01l
092200020702     c                   if        %found(fnacr01l) and acratb = *blanks
092210020702     c                             and acrksc <> *zeros
092220090504     c                   move      acrksc        §lpksca
092230090505     c                   if        acrccc<>999
092240090504     c                   move      acrccc        §lpctra
092250090505     c                   endif
092260020702     c                   endif
092270020522     c                   eval      §lprsd = ormrsr
092280020522     c                   eval      §lpind = orminr
092290020522     c                   eval      §lpcad = ormcar
092300020522     c                   eval      §lplod = ormlor
092310020522     c                   eval      §lpprd = ormprr
092320020522     c                   eval      §lpnzd = ormnar
092330020522     c                   exsr      sr_wrtbolla
092340020522 e5  c                   endif
092350020522 e4  c                   endif
092360020522 e3  c                   endif
092370020522 e2  c                   endif
092380020522 e1  c                   endif
092390020522
092400020522     c                   endsr
092410020522      **********************************************************************
092420020522      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
092430020522      **********************************************************************
092440020522     c     sr_wrtbolla   begsr
092450020522
092460030623     c                   Clear                   wtrulvid
092470020522      * richiamo il programma per la scelta delle stampanti
092480030307     c                   Clear                   wdatibolla
092490030307     c                   Eval      wdatibolla = 'BollaAdd.'
092500030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
092510030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
092520020522     c                   exsr      sr_scestp
092530020524     c   92              eval      okbolla = '1'
092540020522     c   92              goto      endwrtbolla
092550020522
092560020522     c                   clear                   kpjbu
092570020522     c                   clear                   dtasv
092580020522
092590020522     c                   eval      §lpfpr = 'T'
092600020522     c                   movel     ormpor        §lpccn
092610020522     c                   move      9999          §lpccn
092620020523     c                   movel     oggi_aammgg   §lpaas
092630020523     c                   move      oggi_aammgg   §lpmgs
092640020522     c                   eval      §lplnp = ormpor
092650020522     c                   eval      §lpcbo = 'FY'
092660020522     c                   eval      §lpsop = '+'
092670020530     c                   eval      §lplna = ormpoe
092680020522     c                   eval      §lprsm = ormrsr
092690020522     c                   eval      §lpinm = orminr
092700020522     c                   eval      §lpcam = ormcar
092710020522     c                   eval      §lplom = ormlor
092720020522     c                   eval      §lpprm = ormprr
092730020522     c                   eval      §lpnzm = ormnar
092740020522
092750020522 b1  c                   if        ormncl <> 0
092760020522     c                   eval      §lpncl = ormncl
092770020522 x1  c                   else
092780020522     c                   eval      §lpncl = 1
092790020522 e1  c                   endif
092800020522 b1  c                   if        ormpkg <> 0
092810020522     c                   eval      §lppkb = ormpkg
092820020522 x1  c                   else
092830070918 b3  c                   if        ormbnc <> 0
092840070918     c                   eval      wpkb = ormbnc / d§dftbnc * d§dftpkg
092850070918     c                   if        wpkb > 999999,9
092860070918     c                   eval      wpkb = 999999,9
092870070918     c                   endif
092880070918     c                   eval      §lppkb = wpkb
092890070918 x2  c                   else
092900020522 b2  c                   if        ormvlm <> 0
092910070918     c                   eval      wpkb = ormvlm * d§dftpkg
092920070918     c                   if        wpkb > 999999,9
092930070918     c                   eval      wpkb = 999999,9
092940070918     c                   endif
092950070918     c                   eval      §lppkb = wpkb
092960020522 e3  c                   endif
092970020522 e2  c                   endif
092980020522 e1  c                   endif
092990020522
093000020522     c                   clear                   ds_keyorm
093010020522     c                   eval      ds_poe = ORMpoe
093020020522     c                   eval      ds_nsr = ORMnsr
093030020522     c                   eval      ds_nor = ORMnor
093040020522     c                   eval      ds_nrv = ORMnrv
093050030716     c                   move      ds_keyorm     §lprmn
093060020522
093070020522     c                   eval      §lprma = ormrfa
093080020523     c                   eval      §lprmo = v3cau + '-' + d§cmrdes1
093090020522     c                   eval      §lpctm = '4F'
093100020523     c                   movel     oggi_aammgg   §lpdrt
093110020522
093120020522      * Cerca codice autotrasportatore e flag mattino/pomeriggio
093130020522 b1  c                   if        ormndc <> 0
093140120612      /free
093150120612        DSTnpg = ORMnpg;
093160120612        DSTnfv = ORMndc;
093170120612        DSTfgs = ORMpor;
093180120612        chain (DSTnpg:DSTnfv:DSTfgs) FIDST01L;
093190120612      /end-free
093200120612 b2  c                   if        %found(fidst01l)
093210020522     c                   eval      §lppdr = DSTpdr
093220120612     c                   eval      §lpfpp = DSTfpp
093230020523 x2  c                   else
093240020523     c                   movel     §lplnp        §lppdr
093250020523     c                   move      0999          §lppdr
093260020523     c                   eval      §lpfpp = 'M'
093270020523 e2  c                   endif
093280020522 x1  c                   else
093290020522     c                   movel     §lplnp        §lppdr
093300020522     c                   move      0999          §lppdr
093310020522     c                   eval      §lpfpp = 'M'
093320020522 e1  c                   endif
093330020522
093340020522     C                   call      'FNLS01R'
093350020522     C                   parm                    kpjba
093360020522     C                   parm                    dblp
093370020522     C                   parm                    dtasv
093380030623     c                   Parm                    Trul90ds
093390020522
093400020522     C                   if        §lpfpr = '4' or §lpfpr = '3'
093410020522     C                             or §lpfpr = '5'
093420020522     C                   eval      okbolla = '1'
093430020524     c                   else
093440020524     c                   eval      wbolla = *on
093450020522     C                   endif
093460020522
093470020522     c     endwrtbolla   endsr
093480030606      **********************************************************************
093490030606      * Scrivo bolla addebito "FT"
093500030606      **********************************************************************
093510030606     c     Sr_BollaFt    BegSr
093520030609
093530030609     c                   Clear                   okbolla
093540030623     c                   Eval      wtrulvid = 'S'
093550030623
093560030623      * richiamo il programma per la scelta delle stampanti
093570030623     c                   Clear                   wdatibolla
093580030623     c                   Eval      wdatibolla = 'BollaAdd.'
093590030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
093600030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
093610030623     c                   exsr      sr_scestp
093620030623     c   92              eval      okbolla = '1'
093630030623     c   92              GoTo      endbollaft
093640030606
093650030606     c                   Clear                   Kpjbu
093660030606     c                   Clear                   dtasv
093670030606     c                   Clear                   dblp
093680030606
093690030606     c                   Eval      §lpFpr = 'T'
093700030606      * Cod.Cliente mittente
093710030606      * Ksc dell'ordinante da Fnacr
093720030606     c                   If        AcrKscCor <> *Zeros
093730090504     c                   Eval      §lpksca = %editc(AcrKscCor:'X')
093740090505     c                   if        ACrCCCCor<>999
093750090505     c                   move      AcrCccCor     §lpctra
093760090505     c                   endif
093770030606     c                   Else
093780030606      * Primi 7 byte dell'ordinante
093790090504     c                   Eval      §lpksca = %editc(Ds_V1cor12:'X')
093800030606     c                   EndIf
093810030606
093820050324     c                   Movel     OrmPor        §lpCcn
093830030606     c                   Move      9999          §lpCcn
093840030606     c                   Movel     oggi_aammgg   §lpAas
093850030606     c                   Move      oggi_aammgg   §lpMgs
093860050324     c                   Eval      §lpLnp = OrmPor
093870030606     c                   Eval      §lpCbo = 'FT'
093880030606     c                   Eval      §lpSop = '+'
093890090504     c                   Movel     §lpksca       §lpLna
093900030606      * Mittente bolla = mittente ORM
093910030606     c                   Eval      §lpRsm = V1rsr
093920030606     c                   Eval      §lpInm = V1inr
093930030606     c                   Eval      §lpCam = V1car
093940030606     c                   Eval      §lpLom = V1lor
093950030606     c                   Eval      §lpPrm = V1prr
093960030606     c                   Eval      §lpNzm = V1nar
093970030606      * Destinatario bolla = ordinante ORM
093980030606     c                   Eval      §lpRsd = V1rso
093990030606     c                   Eval      §lpInd = V1ino
094000030606     c                   Eval      §lpCad = V1cao
094010030922     c                   Eval      §lpLod = sav_V1loo35
094020030606     c                   Eval      §lpPrd = V1pro
094030030606     c                   Eval      §lpNzd = V1nao
094040030606      * colli = colli ORM - se a 0 imposto 1
094050030609 b1  c                   if        V1ncl <> 0
094060030609     c                   eval      §lpncl = V1ncl
094070030606 x1  c                   else
094080030606     c                   eval      §lpncl = 1
094090030606 e1  c                   endif
094100030606      * peso = peso ORM - se a 0 ricavo dal volume o dai bancali dell'ORM
094110030609 b1  c                   if        V1pkg <> 0
094120030609     c                   eval      §lppkb = V1pkg
094130030606 x1  c                   else
094140070918 b3  c                   if        V1bnc <> 0
094150070918     c                   eval      wpkb = v1bnc / d§dftbnc * d§dftpkg
094160070918     c                   if        wpkb > 999999,9
094170070918     c                   eval      wpkb = 999999,9
094180070918     c                   endif
094190070918     c                   eval      §lppkb = wpkb
094200070918 x2  c                   else
094210030609 b2  c                   if        V1vlm <> 0
094220070918     c                   eval      wpkb = v1vlm * d§dftpkg
094230070918     c                   if        wpkb > 999999,9
094240070918     c                   eval      wpkb = 999999,9
094250070918     c                   endif
094260070918     c                   eval      §lppkb = wpkb
094270030606 e3  c                   endif
094280030606 e2  c                   endif
094290030606 e1  c                   endif
094300030606      * rif. mittente numerico = n.ORM
094310030606     c                   clear                   ds_keyorm
094320030609     c                   eval      ds_poe = V1cpoe
094330030609     c                   eval      ds_nsr = V1nsr
094340030609     c                   eval      ds_nor = V1nor
094350030609     c                   eval      ds_nrv = V1nrv
094360030716     c                   move      ds_keyorm     §lprmn
094370030606      * rif. mittente alfabetico = rif. alfabetico ORM
094380030609     c                   eval      §lprma = V1rfa
094390030606      * trattamento merce
094400030606     c                   eval      §lpctm = '4F'
094410030606      * data ritiro merce
094420030606     c                   movel     oggi_aammgg   §lpdrt
094430030606
094440030606      * codice autotrasportatore e flag mattino/pomeriggio
094450030606     c                   Eval      §lpFpp = 'M'
094460030606     c                   Movel     §lpLnp        §lpPdr
094470030606     c                   Move      0999          §lpPdr
094480030606
094490030606     c                   Call      'FNLS01R'
094500030606     c                   Parm                    Kpjba
094510030606     c                   Parm                    dblp
094520030606     c                   Parm                    dtasv
094530030623     c                   Parm                    Trul90ds
094540030606
094550030606     c                   If        §lpFpr = '4' or §lpFpr = '3'
094560030606     c                             or §lpFpr = '5'
094570030606     c                   Eval      okbolla = '1'
094580030606     c                   EndIf
094590030606
094600030606     c     endbollaft    EndSr
094610140130
094620140130      /free
094630140130       //--------------------------------------------------------------------
094640140130       //?F17 - Note AUT.
094650140130       //--------------------------------------------------------------------
094660140130       BEGSR F17D01;
094670140130
094680140130       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
094690140130       //?da rcd già vincolato da questo lavoro
094700140130         unlock FNORG01L;
094710140130
094720140130         clear fiort1ds;
094730140226         IORT1mod = 'G';
094740140130         IORT1poe = ORMpoe;
094750140130         IORT1nsr = ORMnsr;
094760140130         IORT1nor = ORMnor;
094770140130         IORT1nrv = ORMnrv;
094780140130         IORT1pgm = 'FIOR05R';
094790140130         §kpjbu = kpjbu;
094800140130
094810140130         fiort1r (kpjba:fiort1ds);
094820140130         kpjbu = §kpjbu;
094830140226
094840140226       //?Se ho scritto note ho la data e ora di immissione
094850140226         IF  OORT1dim > 0 and OORT1oim > 0;
094860140226           wF17write = *on;
094870140226         ENDIF;
094880140130
094890140130       //?Rialloco FNORG
094900140130         chain (kpoe:knsr:knor:knrv) FNORG01L;
094910140314
094920140314       //?Controllo se da F17 sono state scritte delle proposte
094930140314         exsr sr_orp;
094940140130
094950140130       //?Imposto la riga dei tasti funzione
094960140130         ExSr Sr_Tastifun;
094970140130
094980140130       ENDSR;
094990140130
095000140130      /end-free
095010140130
095020001025      **********************************************************************
095030001025      * F18 - Note
095040001025      **********************************************************************
095050001025     C     Sr_Note       BEGSR
095060001025
095070001025     C                   clear                   fior06DS
095080010118     C                   eval      i06dta = v1dao
095090010118     C                   eval      i06poe = v1cpoe
095100010118     C                   eval      i06nor = v1nor
095110010118     C                   eval      i06nsr = v1nsr
095120010118     C                   eval      i06nrv = v1nrv
095130010118     C                   eval      i06far = v1fao
095140010301     C                   eval      i06dai = oggi_aammgg
095150010301     C                   movel     w0140         I06ori
095160010220     C                   if        v1cpoe = *zeros
095170150305     C                   eval      i06poe = DUTpou
095180010220     C                   endif
095190001026
095200001025     C                   select
095210001026     C                   when      *in01 = *on
095220001026     C                   movel     'M'           i06flm
095230001026
095240001109     C                   when      *in03 = *on
095250001109     C                   movel     'M'           i06flm
095260001026
095270001026     C                   when      *in02 = *on
095280040402      * richiamo particolare nel caso di orm commissionato e variazione data ritiro
095290040402     c                   If        ormdar <> inv_v1dar and v1Com = 'S'
095300040419     c                   Eval      I06Flm = 'D'
095310040402     c                   Else
095320001026     C                   movel     'V'           i06flm
095330050421      * se p.o. ritiro diverso da p.o. emissione e da p.o. gestione e non è chiusura ORM
095340050421      * il pgm va in interrogazione note non da la possibilità di inserirne.....
095350050421      * aggiungo che la data ritiro deve essere uguale
095360050421      * xchè per data ritiro diversa e non è un ORM commissionato devo dare la possibilità
095370050421      * di inserire la nota
095380010116     C                   if        v1cpoe <> num_v1cpor  and
095390150305     C                             %lookup(%editc(num_v1cpor:'X'):skpog) = 0
095400150305     C                             and *in10 = *off
095410050421     c                             and ormdar = inv_v1dar
095420001027     C                   movel     'I'           i06flm
095430001027     C                   endif
095440050420     c                   EndIf
095450050128      * se sto facendo una proposta di chiusura ORM la fase deve essere la fase ORM e non la nuova
095460150305     c                   If        *In10 and
095470150305     c                             %lookup(%editc(ORMpor:'X'):skpog) = 0
095480050128     c                   Eval      i06far = OrmFao
095490050128     c                   EndIf
095500050322      * se sto confermando un ORM la fase deve essere la fase ORM e non la nuova
095510050324      * se non è chiusura ORM
095520050324     c                   If        §rmfpr = 'O' and Not *In10
095530050322     c                   Eval      i06far = OrmFao
095540050322     c                   EndIf
095550001026     C                   endsl
095560001026
095570001110     C                   movel(p)  kpjbu         §kpjbu
095580001110     C                   movel(p)  parm01        kpjbu
095590001025     C                   CALL      'FIOR06R'
095600001025     C                   PARM                    kpjba
095610001027     C                   parm                    fior06ds
095620001110     C                   movel     §kpjbu        kpjbu
095630131212      /free
095640131212       IF not *in01;
095650131212         exsr sr_orn;
095660131212       ENDIF;
095670131212       //?Imposto la riga dei tasti funzione
095680131212       ExSr Sr_Tastifun;
095690131212      /end-free
095700001025
095710001025     C                   endsr
095720001103      **********************************************************************
095730001103      * F19 - Proposte
095740001103      **********************************************************************
095750001103     C     Sr_Proposte   BEGSR
095760001103
095770001103     C                   movel     'P'           psce
095780001103     C                   movel     kpjbu         §kpjbu
095790001103     C                   clear                   kpjbu
095800001103     C                   movel     parm01        kpjbu
095810001103     C                   call      'FIOR07R'
095820001103     C                   parm                    kpjba
095830001103     C                   movel     §kpjbu        kpjbu
095840001103
095850001103     C                   endsr
095860001102      **********************************************************************
095870001102      * F20 - Simulazione delivery-tassazione
095880001102      **********************************************************************
095890001102     C     Sr_Simula     BEGSR
095900001116
095910001116     C                   clear                   TISI92DS
095920001116     C                   movel     'ORM'         D92op0
095930001116     C                   movel     v1cpor        D92lnp
095940001116     C                   movel     v1nac         D92nar
095950001116     C                   movel     v1cac         D92cad
095960030922     C                   movel(p)  sav_v1loc35   D92lod
095970001116     C                   movel     v1ffd         D92ffd
095980001116     C                   movel     v1ksc         D92ksc
095990001116     C                   movel     v1ctr         D92ctr
096000001116     C                   z-add     v1ncl         D92ncl
096010001116     C                   z-add     v1pkg         D92pkb
096020001116     C                   z-add     v1vlm         D92vlf
096030001116     C                   movel     v1ddt         D92ddt
096040001102
096050001102     C                   movel     kpjbu         §kpjbu
096060001116     C                   movel(p)  tisi92ds      kpjbu
096070001102     C                   call      'TISI92R'
096080001102     C                   parm                    kpjba
096090001102     C                   movel     §kpjbu        kpjbu
096100001102
096110001102     C                   endsr
096120061023
096130061023      *--------------------------------------------------------------------*
096140061023      * F2 - Dati DPD
096150061023      *--------------------------------------------------------------------*
096160061023     c     sr_datidpd    begsr
096170061023
096180080430     c                   clear                   fieu55ds
096190080430
096200080430      * se ORM commissionato da DPD passo il n. ORM DPD
096210080430     c     ormpoe        lookup    skpodpd                                30
096220080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
096230080430     c                   eval      i55dep = %subst(ormrfa:4:4)
096240080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
096250081015     c                   eval      i55ie = 'I'
096260080430     c                   eval      kpjbu = fieu55ds
096270080430     c                   call      'FIEU55C'
096280080430     c                   parm                    kpjba
096290080430     c                   endif
096300080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
096310080430     c     ormpor        lookup    skpodpd                                30
096320080430     c                   if        *in30
096330061023     c                   eval      i55poe = v1cpoe
096340061023     c                   eval      i55nrs = v1nsr
096350061023     c                   eval      i55nro = v1nor
096360061023     c                   eval      i55nrv = v1nrv
096370081015     c                   eval      i55ie = 'E'
096380061023     c                   eval      kpjbu = fieu55ds
096390061206     c                   call      'FIEU55C'
096400061023     c                   parm                    kpjba
096410080430     c                   endif
096420061023
096430061023     c                   endsr
096440061023
096450050422      *--------------------------------------------------------------------*
096460050420      * CONTROLLO PROPOSTE
096470050422      *--------------------------------------------------------------------*
096480050420     c     Sr_ContrProp  BegSr
096490050420
096500050420     c                   Eval      wproposte = *Off
096510050420     c                   Movel     v1cpoe        kpoe
096520050420     c                   Movel     v1nsr         knsr
096530050420     c                   Movel     v1nor         knor
096540050420     c                   Movel     v1nrv         knrv
096550050420
096560050420     c     kfnor         Setll     Fnorp01l
096570050420     c                   Do        *Hival
096580140227     c     kfnor         Reade(n)  Fnorp01l
096590050420     c                   If        %Eof(Fnorp01l)
096600050420     c                   Leave
096610050420     c                   EndIf
096620050420     c                   If        OrpFev <> *Blanks
096630050420     c                   Iter
096640050420     c                   EndIf
096650050420     c                   Eval      wproposte = *On
096660050420     c                   Leave
096670050420     c                   EndDo
096680050420
096690050420     c                   EndSr
096700040419      **********************************************************************
096710040419      * F21 - Dirottamento
096720040419      **********************************************************************
096730040419     c     Sr_Dirotta    BegSr
096740040419
096750040419     c                   Clear                   wdcpor
096760040419     c                   Clear                   wddpor
096770040419     c                   Move      v1cpor        sav_wdcpor
096780040420     c                   Eval      wdcdar = v1dar
096790040419     c                   Do        *Hival
096800040419     c                   Exfmt     Fior05dw
096810040420     c                   Eval      *In28 = *Off
096820040419      * F12=Ritorno
096830040419     c   kl              Leave
096840040419     c                   ExSr      Sr_ContrWd
096850040420     c   28              Iter
096860040420      * F6=Conferma
096870040420     c                   If        *InKf
096880040615     c                   movel     wdcpor        v1cpor
096890040615     c                   movel     wdcdar        v1dar
096900040615     c                   Eval      v1fao = 050
096910050422     c                   Eval      v1dfo = oggi_aammgg
096920040615     c                   Time                    v1ofo
096930170411      * devo pulire anche i dati relativi al giro
096940170411     c                   clear                   V1Ccgi
096950170411     c                   clear                   V1tgi
096960170411     c                   clear                   sav_V1Ccgi
096970040615     c                   Eval      wokdirotta = *On
096980040420     c                   Leave
096990040420     c                   EndIf
097000040419     c                   EndDo
097010040419
097020040419     c                   EndSr
097030090205
097040090205      *--------------------------------------------------------------------*
097050090205      * Window per immissione dati appuntamento ORM
097060090205      *--------------------------------------------------------------------*
097070090205     c     sr_ormapp     begsr
097080090205
097090090206     c                   eval      *in90 = *off
097100090206     c                   eval      *in93 = *off
097110090206     c                   eval      *in94 = *off
097120090205     c                   do        *hival
097130090206     c                   exfmt     fior05app
097140090206     c                   eval      *in28 = *off
097150090206     c                   eval      *in93 = *off
097160090206     c                   eval      *in94 = *off
097170090206     c                   clear                   wappmsg
097180090206      * f12-ritorno
097190090206     c                   if        *inkl
097200090206     c                   eval      *in90 = *on
097210090206     c                   leave
097220090206     c                   endif
097230090206      * controllo
097240090206     c                   if        wappref = *blanks
097250090206     c                   eval      wappmsg = 'Inserire la persona da -
097260090206     c                             contattare'
097270090206     c                   eval      *in28 = *on
097280090206     c                   eval      *in93 = *on
097290090206     c                   iter
097300090206     c                   endif
097310090206      * almeno 5 caratteri
097320090206     c                   if        %subst(wappref:4:31) = *blanks
097330090206     c                   eval      wappmsg = 'Completare la persona -
097340090206     c                             da contattare'
097350090206     c                   eval      *in28 = *on
097360090206     c                   eval      *in93 = *on
097370090206     c                   iter
097380090206     c                   endif
097390090206      * controllo
097400090206     c                   if        wappnota1 = *blanks
097410090206     c                   eval      wappmsg = 'Inserire le annotazioni'
097420090206     c                   eval      *in28 = *on
097430090206     c                   eval      *in94 = *on
097440090206     c                   iter
097450090206     c                   endif
097460090206      * almeno 5 caratteri
097470090206     c                   if        %subst(wappnota1:4:31) = *blanks
097480090206     c                   eval      wappmsg = 'Completare le -
097490090206     c                             annotazioni'
097500090206     c                   eval      *in28 = *on
097510090206     c                   eval      *in94 = *on
097520090206     c                   iter
097530090206     c                   endif
097540090206      * f6-conferma
097550090206     c   kf              leave
097560090205     c                   enddo
097570090205
097580090205     c                   endsr
097590090205
097600040419      **********************************************************************
097610040419      * Controllo i dati per il dirottamento
097620040419      **********************************************************************
097630040419     c     Sr_ContrWd    BegSr
097640040419
097650040419      * --> P.O. ritiro nuovo
097660040419     c                   Clear                   wddpor
097670040419      * Obbligatorio
097680040419     c                   If        wdcpor = *Zeros
097690040419     c                   Eval      wdcmsg = msg(11)
097700070124     c                   Seton                                        28
097710040419     c                   Goto      endctrwd
097720040419     c                   EndIf
097730040419      * Non può essere lo stesso di prima
097740040419     c                   If        wdcpor = sav_wdcpor
097750040419     c                   Eval      wdcmsg = msg(78)
097760070124     c                   Seton                                        28
097770040419     c                   Goto      endctrwd
097780040419     c                   EndIf
097790040419      * Deve esistere
097800040419     c     wdcpor        Chain     Azorg01L
097810040419     c                   If        Not %Found(Azorg01l)
097820040419     c                   Eval      wdcmsg = msg(33)
097830070124     c                   Seton                                        28
097840040419     c                   Goto      endctrwd
097850040419     c                   EndIf
097860040419      * Non deve essere annullato
097870040419     c                   If        OrgFva <> *Blanks  or
097880040419     c                             (OrgFag <> 'A' and OrgFag <> 'F')
097890040419     c                   Eval      wdcmsg = msg(33)
097900070124     c                   Seton                                        28
097910040419     c                   Goto      endctrwd
097920040419     c                   EndIf
097930040419      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
097940040419     c                   Eval      og148 = OrgDe8
097950040419     c                   If        §ogorm <> 'S'
097960040628     c                   Eval      wdcmsg = msg(79)
097970070124     c                   Seton                                        28
097980040419     c                   Goto      endctrwd
097990040419     c                   EndIf
098000040419      * Decodifico
098010040419     c                   Eval      wddpor = OrgDes
098020070124
098030070124      * se il p.o. che dirotta è abilitato al PDA
098040070124      * e fase 400 o 410
098050071210     c                   if        *in22 and
098060070124     c                             (ormfao = 400 or ormfao = 410)
098070070124      * devo avvisare che ho già avuto un esito da AUT
098080070124      * se l'orm è in fase 410
098090070124     c                   if        ormfao = 410 and wforzadir = *off
098100070124     c                   eval      wdcmsg = msg(41)
098110070124     c                   seton                                        28
098120070124     c                   eval      wforzadir = *on
098130070124     c                   goto      endctrwd
098140070124     c                   endif
098150070124      * in ogni caso devo salvarmi la distinta per passarla al pgm che aggiorna
098160070124      * fipdo
098170070124     c                   eval      savorffgs = orffgs
098180070124     c                   eval      savorfndc = orfndc
098190070125     c                   eval      savorfddc = orfddc
098200070125      * mi salvo anche la fase che mi servirà poi nel caso di ORM dirottato
098210070125     c                   eval      savorffar = orffar
098220070124     c                   endif
098230040419
098240040615      * è sempre un orm commissionato controllo se ok tutti i dati
098250130920      * ma solo se era già un ORM COMMISSIONATO
098260130920     c                   IF        §ORcom = 'S'
098270040615     c                   Eval      V1com = 'S'
098280130918     c                   ENDIF
098290040419
098300040419     c     endctrwd      EndSr
098310001011      **********************************************************************
098320001023      * PRENDE IL NUOVO NUMERO ORDINE
098330001011      **********************************************************************
098340001011     C     Sr_Numor      BEGSR
098350001011
098360010222     C                   if        v1cpoe = *zeros
098370150305     C                   z-add     DUTpou        v1cpoe
098380010222     C                   endif
098390030922
098400030922     c                   clear                   TRUL33DS
098410030922     c                   Eval      I33cnu = 300
098420030922     c                   Eval      I33po1 = V1cpoe
098430030922     c                   Eval      I33num = 1
098440030922     c                   movel(p)  TRUL33DS      KPJBU
098450030922     c                   call      'TRUL33R'
098460030922     c                   parm                    KPJBA
098470030922     c                   movel     KPJBU         TRUL33DS
098480030922
098490030922    1c                   Select
098500030922      * Numeratore non trovato
098510030922     C                   when      O33err =  01
098520030922     C                   eval      *in28 = *on
098530030922     C                   movel     msg(72)       v1cmsg
098540030922     C                   goto      endsrnumor
098550030922      * Numeratore allocato
098560030922     C                   when      O33err =  02
098570030922     C                   eval      *in28 = *on
098580030922     C                   movel     msg(71)       v1cmsg
098590030922     C                   goto      endsrnumor
098600030922      * altro errore
098610030922     C                   when      O33err <> *zeros
098620030922     C                   eval      *in28 = *on
098630030922     C                   movel     O33msg        v1cmsg
098640030922     C                   goto      endsrnumor
098650030922    1C                   endsl
098660030922
098670030922     C                   z-add     O33nrf        v1nor
098680001011
098690010507     C     endsrnumor    endsr
098700070119
098710070119      *--------------------------------------------------------------------*
098720070119      * Carico dati sensibili per PDA modificati
098730070119      *--------------------------------------------------------------------*
098740070119     c     sr_datipdamod begsr
098750070119
098760070119     c                   clear                   datipdamod
098770070119     c                   eval      pdamodrsr = v1rsr
098780070119     c                   eval      pdamodinr = v1inr
098790070119     c                   eval      pdamodlor = v1lor
098800070119     c                   eval      pdamodprr = v1prr
098810070119     c                   eval      pdamodrer = v1rer
098820070119     c                   eval      pdamodter = v1ter
098830070119     c                   eval      pdamodorr = v1orr
098840070119     c                   eval      pdamodncl = v1ncl
098850070119     c                   eval      pdamodpkg = v1pkg
098860070119     c                   eval      pdamodvlm = v1vlm
098870070119     c                   eval      pdamodbnc = v1bnc
098880070119     c                   eval      pdamodno1 = v1not1
098890070119     c                   eval      pdamodno2 = v1not2
098900131107      /free
098910140123        pdamodora1 = V1OraAMda;
098920140123        pdamodora2 = V1OraAMa;
098930140123        pdamodora3 = V1OraPMda;
098940140123        pdamodora4 = V1OraPMa;
098950131107      /end-free
098960070119
098970070119     c                   endsr
098980070119
098990001011      **********************************************************************
099000001026      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
099010001011      **********************************************************************
099020001011     C     Sr_Carfile    BEGSR
099030001011
099040010116     C                   eval      ORMatb = v1atb
099050010116     C                   eval      ORMpoe = v1cpoe
099060010116     C                   eval      ORMnsr = v1nsr
099070010116     C                   eval      ORMnor = v1nor
099080010116     C                   eval      ORMnrv = v1nrv
099090010116     C                   eval      ORMtor = v1ctor
099100010116     C                   eval      ORMtco = v1ctco
099110010116      * Inversione data emissione ORM
099120010116     C                   clear                   wlbdat
099130010116     C                   z-add     v1dao         G02dat
099140010116     C                   movel     *blanks       G02err
099150010116     C                   call      'XSRDA8'
099160010116     C                   parm                    wlbdat
099170010116     C                   z-add     G02inv        ORMdao
099180010116
099190131211     C                   eval      ORMoao = v1oao
099200010116
099210010116     C                   eval      ds_v1cor1 = v1cor1
099220010116     C                   eval      ds_v1cor2 = v1cor2
099230010116     C                   eval      ds_v1cor3 = v1cor3
099240010116     C                   eval      ORMcor = ds_v1ccor
099250010116     C                   eval      ORMrso = v1rso
099260010116     C                   eval      ORMino = v1ino
099270010116     C                   eval      ORMcao = v1cao
099280030922     C                   movel(p)  sav_v1loo35   ORMloo
099290010116     C                   eval      ORMpro = v1pro
099300010116     C                   eval      ORMnao = v1nao
099310010116
099320010116     C                   eval      ds_v1cra1 = v1cra1
099330010116     C                   eval      ds_v1cra2 = v1cra2
099340010116     C                   eval      ds_v1cra3 = v1cra3
099350010116     C                   eval      ORMcra = ds_v1ccra
099360010116     C                   eval      ORMrsr = v1rsr
099370010116     C                   eval      ORMinr = v1inr
099380010116     C                   eval      ORMcar = v1car
099390001103     C                   movel(p)  v1lor         ORMlor
099400010116     C                   eval      ORMprr = v1prr
099410010116     C                   eval      ORMnar = v1nar
099420010116     C                   eval      ORMrer = v1rer
099430010116     C                   eval      ORMter = v1ter
099440010116
099450010116     C                   eval      ds_v1crc1 = v1crc1
099460010116     C                   eval      ds_v1crc2 = v1crc2
099470010116     C                   eval      ds_v1crc3 = v1crc3
099480010116     C                   eval      ORMcrc = ds_v1ccrc
099490010116     C                   eval      ORMrsc = v1rsc
099500010116     C                   eval      ORMinc = v1inc
099510030922     C                   movel(p)  sav_v1loc35   ORMloc
099520010116     C                   eval      ORMcac = v1cac
099530010116     C                   eval      ORMprc = v1prc
099540010116     C                   eval      ORmnac = v1nac
099550001023      * Inversione data ritiro
099560001017     C                   clear                   wlbdat
099570001017     C                   z-add     v1dar         G02dat
099580001017     C                   movel     *blanks       G02err
099590001017     C                   call      'XSRDA8'
099600001017     C                   parm                    wlbdat
099610001025     C                   z-add     G02inv        ORMdar
099620010118     C                   eval      ORMorr = v1orr
099630010116
099640010116     C                   eval      ORMffd = v1ffd
099650010116     C                   eval      ORMnam = v1nam
099660010116     C                   eval      ORMncl = v1ncl
099670010116     C                   eval      ORMpkg = v1pkg
099680010116     C                   eval      ORMvlm = v1vlm
099690010116     C                   eval      ORMbnc = v1bnc
099700010116     C                   eval      ORMblc = v1blc
099710010116     C                   eval      ORMatt = v1att
099720010116     C                   eval      ORMmtc = v1mtc
099730010116     C                   eval      ORMddt = v1ddt
099740140331      * se codice ordinante = codice mittente e impostato che paga l'Ordinante
099750140331      * in automatico metto il 'Mittente' in questo modo elimino un msg di errore
099760140331     c                   IF        V1pag = 'O' and
099770140331     c                             ds_V1cCor = ds_V1cCra and ds_V1cCra > *Zeros
099780140331     c                   eval      v1pag = 'M'
099790140331     c                   ENDIF
099800140331
099810010116     C                   eval      ORMpag = v1pag
099820010116     C                   eval      ORMksc = v1ksc
099830010202     C                   eval      ORMctr = v1ctr
099840001017     C                   movel     v1cpor        ORMpor
099850001016     C                   movel     v1cpoc        ORMpoc
099860010116     C                   move      v1sto         ORMsto
099870010116
099880010116     C                   eval      ORMpos = v1cpos
099890010116     C                   eval      ORMors = v1ors
099900010116     C                   eval      ORMdst = v1dst
099910010116     C                   eval      ORMno1 = v1not1
099920010116     C                   eval      ORMno2 = v1not2
099930010116     C                   eval      ORMrfa = v1rfa
099940010712
099950010116     C                   eval      ORMcst = v1cst
099960010116     C                   eval      ORMvcs = v1vcs
099970010116     C                   eval      ORMfcs = v1fcs
099980050316      * se ORM commissionato e sono in immissione l'ORM deve essere creato in fase
099990050316      * 50 mentre se non è commissionato subito in fase 100
100000081211      * se ORM export DPD sempre in fase 50
100010050316     c                   If        *In01 or *In03
100020081127     c                   If        v1com = 'S' or *in15
100030050316     c                   Eval      v1fao = 50
100040050316     c                   Else
100050050316     c                   Eval      v1fao = 100
100060050316     c                   EndIf
100070151118      /free
100080151118       //?Se ORM da internet ed è un prepagato locale
100090151118       //?lo immetto subito in fase 100 anche se è commissionato
100100151118       //?la telefonata al mittente viene fatta al momento della conferma ORM
100110151118         IF  §RMfpr = 'C' and ORMtco = 'I' and ORMtor = 'P' and
100120151118             ORMpoe = ORMpor;
100130151118           V1fao = 100;
100140151118         ENDIF;
100150140428       //?Alert da inviare
100160140422         IF  walert;
100170140428         //?Se ora ritiro a 0 imposto inizio servizio o l'ora "tappo"
100180140422           IF  ORMorr = 0 and OOR2stis > 0;
100190140422             ORMorr = OOR2stis;
100200140422           ENDIF;
100210140422           IF  ORMorr = 0;
100220140422             ORMorr = d§DFTorrtp;
100230140422           ENDIF;
100240140422         //?Se ora telefono non impostato metto il cell.x sms
100250140422           IF  ORMter = *blanks;
100260140422             ORMter = W03sms;
100270140422           ENDIF;
100280140422         ENDIF;
100290140422      /end-free
100300070912      * devo aggiornare il flag di ORM assegnabile in base alla fase
100310070912     c                   exsr      sr_contrfao
100320050316     c                   EndIf
100330140429
100340140109     C                   eval      ORMdfo = v1dfo
100350140109     C                   eval      ORMofo = v1ofo
100360010116     C                   eval      ORMfao = v1fao
100370010116     C                   eval      ORMnpg = v1npg
100380010116     C                   eval      ORMndc = v1ndc
100390010116     C                   eval      ORMddc = v1ddc
100400010622     C                   eval      ORMstp = v1stp
100410010622     C   10              clear                   ORMnpg
100420010622     C   10              clear                   ORMndc
100430010622     C   10              clear                   ORMddc
100440010622     C   10              clear                   ORMtap
100450010116     C                   eval      ORMtap = v1tap
100460010116     C                   eval      ORMeti = v1eti
100470010119     C                   eval      ORMspi = v1spi
100480011114      * Se forzato blocco destinatario devo pulire il relativo flag
100490011114     C                   if        §ormfd = 'S'
100500011119     C                   if        v1ctor <> 'S'
100510011114     C                   clear                   §ormfd
100520011119     C                   else
100530070219     C                   if        v1rsc = *blanks and ds_v1ccrc = *zeros
100540011119     C                   clear                   §ormfd
100550011114     C                   endif
100560011119     C                   endif
100570011114     C                   endif
100580030606      * ORM commissionato
100590131106      /free
100600131106       //?Se il campo è vuoto imposto 'N' in questo modo in immissione fino a che
100610131106       //?non viene forzato dall'utente il pgm calcola se ORM commissionato o meno
100620131106       //?mentre nel file ho solo 'N' o 'S' (da ora in poi)
100630140424       //?gli unici ORM con il campo a blank sono gli ORM con serie (RC) scritti
100640140424       //?automaticamente dal traduttore
100650140424       //?e gli ORM commissionati da DPD chiusi in automatico da DPD
100660131106         IF  V1com = *blanks;
100670131106           V1com = 'N';
100680131106         ENDIF;
100690131106      /end-free
100700030606     c                   Eval      §orcom = v1com
100710030731
100720030731      * in base al tipo orm e a chi paga imposto il campo §orpfb
100730030731      * imposto 'F' se paga mittente per orm singolo o prepagato
100740030731      * imposto 'A' se NON paga mittente
100750030731     c                   Clear                   §orPfb
100760030731     c                   Select
100770030731     c                   When      OrmPag = 'M'
100780030731     c                   If        OrmTor = 'P' or OrmTor = 'S'
100790030731     c                   Eval      §orPfb = 'F'
100800030731     c                   EndIf
100810030731     c                   Other
100820030731     c                   Eval      §orPfb = 'A'
100830030731     c                   EndSl
100840090403
100850090403      * in immissione ORM
100860090403      * se orm da internet o manuale verifico se impostare il §ormks
100870100913      * solo però se il campo non è già stato impostato da tabella PVO
100880090403     c                   if         *in01 and
100890141211     c                             (ormtco = 'I' or ormtco = 'M' or
100900141212     c                              ORMtco = 'E')
100910100913     c                             and (not ok_pvo or
100920100913     c                                 (ok_pvo and d§pvoks = ' '))
100930090403     c                   exsr      sr_vac
100940090403     c                   endif
100950090403
100960090403      * imposto flag operativi
100970030731     C                   eval      ORMflo = dorm01
100980161115      /free
100990161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
101000161115         //?ipotetico ritiro da effettuare al mattino
101010161115         IF  Orariestesi;
101020161115           ORMrmp = 'S';
101030161115         ELSE;
101040161115           clear ORMrmp;
101050161115         ENDIF;
101060161116         //?Se ORM da Internet non lo memorizzo
101070161116         IF  ORMtco = 'I';
101080161116           clear ORMrmp;
101090161116         ENDIF;
101100161116
101110161115      /end-free
101120010228
101130040908     c                   Eval      OrmDtt = oggi_aammgg
101140001204      * Mette sempre in positivo i campi delle quantità
101150001204     C                   mllzo     1             ORMorr
101160001204     C                   mllzo     1             ORMncl
101170001204     C                   mllzo     1             ORMpkg
101180001204     C                   mllzo     1             ORMvlm
101190001204     C                   mllzo     1             ORMbnc
101200001204     C                   mllzo     1             ORMblc
101210001204     C                   mllzo     1             ORMatt
101220001204     C                   mllzo     1             ORMmtc
101230001109      * Dati di copia
101240001109     C                   if        *in03 = *on
101250001109     C                   z-add     knrv          ORMnrv
101260050316      * se ORM commissionato l'ORM deve essere creato in fase
101270050316      * 50 mentre se non è commissionato subito in fase 100
101280081211      * se ORM export DPD sempre in fase 50
101290081211     c                   If        v1com = 'S' or *in15
101300050316     c                   Eval      ORMfao = 50
101310050316     c                   Else
101320050316     c                   Eval      ORMfao = 100
101330050316     c                   EndIf
101340010118     C                   eval      ORMdfo = oggi_aammgg
101350010118     C                   movel     w0140         ORMofo
101360001130     C                   clear                   ORMnpg
101370001129     C                   z-add     *zeros        ORMndc
101380001129     C                   z-add     *zeros        ORMddc
101390001129     C                   movel     *blanks       ORMtap
101400010223     C                   z-add     *zeros        ORMdst
101410010209      * Aggiorno anche i dati che sono a video per poter scrivere la fase
101420010209     C                   movel     ORMnrv        v1nrv
101430010209     C                   movel     ORMfao        v1fao
101440010209     C                   movel     ORMdfo        v1dfo
101450010209     C                   movel     ORMofo        v1ofo
101460070706      * devo aggiornare il flag di ORM assegnabile in base alla fase
101470070706     c                   exsr      sr_contrfao
101480070706     c                   eval      ormeti = v1eti
101490001109     C                   endif
101500131126
101510131126      /free
101520131126       //?Immissione/Copia ORM
101530131126         IF  *in01 or *in03;
101540131126       //?imposto ora cut-off nel campo Stop
101550140318           ORMstp = OOR2lrnc;
101560160526       //?in teoria se ritiro estero dovrei mettere il nuovo limite per orm esteri
101570160526       //?ma dato che il campo ORMSTP non è usato non lo faccio
101580131126         ENDIF;
101590131126      /end-free
101600001017
101610001017     C                   endsr
101620090403
101630090403      *--------------------------------------------------------------------*
101640090403      * Se conferma orm da VAO controllo se impostare il ritorno VAC
101650090403      *--------------------------------------------------------------------*
101660090403     c     sr_vac        begsr
101670090403
101680090403      * se paga Ordinante
101690090403      * se ho il KSC
101700090403     c                   if        v1pag = 'O' and
101710090403     c                             v1ksc <> *zeros
101720090403      * in tabella 3K trovo il cliente e non devo escludere
101730090403     c                   clear                   ds3k
101740090403     c                   eval      kcod = '3K'
101750090403     c                   movel(p)  v1ksc         kkey
101760090403     c     ktab          chain     tabel00f
101770090403     c                   if        %found(tabel00f) and tblflg = *blanks
101780090403     c                   eval      ds3k = tbluni
101790090403      * ritorno VAC x triangolazione ORM
101800090403     c                   if        §3kvto = 'S'
101810090403      * imposto il falg per ritorno VAC
101820090403     c                   eval      §ormks = 'S'
101830100812     c                   leavesr
101840090403     c                   endif
101850090403     c                   endif
101860100812
101870100812      * se non ho trovato con 3K provo con 3Q
101880100812     c                   clear                   ds3q
101890100812     c                   eval      kcod = '3Q'
101900100812     c                   movel(p)  v1ksc         kkey
101910100812     c     ktab          chain     tabel00f
101920100812     c                   if        %found(tabel00f) and tblflg = *blanks
101930100812     c                   eval      ds3Q = tbluni
101940100812      * ritorno VAB x triangolazione ORM
101950100812     c                   if        §3Qvto = 'S'
101960100812      * imposto il flag per ritorno VAB
101970100812     c                   eval      §ormks = 'S'
101980100812     c                   endif
101990100812     c                   endif
102000100812
102010090403     c                   endif
102020090403
102030090403     c                   endsr
102040140122
102050140122      /free
102060140122       //--------------------------------------------------------------------
102070140122       //?Controllo i dati variati.
102080140122       //--------------------------------------------------------------------
102090140122       BEGSR ctrvardati;
102100140122
102110140122         clear wVardati;
102120140122
102130140122       //?Variazione dati mittente ==> M
102140140122         IF  pdasavrsr <> pdamodrsr or
102150140122             pdasavinr <> pdamodinr or
102160140122             pdasavlor <> pdamodlor or
102170140122             pdasavprr <> pdamodprr or
102180140122             pdasavrer <> pdamodrer or
102190140122             pdasavter <> pdamodter;
102200140122           wVardati = %trim(wVardati) + 'M';
102210140122         ENDIF;
102220140122       //?Variazione dati ora pronta merce ==> O
102230140122         IF  pdasavorr <> pdamodorr or
102240140122             pdasavora1 <> pdamodora1 or
102250140122             pdasavora2 <> pdamodora2 or
102260140122             pdasavora3 <> pdamodora3 or
102270140122             pdasavora4 <> pdamodora4;
102280140122           wVardati = %trim(wVardati) + 'O';
102290140122         ENDIF;
102300140122       //?Variazione dati quantità ==> Q
102310140122         IF  pdasavncl <> pdamodncl or
102320140122             pdasavpkg <> pdamodpkg or
102330140122             pdasavvlm <> pdamodvlm or
102340140122             pdasavbnc <> pdamodbnc;
102350140122           wVardati = %trim(wVardati) + 'Q';
102360140122         ENDIF;
102370140124       //?Variazione dati note ==> N
102380140122         IF  pdasavno1 <> pdamodno1 or
102390140122             pdasavno2 <> pdamodno2;
102400140124           wVardati = %trim(wVardati) + 'N';
102410140122         ENDIF;
102420140122
102430140122       ENDSR;
102440140122
102450140122      /end-free
102460070119
102470070119      *--------------------------------------------------------------------*
102480070119      * Aggiorno file PDO
102490070119      *--------------------------------------------------------------------*
102500070119     c     sr_aggpdo     begsr
102510070301
102520140122      * Richiamo sempre il FIOR56R, ci pensa poi lui a decidere se scrivere i dati
102530140122      * per il PDA e/o la telefonata AUT
102540070123
102550120612      * devo aggiornare FIPDO
102560070228     c                   eval      or56poe = ormpoe
102570070228     c                   eval      or56nsr = ormnsr
102580070228     c                   eval      or56nor = ormnor
102590070228     c                   eval      or56nrv = ormnrv
102600070228     c                   call      'FIOR56R'
102610140122     c                   parm                    kpjba
102620070228     c                   parm                    fior56ds
102630070228      * il pgm torna degli errori, ma dato che ho già fatto qua tutti i controlli
102640070228      * del caso, non me ne preoccupo
102650070119
102660070119     c                   endsr
102670070119
102680001017      **********************************************************************
102690001026      * CARICA IL FILE FNORF DAL FILE FNORM
102700001017      **********************************************************************
102710001017     C     Sr_Carfilef   BEGSR
102720001011
102730081125     c                   clear                   dorf01
102740001019     C                   clear                   fnorf000
102750010116     C                   eval      ORFpoe = v1cpoe
102760010116     C                   eval      ORFnsr = v1nsr
102770010116     C                   eval      ORFnor = v1nor
102780050322     C                   eval      ORFnrv = v1nrv
102790050322      * sono in conferma orm commissioni e sto dirottando l'orm imposto il p.o. ritiro
102800150305      * come po fase
102810050322     c                   If        §rmfpr = 'O' and wokdirotta = *On
102820050322     c                   movel     v1cpor        orfpog
102830050322     c                   Else
102840150305     C                   eval      ORFpog = DUTpou
102850050322     c                   EndIf
102860010116     C                   eval      ORFdae = v1dfo
102870010116     C                   eval      ORFore = v1ofo
102880010116     C                   eval      ORFfar = v1fao
102890001019     C                   eval      ORFpue = knmus
102900010202     C                   eval      ORFcar = v3cau
102910080523      * se sono in conferma proposta di chiusura ORM e l'orm non ha la distinta non la riporto dalla
102920080523      * conferma chiusura perchè ormai l'ORM non è più assegnato
102930080523      * caso di orm assegnato, proposta di chiusura, disassegnato, accetto porposta di chiusura
102940080523     c                   select
102950080523     c                   when      §rmfpr = 'P' and *in10 and ormndc = 0
102960080523     c                   clear                   orffgs
102970080523     c                   clear                   orfndc
102980080523     c                   clear                   orfddc
102990080523      * se c'è la distinta la riporto sulla fase
103000080523     c                   when      OrmNdc > *Zeros
103010040909     c                   Eval      OrfFgs = OrmPor
103020040909     c                   Eval      OrfNdc = OrmNdc
103030040909     c                   Eval      OrfDdc = OrmDdc
103040080523      * se non c'è la distinta riporto sulla fase quella salvata in precedenza
103050080523     c                   when      savndc > *Zeros
103060060516     c                   eval      orffgs = savfgs
103070041014     c                   Eval      OrfNdc = savndc
103080041014     c                   Eval      OrfDdc = savddc
103090080523     c                   endsl
103100081125      * imposto se non ho fatto l'ulteriore addebito
103110081125     c                   if        wnobolla = *on
103120081125     c                   eval      §orfadd = 'S'
103130081125     c                   endif
103140081125     c                   eval      orfflo = dorf01
103150001109
103160001019     C                   write     fnorf000
103170001017
103180001011     C                   endsr
103190001026      **********************************************************************
103200001026      * CARICA IL FILE DELLE NOTE FNORN RICHIAMANDO IL PROGRAMMA
103210001026      **********************************************************************
103220001026     C     Sr_Carnote    BEGSR
103230001026
103240001026     C                   if        *in01 = *on or *in02 = *on
103250001109     C                             or *in03 = *on
103260001026     C                   clear                   fior06ds
103270001026     C                   movel     'C'           i06flm
103280010118     C                   eval      i06dta = ORMdao
103290010118     C                   eval      i06poe = ORMpoe
103300010118     C                   eval      i06nor = ORMnor
103310010118     C                   eval      i06nsr = ORMnsr
103320010118     C                   eval      i06nrv = ORMnrv
103330010118     C                   eval      i06far = ORMfao
103340010301     C                   eval      i06dai = oggi_aammgg
103350010301     C                   movel     w0140         I06ori
103360001026
103370010405     C                   movel(p)  kpjbu         §kpjbu
103380010405     C                   movel(p)  parm01        kpjbu
103390001026     C                   call      'FIOR06R'
103400001026     C                   parm                    kpjba
103410001027     C                   parm                    fior06ds
103420010405     C                   movel     §kpjbu        kpjbu
103430001026
103440001026     C                   endif
103450001026
103460001026     C                   endsr
103470070903
103480070706      *--------------------------------------------------------------------*
103490151016      * File FNORG00F - Scrivo
103500070706      *--------------------------------------------------------------------*
103510151016     c     sr_wrtorg     begsr
103520151016
103530151016     c                   clear                   fnorg000
103540151016     c                   eval      orgpoe = ormpoe
103550151016     c                   eval      orgnsr = ormnsr
103560151016     c                   eval      orgnor = ormnor
103570151016     c                   eval      orgnrv = ormnrv
103580151016     c                   eval      orgpor = ormpor
103590151016     c                   eval      orgpkg = wpkg
103600151016     c                   eval      orgvlm = wvlm
103610151016      * giro immesso
103620151016     c                   if        v1ccgi <> *blanks
103630151016     c                   eval      orgcgi = v1ccgi
103640151016     c                   eval      orgtgi = v1tgi
103650151016     c                   eval      orgpocgi = num_v1cpor
103660151016     c                   time                    w0140
103670151016     c                   movel     w0140         orghvtel
103680151016     c                   move      w0140         orgdtvtel
103690151016     c     *eur          move      orgdtvtel     dataeur
103700151016     c                   move      dataeur       dataiso
103710151016     c                   move      dataiso       orgdtvtel
103720151016     c                   endif
103730151016      * scrivo
103740151016     c                   write     fnorg000
103750151016
103760151016     c                   endsr
103770151016
103780151016      *--------------------------------------------------------------------*
103790151016      * File FNORG00F - Aggiorno
103800151016      *--------------------------------------------------------------------*
103810151016     c     sr_aggorg     begsr
103820070706
103830070706     c                   eval      orgpor = ormpor
103840070706     c                   eval      orgpkg = wpkg
103850070706     c                   eval      orgvlm = wvlm
103860071031     c                   select
103870071031      * non c'è il giro ma prima c'era
103880071031     c                   when      v1ccgi = *blanks and orgcgi <> *blanks
103890071031     c                   clear                   orgcgi
103900071031     c                   clear                   orgtgi
103910071031     c                   clear                   orgpocgi
103920071031     c                   time                    w0140
103930071031     c                   movel     w0140         orghvtel
103940071031     c                   move      w0140         orgdtvtel
103950071031     c     *eur          move      orgdtvtel     dataeur
103960071031     c                   move      dataeur       dataiso
103970071031     c                   move      dataiso       orgdtvtel
103980071031      * non c'è il giro
103990071031     c                   when      v1ccgi = *blanks
104000071031     c                   clear                   orgcgi
104010071031     c                   clear                   orgtgi
104020071031     c                   clear                   orgpocgi
104030071031     c                   clear                   orghvtel
104040071031     c                   clear                   orgdtvtel
104050071031      * giro variato
104060080305      * o stesso giro ma dirottamento (caso di stesso giro codificato su due filiali diverse)
104070071031     c                   when      orgcgi <> v1ccgi or orgtgi <> v1tgi
104080080305     c                             or (orgcgi = v1ccgi and wokdirotta = *on)
104090070919     c                   eval      orgcgi = v1ccgi
104100070919     c                   eval      orgtgi = v1tgi
104110071023     c                   eval      orgpocgi = num_v1cpor
104120071003     c                   time                    w0140
104130071003     c                   movel     w0140         orghvtel
104140071003     c                   move      w0140         orgdtvtel
104150071003     c     *eur          move      orgdtvtel     dataeur
104160071003     c                   move      dataeur       dataiso
104170071003     c                   move      dataiso       orgdtvtel
104180071031     c                   endsl
104190080130      * se è un dirottamento devo pulire i dati relativi a NFTL e SLO
104200080130      * che potrebbero essere rimasti sporchi da un'assegnazione automatica
104210080130      * fatta in precedenza
104220080211      * pulisco anche quello che riguarda la distinta
104230080130     c                   if        wokdirotta = *on
104240080211     c                   clear                   orgpdc
104250080211     c                   clear                   orgfgs
104260080211     c                   clear                   orgndc
104270080211     c                   clear                   orgddc
104280080130     c                   clear                   orgnftl
104290080130     c                   clear                   orgslo
104300080211     c                   clear                   orgdtvdis
104310080211     c                   clear                   orghvdis
104320080130     c                   endif
104330071031      * aggiorno
104340070706     c                   update    fnorg000
104350070706
104360070706     c                   endsr
104370100218
104380100218      /free
104390100218       //--------------------------------------------------------------------
104400100218       //?Scrivo file FNORE00F.
104410100218       //--------------------------------------------------------------------
104420100218       BEGSR sr_Fnore;
104430140516
104440140516         wOKrcdg = *off;
104450100218
104460100225       //?Se previsto il preavviso scrivo l'estensione 'M ' per memorizzare
104470100225       //?l'indirizzo mail dove inviare il preavviso
104480100225       //?(non aggancio il FNVAOE rcd 'M ' in quanto l'indirizzo mail è già recuperato)
104490100225         IF  §ormpre = 'M';
104500100225           clear fnore000;
104510100225           OREpoe = ORMpoe;
104520100225           OREnsr = ORMnsr;
104530100225           OREnor = ORMnor;
104540100225           OREnrv = ORMnrv;
104550100225           OREtrc = 'M ';
104560100225           OREdati = VAOEdati;
104570100225           write fnore000;
104580100225         ENDIF;
104590100218
104600100318       //?Importo le estensioni da VAO a ORM
104610100218         k_vaoecor = sav_vaocor;
104620100218         k_vaoepoe = sav_vaopoe;
104630100218         k_vaoensr = sav_vaonsr;
104640100218         k_vaoenor = sav_vaonor;
104650100218         k_vaoenrv = sav_vaonrv;
104660100218       //?Aggancio il rcd
104670100318         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
104680100318         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104690100318         DOW not %eof(FNVAOE1l);
104700140422         //?ma solo dei rcd che non vengono variati da pgm
104710140422           IF  %lookup(VAOEtrc:skTRCvao) = 0;
104720100318             clear fnore000;
104730100318             OREpoe = ORMpoe;
104740100318             OREnsr = ORMnsr;
104750100318             OREnor = ORMnor;
104760100318             OREnrv = ORMnrv;
104770100318             OREtrc = VAOEtrc;
104780100318             OREdati = VAOEdati;
104790100318             write fnore000;
104800100318           ENDIF;
104810100318           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104820100318         ENDDO;
104830140422
104840140422       //?Visto che questa routine viene richiamata da conferma VAO
104850140422       //?scrivo subito il rcd 'G', con i dati da VAO tranne i 2 flag ORM per alert
104860140422       //?che sono da impostare in base ai valori calcolati nel pgm
104870140422         k_vaoecor = sav_vaocor;
104880140422         k_vaoepoe = sav_vaopoe;
104890140422         k_vaoensr = sav_vaonsr;
104900140422         k_vaoenor = sav_vaonor;
104910140422         k_vaoenrv = sav_vaonrv;
104920140516         k_vaoetrc = 'G ';
104930140422       //?Scrivo il rcd solo se trovo su VAO
104940140422         chain  %kds(k06fnvaoe) FNVAOE1L;
104950140422         IF  %found(FNVAOE1L);
104960140422           clear fnore000;
104970140422           OREpoe = ORMpoe;
104980140422           OREnsr = ORMnsr;
104990140422           OREnor = ORMnor;
105000140422           OREnrv = ORMnrv;
105010140422           OREtrc = VAOEtrc;
105020140422           OREdati = dOREgen;
105030140422           write fnore000;
105040140516           wOKrcdg = *on;
105050140422         ENDIF;
105060100218
105070100218       ENDSR;
105080131018
105090131018       //--------------------------------------------------------------------
105100131018       //?Scrivo/Aggiorno file FNORE00F.
105110131018       //--------------------------------------------------------------------
105120131018       BEGSR aggFNORE;
105130131018
105140131018         k_OREpoe = ORMpoe;
105150131018         k_OREnsr = ORMnsr;
105160131018         k_OREnor = ORMnor;
105170131018         k_OREnrv = ORMnrv;
105180131018
105190131018       //?Rcd 'O ' - Orari apertura
105200131018        IF  v1OraAMda > 0 or v1OraAMa > 0 or
105210131018            v1OraPMda > 0 or v1OraPMa > 0;
105220131018          k_OREtrc = 'O ';
105230131018          §OREoramda = v1OraAMda;
105240131018          §OREorama  = v1OraAMa;
105250131018          §OREorapda = v1OraPMda;
105260131018          §OREorapa  = v1OraPMa;
105270131029          chain  %kds(k05fnore) FNORE01L;
105280131018          IF  %found(FNORE01L);
105290131125            OREdati = dOREorari;
105300131125            update fnore000;
105310131018          ELSE;
105320131018            clear fnore000;
105330131018            OREpoe = ORMpoe;
105340131018            OREnsr = ORMnsr;
105350131018            OREnor = ORMnor;
105360131018            OREnrv = ORMnrv;
105370131018            OREtrc = 'O ';
105380131018            OREdati = dOREorari;
105390131018            write fnore000;
105400131018          ENDIF;
105410131018        ENDIF;
105420131125       //?Rcd 'O ' - Orari apertura da cancellare
105430131125        IF  v1OraAMda = 0 and v1OraAMa = 0 and
105440131125            v1OraPMda = 0 and v1OraPMa = 0;
105450131125          k_OREtrc = 'O ';
105460131125          §OREoramda = v1OraAMda;
105470131125          §OREorama  = v1OraAMa;
105480131125          §OREorapda = v1OraPMda;
105490131125          §OREorapa  = v1OraPMa;
105500131125          chain  %kds(k05fnore) FNORE01L;
105510131125          IF  %found(FNORE01L);
105520131125            delete fnore000;
105530131125          ENDIF;
105540131125        ENDIF;
105550161020
105560161020       //?Rcd 'DT' - Dta Pronta Merce
105570170524       //?         --> Data Ritiro Calcolata e Anticipo
105580161020        k_OREtrc = 'DT';
105590161027        clear dOREdt;
105600161027        §OREdpm = inv_V1dpm;
105610170524       //?Per immissione o copia devo salvare i dati relativi
105620170524       //?alla data ritiro calcolata se potevo anticipare e se ho anticipato
105630170524       //?in variazione riporto quello che c'era già sul file
105640170524       //?anche se viene variata la data di ritiro
105650170524        §OREdar = %editc(DarCalcolata:'X');
105660170524        IF  wOkPosticipa;
105670170524          §OREposd = 'S';
105680170524        ENDIF;
105690170524        IF  wAnticipa;
105700170524          §OREant = 'S';
105710170524        ENDIF;
105720170524        IF  Anticipato;
105730170524          §OREmod = 'S';
105740170524        ENDIF;
105750170524        §OREgga = %editc(ggAnticipo:'X');
105760161020        chain  %kds(k05fnore) FNORE01L;
105770161020        IF  %found(FNORE01L);
105780161027          OREdati = dOREdt;
105790161020          update fnore000;
105800161020        ELSE;
105810161020          clear fnore000;
105820161020          OREpoe = ORMpoe;
105830161020          OREnsr = ORMnsr;
105840161020          OREnor = ORMnor;
105850161020          OREnrv = ORMnrv;
105860161020          OREtrc = 'DT';
105870161027          OREdati = dOREdt;
105880161020          write fnore000;
105890161020        ENDIF;
105900131018
105910131018       //?Rcd 'MA' - Mail x ALERT
105920131029        IF  old_w03mail <> w03mail or §RMfpr = 'P';
105930131018          k_OREtrc = 'MA';
105940131029          chain  %kds(k05fnore) FNORE01L;
105950131018          IF  %found(FNORE01L);
105960131029            IF  w03mail <> *blanks;
105970131029              OREdati = w03mail;
105980131029              update fnore000;
105990131029            ELSE;
106000131029              delete fnore000;
106010131029            ENDIF;
106020131018          ELSE;
106030131029            IF  w03mail <> *blanks;
106040131029              clear fnore000;
106050131029              OREpoe = ORMpoe;
106060131029              OREnsr = ORMnsr;
106070131029              OREnor = ORMnor;
106080131029              OREnrv = ORMnrv;
106090131029              OREtrc = 'MA';
106100131029              OREdati = w03mail;
106110131029              write fnore000;
106120131029            ENDIF;
106130131018          ENDIF;
106140131018        ENDIF;
106150131018
106160131018       //?Rcd 'S ' - SMS x ALERT
106170131029        IF  old_w03sms <> w03sms or §rmfpr = 'P';
106180131018          k_OREtrc = 'S ';
106190140422          §OREsms = w03sms;
106200131029          chain  %kds(k05fnore) FNORE01L;
106210131018          IF  %found(FNORE01L);
106220131029            IF  w03sms <> *blanks;
106230140422              OREdati = dOREsms;
106240131029              update fnore000;
106250131029            ELSE;
106260131029              delete fnore000;
106270131029            ENDIF;
106280131018          ELSE;
106290131029            IF  w03sms <> *blanks;
106300131029              clear fnore000;
106310131029              OREpoe = ORMpoe;
106320131029              OREnsr = ORMnsr;
106330131029              OREnor = ORMnor;
106340131029              OREnrv = ORMnrv;
106350131029              OREtrc = 'S ';
106360140422              OREdati = dOREsms;
106370131029              write fnore000;
106380131029            ENDIF;
106390131018          ENDIF;
106400131018        ENDIF;
106410160310
106420160406       //?Rcd 'MC' - Mail x Conferma Prenotazione Ritiro
106430160406        IF  old_w04mail <> w04mail;
106440160310          k_OREtrc = 'MC';
106450160310          chain  %kds(k05fnore) FNORE01L;
106460160310          IF  %found(FNORE01L);
106470160406            IF  w04mail <> *blanks;
106480160406              OREdati = w04mail;
106490160310              update fnore000;
106500160310            ELSE;
106510160310              delete fnore000;
106520160310            ENDIF;
106530160310          ELSE;
106540160406            IF  w04mail <> *blanks;
106550160310              clear fnore000;
106560160310              OREpoe = ORMpoe;
106570160310              OREnsr = ORMnsr;
106580160310              OREnor = ORMnor;
106590160310              OREnrv = ORMnrv;
106600160310              OREtrc = 'MC';
106610160406              OREdati = w04mail;
106620160310              write fnore000;
106630160310            ENDIF;
106640160310          ENDIF;
106650160310        ENDIF;
106660160310
106670160406       //?Rcd 'SC' - SMS x Conferma Prenotazione Ritiro
106680160406        IF  old_w04sms <> w04sms;
106690160310          k_OREtrc = 'SC';
106700160310          chain  %kds(k05fnore) FNORE01L;
106710160310          IF  %found(FNORE01L);
106720160406            IF  w04sms <> *blanks;
106730160406              OREdati = w04sms;
106740160310              update fnore000;
106750160310            ELSE;
106760160310              delete fnore000;
106770160310            ENDIF;
106780160310          ELSE;
106790160406            IF  w04sms <> *blanks;
106800160310              clear fnore000;
106810160310              OREpoe = ORMpoe;
106820160310              OREnsr = ORMnsr;
106830160310              OREnor = ORMnor;
106840160310              OREnrv = ORMnrv;
106850160310              OREtrc = 'SC';
106860160406              OREdati = w04sms;
106870160310              write fnore000;
106880160310            ENDIF;
106890160310          ENDIF;
106900160310        ENDIF;
106910140507
106920150305       //?Se sono in immissione
106930160208       //?o copia ORM
106940140603       //?oppure sono in conferma da VAO ma non ho scritto il rcd
106950160208           IF  *in01 or *in03 or
106960140603               (§RMfpr = 'C' and not wOKrcdg);
106970140507         //?Se flag impostati memorizzo rcd 'G'
106980160310           IF  §OREfiso <> *blanks or §OREfimo <> *blanks or
106990160310               §OREfmco <> *blanks or §OREfsco <> *blanks;
107000140507             k_OREtrc = 'G ';
107010140507             chain  %kds(k05fnore) FNORE01L;
107020140507             IF  %found(FNORE01L);
107030140507               OREdati = dOREgen;
107040140507               update fnore000;
107050140507             ELSE;
107060140507               clear fnore000;
107070140507               OREpoe = ORMpoe;
107080140507               OREnsr = ORMnsr;
107090140507               OREnor = ORMnor;
107100140507               OREnrv = ORMnrv;
107110140507               OREtrc = 'G ';
107120140507               OREdati = dOREgen;
107130140507               write fnore000;
107140140507             ENDIF;
107150140507           ENDIF;
107160140507         ENDIF;
107170131014
107180131014       ENDSR;
107190131014
107200131014       //--------------------------------------------------------------------
107210160406       //?F4 - Alert ORM Commissionato.
107220131014       //--------------------------------------------------------------------
107230131014       BEGSR F04D01;
107240131014
107250131025         wEndF04 = *off;
107260131025         wOkF6F04 = *off;
107270140423         IF  not wErrF04;
107280131030           *in28 = *off;
107290131030         ENDIF;
107300131212
107310131212       //?Salvo i dati della videata
107320131212         vidolddA = vidnewA;
107330131025
107340131025       //?emetto la videata relativa alla MAIL e SMS
107350131025         write fior051;
107360140423         *in28 = werralert;
107370140423         werralert = *off;
107380131025       //?emetto la window
107390131025         DOW not wEndF04;
107400131025           exfmt fior053;
107410131025           clear  w03msg;
107420131025           *in28 = *off;
107430131025         SELECT;
107440131025       //?F12-Ritorno
107450131025           WHEN  *inkl;
107460131025             wEndF04 = *on;
107470131025             *in28 = *off;
107480131025             *in90 = *on;
107490131212             vidnewA = vidolddA;
107500131025       //?F6-Conferma
107510131025           WHEN  *inkf;
107520131025             exsr CtrDatiF04;
107530131025             IF  not *in28;
107540131025               wEndF04 = *on;
107550131025               wOkF6F04 = *on;
107560131025             ENDIF;
107570131212             vidolddA = vidnewA;
107580131025       //?F10-Copia n.Telefono
107590131025           WHEN  *inkj;
107600160406             IF  w03sms = *blanks;
107610160310               w03sms = V1ter;
107620160310             ENDIF;
107630131025           OTHER;
107640131025       //?controllo i dati
107650131025             exsr CtrDatiF04;
107660131025           ENDSL;
107670131025         ENDDO;
107680131212       //?Imposto la riga dei tasti funzione
107690131212       ExSr Sr_Tastifun;
107700140509
107710140509       //?Se immissione/copia ORM commissionato e dati alert inseriti
107720140509       //?devo ricalcolare la data di ritiro
107730140512       //?se = ad oggi
107740140509         IF  (*in01 or *in03) and V1com = 'S' and
107750140512             (w03sms <> *blanks or w03mail <> *blanks) and
107760140512              V1dar > 0 and V1dar = oggi_ggmmaa;
107770140509           wCalDtRit = *on;
107780140509         ENDIF;
107790140509
107800140509         *inkf = *off;
107810140509         *inkl = *off;
107820140509         *inkj = *off;
107830131014
107840131014       ENDSR;
107850131011
107860131011       //--------------------------------------------------------------------
107870131011       //?F5 - Altri dati.
107880131011       //--------------------------------------------------------------------
107890131011       BEGSR F05D01;
107900131011
107910131011         wEndF05 = *off;
107920131011         wOkF6F05 = *off;
107930131030         IF  not wErrF05;
107940131030           *in28 = *off;
107950131030         ENDIF;
107960131212
107970131212       //?Salvo i dati della videata
107980131212         vidoldd2 = vidnew2;
107990131212
108000131212       //?Decodifico lo stato
108010131212         exsr sr_contrsto;
108020131212
108030131212       //?Decodifico il giro se impostato
108040131212         clear V1Dcgi;
108050131212         IF  V1Ccgi <> *blanks;
108060131212           exsr ctrgiro;
108070131212           IF  D09ocgi <> *blanks;
108080131212             V1Dcgi = D09odes;
108090131212           ENDIF;
108100131212         ENDIF;
108110131011
108120131011       //?emetto la videata relativa agli altri dati
108130131011         write fior051;
108140131011       //?emetto la window dove richiedo gli altri dati
108150131011         DOW not wEndF05;
108160131011           exfmt fior052;
108170131011           clear  w2cmsg;
108180131011           *in28 = *off;
108190131011         SELECT;
108200131011       //?F12-Ritorno
108210131011           WHEN  *inkl;
108220131011             wEndF05 = *on;
108230131011             *in28 = *off;
108240131011             *in90 = *on;
108250131212             vidnew2 = vidoldd2;
108260131212             wforzaqta = *off;
108270131011       //?F6-Conferma
108280131011           WHEN  *inkf;
108290131017             exsr CtrDatiF05;
108300131011             IF  not *in28;
108310131011               wEndF05 = *on;
108320131011               wOkF6F05 = *on;
108330131011             ENDIF;
108340131212             vidoldd2 = vidnew2;
108350131011           OTHER;
108360131011       //?controllo i dati
108370131017             exsr CtrDatiF05;
108380131011           ENDSL;
108390131011         ENDDO;
108400131212       //?Imposto la riga dei tasti funzione
108410131212       ExSr Sr_Tastifun;
108420131011
108430131011       ENDSR;
108440160406
108450160406       //--------------------------------------------------------------------
108460160406       //?F13 - Conferma Prenotazione.
108470160406       //--------------------------------------------------------------------
108480160406       BEGSR F13D01;
108490160406
108500160406         wEndF13 = *off;
108510160406         wOkF6F13 = *off;
108520160406         IF  not wErrF13;
108530160406           *in28 = *off;
108540160406         ENDIF;
108550160406
108560160413       //?Immissione ORM manuale o copia
108570160406       //?e non ci sono ancora i dati
108580160406         IF  (*in01 or *in03) and
108590160406             (V1Ctco = 'M' or V1Ctco = 'E') and
108600160406              W04sms = *blanks and W04mail = *blanks;
108610160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
108620160406           exsr RecDatiConf;
108630160406         ENDIF;
108640160406
108650160406       //?Se è richiamato da Conferma VAS
108660160406       //?proteggo i dati per Conferma Prenotazione
108670160406       //?a meno che non stia emettendo la videata per sistemare
108680160406       //?eventuali errori
108690160406         IF  §RMfpr = 'C' and not wErrConferma;
108700160406           *in20 = *on;
108710160406         ENDIF;
108720160406
108730160406       //?Salvo i dati della videata
108740160406         vidolddC = vidnewC;
108750160406
108760160406       //?emetto la videata relativa alla MAIL e SMS
108770160406         write fior051;
108780160406         *in28 = wErrConferma;
108790160406         wErrConferma = *off;
108800160406       //?emetto la window
108810160406         DOW  not wEndF13;
108820160406           exfmt fior054;
108830160406           clear w04msg;
108840160406           *in28 = *off;
108850160406           SELECT;
108860160406       //?F6-Conferma
108870160406           WHEN  *inkf;
108880160406             exsr CtrDatiF13;
108890160406             IF  not *in28;
108900160406               wEndF13 = *on;
108910160406               wOkF6F13 = *on;
108920160406             ENDIF;
108930160406             vidolddC = vidnewC;
108940160406       //?F10-Copia n.Telefono
108950160406           WHEN  *inkj;
108960160406             IF  w04sms = *blanks;
108970160406               w04sms = V1ter;
108980160406             ENDIF;
108990160406             HWriga = 06;
109000160406             HWcolo = 07;
109010160406       //?F12-Ritorno
109020160406           WHEN  *inkl;
109030160406             wEndF13 = *on;
109040160406             *in28 = *off;
109050160406             *in90 = *on;
109060160406             vidnewC = vidolddC;
109070160406       //?Enter
109080160406       //?controllo i dati
109090160406           OTHER;
109100160406             exsr CtrDatiF13;
109110160406           ENDSL;
109120160406         ENDDO;
109130160406
109140160406         *inkf = *off;
109150160406         *inkl = *off;
109160160406         *inkj = *off;
109170160406
109180160406       ENDSR;
109190130912
109200130912       //--------------------------------------------------------------------
109210130912       //?F14 - Interrogazione orari di servizio.
109220130912       //--------------------------------------------------------------------
109230130913       BEGSR F14D01;
109240150921
109250150921       //?Se non c'è la filiale ritiro e non ho peso/volume/bancali e non è
109260150921       //?un mittente codificato e siamo in immissione/copia
109270150921       //?non posso interrogare gli orari
109280150921         IF  num_V1Cpor = *zeros and V1pkg = 0 and V1vlm = 0 and
109290150921             V1bnc = 0 and (*in01 or *in03) and ds_v1ccra = 0;
109300150921           V1Cmsg = 'Per F14 serve la fil.ritiro e +
109310150921                     almeno 1 tra peso/vol./bancali';
109320150921           *in28 = *on;
109330150921           *in44 = *on;
109340150921           leavesr;
109350150921         ENDIF;
109360131024
109370131024       //?Se non presente calcolo la filiale ritiro
109380131024         IF  num_V1Cpor = *zeros and
109390131024            (*in01 or *in03);
109400131024           exsr  CALPOR;
109410140109         //?Recupero gli ambiti per il recupero del giro da ACR
109420140109           exsr sr_caragr;
109430131024           IF  *in28;
109440131024             leavesr;
109450131024           ENDIF;
109460131024         ENDIF;
109470130912
109480140319       //?Posso richiamare il pgm di int.orari di servizio solo se ho già
109490130912       //?la filiale di ritiro
109500130912         IF  num_V1Cpor = *zeros;
109510130912           V1Cmsg = 'Per interrogare gli orari serve la filiale ritiro';
109520130912           *in28 = *on;
109530130912           *in59 = *on;
109540130912           leavesr;
109550130912         ENDIF;
109560130912       //?l'indirizzo completo del mittente
109570130912         IF  V1car = *blanks or V1lor = *blanks;
109580130912           V1Cmsg = 'Per interrogare gli orari serve l''indirizzo del +
109590130912                     mittente';
109600130912           *in28 = *on;
109610130912           *in63 = *on;
109620130912           leavesr;
109630130912         ENDIF;
109640161116       //?il peso/volume/bancali
109650161116         IF  V1pkg = 0 and V1vlm = 0 and V1bnc = 0;
109660161116           V1Cmsg = 'Per interrogare gli orari occorre inserire +
109670161116                     almeno 1 tra peso/vol./bancali';
109680161116           *in28 = *on;
109690161116           *in44 = *on;
109700161116           leavesr;
109710161116         ENDIF;
109720161116       //?Calcolo il peso/volume se no in caso di conferma proposta si azzerano
109730161116          exsr sr_pesvol;
109740161116
109750161116       //?Cerco il giorno della settimana di "oggi"
109760161116         exsr DatiPerOrari;
109770140318
109780140318       //?Cerco eventuale ora presunta ritiro
109790140318         clear fior01ds;
109800140319       //?ma solo se ORM in distinta e NON quadrato
109810140318         IF  V1ndc > 0 and V1fao < 600;
109820140318           IOR01fgs = num_V1cpor;
109830140318           IOR01ndc = V1ndc;
109840140318           IOR01poe = V1cpoe;
109850140318           IOR01nsr = V1nsr;
109860140318           IOR01nor = V1nor;
109870140318           IOR01nrv = V1nrv;
109880140318           fior01r (fior01ds);
109890140318         ENDIF;
109900130912
109910140318         clear trulorsds;
109920130912         IOREfil = num_V1Cpor;
109930130912         IOREcap = V1car;
109940130912         IOREloc = V1lor;
109950130920         IOREnar = V1nar;
109960131024         IF  V1dar <> 0;
109970131024           IOREdta = inv_V1dar;
109980131024         ELSE;
109990131024           IOREdta = inv_V1dao;
110000131024         ENDIF;
110010130912         IOREtser = 'R';
110020131108         IOREgf2 = sav_O95gf2;
110030140318         IOREorp = OOR01ora;
110040161026
110050161116       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
110060161116       //?non è da fare per i ritiri esteri
110070161116         clear dIORE01;
110080170529         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks;
110090161116           §IOREr_mnx = 'S';
110100161116         ENDIF;
110110161026         IOREflo = dIORE01;
110120130912
110130140318         trulorsr (kpjba:trulorsds);
110140130912
110150130912       ENDSR;
110160131014
110170131014       //--------------------------------------------------------------------
110180131014       //?Controllo dati 'Altri Dati'.
110190131014       //--------------------------------------------------------------------
110200131014       BEGSR CtrDatiF05;
110210131030
110220131030         *in47 = *off;
110230131030         *in48 = *off;
110240131030         *in49 = *off;
110250131030         *in51 = *off;
110260131030         *in84 = *off;
110270131014
110280131014         exsr Sr_Contrsto;
110290131014         IF  *in28;
110300131014           leavesr;
110310131014         ENDIF;
110320131014
110330131211         exsr Sr_Contrddt;
110340131014         IF  *in28;
110350131014           leavesr;
110360131014         ENDIF;
110370131014
110380131014         exsr sr_Contrqta1;
110390131014         IF  *in28;
110400131014           leavesr;
110410131014         ENDIF;
110420131202       //?calcolo peso o volume se non impostati a video
110430131202       //?considerando anche le cubature degli automezzi eventualmente immessi
110440131202         exsr sr_pesvol;
110450131014
110460131014         exsr sr_contrcgi;
110470131014         IF  *in28 or *in90;
110480131014           leavesr;
110490131014         ENDIF;
110500131014
110510131014       ENDSR;
110520131014
110530131014       //--------------------------------------------------------------------
110540131017       //?Controllo dati Alert.
110550131014       //--------------------------------------------------------------------
110560131025       BEGSR CtrDatiF04;
110570131014
110580160310       //?Dati Alert ORM Commissionato
110590131017       //?controllo SMS;
110600131025         IF  w03sms <> *blanks;
110610131025           pInCell = %trim(w03sms);
110620131017           clear pOutErr;
110630140722           clear pOutCell;
110640131017           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
110650150526             IF  §RMfpr = 'C';
110660150526               clear W03sms;
110670150526               leavesr;
110680150526             ENDIF;
110690131017             *in28 = *on;
110700160406             HWriga = 04;
110710131017             HWcolo = 26;
110720140424             w03msg = msg(60);
110730131017             leavesr;
110740131017           ENDIF;
110750140722           IF  pOutCell <> *blanks;
110760140722             w03sms = pOutCell;
110770140722           ENDIF;
110780131014         ENDIF;
110790131014
110800131025         IF  w03mail <> *blanks;
110810131025           §emlindi = w03mail;
110820131014           xemail (dsemail);
110830131014           IF  §emlerro <> '0';
110840150526             IF  §RMfpr = 'C';
110850150526               clear W03mail;
110860150526               leavesr;
110870150526             ENDIF;
110880160406             HWriga = 06;
110890131017             HWcolo = 07;
110900131014             *in28 = *on;
110910140424             w03msg = msg(55);
110920131014             leavesr;
110930131014           ENDIF;
110940140722           IF  §emlindo <> *blanks;
110950140722             w03mail = §emlindo;
110960140722           ENDIF;
110970131014         ENDIF;
110980131014
110990131014       ENDSR;
111000160406
111010160406       //--------------------------------------------------------------------
111020160406       //?Controllo dati Conferma Prenotazione.
111030160406       //--------------------------------------------------------------------
111040160406       BEGSR CtrDatiF13;
111050160406
111060160406       //?Dati Conferma Prenotazione
111070160406         IF  w04sms <> *blanks;
111080160406           pInCell = %trim(w04sms);
111090160406           clear pOutErr;
111100160406           clear pOutCell;
111110160406           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
111120160406             *in28 = *on;
111130160406             H4riga = 04;
111140160406             H4colo = 26;
111150160406             w04msg = msg(60);
111160160406             leavesr;
111170160406           ENDIF;
111180160406           IF  pOutCell <> *blanks;
111190160406             w04sms = pOutCell;
111200160406           ENDIF;
111210160406         ENDIF;
111220160406
111230160406         IF  w04mail <> *blanks;
111240160406           §emlindi = w04mail;
111250160406           xemail (dsemail);
111260160406           IF  §emlerro <> '0';
111270160406             H4riga = 06;
111280160406             H4colo = 07;
111290160406             *in28 = *on;
111300160406             w04msg = msg(55);
111310160406             leavesr;
111320160406           ENDIF;
111330160406           IF  §emlindo <> *blanks;
111340160406             w04mail = §emlindo;
111350160406           ENDIF;
111360160406         ENDIF;
111370160406
111380160406       //?Dati Conferma VAS e avevo il flag di OK conferma ma non ho i dati
111390160406       //?pulisco il flag in modo da non scrivere il file spia
111400160406         IF  §RMfpr = 'C' and
111410160406             W04mail = *blanks and W04sms = *blanks;
111420160406           wconferma = *off;
111430160406         ENDIF;
111440160406
111450160406         sav_mailconf = w04mail;
111460160406         sav_smsconf  = w04sms;
111470160406
111480160406       ENDSR;
111490140320
111500140320       //--------------------------------------------------------------------
111510140320       //?Cerco orari servizio.
111520140320       //--------------------------------------------------------------------
111530140320       BEGSR CercaOrari;
111540161026
111550161026       //?Se immissione/copia o conferma ORM commissionati
111560161026       //?Visualizzo orari servizio
111570161026         IF  *in01 or *in03 or §RMfpr = 'O';
111580161026           *in95 = *on;
111590161026         ENDIF;
111600140320
111610160526       //?Richiamo il TRUL solo se non è ritiro Estero
111620140320         clear trulorsds;
111630140320         clear trulor2ds;
111640160526         IF  V1nar = *blanks;
111650140320         IOREfil = num_V1Cpor;
111660140320         IOREcap = V1car;
111670140320         IOREloc = V1lor;
111680140320         IOREnar = V1nar;
111690140320         IF  V1dar <> 0;
111700140320           IOREdta = inv_V1dar;
111710140320         ELSE;
111720140320           IOREdta = inv_V1dao;
111730140320         ENDIF;
111740140320         IOREtser = 'R';
111750140320         IOREgf2 = sav_O95gf2;
111760140320         trulorsr (kpjba:trulorsds:trulor2ds);
111770140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
111780140320           *in95 = *off;
111790140320           leavesr;
111800140320         ELSE;
111810140320           V1ostis = OOR2stis;
111820140320           V1ostfs = OOR2stfs;
111830140320         ENDIF;
111840160526         ENDIF;
111850160526       //?Se ritiro Estero chiamo nuovo FIORE00R
111860160526         IF  V1nar <> *blanks;
111870160526           clear FIORE00DS;
111880160526           IORE00por = num_V1Cpor;
111890160526           IORE00nar = V1nar;
111900160526           fiore00r (fiore00ds);
111910160526           IF  OORE00err <> *blanks;
111920160526             *in95 = *off;
111930160526             leavesr;
111940160526           ELSE;
111950160526             V1ostis = OORE00min;
111960160526             V1ostfs = OORE00max;
111970160526           ENDIF;
111980160526         ENDIF;
111990161115
112000161115         exsr DatiPerOrari;
112010161021
112020161021       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
112030161021       //?non è da fare per i ritiri esteri
112040161115       //?non è da fare se non ho gli orari minimo e massimo
112050161027         OrariEstesi = *off;
112060161115         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks and
112070161027             OOR2miis > *zeros and OOR2mxfs > *zeros;
112080161021           V1ostis = OOR2miis;
112090161021           V1ostfs = OOR2mxfs;
112100161027           OrariEstesi = *on;
112110161021         ENDIF;
112120140320
112130140320         wOKorariSer = *on;
112140140320
112150140320       ENDSR;
112160161021
112170161021       //--------------------------------------------------------------------
112180161026       //?Aggiungo/Sottraggo 1 gg lavorativo alla data ritiro
112190161021       //--------------------------------------------------------------------
112200161026       BEGSR AddSubUnoDar;
112210161027
112220161027       //?Non dovrebbe essere possibile, ma non si sa mai
112230161027         IF  V1dar = 0;
112240161027           *in28 = *on;
112250161027           V1Cmsg = 'PageUp/PageDown non possibile senza data ritiro';
112260161027           leavesr;
112270161027         ENDIF;
112280161027
112290161027         clear XGIOLAVDS;
112300161027         clear wlbdat;
112310161027         IXGLdata = inv_V1dar;
112320161027         IF  dsp_aid = c_PageDown;
112330161027           IXGLsub  = 'S';
112340161027         ENDIF;
112350161027         IF  dsp_aid = c_PageUp;
112360161027           IXGLadd  = 'S';
112370161027         ENDIF;
112380161027         IXGLgg   = 1;
112390161027         IXGLlav  = 'S';
112400161027         IXGLpa   = 'P';
112410161027         IXGLfil  = num_V1Cpor;
112420161027         XGIOLAV (xgiolavds);
112430161027         IF  OXGLerr <> *blanks;
112440161027           leavesr;
112450161027         ENDIF;
112460161027
112470161122       //?Solo se immissione
112480161122         IF  *in01;
112490161028         //?Se data ritiro inferiore alla data ritiro calcolata errore
112500161028           IF  OXGldata < OOR97dar and
112510161028               OXGLdata < OOR97dmin;
112520161028             *in28 = *on;
112530161028             V1Cmsg = msg(51);
112540161028             leavesr;
112550161028           ENDIF;
112560161028         ENDIF;
112570161027
112580161108       //?Supera la data max consentita
112590161108         IF  OXGLdata > OOR97dmaxb;
112600161108           *in28 = *on;
112610161108           V1Cmsg = msg(35);
112620161108           V1Cmsg = %trim(V1Cmsg) + ' supera '+
112630161108           d§dftgg2 + ' gg. dalla data immissione ORM';
112640161108           leavesr;
112650161108         ENDIF;
112660161108
112670161108       //?Non può essere inferiore alla data immissione ORM
112680161108         IF  OXGLdata < inv_V1dao;
112690161108           *in28 = *on;
112700161108           V1Cmsg = msg(65);
112710161108           leavesr;
112720161108         ENDIF;
112730161027
112740161110         clear wlbdat;
112750161027         G02inv = OXGLdata;
112760161027         G02err = '3';
112770161027         xsrda8 (wlbdat);
112780161027         V1dar = G02dat;
112790161027         inv_v1dar = OXGLdata;
112800161108         old_invv1dar = OXGLdata;
112810161021
112820161021       ENDSR;
112830130912
112840100218      /end-free
112850090206
112860090206      *--------------------------------------------------------------------*
112870090206      * File FNORA00F
112880090206      *--------------------------------------------------------------------*
112890090206     c     sr_wrtapp     begsr
112900090206
112910090206     c     kfnora        chain     fnora01l
112920090206     c                   if        not %found(fnora01l)
112930090206     c                   clear                   fnora000
112940090206     c                   eval      orapoe = ormpoe
112950090206     c                   eval      oransr = ormnsr
112960090206     c                   eval      oranor = ormnor
112970090206     c                   eval      oranrv = ormnrv
112980090407     c                   eval      orafar = 100
112990090216     c                   eval      oradae = v1dfo
113000090216     c                   eval      oraore = v1ofo
113010090206     c                   eval      oraref = wappref
113020090206     c                   eval      oranote = wappnota1 + wappnota2
113030090206     c                   eval      oradata = oggi_aammgg
113040090206     c                   time                    w0140
113050090206     c                   movel     w0140         oraora
113060090206     c                   eval      oraute = knmus
113070090206     c                   write     fnora000
113080090206     c                   endif
113090090206
113100090206     c                   if        %found(fnora01l)
113110090206     c                   eval      oraref = wappref
113120090206     c                   eval      oranote = wappnota1 + wappnota2
113130090206     c                   eval      oradata = oggi_aammgg
113140090206     c                   time                    w0140
113150090206     c                   movel     w0140         oraora
113160090206     c                   eval      oraute = knmus
113170090206     c                   update    fnora000
113180090206     c                   endif
113190090206
113200090206     c                   endsr
113210070903
113220010704      **********************************************************************
113230010704      * RICHIAMO IMMISSIONE PREPAGATI
113240010704      **********************************************************************
113250010704     C     Sr_Immprep    begsr
113260030623
113270030623     c                   Clear                   wtrulvid
113280010704
113290020522      * richiamo il programma per la scelta delle stampanti
113300030307     c                   Clear                   wdatibolla
113310030307     c                   Eval      wdatibolla = 'Prepagati'
113320030623     c                   Eval      wdatibolA4 = 'Prepag_A4'
113330030623     c                   Eval      wdatibolA5 = 'Prepag_A5'
113340020522     c                   exsr      sr_scestp
113350020522     c   92              goto      endsrimmprep
113360010704
113370010704      * Richiamo immissione prepagati
113380010704     C                   clear                   kpjbu
113390010704     C                   clear                   dtasv
113400010704     C                   clear                   dblp
113410010704
113420010704     C                   move      'O'           §LPfpr
113430010704     C                   movel     ORMpoe        §LPpoe
113440010704     C                   movel     ORMnsr        §LPnsr
113450010704     C                   movel     ORMnor        §LPnor
113460010704     C                   movel     ORMnrv        §LPnrv
113470010704
113480010704     C                   call      'FNLS01R'
113490010704     C                   parm                    kpjba
113500010704     C                   parm                    dblp
113510010704     C                   parm                    dtasv
113520030623     c                   Parm                    Trul90ds
113530010704
113540010704     C                   eval      okbolla = '1'
113550010704
113560010704     C                   if        §lpfpr = '4' or §lpfpr = '3'
113570010704     C                             or §lpfpr = '5'
113580010704     C                   eval      okbolla = ' '
113590010704     C                   endif
113600010704
113610010704     C     endsrimmprep  endsr
113620020522      **********************************************************************
113630020522      * scelta stampante per immissione bolla
113640020522      **********************************************************************
113650020522     c     sr_scestp     begsr
113660020522
113670020522     C                   clear                   kpjbu
113680020522     c                   setoff                                       92
113690020522
113700020522      * Richiesta stampanti
113710020522     C                   if        conta = *zeros
113720030623     C                   clear                   trul90ds
113730030623      * se non serve non faccio vedere il video della scelta stampanti
113740030623     c                   If        wtrulvid <> *Blanks
113750030623     C                   movel     'X'           D90rse
113760030623     C                   movel     'X'           D90rsb
113770030623     c                   Else
113780030623     C                   movel     'S'           D90rse
113790030623     C                   movel     'S'           D90rsb
113800030623     c                   EndIf
113810030623     C                   call      'TRUL90R'
113820020522     C                   parm                    kpjba
113830030623     C                   parm                    trul90ds
113840020522      * F3=Fine
113850030623     C                   if        D90f3 = '1'
113860020522     C                   eval      *in92 = *on
113870020522     C                   goto      endstp
113880020522     C                   endif
113890030307
113900030307     C                   z-add     103           lung
113910020522
113920020522      * Ovrprtf Segnacolli
113930030623     C                   movea     D90pre        cm3(28)
113940030623     C                   movea     D90mde        cm3(50)
113950020522     C                   clear                   comman
113960020522     C                   movea     cm3           comman
113970020522     C                   CALL      'QCMDEXC'                            92
113980020522     C                   PARM                    comman
113990020522     C                   PARM                    lung
114000020522     C   92              goto      endstp
114010030623
114020030623      * Bolle - Nuovo modulo
114030030623
114040030623     c                   Z-Add     110           lung
114050030623
114060030623      * Ovrprtf Bolle A4
114070030623     c                   Movea     D90prb4       cma4(30)
114080030623     c                   Movea     D90mdb4       cma4(52)
114090030623     c                   Movea     wdatibolA4    cma4(96)
114100030623     c                   Clear                   Comman
114110030623     c                   Movea     cmA4          Comman
114120030623     c                   Call      'QCMDEXC'                            92
114130030623     c                   Parm                    Comman
114140030623     c                   Parm                    Lung
114150030623     c   92              goto      endstp
114160030623
114170030623      * Ovrprtf Bolle A5
114180030623     c                   Movea     D90prb5       cmA5(30)
114190030623     c                   Movea     D90mdb5       cmA5(52)
114200030623     c                   Movea     wdatibolA5    cma5(96)
114210030623     c                   Clear                   Comman
114220030623     c                   Movea     cmA5          Comman
114230030623     c                   Call      'QCMDEXC'                            92
114240030623     c                   Parm                    Comman
114250030623     c                   Parm                    Lung
114260030623     c   92              goto      endstp
114270020522
114280030623      * se non faccio vedere il video non aggiorno il contatore
114290030623     c                   If        wtrulvid = *Blanks
114300020522     C                   eval      conta = conta +1
114310030623     c                   EndIf
114320020522     C                   endif
114330020522
114340020522     c     endstp        endsr
114350001019      **********************************************************************
114360001023      * SCRIVE storico variazioni
114370001019      **********************************************************************
114380001019     C     Sr_Storico    BEGSR
114390001019
114400001019     C                   clear                   fnorv000
114410001019     C                   eval      ORVpoe = ORMpoe
114420001019     C                   eval      ORVnsr = ORMnsr
114430001019     C                   eval      ORVnor = ORMnor
114440001019     C                   eval      ORVnrv = ORMnrv
114450001019     C                   eval      ORVtor = ORMtor
114460001019     C                   eval      ORVcor = ORMcor
114470001019     C                   eval      ORVrso = ORMrso
114480001019     C                   eval      ORVino = ORMino
114490001019     C                   eval      ORVcao = ORMcao
114500001019     C                   eval      ORVloo = ORMloo
114510001019     C                   eval      ORVpro = ORMpro
114520001019     C                   eval      ORVnao = ORMnao
114530001019     C                   eval      ORVcra = ORMcra
114540001019     C                   eval      ORVrsr = ORMrsr
114550001019     C                   eval      ORVinr = ORMinr
114560001019     C                   eval      ORVcar = ORMcar
114570001019     C                   eval      ORVlor = ORMlor
114580001019     C                   eval      ORVprr = ORMprr
114590001019     C                   eval      ORVnar = ORMnar
114600001019     C                   eval      ORVrer = ORMrer
114610001019     C                   eval      ORVter = ORMter
114620001019     C                   eval      ORVcrc = ORMcrc
114630001019     C                   eval      ORVrsc = ORMrsc
114640001019     C                   eval      ORVinc = ORMinc
114650001019     C                   eval      ORVloc = ORMloc
114660001019     C                   eval      ORVcac = ORMcac
114670001019     C                   eval      ORVprc = ORMprc
114680001019     C                   eval      ORVnac = ORMnac
114690001019     C                   eval      ORVffd = ORMffd
114700001019     C                   eval      ORVdar = ORMdar
114710001019     C                   eval      ORVorr = ORMorr
114720001019     C                   eval      ORVnam = ORMnam
114730001019     C                   eval      ORVncl = ORMncl
114740001019     C                   eval      ORVpkg = ORMpkg
114750001019     C                   eval      ORVvlm = ORMvlm
114760001019     C                   eval      ORVbnc = ORMbnc
114770001019     C                   eval      ORVblc = ORMblc
114780001019     C                   eval      ORVatt = ORMatt
114790001019     C                   eval      ORVmtc = ORMmtc
114800001019     C                   eval      ORVpag = ORMpag
114810001019     C                   eval      ORVksc = ORMksc
114820001019     C                   eval      ORVctr = ORMctr
114830001024     C                   eval      ORVddt = ORMddt
114840001019     C                   eval      ORVpor = ORMpor
114850130416     C                   eval      ORVzor = ORMzor
114860001019     C                   eval      ORVpoc = ORMpoc
114870001019     C                   eval      ORVno1 = ORMno1
114880001019     C                   eval      ORVno2 = ORMno2
114890001019     C                   eval      ORVcst = ORMcst
114900001019     C                   eval      ORVvcs = ORMvcs
114910001019     C                   eval      ORVfcs = ORMfcs
114920001019     C                   eval      ORVdfo = ORMdfo
114930001019     C                   eval      ORVofo = ORMofo
114940001019     C                   eval      ORVfao = ORMfao
114950001027     C                   eval      ORVsto = ORMsto
114960001019     C                   eval      ORVdst = ORMdst
114970001019     C                   eval      ORVnpg = ORMnpg
114980001019     C                   eval      ORVndc = ORMndc
114990001019     C                   eval      ORVddc = ORMddc
115000001019     C                   eval      ORVstp = ORMstp
115010001027     C                   eval      ORVrfa = ORMrfa
115020070706     C                   eval      ORVeti = ORMeti
115030001103     C                   eval      ORVsto = ORMsto
115040010119     C                   eval      ORVspi = ORMspi
115050001019     C                   eval      ORVflo = ORMflo
115060010116     C                   z-add     oggi_aammgg   ORVdtv
115070001019     C                   movel     w0140         ORVorv
115080001019     C                   movel     knmus         ORVutv
115090040908     c                   Eval      OrvDtt = oggi_aammgg
115100161115     c                   eval      ORVrmp = ORMrmp
115110001019     C                   write     fnorv000
115120131024
115130131024      /free
115140131024       //?Scrivo anche le estensioni
115150131024         IF  wOkestensione;
115160131024           setll    (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115170131024           reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115180140520           DOW  not %eof(FNORE01L);
115190131024             clear FNORVE00;
115200131024             ORVEpoe = OREpoe;
115210131024             ORVEnsr = OREnsr;
115220131024             ORVEnor = OREnor;
115230131024             ORVEnrv = OREnrv;
115240131024             ORVEdtv = ORVdtv;
115250131024             ORVEorv = ORVorv;
115260131024             ORVEutv = ORVutv;
115270131024             ORVEtrc = OREtrc;
115280131024             ORVEdati = OREdati;
115290131024             write FNORVE00;
115300140520             reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115310140520           ENDDO;
115320131024         ENDIF;
115330131024      /end-free
115340001019
115350001019     C                   endsr
115360001019      **********************************************************************
115370001023      * SCRIVE proposte variazioni
115380001019      **********************************************************************
115390001103     C     Sr_CarProposteBEGSR
115400140227
115410140227      /free
115420140227       //?Se è stata fatta una proposta di nota AUT devo aggiornare FNORP
115430140227       //?scritto da FIORT1R
115440140227         IF  wF17write;
115450140227           chain (kpoe:knsr:knor:knrv:OORT1dim:OORT1oim) FNORP01L;
115460140227           IF  %found(FNORP01L);
115470140227             exsr move_proposte;
115480140227             update fnorp000;
115490140227           ENDIF;
115500140227         ENDIF;
115510140227      /end-free
115520001019
115530140227     c                   IF        not wF17write
115540010116     C                   clear                   fnorp000
115550010116     C                   eval      ORPpoe = v1cpoe
115560010116     C                   eval      ORPnsr = v1nsr
115570010116     C                   eval      ORPnor = v1nor
115580010116     C                   eval      ORPnrv = v1nrv
115590140227     c                   exsr      move_proposte
115600140227     C                   z-add     oggi_aammgg   ORPdtv
115610140227     C                   movel     w0140         ORPorv
115620140227     C                   movel     knmus         ORPutv
115630001019     C                   write     fnorp000
115640140227     c                   ENDIF
115650131024
115660131024      /free
115670131024       //?Scrivo anche le estensioni
115680131024         IF  v1OraAMda <> 0 or v1OraAMa <> 0 or
115690131024             v1OraPMda <> 0 or v1OraPMa <> 0;
115700131024           §OREoramda = v1OraAMda;
115710131024           §OREorama  = v1OraAMa;
115720131024           §OREorapda = v1OraPMda;
115730131024           §OREorapa  = v1OraPMa;
115740131024           clear FNORPE00;
115750131024           ORPEpoe = ORPpoe;
115760131024           ORPEnsr = ORPnsr;
115770131024           ORPEnor = ORPnor;
115780131024           ORPEnrv = ORPnrv;
115790131024           ORPEdtv = ORPdtv;
115800131024           ORPEorv = ORPorv;
115810131024           ORPEutv = ORPutv;
115820131024           ORPEtrc = 'O ';
115830131024           ORPEdati = dOREorari;
115840131024           write FNORPE00;
115850131024         ENDIF;
115860161020         //?RCD DT - Data Pronta Merce
115870161027         clear dOREdt;
115880161027         §OREdpm = inv_v1dpm;
115890170605         §OREdar = %editc(DarCalcolata:'X');
115900170605         IF  wOkPosticipa;
115910170605           §OREposd = 'S';
115920170605         ENDIF;
115930170605         IF  wAnticipa;
115940170605           §OREant = 'S';
115950170605         ENDIF;
115960170605         IF  Anticipato;
115970170605           §OREmod = 'S';
115980170605         ENDIF;
115990170605         §OREgga = %editc(ggAnticipo:'X');
116000161020         clear fnorpe00;
116010161020         ORPEpoe = ORPpoe;
116020161020         ORPEnsr = ORPnsr;
116030161020         ORPEnor = ORPnor;
116040161020         ORPEnrv = ORPnrv;
116050161020         ORPEdtv = ORPdtv;
116060161020         ORPEorv = ORPorv;
116070161020         ORPEutv = ORPutv;
116080161020         ORPEtrc = 'DT';
116090161027         ORPEdati = dOREdt;
116100161020         write FNORPE00;
116110160406         IF  w04sms <> *blanks;
116120160311           clear FNORPE00;
116130160311           ORPEpoe = ORPpoe;
116140160311           ORPEnsr = ORPnsr;
116150160311           ORPEnor = ORPnor;
116160160311           ORPEnrv = ORPnrv;
116170160311           ORPEdtv = ORPdtv;
116180160311           ORPEorv = ORPorv;
116190160311           ORPEutv = ORPutv;
116200160311           ORPEtrc = 'SC';
116210160406           ORPEdati = w04sms;
116220160311           write FNORPE00;
116230160311         ENDIF;
116240160406         IF  w04mail <> *blanks;
116250160311           clear FNORPE00;
116260160311           ORPEpoe = ORPpoe;
116270160311           ORPEnsr = ORPnsr;
116280160311           ORPEnor = ORPnor;
116290160311           ORPEnrv = ORPnrv;
116300160311           ORPEdtv = ORPdtv;
116310160311           ORPEorv = ORPorv;
116320160311           ORPEutv = ORPutv;
116330160311           ORPEtrc = 'MC';
116340160406           ORPEdati = w04mail;
116350160311           write FNORPE00;
116360160311         ENDIF;
116370131025         IF  w03sms <> *blanks;
116380140505           §OREsms = w03sms;
116390131024           clear FNORPE00;
116400131024           ORPEpoe = ORPpoe;
116410131024           ORPEnsr = ORPnsr;
116420131024           ORPEnor = ORPnor;
116430131024           ORPEnrv = ORPnrv;
116440131024           ORPEdtv = ORPdtv;
116450131024           ORPEorv = ORPorv;
116460131024           ORPEutv = ORPutv;
116470131024           ORPEtrc = 'S ';
116480140505           ORPEdati = dOREsms;
116490131024           write FNORPE00;
116500131024         ENDIF;
116510131025         IF  w03mail <> *blanks;
116520131024           clear FNORPE00;
116530131024           ORPEpoe = ORPpoe;
116540131024           ORPEnsr = ORPnsr;
116550131024           ORPEnor = ORPnor;
116560131024           ORPEnrv = ORPnrv;
116570131024           ORPEdtv = ORPdtv;
116580131024           ORPEorv = ORPorv;
116590131024           ORPEutv = ORPutv;
116600131024           ORPEtrc = 'MA';
116610131025           ORPEdati = w03mail;
116620131024           write FNORPE00;
116630131024         ENDIF;
116640131024      /end-free
116650001019
116660001019     C                   endsr
116670140227
116680140227      *------------------------------------------------------------------------*
116690140227      *  Muovo i campi del video al file delle proposte
116700140227      *------------------------------------------------------------------------*
116710140227     c     move_proposte begsr
116720140227
116730140227     C                   eval      ORPtor = v1ctor
116740140227     C                   eval      ds_v1cor1 = v1cor1
116750140227     C                   eval      ds_v1cor2 = v1cor2
116760140227     C                   eval      ds_v1cor3 = v1cor3
116770140227     C                   eval      ORPcor = ds_v1ccor
116780140227     C                   eval      ORPrso = v1rso
116790140227     C                   eval      ORPino = v1ino
116800140227     C                   eval      ORPcao = v1cao
116810140227     C                   movel(p)  sav_v1loo35   ORPloo
116820140227     C                   eval      ORPpro = v1pro
116830140227     C                   eval      ORPnao = v1nao
116840140227     C                   eval      ds_v1cra1 = v1cra1
116850140227     C                   eval      ds_v1cra2 = v1cra2
116860140227     C                   eval      ds_v1cra3 = v1cra3
116870140227     C                   eval      ORPcra = ds_v1ccra
116880140227     C                   eval      ORPrsr = v1rsr
116890140227     C                   eval      ORPinr = v1inr
116900140227     C                   eval      ORPcar = v1car
116910140227     C                   movel(p)  v1lor         ORPlor
116920140227     C                   eval      ORPprr = v1prr
116930140227     C                   eval      ORPnar = v1nar
116940140227     C                   eval      ORPrer = v1rer
116950140227     C                   eval      ORPter = v1ter
116960140227     C                   eval      ds_v1crc1 = v1crc1
116970140227     C                   eval      ds_v1crc2 = v1crc2
116980140227     C                   eval      ds_v1crc3 = v1crc3
116990140227     C                   eval      ORPcrc = ds_v1ccrc
117000140227     C                   eval      ORPrsc = v1rsc
117010140227     C                   eval      ORPinc = v1inc
117020140227     C                   movel(p)  sav_v1loc35   ORPloc
117030140227     C                   eval      ORPcac = v1cac
117040140227     C                   eval      ORPprc = v1prc
117050140227     C                   eval      ORPnac = v1nac
117060140227     C                   eval      ORPffd = v1ffd
117070140227      * Inversione data ritiro
117080140227     C                   clear                   wlbdat
117090140227     C                   z-add     v1dar         G02dat
117100140227     C                   movel     *blanks       G02err
117110140227     C                   call      'XSRDA8'
117120140227     C                   parm                    wlbdat
117130140227     C                   z-add     G02inv        ORPdar
117140140227
117150140227     C                   eval      ORPorr = v1orr
117160140227     C                   eval      ORPnam = v1nam
117170140227     C                   eval      ORPncl = v1ncl
117180140227     C                   eval      ORPpkg = v1pkg
117190140227     C                   eval      ORPvlm = v1vlm
117200140227     C                   eval      ORPbnc = v1bnc
117210140227     C                   eval      ORPblc = v1blc
117220140227     C                   eval      ORPatt = v1att
117230140227     C                   eval      ORPmtc = v1mtc
117240140227     C                   eval      ORPpag = v1pag
117250140227     C                   eval      ORPksc = v1ksc
117260140227     C                   eval      ORPctr = v1ctr
117270140227     C                   eval      ORPddt = v1ddt
117280140227     C                   movel     v1cpor        ORPpor
117290140227     C                   movel     v1cpoc        ORPpoc
117300140227     C                   eval      ORPno1 = v1not1
117310140227     C                   eval      ORPno2 = v1not2
117320140227     C                   eval      ORPrfa = v1rfa
117330140227     C                   move      v1sto         ORPsto
117340140227     C                   eval      ORPcst = v1cst
117350140227     C                   eval      ORPvcs = v1vcs
117360140227     C                   eval      ORPfcs = v1fcs
117370140227     C                   eval      ORPdfo = v1dfo
117380140227     C                   eval      ORPofo = v1ofo
117390140227     C                   eval      ORPfao = v1fao
117400140227     C                   eval      ORPdst = v1dst
117410140227     C                   eval      ORPnpg = v1npg
117420140227     C                   eval      ORPndc = v1ndc
117430140227     C                   eval      ORPddc = v1ddc
117440140227     C                   eval      ORPstp = v1stp
117450140227     C                   eval      ORPtap = v1tap
117460140227     C                   eval      ORPeti = v1eti
117470140227     C                   eval      ORPspi = v1spi
117480140227     C                   eval      ORPcau = v3cau
117490140227      * Se forzato blocco destinatario devo pulire il relativo flag
117500140227     C                   if        §ormfd = 'S'
117510140227     C                   if        v1ctor <> 'S'
117520140227     C                   clear                   §ormfd
117530140227     C                   else
117540140227     C                   if        v1rsc = *blanks or ds_v1ccrc = *zeros
117550140227     C                   clear                   §ormfd
117560140227     C                   endif
117570140227     C                   endif
117580140227     C                   endif
117590140227      * ORM commissionato
117600140227     c                   Eval      §orcom = v1com
117610140227
117620140227      * in base al tipo orm e a chi paga imposto il campo §orpfb
117630140227      * imposto 'F' se paga mittente per orm singolo o prepagato
117640140227      * imposto 'A' se NON paga mittente
117650140227     c                   Clear                   §orPfb
117660140227     c                   Select
117670140227     c                   When      OrpPag = 'M'
117680140227     c                   If        OrpTor = 'P' or OrpTor = 'S'
117690140227     c                   Eval      §orPfb = 'F'
117700140227     c                   EndIf
117710140227     c                   Other
117720140227     c                   Eval      §orPfb = 'A'
117730140227     c                   EndSl
117740140227
117750140227     C                   eval      ORPflo = dorm01
117760140227
117770140227     c                   Eval      OrpDtt = oggi_aammgg
117780140227      * Mette sempre in positivo i campi delle quantità
117790140227     C                   mllzo     1             ORPorr
117800140227     C                   mllzo     1             ORPncl
117810140227     C                   mllzo     1             ORPpkg
117820140227     C                   mllzo     1             ORPvlm
117830140227     C                   mllzo     1             ORPbnc
117840140227     C                   mllzo     1             ORPblc
117850140227     C                   mllzo     1             ORPatt
117860140227     C                   mllzo     1             ORPmtc
117870161115      /free
117880161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
117890161115         //?ipotetico ritiro da effettuare al mattino
117900161115         IF  Orariestesi;
117910161115           ORPrmp = 'M';
117920161115         ENDIF;
117930161116         //?Se ORM da Internet non lo memorizzo
117940161116         IF  ORMtco = 'I';
117950161116           clear ORPrmp;
117960161116         ENDIF;
117970161115      /end-free
117980161115
117990140227     c                   endsr
118000131011
118010131011      *------------------------------------------------------------------------*
118020131011      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
118030131011      *------------------------------------------------------------------------*
118040131011     c     sr_f24        begsr
118050131011
118060131011     c                   Select
118070131011
118080131011     c                   When      $loop = 2 and $rig =2  or
118090131011     c                             $loop = 3 and $rig =3
118100131011     c                   Movea     rigatf1       vzfd01
118110131011     c                   Z-add     1             $rig
118120131011
118130131011     c                   If        $loop = 2
118140131011     c                   Eval      vzfd02= cf2412
118150131011     c                   Else
118160131011     c                   Eval      vzfd02= cf2413
118170131011     c                   EndIf
118180131011
118190131011     c                   When      $loop = 2 and $rig =1 or
118200131011     c                             $loop = 3 and $rig =1
118210131011     c                   Movea     rigatf2       vzfd01
118220131011     c                   Z-add     2             $rig
118230131011
118240131011     c                   If        $loop = 2
118250131011     c                   Eval      vzfd02 = cf2422
118260131011     c                   Else
118270131011     c                   Eval      vzfd02 = cf2423
118280131011     c                   EndIf
118290131011
118300131011     c                   When      $loop = 3 and $rig =2
118310131011     c                   Movea     rigatf3       vzfd01
118320131011     c                   Z-add     3             $rig
118330131011     c                   Eval      vzfd02 = cf2433
118340131011     c                   EndSl
118350131011
118360131011     c                   EndSr
118370131011
118380131011      *------------------------------------------------------------------------*
118390131011      *  Imposto descrizione tasti funzione
118400131011      *------------------------------------------------------------------------*
118410131011     c     Sr_Tastifun   BegSr
118420140130
118430140130      /free
118440140130       //?Routine per abilitazione tasti funzione riga fissa
118450140130         exsr Abi_fxx;
118460140130      /end-free
118470131011
118480131011      * Conta i caratteri riempiti dalle RigaTf1
118490131011     c                   Clear                   $z
118500131011      * Conta i caratteri riempiti dalle RigaTf2
118510131011     c                   Clear                   $k
118520131011      * Conta i caratteri riempiti dalle RigaTf3
118530131011     c                   Clear                   $j
118540131011      * Conta la posizione del campone da cui partire per impostare
118550131011      * la descrizione del tasto funzione
118560131011     c                   Clear                   $y
118570131011
118580131011      * Stringhe che contengono le descrizioni dei tasti funzione
118590131011     c                   Reset                   rigatf1
118600131011     c                   Reset                   rigatf2
118610131011     c                   Reset                   rigatf3
118620131011
118630131011      * F2 - Dati DPD
118640131011     c                   if        *in02 and not *in10 and
118650131011     c                             *in18 and not *in17
118660131011     c                   reset                   $tf
118670131011     c                   movea     tf02          $tf
118680131011     c                   exsr      rie_$tf
118690131011     c                   endif
118700131011      * F11 - Volume
118710131011     c                   if        not *in10 and not *in26
118720131011     c                   reset                   $tf
118730131011     c                   movea     tf11          $tf
118740131011     c                   exsr      rie_$tf
118750131011     c                   endif
118760131011      * F6 - Conferma
118770131011     c                   if        *in01 or *in02 or *in03
118780131011     c                   reset                   $tf
118790131011     c                   movea     tf06          $tf
118800131011     c                   exsr      rie_$tf
118810131011     c                   endif
118820160406      * F13 - Conferma Prenotazione Ritiro
118830160406     c                   if        *in88
118840160406     c                   reset                   $tf
118850160406     c                   movea     tf13          $tf
118860160406     c                   exsr      rie_$tf
118870160406     c                   endif
118880131011      * F14 - Orari Servizio
118890131108     c                   if        not *in10 and
118900131025     c                             not *in17
118910131011     c                   reset                   $tf
118920131011     c                   movea     tf14          $tf
118930131011     c                   exsr      rie_$tf
118940131011     c                   endif
118950131011      * F20 - Simulazione
118960131011     c                   if        *in01
118970131011     c                   reset                   $tf
118980131011     c                   movea     tf20          $tf
118990131011     c                   exsr      rie_$tf
119000131011     c                   endif
119010131011      * F16 - Chiusura
119020140918     c                   if        *in98
119030131011     c                   reset                   $tf
119040131011     c                   movea     tf16          $tf
119050131011     c                   exsr      rie_$tf
119060131011     c                   endif
119070131011      * F19 - Proposte variazione
119080131011     c                   if        *in57 and not *in10
119090131011     c                   reset                   $tf
119100131011     c                   movea     tf19          $tf
119110131011     c                   exsr      rie_$tf
119120131011     c                   endif
119130131011      * F21 - Dirottamento
119140131011     c                   if        *in19 and not *in10 and
119150131011     c                             not *in26
119160131011     c                   reset                   $tf
119170131011     c                   movea     tf21          $tf
119180131011     c                   exsr      rie_$tf
119190131011     c                   endif
119200131011
119210131011      * Pulisco la stringa
119220131011     c                   Eval      $h = 1
119230131011     c                   For       $h by 1 to 62
119240131011     c                   If        rigatf1($h) = '#'
119250131011     c                   Clear                   rigatf1($h)
119260131011     c                   EndIf
119270131011     c                   EndFor
119280131011     c                   Eval      $h = 1
119290131011     c                   For       $h by 1 to 62
119300131011     c                   If        rigatf2($h) = '#'
119310131011     c                   Clear                   rigatf2($h)
119320131011     c                   EndIf
119330131011     c                   EndFor
119340131011     c                   Eval      $h = 1
119350131011     c                   For       $h by 1 to 62
119360131011     c                   If        rigatf3($h) = '#'
119370131011     c                   Clear                   rigatf3($h)
119380131011     c                   EndIf
119390131011     c                   EndFor
119400131011
119410131011      * Imposto la descrizione del tasto funzione F24 e segnalo
119420131011      * quante righe ho riempito e quale devo emettere
119430131011     c                   Select
119440131011
119450131011     c                   When      $k <> *Zeros and $y <> *Zeros
119460131011     c                   Eval      $loop = 3
119470131011     c                   Eval      vzfd02 = cf2413
119480131011
119490131011     c                   When      $k <> *Zeros and $y = *Zeros
119500131011     c                   Eval      $loop = 2
119510131011     c                   Eval      vzfd02 = cf2412
119520131011
119530131011     c                   Other
119540131011
119550131011     c                   Eval      $loop = 1
119560131011     c                   Clear                   vzfd02
119570131011     c                   EndSl
119580131011
119590131011      * Prima riga di tasti funzione
119600131011     c                   Movea     rigatf1       vzfd01
119610131011     c                   Eval      $rig = 1
119620131212
119630131212      /free
119640131212        *in80 = *off;
119650131212        *in83 = *off;
119660131212       //?Imposto tasto funzione in HI se presente dati
119670131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
119680131216             V1mtc > 0 or V1sto > 0 or V1ffd <> *blanks;
119690131212           *in80 = *on;
119700131212         ENDIF;
119710160513         IF  ((*in01 or *in03) and
119720160513              (w03sms <> *blanks or W03mail <> *blanks)) or
119730160513               walert;
119740131212           *in83 = *on;
119750131212         ENDIF;
119760131212      /end-free
119770131011
119780131011     c                   EndSr
119790140130
119800140130      /free
119810140130       //--------------------------------------------------------------------
119820140130       //?Abilitazione tasti funzione riga fissa.
119830140130       //--------------------------------------------------------------------
119840140130       BEGSR Abi_Fxx;
119850140130
119860140130         *in97 = *off;
119870140130       //?Abilito il tasto F17-Note AUT solo se sono in manutenzione
119880140227       //?e se NON è conferma proposta di variazione
119890140317           IF  *in02 and §RMfpr <> 'P';
119900140204             *in97 = *on;
119910140204           ENDIF;
119920140918
119930140918         *in98 = *off;
119940140918       //?Attivo F16 Chiusura ORM se
119950140918       //?non è copia/immissione ORM
119960140918       //?non è già chiusura ORM
119970140918       //?non siamo in conferma proposta di variazione
119980140918       //?se ORM non è in fase 420
119990140918         IF  not *in10 and not *in03 and not *in01 and not *in81 and
120000140918             not OrmFase420;
120010140918           *in98 = *on;
120020140918         ENDIF;
120030140130
120040140130       ENDSR;
120050140130      /end-free
120060131011
120070131011      *------------------------------------------------------------------------*
120080131011      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
120090131011      *------------------------------------------------------------------------*
120100131011     c     Rie_$tf       BegSr
120110131011      *
120120131011     c                   Eval      $x = 1
120130131011     c     '#'           Lookup    $tf($x)                                30
120140131011     c                   Add       $x            $z
120150131011     c                   If        $Z <= 63
120160131011     c                   Eval      $j = $Z - $x + 1
120170131011     c                   Movea     $tf           rigatf1($j)
120180131011     c                   Else
120190131011     c                   Add       $x            $k
120200131011     c                   If        $k <= 63
120210131011     c                   Eval      $j = $K - $x + 1
120220131011     c                   Movea     $tf           rigatf2($j)
120230131011     c                   Else
120240131011     c                   Add       $x            $y
120250131011     c                   If        $y <= 63
120260131011     c                   Eval      $j = $y - $x + 1
120270131011     c                   Movea     $tf           rigatf3($j)
120280131011     c                   EndIf
120290131011     c                   EndIf
120300131011     c                   EndIf
120310131011
120320131011     c                   EndSr
120330140422
120340140422      /free
120350140422       //--------------------------------------------------------------------
120360140422       //?Controllo se Alert o meno.
120370140422       //--------------------------------------------------------------------
120380140422       BEGSR AlertORM;
120390140422
120400140424         walert = *off;
120410140422         clear §OREfimo;
120420140422         clear §OREfiso;
120430140515
120440140515         exsr VerificaAlert;
120450140422
120460140515       //?Se OK alert, sono in conferma da VAO e
120470140515       //?ci sono i dati dell'alert li controllo
120480140515         IF  wOKalert and §RMfpr = 'C' and
120490140515            (w03mail <> *blanks or w03sms <> *blanks);
120500140424           exsr CtrDatiF04;
120510140424           IF  *in28;
120520140424             leavesr;
120530140424           ENDIF;
120540140424         ENDIF;
120550140422
120560140422       //?Imposto il flag ORM alert via SMS
120570140515         IF  w03sms <> *blanks;
120580140515           IF  not wOKalert;
120590140515             §OREfiso = 'N';
120600140515           ELSE;
120610140515             SELECT;
120620140422         //?Imposto in base al flag VAO alert via SMS
120630140422         //?per 'N' no alert
120640140423             WHEN  §OREfisv = 'N';
120650140423               §OREfiso = 'N';
120660140422         //?per 'X' alert solo se data ritiro > di oggi
120670140423             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
120680140423               §OREfiso = 'S';
120690140423             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
120700140423               §OREfiso = 'N';
120710140422         //?no 'N' e no 'X' alert calcolato
120720140423             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
120730140515               IF  inv_V1dar > oggi_aammgg;
120740140515                 §OREfiso = 'S';
120750140515               ELSE;
120760140515                 §OREfiso = 'N';
120770140515               ENDIF;
120780140515             ENDSL;
120790140515           ENDIF;
120800140422         ENDIF;
120810140422
120820140422       //?Imposto il flag ORM alert via mail
120830140515         IF  w03mail<> *blanks;
120840140515           IF  not wOKalert;
120850140515             §OREfimo = 'N';
120860140515           ELSE;
120870140515             SELECT;
120880140422         //?Imposto in base al flag VAO alert via mail
120890140422         //?per 'N' no alert
120900140507             WHEN  §OREfimv = 'N';
120910140423               §OREfimo = 'N';
120920140422         //?per 'X' alert solo se data ritiro > di oggi
120930140423             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
120940140423               §OREfimo = 'S';
120950140423             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
120960140423               §OREfimo = 'N';
120970140422         //?no 'N' e no 'X' alert calcolato
120980140423             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
120990140515               IF  inv_V1dar > oggi_aammgg;
121000140515                 §OREfimo = 'S';
121010140515               ELSE;
121020140515                 §OREfimo = 'N';
121030140515               ENDIF;
121040140515             ENDSL;
121050140515           ENDIF;
121060140422         ENDIF;
121070140422
121080140422       //?imposto flag per OK alert da mandare
121090140422         IF  §OREfiso = 'S' or §OREfimo = 'S';
121100140422           walert = *on;
121110140422         ENDIF;
121120140422
121130140422       ENDSR;
121140140515
121150140515       //--------------------------------------------------------------------
121160140515       //?Verifico se OK alert.
121170140515       //--------------------------------------------------------------------
121180140515       BEGSR VerificaAlert;
121190140515
121200140515         wOKalert = *off;
121210140515
121220140515         SELECT;
121230140515
121240140515       //?Se mittente codificato e filiale mittente ha ntw estero
121250140515       //?o mittente NON codificato con nazione estera
121260140515       //?NO alert e NON memorizzo i dati su FNORE
121270140515         WHEN  (ds_V1Ccra > 0 and
121280140515               (m_§OGntw = 'DPD' or m_§OGntw = 'EEX' or m_§OGntw = 'FED')) or
121290140515               (ds_V1Ccra = 0 and V1nar <> *blanks);
121300140515           clear w03sms;
121310140515           clear w03mail;
121320140515
121330140515         //?Se ORM NON commissionato NO alert
121340140515         WHEN  V1com <> 'S';
121350140515         //?Se ORM prepagato NO alert
121360140515         WHEN  V1Ctor = 'P';
121370140515         //?Se Ordinante non codificato NO alert
121380140515         WHEN  ds_V1Ccor = *zeros or
121390140515               ds_V1cor2 = 9999 or ds_V1cor2 = 8888;
121400140515         //?Controllo se peso/volume/bancali superano i limit massimi da DFT
121410140515         //?NO alert
121420140515         WHEN  (d§DFTpkga > 0 and V1pkg > d§DFTpkga) or
121430140515               (d§DFTvlma > 0 and V1vlm > d§DFTvlma) or
121440140515               (d§DFTbnca > 0 and V1bnc > d§DFTbnca);
121450140515         OTHER;
121460140515           wOKalert = *on;
121470140515         ENDSL;
121480140515
121490140515       ENDSR;
121500140505
121510140505       //--------------------------------------------------------------------
121520140505       //?Calcolo data ritiro Alert.
121530140505       //--------------------------------------------------------------------
121540140505       BEGSR DataRitiroAlert;
121550140515
121560140515       //?data ritiro video > oggi non faccio niente
121570140515         dataEUR = %date(V1dar:*eur);
121580140515         dataISO = dataeur;
121590140515         wdar = %dec(dataISO);
121600140515         IF  wdar > oggi_aammgg;
121610140515           leavesr;
121620140515         ENDIF;
121630140515
121640140515       //?se non ci sono i presupposti per inviare alert non faccio niente
121650140515         IF  not wOKalert;
121660140515           leavesr;
121670140515         ENDIF;
121680140515
121690140515       //?se i presupposti ci sono allora devo ricalcolare la data ritiro
121700140515       //?ma solo se i flag da VAO non sono 'X' o 'N'
121710140710         IF  (§OREfisv <> 'X' and §OREfisv <> 'N' and w03sms <> *blanks) or
121720140710             (§OREfimv <> 'X' and §OREfimv <> 'N' and w03mail <> *blanks);
121730140519           IF  V1Cpor > *zeros and num_V1Cpor = *zeros;
121740140519             num_v1Cpor = %dec(V1Cpor:3:0);
121750140519           ENDIF;
121760140527         //?se conferma ORM da VAO e NON ho ancora il POR lo prendo da O95lna
121770140529         //?se impostato
121780140527         //?capita perchè c'è un errore nel controllo del cap del mittente
121790140529           IF  §RMfpr = 'C' and num_V1Cpor = 0 and sav_O95lna > 0;
121800140527             num_V1Cpor = sav_O95lna;
121810140527           ENDIF;
121820140529         //?Calcolo la data solo se ho già la filiale ritiro
121830140529           IF  num_V1Cpor > 0;
121840140515           exsr sr_calpikup;
121850140515           v1dar = new_v1dar;
121860140515           dataEUR = %date(V1dar:*eur);
121870140515           dataISO = dataeur;
121880140515           inv_v1dar = %dec(dataISO);
121890140529           ENDIF;
121900140515         ENDIF;
121910140505
121920140505       ENDSR;
121930160406
121940160406       //--------------------------------------------------------------------
121950160406       //?Controllo se Conferma Prenotazione o meno.
121960160406       //--------------------------------------------------------------------
121970160406       BEGSR ConfermaORM;
121980160406
121990160406         clear §OREfmco;
122000160406         clear §OREfsco;
122010160406
122020160406       //?Se sono in conferma da VAO ed è stata richiesta la conferma
122030160406       //?controllo i dati
122040160406         IF  wConferma and §RMfpr = 'C' and
122050160406            (w04mail <> *blanks or w04sms <> *blanks);
122060160406           exsr CtrDatiF04;
122070160406           IF  *in28;
122080160406             leavesr;
122090160406           ENDIF;
122100160406         ENDIF;
122110160406
122120160406       //?Imposto il flag ORM alert via SMS
122130160406         IF  w03sms <> *blanks;
122140160406           IF  not wOKalert;
122150160406             §OREfiso = 'N';
122160160406           ELSE;
122170160406             SELECT;
122180160406         //?Imposto in base al flag VAO alert via SMS
122190160406         //?per 'N' no alert
122200160406             WHEN  §OREfisv = 'N';
122210160406               §OREfiso = 'N';
122220160406         //?per 'X' alert solo se data ritiro > di oggi
122230160406             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
122240160406               §OREfiso = 'S';
122250160406             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
122260160406               §OREfiso = 'N';
122270160406         //?no 'N' e no 'X' alert calcolato
122280160406             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
122290160406               IF  inv_V1dar > oggi_aammgg;
122300160406                 §OREfiso = 'S';
122310160406               ELSE;
122320160406                 §OREfiso = 'N';
122330160406               ENDIF;
122340160406             ENDSL;
122350160406           ENDIF;
122360160406         ENDIF;
122370160406
122380160406       //?Imposto il flag ORM alert via mail
122390160406         IF  w03mail<> *blanks;
122400160406           IF  not wOKalert;
122410160406             §OREfimo = 'N';
122420160406           ELSE;
122430160406             SELECT;
122440160406         //?Imposto in base al flag VAO alert via mail
122450160406         //?per 'N' no alert
122460160406             WHEN  §OREfimv = 'N';
122470160406               §OREfimo = 'N';
122480160406         //?per 'X' alert solo se data ritiro > di oggi
122490160406             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
122500160406               §OREfimo = 'S';
122510160406             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
122520160406               §OREfimo = 'N';
122530160406         //?no 'N' e no 'X' alert calcolato
122540160406             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
122550160406               IF  inv_V1dar > oggi_aammgg;
122560160406                 §OREfimo = 'S';
122570160406               ELSE;
122580160406                 §OREfimo = 'N';
122590160406               ENDIF;
122600160406             ENDSL;
122610160406           ENDIF;
122620160406         ENDIF;
122630160406
122640160406       //?imposto flag per OK alert da mandare
122650160406         IF  §OREfiso = 'S' or §OREfimo = 'S';
122660160406           walert = *on;
122670160406         ENDIF;
122680160406
122690160406       ENDSR;
122700140422
122710140422       //--------------------------------------------------------------------
122720140422       //?Scrivo file spia Alert.
122730140422       //--------------------------------------------------------------------
122740140422       BEGSR wrtspia;
122750140422
122760151008         IF  walert;
122770140422         clear FISAO000;
122780140422         SAOdaori = %timestamp();
122790140422         SAOpoe = ORMpoe;
122800140422         SAOnsr = ORMnsr;
122810140422         SAOnor = ORMnor;
122820140422         SAOnrv = ORMnrv;
122830140422         SAOtipa = 'AVV';
122840140422         SAOsts = '0';
122850140422         write FISAO000;
122860151008         ENDIF;
122870151008
122880151008         IF  wconferma;
122890151021           clear FISAO000;
122900151021           SAOdaori = %timestamp();
122910151021           SAOpoe = ORMpoe;
122920151021           SAOnsr = ORMnsr;
122930151021           SAOnor = ORMnor;
122940151021           SAOnrv = ORMnrv;
122950151021           SAOtipa = 'CON';
122960151021           SAOsts = '0';
122970151021           write FISAO000;
122980151008         ENDIF;
122990140422
123000140422       ENDSR;
123010140506
123020140506       //--------------------------------------------------------------------
123030140506       //?Imposto la frase per NON TELEFONARE
123040140506       //?se ORM commissionato con Alert
123050140506       //--------------------------------------------------------------------
123060140506       BEGSR CommissionatoAlert;
123070140506
123080140506         clear wtel01;
123090140506         clear wpos;
123100140506         wpos = (%len(wtel01) - %len(cCommAlert)) / 2;
123110140506         %subst(wtel01:wpos:%len(cCommAlert)) = cCommAlert;
123120140506
123130140506         clear wtel02;
123140140506         clear wpos;
123150140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123160140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123170140506
123180140506       ENDSR;
123190140506
123200140506       //--------------------------------------------------------------------
123210140506       //?Imposto la frase per NON TELEFONARE
123220140506       //?se ORM NON commissionato dirottato
123230140506       //--------------------------------------------------------------------
123240140506       BEGSR DirottaNoComm;
123250140506
123260140506         clear wtel01;
123270140506         clear wpos;
123280140506         wpos = (%len(wtel01) - %len(cDirottaNoComm)) / 2;
123290140506         %subst(wtel01:wpos:%len(cDirottaNoComm)) = cDirottaNoComm;
123300140506
123310140506         clear wtel02;
123320140506         clear wpos;
123330140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123340140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123350140506
123360140506       ENDSR;
123370140506
123380140506       //--------------------------------------------------------------------
123390140506       //?Imposto la frase per NON TELEFONARE
123400140506       //?se ORM Con preavviso via mail (CEVA) dirottato
123410140506       //--------------------------------------------------------------------
123420140506       BEGSR DirottaPreavviso;
123430140506
123440140506         clear wtel01;
123450140506         clear wpos;
123460140506         wpos = (%len(wtel01) - %len(cDirottaPreavv)) / 2;
123470140506         %subst(wtel01:wpos:%len(cDirottaPreavv)) = cDirottaPreavv;
123480140506
123490140506         clear wtel02;
123500140506         clear wpos;
123510140506         wpos = (%len(wtel02) - %len(cGiaAvvisato)) / 2;
123520140506         %subst(wtel02:wpos:%len(cGiaAvvisato)) = cGiaAvvisato;
123530140506
123540140506       ENDSR;
123550151106
123560160406       //--------------------------------------------------------------------
123570160406       //?Recupero i dati per Conferma Prenotazione da anagrafica clienti.
123580160406       //--------------------------------------------------------------------
123590160406       BEGSR RecDatiConf;
123600160406
123610160406         clear wCodAcre;
123620160406       //?dall'ordinante se presente
123630160406         IF  ds_V1Ccor > 0;
123640160406           wCodAcre = ds_V1Ccor;
123650160406         ELSE;
123660160406         //?dal mittente se ordinante non presente
123670160406           IF  V1rso = *blanks and ds_V1Ccra > 0;
123680160406             wCodAcre = ds_V1Ccra;
123690160406           ENDIF;
123700160406         ENDIF;
123710160406       //?Ho recuperato un codice quindi aggancio l'angrafica
123720160406          IF  wCodAcre = 0;
123730160406            leavesr;
123740160406          ENDIF;
123750160406          clear dACR01;
123760160406          chain(n) (wCodAcre) FNACR01L;
123770160406          IF  %found(FNACR01L);
123780160406            dACR01 = ACRmai;
123790160406          ENDIF;
123800160406         //?Se il cliente ha il flag di invio conferma
123810160406         //?imposto i dati a video
123820160406          IF  §ACRfmar <> 'S';
123830160406            leavesr;
123840160406          ENDIF;
123850160406          IF  w04mail = *blanks;
123860160406            chain(n) (wCodAcre:'MC') FNACRE1L;
123870160406            IF  %found(FNACRE1L);
123880160406              w04mail = ACREdati;
123890160406            ENDIF;
123900160406          ENDIF;
123910160406          IF  w04sms = *blanks;
123920160406            chain(n) (wCodAcre:'SC') FNACRE1L;
123930160406            IF  %found(FNACRE1L);
123940160406              w04sms = ACREdati;
123950160406            ENDIF;
123960160406          ENDIF;
123970160406
123980160406       ENDSR;
123990160406
124000151106       //--------------------------------------------------------------------
124010151106       //?Aggiorno Anagrafica clienti ritiro Ordinante x mail conferma.
124020151106       //--------------------------------------------------------------------
124030151106       BEGSR aggACR;
124040160505
124050160505       //?Se ORM con Ordinante YOOX non aggiorno
124060160505         IF  ORMcor = 17732000;
124070160505           leavesr;
124080160505         ENDIF;
124090151106
124100151109         chain(e) ORMcor FNACR01L;
124110161118         IF  not  %error;
124120151106         IF  %found(FNACR01L);
124130151109           dACR01 = ACRmai;
124140160322           IF  §OREfmco = 'S' or §OREfsco = 'S';
124150160322             §ACRfmar = 'S';
124160160322           ELSE;
124170151109             clear §ACRfmar;
124180160322           ENDIF;
124190151109           ACRmai = dACR01;
124200151109           update FNACR000;
124210151106         ENDIF;
124220161118         ENDIF;
124230151109
124240151109         chain(e) (ORMcor:'MC') FNACRE1L;
124250161118         IF  not %error;
124260151109         IF  %found(FNACRE1L);
124270160322           IF  sav_mailconf <> *blanks and §ACRfmar = 'S';
124280151109             ACREdati = sav_mailconf;
124290151109             update FNACRE00;
124300151109           ELSE;
124310151109             delete FNACRE00;
124320151109           ENDIF;
124330151109         ENDIF;
124340160112         IF  not %found(FNACRE1L) and sav_mailconf <> *blanks and
124350160322             §ACRfmar = 'S';
124360151109           clear FNACRE00;
124370151109           ACREcro = ORMcor;
124380151109           ACREtrc = 'MC';
124390151109           ACREdati = sav_mailconf;
124400151109           write FNACRE00;
124410151109         ENDIF;
124420161118         ENDIF;
124430160318
124440160318         chain(e) (ORMcor:'SC') FNACRE1L;
124450161118         IF  not %error;
124460160318         IF  %found(FNACRE1L);
124470160322           IF  sav_smsconf <> *blanks and §ACRfmar = 'S';
124480160318             ACREdati = sav_smsconf;
124490160318             update FNACRE00;
124500160318           ELSE;
124510160318             delete FNACRE00;
124520160318           ENDIF;
124530160318         ENDIF;
124540160318         IF  not %found(FNACRE1L) and sav_smsconf <> *blanks and
124550160322             §ACRfmar = 'S';
124560160318           clear FNACRE00;
124570160318           ACREcro = ORMcor;
124580160318           ACREtrc = 'SC';
124590160318           ACREdati = sav_smsconf;
124600160318           write FNACRE00;
124610160318         ENDIF;
124620161118         ENDIF;
124630160712
124640161118       //?Cerco il codice utente da memorizzare su FNACRE
124650160712         clear ORIidc;
124660160712         IF  §ORMpg > *zeros;
124670160712           ORIprg = %dec(§ORMpg:7:0);
124680160712           chain (ORIprg) TIORI01L;
124690160712         ENDIF;
124700161118
124710161118       //?Memorizzo chi ha fatto la richiesta
124720161118         clear dOREana;
124730161118         §OREtipo = 'I';
124740161118         §OREjob = §OREorip;
124750161118         §OREute = ORIidc;
124760161118         §OREdtv = %dec(%date());
124770161118         §OREorv = %dec(%time());
124780161118         chain(e) (ORMcor:'AA') FNACRE1L;
124790161118         IF  not %error;
124800161118           IF  %found(FNACRE1L);
124810161118             ACREdati = dOREana;
124820161118             update FNACRE00;
124830161118           ENDIF;
124840161118           IF  not %found(FNACRE1L);
124850161118             clear FNACRE00;
124860161118             ACREcro = ORMcor;
124870161118             ACREtrc = 'AA';
124880161118             ACREdati = dOREana;
124890161118             write FNACRE00;
124900161118           ENDIF;
124910161118         ENDIF;
124920151106
124930151106       ENDSR;
124940161115
124950161115       //--------------------------------------------------------------------
124960161115       //?Imposto i dati necessari alla visualizzazione orari servizio.
124970161115       //--------------------------------------------------------------------
124980161115       BEGSR DatiPerOrari;
124990161115
125000161115       //?Imposto la data per calcolo data ritiro = oggi
125010161115         DataCalcolo = Oggi_aammgg;
125020161115       //?Se data pronta merce > oggi la data calcolo è la data pronta merce
125030161115         IF  inv_V1dpm > 0 and inv_V1dpm > Oggi_aammgg;
125040161115           DataCalcolo = inv_V1dpm;
125050161115         ENDIF;
125060161115       //?Cerco giorno della settimana della data calcolo data ritiro
125070161115         dataiso = %date(DataCalcolo);
125080161115         clear wDay_Of_Week;
125090161115         exec sql
125100161115         set :wDay_Of_Week = dayofweek_iso(:dataiso);
125110161115
125120161115       //?Aggancio la tabella GPD con filiale ritiro ORM
125130161115         clear TIBS02ds;
125140161115         clear dGPD;
125150161115         T02mod = 'C';
125160161115         T02cod = 'GPD';
125170161115         T02sif = KNSIF;
125180161115         T02ke1 = %editc(num_V1cpor:'X');
125190161115         TNTBE_RicercaControllo (kpjba : tibs02ds);
125200161115         IF  T02err = *blanks;
125210161115           dGPD = T02uni;
125220161115         ELSE;
125230161115         //?Non trovo GPD con filiale ritiro
125240161115         //?la cerco con filiale generica 999
125250161115           clear TIBS02ds;
125260161115           T02mod = 'C';
125270161115           T02cod = 'GPD';
125280161115           T02sif = KNSIF;
125290161115           T02ke1 = '999';
125300161115           TNTBE_RicercaControllo (kpjba : tibs02ds);
125310161115           IF  T02err = *blanks;
125320161115             dGPD = T02uni;
125330161115           ENDIF;
125340161115         ENDIF;
125350161115
125360161115       ENDSR;
125370140422
125380140422      /end-free
125390131011
125400001010      *****************************************************************
125410001010      * ROUTINE INIZIALE
125420001010      *****************************************************************
125430001010     C     *INZSR        BEGSR
125440001009
125450001010     C     *ENTRY        PLIST
125460001010     C                   PARM                    KPJBA
125470001023     C                   PARM                    FIOR05DS
125480001011
125490001011     C                   movel     TIT_A         VTCtit
125500001009
125510150306     c                   IF        kpjbu <> *blanks
125520001026     C                   movel(p)  kpjbu         parm01
125530150306     c                   ELSE
125540150306     c                   clear                   parm01
125550150306     c                   ENDIF
125560001016
125570060203     C                   Z-ADD     1             CODUT             1 0
125580060203
125590060203      * Reperisco dati job
125600150713     c                   IF        §RMfpr <> 'K'
125610060203     c                   exsr      DatiJob
125620150713     c                   open      FIOR05D
125630150713     c                   ENDIF
125640150713
125650150713      /free
125660160713
125670160713       //?Determino la libreria
125680160713         IF  %subst(knsif : 7 : 1) = 'P';
125690160713           wLib = 'GAITRAGRPS';
125700160713         ELSE;
125710160713           wLib = 'GAITRAGRU';
125720160713         ENDIF;
125730160713       //?Apro i file
125740160713         wFLib = %trim(wLib) + '/TIORI01L';
125750160713         open  TIORI01L;
125760160713
125770150713       //?Se richiamato da pgm in ascolto (conferma automatica ORM da file/internet)
125780150713         IF  §RMfpr = 'K';
125790150713       //?imposto DUTpou = filiale emissione
125800150713           DUTpou = §RMpoe;
125810150713       //?carico la SKpog con la filiale emissione
125820150713           SKpog(1) = %editc(§RMpoe:'X');
125830150713       //?il flag di tipo richiamo diventa 'C'
125840150713           §RMfpr = 'C';
125850150713       //?Conferm automatica
125860150713           ConfAuto = *on;
125870150713       //?Utente batch
125880150713           knmus = 'BATCH';
125890150713         ENDIF;
125900150713      /end-free
125910001018
125920041014      * salvo il n. di distinta quando è proposta di chiusura
125930041014     c                   If        §rmtla = 'C' and §rmfpr = 'P' and
125940041014     c                             pndc > *Zeros
125950041014     c                   Eval      savndc = pndc
125960041014     c                   Eval      savddc = pddc
125970060516     c                   Eval      savfgs = pfvvfgs
125980041014     c                   EndIf
125990150304      * imposto il n. ORM quando è proposta variazione
126000001106     C                   if        §RMfpr = 'P'
126010001106     C                   eval      ppoe = §RMpoe
126020001106     C                   eval      pnsr = §RMnsr
126030001106     C                   eval      pnor = §RMnor
126040001106     C                   eval      pnrv = §RMnrv
126050001106     C                   endif
126060110420
126070110420      /free
126080110420       //?Imposto flag per arrivo da conferma proposta di variazione
126090110420       *in81 = *off;
126100110420       IF  §rmfpr = 'P';
126110110420         *in81 = *on;
126120110420       ENDIF;
126130110420      /end-free
126140001023
126150150304     C                   movel     DUTpou        kazorg
126160150310     C                   clear                   network
126170150612     C                   clear                   og143
126180001019     C     kazorg        chain     azorg01L
126190001016     C                   if        %found(azorg01l)
126200150612     C                   movel     orgde3        OG143
126210150612     c                   eval      network = §OGntw
126220001016     C                   endif
126230150609      * Carico £6
126240150609     C                   CLEAR                   trul06ds
126250150609     C                   MOVE      '£6'          D06COD
126260150609     C                   MOVEL     DUTpou        D06KEY
126270150609     C                   MOVEL     trul06ds      KPJBU
126280150609     C                   CALL      'TRUL06R'
126290150609     C                   PARM                    KPJBA
126300150609     C                   MOVEL     KPJBU         trul06ds
126310150609     C                   MOVEA     LIN           L6
126320061023
126330150305      * carico sk dei p.o. DPD
126340061023     c                   clear                   xx
126350061023     c     *loval        setll     azorg01l
126360061023     c                   do        *hival
126370061023     c                   read      azorg01l
126380061023     c                   if        %eof(azorg01l)
126390061023     c                   leave
126400061023     c                   endif
126410061023     c                   if        orgfva <> *blanks  or
126420061023     c                             (orgfag <> 'F' and orgfag <> 'A')
126430061023     c                   iter
126440061023     c                   endif
126450061023     c                   eval      og143 = orgde3
126460061023     c                   if        §ogntw = 'DPD'
126470061023     c                   add       1             xx
126480061023     c                   eval      skpodpd(xx) = orgfil
126490061023     c                   endif
126500061023     c                   enddo
126510081126
126520081126      * tabella 3I x DPD
126530081126     c                   clear                   ds3idp
126540081126     c                   eval      kcod = '3I'
126550081126     c                   eval      kkey = 'DPD'
126560081126     c     ktab          chain     tabel00f
126570081126     c                   if        %found(tabel00f)
126580081126     c                             and tblflg = *blanks
126590081126     c                   eval      ds3idp = tbluni
126600081126     c                   endif
126610001009
126620001023      * reperisce data e ora
126630001010     C                   TIME                    W0140
126640001010      * UDATE IN GGMMAAAA
126650010116     C                   MOVE      W0140         oggi_ggmmaa
126660001010      * UDATE IN AAAAMMGG
126670010124     C                   Z-ADD     oggi_ggmmaa   G02DAT
126680001010     C                   MOVEL     *BLANK        G02ERR
126690001010     C                   CALL      'XSRDA8'
126700001215     C                   PARM                    WLBDAT
126710010116     C                   Z-ADD     G02INV        oggi_aammgg
126720010907
126730010907     C                   movel     oggi_aammgg   oggi_anno
126740010907     C                   add       1             oggi_anno
126750001016
126760001009     C     Kfnor         klist
126770001011     C                   kfld                    kpoe
126780001016     C                   kfld                    knsr
126790001009     C                   kfld                    knor
126800001016     C                   kfld                    knrv
126810001109
126820001109     C     Kfnor1        klist
126830001109     C                   kfld                    kpoe
126840001109     C                   kfld                    knsr
126850001109     C                   kfld                    knor
126860001018
126870001026     C     Kfnorn        klist
126880001018     C                   kfld                    kpoe
126890001018     C                   kfld                    knsr
126900001018     C                   kfld                    knor
126910001018     C                   kfld                    knrv
126920001026     C                   kfld                    kdai
126930001026     C                   kfld                    kori
126940001018     C                   kfld                    kfar
126950001018     C                   kfld                    kprg
126960001026
126970001026     C     Kfnorn1       klist
126980001026     C                   kfld                    kpoe
126990001026     C                   kfld                    knsr
127000001026     C                   kfld                    knor
127010001026     C                   kfld                    knrv
127020001026     C                   kfld                    kdai
127030001026     C                   kfld                    kori
127040001026     C                   kfld                    kfar
127050070806
127060070806     c     kors          klist
127070070806     c                   kfld                    v1cpos
127080070806     c                   kfld                    v1ors
127090001009
127100010118     C     Ktab          klist
127110010119     C                   kfld                    codut
127120010118     C                   kfld                    kcod
127130010118     C                   kfld                    kkey
127140001010
127150001010     C     Ktntam        klist
127160001010     C                   kfld                    kksc
127170001010     C                   kfld                    kctr
127180010122
127190010122     C     Ktnta1        klist
127200010122     C                   kfld                    kksc
127210010122     C                   kfld                    kctr
127220010122     C                   kfld                    kprg
127230001215
127240001215     C     kazcln        klist
127250001215     C                   kfld                    ktfp
127260001215     C                   kfld                    ktfa
127270001215     C                   kfld                    kann
127280001215     C                   kfld                    kmes
127290001009
127300040426     c     kOrm08        klist
127310040426     c                   kfld                    ds_v1ccra
127320040426     c                   kfld                    inv_v1dar
127330040930
127340040930     c     kCnaco        klist
127350040930     c                   kfld                    codut
127360040930     c                   kfld                    kci
127370040930     c                   kfld                    kksc
127380070123
127390070301     c     kfidst        klist
127400070301     c                   kfld                    kdstnpg
127410080226     c                   kfld                    kdstnfv
127420080226     c                   kfld                    kdstfgs
127430071023
127440071023     c     kacr1         klist
127450071023     c                   kfld                    ds_v1ccra
127460071023     c                   kfld                    num_v1cpor
127470071031     c                   kfld                    kacr1ain
127480090206
127490090206     c     kfnora        klist
127500090206     c                   kfld                    ormpoe
127510090206     c                   kfld                    ormnsr
127520090206     c                   kfld                    ormnor
127530090206     c                   kfld                    ormnrv
127540090206     c                   kfld                    ormfao
127550090206     c                   kfld                    ormdfo
127560090206     c                   kfld                    ormofo
127570150615
127580150615     c                   eval      *in04 = *off
127590040426
127600001009     C                   select
127610010124      * Immissione
127620141212     C                   when      psce = '1' or psce = 'E'
127630001009     C                   movel     mod(1)        v1mod
127640001009     C                   eval      *in01 = *on
127650150615     c                   IF        §RMtla = *blanks
127660150615     c                   eval      *in04 = *on
127670150615     c                   ENDIF
127680010124      * Manutenzione
127690001009     C                   when      psce = '2'
127700001009     C                   movel     mod(2)        v1mod
127710001009     C                   eval      *in02 = *on
127720010124      * Copia
127730001109     C                   when      psce = '3'
127740001109     C                   movel     mod(1)        v1mod
127750010124     C                   eval      *in03 = *on
127760010124      * Conferma
127770010124     C                   when      psce = 'C'
127780090211     C                   movel     mod(7)        v1mod
127790010124     C                   eval      *in02 = *on
127800010124      * Richiamato x immissione
127810001025     C                   when      §RMtla <> *blanks
127820010124     C                             and §RMfpr <> 'P' and §RMfpr <> 'O'
127830001023     C                   movel     mod(1)        v1mod
127840001025     C                   eval      *in01 = *on
127850010124      * Richiamato x manutenzione
127860001106     C                   when      §RMtla <> *blanks
127870001106     C                             and §RMfpr = 'P'
127880001106     C                   movel     mod(2)        v1mod
127890001106     C                   eval      *in02 = *on
127900001009     C                   endsl
127910001009
127920001011     C                   eval      kpoe = ppoe
127930001016     C                   eval      knsr = pnsr
127940001011     C                   eval      knor = pnor
127950001016     C                   eval      knrv = pnrv
127960001011
127970001025     C                   eval      v1cpoe = ppoe
127980001016     C                   eval      v1nsr  = pnsr
127990001025     C                   eval      v1nor  = pnor
128000001025     C                   eval      v1nrv  = pnrv
128010150305
128020150305      * Recupero i dati di default prima con la filiale emissione
128030150305     C                   clear                   ddft
128040150305     C                   clear                   TIBS02DS
128050150305     C                   movel     'C'           T02mod
128060150305     C                   movel     knsif         t02sif
128070150305     C                   movel     'DFT'         t02cod
128080150305     C                   movel(p)  v1cpoe        T02ke1
128090150305     C                   call      'TIBS02R'
128100150305     C                   parm                    KPJBA
128110150305     C                   parm                    TIBS02DS
128120150305     C                   if        t02err =  *blanks
128130150305     C                   movel     t02uni        ddft
128140150305     C                   else
128150150305      * se non tropo provo con 999 generico
128160150305     C                   clear                   TIBS02DS
128170150305     C                   movel     'C'           T02mod
128180150305     C                   movel     knsif         t02sif
128190150305     C                   movel     'DFT'         t02cod
128200150305     C                   movel(p)  '999'         T02ke1
128210150305     C                   call      'TIBS02R'
128220150305     C                   parm                    KPJBA
128230150305     C                   parm                    TIBS02DS
128240150305     C                   if        t02err =  *blanks
128250150305     C                   movel     t02uni        ddft
128260150305     C                   endif
128270150305     C                   endif
128280150305
128290150305      * se i dati relativi al peso e volume sono a zero li imposto a di dft io
128300150305      * caso che non dovrebbe capitare ma se capita nella routine 'pesvol' il
128310150305      * programma si spacca
128320150305     c                   if        d§dftpkg = *zeros
128330150305     c                   eval      d§dftpkg = 200
128340150305     c                   endif
128350150305     c                   if        d§dftbnc = *zeros
128360150305     c                   eval      d§dftbnc = 1
128370150305     c                   endif
128380140318
128390140318      /free
128400140422       //?Carico i Tipi rcd FNORE che NON sono da copiare pari pari da VAOE
128410140422         clear dORE;
128420140422         clear xx;
128430140422         kTBEcod = 'ORE';
128440140422         setll kTBEcod tntbe01l;
128450140422         reade kTBEcod tntbe01l;
128460140422         DOW not %eof(tntbe01l);
128470140422           dORE = TBEUni;
128480140422           IF  §OREvar = 'S';
128490140422             xx += 1;
128500140423             skTRCvao(xx) = %subst(TBEke1:1:2);
128510140422           ENDIF;
128520140422           reade kTBEcod tntbe01l;
128530140422         ENDDO;
128540131022      /end-free
128550001006
128560001006     C                   endsr
128570060203
128580060203      *---------------------------------------------------------------*
128590060203      * Reperimento Dati del job (Utente/Operativi)                   *
128600060203      *---------------------------------------------------------------*
128610060203     c     DatiJob       BEGSR
128620060203      *
128630060203     c     *dtaara       define    §azute        azuteds
128640060203     c     *dtaara       define    §datiute      ddatiute
128650060203      *
128660060203     c                   in(E)     *dtaara
128670060203     c                   IF        %ERROR or RSUT = *blanks
128680060203     c                   clear                   Tibs34Ds
128690060203     c                   call      'TIBS34R'
128700060203     c                   parm                    Tibs34Ds
128710060203     c                   in        *dtaara
128720060203     c                   ENDIF
128730150305
128740150305      * abilitazione utente
128750150305     c                   clear                   wabi
128760150305     c                   eval      dUTE01 = UTEfaf
128770150305     c                   SELECT
128780150305      * carica abilitazioni del profilo
128790150305     c                   WHEN      §UTEorm <> *blanks
128800150305     c                   eval      wabi = §UTEorm
128810150305     c                   WHEN      UTEaut <> *blanks
128820150305     c                   eval      wabi = UTEaut
128830150305      * se non c'è l'abilitazione
128840150305      * --> se 1° livello, abilitazioni al terminal
128850150305      *     se 2° livello, abilitazioni al punto operativo
128860150305      *     se sede no abilitazioni
128870150305     c                   OTHER
128880150305     c                   IF        DUTlpo = '1'
128890150305     c                   eval      wabi   = 'TP'
128900150305     c                   ENDIF
128910150305     c                   IF        DUTlpo = '2'
128920150305     c                   eval      wabi   = 'PO'
128930150305     c                   ENDIF
128940150305     c                   ENDSL
128950150305
128960150305      * Reperimento delle filiali gestibili dall'utente
128970150305     c                   clear                   TRUL31DS
128980150305     c                   eval      I31abi = wabi
128990150305     c                   eval      I31cdi = DUTdis
129000150305     c                   eval      I31car = DUTare
129010150305     c                   eval      I31cpo = DUTpou
129020150305     c                   call      'TRUL31R'
129030150305     c                   parm                    KPJBA
129040150305     c                   parm                    TRUL31DS
129050150305     c                   IF        O31pog > *zeros
129060150305     c                   movea     O31pog        SKpog
129070150305     c                   ENDIF
129080060203      *
129090060203     c                   ENDSR
129100001010      *---------------------------------------------------------------------------------------------
129110001010** MSG  Lungh. 78                                                            *
129120010420Inserire mittente                                                                   1
129130010420Codice mittente non valido                                                          2
129140010213Ordinante non valido                                                                3
129150001010Immettere i colli                                                                   4
129160001010Immettere il peso                                                                   5
129170001010Immettere il volume                                                                 6
129180001010Immettere i bancali                                                                 7
129190001010Immettere il bilico                                                                 8
129200001010Immettere l'autotreno                                                               9
129210001010Immettere la motrice                                                                10
129220070418Inserire Filiale di ritiro                                                          11
129230131016Ora pronta merce maggiore dell'orario di fine servizio                              12
129240001010Tipo ordine non valido                                                              13
129250010202Immettere la cusale di chiusura                                                     14
129260010202Causale chiusura errata                                                             15
129270001010Codice tassazione o codice tariffa errati                                           16
129280070418Filiale consegna errata                                                             17
129290010118Destinatario non valido                                                             18
129300001010Attenzione !! Modificata Località                                                   19
129310001010Attenzione !! Modificato CAP                                                        20
129320001010Attenzione !! Modificata Provincia                                                  21
129330001010Attenzione !! Modificata Nazione                                                    22
129340030609Pagamento a carico dell'ordinante...inserire un codice valido                       23
129350001012Ora errata                                                                          24
129360001012Immettere i colli ------> ENTER x forzare                                           25
129370001012Immettere il peso ------> ENTER x forzare                                           26
129380001012Immettere il volume ------> ENTER x forzare                                         27
129390001012Immettere i bancali ------> ENTER x forzare                                         28
129400001012Immettere il bilico ------> ENTER x forzare                                         29
129410001012Immettere l'autotreno ------> ENTER x forzare                                       30
129420001012Immettere la motrice ------> ENTER x forzare                                        31
129430131016Ritiro no in giornata, superato l'orario di fine servizio                           32
129440070418Filiale ritiro errata                                                               33
129450001016Immettere data ritiro                                                               34
129460001016Data ritiro errata                                                                  35
129470070418La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica        36
129480010205Immettere almeno una quantità (Kg. Volume Bancali)                                  37
129490131107ORM multiplo non possibile se presente il destinatario                              38
129500010118Dati del destinatario incompleti                                                    39
129510010420Non è possibile variare il mittente                                                 40
129520070130ATTENZIONE!! Già avuto esito da PDA. Enter x forzare                                41
129530001103Non è stato modificato nulla                                                        42
129540070913Giro Ritiro errato                                                                  43
129550001215Data chiamata errata                                                                44
129560001215Data chiamata obbligatoria                                                          45
129570131107Orari di apertura incongruenti                                                      46
129580001215Ora chiamata obbligatoria                                                           47
129590010213ORM prepagato non possibile, inserire prima il prepagato!                           48
129600081007ORM IMPORT DPD automatico, non si può chiudere con causale "80"                     49
129610010223Immettere destinatario se ORM prepagato                                             50
129620161108Data ritiro immessa inferiore a quella calcolata.                                   51
129630010126Immettere l'ora del ritiro                                                          52
129640010202Priorità non trovata in tabella                                                     53
129650070913Per ORM prepagato immettere colli e peso                                            54
129660140424Immettere un indirizzo e-mail valido                                                55
129670010420Per ORM prepagato deve pagare il mittente                                           56
129680010215Inserire il referente                                                               57
129690010215Inserire il telefono                                                                58
129700040630Non immettere il codice tariffa se non immesso il codice cliente                    59
129710140424Immettere un numero di telefono valido                                              60
129720010223Immettere il destinatario. Orm singolo con pagamento a carico destinatario          61
129730070418ATTENZIONE!! Filiale ritiro diversa da quella calcolata                             62
129740010906ATTENZIONE!! data ritiro ricalcolata!                                               63
129750030806Causale chiusura non gestibile                                                      64
129760010305Data ritiro inferiore alla data immissione ORM                                      65
129770170303ATTENZIONE!! La data ritiro immessa è un giorno festivo                             66
129780010413ATTENZIONE!! fare F6 x confermare                                                   67
129790010906ATTENZIONE!! Volume troppo alto. Ricalcolare.                                       68
129800010502ATTENZIONE!! Mittente BLOCCATO                                                      69
129810070921ATTENZIONE!! Modificato codice mittente e/o codice cliente tariffa                  70
129820010507ATTENZIONE!! Numeratore ORM impegnato riprovare                                     71
129830010508ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                            72
129840140424Messaggi in sospeso su altre videate                                                73
129850011114Richiesta forzatura del destinatario ma l'ORM non è singolo                         74
129860011114Richiesta forzatura del destinatario ma i dati sono incompleti o errati             75
129870030808Se Mittente e Ordinante sono uguali impostare che paga il Mittente                  76
129880040419Non è possibile dirottare l'ORM                                                     77
129890070418Modificare la filiale ritiro                                                        78
129900070418La filiale ritiro non ha la proc.ORM attiva                                         79
129910040628ORM xxx-xx-xxxxxxx-xx da ritirare lo stesso giorno per lo stesso mittente           80
129920131202ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!        81
129930040727ATTENZIONE!! Ordinante BLOCCATO                                                     82
129940040727ATTENZIONE!! Destinatario BLOCCATO                                                  83
129950050120Pagamento a carico del destinatario...inserire un codice valido                     84
129960050128ATTENZIONE!! Le note immesse sono state sovrascritte                                85
129970050323ATTENZIONE!! Non esiste legame tra il mittente e l'ordinante. VERIFICARE!!!         86
129980050324ATTENZIONE!! ORM PREPAGATO non locale deve essere un ORM commissionato              87
129990050420Prima confermare le proposte di variazione                                          88
130000070418ATTENZIONE!! Filiale ritiro non gestibile                                           89
130010080422ATTENZIONE!! Per ORM commissionati a DPD il ritiro deve essere di 1 collo           90
130020140331Immettere la natura della merce.                                                    91
130030081126ATTENZIONE!! ORM Export DPD peso maggiore del limite consentito                     92
130040081126ATTENZIONE!! ORM Export DPD destinatario obbligatorio                               93
130050110120ATTENZIONE!! Presenti incongurenze fra Cap, Prov, Loc: modificare l'anagrafica      94
130060110610Per ORM Export EEX non è possibile la consegna all'estero                           95
130070110610Per ORM Export DPD non è possibile la consegna a                                    96
130080150615Tipo Comunicazione ORM non valido.                                                  97
130090160329ATTENZIONE!! Il giro è stato variato a seguito delle modifiche effettuate           98
130100010508** MOD  Lungh. 15                                                            *
130110001013  IMMISSIONE                                                                        1
130120001013 MANUTENZIONE                                                                       2
130130001009VISUALIZZAZIONE                                                                     3
130140001013   ANNULLATO                                                                        4
130150001106   CHIUSURA                                                                         5
130160040628 DIROTTAMENTO                                                                       6
130170090211   CONFERMA                                                                         7
130180010704** CM3
130190030611OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('ETICH     ')
130200160229 ovrscope(*calllvl) SHARE(*YES)
130210030623** cmA4
130220030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130230030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130240030623**  cmA5
130250030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130260030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130270131011** TF02
130280131211F2=Dati DPD  #
130290131014** TF06
130300131211F6=Conferma  #
130310131011** TF11
130320131211F11=Volume  #
130330160405** TF13
130340160405F13=Conf.Prenotazione  #
130350131011** TF14
130360131211F14=OrariSr  #
130370131011** TF16
130380131211F16=Chiusura  #
130390131011** TF19
130400131211F19=Proposte  #
130410131011** TF20
130420131211F20=Simulazione  #
130430131011** TF21
130440131211F21=Dirottamento  #
