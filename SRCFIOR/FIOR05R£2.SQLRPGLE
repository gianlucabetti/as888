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
015720100222           clear tisie8ds;
015730100222           isie8dri = oggi_aammgg;
015740100222           isie8op0 = 'D01';
015750100907           isie8nar = v1nar;
015760100222           isie8cap = wcap;
015770100222           tisie8r (kpjba : tisie8ds);
015780100222       //?Per errore riemetto la videata con errore
015790100222           IF  osie8err <> *blanks;
015800100222             *in28 = *on;
015810100907             *in50 = *on;
015820100222             *in62 = *on;
015830100222             v1cmsg = osie8msg;
015840100222             iter;
015850100222           ENDIF;
015860100222       //?Selezionato un cap lo imposto e riemetto la videata
015870100222           IF  osie8capb <> *blanks;
015880100907             v1car = osie8capb;
015890100907             *in50 = *on;
015900100907             *in79 = *off;
015910100222             *in62 = *on;
015920100222             iter;
015930100222           ENDIF;
015940100222       //?Se non è stato selezionato un cap riemetto la videata
015950100222           IF  osie8capb = *blanks;
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
056100161028       //?Passo Data Pronta Merce
056110161028         IOR97dpm = inv_V1dpm;
056120161024
056130161027         fior97r(kpjba : fior97ds : fior971ds);
056140130910         IF  OOR97err = *blanks and OOR97dar <> *zeros;
056150130910           inv_newV1dar = OOR97dar;
056160130910           dataiso = %date(inv_newV1dar:*iso);
056170130910           dataeur = dataiso;
056180130910           new_V1dar = %dec(dataeur);
056190130910         ENDIF;
056200131015
056210131015       //?Imposto 'A' a video se si può anticipare
056220131015         clear v1anticipa;
056230170524         IF  (*in01 or *in03) and OOR97dmin < OOR97dar;
056240131015           v1anticipa = 'A';
056250131015         ENDIF;
056260170524       //?In immissione o copia mi salvo anche la data ritiro calcolata
056270170524       //?e se ORM da posticipare
056280170525       //?e se posso posticipare
056290170524         IF  *in01 or *in03;
056300170524           wOkPosticipa = *off;
056310170525           wAnticipa = *off;
056320170524           IF  OOR97postd = 'S';
056330170524             wOkPosticipa = *on;
056340170524           ENDIF;
056350170524           DarCalcolata = OOR97dar;
056360170525           wAnticipa = (OOR97dmin < OOR97dar);
056370170524         ENDIF;
056380131107
056390131107       //?Mi memorizzo che ho già richiamato il calcolo della data
056400131107         wokOR97 = *on;
056410161026
056420161026       //?Se ho data pronta merce maggiore data ritiro e sto confermando da VAS
056430161026       //?metto la data pronta merce come data ritiro
056440170524       //?da VAS non si anticipa
056450161026         IF  §RMfpr = 'C' and inv_newv1dar < inv_v1dpm;
056460161026           old_invV1dar = inv_v1dpm;
056470161026           inv_newV1dar = inv_v1dpm;
056480161026           new_V1dar = v1dpm;
056490170524           clear v1anticipa;
056500161026         ENDIF;
056510130910
056520130910      /end-free
056530130910
056540040615     c                   EndSr
056550001017      **********************************************************************
056560001017      * CONTROLLI     * ora ritiro *
056570001017      **********************************************************************
056580001017     C     Sr_Controrr   begsr
056590001017
056600010117     C                   if        v1orr <> *zeros
056610010117     C                   movel     v1orr         ore
056620010117     C                   move      v1orr         minuti
056630001017
056640071107     C                   if        ore > 23 or minuti > 59
056650010220     C                             or ore < 1
056660001017     C                   seton                                        58  28
056670001017     C                   movel     msg(24)       v1cmsg
056680130917     c                   leavesr
056690001017     C                   endif
056700131111     C                   ELSE
056710140320
056720140320      /free
056730131106       //?Se immissione/copia ORM commissionato non c'è obbligo dell'ora
056740131106         IF  (*in01 or *in03) and V1com = 'S';
056750131106         ELSE;
056760131106           *in28 = *on;
056770131106           *in58 = *on;
056780131106           V1Cmsg = msg(52);
056790131106           leavesr;
056800131106         ENDIF;
056810131106      /end-free
056820131111
056830131111     c                   ENDIF
056840001017
056850001017     C                   endsr
056860001103      **********************************************************************
056870070913      * CONTROLLI     * priorità
056880001103      **********************************************************************
056890001103     C     Sr_Contrsto   begsr
056900001103
056910010123     C                   clear                   v1dsto
056920010123
056930010123      * Decodifica priorità ORM
056940010202     C                   if        v1sto <> *zeros
056950010202     C                   clear                   TIBS02DS
056960010202     C                   move      *all'0'       priorita
056970010202     C                   movel     'C'           T02mod
056980010202     C                   movel     knsif         t02sif
056990010202     C                   movel     'STO'         t02cod
057000010202     C                   move      v1sto         priorita
057010010202     C                   movel(p)  priorita      T02ke1
057020010202     C                   call      'TIBS02R'
057030010202     C                   parm                    KPJBA
057040010202     C                   parm                    TIBS02DS
057050010202     C                   if        t02err <> *blanks
057060131030     C                   movel     msg(53)       w2cmsg
057070010202     C                   seton                                        84  28
057080010202     C                   goto      endsrsto
057090010202     C                   endif
057100010202     C                   movel     t02ke1        priorita
057110010202     C                   move      priorita      v1sto
057120010202     C                   movel     t02uni        v1dsto
057130010202     C                   endif
057140001103
057150001103     C     endsrsto      endsr
057160010223      **********************************************************************
057170010223      * CONTROLLI     * Referente e telefono *
057180010223      **********************************************************************
057190010223     C     Sr_Contrref   begsr
057200010223
057210131106      * Per orm prepagato obbligatori tel. e ref.
057220131106     C                   if        v1ctor = 'P'
057230030610      * anche per ORM commissionati
057240030610     c                             or V1com = 'S'
057250010223     C                   if        v1rer = *blanks
057260010223     C                   movel     msg(57)       v1cmsg
057270010223     C                   seton                                        86  28
057280010223     C                   goto      endsrcref
057290010223     C                   endif
057300010223     C                   if        v1ter = *blanks
057310010223     C                   movel     msg(58)       v1cmsg
057320010223     C                   seton                                        87  28
057330010223     C                   goto      endsrcref
057340010223     C                   endif
057350010223     C                   endif
057360010223
057370010223     C     endsrcref     endsr
057380010202      **********************************************************************
057390010202      * CONTROLLI     * pagamento *
057400010202      **********************************************************************
057410010202     C     Sr_Contrpag   begsr
057420010202
057430010202      * Se non inserito prendo di dft
057440010202     C                   if        v1pag = *blanks
057450010202     C                   eval      v1pag = d§dftpag
057460010308     C                   eval      sav_v1pag = d§dftpag
057470010514     C                   endif
057480070912      * controllo che sia valido
057490070912      * nel caso di dato sbagliato che arriva da FNAVO e non viene toccato il campo
057500070912     c                   if        v1pag <> 'M' and v1pag <> 'O' and
057510070912     c                             v1pag <> 'D'
057520070912     c                   eval      v1cmsg = 'Pagamento errato'
057530070912     c                   eval      *in28 = *on
057540070912     c                   eval      *in53 = *on
057550070912     c                   leavesr
057560070912     c                   endif
057570011113      * Se prepagato obbligatorio che paghi il mittente
057580081216      * se non è un ritiro all'estero
057590010213     C                   if        v1ctor = 'P' and v1pag <> 'M'
057600081216     c                             and v1nar = *blanks
057610010213     C                   movel     msg(56)       v1cmsg
057620010213     C                   seton                                        53  28
057630010213     C                   goto      endsrcpag
057640010213     C                   endif
057650010223      * Se singolo e paga il destinatario quest'ultimo è obbligatorio
057660010223     C                   if        v1ctor = 'S' and v1pag = 'D'
057670010223     C                   eval      ds_v1crc1 = v1crc1
057680010223     C                   eval      ds_v1crc2 = v1crc2
057690010223     C                   eval      ds_v1crc3 = v1crc3
057700010223     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
057710010223     C                   movel     msg(61)       v1cmsg
057720010223     C                   seton                                        56  28
057730010223     C                   goto      endsrcpag
057740010223     C                   endif
057750050121      * il codice destinatario non può essere un 8888 o 9999
057760050121     C                   If        ds_v1ccrc > *zeros and
057770050121     c                             (ds_v1crc2 = 9999 or ds_v1crc2 = 8888)
057780050120     C                   movel     msg(84)       v1cmsg
057790050120     C                   seton                                        56  28
057800050120     C                   goto      endsrcpag
057810050120     C                   endif
057820010223     C                   endif
057830030808      * Se inserito che paga l'ordinante
057840030808      * l'ordinante deve essere codificato e non può esserre un 8888 o 9999
057850030808      * se mittente e ordinante sono uguali obbligatorio che paghi mittente
057860010202     C                   eval      ds_v1cor1 = v1cor1
057870010202     C                   eval      ds_v1cor2 = v1cor2
057880010202     C                   eval      ds_v1cor3 = v1cor3
057890030808     c                   Eval      ds_V1Cra1 = V1Cra1
057900030808     c                   eval      ds_V1Cra2 = V1Cra2
057910030808     c                   eval      ds_V1Cra3 = V1Cra3
057920030808     C                   if        v1pag = 'O'
057930030808     C                   If        (ds_v1ccor = *zeros or
057940030606     c                             (ds_v1cor2 = 9999 or ds_v1cor2 = 8888))
057950010202     C                   movel     msg(23)       v1cmsg
057960010202     C                   seton                                        42  28
057970010202     C                   goto      endsrcpag
057980010202     C                   endif
057990030808     c                   EndIF
058000010507
058010010507     C  nkf              eval      sav_v1ksc = v1ksc
058020010202
058030010202     C     endsrcpag     endsr
058040010202      **********************************************************************
058050010202      * CONTROLLI     * cod.cliente e tariffa *
058060010202      **********************************************************************
058070010202     C     Sr_Contrksc   begsr
058080010202
058090040930      * Controllo se il codice cliente esiste su CNACO
058100040930if  1c                   If        v1ksc <> *Zeros
058110040930     c                   z-add     v1ksc         kksc
058120040930     c     kCnaco        Chain     Cnaco00f
058130040930if  2c                   If        Not %Found(Cnaco00f) or
058140040930     c                             AcoFlg <> *Blanks
058150040930     c                   Eval      v1cmsg = msg(16)
058160040930     c                   Seton                                        54  28
058170040930     c                   Goto      endsrcksc
058180040930    2c                   EndIf
058190040930      * Controllo il network del p.o. del cliente non deve essere LED o SEDE
058200040930     c                   Movel     v1ksc         kazorg
058210040930     c                   Clear                   Og143
058220040930     c     kazorg        Chain     Azorg01l
058230040930     c                   If        %Found(Azorg01l)
058240040930     c                   Eval      Og143 = Orgde3
058250040930     c                   EndIf
058260040930if  1c                   If        §OgNtw = 'LOG' or §OgNtw = 'XXX'
058270040930     c                   Eval      v1cmsg = msg(16)
058280040930     c                   Seton                                        54  28
058290040930     c                   Goto      endsrcksc
058300040930e   2c                   EndIf
058310040930e   1c                   EndIf
058320010202      * Se codice tariffa inserito deve essere un numero
058330010202     C                   if        v1ctr <> *blanks  and
058340010320     C                             (%subst(v1ctr:1:1) < '0'  and
058350010320     C                              %subst(v1ctr:1:1) <> ' ') or
058360010320     C                             (%subst(v1ctr:2:1) < '0'  and
058370010320     C                              %subst(v1ctr:2:1) <> ' ') or
058380010320     C                             (%subst(v1ctr:3:1) < '0'   and
058390010320     C                              %subst(v1ctr:3:1) <> ' ')
058400010202     C                   movel     msg(16)       v1cmsg
058410010202     C                   seton                                        85  28
058420010202     C                   goto      endsrcksc
058430010202     C                   endif
058440040630      * non posso inserire solo la tariffa senza il codice cliente
058450040630     C                   if        v1ksc = *zeros and v1ctr <> *blanks
058460040630     C                   movel     msg(59)       v1cmsg
058470040630     C                   seton                                        85  28
058480040630     C                   goto      endsrcksc
058490040630     C                   endif
058500010202      * Controlla validità codice + tariffa
058510010202     C                   if        v1ksc <> *zeros and v1ctr <> *blanks
058520010202     C                   z-add     v1ksc         kksc
058530010202     C                   movel     v1ctr         kctr
058540090128     c                   eval      $notam = *off
058550090128     c     ktntam        setll     tntam01l
058560090128      * se non trovo errore
058570090128     c                   if        not %equal(tntam01l)
058580090128     c                   eval      v1cmsg = msg(16)
058590090128     c                   seton                                        54  28
058600090128     c                   leavesr
058610090128     c                   endif
058620090128     c                   do        *hival
058630090128     c     ktntam        Reade     Tntam01l
058640040713     C                   if        %Eof(tntam01l)
058650090128     C                   leavesr
058660010202     C                   endif
058670090128      * se annullato leggo altro record
058680010202     C                   if        TAMatb <> *blanks
058690090128     C                   iter
058700010202     C                   endif
058710090128      * trovato almeno 1 rcd ok
058720090128     c                   eval      $notam = *on
058730090128     c                   enddo
058740090128      * se non ho trovato nessuna tariffa errore
058750090128     c                   if        not $notam
058760090128     c                   eval      v1cmsg = msg(16)
058770090128     c                   seton                                        54  28
058780090128     c                   leavesr
058790090128     c                   endif
058800010202     C                   endif
058810010202
058820010202     C     endsrcksc     endsr
058830010223      **********************************************************************
058840010223      * CONTROLLI     * flag ddt *
058850010223      **********************************************************************
058860010223     C     Sr_Contrddt   begsr
058870010223
058880021119      * Da default immissione ORM
058890010223     C                   if        v1ddt = *blanks
058900010223     C                   eval      v1ddt = d§dftddt
058910010223     C                   endif
058920010223
058930010223     C                   endsr
058940010202      **********************************************************************
058950010202      * CONTROLLI     * P.O. consegna *
058960010202      **********************************************************************
058970010202     C     Sr_Contrpoc   begsr
058980010202
058990010320     C                   clear                   v1dpoc
059000060929     c                   clear                   c_og143
059010010320
059020010202     C                   if        v1cpoc <> *blanks
059030010202      * Ricerca P.O.Consegna con "?"
059040010202     C     '?'           scan      v1cpoc                                 30
059050010202     C     *in30         ifeq      *on
059060010202     C                   eval      *in55 = *on
059070010202     C                   eval      *in90 = *on
059080010202     C                   clear                   ds_tnsd
059090010202     C                   call      'TNSD24R'
059100010202     C                   parm                    ds_cod
059110010202     C                   parm                    ds_tip
059120010202     C                   parm                    ds_des
059130010202     C                   parm                    ds_pra
059140010202     C                   movel     ds_cod        v1cpoc
059150010202     C                   movel     ds_des        v1dpoc
059160010202     C                   endif
059170010316     C                   endif
059180010316      * Solo campi numerico
059190010320     C                   if        v1cpoc <> *blanks and
059200010320     C                             (%subst(v1cpoc:1:1) < '0' and
059210010320     C                              %subst(v1cpoc:1:1) <> ' ') or
059220010320     C                             (%subst(v1cpoc:2:1) < '0' and
059230010320     C                              %subst(v1cpoc:2:1) <> ' ') or
059240010320     C                             (%subst(v1cpoc:3:1) < '0' and
059250010320     C                              %subst(v1cpoc:3:1) <> ' ')
059260010316     C                   seton                                        55  28
059270010316     C                   movel     msg(17)       v1cmsg
059280010316     C                   goto      endsrcpoc
059290010316     C                   endif
059300010316      * Controllo
059310010202     C                   if        v1cpoc <> *blanks
059320010202     C                   movel     v1cpoc        kazorg
059330010202     C     kazorg        chain     azorg01L
059340010316     C                   if        not%found(azorg01l)
059350010316     C                   movel     msg(17)       v1cmsg
059360010316     C                   seton                                        55  28
059370010316     C                   goto      endsrcpoc
059380010316     C                   endif
059390010202     C                   if        ORGfva <> *blanks or
059400010202     C                             (ORGfag <> 'A' and ORGfag <> 'F')
059410010202     C                   movel     msg(17)       v1cmsg
059420010202     C                   seton                                        55  28
059430010202     C                   goto      endsrcpoc
059440010202     C                   endif
059450010202     C                   movel     ORGdes        v1dpoc
059460060929     c                   eval      c_og143 = orgde3
059470010202     C                   endif
059480111221
059490111221      /free
059500111221       //?Forzo la filiale di consegna in base alla tabella FFC
059510120116       //?se il destinatario è estero
059520150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
059530120116       IF  V1nac <> *blanks;
059540120116         IF  V1Cpoe = *zeros;
059550150305           w_V1Cpoe = DUTpou;
059560120116         ELSE;
059570120116           w_V1Cpoe = V1Cpoe;
059580120116         ENDIF;
059590120116         //?Provo con la filiale dell'ordinante se presente
059600120116         IF  ds_V1Ccor > 0;
059610120116           clear TIBS02ds;
059620120116           clear dFFC;
059630120116           T02mod = 'C';
059640120116           T02cod = 'FFC';
059650120116           T02ke1 = %editc(ds_V1Cor1:'X');
059660120116           T02sif = KNSIF;
059670120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
059680120116           IF  T02err = *blanks;
059690120116             dFFC = T02uni;
059700120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
059710120116               V1Cpoc = %editc(§FFCpoc:'X');
059720120116             ENDIF;
059730120116             leavesr;
059740120116           ENDIF;
059750120116         ENDIF;
059760120116         //?Se non trovo con Filiale Ordinante provo con Filiale Emissione
059770120116         //?se diversa da Filiale Ordinante
059780120116         IF  w_V1Cpoe <> ds_V1Cor1;
059790120116           clear TIBS02ds;
059800120116           clear dFFC;
059810120116           T02mod = 'C';
059820120116           T02cod = 'FFC';
059830120116           T02ke1 = %editc(w_V1Cpoe:'X');
059840120116           T02sif = KNSIF;
059850120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
059860120116           IF  T02err = *blanks;
059870120116             dFFC = T02uni;
059880120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
059890120116               V1Cpoc = %editc(§FFCpoc:'X');
059900120116             ENDIF;
059910120116             leavesr;
059920120116           ENDIF;
059930120116         ENDIF;
059940111221       ENDIF;
059950111221      /end-free
059960010202
059970010202     C     endsrcpoc     endsr
059980010202      **********************************************************************
059990010202      * CONTROLLI     * cliente destinatario *
060000010202      **********************************************************************
060010010202     C     Sr_Contrclid  begsr
060020110610
060030110610     c                   eval      $cacgdd = *off
060040010202
060050010202     C                   eval      ds_v1crc1 = v1crc1
060060010202     C                   eval      ds_v1crc2 = v1crc2
060070010202     C                   eval      ds_v1crc3 = v1crc3
060080010223
060090010223      * Se orm prepagato deve esserci il destinatario
060100010223     C                   if        v1ctor = 'P' and
060110010223     C                             (ds_v1ccrc = *zeros and v1rsc = *blanks)
060120010223     C                   movel     msg(50)       v1cmsg
060130010223     C                   seton                                        56  28
060140010223     C                   goto      endsrclid
060150010223     C                   endif
060160131107      /free
060170131107       //?Se sono in immissione/copia e sto inserendo il destinatario
060180131107       //?se il tipo ORM è impostato a Multiplo lo aggiorno in automatico
060190131107       //?in Singolo in modo da non dare il messaggio di errore
060200131107         IF  (*in01 or *in03) and
060210131107             ((sav_V1Ccrc <> ds_V1Ccrc and sav_V1Ccrc = 0) or
060220131107             (sav_V1rsc <> V1rsc and sav_V1rsc = *blanks)) and
060230131107             V1Ctor = 'M';
060240131107           V1Ctor = 'S';
060250131107         ENDIF;
060260131107      /end-free
060270131107
060280010223      * Se orm multiplo non deve esserci il destinatario
060290010223     C                   if        v1ctor = 'M' and
060300010223     C                             (ds_v1ccrc > *zeros or v1rsc <> *blanks)
060310010223     C                   movel     msg(38)       v1cmsg
060320160316     c                   eval      H1riga = 05
060330160316     c                   eval      H1colo = 11
060340160316     C                   seton                                            28
060350011114     C                   setoff                                       0616
060360010223     C                   goto      endsrclid
060370010223     C                   endif
060380011114
060390011114      * Se blocco destinatario l'ORM deve essere singolo e i dati del
060400011114      * destinatario devono essere completi
060410011114     C                   if        §ormfd = 'S'
060420011114     C                   if        v1ctor <> 'S' and *in38 = *off
060430011114     C                   movel     msg(74)       v1cmsg
060440160316     c                   eval      H1riga = 05
060450160316     c                   eval      H1colo = 11
060460160316     C                   seton                                          3828
060470011114     C                   setoff                                       0616
060480011114     C                   goto      endsrclid
060490011114     C                   endif
060500011114     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
060510011114     C                             and *in39 = *off
060520011114     C                   movel     msg(75)       v1cmsg
060530011114     C                   seton                                        563928
060540011114     C                   setoff                                       0616
060550011114     C                   goto      endsrclid
060560011114     C                   endif
060570011114     C                   endif
060580010223
060590010202      * Inserito codice
060600010202     C                   if        ds_v1ccrc <> *zeros
060610010202     C                   eval      *in06 = *on
060620010202     C                   exsr      Sr_Contrcrc
060630010202     C                   eval      sav_v1ccrc = ds_v1ccrc
060640010202     C                   else
060650010202      * Inserita ragione sociale
060660010202     C                   clear                   sav_v1ccrc
060670010202     C                   exsr      Sr_Contrrsc
060680131107     c                   eval      sav_V1rsc = V1rsc
060690010202     C                   eval      *in06 = *off
060700010202     C                   endif
060710110120     c   28              leavesr
060720011119      * Se ORM con blocco destinatario faccio già i controlli
060730011119     C                   if        §ormfd = 'S' and
060740011119     C                             (ds_v1ccrc <> *zeros or v1rsc <> *blanks)
060750011119     C                   eval      *in16 = *on
060760011119     C                   endif
060770060929
060780060929      * se oltre al destinatario è stato inserito anche il p.o. di consegna
060790060929      * devo fare un ulteriore controllo
060800060929      * ai fini dell'orm non serve, ma serve per impedire l'instradamento sbagliato
060810060929      * al momento della creazione della bolla legata a questo ORM
060820110120     c                   if        v1cpoc<> *blank and v1rsc <> *blanks
060830060929     c                   exsr      sr_contrcapc
060840060929     c                   endif
060850100811
060860100811      /free
060870100811       //?Se ORM import DPD devo controllare che la nazione di destino sia
060880100907       //?servita da DPD --> chiodo DPD come ntw.
060890150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
060900130107         IF  V1Cpoe = *zeros;
060910150305           w_V1Cpoe = DUTpou;
060920130107         ELSE;
060930130107           w_V1Cpoe = V1Cpoe;
060940130107         ENDIF;
060950130107         IF  %lookup(w_V1Cpoe:skpodpd) > 0 and v1nac <> *blanks and
060960101102             O14lad <= 0;
060970100909           v1cmsg = 'Nazione destino non servita da DPD. +
060980100909                     Richiedere al Depot l''annullamento';
060990100907           *in28 = *on;
061000100907           *in65 = *on;
061010100907           *in75 = *on;
061020100811         ENDIF;
061030160208
061040160209       //?Se inserito destinatario
061050160209         IF  ds_V1Ccrc > 0 or V1rsc <> *blanks;
061060160209         //?Se c'è la filiale di consegna uso quella
061070160209           IF  V1Cpoc > *zeros;
061080160209             O95lna = %dec(V1Cpoc:3:0);
061090160209           ELSE;
061100160209         //?Se non c'è la filiale di consegna
061110160209         //?richiamo routine sr_contrcapc per il calcolo della LNA
061120160209             exsr sr_contrcapc;
061130160209           ENDIF;
061140160209         //?Se ORM Prepagato e consegna a lna 340-341-345 ORM non possibile vedi DV 1540
061150160209           IF  (O95lna = 340 or O95lna = 341 or O95lna = 345) and
061160160209                V1Ctor = 'P';
061170160209             TBLcod = '15';
061180160209             TBLkey = v1nac;
061190160209             chain (CodUt:TBLcod:TBLkey) TABEL00F;
061200160209             IF  %found(TABEL00F);
061210160209               ds15 = TBLuni;
061220160209             ENDIF;
061230160209             V1Cmsg = 'Per consegne in';
061240160209             V1Cmsg = %trim(V1Cmsg) + ' ' + §15des;
061250160209             V1Cmsg = %trim(V1Cmsg) +
061260160209                      ' no pagamento a carico del Mittente';
061270160209             *in28 = *on;
061280160209             *in65 = *on;
061290160209             *in75 = *on;
061300160209           ENDIF;
061310160209         ENDIF;
061320160208
061330100811      /end-free
061340010202
061350010202     C     endsrclid     endsr
061360010202      **********************************************************************
061370010202      * CONTROLLI     * cliente destinatario *
061380010202      **********************************************************************
061390010202     C     Sr_Contrcrc   begsr
061400010202
061410010202     C                   z-add     ds_v1ccrc     kfnacr
061420151109     C     kfnacr        chain(n)  fnacr01l
061430010202     C                   if        %found(fnacr01l)
061440010202     C                   if        ACRatb <> *blanks
061450010202     C                   movel     msg(18)       v1cmsg
061460010202     C                   seton                                        56  28
061470010202     C                   goto      endsrccrc
061480010202     C                   endif
061490040727     C                   if        ACRtcr = 'M'
061500040727     C                   movel     msg(83)       v1cmsg
061510040727     C                   seton                                        56  28
061520040727     C                   goto      endsrccrc
061530040727     C                   endif
061540010906
061550010906      * Se modificato il cliente pulisco il ksc
061560010906     C                   if        v1pag = 'D'
061570010906     C                             and (ds_v1ccrc <> sav_v1ccrc
061580031010     C                             or v1pag <> sav_v1pag
061590040630     c                             or v1ksc = *Zeros
061600040630     c                             or V1Ctr = *Blanks)
061610031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocrc mi carica il ksc
061620040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
061630031010     c                   Clear                   sav_V1pag
061640031010     c                   EndIf
061650010906     C                   clear                   v1ksc
061660010906     C                   clear                   v1ctr
061670010906     C                   endif
061680010202     C                   exsr      Sr_Decocrc
061690010202     C                   else
061700010202     C                   movel     msg(18)       v1cmsg
061710010202     C                   seton                                        56  28
061720010202     C                   endif
061730010202
061740010202     C     endsrccrc     endsr
061750010202      **********************************************************************
061760011113      * DECODIFICHE     * cliente destinatario *
061770010202      **********************************************************************
061780010202     C     Sr_Decocrc    begsr
061790010202
061800010202     C                   movel     ACRrsr        v1rsc
061810010202     C                   movel     ACRinr        v1inc
061820010202     C                   movel     ACRcar        v1cac
061830010202     C                   movel     ACRlor        v1loc
061840030922     C                   movel     ACRlor        sav_v1loc35
061850010202     C                   movel     ACRprr        v1prc
061860010202     C                   movel     ACRnar        v1nac
061870010207      * Se paga destinatario memorizzo il codice piano dei conti
061880010207     C                   if        v1pag = 'D'
061890010906     C                             and (ds_v1ccrc <> sav_v1ccrc
061900010906     C                             or v1pag <> sav_v1pag)
061910010906     C                             and  v1ksc = *zeros
061920050120     c                   If        AcrKsc > *Zeros
061930010207     C                   eval      v1ksc = ACRksc
061940010903     C  nkf              eval      sav_v1ksc  = ACRksc
061950010309     C                   eval      sav_v1pag = v1pag
061960040629      * memorizzo anche il codice tariffa
061970040629     c                   If        AcrCcc = 999
061980040629     c                   Clear                   V1Ctr
061990040629     c                   Else
062000040629     c                   Move      AcrCcc        V1Ctr
062010040629     c                   EndIf
062020050120     c                   Else
062030050120     c                   Eval      V1Ksc = ds_v1crc12
062040050120     c                   EndIf
062050010207     C                   endif
062060010202
062070010202     C                   endsr
062080010202      **********************************************************************
062090011113      * CONTROLLI     * dati cliente destinatario *
062100010202      **********************************************************************
062110010202     C     Sr_Contrrsc   begsr
062120030922
062130030930     c                   If        V1loc = *Blanks or
062140030930     c                             (v1loc <> sav_v1loc)
062150030929     c                   Clear                   sav_v1loc35
062160030929     c                   EndIf
062170030922     c                   If        sav_v1loc35 = *blanks
062180030922     c                   Movel(p)  v1loc         sav_v1loc35
062190030922     c                   EndIf
062200131202
062210131202      /free
062220131202         IF  V1loc <> sav_V1loc or V1cac <> sav_V1cac or V1nac <> sav_V1nac;
062230131202           wForzaNAC = *off;
062240131202         ENDIF;
062250131202      /end-free
062260010202
062270010202     C                   clear                   FNLV14DS
062280010202     C                   z-add     *date         I14dri
062290010202     C                   movel     v1rsc         I14rsc
062300010202     C                   movel     v1inc         I14ind
062310030922     C                   movel(p)  sav_v1loc35   E14loc
062320010202     C                   movel     v1prc         E14prv
062330010202     C                   movel     v1cac         E14cap
062340010202     C                   movel     v1nac         E14nar
062350161109      /free
062360161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
062370161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
062380161109       //?il pgm di interrogazione cappario
062390161109         IF  ConfAuto;
062400161109           I14batch = 'S';
062410161109         ENDIF;
062420161109      /end-free
062430010202
062440010202     C                   call      'FNLV14R'
062450010202     C                   parm                    KPJBA
062460010202     C                   parm                    FNLV14DS
062470010202
062480010202     C                   movel     I14ind        v1inc
062490010202     C                   movel     E14loc        v1loc
062500030922     C                   movel     E14loc        sav_v1loc35
062510010202     C                   movel     E14prv        v1prc
062520010202     C                   movel     E14cap        v1cac
062530010202     C                   movel     E14nar        v1nac
062540010202
062550010202     C                   if        v1rsc <> *blanks or v1inc <> *blanks
062560010202     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
062570010202     C                   eval      *in61 = (O14err = '2')                       Indirizzo
062580010202     C                   eval      *in62 = (O14err = '5')                       CAP
062590010202     C                   eval      *in63 = (O14err = '3')                       Località
062600010202     C                   eval      *in64 = (O14err = '4')                       Provincia
062610010202     C                   eval      *in65 = (O14err = '6')                       Nazione
062620010202
062630010202     C                   if        O14msg <> *blanks
062640010202     C                   movel(p)  O14msg        v1cmsg
062650010202     C                   eval      *IN28 = *ON
062660010202     C                   eval      *in75 = *on
062670010202     C                   goto      endsrcrsc
062680010202     C                   endif
062690010202     C                   endif
062700010202
062710010202      * Controlla indirizzo
062720010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062730010202     C                             v1prc <> *blanks or v1nac <> *blanks
062740010202     C                   exsr      Sr_Contrcapc
062750010202     C                   eval      *in75 = (*in28 = *on)
062760010202     C   28              goto      endsrcrsc
062770010202     C                   endif
062780010202
062790010202      * Controlla se dati completi o parziali
062800010202     C                   if        v1rsc = *blanks
062810010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062820010202     C                             v1prc <> *blanks or v1nac <> *blanks
062830010202     C                   movel     msg(39)       v1cmsg
062840010202     C                   eval      *IN28 = *ON
062850010202     C                   eval      *in56 = *on
062860010202     C                   goto      endsrcrsc
062870010202     C                   endif
062880010202     C                   endif
062890131202      /free
062900131202       //?Se inserita la nazione e questa è lunga 2 byte e presente in tabella PR-provincia
062910131202       //?devo avvisare l'utente se è giusta la nazione o se per caso voleva inserire la
062920131202       //?provincia. Caso di nazione 'VE' accettata in immissione ORM perchè esiste la
062930131202       //?nazione VE Venezuale ma l'utente voleva inserire VE Venezia, ha sbagliato campo
062940131202         IF  V1rsc <> *blanks and V1nac <> *blanks and
062950131202            ((%subst(V1nac:1:1) = *blanks and %subst(V1nac:2:2) <> *blanks) or
062960131202             (%subst(V1nac:1:2) <> *blanks and %subst(V1nac:3:1) = *blanks));
062970131202           TBLcod = 'PR';
062980131202           TBLkey = v1nac;
062990131202           chain (CodUt:TBLcod:TBLkey) TABEL00F;
063000131202           IF  %found(TABEL00F) and TBLflg = *blanks and not wForzaNAC;
063010131202             TBLcod = '15';
063020131202             TBLkey = v1nac;
063030131202             chain (CodUt:TBLcod:TBLkey) TABEL00F;
063040131202             IF  %found(TABEL00F);
063050131202               ds15 = TBLuni;
063060131202             ENDIF;
063070131202             wForzaNAC = *on;
063080131202             sav_V1nac = V1nac;
063090131202             *in28 = *on;
063100131202             *in65 = *on;
063110131202             *in75 = *on;
063120131202             V1Cmsg = 'Verificare la nazione del destinatario';
063130131202             V1Cmsg = %trim(V1Cmsg) + ' ' +
063140131202                      %trim(V1nac) + '-' +
063150131202                      %trim(§15des);
063160131202             leavesr;
063170131202           ENDIF;
063180131202         ENDIF;
063190131202      /end-free
063200010207
063210010207      * Se paga il destinatario devo pulire il codice cliente che paga
063220010329     C                   if        v1pag = 'D'
063230010507     C                             and (v1pag <> sav_v1pag
063240010917     C                             or   sav_v1ccrc <> ds_v1ccrc)
063250010207     C                   clear                   v1ksc
063260010906     C                   clear                   v1ctr
063270010903     C  nkf              clear                   sav_v1ksc
063280010329     C                   eval      sav_v1pag = v1pag
063290010207     C                   endif
063300010202
063310010202     C     endsrcrsc     endsr
063320010202      **********************************************************************
063330010202      * CONTROLLI     * cap - località - provincia *
063340010202      **********************************************************************
063350010202     C     Sr_Contrcapc  begsr
063360010202
063370010202     C                   eval      *in32 = *off
063380010202
063390010202     C                   clear                   TISI95DS
063400010202     C                   clear                   FNLV13DS
063410080423     c                   clear                   tisi97ds
063420010202     C                   movel     ' '           I95tla
063430010202     C                   movel     '7'           I95tcn
063440010202     C                   movel     v1cac         I95cap
063450010202     C                   movel     v1nac         I95nar
063460010202     C                   movel     v1inc         I95ind
063470030922     C                   movel(p)  sav_v1loc35   I95loc
063480010202     C                   movel     v1prc         I95prv
063490010202     C                   movel     'S'           I13af0
063500010202     C                   movel     'S'           I13af1
063510010202     C                   movel     'S'           I13sz2
063520010202     C                   movel     'S'           I13la3
063530010202     C                   movel     'S'           I13sz3
063540010202     C                   movel     'S'           I13cnv
063550101215     c                   if        v1pag<>'M'
063560101214     c                   eval      i95tpo='A'
063570101214     c                   endif
063580010202
063590010202      * Gestione errori forzabili: se cambiato CAP/Località
063600010202      * riazzera i valori di errori forzabili
063610080910
063620080910      * CAP
063630080910     C     v1cac         ifne      sav_v1cac
063640080910     C                   movel     v1cac         sav_v1cac
063650080910     C                   clear                   wfzv
063660080910     C                   clear                   wfz1
063670080910     C                   clear                   wfz2
063680080910     C                   clear                   wfz3
063690080910     C                   eval      *in32 = *on
063700080910     C                   endif
063710010202
063720010202      * Modifica località
063730010202
063740010202     C     v1loc         ifne      sav_v1loc
063750010202     C                   clear                   wfz1
063760010202     C                   clear                   wfz2
063770010202     C                   clear                   wfz3
063780010202     C                   movel     v1loc         sav_v1loc
063790010202     C                   eval      *in32 = *on
063800010202     C                   endif
063810010202
063820010202      * Modifica provincia
063830010202
063840010202     C     v1prc         ifne      sav_v1prc
063850010202     C                   clear                   wfz3
063860010202     C                   movel     v1prc         sav_v1prc
063870010202     C                   eval      *in32 = *on
063880010202     C                   endif
063890010202
063900010202     C                   movel     wfzv          E13fzv
063910010202     C                   movel     wfz1          E13fz1
063920010202     C                   movel     wfz2          E13fz2
063930010202     C                   movel     wfz3          E13fz3
063940060929
063950060929     c                   eval      i97ntw = c_§ogntw
063960010202
063970010202     C                   call      'FNLV13R'
063980010202     C                   parm                    KPJBA
063990010202     C                   parm                    FNLV13DS
064000010202     C                   parm                    TISI95DS
064010060929     c                   parm      ' '           flgbac            1
064020060929     c                   parm                    tisi97ds
064030010202
064040010202      * Ricontrolla errori
064050010202
064060010202     C                   movel     O13err        werr
064070010202     C                   movel     O13msg        wmsg
064080010202     C                   movel     E13fzv        wfzv
064090010202     C                   movel     E13fz1        wfz1
064100010202     C                   movel     E13fz2        wfz2
064110010202     C                   movel     E13fz3        wfz3
064120010202
064130010202      * Se il livello di affidabilità non è inferiore a 2 e
064140010202      * il livello di reperimento dati non è inferiore a 3
064150010202      * reimposta i dati da TISI95DS
064160110120      * I dati non vengono sostituiti se i dati provengono da anagrafica
064170010202
064180110120     c                   if        ds_v1ccrc=0
064190010202     C     O13ron        ifeq      'S'
064200110120     C                   movel     O95nar        v1nac
064210110120     c                   endif
064220110120     C     O13roc        ifeq      'S'
064230110120     C                   movel     O95cap        v1cac
064240110120     c                   endif
064250110120     C     O13rop        ifeq      'S'
064260110120     C                   movel     O95prv        v1prc
064270110120     c                   endif
064280110120     C     O13rol        ifeq      'S'
064290010202     C                   movel     O95loc        v1loc
064300030922     C                   movel     O95loc        sav_v1loc35
064310010202     C                   end
064320161109
064330161109      /free
064340161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
064350161109       //?conferma automatica batch non devo dare errore
064360161109         IF  ConfAuto and O13roc = 'S';
064370161109           clear werr;
064380161109           clear O13roc;
064390161109         ENDIF;
064400161109      /end-free
064410010202
064420010202     C     werr          ifne      ' '
064430010202     C     wmsg          andne     *blanks
064440080910     C     *in32         oreq      *on
064450010202
064460010202     C                   select
064470010202
064480010202     C     werr          wheneq    '3'
064490010202     C                   eval      *in63 = *on
064500010202     C                   eval      *in28 = *on
064510010202
064520010202     C     werr          wheneq    '5'
064530010202     C                   eval      *in62 = *on
064540010202     C                   eval      *in28 = *on
064550010202
064560010202     C     werr          wheneq    '4'
064570010202     C                   eval      *in64 = *on
064580010202     C                   eval      *in28 = *on
064590010202
064600010202     C     werr          wheneq    '6'
064610010202     C                   eval      *in65 = *on
064620010202     C                   eval      *in28 = *on
064630010202     C                   endsl
064640010202
064650010202     C                   movel(p)  wmsg          v1cmsg
064660010202     C                   end
064670110120     c                   else
064680110120     C     werr          ifne      ' '
064690110120     C     wmsg          andne     *blanks
064700110120     C                   eval      *in56 = *on
064710110120     C                   eval      *in28 = *on
064720110120     C                   movel(p)  msg(94)       v1cmsg
064730110120     c                   endif
064740110120     c                   endif
064750010202
064760010202     C   28              goto      endsrcapc
064770010202
064780010202      * Controlla se reimpostata località
064790010202
064800010202     C     O13rol        ifeq      'S'
064810010202     C                   eval      *in63 = *on
064820010202     C                   eval      *in28 = *on
064830010202     C                   movel     msg(19)       v1cmsg
064840010202     C                   goto      endsrcapc
064850010202     C                   end
064860010202
064870010202      * Controlla se reimpostata Provincia
064880010202
064890010202     C     O13rop        ifeq      'S'
064900010202     C                   eval      *in64 = *on
064910010202     C                   eval      *in28 = *on
064920010202     C                   movel     msg(21)       v1cmsg
064930010202     C                   goto      endsrcapc
064940010202     C                   end
064950010202
064960010202      * Controlla se reimpostato cap
064970010202
064980010202     C     O13roc        ifeq      'S'
064990010202     C                   eval      *in62 = *on
065000010202     C                   eval      *in28 = *on
065010010202     C                   movel     msg(20)       v1cmsg
065020010202     C                   goto      endsrcapc
065030010202     C                   end
065040010202
065050010202      * Controlla se reimpostata Nazione
065060010202
065070010202     C     O13ron        ifeq      'S'
065080010202     C                   eval      *in65 = *on
065090010202     C                   eval      *in28 = *on
065100010202     C                   movel     msg(22)       v1cmsg
065110010202     C                   goto      endsrcapc
065120010202     C                   end
065130110610
065140110610      /free
065150110610       //?Mi salvo se il cap del destinatario prevede l'obbligo DDT
065160110610         $cacgdd = (O95gdd = 'S');
065170110610      /end-free
065180110610
065190010202
065200010202     C     endsrcapc     endsr
065210010208      **********************************************************************
065220010208      * CONTROLLI     * cliente ordinante *
065230010208      **********************************************************************
065240010208     C     Sr_Contrclio  begsr
065250010208
065260010208     C                   eval      ds_v1cor1 = v1cor1
065270010208     C                   eval      ds_v1cor2 = v1cor2
065280010208     C                   eval      ds_v1cor3 = v1cor3
065290160413      /free
065300160413       //?Se è stato variato il codice ordinante
065310160413       //?in caso di immissione ORM manuale a mittente non codificato
065320160413       //?devo reimpostare i dati per la conferma prenotazione
065330160413         IF  sav_V1Ccor <> ds_V1Ccor and
065340160413             (*in01 or *in03) and
065350160413             (V1Ctco = 'M' or V1Ctco = 'E') and
065360160413             ds_V1Ccra = 0;
065370160413           wOkF6F13 = *off;
065380160413           wRicConf = *off;
065390160413           clear W04sms;
065400160413           clear W04mail;
065410160413         ENDIF;
065420160413      /end-free
065430010208      * Inserito codice
065440010208     C                   if        ds_v1ccor <> *zeros
065450010208     C                   eval      *in08 = *on
065460010208     C                   exsr      Sr_Contrcor
065470010208     C                   eval      sav_v1ccor = ds_v1ccor
065480010208     C                   else
065490010208      * Inserita ragione sociale
065500010208     C                   clear                   sav_v1ccor
065510010208     C                   exsr      Sr_Contrrso
065520010208     C                   eval      *in08 = *off
065530010208     C                   endif
065540010208
065550010702     C                   endsr
065560010208      **********************************************************************
065570010208      * CONTROLLI     * cliente ordinante *
065580010208      **********************************************************************
065590010208     C     Sr_Contrcor   begsr
065600010208
065610010208     C                   z-add     ds_v1ccor     kfnacr
065620151109     C     kfnacr        chain(n)  fnacr01l
065630010208     C                   if        not%found(fnacr01l)
065640010208     C                   movel     msg(3)        v1cmsg
065650010208     C                   seton                                        42  28
065660010208     C                   goto      endsrccor
065670010208     C                   endif
065680010208     C                   if        ACRatb <> *blanks
065690010208     C                   movel     msg(3)        v1cmsg
065700010208     C                   seton                                        42  28
065710010208     C                   goto      endsrccor
065720010208     C                   endif
065730040727     C                   if        ACRtcr = 'M'
065740040727     C                   movel     msg(82)       v1cmsg
065750040727     C                   seton                                        42  28
065760040727     C                   goto      endsrccor
065770040727     C                   endif
065780010906      * Se modificato il cliente pulisco il ksc
065790010906     C                   if        v1pag = 'O'
065800010906     C                             and (ds_v1ccor <> sav_v1ccor
065810031010     C                             or v1pag <> sav_v1pag
065820040630     c                             or   v1ksc = *Zeros
065830040630     c                             or   V1Ctr = *Blanks)
065840031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocro mi carica il ksc
065850040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
065860031010     c                   Clear                   sav_V1pag
065870031010     c                   EndIf
065880010906     C                   clear                   v1ksc
065890010906     C                   clear                   v1ctr
065900010906     C                   endif
065910010208     C                   exsr      Sr_Decocor
065920010208
065930010208     C     endsrccor     endsr
065940010208      **********************************************************************
065950010208      * DECODIFICHE     * cliente ordinante *
065960010208      **********************************************************************
065970010208     C     Sr_Decocor    begsr
065980010208
065990010208     C                   movel     ACRrsr        v1rso
066000010208     C                   movel     ACRinr        v1ino
066010010208     C                   movel     ACRcar        v1cao
066020010208     C                   movel     ACRlor        v1loo
066030030922     C                   movel     ACRlor        sav_v1loo35
066040010208     C                   movel     ACRprr        v1pro
066050010208     C                   movel     ACRnar        v1nao
066060010208      * Se paga ordinante memorizzo il codice piano dei conti
066070030902      * se non c'è il ksc memorizzo i primi 7 byte del codice anagrafica ritiro
066080010208     C                   if        v1pag = 'O'
066090010906     C                             and (ds_v1ccor <> sav_v1ccor
066100010906     C                             or v1pag <> sav_v1pag)
066110010906     C                             and  v1ksc = *zeros
066120030902     c                   If        AcrKsc > *Zeros
066130010208     C                   eval      v1ksc = ACRksc
066140040629      * memorizzo anche il codice tariffa
066150040629     c                   If        AcrCcc = 999
066160040629     c                   Clear                   V1Ctr
066170040629     c                   Else
066180040629     c                   Move      AcrCcc        V1Ctr
066190040629     c                   EndIf
066200030902     c                   Else
066210030902     c                   Eval      V1Ksc = Ds_V1cor12
066220030902     c                   EndIf
066230030902     C  nkf              eval      sav_v1ksc  = V1ksc
066240010309     C                   eval      sav_v1pag = v1pag
066250010208     C                   endif
066260030606      * lo salvo comunque xchè mi può servire nel caso di creazione
066270030606      * bolla x orm commissionato
066280030606     c                   Eval      AcrKscCor = AcrKsc
066290050202     c                   Eval      AcrCccCor = AcrCcc
066300040405
066310040405      * recupero le note del cliente ordinante
066320040405      * se il cliente ritiro non è codificato
066330040405if  1c                   if        ds_v1ccor <> sav_v1ccor and ds_v1ccra = *Zeros
066340040726      * solo se non sono già state immesse
066350040405     c                   If        v1not1 = *Blanks
066360040405     c                   Eval      V1not1 = AcrNo1
066370040405     c                   EndIf
066380040405     c                   If        v1not2 = *Blanks
066390040405     c                   Eval      V1not2 = AcrNo2
066400040405     c                   EndIf
066410040405e   1c                   EndIf
066420050321
066430050321      * imposto se ORM commissionato o meno
066440050323     c                   If        *In01
066450050322     c                             and (sav_v1ccor <> ds_v1ccor or
066460050322     c                                  v1com = *blanks)
066470050321     c                   Eval      v1com = AcrFcc
066480050322     c                   EndIf
066490010208
066500010208     C                   endsr
066510010208      **********************************************************************
066520010208      * CONTROLLI     * dati cliente ordinante *
066530010208      **********************************************************************
066540010208     C     Sr_Contrrso   begsr
066550030922
066560030930     c                   If        V1loo = *Blanks or
066570030930     c                             (v1loo <> sav_v1loo)
066580030929     c                   Clear                   sav_v1loo35
066590030929     c                   EndIf
066600030922     c                   If        sav_v1loo35 = *blanks
066610030922     c                   Movel(p)  v1loo         sav_v1loo35
066620030922     c                   EndIf
066630010208
066640010208     C                   clear                   FNLV14DS
066650010208     C                   z-add     *date         I14dri
066660010208     C                   movel     v1rso         I14rsc
066670010208     C                   movel     v1ino         I14ind
066680030922     C                   movel(p)  sav_v1loo35   E14loc
066690010208     C                   movel     v1pro         E14prv
066700010208     C                   movel     v1cao         E14cap
066710010208     C                   movel     v1nao         E14nar
066720161109      /free
066730161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
066740161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
066750161109       //?il pgm di interrogazione cappario
066760161109         IF  ConfAuto;
066770161109           I14batch = 'S';
066780161109         ENDIF;
066790161109      /end-free
066800010208
066810010208     C                   call      'FNLV14R'
066820010208     C                   parm                    KPJBA
066830010208     C                   parm                    FNLV14DS
066840010208
066850010208     C                   movel     I14ind        v1ino
066860010208     C                   movel     E14loc        v1loo
066870030922     C                   movel     E14loc        sav_v1loo35
066880010208     C                   movel     E14prv        v1pro
066890010208     C                   movel     E14cap        v1cao
066900010208     C                   movel     E14nar        v1nao
066910010220
066920010220     C                   if        v1rso <> *blanks or v1ino <> *blanks
066930010220     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
066940010220     C                   eval      *in61 = (O14err = '2')                       Indirizzo
066950010220     C                   eval      *in62 = (O14err = '5')                       CAP
066960010220     C                   eval      *in63 = (O14err = '3')                       Località
066970010220     C                   eval      *in64 = (O14err = '4')                       Provincia
066980010220     C                   eval      *in65 = (O14err = '6')                       Nazione
066990010220
067000010220     C                   if        O14msg <> *blanks
067010010220     C                   movel(p)  O14msg        v1cmsg
067020010220     C                   eval      *IN28 = *ON
067030010220     C                   eval      *in74 = *on
067040010220     C                   goto      endsrcrso
067050010220     C                   endif
067060010220     C                   endif
067070010208
067080010208      * Controlla indirizzo
067090010208     C                   if        v1cao <> *blanks or v1loo <> *blanks or
067100010208     C                             v1pro <> *blanks or v1nao <> *blanks
067110010208     C                   exsr      Sr_Contrcapo
067120010208     C                   eval      *in74 = (*in28 = *on)
067130010208     C   28              goto      endsrcrso
067140010208     C                   endif
067150010208      * Se paga l'ordinante devo pulire il codice cliente che paga
067160010329     C                   if        v1pag = 'O'
067170010507     C                             and (v1pag <> sav_v1pag
067180010917     C                             or   sav_v1ccor <> ds_v1ccor)
067190010208     C                   clear                   v1ksc
067200010906     C                   clear                   v1ctr
067210010507     C  nkf              clear                   sav_v1ksc
067220010329     C                   eval      sav_v1pag = v1pag
067230010208     C                   endif
067240010208
067250010208     C     endsrcrso     endsr
067260010208      **********************************************************************
067270010208      * CONTROLLI     * cap - località - provincia *
067280010208      **********************************************************************
067290010208     C     Sr_Contrcapo  begsr
067300010208
067310010208     C                   eval      *in32 = *off
067320010208
067330010208     C                   clear                   TISI95DS
067340010208     C                   clear                   FNLV13DS
067350010208     C                   movel     ' '           I95tla
067360010208     C                   movel     '7'           I95tcn
067370010208     C                   movel     v1cao         I95cap
067380010208     C                   movel     v1nao         I95nar
067390010208     C                   movel     v1ino         I95ind
067400030922     C                   movel(p)  sav_v1loo35   I95loc
067410010208     C                   movel     v1pro         I95prv
067420010208     C                   movel     'S'           I13af0
067430010208     C                   movel     'S'           I13af1
067440010208     C                   movel     'S'           I13sz2
067450010208     C                   movel     'S'           I13la3
067460010208     C                   movel     'S'           I13sz3
067470010208     C                   movel     'S'           I13cnv
067480010208
067490010906      * Gestione errori forzabili: se cambiato CAP/Località
067500010208      * riazzera i valori di errori forzabili
067510010208
067520080910      * CAP
067530080910     C     v1cao         ifne      sav_v1cao
067540080910     C                   movel     v1cao         sav_v1cao
067550080910     C                   clear                   wfzv
067560080910     C                   clear                   wfz1
067570080910     C                   clear                   wfz2
067580080910     C                   clear                   wfz3
067590080910     C                   eval      *in32 = *on
067600080910     C                   endif
067610010208
067620010208      * Modifica località
067630010208
067640010208     C     v1loo         ifne      sav_v1loo
067650010208     C                   clear                   wfz1
067660010208     C                   clear                   wfz2
067670010208     C                   clear                   wfz3
067680010208     C                   movel     v1loo         sav_v1loo
067690010208     C                   eval      *in32 = *on
067700010208     C                   endif
067710010208
067720010208      * Modifica provincia
067730010208
067740010208     C     v1pro         ifne      sav_v1pro
067750010208     C                   clear                   wfz3
067760010208     C                   movel     v1pro         sav_v1pro
067770010208     C                   eval      *in32 = *on
067780010208     C                   endif
067790010208
067800010208     C                   movel     wfzv          E13fzv
067810010208     C                   movel     wfz1          E13fz1
067820010208     C                   movel     wfz2          E13fz2
067830010208     C                   movel     wfz3          E13fz3
067840010208
067850010208     C                   call      'FNLV13R'
067860010208     C                   parm                    KPJBA
067870010208     C                   parm                    FNLV13DS
067880010208     C                   parm                    TISI95DS
067890010208
067900010208      * Ricontrolla errori
067910010208
067920010208     C                   movel     O13err        werr
067930010208     C                   movel     O13msg        wmsg
067940010208     C                   movel     E13fzv        wfzv
067950010208     C                   movel     E13fz1        wfz1
067960010208     C                   movel     E13fz2        wfz2
067970010208     C                   movel     E13fz3        wfz3
067980010208
067990010208      * Se il livello di affidabilità non è inferiore a 2 e
068000010208      * il livello di reperimento dati non è inferiore a 3
068010010208      * reimposta i dati da TISI95DS
068020010208
068030010208     C     O13ron        ifeq      'S'
068040110120     C                   movel     O95nar        v1nao
068050110120     c                   endif
068060110120     C     O13roc        ifeq      'S'
068070110120     C                   movel     O95cap        v1cao
068080110120     c                   endif
068090110120     C     O13rop        ifeq      'S'
068100110120     C                   movel     O95prv        v1pro
068110110120     c                   endif
068120110120     C     O13rol        ifeq      'S'
068130010208     C                   movel     O95loc        v1loo
068140030922     C                   movel     O95loc        sav_v1loo35
068150010208     C                   end
068160161109
068170161109      /free
068180161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
068190161109       //?conferma automatica batch non devo dare errore
068200161109         IF  ConfAuto and O13roc = 'S';
068210161109           clear werr;
068220161109           clear O13roc;
068230161109         ENDIF;
068240161109      /end-free
068250010208
068260010208     C     werr          ifne      ' '
068270010208     C     wmsg          andne     *blanks
068280080910     C     *in32         oreq      *on
068290010208
068300010208     C                   select
068310010208
068320010208     C     werr          wheneq    '3'
068330010208     C                   eval      *in63 = *on
068340010208     C                   eval      *in28 = *on
068350010208
068360010208     C     werr          wheneq    '5'
068370010208     C                   eval      *in62 = *on
068380010208     C                   eval      *in28 = *on
068390010208
068400010208     C     werr          wheneq    '4'
068410010208     C                   eval      *in64 = *on
068420010208     C                   eval      *in28 = *on
068430010208
068440010208     C     werr          wheneq    '6'
068450010208     C                   eval      *in65 = *on
068460010208     C                   eval      *in28 = *on
068470010208     C                   endsl
068480010208      *
068490010208     C                   movel(p)  wmsg          v1cmsg
068500010208     C                   end
068510010208
068520010208     C   28              goto      endsrcapo
068530010208
068540010208      * Controlla se reimpostata località
068550010208
068560010208     C     O13rol        ifeq      'S'
068570010208     C                   eval      *in63 = *on
068580010208     C                   eval      *in28 = *on
068590010208     C                   movel     msg(19)       v1cmsg
068600010208     C                   goto      endsrcapo
068610010208     C                   end
068620010208
068630010208      * Controlla se reimpostata Provincia
068640010208
068650010208     C     O13rop        ifeq      'S'
068660010208     C                   eval      *in64 = *on
068670010208     C                   eval      *in28 = *on
068680010208     C                   movel     msg(21)       v1cmsg
068690010208     C                   goto      endsrcapo
068700010208     C                   end
068710010208
068720010208      * Controlla se reimpostato cap
068730010208
068740010208     C     O13roc        ifeq      'S'
068750010208     C                   eval      *in62 = *on
068760010208     C                   eval      *in28 = *on
068770010208     C                   movel     msg(20)       v1cmsg
068780010208     C                   goto      endsrcapo
068790010208     C                   end
068800010208
068810010208      * Controlla se reimpostata Nazione
068820010208
068830010208     C     O13ron        ifeq      'S'
068840010208     C                   eval      *in65 = *on
068850010208     C                   eval      *in28 = *on
068860010208     C                   movel     msg(22)       v1cmsg
068870010208     C                   goto      endsrcapo
068880010208     C                   end
068890010208
068900010208     C     endsrcapo     endsr
068910050323
068920050323      *--------------------------------------------------------------------*
068930050323      * CONTROLLO PADRE/FIGLI ordinante con codice ritiro
068940050323      *--------------------------------------------------------------------*
068950050323     c     Sr_Figli      BegSr
068960050323
068970050323      * controllo se l'ordinante è un papà
068980050323     c                   Clear                   Tibs10ds
068990050323     c                   Eval      d10tle = 'WW'
069000050323     c                   Eval      d10paf = 'F'
069010050323     c                   Move      ds_v1cor12    d10cod
069020050323     c                   Eval      d10drf = *date
069030050323     c                   Call      'TIBS10R'
069040050323     c                   Parm                    Tibs10ds
069050050323if  1c                   If        d10err <> *Blanks
069060050323      * controllo se il codice inserito è un figlio
069070050323     c                   Clear                   Tibs10ds
069080050323     c                   Eval      d10tle = 'WW'
069090050323     c                   Eval      d10paf = 'P'
069100050323     c                   Move      ds_v1cor12    d10cod
069110050323     c                   Eval      d10drf = *date
069120050323     c                   Call      'TIBS10R'
069130050323     c                   Parm                    Tibs10ds
069140050323if  2c                   If        d10err <> *Blanks
069150050323     c                   Clear                   skfigli
069160050323e   2c                   EndIf
069170050323e   1c                   EndIf
069180050323
069190050323     c                   EndSr
069200050323
069210001016      **********************************************************************
069220001016      * CONTROLLI     * P.O. ritiro *
069230001016      **********************************************************************
069240001016     C     Sr_Contrpor   begsr
069250131024
069260010413      * Controllo il p.o. di ritiro
069270010413     C                   clear                   v1dpor
069280010202
069290010413      * Ricerca P.O.Ritiro  con "?"
069300010413     C     '?'           scan      v1cpor                                 30
069310010413     C     *in30         ifeq      *on
069320010413     C                   eval      *in59 = *on
069330010413     C                   eval      *in90 = *on
069340010413     C                   clear                   ds_tnsd
069350010413     C                   call      'TNSD24R'
069360010413     C                   parm                    ds_cod
069370010413     C                   parm                    ds_tip
069380010413     C                   parm                    ds_des
069390010413     C                   parm                    ds_pra
069400010413     C                   movel     ds_cod        v1cpor
069410010413     C                   movel     ds_des        v1dpor
069420010413     C                   endif
069430001023
069440010316      * Solo campi numerico
069450010320     C                   if        v1cpor = *blanks and
069460010320     C                             (%subst(v1cpor:1:1) < '0' and
069470010320     C                              %subst(v1cpor:1:1) <> ' ') or
069480010320     C                             (%subst(v1cpor:2:1) < '0' and
069490010320     C                              %subst(v1cpor:2:1) <> ' ') or
069500010320     C                             (%subst(v1cpor:3:1) < '0' and
069510010320     C                              %subst(v1cpor:3:1) <> ' ')
069520010316     C                   seton                                        59  28
069530010316     C                   movel     msg(33)       v1cmsg
069540010316     C                   goto      endsrcpor
069550010316     C                   endif
069560010202      * Obbligatorio
069570010315     C                   if        v1cpor = *blanks
069580010202     C                   movel     msg(11)       v1cmsg
069590010202     C                   seton                                        59  28
069600010202     C                   goto      endsrcpor
069610010202     C                   endif
069620060109
069630060109      * Se immissione ORM e p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
069640060109      * come p.o. ritiro OMR
069650060110     c                   If        (*In01 or *In03 ) and wforzapor = *Off
069660060109     c                   Clear                   fnlv55ds
069670060109     c                   Eval      d55tpt = '6'
069680060109     c                   Move      v1cpor        d55lin
069690060109     c                   Eval      d55drf = oggi_aammgg
069700060109     c                   Call      'FNLV55R'
069710060109     c                   Parm                    Fnlv55ds
069720060109     c                   If        d55err = *Blanks
069730060109     c                   Move      d55tfa        v1cpor
069740060109     c                   Eval      wforzapor = *On
069750060109     c                   EndIf
069760060109     c                   EndIf
069770151203
069780151203      /free
069790151203       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
069800151203       //?solo se immissione/copia ma no conferma ritiri fissi
069810151203       //?o manutenzione dopo dirottamento
069820151203         IF  ((*in01 or *in03) and §RMtco <> 'S') or
069830151203              (*in09 and wokdirotta = *on);
069840151203           clear FIOR96ds;
069850151203       //?se la filiale emissione non è ancora stata impostata passo la filiale utente
069860151203           IF  V1Cpoe = *zeros;
069870151203             IOR96poe = DUTpou;
069880151203           ELSE;
069890151203             IOR96poe = V1Cpoe;
069900151203           ENDIF;
069910151203           IOR96por = %dec(V1Cpor:3:0);
069920151203           fior96r(kpjba : fior96ds);
069930151203           IF  OOR96err = *blanks and OOR96por <> *zeros;
069940151203             V1Cpor = %editc(OOR96por:'X');
069950151203           ENDIF;
069960151203         ENDIF;
069970151203      /end-free
069980060109
069990010202      * Controllo
070000131112     c                   clear                   sav_ORGfl1
070010131210     c                   clear                   ntw_V1Cpor
070020001016     C                   movel     v1cpor        kazorg
070030001016     C     kazorg        chain     azorg01L
070040010202     C                   if        not%found(azorg01l)
070050010202     C                   movel     msg(33)       v1cmsg
070060010202     C                   seton                                        59  28
070070010202     C                   goto      endsrcpor
070080010202     C                   endif
070090001204     C                   if        ORGfva <> *blanks  or
070100001204     C                             (ORGfag <> 'A' and ORGfag <> 'F')
070110001017     C                   movel     msg(33)       v1cmsg
070120001016     C                   seton                                        59  28
070130001019     C                   goto      endsrcpor
070140001019     C                   endif
070150131112     c                   eval      sav_ORGfl1 = ORGfl1
070160080812     C                   movel     orgde3        og143
070170010206     C                   movel     ORGde8        og148
070180131210     c                   eval      ntw_V1Cpor = §OGntw
070190010206      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
070200010206     C                   if        §ogorm <> 'S'
070210010206     C                   movel     msg(36)       v1cmsg
070220010206     C                   seton                                        59  28
070230010206     C                   goto      endsrcpor
070240010206     C                   endif
070250131024
070260131024     C                   movel     ORGdes        v1dpor
070270131024     C                   movel     v1cpor        num_v1cpor
070280131024
070290131024      /free
070300150310       //?Imposto se filiale ritiro ha la procedura PDA attiva
070310150310         *in22 = (§ogpdaorm <> *blanks);
070320131024      /end-free
070330131024
070340060110      * Se il p.o. immesso a video è un p.o. gestito
070350060110      * errore per p.o. non gestibile
070360060110     c                   Clear                   fnlv55ds
070370060110     c                   Eval      d55tpt = '6'
070380060110     c                   Move      v1cpor        d55lin
070390060110     c                   Eval      d55drf = oggi_aammgg
070400060110     c                   Call      'FNLV55R'
070410060110     c                   Parm                    Fnlv55ds
070420060110     c                   If        d55err = *Blanks and d55tfa <> d55lin
070430060110     c                   movel     msg(89)       v1cmsg
070440060110     c                   seton                                        59  28
070450060110     c                   goto      endsrcpor
070460060110     c                   EndIf
070470080812
070480080812      * controllo se ritiro all'estero possibile
070490131030     c                   if        ntw_V1Cpor = 'DPD' or ntw_V1Cpor = 'EEX' or
070500131030     c                             ntw_V1Cpor = 'FED'
070510080812     c                   clear                   fnlv12ds
070520111206     c                   clear                   tisi95ds1
070530080812     c                   clear                   tisi97ds
070540111206     c                   eval      ai95nar = v1nar
070550111206     c                   eval      ai95cap = v1car
070560111206     c                   eval      ai95dat = oggi_aammgg
070570131030     c                   eval      i97ntw = ntw_V1Cpor
070580081215      * se impostato network DPD lo passo
070590081215     c                   if        v1ntwd <> *blanks
070600111206     c                   eval      ai95fi1 = v1ntwd
070610081215     c                   else
070620111206     c                   clear                   ai95fi1
070630081215     c                   endif
070640080812     c                   call      'FNLV12R'
070650080812     c                   parm                    fnlv12ds
070660111206     c                   parm                    tisi95ds1
070670080812     c                   parm                    tisi97ds
070680080812     c                   if        o12err <> *blanks
070690080812     c                   eval      v1cmsg = o12msg
070700080812     c                   seton                                        59  28
070710080812     c                   goto      endsrcpor
070720080812     c                   endif
070730080812     c                   endif
070740081215      * controllo congruenza tra network DPD e po ritiro
070750081215      * in immissione o copia
070760131030     c                   if        v1ntwd <> *blanks and ntw_V1Cpor <> 'DPD'
070770081215     c                             and (*in01 or *in03)
070780081215     c                   eval      *in28 = *on
070790081215     c                   eval      *in59 = *on
070800081215     c                   eval      v1cmsg = 'Indicato Network DPD. Incongruenza-
070810081215     c                              con la filiale ritiro'
070820081215     c                   goto      endsrcpor
070830081215     c                   endif
070840100219
070850100219      /free
070860100219       //?Se inserito ntw DPD richiamo il controllo del cappario DPD
070870110216       //?oppure filiale ritiro è DPD
070880131030       IF  v1ntwd <> *blanks or ntw_V1Cpor = 'DPD';
070890100811         wNazione = v1nar;
070900100811         wCapDPD  = v1car;
070910100811         exsr CtrCapDPD;
070920100811         IF  *in28;
070930100811           *in50 = *on;
070940100811           *in79 = *on;
070950100811           v1cmsg = %trim(v1cmsg) + ' F15 x interrogazione Cappario DPD';
070960100811           leavesr;
070970100811         ENDIF;
070980100219       ENDIF;
070990100219      /end-free
071000080812
071010010515      * Se p.o. estero controllare se DPD o EUROEXPRESS
071020010515     C                   eval      *in14 = *off
071030010515     C                   eval      *in15 = *off
071040131112     C                   if        sav_ORGfl1 = 'E'
071050010515     C                   eval      *in14 = *on
071060010515     C                   endif
071070131030     C                   if        ntw_V1Cpor = 'DPD'
071080010515     C                   eval      *in15 = *on
071090010515     C                   endif
071100010417
071110010213      * Se il p.o. ritiro è della mia £6 l'ORM non può essere prepagato
071120010213      * a meno che non sia richiamato da programma immissione prepagati
071130150609      * in immissione ORM
071140010704     C                   if        v1ctor = 'P' and §RMfpr <> 'B'
071150150609     C                             and §RMfpr <> 'C'
071160010307     C     num_v1cpor    lookup    l6                                     30
071170010307     C                   if        *in30 = *on
071180150609     C                             and *in01
071190010307     C                   movel     msg(48)       v1cmsg
071200160316     c                   eval      H1riga = 05
071210160316     c                   eval      H1colo = 11
071220160316     C                   seton                                            28
071230010307     C                   goto      endsrcpor
071240010307     C                   endif
071250010307     C                   endif
071260010417
071270010417     C                   if        %subst(v1cpor:1:1) = ' '  and
071280010417     C                             %subst(v1cpor:2:1) = ' '
071290010417     C                   movel     '00'          v1cpor
071300010417     C                   endif
071310010417     C                   if        %subst(v1cpor:1:1) = ' '
071320010417     C                   movel     '0'           v1cpor
071330010417     C                   endif
071340010417
071350010417     C                   if        v1cpor <> sav_v1cpor
071360010417     C                   eval      *in89 = *off
071370161116     c                   IF        sav_v1cpor <> *zeros and
071380161116     c                             sav_v1cpor <> *blanks
071390170613     c                             and wokdirotta = *off
071400161116     c                   eval      wModPor = *on
071410161116     c                   ENDIF
071420010417     C                   movel     v1cpor        sav_v1cpor
071430071023     c                   clear                   v1ccgi
071440071023     c                   clear                   sav_v1ccgi
071450071023     c                   clear                   sav_acrcgi
071460010417     C                   endif
071470010515
071480010521      * Se p.o. ritiro Euroexpress o DPD obbligatorio il peso e
071490010521      * i colli
071500010521     C                   if        *in14 = *on or *in15 = *on
071510010521     C                   if        v1pkg = *zeros
071520010515     C                   movel     msg(5)        v1cmsg
071530010515     C                   seton                                        44  28
071540010515     C                   goto      endsrcpor
071550010521     C                   endif
071560010521     C                   if        v1ncl = *zeros
071570010521     C                   movel     msg(4)        v1cmsg
071580010521     C                   seton                                        43  28
071590010521     C                   goto      endsrcpor
071600010521     C                   endif
071610081216      * no prepagato e no pagamento a carico del mittente
071620081216     c                   if        v1ctor = 'P'
071630160316     c                   eval      H1riga = 05
071640160316     c                   eval      H1colo = 11
071650081216     c                   eval      *in28 = *on
071660081216     c                   eval      v1cmsg = 'Per ORM Export non si può fare un -
071670081216     c                             prepagato'
071680081216     c                   goto      endsrcpor
071690081216     c                   endif
071700081216     c                   if        v1pag = 'M'
071710081216     c                   eval      *in28 = *on
071720081216     c                   eval      *in53 = *on
071730081216     c                   eval      v1cmsg = 'Per ORM Export non può pagare il -
071740081216     c                             mittente'
071750081216     c                   goto      endsrcpor
071760081216     c                   endif
071770010515     C                   endif
071780080422
071790080422      * se filiale ritiro DPD controllo i colli
071800080422      * deve essere obbligatoriamente 1
071810080422     c                   if        *in15 = *on and v1ncl > 1
071820080422     c                   eval      v1cmsg = msg(90)
071830080422     c                   eval      *in28 = *on
071840080422     c                   eval      *in43 = *on
071850080422     c                   leavesr
071860080422     c                   endif
071870081126      * se orm export DPD il peso non può essere maggiore del limite stabilito
071880081126     c                   if        *in15 and v1pkg > §3ipkd
071890081126     c                   eval      v1cmsg = msg(92)
071900081126     c                   eval      *in28 = *on
071910081126     c                   eval      *in44 = *on
071920081126     c                   leavesr
071930081126     c                   endif
071940081126      * se orm export DPD obbligatorio il destinatario
071950081127     c                   if        *in15 and ds_v1ccrc = *zeros and
071960081126     c                             v1rsc = *blanks
071970081126     c                   eval      v1cmsg = msg(93)
071980081126     c                   eval      *in28 = *on
071990081211     c                   eval      *in56 = *on
072000081126     c                   leavesr
072010081126     c                   endif
072020010417
072030010417      * Controllo p.o. ritiro del video con quello calcolato
072040020523      * non in chiusura orm
072050050711      * non se p.o. ritiro protetto o se è richiamato da programma
072060050711      * immissione prepagati
072070060109      * se non forzato da me
072080150921      * non controllo se conferma automatica VAS
072090150921     c                   IF        not ConfAuto
072100050711     c                   If        Not *In09 or §rmfpr = 'B'
072110060109     c                   if        wforzapor = *Off
072120010417     C                   if        v1cpos <> *zeros     and
072130010417     C                             v1cpos <> num_v1cpor and
072140020523     C                             *in89 = *off and *in10 = *off
072150010417     C                   movel     msg(62)       v1cmsg
072160010417     C                   seton                                        895928
072170010417     C                   goto      endsrcpor
072180010417     C                   endif
072190010417     C                   if        sav_acrpoa <> *zeros     and
072200010417     C                             sav_acrpoa <> num_v1cpor and
072210150921     C                             *in89 = *off and not *in10
072220010417     C                   movel     msg(62)       v1cmsg
072230010417     C                   seton                                        895928
072240010417     C                   goto      endsrcpor
072250010417     C                   endif
072260131111     C                   if        sav_O95lna <> *zeros       and
072270131111     C                             sav_O95lna <> num_v1cpor   and
072280010417     C                             sav_acrpoa = *zeros    and
072290150921     C                             V1Cpos = *zeros    and
072300150921     C                             *in89 = *off and not *in10
072310010417     C                   movel     msg(62)       v1cmsg
072320010417     C                   seton                                        895928
072330010417     C                   goto      endsrcpor
072340010417     C                   endif
072350060109     c                   EndIf
072360040420     c                   EndIf
072370150921     c                   ENDIF
072380010417
072390010417     C                   movel     v1cpor        sav_v1cpor
072400071031
072410071031      * recupero gli ambiti per il recupero del giro da ACR
072420071031     c                   exsr      sr_caragr
072430010301
072440001016     C     endsrcpor     endsr
072450070920
072460070913      *------------------------------------------------------------------------*
072470070913      * CONTROLLI     * giro
072480070913      *------------------------------------------------------------------------*
072490070913     c     sr_contrcgi   begsr
072500070919
072510070919     c                   clear                   v1dcgi
072520160325     c                   eval      wModGiroAut = *off
072530071105
072540071105      * ricerca giro
072550071105     c                   if        %scan('?':v1ccgi) > *zeros
072560071105     c                   clear                   fidg09ds
072570071105     c                   eval      d09iop0 = 'P01'
072580071105     c                   eval      d09ifgs = num_v1cpor
072590071105     c                   eval      d09idat = *date
072600071105     c                   eval      d09itug = 'R'
072610071105     c                   eval      kpjbu = fidg09ds
072620071105     c                   call      'FIDG09R'
072630071105     c                   parm                    kpjba
072640071105     c                   eval      fidg09ds = kpjbu
072650071105      *     imposto il giro scelto
072660071105     c                   if        d09ocgi <> *blanks
072670071105     c                   eval      v1ccgi = d09ocgi
072680071105     c                   endif
072690071105     c                   endif
072700070919
072710071023      * Aggancio ACR e recupero il giro
072720071031     c                   if        (sav_acrcgi = *blanks and
072730071031     c                              ds_v1ccra <> *zeros)
072740131212     c                             or ($varqta = *on and
072750131212     c                                 sav_acrcgi = v1ccgi)
072760071023     c                   exsr      sr_carcgi
072770071023     c                   endif
072780071023
072790071023      * se giro non immesso da utente imposto il giro recuperato
072800071031      * oppure se variate le quantità
072810071031     c                   if        (sav_v1ccgi = *blanks and
072820071031     c                              sav_acrcgi <> *blanks)
072830131018     c                             or (sav_v1ccgi <> sav_acrcgi
072840131018     c                                 and sav_acrcgi <> *blanks)
072850131212     c                             or ($varqta = *on and
072860131212     c                                (sav_acrcgi = v1ccgi or
072870131212     c                                 sav_v1ccgi = v1ccgi))
072880070920     c                   eval      v1tgi = 'A'
072890070920     c                   eval      v1ccgi = sav_acrcgi
072900070920     c                   eval      sav_v1ccgi = sav_acrcgi
072910070920     c                   endif
072920160325      /free
072930160325       //?Se immissione o copia ORM
072940160325       //?Se variato il mittente e ora è un NON codificato
072950160429       //?Se il giro è rimasto lo stesso di prima
072960160429         IF  (*in01 or *in03) and
072970160429             wModCodCra and ds_V1Ccra = *zeros and
072980160429             V1Ccgi = sav_V1Ccgi and sav_ACRcgi = *blanks;
072990160429           clear V1tgi;
073000160429           clear V1Ccgi;
073010160429           clear sav_V1Ccgi;
073020160429           wModGiroAut = *on;
073030160429           *in28 = *on;
073040160429           *in90 = *on;
073050160429           V1Cmsg = msg(98);
073060160429           leavesr;
073070160325         ENDIF;
073080160325      /end-free
073090070913      *     controllo
073100070913     c                   if        v1ccgi <> *blank
073110070913     c                   exsr      ctrgiro
073120070913      *     giro errato
073130070924     c                   if        d09oerr = '1' and not *in24
073140070913     c                   eval      *in28 = *on
073150070913     c                   eval      *in51 = *on
073160131030     c                   eval      w2cmsg = msg(43)
073170070913     c                   leavesr
073180070913     c                   endif
073190070913      *     imposto la descrizione del giro
073200071011     c                   if        d09ocgi <> *blanks
073210070913     c                   eval      v1dcgi = d09odes
073220070913     c                   endif
073230070913      * se giro diverso da quello salvato imposto che giro manuale
073240070919     c                   if        v1ccgi <> sav_v1ccgi
073250070913     c                   eval      v1tgi = 'M'
073260070913     c                   endif
073270070913     c                   endif
073280070913
073290070913     c                   endsr
073300070913
073310070913      *------------------------------------------------------------------------*
073320070913      *   Controllo e decodifica giro
073330070913      *------------------------------------------------------------------------*
073340070913     c     ctrgiro       begsr
073350070913
073360070913     c                   clear                   fidg09ds
073370070913     c                   eval      d09iop0 = '001'
073380071011     c                   eval      d09ifgs = num_v1cpor
073390070913     c                   eval      d09idat = *date
073400070913     c                   eval      d09icgi = v1ccgi
073410070913     c                   eval      d09itug = 'R'
073420070913     c                   eval      kpjbu = fidg09ds
073430070913     c                   call      'FIDG09R'
073440070913     c                   parm                    kpjba
073450070913     c                   eval      fidg09ds = kpjbu
073460070913
073470070913     c                   endsr
073480070913
073490010202      **********************************************************************
073500010202      * CONTROLLI     * fase *
073510010202      **********************************************************************
073520010202     C     Sr_Contrfao   begsr
073530010202
073540010723      * Se non ho la data e ora fase caricata le imposto con data e ora del sistema
073550140107     c                   IF        *in01
073560050422     C                   if        v1dfo = *zeros
073570050422     C                   eval      v1dfo = oggi_aammgg
073580010723     C                   endif
073590050422     C                   if        v1ofo = *zeros
073600050422     C                   time                    w0140
073610010723     C                   movel     w0140         v1ofo
073620010202     C                   endif
073630010202     C                   endif
073640010202      * Decodifica la fase
073650010202     C                   if        v1fao <> *zeros
073660010202     C                   clear                   TIBS02DS
073670010202     C                   movel     'C'           T02mod
073680010202     C                   movel     knsif         t02sif
073690010202     C                   movel     'FAR'         t02cod
073700010202     C                   movel(p)  v1fao         T02ke1
073710010202     C                   call      'TIBS02R'
073720010202     C                   parm                    KPJBA
073730010202     C                   parm                    TIBS02DS
073740010314     C                   movel     t02uni        dfar
073750010314     C                   movel     d§fardes      v1dfao
073760070706     c                   eval      v1eti = d§farass
073770010202     C                   endif
073780010202
073790010202     C                   endsr
073800130917      /free
073810130917       //--------------------------------------------------------------
073820130917       //?Controllo ora ritiro merce con ora massima per il ritiro.
073830130917       //--------------------------------------------------------------
073840130917       BEGSR contrORRmax;
073850130917
073860131024       //?Controllo ora pronta merce se non è presa da anagrafica clienti
073870131022         SELECT;
073880140320         //?Se ora ritiro a 0 = commissionato
073890140320         //?Se ora fine servizio a 0 = ORM EXPORT no orari servizio
073900140320         //?per ora ritiro a 0 ci pensa la routine CONTRORR
073910140320         //?se non ci sono orari servizio non devo controllare
073920161104         //?non controllo se la data ritiro è maggiore della data pronta merce
073930161028           WHEN  V1orr = 0 or V1ostfs = 0;
073940140414           WHEN  sav_ACRorr <> *zeros and V1orr <= sav_ACRorr;
073950161104           WHEN  inv_v1dar > inv_v1dpm;
073960131022           OTHER;
073970131022       //?Controllo ora pronta merce con ora fine standard
073980161028       //?o con ora max
073990161028           IF  V1orr > V1ostfs;
074000131022             V1Cmsg = msg(12);
074010131022             V1Cmsg = %trim(V1Cmsg) + ' (' +
074020161028                      %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074030161028                      %subst(%editc(V1ostfs:'X'):3:2) + ')';
074040131022             *in28 = *on;
074050131022             *in58 = *on;
074060131022           ENDIF;
074070131022         ENDSL;
074080130917
074090140317       //?Controllo ora immissione ORM con ora fine standard
074100161028       //?o con ora max
074110161028         IF  V1ostfs > 0 and
074120161028             %dec(%subst(%editc(v1oao:'X'):1:4):4:0) > V1ostfs and
074130161028             inv_V1dao = inv_V1dar;
074140131016           V1Cmsg = msg(32);
074150130920           V1Cmsg = %trim(V1Cmsg) + ' (' +
074160161028                    %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074170161028                    %subst(%editc(V1ostfs:'X'):3:2) + ')';
074180130917           *in28 = *on;
074190130917         ENDIF;
074200130917
074210130917       ENDSR;
074220131014
074230131014       //--------------------------------------------------------------
074240131112       //?Controllo orari di apertura.
074250131014       //--------------------------------------------------------------
074260131112       BEGSR OrariApertura;
074270131017
074280131112       //?Richiamo pgm per controllo orari ritiro
074290131112       //?per ora il pgm deve controllare anche che ci sia abbastanza
074300131112       //?spazio nella nota1 e nella nota2 per poterli inserire in
074310131112       //?questi campi per poi inviarli a PDA
074320131112         IF  V1OraAMda > 0 or V1OraAMa > 0 or
074330131112             V1OraPMda > 0 or V1OraPma > 0;
074340131112           clear TRUL03DS;
074350131112           I03hm1 = V1OraAMda;
074360131112           I03hm2 = V1OraAMa;
074370131112           I03hm3 = V1OraPMda;
074380131112           I03hm4 = V1OraPMa;
074390131112           trul03r (TRUL03DS);
074400131112           IF  O03err <> *off;
074410131112             V1Cmsg = O03msg;
074420131112             *in28 = *on;
074430131112             SELECT;
074440131112               WHEN  O03errpos = 1;
074450131112                 H1riga = 12;
074460131112                 H1colo = 21;
074470131112               WHEN  O03errpos = 2;
074480131112                 H1riga = 12;
074490131112                 H1colo = 28;
074500131112               WHEN  O03errpos = 3;
074510131112                 H1riga = 12;
074520131112                 H1colo = 38;
074530131112               WHEN  O03errpos = 4;
074540131112                 H1riga = 12;
074550131112                 H1colo = 45;
074560131112               WHEN  O03errpos = 5;
074570131112                 *in82 = *on;
074580131112               WHEN  O03errpos = 6;
074590131112                 H1riga = 14;
074600131112                 H1colo = 43;
074610131112             ENDSL;
074620131112           ENDIF;
074630131112         ENDIF;
074640131107
074650131014       ENDSR;
074660100811
074670100811       //--------------------------------------------------------------
074680100811       //?Controllo cappario DPD.
074690100811       //--------------------------------------------------------------
074700100811       BEGSR CtrCapDPD;
074710100811
074720100811         clear tisie3ds;
074730100811         isie3dri = oggi_aammgg;
074740100811         IF  inv_v1dar = 0;
074750100811           isie3dsp = oggi_aammgg;
074760100811         ELSE;
074770100811           isie3dsp = inv_v1dar;
074780100811         ENDIF;
074790100811         isie3hsp = %dec(%time());
074800100811         isie3nzd = wNazione;
074810100811         isie3cad = wCapDPD;
074820100811         IF  v1pkg > §3ilmp;
074830100811           isie3srv = 101;
074840100811         ELSE;
074850100811           isie3srv = 136;
074860100811         ENDIF;
074870100811         IF  v1cpoe = *zeros;
074880150305           isie3lnp = DUTpou;
074890100811         ELSE;
074900100811           isie3lnp = v1cpoe;
074910100811         ENDIF;
074920100811         tisie3r (tisie3ds);
074930100811         IF  osie3err <> *blanks;
074940100811           *in28 = *on;
074950100811           *in62 = *on;
074960100811           v1cmsg = 'CAP non trovato su cappario DPD.';
074970100811           leavesr;
074980100811         ENDIF;
074990100811
075000100811       ENDSR;
075010161019
075020161019       //--------------------------------------------------------------
075030161019       //?Controllo Data Pronta Merce.
075040161019       //--------------------------------------------------------------
075050161019       BEGSR ContrDataProntaMerce;
075060161020
075070161020       //?Se sono in immissione o copia e la data è a 0 la imposto = oggi
075080161020         IF  (*in01 or *in03) and V1dpm = 0;
075090161020           V1dpm = oggi_ggmmaa;
075100161028           IF  sav_v1dpm > 0;
075110161028             clear sav_v1dpm;
075120161028           ELSE;
075130161028             sav_V1dpm = V1dpm;
075140161028           ENDIF;
075150161020         ENDIF;
075160161019
075170161019       //?La data è obbligatoria
075180161019         IF  V1dpm = 0;
075190161019           *in28 = *on;
075200161019           *in52 = *on;
075210161020           V1Cmsg = 'Immettere la Data Pronta Merce';
075220161019           leavesr;
075230161019         ENDIF;
075240161019
075250161019       //?Data Valida
075260161019         clear inv_V1dpm;
075270161019         clear wlbdat;
075280161019         G02dat = V1dpm;
075290161019         xsrda8 (wlbdat);
075300161019         IF  G02err = '1';
075310161019           *in28 = *on;
075320161019           *in52 = *on;
075330161020           V1Cmsg = 'Data Pronta Merce errata';
075340161019           leavesr;
075350161019         ENDIF;
075360161019         V1dpm = G02dat;
075370161019         inv_V1dpm = G02inv;
075380170303
075390170303       //?Data non più vecchia di 7 gg da oggi
075400170427       //?No se sono in chiusura ORM
075410170427         IF  not *in10;
075420170303         clear wdata;
075430170303         dataiso = %date(oggi_aammgg:*iso);
075440170303         wdata = %dec(dataiso - %days(7));
075450170303         IF  inv_V1dpm < wdata;
075460170303           *in28 = *on;
075470170303           *in52 = *on;
075480170303           V1Cmsg = 'Data Pronta Merce errata. Inferiore a 7 gg da oggi';
075490170303           leavesr;
075500170303         ENDIF;
075510170427         ENDIF;
075520161115
075530161115       //?Se sono in manutenzione non devo ricalcolare la data ritiro
075540161115       //?se la data pronta merce immessa è inferiore alla data pronta merce dell'ORM
075550161115         IF  *in02 and §OREdpm > 0 and inv_v1dpm <= §OREdpm;
075560161115           leavesr;
075570161115         ENDIF;
075580161028
075590161028         IF  V1dpm <> sav_V1dpm;
075600161028           wModDpm = *on;
075610161028           sav_V1dpm = V1dpm;
075620161028         ENDIF;
075630161019
075640161019       ENDSR;
075650100811      /end-free
075660100204
075670100204      *--------------------------------------------------------------------*
075680100204      * Imposto le personalizzazioni di ORMFLO da tabella PVO
075690100204      *--------------------------------------------------------------------*
075700100204     c     sr_pvo        begsr
075710100204
075720100204     c                   eval      ok_pvo = *off
075730100204
075740100204      * aggancio la tabella per:
075750100204      * --> ordinante lungo 10
075760100205     c                   clear                   dpvo
075770100204     c                   clear                   tibs02ds
075780100204     c                   eval      t02mod = 'C'
075790100204     c                   eval      t02sif = knsif
075800100204     c                   eval      t02cod = 'PVO'
075810100205     c                   eval      t02ke1 = %editc(ds_v1ccor:'X')
075820100204     c                   eval      t02ke2 = 'O'
075830100204     c                   call      'TIBS02R'
075840100204     c                   parm                    kpjba
075850100204     c                   parm                    tibs02ds
075860100204     c                   if        t02err = *blanks
075870100204     c                   eval      dpvo = t02uni
075880100204     c                   eval      OK_pvo = *on
075890100204     c                   endif
075900100204      * --> ordinante lungo 7
075910100204     c                   if        not OK_pvo
075920100204     c                   clear                   dpvo
075930100204     c                   clear                   tibs02ds
075940100204     c                   eval      t02mod = 'C'
075950100204     c                   eval      t02sif = knsif
075960100204     c                   eval      t02cod = 'PVO'
075970100204     c                   eval      t02ke1 = %editc(ds_v1cor12:'X')
075980100204     c                   eval      t02ke2 = 'O'
075990100204     c                   call      'TIBS02R'
076000100204     c                   parm                    kpjba
076010100204     c                   parm                    tibs02ds
076020100204     c                   if        t02err = *blanks
076030100204     c                   eval      dpvo = t02uni
076040100204     c                   eval      OK_pvo = *on
076050100204     c                   endif
076060100204     c                   endif
076070100204      * --> ksc
076080100204     c                   if        not OK_pvo
076090100204     c                   clear                   dpvo
076100100204     c                   clear                   tibs02ds
076110100204     c                   eval      t02mod = 'C'
076120100204     c                   eval      t02sif = knsif
076130100204     c                   eval      t02cod = 'PVO'
076140100204     c                   eval      t02ke1 = %editc(v1ksc:'X')
076150100204     c                   eval      t02ke2 = 'K'
076160100204     c                   call      'TIBS02R'
076170100204     c                   parm                    kpjba
076180100204     c                   parm                    tibs02ds
076190100204     c                   if        t02err = *blanks
076200100204     c                   eval      dpvo = t02uni
076210100204     c                   eval      OK_pvo = *on
076220100204     c                   endif
076230100204     c                   endif
076240100204
076250100204      * se ho trovato uno dei 3 codici in tabella devo personalizzare ORMFLO
076260100204     c                   if        OK_pvo
076270100205
076280100205     c                   if        d§pvocb = 'S'
076290100205     c                   eval      §ormcb = d§pvocb
076300100204     c                   endif
076310100205     c                   if        d§pvofr = 'S'
076320100205     c                   eval      §ormfr = d§pvofr
076330100204     c                   endif
076340100205     c                   if        d§pvoks = 'S'
076350100205     c                   eval      §ormks = d§pvoks
076360100204     c                   endif
076370100205     c                   if        d§pvofd = 'S'
076380100205     c                   eval      §ormfd = d§pvofd
076390100204     c                   endif
076400100205     c                   if        d§pvoic <> *blanks
076410100205     c                   eval      §ormic = d§pvoic
076420100204     c                   endif
076430100204      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
076440100204      * di variazione
076450100204      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
076460100205     c                   if        d§pvosrm = 'S'
076470100205     c                   eval      §orsrm = d§pvosrm
076480100204     c                   eval      v1com = 'S'
076490100204     c                   eval      *in34 = *on
076500100204     c                   clear                   v1dar
076510100204     c                   endif
076520100208     c                   if        *in01 and §rmtla = *blanks
076530100208     c                   eval      v1com = §orcomc
076540100208     c                   endif
076550100812      /free
076560100812       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
076570100812       //?di variazione
076580100812        IF  D§PVOpre = 'M';
076590100812          §ORMpre = D§PVOpre;
076600100812          V1com = 'N';
076610100812          *in34 = *on;
076620100812        ENDIF;
076630100812      /end-free
076640100204
076650100204     c                   endif
076660100208
076670100208     c                   if        *in01 and §rmtla = *blanks and
076680100208     c                             not ok_pvo
076690100208      * devo pulire tutti i dati del campo ormflo
076700100208     c                   clear                   dorm01
076710100208     c                   endif
076720100204
076730100204     c                   endsr
076740071031
076750071031      *--------------------------------------------------------------------*
076760071031      * Carico tabella AGR del po ritiro
076770071031      *--------------------------------------------------------------------*
076780071031     c     sr_caragr     begsr
076790071105
076800071031     c                   clear                   tibs02ds
076810071031     c                   clear                   dagr
076820071112     c                   eval      $noagr = *off
076830071031     c                   eval      t02mod = 'C'
076840071031     c                   eval      t02sif = knsif
076850071031     c                   eval      t02cod = 'AGR'
076860071031     c                   movel(p)  v1cpor        t02ke1
076870071031     c                   call      'TIBS02R'
076880071031     c                   parm                    kpjba
076890071031     c                   parm                    tibs02ds
076900071105     c                   if        t02err = *blanks
076910071031     c                   eval      dagr = t02uni
076920071112     c                   else
076930071112     c                   eval      $noagr = *on
076940071105     c                   endif
076950071031
076960071031     c                   endsr
076970071023
076980071023      *--------------------------------------------------------------------*
076990071023      * Carico il giro da FNACR o da FNORS
077000071023      *--------------------------------------------------------------------*
077010071023     c     sr_carcgi     begsr
077020071023
077030071023     c                   clear                   sav_acrcgi
077040071031
077050071106     c                   eval      kacr1ain = '='
077060071112      * se tabella AGR inserita per filiale ritiro
077070071112     c                   if        $noagr = *off
077080071031      * controllo se devo recuperare il giro Oltre, il giro Sotto o lo Standard
077090071031     c                   if        wpkg > §agrpkgo or wvlm > §agrvlmo or
077100071031     c                             v1bnc > §agrbnco
077110071031     c                   eval      kacr1ain = '>'
077120071031     c                   endif
077130071031     c                   if        wpkg < §agrpkgs and wvlm < §agrvlms and
077140071031     c                             v1bnc < §agrbncs
077150071031     c                   eval      kacr1ain = '<'
077160071031     c                   endif
077170071112     c                   endif
077180071023
077190071023      * da anagrafica clienti ritiro
077200071031     c     kacr1         chain     fnacr13l
077210071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077220071023     c                   eval      sav_acrcgi = acr1cgi
077230071031     c                   else
077240071106     c                   if        kacr1ain <> '='
077250071106     c                   eval      kacr1ain = '='
077260071031     c     kacr1         chain     fnacr13l
077270071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077280071031     c                   eval      sav_acrcgi = acr1cgi
077290071031     c                   endif
077300071031     c                   endif
077310071031     c                   endif
077320071031
077330071023      * se conferma ORM fisso recupero il giro dal fisso se impostato
077340071031      * quindi solo immissione
077350161117     c                   if        §rmfpr = 'S'
077360071023     c     kors          chain     fnors11l
077370071023     c                   if        %found(fnors11l) and ors1cgi <> *blanks
077380071023     c                   eval      sav_acrcgi = ors1cgi
077390071023     c                   endif
077400071023     c                   endif
077410071023
077420071023     c                   endsr
077430100217
077440100218      /free
077450100218       //--------------------------------------------------------------------
077460100218       //?Controllo se OK tutti i dati per il preavviso.
077470100218       //--------------------------------------------------------------------
077480100218       BEGSR sr_Preavviso;
077490100217
077500100218       //?Imposto la chiave per il FNVAOE rcd 'M '
077510100218         k_vaoecor = sav_vaocor;
077520100218         k_vaoepoe = sav_vaopoe;
077530100218         k_vaoensr = sav_vaonsr;
077540100218         k_vaoenor = sav_vaonor;
077550100218         k_vaoenrv = sav_vaonrv;
077560100218         k_vaoetrc = 'M ';
077570100218       //?Aggancio il rcd
077580100812         chain  %kds(k06fnvaoe) FNVAOE1L;
077590100218
077600100218         SELECT;
077610100218       //?Se non trovo il rcd e non c'è il flag di preavviso mail OK
077620100218           WHEN  not %found(FNVAOE1L) and §ormpre <> 'M';
077630100218             clear vaoedati;
077640100218       //?Se trovo il rcd e c'è il flag di preavviso mail OK e controllo la mail
077650100218           WHEN  %found(FNVAOE1L) and §ormpre = 'M';
077660100218             wmail = vaoedati;
077670100218             exsr sr_CtrMail;
077680100223             vaoedati = wmail;
077690100223             IF  $errpre;
077700100223               wpremsg = §emlmsgo;
077710100225               *in94 = *on;
077720100223             ENDIF;
077730100218       //?Se non trovo il rcd e c'è il flag di preavviso mail errore
077740100218           WHEN  not %found(FNVAOE1L) and §ormpre = 'M';
077750100218             clear vaoedati;
077760100223             $errpre = *on;
077770100225             wpremsg = 'Dati incongruenti. ' +
077780100225                       'ORM con preavviso senza indirizzo mail';
077790100218       //?Se trovo il rcd e non c'è il flag di preavviso mail errore
077800100218           WHEN  %found(FNVAOE1L) and §ormpre <> 'M';
077810100223             $errpre = *on;
077820100225             wpremsg = 'Dati incongruenti. ' +
077830100225                       'ORM senza preavviso con indirizzo mail';
077840100218         ENDSL;
077850100218
077860100218       ENDSR;
077870100218
077880100218       //--------------------------------------------------------------------
077890100218       //?Controllo validità indirizzo mail.
077900100218       //--------------------------------------------------------------------
077910100218       BEGSR sr_CtrMail;
077920100218
077930100218         clear dsemail;
077940100223         §emlindi = wmail;
077950100218         xemail (dsemail);
077960100223         $errpre = (§emlerro <> '0');
077970100223         IF  not $errpre;
077980100223           wmail = §emlindo;
077990100223         ENDIF;
078000100218
078010100218       ENDSR;
078020100218
078030100218       //--------------------------------------------------------------------
078040100218       //?Richiesta dati preavviso con window.
078050100218       //--------------------------------------------------------------------
078060100218       BEGSR sr_DatiPre;
078070100218
078080100218         $endpre = *off;
078090100223         *in90 = *off;
078100100223         $okf6pre = *off;
078110100224         *in28 = *off;
078120100218
078130100218       //?emetto la videata con i dati dell'ORM
078140100218         write fior051;
078150100322         IF  $errpre;
078160100322           *in28 = *on;
078170100322         ENDIF;
078180100218       //?emetto la window dove richiedo i dati del preavviso
078190100218         DOW not $endpre;
078200100218           exfmt fior05pre;
078210100218           clear  wpremsg;
078220100218           *in28 = *off;
078230100225           *in94 = *off;
078240100223         SELECT;
078250100223       //?F12-Ritorno
078260100223           WHEN  *inkl;
078270100218             $endpre = *on;
078280100223             *in28 = *off;
078290100223             *in90 = *on;
078300100223             $errpre = *off;
078310100223       //?F6-Conferma
078320100223           WHEN  *inkf;
078330100225             exsr sr_CtrDatiPre;
078340100223             IF  not *in28;
078350100223               IF  wpresino = 'NO';
078360100223                 clear §ormpre;
078370100325           //?Se non è più ORM con preavviso MAIL pulisco il campo di ORM commissionato
078380100325           //?e lo sblocco, così il pgm ricalcola se commissionato o meno e l'utente può
078390100325           //?variarlo
078400100325                 clear V1com;
078410100325                 *in34 = *off;
078420100223               ENDIF;
078430100223               IF  wpresino = 'SI';
078440100223                 §ormpre = 'M';
078450100325           //?Se è ORM con preavviso MAIL imposto che ORM NON commissionato
078460100325           //?e sblocco il campo così l'utente non può variarlo
078470100325                 V1com = 'N';
078480100325                 *in34 = *on;
078490100223               ENDIF;
078500100223               vaoedati = wpremail;
078510100223               $endpre = *on;
078520100223               $okf6pre = *on;
078530100223               $errpre = *off;
078540100223             ENDIF;
078550100223           OTHER;
078560100218       //?controllo i dati
078570100225             exsr sr_CtrDatiPre;
078580100223           ENDSL;
078590100218         ENDDO;
078600100218
078610100218       ENDSR;
078620100218
078630100218       //--------------------------------------------------------------------
078640100218       //?Controlli dati preavviso.
078650100218       //--------------------------------------------------------------------
078660100225       BEGSR sr_CtrDatiPre;
078670100218
078680100218         *in28 = *off;
078690100218
078700100218       //?se preavviso = 'SI' deve esserci la mail
078710100218         IF  wpresino = 'SI' and wpremail = *blanks;
078720100218           *in28 = *on;
078730100225           wpremsg = 'Dati incongruenti. ' +
078740100225                     'ORM con preavviso senza indirizzo mail';
078750100218           leavesr;
078760100218         ENDIF;
078770100218       //?se preavviso = 'NO' NON deve esserci la mail
078780100218         IF  wpresino = 'NO' and wpremail <> *blanks;
078790100218           *in28 = *on;
078800100225           wpremsg = 'Dati incongruenti. ' +
078810100225                     'ORM senza preavviso con indirizzo mail';
078820100218           leavesr;
078830100218         ENDIF;
078840100218       //?la mail deve essere valida
078850100218         IF  wpremail <> *blanks;
078860100223           wmail = wpremail;
078870100218           exsr sr_CtrMail;
078880100223           IF  $errpre;
078890100223             $errpre = *off;
078900100218             *in28 = *on;
078910100225             *in94 = *on;
078920100218             wpremsg = §emlmsgo;
078930100218             leavesr;
078940100218           ENDIF;
078950100223           wpremail = wmail;
078960100218         ENDIF;
078970100218
078980100218       ENDSR;
078990100218
079000100218      /end-free
079010010305
079020030606      **********************************************************************
079030030611      * Imposta flag x Orm commissionato *
079040030606      **********************************************************************
079050030611     c     Sr_OrmCom     BegSr
079060030611
079070030611      * Se p.o. emissione a 0 vuol dire che sono in immissione
079080150305      * in questo caso lo imposto già uguale al p.o. utente
079090030611      * per poter fare il controllo tra p.o. ritiro e p.o. emissione
079100030611     c                   If        v1cpoe = *zeros
079110150305     c                   z-add     DUTpou        v1cpoe
079120030611     c                   EndIf
079130050323      * RICORDA!!!!
079140050324      * Un orm da INTERNET/FILE con codice ordinante e p.o. emissione = a p.o. ritiro
079150050323      * non è un ORM commissionato
079160060103      * Un orm FISSO con codice ordinate e p.o. emissione = a p.o. ritiro (x forza)
079170060103      * non è un ORM commissionato (in automatico l'utente può variarlo)
079180100205
079190100205      * come prima cosa imposto il campo di ORM commissionato se lo recupero dalla
079200100205      * personalizzazione del cliente
079210100205     c                   if        v1com = *blanks
079220100205     c                             and d§pvocomc <> *blanks
079230100205     c                   eval      v1com = d§pvocomc
079240100205     c                   leavesr
079250100205     c                   endif
079260030606
079270030606      * Imposto che è un orm commissionato se:
079280030606      * - Cod.mittente <> Cod.ordinante
079290030606      * - Ordinante presente ma non codificato o 8888 o 9999
079300130911      * - P.o. ritiro <> p.o. emissione
079310130911      *   ma solo se mittente NON codificato
079320030606
079330050323If  1c                   If        V1Com = *Blanks  and
079340030902     c                                             ((Ds_V1Cor12 > *Zeros and
079350040707     c                                              Ds_V1cra12 <> Ds_V1Cor12 and
079360050323     c                                              V1ctco <> 'I' and
079370060103     c                                              V1ctco <> 'F' and
079380060103     c                                              V1ctco <> 'S')
079390030902     c                                              or
079400030902     c                                             (V1rso <> *Blanks   and
079410030902     c                                              (Ds_V1ccor = *Zeros or
079420030902     c                                               Ds_V1cor2 = 8888   or
079430050323     c                                               Ds_V1cor2 = 9999)))
079440030606     c                   Eval      V1Com = 'S'
079450030902     c                   Goto      Endormcom
079460030606    1c                   EndIf
079470050323If  1c                   If        V1Com = *Blanks  and
079480030902     c                                              Num_V1cPor <> V1cpoe
079490130911     c                                         and  ds_V1Ccra = 0
079500030902     c                   Eval      V1Com = 'S'
079510030902     c                   Goto      Endormcom
079520030902    1c                   EndIf
079530030611
079540030902     c     endormcom     EndSr
079550030611      **********************************************************************
079560030611      * Controllo se Ok tutti i dati
079570030611      **********************************************************************
079580030611     c     Sr_ContrCom   BegSr
079590030606
079600030606      * Se è un orm commissionato l'ordinante è obbligatorio
079610030606      * se l'ordinante non c'è devo emettere una window dove chiedo chi è l'ordinante
079620030606If  1c                   If        V1Com = 'S' and
079630030606     c                             V1rso = *Blanks and Ds_V1ccor = *Zeros
079640030610     c                   Eval      whocor = 'S'
079650030610     c                   Eval      *In90 = *On
079660131106      /free
079670131106       //?pulisco il campo del commissionato così viene rifatto il calcolo
079680131106       //?se ORM commissionato o meno
079690131106         clear V1com;
079700131106      /end-free
079710030611      * se non sto facendo una conferma di massa emetto la window
079720030611If 1ac                   If        §rmtla <> '0' and §rmtla <> '1'
079730030610Do  2c                   Do        *Hival
079740030610     c                   Clear                   wdcor
079750030606     c                   Exfmt     Fior05o
079760030610If  3C                   If        *Inkl
079770030606     c                   Leave
079780030610    3c                   EndIf
079790030610If  3c                   If        wdcor <> *Blanks
079800030609     c                   Leave
079810030610    3c                   EndIf
079820030610    2c                   EndDo
079830030611      * se sto facendo una conferma di massa errore
079840030611  x1ac                   Else
079850030611     C                   seton                                        28
079860030611   1ac                   EndIf
079870030606    1c                   EndIf
079880030610
079890030610      * devo controllare il referente e il n. telefono
079900030612If  1c                   If        V1Com = 'S' and Not *In28
079910030610     C                   exsr      Sr_Contrref
079920030610    1c                   EndIf
079930030606
079940030606     c                   EndSr
079950070706
079960070706      *--------------------------------------------------------------------*
079970070706      * Calcolo peso e volume
079980070706      *--------------------------------------------------------------------*
079990070706     c     sr_pesvol     begsr
080000071108
080010071108     c                   clear                   wvlmaut
080020071108     c                   clear                   wpkgaut
080030071108     c                   clear                   wvlmmtc
080040071108     c                   clear                   wpkgmtc
080050071108     c                   clear                   wvlmblc
080060071108     c                   clear                   wpkgblc
080070070706
080080070706     c                   eval      wpkg = v1pkg
080090070706     c                   eval      wvlm = v1vlm
080100070706
080110070706      * se non inserito peso a video lo calcolo per memorizzarlo su fnorg00f
080120070706     c                   if        wpkg = *zeros
080130070706     c                   select
080140070918 b3  c                   when      v1bnc <> *zeros
080150070918     c                   eval      wpkg = v1bnc / d§dftbnc * d§dftpkg
080160070706 b2  c                   when      v1vlm <> *zeros
080170070918     c                   eval      wpkg = v1vlm * d§dftpkg
080180070706 e3  c                   endsl
080190070706      * se troppo alto il peso lo imposto al massimo
080200070706     c                   if        wpkg > 999999,9
080210070706     c                   eval      wpkg = 999999,9
080220070706     c                   endif
080230070706     c                   endif
080240070706
080250070706      * se non inserito volume a video lo calcolo per memorizzarlo su fnorg00f
080260070706     c                   if        wvlm = *zeros
080270070706     c                   select
080280070918 b3  c                   when      v1bnc <> *zeros
080290070918     c                   eval(h)   wvlm = v1bnc / d§dftbnc
080300070918 b2  c                   when      v1pkg <> *zeros
080310070918     c                   eval(h)   wvlm = v1pkg / d§dftpkg
080320070706 e3  c                   endsl
080330070706      * se troppo alto il volume lo imposto al massimo
080340070706     c                   if        wvlm > 99,999
080350070706     c                   eval      wvlm = 99,990
080360070706     c                   endif
080370070706     c                   endif
080380070918
080390070918      * se inseriti gli automezzi devo convertirli in m³ per poi memorizzare
080400070918      * il dato più alto su FNORG
080410070918      * AUTOTRENO
080420070918     c                   if        v1att > *zeros
080430070918     c                   eval      wvlmaut = v1att * d§dftaut
080440070918     c                   eval      wpkgaut = wvlmaut * d§dftpkg
080450070918     c                   if        wvlmaut > 99,999
080460070918     c                   eval      wvlmaut = 99,999
080470070918     c                   endif
080480070918     c                   if        wpkgaut > 999999,9
080490070918     c                   eval      wpkgaut = 999999,9
080500070918     c                   endif
080510070918     c                   endif
080520070918      * MOTRICE
080530070918     c                   if        v1mtc > *zeros
080540070918     c                   eval      wvlmmtc = v1mtc * d§dftmtc
080550070918     c                   eval      wpkgmtc = wvlmmtc * d§dftpkg
080560070918     c                   if        wvlmmtc > 99,999
080570070918     c                   eval      wvlmmtc = 99,999
080580070918     c                   endif
080590070918     c                   if        wpkgmtc > 999999,9
080600070918     c                   eval      wpkgmtc = 999999,9
080610070918     c                   endif
080620070918     c                   endif
080630070918      * BILICO
080640070918     c                   if        v1blc > *zeros
080650070918     c                   eval      wvlmblc = v1blc * d§dftblc
080660070918     c                   eval      wpkgblc = wvlmblc * d§dftpkg
080670070918     c                   if        wvlmblc > 99,999
080680070918     c                   eval      wvlmblc = 99,999
080690070918     c                   endif
080700070918     c                   if        wpkgblc > 999999,9
080710070918     c                   eval      wpkgblc = 999999,9
080720070918     c                   endif
080730070918     c                   endif
080740070918
080750070918      * cerco il volume più alto
080760070918     c                   if        wvlmaut > wvlm
080770070918     c                   eval      wvlm = wvlmaut
080780070918     c                   endif
080790070918     c                   if        wvlmmtc > wvlm
080800070918     c                   eval      wvlm = wvlmmtc
080810070918     c                   endif
080820070918     c                   if        wvlmblc > wvlm
080830070918     c                   eval      wvlm = wvlmblc
080840070918     c                   endif
080850070918
080860070918      * cerco il peso più alto
080870070918     c                   if        wpkgaut > wpkg
080880070918     c                   eval      wpkg = wpkgaut
080890070918     c                   endif
080900070918     c                   if        wpkgmtc > wpkg
080910070918     c                   eval      wpkg = wpkgmtc
080920070918     c                   endif
080930070918     c                   if        wpkgblc > wpkg
080940070918     c                   eval      wpkg = wpkgblc
080950070918     c                   endif
080960071031
080970071108      * se sono in modifica e non devo fare una proposta di variazione
080980071031      * controllo se variati peso/volume/bancali per ricalcolare il giro in base gli ambiti
080990071031      * per il peso e il volume devo utilizzare i valori sviluppati dal pgm
081000071031     c                   eval      $varqta = *off
081010150609     c     v1cpor        Lookup    skpog                                  30
081020071108     c                   select
081030080314     c                   when      *in30 and *in02 and (v1fao = 400
081040080314     c                             or v1fao = 410)
081050071108     c                   when      not *in30 and *in02
081060071108     c                   other
081070131202     c                   if        (savpkg <> wpkg or savvlm <> wvlm or
081080131202     c                              savbnc <> v1bnc) and
081090131202     c                             (savpkg > 0 or savvlm > 0 or
081100131202     c                              savbnc > 0)
081110071031     c                   eval      $varqta = *on
081120071031     c                   eval      savpkg = wpkg
081130071031     c                   eval      savvlm = wvlm
081140071031     c                   eval      savbnc = v1bnc
081150071031     c                   endif
081160071108     c                   endsl
081170161025      /free
081180161025       //?Controllo se ho variato il peso, serve per ricalcolare la data ritiro
081190161025       //?ma solo se data ritiro a video non impostata da utente
081200161025         IF  old_wpkg <> wpkg;
081210161026           IF  old_wpkg > 0 and inv_newv1dar >= inv_v1dar;
081220161025             wModPeso = *on;
081230161025           ENDIF;
081240161025           old_wpkg = wpkg;
081250161025         ENDIF;
081260161025      /end-free
081270070706
081280070706     c                   endsr
081290070706
081300010305      **********************************************************************
081310010305      * CONTROLLI     * cerco causale chiusura e decodifico *
081320010305      **********************************************************************
081330010305     C     Sr_Contrcau   begsr
081340010305
081350010305     C                   eval      kpoe = ORMpoe
081360010305     C                   eval      knsr = ORMnsr
081370010305     C                   eval      knor = ORMnor
081380010305     C                   eval      knrv = ORMnrv
081390010305     C                   eval      kdai = ORMdfo
081400010305     C                   eval      kori = ORMofo
081410010305     C                   eval      kfar = ORMfao
081420110418     c     kfnorn1       setll     fnorf01l
081430110418     c                   do        *hival
081440110418     c     kfnorn1       reade(n)  fnorf01L
081450110418     c                   if        %eof(fnorf01L)
081460110418     c                   leave
081470110418     c                   endif
081480010305     C                   eval      v1cau = orfcar
081490110418     C                   enddo
081500010305
081510010305     C                   clear                   dcmr
081520010305     C                   clear                   TIBS02DS
081530010305     C                   movel     'C'           T02mod
081540010305     C                   movel     knsif         t02sif
081550010305     C                   movel     'CMR'         t02cod
081560010305     C                   movel(p)  v1cau         T02ke1
081570010305     C                   call      'TIBS02R'
081580010305     C                   parm                    KPJBA
081590010305     C                   parm                    TIBS02DS
081600010305     C                   movel     t02uni        dcmr
081610010305     C                   movel     d§cmrdes      v1dcau
081620010309
081630010309     C                   eval      *in13 = (v1cau <> *blanks)
081640070123
081650070123      * mi salvo il p.o. e il n. distinta
081660080226     c                   eval      kdstfgs = orffgs
081670080226     c                   eval      kdstnfv = orfndc
081680010305
081690010305     C                   endsr
081700001019      **********************************************************************
081710001023      * CONTROLLI     * se già stampato deve confermare *
081720001019      **********************************************************************
081730001019     C     Sr_Contrstp   begsr
081740001019
081750031201     C                   if        v1dst  >  *zeros
081760001019     C                   movel     'NO'          wfstp
081770010221     C     emetto        tag
081780001019     C                   exfmt     fior05w
081790001107     C                   if        wfstp = 'SI'
081800031125     C                   clear                   FIOR16ds
081810041013     C                   eval      R16ris = 'R'
081820030623     C                   clear                   trul90ds
081830030623     C                   movel     'S'           D90rso
081840030623     C                   call      'TRUL90R'
081850010314     C                   parm                    kpjba
081860030623     C                   parm                    trul90ds
081870001108
081880001108      * F3=Fine
081890030623     C                   if        D90f3 = '1'
081900010314     C                   goto      endsrstampa
081910010314     C                   endif
081920031125
081930090202      /free
081940090202         reset ds5P;
081950090202         if  %subst(knmus : 1 : 3) = 'EDP';
081960090202           TBLkey = 'O' + 'EDP';
081970090202         else;
081980090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
081990090202         endif;
082000090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
082010090202         if  %found( TABEL00F );
082020090202           ds5P = TBLuni;
082030090202         endif;
082040090202      /end-free
082050031201
082060031125     C                   movel     D90mdo        R16mdo
082070031125     C                   movel     D90pro        R16oqo
082080001108
082090031201     c                   eval      R16poe = Ppoe
082100031201     c                   eval      R16nsr = Pnsr
082110031201     c                   eval      R16nor = Pnor
082120031201     c                   eval      R16nrv = Pnrv
082130031125     C                   movel     v1cpor        R16por
082140031125     C                   eval      R16dst = v1dst
082150031125     C                   eval      R16dar = inv_v1dar
082160001108
082170001108     C                   movel     kpjbu         §kpjbu
082180031125     C                   movel(p)  FIOR16ds      kpjbu
082190090202     C                   call      §5Ppgm
082200031125     C                   parm                    KPJBA
082210031125     C                   movel     kpjbu         FIOR16DS
082220001108     C                   movel     §kpjbu        kpjbu
082230001019     C                   endif
082240001019     C                   endif
082250001019
082260001108     C     endsrstampa   endsr
082270001011      **********************************************************************
082280001011      * F6 - Conferma
082290001011      **********************************************************************
082300001011     C     Sr_Conferma   BEGSR
082310140422
082320140422      /free
082330140423        werralert = *off;
082340140422       //?Se conferma ORM da clienti come prima cosa verifico gli alert
082350140507        IF  §rmfpr = 'C' or
082360150305       //?oppure se sono in immissione ORM
082370140507       //?e non è stato richiamato (quindi immissione ORM MANUALE)
082380160208       //?o copia ORM
082390160208            ((*in01 or *in03 )and §RMtla = *blanks);
082400140423          exsr AlertORM;
082410140423          IF  *in28;
082420140423            werralert = *on;
082430140423            leavesr;
082440140423          ENDIF;
082450140422        ENDIF;
082460160406
082470160413       //?Se immissione ORM manuale o copia
082480160406         IF  (*in01 or *in03) and
082490160406             (V1Ctco = 'M' or V1Ctco = 'E');
082500160406         //?se ho almeno 1 dei 2 codici (ordinante - mittente) e non ho emesso
082510160406         //?la WIN (non ho mai dati un F6 sulla WIN)
082520160406         //?e non ho i dati mail e/o sms provo a recuperarli
082530160406         //?dall'anagrafica clienti
082540160406           IF  (ds_V1Ccor > 0 or ds_V1Ccra > 0) and
082550160406                W04mail = *blanks and W04sms = *blanks and
082560160406                not wOkF6F13;
082570160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
082580160406             exsr RecDatiConf;
082590160406           ENDIF;
082600160406       //?se ho i dati relativi alla conferma prenotazione ritiro
082610160405       //?imposto i dati per scrivere il rcd 'G' di fnore e il file spia
082620160406       //?da conferma VAS ho già tutto impostato
082630160406           IF  (w04sms <> *blanks or w04mail <> *blanks);
082640160406             SELECT;
082650160406             WHEN  W04sms <> *blanks and W04mail <> *blanks;
082660160406               §OREfsco = 'S';
082670160406               §OREfmco = 'S';
082680160406             WHEN  W04sms <> *blanks;
082690160406               §OREfsco = 'S';
082700160406               §OREfmco = 'N';
082710160406             WHEN  W04mail <> *blanks;
082720160406               §OREfmco = 'S';
082730160406               §OREfsco = 'N';
082740160406             ENDSL;
082750160406             wconferma = *on;
082760160406           ENDIF;
082770160310         ENDIF;
082780140422      /end-free
082790001011
082800071108     ?* Scrive
082810001109     C                   if        *in01 = *on or *in03 = *on
082820001117     C                   clear                   fnorm000
082830050422     C  n03              exsr      Sr_Numor
082840050422     C   28              goto      endsrconf
082850001011     C                   exsr      Sr_Carfile
082860001108      * Scrive la fase
082870001017     C                   exsr      Sr_Carfilef
082880001108      * Scrive le note
082890150713     c                   IF        not ConfAuto
082900001026     C                   exsr      Sr_Carnote
082910150713     c                   ENDIF
082920070919      * scrivo fnorg (GIRO)
082930151016     c                   exsr      sr_wrtorg
082940100218      /free
082950160310
082960170524       //?Se dovevo posticipare
082970170524        Anticipato = *off;
082980170524        clear ggAnticipo;
082990170524       //?Imposto se l'utente ha anticipato la data ritiro
083000170615       //?se c'era la 'A' di anticipa
083010170524        IF  wAnticipa and ORMdar < DarCalcolata;
083020170524          Anticipato = *on;
083030170615        ENDIF;
083040170525       //?e di quanti gg lavorativi ha anticipato
083050170615       //?con o senza 'A' di anticipa, in alcuni casi possono anticipare alla
083060170615       //?data immissione senza che sia visualizzata la 'A', questo soprattutto in
083070170615       //?copia ORM oppure se immissione prepagato
083080170525        clear wdat8;
083090170525        clear tfptfa;
083100170525        p_tfa = ORMpor;
083110170525        data1 = DarCalcolata;
083120170525        data2 = ORMdar;
083130170525        xsrlav8(wdat8);
083140170525       //?dal pgm di calcolo differenza tra le 2 date torna sempre almeno 1, io lo tolgo
083150170525        ggAnticipo = giolav -1;
083160100225       //?Se conferma ORM da clienti scrivo eventuale estensione inviata dal cliente
083170100225        IF  §rmfpr = 'C';
083180100218          exsr sr_fnore;
083190100218        ENDIF;
083200131014       //?Se presenti i dati scrivo fnore
083210140422        IF  w03mail <> *blanks;
083220140422          clear old_w03mail;
083230140422        ENDIF;
083240140422        IF  w03sms <> *blanks;
083250140422          clear old_w03sms;
083260140422        ENDIF;
083270160405        IF  w04mail <> *blanks;
083280160405          clear old_w04mail;
083290160310        ENDIF;
083300160405        IF  w04sms <> *blanks;
083310160405          clear old_w04sms;
083320160310        ENDIF;
083330140422        exsr aggFNORE;
083340160322
083350160322       //?Se conferma ORM da clienti per orm da internet
083360160322       //?memorizzo mail sull'anagrafica clienti ritiro, così anche il relativo flag
083370160322       //?anche sms
083380160712       //?Solo però se richiesto da Ordinante
083390160712        IF  §rmfpr = 'C' and ORMtco = 'I' and ORMcor > 0 and §OREmemo = 'S';
083400160712          exsr aggACR;
083410160322        ENDIF;
083420160314
083430160314       //?Se ORM con Conferma Prenotazione Ritiro e sono in immissione manuale
083440160314       //?cioè ORM Telefonico o Mail/Fax
083450160314       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
083460160314       //?servono nell'invio dell'alert
083470160318       //?ma solo se devo creare l'alert conferma prenotazione ritiro
083480160318        IF  §RMfpr <> 'C' and (ORMtco = 'M' or ORMtco = 'E') and
083490160318            wconferma;
083500160314         //?imposto orario ritiro standard
083510160314          clear dOREor;
083520160314          §OREori = V1ostis;
083530160314          §OREorf = V1ostfs;
083540170609         //?Se la data ritiro è uguale a data immissione
083550170609          IF  ORMdar = ORMdao;
083560160314         //?Se l'ora pronta merce è maggiore
083570160314         //?dell'ora inizio servizio
083580160314         //?e minore dell'ora fine
083590160314         //?la imposto come orario inzio
083600160314          IF  V1orr > V1ostis and
083610160314              V1orr < V1ostfs;
083620160314            §OREori = V1orr;
083630160314          ENDIF;
083640160314         //?Se l'ora pronta merce è maggiore
083650160314         //?dell'ora fine servizio
083660160314         //?la imposto come orario fine
083670160314          IF  V1orr > V1ostfs;
083680160314            §OREorf = V1orr;
083690160314          ENDIF;
083700170609          ENDIF;
083710160314          clear FNORE000;
083720160314          OREpoe = ORMpoe;
083730160314          OREnsr = ORMnsr;
083740160314          OREnor = ORMnor;
083750160314          OREnrv = ORMnrv;
083760160314          OREtrc = 'OR';
083770160314          OREdati = dOREor;
083780160314          write FNORE000;
083790160517          feod  FNORE01L;
083800160314        ENDIF;
083810160217
083820160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
083830160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
083840160224       //?il NPR lo recupero solo se non sto facendo conferma ORM da INTERNET
083850160426       //?e se non è già stato calcolato
083860160426        IF  §rmfpr <> 'C' or
083870160426           (§rmfpr = 'C' and ORMtco <> 'I') or
083880160426           (§rmfpr = 'C' and ORMtco = 'I' and not WebNPR);
083890160217          wnpr = GetNPR(parametroNPR);
083900160217       //?Se il rif. ORM è vuoto imposto il NPR
083910160217          IF  ORMrfa = *blanks;
083920160317            ORMrfa = wnpr;
083930160217          ENDIF;
083940160314       //?scrivo FNORE00F rcd NP
083950160314       //?per memorizzare il NPR
083960160314          clear FNORE000;
083970160314          OREpoe = ORMpoe;
083980160314          OREnsr = ORMnsr;
083990160314          OREnor = ORMnor;
084000160314          OREnrv = ORMnrv;
084010160314          OREtrc = 'NP';
084020160314          OREdati = wnpr;
084030160314          write FNORE000;
084040160517          feod  FNORE01L;
084050160217        ENDIF;
084060100218      /end-free
084070100218
084080001108     C                   write     fnorm000
084090160509     C                   feod      fnorm01L
084100100218
084110100218      /free
084120100218       //?dopo aver scritto l'ORM se previsto il preavviso richiamo programma
084130100218       //?per invio mail di preavviso
084140100218        IF  §ormpre = 'M';
084150100224         clear fior52ds;
084160100224         I52poe = ORMpoe;
084170100224         I52nsr = ORMnsr;
084180100224         I52nor = ORMnor;
084190100224         I52nrv = ORMnrv;
084200100224         fior52r (kpjba:fior52ds);
084210100218        ENDIF;
084220140422
084230140428       //?dopo aver scritto l'ORM se previsto Alert
084240151008       //?e/o previsto invio mail x conferma accettazione ORM
084250140428       //?scrivo file spia
084260151008       IF  walert or wconferma;
084270140422         exsr wrtspia;
084280140422       ENDIF;
084290160217
084300100218      /end-free
084310100218
084320010202     C                   eval      §RMpoeo = ormpoe
084330010202     C                   eval      §RMnsro = ormnsr
084340010202     C                   eval      §RMnoro = ormnor
084350010202     C                   eval      §RMnrvo = ormnrv
084360010704
084370010704      * Richiamo il programma immissione prepagati
084380050324      * solo se sono da conferma ORM VAO, se l'orm è di tipo 'P'
084390050324      * e se è un ORM locale
084400050324      * se l'ORM non è locale il prepagato lo creo al momento della conferma ORM
084410050324      * commissionati (Un ORM prepagato non locale è SEMPRE commissionato)
084420010704     C                   if        §RMfpr = 'C' and
084430011012     C                             ORMtor = 'P'
084440080407      * se po emissione diverso da po ritiro controllo se il po ritiro è la capofila
084450080407      * del po emissione
084460080407     c                   if        ormpoe <> ormpor
084470080407     c                   clear                   fnlv55ds
084480080407     c                   eval      d55tpt = '6'
084490080407     c                   eval      d55lin = ormpoe
084500080407     c                   eval      d55drf = oggi_aammgg
084510080407     c                   call      'FNLV55R'
084520080407     c                   parm                    fnlv55ds
084530080407     c                   if        d55err <> *blanks
084540080407     c                   clear                   d55tfa
084550080407     c                   endif
084560080407     c                   endif
084570080407      * questo per parare gli errori dovuti a orm emessi da filiali in £6 con ritiro
084580080407      * presso la capofila
084590080407     c                   if        ormpoe = ormpor or ormpor = d55tfa
084600010704     C                   exsr      Sr_Immprep
084610010704
084620010704      * Non è stato creato il prepagato
084630010704      * devo annullare l'ORM appena creato
084640010704     C                   if        okbolla = *blanks
084650010704     C                   eval      §RMerr = '5'
084660010704      * Deleto l'ORM
084670010704     C                   clear                   fior30ds
084680010704     C                   eval      i30mod = 'D'
084690010704     C                   eval      i30poe = ormpoe
084700010704     C                   eval      i30nsr = ormnsr
084710010704     C                   eval      i30nor = ormnor
084720010704     C                   eval      i30nrv = ormnrv
084730010704     C                   call      'FIOR30R'
084740010704     C                   parm                    kpjba
084750010704     C                   parm                    fior30ds
084760010704
084770010704     C                   endif
084780080407     c                   endif
084790010704     C                   endif
084800010704
084810001026     C                   endif
084820001011
084830071108     ?* Aggiorna
084840010919   1bC                   if        *in02 = *on
084850071108     c                   select
084860071126      * sono in conferma proposte di variazione
084870071126     C                   when      §rmfpr = 'P'
084880071126     c                   eval      $modifica = *on
084890071126     C  n10              exsr      Sr_Storico
084900140227      /free
084910151023       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
084920151023       //?da rcd già vincolato da questo lavoro
084930151023         unlock FNORG01L;
084940140227       //?Richiamo pgm di gestione note per convalidare la proposta di variazione
084950140227       //?della nota
084960140227         chain (§RMpoe:§RMnsr:§RMnor:§RMnrv:§RMdtv:§RMorv) FNORPT1L;
084970140227         IF  %found(FNORPT1L);
084980140227           clear fiort1ds;
084990140227           IORT1mod = 'P';
085000140227           IORT1poe = ORMpoe;
085010140227           IORT1nsr = ORMnsr;
085020140227           IORT1nor = ORMnor;
085030140227           IORT1nrv = ORMnrv;
085040140227           IORT1dim = §RMdtv;
085050140227           IORT1oim = §RMorv;
085060140227           IORT1pgm = 'FIOR05R';
085070140227           §kpjbu = kpjbu;
085080140227           fiort1r (kpjba:fiort1ds);
085090140227           kpjbu = §kpjbu;
085100140227         ENDIF;
085110151023       //?Rialloco FNORG
085120151023         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
085130140227      /end-free
085140140227
085150010205      * Non sono in conferma orm da altri p.o.
085160071108     C                   when      §rmfpr <> 'O'
085170071108      * è stato modificato qualcosa
085180131018     c                             and (vidnew <> vidold
085190160406     c                               or vidnew2 <> vidold2
085200160406     c                               or vidnewA <> vidoldA
085210160406     c                               or vidnewC <> vidoldC)
085220071108      * non è un orm dirottato
085230071108     c                             and wokdirotta = *off
085240071108     c                   eval      $modifica = *on
085250071106      * se devo scrivere una proposta lo faccio subito
085260071106      * e poi esco dalla routine
085270140918     c                   if        $proposte = *on and not OrmFase420
085280071106     c                   exsr      sr_carproposte
085290080225     c                   unlock    fnorm01l
085300151023     c                   unlock    fnorg01l
085310071106     c                   goto      endsrconf
085320071106     c                   endif
085330071108      * controllo se devo scrivere lo Storico o la proposta di variazione
085340150305    2C                   if        %lookup(%editc(ormpor:'X'):skpog) > 0
085350071108      * scrivo lo storico
085360170605    3C*******            if        num_v1cpor <> v1cpoe
085370010222     C  n10              exsr      Sr_Storico
085380170605   3xC*******            else
085390170605    4C*******            if        v1fao >=300
085400170605     C**n10              exsr      Sr_Storico
085410170605    4C*******            endif
085420170605    3C*******            endif
085430071108      * scrivo la proposta
085440071108   2xC                   else
085450010202     C                   exsr      Sr_CarProposte
085460080225     c                   unlock    fnorm01l
085470151023     c                   unlock    fnorg01l
085480010919     C                   goto      endsrconf
085490071108    2C                   endif
085500071108      * Non sono in conferma orm da altri p.o.
085510071108     C                   when      §rmfpr <> 'O'
085520071108      * è un orm dirottato
085530071108     c                             and wokdirotta = *on
085540071108     c                   eval      $modifica = *on
085550040615      * Storico
085560040615     c                   ExSr      Sr_Storico
085570040618      * scrivo la fase 200
085580040615     C                   clear                   fnorf000
085590040615     C                   eval      ORFpoe = v1cpoe
085600040615     C                   eval      ORFnsr = v1nsr
085610040615     C                   eval      ORFnor = v1nor
085620040615     C                   eval      ORFnrv = v1nrv
085630040615     C                   eval      ORFpog = OrmPor
085640040615     C                   eval      ORFdae = v1dfo
085650040615     C                   eval      ORFore = v1ofo
085660051220     c                   Move      v1ofo         wora
085670051220     c                   subdur    1:*s          wora
085680051220     c                   move      wora          orfore
085690040618     C                   eval      ORFfar = 200
085700040615     C                   eval      ORFpue = knmus
085710040615     C                   write     fnorf000
085720040615      * scrivo fase 50
085730040615     C                   clear                   fnorf000
085740040615     C                   eval      ORFpoe = v1cpoe
085750040615     C                   eval      ORFnsr = v1nsr
085760040615     C                   eval      ORFnor = v1nor
085770040615     C                   eval      ORFnrv = v1nrv
085780040615     C                   eval      ORFpog = num_v1cpor
085790040615     C                   eval      ORFdae = v1dfo
085800040615     C                   eval      ORFore = v1ofo
085810040615     C                   eval      ORFfar = v1fao
085820040615     C                   eval      ORFpue = knmus
085830040615     C                   write     fnorf000
085840160517     C                   feod      fnorf01l
085850040615      * pulisco i campi che non devo reimpostare
085860040615     c                   Clear                   v1npg
085870040615     c                   Clear                   v1ndc
085880040615     c                   Clear                   v1ddc
085890040615     c                   Clear                   v1dst
085900140918
085910010919      * se sono in conferma ORM da altri p.o. devo storicizzare
085920010919      * se sono state fatte delle variazioni
085930071108     C                   when      §rmfpr = 'O'
085940071108     c                   eval      $modifica = *on
085950030609      * Prima di confermare la fase 100 dell'orm genero la bolla 'FT' di orm commissionato
085960030623      * se cod.ordinante codificato e se non è un ORM prepagato
085970071108    2c                   If        v1com = 'S' and
085980030610     c                             ds_V1ccor > *Zeros and
085990030610     c                             ds_V1cor2 <> 8888 and ds_V1cor2 <> 9999
086000030623     c                             and v1ctor <> 'P'
086010041130      * non deve esserci già una fase 200
086020041130     c                   Eval      wfase200 = *Off
086030041130     C     kfnor         setll     fnorf01l
086040071108    3c                   Do        *Hival
086050041130     C     kfnor         reade(n)  fnorf01l
086060041130     C                   if        %eof (fnorf01l)
086070041130     c                   Leave
086080041130     c                   EndIf
086090071108    4C                   If        orffar = 200
086100041130     c                   eval      wfase200 = *On
086110041130     c                   Leave
086120071108    4c                   EndIF
086130071108    3c                   EndDo
086140071108    3c                   If        wfase200 = *Off
086150030609     c                   Exsr      Sr_BollaFt
086160030609      * se f12 o f03 da immissione bolla riemetto la videata
086170071108    4c                   If        okbolla = '1'
086180030609     c                   Eval      *in28 = *On
086190030609     c                   GoTo      endsrconf
086200071108    4c                   EndIf
086210140506      /free
086220140506       //?se OK bolle e sto confermando un ORM commissionato con Alert
086230140506       //?imposto già che l'ORM è confermato così poi esce dal FIOR05R
086240140506         IF  walert;
086250140506           wormconf = *on;
086260140506         ENDIF;
086270140506      /end-free
086280071108    3c                   EndIf
086290071108    2c                   EndIf
086300030612      * storicizzo se ho fatto vedere la videata
086310131018     C                   if        (Vidnew <> Vidold
086320131211     c                              or vidnew2 <> vidold2
086330160406     c                              or vidnewA <> vidoldA
086340160406     c                              or vidnewC <> vidoldC)
086350030612     c                             and §rmtla = '2'
086360010919     C                   exsr      Sr_Storico
086370071108     C                   endif
086380050322      * sto dirottatando l'orm invece di confermarlo
086390071108    2c                   if        wokdirotta = *On
086400050322      * scrivo la fase 200
086410050322     C                   clear                   fnorf000
086420050322     C                   eval      ORFpoe = v1cpoe
086430050322     C                   eval      ORFnsr = v1nsr
086440050322     C                   eval      ORFnor = v1nor
086450050322     C                   eval      ORFnrv = v1nrv
086460050322     C                   eval      ORFpog = OrmPor
086470050322     C                   eval      ORFdae = v1dfo
086480050322     C                   eval      ORFore = v1ofo
086490070125     c                   Move      v1ofo         wora
086500070125     c                   subdur    1:*s          wora
086510070125     c                   move      wora          orfore
086520050322     C                   eval      ORFfar = 200
086530050322     C                   eval      ORFpue = knmus
086540050322     C                   write     fnorf000
086550160517     c                   feod      fnorf01l
086560071108    2c                   EndIf
086570071108     c                   endsl
086580140918
086590010202      * Scrive fase per chisura o conferma orm da altri p.o.
086600010202     C                   if        *in10 = *on or §RMfpr = 'O'
086610010202     C                   exsr      Sr_Carfilef
086620010202     C                   endif
086630140918
086640090211      * memorizzo i dati dell'appuntamento
086650150305     c                   if        §RMfpr = 'O'
086660090330     c                             and wokdirotta = *off
086670090407     c                             and wokapp = *on
086680090407      * l'appuntamento va inserito in fase 100 quindi se sto chiudendo
086690090407      * l'ORM prima scrivo la fase 100 poi scrivo i dati dell'appuntamento
086700090407     c                   if        *in10
086710090407     C                   clear                   fnorf000
086720090407     C                   eval      ORFpoe = ormpoe
086730090407     C                   eval      ORFnsr = ormnsr
086740090407     C                   eval      ORFnor = ormnor
086750090407     C                   eval      ORFnrv = ormnrv
086760090407     C                   eval      ORFpog = ormpor
086770090407     C                   eval      ORFdae = v1dfo
086780090407     C                   eval      ORFore = v1ofo
086790090407     c                   Move      v1ofo         wora
086800090407     c                   subdur    1:*s          wora
086810090407     c                   move      wora          orfore
086820090407     C                   eval      ORFfar = 100
086830090407     C                   eval      ORFpue = knmus
086840090407     C                   write     fnorf000
086850160517     c                   feod      fnorf01l
086860090407     c                   endif
086870090211     c                   exsr      sr_wrtapp
086880090211     c                   endif
086890070119
086900070119      * salvo i dati modificati sensibili per PDA
086910070123      * se p.o. abilitato al PDA
086920140122      * o se filiale ritiro abilitata alle telefonate AUT
086930070119      * se sono in modifica (se proposta variazione non arriva qua)
086940070119      * se ORM in fase 400 o 410
086950070119      * negli altri casi non serve
086960140122     c                   if        *in02 and
086970140317     c                             *in22 and
086980071210     c                             (v1fao = 400 or v1fao = 410)
086990070119     c                   exsr      sr_datipdamod
087000070119     c                   endif
087010070119
087020071108      * se aggiornato qualcosa
087030071108     c                   if        $modifica = *on
087040001023      * Aggiorna ORM
087050001011     C                   exsr      Sr_Carfile
087060151016      * aggiorno fnorg (GIRO)
087070151016     c                   exsr      sr_aggorg
087080131018      /free
087090131018       //?Se modificati i dati win alert o dati orari apertura
087100161028       //?o se modificata data pronta merce
087110131029       //?o se conferma proposta di variazione
087120131018       //?aggiorno FNORE
087130131018        IF  old_v1OraAMda <> v1OraAMda or
087140131018            old_v1OraAMa  <> v1OraAMa  or
087150131018            old_v1OraPMda <> v1OraPMda or
087160131018            old_v1OraPMa  <> v1OraPMa  or
087170161028            old_v1dpm <> v1dpm or
087180131029            vidoldA <>  vidnewA or
087190131029            §RMfpr = 'P';
087200131018          exsr aggFNORE;
087210131018        ENDIF;
087220131018      /end-free
087230001108     C                   update    fnorm000
087240090202     c                   else
087250090202     c                   UNLOCK    FNORM01L
087260151023     c                   unlock    fnorg01l
087270071108     c                   endif
087280070119
087290070123      * se il p.o. è abilitato al PDA
087300140122      * o abilitato alle telefonate AUT
087310140317     c                   if         *in22  and
087320070228      * non è un dirottamento o chiusura ORM
087330070228     c                             (wokdirotta = *off or not *in10) and
087340070119      * l'orm è in fase 400 o 410
087350080314     c                             (ormfao = 400 or ormfao = 410) and
087360070119      * almeno uno dei dati sensibili per PDA è stato variato
087370070119     c                             datipdasav <> datipdamod
087380140123       //?Verifico cosa è stato variato
087390140122     c                   exsr      ctrvardati
087400070122      * aggiorno i dati inviati al PDA
087410070124     c                   clear                   fior56ds
087420070124     c                   eval      or56tla = 'O'
087430080121     c                   eval      or56fgs = orffgs
087440080121     c                   eval      or56ndcd = orfndc
087450080121     c                   eval      or56ndca = orfndc
087460140120     c                   eval      OR56cmd = 'V'
087470140122     c                   eval      OR56var = wVardati
087480070119     c                   exsr      sr_aggpdo
087490070119     c                   endif
087500070119
087510010919   1eC                   endif
087520001106
087530001109     C                   eval      *in10 = *off
087540001011
087550001019     C     endsrconf     endsr
087560010202      **********************************************************************
087570010202      * F7 - Ricerca cliente ritiro
087580010202      **********************************************************************
087590010202     C     Sr_RicRitiro  BEGSR
087600010202
087610010202     C                   eval      *in90 = *on
087620010202
087630130916     c                   clear                   FIOR81DS
087640130916     c                   IF        V1cra1 > 0
087650130916     c                   eval      I81fil = V1cra1
087660130916     c                   ENDIF
087670130916     c                   IF        V1rsr <> *blanks
087680130916     c                   eval      I81rag = V1rsr
087690130916     c                   ENDIF
087700150907     c                   IF        V1prr <> *blanks
087710150907     c                   eval      I81prv = V1prr
087720150907     c                   ENDIF
087730130916     c                   call      'FIOR81R'
087740130916     c                   parm                    kpjba
087750130916     c                   parm                    FIOR81DS
087760130916     c                   IF        O81err <> *blanks
087770130916     c                   eval      V1Cmsg = O81msg
087780130916     c                   seton                                        41  28
087790130916     c                   leavesr
087800130916     c                   ENDIF
087810130916     c                   IF        O81ret = '1'
087820130916     c                   leavesr
087830130916     c                   ENDIF
087840130916
087850130916     c                   eval      ds_v1ccra = O81cro
087860070905     c                   eval      v1cra1 = ds_v1cra1
087870070905     c                   eval      v1cra2 = ds_v1cra2
087880070905     c                   eval      v1cra3 = ds_v1cra3
087890070905     c                   exsr      sr_contrclir
087900010202
087910010202     C                   endsr
087920010202      **********************************************************************
087930010202      * F8 - Ricerca cliente ordinante
087940010202      **********************************************************************
087950010202     C     Sr_RicOrdin   BEGSR
087960010202
087970010202     C                   eval      *in90 = *on
087980070905
087990130916     c                   clear                   FIOR81DS
088000130916     c                   IF        V1cor1 > 0
088010130916     c                   eval      I81fil = V1cor1
088020130916     c                   ENDIF
088030130916     c                   IF        V1rso <> *blanks
088040130916     c                   eval      I81rag = V1rso
088050130916     c                   ENDIF
088060150907     c                   IF        V1pro <> *blanks
088070150907     c                   eval      I81prv = V1pro
088080150907     c                   ENDIF
088090130916     c                   call      'FIOR81R'
088100130916     c                   parm                    kpjba
088110130916     c                   parm                    FIOR81DS
088120130916     c                   IF        O81err <> *blanks
088130130916     c                   eval      V1Cmsg = O81msg
088140130916     c                   seton                                        42  28
088150130916     c                   leavesr
088160130916     c                   ENDIF
088170130916     c                   IF        O81ret = '1'
088180130916     c                   leavesr
088190130916     c                   ENDIF
088200130916
088210130916     c                   eval      ds_v1ccor = O81cro
088220070905     c                   eval      v1cor1 = ds_v1cor1
088230070905     c                   eval      v1cor2 = ds_v1cor2
088240070905     c                   eval      v1cor3 = ds_v1cor3
088250070905     c                   exsr      sr_contrclio
088260010202
088270010202     C                   endsr
088280010202      **********************************************************************
088290011113      * F9 - Ricerca cliente destinatario
088300010202      **********************************************************************
088310010202     C     Sr_RicCons    BEGSR
088320010202
088330010202     C                   eval      *in90 = *on
088340070905
088350130916     c                   clear                   FIOR81DS
088360130916     c                   IF        V1crc1 > 0
088370130916     c                   eval      I81fil = V1crc1
088380130916     c                   ENDIF
088390130916     c                   IF        V1rsc <> *blanks
088400130916     c                   eval      I81rag = V1rsc
088410130916     c                   ENDIF
088420150907     c                   IF        V1prc <> *blanks
088430150907     c                   eval      I81prv = V1prc
088440150907     c                   ENDIF
088450130916     c                   call      'FIOR81R'
088460130916     c                   parm                    kpjba
088470130916     c                   parm                    FIOR81DS
088480130916     c                   IF        O81err <> *blanks
088490130916     c                   eval      V1Cmsg = O81msg
088500130916     c                   seton                                        56  28
088510130916     c                   leavesr
088520130916     c                   ENDIF
088530130916     c                   IF        O81ret = '1'
088540130916     c                   leavesr
088550130916     c                   ENDIF
088560130916
088570130916     c                   eval      ds_v1ccrc = O81cro
088580070905     c                   eval      v1crc1 = ds_v1crc1
088590070905     c                   eval      v1crc2 = ds_v1crc2
088600070905     c                   eval      v1crc3 = ds_v1crc3
088610070905     c                   exsr      sr_contrclid
088620010202
088630010202     C                   endsr
088640010202      **********************************************************************
088650010202      * F11 - Calcola Volume
088660010202      **********************************************************************
088670010202     C     Sr_Volume     BEGSR
088680010202
088690040301     c                   clear                   UL36vol
088700040301     c                   call      'TRUL36R'
088710040301     c                   parm                    UL36vol
088720040301     c                   add       UL36vol       V1vlm
088730010202
088740010202     C                   endsr
088750010202      **********************************************************************
088760010202      * F16 - Chiusura
088770010202      **********************************************************************
088780010202     C     Sr_Chiusura   BEGSR
088790010202
088800081125     c                   eval      wnobolla =  *off
088810020523     c                   eval      *in17 = *off
088820010202     C                   clear                   v3cau
088830010202     C                   clear                   v3dcau
088840010202     C                   eval      v3cpoe = ppoe
088850010202     C                   eval      v3nsr  = pnsr
088860010202     C                   eval      v3nor  = pnor
088870010202     C                   eval      v3nrv  = pnrv
088880071126
088890071126      * se è manutenzione
088900130412     c                   if        *in02 and §rmtla = *blanks and
088910071126      * ORM assegnato
088920071126      * scrivo proposta variazione
088930080314     c                             (v1fao = 400 or v1fao = 410)
088940071126     c                   eval      $proposte = *on
088950071126     c                   endif
088960071126
088970010202      * Videata con richiesta causale di chiusura (obbligatoria)
088980010227     C                   do        *hival
088990010202     C                   exfmt     fior05c
089000010301     C                   setoff                                       28
089010010202      * F12=Ritorno e non chiudo
089020050609     c   kl              clear                   v3cau
089030010202     C   kl              goto      endsrchiuso
089040070228
089050070228      * se il p.o. che chiude è abilitato al PDA
089060070228      * e fase 400 o 410
089070071210     c                   if        *in22 and
089080070228     c                             (ormfao = 400 or ormfao = 410)
089090070228      * devo avvisare che ho già avuto un esito da AUT
089100070228      * se l'orm è in fase 410
089110070228     c                   if        ormfao = 410 and wforzachi = *off
089120070228     c                   eval      v3cmsg = msg(41)
089130070228     c                   seton                                        28
089140070228     c                   eval      wforzachi = *on
089150070228     c                   iter
089160070228     c                   endif
089170070228     c                   endif
089180070228
089190010202      * Causale obbligatoria
089200010202     C                   if        v3cau = *blanks
089210010202     C                   movel     msg(14)       v3cmsg
089220010202     C                   clear                   v3dcau
089230010202     C                   eval      *in28 = *on
089240010202     C                   iter
089250010202     C                   endif
089260010302
089270010302     C                   clear                   v3dcau
089280010202      * Ricerco causale
089290010202     C     '?'           scan      v3cau                                  30
089300010202     C     *in30         ifeq      *on
089310010202     C                   clear                   TIBS02DS
089320010301     C                   clear                   dcmr
089330010202     C                   movel     'R'           t02mod
089340010202     C                   movel     knsif         t02sif
089350010227     C                   movel     'CMR'         t02cod
089360010202     C                   call      'TIBS02R'
089370010202     C                   parm                    KPJBA
089380010202     C                   parm                    TIBS02DS
089390010202     C                   movel     T02ke1        v3cau
089400010301     C                   movel     T02uni        dcmr
089410010202     C                   iter
089420010202     C                   endif
089430010202      * Decodifico
089440010301     C                   clear                   dcmr
089450010202     C                   clear                   TIBS02DS
089460010202     C                   movel     'C'           T02mod
089470010202     C                   movel     knsif         t02sif
089480010227     C                   movel     'CMR'         t02cod
089490010202     C                   movel(p)  v3cau         T02ke1
089500010202     C                   call      'TIBS02R'
089510010202     C                   parm                    KPJBA
089520010202     C                   parm                    TIBS02DS
089530010202     C                   if        t02err <> *blanks
089540010202     C                   movel     msg(15)       v3cmsg
089550010202     C                   eval      *in28 = *on
089560010202     C                   iter
089570010202     C                   endif
089580010202     C                   movel     t02ke1        v3cau
089590010301     C                   movel     t02uni        dcmr
089600010622
089610010301     C                   movel     d§cmrdes      v3dcau
089620030912
089630030912      * nei casi in cui il flag sia impostato a 'E' o a 'R'
089640030912      * se p.o. ritiro diverso da p.o. emissione controllo se posso usare la causale
089650030912     c                   If        Ormpoe <> OrmPor
089660030912      * se impostato a 'E' il p.o. emissione non può usare la causale
089670151014     c                   If        d§CmrNoc = 'E' and  ORMpoe = DUTpou and
089680150622     c                             %lookup(%editc(ORMpoe:'X'):skpog) > 0
089690030806     C                   movel     msg(64)       v3cmsg
089700030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089710070418     c                             'dalla filiale emissione'
089720030806     C                   eval      *in28 = *on
089730030806     C                   iter
089740030806     c                   EndIf
089750030912      * se impostato a 'R' il p.o. ritiro non può usare la causale
089760151014     c                   If        d§CmrNoc = 'R' and  ORMpor = DUTpou and
089770150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
089780030806     C                   movel     msg(64)       v3cmsg
089790030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089800070418     c                             'dalla filiale ritiro'
089810030806     C                   eval      *in28 = *on
089820030806     C                   iter
089830030806     c                   EndIf
089840030912     c                   EndIf
089850030912      * se impostato a 'S' non si può usare e basta
089860030806     C                   if        d§cmrnoc = 'S'
089870010305     C                   movel     msg(64)       v3cmsg
089880010302     C                   eval      *in28 = *on
089890010302     C                   iter
089900010302     C                   endif
089910100218
089920100218      /free
089930100218       //?Se ORM NON prevede il preavviso non posso utilizzare la nuova causale '98'
089940100218       IF  §ormpre <> 'M' and v3cau = '98';
089950100218         v3cmsg = msg(64);
089960100218         v3cmsg = %trim(v3cmsg) + '. Il ritiro non prevede il PREAVVISO';
089970100218         *in28 = *on;
089980100218         iter;
089990100218       ENDIF;
090000100218      /end-free
090010081028
090020081124      * se causale inserita genera bolla di addebito (colpa cliente)
090030081028      * controllo che l'ORM non abbia già avuto una causale che genera addebito
090040081125      * in questo caso faccio solo un addebito
090050081124     c                   if        d§cmradd = 'S'
090060081028     c     kfnor         setll     fnorf01l
090070081028     c                   do        *hival
090080081028     c     kfnor         reade(n)  fnorf01l
090090081028     c                   if        %eof (fnorf01l)
090100081028     c                   leave
090110081028     c                   endif
090120081029      * senza causale rileggo
090130081028     c                   if        orfcar = *blanks
090140081028     c                   iter
090150081028     c                   endif
090160081029      * fase da PDA rileggo
090170081029     c                   if        orffar = 410
090180081029     c                   iter
090190081029     c                   endif
090200081028     c                   clear                   a_dcmr
090210081028     c                   clear                   tibs02ds
090220081028     c                   eval      t02mod = 'C'
090230081028     c                   eval      t02sif = knsif
090240081028     c                   eval      t02cod = 'CMR'
090250081028     c                   eval      t02ke1 = orfcar
090260081028     c                   call      'TIBS02R'
090270081028     c                   parm                    kpjba
090280081028     c                   parm                    tibs02ds
090290081028     c                   eval      a_dcmr = t02uni
090300081124      * se causale che genera bolla vado avanti senza generare l'addebito per la seconda volta
090310081028     c                   if        a_d§cmradd = 'S'
090320081125     c                   eval      wnobolla = *on
090330081028     c                   leave
090340081028     c                   endif
090350081028     c                   enddo
090360081028     c                   endif
090370081007
090380081007      * se ORM automatico DPD non si può chiudere l'ORM con la causale 80
090390081007      * l'unica eccezione è per la generazione automatica da parte dei pgm di AB
090400081007      * che creano la proposta di chiusura con causale 80
090410081007     c     ormpoe        lookup    skpodpd                                30
090420081007     c                   if        §rmfpr <> 'P' and ormtco = 'F' and
090430081007     c                             *in30 and v3cau = '80'
090440081007     c                   eval      v3cmsg = msg(49)
090450081007     c                   eval      *in28 = *on
090460081007     c                   iter
090470081007     c                   endif
090480081007
090490010227      * Conferma chiusura
090500010227     C                   if        *inkf = *on
090510071126      * se non è una proposta di variazione
090520071126     c                   if        $proposte = *off
090530020524      * procedo con i controlli per creare l'addebito solo se l'orm è mio
090540150306     c                   if        ormpoe = ormpor or
090550150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
090560020523     c                   if        d§cmradd <> *blanks
090570080314     c                             and (ormfao = 400 or ormfao = 410)
090580020523     c                   exsr      sr_contradd
090590020523      * se f12 o f03 da immissione bolla riemetto la videata
090600020523     c                   if        okbolla = '1'
090610020523     c                   iter
090620020523     c                   endif
090630020524     c                   if        wbolla = *on
090640020523     c                   eval      *in17 = *on
090650020524     c                   endif
090660020523     c                   endif
090670020524     c                   endif
090680071126     c                   endif
090690010202
090700010202      * Salva dati della fase nel caso l'utente cambi idea (f12)
090710010202     C                   eval      sav_v1fao = v1fao
090720010202     C                   eval      sav_v1dfo = v1dfo
090730010202     C                   eval      sav_v1ofo = v1ofo
090740070228      * salvo la fase anche per aggiornare FIPDO
090750070228     c                   eval      savorffar = orffar
090760070228      * e salvo anche la distinta per passarla al pgm che aggiorna FIPDO
090770070228     c                   eval      savorffgs = orffgs
090780070228     c                   eval      savorfndc = orfndc
090790070228     c                   eval      savorfddc = orfddc
090800150922      * imposto peso/volume di ORG in modo da non azzerarlo al momento della conferma
090810150922      * della chiusura ORM
090820150922     c                   eval      wpkg = ORGpkg
090830150922     c                   eval      wvlm = ORGvlm
090840010202
090850010301     C                   movel     d§cmrfar      v1fao                          chiuso
090860050422     C                   z-add     oggi_aammgg   v1dfo
090870050422     C                   time                    w0140
090880010202     C                   movel     w0140         v1ofo
090890010202     C                   eval      *in10 = *on
090900010305
090910010305     C                   eval      v1cau = v3cau
090920010305     C                   eval      v1dcau = v3dcau
090930010301
090940010227     C                   leave
090950010227     C                   endif
090960010227     C
090970010227     C                   enddo
090980010202      * Decodifica la fase
090990010202     C                   exsr      Sr_Contrfao
091000131025
091010131025      /free
091020131025       //?Imposto la riga dei tasti funzione
091030131025       ExSr Sr_Tastifun;
091040131025      /end-free
091050010202
091060010202     C     endsrchiuso   endsr
091070020522      **********************************************************************
091080020522      * Controllo se devo generare la bolla di addebito
091090020522      **********************************************************************
091100020522     c     sr_contradd   begsr
091110020522
091120020522     c                   clear                   sav_lpksc
091130020523     c                   clear                   okbolla
091140020524     c                   eval      wbolla = *off
091150081125
091160081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
091170081125      * come se avessi fatto la bolla
091180081125     c                   if        wnobolla = *on
091190081125     c                   eval      wbolla = *on
091200081125     c                   leavesr
091210081125     c                   endif
091220020522
091230020522      * se cod.ordinante codificato addebito
091240020522 b1  c                   if        ds_v1ccor <> *zeros and v1cor2 <> 8888
091250020522     c                             and v1cor2 <> 9999
091260020523     c                   clear                   dblp
091270090504     c                   movel     ormcor        §lpksca
091280020702      * se c'è imposto il codice bollettazione
091290151109     c     ormcor        chain(n)  fnacr01l
091300020702     c                   if        %found(fnacr01l) and acratb = *blanks
091310020702     c                             and acrksc <> *zeros
091320090504     c                   move      acrksc        §lpksca
091330090505     c                   if        acrccc<>999
091340090504     c                   move      acrccc        §lpctra
091350090505     c                   endif
091360020702     c                   endif
091370020522     c                   eval      §lprsd = ormrso
091380020522     c                   eval      §lpind = ormino
091390020522     c                   eval      §lpcad = ormcao
091400020522     c                   eval      §lplod = ormloo
091410020522     c                   eval      §lpprd = ormpro
091420020522     c                   eval      §lpnzd = ormnao
091430020522     c                   exsr      sr_wrtbolla
091440020522 x1  c                   else
091450020522      * se non inserito l'ordinante
091460020522 b2  c                   if        v1rso = *blanks
091470050323      * orm locale
091480050323 b3  c                   if        v1cpoe = num_v1cpor
091490020522      * se cod.mittente codificato addebito
091500020522 b4  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
091510020522     c                             and v1cra2 <> 9999
091520020523     c                   clear                   dblp
091530090504     c                   movel     ormcra        §lpksca
091540020702      * se c'è imposto il codice bollettazione
091550151109     c     ormcra        chain(n)  fnacr01l
091560020702     c                   if        %found(fnacr01l) and acratb = *blanks
091570020702     c                             and acrksc <> *zeros
091580090504     c                   move      acrksc        §lpksca
091590090505     c                   if        acrccc<>999
091600090504     c                   move      acrccc        §lpctra
091610090505     c                   endif
091620020702     c                   endif
091630020522     c                   eval      §lprsd = ormrsr
091640020522     c                   eval      §lpind = orminr
091650020522     c                   eval      §lpcad = ormcar
091660020522     c                   eval      §lplod = ormlor
091670020522     c                   eval      §lpprd = ormprr
091680020522     c                   eval      §lpnzd = ormnar
091690020522     c                   exsr      sr_wrtbolla
091700020522      * se cod.mittente non codificato
091710020522 x4  c                   else
091720020522      * se cod.destinatario codificato addebito
091730020522 b5  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
091740020522     c                             and v1crc2 <> 9999
091750020523     c                   clear                   dblp
091760090504     c                   movel     ormcrc        §lpksca
091770020702      * se c'è imposto il codice bollettazione
091780151109     c     ormcrc        chain(n)  fnacr01l
091790020702     c                   if        %found(fnacr01l) and acratb = *blanks
091800020702     c                             and acrksc <> *zeros
091810090504     c                   move      acrksc        §lpksca
091820090505     c                   if        acrccc<>999
091830090504     c                   move      acrccc        §lpctra
091840090505     c                   endif
091850020702     c                   endif
091860020522     c                   eval      §lprsd = ormrsc
091870020522     c                   eval      §lpind = orminc
091880020522     c                   eval      §lpcad = ormcac
091890020522     c                   eval      §lplod = ormloc
091900020522     c                   eval      §lpprd = ormprc
091910020522     c                   eval      §lpnzd = ormnac
091920020522     c                   exsr      sr_wrtbolla
091930020522 e5  c                   endif
091940020522 e4  c                   endif
091950050323      * orm non locale
091960020522 x3  c                   else
091970020522      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
091980020522 b4  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
091990020522     c                             and v1crc2 <> 9999  and v1crc1 = v1cpoe
092000020523     c                   clear                   dblp
092010090504     c                   movel     ormcrc        §lpksca
092020020702      * se c'è imposto il codice bollettazione
092030151109     c     ormcrc        chain(n)  fnacr01l
092040020702     c                   if        %found(fnacr01l) and acratb = *blanks
092050020702     c                             and acrksc <> *zeros
092060090504     c                   move      acrksc        §lpksca
092070090505     c                   if        acrccc<>999
092080090504     c                   move      acrccc        §lpctra
092090090505     c                   endif
092100020702     c                   endif
092110020522     c                   eval      §lprsd = ormrsc
092120020522     c                   eval      §lpind = orminc
092130020522     c                   eval      §lpcad = ormcac
092140020522     c                   eval      §lplod = ormloc
092150020522     c                   eval      §lpprd = ormprc
092160020522     c                   eval      §lpnzd = ormnac
092170020522     c                   exsr      sr_wrtbolla
092180020522      * se non codificato o p.o. differenti
092190020522 x4  c                   else
092200020522      * se cod.mittente codificato addebito
092210020522 b5  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
092220020522     c                             and v1cra2 <> 9999
092230020523     c                   clear                   dblp
092240090504     c                   movel     ormcra        §lpksca
092250020702      * se c'è imposto il codice bollettazione
092260151109     c     ormcra        chain(n)  fnacr01l
092270020702     c                   if        %found(fnacr01l) and acratb = *blanks
092280020702     c                             and acrksc <> *zeros
092290090504     c                   move      acrksc        §lpksca
092300090505     c                   if        acrccc<>999
092310090504     c                   move      acrccc        §lpctra
092320090505     c                   endif
092330020702     c                   endif
092340020522     c                   eval      §lprsd = ormrsr
092350020522     c                   eval      §lpind = orminr
092360020522     c                   eval      §lpcad = ormcar
092370020522     c                   eval      §lplod = ormlor
092380020522     c                   eval      §lpprd = ormprr
092390020522     c                   eval      §lpnzd = ormnar
092400020522     c                   exsr      sr_wrtbolla
092410020522 e5  c                   endif
092420020522 e4  c                   endif
092430020522 e3  c                   endif
092440020522 e2  c                   endif
092450020522 e1  c                   endif
092460020522
092470020522     c                   endsr
092480020522      **********************************************************************
092490020522      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
092500020522      **********************************************************************
092510020522     c     sr_wrtbolla   begsr
092520020522
092530030623     c                   Clear                   wtrulvid
092540020522      * richiamo il programma per la scelta delle stampanti
092550030307     c                   Clear                   wdatibolla
092560030307     c                   Eval      wdatibolla = 'BollaAdd.'
092570030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
092580030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
092590020522     c                   exsr      sr_scestp
092600020524     c   92              eval      okbolla = '1'
092610020522     c   92              goto      endwrtbolla
092620020522
092630020522     c                   clear                   kpjbu
092640020522     c                   clear                   dtasv
092650020522
092660020522     c                   eval      §lpfpr = 'T'
092670020522     c                   movel     ormpor        §lpccn
092680020522     c                   move      9999          §lpccn
092690020523     c                   movel     oggi_aammgg   §lpaas
092700020523     c                   move      oggi_aammgg   §lpmgs
092710020522     c                   eval      §lplnp = ormpor
092720020522     c                   eval      §lpcbo = 'FY'
092730020522     c                   eval      §lpsop = '+'
092740020530     c                   eval      §lplna = ormpoe
092750020522     c                   eval      §lprsm = ormrsr
092760020522     c                   eval      §lpinm = orminr
092770020522     c                   eval      §lpcam = ormcar
092780020522     c                   eval      §lplom = ormlor
092790020522     c                   eval      §lpprm = ormprr
092800020522     c                   eval      §lpnzm = ormnar
092810020522
092820020522 b1  c                   if        ormncl <> 0
092830020522     c                   eval      §lpncl = ormncl
092840020522 x1  c                   else
092850020522     c                   eval      §lpncl = 1
092860020522 e1  c                   endif
092870020522 b1  c                   if        ormpkg <> 0
092880020522     c                   eval      §lppkb = ormpkg
092890020522 x1  c                   else
092900070918 b3  c                   if        ormbnc <> 0
092910070918     c                   eval      wpkb = ormbnc / d§dftbnc * d§dftpkg
092920070918     c                   if        wpkb > 999999,9
092930070918     c                   eval      wpkb = 999999,9
092940070918     c                   endif
092950070918     c                   eval      §lppkb = wpkb
092960070918 x2  c                   else
092970020522 b2  c                   if        ormvlm <> 0
092980070918     c                   eval      wpkb = ormvlm * d§dftpkg
092990070918     c                   if        wpkb > 999999,9
093000070918     c                   eval      wpkb = 999999,9
093010070918     c                   endif
093020070918     c                   eval      §lppkb = wpkb
093030020522 e3  c                   endif
093040020522 e2  c                   endif
093050020522 e1  c                   endif
093060020522
093070020522     c                   clear                   ds_keyorm
093080020522     c                   eval      ds_poe = ORMpoe
093090020522     c                   eval      ds_nsr = ORMnsr
093100020522     c                   eval      ds_nor = ORMnor
093110020522     c                   eval      ds_nrv = ORMnrv
093120030716     c                   move      ds_keyorm     §lprmn
093130020522
093140020522     c                   eval      §lprma = ormrfa
093150020523     c                   eval      §lprmo = v3cau + '-' + d§cmrdes1
093160020522     c                   eval      §lpctm = '4F'
093170020523     c                   movel     oggi_aammgg   §lpdrt
093180020522
093190020522      * Cerca codice autotrasportatore e flag mattino/pomeriggio
093200020522 b1  c                   if        ormndc <> 0
093210120612      /free
093220120612        DSTnpg = ORMnpg;
093230120612        DSTnfv = ORMndc;
093240120612        DSTfgs = ORMpor;
093250120612        chain (DSTnpg:DSTnfv:DSTfgs) FIDST01L;
093260120612      /end-free
093270120612 b2  c                   if        %found(fidst01l)
093280020522     c                   eval      §lppdr = DSTpdr
093290120612     c                   eval      §lpfpp = DSTfpp
093300020523 x2  c                   else
093310020523     c                   movel     §lplnp        §lppdr
093320020523     c                   move      0999          §lppdr
093330020523     c                   eval      §lpfpp = 'M'
093340020523 e2  c                   endif
093350020522 x1  c                   else
093360020522     c                   movel     §lplnp        §lppdr
093370020522     c                   move      0999          §lppdr
093380020522     c                   eval      §lpfpp = 'M'
093390020522 e1  c                   endif
093400020522
093410020522     C                   call      'FNLS01R'
093420020522     C                   parm                    kpjba
093430020522     C                   parm                    dblp
093440020522     C                   parm                    dtasv
093450030623     c                   Parm                    Trul90ds
093460020522
093470020522     C                   if        §lpfpr = '4' or §lpfpr = '3'
093480020522     C                             or §lpfpr = '5'
093490020522     C                   eval      okbolla = '1'
093500020524     c                   else
093510020524     c                   eval      wbolla = *on
093520020522     C                   endif
093530020522
093540020522     c     endwrtbolla   endsr
093550030606      **********************************************************************
093560030606      * Scrivo bolla addebito "FT"
093570030606      **********************************************************************
093580030606     c     Sr_BollaFt    BegSr
093590030609
093600030609     c                   Clear                   okbolla
093610030623     c                   Eval      wtrulvid = 'S'
093620030623
093630030623      * richiamo il programma per la scelta delle stampanti
093640030623     c                   Clear                   wdatibolla
093650030623     c                   Eval      wdatibolla = 'BollaAdd.'
093660030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
093670030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
093680030623     c                   exsr      sr_scestp
093690030623     c   92              eval      okbolla = '1'
093700030623     c   92              GoTo      endbollaft
093710030606
093720030606     c                   Clear                   Kpjbu
093730030606     c                   Clear                   dtasv
093740030606     c                   Clear                   dblp
093750030606
093760030606     c                   Eval      §lpFpr = 'T'
093770030606      * Cod.Cliente mittente
093780030606      * Ksc dell'ordinante da Fnacr
093790030606     c                   If        AcrKscCor <> *Zeros
093800090504     c                   Eval      §lpksca = %editc(AcrKscCor:'X')
093810090505     c                   if        ACrCCCCor<>999
093820090505     c                   move      AcrCccCor     §lpctra
093830090505     c                   endif
093840030606     c                   Else
093850030606      * Primi 7 byte dell'ordinante
093860090504     c                   Eval      §lpksca = %editc(Ds_V1cor12:'X')
093870030606     c                   EndIf
093880030606
093890050324     c                   Movel     OrmPor        §lpCcn
093900030606     c                   Move      9999          §lpCcn
093910030606     c                   Movel     oggi_aammgg   §lpAas
093920030606     c                   Move      oggi_aammgg   §lpMgs
093930050324     c                   Eval      §lpLnp = OrmPor
093940030606     c                   Eval      §lpCbo = 'FT'
093950030606     c                   Eval      §lpSop = '+'
093960090504     c                   Movel     §lpksca       §lpLna
093970030606      * Mittente bolla = mittente ORM
093980030606     c                   Eval      §lpRsm = V1rsr
093990030606     c                   Eval      §lpInm = V1inr
094000030606     c                   Eval      §lpCam = V1car
094010030606     c                   Eval      §lpLom = V1lor
094020030606     c                   Eval      §lpPrm = V1prr
094030030606     c                   Eval      §lpNzm = V1nar
094040030606      * Destinatario bolla = ordinante ORM
094050030606     c                   Eval      §lpRsd = V1rso
094060030606     c                   Eval      §lpInd = V1ino
094070030606     c                   Eval      §lpCad = V1cao
094080030922     c                   Eval      §lpLod = sav_V1loo35
094090030606     c                   Eval      §lpPrd = V1pro
094100030606     c                   Eval      §lpNzd = V1nao
094110030606      * colli = colli ORM - se a 0 imposto 1
094120030609 b1  c                   if        V1ncl <> 0
094130030609     c                   eval      §lpncl = V1ncl
094140030606 x1  c                   else
094150030606     c                   eval      §lpncl = 1
094160030606 e1  c                   endif
094170030606      * peso = peso ORM - se a 0 ricavo dal volume o dai bancali dell'ORM
094180030609 b1  c                   if        V1pkg <> 0
094190030609     c                   eval      §lppkb = V1pkg
094200030606 x1  c                   else
094210070918 b3  c                   if        V1bnc <> 0
094220070918     c                   eval      wpkb = v1bnc / d§dftbnc * d§dftpkg
094230070918     c                   if        wpkb > 999999,9
094240070918     c                   eval      wpkb = 999999,9
094250070918     c                   endif
094260070918     c                   eval      §lppkb = wpkb
094270070918 x2  c                   else
094280030609 b2  c                   if        V1vlm <> 0
094290070918     c                   eval      wpkb = v1vlm * d§dftpkg
094300070918     c                   if        wpkb > 999999,9
094310070918     c                   eval      wpkb = 999999,9
094320070918     c                   endif
094330070918     c                   eval      §lppkb = wpkb
094340030606 e3  c                   endif
094350030606 e2  c                   endif
094360030606 e1  c                   endif
094370030606      * rif. mittente numerico = n.ORM
094380030606     c                   clear                   ds_keyorm
094390030609     c                   eval      ds_poe = V1cpoe
094400030609     c                   eval      ds_nsr = V1nsr
094410030609     c                   eval      ds_nor = V1nor
094420030609     c                   eval      ds_nrv = V1nrv
094430030716     c                   move      ds_keyorm     §lprmn
094440030606      * rif. mittente alfabetico = rif. alfabetico ORM
094450030609     c                   eval      §lprma = V1rfa
094460030606      * trattamento merce
094470030606     c                   eval      §lpctm = '4F'
094480030606      * data ritiro merce
094490030606     c                   movel     oggi_aammgg   §lpdrt
094500030606
094510030606      * codice autotrasportatore e flag mattino/pomeriggio
094520030606     c                   Eval      §lpFpp = 'M'
094530030606     c                   Movel     §lpLnp        §lpPdr
094540030606     c                   Move      0999          §lpPdr
094550030606
094560030606     c                   Call      'FNLS01R'
094570030606     c                   Parm                    Kpjba
094580030606     c                   Parm                    dblp
094590030606     c                   Parm                    dtasv
094600030623     c                   Parm                    Trul90ds
094610030606
094620030606     c                   If        §lpFpr = '4' or §lpFpr = '3'
094630030606     c                             or §lpFpr = '5'
094640030606     c                   Eval      okbolla = '1'
094650030606     c                   EndIf
094660030606
094670030606     c     endbollaft    EndSr
094680140130
094690140130      /free
094700140130       //--------------------------------------------------------------------
094710140130       //?F17 - Note AUT.
094720140130       //--------------------------------------------------------------------
094730140130       BEGSR F17D01;
094740140130
094750140130       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
094760140130       //?da rcd già vincolato da questo lavoro
094770140130         unlock FNORG01L;
094780140130
094790140130         clear fiort1ds;
094800140226         IORT1mod = 'G';
094810140130         IORT1poe = ORMpoe;
094820140130         IORT1nsr = ORMnsr;
094830140130         IORT1nor = ORMnor;
094840140130         IORT1nrv = ORMnrv;
094850140130         IORT1pgm = 'FIOR05R';
094860140130         §kpjbu = kpjbu;
094870140130
094880140130         fiort1r (kpjba:fiort1ds);
094890140130         kpjbu = §kpjbu;
094900140226
094910140226       //?Se ho scritto note ho la data e ora di immissione
094920140226         IF  OORT1dim > 0 and OORT1oim > 0;
094930140226           wF17write = *on;
094940140226         ENDIF;
094950140130
094960140130       //?Rialloco FNORG
094970140130         chain (kpoe:knsr:knor:knrv) FNORG01L;
094980140314
094990140314       //?Controllo se da F17 sono state scritte delle proposte
095000140314         exsr sr_orp;
095010140130
095020140130       //?Imposto la riga dei tasti funzione
095030140130         ExSr Sr_Tastifun;
095040140130
095050140130       ENDSR;
095060140130
095070140130      /end-free
095080140130
095090001025      **********************************************************************
095100001025      * F18 - Note
095110001025      **********************************************************************
095120001025     C     Sr_Note       BEGSR
095130001025
095140001025     C                   clear                   fior06DS
095150010118     C                   eval      i06dta = v1dao
095160010118     C                   eval      i06poe = v1cpoe
095170010118     C                   eval      i06nor = v1nor
095180010118     C                   eval      i06nsr = v1nsr
095190010118     C                   eval      i06nrv = v1nrv
095200010118     C                   eval      i06far = v1fao
095210010301     C                   eval      i06dai = oggi_aammgg
095220010301     C                   movel     w0140         I06ori
095230010220     C                   if        v1cpoe = *zeros
095240150305     C                   eval      i06poe = DUTpou
095250010220     C                   endif
095260001026
095270001025     C                   select
095280001026     C                   when      *in01 = *on
095290001026     C                   movel     'M'           i06flm
095300001026
095310001109     C                   when      *in03 = *on
095320001109     C                   movel     'M'           i06flm
095330001026
095340001026     C                   when      *in02 = *on
095350040402      * richiamo particolare nel caso di orm commissionato e variazione data ritiro
095360040402     c                   If        ormdar <> inv_v1dar and v1Com = 'S'
095370040419     c                   Eval      I06Flm = 'D'
095380040402     c                   Else
095390001026     C                   movel     'V'           i06flm
095400050421      * se p.o. ritiro diverso da p.o. emissione e da p.o. gestione e non è chiusura ORM
095410050421      * il pgm va in interrogazione note non da la possibilità di inserirne.....
095420050421      * aggiungo che la data ritiro deve essere uguale
095430050421      * xchè per data ritiro diversa e non è un ORM commissionato devo dare la possibilità
095440050421      * di inserire la nota
095450010116     C                   if        v1cpoe <> num_v1cpor  and
095460150305     C                             %lookup(%editc(num_v1cpor:'X'):skpog) = 0
095470150305     C                             and *in10 = *off
095480050421     c                             and ormdar = inv_v1dar
095490001027     C                   movel     'I'           i06flm
095500001027     C                   endif
095510050420     c                   EndIf
095520050128      * se sto facendo una proposta di chiusura ORM la fase deve essere la fase ORM e non la nuova
095530150305     c                   If        *In10 and
095540150305     c                             %lookup(%editc(ORMpor:'X'):skpog) = 0
095550050128     c                   Eval      i06far = OrmFao
095560050128     c                   EndIf
095570050322      * se sto confermando un ORM la fase deve essere la fase ORM e non la nuova
095580050324      * se non è chiusura ORM
095590050324     c                   If        §rmfpr = 'O' and Not *In10
095600050322     c                   Eval      i06far = OrmFao
095610050322     c                   EndIf
095620001026     C                   endsl
095630001026
095640001110     C                   movel(p)  kpjbu         §kpjbu
095650001110     C                   movel(p)  parm01        kpjbu
095660001025     C                   CALL      'FIOR06R'
095670001025     C                   PARM                    kpjba
095680001027     C                   parm                    fior06ds
095690001110     C                   movel     §kpjbu        kpjbu
095700131212      /free
095710131212       IF not *in01;
095720131212         exsr sr_orn;
095730131212       ENDIF;
095740131212       //?Imposto la riga dei tasti funzione
095750131212       ExSr Sr_Tastifun;
095760131212      /end-free
095770001025
095780001025     C                   endsr
095790001103      **********************************************************************
095800001103      * F19 - Proposte
095810001103      **********************************************************************
095820001103     C     Sr_Proposte   BEGSR
095830001103
095840001103     C                   movel     'P'           psce
095850001103     C                   movel     kpjbu         §kpjbu
095860001103     C                   clear                   kpjbu
095870001103     C                   movel     parm01        kpjbu
095880001103     C                   call      'FIOR07R'
095890001103     C                   parm                    kpjba
095900001103     C                   movel     §kpjbu        kpjbu
095910001103
095920001103     C                   endsr
095930001102      **********************************************************************
095940001102      * F20 - Simulazione delivery-tassazione
095950001102      **********************************************************************
095960001102     C     Sr_Simula     BEGSR
095970001116
095980001116     C                   clear                   TISI92DS
095990001116     C                   movel     'ORM'         D92op0
096000001116     C                   movel     v1cpor        D92lnp
096010001116     C                   movel     v1nac         D92nar
096020001116     C                   movel     v1cac         D92cad
096030030922     C                   movel(p)  sav_v1loc35   D92lod
096040001116     C                   movel     v1ffd         D92ffd
096050001116     C                   movel     v1ksc         D92ksc
096060001116     C                   movel     v1ctr         D92ctr
096070001116     C                   z-add     v1ncl         D92ncl
096080001116     C                   z-add     v1pkg         D92pkb
096090001116     C                   z-add     v1vlm         D92vlf
096100001116     C                   movel     v1ddt         D92ddt
096110001102
096120001102     C                   movel     kpjbu         §kpjbu
096130001116     C                   movel(p)  tisi92ds      kpjbu
096140001102     C                   call      'TISI92R'
096150001102     C                   parm                    kpjba
096160001102     C                   movel     §kpjbu        kpjbu
096170001102
096180001102     C                   endsr
096190061023
096200061023      *--------------------------------------------------------------------*
096210061023      * F2 - Dati DPD
096220061023      *--------------------------------------------------------------------*
096230061023     c     sr_datidpd    begsr
096240061023
096250080430     c                   clear                   fieu55ds
096260080430
096270080430      * se ORM commissionato da DPD passo il n. ORM DPD
096280080430     c     ormpoe        lookup    skpodpd                                30
096290080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
096300080430     c                   eval      i55dep = %subst(ormrfa:4:4)
096310080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
096320081015     c                   eval      i55ie = 'I'
096330080430     c                   eval      kpjbu = fieu55ds
096340080430     c                   call      'FIEU55C'
096350080430     c                   parm                    kpjba
096360080430     c                   endif
096370080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
096380080430     c     ormpor        lookup    skpodpd                                30
096390080430     c                   if        *in30
096400061023     c                   eval      i55poe = v1cpoe
096410061023     c                   eval      i55nrs = v1nsr
096420061023     c                   eval      i55nro = v1nor
096430061023     c                   eval      i55nrv = v1nrv
096440081015     c                   eval      i55ie = 'E'
096450061023     c                   eval      kpjbu = fieu55ds
096460061206     c                   call      'FIEU55C'
096470061023     c                   parm                    kpjba
096480080430     c                   endif
096490061023
096500061023     c                   endsr
096510061023
096520050422      *--------------------------------------------------------------------*
096530050420      * CONTROLLO PROPOSTE
096540050422      *--------------------------------------------------------------------*
096550050420     c     Sr_ContrProp  BegSr
096560050420
096570050420     c                   Eval      wproposte = *Off
096580050420     c                   Movel     v1cpoe        kpoe
096590050420     c                   Movel     v1nsr         knsr
096600050420     c                   Movel     v1nor         knor
096610050420     c                   Movel     v1nrv         knrv
096620050420
096630050420     c     kfnor         Setll     Fnorp01l
096640050420     c                   Do        *Hival
096650140227     c     kfnor         Reade(n)  Fnorp01l
096660050420     c                   If        %Eof(Fnorp01l)
096670050420     c                   Leave
096680050420     c                   EndIf
096690050420     c                   If        OrpFev <> *Blanks
096700050420     c                   Iter
096710050420     c                   EndIf
096720050420     c                   Eval      wproposte = *On
096730050420     c                   Leave
096740050420     c                   EndDo
096750050420
096760050420     c                   EndSr
096770040419      **********************************************************************
096780040419      * F21 - Dirottamento
096790040419      **********************************************************************
096800040419     c     Sr_Dirotta    BegSr
096810040419
096820040419     c                   Clear                   wdcpor
096830040419     c                   Clear                   wddpor
096840040419     c                   Move      v1cpor        sav_wdcpor
096850040420     c                   Eval      wdcdar = v1dar
096860040419     c                   Do        *Hival
096870040419     c                   Exfmt     Fior05dw
096880040420     c                   Eval      *In28 = *Off
096890040419      * F12=Ritorno
096900040419     c   kl              Leave
096910040419     c                   ExSr      Sr_ContrWd
096920040420     c   28              Iter
096930040420      * F6=Conferma
096940040420     c                   If        *InKf
096950040615     c                   movel     wdcpor        v1cpor
096960040615     c                   movel     wdcdar        v1dar
096970040615     c                   Eval      v1fao = 050
096980050422     c                   Eval      v1dfo = oggi_aammgg
096990040615     c                   Time                    v1ofo
097000170411      * devo pulire anche i dati relativi al giro
097010170411     c                   clear                   V1Ccgi
097020170411     c                   clear                   V1tgi
097030170411     c                   clear                   sav_V1Ccgi
097040040615     c                   Eval      wokdirotta = *On
097050040420     c                   Leave
097060040420     c                   EndIf
097070040419     c                   EndDo
097080040419
097090040419     c                   EndSr
097100090205
097110090205      *--------------------------------------------------------------------*
097120090205      * Window per immissione dati appuntamento ORM
097130090205      *--------------------------------------------------------------------*
097140090205     c     sr_ormapp     begsr
097150090205
097160090206     c                   eval      *in90 = *off
097170090206     c                   eval      *in93 = *off
097180090206     c                   eval      *in94 = *off
097190090205     c                   do        *hival
097200090206     c                   exfmt     fior05app
097210090206     c                   eval      *in28 = *off
097220090206     c                   eval      *in93 = *off
097230090206     c                   eval      *in94 = *off
097240090206     c                   clear                   wappmsg
097250090206      * f12-ritorno
097260090206     c                   if        *inkl
097270090206     c                   eval      *in90 = *on
097280090206     c                   leave
097290090206     c                   endif
097300090206      * controllo
097310090206     c                   if        wappref = *blanks
097320090206     c                   eval      wappmsg = 'Inserire la persona da -
097330090206     c                             contattare'
097340090206     c                   eval      *in28 = *on
097350090206     c                   eval      *in93 = *on
097360090206     c                   iter
097370090206     c                   endif
097380090206      * almeno 5 caratteri
097390090206     c                   if        %subst(wappref:4:31) = *blanks
097400090206     c                   eval      wappmsg = 'Completare la persona -
097410090206     c                             da contattare'
097420090206     c                   eval      *in28 = *on
097430090206     c                   eval      *in93 = *on
097440090206     c                   iter
097450090206     c                   endif
097460090206      * controllo
097470090206     c                   if        wappnota1 = *blanks
097480090206     c                   eval      wappmsg = 'Inserire le annotazioni'
097490090206     c                   eval      *in28 = *on
097500090206     c                   eval      *in94 = *on
097510090206     c                   iter
097520090206     c                   endif
097530090206      * almeno 5 caratteri
097540090206     c                   if        %subst(wappnota1:4:31) = *blanks
097550090206     c                   eval      wappmsg = 'Completare le -
097560090206     c                             annotazioni'
097570090206     c                   eval      *in28 = *on
097580090206     c                   eval      *in94 = *on
097590090206     c                   iter
097600090206     c                   endif
097610090206      * f6-conferma
097620090206     c   kf              leave
097630090205     c                   enddo
097640090205
097650090205     c                   endsr
097660090205
097670040419      **********************************************************************
097680040419      * Controllo i dati per il dirottamento
097690040419      **********************************************************************
097700040419     c     Sr_ContrWd    BegSr
097710040419
097720040419      * --> P.O. ritiro nuovo
097730040419     c                   Clear                   wddpor
097740040419      * Obbligatorio
097750040419     c                   If        wdcpor = *Zeros
097760040419     c                   Eval      wdcmsg = msg(11)
097770070124     c                   Seton                                        28
097780040419     c                   Goto      endctrwd
097790040419     c                   EndIf
097800040419      * Non può essere lo stesso di prima
097810040419     c                   If        wdcpor = sav_wdcpor
097820040419     c                   Eval      wdcmsg = msg(78)
097830070124     c                   Seton                                        28
097840040419     c                   Goto      endctrwd
097850040419     c                   EndIf
097860040419      * Deve esistere
097870040419     c     wdcpor        Chain     Azorg01L
097880040419     c                   If        Not %Found(Azorg01l)
097890040419     c                   Eval      wdcmsg = msg(33)
097900070124     c                   Seton                                        28
097910040419     c                   Goto      endctrwd
097920040419     c                   EndIf
097930040419      * Non deve essere annullato
097940040419     c                   If        OrgFva <> *Blanks  or
097950040419     c                             (OrgFag <> 'A' and OrgFag <> 'F')
097960040419     c                   Eval      wdcmsg = msg(33)
097970070124     c                   Seton                                        28
097980040419     c                   Goto      endctrwd
097990040419     c                   EndIf
098000040419      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
098010040419     c                   Eval      og148 = OrgDe8
098020040419     c                   If        §ogorm <> 'S'
098030040628     c                   Eval      wdcmsg = msg(79)
098040070124     c                   Seton                                        28
098050040419     c                   Goto      endctrwd
098060040419     c                   EndIf
098070040419      * Decodifico
098080040419     c                   Eval      wddpor = OrgDes
098090070124
098100070124      * se il p.o. che dirotta è abilitato al PDA
098110070124      * e fase 400 o 410
098120071210     c                   if        *in22 and
098130070124     c                             (ormfao = 400 or ormfao = 410)
098140070124      * devo avvisare che ho già avuto un esito da AUT
098150070124      * se l'orm è in fase 410
098160070124     c                   if        ormfao = 410 and wforzadir = *off
098170070124     c                   eval      wdcmsg = msg(41)
098180070124     c                   seton                                        28
098190070124     c                   eval      wforzadir = *on
098200070124     c                   goto      endctrwd
098210070124     c                   endif
098220070124      * in ogni caso devo salvarmi la distinta per passarla al pgm che aggiorna
098230070124      * fipdo
098240070124     c                   eval      savorffgs = orffgs
098250070124     c                   eval      savorfndc = orfndc
098260070125     c                   eval      savorfddc = orfddc
098270070125      * mi salvo anche la fase che mi servirà poi nel caso di ORM dirottato
098280070125     c                   eval      savorffar = orffar
098290070124     c                   endif
098300040419
098310040615      * è sempre un orm commissionato controllo se ok tutti i dati
098320130920      * ma solo se era già un ORM COMMISSIONATO
098330130920     c                   IF        §ORcom = 'S'
098340040615     c                   Eval      V1com = 'S'
098350130918     c                   ENDIF
098360040419
098370040419     c     endctrwd      EndSr
098380001011      **********************************************************************
098390001023      * PRENDE IL NUOVO NUMERO ORDINE
098400001011      **********************************************************************
098410001011     C     Sr_Numor      BEGSR
098420001011
098430010222     C                   if        v1cpoe = *zeros
098440150305     C                   z-add     DUTpou        v1cpoe
098450010222     C                   endif
098460030922
098470030922     c                   clear                   TRUL33DS
098480030922     c                   Eval      I33cnu = 300
098490030922     c                   Eval      I33po1 = V1cpoe
098500030922     c                   Eval      I33num = 1
098510030922     c                   movel(p)  TRUL33DS      KPJBU
098520030922     c                   call      'TRUL33R'
098530030922     c                   parm                    KPJBA
098540030922     c                   movel     KPJBU         TRUL33DS
098550030922
098560030922    1c                   Select
098570030922      * Numeratore non trovato
098580030922     C                   when      O33err =  01
098590030922     C                   eval      *in28 = *on
098600030922     C                   movel     msg(72)       v1cmsg
098610030922     C                   goto      endsrnumor
098620030922      * Numeratore allocato
098630030922     C                   when      O33err =  02
098640030922     C                   eval      *in28 = *on
098650030922     C                   movel     msg(71)       v1cmsg
098660030922     C                   goto      endsrnumor
098670030922      * altro errore
098680030922     C                   when      O33err <> *zeros
098690030922     C                   eval      *in28 = *on
098700030922     C                   movel     O33msg        v1cmsg
098710030922     C                   goto      endsrnumor
098720030922    1C                   endsl
098730030922
098740030922     C                   z-add     O33nrf        v1nor
098750001011
098760010507     C     endsrnumor    endsr
098770070119
098780070119      *--------------------------------------------------------------------*
098790070119      * Carico dati sensibili per PDA modificati
098800070119      *--------------------------------------------------------------------*
098810070119     c     sr_datipdamod begsr
098820070119
098830070119     c                   clear                   datipdamod
098840070119     c                   eval      pdamodrsr = v1rsr
098850070119     c                   eval      pdamodinr = v1inr
098860070119     c                   eval      pdamodlor = v1lor
098870070119     c                   eval      pdamodprr = v1prr
098880070119     c                   eval      pdamodrer = v1rer
098890070119     c                   eval      pdamodter = v1ter
098900070119     c                   eval      pdamodorr = v1orr
098910070119     c                   eval      pdamodncl = v1ncl
098920070119     c                   eval      pdamodpkg = v1pkg
098930070119     c                   eval      pdamodvlm = v1vlm
098940070119     c                   eval      pdamodbnc = v1bnc
098950070119     c                   eval      pdamodno1 = v1not1
098960070119     c                   eval      pdamodno2 = v1not2
098970131107      /free
098980140123        pdamodora1 = V1OraAMda;
098990140123        pdamodora2 = V1OraAMa;
099000140123        pdamodora3 = V1OraPMda;
099010140123        pdamodora4 = V1OraPMa;
099020131107      /end-free
099030070119
099040070119     c                   endsr
099050070119
099060001011      **********************************************************************
099070001026      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
099080001011      **********************************************************************
099090001011     C     Sr_Carfile    BEGSR
099100001011
099110010116     C                   eval      ORMatb = v1atb
099120010116     C                   eval      ORMpoe = v1cpoe
099130010116     C                   eval      ORMnsr = v1nsr
099140010116     C                   eval      ORMnor = v1nor
099150010116     C                   eval      ORMnrv = v1nrv
099160010116     C                   eval      ORMtor = v1ctor
099170010116     C                   eval      ORMtco = v1ctco
099180010116      * Inversione data emissione ORM
099190010116     C                   clear                   wlbdat
099200010116     C                   z-add     v1dao         G02dat
099210010116     C                   movel     *blanks       G02err
099220010116     C                   call      'XSRDA8'
099230010116     C                   parm                    wlbdat
099240010116     C                   z-add     G02inv        ORMdao
099250010116
099260131211     C                   eval      ORMoao = v1oao
099270010116
099280010116     C                   eval      ds_v1cor1 = v1cor1
099290010116     C                   eval      ds_v1cor2 = v1cor2
099300010116     C                   eval      ds_v1cor3 = v1cor3
099310010116     C                   eval      ORMcor = ds_v1ccor
099320010116     C                   eval      ORMrso = v1rso
099330010116     C                   eval      ORMino = v1ino
099340010116     C                   eval      ORMcao = v1cao
099350030922     C                   movel(p)  sav_v1loo35   ORMloo
099360010116     C                   eval      ORMpro = v1pro
099370010116     C                   eval      ORMnao = v1nao
099380010116
099390010116     C                   eval      ds_v1cra1 = v1cra1
099400010116     C                   eval      ds_v1cra2 = v1cra2
099410010116     C                   eval      ds_v1cra3 = v1cra3
099420010116     C                   eval      ORMcra = ds_v1ccra
099430010116     C                   eval      ORMrsr = v1rsr
099440010116     C                   eval      ORMinr = v1inr
099450010116     C                   eval      ORMcar = v1car
099460001103     C                   movel(p)  v1lor         ORMlor
099470010116     C                   eval      ORMprr = v1prr
099480010116     C                   eval      ORMnar = v1nar
099490010116     C                   eval      ORMrer = v1rer
099500010116     C                   eval      ORMter = v1ter
099510010116
099520010116     C                   eval      ds_v1crc1 = v1crc1
099530010116     C                   eval      ds_v1crc2 = v1crc2
099540010116     C                   eval      ds_v1crc3 = v1crc3
099550010116     C                   eval      ORMcrc = ds_v1ccrc
099560010116     C                   eval      ORMrsc = v1rsc
099570010116     C                   eval      ORMinc = v1inc
099580030922     C                   movel(p)  sav_v1loc35   ORMloc
099590010116     C                   eval      ORMcac = v1cac
099600010116     C                   eval      ORMprc = v1prc
099610010116     C                   eval      ORmnac = v1nac
099620001023      * Inversione data ritiro
099630001017     C                   clear                   wlbdat
099640001017     C                   z-add     v1dar         G02dat
099650001017     C                   movel     *blanks       G02err
099660001017     C                   call      'XSRDA8'
099670001017     C                   parm                    wlbdat
099680001025     C                   z-add     G02inv        ORMdar
099690010118     C                   eval      ORMorr = v1orr
099700010116
099710010116     C                   eval      ORMffd = v1ffd
099720010116     C                   eval      ORMnam = v1nam
099730010116     C                   eval      ORMncl = v1ncl
099740010116     C                   eval      ORMpkg = v1pkg
099750010116     C                   eval      ORMvlm = v1vlm
099760010116     C                   eval      ORMbnc = v1bnc
099770010116     C                   eval      ORMblc = v1blc
099780010116     C                   eval      ORMatt = v1att
099790010116     C                   eval      ORMmtc = v1mtc
099800010116     C                   eval      ORMddt = v1ddt
099810140331      * se codice ordinante = codice mittente e impostato che paga l'Ordinante
099820140331      * in automatico metto il 'Mittente' in questo modo elimino un msg di errore
099830140331     c                   IF        V1pag = 'O' and
099840140331     c                             ds_V1cCor = ds_V1cCra and ds_V1cCra > *Zeros
099850140331     c                   eval      v1pag = 'M'
099860140331     c                   ENDIF
099870140331
099880010116     C                   eval      ORMpag = v1pag
099890010116     C                   eval      ORMksc = v1ksc
099900010202     C                   eval      ORMctr = v1ctr
099910001017     C                   movel     v1cpor        ORMpor
099920001016     C                   movel     v1cpoc        ORMpoc
099930010116     C                   move      v1sto         ORMsto
099940010116
099950010116     C                   eval      ORMpos = v1cpos
099960010116     C                   eval      ORMors = v1ors
099970010116     C                   eval      ORMdst = v1dst
099980010116     C                   eval      ORMno1 = v1not1
099990010116     C                   eval      ORMno2 = v1not2
100000010116     C                   eval      ORMrfa = v1rfa
100010010712
100020010116     C                   eval      ORMcst = v1cst
100030010116     C                   eval      ORMvcs = v1vcs
100040010116     C                   eval      ORMfcs = v1fcs
100050050316      * se ORM commissionato e sono in immissione l'ORM deve essere creato in fase
100060050316      * 50 mentre se non è commissionato subito in fase 100
100070081211      * se ORM export DPD sempre in fase 50
100080050316     c                   If        *In01 or *In03
100090081127     c                   If        v1com = 'S' or *in15
100100050316     c                   Eval      v1fao = 50
100110050316     c                   Else
100120050316     c                   Eval      v1fao = 100
100130050316     c                   EndIf
100140151118      /free
100150151118       //?Se ORM da internet ed è un prepagato locale
100160151118       //?lo immetto subito in fase 100 anche se è commissionato
100170151118       //?la telefonata al mittente viene fatta al momento della conferma ORM
100180151118         IF  §RMfpr = 'C' and ORMtco = 'I' and ORMtor = 'P' and
100190151118             ORMpoe = ORMpor;
100200151118           V1fao = 100;
100210151118         ENDIF;
100220140428       //?Alert da inviare
100230140422         IF  walert;
100240140428         //?Se ora ritiro a 0 imposto inizio servizio o l'ora "tappo"
100250140422           IF  ORMorr = 0 and OOR2stis > 0;
100260140422             ORMorr = OOR2stis;
100270140422           ENDIF;
100280140422           IF  ORMorr = 0;
100290140422             ORMorr = d§DFTorrtp;
100300140422           ENDIF;
100310140422         //?Se ora telefono non impostato metto il cell.x sms
100320140422           IF  ORMter = *blanks;
100330140422             ORMter = W03sms;
100340140422           ENDIF;
100350140422         ENDIF;
100360140422      /end-free
100370070912      * devo aggiornare il flag di ORM assegnabile in base alla fase
100380070912     c                   exsr      sr_contrfao
100390050316     c                   EndIf
100400140429
100410140109     C                   eval      ORMdfo = v1dfo
100420140109     C                   eval      ORMofo = v1ofo
100430010116     C                   eval      ORMfao = v1fao
100440010116     C                   eval      ORMnpg = v1npg
100450010116     C                   eval      ORMndc = v1ndc
100460010116     C                   eval      ORMddc = v1ddc
100470010622     C                   eval      ORMstp = v1stp
100480010622     C   10              clear                   ORMnpg
100490010622     C   10              clear                   ORMndc
100500010622     C   10              clear                   ORMddc
100510010622     C   10              clear                   ORMtap
100520010116     C                   eval      ORMtap = v1tap
100530010116     C                   eval      ORMeti = v1eti
100540010119     C                   eval      ORMspi = v1spi
100550011114      * Se forzato blocco destinatario devo pulire il relativo flag
100560011114     C                   if        §ormfd = 'S'
100570011119     C                   if        v1ctor <> 'S'
100580011114     C                   clear                   §ormfd
100590011119     C                   else
100600070219     C                   if        v1rsc = *blanks and ds_v1ccrc = *zeros
100610011119     C                   clear                   §ormfd
100620011114     C                   endif
100630011119     C                   endif
100640011114     C                   endif
100650030606      * ORM commissionato
100660131106      /free
100670131106       //?Se il campo è vuoto imposto 'N' in questo modo in immissione fino a che
100680131106       //?non viene forzato dall'utente il pgm calcola se ORM commissionato o meno
100690131106       //?mentre nel file ho solo 'N' o 'S' (da ora in poi)
100700140424       //?gli unici ORM con il campo a blank sono gli ORM con serie (RC) scritti
100710140424       //?automaticamente dal traduttore
100720140424       //?e gli ORM commissionati da DPD chiusi in automatico da DPD
100730131106         IF  V1com = *blanks;
100740131106           V1com = 'N';
100750131106         ENDIF;
100760131106      /end-free
100770030606     c                   Eval      §orcom = v1com
100780030731
100790030731      * in base al tipo orm e a chi paga imposto il campo §orpfb
100800030731      * imposto 'F' se paga mittente per orm singolo o prepagato
100810030731      * imposto 'A' se NON paga mittente
100820030731     c                   Clear                   §orPfb
100830030731     c                   Select
100840030731     c                   When      OrmPag = 'M'
100850030731     c                   If        OrmTor = 'P' or OrmTor = 'S'
100860030731     c                   Eval      §orPfb = 'F'
100870030731     c                   EndIf
100880030731     c                   Other
100890030731     c                   Eval      §orPfb = 'A'
100900030731     c                   EndSl
100910090403
100920090403      * in immissione ORM
100930090403      * se orm da internet o manuale verifico se impostare il §ormks
100940100913      * solo però se il campo non è già stato impostato da tabella PVO
100950090403     c                   if         *in01 and
100960141211     c                             (ormtco = 'I' or ormtco = 'M' or
100970141212     c                              ORMtco = 'E')
100980100913     c                             and (not ok_pvo or
100990100913     c                                 (ok_pvo and d§pvoks = ' '))
101000090403     c                   exsr      sr_vac
101010090403     c                   endif
101020090403
101030090403      * imposto flag operativi
101040030731     C                   eval      ORMflo = dorm01
101050161115      /free
101060161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
101070161115         //?ipotetico ritiro da effettuare al mattino
101080161115         IF  Orariestesi;
101090161115           ORMrmp = 'S';
101100161115         ELSE;
101110161115           clear ORMrmp;
101120161115         ENDIF;
101130161116         //?Se ORM da Internet non lo memorizzo
101140161116         IF  ORMtco = 'I';
101150161116           clear ORMrmp;
101160161116         ENDIF;
101170161116
101180161115      /end-free
101190010228
101200040908     c                   Eval      OrmDtt = oggi_aammgg
101210001204      * Mette sempre in positivo i campi delle quantità
101220001204     C                   mllzo     1             ORMorr
101230001204     C                   mllzo     1             ORMncl
101240001204     C                   mllzo     1             ORMpkg
101250001204     C                   mllzo     1             ORMvlm
101260001204     C                   mllzo     1             ORMbnc
101270001204     C                   mllzo     1             ORMblc
101280001204     C                   mllzo     1             ORMatt
101290001204     C                   mllzo     1             ORMmtc
101300001109      * Dati di copia
101310001109     C                   if        *in03 = *on
101320001109     C                   z-add     knrv          ORMnrv
101330050316      * se ORM commissionato l'ORM deve essere creato in fase
101340050316      * 50 mentre se non è commissionato subito in fase 100
101350081211      * se ORM export DPD sempre in fase 50
101360081211     c                   If        v1com = 'S' or *in15
101370050316     c                   Eval      ORMfao = 50
101380050316     c                   Else
101390050316     c                   Eval      ORMfao = 100
101400050316     c                   EndIf
101410010118     C                   eval      ORMdfo = oggi_aammgg
101420010118     C                   movel     w0140         ORMofo
101430001130     C                   clear                   ORMnpg
101440001129     C                   z-add     *zeros        ORMndc
101450001129     C                   z-add     *zeros        ORMddc
101460001129     C                   movel     *blanks       ORMtap
101470010223     C                   z-add     *zeros        ORMdst
101480010209      * Aggiorno anche i dati che sono a video per poter scrivere la fase
101490010209     C                   movel     ORMnrv        v1nrv
101500010209     C                   movel     ORMfao        v1fao
101510010209     C                   movel     ORMdfo        v1dfo
101520010209     C                   movel     ORMofo        v1ofo
101530070706      * devo aggiornare il flag di ORM assegnabile in base alla fase
101540070706     c                   exsr      sr_contrfao
101550070706     c                   eval      ormeti = v1eti
101560001109     C                   endif
101570131126
101580131126      /free
101590131126       //?Immissione/Copia ORM
101600131126         IF  *in01 or *in03;
101610131126       //?imposto ora cut-off nel campo Stop
101620140318           ORMstp = OOR2lrnc;
101630160526       //?in teoria se ritiro estero dovrei mettere il nuovo limite per orm esteri
101640160526       //?ma dato che il campo ORMSTP non è usato non lo faccio
101650131126         ENDIF;
101660131126      /end-free
101670001017
101680001017     C                   endsr
101690090403
101700090403      *--------------------------------------------------------------------*
101710090403      * Se conferma orm da VAO controllo se impostare il ritorno VAC
101720090403      *--------------------------------------------------------------------*
101730090403     c     sr_vac        begsr
101740090403
101750090403      * se paga Ordinante
101760090403      * se ho il KSC
101770090403     c                   if        v1pag = 'O' and
101780090403     c                             v1ksc <> *zeros
101790090403      * in tabella 3K trovo il cliente e non devo escludere
101800090403     c                   clear                   ds3k
101810090403     c                   eval      kcod = '3K'
101820090403     c                   movel(p)  v1ksc         kkey
101830090403     c     ktab          chain     tabel00f
101840090403     c                   if        %found(tabel00f) and tblflg = *blanks
101850090403     c                   eval      ds3k = tbluni
101860090403      * ritorno VAC x triangolazione ORM
101870090403     c                   if        §3kvto = 'S'
101880090403      * imposto il falg per ritorno VAC
101890090403     c                   eval      §ormks = 'S'
101900100812     c                   leavesr
101910090403     c                   endif
101920090403     c                   endif
101930100812
101940100812      * se non ho trovato con 3K provo con 3Q
101950100812     c                   clear                   ds3q
101960100812     c                   eval      kcod = '3Q'
101970100812     c                   movel(p)  v1ksc         kkey
101980100812     c     ktab          chain     tabel00f
101990100812     c                   if        %found(tabel00f) and tblflg = *blanks
102000100812     c                   eval      ds3Q = tbluni
102010100812      * ritorno VAB x triangolazione ORM
102020100812     c                   if        §3Qvto = 'S'
102030100812      * imposto il flag per ritorno VAB
102040100812     c                   eval      §ormks = 'S'
102050100812     c                   endif
102060100812     c                   endif
102070100812
102080090403     c                   endif
102090090403
102100090403     c                   endsr
102110140122
102120140122      /free
102130140122       //--------------------------------------------------------------------
102140140122       //?Controllo i dati variati.
102150140122       //--------------------------------------------------------------------
102160140122       BEGSR ctrvardati;
102170140122
102180140122         clear wVardati;
102190140122
102200140122       //?Variazione dati mittente ==> M
102210140122         IF  pdasavrsr <> pdamodrsr or
102220140122             pdasavinr <> pdamodinr or
102230140122             pdasavlor <> pdamodlor or
102240140122             pdasavprr <> pdamodprr or
102250140122             pdasavrer <> pdamodrer or
102260140122             pdasavter <> pdamodter;
102270140122           wVardati = %trim(wVardati) + 'M';
102280140122         ENDIF;
102290140122       //?Variazione dati ora pronta merce ==> O
102300140122         IF  pdasavorr <> pdamodorr or
102310140122             pdasavora1 <> pdamodora1 or
102320140122             pdasavora2 <> pdamodora2 or
102330140122             pdasavora3 <> pdamodora3 or
102340140122             pdasavora4 <> pdamodora4;
102350140122           wVardati = %trim(wVardati) + 'O';
102360140122         ENDIF;
102370140122       //?Variazione dati quantità ==> Q
102380140122         IF  pdasavncl <> pdamodncl or
102390140122             pdasavpkg <> pdamodpkg or
102400140122             pdasavvlm <> pdamodvlm or
102410140122             pdasavbnc <> pdamodbnc;
102420140122           wVardati = %trim(wVardati) + 'Q';
102430140122         ENDIF;
102440140124       //?Variazione dati note ==> N
102450140122         IF  pdasavno1 <> pdamodno1 or
102460140122             pdasavno2 <> pdamodno2;
102470140124           wVardati = %trim(wVardati) + 'N';
102480140122         ENDIF;
102490140122
102500140122       ENDSR;
102510140122
102520140122      /end-free
102530070119
102540070119      *--------------------------------------------------------------------*
102550070119      * Aggiorno file PDO
102560070119      *--------------------------------------------------------------------*
102570070119     c     sr_aggpdo     begsr
102580070301
102590140122      * Richiamo sempre il FIOR56R, ci pensa poi lui a decidere se scrivere i dati
102600140122      * per il PDA e/o la telefonata AUT
102610070123
102620120612      * devo aggiornare FIPDO
102630070228     c                   eval      or56poe = ormpoe
102640070228     c                   eval      or56nsr = ormnsr
102650070228     c                   eval      or56nor = ormnor
102660070228     c                   eval      or56nrv = ormnrv
102670070228     c                   call      'FIOR56R'
102680140122     c                   parm                    kpjba
102690070228     c                   parm                    fior56ds
102700070228      * il pgm torna degli errori, ma dato che ho già fatto qua tutti i controlli
102710070228      * del caso, non me ne preoccupo
102720070119
102730070119     c                   endsr
102740070119
102750001017      **********************************************************************
102760001026      * CARICA IL FILE FNORF DAL FILE FNORM
102770001017      **********************************************************************
102780001017     C     Sr_Carfilef   BEGSR
102790001011
102800081125     c                   clear                   dorf01
102810001019     C                   clear                   fnorf000
102820010116     C                   eval      ORFpoe = v1cpoe
102830010116     C                   eval      ORFnsr = v1nsr
102840010116     C                   eval      ORFnor = v1nor
102850050322     C                   eval      ORFnrv = v1nrv
102860050322      * sono in conferma orm commissioni e sto dirottando l'orm imposto il p.o. ritiro
102870150305      * come po fase
102880050322     c                   If        §rmfpr = 'O' and wokdirotta = *On
102890050322     c                   movel     v1cpor        orfpog
102900050322     c                   Else
102910150305     C                   eval      ORFpog = DUTpou
102920050322     c                   EndIf
102930010116     C                   eval      ORFdae = v1dfo
102940010116     C                   eval      ORFore = v1ofo
102950010116     C                   eval      ORFfar = v1fao
102960001019     C                   eval      ORFpue = knmus
102970010202     C                   eval      ORFcar = v3cau
102980080523      * se sono in conferma proposta di chiusura ORM e l'orm non ha la distinta non la riporto dalla
102990080523      * conferma chiusura perchè ormai l'ORM non è più assegnato
103000080523      * caso di orm assegnato, proposta di chiusura, disassegnato, accetto porposta di chiusura
103010080523     c                   select
103020080523     c                   when      §rmfpr = 'P' and *in10 and ormndc = 0
103030080523     c                   clear                   orffgs
103040080523     c                   clear                   orfndc
103050080523     c                   clear                   orfddc
103060080523      * se c'è la distinta la riporto sulla fase
103070080523     c                   when      OrmNdc > *Zeros
103080040909     c                   Eval      OrfFgs = OrmPor
103090040909     c                   Eval      OrfNdc = OrmNdc
103100040909     c                   Eval      OrfDdc = OrmDdc
103110080523      * se non c'è la distinta riporto sulla fase quella salvata in precedenza
103120080523     c                   when      savndc > *Zeros
103130060516     c                   eval      orffgs = savfgs
103140041014     c                   Eval      OrfNdc = savndc
103150041014     c                   Eval      OrfDdc = savddc
103160080523     c                   endsl
103170081125      * imposto se non ho fatto l'ulteriore addebito
103180081125     c                   if        wnobolla = *on
103190081125     c                   eval      §orfadd = 'S'
103200081125     c                   endif
103210081125     c                   eval      orfflo = dorf01
103220001109
103230001019     C                   write     fnorf000
103240001017
103250001011     C                   endsr
103260001026      **********************************************************************
103270001026      * CARICA IL FILE DELLE NOTE FNORN RICHIAMANDO IL PROGRAMMA
103280001026      **********************************************************************
103290001026     C     Sr_Carnote    BEGSR
103300001026
103310001026     C                   if        *in01 = *on or *in02 = *on
103320001109     C                             or *in03 = *on
103330001026     C                   clear                   fior06ds
103340001026     C                   movel     'C'           i06flm
103350010118     C                   eval      i06dta = ORMdao
103360010118     C                   eval      i06poe = ORMpoe
103370010118     C                   eval      i06nor = ORMnor
103380010118     C                   eval      i06nsr = ORMnsr
103390010118     C                   eval      i06nrv = ORMnrv
103400010118     C                   eval      i06far = ORMfao
103410010301     C                   eval      i06dai = oggi_aammgg
103420010301     C                   movel     w0140         I06ori
103430001026
103440010405     C                   movel(p)  kpjbu         §kpjbu
103450010405     C                   movel(p)  parm01        kpjbu
103460001026     C                   call      'FIOR06R'
103470001026     C                   parm                    kpjba
103480001027     C                   parm                    fior06ds
103490010405     C                   movel     §kpjbu        kpjbu
103500001026
103510001026     C                   endif
103520001026
103530001026     C                   endsr
103540070903
103550070706      *--------------------------------------------------------------------*
103560151016      * File FNORG00F - Scrivo
103570070706      *--------------------------------------------------------------------*
103580151016     c     sr_wrtorg     begsr
103590151016
103600151016     c                   clear                   fnorg000
103610151016     c                   eval      orgpoe = ormpoe
103620151016     c                   eval      orgnsr = ormnsr
103630151016     c                   eval      orgnor = ormnor
103640151016     c                   eval      orgnrv = ormnrv
103650151016     c                   eval      orgpor = ormpor
103660151016     c                   eval      orgpkg = wpkg
103670151016     c                   eval      orgvlm = wvlm
103680151016      * giro immesso
103690151016     c                   if        v1ccgi <> *blanks
103700151016     c                   eval      orgcgi = v1ccgi
103710151016     c                   eval      orgtgi = v1tgi
103720151016     c                   eval      orgpocgi = num_v1cpor
103730151016     c                   time                    w0140
103740151016     c                   movel     w0140         orghvtel
103750151016     c                   move      w0140         orgdtvtel
103760151016     c     *eur          move      orgdtvtel     dataeur
103770151016     c                   move      dataeur       dataiso
103780151016     c                   move      dataiso       orgdtvtel
103790151016     c                   endif
103800151016      * scrivo
103810151016     c                   write     fnorg000
103820151016
103830151016     c                   endsr
103840151016
103850151016      *--------------------------------------------------------------------*
103860151016      * File FNORG00F - Aggiorno
103870151016      *--------------------------------------------------------------------*
103880151016     c     sr_aggorg     begsr
103890070706
103900070706     c                   eval      orgpor = ormpor
103910070706     c                   eval      orgpkg = wpkg
103920070706     c                   eval      orgvlm = wvlm
103930071031     c                   select
103940071031      * non c'è il giro ma prima c'era
103950071031     c                   when      v1ccgi = *blanks and orgcgi <> *blanks
103960071031     c                   clear                   orgcgi
103970071031     c                   clear                   orgtgi
103980071031     c                   clear                   orgpocgi
103990071031     c                   time                    w0140
104000071031     c                   movel     w0140         orghvtel
104010071031     c                   move      w0140         orgdtvtel
104020071031     c     *eur          move      orgdtvtel     dataeur
104030071031     c                   move      dataeur       dataiso
104040071031     c                   move      dataiso       orgdtvtel
104050071031      * non c'è il giro
104060071031     c                   when      v1ccgi = *blanks
104070071031     c                   clear                   orgcgi
104080071031     c                   clear                   orgtgi
104090071031     c                   clear                   orgpocgi
104100071031     c                   clear                   orghvtel
104110071031     c                   clear                   orgdtvtel
104120071031      * giro variato
104130080305      * o stesso giro ma dirottamento (caso di stesso giro codificato su due filiali diverse)
104140071031     c                   when      orgcgi <> v1ccgi or orgtgi <> v1tgi
104150080305     c                             or (orgcgi = v1ccgi and wokdirotta = *on)
104160070919     c                   eval      orgcgi = v1ccgi
104170070919     c                   eval      orgtgi = v1tgi
104180071023     c                   eval      orgpocgi = num_v1cpor
104190071003     c                   time                    w0140
104200071003     c                   movel     w0140         orghvtel
104210071003     c                   move      w0140         orgdtvtel
104220071003     c     *eur          move      orgdtvtel     dataeur
104230071003     c                   move      dataeur       dataiso
104240071003     c                   move      dataiso       orgdtvtel
104250071031     c                   endsl
104260080130      * se è un dirottamento devo pulire i dati relativi a NFTL e SLO
104270080130      * che potrebbero essere rimasti sporchi da un'assegnazione automatica
104280080130      * fatta in precedenza
104290080211      * pulisco anche quello che riguarda la distinta
104300080130     c                   if        wokdirotta = *on
104310080211     c                   clear                   orgpdc
104320080211     c                   clear                   orgfgs
104330080211     c                   clear                   orgndc
104340080211     c                   clear                   orgddc
104350080130     c                   clear                   orgnftl
104360080130     c                   clear                   orgslo
104370080211     c                   clear                   orgdtvdis
104380080211     c                   clear                   orghvdis
104390080130     c                   endif
104400071031      * aggiorno
104410070706     c                   update    fnorg000
104420070706
104430070706     c                   endsr
104440100218
104450100218      /free
104460100218       //--------------------------------------------------------------------
104470100218       //?Scrivo file FNORE00F.
104480100218       //--------------------------------------------------------------------
104490100218       BEGSR sr_Fnore;
104500140516
104510140516         wOKrcdg = *off;
104520100218
104530100225       //?Se previsto il preavviso scrivo l'estensione 'M ' per memorizzare
104540100225       //?l'indirizzo mail dove inviare il preavviso
104550100225       //?(non aggancio il FNVAOE rcd 'M ' in quanto l'indirizzo mail è già recuperato)
104560100225         IF  §ormpre = 'M';
104570100225           clear fnore000;
104580100225           OREpoe = ORMpoe;
104590100225           OREnsr = ORMnsr;
104600100225           OREnor = ORMnor;
104610100225           OREnrv = ORMnrv;
104620100225           OREtrc = 'M ';
104630100225           OREdati = VAOEdati;
104640100225           write fnore000;
104650100225         ENDIF;
104660100218
104670100318       //?Importo le estensioni da VAO a ORM
104680100218         k_vaoecor = sav_vaocor;
104690100218         k_vaoepoe = sav_vaopoe;
104700100218         k_vaoensr = sav_vaonsr;
104710100218         k_vaoenor = sav_vaonor;
104720100218         k_vaoenrv = sav_vaonrv;
104730100218       //?Aggancio il rcd
104740100318         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
104750100318         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104760100318         DOW not %eof(FNVAOE1l);
104770140422         //?ma solo dei rcd che non vengono variati da pgm
104780140422           IF  %lookup(VAOEtrc:skTRCvao) = 0;
104790100318             clear fnore000;
104800100318             OREpoe = ORMpoe;
104810100318             OREnsr = ORMnsr;
104820100318             OREnor = ORMnor;
104830100318             OREnrv = ORMnrv;
104840100318             OREtrc = VAOEtrc;
104850100318             OREdati = VAOEdati;
104860100318             write fnore000;
104870100318           ENDIF;
104880100318           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104890100318         ENDDO;
104900140422
104910140422       //?Visto che questa routine viene richiamata da conferma VAO
104920140422       //?scrivo subito il rcd 'G', con i dati da VAO tranne i 2 flag ORM per alert
104930140422       //?che sono da impostare in base ai valori calcolati nel pgm
104940140422         k_vaoecor = sav_vaocor;
104950140422         k_vaoepoe = sav_vaopoe;
104960140422         k_vaoensr = sav_vaonsr;
104970140422         k_vaoenor = sav_vaonor;
104980140422         k_vaoenrv = sav_vaonrv;
104990140516         k_vaoetrc = 'G ';
105000140422       //?Scrivo il rcd solo se trovo su VAO
105010140422         chain  %kds(k06fnvaoe) FNVAOE1L;
105020140422         IF  %found(FNVAOE1L);
105030140422           clear fnore000;
105040140422           OREpoe = ORMpoe;
105050140422           OREnsr = ORMnsr;
105060140422           OREnor = ORMnor;
105070140422           OREnrv = ORMnrv;
105080140422           OREtrc = VAOEtrc;
105090140422           OREdati = dOREgen;
105100140422           write fnore000;
105110140516           wOKrcdg = *on;
105120140422         ENDIF;
105130100218
105140100218       ENDSR;
105150131018
105160131018       //--------------------------------------------------------------------
105170131018       //?Scrivo/Aggiorno file FNORE00F.
105180131018       //--------------------------------------------------------------------
105190131018       BEGSR aggFNORE;
105200131018
105210131018         k_OREpoe = ORMpoe;
105220131018         k_OREnsr = ORMnsr;
105230131018         k_OREnor = ORMnor;
105240131018         k_OREnrv = ORMnrv;
105250131018
105260131018       //?Rcd 'O ' - Orari apertura
105270131018        IF  v1OraAMda > 0 or v1OraAMa > 0 or
105280131018            v1OraPMda > 0 or v1OraPMa > 0;
105290131018          k_OREtrc = 'O ';
105300131018          §OREoramda = v1OraAMda;
105310131018          §OREorama  = v1OraAMa;
105320131018          §OREorapda = v1OraPMda;
105330131018          §OREorapa  = v1OraPMa;
105340131029          chain  %kds(k05fnore) FNORE01L;
105350131018          IF  %found(FNORE01L);
105360131125            OREdati = dOREorari;
105370131125            update fnore000;
105380131018          ELSE;
105390131018            clear fnore000;
105400131018            OREpoe = ORMpoe;
105410131018            OREnsr = ORMnsr;
105420131018            OREnor = ORMnor;
105430131018            OREnrv = ORMnrv;
105440131018            OREtrc = 'O ';
105450131018            OREdati = dOREorari;
105460131018            write fnore000;
105470131018          ENDIF;
105480131018        ENDIF;
105490131125       //?Rcd 'O ' - Orari apertura da cancellare
105500131125        IF  v1OraAMda = 0 and v1OraAMa = 0 and
105510131125            v1OraPMda = 0 and v1OraPMa = 0;
105520131125          k_OREtrc = 'O ';
105530131125          §OREoramda = v1OraAMda;
105540131125          §OREorama  = v1OraAMa;
105550131125          §OREorapda = v1OraPMda;
105560131125          §OREorapa  = v1OraPMa;
105570131125          chain  %kds(k05fnore) FNORE01L;
105580131125          IF  %found(FNORE01L);
105590131125            delete fnore000;
105600131125          ENDIF;
105610131125        ENDIF;
105620161020
105630161020       //?Rcd 'DT' - Dta Pronta Merce
105640170524       //?         --> Data Ritiro Calcolata e Anticipo
105650161020        k_OREtrc = 'DT';
105660161027        clear dOREdt;
105670161027        §OREdpm = inv_V1dpm;
105680170524       //?Per immissione o copia devo salvare i dati relativi
105690170524       //?alla data ritiro calcolata se potevo anticipare e se ho anticipato
105700170524       //?in variazione riporto quello che c'era già sul file
105710170524       //?anche se viene variata la data di ritiro
105720170524        §OREdar = %editc(DarCalcolata:'X');
105730170524        IF  wOkPosticipa;
105740170524          §OREposd = 'S';
105750170524        ENDIF;
105760170524        IF  wAnticipa;
105770170524          §OREant = 'S';
105780170524        ENDIF;
105790170524        IF  Anticipato;
105800170524          §OREmod = 'S';
105810170524        ENDIF;
105820170524        §OREgga = %editc(ggAnticipo:'X');
105830161020        chain  %kds(k05fnore) FNORE01L;
105840161020        IF  %found(FNORE01L);
105850161027          OREdati = dOREdt;
105860161020          update fnore000;
105870161020        ELSE;
105880161020          clear fnore000;
105890161020          OREpoe = ORMpoe;
105900161020          OREnsr = ORMnsr;
105910161020          OREnor = ORMnor;
105920161020          OREnrv = ORMnrv;
105930161020          OREtrc = 'DT';
105940161027          OREdati = dOREdt;
105950161020          write fnore000;
105960161020        ENDIF;
105970131018
105980131018       //?Rcd 'MA' - Mail x ALERT
105990131029        IF  old_w03mail <> w03mail or §RMfpr = 'P';
106000131018          k_OREtrc = 'MA';
106010131029          chain  %kds(k05fnore) FNORE01L;
106020131018          IF  %found(FNORE01L);
106030131029            IF  w03mail <> *blanks;
106040131029              OREdati = w03mail;
106050131029              update fnore000;
106060131029            ELSE;
106070131029              delete fnore000;
106080131029            ENDIF;
106090131018          ELSE;
106100131029            IF  w03mail <> *blanks;
106110131029              clear fnore000;
106120131029              OREpoe = ORMpoe;
106130131029              OREnsr = ORMnsr;
106140131029              OREnor = ORMnor;
106150131029              OREnrv = ORMnrv;
106160131029              OREtrc = 'MA';
106170131029              OREdati = w03mail;
106180131029              write fnore000;
106190131029            ENDIF;
106200131018          ENDIF;
106210131018        ENDIF;
106220131018
106230131018       //?Rcd 'S ' - SMS x ALERT
106240131029        IF  old_w03sms <> w03sms or §rmfpr = 'P';
106250131018          k_OREtrc = 'S ';
106260140422          §OREsms = w03sms;
106270131029          chain  %kds(k05fnore) FNORE01L;
106280131018          IF  %found(FNORE01L);
106290131029            IF  w03sms <> *blanks;
106300140422              OREdati = dOREsms;
106310131029              update fnore000;
106320131029            ELSE;
106330131029              delete fnore000;
106340131029            ENDIF;
106350131018          ELSE;
106360131029            IF  w03sms <> *blanks;
106370131029              clear fnore000;
106380131029              OREpoe = ORMpoe;
106390131029              OREnsr = ORMnsr;
106400131029              OREnor = ORMnor;
106410131029              OREnrv = ORMnrv;
106420131029              OREtrc = 'S ';
106430140422              OREdati = dOREsms;
106440131029              write fnore000;
106450131029            ENDIF;
106460131018          ENDIF;
106470131018        ENDIF;
106480160310
106490160406       //?Rcd 'MC' - Mail x Conferma Prenotazione Ritiro
106500160406        IF  old_w04mail <> w04mail;
106510160310          k_OREtrc = 'MC';
106520160310          chain  %kds(k05fnore) FNORE01L;
106530160310          IF  %found(FNORE01L);
106540160406            IF  w04mail <> *blanks;
106550160406              OREdati = w04mail;
106560160310              update fnore000;
106570160310            ELSE;
106580160310              delete fnore000;
106590160310            ENDIF;
106600160310          ELSE;
106610160406            IF  w04mail <> *blanks;
106620160310              clear fnore000;
106630160310              OREpoe = ORMpoe;
106640160310              OREnsr = ORMnsr;
106650160310              OREnor = ORMnor;
106660160310              OREnrv = ORMnrv;
106670160310              OREtrc = 'MC';
106680160406              OREdati = w04mail;
106690160310              write fnore000;
106700160310            ENDIF;
106710160310          ENDIF;
106720160310        ENDIF;
106730160310
106740160406       //?Rcd 'SC' - SMS x Conferma Prenotazione Ritiro
106750160406        IF  old_w04sms <> w04sms;
106760160310          k_OREtrc = 'SC';
106770160310          chain  %kds(k05fnore) FNORE01L;
106780160310          IF  %found(FNORE01L);
106790160406            IF  w04sms <> *blanks;
106800160406              OREdati = w04sms;
106810160310              update fnore000;
106820160310            ELSE;
106830160310              delete fnore000;
106840160310            ENDIF;
106850160310          ELSE;
106860160406            IF  w04sms <> *blanks;
106870160310              clear fnore000;
106880160310              OREpoe = ORMpoe;
106890160310              OREnsr = ORMnsr;
106900160310              OREnor = ORMnor;
106910160310              OREnrv = ORMnrv;
106920160310              OREtrc = 'SC';
106930160406              OREdati = w04sms;
106940160310              write fnore000;
106950160310            ENDIF;
106960160310          ENDIF;
106970160310        ENDIF;
106980140507
106990150305       //?Se sono in immissione
107000160208       //?o copia ORM
107010140603       //?oppure sono in conferma da VAO ma non ho scritto il rcd
107020160208           IF  *in01 or *in03 or
107030140603               (§RMfpr = 'C' and not wOKrcdg);
107040140507         //?Se flag impostati memorizzo rcd 'G'
107050160310           IF  §OREfiso <> *blanks or §OREfimo <> *blanks or
107060160310               §OREfmco <> *blanks or §OREfsco <> *blanks;
107070140507             k_OREtrc = 'G ';
107080140507             chain  %kds(k05fnore) FNORE01L;
107090140507             IF  %found(FNORE01L);
107100140507               OREdati = dOREgen;
107110140507               update fnore000;
107120140507             ELSE;
107130140507               clear fnore000;
107140140507               OREpoe = ORMpoe;
107150140507               OREnsr = ORMnsr;
107160140507               OREnor = ORMnor;
107170140507               OREnrv = ORMnrv;
107180140507               OREtrc = 'G ';
107190140507               OREdati = dOREgen;
107200140507               write fnore000;
107210140507             ENDIF;
107220140507           ENDIF;
107230140507         ENDIF;
107240131014
107250131014       ENDSR;
107260131014
107270131014       //--------------------------------------------------------------------
107280160406       //?F4 - Alert ORM Commissionato.
107290131014       //--------------------------------------------------------------------
107300131014       BEGSR F04D01;
107310131014
107320131025         wEndF04 = *off;
107330131025         wOkF6F04 = *off;
107340140423         IF  not wErrF04;
107350131030           *in28 = *off;
107360131030         ENDIF;
107370131212
107380131212       //?Salvo i dati della videata
107390131212         vidolddA = vidnewA;
107400131025
107410131025       //?emetto la videata relativa alla MAIL e SMS
107420131025         write fior051;
107430140423         *in28 = werralert;
107440140423         werralert = *off;
107450131025       //?emetto la window
107460131025         DOW not wEndF04;
107470131025           exfmt fior053;
107480131025           clear  w03msg;
107490131025           *in28 = *off;
107500131025         SELECT;
107510131025       //?F12-Ritorno
107520131025           WHEN  *inkl;
107530131025             wEndF04 = *on;
107540131025             *in28 = *off;
107550131025             *in90 = *on;
107560131212             vidnewA = vidolddA;
107570131025       //?F6-Conferma
107580131025           WHEN  *inkf;
107590131025             exsr CtrDatiF04;
107600131025             IF  not *in28;
107610131025               wEndF04 = *on;
107620131025               wOkF6F04 = *on;
107630131025             ENDIF;
107640131212             vidolddA = vidnewA;
107650131025       //?F10-Copia n.Telefono
107660131025           WHEN  *inkj;
107670160406             IF  w03sms = *blanks;
107680160310               w03sms = V1ter;
107690160310             ENDIF;
107700131025           OTHER;
107710131025       //?controllo i dati
107720131025             exsr CtrDatiF04;
107730131025           ENDSL;
107740131025         ENDDO;
107750131212       //?Imposto la riga dei tasti funzione
107760131212       ExSr Sr_Tastifun;
107770140509
107780140509       //?Se immissione/copia ORM commissionato e dati alert inseriti
107790140509       //?devo ricalcolare la data di ritiro
107800140512       //?se = ad oggi
107810140509         IF  (*in01 or *in03) and V1com = 'S' and
107820140512             (w03sms <> *blanks or w03mail <> *blanks) and
107830140512              V1dar > 0 and V1dar = oggi_ggmmaa;
107840140509           wCalDtRit = *on;
107850140509         ENDIF;
107860140509
107870140509         *inkf = *off;
107880140509         *inkl = *off;
107890140509         *inkj = *off;
107900131014
107910131014       ENDSR;
107920131011
107930131011       //--------------------------------------------------------------------
107940131011       //?F5 - Altri dati.
107950131011       //--------------------------------------------------------------------
107960131011       BEGSR F05D01;
107970131011
107980131011         wEndF05 = *off;
107990131011         wOkF6F05 = *off;
108000131030         IF  not wErrF05;
108010131030           *in28 = *off;
108020131030         ENDIF;
108030131212
108040131212       //?Salvo i dati della videata
108050131212         vidoldd2 = vidnew2;
108060131212
108070131212       //?Decodifico lo stato
108080131212         exsr sr_contrsto;
108090131212
108100131212       //?Decodifico il giro se impostato
108110131212         clear V1Dcgi;
108120131212         IF  V1Ccgi <> *blanks;
108130131212           exsr ctrgiro;
108140131212           IF  D09ocgi <> *blanks;
108150131212             V1Dcgi = D09odes;
108160131212           ENDIF;
108170131212         ENDIF;
108180131011
108190131011       //?emetto la videata relativa agli altri dati
108200131011         write fior051;
108210131011       //?emetto la window dove richiedo gli altri dati
108220131011         DOW not wEndF05;
108230131011           exfmt fior052;
108240131011           clear  w2cmsg;
108250131011           *in28 = *off;
108260131011         SELECT;
108270131011       //?F12-Ritorno
108280131011           WHEN  *inkl;
108290131011             wEndF05 = *on;
108300131011             *in28 = *off;
108310131011             *in90 = *on;
108320131212             vidnew2 = vidoldd2;
108330131212             wforzaqta = *off;
108340131011       //?F6-Conferma
108350131011           WHEN  *inkf;
108360131017             exsr CtrDatiF05;
108370131011             IF  not *in28;
108380131011               wEndF05 = *on;
108390131011               wOkF6F05 = *on;
108400131011             ENDIF;
108410131212             vidoldd2 = vidnew2;
108420131011           OTHER;
108430131011       //?controllo i dati
108440131017             exsr CtrDatiF05;
108450131011           ENDSL;
108460131011         ENDDO;
108470131212       //?Imposto la riga dei tasti funzione
108480131212       ExSr Sr_Tastifun;
108490131011
108500131011       ENDSR;
108510160406
108520160406       //--------------------------------------------------------------------
108530160406       //?F13 - Conferma Prenotazione.
108540160406       //--------------------------------------------------------------------
108550160406       BEGSR F13D01;
108560160406
108570160406         wEndF13 = *off;
108580160406         wOkF6F13 = *off;
108590160406         IF  not wErrF13;
108600160406           *in28 = *off;
108610160406         ENDIF;
108620160406
108630160413       //?Immissione ORM manuale o copia
108640160406       //?e non ci sono ancora i dati
108650160406         IF  (*in01 or *in03) and
108660160406             (V1Ctco = 'M' or V1Ctco = 'E') and
108670160406              W04sms = *blanks and W04mail = *blanks;
108680160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
108690160406           exsr RecDatiConf;
108700160406         ENDIF;
108710160406
108720160406       //?Se è richiamato da Conferma VAS
108730160406       //?proteggo i dati per Conferma Prenotazione
108740160406       //?a meno che non stia emettendo la videata per sistemare
108750160406       //?eventuali errori
108760160406         IF  §RMfpr = 'C' and not wErrConferma;
108770160406           *in20 = *on;
108780160406         ENDIF;
108790160406
108800160406       //?Salvo i dati della videata
108810160406         vidolddC = vidnewC;
108820160406
108830160406       //?emetto la videata relativa alla MAIL e SMS
108840160406         write fior051;
108850160406         *in28 = wErrConferma;
108860160406         wErrConferma = *off;
108870160406       //?emetto la window
108880160406         DOW  not wEndF13;
108890160406           exfmt fior054;
108900160406           clear w04msg;
108910160406           *in28 = *off;
108920160406           SELECT;
108930160406       //?F6-Conferma
108940160406           WHEN  *inkf;
108950160406             exsr CtrDatiF13;
108960160406             IF  not *in28;
108970160406               wEndF13 = *on;
108980160406               wOkF6F13 = *on;
108990160406             ENDIF;
109000160406             vidolddC = vidnewC;
109010160406       //?F10-Copia n.Telefono
109020160406           WHEN  *inkj;
109030160406             IF  w04sms = *blanks;
109040160406               w04sms = V1ter;
109050160406             ENDIF;
109060160406             HWriga = 06;
109070160406             HWcolo = 07;
109080160406       //?F12-Ritorno
109090160406           WHEN  *inkl;
109100160406             wEndF13 = *on;
109110160406             *in28 = *off;
109120160406             *in90 = *on;
109130160406             vidnewC = vidolddC;
109140160406       //?Enter
109150160406       //?controllo i dati
109160160406           OTHER;
109170160406             exsr CtrDatiF13;
109180160406           ENDSL;
109190160406         ENDDO;
109200160406
109210160406         *inkf = *off;
109220160406         *inkl = *off;
109230160406         *inkj = *off;
109240160406
109250160406       ENDSR;
109260130912
109270130912       //--------------------------------------------------------------------
109280130912       //?F14 - Interrogazione orari di servizio.
109290130912       //--------------------------------------------------------------------
109300130913       BEGSR F14D01;
109310150921
109320150921       //?Se non c'è la filiale ritiro e non ho peso/volume/bancali e non è
109330150921       //?un mittente codificato e siamo in immissione/copia
109340150921       //?non posso interrogare gli orari
109350150921         IF  num_V1Cpor = *zeros and V1pkg = 0 and V1vlm = 0 and
109360150921             V1bnc = 0 and (*in01 or *in03) and ds_v1ccra = 0;
109370150921           V1Cmsg = 'Per F14 serve la fil.ritiro e +
109380150921                     almeno 1 tra peso/vol./bancali';
109390150921           *in28 = *on;
109400150921           *in44 = *on;
109410150921           leavesr;
109420150921         ENDIF;
109430131024
109440131024       //?Se non presente calcolo la filiale ritiro
109450131024         IF  num_V1Cpor = *zeros and
109460131024            (*in01 or *in03);
109470131024           exsr  CALPOR;
109480140109         //?Recupero gli ambiti per il recupero del giro da ACR
109490140109           exsr sr_caragr;
109500131024           IF  *in28;
109510131024             leavesr;
109520131024           ENDIF;
109530131024         ENDIF;
109540130912
109550140319       //?Posso richiamare il pgm di int.orari di servizio solo se ho già
109560130912       //?la filiale di ritiro
109570130912         IF  num_V1Cpor = *zeros;
109580130912           V1Cmsg = 'Per interrogare gli orari serve la filiale ritiro';
109590130912           *in28 = *on;
109600130912           *in59 = *on;
109610130912           leavesr;
109620130912         ENDIF;
109630130912       //?l'indirizzo completo del mittente
109640130912         IF  V1car = *blanks or V1lor = *blanks;
109650130912           V1Cmsg = 'Per interrogare gli orari serve l''indirizzo del +
109660130912                     mittente';
109670130912           *in28 = *on;
109680130912           *in63 = *on;
109690130912           leavesr;
109700130912         ENDIF;
109710161116       //?il peso/volume/bancali
109720161116         IF  V1pkg = 0 and V1vlm = 0 and V1bnc = 0;
109730161116           V1Cmsg = 'Per interrogare gli orari occorre inserire +
109740161116                     almeno 1 tra peso/vol./bancali';
109750161116           *in28 = *on;
109760161116           *in44 = *on;
109770161116           leavesr;
109780161116         ENDIF;
109790161116       //?Calcolo il peso/volume se no in caso di conferma proposta si azzerano
109800161116          exsr sr_pesvol;
109810161116
109820161116       //?Cerco il giorno della settimana di "oggi"
109830161116         exsr DatiPerOrari;
109840140318
109850140318       //?Cerco eventuale ora presunta ritiro
109860140318         clear fior01ds;
109870140319       //?ma solo se ORM in distinta e NON quadrato
109880140318         IF  V1ndc > 0 and V1fao < 600;
109890140318           IOR01fgs = num_V1cpor;
109900140318           IOR01ndc = V1ndc;
109910140318           IOR01poe = V1cpoe;
109920140318           IOR01nsr = V1nsr;
109930140318           IOR01nor = V1nor;
109940140318           IOR01nrv = V1nrv;
109950140318           fior01r (fior01ds);
109960140318         ENDIF;
109970130912
109980140318         clear trulorsds;
109990130912         IOREfil = num_V1Cpor;
110000130912         IOREcap = V1car;
110010130912         IOREloc = V1lor;
110020130920         IOREnar = V1nar;
110030131024         IF  V1dar <> 0;
110040131024           IOREdta = inv_V1dar;
110050131024         ELSE;
110060131024           IOREdta = inv_V1dao;
110070131024         ENDIF;
110080130912         IOREtser = 'R';
110090131108         IOREgf2 = sav_O95gf2;
110100140318         IOREorp = OOR01ora;
110110161026
110120161116       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
110130161116       //?non è da fare per i ritiri esteri
110140161116         clear dIORE01;
110150170529         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks;
110160161116           §IOREr_mnx = 'S';
110170161116         ENDIF;
110180161026         IOREflo = dIORE01;
110190130912
110200140318         trulorsr (kpjba:trulorsds);
110210130912
110220130912       ENDSR;
110230131014
110240131014       //--------------------------------------------------------------------
110250131014       //?Controllo dati 'Altri Dati'.
110260131014       //--------------------------------------------------------------------
110270131014       BEGSR CtrDatiF05;
110280131030
110290131030         *in47 = *off;
110300131030         *in48 = *off;
110310131030         *in49 = *off;
110320131030         *in51 = *off;
110330131030         *in84 = *off;
110340131014
110350131014         exsr Sr_Contrsto;
110360131014         IF  *in28;
110370131014           leavesr;
110380131014         ENDIF;
110390131014
110400131211         exsr Sr_Contrddt;
110410131014         IF  *in28;
110420131014           leavesr;
110430131014         ENDIF;
110440131014
110450131014         exsr sr_Contrqta1;
110460131014         IF  *in28;
110470131014           leavesr;
110480131014         ENDIF;
110490131202       //?calcolo peso o volume se non impostati a video
110500131202       //?considerando anche le cubature degli automezzi eventualmente immessi
110510131202         exsr sr_pesvol;
110520131014
110530131014         exsr sr_contrcgi;
110540131014         IF  *in28 or *in90;
110550131014           leavesr;
110560131014         ENDIF;
110570131014
110580131014       ENDSR;
110590131014
110600131014       //--------------------------------------------------------------------
110610131017       //?Controllo dati Alert.
110620131014       //--------------------------------------------------------------------
110630131025       BEGSR CtrDatiF04;
110640131014
110650160310       //?Dati Alert ORM Commissionato
110660131017       //?controllo SMS;
110670131025         IF  w03sms <> *blanks;
110680131025           pInCell = %trim(w03sms);
110690131017           clear pOutErr;
110700140722           clear pOutCell;
110710131017           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
110720150526             IF  §RMfpr = 'C';
110730150526               clear W03sms;
110740150526               leavesr;
110750150526             ENDIF;
110760131017             *in28 = *on;
110770160406             HWriga = 04;
110780131017             HWcolo = 26;
110790140424             w03msg = msg(60);
110800131017             leavesr;
110810131017           ENDIF;
110820140722           IF  pOutCell <> *blanks;
110830140722             w03sms = pOutCell;
110840140722           ENDIF;
110850131014         ENDIF;
110860131014
110870131025         IF  w03mail <> *blanks;
110880131025           §emlindi = w03mail;
110890131014           xemail (dsemail);
110900131014           IF  §emlerro <> '0';
110910150526             IF  §RMfpr = 'C';
110920150526               clear W03mail;
110930150526               leavesr;
110940150526             ENDIF;
110950160406             HWriga = 06;
110960131017             HWcolo = 07;
110970131014             *in28 = *on;
110980140424             w03msg = msg(55);
110990131014             leavesr;
111000131014           ENDIF;
111010140722           IF  §emlindo <> *blanks;
111020140722             w03mail = §emlindo;
111030140722           ENDIF;
111040131014         ENDIF;
111050131014
111060131014       ENDSR;
111070160406
111080160406       //--------------------------------------------------------------------
111090160406       //?Controllo dati Conferma Prenotazione.
111100160406       //--------------------------------------------------------------------
111110160406       BEGSR CtrDatiF13;
111120160406
111130160406       //?Dati Conferma Prenotazione
111140160406         IF  w04sms <> *blanks;
111150160406           pInCell = %trim(w04sms);
111160160406           clear pOutErr;
111170160406           clear pOutCell;
111180160406           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
111190160406             *in28 = *on;
111200160406             H4riga = 04;
111210160406             H4colo = 26;
111220160406             w04msg = msg(60);
111230160406             leavesr;
111240160406           ENDIF;
111250160406           IF  pOutCell <> *blanks;
111260160406             w04sms = pOutCell;
111270160406           ENDIF;
111280160406         ENDIF;
111290160406
111300160406         IF  w04mail <> *blanks;
111310160406           §emlindi = w04mail;
111320160406           xemail (dsemail);
111330160406           IF  §emlerro <> '0';
111340160406             H4riga = 06;
111350160406             H4colo = 07;
111360160406             *in28 = *on;
111370160406             w04msg = msg(55);
111380160406             leavesr;
111390160406           ENDIF;
111400160406           IF  §emlindo <> *blanks;
111410160406             w04mail = §emlindo;
111420160406           ENDIF;
111430160406         ENDIF;
111440160406
111450160406       //?Dati Conferma VAS e avevo il flag di OK conferma ma non ho i dati
111460160406       //?pulisco il flag in modo da non scrivere il file spia
111470160406         IF  §RMfpr = 'C' and
111480160406             W04mail = *blanks and W04sms = *blanks;
111490160406           wconferma = *off;
111500160406         ENDIF;
111510160406
111520160406         sav_mailconf = w04mail;
111530160406         sav_smsconf  = w04sms;
111540160406
111550160406       ENDSR;
111560140320
111570140320       //--------------------------------------------------------------------
111580140320       //?Cerco orari servizio.
111590140320       //--------------------------------------------------------------------
111600140320       BEGSR CercaOrari;
111610161026
111620161026       //?Se immissione/copia o conferma ORM commissionati
111630161026       //?Visualizzo orari servizio
111640161026         IF  *in01 or *in03 or §RMfpr = 'O';
111650161026           *in95 = *on;
111660161026         ENDIF;
111670140320
111680160526       //?Richiamo il TRUL solo se non è ritiro Estero
111690140320         clear trulorsds;
111700140320         clear trulor2ds;
111710160526         IF  V1nar = *blanks;
111720140320         IOREfil = num_V1Cpor;
111730140320         IOREcap = V1car;
111740140320         IOREloc = V1lor;
111750140320         IOREnar = V1nar;
111760140320         IF  V1dar <> 0;
111770140320           IOREdta = inv_V1dar;
111780140320         ELSE;
111790140320           IOREdta = inv_V1dao;
111800140320         ENDIF;
111810140320         IOREtser = 'R';
111820140320         IOREgf2 = sav_O95gf2;
111830140320         trulorsr (kpjba:trulorsds:trulor2ds);
111840140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
111850140320           *in95 = *off;
111860140320           leavesr;
111870140320         ELSE;
111880140320           V1ostis = OOR2stis;
111890140320           V1ostfs = OOR2stfs;
111900140320         ENDIF;
111910160526         ENDIF;
111920160526       //?Se ritiro Estero chiamo nuovo FIORE00R
111930160526         IF  V1nar <> *blanks;
111940160526           clear FIORE00DS;
111950160526           IORE00por = num_V1Cpor;
111960160526           IORE00nar = V1nar;
111970160526           fiore00r (fiore00ds);
111980160526           IF  OORE00err <> *blanks;
111990160526             *in95 = *off;
112000160526             leavesr;
112010160526           ELSE;
112020160526             V1ostis = OORE00min;
112030160526             V1ostfs = OORE00max;
112040160526           ENDIF;
112050160526         ENDIF;
112060161115
112070161115         exsr DatiPerOrari;
112080161021
112090161021       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
112100161021       //?non è da fare per i ritiri esteri
112110161115       //?non è da fare se non ho gli orari minimo e massimo
112120161027         OrariEstesi = *off;
112130161115         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks and
112140161027             OOR2miis > *zeros and OOR2mxfs > *zeros;
112150161021           V1ostis = OOR2miis;
112160161021           V1ostfs = OOR2mxfs;
112170161027           OrariEstesi = *on;
112180161021         ENDIF;
112190140320
112200140320         wOKorariSer = *on;
112210140320
112220140320       ENDSR;
112230161021
112240161021       //--------------------------------------------------------------------
112250161026       //?Aggiungo/Sottraggo 1 gg lavorativo alla data ritiro
112260161021       //--------------------------------------------------------------------
112270161026       BEGSR AddSubUnoDar;
112280161027
112290161027       //?Non dovrebbe essere possibile, ma non si sa mai
112300161027         IF  V1dar = 0;
112310161027           *in28 = *on;
112320161027           V1Cmsg = 'PageUp/PageDown non possibile senza data ritiro';
112330161027           leavesr;
112340161027         ENDIF;
112350161027
112360161027         clear XGIOLAVDS;
112370161027         clear wlbdat;
112380161027         IXGLdata = inv_V1dar;
112390161027         IF  dsp_aid = c_PageDown;
112400161027           IXGLsub  = 'S';
112410161027         ENDIF;
112420161027         IF  dsp_aid = c_PageUp;
112430161027           IXGLadd  = 'S';
112440161027         ENDIF;
112450161027         IXGLgg   = 1;
112460161027         IXGLlav  = 'S';
112470161027         IXGLpa   = 'P';
112480161027         IXGLfil  = num_V1Cpor;
112490161027         XGIOLAV (xgiolavds);
112500161027         IF  OXGLerr <> *blanks;
112510161027           leavesr;
112520161027         ENDIF;
112530161027
112540161122       //?Solo se immissione
112550161122         IF  *in01;
112560161028         //?Se data ritiro inferiore alla data ritiro calcolata errore
112570161028           IF  OXGldata < OOR97dar and
112580161028               OXGLdata < OOR97dmin;
112590161028             *in28 = *on;
112600161028             V1Cmsg = msg(51);
112610161028             leavesr;
112620161028           ENDIF;
112630161028         ENDIF;
112640161027
112650161108       //?Supera la data max consentita
112660161108         IF  OXGLdata > OOR97dmaxb;
112670161108           *in28 = *on;
112680161108           V1Cmsg = msg(35);
112690161108           V1Cmsg = %trim(V1Cmsg) + ' supera '+
112700161108           d§dftgg2 + ' gg. dalla data immissione ORM';
112710161108           leavesr;
112720161108         ENDIF;
112730161108
112740161108       //?Non può essere inferiore alla data immissione ORM
112750161108         IF  OXGLdata < inv_V1dao;
112760161108           *in28 = *on;
112770161108           V1Cmsg = msg(65);
112780161108           leavesr;
112790161108         ENDIF;
112800161027
112810161110         clear wlbdat;
112820161027         G02inv = OXGLdata;
112830161027         G02err = '3';
112840161027         xsrda8 (wlbdat);
112850161027         V1dar = G02dat;
112860161027         inv_v1dar = OXGLdata;
112870161108         old_invv1dar = OXGLdata;
112880161021
112890161021       ENDSR;
112900130912
112910100218      /end-free
112920090206
112930090206      *--------------------------------------------------------------------*
112940090206      * File FNORA00F
112950090206      *--------------------------------------------------------------------*
112960090206     c     sr_wrtapp     begsr
112970090206
112980090206     c     kfnora        chain     fnora01l
112990090206     c                   if        not %found(fnora01l)
113000090206     c                   clear                   fnora000
113010090206     c                   eval      orapoe = ormpoe
113020090206     c                   eval      oransr = ormnsr
113030090206     c                   eval      oranor = ormnor
113040090206     c                   eval      oranrv = ormnrv
113050090407     c                   eval      orafar = 100
113060090216     c                   eval      oradae = v1dfo
113070090216     c                   eval      oraore = v1ofo
113080090206     c                   eval      oraref = wappref
113090090206     c                   eval      oranote = wappnota1 + wappnota2
113100090206     c                   eval      oradata = oggi_aammgg
113110090206     c                   time                    w0140
113120090206     c                   movel     w0140         oraora
113130090206     c                   eval      oraute = knmus
113140090206     c                   write     fnora000
113150090206     c                   endif
113160090206
113170090206     c                   if        %found(fnora01l)
113180090206     c                   eval      oraref = wappref
113190090206     c                   eval      oranote = wappnota1 + wappnota2
113200090206     c                   eval      oradata = oggi_aammgg
113210090206     c                   time                    w0140
113220090206     c                   movel     w0140         oraora
113230090206     c                   eval      oraute = knmus
113240090206     c                   update    fnora000
113250090206     c                   endif
113260090206
113270090206     c                   endsr
113280070903
113290010704      **********************************************************************
113300010704      * RICHIAMO IMMISSIONE PREPAGATI
113310010704      **********************************************************************
113320010704     C     Sr_Immprep    begsr
113330030623
113340030623     c                   Clear                   wtrulvid
113350010704
113360020522      * richiamo il programma per la scelta delle stampanti
113370030307     c                   Clear                   wdatibolla
113380030307     c                   Eval      wdatibolla = 'Prepagati'
113390030623     c                   Eval      wdatibolA4 = 'Prepag_A4'
113400030623     c                   Eval      wdatibolA5 = 'Prepag_A5'
113410020522     c                   exsr      sr_scestp
113420020522     c   92              goto      endsrimmprep
113430010704
113440010704      * Richiamo immissione prepagati
113450010704     C                   clear                   kpjbu
113460010704     C                   clear                   dtasv
113470010704     C                   clear                   dblp
113480010704
113490010704     C                   move      'O'           §LPfpr
113500010704     C                   movel     ORMpoe        §LPpoe
113510010704     C                   movel     ORMnsr        §LPnsr
113520010704     C                   movel     ORMnor        §LPnor
113530010704     C                   movel     ORMnrv        §LPnrv
113540010704
113550010704     C                   call      'FNLS01R'
113560010704     C                   parm                    kpjba
113570010704     C                   parm                    dblp
113580010704     C                   parm                    dtasv
113590030623     c                   Parm                    Trul90ds
113600010704
113610010704     C                   eval      okbolla = '1'
113620010704
113630010704     C                   if        §lpfpr = '4' or §lpfpr = '3'
113640010704     C                             or §lpfpr = '5'
113650010704     C                   eval      okbolla = ' '
113660010704     C                   endif
113670010704
113680010704     C     endsrimmprep  endsr
113690020522      **********************************************************************
113700020522      * scelta stampante per immissione bolla
113710020522      **********************************************************************
113720020522     c     sr_scestp     begsr
113730020522
113740020522     C                   clear                   kpjbu
113750020522     c                   setoff                                       92
113760020522
113770020522      * Richiesta stampanti
113780020522     C                   if        conta = *zeros
113790030623     C                   clear                   trul90ds
113800030623      * se non serve non faccio vedere il video della scelta stampanti
113810030623     c                   If        wtrulvid <> *Blanks
113820030623     C                   movel     'X'           D90rse
113830030623     C                   movel     'X'           D90rsb
113840030623     c                   Else
113850030623     C                   movel     'S'           D90rse
113860030623     C                   movel     'S'           D90rsb
113870030623     c                   EndIf
113880030623     C                   call      'TRUL90R'
113890020522     C                   parm                    kpjba
113900030623     C                   parm                    trul90ds
113910020522      * F3=Fine
113920030623     C                   if        D90f3 = '1'
113930020522     C                   eval      *in92 = *on
113940020522     C                   goto      endstp
113950020522     C                   endif
113960030307
113970030307     C                   z-add     103           lung
113980020522
113990020522      * Ovrprtf Segnacolli
114000030623     C                   movea     D90pre        cm3(28)
114010030623     C                   movea     D90mde        cm3(50)
114020020522     C                   clear                   comman
114030020522     C                   movea     cm3           comman
114040020522     C                   CALL      'QCMDEXC'                            92
114050020522     C                   PARM                    comman
114060020522     C                   PARM                    lung
114070020522     C   92              goto      endstp
114080030623
114090030623      * Bolle - Nuovo modulo
114100030623
114110030623     c                   Z-Add     110           lung
114120030623
114130030623      * Ovrprtf Bolle A4
114140030623     c                   Movea     D90prb4       cma4(30)
114150030623     c                   Movea     D90mdb4       cma4(52)
114160030623     c                   Movea     wdatibolA4    cma4(96)
114170030623     c                   Clear                   Comman
114180030623     c                   Movea     cmA4          Comman
114190030623     c                   Call      'QCMDEXC'                            92
114200030623     c                   Parm                    Comman
114210030623     c                   Parm                    Lung
114220030623     c   92              goto      endstp
114230030623
114240030623      * Ovrprtf Bolle A5
114250030623     c                   Movea     D90prb5       cmA5(30)
114260030623     c                   Movea     D90mdb5       cmA5(52)
114270030623     c                   Movea     wdatibolA5    cma5(96)
114280030623     c                   Clear                   Comman
114290030623     c                   Movea     cmA5          Comman
114300030623     c                   Call      'QCMDEXC'                            92
114310030623     c                   Parm                    Comman
114320030623     c                   Parm                    Lung
114330030623     c   92              goto      endstp
114340020522
114350030623      * se non faccio vedere il video non aggiorno il contatore
114360030623     c                   If        wtrulvid = *Blanks
114370020522     C                   eval      conta = conta +1
114380030623     c                   EndIf
114390020522     C                   endif
114400020522
114410020522     c     endstp        endsr
114420001019      **********************************************************************
114430001023      * SCRIVE storico variazioni
114440001019      **********************************************************************
114450001019     C     Sr_Storico    BEGSR
114460001019
114470001019     C                   clear                   fnorv000
114480001019     C                   eval      ORVpoe = ORMpoe
114490001019     C                   eval      ORVnsr = ORMnsr
114500001019     C                   eval      ORVnor = ORMnor
114510001019     C                   eval      ORVnrv = ORMnrv
114520001019     C                   eval      ORVtor = ORMtor
114530001019     C                   eval      ORVcor = ORMcor
114540001019     C                   eval      ORVrso = ORMrso
114550001019     C                   eval      ORVino = ORMino
114560001019     C                   eval      ORVcao = ORMcao
114570001019     C                   eval      ORVloo = ORMloo
114580001019     C                   eval      ORVpro = ORMpro
114590001019     C                   eval      ORVnao = ORMnao
114600001019     C                   eval      ORVcra = ORMcra
114610001019     C                   eval      ORVrsr = ORMrsr
114620001019     C                   eval      ORVinr = ORMinr
114630001019     C                   eval      ORVcar = ORMcar
114640001019     C                   eval      ORVlor = ORMlor
114650001019     C                   eval      ORVprr = ORMprr
114660001019     C                   eval      ORVnar = ORMnar
114670001019     C                   eval      ORVrer = ORMrer
114680001019     C                   eval      ORVter = ORMter
114690001019     C                   eval      ORVcrc = ORMcrc
114700001019     C                   eval      ORVrsc = ORMrsc
114710001019     C                   eval      ORVinc = ORMinc
114720001019     C                   eval      ORVloc = ORMloc
114730001019     C                   eval      ORVcac = ORMcac
114740001019     C                   eval      ORVprc = ORMprc
114750001019     C                   eval      ORVnac = ORMnac
114760001019     C                   eval      ORVffd = ORMffd
114770001019     C                   eval      ORVdar = ORMdar
114780001019     C                   eval      ORVorr = ORMorr
114790001019     C                   eval      ORVnam = ORMnam
114800001019     C                   eval      ORVncl = ORMncl
114810001019     C                   eval      ORVpkg = ORMpkg
114820001019     C                   eval      ORVvlm = ORMvlm
114830001019     C                   eval      ORVbnc = ORMbnc
114840001019     C                   eval      ORVblc = ORMblc
114850001019     C                   eval      ORVatt = ORMatt
114860001019     C                   eval      ORVmtc = ORMmtc
114870001019     C                   eval      ORVpag = ORMpag
114880001019     C                   eval      ORVksc = ORMksc
114890001019     C                   eval      ORVctr = ORMctr
114900001024     C                   eval      ORVddt = ORMddt
114910001019     C                   eval      ORVpor = ORMpor
114920130416     C                   eval      ORVzor = ORMzor
114930001019     C                   eval      ORVpoc = ORMpoc
114940001019     C                   eval      ORVno1 = ORMno1
114950001019     C                   eval      ORVno2 = ORMno2
114960001019     C                   eval      ORVcst = ORMcst
114970001019     C                   eval      ORVvcs = ORMvcs
114980001019     C                   eval      ORVfcs = ORMfcs
114990001019     C                   eval      ORVdfo = ORMdfo
115000001019     C                   eval      ORVofo = ORMofo
115010001019     C                   eval      ORVfao = ORMfao
115020001027     C                   eval      ORVsto = ORMsto
115030001019     C                   eval      ORVdst = ORMdst
115040001019     C                   eval      ORVnpg = ORMnpg
115050001019     C                   eval      ORVndc = ORMndc
115060001019     C                   eval      ORVddc = ORMddc
115070001019     C                   eval      ORVstp = ORMstp
115080001027     C                   eval      ORVrfa = ORMrfa
115090070706     C                   eval      ORVeti = ORMeti
115100001103     C                   eval      ORVsto = ORMsto
115110010119     C                   eval      ORVspi = ORMspi
115120001019     C                   eval      ORVflo = ORMflo
115130010116     C                   z-add     oggi_aammgg   ORVdtv
115140001019     C                   movel     w0140         ORVorv
115150001019     C                   movel     knmus         ORVutv
115160040908     c                   Eval      OrvDtt = oggi_aammgg
115170161115     c                   eval      ORVrmp = ORMrmp
115180001019     C                   write     fnorv000
115190131024
115200131024      /free
115210131024       //?Scrivo anche le estensioni
115220131024         IF  wOkestensione;
115230131024           setll    (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115240131024           reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115250140520           DOW  not %eof(FNORE01L);
115260131024             clear FNORVE00;
115270131024             ORVEpoe = OREpoe;
115280131024             ORVEnsr = OREnsr;
115290131024             ORVEnor = OREnor;
115300131024             ORVEnrv = OREnrv;
115310131024             ORVEdtv = ORVdtv;
115320131024             ORVEorv = ORVorv;
115330131024             ORVEutv = ORVutv;
115340131024             ORVEtrc = OREtrc;
115350131024             ORVEdati = OREdati;
115360131024             write FNORVE00;
115370140520             reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115380140520           ENDDO;
115390131024         ENDIF;
115400131024      /end-free
115410001019
115420001019     C                   endsr
115430001019      **********************************************************************
115440001023      * SCRIVE proposte variazioni
115450001019      **********************************************************************
115460001103     C     Sr_CarProposteBEGSR
115470140227
115480140227      /free
115490140227       //?Se è stata fatta una proposta di nota AUT devo aggiornare FNORP
115500140227       //?scritto da FIORT1R
115510140227         IF  wF17write;
115520140227           chain (kpoe:knsr:knor:knrv:OORT1dim:OORT1oim) FNORP01L;
115530140227           IF  %found(FNORP01L);
115540140227             exsr move_proposte;
115550140227             update fnorp000;
115560140227           ENDIF;
115570140227         ENDIF;
115580140227      /end-free
115590001019
115600140227     c                   IF        not wF17write
115610010116     C                   clear                   fnorp000
115620010116     C                   eval      ORPpoe = v1cpoe
115630010116     C                   eval      ORPnsr = v1nsr
115640010116     C                   eval      ORPnor = v1nor
115650010116     C                   eval      ORPnrv = v1nrv
115660140227     c                   exsr      move_proposte
115670140227     C                   z-add     oggi_aammgg   ORPdtv
115680140227     C                   movel     w0140         ORPorv
115690140227     C                   movel     knmus         ORPutv
115700001019     C                   write     fnorp000
115710140227     c                   ENDIF
115720131024
115730131024      /free
115740131024       //?Scrivo anche le estensioni
115750131024         IF  v1OraAMda <> 0 or v1OraAMa <> 0 or
115760131024             v1OraPMda <> 0 or v1OraPMa <> 0;
115770131024           §OREoramda = v1OraAMda;
115780131024           §OREorama  = v1OraAMa;
115790131024           §OREorapda = v1OraPMda;
115800131024           §OREorapa  = v1OraPMa;
115810131024           clear FNORPE00;
115820131024           ORPEpoe = ORPpoe;
115830131024           ORPEnsr = ORPnsr;
115840131024           ORPEnor = ORPnor;
115850131024           ORPEnrv = ORPnrv;
115860131024           ORPEdtv = ORPdtv;
115870131024           ORPEorv = ORPorv;
115880131024           ORPEutv = ORPutv;
115890131024           ORPEtrc = 'O ';
115900131024           ORPEdati = dOREorari;
115910131024           write FNORPE00;
115920131024         ENDIF;
115930161020         //?RCD DT - Data Pronta Merce
115940161027         clear dOREdt;
115950161027         §OREdpm = inv_v1dpm;
115960170605         §OREdar = %editc(DarCalcolata:'X');
115970170605         IF  wOkPosticipa;
115980170605           §OREposd = 'S';
115990170605         ENDIF;
116000170605         IF  wAnticipa;
116010170605           §OREant = 'S';
116020170605         ENDIF;
116030170605         IF  Anticipato;
116040170605           §OREmod = 'S';
116050170605         ENDIF;
116060170605         §OREgga = %editc(ggAnticipo:'X');
116070161020         clear fnorpe00;
116080161020         ORPEpoe = ORPpoe;
116090161020         ORPEnsr = ORPnsr;
116100161020         ORPEnor = ORPnor;
116110161020         ORPEnrv = ORPnrv;
116120161020         ORPEdtv = ORPdtv;
116130161020         ORPEorv = ORPorv;
116140161020         ORPEutv = ORPutv;
116150161020         ORPEtrc = 'DT';
116160161027         ORPEdati = dOREdt;
116170161020         write FNORPE00;
116180160406         IF  w04sms <> *blanks;
116190160311           clear FNORPE00;
116200160311           ORPEpoe = ORPpoe;
116210160311           ORPEnsr = ORPnsr;
116220160311           ORPEnor = ORPnor;
116230160311           ORPEnrv = ORPnrv;
116240160311           ORPEdtv = ORPdtv;
116250160311           ORPEorv = ORPorv;
116260160311           ORPEutv = ORPutv;
116270160311           ORPEtrc = 'SC';
116280160406           ORPEdati = w04sms;
116290160311           write FNORPE00;
116300160311         ENDIF;
116310160406         IF  w04mail <> *blanks;
116320160311           clear FNORPE00;
116330160311           ORPEpoe = ORPpoe;
116340160311           ORPEnsr = ORPnsr;
116350160311           ORPEnor = ORPnor;
116360160311           ORPEnrv = ORPnrv;
116370160311           ORPEdtv = ORPdtv;
116380160311           ORPEorv = ORPorv;
116390160311           ORPEutv = ORPutv;
116400160311           ORPEtrc = 'MC';
116410160406           ORPEdati = w04mail;
116420160311           write FNORPE00;
116430160311         ENDIF;
116440131025         IF  w03sms <> *blanks;
116450140505           §OREsms = w03sms;
116460131024           clear FNORPE00;
116470131024           ORPEpoe = ORPpoe;
116480131024           ORPEnsr = ORPnsr;
116490131024           ORPEnor = ORPnor;
116500131024           ORPEnrv = ORPnrv;
116510131024           ORPEdtv = ORPdtv;
116520131024           ORPEorv = ORPorv;
116530131024           ORPEutv = ORPutv;
116540131024           ORPEtrc = 'S ';
116550140505           ORPEdati = dOREsms;
116560131024           write FNORPE00;
116570131024         ENDIF;
116580131025         IF  w03mail <> *blanks;
116590131024           clear FNORPE00;
116600131024           ORPEpoe = ORPpoe;
116610131024           ORPEnsr = ORPnsr;
116620131024           ORPEnor = ORPnor;
116630131024           ORPEnrv = ORPnrv;
116640131024           ORPEdtv = ORPdtv;
116650131024           ORPEorv = ORPorv;
116660131024           ORPEutv = ORPutv;
116670131024           ORPEtrc = 'MA';
116680131025           ORPEdati = w03mail;
116690131024           write FNORPE00;
116700131024         ENDIF;
116710131024      /end-free
116720001019
116730001019     C                   endsr
116740140227
116750140227      *------------------------------------------------------------------------*
116760140227      *  Muovo i campi del video al file delle proposte
116770140227      *------------------------------------------------------------------------*
116780140227     c     move_proposte begsr
116790140227
116800140227     C                   eval      ORPtor = v1ctor
116810140227     C                   eval      ds_v1cor1 = v1cor1
116820140227     C                   eval      ds_v1cor2 = v1cor2
116830140227     C                   eval      ds_v1cor3 = v1cor3
116840140227     C                   eval      ORPcor = ds_v1ccor
116850140227     C                   eval      ORPrso = v1rso
116860140227     C                   eval      ORPino = v1ino
116870140227     C                   eval      ORPcao = v1cao
116880140227     C                   movel(p)  sav_v1loo35   ORPloo
116890140227     C                   eval      ORPpro = v1pro
116900140227     C                   eval      ORPnao = v1nao
116910140227     C                   eval      ds_v1cra1 = v1cra1
116920140227     C                   eval      ds_v1cra2 = v1cra2
116930140227     C                   eval      ds_v1cra3 = v1cra3
116940140227     C                   eval      ORPcra = ds_v1ccra
116950140227     C                   eval      ORPrsr = v1rsr
116960140227     C                   eval      ORPinr = v1inr
116970140227     C                   eval      ORPcar = v1car
116980140227     C                   movel(p)  v1lor         ORPlor
116990140227     C                   eval      ORPprr = v1prr
117000140227     C                   eval      ORPnar = v1nar
117010140227     C                   eval      ORPrer = v1rer
117020140227     C                   eval      ORPter = v1ter
117030140227     C                   eval      ds_v1crc1 = v1crc1
117040140227     C                   eval      ds_v1crc2 = v1crc2
117050140227     C                   eval      ds_v1crc3 = v1crc3
117060140227     C                   eval      ORPcrc = ds_v1ccrc
117070140227     C                   eval      ORPrsc = v1rsc
117080140227     C                   eval      ORPinc = v1inc
117090140227     C                   movel(p)  sav_v1loc35   ORPloc
117100140227     C                   eval      ORPcac = v1cac
117110140227     C                   eval      ORPprc = v1prc
117120140227     C                   eval      ORPnac = v1nac
117130140227     C                   eval      ORPffd = v1ffd
117140140227      * Inversione data ritiro
117150140227     C                   clear                   wlbdat
117160140227     C                   z-add     v1dar         G02dat
117170140227     C                   movel     *blanks       G02err
117180140227     C                   call      'XSRDA8'
117190140227     C                   parm                    wlbdat
117200140227     C                   z-add     G02inv        ORPdar
117210140227
117220140227     C                   eval      ORPorr = v1orr
117230140227     C                   eval      ORPnam = v1nam
117240140227     C                   eval      ORPncl = v1ncl
117250140227     C                   eval      ORPpkg = v1pkg
117260140227     C                   eval      ORPvlm = v1vlm
117270140227     C                   eval      ORPbnc = v1bnc
117280140227     C                   eval      ORPblc = v1blc
117290140227     C                   eval      ORPatt = v1att
117300140227     C                   eval      ORPmtc = v1mtc
117310140227     C                   eval      ORPpag = v1pag
117320140227     C                   eval      ORPksc = v1ksc
117330140227     C                   eval      ORPctr = v1ctr
117340140227     C                   eval      ORPddt = v1ddt
117350140227     C                   movel     v1cpor        ORPpor
117360140227     C                   movel     v1cpoc        ORPpoc
117370140227     C                   eval      ORPno1 = v1not1
117380140227     C                   eval      ORPno2 = v1not2
117390140227     C                   eval      ORPrfa = v1rfa
117400140227     C                   move      v1sto         ORPsto
117410140227     C                   eval      ORPcst = v1cst
117420140227     C                   eval      ORPvcs = v1vcs
117430140227     C                   eval      ORPfcs = v1fcs
117440140227     C                   eval      ORPdfo = v1dfo
117450140227     C                   eval      ORPofo = v1ofo
117460140227     C                   eval      ORPfao = v1fao
117470140227     C                   eval      ORPdst = v1dst
117480140227     C                   eval      ORPnpg = v1npg
117490140227     C                   eval      ORPndc = v1ndc
117500140227     C                   eval      ORPddc = v1ddc
117510140227     C                   eval      ORPstp = v1stp
117520140227     C                   eval      ORPtap = v1tap
117530140227     C                   eval      ORPeti = v1eti
117540140227     C                   eval      ORPspi = v1spi
117550140227     C                   eval      ORPcau = v3cau
117560140227      * Se forzato blocco destinatario devo pulire il relativo flag
117570140227     C                   if        §ormfd = 'S'
117580140227     C                   if        v1ctor <> 'S'
117590140227     C                   clear                   §ormfd
117600140227     C                   else
117610140227     C                   if        v1rsc = *blanks or ds_v1ccrc = *zeros
117620140227     C                   clear                   §ormfd
117630140227     C                   endif
117640140227     C                   endif
117650140227     C                   endif
117660140227      * ORM commissionato
117670140227     c                   Eval      §orcom = v1com
117680140227
117690140227      * in base al tipo orm e a chi paga imposto il campo §orpfb
117700140227      * imposto 'F' se paga mittente per orm singolo o prepagato
117710140227      * imposto 'A' se NON paga mittente
117720140227     c                   Clear                   §orPfb
117730140227     c                   Select
117740140227     c                   When      OrpPag = 'M'
117750140227     c                   If        OrpTor = 'P' or OrpTor = 'S'
117760140227     c                   Eval      §orPfb = 'F'
117770140227     c                   EndIf
117780140227     c                   Other
117790140227     c                   Eval      §orPfb = 'A'
117800140227     c                   EndSl
117810140227
117820140227     C                   eval      ORPflo = dorm01
117830140227
117840140227     c                   Eval      OrpDtt = oggi_aammgg
117850140227      * Mette sempre in positivo i campi delle quantità
117860140227     C                   mllzo     1             ORPorr
117870140227     C                   mllzo     1             ORPncl
117880140227     C                   mllzo     1             ORPpkg
117890140227     C                   mllzo     1             ORPvlm
117900140227     C                   mllzo     1             ORPbnc
117910140227     C                   mllzo     1             ORPblc
117920140227     C                   mllzo     1             ORPatt
117930140227     C                   mllzo     1             ORPmtc
117940161115      /free
117950161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
117960161115         //?ipotetico ritiro da effettuare al mattino
117970161115         IF  Orariestesi;
117980161115           ORPrmp = 'M';
117990161115         ENDIF;
118000161116         //?Se ORM da Internet non lo memorizzo
118010161116         IF  ORMtco = 'I';
118020161116           clear ORPrmp;
118030161116         ENDIF;
118040161115      /end-free
118050161115
118060140227     c                   endsr
118070131011
118080131011      *------------------------------------------------------------------------*
118090131011      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
118100131011      *------------------------------------------------------------------------*
118110131011     c     sr_f24        begsr
118120131011
118130131011     c                   Select
118140131011
118150131011     c                   When      $loop = 2 and $rig =2  or
118160131011     c                             $loop = 3 and $rig =3
118170131011     c                   Movea     rigatf1       vzfd01
118180131011     c                   Z-add     1             $rig
118190131011
118200131011     c                   If        $loop = 2
118210131011     c                   Eval      vzfd02= cf2412
118220131011     c                   Else
118230131011     c                   Eval      vzfd02= cf2413
118240131011     c                   EndIf
118250131011
118260131011     c                   When      $loop = 2 and $rig =1 or
118270131011     c                             $loop = 3 and $rig =1
118280131011     c                   Movea     rigatf2       vzfd01
118290131011     c                   Z-add     2             $rig
118300131011
118310131011     c                   If        $loop = 2
118320131011     c                   Eval      vzfd02 = cf2422
118330131011     c                   Else
118340131011     c                   Eval      vzfd02 = cf2423
118350131011     c                   EndIf
118360131011
118370131011     c                   When      $loop = 3 and $rig =2
118380131011     c                   Movea     rigatf3       vzfd01
118390131011     c                   Z-add     3             $rig
118400131011     c                   Eval      vzfd02 = cf2433
118410131011     c                   EndSl
118420131011
118430131011     c                   EndSr
118440131011
118450131011      *------------------------------------------------------------------------*
118460131011      *  Imposto descrizione tasti funzione
118470131011      *------------------------------------------------------------------------*
118480131011     c     Sr_Tastifun   BegSr
118490140130
118500140130      /free
118510140130       //?Routine per abilitazione tasti funzione riga fissa
118520140130         exsr Abi_fxx;
118530140130      /end-free
118540131011
118550131011      * Conta i caratteri riempiti dalle RigaTf1
118560131011     c                   Clear                   $z
118570131011      * Conta i caratteri riempiti dalle RigaTf2
118580131011     c                   Clear                   $k
118590131011      * Conta i caratteri riempiti dalle RigaTf3
118600131011     c                   Clear                   $j
118610131011      * Conta la posizione del campone da cui partire per impostare
118620131011      * la descrizione del tasto funzione
118630131011     c                   Clear                   $y
118640131011
118650131011      * Stringhe che contengono le descrizioni dei tasti funzione
118660131011     c                   Reset                   rigatf1
118670131011     c                   Reset                   rigatf2
118680131011     c                   Reset                   rigatf3
118690131011
118700131011      * F2 - Dati DPD
118710131011     c                   if        *in02 and not *in10 and
118720131011     c                             *in18 and not *in17
118730131011     c                   reset                   $tf
118740131011     c                   movea     tf02          $tf
118750131011     c                   exsr      rie_$tf
118760131011     c                   endif
118770131011      * F11 - Volume
118780131011     c                   if        not *in10 and not *in26
118790131011     c                   reset                   $tf
118800131011     c                   movea     tf11          $tf
118810131011     c                   exsr      rie_$tf
118820131011     c                   endif
118830131011      * F6 - Conferma
118840131011     c                   if        *in01 or *in02 or *in03
118850131011     c                   reset                   $tf
118860131011     c                   movea     tf06          $tf
118870131011     c                   exsr      rie_$tf
118880131011     c                   endif
118890160406      * F13 - Conferma Prenotazione Ritiro
118900160406     c                   if        *in88
118910160406     c                   reset                   $tf
118920160406     c                   movea     tf13          $tf
118930160406     c                   exsr      rie_$tf
118940160406     c                   endif
118950131011      * F14 - Orari Servizio
118960131108     c                   if        not *in10 and
118970131025     c                             not *in17
118980131011     c                   reset                   $tf
118990131011     c                   movea     tf14          $tf
119000131011     c                   exsr      rie_$tf
119010131011     c                   endif
119020131011      * F20 - Simulazione
119030131011     c                   if        *in01
119040131011     c                   reset                   $tf
119050131011     c                   movea     tf20          $tf
119060131011     c                   exsr      rie_$tf
119070131011     c                   endif
119080131011      * F16 - Chiusura
119090140918     c                   if        *in98
119100131011     c                   reset                   $tf
119110131011     c                   movea     tf16          $tf
119120131011     c                   exsr      rie_$tf
119130131011     c                   endif
119140131011      * F19 - Proposte variazione
119150131011     c                   if        *in57 and not *in10
119160131011     c                   reset                   $tf
119170131011     c                   movea     tf19          $tf
119180131011     c                   exsr      rie_$tf
119190131011     c                   endif
119200131011      * F21 - Dirottamento
119210131011     c                   if        *in19 and not *in10 and
119220131011     c                             not *in26
119230131011     c                   reset                   $tf
119240131011     c                   movea     tf21          $tf
119250131011     c                   exsr      rie_$tf
119260131011     c                   endif
119270131011
119280131011      * Pulisco la stringa
119290131011     c                   Eval      $h = 1
119300131011     c                   For       $h by 1 to 62
119310131011     c                   If        rigatf1($h) = '#'
119320131011     c                   Clear                   rigatf1($h)
119330131011     c                   EndIf
119340131011     c                   EndFor
119350131011     c                   Eval      $h = 1
119360131011     c                   For       $h by 1 to 62
119370131011     c                   If        rigatf2($h) = '#'
119380131011     c                   Clear                   rigatf2($h)
119390131011     c                   EndIf
119400131011     c                   EndFor
119410131011     c                   Eval      $h = 1
119420131011     c                   For       $h by 1 to 62
119430131011     c                   If        rigatf3($h) = '#'
119440131011     c                   Clear                   rigatf3($h)
119450131011     c                   EndIf
119460131011     c                   EndFor
119470131011
119480131011      * Imposto la descrizione del tasto funzione F24 e segnalo
119490131011      * quante righe ho riempito e quale devo emettere
119500131011     c                   Select
119510131011
119520131011     c                   When      $k <> *Zeros and $y <> *Zeros
119530131011     c                   Eval      $loop = 3
119540131011     c                   Eval      vzfd02 = cf2413
119550131011
119560131011     c                   When      $k <> *Zeros and $y = *Zeros
119570131011     c                   Eval      $loop = 2
119580131011     c                   Eval      vzfd02 = cf2412
119590131011
119600131011     c                   Other
119610131011
119620131011     c                   Eval      $loop = 1
119630131011     c                   Clear                   vzfd02
119640131011     c                   EndSl
119650131011
119660131011      * Prima riga di tasti funzione
119670131011     c                   Movea     rigatf1       vzfd01
119680131011     c                   Eval      $rig = 1
119690131212
119700131212      /free
119710131212        *in80 = *off;
119720131212        *in83 = *off;
119730131212       //?Imposto tasto funzione in HI se presente dati
119740131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
119750131216             V1mtc > 0 or V1sto > 0 or V1ffd <> *blanks;
119760131212           *in80 = *on;
119770131212         ENDIF;
119780160513         IF  ((*in01 or *in03) and
119790160513              (w03sms <> *blanks or W03mail <> *blanks)) or
119800160513               walert;
119810131212           *in83 = *on;
119820131212         ENDIF;
119830131212      /end-free
119840131011
119850131011     c                   EndSr
119860140130
119870140130      /free
119880140130       //--------------------------------------------------------------------
119890140130       //?Abilitazione tasti funzione riga fissa.
119900140130       //--------------------------------------------------------------------
119910140130       BEGSR Abi_Fxx;
119920140130
119930140130         *in97 = *off;
119940140130       //?Abilito il tasto F17-Note AUT solo se sono in manutenzione
119950140227       //?e se NON è conferma proposta di variazione
119960140317           IF  *in02 and §RMfpr <> 'P';
119970140204             *in97 = *on;
119980140204           ENDIF;
119990140918
120000140918         *in98 = *off;
120010140918       //?Attivo F16 Chiusura ORM se
120020140918       //?non è copia/immissione ORM
120030140918       //?non è già chiusura ORM
120040140918       //?non siamo in conferma proposta di variazione
120050140918       //?se ORM non è in fase 420
120060140918         IF  not *in10 and not *in03 and not *in01 and not *in81 and
120070140918             not OrmFase420;
120080140918           *in98 = *on;
120090140918         ENDIF;
120100140130
120110140130       ENDSR;
120120140130      /end-free
120130131011
120140131011      *------------------------------------------------------------------------*
120150131011      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
120160131011      *------------------------------------------------------------------------*
120170131011     c     Rie_$tf       BegSr
120180131011      *
120190131011     c                   Eval      $x = 1
120200131011     c     '#'           Lookup    $tf($x)                                30
120210131011     c                   Add       $x            $z
120220131011     c                   If        $Z <= 63
120230131011     c                   Eval      $j = $Z - $x + 1
120240131011     c                   Movea     $tf           rigatf1($j)
120250131011     c                   Else
120260131011     c                   Add       $x            $k
120270131011     c                   If        $k <= 63
120280131011     c                   Eval      $j = $K - $x + 1
120290131011     c                   Movea     $tf           rigatf2($j)
120300131011     c                   Else
120310131011     c                   Add       $x            $y
120320131011     c                   If        $y <= 63
120330131011     c                   Eval      $j = $y - $x + 1
120340131011     c                   Movea     $tf           rigatf3($j)
120350131011     c                   EndIf
120360131011     c                   EndIf
120370131011     c                   EndIf
120380131011
120390131011     c                   EndSr
120400140422
120410140422      /free
120420140422       //--------------------------------------------------------------------
120430140422       //?Controllo se Alert o meno.
120440140422       //--------------------------------------------------------------------
120450140422       BEGSR AlertORM;
120460140422
120470140424         walert = *off;
120480140422         clear §OREfimo;
120490140422         clear §OREfiso;
120500140515
120510140515         exsr VerificaAlert;
120520140422
120530140515       //?Se OK alert, sono in conferma da VAO e
120540140515       //?ci sono i dati dell'alert li controllo
120550140515         IF  wOKalert and §RMfpr = 'C' and
120560140515            (w03mail <> *blanks or w03sms <> *blanks);
120570140424           exsr CtrDatiF04;
120580140424           IF  *in28;
120590140424             leavesr;
120600140424           ENDIF;
120610140424         ENDIF;
120620140422
120630140422       //?Imposto il flag ORM alert via SMS
120640140515         IF  w03sms <> *blanks;
120650140515           IF  not wOKalert;
120660140515             §OREfiso = 'N';
120670140515           ELSE;
120680140515             SELECT;
120690140422         //?Imposto in base al flag VAO alert via SMS
120700140422         //?per 'N' no alert
120710140423             WHEN  §OREfisv = 'N';
120720140423               §OREfiso = 'N';
120730140422         //?per 'X' alert solo se data ritiro > di oggi
120740140423             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
120750140423               §OREfiso = 'S';
120760140423             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
120770140423               §OREfiso = 'N';
120780140422         //?no 'N' e no 'X' alert calcolato
120790140423             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
120800140515               IF  inv_V1dar > oggi_aammgg;
120810140515                 §OREfiso = 'S';
120820140515               ELSE;
120830140515                 §OREfiso = 'N';
120840140515               ENDIF;
120850140515             ENDSL;
120860140515           ENDIF;
120870140422         ENDIF;
120880140422
120890140422       //?Imposto il flag ORM alert via mail
120900140515         IF  w03mail<> *blanks;
120910140515           IF  not wOKalert;
120920140515             §OREfimo = 'N';
120930140515           ELSE;
120940140515             SELECT;
120950140422         //?Imposto in base al flag VAO alert via mail
120960140422         //?per 'N' no alert
120970140507             WHEN  §OREfimv = 'N';
120980140423               §OREfimo = 'N';
120990140422         //?per 'X' alert solo se data ritiro > di oggi
121000140423             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
121010140423               §OREfimo = 'S';
121020140423             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
121030140423               §OREfimo = 'N';
121040140422         //?no 'N' e no 'X' alert calcolato
121050140423             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
121060140515               IF  inv_V1dar > oggi_aammgg;
121070140515                 §OREfimo = 'S';
121080140515               ELSE;
121090140515                 §OREfimo = 'N';
121100140515               ENDIF;
121110140515             ENDSL;
121120140515           ENDIF;
121130140422         ENDIF;
121140140422
121150140422       //?imposto flag per OK alert da mandare
121160140422         IF  §OREfiso = 'S' or §OREfimo = 'S';
121170140422           walert = *on;
121180140422         ENDIF;
121190140422
121200140422       ENDSR;
121210140515
121220140515       //--------------------------------------------------------------------
121230140515       //?Verifico se OK alert.
121240140515       //--------------------------------------------------------------------
121250140515       BEGSR VerificaAlert;
121260140515
121270140515         wOKalert = *off;
121280140515
121290140515         SELECT;
121300140515
121310140515       //?Se mittente codificato e filiale mittente ha ntw estero
121320140515       //?o mittente NON codificato con nazione estera
121330140515       //?NO alert e NON memorizzo i dati su FNORE
121340140515         WHEN  (ds_V1Ccra > 0 and
121350140515               (m_§OGntw = 'DPD' or m_§OGntw = 'EEX' or m_§OGntw = 'FED')) or
121360140515               (ds_V1Ccra = 0 and V1nar <> *blanks);
121370140515           clear w03sms;
121380140515           clear w03mail;
121390140515
121400140515         //?Se ORM NON commissionato NO alert
121410140515         WHEN  V1com <> 'S';
121420140515         //?Se ORM prepagato NO alert
121430140515         WHEN  V1Ctor = 'P';
121440140515         //?Se Ordinante non codificato NO alert
121450140515         WHEN  ds_V1Ccor = *zeros or
121460140515               ds_V1cor2 = 9999 or ds_V1cor2 = 8888;
121470140515         //?Controllo se peso/volume/bancali superano i limit massimi da DFT
121480140515         //?NO alert
121490140515         WHEN  (d§DFTpkga > 0 and V1pkg > d§DFTpkga) or
121500140515               (d§DFTvlma > 0 and V1vlm > d§DFTvlma) or
121510140515               (d§DFTbnca > 0 and V1bnc > d§DFTbnca);
121520140515         OTHER;
121530140515           wOKalert = *on;
121540140515         ENDSL;
121550140515
121560140515       ENDSR;
121570140505
121580140505       //--------------------------------------------------------------------
121590140505       //?Calcolo data ritiro Alert.
121600140505       //--------------------------------------------------------------------
121610140505       BEGSR DataRitiroAlert;
121620140515
121630140515       //?data ritiro video > oggi non faccio niente
121640140515         dataEUR = %date(V1dar:*eur);
121650140515         dataISO = dataeur;
121660140515         wdar = %dec(dataISO);
121670140515         IF  wdar > oggi_aammgg;
121680140515           leavesr;
121690140515         ENDIF;
121700140515
121710140515       //?se non ci sono i presupposti per inviare alert non faccio niente
121720140515         IF  not wOKalert;
121730140515           leavesr;
121740140515         ENDIF;
121750140515
121760140515       //?se i presupposti ci sono allora devo ricalcolare la data ritiro
121770140515       //?ma solo se i flag da VAO non sono 'X' o 'N'
121780140710         IF  (§OREfisv <> 'X' and §OREfisv <> 'N' and w03sms <> *blanks) or
121790140710             (§OREfimv <> 'X' and §OREfimv <> 'N' and w03mail <> *blanks);
121800140519           IF  V1Cpor > *zeros and num_V1Cpor = *zeros;
121810140519             num_v1Cpor = %dec(V1Cpor:3:0);
121820140519           ENDIF;
121830140527         //?se conferma ORM da VAO e NON ho ancora il POR lo prendo da O95lna
121840140529         //?se impostato
121850140527         //?capita perchè c'è un errore nel controllo del cap del mittente
121860140529           IF  §RMfpr = 'C' and num_V1Cpor = 0 and sav_O95lna > 0;
121870140527             num_V1Cpor = sav_O95lna;
121880140527           ENDIF;
121890140529         //?Calcolo la data solo se ho già la filiale ritiro
121900140529           IF  num_V1Cpor > 0;
121910140515           exsr sr_calpikup;
121920140515           v1dar = new_v1dar;
121930140515           dataEUR = %date(V1dar:*eur);
121940140515           dataISO = dataeur;
121950140515           inv_v1dar = %dec(dataISO);
121960140529           ENDIF;
121970140515         ENDIF;
121980140505
121990140505       ENDSR;
122000160406
122010160406       //--------------------------------------------------------------------
122020160406       //?Controllo se Conferma Prenotazione o meno.
122030160406       //--------------------------------------------------------------------
122040160406       BEGSR ConfermaORM;
122050160406
122060160406         clear §OREfmco;
122070160406         clear §OREfsco;
122080160406
122090160406       //?Se sono in conferma da VAO ed è stata richiesta la conferma
122100160406       //?controllo i dati
122110160406         IF  wConferma and §RMfpr = 'C' and
122120160406            (w04mail <> *blanks or w04sms <> *blanks);
122130160406           exsr CtrDatiF04;
122140160406           IF  *in28;
122150160406             leavesr;
122160160406           ENDIF;
122170160406         ENDIF;
122180160406
122190160406       //?Imposto il flag ORM alert via SMS
122200160406         IF  w03sms <> *blanks;
122210160406           IF  not wOKalert;
122220160406             §OREfiso = 'N';
122230160406           ELSE;
122240160406             SELECT;
122250160406         //?Imposto in base al flag VAO alert via SMS
122260160406         //?per 'N' no alert
122270160406             WHEN  §OREfisv = 'N';
122280160406               §OREfiso = 'N';
122290160406         //?per 'X' alert solo se data ritiro > di oggi
122300160406             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
122310160406               §OREfiso = 'S';
122320160406             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
122330160406               §OREfiso = 'N';
122340160406         //?no 'N' e no 'X' alert calcolato
122350160406             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
122360160406               IF  inv_V1dar > oggi_aammgg;
122370160406                 §OREfiso = 'S';
122380160406               ELSE;
122390160406                 §OREfiso = 'N';
122400160406               ENDIF;
122410160406             ENDSL;
122420160406           ENDIF;
122430160406         ENDIF;
122440160406
122450160406       //?Imposto il flag ORM alert via mail
122460160406         IF  w03mail<> *blanks;
122470160406           IF  not wOKalert;
122480160406             §OREfimo = 'N';
122490160406           ELSE;
122500160406             SELECT;
122510160406         //?Imposto in base al flag VAO alert via mail
122520160406         //?per 'N' no alert
122530160406             WHEN  §OREfimv = 'N';
122540160406               §OREfimo = 'N';
122550160406         //?per 'X' alert solo se data ritiro > di oggi
122560160406             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
122570160406               §OREfimo = 'S';
122580160406             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
122590160406               §OREfimo = 'N';
122600160406         //?no 'N' e no 'X' alert calcolato
122610160406             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
122620160406               IF  inv_V1dar > oggi_aammgg;
122630160406                 §OREfimo = 'S';
122640160406               ELSE;
122650160406                 §OREfimo = 'N';
122660160406               ENDIF;
122670160406             ENDSL;
122680160406           ENDIF;
122690160406         ENDIF;
122700160406
122710160406       //?imposto flag per OK alert da mandare
122720160406         IF  §OREfiso = 'S' or §OREfimo = 'S';
122730160406           walert = *on;
122740160406         ENDIF;
122750160406
122760160406       ENDSR;
122770140422
122780140422       //--------------------------------------------------------------------
122790140422       //?Scrivo file spia Alert.
122800140422       //--------------------------------------------------------------------
122810140422       BEGSR wrtspia;
122820140422
122830151008         IF  walert;
122840140422         clear FISAO000;
122850140422         SAOdaori = %timestamp();
122860140422         SAOpoe = ORMpoe;
122870140422         SAOnsr = ORMnsr;
122880140422         SAOnor = ORMnor;
122890140422         SAOnrv = ORMnrv;
122900140422         SAOtipa = 'AVV';
122910140422         SAOsts = '0';
122920140422         write FISAO000;
122930151008         ENDIF;
122940151008
122950151008         IF  wconferma;
122960151021           clear FISAO000;
122970151021           SAOdaori = %timestamp();
122980151021           SAOpoe = ORMpoe;
122990151021           SAOnsr = ORMnsr;
123000151021           SAOnor = ORMnor;
123010151021           SAOnrv = ORMnrv;
123020151021           SAOtipa = 'CON';
123030151021           SAOsts = '0';
123040151021           write FISAO000;
123050151008         ENDIF;
123060140422
123070140422       ENDSR;
123080140506
123090140506       //--------------------------------------------------------------------
123100140506       //?Imposto la frase per NON TELEFONARE
123110140506       //?se ORM commissionato con Alert
123120140506       //--------------------------------------------------------------------
123130140506       BEGSR CommissionatoAlert;
123140140506
123150140506         clear wtel01;
123160140506         clear wpos;
123170140506         wpos = (%len(wtel01) - %len(cCommAlert)) / 2;
123180140506         %subst(wtel01:wpos:%len(cCommAlert)) = cCommAlert;
123190140506
123200140506         clear wtel02;
123210140506         clear wpos;
123220140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123230140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123240140506
123250140506       ENDSR;
123260140506
123270140506       //--------------------------------------------------------------------
123280140506       //?Imposto la frase per NON TELEFONARE
123290140506       //?se ORM NON commissionato dirottato
123300140506       //--------------------------------------------------------------------
123310140506       BEGSR DirottaNoComm;
123320140506
123330140506         clear wtel01;
123340140506         clear wpos;
123350140506         wpos = (%len(wtel01) - %len(cDirottaNoComm)) / 2;
123360140506         %subst(wtel01:wpos:%len(cDirottaNoComm)) = cDirottaNoComm;
123370140506
123380140506         clear wtel02;
123390140506         clear wpos;
123400140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123410140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123420140506
123430140506       ENDSR;
123440140506
123450140506       //--------------------------------------------------------------------
123460140506       //?Imposto la frase per NON TELEFONARE
123470140506       //?se ORM Con preavviso via mail (CEVA) dirottato
123480140506       //--------------------------------------------------------------------
123490140506       BEGSR DirottaPreavviso;
123500140506
123510140506         clear wtel01;
123520140506         clear wpos;
123530140506         wpos = (%len(wtel01) - %len(cDirottaPreavv)) / 2;
123540140506         %subst(wtel01:wpos:%len(cDirottaPreavv)) = cDirottaPreavv;
123550140506
123560140506         clear wtel02;
123570140506         clear wpos;
123580140506         wpos = (%len(wtel02) - %len(cGiaAvvisato)) / 2;
123590140506         %subst(wtel02:wpos:%len(cGiaAvvisato)) = cGiaAvvisato;
123600140506
123610140506       ENDSR;
123620151106
123630160406       //--------------------------------------------------------------------
123640160406       //?Recupero i dati per Conferma Prenotazione da anagrafica clienti.
123650160406       //--------------------------------------------------------------------
123660160406       BEGSR RecDatiConf;
123670160406
123680160406         clear wCodAcre;
123690160406       //?dall'ordinante se presente
123700160406         IF  ds_V1Ccor > 0;
123710160406           wCodAcre = ds_V1Ccor;
123720160406         ELSE;
123730160406         //?dal mittente se ordinante non presente
123740160406           IF  V1rso = *blanks and ds_V1Ccra > 0;
123750160406             wCodAcre = ds_V1Ccra;
123760160406           ENDIF;
123770160406         ENDIF;
123780160406       //?Ho recuperato un codice quindi aggancio l'angrafica
123790160406          IF  wCodAcre = 0;
123800160406            leavesr;
123810160406          ENDIF;
123820160406          clear dACR01;
123830160406          chain(n) (wCodAcre) FNACR01L;
123840160406          IF  %found(FNACR01L);
123850160406            dACR01 = ACRmai;
123860160406          ENDIF;
123870160406         //?Se il cliente ha il flag di invio conferma
123880160406         //?imposto i dati a video
123890160406          IF  §ACRfmar <> 'S';
123900160406            leavesr;
123910160406          ENDIF;
123920160406          IF  w04mail = *blanks;
123930160406            chain(n) (wCodAcre:'MC') FNACRE1L;
123940160406            IF  %found(FNACRE1L);
123950160406              w04mail = ACREdati;
123960160406            ENDIF;
123970160406          ENDIF;
123980160406          IF  w04sms = *blanks;
123990160406            chain(n) (wCodAcre:'SC') FNACRE1L;
124000160406            IF  %found(FNACRE1L);
124010160406              w04sms = ACREdati;
124020160406            ENDIF;
124030160406          ENDIF;
124040160406
124050160406       ENDSR;
124060160406
124070151106       //--------------------------------------------------------------------
124080151106       //?Aggiorno Anagrafica clienti ritiro Ordinante x mail conferma.
124090151106       //--------------------------------------------------------------------
124100151106       BEGSR aggACR;
124110160505
124120160505       //?Se ORM con Ordinante YOOX non aggiorno
124130160505         IF  ORMcor = 17732000;
124140160505           leavesr;
124150160505         ENDIF;
124160151106
124170151109         chain(e) ORMcor FNACR01L;
124180161118         IF  not  %error;
124190151106         IF  %found(FNACR01L);
124200151109           dACR01 = ACRmai;
124210160322           IF  §OREfmco = 'S' or §OREfsco = 'S';
124220160322             §ACRfmar = 'S';
124230160322           ELSE;
124240151109             clear §ACRfmar;
124250160322           ENDIF;
124260151109           ACRmai = dACR01;
124270151109           update FNACR000;
124280151106         ENDIF;
124290161118         ENDIF;
124300151109
124310151109         chain(e) (ORMcor:'MC') FNACRE1L;
124320161118         IF  not %error;
124330151109         IF  %found(FNACRE1L);
124340160322           IF  sav_mailconf <> *blanks and §ACRfmar = 'S';
124350151109             ACREdati = sav_mailconf;
124360151109             update FNACRE00;
124370151109           ELSE;
124380151109             delete FNACRE00;
124390151109           ENDIF;
124400151109         ENDIF;
124410160112         IF  not %found(FNACRE1L) and sav_mailconf <> *blanks and
124420160322             §ACRfmar = 'S';
124430151109           clear FNACRE00;
124440151109           ACREcro = ORMcor;
124450151109           ACREtrc = 'MC';
124460151109           ACREdati = sav_mailconf;
124470151109           write FNACRE00;
124480151109         ENDIF;
124490161118         ENDIF;
124500160318
124510160318         chain(e) (ORMcor:'SC') FNACRE1L;
124520161118         IF  not %error;
124530160318         IF  %found(FNACRE1L);
124540160322           IF  sav_smsconf <> *blanks and §ACRfmar = 'S';
124550160318             ACREdati = sav_smsconf;
124560160318             update FNACRE00;
124570160318           ELSE;
124580160318             delete FNACRE00;
124590160318           ENDIF;
124600160318         ENDIF;
124610160318         IF  not %found(FNACRE1L) and sav_smsconf <> *blanks and
124620160322             §ACRfmar = 'S';
124630160318           clear FNACRE00;
124640160318           ACREcro = ORMcor;
124650160318           ACREtrc = 'SC';
124660160318           ACREdati = sav_smsconf;
124670160318           write FNACRE00;
124680160318         ENDIF;
124690161118         ENDIF;
124700160712
124710161118       //?Cerco il codice utente da memorizzare su FNACRE
124720160712         clear ORIidc;
124730160712         IF  §ORMpg > *zeros;
124740160712           ORIprg = %dec(§ORMpg:7:0);
124750160712           chain (ORIprg) TIORI01L;
124760160712         ENDIF;
124770161118
124780161118       //?Memorizzo chi ha fatto la richiesta
124790161118         clear dOREana;
124800161118         §OREtipo = 'I';
124810161118         §OREjob = §OREorip;
124820161118         §OREute = ORIidc;
124830161118         §OREdtv = %dec(%date());
124840161118         §OREorv = %dec(%time());
124850161118         chain(e) (ORMcor:'AA') FNACRE1L;
124860161118         IF  not %error;
124870161118           IF  %found(FNACRE1L);
124880161118             ACREdati = dOREana;
124890161118             update FNACRE00;
124900161118           ENDIF;
124910161118           IF  not %found(FNACRE1L);
124920161118             clear FNACRE00;
124930161118             ACREcro = ORMcor;
124940161118             ACREtrc = 'AA';
124950161118             ACREdati = dOREana;
124960161118             write FNACRE00;
124970161118           ENDIF;
124980161118         ENDIF;
124990151106
125000151106       ENDSR;
125010161115
125020161115       //--------------------------------------------------------------------
125030161115       //?Imposto i dati necessari alla visualizzazione orari servizio.
125040161115       //--------------------------------------------------------------------
125050161115       BEGSR DatiPerOrari;
125060161115
125070161115       //?Imposto la data per calcolo data ritiro = oggi
125080161115         DataCalcolo = Oggi_aammgg;
125090161115       //?Se data pronta merce > oggi la data calcolo è la data pronta merce
125100161115         IF  inv_V1dpm > 0 and inv_V1dpm > Oggi_aammgg;
125110161115           DataCalcolo = inv_V1dpm;
125120161115         ENDIF;
125130161115       //?Cerco giorno della settimana della data calcolo data ritiro
125140161115         dataiso = %date(DataCalcolo);
125150161115         clear wDay_Of_Week;
125160161115         exec sql
125170161115         set :wDay_Of_Week = dayofweek_iso(:dataiso);
125180161115
125190161115       //?Aggancio la tabella GPD con filiale ritiro ORM
125200161115         clear TIBS02ds;
125210161115         clear dGPD;
125220161115         T02mod = 'C';
125230161115         T02cod = 'GPD';
125240161115         T02sif = KNSIF;
125250161115         T02ke1 = %editc(num_V1cpor:'X');
125260161115         TNTBE_RicercaControllo (kpjba : tibs02ds);
125270161115         IF  T02err = *blanks;
125280161115           dGPD = T02uni;
125290161115         ELSE;
125300161115         //?Non trovo GPD con filiale ritiro
125310161115         //?la cerco con filiale generica 999
125320161115           clear TIBS02ds;
125330161115           T02mod = 'C';
125340161115           T02cod = 'GPD';
125350161115           T02sif = KNSIF;
125360161115           T02ke1 = '999';
125370161115           TNTBE_RicercaControllo (kpjba : tibs02ds);
125380161115           IF  T02err = *blanks;
125390161115             dGPD = T02uni;
125400161115           ENDIF;
125410161115         ENDIF;
125420161115
125430161115       ENDSR;
125440140422
125450140422      /end-free
125460131011
125470001010      *****************************************************************
125480001010      * ROUTINE INIZIALE
125490001010      *****************************************************************
125500001010     C     *INZSR        BEGSR
125510001009
125520001010     C     *ENTRY        PLIST
125530001010     C                   PARM                    KPJBA
125540001023     C                   PARM                    FIOR05DS
125550001011
125560001011     C                   movel     TIT_A         VTCtit
125570001009
125580150306     c                   IF        kpjbu <> *blanks
125590001026     C                   movel(p)  kpjbu         parm01
125600150306     c                   ELSE
125610150306     c                   clear                   parm01
125620150306     c                   ENDIF
125630001016
125640060203     C                   Z-ADD     1             CODUT             1 0
125650060203
125660060203      * Reperisco dati job
125670150713     c                   IF        §RMfpr <> 'K'
125680060203     c                   exsr      DatiJob
125690150713     c                   open      FIOR05D
125700150713     c                   ENDIF
125710150713
125720150713      /free
125730160713
125740160713       //?Determino la libreria
125750160713         IF  %subst(knsif : 7 : 1) = 'P';
125760160713           wLib = 'GAITRAGRPS';
125770160713         ELSE;
125780160713           wLib = 'GAITRAGRU';
125790160713         ENDIF;
125800160713       //?Apro i file
125810160713         wFLib = %trim(wLib) + '/TIORI01L';
125820160713         open  TIORI01L;
125830160713
125840150713       //?Se richiamato da pgm in ascolto (conferma automatica ORM da file/internet)
125850150713         IF  §RMfpr = 'K';
125860150713       //?imposto DUTpou = filiale emissione
125870150713           DUTpou = §RMpoe;
125880150713       //?carico la SKpog con la filiale emissione
125890150713           SKpog(1) = %editc(§RMpoe:'X');
125900150713       //?il flag di tipo richiamo diventa 'C'
125910150713           §RMfpr = 'C';
125920150713       //?Conferm automatica
125930150713           ConfAuto = *on;
125940150713       //?Utente batch
125950150713           knmus = 'BATCH';
125960150713         ENDIF;
125970150713      /end-free
125980001018
125990041014      * salvo il n. di distinta quando è proposta di chiusura
126000041014     c                   If        §rmtla = 'C' and §rmfpr = 'P' and
126010041014     c                             pndc > *Zeros
126020041014     c                   Eval      savndc = pndc
126030041014     c                   Eval      savddc = pddc
126040060516     c                   Eval      savfgs = pfvvfgs
126050041014     c                   EndIf
126060150304      * imposto il n. ORM quando è proposta variazione
126070001106     C                   if        §RMfpr = 'P'
126080001106     C                   eval      ppoe = §RMpoe
126090001106     C                   eval      pnsr = §RMnsr
126100001106     C                   eval      pnor = §RMnor
126110001106     C                   eval      pnrv = §RMnrv
126120001106     C                   endif
126130110420
126140110420      /free
126150110420       //?Imposto flag per arrivo da conferma proposta di variazione
126160110420       *in81 = *off;
126170110420       IF  §rmfpr = 'P';
126180110420         *in81 = *on;
126190110420       ENDIF;
126200110420      /end-free
126210001023
126220150304     C                   movel     DUTpou        kazorg
126230150310     C                   clear                   network
126240150612     C                   clear                   og143
126250001019     C     kazorg        chain     azorg01L
126260001016     C                   if        %found(azorg01l)
126270150612     C                   movel     orgde3        OG143
126280150612     c                   eval      network = §OGntw
126290001016     C                   endif
126300150609      * Carico £6
126310150609     C                   CLEAR                   trul06ds
126320150609     C                   MOVE      '£6'          D06COD
126330150609     C                   MOVEL     DUTpou        D06KEY
126340150609     C                   MOVEL     trul06ds      KPJBU
126350150609     C                   CALL      'TRUL06R'
126360150609     C                   PARM                    KPJBA
126370150609     C                   MOVEL     KPJBU         trul06ds
126380150609     C                   MOVEA     LIN           L6
126390061023
126400150305      * carico sk dei p.o. DPD
126410061023     c                   clear                   xx
126420061023     c     *loval        setll     azorg01l
126430061023     c                   do        *hival
126440061023     c                   read      azorg01l
126450061023     c                   if        %eof(azorg01l)
126460061023     c                   leave
126470061023     c                   endif
126480061023     c                   if        orgfva <> *blanks  or
126490061023     c                             (orgfag <> 'F' and orgfag <> 'A')
126500061023     c                   iter
126510061023     c                   endif
126520061023     c                   eval      og143 = orgde3
126530061023     c                   if        §ogntw = 'DPD'
126540061023     c                   add       1             xx
126550061023     c                   eval      skpodpd(xx) = orgfil
126560061023     c                   endif
126570061023     c                   enddo
126580081126
126590081126      * tabella 3I x DPD
126600081126     c                   clear                   ds3idp
126610081126     c                   eval      kcod = '3I'
126620081126     c                   eval      kkey = 'DPD'
126630081126     c     ktab          chain     tabel00f
126640081126     c                   if        %found(tabel00f)
126650081126     c                             and tblflg = *blanks
126660081126     c                   eval      ds3idp = tbluni
126670081126     c                   endif
126680001009
126690001023      * reperisce data e ora
126700001010     C                   TIME                    W0140
126710001010      * UDATE IN GGMMAAAA
126720010116     C                   MOVE      W0140         oggi_ggmmaa
126730001010      * UDATE IN AAAAMMGG
126740010124     C                   Z-ADD     oggi_ggmmaa   G02DAT
126750001010     C                   MOVEL     *BLANK        G02ERR
126760001010     C                   CALL      'XSRDA8'
126770001215     C                   PARM                    WLBDAT
126780010116     C                   Z-ADD     G02INV        oggi_aammgg
126790010907
126800010907     C                   movel     oggi_aammgg   oggi_anno
126810010907     C                   add       1             oggi_anno
126820001016
126830001009     C     Kfnor         klist
126840001011     C                   kfld                    kpoe
126850001016     C                   kfld                    knsr
126860001009     C                   kfld                    knor
126870001016     C                   kfld                    knrv
126880001109
126890001109     C     Kfnor1        klist
126900001109     C                   kfld                    kpoe
126910001109     C                   kfld                    knsr
126920001109     C                   kfld                    knor
126930001018
126940001026     C     Kfnorn        klist
126950001018     C                   kfld                    kpoe
126960001018     C                   kfld                    knsr
126970001018     C                   kfld                    knor
126980001018     C                   kfld                    knrv
126990001026     C                   kfld                    kdai
127000001026     C                   kfld                    kori
127010001018     C                   kfld                    kfar
127020001018     C                   kfld                    kprg
127030001026
127040001026     C     Kfnorn1       klist
127050001026     C                   kfld                    kpoe
127060001026     C                   kfld                    knsr
127070001026     C                   kfld                    knor
127080001026     C                   kfld                    knrv
127090001026     C                   kfld                    kdai
127100001026     C                   kfld                    kori
127110001026     C                   kfld                    kfar
127120070806
127130070806     c     kors          klist
127140070806     c                   kfld                    v1cpos
127150070806     c                   kfld                    v1ors
127160001009
127170010118     C     Ktab          klist
127180010119     C                   kfld                    codut
127190010118     C                   kfld                    kcod
127200010118     C                   kfld                    kkey
127210001010
127220001010     C     Ktntam        klist
127230001010     C                   kfld                    kksc
127240001010     C                   kfld                    kctr
127250010122
127260010122     C     Ktnta1        klist
127270010122     C                   kfld                    kksc
127280010122     C                   kfld                    kctr
127290010122     C                   kfld                    kprg
127300001215
127310001215     C     kazcln        klist
127320001215     C                   kfld                    ktfp
127330001215     C                   kfld                    ktfa
127340001215     C                   kfld                    kann
127350001215     C                   kfld                    kmes
127360001009
127370040426     c     kOrm08        klist
127380040426     c                   kfld                    ds_v1ccra
127390040426     c                   kfld                    inv_v1dar
127400040930
127410040930     c     kCnaco        klist
127420040930     c                   kfld                    codut
127430040930     c                   kfld                    kci
127440040930     c                   kfld                    kksc
127450070123
127460070301     c     kfidst        klist
127470070301     c                   kfld                    kdstnpg
127480080226     c                   kfld                    kdstnfv
127490080226     c                   kfld                    kdstfgs
127500071023
127510071023     c     kacr1         klist
127520071023     c                   kfld                    ds_v1ccra
127530071023     c                   kfld                    num_v1cpor
127540071031     c                   kfld                    kacr1ain
127550090206
127560090206     c     kfnora        klist
127570090206     c                   kfld                    ormpoe
127580090206     c                   kfld                    ormnsr
127590090206     c                   kfld                    ormnor
127600090206     c                   kfld                    ormnrv
127610090206     c                   kfld                    ormfao
127620090206     c                   kfld                    ormdfo
127630090206     c                   kfld                    ormofo
127640150615
127650150615     c                   eval      *in04 = *off
127660040426
127670001009     C                   select
127680010124      * Immissione
127690141212     C                   when      psce = '1' or psce = 'E'
127700001009     C                   movel     mod(1)        v1mod
127710001009     C                   eval      *in01 = *on
127720150615     c                   IF        §RMtla = *blanks
127730150615     c                   eval      *in04 = *on
127740150615     c                   ENDIF
127750010124      * Manutenzione
127760001009     C                   when      psce = '2'
127770001009     C                   movel     mod(2)        v1mod
127780001009     C                   eval      *in02 = *on
127790010124      * Copia
127800001109     C                   when      psce = '3'
127810001109     C                   movel     mod(1)        v1mod
127820010124     C                   eval      *in03 = *on
127830010124      * Conferma
127840010124     C                   when      psce = 'C'
127850090211     C                   movel     mod(7)        v1mod
127860010124     C                   eval      *in02 = *on
127870010124      * Richiamato x immissione
127880001025     C                   when      §RMtla <> *blanks
127890010124     C                             and §RMfpr <> 'P' and §RMfpr <> 'O'
127900001023     C                   movel     mod(1)        v1mod
127910001025     C                   eval      *in01 = *on
127920010124      * Richiamato x manutenzione
127930001106     C                   when      §RMtla <> *blanks
127940001106     C                             and §RMfpr = 'P'
127950001106     C                   movel     mod(2)        v1mod
127960001106     C                   eval      *in02 = *on
127970001009     C                   endsl
127980001009
127990001011     C                   eval      kpoe = ppoe
128000001016     C                   eval      knsr = pnsr
128010001011     C                   eval      knor = pnor
128020001016     C                   eval      knrv = pnrv
128030001011
128040001025     C                   eval      v1cpoe = ppoe
128050001016     C                   eval      v1nsr  = pnsr
128060001025     C                   eval      v1nor  = pnor
128070001025     C                   eval      v1nrv  = pnrv
128080150305
128090150305      * Recupero i dati di default prima con la filiale emissione
128100150305     C                   clear                   ddft
128110150305     C                   clear                   TIBS02DS
128120150305     C                   movel     'C'           T02mod
128130150305     C                   movel     knsif         t02sif
128140150305     C                   movel     'DFT'         t02cod
128150150305     C                   movel(p)  v1cpoe        T02ke1
128160150305     C                   call      'TIBS02R'
128170150305     C                   parm                    KPJBA
128180150305     C                   parm                    TIBS02DS
128190150305     C                   if        t02err =  *blanks
128200150305     C                   movel     t02uni        ddft
128210150305     C                   else
128220150305      * se non tropo provo con 999 generico
128230150305     C                   clear                   TIBS02DS
128240150305     C                   movel     'C'           T02mod
128250150305     C                   movel     knsif         t02sif
128260150305     C                   movel     'DFT'         t02cod
128270150305     C                   movel(p)  '999'         T02ke1
128280150305     C                   call      'TIBS02R'
128290150305     C                   parm                    KPJBA
128300150305     C                   parm                    TIBS02DS
128310150305     C                   if        t02err =  *blanks
128320150305     C                   movel     t02uni        ddft
128330150305     C                   endif
128340150305     C                   endif
128350150305
128360150305      * se i dati relativi al peso e volume sono a zero li imposto a di dft io
128370150305      * caso che non dovrebbe capitare ma se capita nella routine 'pesvol' il
128380150305      * programma si spacca
128390150305     c                   if        d§dftpkg = *zeros
128400150305     c                   eval      d§dftpkg = 200
128410150305     c                   endif
128420150305     c                   if        d§dftbnc = *zeros
128430150305     c                   eval      d§dftbnc = 1
128440150305     c                   endif
128450140318
128460140318      /free
128470140422       //?Carico i Tipi rcd FNORE che NON sono da copiare pari pari da VAOE
128480140422         clear dORE;
128490140422         clear xx;
128500140422         kTBEcod = 'ORE';
128510140422         setll kTBEcod tntbe01l;
128520140422         reade kTBEcod tntbe01l;
128530140422         DOW not %eof(tntbe01l);
128540140422           dORE = TBEUni;
128550140422           IF  §OREvar = 'S';
128560140422             xx += 1;
128570140423             skTRCvao(xx) = %subst(TBEke1:1:2);
128580140422           ENDIF;
128590140422           reade kTBEcod tntbe01l;
128600140422         ENDDO;
128610131022      /end-free
128620001006
128630001006     C                   endsr
128640060203
128650060203      *---------------------------------------------------------------*
128660060203      * Reperimento Dati del job (Utente/Operativi)                   *
128670060203      *---------------------------------------------------------------*
128680060203     c     DatiJob       BEGSR
128690060203      *
128700060203     c     *dtaara       define    §azute        azuteds
128710060203     c     *dtaara       define    §datiute      ddatiute
128720060203      *
128730060203     c                   in(E)     *dtaara
128740060203     c                   IF        %ERROR or RSUT = *blanks
128750060203     c                   clear                   Tibs34Ds
128760060203     c                   call      'TIBS34R'
128770060203     c                   parm                    Tibs34Ds
128780060203     c                   in        *dtaara
128790060203     c                   ENDIF
128800150305
128810150305      * abilitazione utente
128820150305     c                   clear                   wabi
128830150305     c                   eval      dUTE01 = UTEfaf
128840150305     c                   SELECT
128850150305      * carica abilitazioni del profilo
128860150305     c                   WHEN      §UTEorm <> *blanks
128870150305     c                   eval      wabi = §UTEorm
128880150305     c                   WHEN      UTEaut <> *blanks
128890150305     c                   eval      wabi = UTEaut
128900150305      * se non c'è l'abilitazione
128910150305      * --> se 1° livello, abilitazioni al terminal
128920150305      *     se 2° livello, abilitazioni al punto operativo
128930150305      *     se sede no abilitazioni
128940150305     c                   OTHER
128950150305     c                   IF        DUTlpo = '1'
128960150305     c                   eval      wabi   = 'TP'
128970150305     c                   ENDIF
128980150305     c                   IF        DUTlpo = '2'
128990150305     c                   eval      wabi   = 'PO'
129000150305     c                   ENDIF
129010150305     c                   ENDSL
129020150305
129030150305      * Reperimento delle filiali gestibili dall'utente
129040150305     c                   clear                   TRUL31DS
129050150305     c                   eval      I31abi = wabi
129060150305     c                   eval      I31cdi = DUTdis
129070150305     c                   eval      I31car = DUTare
129080150305     c                   eval      I31cpo = DUTpou
129090150305     c                   call      'TRUL31R'
129100150305     c                   parm                    KPJBA
129110150305     c                   parm                    TRUL31DS
129120150305     c                   IF        O31pog > *zeros
129130150305     c                   movea     O31pog        SKpog
129140150305     c                   ENDIF
129150060203      *
129160060203     c                   ENDSR
129170001010      *---------------------------------------------------------------------------------------------
129180001010** MSG  Lungh. 78                                                            *
129190010420Inserire mittente                                                                   1
129200010420Codice mittente non valido                                                          2
129210010213Ordinante non valido                                                                3
129220001010Immettere i colli                                                                   4
129230001010Immettere il peso                                                                   5
129240001010Immettere il volume                                                                 6
129250001010Immettere i bancali                                                                 7
129260001010Immettere il bilico                                                                 8
129270001010Immettere l'autotreno                                                               9
129280001010Immettere la motrice                                                                10
129290070418Inserire Filiale di ritiro                                                          11
129300131016Ora pronta merce maggiore dell'orario di fine servizio                              12
129310001010Tipo ordine non valido                                                              13
129320010202Immettere la cusale di chiusura                                                     14
129330010202Causale chiusura errata                                                             15
129340001010Codice tassazione o codice tariffa errati                                           16
129350070418Filiale consegna errata                                                             17
129360010118Destinatario non valido                                                             18
129370001010Attenzione !! Modificata Località                                                   19
129380001010Attenzione !! Modificato CAP                                                        20
129390001010Attenzione !! Modificata Provincia                                                  21
129400001010Attenzione !! Modificata Nazione                                                    22
129410030609Pagamento a carico dell'ordinante...inserire un codice valido                       23
129420001012Ora errata                                                                          24
129430001012Immettere i colli ------> ENTER x forzare                                           25
129440001012Immettere il peso ------> ENTER x forzare                                           26
129450001012Immettere il volume ------> ENTER x forzare                                         27
129460001012Immettere i bancali ------> ENTER x forzare                                         28
129470001012Immettere il bilico ------> ENTER x forzare                                         29
129480001012Immettere l'autotreno ------> ENTER x forzare                                       30
129490001012Immettere la motrice ------> ENTER x forzare                                        31
129500131016Ritiro no in giornata, superato l'orario di fine servizio                           32
129510070418Filiale ritiro errata                                                               33
129520001016Immettere data ritiro                                                               34
129530001016Data ritiro errata                                                                  35
129540070418La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica        36
129550010205Immettere almeno una quantità (Kg. Volume Bancali)                                  37
129560131107ORM multiplo non possibile se presente il destinatario                              38
129570010118Dati del destinatario incompleti                                                    39
129580010420Non è possibile variare il mittente                                                 40
129590070130ATTENZIONE!! Già avuto esito da PDA. Enter x forzare                                41
129600001103Non è stato modificato nulla                                                        42
129610070913Giro Ritiro errato                                                                  43
129620001215Data chiamata errata                                                                44
129630001215Data chiamata obbligatoria                                                          45
129640131107Orari di apertura incongruenti                                                      46
129650001215Ora chiamata obbligatoria                                                           47
129660010213ORM prepagato non possibile, inserire prima il prepagato!                           48
129670081007ORM IMPORT DPD automatico, non si può chiudere con causale "80"                     49
129680010223Immettere destinatario se ORM prepagato                                             50
129690161108Data ritiro immessa inferiore a quella calcolata.                                   51
129700010126Immettere l'ora del ritiro                                                          52
129710010202Priorità non trovata in tabella                                                     53
129720070913Per ORM prepagato immettere colli e peso                                            54
129730140424Immettere un indirizzo e-mail valido                                                55
129740010420Per ORM prepagato deve pagare il mittente                                           56
129750010215Inserire il referente                                                               57
129760010215Inserire il telefono                                                                58
129770040630Non immettere il codice tariffa se non immesso il codice cliente                    59
129780140424Immettere un numero di telefono valido                                              60
129790010223Immettere il destinatario. Orm singolo con pagamento a carico destinatario          61
129800070418ATTENZIONE!! Filiale ritiro diversa da quella calcolata                             62
129810010906ATTENZIONE!! data ritiro ricalcolata!                                               63
129820030806Causale chiusura non gestibile                                                      64
129830010305Data ritiro inferiore alla data immissione ORM                                      65
129840170303ATTENZIONE!! La data ritiro immessa è un giorno festivo                             66
129850010413ATTENZIONE!! fare F6 x confermare                                                   67
129860010906ATTENZIONE!! Volume troppo alto. Ricalcolare.                                       68
129870010502ATTENZIONE!! Mittente BLOCCATO                                                      69
129880070921ATTENZIONE!! Modificato codice mittente e/o codice cliente tariffa                  70
129890010507ATTENZIONE!! Numeratore ORM impegnato riprovare                                     71
129900010508ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                            72
129910140424Messaggi in sospeso su altre videate                                                73
129920011114Richiesta forzatura del destinatario ma l'ORM non è singolo                         74
129930011114Richiesta forzatura del destinatario ma i dati sono incompleti o errati             75
129940030808Se Mittente e Ordinante sono uguali impostare che paga il Mittente                  76
129950040419Non è possibile dirottare l'ORM                                                     77
129960070418Modificare la filiale ritiro                                                        78
129970070418La filiale ritiro non ha la proc.ORM attiva                                         79
129980040628ORM xxx-xx-xxxxxxx-xx da ritirare lo stesso giorno per lo stesso mittente           80
129990131202ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!        81
130000040727ATTENZIONE!! Ordinante BLOCCATO                                                     82
130010040727ATTENZIONE!! Destinatario BLOCCATO                                                  83
130020050120Pagamento a carico del destinatario...inserire un codice valido                     84
130030050128ATTENZIONE!! Le note immesse sono state sovrascritte                                85
130040050323ATTENZIONE!! Non esiste legame tra il mittente e l'ordinante. VERIFICARE!!!         86
130050050324ATTENZIONE!! ORM PREPAGATO non locale deve essere un ORM commissionato              87
130060050420Prima confermare le proposte di variazione                                          88
130070070418ATTENZIONE!! Filiale ritiro non gestibile                                           89
130080080422ATTENZIONE!! Per ORM commissionati a DPD il ritiro deve essere di 1 collo           90
130090140331Immettere la natura della merce.                                                    91
130100081126ATTENZIONE!! ORM Export DPD peso maggiore del limite consentito                     92
130110081126ATTENZIONE!! ORM Export DPD destinatario obbligatorio                               93
130120110120ATTENZIONE!! Presenti incongurenze fra Cap, Prov, Loc: modificare l'anagrafica      94
130130110610Per ORM Export EEX non è possibile la consegna all'estero                           95
130140110610Per ORM Export DPD non è possibile la consegna a                                    96
130150150615Tipo Comunicazione ORM non valido.                                                  97
130160160329ATTENZIONE!! Il giro è stato variato a seguito delle modifiche effettuate           98
130170010508** MOD  Lungh. 15                                                            *
130180001013  IMMISSIONE                                                                        1
130190001013 MANUTENZIONE                                                                       2
130200001009VISUALIZZAZIONE                                                                     3
130210001013   ANNULLATO                                                                        4
130220001106   CHIUSURA                                                                         5
130230040628 DIROTTAMENTO                                                                       6
130240090211   CONFERMA                                                                         7
130250010704** CM3
130260030611OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('ETICH     ')
130270160229 ovrscope(*calllvl) SHARE(*YES)
130280030623** cmA4
130290030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130300030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130310030623**  cmA5
130320030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130330030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130340131011** TF02
130350131211F2=Dati DPD  #
130360131014** TF06
130370131211F6=Conferma  #
130380131011** TF11
130390131211F11=Volume  #
130400160405** TF13
130410160405F13=Conf.Prenotazione  #
130420131011** TF14
130430131211F14=OrariSr  #
130440131011** TF16
130450131211F16=Chiusura  #
130460131011** TF19
130470131211F19=Proposte  #
130480131011** TF20
130490131211F20=Simulazione  #
130500131011** TF21
130510131211F21=Dirottamento  #
