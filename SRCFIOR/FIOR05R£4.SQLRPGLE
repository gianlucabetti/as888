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
056450170616       //?ma solo se tipo comunicazione da FILE, da internet facciamo anticipare
056460170616         IF  §RMfpr = 'C' and §RMtco = 'F' and
056470170616             inv_newv1dar < inv_v1dpm;
056480161026           old_invV1dar = inv_v1dpm;
056490161026           inv_newV1dar = inv_v1dpm;
056500161026           new_V1dar = v1dpm;
056510170524           clear v1anticipa;
056520161026         ENDIF;
056530130910
056540130910      /end-free
056550130910
056560040615     c                   EndSr
056570001017      **********************************************************************
056580001017      * CONTROLLI     * ora ritiro *
056590001017      **********************************************************************
056600001017     C     Sr_Controrr   begsr
056610001017
056620010117     C                   if        v1orr <> *zeros
056630010117     C                   movel     v1orr         ore
056640010117     C                   move      v1orr         minuti
056650001017
056660071107     C                   if        ore > 23 or minuti > 59
056670010220     C                             or ore < 1
056680001017     C                   seton                                        58  28
056690001017     C                   movel     msg(24)       v1cmsg
056700130917     c                   leavesr
056710001017     C                   endif
056720131111     C                   ELSE
056730140320
056740140320      /free
056750131106       //?Se immissione/copia ORM commissionato non c'è obbligo dell'ora
056760131106         IF  (*in01 or *in03) and V1com = 'S';
056770131106         ELSE;
056780131106           *in28 = *on;
056790131106           *in58 = *on;
056800131106           V1Cmsg = msg(52);
056810131106           leavesr;
056820131106         ENDIF;
056830131106      /end-free
056840131111
056850131111     c                   ENDIF
056860001017
056870001017     C                   endsr
056880001103      **********************************************************************
056890070913      * CONTROLLI     * priorità
056900001103      **********************************************************************
056910001103     C     Sr_Contrsto   begsr
056920001103
056930010123     C                   clear                   v1dsto
056940010123
056950010123      * Decodifica priorità ORM
056960010202     C                   if        v1sto <> *zeros
056970010202     C                   clear                   TIBS02DS
056980010202     C                   move      *all'0'       priorita
056990010202     C                   movel     'C'           T02mod
057000010202     C                   movel     knsif         t02sif
057010010202     C                   movel     'STO'         t02cod
057020010202     C                   move      v1sto         priorita
057030010202     C                   movel(p)  priorita      T02ke1
057040010202     C                   call      'TIBS02R'
057050010202     C                   parm                    KPJBA
057060010202     C                   parm                    TIBS02DS
057070010202     C                   if        t02err <> *blanks
057080131030     C                   movel     msg(53)       w2cmsg
057090010202     C                   seton                                        84  28
057100010202     C                   goto      endsrsto
057110010202     C                   endif
057120010202     C                   movel     t02ke1        priorita
057130010202     C                   move      priorita      v1sto
057140010202     C                   movel     t02uni        v1dsto
057150010202     C                   endif
057160001103
057170001103     C     endsrsto      endsr
057180010223      **********************************************************************
057190010223      * CONTROLLI     * Referente e telefono *
057200010223      **********************************************************************
057210010223     C     Sr_Contrref   begsr
057220010223
057230131106      * Per orm prepagato obbligatori tel. e ref.
057240131106     C                   if        v1ctor = 'P'
057250030610      * anche per ORM commissionati
057260030610     c                             or V1com = 'S'
057270010223     C                   if        v1rer = *blanks
057280010223     C                   movel     msg(57)       v1cmsg
057290010223     C                   seton                                        86  28
057300010223     C                   goto      endsrcref
057310010223     C                   endif
057320010223     C                   if        v1ter = *blanks
057330010223     C                   movel     msg(58)       v1cmsg
057340010223     C                   seton                                        87  28
057350010223     C                   goto      endsrcref
057360010223     C                   endif
057370010223     C                   endif
057380010223
057390010223     C     endsrcref     endsr
057400010202      **********************************************************************
057410010202      * CONTROLLI     * pagamento *
057420010202      **********************************************************************
057430010202     C     Sr_Contrpag   begsr
057440010202
057450010202      * Se non inserito prendo di dft
057460010202     C                   if        v1pag = *blanks
057470010202     C                   eval      v1pag = d§dftpag
057480010308     C                   eval      sav_v1pag = d§dftpag
057490010514     C                   endif
057500070912      * controllo che sia valido
057510070912      * nel caso di dato sbagliato che arriva da FNAVO e non viene toccato il campo
057520070912     c                   if        v1pag <> 'M' and v1pag <> 'O' and
057530070912     c                             v1pag <> 'D'
057540070912     c                   eval      v1cmsg = 'Pagamento errato'
057550070912     c                   eval      *in28 = *on
057560070912     c                   eval      *in53 = *on
057570070912     c                   leavesr
057580070912     c                   endif
057590011113      * Se prepagato obbligatorio che paghi il mittente
057600081216      * se non è un ritiro all'estero
057610010213     C                   if        v1ctor = 'P' and v1pag <> 'M'
057620081216     c                             and v1nar = *blanks
057630010213     C                   movel     msg(56)       v1cmsg
057640010213     C                   seton                                        53  28
057650010213     C                   goto      endsrcpag
057660010213     C                   endif
057670010223      * Se singolo e paga il destinatario quest'ultimo è obbligatorio
057680010223     C                   if        v1ctor = 'S' and v1pag = 'D'
057690010223     C                   eval      ds_v1crc1 = v1crc1
057700010223     C                   eval      ds_v1crc2 = v1crc2
057710010223     C                   eval      ds_v1crc3 = v1crc3
057720010223     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
057730010223     C                   movel     msg(61)       v1cmsg
057740010223     C                   seton                                        56  28
057750010223     C                   goto      endsrcpag
057760010223     C                   endif
057770050121      * il codice destinatario non può essere un 8888 o 9999
057780050121     C                   If        ds_v1ccrc > *zeros and
057790050121     c                             (ds_v1crc2 = 9999 or ds_v1crc2 = 8888)
057800050120     C                   movel     msg(84)       v1cmsg
057810050120     C                   seton                                        56  28
057820050120     C                   goto      endsrcpag
057830050120     C                   endif
057840010223     C                   endif
057850030808      * Se inserito che paga l'ordinante
057860030808      * l'ordinante deve essere codificato e non può esserre un 8888 o 9999
057870030808      * se mittente e ordinante sono uguali obbligatorio che paghi mittente
057880010202     C                   eval      ds_v1cor1 = v1cor1
057890010202     C                   eval      ds_v1cor2 = v1cor2
057900010202     C                   eval      ds_v1cor3 = v1cor3
057910030808     c                   Eval      ds_V1Cra1 = V1Cra1
057920030808     c                   eval      ds_V1Cra2 = V1Cra2
057930030808     c                   eval      ds_V1Cra3 = V1Cra3
057940030808     C                   if        v1pag = 'O'
057950030808     C                   If        (ds_v1ccor = *zeros or
057960030606     c                             (ds_v1cor2 = 9999 or ds_v1cor2 = 8888))
057970010202     C                   movel     msg(23)       v1cmsg
057980010202     C                   seton                                        42  28
057990010202     C                   goto      endsrcpag
058000010202     C                   endif
058010030808     c                   EndIF
058020010507
058030010507     C  nkf              eval      sav_v1ksc = v1ksc
058040010202
058050010202     C     endsrcpag     endsr
058060010202      **********************************************************************
058070010202      * CONTROLLI     * cod.cliente e tariffa *
058080010202      **********************************************************************
058090010202     C     Sr_Contrksc   begsr
058100010202
058110040930      * Controllo se il codice cliente esiste su CNACO
058120040930if  1c                   If        v1ksc <> *Zeros
058130040930     c                   z-add     v1ksc         kksc
058140040930     c     kCnaco        Chain     Cnaco00f
058150040930if  2c                   If        Not %Found(Cnaco00f) or
058160040930     c                             AcoFlg <> *Blanks
058170040930     c                   Eval      v1cmsg = msg(16)
058180040930     c                   Seton                                        54  28
058190040930     c                   Goto      endsrcksc
058200040930    2c                   EndIf
058210040930      * Controllo il network del p.o. del cliente non deve essere LED o SEDE
058220040930     c                   Movel     v1ksc         kazorg
058230040930     c                   Clear                   Og143
058240040930     c     kazorg        Chain     Azorg01l
058250040930     c                   If        %Found(Azorg01l)
058260040930     c                   Eval      Og143 = Orgde3
058270040930     c                   EndIf
058280040930if  1c                   If        §OgNtw = 'LOG' or §OgNtw = 'XXX'
058290040930     c                   Eval      v1cmsg = msg(16)
058300040930     c                   Seton                                        54  28
058310040930     c                   Goto      endsrcksc
058320040930e   2c                   EndIf
058330040930e   1c                   EndIf
058340010202      * Se codice tariffa inserito deve essere un numero
058350010202     C                   if        v1ctr <> *blanks  and
058360010320     C                             (%subst(v1ctr:1:1) < '0'  and
058370010320     C                              %subst(v1ctr:1:1) <> ' ') or
058380010320     C                             (%subst(v1ctr:2:1) < '0'  and
058390010320     C                              %subst(v1ctr:2:1) <> ' ') or
058400010320     C                             (%subst(v1ctr:3:1) < '0'   and
058410010320     C                              %subst(v1ctr:3:1) <> ' ')
058420010202     C                   movel     msg(16)       v1cmsg
058430010202     C                   seton                                        85  28
058440010202     C                   goto      endsrcksc
058450010202     C                   endif
058460170728         IF  V1ctr <> *blanks and
058470170728             (%subst(v1ctr:1:1) = ' ' or %subst(V1ctr:2:1) = ' ' or
058480170728              %subst(V1ctr:3:1) = ' ');
058490170728           V1Cmsg = msg(16);
058500170728           *in28 = *on;
058510170728           *in85 = *on;
058520170728           leavesr;
058530170728         ENDIF;
058540040630      * non posso inserire solo la tariffa senza il codice cliente
058550040630     C                   if        v1ksc = *zeros and v1ctr <> *blanks
058560040630     C                   movel     msg(59)       v1cmsg
058570040630     C                   seton                                        85  28
058580040630     C                   goto      endsrcksc
058590040630     C                   endif
058600010202      * Controlla validità codice + tariffa
058610010202     C                   if        v1ksc <> *zeros and v1ctr <> *blanks
058620010202     C                   z-add     v1ksc         kksc
058630010202     C                   movel     v1ctr         kctr
058640090128     c                   eval      $notam = *off
058650090128     c     ktntam        setll     tntam01l
058660090128      * se non trovo errore
058670090128     c                   if        not %equal(tntam01l)
058680090128     c                   eval      v1cmsg = msg(16)
058690090128     c                   seton                                        54  28
058700090128     c                   leavesr
058710090128     c                   endif
058720090128     c                   do        *hival
058730090128     c     ktntam        Reade     Tntam01l
058740040713     C                   if        %Eof(tntam01l)
058750090128     C                   leavesr
058760010202     C                   endif
058770090128      * se annullato leggo altro record
058780010202     C                   if        TAMatb <> *blanks
058790090128     C                   iter
058800010202     C                   endif
058810090128      * trovato almeno 1 rcd ok
058820090128     c                   eval      $notam = *on
058830090128     c                   enddo
058840090128      * se non ho trovato nessuna tariffa errore
058850090128     c                   if        not $notam
058860090128     c                   eval      v1cmsg = msg(16)
058870090128     c                   seton                                        54  28
058880090128     c                   leavesr
058890090128     c                   endif
058900010202     C                   endif
058910010202
058920010202     C     endsrcksc     endsr
058930010223      **********************************************************************
058940010223      * CONTROLLI     * flag ddt *
058950010223      **********************************************************************
058960010223     C     Sr_Contrddt   begsr
058970010223
058980021119      * Da default immissione ORM
058990010223     C                   if        v1ddt = *blanks
059000010223     C                   eval      v1ddt = d§dftddt
059010010223     C                   endif
059020010223
059030010223     C                   endsr
059040010202      **********************************************************************
059050010202      * CONTROLLI     * P.O. consegna *
059060010202      **********************************************************************
059070010202     C     Sr_Contrpoc   begsr
059080010202
059090010320     C                   clear                   v1dpoc
059100060929     c                   clear                   c_og143
059110010320
059120010202     C                   if        v1cpoc <> *blanks
059130010202      * Ricerca P.O.Consegna con "?"
059140010202     C     '?'           scan      v1cpoc                                 30
059150010202     C     *in30         ifeq      *on
059160010202     C                   eval      *in55 = *on
059170010202     C                   eval      *in90 = *on
059180010202     C                   clear                   ds_tnsd
059190010202     C                   call      'TNSD24R'
059200010202     C                   parm                    ds_cod
059210010202     C                   parm                    ds_tip
059220010202     C                   parm                    ds_des
059230010202     C                   parm                    ds_pra
059240010202     C                   movel     ds_cod        v1cpoc
059250010202     C                   movel     ds_des        v1dpoc
059260010202     C                   endif
059270010316     C                   endif
059280010316      * Solo campi numerico
059290010320     C                   if        v1cpoc <> *blanks and
059300010320     C                             (%subst(v1cpoc:1:1) < '0' and
059310010320     C                              %subst(v1cpoc:1:1) <> ' ') or
059320010320     C                             (%subst(v1cpoc:2:1) < '0' and
059330010320     C                              %subst(v1cpoc:2:1) <> ' ') or
059340010320     C                             (%subst(v1cpoc:3:1) < '0' and
059350010320     C                              %subst(v1cpoc:3:1) <> ' ')
059360010316     C                   seton                                        55  28
059370010316     C                   movel     msg(17)       v1cmsg
059380010316     C                   goto      endsrcpoc
059390010316     C                   endif
059400010316      * Controllo
059410010202     C                   if        v1cpoc <> *blanks
059420010202     C                   movel     v1cpoc        kazorg
059430010202     C     kazorg        chain     azorg01L
059440010316     C                   if        not%found(azorg01l)
059450010316     C                   movel     msg(17)       v1cmsg
059460010316     C                   seton                                        55  28
059470010316     C                   goto      endsrcpoc
059480010316     C                   endif
059490010202     C                   if        ORGfva <> *blanks or
059500010202     C                             (ORGfag <> 'A' and ORGfag <> 'F')
059510010202     C                   movel     msg(17)       v1cmsg
059520010202     C                   seton                                        55  28
059530010202     C                   goto      endsrcpoc
059540010202     C                   endif
059550010202     C                   movel     ORGdes        v1dpoc
059560060929     c                   eval      c_og143 = orgde3
059570010202     C                   endif
059580111221
059590111221      /free
059600111221       //?Forzo la filiale di consegna in base alla tabella FFC
059610120116       //?se il destinatario è estero
059620150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
059630120116       IF  V1nac <> *blanks;
059640120116         IF  V1Cpoe = *zeros;
059650150305           w_V1Cpoe = DUTpou;
059660120116         ELSE;
059670120116           w_V1Cpoe = V1Cpoe;
059680120116         ENDIF;
059690120116         //?Provo con la filiale dell'ordinante se presente
059700120116         IF  ds_V1Ccor > 0;
059710120116           clear TIBS02ds;
059720120116           clear dFFC;
059730120116           T02mod = 'C';
059740120116           T02cod = 'FFC';
059750120116           T02ke1 = %editc(ds_V1Cor1:'X');
059760120116           T02sif = KNSIF;
059770120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
059780120116           IF  T02err = *blanks;
059790120116             dFFC = T02uni;
059800120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
059810120116               V1Cpoc = %editc(§FFCpoc:'X');
059820120116             ENDIF;
059830120116             leavesr;
059840120116           ENDIF;
059850120116         ENDIF;
059860120116         //?Se non trovo con Filiale Ordinante provo con Filiale Emissione
059870120116         //?se diversa da Filiale Ordinante
059880120116         IF  w_V1Cpoe <> ds_V1Cor1;
059890120116           clear TIBS02ds;
059900120116           clear dFFC;
059910120116           T02mod = 'C';
059920120116           T02cod = 'FFC';
059930120116           T02ke1 = %editc(w_V1Cpoe:'X');
059940120116           T02sif = KNSIF;
059950120116           TNTBE_RicercaControllo (kpjba : tibs02ds);
059960120116           IF  T02err = *blanks;
059970120116             dFFC = T02uni;
059980120116             IF  oggi_aammgg >= §FFCdti and oggi_aammgg <= §FFCdtf;
059990120116               V1Cpoc = %editc(§FFCpoc:'X');
060000120116             ENDIF;
060010120116             leavesr;
060020120116           ENDIF;
060030120116         ENDIF;
060040111221       ENDIF;
060050111221      /end-free
060060010202
060070010202     C     endsrcpoc     endsr
060080010202      **********************************************************************
060090010202      * CONTROLLI     * cliente destinatario *
060100010202      **********************************************************************
060110010202     C     Sr_Contrclid  begsr
060120110610
060130110610     c                   eval      $cacgdd = *off
060140010202
060150010202     C                   eval      ds_v1crc1 = v1crc1
060160010202     C                   eval      ds_v1crc2 = v1crc2
060170010202     C                   eval      ds_v1crc3 = v1crc3
060180010223
060190010223      * Se orm prepagato deve esserci il destinatario
060200010223     C                   if        v1ctor = 'P' and
060210010223     C                             (ds_v1ccrc = *zeros and v1rsc = *blanks)
060220010223     C                   movel     msg(50)       v1cmsg
060230010223     C                   seton                                        56  28
060240010223     C                   goto      endsrclid
060250010223     C                   endif
060260131107      /free
060270131107       //?Se sono in immissione/copia e sto inserendo il destinatario
060280131107       //?se il tipo ORM è impostato a Multiplo lo aggiorno in automatico
060290131107       //?in Singolo in modo da non dare il messaggio di errore
060300131107         IF  (*in01 or *in03) and
060310131107             ((sav_V1Ccrc <> ds_V1Ccrc and sav_V1Ccrc = 0) or
060320131107             (sav_V1rsc <> V1rsc and sav_V1rsc = *blanks)) and
060330131107             V1Ctor = 'M';
060340131107           V1Ctor = 'S';
060350131107         ENDIF;
060360131107      /end-free
060370131107
060380010223      * Se orm multiplo non deve esserci il destinatario
060390010223     C                   if        v1ctor = 'M' and
060400010223     C                             (ds_v1ccrc > *zeros or v1rsc <> *blanks)
060410010223     C                   movel     msg(38)       v1cmsg
060420160316     c                   eval      H1riga = 05
060430160316     c                   eval      H1colo = 11
060440160316     C                   seton                                            28
060450011114     C                   setoff                                       0616
060460010223     C                   goto      endsrclid
060470010223     C                   endif
060480011114
060490011114      * Se blocco destinatario l'ORM deve essere singolo e i dati del
060500011114      * destinatario devono essere completi
060510011114     C                   if        §ormfd = 'S'
060520011114     C                   if        v1ctor <> 'S' and *in38 = *off
060530011114     C                   movel     msg(74)       v1cmsg
060540160316     c                   eval      H1riga = 05
060550160316     c                   eval      H1colo = 11
060560160316     C                   seton                                          3828
060570011114     C                   setoff                                       0616
060580011114     C                   goto      endsrclid
060590011114     C                   endif
060600011114     C                   if        ds_v1ccrc = *zeros and v1rsc = *blanks
060610011114     C                             and *in39 = *off
060620011114     C                   movel     msg(75)       v1cmsg
060630011114     C                   seton                                        563928
060640011114     C                   setoff                                       0616
060650011114     C                   goto      endsrclid
060660011114     C                   endif
060670011114     C                   endif
060680010223
060690010202      * Inserito codice
060700010202     C                   if        ds_v1ccrc <> *zeros
060710010202     C                   eval      *in06 = *on
060720010202     C                   exsr      Sr_Contrcrc
060730010202     C                   eval      sav_v1ccrc = ds_v1ccrc
060740010202     C                   else
060750010202      * Inserita ragione sociale
060760010202     C                   clear                   sav_v1ccrc
060770010202     C                   exsr      Sr_Contrrsc
060780131107     c                   eval      sav_V1rsc = V1rsc
060790010202     C                   eval      *in06 = *off
060800010202     C                   endif
060810110120     c   28              leavesr
060820011119      * Se ORM con blocco destinatario faccio già i controlli
060830011119     C                   if        §ormfd = 'S' and
060840011119     C                             (ds_v1ccrc <> *zeros or v1rsc <> *blanks)
060850011119     C                   eval      *in16 = *on
060860011119     C                   endif
060870060929
060880060929      * se oltre al destinatario è stato inserito anche il p.o. di consegna
060890060929      * devo fare un ulteriore controllo
060900060929      * ai fini dell'orm non serve, ma serve per impedire l'instradamento sbagliato
060910060929      * al momento della creazione della bolla legata a questo ORM
060920110120     c                   if        v1cpoc<> *blank and v1rsc <> *blanks
060930060929     c                   exsr      sr_contrcapc
060940060929     c                   endif
060950100811
060960100811      /free
060970100811       //?Se ORM import DPD devo controllare che la nazione di destino sia
060980100907       //?servita da DPD --> chiodo DPD come ntw.
060990150305       //?Se la Filiale Emissione non è ancora stata impostata uso la Filiale utente
061000130107         IF  V1Cpoe = *zeros;
061010150305           w_V1Cpoe = DUTpou;
061020130107         ELSE;
061030130107           w_V1Cpoe = V1Cpoe;
061040130107         ENDIF;
061050130107         IF  %lookup(w_V1Cpoe:skpodpd) > 0 and v1nac <> *blanks and
061060101102             O14lad <= 0;
061070100909           v1cmsg = 'Nazione destino non servita da DPD. +
061080100909                     Richiedere al Depot l''annullamento';
061090100907           *in28 = *on;
061100100907           *in65 = *on;
061110100907           *in75 = *on;
061120100811         ENDIF;
061130160208
061140160209       //?Se inserito destinatario
061150160209         IF  ds_V1Ccrc > 0 or V1rsc <> *blanks;
061160160209         //?Se c'è la filiale di consegna uso quella
061170160209           IF  V1Cpoc > *zeros;
061180160209             O95lna = %dec(V1Cpoc:3:0);
061190160209           ELSE;
061200160209         //?Se non c'è la filiale di consegna
061210160209         //?richiamo routine sr_contrcapc per il calcolo della LNA
061220160209             exsr sr_contrcapc;
061230160209           ENDIF;
061240160209         //?Se ORM Prepagato e consegna a lna 340-341-345 ORM non possibile vedi DV 1540
061250160209           IF  (O95lna = 340 or O95lna = 341 or O95lna = 345) and
061260160209                V1Ctor = 'P';
061270160209             TBLcod = '15';
061280160209             TBLkey = v1nac;
061290160209             chain (CodUt:TBLcod:TBLkey) TABEL00F;
061300160209             IF  %found(TABEL00F);
061310160209               ds15 = TBLuni;
061320160209             ENDIF;
061330160209             V1Cmsg = 'Per consegne in';
061340160209             V1Cmsg = %trim(V1Cmsg) + ' ' + §15des;
061350160209             V1Cmsg = %trim(V1Cmsg) +
061360160209                      ' no pagamento a carico del Mittente';
061370160209             *in28 = *on;
061380160209             *in65 = *on;
061390160209             *in75 = *on;
061400160209           ENDIF;
061410160209         ENDIF;
061420160208
061430100811      /end-free
061440010202
061450010202     C     endsrclid     endsr
061460010202      **********************************************************************
061470010202      * CONTROLLI     * cliente destinatario *
061480010202      **********************************************************************
061490010202     C     Sr_Contrcrc   begsr
061500010202
061510010202     C                   z-add     ds_v1ccrc     kfnacr
061520151109     C     kfnacr        chain(n)  fnacr01l
061530010202     C                   if        %found(fnacr01l)
061540010202     C                   if        ACRatb <> *blanks
061550010202     C                   movel     msg(18)       v1cmsg
061560010202     C                   seton                                        56  28
061570010202     C                   goto      endsrccrc
061580010202     C                   endif
061590040727     C                   if        ACRtcr = 'M'
061600040727     C                   movel     msg(83)       v1cmsg
061610040727     C                   seton                                        56  28
061620040727     C                   goto      endsrccrc
061630040727     C                   endif
061640010906
061650010906      * Se modificato il cliente pulisco il ksc
061660010906     C                   if        v1pag = 'D'
061670010906     C                             and (ds_v1ccrc <> sav_v1ccrc
061680031010     C                             or v1pag <> sav_v1pag
061690040630     c                             or v1ksc = *Zeros
061700040630     c                             or V1Ctr = *Blanks)
061710031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocrc mi carica il ksc
061720040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
061730031010     c                   Clear                   sav_V1pag
061740031010     c                   EndIf
061750010906     C                   clear                   v1ksc
061760010906     C                   clear                   v1ctr
061770010906     C                   endif
061780010202     C                   exsr      Sr_Decocrc
061790010202     C                   else
061800010202     C                   movel     msg(18)       v1cmsg
061810010202     C                   seton                                        56  28
061820010202     C                   endif
061830010202
061840010202     C     endsrccrc     endsr
061850010202      **********************************************************************
061860011113      * DECODIFICHE     * cliente destinatario *
061870010202      **********************************************************************
061880010202     C     Sr_Decocrc    begsr
061890010202
061900010202     C                   movel     ACRrsr        v1rsc
061910010202     C                   movel     ACRinr        v1inc
061920010202     C                   movel     ACRcar        v1cac
061930010202     C                   movel     ACRlor        v1loc
061940030922     C                   movel     ACRlor        sav_v1loc35
061950010202     C                   movel     ACRprr        v1prc
061960010202     C                   movel     ACRnar        v1nac
061970010207      * Se paga destinatario memorizzo il codice piano dei conti
061980010207     C                   if        v1pag = 'D'
061990010906     C                             and (ds_v1ccrc <> sav_v1ccrc
062000010906     C                             or v1pag <> sav_v1pag)
062010010906     C                             and  v1ksc = *zeros
062020050120     c                   If        AcrKsc > *Zeros
062030010207     C                   eval      v1ksc = ACRksc
062040010903     C  nkf              eval      sav_v1ksc  = ACRksc
062050010309     C                   eval      sav_v1pag = v1pag
062060040629      * memorizzo anche il codice tariffa
062070040629     c                   If        AcrCcc = 999
062080040629     c                   Clear                   V1Ctr
062090040629     c                   Else
062100040629     c                   Move      AcrCcc        V1Ctr
062110040629     c                   EndIf
062120050120     c                   Else
062130050120     c                   Eval      V1Ksc = ds_v1crc12
062140050120     c                   EndIf
062150010207     C                   endif
062160010202
062170010202     C                   endsr
062180010202      **********************************************************************
062190011113      * CONTROLLI     * dati cliente destinatario *
062200010202      **********************************************************************
062210010202     C     Sr_Contrrsc   begsr
062220030922
062230030930     c                   If        V1loc = *Blanks or
062240030930     c                             (v1loc <> sav_v1loc)
062250030929     c                   Clear                   sav_v1loc35
062260030929     c                   EndIf
062270030922     c                   If        sav_v1loc35 = *blanks
062280030922     c                   Movel(p)  v1loc         sav_v1loc35
062290030922     c                   EndIf
062300131202
062310131202      /free
062320131202         IF  V1loc <> sav_V1loc or V1cac <> sav_V1cac or V1nac <> sav_V1nac;
062330131202           wForzaNAC = *off;
062340131202         ENDIF;
062350131202      /end-free
062360010202
062370010202     C                   clear                   FNLV14DS
062380010202     C                   z-add     *date         I14dri
062390010202     C                   movel     v1rsc         I14rsc
062400010202     C                   movel     v1inc         I14ind
062410030922     C                   movel(p)  sav_v1loc35   E14loc
062420010202     C                   movel     v1prc         E14prv
062430010202     C                   movel     v1cac         E14cap
062440010202     C                   movel     v1nac         E14nar
062450161109      /free
062460161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
062470161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
062480161109       //?il pgm di interrogazione cappario
062490161109         IF  ConfAuto;
062500161109           I14batch = 'S';
062510161109         ENDIF;
062520161109      /end-free
062530010202
062540010202     C                   call      'FNLV14R'
062550010202     C                   parm                    KPJBA
062560010202     C                   parm                    FNLV14DS
062570010202
062580010202     C                   movel     I14ind        v1inc
062590010202     C                   movel     E14loc        v1loc
062600030922     C                   movel     E14loc        sav_v1loc35
062610010202     C                   movel     E14prv        v1prc
062620010202     C                   movel     E14cap        v1cac
062630010202     C                   movel     E14nar        v1nac
062640010202
062650010202     C                   if        v1rsc <> *blanks or v1inc <> *blanks
062660010202     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
062670010202     C                   eval      *in61 = (O14err = '2')                       Indirizzo
062680010202     C                   eval      *in62 = (O14err = '5')                       CAP
062690010202     C                   eval      *in63 = (O14err = '3')                       Località
062700010202     C                   eval      *in64 = (O14err = '4')                       Provincia
062710010202     C                   eval      *in65 = (O14err = '6')                       Nazione
062720010202
062730010202     C                   if        O14msg <> *blanks
062740010202     C                   movel(p)  O14msg        v1cmsg
062750010202     C                   eval      *IN28 = *ON
062760010202     C                   eval      *in75 = *on
062770010202     C                   goto      endsrcrsc
062780010202     C                   endif
062790010202     C                   endif
062800010202
062810010202      * Controlla indirizzo
062820010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062830010202     C                             v1prc <> *blanks or v1nac <> *blanks
062840010202     C                   exsr      Sr_Contrcapc
062850010202     C                   eval      *in75 = (*in28 = *on)
062860010202     C   28              goto      endsrcrsc
062870010202     C                   endif
062880010202
062890010202      * Controlla se dati completi o parziali
062900010202     C                   if        v1rsc = *blanks
062910010202     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062920010202     C                             v1prc <> *blanks or v1nac <> *blanks
062930010202     C                   movel     msg(39)       v1cmsg
062940010202     C                   eval      *IN28 = *ON
062950010202     C                   eval      *in56 = *on
062960010202     C                   goto      endsrcrsc
062970010202     C                   endif
062980010202     C                   endif
062990131202      /free
063000131202       //?Se inserita la nazione e questa è lunga 2 byte e presente in tabella PR-provincia
063010131202       //?devo avvisare l'utente se è giusta la nazione o se per caso voleva inserire la
063020131202       //?provincia. Caso di nazione 'VE' accettata in immissione ORM perchè esiste la
063030131202       //?nazione VE Venezuale ma l'utente voleva inserire VE Venezia, ha sbagliato campo
063040131202         IF  V1rsc <> *blanks and V1nac <> *blanks and
063050131202            ((%subst(V1nac:1:1) = *blanks and %subst(V1nac:2:2) <> *blanks) or
063060131202             (%subst(V1nac:1:2) <> *blanks and %subst(V1nac:3:1) = *blanks));
063070131202           TBLcod = 'PR';
063080131202           TBLkey = v1nac;
063090131202           chain (CodUt:TBLcod:TBLkey) TABEL00F;
063100131202           IF  %found(TABEL00F) and TBLflg = *blanks and not wForzaNAC;
063110131202             TBLcod = '15';
063120131202             TBLkey = v1nac;
063130131202             chain (CodUt:TBLcod:TBLkey) TABEL00F;
063140131202             IF  %found(TABEL00F);
063150131202               ds15 = TBLuni;
063160131202             ENDIF;
063170131202             wForzaNAC = *on;
063180131202             sav_V1nac = V1nac;
063190131202             *in28 = *on;
063200131202             *in65 = *on;
063210131202             *in75 = *on;
063220131202             V1Cmsg = 'Verificare la nazione del destinatario';
063230131202             V1Cmsg = %trim(V1Cmsg) + ' ' +
063240131202                      %trim(V1nac) + '-' +
063250131202                      %trim(§15des);
063260131202             leavesr;
063270131202           ENDIF;
063280131202         ENDIF;
063290131202      /end-free
063300010207
063310010207      * Se paga il destinatario devo pulire il codice cliente che paga
063320010329     C                   if        v1pag = 'D'
063330010507     C                             and (v1pag <> sav_v1pag
063340010917     C                             or   sav_v1ccrc <> ds_v1ccrc)
063350010207     C                   clear                   v1ksc
063360010906     C                   clear                   v1ctr
063370010903     C  nkf              clear                   sav_v1ksc
063380010329     C                   eval      sav_v1pag = v1pag
063390010207     C                   endif
063400010202
063410010202     C     endsrcrsc     endsr
063420010202      **********************************************************************
063430010202      * CONTROLLI     * cap - località - provincia *
063440010202      **********************************************************************
063450010202     C     Sr_Contrcapc  begsr
063460010202
063470010202     C                   eval      *in32 = *off
063480010202
063490010202     C                   clear                   TISI95DS
063500010202     C                   clear                   FNLV13DS
063510080423     c                   clear                   tisi97ds
063520010202     C                   movel     ' '           I95tla
063530010202     C                   movel     '7'           I95tcn
063540010202     C                   movel     v1cac         I95cap
063550010202     C                   movel     v1nac         I95nar
063560010202     C                   movel     v1inc         I95ind
063570030922     C                   movel(p)  sav_v1loc35   I95loc
063580010202     C                   movel     v1prc         I95prv
063590010202     C                   movel     'S'           I13af0
063600010202     C                   movel     'S'           I13af1
063610010202     C                   movel     'S'           I13sz2
063620010202     C                   movel     'S'           I13la3
063630010202     C                   movel     'S'           I13sz3
063640010202     C                   movel     'S'           I13cnv
063650101215     c                   if        v1pag<>'M'
063660101214     c                   eval      i95tpo='A'
063670101214     c                   endif
063680010202
063690010202      * Gestione errori forzabili: se cambiato CAP/Località
063700010202      * riazzera i valori di errori forzabili
063710080910
063720080910      * CAP
063730080910     C     v1cac         ifne      sav_v1cac
063740080910     C                   movel     v1cac         sav_v1cac
063750080910     C                   clear                   wfzv
063760080910     C                   clear                   wfz1
063770080910     C                   clear                   wfz2
063780080910     C                   clear                   wfz3
063790080910     C                   eval      *in32 = *on
063800080910     C                   endif
063810010202
063820010202      * Modifica località
063830010202
063840010202     C     v1loc         ifne      sav_v1loc
063850010202     C                   clear                   wfz1
063860010202     C                   clear                   wfz2
063870010202     C                   clear                   wfz3
063880010202     C                   movel     v1loc         sav_v1loc
063890010202     C                   eval      *in32 = *on
063900010202     C                   endif
063910010202
063920010202      * Modifica provincia
063930010202
063940010202     C     v1prc         ifne      sav_v1prc
063950010202     C                   clear                   wfz3
063960010202     C                   movel     v1prc         sav_v1prc
063970010202     C                   eval      *in32 = *on
063980010202     C                   endif
063990010202
064000010202     C                   movel     wfzv          E13fzv
064010010202     C                   movel     wfz1          E13fz1
064020010202     C                   movel     wfz2          E13fz2
064030010202     C                   movel     wfz3          E13fz3
064040060929
064050060929     c                   eval      i97ntw = c_§ogntw
064060010202
064070010202     C                   call      'FNLV13R'
064080010202     C                   parm                    KPJBA
064090010202     C                   parm                    FNLV13DS
064100010202     C                   parm                    TISI95DS
064110060929     c                   parm      ' '           flgbac            1
064120060929     c                   parm                    tisi97ds
064130010202
064140010202      * Ricontrolla errori
064150010202
064160010202     C                   movel     O13err        werr
064170010202     C                   movel     O13msg        wmsg
064180010202     C                   movel     E13fzv        wfzv
064190010202     C                   movel     E13fz1        wfz1
064200010202     C                   movel     E13fz2        wfz2
064210010202     C                   movel     E13fz3        wfz3
064220010202
064230010202      * Se il livello di affidabilità non è inferiore a 2 e
064240010202      * il livello di reperimento dati non è inferiore a 3
064250010202      * reimposta i dati da TISI95DS
064260110120      * I dati non vengono sostituiti se i dati provengono da anagrafica
064270010202
064280110120     c                   if        ds_v1ccrc=0
064290010202     C     O13ron        ifeq      'S'
064300110120     C                   movel     O95nar        v1nac
064310110120     c                   endif
064320110120     C     O13roc        ifeq      'S'
064330110120     C                   movel     O95cap        v1cac
064340110120     c                   endif
064350110120     C     O13rop        ifeq      'S'
064360110120     C                   movel     O95prv        v1prc
064370110120     c                   endif
064380110120     C     O13rol        ifeq      'S'
064390010202     C                   movel     O95loc        v1loc
064400030922     C                   movel     O95loc        sav_v1loc35
064410010202     C                   end
064420161109
064430161109      /free
064440161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
064450161109       //?conferma automatica batch non devo dare errore
064460161109         IF  ConfAuto and O13roc = 'S';
064470161109           clear werr;
064480161109           clear O13roc;
064490161109         ENDIF;
064500161109      /end-free
064510010202
064520010202     C     werr          ifne      ' '
064530010202     C     wmsg          andne     *blanks
064540080910     C     *in32         oreq      *on
064550010202
064560010202     C                   select
064570010202
064580010202     C     werr          wheneq    '3'
064590010202     C                   eval      *in63 = *on
064600010202     C                   eval      *in28 = *on
064610010202
064620010202     C     werr          wheneq    '5'
064630010202     C                   eval      *in62 = *on
064640010202     C                   eval      *in28 = *on
064650010202
064660010202     C     werr          wheneq    '4'
064670010202     C                   eval      *in64 = *on
064680010202     C                   eval      *in28 = *on
064690010202
064700010202     C     werr          wheneq    '6'
064710010202     C                   eval      *in65 = *on
064720010202     C                   eval      *in28 = *on
064730010202     C                   endsl
064740010202
064750010202     C                   movel(p)  wmsg          v1cmsg
064760010202     C                   end
064770110120     c                   else
064780110120     C     werr          ifne      ' '
064790110120     C     wmsg          andne     *blanks
064800110120     C                   eval      *in56 = *on
064810110120     C                   eval      *in28 = *on
064820110120     C                   movel(p)  msg(94)       v1cmsg
064830110120     c                   endif
064840110120     c                   endif
064850010202
064860010202     C   28              goto      endsrcapc
064870010202
064880010202      * Controlla se reimpostata località
064890010202
064900010202     C     O13rol        ifeq      'S'
064910010202     C                   eval      *in63 = *on
064920010202     C                   eval      *in28 = *on
064930010202     C                   movel     msg(19)       v1cmsg
064940010202     C                   goto      endsrcapc
064950010202     C                   end
064960010202
064970010202      * Controlla se reimpostata Provincia
064980010202
064990010202     C     O13rop        ifeq      'S'
065000010202     C                   eval      *in64 = *on
065010010202     C                   eval      *in28 = *on
065020010202     C                   movel     msg(21)       v1cmsg
065030010202     C                   goto      endsrcapc
065040010202     C                   end
065050010202
065060010202      * Controlla se reimpostato cap
065070010202
065080010202     C     O13roc        ifeq      'S'
065090010202     C                   eval      *in62 = *on
065100010202     C                   eval      *in28 = *on
065110010202     C                   movel     msg(20)       v1cmsg
065120010202     C                   goto      endsrcapc
065130010202     C                   end
065140010202
065150010202      * Controlla se reimpostata Nazione
065160010202
065170010202     C     O13ron        ifeq      'S'
065180010202     C                   eval      *in65 = *on
065190010202     C                   eval      *in28 = *on
065200010202     C                   movel     msg(22)       v1cmsg
065210010202     C                   goto      endsrcapc
065220010202     C                   end
065230110610
065240110610      /free
065250110610       //?Mi salvo se il cap del destinatario prevede l'obbligo DDT
065260110610         $cacgdd = (O95gdd = 'S');
065270110610      /end-free
065280110610
065290010202
065300010202     C     endsrcapc     endsr
065310010208      **********************************************************************
065320010208      * CONTROLLI     * cliente ordinante *
065330010208      **********************************************************************
065340010208     C     Sr_Contrclio  begsr
065350010208
065360010208     C                   eval      ds_v1cor1 = v1cor1
065370010208     C                   eval      ds_v1cor2 = v1cor2
065380010208     C                   eval      ds_v1cor3 = v1cor3
065390160413      /free
065400160413       //?Se è stato variato il codice ordinante
065410160413       //?in caso di immissione ORM manuale a mittente non codificato
065420160413       //?devo reimpostare i dati per la conferma prenotazione
065430160413         IF  sav_V1Ccor <> ds_V1Ccor and
065440160413             (*in01 or *in03) and
065450160413             (V1Ctco = 'M' or V1Ctco = 'E') and
065460160413             ds_V1Ccra = 0;
065470160413           wOkF6F13 = *off;
065480160413           wRicConf = *off;
065490160413           clear W04sms;
065500160413           clear W04mail;
065510160413         ENDIF;
065520160413      /end-free
065530010208      * Inserito codice
065540010208     C                   if        ds_v1ccor <> *zeros
065550010208     C                   eval      *in08 = *on
065560010208     C                   exsr      Sr_Contrcor
065570010208     C                   eval      sav_v1ccor = ds_v1ccor
065580010208     C                   else
065590010208      * Inserita ragione sociale
065600010208     C                   clear                   sav_v1ccor
065610010208     C                   exsr      Sr_Contrrso
065620010208     C                   eval      *in08 = *off
065630010208     C                   endif
065640010208
065650010702     C                   endsr
065660010208      **********************************************************************
065670010208      * CONTROLLI     * cliente ordinante *
065680010208      **********************************************************************
065690010208     C     Sr_Contrcor   begsr
065700010208
065710010208     C                   z-add     ds_v1ccor     kfnacr
065720151109     C     kfnacr        chain(n)  fnacr01l
065730010208     C                   if        not%found(fnacr01l)
065740010208     C                   movel     msg(3)        v1cmsg
065750010208     C                   seton                                        42  28
065760010208     C                   goto      endsrccor
065770010208     C                   endif
065780010208     C                   if        ACRatb <> *blanks
065790010208     C                   movel     msg(3)        v1cmsg
065800010208     C                   seton                                        42  28
065810010208     C                   goto      endsrccor
065820010208     C                   endif
065830040727     C                   if        ACRtcr = 'M'
065840040727     C                   movel     msg(82)       v1cmsg
065850040727     C                   seton                                        42  28
065860040727     C                   goto      endsrccor
065870040727     C                   endif
065880010906      * Se modificato il cliente pulisco il ksc
065890010906     C                   if        v1pag = 'O'
065900010906     C                             and (ds_v1ccor <> sav_v1ccor
065910031010     C                             or v1pag <> sav_v1pag
065920040630     c                             or   v1ksc = *Zeros
065930040630     c                             or   V1Ctr = *Blanks)
065940031010      * se il ksc è a zero pulisco anche il sav_v1pag, facendo così nella decocro mi carica il ksc
065950040630     c                   If        v1ksc = *zeros or V1Ctr = *Blanks
065960031010     c                   Clear                   sav_V1pag
065970031010     c                   EndIf
065980010906     C                   clear                   v1ksc
065990010906     C                   clear                   v1ctr
066000010906     C                   endif
066010010208     C                   exsr      Sr_Decocor
066020010208
066030010208     C     endsrccor     endsr
066040010208      **********************************************************************
066050010208      * DECODIFICHE     * cliente ordinante *
066060010208      **********************************************************************
066070010208     C     Sr_Decocor    begsr
066080010208
066090010208     C                   movel     ACRrsr        v1rso
066100010208     C                   movel     ACRinr        v1ino
066110010208     C                   movel     ACRcar        v1cao
066120010208     C                   movel     ACRlor        v1loo
066130030922     C                   movel     ACRlor        sav_v1loo35
066140010208     C                   movel     ACRprr        v1pro
066150010208     C                   movel     ACRnar        v1nao
066160010208      * Se paga ordinante memorizzo il codice piano dei conti
066170030902      * se non c'è il ksc memorizzo i primi 7 byte del codice anagrafica ritiro
066180010208     C                   if        v1pag = 'O'
066190010906     C                             and (ds_v1ccor <> sav_v1ccor
066200010906     C                             or v1pag <> sav_v1pag)
066210010906     C                             and  v1ksc = *zeros
066220030902     c                   If        AcrKsc > *Zeros
066230010208     C                   eval      v1ksc = ACRksc
066240040629      * memorizzo anche il codice tariffa
066250040629     c                   If        AcrCcc = 999
066260040629     c                   Clear                   V1Ctr
066270040629     c                   Else
066280040629     c                   Move      AcrCcc        V1Ctr
066290040629     c                   EndIf
066300030902     c                   Else
066310030902     c                   Eval      V1Ksc = Ds_V1cor12
066320030902     c                   EndIf
066330030902     C  nkf              eval      sav_v1ksc  = V1ksc
066340010309     C                   eval      sav_v1pag = v1pag
066350010208     C                   endif
066360030606      * lo salvo comunque xchè mi può servire nel caso di creazione
066370030606      * bolla x orm commissionato
066380030606     c                   Eval      AcrKscCor = AcrKsc
066390050202     c                   Eval      AcrCccCor = AcrCcc
066400040405
066410040405      * recupero le note del cliente ordinante
066420040405      * se il cliente ritiro non è codificato
066430040405if  1c                   if        ds_v1ccor <> sav_v1ccor and ds_v1ccra = *Zeros
066440040726      * solo se non sono già state immesse
066450040405     c                   If        v1not1 = *Blanks
066460040405     c                   Eval      V1not1 = AcrNo1
066470040405     c                   EndIf
066480040405     c                   If        v1not2 = *Blanks
066490040405     c                   Eval      V1not2 = AcrNo2
066500040405     c                   EndIf
066510040405e   1c                   EndIf
066520050321
066530050321      * imposto se ORM commissionato o meno
066540050323     c                   If        *In01
066550050322     c                             and (sav_v1ccor <> ds_v1ccor or
066560050322     c                                  v1com = *blanks)
066570050321     c                   Eval      v1com = AcrFcc
066580050322     c                   EndIf
066590010208
066600010208     C                   endsr
066610010208      **********************************************************************
066620010208      * CONTROLLI     * dati cliente ordinante *
066630010208      **********************************************************************
066640010208     C     Sr_Contrrso   begsr
066650030922
066660030930     c                   If        V1loo = *Blanks or
066670030930     c                             (v1loo <> sav_v1loo)
066680030929     c                   Clear                   sav_v1loo35
066690030929     c                   EndIf
066700030922     c                   If        sav_v1loo35 = *blanks
066710030922     c                   Movel(p)  v1loo         sav_v1loo35
066720030922     c                   EndIf
066730010208
066740010208     C                   clear                   FNLV14DS
066750010208     C                   z-add     *date         I14dri
066760010208     C                   movel     v1rso         I14rsc
066770010208     C                   movel     v1ino         I14ind
066780030922     C                   movel(p)  sav_v1loo35   E14loc
066790010208     C                   movel     v1pro         E14prv
066800010208     C                   movel     v1cao         E14cap
066810010208     C                   movel     v1nao         E14nar
066820161109      /free
066830161109       //?Se FIOR05R richiamato per conferma automatica passo 'S' di batch al
066840161109       //?FNLV14R, in questo modo se non ho cap e provincia non viene richiamato
066850161109       //?il pgm di interrogazione cappario
066860161109         IF  ConfAuto;
066870161109           I14batch = 'S';
066880161109         ENDIF;
066890161109      /end-free
066900010208
066910010208     C                   call      'FNLV14R'
066920010208     C                   parm                    KPJBA
066930010208     C                   parm                    FNLV14DS
066940010208
066950010208     C                   movel     I14ind        v1ino
066960010208     C                   movel     E14loc        v1loo
066970030922     C                   movel     E14loc        sav_v1loo35
066980010208     C                   movel     E14prv        v1pro
066990010208     C                   movel     E14cap        v1cao
067000010208     C                   movel     E14nar        v1nao
067010010220
067020010220     C                   if        v1rso <> *blanks or v1ino <> *blanks
067030010220     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
067040010220     C                   eval      *in61 = (O14err = '2')                       Indirizzo
067050010220     C                   eval      *in62 = (O14err = '5')                       CAP
067060010220     C                   eval      *in63 = (O14err = '3')                       Località
067070010220     C                   eval      *in64 = (O14err = '4')                       Provincia
067080010220     C                   eval      *in65 = (O14err = '6')                       Nazione
067090010220
067100010220     C                   if        O14msg <> *blanks
067110010220     C                   movel(p)  O14msg        v1cmsg
067120010220     C                   eval      *IN28 = *ON
067130010220     C                   eval      *in74 = *on
067140010220     C                   goto      endsrcrso
067150010220     C                   endif
067160010220     C                   endif
067170010208
067180010208      * Controlla indirizzo
067190010208     C                   if        v1cao <> *blanks or v1loo <> *blanks or
067200010208     C                             v1pro <> *blanks or v1nao <> *blanks
067210010208     C                   exsr      Sr_Contrcapo
067220010208     C                   eval      *in74 = (*in28 = *on)
067230010208     C   28              goto      endsrcrso
067240010208     C                   endif
067250010208      * Se paga l'ordinante devo pulire il codice cliente che paga
067260010329     C                   if        v1pag = 'O'
067270010507     C                             and (v1pag <> sav_v1pag
067280010917     C                             or   sav_v1ccor <> ds_v1ccor)
067290010208     C                   clear                   v1ksc
067300010906     C                   clear                   v1ctr
067310010507     C  nkf              clear                   sav_v1ksc
067320010329     C                   eval      sav_v1pag = v1pag
067330010208     C                   endif
067340010208
067350010208     C     endsrcrso     endsr
067360010208      **********************************************************************
067370010208      * CONTROLLI     * cap - località - provincia *
067380010208      **********************************************************************
067390010208     C     Sr_Contrcapo  begsr
067400010208
067410010208     C                   eval      *in32 = *off
067420010208
067430010208     C                   clear                   TISI95DS
067440010208     C                   clear                   FNLV13DS
067450010208     C                   movel     ' '           I95tla
067460010208     C                   movel     '7'           I95tcn
067470010208     C                   movel     v1cao         I95cap
067480010208     C                   movel     v1nao         I95nar
067490010208     C                   movel     v1ino         I95ind
067500030922     C                   movel(p)  sav_v1loo35   I95loc
067510010208     C                   movel     v1pro         I95prv
067520010208     C                   movel     'S'           I13af0
067530010208     C                   movel     'S'           I13af1
067540010208     C                   movel     'S'           I13sz2
067550010208     C                   movel     'S'           I13la3
067560010208     C                   movel     'S'           I13sz3
067570010208     C                   movel     'S'           I13cnv
067580010208
067590010906      * Gestione errori forzabili: se cambiato CAP/Località
067600010208      * riazzera i valori di errori forzabili
067610010208
067620080910      * CAP
067630080910     C     v1cao         ifne      sav_v1cao
067640080910     C                   movel     v1cao         sav_v1cao
067650080910     C                   clear                   wfzv
067660080910     C                   clear                   wfz1
067670080910     C                   clear                   wfz2
067680080910     C                   clear                   wfz3
067690080910     C                   eval      *in32 = *on
067700080910     C                   endif
067710010208
067720010208      * Modifica località
067730010208
067740010208     C     v1loo         ifne      sav_v1loo
067750010208     C                   clear                   wfz1
067760010208     C                   clear                   wfz2
067770010208     C                   clear                   wfz3
067780010208     C                   movel     v1loo         sav_v1loo
067790010208     C                   eval      *in32 = *on
067800010208     C                   endif
067810010208
067820010208      * Modifica provincia
067830010208
067840010208     C     v1pro         ifne      sav_v1pro
067850010208     C                   clear                   wfz3
067860010208     C                   movel     v1pro         sav_v1pro
067870010208     C                   eval      *in32 = *on
067880010208     C                   endif
067890010208
067900010208     C                   movel     wfzv          E13fzv
067910010208     C                   movel     wfz1          E13fz1
067920010208     C                   movel     wfz2          E13fz2
067930010208     C                   movel     wfz3          E13fz3
067940010208
067950010208     C                   call      'FNLV13R'
067960010208     C                   parm                    KPJBA
067970010208     C                   parm                    FNLV13DS
067980010208     C                   parm                    TISI95DS
067990010208
068000010208      * Ricontrolla errori
068010010208
068020010208     C                   movel     O13err        werr
068030010208     C                   movel     O13msg        wmsg
068040010208     C                   movel     E13fzv        wfzv
068050010208     C                   movel     E13fz1        wfz1
068060010208     C                   movel     E13fz2        wfz2
068070010208     C                   movel     E13fz3        wfz3
068080010208
068090010208      * Se il livello di affidabilità non è inferiore a 2 e
068100010208      * il livello di reperimento dati non è inferiore a 3
068110010208      * reimposta i dati da TISI95DS
068120010208
068130010208     C     O13ron        ifeq      'S'
068140110120     C                   movel     O95nar        v1nao
068150110120     c                   endif
068160110120     C     O13roc        ifeq      'S'
068170110120     C                   movel     O95cap        v1cao
068180110120     c                   endif
068190110120     C     O13rop        ifeq      'S'
068200110120     C                   movel     O95prv        v1pro
068210110120     c                   endif
068220110120     C     O13rol        ifeq      'S'
068230010208     C                   movel     O95loc        v1loo
068240030922     C                   movel     O95loc        sav_v1loo35
068250010208     C                   end
068260161109
068270161109      /free
068280161109       //?Se da LV13 torna che è stato sostituito il cap e sono in
068290161109       //?conferma automatica batch non devo dare errore
068300161109         IF  ConfAuto and O13roc = 'S';
068310161109           clear werr;
068320161109           clear O13roc;
068330161109         ENDIF;
068340161109      /end-free
068350010208
068360010208     C     werr          ifne      ' '
068370010208     C     wmsg          andne     *blanks
068380080910     C     *in32         oreq      *on
068390010208
068400010208     C                   select
068410010208
068420010208     C     werr          wheneq    '3'
068430010208     C                   eval      *in63 = *on
068440010208     C                   eval      *in28 = *on
068450010208
068460010208     C     werr          wheneq    '5'
068470010208     C                   eval      *in62 = *on
068480010208     C                   eval      *in28 = *on
068490010208
068500010208     C     werr          wheneq    '4'
068510010208     C                   eval      *in64 = *on
068520010208     C                   eval      *in28 = *on
068530010208
068540010208     C     werr          wheneq    '6'
068550010208     C                   eval      *in65 = *on
068560010208     C                   eval      *in28 = *on
068570010208     C                   endsl
068580010208      *
068590010208     C                   movel(p)  wmsg          v1cmsg
068600010208     C                   end
068610010208
068620010208     C   28              goto      endsrcapo
068630010208
068640010208      * Controlla se reimpostata località
068650010208
068660010208     C     O13rol        ifeq      'S'
068670010208     C                   eval      *in63 = *on
068680010208     C                   eval      *in28 = *on
068690010208     C                   movel     msg(19)       v1cmsg
068700010208     C                   goto      endsrcapo
068710010208     C                   end
068720010208
068730010208      * Controlla se reimpostata Provincia
068740010208
068750010208     C     O13rop        ifeq      'S'
068760010208     C                   eval      *in64 = *on
068770010208     C                   eval      *in28 = *on
068780010208     C                   movel     msg(21)       v1cmsg
068790010208     C                   goto      endsrcapo
068800010208     C                   end
068810010208
068820010208      * Controlla se reimpostato cap
068830010208
068840010208     C     O13roc        ifeq      'S'
068850010208     C                   eval      *in62 = *on
068860010208     C                   eval      *in28 = *on
068870010208     C                   movel     msg(20)       v1cmsg
068880010208     C                   goto      endsrcapo
068890010208     C                   end
068900010208
068910010208      * Controlla se reimpostata Nazione
068920010208
068930010208     C     O13ron        ifeq      'S'
068940010208     C                   eval      *in65 = *on
068950010208     C                   eval      *in28 = *on
068960010208     C                   movel     msg(22)       v1cmsg
068970010208     C                   goto      endsrcapo
068980010208     C                   end
068990010208
069000010208     C     endsrcapo     endsr
069010050323
069020050323      *--------------------------------------------------------------------*
069030050323      * CONTROLLO PADRE/FIGLI ordinante con codice ritiro
069040050323      *--------------------------------------------------------------------*
069050050323     c     Sr_Figli      BegSr
069060050323
069070050323      * controllo se l'ordinante è un papà
069080050323     c                   Clear                   Tibs10ds
069090050323     c                   Eval      d10tle = 'WW'
069100050323     c                   Eval      d10paf = 'F'
069110050323     c                   Move      ds_v1cor12    d10cod
069120050323     c                   Eval      d10drf = *date
069130050323     c                   Call      'TIBS10R'
069140050323     c                   Parm                    Tibs10ds
069150050323if  1c                   If        d10err <> *Blanks
069160050323      * controllo se il codice inserito è un figlio
069170050323     c                   Clear                   Tibs10ds
069180050323     c                   Eval      d10tle = 'WW'
069190050323     c                   Eval      d10paf = 'P'
069200050323     c                   Move      ds_v1cor12    d10cod
069210050323     c                   Eval      d10drf = *date
069220050323     c                   Call      'TIBS10R'
069230050323     c                   Parm                    Tibs10ds
069240050323if  2c                   If        d10err <> *Blanks
069250050323     c                   Clear                   skfigli
069260050323e   2c                   EndIf
069270050323e   1c                   EndIf
069280050323
069290050323     c                   EndSr
069300050323
069310001016      **********************************************************************
069320001016      * CONTROLLI     * P.O. ritiro *
069330001016      **********************************************************************
069340001016     C     Sr_Contrpor   begsr
069350131024
069360010413      * Controllo il p.o. di ritiro
069370010413     C                   clear                   v1dpor
069380010202
069390010413      * Ricerca P.O.Ritiro  con "?"
069400010413     C     '?'           scan      v1cpor                                 30
069410010413     C     *in30         ifeq      *on
069420010413     C                   eval      *in59 = *on
069430010413     C                   eval      *in90 = *on
069440010413     C                   clear                   ds_tnsd
069450010413     C                   call      'TNSD24R'
069460010413     C                   parm                    ds_cod
069470010413     C                   parm                    ds_tip
069480010413     C                   parm                    ds_des
069490010413     C                   parm                    ds_pra
069500010413     C                   movel     ds_cod        v1cpor
069510010413     C                   movel     ds_des        v1dpor
069520010413     C                   endif
069530001023
069540010316      * Solo campi numerico
069550010320     C                   if        v1cpor = *blanks and
069560010320     C                             (%subst(v1cpor:1:1) < '0' and
069570010320     C                              %subst(v1cpor:1:1) <> ' ') or
069580010320     C                             (%subst(v1cpor:2:1) < '0' and
069590010320     C                              %subst(v1cpor:2:1) <> ' ') or
069600010320     C                             (%subst(v1cpor:3:1) < '0' and
069610010320     C                              %subst(v1cpor:3:1) <> ' ')
069620010316     C                   seton                                        59  28
069630010316     C                   movel     msg(33)       v1cmsg
069640010316     C                   goto      endsrcpor
069650010316     C                   endif
069660010202      * Obbligatorio
069670010315     C                   if        v1cpor = *blanks
069680010202     C                   movel     msg(11)       v1cmsg
069690010202     C                   seton                                        59  28
069700010202     C                   goto      endsrcpor
069710010202     C                   endif
069720060109
069730060109      * Se immissione ORM e p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
069740060109      * come p.o. ritiro OMR
069750060110     c                   If        (*In01 or *In03 ) and wforzapor = *Off
069760060109     c                   Clear                   fnlv55ds
069770060109     c                   Eval      d55tpt = '6'
069780060109     c                   Move      v1cpor        d55lin
069790060109     c                   Eval      d55drf = oggi_aammgg
069800060109     c                   Call      'FNLV55R'
069810060109     c                   Parm                    Fnlv55ds
069820060109     c                   If        d55err = *Blanks
069830060109     c                   Move      d55tfa        v1cpor
069840060109     c                   Eval      wforzapor = *On
069850060109     c                   EndIf
069860060109     c                   EndIf
069870151203
069880151203      /free
069890151203       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
069900151203       //?solo se immissione/copia ma no conferma ritiri fissi
069910151203       //?o manutenzione dopo dirottamento
069920151203         IF  ((*in01 or *in03) and §RMtco <> 'S') or
069930151203              (*in09 and wokdirotta = *on);
069940151203           clear FIOR96ds;
069950151203       //?se la filiale emissione non è ancora stata impostata passo la filiale utente
069960151203           IF  V1Cpoe = *zeros;
069970151203             IOR96poe = DUTpou;
069980151203           ELSE;
069990151203             IOR96poe = V1Cpoe;
070000151203           ENDIF;
070010151203           IOR96por = %dec(V1Cpor:3:0);
070020151203           fior96r(kpjba : fior96ds);
070030151203           IF  OOR96err = *blanks and OOR96por <> *zeros;
070040151203             V1Cpor = %editc(OOR96por:'X');
070050151203           ENDIF;
070060151203         ENDIF;
070070151203      /end-free
070080060109
070090010202      * Controllo
070100131112     c                   clear                   sav_ORGfl1
070110131210     c                   clear                   ntw_V1Cpor
070120001016     C                   movel     v1cpor        kazorg
070130001016     C     kazorg        chain     azorg01L
070140010202     C                   if        not%found(azorg01l)
070150010202     C                   movel     msg(33)       v1cmsg
070160010202     C                   seton                                        59  28
070170010202     C                   goto      endsrcpor
070180010202     C                   endif
070190001204     C                   if        ORGfva <> *blanks  or
070200001204     C                             (ORGfag <> 'A' and ORGfag <> 'F')
070210001017     C                   movel     msg(33)       v1cmsg
070220001016     C                   seton                                        59  28
070230001019     C                   goto      endsrcpor
070240001019     C                   endif
070250131112     c                   eval      sav_ORGfl1 = ORGfl1
070260080812     C                   movel     orgde3        og143
070270010206     C                   movel     ORGde8        og148
070280131210     c                   eval      ntw_V1Cpor = §OGntw
070290010206      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
070300010206     C                   if        §ogorm <> 'S'
070310010206     C                   movel     msg(36)       v1cmsg
070320010206     C                   seton                                        59  28
070330010206     C                   goto      endsrcpor
070340010206     C                   endif
070350131024
070360131024     C                   movel     ORGdes        v1dpor
070370131024     C                   movel     v1cpor        num_v1cpor
070380131024
070390131024      /free
070400150310       //?Imposto se filiale ritiro ha la procedura PDA attiva
070410150310         *in22 = (§ogpdaorm <> *blanks);
070420131024      /end-free
070430131024
070440060110      * Se il p.o. immesso a video è un p.o. gestito
070450060110      * errore per p.o. non gestibile
070460060110     c                   Clear                   fnlv55ds
070470060110     c                   Eval      d55tpt = '6'
070480060110     c                   Move      v1cpor        d55lin
070490060110     c                   Eval      d55drf = oggi_aammgg
070500060110     c                   Call      'FNLV55R'
070510060110     c                   Parm                    Fnlv55ds
070520060110     c                   If        d55err = *Blanks and d55tfa <> d55lin
070530060110     c                   movel     msg(89)       v1cmsg
070540060110     c                   seton                                        59  28
070550060110     c                   goto      endsrcpor
070560060110     c                   EndIf
070570080812
070580080812      * controllo se ritiro all'estero possibile
070590131030     c                   if        ntw_V1Cpor = 'DPD' or ntw_V1Cpor = 'EEX' or
070600131030     c                             ntw_V1Cpor = 'FED'
070610080812     c                   clear                   fnlv12ds
070620111206     c                   clear                   tisi95ds1
070630080812     c                   clear                   tisi97ds
070640111206     c                   eval      ai95nar = v1nar
070650111206     c                   eval      ai95cap = v1car
070660111206     c                   eval      ai95dat = oggi_aammgg
070670131030     c                   eval      i97ntw = ntw_V1Cpor
070680081215      * se impostato network DPD lo passo
070690081215     c                   if        v1ntwd <> *blanks
070700111206     c                   eval      ai95fi1 = v1ntwd
070710081215     c                   else
070720111206     c                   clear                   ai95fi1
070730081215     c                   endif
070740080812     c                   call      'FNLV12R'
070750080812     c                   parm                    fnlv12ds
070760111206     c                   parm                    tisi95ds1
070770080812     c                   parm                    tisi97ds
070780080812     c                   if        o12err <> *blanks
070790080812     c                   eval      v1cmsg = o12msg
070800080812     c                   seton                                        59  28
070810080812     c                   goto      endsrcpor
070820080812     c                   endif
070830080812     c                   endif
070840081215      * controllo congruenza tra network DPD e po ritiro
070850081215      * in immissione o copia
070860131030     c                   if        v1ntwd <> *blanks and ntw_V1Cpor <> 'DPD'
070870081215     c                             and (*in01 or *in03)
070880081215     c                   eval      *in28 = *on
070890081215     c                   eval      *in59 = *on
070900081215     c                   eval      v1cmsg = 'Indicato Network DPD. Incongruenza-
070910081215     c                              con la filiale ritiro'
070920081215     c                   goto      endsrcpor
070930081215     c                   endif
070940100219
070950100219      /free
070960100219       //?Se inserito ntw DPD richiamo il controllo del cappario DPD
070970110216       //?oppure filiale ritiro è DPD
070980131030       IF  v1ntwd <> *blanks or ntw_V1Cpor = 'DPD';
070990100811         wNazione = v1nar;
071000100811         wCapDPD  = v1car;
071010100811         exsr CtrCapDPD;
071020100811         IF  *in28;
071030100811           *in50 = *on;
071040100811           *in79 = *on;
071050100811           v1cmsg = %trim(v1cmsg) + ' F15 x interrogazione Cappario DPD';
071060100811           leavesr;
071070100811         ENDIF;
071080100219       ENDIF;
071090100219      /end-free
071100080812
071110010515      * Se p.o. estero controllare se DPD o EUROEXPRESS
071120010515     C                   eval      *in14 = *off
071130010515     C                   eval      *in15 = *off
071140131112     C                   if        sav_ORGfl1 = 'E'
071150010515     C                   eval      *in14 = *on
071160010515     C                   endif
071170131030     C                   if        ntw_V1Cpor = 'DPD'
071180010515     C                   eval      *in15 = *on
071190010515     C                   endif
071200010417
071210010213      * Se il p.o. ritiro è della mia £6 l'ORM non può essere prepagato
071220010213      * a meno che non sia richiamato da programma immissione prepagati
071230150609      * in immissione ORM
071240010704     C                   if        v1ctor = 'P' and §RMfpr <> 'B'
071250150609     C                             and §RMfpr <> 'C'
071260010307     C     num_v1cpor    lookup    l6                                     30
071270010307     C                   if        *in30 = *on
071280150609     C                             and *in01
071290010307     C                   movel     msg(48)       v1cmsg
071300160316     c                   eval      H1riga = 05
071310160316     c                   eval      H1colo = 11
071320160316     C                   seton                                            28
071330010307     C                   goto      endsrcpor
071340010307     C                   endif
071350010307     C                   endif
071360010417
071370010417     C                   if        %subst(v1cpor:1:1) = ' '  and
071380010417     C                             %subst(v1cpor:2:1) = ' '
071390010417     C                   movel     '00'          v1cpor
071400010417     C                   endif
071410010417     C                   if        %subst(v1cpor:1:1) = ' '
071420010417     C                   movel     '0'           v1cpor
071430010417     C                   endif
071440010417
071450010417     C                   if        v1cpor <> sav_v1cpor
071460010417     C                   eval      *in89 = *off
071470161116     c                   IF        sav_v1cpor <> *zeros and
071480161116     c                             sav_v1cpor <> *blanks
071490170613     c                             and wokdirotta = *off
071500161116     c                   eval      wModPor = *on
071510161116     c                   ENDIF
071520010417     C                   movel     v1cpor        sav_v1cpor
071530071023     c                   clear                   v1ccgi
071540071023     c                   clear                   sav_v1ccgi
071550071023     c                   clear                   sav_acrcgi
071560010417     C                   endif
071570010515
071580010521      * Se p.o. ritiro Euroexpress o DPD obbligatorio il peso e
071590010521      * i colli
071600010521     C                   if        *in14 = *on or *in15 = *on
071610010521     C                   if        v1pkg = *zeros
071620010515     C                   movel     msg(5)        v1cmsg
071630010515     C                   seton                                        44  28
071640010515     C                   goto      endsrcpor
071650010521     C                   endif
071660010521     C                   if        v1ncl = *zeros
071670010521     C                   movel     msg(4)        v1cmsg
071680010521     C                   seton                                        43  28
071690010521     C                   goto      endsrcpor
071700010521     C                   endif
071710081216      * no prepagato e no pagamento a carico del mittente
071720081216     c                   if        v1ctor = 'P'
071730160316     c                   eval      H1riga = 05
071740160316     c                   eval      H1colo = 11
071750081216     c                   eval      *in28 = *on
071760081216     c                   eval      v1cmsg = 'Per ORM Export non si può fare un -
071770081216     c                             prepagato'
071780081216     c                   goto      endsrcpor
071790081216     c                   endif
071800081216     c                   if        v1pag = 'M'
071810081216     c                   eval      *in28 = *on
071820081216     c                   eval      *in53 = *on
071830081216     c                   eval      v1cmsg = 'Per ORM Export non può pagare il -
071840081216     c                             mittente'
071850081216     c                   goto      endsrcpor
071860081216     c                   endif
071870010515     C                   endif
071880080422
071890080422      * se filiale ritiro DPD controllo i colli
071900080422      * deve essere obbligatoriamente 1
071910080422     c                   if        *in15 = *on and v1ncl > 1
071920080422     c                   eval      v1cmsg = msg(90)
071930080422     c                   eval      *in28 = *on
071940080422     c                   eval      *in43 = *on
071950080422     c                   leavesr
071960080422     c                   endif
071970081126      * se orm export DPD il peso non può essere maggiore del limite stabilito
071980081126     c                   if        *in15 and v1pkg > §3ipkd
071990081126     c                   eval      v1cmsg = msg(92)
072000081126     c                   eval      *in28 = *on
072010081126     c                   eval      *in44 = *on
072020081126     c                   leavesr
072030081126     c                   endif
072040081126      * se orm export DPD obbligatorio il destinatario
072050081127     c                   if        *in15 and ds_v1ccrc = *zeros and
072060081126     c                             v1rsc = *blanks
072070081126     c                   eval      v1cmsg = msg(93)
072080081126     c                   eval      *in28 = *on
072090081211     c                   eval      *in56 = *on
072100081126     c                   leavesr
072110081126     c                   endif
072120010417
072130010417      * Controllo p.o. ritiro del video con quello calcolato
072140020523      * non in chiusura orm
072150050711      * non se p.o. ritiro protetto o se è richiamato da programma
072160050711      * immissione prepagati
072170060109      * se non forzato da me
072180150921      * non controllo se conferma automatica VAS
072190150921     c                   IF        not ConfAuto
072200050711     c                   If        Not *In09 or §rmfpr = 'B'
072210060109     c                   if        wforzapor = *Off
072220010417     C                   if        v1cpos <> *zeros     and
072230010417     C                             v1cpos <> num_v1cpor and
072240020523     C                             *in89 = *off and *in10 = *off
072250010417     C                   movel     msg(62)       v1cmsg
072260010417     C                   seton                                        895928
072270010417     C                   goto      endsrcpor
072280010417     C                   endif
072290010417     C                   if        sav_acrpoa <> *zeros     and
072300010417     C                             sav_acrpoa <> num_v1cpor and
072310150921     C                             *in89 = *off and not *in10
072320010417     C                   movel     msg(62)       v1cmsg
072330010417     C                   seton                                        895928
072340010417     C                   goto      endsrcpor
072350010417     C                   endif
072360131111     C                   if        sav_O95lna <> *zeros       and
072370131111     C                             sav_O95lna <> num_v1cpor   and
072380010417     C                             sav_acrpoa = *zeros    and
072390150921     C                             V1Cpos = *zeros    and
072400150921     C                             *in89 = *off and not *in10
072410010417     C                   movel     msg(62)       v1cmsg
072420010417     C                   seton                                        895928
072430010417     C                   goto      endsrcpor
072440010417     C                   endif
072450060109     c                   EndIf
072460040420     c                   EndIf
072470150921     c                   ENDIF
072480010417
072490010417     C                   movel     v1cpor        sav_v1cpor
072500071031
072510071031      * recupero gli ambiti per il recupero del giro da ACR
072520071031     c                   exsr      sr_caragr
072530010301
072540001016     C     endsrcpor     endsr
072550070920
072560070913      *------------------------------------------------------------------------*
072570070913      * CONTROLLI     * giro
072580070913      *------------------------------------------------------------------------*
072590070913     c     sr_contrcgi   begsr
072600070919
072610070919     c                   clear                   v1dcgi
072620160325     c                   eval      wModGiroAut = *off
072630071105
072640071105      * ricerca giro
072650071105     c                   if        %scan('?':v1ccgi) > *zeros
072660071105     c                   clear                   fidg09ds
072670071105     c                   eval      d09iop0 = 'P01'
072680071105     c                   eval      d09ifgs = num_v1cpor
072690071105     c                   eval      d09idat = *date
072700071105     c                   eval      d09itug = 'R'
072710071105     c                   eval      kpjbu = fidg09ds
072720071105     c                   call      'FIDG09R'
072730071105     c                   parm                    kpjba
072740071105     c                   eval      fidg09ds = kpjbu
072750071105      *     imposto il giro scelto
072760071105     c                   if        d09ocgi <> *blanks
072770071105     c                   eval      v1ccgi = d09ocgi
072780071105     c                   endif
072790071105     c                   endif
072800070919
072810071023      * Aggancio ACR e recupero il giro
072820071031     c                   if        (sav_acrcgi = *blanks and
072830071031     c                              ds_v1ccra <> *zeros)
072840131212     c                             or ($varqta = *on and
072850131212     c                                 sav_acrcgi = v1ccgi)
072860071023     c                   exsr      sr_carcgi
072870071023     c                   endif
072880071023
072890071023      * se giro non immesso da utente imposto il giro recuperato
072900071031      * oppure se variate le quantità
072910071031     c                   if        (sav_v1ccgi = *blanks and
072920071031     c                              sav_acrcgi <> *blanks)
072930131018     c                             or (sav_v1ccgi <> sav_acrcgi
072940131018     c                                 and sav_acrcgi <> *blanks)
072950131212     c                             or ($varqta = *on and
072960131212     c                                (sav_acrcgi = v1ccgi or
072970131212     c                                 sav_v1ccgi = v1ccgi))
072980070920     c                   eval      v1tgi = 'A'
072990070920     c                   eval      v1ccgi = sav_acrcgi
073000070920     c                   eval      sav_v1ccgi = sav_acrcgi
073010070920     c                   endif
073020160325      /free
073030160325       //?Se immissione o copia ORM
073040160325       //?Se variato il mittente e ora è un NON codificato
073050160429       //?Se il giro è rimasto lo stesso di prima
073060160429         IF  (*in01 or *in03) and
073070160429             wModCodCra and ds_V1Ccra = *zeros and
073080160429             V1Ccgi = sav_V1Ccgi and sav_ACRcgi = *blanks;
073090160429           clear V1tgi;
073100160429           clear V1Ccgi;
073110160429           clear sav_V1Ccgi;
073120160429           wModGiroAut = *on;
073130160429           *in28 = *on;
073140160429           *in90 = *on;
073150160429           V1Cmsg = msg(98);
073160160429           leavesr;
073170160325         ENDIF;
073180160325      /end-free
073190070913      *     controllo
073200070913     c                   if        v1ccgi <> *blank
073210070913     c                   exsr      ctrgiro
073220070913      *     giro errato
073230070924     c                   if        d09oerr = '1' and not *in24
073240070913     c                   eval      *in28 = *on
073250070913     c                   eval      *in51 = *on
073260131030     c                   eval      w2cmsg = msg(43)
073270070913     c                   leavesr
073280070913     c                   endif
073290070913      *     imposto la descrizione del giro
073300071011     c                   if        d09ocgi <> *blanks
073310070913     c                   eval      v1dcgi = d09odes
073320070913     c                   endif
073330070913      * se giro diverso da quello salvato imposto che giro manuale
073340070919     c                   if        v1ccgi <> sav_v1ccgi
073350070913     c                   eval      v1tgi = 'M'
073360070913     c                   endif
073370070913     c                   endif
073380070913
073390070913     c                   endsr
073400070913
073410070913      *------------------------------------------------------------------------*
073420070913      *   Controllo e decodifica giro
073430070913      *------------------------------------------------------------------------*
073440070913     c     ctrgiro       begsr
073450070913
073460070913     c                   clear                   fidg09ds
073470070913     c                   eval      d09iop0 = '001'
073480071011     c                   eval      d09ifgs = num_v1cpor
073490070913     c                   eval      d09idat = *date
073500070913     c                   eval      d09icgi = v1ccgi
073510070913     c                   eval      d09itug = 'R'
073520070913     c                   eval      kpjbu = fidg09ds
073530070913     c                   call      'FIDG09R'
073540070913     c                   parm                    kpjba
073550070913     c                   eval      fidg09ds = kpjbu
073560070913
073570070913     c                   endsr
073580070913
073590010202      **********************************************************************
073600010202      * CONTROLLI     * fase *
073610010202      **********************************************************************
073620010202     C     Sr_Contrfao   begsr
073630010202
073640010723      * Se non ho la data e ora fase caricata le imposto con data e ora del sistema
073650140107     c                   IF        *in01
073660050422     C                   if        v1dfo = *zeros
073670050422     C                   eval      v1dfo = oggi_aammgg
073680010723     C                   endif
073690050422     C                   if        v1ofo = *zeros
073700050422     C                   time                    w0140
073710010723     C                   movel     w0140         v1ofo
073720010202     C                   endif
073730010202     C                   endif
073740010202      * Decodifica la fase
073750010202     C                   if        v1fao <> *zeros
073760010202     C                   clear                   TIBS02DS
073770010202     C                   movel     'C'           T02mod
073780010202     C                   movel     knsif         t02sif
073790010202     C                   movel     'FAR'         t02cod
073800010202     C                   movel(p)  v1fao         T02ke1
073810010202     C                   call      'TIBS02R'
073820010202     C                   parm                    KPJBA
073830010202     C                   parm                    TIBS02DS
073840010314     C                   movel     t02uni        dfar
073850010314     C                   movel     d§fardes      v1dfao
073860070706     c                   eval      v1eti = d§farass
073870010202     C                   endif
073880010202
073890010202     C                   endsr
073900130917      /free
073910130917       //--------------------------------------------------------------
073920130917       //?Controllo ora ritiro merce con ora massima per il ritiro.
073930130917       //--------------------------------------------------------------
073940130917       BEGSR contrORRmax;
073950130917
073960131024       //?Controllo ora pronta merce se non è presa da anagrafica clienti
073970131022         SELECT;
073980140320         //?Se ora ritiro a 0 = commissionato
073990140320         //?Se ora fine servizio a 0 = ORM EXPORT no orari servizio
074000140320         //?per ora ritiro a 0 ci pensa la routine CONTRORR
074010140320         //?se non ci sono orari servizio non devo controllare
074020161104         //?non controllo se la data ritiro è maggiore della data pronta merce
074030161028           WHEN  V1orr = 0 or V1ostfs = 0;
074040140414           WHEN  sav_ACRorr <> *zeros and V1orr <= sav_ACRorr;
074050161104           WHEN  inv_v1dar > inv_v1dpm;
074060131022           OTHER;
074070131022       //?Controllo ora pronta merce con ora fine standard
074080161028       //?o con ora max
074090161028           IF  V1orr > V1ostfs;
074100131022             V1Cmsg = msg(12);
074110131022             V1Cmsg = %trim(V1Cmsg) + ' (' +
074120161028                      %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074130161028                      %subst(%editc(V1ostfs:'X'):3:2) + ')';
074140131022             *in28 = *on;
074150131022             *in58 = *on;
074160131022           ENDIF;
074170131022         ENDSL;
074180130917
074190140317       //?Controllo ora immissione ORM con ora fine standard
074200161028       //?o con ora max
074210161028         IF  V1ostfs > 0 and
074220161028             %dec(%subst(%editc(v1oao:'X'):1:4):4:0) > V1ostfs and
074230161028             inv_V1dao = inv_V1dar;
074240131016           V1Cmsg = msg(32);
074250130920           V1Cmsg = %trim(V1Cmsg) + ' (' +
074260161028                    %subst(%editc(V1ostfs:'X'):1:2) + ':' +
074270161028                    %subst(%editc(V1ostfs:'X'):3:2) + ')';
074280130917           *in28 = *on;
074290130917         ENDIF;
074300130917
074310130917       ENDSR;
074320131014
074330131014       //--------------------------------------------------------------
074340131112       //?Controllo orari di apertura.
074350131014       //--------------------------------------------------------------
074360131112       BEGSR OrariApertura;
074370131017
074380131112       //?Richiamo pgm per controllo orari ritiro
074390131112       //?per ora il pgm deve controllare anche che ci sia abbastanza
074400131112       //?spazio nella nota1 e nella nota2 per poterli inserire in
074410131112       //?questi campi per poi inviarli a PDA
074420131112         IF  V1OraAMda > 0 or V1OraAMa > 0 or
074430131112             V1OraPMda > 0 or V1OraPma > 0;
074440131112           clear TRUL03DS;
074450131112           I03hm1 = V1OraAMda;
074460131112           I03hm2 = V1OraAMa;
074470131112           I03hm3 = V1OraPMda;
074480131112           I03hm4 = V1OraPMa;
074490131112           trul03r (TRUL03DS);
074500131112           IF  O03err <> *off;
074510131112             V1Cmsg = O03msg;
074520131112             *in28 = *on;
074530131112             SELECT;
074540131112               WHEN  O03errpos = 1;
074550131112                 H1riga = 12;
074560131112                 H1colo = 21;
074570131112               WHEN  O03errpos = 2;
074580131112                 H1riga = 12;
074590131112                 H1colo = 28;
074600131112               WHEN  O03errpos = 3;
074610131112                 H1riga = 12;
074620131112                 H1colo = 38;
074630131112               WHEN  O03errpos = 4;
074640131112                 H1riga = 12;
074650131112                 H1colo = 45;
074660131112               WHEN  O03errpos = 5;
074670131112                 *in82 = *on;
074680131112               WHEN  O03errpos = 6;
074690131112                 H1riga = 14;
074700131112                 H1colo = 43;
074710131112             ENDSL;
074720131112           ENDIF;
074730131112         ENDIF;
074740131107
074750131014       ENDSR;
074760100811
074770100811       //--------------------------------------------------------------
074780100811       //?Controllo cappario DPD.
074790100811       //--------------------------------------------------------------
074800100811       BEGSR CtrCapDPD;
074810100811
074820100811         clear tisie3ds;
074830100811         isie3dri = oggi_aammgg;
074840100811         IF  inv_v1dar = 0;
074850100811           isie3dsp = oggi_aammgg;
074860100811         ELSE;
074870100811           isie3dsp = inv_v1dar;
074880100811         ENDIF;
074890100811         isie3hsp = %dec(%time());
074900100811         isie3nzd = wNazione;
074910100811         isie3cad = wCapDPD;
074920100811         IF  v1pkg > §3ilmp;
074930100811           isie3srv = 101;
074940100811         ELSE;
074950100811           isie3srv = 136;
074960100811         ENDIF;
074970100811         IF  v1cpoe = *zeros;
074980150305           isie3lnp = DUTpou;
074990100811         ELSE;
075000100811           isie3lnp = v1cpoe;
075010100811         ENDIF;
075020100811         tisie3r (tisie3ds);
075030100811         IF  osie3err <> *blanks;
075040100811           *in28 = *on;
075050100811           *in62 = *on;
075060100811           v1cmsg = 'CAP non trovato su cappario DPD.';
075070100811           leavesr;
075080100811         ENDIF;
075090100811
075100100811       ENDSR;
075110161019
075120161019       //--------------------------------------------------------------
075130161019       //?Controllo Data Pronta Merce.
075140161019       //--------------------------------------------------------------
075150161019       BEGSR ContrDataProntaMerce;
075160161020
075170161020       //?Se sono in immissione o copia e la data è a 0 la imposto = oggi
075180161020         IF  (*in01 or *in03) and V1dpm = 0;
075190161020           V1dpm = oggi_ggmmaa;
075200161028           IF  sav_v1dpm > 0;
075210161028             clear sav_v1dpm;
075220161028           ELSE;
075230161028             sav_V1dpm = V1dpm;
075240161028           ENDIF;
075250161020         ENDIF;
075260161019
075270161019       //?La data è obbligatoria
075280161019         IF  V1dpm = 0;
075290161019           *in28 = *on;
075300161019           *in52 = *on;
075310161020           V1Cmsg = 'Immettere la Data Pronta Merce';
075320161019           leavesr;
075330161019         ENDIF;
075340161019
075350161019       //?Data Valida
075360161019         clear inv_V1dpm;
075370161019         clear wlbdat;
075380161019         G02dat = V1dpm;
075390161019         xsrda8 (wlbdat);
075400161019         IF  G02err = '1';
075410161019           *in28 = *on;
075420161019           *in52 = *on;
075430161020           V1Cmsg = 'Data Pronta Merce errata';
075440161019           leavesr;
075450161019         ENDIF;
075460161019         V1dpm = G02dat;
075470161019         inv_V1dpm = G02inv;
075480170303
075490170303       //?Data non più vecchia di 7 gg da oggi
075500170427       //?No se sono in chiusura ORM
075510170427         IF  not *in10;
075520170303         clear wdata;
075530170303         dataiso = %date(oggi_aammgg:*iso);
075540170303         wdata = %dec(dataiso - %days(7));
075550170303         IF  inv_V1dpm < wdata;
075560170303           *in28 = *on;
075570170303           *in52 = *on;
075580170303           V1Cmsg = 'Data Pronta Merce errata. Inferiore a 7 gg da oggi';
075590170303           leavesr;
075600170303         ENDIF;
075610170427         ENDIF;
075620161115
075630161115       //?Se sono in manutenzione non devo ricalcolare la data ritiro
075640161115       //?se la data pronta merce immessa è inferiore alla data pronta merce dell'ORM
075650161115         IF  *in02 and §OREdpm > 0 and inv_v1dpm <= §OREdpm;
075660161115           leavesr;
075670161115         ENDIF;
075680161028
075690161028         IF  V1dpm <> sav_V1dpm;
075700161028           wModDpm = *on;
075710161028           sav_V1dpm = V1dpm;
075720161028         ENDIF;
075730161019
075740161019       ENDSR;
075750100811      /end-free
075760100204
075770100204      *--------------------------------------------------------------------*
075780100204      * Imposto le personalizzazioni di ORMFLO da tabella PVO
075790100204      *--------------------------------------------------------------------*
075800100204     c     sr_pvo        begsr
075810100204
075820100204     c                   eval      ok_pvo = *off
075830100204
075840100204      * aggancio la tabella per:
075850100204      * --> ordinante lungo 10
075860100205     c                   clear                   dpvo
075870100204     c                   clear                   tibs02ds
075880100204     c                   eval      t02mod = 'C'
075890100204     c                   eval      t02sif = knsif
075900100204     c                   eval      t02cod = 'PVO'
075910100205     c                   eval      t02ke1 = %editc(ds_v1ccor:'X')
075920100204     c                   eval      t02ke2 = 'O'
075930100204     c                   call      'TIBS02R'
075940100204     c                   parm                    kpjba
075950100204     c                   parm                    tibs02ds
075960100204     c                   if        t02err = *blanks
075970100204     c                   eval      dpvo = t02uni
075980100204     c                   eval      OK_pvo = *on
075990100204     c                   endif
076000100204      * --> ordinante lungo 7
076010100204     c                   if        not OK_pvo
076020100204     c                   clear                   dpvo
076030100204     c                   clear                   tibs02ds
076040100204     c                   eval      t02mod = 'C'
076050100204     c                   eval      t02sif = knsif
076060100204     c                   eval      t02cod = 'PVO'
076070100204     c                   eval      t02ke1 = %editc(ds_v1cor12:'X')
076080100204     c                   eval      t02ke2 = 'O'
076090100204     c                   call      'TIBS02R'
076100100204     c                   parm                    kpjba
076110100204     c                   parm                    tibs02ds
076120100204     c                   if        t02err = *blanks
076130100204     c                   eval      dpvo = t02uni
076140100204     c                   eval      OK_pvo = *on
076150100204     c                   endif
076160100204     c                   endif
076170100204      * --> ksc
076180100204     c                   if        not OK_pvo
076190100204     c                   clear                   dpvo
076200100204     c                   clear                   tibs02ds
076210100204     c                   eval      t02mod = 'C'
076220100204     c                   eval      t02sif = knsif
076230100204     c                   eval      t02cod = 'PVO'
076240100204     c                   eval      t02ke1 = %editc(v1ksc:'X')
076250100204     c                   eval      t02ke2 = 'K'
076260100204     c                   call      'TIBS02R'
076270100204     c                   parm                    kpjba
076280100204     c                   parm                    tibs02ds
076290100204     c                   if        t02err = *blanks
076300100204     c                   eval      dpvo = t02uni
076310100204     c                   eval      OK_pvo = *on
076320100204     c                   endif
076330100204     c                   endif
076340100204
076350100204      * se ho trovato uno dei 3 codici in tabella devo personalizzare ORMFLO
076360100204     c                   if        OK_pvo
076370100205
076380100205     c                   if        d§pvocb = 'S'
076390100205     c                   eval      §ormcb = d§pvocb
076400100204     c                   endif
076410100205     c                   if        d§pvofr = 'S'
076420100205     c                   eval      §ormfr = d§pvofr
076430100204     c                   endif
076440100205     c                   if        d§pvoks = 'S'
076450100205     c                   eval      §ormks = d§pvoks
076460100204     c                   endif
076470100205     c                   if        d§pvofd = 'S'
076480100205     c                   eval      §ormfd = d§pvofd
076490100204     c                   endif
076500100205     c                   if        d§pvoic <> *blanks
076510100205     c                   eval      §ormic = d§pvoic
076520100204     c                   endif
076530100204      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
076540100204      * di variazione
076550100204      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
076560100205     c                   if        d§pvosrm = 'S'
076570100205     c                   eval      §orsrm = d§pvosrm
076580100204     c                   eval      v1com = 'S'
076590100204     c                   eval      *in34 = *on
076600100204     c                   clear                   v1dar
076610100204     c                   endif
076620100208     c                   if        *in01 and §rmtla = *blanks
076630100208     c                   eval      v1com = §orcomc
076640100208     c                   endif
076650100812      /free
076660100812       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
076670100812       //?di variazione
076680100812        IF  D§PVOpre = 'M';
076690100812          §ORMpre = D§PVOpre;
076700100812          V1com = 'N';
076710100812          *in34 = *on;
076720100812        ENDIF;
076730100812      /end-free
076740100204
076750100204     c                   endif
076760100208
076770100208     c                   if        *in01 and §rmtla = *blanks and
076780100208     c                             not ok_pvo
076790100208      * devo pulire tutti i dati del campo ormflo
076800100208     c                   clear                   dorm01
076810100208     c                   endif
076820100204
076830100204     c                   endsr
076840071031
076850071031      *--------------------------------------------------------------------*
076860071031      * Carico tabella AGR del po ritiro
076870071031      *--------------------------------------------------------------------*
076880071031     c     sr_caragr     begsr
076890071105
076900071031     c                   clear                   tibs02ds
076910071031     c                   clear                   dagr
076920071112     c                   eval      $noagr = *off
076930071031     c                   eval      t02mod = 'C'
076940071031     c                   eval      t02sif = knsif
076950071031     c                   eval      t02cod = 'AGR'
076960071031     c                   movel(p)  v1cpor        t02ke1
076970071031     c                   call      'TIBS02R'
076980071031     c                   parm                    kpjba
076990071031     c                   parm                    tibs02ds
077000071105     c                   if        t02err = *blanks
077010071031     c                   eval      dagr = t02uni
077020071112     c                   else
077030071112     c                   eval      $noagr = *on
077040071105     c                   endif
077050071031
077060071031     c                   endsr
077070071023
077080071023      *--------------------------------------------------------------------*
077090071023      * Carico il giro da FNACR o da FNORS
077100071023      *--------------------------------------------------------------------*
077110071023     c     sr_carcgi     begsr
077120071023
077130071023     c                   clear                   sav_acrcgi
077140071031
077150071106     c                   eval      kacr1ain = '='
077160071112      * se tabella AGR inserita per filiale ritiro
077170071112     c                   if        $noagr = *off
077180071031      * controllo se devo recuperare il giro Oltre, il giro Sotto o lo Standard
077190071031     c                   if        wpkg > §agrpkgo or wvlm > §agrvlmo or
077200071031     c                             v1bnc > §agrbnco
077210071031     c                   eval      kacr1ain = '>'
077220071031     c                   endif
077230071031     c                   if        wpkg < §agrpkgs and wvlm < §agrvlms and
077240071031     c                             v1bnc < §agrbncs
077250071031     c                   eval      kacr1ain = '<'
077260071031     c                   endif
077270071112     c                   endif
077280071023
077290071023      * da anagrafica clienti ritiro
077300071031     c     kacr1         chain     fnacr13l
077310071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077320071023     c                   eval      sav_acrcgi = acr1cgi
077330071031     c                   else
077340071106     c                   if        kacr1ain <> '='
077350071106     c                   eval      kacr1ain = '='
077360071031     c     kacr1         chain     fnacr13l
077370071031     c                   if        %found(fnacr13l) and acr1cgi <> *blanks
077380071031     c                   eval      sav_acrcgi = acr1cgi
077390071031     c                   endif
077400071031     c                   endif
077410071031     c                   endif
077420071031
077430071023      * se conferma ORM fisso recupero il giro dal fisso se impostato
077440071031      * quindi solo immissione
077450161117     c                   if        §rmfpr = 'S'
077460071023     c     kors          chain     fnors11l
077470071023     c                   if        %found(fnors11l) and ors1cgi <> *blanks
077480071023     c                   eval      sav_acrcgi = ors1cgi
077490071023     c                   endif
077500071023     c                   endif
077510071023
077520071023     c                   endsr
077530100217
077540100218      /free
077550100218       //--------------------------------------------------------------------
077560100218       //?Controllo se OK tutti i dati per il preavviso.
077570100218       //--------------------------------------------------------------------
077580100218       BEGSR sr_Preavviso;
077590100217
077600100218       //?Imposto la chiave per il FNVAOE rcd 'M '
077610100218         k_vaoecor = sav_vaocor;
077620100218         k_vaoepoe = sav_vaopoe;
077630100218         k_vaoensr = sav_vaonsr;
077640100218         k_vaoenor = sav_vaonor;
077650100218         k_vaoenrv = sav_vaonrv;
077660100218         k_vaoetrc = 'M ';
077670100218       //?Aggancio il rcd
077680100812         chain  %kds(k06fnvaoe) FNVAOE1L;
077690100218
077700100218         SELECT;
077710100218       //?Se non trovo il rcd e non c'è il flag di preavviso mail OK
077720100218           WHEN  not %found(FNVAOE1L) and §ormpre <> 'M';
077730100218             clear vaoedati;
077740100218       //?Se trovo il rcd e c'è il flag di preavviso mail OK e controllo la mail
077750100218           WHEN  %found(FNVAOE1L) and §ormpre = 'M';
077760100218             wmail = vaoedati;
077770100218             exsr sr_CtrMail;
077780100223             vaoedati = wmail;
077790100223             IF  $errpre;
077800100223               wpremsg = §emlmsgo;
077810100225               *in94 = *on;
077820100223             ENDIF;
077830100218       //?Se non trovo il rcd e c'è il flag di preavviso mail errore
077840100218           WHEN  not %found(FNVAOE1L) and §ormpre = 'M';
077850100218             clear vaoedati;
077860100223             $errpre = *on;
077870100225             wpremsg = 'Dati incongruenti. ' +
077880100225                       'ORM con preavviso senza indirizzo mail';
077890100218       //?Se trovo il rcd e non c'è il flag di preavviso mail errore
077900100218           WHEN  %found(FNVAOE1L) and §ormpre <> 'M';
077910100223             $errpre = *on;
077920100225             wpremsg = 'Dati incongruenti. ' +
077930100225                       'ORM senza preavviso con indirizzo mail';
077940100218         ENDSL;
077950100218
077960100218       ENDSR;
077970100218
077980100218       //--------------------------------------------------------------------
077990100218       //?Controllo validità indirizzo mail.
078000100218       //--------------------------------------------------------------------
078010100218       BEGSR sr_CtrMail;
078020100218
078030100218         clear dsemail;
078040100223         §emlindi = wmail;
078050100218         xemail (dsemail);
078060100223         $errpre = (§emlerro <> '0');
078070100223         IF  not $errpre;
078080100223           wmail = §emlindo;
078090100223         ENDIF;
078100100218
078110100218       ENDSR;
078120100218
078130100218       //--------------------------------------------------------------------
078140100218       //?Richiesta dati preavviso con window.
078150100218       //--------------------------------------------------------------------
078160100218       BEGSR sr_DatiPre;
078170100218
078180100218         $endpre = *off;
078190100223         *in90 = *off;
078200100223         $okf6pre = *off;
078210100224         *in28 = *off;
078220100218
078230100218       //?emetto la videata con i dati dell'ORM
078240100218         write fior051;
078250100322         IF  $errpre;
078260100322           *in28 = *on;
078270100322         ENDIF;
078280100218       //?emetto la window dove richiedo i dati del preavviso
078290100218         DOW not $endpre;
078300100218           exfmt fior05pre;
078310100218           clear  wpremsg;
078320100218           *in28 = *off;
078330100225           *in94 = *off;
078340100223         SELECT;
078350100223       //?F12-Ritorno
078360100223           WHEN  *inkl;
078370100218             $endpre = *on;
078380100223             *in28 = *off;
078390100223             *in90 = *on;
078400100223             $errpre = *off;
078410100223       //?F6-Conferma
078420100223           WHEN  *inkf;
078430100225             exsr sr_CtrDatiPre;
078440100223             IF  not *in28;
078450100223               IF  wpresino = 'NO';
078460100223                 clear §ormpre;
078470100325           //?Se non è più ORM con preavviso MAIL pulisco il campo di ORM commissionato
078480100325           //?e lo sblocco, così il pgm ricalcola se commissionato o meno e l'utente può
078490100325           //?variarlo
078500100325                 clear V1com;
078510100325                 *in34 = *off;
078520100223               ENDIF;
078530100223               IF  wpresino = 'SI';
078540100223                 §ormpre = 'M';
078550100325           //?Se è ORM con preavviso MAIL imposto che ORM NON commissionato
078560100325           //?e sblocco il campo così l'utente non può variarlo
078570100325                 V1com = 'N';
078580100325                 *in34 = *on;
078590100223               ENDIF;
078600100223               vaoedati = wpremail;
078610100223               $endpre = *on;
078620100223               $okf6pre = *on;
078630100223               $errpre = *off;
078640100223             ENDIF;
078650100223           OTHER;
078660100218       //?controllo i dati
078670100225             exsr sr_CtrDatiPre;
078680100223           ENDSL;
078690100218         ENDDO;
078700100218
078710100218       ENDSR;
078720100218
078730100218       //--------------------------------------------------------------------
078740100218       //?Controlli dati preavviso.
078750100218       //--------------------------------------------------------------------
078760100225       BEGSR sr_CtrDatiPre;
078770100218
078780100218         *in28 = *off;
078790100218
078800100218       //?se preavviso = 'SI' deve esserci la mail
078810100218         IF  wpresino = 'SI' and wpremail = *blanks;
078820100218           *in28 = *on;
078830100225           wpremsg = 'Dati incongruenti. ' +
078840100225                     'ORM con preavviso senza indirizzo mail';
078850100218           leavesr;
078860100218         ENDIF;
078870100218       //?se preavviso = 'NO' NON deve esserci la mail
078880100218         IF  wpresino = 'NO' and wpremail <> *blanks;
078890100218           *in28 = *on;
078900100225           wpremsg = 'Dati incongruenti. ' +
078910100225                     'ORM senza preavviso con indirizzo mail';
078920100218           leavesr;
078930100218         ENDIF;
078940100218       //?la mail deve essere valida
078950100218         IF  wpremail <> *blanks;
078960100223           wmail = wpremail;
078970100218           exsr sr_CtrMail;
078980100223           IF  $errpre;
078990100223             $errpre = *off;
079000100218             *in28 = *on;
079010100225             *in94 = *on;
079020100218             wpremsg = §emlmsgo;
079030100218             leavesr;
079040100218           ENDIF;
079050100223           wpremail = wmail;
079060100218         ENDIF;
079070100218
079080100218       ENDSR;
079090100218
079100100218      /end-free
079110010305
079120030606      **********************************************************************
079130030611      * Imposta flag x Orm commissionato *
079140030606      **********************************************************************
079150030611     c     Sr_OrmCom     BegSr
079160030611
079170030611      * Se p.o. emissione a 0 vuol dire che sono in immissione
079180150305      * in questo caso lo imposto già uguale al p.o. utente
079190030611      * per poter fare il controllo tra p.o. ritiro e p.o. emissione
079200030611     c                   If        v1cpoe = *zeros
079210150305     c                   z-add     DUTpou        v1cpoe
079220030611     c                   EndIf
079230050323      * RICORDA!!!!
079240050324      * Un orm da INTERNET/FILE con codice ordinante e p.o. emissione = a p.o. ritiro
079250050323      * non è un ORM commissionato
079260060103      * Un orm FISSO con codice ordinate e p.o. emissione = a p.o. ritiro (x forza)
079270060103      * non è un ORM commissionato (in automatico l'utente può variarlo)
079280100205
079290100205      * come prima cosa imposto il campo di ORM commissionato se lo recupero dalla
079300100205      * personalizzazione del cliente
079310100205     c                   if        v1com = *blanks
079320100205     c                             and d§pvocomc <> *blanks
079330100205     c                   eval      v1com = d§pvocomc
079340100205     c                   leavesr
079350100205     c                   endif
079360030606
079370030606      * Imposto che è un orm commissionato se:
079380030606      * - Cod.mittente <> Cod.ordinante
079390030606      * - Ordinante presente ma non codificato o 8888 o 9999
079400130911      * - P.o. ritiro <> p.o. emissione
079410130911      *   ma solo se mittente NON codificato
079420030606
079430050323If  1c                   If        V1Com = *Blanks  and
079440030902     c                                             ((Ds_V1Cor12 > *Zeros and
079450040707     c                                              Ds_V1cra12 <> Ds_V1Cor12 and
079460050323     c                                              V1ctco <> 'I' and
079470060103     c                                              V1ctco <> 'F' and
079480060103     c                                              V1ctco <> 'S')
079490030902     c                                              or
079500030902     c                                             (V1rso <> *Blanks   and
079510030902     c                                              (Ds_V1ccor = *Zeros or
079520030902     c                                               Ds_V1cor2 = 8888   or
079530050323     c                                               Ds_V1cor2 = 9999)))
079540030606     c                   Eval      V1Com = 'S'
079550030902     c                   Goto      Endormcom
079560030606    1c                   EndIf
079570050323If  1c                   If        V1Com = *Blanks  and
079580030902     c                                              Num_V1cPor <> V1cpoe
079590130911     c                                         and  ds_V1Ccra = 0
079600030902     c                   Eval      V1Com = 'S'
079610030902     c                   Goto      Endormcom
079620030902    1c                   EndIf
079630030611
079640030902     c     endormcom     EndSr
079650030611      **********************************************************************
079660030611      * Controllo se Ok tutti i dati
079670030611      **********************************************************************
079680030611     c     Sr_ContrCom   BegSr
079690030606
079700030606      * Se è un orm commissionato l'ordinante è obbligatorio
079710030606      * se l'ordinante non c'è devo emettere una window dove chiedo chi è l'ordinante
079720030606If  1c                   If        V1Com = 'S' and
079730030606     c                             V1rso = *Blanks and Ds_V1ccor = *Zeros
079740030610     c                   Eval      whocor = 'S'
079750030610     c                   Eval      *In90 = *On
079760131106      /free
079770131106       //?pulisco il campo del commissionato così viene rifatto il calcolo
079780131106       //?se ORM commissionato o meno
079790131106         clear V1com;
079800131106      /end-free
079810030611      * se non sto facendo una conferma di massa emetto la window
079820030611If 1ac                   If        §rmtla <> '0' and §rmtla <> '1'
079830030610Do  2c                   Do        *Hival
079840030610     c                   Clear                   wdcor
079850030606     c                   Exfmt     Fior05o
079860030610If  3C                   If        *Inkl
079870030606     c                   Leave
079880030610    3c                   EndIf
079890030610If  3c                   If        wdcor <> *Blanks
079900030609     c                   Leave
079910030610    3c                   EndIf
079920030610    2c                   EndDo
079930030611      * se sto facendo una conferma di massa errore
079940030611  x1ac                   Else
079950030611     C                   seton                                        28
079960030611   1ac                   EndIf
079970030606    1c                   EndIf
079980030610
079990030610      * devo controllare il referente e il n. telefono
080000030612If  1c                   If        V1Com = 'S' and Not *In28
080010030610     C                   exsr      Sr_Contrref
080020030610    1c                   EndIf
080030030606
080040030606     c                   EndSr
080050070706
080060070706      *--------------------------------------------------------------------*
080070070706      * Calcolo peso e volume
080080070706      *--------------------------------------------------------------------*
080090070706     c     sr_pesvol     begsr
080100071108
080110071108     c                   clear                   wvlmaut
080120071108     c                   clear                   wpkgaut
080130071108     c                   clear                   wvlmmtc
080140071108     c                   clear                   wpkgmtc
080150071108     c                   clear                   wvlmblc
080160071108     c                   clear                   wpkgblc
080170070706
080180070706     c                   eval      wpkg = v1pkg
080190070706     c                   eval      wvlm = v1vlm
080200070706
080210070706      * se non inserito peso a video lo calcolo per memorizzarlo su fnorg00f
080220070706     c                   if        wpkg = *zeros
080230070706     c                   select
080240070918 b3  c                   when      v1bnc <> *zeros
080250070918     c                   eval      wpkg = v1bnc / d§dftbnc * d§dftpkg
080260070706 b2  c                   when      v1vlm <> *zeros
080270070918     c                   eval      wpkg = v1vlm * d§dftpkg
080280070706 e3  c                   endsl
080290070706      * se troppo alto il peso lo imposto al massimo
080300070706     c                   if        wpkg > 999999,9
080310070706     c                   eval      wpkg = 999999,9
080320070706     c                   endif
080330070706     c                   endif
080340070706
080350070706      * se non inserito volume a video lo calcolo per memorizzarlo su fnorg00f
080360070706     c                   if        wvlm = *zeros
080370070706     c                   select
080380070918 b3  c                   when      v1bnc <> *zeros
080390070918     c                   eval(h)   wvlm = v1bnc / d§dftbnc
080400070918 b2  c                   when      v1pkg <> *zeros
080410070918     c                   eval(h)   wvlm = v1pkg / d§dftpkg
080420070706 e3  c                   endsl
080430070706      * se troppo alto il volume lo imposto al massimo
080440070706     c                   if        wvlm > 99,999
080450070706     c                   eval      wvlm = 99,990
080460070706     c                   endif
080470070706     c                   endif
080480070918
080490070918      * se inseriti gli automezzi devo convertirli in m³ per poi memorizzare
080500070918      * il dato più alto su FNORG
080510070918      * AUTOTRENO
080520070918     c                   if        v1att > *zeros
080530070918     c                   eval      wvlmaut = v1att * d§dftaut
080540070918     c                   eval      wpkgaut = wvlmaut * d§dftpkg
080550070918     c                   if        wvlmaut > 99,999
080560070918     c                   eval      wvlmaut = 99,999
080570070918     c                   endif
080580070918     c                   if        wpkgaut > 999999,9
080590070918     c                   eval      wpkgaut = 999999,9
080600070918     c                   endif
080610070918     c                   endif
080620070918      * MOTRICE
080630070918     c                   if        v1mtc > *zeros
080640070918     c                   eval      wvlmmtc = v1mtc * d§dftmtc
080650070918     c                   eval      wpkgmtc = wvlmmtc * d§dftpkg
080660070918     c                   if        wvlmmtc > 99,999
080670070918     c                   eval      wvlmmtc = 99,999
080680070918     c                   endif
080690070918     c                   if        wpkgmtc > 999999,9
080700070918     c                   eval      wpkgmtc = 999999,9
080710070918     c                   endif
080720070918     c                   endif
080730070918      * BILICO
080740070918     c                   if        v1blc > *zeros
080750070918     c                   eval      wvlmblc = v1blc * d§dftblc
080760070918     c                   eval      wpkgblc = wvlmblc * d§dftpkg
080770070918     c                   if        wvlmblc > 99,999
080780070918     c                   eval      wvlmblc = 99,999
080790070918     c                   endif
080800070918     c                   if        wpkgblc > 999999,9
080810070918     c                   eval      wpkgblc = 999999,9
080820070918     c                   endif
080830070918     c                   endif
080840070918
080850070918      * cerco il volume più alto
080860070918     c                   if        wvlmaut > wvlm
080870070918     c                   eval      wvlm = wvlmaut
080880070918     c                   endif
080890070918     c                   if        wvlmmtc > wvlm
080900070918     c                   eval      wvlm = wvlmmtc
080910070918     c                   endif
080920070918     c                   if        wvlmblc > wvlm
080930070918     c                   eval      wvlm = wvlmblc
080940070918     c                   endif
080950070918
080960070918      * cerco il peso più alto
080970070918     c                   if        wpkgaut > wpkg
080980070918     c                   eval      wpkg = wpkgaut
080990070918     c                   endif
081000070918     c                   if        wpkgmtc > wpkg
081010070918     c                   eval      wpkg = wpkgmtc
081020070918     c                   endif
081030070918     c                   if        wpkgblc > wpkg
081040070918     c                   eval      wpkg = wpkgblc
081050070918     c                   endif
081060071031
081070071108      * se sono in modifica e non devo fare una proposta di variazione
081080071031      * controllo se variati peso/volume/bancali per ricalcolare il giro in base gli ambiti
081090071031      * per il peso e il volume devo utilizzare i valori sviluppati dal pgm
081100071031     c                   eval      $varqta = *off
081110150609     c     v1cpor        Lookup    skpog                                  30
081120071108     c                   select
081130080314     c                   when      *in30 and *in02 and (v1fao = 400
081140080314     c                             or v1fao = 410)
081150071108     c                   when      not *in30 and *in02
081160071108     c                   other
081170131202     c                   if        (savpkg <> wpkg or savvlm <> wvlm or
081180131202     c                              savbnc <> v1bnc) and
081190131202     c                             (savpkg > 0 or savvlm > 0 or
081200131202     c                              savbnc > 0)
081210071031     c                   eval      $varqta = *on
081220071031     c                   eval      savpkg = wpkg
081230071031     c                   eval      savvlm = wvlm
081240071031     c                   eval      savbnc = v1bnc
081250071031     c                   endif
081260071108     c                   endsl
081270161025      /free
081280161025       //?Controllo se ho variato il peso, serve per ricalcolare la data ritiro
081290161025       //?ma solo se data ritiro a video non impostata da utente
081300161025         IF  old_wpkg <> wpkg;
081310161026           IF  old_wpkg > 0 and inv_newv1dar >= inv_v1dar;
081320161025             wModPeso = *on;
081330161025           ENDIF;
081340161025           old_wpkg = wpkg;
081350161025         ENDIF;
081360161025      /end-free
081370070706
081380070706     c                   endsr
081390070706
081400010305      **********************************************************************
081410010305      * CONTROLLI     * cerco causale chiusura e decodifico *
081420010305      **********************************************************************
081430010305     C     Sr_Contrcau   begsr
081440010305
081450010305     C                   eval      kpoe = ORMpoe
081460010305     C                   eval      knsr = ORMnsr
081470010305     C                   eval      knor = ORMnor
081480010305     C                   eval      knrv = ORMnrv
081490010305     C                   eval      kdai = ORMdfo
081500010305     C                   eval      kori = ORMofo
081510010305     C                   eval      kfar = ORMfao
081520110418     c     kfnorn1       setll     fnorf01l
081530110418     c                   do        *hival
081540110418     c     kfnorn1       reade(n)  fnorf01L
081550110418     c                   if        %eof(fnorf01L)
081560110418     c                   leave
081570110418     c                   endif
081580010305     C                   eval      v1cau = orfcar
081590110418     C                   enddo
081600010305
081610010305     C                   clear                   dcmr
081620010305     C                   clear                   TIBS02DS
081630010305     C                   movel     'C'           T02mod
081640010305     C                   movel     knsif         t02sif
081650010305     C                   movel     'CMR'         t02cod
081660010305     C                   movel(p)  v1cau         T02ke1
081670010305     C                   call      'TIBS02R'
081680010305     C                   parm                    KPJBA
081690010305     C                   parm                    TIBS02DS
081700010305     C                   movel     t02uni        dcmr
081710010305     C                   movel     d§cmrdes      v1dcau
081720010309
081730010309     C                   eval      *in13 = (v1cau <> *blanks)
081740070123
081750070123      * mi salvo il p.o. e il n. distinta
081760080226     c                   eval      kdstfgs = orffgs
081770080226     c                   eval      kdstnfv = orfndc
081780010305
081790010305     C                   endsr
081800001019      **********************************************************************
081810001023      * CONTROLLI     * se già stampato deve confermare *
081820001019      **********************************************************************
081830001019     C     Sr_Contrstp   begsr
081840001019
081850031201     C                   if        v1dst  >  *zeros
081860001019     C                   movel     'NO'          wfstp
081870010221     C     emetto        tag
081880001019     C                   exfmt     fior05w
081890001107     C                   if        wfstp = 'SI'
081900031125     C                   clear                   FIOR16ds
081910041013     C                   eval      R16ris = 'R'
081920030623     C                   clear                   trul90ds
081930030623     C                   movel     'S'           D90rso
081940030623     C                   call      'TRUL90R'
081950010314     C                   parm                    kpjba
081960030623     C                   parm                    trul90ds
081970001108
081980001108      * F3=Fine
081990030623     C                   if        D90f3 = '1'
082000010314     C                   goto      endsrstampa
082010010314     C                   endif
082020031125
082030090202      /free
082040090202         reset ds5P;
082050090202         if  %subst(knmus : 1 : 3) = 'EDP';
082060090202           TBLkey = 'O' + 'EDP';
082070090202         else;
082080090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
082090090202         endif;
082100090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
082110090202         if  %found( TABEL00F );
082120090202           ds5P = TBLuni;
082130090202         endif;
082140090202      /end-free
082150031201
082160031125     C                   movel     D90mdo        R16mdo
082170031125     C                   movel     D90pro        R16oqo
082180001108
082190031201     c                   eval      R16poe = Ppoe
082200031201     c                   eval      R16nsr = Pnsr
082210031201     c                   eval      R16nor = Pnor
082220031201     c                   eval      R16nrv = Pnrv
082230031125     C                   movel     v1cpor        R16por
082240031125     C                   eval      R16dst = v1dst
082250031125     C                   eval      R16dar = inv_v1dar
082260001108
082270001108     C                   movel     kpjbu         §kpjbu
082280031125     C                   movel(p)  FIOR16ds      kpjbu
082290090202     C                   call      §5Ppgm
082300031125     C                   parm                    KPJBA
082310031125     C                   movel     kpjbu         FIOR16DS
082320001108     C                   movel     §kpjbu        kpjbu
082330001019     C                   endif
082340001019     C                   endif
082350001019
082360001108     C     endsrstampa   endsr
082370001011      **********************************************************************
082380001011      * F6 - Conferma
082390001011      **********************************************************************
082400001011     C     Sr_Conferma   BEGSR
082410140422
082420140422      /free
082430140423        werralert = *off;
082440140422       //?Se conferma ORM da clienti come prima cosa verifico gli alert
082450140507        IF  §rmfpr = 'C' or
082460150305       //?oppure se sono in immissione ORM
082470140507       //?e non è stato richiamato (quindi immissione ORM MANUALE)
082480160208       //?o copia ORM
082490160208            ((*in01 or *in03 )and §RMtla = *blanks);
082500140423          exsr AlertORM;
082510140423          IF  *in28;
082520140423            werralert = *on;
082530140423            leavesr;
082540140423          ENDIF;
082550140422        ENDIF;
082560160406
082570160413       //?Se immissione ORM manuale o copia
082580160406         IF  (*in01 or *in03) and
082590160406             (V1Ctco = 'M' or V1Ctco = 'E');
082600160406         //?se ho almeno 1 dei 2 codici (ordinante - mittente) e non ho emesso
082610160406         //?la WIN (non ho mai dati un F6 sulla WIN)
082620160406         //?e non ho i dati mail e/o sms provo a recuperarli
082630160406         //?dall'anagrafica clienti
082640160406           IF  (ds_V1Ccor > 0 or ds_V1Ccra > 0) and
082650160406                W04mail = *blanks and W04sms = *blanks and
082660160406                not wOkF6F13;
082670160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
082680160406             exsr RecDatiConf;
082690160406           ENDIF;
082700160406       //?se ho i dati relativi alla conferma prenotazione ritiro
082710160405       //?imposto i dati per scrivere il rcd 'G' di fnore e il file spia
082720160406       //?da conferma VAS ho già tutto impostato
082730160406           IF  (w04sms <> *blanks or w04mail <> *blanks);
082740160406             SELECT;
082750160406             WHEN  W04sms <> *blanks and W04mail <> *blanks;
082760160406               §OREfsco = 'S';
082770160406               §OREfmco = 'S';
082780160406             WHEN  W04sms <> *blanks;
082790160406               §OREfsco = 'S';
082800160406               §OREfmco = 'N';
082810160406             WHEN  W04mail <> *blanks;
082820160406               §OREfmco = 'S';
082830160406               §OREfsco = 'N';
082840160406             ENDSL;
082850160406             wconferma = *on;
082860160406           ENDIF;
082870160310         ENDIF;
082880140422      /end-free
082890001011
082900071108     ?* Scrive
082910001109     C                   if        *in01 = *on or *in03 = *on
082920001117     C                   clear                   fnorm000
082930050422     C  n03              exsr      Sr_Numor
082940050422     C   28              goto      endsrconf
082950001011     C                   exsr      Sr_Carfile
082960001108      * Scrive la fase
082970001017     C                   exsr      Sr_Carfilef
082980001108      * Scrive le note
082990150713     c                   IF        not ConfAuto
083000001026     C                   exsr      Sr_Carnote
083010150713     c                   ENDIF
083020070919      * scrivo fnorg (GIRO)
083030151016     c                   exsr      sr_wrtorg
083040100218      /free
083050160310
083060170524       //?Se dovevo posticipare
083070170524        Anticipato = *off;
083080170524        clear ggAnticipo;
083090170524       //?Imposto se l'utente ha anticipato la data ritiro
083100170615       //?se c'era la 'A' di anticipa
083110170524        IF  wAnticipa and ORMdar < DarCalcolata;
083120170524          Anticipato = *on;
083130170615        ENDIF;
083140170525       //?e di quanti gg lavorativi ha anticipato
083150170615       //?con o senza 'A' di anticipa, in alcuni casi possono anticipare alla
083160170615       //?data immissione senza che sia visualizzata la 'A', questo soprattutto in
083170170615       //?copia ORM oppure se immissione prepagato
083180170525        clear wdat8;
083190170525        clear tfptfa;
083200170525        p_tfa = ORMpor;
083210170525        data1 = DarCalcolata;
083220170525        data2 = ORMdar;
083230170525        xsrlav8(wdat8);
083240170525       //?dal pgm di calcolo differenza tra le 2 date torna sempre almeno 1, io lo tolgo
083250170525        ggAnticipo = giolav -1;
083260100225       //?Se conferma ORM da clienti scrivo eventuale estensione inviata dal cliente
083270100225        IF  §rmfpr = 'C';
083280100218          exsr sr_fnore;
083290100218        ENDIF;
083300131014       //?Se presenti i dati scrivo fnore
083310140422        IF  w03mail <> *blanks;
083320140422          clear old_w03mail;
083330140422        ENDIF;
083340140422        IF  w03sms <> *blanks;
083350140422          clear old_w03sms;
083360140422        ENDIF;
083370160405        IF  w04mail <> *blanks;
083380160405          clear old_w04mail;
083390160310        ENDIF;
083400160405        IF  w04sms <> *blanks;
083410160405          clear old_w04sms;
083420160310        ENDIF;
083430140422        exsr aggFNORE;
083440160322
083450160322       //?Se conferma ORM da clienti per orm da internet
083460160322       //?memorizzo mail sull'anagrafica clienti ritiro, così anche il relativo flag
083470160322       //?anche sms
083480160712       //?Solo però se richiesto da Ordinante
083490160712        IF  §rmfpr = 'C' and ORMtco = 'I' and ORMcor > 0 and §OREmemo = 'S';
083500160712          exsr aggACR;
083510160322        ENDIF;
083520160314
083530160314       //?Se ORM con Conferma Prenotazione Ritiro e sono in immissione manuale
083540160314       //?cioè ORM Telefonico o Mail/Fax
083550160314       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
083560160314       //?servono nell'invio dell'alert
083570160318       //?ma solo se devo creare l'alert conferma prenotazione ritiro
083580160318        IF  §RMfpr <> 'C' and (ORMtco = 'M' or ORMtco = 'E') and
083590160318            wconferma;
083600160314         //?imposto orario ritiro standard
083610160314          clear dOREor;
083620160314          §OREori = V1ostis;
083630160314          §OREorf = V1ostfs;
083640170609         //?Se la data ritiro è uguale a data immissione
083650170609          IF  ORMdar = ORMdao;
083660160314         //?Se l'ora pronta merce è maggiore
083670160314         //?dell'ora inizio servizio
083680160314         //?e minore dell'ora fine
083690160314         //?la imposto come orario inzio
083700160314          IF  V1orr > V1ostis and
083710160314              V1orr < V1ostfs;
083720160314            §OREori = V1orr;
083730160314          ENDIF;
083740160314         //?Se l'ora pronta merce è maggiore
083750160314         //?dell'ora fine servizio
083760160314         //?la imposto come orario fine
083770160314          IF  V1orr > V1ostfs;
083780160314            §OREorf = V1orr;
083790160314          ENDIF;
083800170609          ENDIF;
083810160314          clear FNORE000;
083820160314          OREpoe = ORMpoe;
083830160314          OREnsr = ORMnsr;
083840160314          OREnor = ORMnor;
083850160314          OREnrv = ORMnrv;
083860160314          OREtrc = 'OR';
083870160314          OREdati = dOREor;
083880160314          write FNORE000;
083890160517          feod  FNORE01L;
083900160314        ENDIF;
083910160217
083920160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
083930160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
083940160224       //?il NPR lo recupero solo se non sto facendo conferma ORM da INTERNET
083950160426       //?e se non è già stato calcolato
083960160426        IF  §rmfpr <> 'C' or
083970160426           (§rmfpr = 'C' and ORMtco <> 'I') or
083980160426           (§rmfpr = 'C' and ORMtco = 'I' and not WebNPR);
083990160217          wnpr = GetNPR(parametroNPR);
084000160217       //?Se il rif. ORM è vuoto imposto il NPR
084010160217          IF  ORMrfa = *blanks;
084020160317            ORMrfa = wnpr;
084030160217          ENDIF;
084040160314       //?scrivo FNORE00F rcd NP
084050160314       //?per memorizzare il NPR
084060160314          clear FNORE000;
084070160314          OREpoe = ORMpoe;
084080160314          OREnsr = ORMnsr;
084090160314          OREnor = ORMnor;
084100160314          OREnrv = ORMnrv;
084110160314          OREtrc = 'NP';
084120160314          OREdati = wnpr;
084130160314          write FNORE000;
084140160517          feod  FNORE01L;
084150160217        ENDIF;
084160100218      /end-free
084170100218
084180001108     C                   write     fnorm000
084190160509     C                   feod      fnorm01L
084200100218
084210100218      /free
084220100218       //?dopo aver scritto l'ORM se previsto il preavviso richiamo programma
084230100218       //?per invio mail di preavviso
084240100218        IF  §ormpre = 'M';
084250100224         clear fior52ds;
084260100224         I52poe = ORMpoe;
084270100224         I52nsr = ORMnsr;
084280100224         I52nor = ORMnor;
084290100224         I52nrv = ORMnrv;
084300100224         fior52r (kpjba:fior52ds);
084310100218        ENDIF;
084320140422
084330140428       //?dopo aver scritto l'ORM se previsto Alert
084340151008       //?e/o previsto invio mail x conferma accettazione ORM
084350140428       //?scrivo file spia
084360151008       IF  walert or wconferma;
084370140422         exsr wrtspia;
084380140422       ENDIF;
084390160217
084400100218      /end-free
084410100218
084420010202     C                   eval      §RMpoeo = ormpoe
084430010202     C                   eval      §RMnsro = ormnsr
084440010202     C                   eval      §RMnoro = ormnor
084450010202     C                   eval      §RMnrvo = ormnrv
084460010704
084470010704      * Richiamo il programma immissione prepagati
084480050324      * solo se sono da conferma ORM VAO, se l'orm è di tipo 'P'
084490050324      * e se è un ORM locale
084500050324      * se l'ORM non è locale il prepagato lo creo al momento della conferma ORM
084510050324      * commissionati (Un ORM prepagato non locale è SEMPRE commissionato)
084520010704     C                   if        §RMfpr = 'C' and
084530011012     C                             ORMtor = 'P'
084540080407      * se po emissione diverso da po ritiro controllo se il po ritiro è la capofila
084550080407      * del po emissione
084560080407     c                   if        ormpoe <> ormpor
084570080407     c                   clear                   fnlv55ds
084580080407     c                   eval      d55tpt = '6'
084590080407     c                   eval      d55lin = ormpoe
084600080407     c                   eval      d55drf = oggi_aammgg
084610080407     c                   call      'FNLV55R'
084620080407     c                   parm                    fnlv55ds
084630080407     c                   if        d55err <> *blanks
084640080407     c                   clear                   d55tfa
084650080407     c                   endif
084660080407     c                   endif
084670080407      * questo per parare gli errori dovuti a orm emessi da filiali in £6 con ritiro
084680080407      * presso la capofila
084690080407     c                   if        ormpoe = ormpor or ormpor = d55tfa
084700010704     C                   exsr      Sr_Immprep
084710010704
084720010704      * Non è stato creato il prepagato
084730010704      * devo annullare l'ORM appena creato
084740010704     C                   if        okbolla = *blanks
084750010704     C                   eval      §RMerr = '5'
084760010704      * Deleto l'ORM
084770010704     C                   clear                   fior30ds
084780010704     C                   eval      i30mod = 'D'
084790010704     C                   eval      i30poe = ormpoe
084800010704     C                   eval      i30nsr = ormnsr
084810010704     C                   eval      i30nor = ormnor
084820010704     C                   eval      i30nrv = ormnrv
084830010704     C                   call      'FIOR30R'
084840010704     C                   parm                    kpjba
084850010704     C                   parm                    fior30ds
084860010704
084870010704     C                   endif
084880080407     c                   endif
084890010704     C                   endif
084900010704
084910001026     C                   endif
084920001011
084930071108     ?* Aggiorna
084940010919   1bC                   if        *in02 = *on
084950071108     c                   select
084960071126      * sono in conferma proposte di variazione
084970071126     C                   when      §rmfpr = 'P'
084980071126     c                   eval      $modifica = *on
084990071126     C  n10              exsr      Sr_Storico
085000140227      /free
085010151023       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
085020151023       //?da rcd già vincolato da questo lavoro
085030151023         unlock FNORG01L;
085040140227       //?Richiamo pgm di gestione note per convalidare la proposta di variazione
085050140227       //?della nota
085060140227         chain (§RMpoe:§RMnsr:§RMnor:§RMnrv:§RMdtv:§RMorv) FNORPT1L;
085070140227         IF  %found(FNORPT1L);
085080140227           clear fiort1ds;
085090140227           IORT1mod = 'P';
085100140227           IORT1poe = ORMpoe;
085110140227           IORT1nsr = ORMnsr;
085120140227           IORT1nor = ORMnor;
085130140227           IORT1nrv = ORMnrv;
085140140227           IORT1dim = §RMdtv;
085150140227           IORT1oim = §RMorv;
085160140227           IORT1pgm = 'FIOR05R';
085170140227           §kpjbu = kpjbu;
085180140227           fiort1r (kpjba:fiort1ds);
085190140227           kpjbu = §kpjbu;
085200140227         ENDIF;
085210151023       //?Rialloco FNORG
085220151023         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
085230140227      /end-free
085240140227
085250010205      * Non sono in conferma orm da altri p.o.
085260071108     C                   when      §rmfpr <> 'O'
085270071108      * è stato modificato qualcosa
085280131018     c                             and (vidnew <> vidold
085290160406     c                               or vidnew2 <> vidold2
085300160406     c                               or vidnewA <> vidoldA
085310160406     c                               or vidnewC <> vidoldC)
085320071108      * non è un orm dirottato
085330071108     c                             and wokdirotta = *off
085340071108     c                   eval      $modifica = *on
085350071106      * se devo scrivere una proposta lo faccio subito
085360071106      * e poi esco dalla routine
085370140918     c                   if        $proposte = *on and not OrmFase420
085380071106     c                   exsr      sr_carproposte
085390080225     c                   unlock    fnorm01l
085400151023     c                   unlock    fnorg01l
085410071106     c                   goto      endsrconf
085420071106     c                   endif
085430071108      * controllo se devo scrivere lo Storico o la proposta di variazione
085440150305    2C                   if        %lookup(%editc(ormpor:'X'):skpog) > 0
085450071108      * scrivo lo storico
085460170605    3C*******            if        num_v1cpor <> v1cpoe
085470010222     C  n10              exsr      Sr_Storico
085480170605   3xC*******            else
085490170605    4C*******            if        v1fao >=300
085500170605     C**n10              exsr      Sr_Storico
085510170605    4C*******            endif
085520170605    3C*******            endif
085530071108      * scrivo la proposta
085540071108   2xC                   else
085550010202     C                   exsr      Sr_CarProposte
085560080225     c                   unlock    fnorm01l
085570151023     c                   unlock    fnorg01l
085580010919     C                   goto      endsrconf
085590071108    2C                   endif
085600071108      * Non sono in conferma orm da altri p.o.
085610071108     C                   when      §rmfpr <> 'O'
085620071108      * è un orm dirottato
085630071108     c                             and wokdirotta = *on
085640071108     c                   eval      $modifica = *on
085650040615      * Storico
085660040615     c                   ExSr      Sr_Storico
085670040618      * scrivo la fase 200
085680040615     C                   clear                   fnorf000
085690040615     C                   eval      ORFpoe = v1cpoe
085700040615     C                   eval      ORFnsr = v1nsr
085710040615     C                   eval      ORFnor = v1nor
085720040615     C                   eval      ORFnrv = v1nrv
085730040615     C                   eval      ORFpog = OrmPor
085740040615     C                   eval      ORFdae = v1dfo
085750040615     C                   eval      ORFore = v1ofo
085760051220     c                   Move      v1ofo         wora
085770051220     c                   subdur    1:*s          wora
085780051220     c                   move      wora          orfore
085790040618     C                   eval      ORFfar = 200
085800040615     C                   eval      ORFpue = knmus
085810040615     C                   write     fnorf000
085820040615      * scrivo fase 50
085830040615     C                   clear                   fnorf000
085840040615     C                   eval      ORFpoe = v1cpoe
085850040615     C                   eval      ORFnsr = v1nsr
085860040615     C                   eval      ORFnor = v1nor
085870040615     C                   eval      ORFnrv = v1nrv
085880040615     C                   eval      ORFpog = num_v1cpor
085890040615     C                   eval      ORFdae = v1dfo
085900040615     C                   eval      ORFore = v1ofo
085910040615     C                   eval      ORFfar = v1fao
085920040615     C                   eval      ORFpue = knmus
085930040615     C                   write     fnorf000
085940160517     C                   feod      fnorf01l
085950040615      * pulisco i campi che non devo reimpostare
085960040615     c                   Clear                   v1npg
085970040615     c                   Clear                   v1ndc
085980040615     c                   Clear                   v1ddc
085990040615     c                   Clear                   v1dst
086000140918
086010010919      * se sono in conferma ORM da altri p.o. devo storicizzare
086020010919      * se sono state fatte delle variazioni
086030071108     C                   when      §rmfpr = 'O'
086040071108     c                   eval      $modifica = *on
086050030609      * Prima di confermare la fase 100 dell'orm genero la bolla 'FT' di orm commissionato
086060030623      * se cod.ordinante codificato e se non è un ORM prepagato
086070071108    2c                   If        v1com = 'S' and
086080030610     c                             ds_V1ccor > *Zeros and
086090030610     c                             ds_V1cor2 <> 8888 and ds_V1cor2 <> 9999
086100030623     c                             and v1ctor <> 'P'
086110041130      * non deve esserci già una fase 200
086120041130     c                   Eval      wfase200 = *Off
086130041130     C     kfnor         setll     fnorf01l
086140071108    3c                   Do        *Hival
086150041130     C     kfnor         reade(n)  fnorf01l
086160041130     C                   if        %eof (fnorf01l)
086170041130     c                   Leave
086180041130     c                   EndIf
086190071108    4C                   If        orffar = 200
086200041130     c                   eval      wfase200 = *On
086210041130     c                   Leave
086220071108    4c                   EndIF
086230071108    3c                   EndDo
086240071108    3c                   If        wfase200 = *Off
086250030609     c                   Exsr      Sr_BollaFt
086260030609      * se f12 o f03 da immissione bolla riemetto la videata
086270071108    4c                   If        okbolla = '1'
086280030609     c                   Eval      *in28 = *On
086290030609     c                   GoTo      endsrconf
086300071108    4c                   EndIf
086310140506      /free
086320140506       //?se OK bolle e sto confermando un ORM commissionato con Alert
086330140506       //?imposto già che l'ORM è confermato così poi esce dal FIOR05R
086340140506         IF  walert;
086350140506           wormconf = *on;
086360140506         ENDIF;
086370140506      /end-free
086380071108    3c                   EndIf
086390071108    2c                   EndIf
086400030612      * storicizzo se ho fatto vedere la videata
086410131018     C                   if        (Vidnew <> Vidold
086420131211     c                              or vidnew2 <> vidold2
086430160406     c                              or vidnewA <> vidoldA
086440160406     c                              or vidnewC <> vidoldC)
086450030612     c                             and §rmtla = '2'
086460010919     C                   exsr      Sr_Storico
086470071108     C                   endif
086480050322      * sto dirottatando l'orm invece di confermarlo
086490071108    2c                   if        wokdirotta = *On
086500050322      * scrivo la fase 200
086510050322     C                   clear                   fnorf000
086520050322     C                   eval      ORFpoe = v1cpoe
086530050322     C                   eval      ORFnsr = v1nsr
086540050322     C                   eval      ORFnor = v1nor
086550050322     C                   eval      ORFnrv = v1nrv
086560050322     C                   eval      ORFpog = OrmPor
086570050322     C                   eval      ORFdae = v1dfo
086580050322     C                   eval      ORFore = v1ofo
086590070125     c                   Move      v1ofo         wora
086600070125     c                   subdur    1:*s          wora
086610070125     c                   move      wora          orfore
086620050322     C                   eval      ORFfar = 200
086630050322     C                   eval      ORFpue = knmus
086640050322     C                   write     fnorf000
086650160517     c                   feod      fnorf01l
086660071108    2c                   EndIf
086670071108     c                   endsl
086680140918
086690010202      * Scrive fase per chisura o conferma orm da altri p.o.
086700010202     C                   if        *in10 = *on or §RMfpr = 'O'
086710010202     C                   exsr      Sr_Carfilef
086720010202     C                   endif
086730140918
086740090211      * memorizzo i dati dell'appuntamento
086750150305     c                   if        §RMfpr = 'O'
086760090330     c                             and wokdirotta = *off
086770090407     c                             and wokapp = *on
086780090407      * l'appuntamento va inserito in fase 100 quindi se sto chiudendo
086790090407      * l'ORM prima scrivo la fase 100 poi scrivo i dati dell'appuntamento
086800090407     c                   if        *in10
086810090407     C                   clear                   fnorf000
086820090407     C                   eval      ORFpoe = ormpoe
086830090407     C                   eval      ORFnsr = ormnsr
086840090407     C                   eval      ORFnor = ormnor
086850090407     C                   eval      ORFnrv = ormnrv
086860090407     C                   eval      ORFpog = ormpor
086870090407     C                   eval      ORFdae = v1dfo
086880090407     C                   eval      ORFore = v1ofo
086890090407     c                   Move      v1ofo         wora
086900090407     c                   subdur    1:*s          wora
086910090407     c                   move      wora          orfore
086920090407     C                   eval      ORFfar = 100
086930090407     C                   eval      ORFpue = knmus
086940090407     C                   write     fnorf000
086950160517     c                   feod      fnorf01l
086960090407     c                   endif
086970090211     c                   exsr      sr_wrtapp
086980090211     c                   endif
086990070119
087000070119      * salvo i dati modificati sensibili per PDA
087010070123      * se p.o. abilitato al PDA
087020140122      * o se filiale ritiro abilitata alle telefonate AUT
087030070119      * se sono in modifica (se proposta variazione non arriva qua)
087040070119      * se ORM in fase 400 o 410
087050070119      * negli altri casi non serve
087060140122     c                   if        *in02 and
087070140317     c                             *in22 and
087080071210     c                             (v1fao = 400 or v1fao = 410)
087090070119     c                   exsr      sr_datipdamod
087100070119     c                   endif
087110070119
087120071108      * se aggiornato qualcosa
087130071108     c                   if        $modifica = *on
087140001023      * Aggiorna ORM
087150001011     C                   exsr      Sr_Carfile
087160151016      * aggiorno fnorg (GIRO)
087170151016     c                   exsr      sr_aggorg
087180131018      /free
087190131018       //?Se modificati i dati win alert o dati orari apertura
087200161028       //?o se modificata data pronta merce
087210131029       //?o se conferma proposta di variazione
087220131018       //?aggiorno FNORE
087230131018        IF  old_v1OraAMda <> v1OraAMda or
087240131018            old_v1OraAMa  <> v1OraAMa  or
087250131018            old_v1OraPMda <> v1OraPMda or
087260131018            old_v1OraPMa  <> v1OraPMa  or
087270161028            old_v1dpm <> v1dpm or
087280131029            vidoldA <>  vidnewA or
087290131029            §RMfpr = 'P';
087300131018          exsr aggFNORE;
087310131018        ENDIF;
087320131018      /end-free
087330001108     C                   update    fnorm000
087340090202     c                   else
087350090202     c                   UNLOCK    FNORM01L
087360151023     c                   unlock    fnorg01l
087370071108     c                   endif
087380070119
087390070123      * se il p.o. è abilitato al PDA
087400140122      * o abilitato alle telefonate AUT
087410140317     c                   if         *in22  and
087420070228      * non è un dirottamento o chiusura ORM
087430070228     c                             (wokdirotta = *off or not *in10) and
087440070119      * l'orm è in fase 400 o 410
087450080314     c                             (ormfao = 400 or ormfao = 410) and
087460070119      * almeno uno dei dati sensibili per PDA è stato variato
087470070119     c                             datipdasav <> datipdamod
087480140123       //?Verifico cosa è stato variato
087490140122     c                   exsr      ctrvardati
087500070122      * aggiorno i dati inviati al PDA
087510070124     c                   clear                   fior56ds
087520070124     c                   eval      or56tla = 'O'
087530080121     c                   eval      or56fgs = orffgs
087540080121     c                   eval      or56ndcd = orfndc
087550080121     c                   eval      or56ndca = orfndc
087560140120     c                   eval      OR56cmd = 'V'
087570140122     c                   eval      OR56var = wVardati
087580070119     c                   exsr      sr_aggpdo
087590070119     c                   endif
087600070119
087610010919   1eC                   endif
087620001106
087630001109     C                   eval      *in10 = *off
087640001011
087650001019     C     endsrconf     endsr
087660010202      **********************************************************************
087670010202      * F7 - Ricerca cliente ritiro
087680010202      **********************************************************************
087690010202     C     Sr_RicRitiro  BEGSR
087700010202
087710010202     C                   eval      *in90 = *on
087720010202
087730130916     c                   clear                   FIOR81DS
087740130916     c                   IF        V1cra1 > 0
087750130916     c                   eval      I81fil = V1cra1
087760130916     c                   ENDIF
087770130916     c                   IF        V1rsr <> *blanks
087780130916     c                   eval      I81rag = V1rsr
087790130916     c                   ENDIF
087800150907     c                   IF        V1prr <> *blanks
087810150907     c                   eval      I81prv = V1prr
087820150907     c                   ENDIF
087830130916     c                   call      'FIOR81R'
087840130916     c                   parm                    kpjba
087850130916     c                   parm                    FIOR81DS
087860130916     c                   IF        O81err <> *blanks
087870130916     c                   eval      V1Cmsg = O81msg
087880130916     c                   seton                                        41  28
087890130916     c                   leavesr
087900130916     c                   ENDIF
087910130916     c                   IF        O81ret = '1'
087920130916     c                   leavesr
087930130916     c                   ENDIF
087940130916
087950130916     c                   eval      ds_v1ccra = O81cro
087960070905     c                   eval      v1cra1 = ds_v1cra1
087970070905     c                   eval      v1cra2 = ds_v1cra2
087980070905     c                   eval      v1cra3 = ds_v1cra3
087990070905     c                   exsr      sr_contrclir
088000010202
088010010202     C                   endsr
088020010202      **********************************************************************
088030010202      * F8 - Ricerca cliente ordinante
088040010202      **********************************************************************
088050010202     C     Sr_RicOrdin   BEGSR
088060010202
088070010202     C                   eval      *in90 = *on
088080070905
088090130916     c                   clear                   FIOR81DS
088100130916     c                   IF        V1cor1 > 0
088110130916     c                   eval      I81fil = V1cor1
088120130916     c                   ENDIF
088130130916     c                   IF        V1rso <> *blanks
088140130916     c                   eval      I81rag = V1rso
088150130916     c                   ENDIF
088160150907     c                   IF        V1pro <> *blanks
088170150907     c                   eval      I81prv = V1pro
088180150907     c                   ENDIF
088190130916     c                   call      'FIOR81R'
088200130916     c                   parm                    kpjba
088210130916     c                   parm                    FIOR81DS
088220130916     c                   IF        O81err <> *blanks
088230130916     c                   eval      V1Cmsg = O81msg
088240130916     c                   seton                                        42  28
088250130916     c                   leavesr
088260130916     c                   ENDIF
088270130916     c                   IF        O81ret = '1'
088280130916     c                   leavesr
088290130916     c                   ENDIF
088300130916
088310130916     c                   eval      ds_v1ccor = O81cro
088320070905     c                   eval      v1cor1 = ds_v1cor1
088330070905     c                   eval      v1cor2 = ds_v1cor2
088340070905     c                   eval      v1cor3 = ds_v1cor3
088350070905     c                   exsr      sr_contrclio
088360010202
088370010202     C                   endsr
088380010202      **********************************************************************
088390011113      * F9 - Ricerca cliente destinatario
088400010202      **********************************************************************
088410010202     C     Sr_RicCons    BEGSR
088420010202
088430010202     C                   eval      *in90 = *on
088440070905
088450130916     c                   clear                   FIOR81DS
088460130916     c                   IF        V1crc1 > 0
088470130916     c                   eval      I81fil = V1crc1
088480130916     c                   ENDIF
088490130916     c                   IF        V1rsc <> *blanks
088500130916     c                   eval      I81rag = V1rsc
088510130916     c                   ENDIF
088520150907     c                   IF        V1prc <> *blanks
088530150907     c                   eval      I81prv = V1prc
088540150907     c                   ENDIF
088550130916     c                   call      'FIOR81R'
088560130916     c                   parm                    kpjba
088570130916     c                   parm                    FIOR81DS
088580130916     c                   IF        O81err <> *blanks
088590130916     c                   eval      V1Cmsg = O81msg
088600130916     c                   seton                                        56  28
088610130916     c                   leavesr
088620130916     c                   ENDIF
088630130916     c                   IF        O81ret = '1'
088640130916     c                   leavesr
088650130916     c                   ENDIF
088660130916
088670130916     c                   eval      ds_v1ccrc = O81cro
088680070905     c                   eval      v1crc1 = ds_v1crc1
088690070905     c                   eval      v1crc2 = ds_v1crc2
088700070905     c                   eval      v1crc3 = ds_v1crc3
088710070905     c                   exsr      sr_contrclid
088720010202
088730010202     C                   endsr
088740010202      **********************************************************************
088750010202      * F11 - Calcola Volume
088760010202      **********************************************************************
088770010202     C     Sr_Volume     BEGSR
088780010202
088790040301     c                   clear                   UL36vol
088800040301     c                   call      'TRUL36R'
088810040301     c                   parm                    UL36vol
088820040301     c                   add       UL36vol       V1vlm
088830010202
088840010202     C                   endsr
088850010202      **********************************************************************
088860010202      * F16 - Chiusura
088870010202      **********************************************************************
088880010202     C     Sr_Chiusura   BEGSR
088890010202
088900081125     c                   eval      wnobolla =  *off
088910020523     c                   eval      *in17 = *off
088920010202     C                   clear                   v3cau
088930010202     C                   clear                   v3dcau
088940010202     C                   eval      v3cpoe = ppoe
088950010202     C                   eval      v3nsr  = pnsr
088960010202     C                   eval      v3nor  = pnor
088970010202     C                   eval      v3nrv  = pnrv
088980071126
088990071126      * se è manutenzione
089000130412     c                   if        *in02 and §rmtla = *blanks and
089010071126      * ORM assegnato
089020071126      * scrivo proposta variazione
089030080314     c                             (v1fao = 400 or v1fao = 410)
089040071126     c                   eval      $proposte = *on
089050071126     c                   endif
089060071126
089070010202      * Videata con richiesta causale di chiusura (obbligatoria)
089080010227     C                   do        *hival
089090010202     C                   exfmt     fior05c
089100010301     C                   setoff                                       28
089110010202      * F12=Ritorno e non chiudo
089120050609     c   kl              clear                   v3cau
089130010202     C   kl              goto      endsrchiuso
089140070228
089150070228      * se il p.o. che chiude è abilitato al PDA
089160070228      * e fase 400 o 410
089170071210     c                   if        *in22 and
089180070228     c                             (ormfao = 400 or ormfao = 410)
089190070228      * devo avvisare che ho già avuto un esito da AUT
089200070228      * se l'orm è in fase 410
089210070228     c                   if        ormfao = 410 and wforzachi = *off
089220070228     c                   eval      v3cmsg = msg(41)
089230070228     c                   seton                                        28
089240070228     c                   eval      wforzachi = *on
089250070228     c                   iter
089260070228     c                   endif
089270070228     c                   endif
089280070228
089290010202      * Causale obbligatoria
089300010202     C                   if        v3cau = *blanks
089310010202     C                   movel     msg(14)       v3cmsg
089320010202     C                   clear                   v3dcau
089330010202     C                   eval      *in28 = *on
089340010202     C                   iter
089350010202     C                   endif
089360010302
089370010302     C                   clear                   v3dcau
089380010202      * Ricerco causale
089390010202     C     '?'           scan      v3cau                                  30
089400010202     C     *in30         ifeq      *on
089410010202     C                   clear                   TIBS02DS
089420010301     C                   clear                   dcmr
089430010202     C                   movel     'R'           t02mod
089440010202     C                   movel     knsif         t02sif
089450010227     C                   movel     'CMR'         t02cod
089460010202     C                   call      'TIBS02R'
089470010202     C                   parm                    KPJBA
089480010202     C                   parm                    TIBS02DS
089490010202     C                   movel     T02ke1        v3cau
089500010301     C                   movel     T02uni        dcmr
089510010202     C                   iter
089520010202     C                   endif
089530010202      * Decodifico
089540010301     C                   clear                   dcmr
089550010202     C                   clear                   TIBS02DS
089560010202     C                   movel     'C'           T02mod
089570010202     C                   movel     knsif         t02sif
089580010227     C                   movel     'CMR'         t02cod
089590010202     C                   movel(p)  v3cau         T02ke1
089600010202     C                   call      'TIBS02R'
089610010202     C                   parm                    KPJBA
089620010202     C                   parm                    TIBS02DS
089630010202     C                   if        t02err <> *blanks
089640010202     C                   movel     msg(15)       v3cmsg
089650010202     C                   eval      *in28 = *on
089660010202     C                   iter
089670010202     C                   endif
089680010202     C                   movel     t02ke1        v3cau
089690010301     C                   movel     t02uni        dcmr
089700010622
089710010301     C                   movel     d§cmrdes      v3dcau
089720030912
089730030912      * nei casi in cui il flag sia impostato a 'E' o a 'R'
089740030912      * se p.o. ritiro diverso da p.o. emissione controllo se posso usare la causale
089750030912     c                   If        Ormpoe <> OrmPor
089760030912      * se impostato a 'E' il p.o. emissione non può usare la causale
089770151014     c                   If        d§CmrNoc = 'E' and  ORMpoe = DUTpou and
089780150622     c                             %lookup(%editc(ORMpoe:'X'):skpog) > 0
089790030806     C                   movel     msg(64)       v3cmsg
089800030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089810070418     c                             'dalla filiale emissione'
089820030806     C                   eval      *in28 = *on
089830030806     C                   iter
089840030806     c                   EndIf
089850030912      * se impostato a 'R' il p.o. ritiro non può usare la causale
089860151014     c                   If        d§CmrNoc = 'R' and  ORMpor = DUTpou and
089870150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
089880030806     C                   movel     msg(64)       v3cmsg
089890030806     c                   Eval      v3cmsg= %trim(v3cmsg) + ' ' +
089900070418     c                             'dalla filiale ritiro'
089910030806     C                   eval      *in28 = *on
089920030806     C                   iter
089930030806     c                   EndIf
089940030912     c                   EndIf
089950030912      * se impostato a 'S' non si può usare e basta
089960030806     C                   if        d§cmrnoc = 'S'
089970010305     C                   movel     msg(64)       v3cmsg
089980010302     C                   eval      *in28 = *on
089990010302     C                   iter
090000010302     C                   endif
090010100218
090020100218      /free
090030100218       //?Se ORM NON prevede il preavviso non posso utilizzare la nuova causale '98'
090040100218       IF  §ormpre <> 'M' and v3cau = '98';
090050100218         v3cmsg = msg(64);
090060100218         v3cmsg = %trim(v3cmsg) + '. Il ritiro non prevede il PREAVVISO';
090070100218         *in28 = *on;
090080100218         iter;
090090100218       ENDIF;
090100100218      /end-free
090110081028
090120081124      * se causale inserita genera bolla di addebito (colpa cliente)
090130081028      * controllo che l'ORM non abbia già avuto una causale che genera addebito
090140081125      * in questo caso faccio solo un addebito
090150081124     c                   if        d§cmradd = 'S'
090160081028     c     kfnor         setll     fnorf01l
090170081028     c                   do        *hival
090180081028     c     kfnor         reade(n)  fnorf01l
090190081028     c                   if        %eof (fnorf01l)
090200081028     c                   leave
090210081028     c                   endif
090220081029      * senza causale rileggo
090230081028     c                   if        orfcar = *blanks
090240081028     c                   iter
090250081028     c                   endif
090260081029      * fase da PDA rileggo
090270081029     c                   if        orffar = 410
090280081029     c                   iter
090290081029     c                   endif
090300081028     c                   clear                   a_dcmr
090310081028     c                   clear                   tibs02ds
090320081028     c                   eval      t02mod = 'C'
090330081028     c                   eval      t02sif = knsif
090340081028     c                   eval      t02cod = 'CMR'
090350081028     c                   eval      t02ke1 = orfcar
090360081028     c                   call      'TIBS02R'
090370081028     c                   parm                    kpjba
090380081028     c                   parm                    tibs02ds
090390081028     c                   eval      a_dcmr = t02uni
090400081124      * se causale che genera bolla vado avanti senza generare l'addebito per la seconda volta
090410081028     c                   if        a_d§cmradd = 'S'
090420081125     c                   eval      wnobolla = *on
090430081028     c                   leave
090440081028     c                   endif
090450081028     c                   enddo
090460081028     c                   endif
090470081007
090480081007      * se ORM automatico DPD non si può chiudere l'ORM con la causale 80
090490081007      * l'unica eccezione è per la generazione automatica da parte dei pgm di AB
090500081007      * che creano la proposta di chiusura con causale 80
090510081007     c     ormpoe        lookup    skpodpd                                30
090520081007     c                   if        §rmfpr <> 'P' and ormtco = 'F' and
090530081007     c                             *in30 and v3cau = '80'
090540081007     c                   eval      v3cmsg = msg(49)
090550081007     c                   eval      *in28 = *on
090560081007     c                   iter
090570081007     c                   endif
090580081007
090590010227      * Conferma chiusura
090600010227     C                   if        *inkf = *on
090610071126      * se non è una proposta di variazione
090620071126     c                   if        $proposte = *off
090630020524      * procedo con i controlli per creare l'addebito solo se l'orm è mio
090640150306     c                   if        ormpoe = ormpor or
090650150306     c                             %lookup(%editc(ORMpor:'X'):skpog) > 0
090660020523     c                   if        d§cmradd <> *blanks
090670080314     c                             and (ormfao = 400 or ormfao = 410)
090680020523     c                   exsr      sr_contradd
090690020523      * se f12 o f03 da immissione bolla riemetto la videata
090700020523     c                   if        okbolla = '1'
090710020523     c                   iter
090720020523     c                   endif
090730020524     c                   if        wbolla = *on
090740020523     c                   eval      *in17 = *on
090750020524     c                   endif
090760020523     c                   endif
090770020524     c                   endif
090780071126     c                   endif
090790010202
090800010202      * Salva dati della fase nel caso l'utente cambi idea (f12)
090810010202     C                   eval      sav_v1fao = v1fao
090820010202     C                   eval      sav_v1dfo = v1dfo
090830010202     C                   eval      sav_v1ofo = v1ofo
090840070228      * salvo la fase anche per aggiornare FIPDO
090850070228     c                   eval      savorffar = orffar
090860070228      * e salvo anche la distinta per passarla al pgm che aggiorna FIPDO
090870070228     c                   eval      savorffgs = orffgs
090880070228     c                   eval      savorfndc = orfndc
090890070228     c                   eval      savorfddc = orfddc
090900150922      * imposto peso/volume di ORG in modo da non azzerarlo al momento della conferma
090910150922      * della chiusura ORM
090920150922     c                   eval      wpkg = ORGpkg
090930150922     c                   eval      wvlm = ORGvlm
090940010202
090950010301     C                   movel     d§cmrfar      v1fao                          chiuso
090960050422     C                   z-add     oggi_aammgg   v1dfo
090970050422     C                   time                    w0140
090980010202     C                   movel     w0140         v1ofo
090990010202     C                   eval      *in10 = *on
091000010305
091010010305     C                   eval      v1cau = v3cau
091020010305     C                   eval      v1dcau = v3dcau
091030010301
091040010227     C                   leave
091050010227     C                   endif
091060010227     C
091070010227     C                   enddo
091080010202      * Decodifica la fase
091090010202     C                   exsr      Sr_Contrfao
091100131025
091110131025      /free
091120131025       //?Imposto la riga dei tasti funzione
091130131025       ExSr Sr_Tastifun;
091140131025      /end-free
091150010202
091160010202     C     endsrchiuso   endsr
091170020522      **********************************************************************
091180020522      * Controllo se devo generare la bolla di addebito
091190020522      **********************************************************************
091200020522     c     sr_contradd   begsr
091210020522
091220020522     c                   clear                   sav_lpksc
091230020523     c                   clear                   okbolla
091240020524     c                   eval      wbolla = *off
091250081125
091260081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
091270081125      * come se avessi fatto la bolla
091280081125     c                   if        wnobolla = *on
091290081125     c                   eval      wbolla = *on
091300081125     c                   leavesr
091310081125     c                   endif
091320020522
091330020522      * se cod.ordinante codificato addebito
091340020522 b1  c                   if        ds_v1ccor <> *zeros and v1cor2 <> 8888
091350020522     c                             and v1cor2 <> 9999
091360020523     c                   clear                   dblp
091370090504     c                   movel     ormcor        §lpksca
091380020702      * se c'è imposto il codice bollettazione
091390151109     c     ormcor        chain(n)  fnacr01l
091400020702     c                   if        %found(fnacr01l) and acratb = *blanks
091410020702     c                             and acrksc <> *zeros
091420090504     c                   move      acrksc        §lpksca
091430090505     c                   if        acrccc<>999
091440090504     c                   move      acrccc        §lpctra
091450090505     c                   endif
091460020702     c                   endif
091470020522     c                   eval      §lprsd = ormrso
091480020522     c                   eval      §lpind = ormino
091490020522     c                   eval      §lpcad = ormcao
091500020522     c                   eval      §lplod = ormloo
091510020522     c                   eval      §lpprd = ormpro
091520020522     c                   eval      §lpnzd = ormnao
091530020522     c                   exsr      sr_wrtbolla
091540020522 x1  c                   else
091550020522      * se non inserito l'ordinante
091560020522 b2  c                   if        v1rso = *blanks
091570050323      * orm locale
091580050323 b3  c                   if        v1cpoe = num_v1cpor
091590020522      * se cod.mittente codificato addebito
091600020522 b4  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
091610020522     c                             and v1cra2 <> 9999
091620020523     c                   clear                   dblp
091630090504     c                   movel     ormcra        §lpksca
091640020702      * se c'è imposto il codice bollettazione
091650151109     c     ormcra        chain(n)  fnacr01l
091660020702     c                   if        %found(fnacr01l) and acratb = *blanks
091670020702     c                             and acrksc <> *zeros
091680090504     c                   move      acrksc        §lpksca
091690090505     c                   if        acrccc<>999
091700090504     c                   move      acrccc        §lpctra
091710090505     c                   endif
091720020702     c                   endif
091730020522     c                   eval      §lprsd = ormrsr
091740020522     c                   eval      §lpind = orminr
091750020522     c                   eval      §lpcad = ormcar
091760020522     c                   eval      §lplod = ormlor
091770020522     c                   eval      §lpprd = ormprr
091780020522     c                   eval      §lpnzd = ormnar
091790020522     c                   exsr      sr_wrtbolla
091800020522      * se cod.mittente non codificato
091810020522 x4  c                   else
091820020522      * se cod.destinatario codificato addebito
091830020522 b5  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
091840020522     c                             and v1crc2 <> 9999
091850020523     c                   clear                   dblp
091860090504     c                   movel     ormcrc        §lpksca
091870020702      * se c'è imposto il codice bollettazione
091880151109     c     ormcrc        chain(n)  fnacr01l
091890020702     c                   if        %found(fnacr01l) and acratb = *blanks
091900020702     c                             and acrksc <> *zeros
091910090504     c                   move      acrksc        §lpksca
091920090505     c                   if        acrccc<>999
091930090504     c                   move      acrccc        §lpctra
091940090505     c                   endif
091950020702     c                   endif
091960020522     c                   eval      §lprsd = ormrsc
091970020522     c                   eval      §lpind = orminc
091980020522     c                   eval      §lpcad = ormcac
091990020522     c                   eval      §lplod = ormloc
092000020522     c                   eval      §lpprd = ormprc
092010020522     c                   eval      §lpnzd = ormnac
092020020522     c                   exsr      sr_wrtbolla
092030020522 e5  c                   endif
092040020522 e4  c                   endif
092050050323      * orm non locale
092060020522 x3  c                   else
092070020522      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
092080020522 b4  c                   if        ds_v1ccrc <> *zeros and v1crc2 <> 8888
092090020522     c                             and v1crc2 <> 9999  and v1crc1 = v1cpoe
092100020523     c                   clear                   dblp
092110090504     c                   movel     ormcrc        §lpksca
092120020702      * se c'è imposto il codice bollettazione
092130151109     c     ormcrc        chain(n)  fnacr01l
092140020702     c                   if        %found(fnacr01l) and acratb = *blanks
092150020702     c                             and acrksc <> *zeros
092160090504     c                   move      acrksc        §lpksca
092170090505     c                   if        acrccc<>999
092180090504     c                   move      acrccc        §lpctra
092190090505     c                   endif
092200020702     c                   endif
092210020522     c                   eval      §lprsd = ormrsc
092220020522     c                   eval      §lpind = orminc
092230020522     c                   eval      §lpcad = ormcac
092240020522     c                   eval      §lplod = ormloc
092250020522     c                   eval      §lpprd = ormprc
092260020522     c                   eval      §lpnzd = ormnac
092270020522     c                   exsr      sr_wrtbolla
092280020522      * se non codificato o p.o. differenti
092290020522 x4  c                   else
092300020522      * se cod.mittente codificato addebito
092310020522 b5  c                   if        ds_v1ccra <> *zeros and v1cra2 <> 8888
092320020522     c                             and v1cra2 <> 9999
092330020523     c                   clear                   dblp
092340090504     c                   movel     ormcra        §lpksca
092350020702      * se c'è imposto il codice bollettazione
092360151109     c     ormcra        chain(n)  fnacr01l
092370020702     c                   if        %found(fnacr01l) and acratb = *blanks
092380020702     c                             and acrksc <> *zeros
092390090504     c                   move      acrksc        §lpksca
092400090505     c                   if        acrccc<>999
092410090504     c                   move      acrccc        §lpctra
092420090505     c                   endif
092430020702     c                   endif
092440020522     c                   eval      §lprsd = ormrsr
092450020522     c                   eval      §lpind = orminr
092460020522     c                   eval      §lpcad = ormcar
092470020522     c                   eval      §lplod = ormlor
092480020522     c                   eval      §lpprd = ormprr
092490020522     c                   eval      §lpnzd = ormnar
092500020522     c                   exsr      sr_wrtbolla
092510020522 e5  c                   endif
092520020522 e4  c                   endif
092530020522 e3  c                   endif
092540020522 e2  c                   endif
092550020522 e1  c                   endif
092560020522
092570020522     c                   endsr
092580020522      **********************************************************************
092590020522      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
092600020522      **********************************************************************
092610020522     c     sr_wrtbolla   begsr
092620020522
092630030623     c                   Clear                   wtrulvid
092640020522      * richiamo il programma per la scelta delle stampanti
092650030307     c                   Clear                   wdatibolla
092660030307     c                   Eval      wdatibolla = 'BollaAdd.'
092670030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
092680030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
092690020522     c                   exsr      sr_scestp
092700020524     c   92              eval      okbolla = '1'
092710020522     c   92              goto      endwrtbolla
092720020522
092730020522     c                   clear                   kpjbu
092740020522     c                   clear                   dtasv
092750020522
092760020522     c                   eval      §lpfpr = 'T'
092770020522     c                   movel     ormpor        §lpccn
092780020522     c                   move      9999          §lpccn
092790020523     c                   movel     oggi_aammgg   §lpaas
092800020523     c                   move      oggi_aammgg   §lpmgs
092810020522     c                   eval      §lplnp = ormpor
092820020522     c                   eval      §lpcbo = 'FY'
092830020522     c                   eval      §lpsop = '+'
092840020530     c                   eval      §lplna = ormpoe
092850020522     c                   eval      §lprsm = ormrsr
092860020522     c                   eval      §lpinm = orminr
092870020522     c                   eval      §lpcam = ormcar
092880020522     c                   eval      §lplom = ormlor
092890020522     c                   eval      §lpprm = ormprr
092900020522     c                   eval      §lpnzm = ormnar
092910020522
092920020522 b1  c                   if        ormncl <> 0
092930020522     c                   eval      §lpncl = ormncl
092940020522 x1  c                   else
092950020522     c                   eval      §lpncl = 1
092960020522 e1  c                   endif
092970020522 b1  c                   if        ormpkg <> 0
092980020522     c                   eval      §lppkb = ormpkg
092990020522 x1  c                   else
093000070918 b3  c                   if        ormbnc <> 0
093010070918     c                   eval      wpkb = ormbnc / d§dftbnc * d§dftpkg
093020070918     c                   if        wpkb > 999999,9
093030070918     c                   eval      wpkb = 999999,9
093040070918     c                   endif
093050070918     c                   eval      §lppkb = wpkb
093060070918 x2  c                   else
093070020522 b2  c                   if        ormvlm <> 0
093080070918     c                   eval      wpkb = ormvlm * d§dftpkg
093090070918     c                   if        wpkb > 999999,9
093100070918     c                   eval      wpkb = 999999,9
093110070918     c                   endif
093120070918     c                   eval      §lppkb = wpkb
093130020522 e3  c                   endif
093140020522 e2  c                   endif
093150020522 e1  c                   endif
093160020522
093170020522     c                   clear                   ds_keyorm
093180020522     c                   eval      ds_poe = ORMpoe
093190020522     c                   eval      ds_nsr = ORMnsr
093200020522     c                   eval      ds_nor = ORMnor
093210020522     c                   eval      ds_nrv = ORMnrv
093220030716     c                   move      ds_keyorm     §lprmn
093230020522
093240020522     c                   eval      §lprma = ormrfa
093250020523     c                   eval      §lprmo = v3cau + '-' + d§cmrdes1
093260020522     c                   eval      §lpctm = '4F'
093270020523     c                   movel     oggi_aammgg   §lpdrt
093280020522
093290020522      * Cerca codice autotrasportatore e flag mattino/pomeriggio
093300020522 b1  c                   if        ormndc <> 0
093310120612      /free
093320120612        DSTnpg = ORMnpg;
093330120612        DSTnfv = ORMndc;
093340120612        DSTfgs = ORMpor;
093350120612        chain (DSTnpg:DSTnfv:DSTfgs) FIDST01L;
093360120612      /end-free
093370120612 b2  c                   if        %found(fidst01l)
093380020522     c                   eval      §lppdr = DSTpdr
093390120612     c                   eval      §lpfpp = DSTfpp
093400020523 x2  c                   else
093410020523     c                   movel     §lplnp        §lppdr
093420020523     c                   move      0999          §lppdr
093430020523     c                   eval      §lpfpp = 'M'
093440020523 e2  c                   endif
093450020522 x1  c                   else
093460020522     c                   movel     §lplnp        §lppdr
093470020522     c                   move      0999          §lppdr
093480020522     c                   eval      §lpfpp = 'M'
093490020522 e1  c                   endif
093500020522
093510020522     C                   call      'FNLS01R'
093520020522     C                   parm                    kpjba
093530020522     C                   parm                    dblp
093540020522     C                   parm                    dtasv
093550030623     c                   Parm                    Trul90ds
093560020522
093570020522     C                   if        §lpfpr = '4' or §lpfpr = '3'
093580020522     C                             or §lpfpr = '5'
093590020522     C                   eval      okbolla = '1'
093600020524     c                   else
093610020524     c                   eval      wbolla = *on
093620020522     C                   endif
093630020522
093640020522     c     endwrtbolla   endsr
093650030606      **********************************************************************
093660030606      * Scrivo bolla addebito "FT"
093670030606      **********************************************************************
093680030606     c     Sr_BollaFt    BegSr
093690030609
093700030609     c                   Clear                   okbolla
093710030623     c                   Eval      wtrulvid = 'S'
093720030623
093730030623      * richiamo il programma per la scelta delle stampanti
093740030623     c                   Clear                   wdatibolla
093750030623     c                   Eval      wdatibolla = 'BollaAdd.'
093760030623     c                   Eval      wdatibolA4 = 'BolAdd_A4'
093770030623     c                   Eval      wdatibolA5 = 'BolAdd_A5'
093780030623     c                   exsr      sr_scestp
093790030623     c   92              eval      okbolla = '1'
093800030623     c   92              GoTo      endbollaft
093810030606
093820030606     c                   Clear                   Kpjbu
093830030606     c                   Clear                   dtasv
093840030606     c                   Clear                   dblp
093850030606
093860030606     c                   Eval      §lpFpr = 'T'
093870030606      * Cod.Cliente mittente
093880030606      * Ksc dell'ordinante da Fnacr
093890030606     c                   If        AcrKscCor <> *Zeros
093900090504     c                   Eval      §lpksca = %editc(AcrKscCor:'X')
093910090505     c                   if        ACrCCCCor<>999
093920090505     c                   move      AcrCccCor     §lpctra
093930090505     c                   endif
093940030606     c                   Else
093950030606      * Primi 7 byte dell'ordinante
093960090504     c                   Eval      §lpksca = %editc(Ds_V1cor12:'X')
093970030606     c                   EndIf
093980030606
093990050324     c                   Movel     OrmPor        §lpCcn
094000030606     c                   Move      9999          §lpCcn
094010030606     c                   Movel     oggi_aammgg   §lpAas
094020030606     c                   Move      oggi_aammgg   §lpMgs
094030050324     c                   Eval      §lpLnp = OrmPor
094040030606     c                   Eval      §lpCbo = 'FT'
094050030606     c                   Eval      §lpSop = '+'
094060090504     c                   Movel     §lpksca       §lpLna
094070030606      * Mittente bolla = mittente ORM
094080030606     c                   Eval      §lpRsm = V1rsr
094090030606     c                   Eval      §lpInm = V1inr
094100030606     c                   Eval      §lpCam = V1car
094110030606     c                   Eval      §lpLom = V1lor
094120030606     c                   Eval      §lpPrm = V1prr
094130030606     c                   Eval      §lpNzm = V1nar
094140030606      * Destinatario bolla = ordinante ORM
094150030606     c                   Eval      §lpRsd = V1rso
094160030606     c                   Eval      §lpInd = V1ino
094170030606     c                   Eval      §lpCad = V1cao
094180030922     c                   Eval      §lpLod = sav_V1loo35
094190030606     c                   Eval      §lpPrd = V1pro
094200030606     c                   Eval      §lpNzd = V1nao
094210030606      * colli = colli ORM - se a 0 imposto 1
094220030609 b1  c                   if        V1ncl <> 0
094230030609     c                   eval      §lpncl = V1ncl
094240030606 x1  c                   else
094250030606     c                   eval      §lpncl = 1
094260030606 e1  c                   endif
094270030606      * peso = peso ORM - se a 0 ricavo dal volume o dai bancali dell'ORM
094280030609 b1  c                   if        V1pkg <> 0
094290030609     c                   eval      §lppkb = V1pkg
094300030606 x1  c                   else
094310070918 b3  c                   if        V1bnc <> 0
094320070918     c                   eval      wpkb = v1bnc / d§dftbnc * d§dftpkg
094330070918     c                   if        wpkb > 999999,9
094340070918     c                   eval      wpkb = 999999,9
094350070918     c                   endif
094360070918     c                   eval      §lppkb = wpkb
094370070918 x2  c                   else
094380030609 b2  c                   if        V1vlm <> 0
094390070918     c                   eval      wpkb = v1vlm * d§dftpkg
094400070918     c                   if        wpkb > 999999,9
094410070918     c                   eval      wpkb = 999999,9
094420070918     c                   endif
094430070918     c                   eval      §lppkb = wpkb
094440030606 e3  c                   endif
094450030606 e2  c                   endif
094460030606 e1  c                   endif
094470030606      * rif. mittente numerico = n.ORM
094480030606     c                   clear                   ds_keyorm
094490030609     c                   eval      ds_poe = V1cpoe
094500030609     c                   eval      ds_nsr = V1nsr
094510030609     c                   eval      ds_nor = V1nor
094520030609     c                   eval      ds_nrv = V1nrv
094530030716     c                   move      ds_keyorm     §lprmn
094540030606      * rif. mittente alfabetico = rif. alfabetico ORM
094550030609     c                   eval      §lprma = V1rfa
094560030606      * trattamento merce
094570030606     c                   eval      §lpctm = '4F'
094580030606      * data ritiro merce
094590030606     c                   movel     oggi_aammgg   §lpdrt
094600030606
094610030606      * codice autotrasportatore e flag mattino/pomeriggio
094620030606     c                   Eval      §lpFpp = 'M'
094630030606     c                   Movel     §lpLnp        §lpPdr
094640030606     c                   Move      0999          §lpPdr
094650030606
094660030606     c                   Call      'FNLS01R'
094670030606     c                   Parm                    Kpjba
094680030606     c                   Parm                    dblp
094690030606     c                   Parm                    dtasv
094700030623     c                   Parm                    Trul90ds
094710030606
094720030606     c                   If        §lpFpr = '4' or §lpFpr = '3'
094730030606     c                             or §lpFpr = '5'
094740030606     c                   Eval      okbolla = '1'
094750030606     c                   EndIf
094760030606
094770030606     c     endbollaft    EndSr
094780140130
094790140130      /free
094800140130       //--------------------------------------------------------------------
094810140130       //?F17 - Note AUT.
094820140130       //--------------------------------------------------------------------
094830140130       BEGSR F17D01;
094840140130
094850140130       //?Disalloco FNORG perchè nel pgm delle note se deve aggiornare PDO
094860140130       //?da rcd già vincolato da questo lavoro
094870140130         unlock FNORG01L;
094880140130
094890140130         clear fiort1ds;
094900140226         IORT1mod = 'G';
094910140130         IORT1poe = ORMpoe;
094920140130         IORT1nsr = ORMnsr;
094930140130         IORT1nor = ORMnor;
094940140130         IORT1nrv = ORMnrv;
094950140130         IORT1pgm = 'FIOR05R';
094960140130         §kpjbu = kpjbu;
094970140130
094980140130         fiort1r (kpjba:fiort1ds);
094990140130         kpjbu = §kpjbu;
095000140226
095010140226       //?Se ho scritto note ho la data e ora di immissione
095020140226         IF  OORT1dim > 0 and OORT1oim > 0;
095030140226           wF17write = *on;
095040140226         ENDIF;
095050140130
095060140130       //?Rialloco FNORG
095070140130         chain (kpoe:knsr:knor:knrv) FNORG01L;
095080140314
095090140314       //?Controllo se da F17 sono state scritte delle proposte
095100140314         exsr sr_orp;
095110140130
095120140130       //?Imposto la riga dei tasti funzione
095130140130         ExSr Sr_Tastifun;
095140140130
095150140130       ENDSR;
095160140130
095170140130      /end-free
095180140130
095190001025      **********************************************************************
095200001025      * F18 - Note
095210001025      **********************************************************************
095220001025     C     Sr_Note       BEGSR
095230001025
095240001025     C                   clear                   fior06DS
095250010118     C                   eval      i06dta = v1dao
095260010118     C                   eval      i06poe = v1cpoe
095270010118     C                   eval      i06nor = v1nor
095280010118     C                   eval      i06nsr = v1nsr
095290010118     C                   eval      i06nrv = v1nrv
095300010118     C                   eval      i06far = v1fao
095310010301     C                   eval      i06dai = oggi_aammgg
095320010301     C                   movel     w0140         I06ori
095330010220     C                   if        v1cpoe = *zeros
095340150305     C                   eval      i06poe = DUTpou
095350010220     C                   endif
095360001026
095370001025     C                   select
095380001026     C                   when      *in01 = *on
095390001026     C                   movel     'M'           i06flm
095400001026
095410001109     C                   when      *in03 = *on
095420001109     C                   movel     'M'           i06flm
095430001026
095440001026     C                   when      *in02 = *on
095450040402      * richiamo particolare nel caso di orm commissionato e variazione data ritiro
095460040402     c                   If        ormdar <> inv_v1dar and v1Com = 'S'
095470040419     c                   Eval      I06Flm = 'D'
095480040402     c                   Else
095490001026     C                   movel     'V'           i06flm
095500050421      * se p.o. ritiro diverso da p.o. emissione e da p.o. gestione e non è chiusura ORM
095510050421      * il pgm va in interrogazione note non da la possibilità di inserirne.....
095520050421      * aggiungo che la data ritiro deve essere uguale
095530050421      * xchè per data ritiro diversa e non è un ORM commissionato devo dare la possibilità
095540050421      * di inserire la nota
095550010116     C                   if        v1cpoe <> num_v1cpor  and
095560150305     C                             %lookup(%editc(num_v1cpor:'X'):skpog) = 0
095570150305     C                             and *in10 = *off
095580050421     c                             and ormdar = inv_v1dar
095590001027     C                   movel     'I'           i06flm
095600001027     C                   endif
095610050420     c                   EndIf
095620050128      * se sto facendo una proposta di chiusura ORM la fase deve essere la fase ORM e non la nuova
095630150305     c                   If        *In10 and
095640150305     c                             %lookup(%editc(ORMpor:'X'):skpog) = 0
095650050128     c                   Eval      i06far = OrmFao
095660050128     c                   EndIf
095670050322      * se sto confermando un ORM la fase deve essere la fase ORM e non la nuova
095680050324      * se non è chiusura ORM
095690050324     c                   If        §rmfpr = 'O' and Not *In10
095700050322     c                   Eval      i06far = OrmFao
095710050322     c                   EndIf
095720001026     C                   endsl
095730001026
095740001110     C                   movel(p)  kpjbu         §kpjbu
095750001110     C                   movel(p)  parm01        kpjbu
095760001025     C                   CALL      'FIOR06R'
095770001025     C                   PARM                    kpjba
095780001027     C                   parm                    fior06ds
095790001110     C                   movel     §kpjbu        kpjbu
095800131212      /free
095810131212       IF not *in01;
095820131212         exsr sr_orn;
095830131212       ENDIF;
095840131212       //?Imposto la riga dei tasti funzione
095850131212       ExSr Sr_Tastifun;
095860131212      /end-free
095870001025
095880001025     C                   endsr
095890001103      **********************************************************************
095900001103      * F19 - Proposte
095910001103      **********************************************************************
095920001103     C     Sr_Proposte   BEGSR
095930001103
095940001103     C                   movel     'P'           psce
095950001103     C                   movel     kpjbu         §kpjbu
095960001103     C                   clear                   kpjbu
095970001103     C                   movel     parm01        kpjbu
095980001103     C                   call      'FIOR07R'
095990001103     C                   parm                    kpjba
096000001103     C                   movel     §kpjbu        kpjbu
096010001103
096020001103     C                   endsr
096030001102      **********************************************************************
096040001102      * F20 - Simulazione delivery-tassazione
096050001102      **********************************************************************
096060001102     C     Sr_Simula     BEGSR
096070001116
096080001116     C                   clear                   TISI92DS
096090001116     C                   movel     'ORM'         D92op0
096100001116     C                   movel     v1cpor        D92lnp
096110001116     C                   movel     v1nac         D92nar
096120001116     C                   movel     v1cac         D92cad
096130030922     C                   movel(p)  sav_v1loc35   D92lod
096140001116     C                   movel     v1ffd         D92ffd
096150001116     C                   movel     v1ksc         D92ksc
096160001116     C                   movel     v1ctr         D92ctr
096170001116     C                   z-add     v1ncl         D92ncl
096180001116     C                   z-add     v1pkg         D92pkb
096190001116     C                   z-add     v1vlm         D92vlf
096200001116     C                   movel     v1ddt         D92ddt
096210001102
096220001102     C                   movel     kpjbu         §kpjbu
096230001116     C                   movel(p)  tisi92ds      kpjbu
096240001102     C                   call      'TISI92R'
096250001102     C                   parm                    kpjba
096260001102     C                   movel     §kpjbu        kpjbu
096270001102
096280001102     C                   endsr
096290061023
096300061023      *--------------------------------------------------------------------*
096310061023      * F2 - Dati DPD
096320061023      *--------------------------------------------------------------------*
096330061023     c     sr_datidpd    begsr
096340061023
096350080430     c                   clear                   fieu55ds
096360080430
096370080430      * se ORM commissionato da DPD passo il n. ORM DPD
096380080430     c     ormpoe        lookup    skpodpd                                30
096390080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
096400080430     c                   eval      i55dep = %subst(ormrfa:4:4)
096410080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
096420081015     c                   eval      i55ie = 'I'
096430080430     c                   eval      kpjbu = fieu55ds
096440080430     c                   call      'FIEU55C'
096450080430     c                   parm                    kpjba
096460080430     c                   endif
096470080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
096480080430     c     ormpor        lookup    skpodpd                                30
096490080430     c                   if        *in30
096500061023     c                   eval      i55poe = v1cpoe
096510061023     c                   eval      i55nrs = v1nsr
096520061023     c                   eval      i55nro = v1nor
096530061023     c                   eval      i55nrv = v1nrv
096540081015     c                   eval      i55ie = 'E'
096550061023     c                   eval      kpjbu = fieu55ds
096560061206     c                   call      'FIEU55C'
096570061023     c                   parm                    kpjba
096580080430     c                   endif
096590061023
096600061023     c                   endsr
096610061023
096620050422      *--------------------------------------------------------------------*
096630050420      * CONTROLLO PROPOSTE
096640050422      *--------------------------------------------------------------------*
096650050420     c     Sr_ContrProp  BegSr
096660050420
096670050420     c                   Eval      wproposte = *Off
096680050420     c                   Movel     v1cpoe        kpoe
096690050420     c                   Movel     v1nsr         knsr
096700050420     c                   Movel     v1nor         knor
096710050420     c                   Movel     v1nrv         knrv
096720050420
096730050420     c     kfnor         Setll     Fnorp01l
096740050420     c                   Do        *Hival
096750140227     c     kfnor         Reade(n)  Fnorp01l
096760050420     c                   If        %Eof(Fnorp01l)
096770050420     c                   Leave
096780050420     c                   EndIf
096790050420     c                   If        OrpFev <> *Blanks
096800050420     c                   Iter
096810050420     c                   EndIf
096820050420     c                   Eval      wproposte = *On
096830050420     c                   Leave
096840050420     c                   EndDo
096850050420
096860050420     c                   EndSr
096870040419      **********************************************************************
096880040419      * F21 - Dirottamento
096890040419      **********************************************************************
096900040419     c     Sr_Dirotta    BegSr
096910040419
096920040419     c                   Clear                   wdcpor
096930040419     c                   Clear                   wddpor
096940040419     c                   Move      v1cpor        sav_wdcpor
096950040420     c                   Eval      wdcdar = v1dar
096960040419     c                   Do        *Hival
096970040419     c                   Exfmt     Fior05dw
096980040420     c                   Eval      *In28 = *Off
096990040419      * F12=Ritorno
097000040419     c   kl              Leave
097010040419     c                   ExSr      Sr_ContrWd
097020040420     c   28              Iter
097030040420      * F6=Conferma
097040040420     c                   If        *InKf
097050040615     c                   movel     wdcpor        v1cpor
097060040615     c                   movel     wdcdar        v1dar
097070040615     c                   Eval      v1fao = 050
097080050422     c                   Eval      v1dfo = oggi_aammgg
097090040615     c                   Time                    v1ofo
097100170411      * devo pulire anche i dati relativi al giro
097110170411     c                   clear                   V1Ccgi
097120170411     c                   clear                   V1tgi
097130170411     c                   clear                   sav_V1Ccgi
097140040615     c                   Eval      wokdirotta = *On
097150040420     c                   Leave
097160040420     c                   EndIf
097170040419     c                   EndDo
097180040419
097190040419     c                   EndSr
097200090205
097210090205      *--------------------------------------------------------------------*
097220090205      * Window per immissione dati appuntamento ORM
097230090205      *--------------------------------------------------------------------*
097240090205     c     sr_ormapp     begsr
097250090205
097260090206     c                   eval      *in90 = *off
097270090206     c                   eval      *in93 = *off
097280090206     c                   eval      *in94 = *off
097290090205     c                   do        *hival
097300090206     c                   exfmt     fior05app
097310090206     c                   eval      *in28 = *off
097320090206     c                   eval      *in93 = *off
097330090206     c                   eval      *in94 = *off
097340090206     c                   clear                   wappmsg
097350090206      * f12-ritorno
097360090206     c                   if        *inkl
097370090206     c                   eval      *in90 = *on
097380090206     c                   leave
097390090206     c                   endif
097400090206      * controllo
097410090206     c                   if        wappref = *blanks
097420090206     c                   eval      wappmsg = 'Inserire la persona da -
097430090206     c                             contattare'
097440090206     c                   eval      *in28 = *on
097450090206     c                   eval      *in93 = *on
097460090206     c                   iter
097470090206     c                   endif
097480090206      * almeno 5 caratteri
097490090206     c                   if        %subst(wappref:4:31) = *blanks
097500090206     c                   eval      wappmsg = 'Completare la persona -
097510090206     c                             da contattare'
097520090206     c                   eval      *in28 = *on
097530090206     c                   eval      *in93 = *on
097540090206     c                   iter
097550090206     c                   endif
097560090206      * controllo
097570090206     c                   if        wappnota1 = *blanks
097580090206     c                   eval      wappmsg = 'Inserire le annotazioni'
097590090206     c                   eval      *in28 = *on
097600090206     c                   eval      *in94 = *on
097610090206     c                   iter
097620090206     c                   endif
097630090206      * almeno 5 caratteri
097640090206     c                   if        %subst(wappnota1:4:31) = *blanks
097650090206     c                   eval      wappmsg = 'Completare le -
097660090206     c                             annotazioni'
097670090206     c                   eval      *in28 = *on
097680090206     c                   eval      *in94 = *on
097690090206     c                   iter
097700090206     c                   endif
097710090206      * f6-conferma
097720090206     c   kf              leave
097730090205     c                   enddo
097740090205
097750090205     c                   endsr
097760090205
097770040419      **********************************************************************
097780040419      * Controllo i dati per il dirottamento
097790040419      **********************************************************************
097800040419     c     Sr_ContrWd    BegSr
097810040419
097820040419      * --> P.O. ritiro nuovo
097830040419     c                   Clear                   wddpor
097840040419      * Obbligatorio
097850040419     c                   If        wdcpor = *Zeros
097860040419     c                   Eval      wdcmsg = msg(11)
097870070124     c                   Seton                                        28
097880040419     c                   Goto      endctrwd
097890040419     c                   EndIf
097900040419      * Non può essere lo stesso di prima
097910040419     c                   If        wdcpor = sav_wdcpor
097920040419     c                   Eval      wdcmsg = msg(78)
097930070124     c                   Seton                                        28
097940040419     c                   Goto      endctrwd
097950040419     c                   EndIf
097960040419      * Deve esistere
097970040419     c     wdcpor        Chain     Azorg01L
097980040419     c                   If        Not %Found(Azorg01l)
097990040419     c                   Eval      wdcmsg = msg(33)
098000070124     c                   Seton                                        28
098010040419     c                   Goto      endctrwd
098020040419     c                   EndIf
098030040419      * Non deve essere annullato
098040040419     c                   If        OrgFva <> *Blanks  or
098050040419     c                             (OrgFag <> 'A' and OrgFag <> 'F')
098060040419     c                   Eval      wdcmsg = msg(33)
098070070124     c                   Seton                                        28
098080040419     c                   Goto      endctrwd
098090040419     c                   EndIf
098100040419      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
098110040419     c                   Eval      og148 = OrgDe8
098120040419     c                   If        §ogorm <> 'S'
098130040628     c                   Eval      wdcmsg = msg(79)
098140070124     c                   Seton                                        28
098150040419     c                   Goto      endctrwd
098160040419     c                   EndIf
098170040419      * Decodifico
098180040419     c                   Eval      wddpor = OrgDes
098190070124
098200070124      * se il p.o. che dirotta è abilitato al PDA
098210070124      * e fase 400 o 410
098220071210     c                   if        *in22 and
098230070124     c                             (ormfao = 400 or ormfao = 410)
098240070124      * devo avvisare che ho già avuto un esito da AUT
098250070124      * se l'orm è in fase 410
098260070124     c                   if        ormfao = 410 and wforzadir = *off
098270070124     c                   eval      wdcmsg = msg(41)
098280070124     c                   seton                                        28
098290070124     c                   eval      wforzadir = *on
098300070124     c                   goto      endctrwd
098310070124     c                   endif
098320070124      * in ogni caso devo salvarmi la distinta per passarla al pgm che aggiorna
098330070124      * fipdo
098340070124     c                   eval      savorffgs = orffgs
098350070124     c                   eval      savorfndc = orfndc
098360070125     c                   eval      savorfddc = orfddc
098370070125      * mi salvo anche la fase che mi servirà poi nel caso di ORM dirottato
098380070125     c                   eval      savorffar = orffar
098390070124     c                   endif
098400040419
098410040615      * è sempre un orm commissionato controllo se ok tutti i dati
098420130920      * ma solo se era già un ORM COMMISSIONATO
098430130920     c                   IF        §ORcom = 'S'
098440040615     c                   Eval      V1com = 'S'
098450130918     c                   ENDIF
098460040419
098470040419     c     endctrwd      EndSr
098480001011      **********************************************************************
098490001023      * PRENDE IL NUOVO NUMERO ORDINE
098500001011      **********************************************************************
098510001011     C     Sr_Numor      BEGSR
098520001011
098530010222     C                   if        v1cpoe = *zeros
098540150305     C                   z-add     DUTpou        v1cpoe
098550010222     C                   endif
098560030922
098570030922     c                   clear                   TRUL33DS
098580030922     c                   Eval      I33cnu = 300
098590030922     c                   Eval      I33po1 = V1cpoe
098600030922     c                   Eval      I33num = 1
098610030922     c                   movel(p)  TRUL33DS      KPJBU
098620030922     c                   call      'TRUL33R'
098630030922     c                   parm                    KPJBA
098640030922     c                   movel     KPJBU         TRUL33DS
098650030922
098660030922    1c                   Select
098670030922      * Numeratore non trovato
098680030922     C                   when      O33err =  01
098690030922     C                   eval      *in28 = *on
098700030922     C                   movel     msg(72)       v1cmsg
098710030922     C                   goto      endsrnumor
098720030922      * Numeratore allocato
098730030922     C                   when      O33err =  02
098740030922     C                   eval      *in28 = *on
098750030922     C                   movel     msg(71)       v1cmsg
098760030922     C                   goto      endsrnumor
098770030922      * altro errore
098780030922     C                   when      O33err <> *zeros
098790030922     C                   eval      *in28 = *on
098800030922     C                   movel     O33msg        v1cmsg
098810030922     C                   goto      endsrnumor
098820030922    1C                   endsl
098830030922
098840030922     C                   z-add     O33nrf        v1nor
098850001011
098860010507     C     endsrnumor    endsr
098870070119
098880070119      *--------------------------------------------------------------------*
098890070119      * Carico dati sensibili per PDA modificati
098900070119      *--------------------------------------------------------------------*
098910070119     c     sr_datipdamod begsr
098920070119
098930070119     c                   clear                   datipdamod
098940070119     c                   eval      pdamodrsr = v1rsr
098950070119     c                   eval      pdamodinr = v1inr
098960070119     c                   eval      pdamodlor = v1lor
098970070119     c                   eval      pdamodprr = v1prr
098980070119     c                   eval      pdamodrer = v1rer
098990070119     c                   eval      pdamodter = v1ter
099000070119     c                   eval      pdamodorr = v1orr
099010070119     c                   eval      pdamodncl = v1ncl
099020070119     c                   eval      pdamodpkg = v1pkg
099030070119     c                   eval      pdamodvlm = v1vlm
099040070119     c                   eval      pdamodbnc = v1bnc
099050070119     c                   eval      pdamodno1 = v1not1
099060070119     c                   eval      pdamodno2 = v1not2
099070131107      /free
099080140123        pdamodora1 = V1OraAMda;
099090140123        pdamodora2 = V1OraAMa;
099100140123        pdamodora3 = V1OraPMda;
099110140123        pdamodora4 = V1OraPMa;
099120131107      /end-free
099130070119
099140070119     c                   endsr
099150070119
099160001011      **********************************************************************
099170001026      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
099180001011      **********************************************************************
099190001011     C     Sr_Carfile    BEGSR
099200001011
099210010116     C                   eval      ORMatb = v1atb
099220010116     C                   eval      ORMpoe = v1cpoe
099230010116     C                   eval      ORMnsr = v1nsr
099240010116     C                   eval      ORMnor = v1nor
099250010116     C                   eval      ORMnrv = v1nrv
099260010116     C                   eval      ORMtor = v1ctor
099270010116     C                   eval      ORMtco = v1ctco
099280010116      * Inversione data emissione ORM
099290010116     C                   clear                   wlbdat
099300010116     C                   z-add     v1dao         G02dat
099310010116     C                   movel     *blanks       G02err
099320010116     C                   call      'XSRDA8'
099330010116     C                   parm                    wlbdat
099340010116     C                   z-add     G02inv        ORMdao
099350010116
099360131211     C                   eval      ORMoao = v1oao
099370010116
099380010116     C                   eval      ds_v1cor1 = v1cor1
099390010116     C                   eval      ds_v1cor2 = v1cor2
099400010116     C                   eval      ds_v1cor3 = v1cor3
099410010116     C                   eval      ORMcor = ds_v1ccor
099420010116     C                   eval      ORMrso = v1rso
099430010116     C                   eval      ORMino = v1ino
099440010116     C                   eval      ORMcao = v1cao
099450030922     C                   movel(p)  sav_v1loo35   ORMloo
099460010116     C                   eval      ORMpro = v1pro
099470010116     C                   eval      ORMnao = v1nao
099480010116
099490010116     C                   eval      ds_v1cra1 = v1cra1
099500010116     C                   eval      ds_v1cra2 = v1cra2
099510010116     C                   eval      ds_v1cra3 = v1cra3
099520010116     C                   eval      ORMcra = ds_v1ccra
099530010116     C                   eval      ORMrsr = v1rsr
099540010116     C                   eval      ORMinr = v1inr
099550010116     C                   eval      ORMcar = v1car
099560001103     C                   movel(p)  v1lor         ORMlor
099570010116     C                   eval      ORMprr = v1prr
099580010116     C                   eval      ORMnar = v1nar
099590010116     C                   eval      ORMrer = v1rer
099600010116     C                   eval      ORMter = v1ter
099610010116
099620010116     C                   eval      ds_v1crc1 = v1crc1
099630010116     C                   eval      ds_v1crc2 = v1crc2
099640010116     C                   eval      ds_v1crc3 = v1crc3
099650010116     C                   eval      ORMcrc = ds_v1ccrc
099660010116     C                   eval      ORMrsc = v1rsc
099670010116     C                   eval      ORMinc = v1inc
099680030922     C                   movel(p)  sav_v1loc35   ORMloc
099690010116     C                   eval      ORMcac = v1cac
099700010116     C                   eval      ORMprc = v1prc
099710010116     C                   eval      ORmnac = v1nac
099720001023      * Inversione data ritiro
099730001017     C                   clear                   wlbdat
099740001017     C                   z-add     v1dar         G02dat
099750001017     C                   movel     *blanks       G02err
099760001017     C                   call      'XSRDA8'
099770001017     C                   parm                    wlbdat
099780001025     C                   z-add     G02inv        ORMdar
099790010118     C                   eval      ORMorr = v1orr
099800010116
099810010116     C                   eval      ORMffd = v1ffd
099820010116     C                   eval      ORMnam = v1nam
099830010116     C                   eval      ORMncl = v1ncl
099840010116     C                   eval      ORMpkg = v1pkg
099850010116     C                   eval      ORMvlm = v1vlm
099860010116     C                   eval      ORMbnc = v1bnc
099870010116     C                   eval      ORMblc = v1blc
099880010116     C                   eval      ORMatt = v1att
099890010116     C                   eval      ORMmtc = v1mtc
099900010116     C                   eval      ORMddt = v1ddt
099910140331      * se codice ordinante = codice mittente e impostato che paga l'Ordinante
099920140331      * in automatico metto il 'Mittente' in questo modo elimino un msg di errore
099930140331     c                   IF        V1pag = 'O' and
099940140331     c                             ds_V1cCor = ds_V1cCra and ds_V1cCra > *Zeros
099950140331     c                   eval      v1pag = 'M'
099960140331     c                   ENDIF
099970140331
099980010116     C                   eval      ORMpag = v1pag
099990010116     C                   eval      ORMksc = v1ksc
100000010202     C                   eval      ORMctr = v1ctr
100010001017     C                   movel     v1cpor        ORMpor
100020001016     C                   movel     v1cpoc        ORMpoc
100030010116     C                   move      v1sto         ORMsto
100040010116
100050010116     C                   eval      ORMpos = v1cpos
100060010116     C                   eval      ORMors = v1ors
100070010116     C                   eval      ORMdst = v1dst
100080010116     C                   eval      ORMno1 = v1not1
100090010116     C                   eval      ORMno2 = v1not2
100100010116     C                   eval      ORMrfa = v1rfa
100110010712
100120010116     C                   eval      ORMcst = v1cst
100130010116     C                   eval      ORMvcs = v1vcs
100140010116     C                   eval      ORMfcs = v1fcs
100150050316      * se ORM commissionato e sono in immissione l'ORM deve essere creato in fase
100160050316      * 50 mentre se non è commissionato subito in fase 100
100170081211      * se ORM export DPD sempre in fase 50
100180050316     c                   If        *In01 or *In03
100190081127     c                   If        v1com = 'S' or *in15
100200050316     c                   Eval      v1fao = 50
100210050316     c                   Else
100220050316     c                   Eval      v1fao = 100
100230050316     c                   EndIf
100240151118      /free
100250151118       //?Se ORM da internet ed è un prepagato locale
100260151118       //?lo immetto subito in fase 100 anche se è commissionato
100270151118       //?la telefonata al mittente viene fatta al momento della conferma ORM
100280151118         IF  §RMfpr = 'C' and ORMtco = 'I' and ORMtor = 'P' and
100290151118             ORMpoe = ORMpor;
100300151118           V1fao = 100;
100310151118         ENDIF;
100320140428       //?Alert da inviare
100330140422         IF  walert;
100340140428         //?Se ora ritiro a 0 imposto inizio servizio o l'ora "tappo"
100350140422           IF  ORMorr = 0 and OOR2stis > 0;
100360140422             ORMorr = OOR2stis;
100370140422           ENDIF;
100380140422           IF  ORMorr = 0;
100390140422             ORMorr = d§DFTorrtp;
100400140422           ENDIF;
100410140422         //?Se ora telefono non impostato metto il cell.x sms
100420140422           IF  ORMter = *blanks;
100430140422             ORMter = W03sms;
100440140422           ENDIF;
100450140422         ENDIF;
100460140422      /end-free
100470070912      * devo aggiornare il flag di ORM assegnabile in base alla fase
100480070912     c                   exsr      sr_contrfao
100490050316     c                   EndIf
100500140429
100510140109     C                   eval      ORMdfo = v1dfo
100520140109     C                   eval      ORMofo = v1ofo
100530010116     C                   eval      ORMfao = v1fao
100540010116     C                   eval      ORMnpg = v1npg
100550010116     C                   eval      ORMndc = v1ndc
100560010116     C                   eval      ORMddc = v1ddc
100570010622     C                   eval      ORMstp = v1stp
100580010622     C   10              clear                   ORMnpg
100590010622     C   10              clear                   ORMndc
100600010622     C   10              clear                   ORMddc
100610010622     C   10              clear                   ORMtap
100620010116     C                   eval      ORMtap = v1tap
100630010116     C                   eval      ORMeti = v1eti
100640010119     C                   eval      ORMspi = v1spi
100650011114      * Se forzato blocco destinatario devo pulire il relativo flag
100660011114     C                   if        §ormfd = 'S'
100670011119     C                   if        v1ctor <> 'S'
100680011114     C                   clear                   §ormfd
100690011119     C                   else
100700070219     C                   if        v1rsc = *blanks and ds_v1ccrc = *zeros
100710011119     C                   clear                   §ormfd
100720011114     C                   endif
100730011119     C                   endif
100740011114     C                   endif
100750030606      * ORM commissionato
100760131106      /free
100770131106       //?Se il campo è vuoto imposto 'N' in questo modo in immissione fino a che
100780131106       //?non viene forzato dall'utente il pgm calcola se ORM commissionato o meno
100790131106       //?mentre nel file ho solo 'N' o 'S' (da ora in poi)
100800140424       //?gli unici ORM con il campo a blank sono gli ORM con serie (RC) scritti
100810140424       //?automaticamente dal traduttore
100820140424       //?e gli ORM commissionati da DPD chiusi in automatico da DPD
100830131106         IF  V1com = *blanks;
100840131106           V1com = 'N';
100850131106         ENDIF;
100860131106      /end-free
100870030606     c                   Eval      §orcom = v1com
100880030731
100890030731      * in base al tipo orm e a chi paga imposto il campo §orpfb
100900030731      * imposto 'F' se paga mittente per orm singolo o prepagato
100910030731      * imposto 'A' se NON paga mittente
100920030731     c                   Clear                   §orPfb
100930030731     c                   Select
100940030731     c                   When      OrmPag = 'M'
100950030731     c                   If        OrmTor = 'P' or OrmTor = 'S'
100960030731     c                   Eval      §orPfb = 'F'
100970030731     c                   EndIf
100980030731     c                   Other
100990030731     c                   Eval      §orPfb = 'A'
101000030731     c                   EndSl
101010090403
101020090403      * in immissione ORM
101030090403      * se orm da internet o manuale verifico se impostare il §ormks
101040100913      * solo però se il campo non è già stato impostato da tabella PVO
101050090403     c                   if         *in01 and
101060141211     c                             (ormtco = 'I' or ormtco = 'M' or
101070141212     c                              ORMtco = 'E')
101080100913     c                             and (not ok_pvo or
101090100913     c                                 (ok_pvo and d§pvoks = ' '))
101100090403     c                   exsr      sr_vac
101110090403     c                   endif
101120090403
101130090403      * imposto flag operativi
101140030731     C                   eval      ORMflo = dorm01
101150161115      /free
101160161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
101170161115         //?ipotetico ritiro da effettuare al mattino
101180161115         IF  Orariestesi;
101190161115           ORMrmp = 'S';
101200161115         ELSE;
101210161115           clear ORMrmp;
101220161115         ENDIF;
101230161116         //?Se ORM da Internet non lo memorizzo
101240161116         IF  ORMtco = 'I';
101250161116           clear ORMrmp;
101260161116         ENDIF;
101270161116
101280161115      /end-free
101290010228
101300040908     c                   Eval      OrmDtt = oggi_aammgg
101310001204      * Mette sempre in positivo i campi delle quantità
101320001204     C                   mllzo     1             ORMorr
101330001204     C                   mllzo     1             ORMncl
101340001204     C                   mllzo     1             ORMpkg
101350001204     C                   mllzo     1             ORMvlm
101360001204     C                   mllzo     1             ORMbnc
101370001204     C                   mllzo     1             ORMblc
101380001204     C                   mllzo     1             ORMatt
101390001204     C                   mllzo     1             ORMmtc
101400001109      * Dati di copia
101410001109     C                   if        *in03 = *on
101420001109     C                   z-add     knrv          ORMnrv
101430050316      * se ORM commissionato l'ORM deve essere creato in fase
101440050316      * 50 mentre se non è commissionato subito in fase 100
101450081211      * se ORM export DPD sempre in fase 50
101460081211     c                   If        v1com = 'S' or *in15
101470050316     c                   Eval      ORMfao = 50
101480050316     c                   Else
101490050316     c                   Eval      ORMfao = 100
101500050316     c                   EndIf
101510010118     C                   eval      ORMdfo = oggi_aammgg
101520010118     C                   movel     w0140         ORMofo
101530001130     C                   clear                   ORMnpg
101540001129     C                   z-add     *zeros        ORMndc
101550001129     C                   z-add     *zeros        ORMddc
101560001129     C                   movel     *blanks       ORMtap
101570010223     C                   z-add     *zeros        ORMdst
101580010209      * Aggiorno anche i dati che sono a video per poter scrivere la fase
101590010209     C                   movel     ORMnrv        v1nrv
101600010209     C                   movel     ORMfao        v1fao
101610010209     C                   movel     ORMdfo        v1dfo
101620010209     C                   movel     ORMofo        v1ofo
101630070706      * devo aggiornare il flag di ORM assegnabile in base alla fase
101640070706     c                   exsr      sr_contrfao
101650070706     c                   eval      ormeti = v1eti
101660001109     C                   endif
101670131126
101680131126      /free
101690131126       //?Immissione/Copia ORM
101700131126         IF  *in01 or *in03;
101710131126       //?imposto ora cut-off nel campo Stop
101720140318           ORMstp = OOR2lrnc;
101730160526       //?in teoria se ritiro estero dovrei mettere il nuovo limite per orm esteri
101740160526       //?ma dato che il campo ORMSTP non è usato non lo faccio
101750131126         ENDIF;
101760131126      /end-free
101770001017
101780001017     C                   endsr
101790090403
101800090403      *--------------------------------------------------------------------*
101810090403      * Se conferma orm da VAO controllo se impostare il ritorno VAC
101820090403      *--------------------------------------------------------------------*
101830090403     c     sr_vac        begsr
101840090403
101850090403      * se paga Ordinante
101860090403      * se ho il KSC
101870090403     c                   if        v1pag = 'O' and
101880090403     c                             v1ksc <> *zeros
101890090403      * in tabella 3K trovo il cliente e non devo escludere
101900090403     c                   clear                   ds3k
101910090403     c                   eval      kcod = '3K'
101920090403     c                   movel(p)  v1ksc         kkey
101930090403     c     ktab          chain     tabel00f
101940090403     c                   if        %found(tabel00f) and tblflg = *blanks
101950090403     c                   eval      ds3k = tbluni
101960090403      * ritorno VAC x triangolazione ORM
101970090403     c                   if        §3kvto = 'S'
101980090403      * imposto il falg per ritorno VAC
101990090403     c                   eval      §ormks = 'S'
102000100812     c                   leavesr
102010090403     c                   endif
102020090403     c                   endif
102030100812
102040100812      * se non ho trovato con 3K provo con 3Q
102050100812     c                   clear                   ds3q
102060100812     c                   eval      kcod = '3Q'
102070100812     c                   movel(p)  v1ksc         kkey
102080100812     c     ktab          chain     tabel00f
102090100812     c                   if        %found(tabel00f) and tblflg = *blanks
102100100812     c                   eval      ds3Q = tbluni
102110100812      * ritorno VAB x triangolazione ORM
102120100812     c                   if        §3Qvto = 'S'
102130100812      * imposto il flag per ritorno VAB
102140100812     c                   eval      §ormks = 'S'
102150100812     c                   endif
102160100812     c                   endif
102170100812
102180090403     c                   endif
102190090403
102200090403     c                   endsr
102210140122
102220140122      /free
102230140122       //--------------------------------------------------------------------
102240140122       //?Controllo i dati variati.
102250140122       //--------------------------------------------------------------------
102260140122       BEGSR ctrvardati;
102270140122
102280140122         clear wVardati;
102290140122
102300140122       //?Variazione dati mittente ==> M
102310140122         IF  pdasavrsr <> pdamodrsr or
102320140122             pdasavinr <> pdamodinr or
102330140122             pdasavlor <> pdamodlor or
102340140122             pdasavprr <> pdamodprr or
102350140122             pdasavrer <> pdamodrer or
102360140122             pdasavter <> pdamodter;
102370140122           wVardati = %trim(wVardati) + 'M';
102380140122         ENDIF;
102390140122       //?Variazione dati ora pronta merce ==> O
102400140122         IF  pdasavorr <> pdamodorr or
102410140122             pdasavora1 <> pdamodora1 or
102420140122             pdasavora2 <> pdamodora2 or
102430140122             pdasavora3 <> pdamodora3 or
102440140122             pdasavora4 <> pdamodora4;
102450140122           wVardati = %trim(wVardati) + 'O';
102460140122         ENDIF;
102470140122       //?Variazione dati quantità ==> Q
102480140122         IF  pdasavncl <> pdamodncl or
102490140122             pdasavpkg <> pdamodpkg or
102500140122             pdasavvlm <> pdamodvlm or
102510140122             pdasavbnc <> pdamodbnc;
102520140122           wVardati = %trim(wVardati) + 'Q';
102530140122         ENDIF;
102540140124       //?Variazione dati note ==> N
102550140122         IF  pdasavno1 <> pdamodno1 or
102560140122             pdasavno2 <> pdamodno2;
102570140124           wVardati = %trim(wVardati) + 'N';
102580140122         ENDIF;
102590140122
102600140122       ENDSR;
102610140122
102620140122      /end-free
102630070119
102640070119      *--------------------------------------------------------------------*
102650070119      * Aggiorno file PDO
102660070119      *--------------------------------------------------------------------*
102670070119     c     sr_aggpdo     begsr
102680070301
102690140122      * Richiamo sempre il FIOR56R, ci pensa poi lui a decidere se scrivere i dati
102700140122      * per il PDA e/o la telefonata AUT
102710070123
102720120612      * devo aggiornare FIPDO
102730070228     c                   eval      or56poe = ormpoe
102740070228     c                   eval      or56nsr = ormnsr
102750070228     c                   eval      or56nor = ormnor
102760070228     c                   eval      or56nrv = ormnrv
102770070228     c                   call      'FIOR56R'
102780140122     c                   parm                    kpjba
102790070228     c                   parm                    fior56ds
102800070228      * il pgm torna degli errori, ma dato che ho già fatto qua tutti i controlli
102810070228      * del caso, non me ne preoccupo
102820070119
102830070119     c                   endsr
102840070119
102850001017      **********************************************************************
102860001026      * CARICA IL FILE FNORF DAL FILE FNORM
102870001017      **********************************************************************
102880001017     C     Sr_Carfilef   BEGSR
102890001011
102900081125     c                   clear                   dorf01
102910001019     C                   clear                   fnorf000
102920010116     C                   eval      ORFpoe = v1cpoe
102930010116     C                   eval      ORFnsr = v1nsr
102940010116     C                   eval      ORFnor = v1nor
102950050322     C                   eval      ORFnrv = v1nrv
102960050322      * sono in conferma orm commissioni e sto dirottando l'orm imposto il p.o. ritiro
102970150305      * come po fase
102980050322     c                   If        §rmfpr = 'O' and wokdirotta = *On
102990050322     c                   movel     v1cpor        orfpog
103000050322     c                   Else
103010150305     C                   eval      ORFpog = DUTpou
103020050322     c                   EndIf
103030010116     C                   eval      ORFdae = v1dfo
103040010116     C                   eval      ORFore = v1ofo
103050010116     C                   eval      ORFfar = v1fao
103060001019     C                   eval      ORFpue = knmus
103070010202     C                   eval      ORFcar = v3cau
103080080523      * se sono in conferma proposta di chiusura ORM e l'orm non ha la distinta non la riporto dalla
103090080523      * conferma chiusura perchè ormai l'ORM non è più assegnato
103100080523      * caso di orm assegnato, proposta di chiusura, disassegnato, accetto porposta di chiusura
103110080523     c                   select
103120080523     c                   when      §rmfpr = 'P' and *in10 and ormndc = 0
103130080523     c                   clear                   orffgs
103140080523     c                   clear                   orfndc
103150080523     c                   clear                   orfddc
103160080523      * se c'è la distinta la riporto sulla fase
103170080523     c                   when      OrmNdc > *Zeros
103180040909     c                   Eval      OrfFgs = OrmPor
103190040909     c                   Eval      OrfNdc = OrmNdc
103200040909     c                   Eval      OrfDdc = OrmDdc
103210080523      * se non c'è la distinta riporto sulla fase quella salvata in precedenza
103220080523     c                   when      savndc > *Zeros
103230060516     c                   eval      orffgs = savfgs
103240041014     c                   Eval      OrfNdc = savndc
103250041014     c                   Eval      OrfDdc = savddc
103260080523     c                   endsl
103270081125      * imposto se non ho fatto l'ulteriore addebito
103280081125     c                   if        wnobolla = *on
103290081125     c                   eval      §orfadd = 'S'
103300081125     c                   endif
103310081125     c                   eval      orfflo = dorf01
103320001109
103330001019     C                   write     fnorf000
103340001017
103350001011     C                   endsr
103360001026      **********************************************************************
103370001026      * CARICA IL FILE DELLE NOTE FNORN RICHIAMANDO IL PROGRAMMA
103380001026      **********************************************************************
103390001026     C     Sr_Carnote    BEGSR
103400001026
103410001026     C                   if        *in01 = *on or *in02 = *on
103420001109     C                             or *in03 = *on
103430001026     C                   clear                   fior06ds
103440001026     C                   movel     'C'           i06flm
103450010118     C                   eval      i06dta = ORMdao
103460010118     C                   eval      i06poe = ORMpoe
103470010118     C                   eval      i06nor = ORMnor
103480010118     C                   eval      i06nsr = ORMnsr
103490010118     C                   eval      i06nrv = ORMnrv
103500010118     C                   eval      i06far = ORMfao
103510010301     C                   eval      i06dai = oggi_aammgg
103520010301     C                   movel     w0140         I06ori
103530001026
103540010405     C                   movel(p)  kpjbu         §kpjbu
103550010405     C                   movel(p)  parm01        kpjbu
103560001026     C                   call      'FIOR06R'
103570001026     C                   parm                    kpjba
103580001027     C                   parm                    fior06ds
103590010405     C                   movel     §kpjbu        kpjbu
103600001026
103610001026     C                   endif
103620001026
103630001026     C                   endsr
103640070903
103650070706      *--------------------------------------------------------------------*
103660151016      * File FNORG00F - Scrivo
103670070706      *--------------------------------------------------------------------*
103680151016     c     sr_wrtorg     begsr
103690151016
103700151016     c                   clear                   fnorg000
103710151016     c                   eval      orgpoe = ormpoe
103720151016     c                   eval      orgnsr = ormnsr
103730151016     c                   eval      orgnor = ormnor
103740151016     c                   eval      orgnrv = ormnrv
103750151016     c                   eval      orgpor = ormpor
103760151016     c                   eval      orgpkg = wpkg
103770151016     c                   eval      orgvlm = wvlm
103780151016      * giro immesso
103790151016     c                   if        v1ccgi <> *blanks
103800151016     c                   eval      orgcgi = v1ccgi
103810151016     c                   eval      orgtgi = v1tgi
103820151016     c                   eval      orgpocgi = num_v1cpor
103830151016     c                   time                    w0140
103840151016     c                   movel     w0140         orghvtel
103850151016     c                   move      w0140         orgdtvtel
103860151016     c     *eur          move      orgdtvtel     dataeur
103870151016     c                   move      dataeur       dataiso
103880151016     c                   move      dataiso       orgdtvtel
103890151016     c                   endif
103900151016      * scrivo
103910151016     c                   write     fnorg000
103920151016
103930151016     c                   endsr
103940151016
103950151016      *--------------------------------------------------------------------*
103960151016      * File FNORG00F - Aggiorno
103970151016      *--------------------------------------------------------------------*
103980151016     c     sr_aggorg     begsr
103990070706
104000070706     c                   eval      orgpor = ormpor
104010070706     c                   eval      orgpkg = wpkg
104020070706     c                   eval      orgvlm = wvlm
104030071031     c                   select
104040071031      * non c'è il giro ma prima c'era
104050071031     c                   when      v1ccgi = *blanks and orgcgi <> *blanks
104060071031     c                   clear                   orgcgi
104070071031     c                   clear                   orgtgi
104080071031     c                   clear                   orgpocgi
104090071031     c                   time                    w0140
104100071031     c                   movel     w0140         orghvtel
104110071031     c                   move      w0140         orgdtvtel
104120071031     c     *eur          move      orgdtvtel     dataeur
104130071031     c                   move      dataeur       dataiso
104140071031     c                   move      dataiso       orgdtvtel
104150071031      * non c'è il giro
104160071031     c                   when      v1ccgi = *blanks
104170071031     c                   clear                   orgcgi
104180071031     c                   clear                   orgtgi
104190071031     c                   clear                   orgpocgi
104200071031     c                   clear                   orghvtel
104210071031     c                   clear                   orgdtvtel
104220071031      * giro variato
104230080305      * o stesso giro ma dirottamento (caso di stesso giro codificato su due filiali diverse)
104240071031     c                   when      orgcgi <> v1ccgi or orgtgi <> v1tgi
104250080305     c                             or (orgcgi = v1ccgi and wokdirotta = *on)
104260070919     c                   eval      orgcgi = v1ccgi
104270070919     c                   eval      orgtgi = v1tgi
104280071023     c                   eval      orgpocgi = num_v1cpor
104290071003     c                   time                    w0140
104300071003     c                   movel     w0140         orghvtel
104310071003     c                   move      w0140         orgdtvtel
104320071003     c     *eur          move      orgdtvtel     dataeur
104330071003     c                   move      dataeur       dataiso
104340071003     c                   move      dataiso       orgdtvtel
104350071031     c                   endsl
104360080130      * se è un dirottamento devo pulire i dati relativi a NFTL e SLO
104370080130      * che potrebbero essere rimasti sporchi da un'assegnazione automatica
104380080130      * fatta in precedenza
104390080211      * pulisco anche quello che riguarda la distinta
104400080130     c                   if        wokdirotta = *on
104410080211     c                   clear                   orgpdc
104420080211     c                   clear                   orgfgs
104430080211     c                   clear                   orgndc
104440080211     c                   clear                   orgddc
104450080130     c                   clear                   orgnftl
104460080130     c                   clear                   orgslo
104470080211     c                   clear                   orgdtvdis
104480080211     c                   clear                   orghvdis
104490080130     c                   endif
104500071031      * aggiorno
104510070706     c                   update    fnorg000
104520070706
104530070706     c                   endsr
104540100218
104550100218      /free
104560100218       //--------------------------------------------------------------------
104570100218       //?Scrivo file FNORE00F.
104580100218       //--------------------------------------------------------------------
104590100218       BEGSR sr_Fnore;
104600140516
104610140516         wOKrcdg = *off;
104620100218
104630100225       //?Se previsto il preavviso scrivo l'estensione 'M ' per memorizzare
104640100225       //?l'indirizzo mail dove inviare il preavviso
104650100225       //?(non aggancio il FNVAOE rcd 'M ' in quanto l'indirizzo mail è già recuperato)
104660100225         IF  §ormpre = 'M';
104670100225           clear fnore000;
104680100225           OREpoe = ORMpoe;
104690100225           OREnsr = ORMnsr;
104700100225           OREnor = ORMnor;
104710100225           OREnrv = ORMnrv;
104720100225           OREtrc = 'M ';
104730100225           OREdati = VAOEdati;
104740100225           write fnore000;
104750100225         ENDIF;
104760100218
104770100318       //?Importo le estensioni da VAO a ORM
104780100218         k_vaoecor = sav_vaocor;
104790100218         k_vaoepoe = sav_vaopoe;
104800100218         k_vaoensr = sav_vaonsr;
104810100218         k_vaoenor = sav_vaonor;
104820100218         k_vaoenrv = sav_vaonrv;
104830100218       //?Aggancio il rcd
104840100318         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
104850100318         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104860100318         DOW not %eof(FNVAOE1l);
104870140422         //?ma solo dei rcd che non vengono variati da pgm
104880140422           IF  %lookup(VAOEtrc:skTRCvao) = 0;
104890100318             clear fnore000;
104900100318             OREpoe = ORMpoe;
104910100318             OREnsr = ORMnsr;
104920100318             OREnor = ORMnor;
104930100318             OREnrv = ORMnrv;
104940100318             OREtrc = VAOEtrc;
104950100318             OREdati = VAOEdati;
104960100318             write fnore000;
104970100318           ENDIF;
104980100318           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104990100318         ENDDO;
105000140422
105010140422       //?Visto che questa routine viene richiamata da conferma VAO
105020140422       //?scrivo subito il rcd 'G', con i dati da VAO tranne i 2 flag ORM per alert
105030140422       //?che sono da impostare in base ai valori calcolati nel pgm
105040140422         k_vaoecor = sav_vaocor;
105050140422         k_vaoepoe = sav_vaopoe;
105060140422         k_vaoensr = sav_vaonsr;
105070140422         k_vaoenor = sav_vaonor;
105080140422         k_vaoenrv = sav_vaonrv;
105090140516         k_vaoetrc = 'G ';
105100140422       //?Scrivo il rcd solo se trovo su VAO
105110140422         chain  %kds(k06fnvaoe) FNVAOE1L;
105120140422         IF  %found(FNVAOE1L);
105130140422           clear fnore000;
105140140422           OREpoe = ORMpoe;
105150140422           OREnsr = ORMnsr;
105160140422           OREnor = ORMnor;
105170140422           OREnrv = ORMnrv;
105180140422           OREtrc = VAOEtrc;
105190140422           OREdati = dOREgen;
105200140422           write fnore000;
105210140516           wOKrcdg = *on;
105220140422         ENDIF;
105230100218
105240100218       ENDSR;
105250131018
105260131018       //--------------------------------------------------------------------
105270131018       //?Scrivo/Aggiorno file FNORE00F.
105280131018       //--------------------------------------------------------------------
105290131018       BEGSR aggFNORE;
105300131018
105310131018         k_OREpoe = ORMpoe;
105320131018         k_OREnsr = ORMnsr;
105330131018         k_OREnor = ORMnor;
105340131018         k_OREnrv = ORMnrv;
105350131018
105360131018       //?Rcd 'O ' - Orari apertura
105370131018        IF  v1OraAMda > 0 or v1OraAMa > 0 or
105380131018            v1OraPMda > 0 or v1OraPMa > 0;
105390131018          k_OREtrc = 'O ';
105400131018          §OREoramda = v1OraAMda;
105410131018          §OREorama  = v1OraAMa;
105420131018          §OREorapda = v1OraPMda;
105430131018          §OREorapa  = v1OraPMa;
105440131029          chain  %kds(k05fnore) FNORE01L;
105450131018          IF  %found(FNORE01L);
105460131125            OREdati = dOREorari;
105470131125            update fnore000;
105480131018          ELSE;
105490131018            clear fnore000;
105500131018            OREpoe = ORMpoe;
105510131018            OREnsr = ORMnsr;
105520131018            OREnor = ORMnor;
105530131018            OREnrv = ORMnrv;
105540131018            OREtrc = 'O ';
105550131018            OREdati = dOREorari;
105560131018            write fnore000;
105570131018          ENDIF;
105580131018        ENDIF;
105590131125       //?Rcd 'O ' - Orari apertura da cancellare
105600131125        IF  v1OraAMda = 0 and v1OraAMa = 0 and
105610131125            v1OraPMda = 0 and v1OraPMa = 0;
105620131125          k_OREtrc = 'O ';
105630131125          §OREoramda = v1OraAMda;
105640131125          §OREorama  = v1OraAMa;
105650131125          §OREorapda = v1OraPMda;
105660131125          §OREorapa  = v1OraPMa;
105670131125          chain  %kds(k05fnore) FNORE01L;
105680131125          IF  %found(FNORE01L);
105690131125            delete fnore000;
105700131125          ENDIF;
105710131125        ENDIF;
105720161020
105730161020       //?Rcd 'DT' - Dta Pronta Merce
105740170524       //?         --> Data Ritiro Calcolata e Anticipo
105750161020        k_OREtrc = 'DT';
105760161027        clear dOREdt;
105770161027        §OREdpm = inv_V1dpm;
105780170524       //?Per immissione o copia devo salvare i dati relativi
105790170524       //?alla data ritiro calcolata se potevo anticipare e se ho anticipato
105800170524       //?in variazione riporto quello che c'era già sul file
105810170524       //?anche se viene variata la data di ritiro
105820170524        §OREdar = %editc(DarCalcolata:'X');
105830170524        IF  wOkPosticipa;
105840170524          §OREposd = 'S';
105850170524        ENDIF;
105860170524        IF  wAnticipa;
105870170524          §OREant = 'S';
105880170524        ENDIF;
105890170524        IF  Anticipato;
105900170524          §OREmod = 'S';
105910170524        ENDIF;
105920170524        §OREgga = %editc(ggAnticipo:'X');
105930161020        chain  %kds(k05fnore) FNORE01L;
105940161020        IF  %found(FNORE01L);
105950161027          OREdati = dOREdt;
105960161020          update fnore000;
105970161020        ELSE;
105980161020          clear fnore000;
105990161020          OREpoe = ORMpoe;
106000161020          OREnsr = ORMnsr;
106010161020          OREnor = ORMnor;
106020161020          OREnrv = ORMnrv;
106030161020          OREtrc = 'DT';
106040161027          OREdati = dOREdt;
106050161020          write fnore000;
106060161020        ENDIF;
106070131018
106080131018       //?Rcd 'MA' - Mail x ALERT
106090131029        IF  old_w03mail <> w03mail or §RMfpr = 'P';
106100131018          k_OREtrc = 'MA';
106110131029          chain  %kds(k05fnore) FNORE01L;
106120131018          IF  %found(FNORE01L);
106130131029            IF  w03mail <> *blanks;
106140131029              OREdati = w03mail;
106150131029              update fnore000;
106160131029            ELSE;
106170131029              delete fnore000;
106180131029            ENDIF;
106190131018          ELSE;
106200131029            IF  w03mail <> *blanks;
106210131029              clear fnore000;
106220131029              OREpoe = ORMpoe;
106230131029              OREnsr = ORMnsr;
106240131029              OREnor = ORMnor;
106250131029              OREnrv = ORMnrv;
106260131029              OREtrc = 'MA';
106270131029              OREdati = w03mail;
106280131029              write fnore000;
106290131029            ENDIF;
106300131018          ENDIF;
106310131018        ENDIF;
106320131018
106330131018       //?Rcd 'S ' - SMS x ALERT
106340131029        IF  old_w03sms <> w03sms or §rmfpr = 'P';
106350131018          k_OREtrc = 'S ';
106360140422          §OREsms = w03sms;
106370131029          chain  %kds(k05fnore) FNORE01L;
106380131018          IF  %found(FNORE01L);
106390131029            IF  w03sms <> *blanks;
106400140422              OREdati = dOREsms;
106410131029              update fnore000;
106420131029            ELSE;
106430131029              delete fnore000;
106440131029            ENDIF;
106450131018          ELSE;
106460131029            IF  w03sms <> *blanks;
106470131029              clear fnore000;
106480131029              OREpoe = ORMpoe;
106490131029              OREnsr = ORMnsr;
106500131029              OREnor = ORMnor;
106510131029              OREnrv = ORMnrv;
106520131029              OREtrc = 'S ';
106530140422              OREdati = dOREsms;
106540131029              write fnore000;
106550131029            ENDIF;
106560131018          ENDIF;
106570131018        ENDIF;
106580160310
106590160406       //?Rcd 'MC' - Mail x Conferma Prenotazione Ritiro
106600160406        IF  old_w04mail <> w04mail;
106610160310          k_OREtrc = 'MC';
106620160310          chain  %kds(k05fnore) FNORE01L;
106630160310          IF  %found(FNORE01L);
106640160406            IF  w04mail <> *blanks;
106650160406              OREdati = w04mail;
106660160310              update fnore000;
106670160310            ELSE;
106680160310              delete fnore000;
106690160310            ENDIF;
106700160310          ELSE;
106710160406            IF  w04mail <> *blanks;
106720160310              clear fnore000;
106730160310              OREpoe = ORMpoe;
106740160310              OREnsr = ORMnsr;
106750160310              OREnor = ORMnor;
106760160310              OREnrv = ORMnrv;
106770160310              OREtrc = 'MC';
106780160406              OREdati = w04mail;
106790160310              write fnore000;
106800160310            ENDIF;
106810160310          ENDIF;
106820160310        ENDIF;
106830160310
106840160406       //?Rcd 'SC' - SMS x Conferma Prenotazione Ritiro
106850160406        IF  old_w04sms <> w04sms;
106860160310          k_OREtrc = 'SC';
106870160310          chain  %kds(k05fnore) FNORE01L;
106880160310          IF  %found(FNORE01L);
106890160406            IF  w04sms <> *blanks;
106900160406              OREdati = w04sms;
106910160310              update fnore000;
106920160310            ELSE;
106930160310              delete fnore000;
106940160310            ENDIF;
106950160310          ELSE;
106960160406            IF  w04sms <> *blanks;
106970160310              clear fnore000;
106980160310              OREpoe = ORMpoe;
106990160310              OREnsr = ORMnsr;
107000160310              OREnor = ORMnor;
107010160310              OREnrv = ORMnrv;
107020160310              OREtrc = 'SC';
107030160406              OREdati = w04sms;
107040160310              write fnore000;
107050160310            ENDIF;
107060160310          ENDIF;
107070160310        ENDIF;
107080140507
107090150305       //?Se sono in immissione
107100160208       //?o copia ORM
107110140603       //?oppure sono in conferma da VAO ma non ho scritto il rcd
107120160208           IF  *in01 or *in03 or
107130140603               (§RMfpr = 'C' and not wOKrcdg);
107140140507         //?Se flag impostati memorizzo rcd 'G'
107150160310           IF  §OREfiso <> *blanks or §OREfimo <> *blanks or
107160160310               §OREfmco <> *blanks or §OREfsco <> *blanks;
107170140507             k_OREtrc = 'G ';
107180140507             chain  %kds(k05fnore) FNORE01L;
107190140507             IF  %found(FNORE01L);
107200140507               OREdati = dOREgen;
107210140507               update fnore000;
107220140507             ELSE;
107230140507               clear fnore000;
107240140507               OREpoe = ORMpoe;
107250140507               OREnsr = ORMnsr;
107260140507               OREnor = ORMnor;
107270140507               OREnrv = ORMnrv;
107280140507               OREtrc = 'G ';
107290140507               OREdati = dOREgen;
107300140507               write fnore000;
107310140507             ENDIF;
107320140507           ENDIF;
107330140507         ENDIF;
107340131014
107350131014       ENDSR;
107360131014
107370131014       //--------------------------------------------------------------------
107380160406       //?F4 - Alert ORM Commissionato.
107390131014       //--------------------------------------------------------------------
107400131014       BEGSR F04D01;
107410131014
107420131025         wEndF04 = *off;
107430131025         wOkF6F04 = *off;
107440140423         IF  not wErrF04;
107450131030           *in28 = *off;
107460131030         ENDIF;
107470131212
107480131212       //?Salvo i dati della videata
107490131212         vidolddA = vidnewA;
107500131025
107510131025       //?emetto la videata relativa alla MAIL e SMS
107520131025         write fior051;
107530140423         *in28 = werralert;
107540140423         werralert = *off;
107550131025       //?emetto la window
107560131025         DOW not wEndF04;
107570131025           exfmt fior053;
107580131025           clear  w03msg;
107590131025           *in28 = *off;
107600131025         SELECT;
107610131025       //?F12-Ritorno
107620131025           WHEN  *inkl;
107630131025             wEndF04 = *on;
107640131025             *in28 = *off;
107650131025             *in90 = *on;
107660131212             vidnewA = vidolddA;
107670131025       //?F6-Conferma
107680131025           WHEN  *inkf;
107690131025             exsr CtrDatiF04;
107700131025             IF  not *in28;
107710131025               wEndF04 = *on;
107720131025               wOkF6F04 = *on;
107730131025             ENDIF;
107740131212             vidolddA = vidnewA;
107750131025       //?F10-Copia n.Telefono
107760131025           WHEN  *inkj;
107770160406             IF  w03sms = *blanks;
107780160310               w03sms = V1ter;
107790160310             ENDIF;
107800131025           OTHER;
107810131025       //?controllo i dati
107820131025             exsr CtrDatiF04;
107830131025           ENDSL;
107840131025         ENDDO;
107850131212       //?Imposto la riga dei tasti funzione
107860131212       ExSr Sr_Tastifun;
107870140509
107880140509       //?Se immissione/copia ORM commissionato e dati alert inseriti
107890140509       //?devo ricalcolare la data di ritiro
107900140512       //?se = ad oggi
107910140509         IF  (*in01 or *in03) and V1com = 'S' and
107920140512             (w03sms <> *blanks or w03mail <> *blanks) and
107930140512              V1dar > 0 and V1dar = oggi_ggmmaa;
107940140509           wCalDtRit = *on;
107950140509         ENDIF;
107960140509
107970140509         *inkf = *off;
107980140509         *inkl = *off;
107990140509         *inkj = *off;
108000131014
108010131014       ENDSR;
108020131011
108030131011       //--------------------------------------------------------------------
108040131011       //?F5 - Altri dati.
108050131011       //--------------------------------------------------------------------
108060131011       BEGSR F05D01;
108070131011
108080131011         wEndF05 = *off;
108090131011         wOkF6F05 = *off;
108100131030         IF  not wErrF05;
108110131030           *in28 = *off;
108120131030         ENDIF;
108130131212
108140131212       //?Salvo i dati della videata
108150131212         vidoldd2 = vidnew2;
108160131212
108170131212       //?Decodifico lo stato
108180131212         exsr sr_contrsto;
108190131212
108200131212       //?Decodifico il giro se impostato
108210131212         clear V1Dcgi;
108220131212         IF  V1Ccgi <> *blanks;
108230131212           exsr ctrgiro;
108240131212           IF  D09ocgi <> *blanks;
108250131212             V1Dcgi = D09odes;
108260131212           ENDIF;
108270131212         ENDIF;
108280131011
108290131011       //?emetto la videata relativa agli altri dati
108300131011         write fior051;
108310131011       //?emetto la window dove richiedo gli altri dati
108320131011         DOW not wEndF05;
108330131011           exfmt fior052;
108340131011           clear  w2cmsg;
108350131011           *in28 = *off;
108360131011         SELECT;
108370131011       //?F12-Ritorno
108380131011           WHEN  *inkl;
108390131011             wEndF05 = *on;
108400131011             *in28 = *off;
108410131011             *in90 = *on;
108420131212             vidnew2 = vidoldd2;
108430131212             wforzaqta = *off;
108440131011       //?F6-Conferma
108450131011           WHEN  *inkf;
108460131017             exsr CtrDatiF05;
108470131011             IF  not *in28;
108480131011               wEndF05 = *on;
108490131011               wOkF6F05 = *on;
108500131011             ENDIF;
108510131212             vidoldd2 = vidnew2;
108520131011           OTHER;
108530131011       //?controllo i dati
108540131017             exsr CtrDatiF05;
108550131011           ENDSL;
108560131011         ENDDO;
108570131212       //?Imposto la riga dei tasti funzione
108580131212       ExSr Sr_Tastifun;
108590131011
108600131011       ENDSR;
108610160406
108620160406       //--------------------------------------------------------------------
108630160406       //?F13 - Conferma Prenotazione.
108640160406       //--------------------------------------------------------------------
108650160406       BEGSR F13D01;
108660160406
108670160406         wEndF13 = *off;
108680160406         wOkF6F13 = *off;
108690160406         IF  not wErrF13;
108700160406           *in28 = *off;
108710160406         ENDIF;
108720160406
108730160413       //?Immissione ORM manuale o copia
108740160406       //?e non ci sono ancora i dati
108750160406         IF  (*in01 or *in03) and
108760160406             (V1Ctco = 'M' or V1Ctco = 'E') and
108770160406              W04sms = *blanks and W04mail = *blanks;
108780160406         //?recupero la mail/sms dall'anagrafica clienti ritiro
108790160406           exsr RecDatiConf;
108800160406         ENDIF;
108810160406
108820160406       //?Se è richiamato da Conferma VAS
108830160406       //?proteggo i dati per Conferma Prenotazione
108840160406       //?a meno che non stia emettendo la videata per sistemare
108850160406       //?eventuali errori
108860160406         IF  §RMfpr = 'C' and not wErrConferma;
108870160406           *in20 = *on;
108880160406         ENDIF;
108890160406
108900160406       //?Salvo i dati della videata
108910160406         vidolddC = vidnewC;
108920160406
108930160406       //?emetto la videata relativa alla MAIL e SMS
108940160406         write fior051;
108950160406         *in28 = wErrConferma;
108960160406         wErrConferma = *off;
108970160406       //?emetto la window
108980160406         DOW  not wEndF13;
108990160406           exfmt fior054;
109000160406           clear w04msg;
109010160406           *in28 = *off;
109020160406           SELECT;
109030160406       //?F6-Conferma
109040160406           WHEN  *inkf;
109050160406             exsr CtrDatiF13;
109060160406             IF  not *in28;
109070160406               wEndF13 = *on;
109080160406               wOkF6F13 = *on;
109090160406             ENDIF;
109100160406             vidolddC = vidnewC;
109110160406       //?F10-Copia n.Telefono
109120160406           WHEN  *inkj;
109130160406             IF  w04sms = *blanks;
109140160406               w04sms = V1ter;
109150160406             ENDIF;
109160160406             HWriga = 06;
109170160406             HWcolo = 07;
109180160406       //?F12-Ritorno
109190160406           WHEN  *inkl;
109200160406             wEndF13 = *on;
109210160406             *in28 = *off;
109220160406             *in90 = *on;
109230160406             vidnewC = vidolddC;
109240160406       //?Enter
109250160406       //?controllo i dati
109260160406           OTHER;
109270160406             exsr CtrDatiF13;
109280160406           ENDSL;
109290160406         ENDDO;
109300160406
109310160406         *inkf = *off;
109320160406         *inkl = *off;
109330160406         *inkj = *off;
109340160406
109350160406       ENDSR;
109360130912
109370130912       //--------------------------------------------------------------------
109380130912       //?F14 - Interrogazione orari di servizio.
109390130912       //--------------------------------------------------------------------
109400130913       BEGSR F14D01;
109410150921
109420150921       //?Se non c'è la filiale ritiro e non ho peso/volume/bancali e non è
109430150921       //?un mittente codificato e siamo in immissione/copia
109440150921       //?non posso interrogare gli orari
109450150921         IF  num_V1Cpor = *zeros and V1pkg = 0 and V1vlm = 0 and
109460150921             V1bnc = 0 and (*in01 or *in03) and ds_v1ccra = 0;
109470150921           V1Cmsg = 'Per F14 serve la fil.ritiro e +
109480150921                     almeno 1 tra peso/vol./bancali';
109490150921           *in28 = *on;
109500150921           *in44 = *on;
109510150921           leavesr;
109520150921         ENDIF;
109530131024
109540131024       //?Se non presente calcolo la filiale ritiro
109550131024         IF  num_V1Cpor = *zeros and
109560131024            (*in01 or *in03);
109570131024           exsr  CALPOR;
109580140109         //?Recupero gli ambiti per il recupero del giro da ACR
109590140109           exsr sr_caragr;
109600131024           IF  *in28;
109610131024             leavesr;
109620131024           ENDIF;
109630131024         ENDIF;
109640130912
109650140319       //?Posso richiamare il pgm di int.orari di servizio solo se ho già
109660130912       //?la filiale di ritiro
109670130912         IF  num_V1Cpor = *zeros;
109680130912           V1Cmsg = 'Per interrogare gli orari serve la filiale ritiro';
109690130912           *in28 = *on;
109700130912           *in59 = *on;
109710130912           leavesr;
109720130912         ENDIF;
109730130912       //?l'indirizzo completo del mittente
109740130912         IF  V1car = *blanks or V1lor = *blanks;
109750130912           V1Cmsg = 'Per interrogare gli orari serve l''indirizzo del +
109760130912                     mittente';
109770130912           *in28 = *on;
109780130912           *in63 = *on;
109790130912           leavesr;
109800130912         ENDIF;
109810161116       //?il peso/volume/bancali
109820161116         IF  V1pkg = 0 and V1vlm = 0 and V1bnc = 0;
109830161116           V1Cmsg = 'Per interrogare gli orari occorre inserire +
109840161116                     almeno 1 tra peso/vol./bancali';
109850161116           *in28 = *on;
109860161116           *in44 = *on;
109870161116           leavesr;
109880161116         ENDIF;
109890161116       //?Calcolo il peso/volume se no in caso di conferma proposta si azzerano
109900161116          exsr sr_pesvol;
109910161116
109920161116       //?Cerco il giorno della settimana di "oggi"
109930161116         exsr DatiPerOrari;
109940140318
109950140318       //?Cerco eventuale ora presunta ritiro
109960140318         clear fior01ds;
109970140319       //?ma solo se ORM in distinta e NON quadrato
109980140318         IF  V1ndc > 0 and V1fao < 600;
109990140318           IOR01fgs = num_V1cpor;
110000140318           IOR01ndc = V1ndc;
110010140318           IOR01poe = V1cpoe;
110020140318           IOR01nsr = V1nsr;
110030140318           IOR01nor = V1nor;
110040140318           IOR01nrv = V1nrv;
110050140318           fior01r (fior01ds);
110060140318         ENDIF;
110070130912
110080140318         clear trulorsds;
110090130912         IOREfil = num_V1Cpor;
110100130912         IOREcap = V1car;
110110130912         IOREloc = V1lor;
110120130920         IOREnar = V1nar;
110130131024         IF  V1dar <> 0;
110140131024           IOREdta = inv_V1dar;
110150131024         ELSE;
110160131024           IOREdta = inv_V1dao;
110170131024         ENDIF;
110180130912         IOREtser = 'R';
110190131108         IOREgf2 = sav_O95gf2;
110200140318         IOREorp = OOR01ora;
110210161026
110220161116       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
110230161116       //?non è da fare per i ritiri esteri
110240161116         clear dIORE01;
110250170529         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks;
110260161116           §IOREr_mnx = 'S';
110270161116         ENDIF;
110280161026         IOREflo = dIORE01;
110290130912
110300140318         trulorsr (kpjba:trulorsds);
110310130912
110320130912       ENDSR;
110330131014
110340131014       //--------------------------------------------------------------------
110350131014       //?Controllo dati 'Altri Dati'.
110360131014       //--------------------------------------------------------------------
110370131014       BEGSR CtrDatiF05;
110380131030
110390131030         *in47 = *off;
110400131030         *in48 = *off;
110410131030         *in49 = *off;
110420131030         *in51 = *off;
110430131030         *in84 = *off;
110440131014
110450131014         exsr Sr_Contrsto;
110460131014         IF  *in28;
110470131014           leavesr;
110480131014         ENDIF;
110490131014
110500131211         exsr Sr_Contrddt;
110510131014         IF  *in28;
110520131014           leavesr;
110530131014         ENDIF;
110540131014
110550131014         exsr sr_Contrqta1;
110560131014         IF  *in28;
110570131014           leavesr;
110580131014         ENDIF;
110590131202       //?calcolo peso o volume se non impostati a video
110600131202       //?considerando anche le cubature degli automezzi eventualmente immessi
110610131202         exsr sr_pesvol;
110620131014
110630131014         exsr sr_contrcgi;
110640131014         IF  *in28 or *in90;
110650131014           leavesr;
110660131014         ENDIF;
110670131014
110680131014       ENDSR;
110690131014
110700131014       //--------------------------------------------------------------------
110710131017       //?Controllo dati Alert.
110720131014       //--------------------------------------------------------------------
110730131025       BEGSR CtrDatiF04;
110740131014
110750160310       //?Dati Alert ORM Commissionato
110760131017       //?controllo SMS;
110770131025         IF  w03sms <> *blanks;
110780131025           pInCell = %trim(w03sms);
110790131017           clear pOutErr;
110800140722           clear pOutCell;
110810131017           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
110820150526             IF  §RMfpr = 'C';
110830150526               clear W03sms;
110840150526               leavesr;
110850150526             ENDIF;
110860131017             *in28 = *on;
110870160406             HWriga = 04;
110880131017             HWcolo = 26;
110890140424             w03msg = msg(60);
110900131017             leavesr;
110910131017           ENDIF;
110920140722           IF  pOutCell <> *blanks;
110930140722             w03sms = pOutCell;
110940140722           ENDIF;
110950131014         ENDIF;
110960131014
110970131025         IF  w03mail <> *blanks;
110980131025           §emlindi = w03mail;
110990131014           xemail (dsemail);
111000131014           IF  §emlerro <> '0';
111010150526             IF  §RMfpr = 'C';
111020150526               clear W03mail;
111030150526               leavesr;
111040150526             ENDIF;
111050160406             HWriga = 06;
111060131017             HWcolo = 07;
111070131014             *in28 = *on;
111080140424             w03msg = msg(55);
111090131014             leavesr;
111100131014           ENDIF;
111110140722           IF  §emlindo <> *blanks;
111120140722             w03mail = §emlindo;
111130140722           ENDIF;
111140131014         ENDIF;
111150131014
111160131014       ENDSR;
111170160406
111180160406       //--------------------------------------------------------------------
111190160406       //?Controllo dati Conferma Prenotazione.
111200160406       //--------------------------------------------------------------------
111210160406       BEGSR CtrDatiF13;
111220160406
111230160406       //?Dati Conferma Prenotazione
111240160406         IF  w04sms <> *blanks;
111250160406           pInCell = %trim(w04sms);
111260160406           clear pOutErr;
111270160406           clear pOutCell;
111280160406           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
111290160406             *in28 = *on;
111300160406             H4riga = 04;
111310160406             H4colo = 26;
111320160406             w04msg = msg(60);
111330160406             leavesr;
111340160406           ENDIF;
111350160406           IF  pOutCell <> *blanks;
111360160406             w04sms = pOutCell;
111370160406           ENDIF;
111380160406         ENDIF;
111390160406
111400160406         IF  w04mail <> *blanks;
111410160406           §emlindi = w04mail;
111420160406           xemail (dsemail);
111430160406           IF  §emlerro <> '0';
111440160406             H4riga = 06;
111450160406             H4colo = 07;
111460160406             *in28 = *on;
111470160406             w04msg = msg(55);
111480160406             leavesr;
111490160406           ENDIF;
111500160406           IF  §emlindo <> *blanks;
111510160406             w04mail = §emlindo;
111520160406           ENDIF;
111530160406         ENDIF;
111540160406
111550160406       //?Dati Conferma VAS e avevo il flag di OK conferma ma non ho i dati
111560160406       //?pulisco il flag in modo da non scrivere il file spia
111570160406         IF  §RMfpr = 'C' and
111580160406             W04mail = *blanks and W04sms = *blanks;
111590160406           wconferma = *off;
111600160406         ENDIF;
111610160406
111620160406         sav_mailconf = w04mail;
111630160406         sav_smsconf  = w04sms;
111640160406
111650160406       ENDSR;
111660140320
111670140320       //--------------------------------------------------------------------
111680140320       //?Cerco orari servizio.
111690140320       //--------------------------------------------------------------------
111700140320       BEGSR CercaOrari;
111710161026
111720161026       //?Se immissione/copia o conferma ORM commissionati
111730161026       //?Visualizzo orari servizio
111740161026         IF  *in01 or *in03 or §RMfpr = 'O';
111750161026           *in95 = *on;
111760161026         ENDIF;
111770140320
111780160526       //?Richiamo il TRUL solo se non è ritiro Estero
111790140320         clear trulorsds;
111800140320         clear trulor2ds;
111810160526         IF  V1nar = *blanks;
111820140320         IOREfil = num_V1Cpor;
111830140320         IOREcap = V1car;
111840140320         IOREloc = V1lor;
111850140320         IOREnar = V1nar;
111860140320         IF  V1dar <> 0;
111870140320           IOREdta = inv_V1dar;
111880140320         ELSE;
111890140320           IOREdta = inv_V1dao;
111900140320         ENDIF;
111910140320         IOREtser = 'R';
111920140320         IOREgf2 = sav_O95gf2;
111930140320         trulorsr (kpjba:trulorsds:trulor2ds);
111940140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
111950140320           *in95 = *off;
111960140320           leavesr;
111970140320         ELSE;
111980140320           V1ostis = OOR2stis;
111990140320           V1ostfs = OOR2stfs;
112000140320         ENDIF;
112010160526         ENDIF;
112020160526       //?Se ritiro Estero chiamo nuovo FIORE00R
112030160526         IF  V1nar <> *blanks;
112040160526           clear FIORE00DS;
112050160526           IORE00por = num_V1Cpor;
112060160526           IORE00nar = V1nar;
112070160526           fiore00r (fiore00ds);
112080160526           IF  OORE00err <> *blanks;
112090160526             *in95 = *off;
112100160526             leavesr;
112110160526           ELSE;
112120160526             V1ostis = OORE00min;
112130160526             V1ostfs = OORE00max;
112140160526           ENDIF;
112150160526         ENDIF;
112160161115
112170161115         exsr DatiPerOrari;
112180161021
112190161021       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
112200161021       //?non è da fare per i ritiri esteri
112210161115       //?non è da fare se non ho gli orari minimo e massimo
112220161027         OrariEstesi = *off;
112230161115         IF  wpkg > 0 and wpkg < d§DFTpkgdt and V1nar = *blanks and
112240161027             OOR2miis > *zeros and OOR2mxfs > *zeros;
112250161021           V1ostis = OOR2miis;
112260161021           V1ostfs = OOR2mxfs;
112270161027           OrariEstesi = *on;
112280161021         ENDIF;
112290140320
112300140320         wOKorariSer = *on;
112310140320
112320140320       ENDSR;
112330161021
112340161021       //--------------------------------------------------------------------
112350161026       //?Aggiungo/Sottraggo 1 gg lavorativo alla data ritiro
112360161021       //--------------------------------------------------------------------
112370161026       BEGSR AddSubUnoDar;
112380161027
112390161027       //?Non dovrebbe essere possibile, ma non si sa mai
112400161027         IF  V1dar = 0;
112410161027           *in28 = *on;
112420161027           V1Cmsg = 'PageUp/PageDown non possibile senza data ritiro';
112430161027           leavesr;
112440161027         ENDIF;
112450161027
112460161027         clear XGIOLAVDS;
112470161027         clear wlbdat;
112480161027         IXGLdata = inv_V1dar;
112490161027         IF  dsp_aid = c_PageDown;
112500161027           IXGLsub  = 'S';
112510161027         ENDIF;
112520161027         IF  dsp_aid = c_PageUp;
112530161027           IXGLadd  = 'S';
112540161027         ENDIF;
112550161027         IXGLgg   = 1;
112560161027         IXGLlav  = 'S';
112570161027         IXGLpa   = 'P';
112580161027         IXGLfil  = num_V1Cpor;
112590161027         XGIOLAV (xgiolavds);
112600161027         IF  OXGLerr <> *blanks;
112610161027           leavesr;
112620161027         ENDIF;
112630161027
112640161122       //?Solo se immissione
112650161122         IF  *in01;
112660161028         //?Se data ritiro inferiore alla data ritiro calcolata errore
112670161028           IF  OXGldata < OOR97dar and
112680161028               OXGLdata < OOR97dmin;
112690161028             *in28 = *on;
112700161028             V1Cmsg = msg(51);
112710161028             leavesr;
112720161028           ENDIF;
112730161028         ENDIF;
112740161027
112750161108       //?Supera la data max consentita
112760161108         IF  OXGLdata > OOR97dmaxb;
112770161108           *in28 = *on;
112780161108           V1Cmsg = msg(35);
112790161108           V1Cmsg = %trim(V1Cmsg) + ' supera '+
112800161108           d§dftgg2 + ' gg. dalla data immissione ORM';
112810161108           leavesr;
112820161108         ENDIF;
112830161108
112840161108       //?Non può essere inferiore alla data immissione ORM
112850161108         IF  OXGLdata < inv_V1dao;
112860161108           *in28 = *on;
112870161108           V1Cmsg = msg(65);
112880161108           leavesr;
112890161108         ENDIF;
112900161027
112910161110         clear wlbdat;
112920161027         G02inv = OXGLdata;
112930161027         G02err = '3';
112940161027         xsrda8 (wlbdat);
112950161027         V1dar = G02dat;
112960161027         inv_v1dar = OXGLdata;
112970161108         old_invv1dar = OXGLdata;
112980161021
112990161021       ENDSR;
113000130912
113010100218      /end-free
113020090206
113030090206      *--------------------------------------------------------------------*
113040090206      * File FNORA00F
113050090206      *--------------------------------------------------------------------*
113060090206     c     sr_wrtapp     begsr
113070090206
113080090206     c     kfnora        chain     fnora01l
113090090206     c                   if        not %found(fnora01l)
113100090206     c                   clear                   fnora000
113110090206     c                   eval      orapoe = ormpoe
113120090206     c                   eval      oransr = ormnsr
113130090206     c                   eval      oranor = ormnor
113140090206     c                   eval      oranrv = ormnrv
113150090407     c                   eval      orafar = 100
113160090216     c                   eval      oradae = v1dfo
113170090216     c                   eval      oraore = v1ofo
113180090206     c                   eval      oraref = wappref
113190090206     c                   eval      oranote = wappnota1 + wappnota2
113200090206     c                   eval      oradata = oggi_aammgg
113210090206     c                   time                    w0140
113220090206     c                   movel     w0140         oraora
113230090206     c                   eval      oraute = knmus
113240090206     c                   write     fnora000
113250090206     c                   endif
113260090206
113270090206     c                   if        %found(fnora01l)
113280090206     c                   eval      oraref = wappref
113290090206     c                   eval      oranote = wappnota1 + wappnota2
113300090206     c                   eval      oradata = oggi_aammgg
113310090206     c                   time                    w0140
113320090206     c                   movel     w0140         oraora
113330090206     c                   eval      oraute = knmus
113340090206     c                   update    fnora000
113350090206     c                   endif
113360090206
113370090206     c                   endsr
113380070903
113390010704      **********************************************************************
113400010704      * RICHIAMO IMMISSIONE PREPAGATI
113410010704      **********************************************************************
113420010704     C     Sr_Immprep    begsr
113430030623
113440030623     c                   Clear                   wtrulvid
113450010704
113460020522      * richiamo il programma per la scelta delle stampanti
113470030307     c                   Clear                   wdatibolla
113480030307     c                   Eval      wdatibolla = 'Prepagati'
113490030623     c                   Eval      wdatibolA4 = 'Prepag_A4'
113500030623     c                   Eval      wdatibolA5 = 'Prepag_A5'
113510020522     c                   exsr      sr_scestp
113520020522     c   92              goto      endsrimmprep
113530010704
113540010704      * Richiamo immissione prepagati
113550010704     C                   clear                   kpjbu
113560010704     C                   clear                   dtasv
113570010704     C                   clear                   dblp
113580010704
113590010704     C                   move      'O'           §LPfpr
113600010704     C                   movel     ORMpoe        §LPpoe
113610010704     C                   movel     ORMnsr        §LPnsr
113620010704     C                   movel     ORMnor        §LPnor
113630010704     C                   movel     ORMnrv        §LPnrv
113640010704
113650010704     C                   call      'FNLS01R'
113660010704     C                   parm                    kpjba
113670010704     C                   parm                    dblp
113680010704     C                   parm                    dtasv
113690030623     c                   Parm                    Trul90ds
113700010704
113710010704     C                   eval      okbolla = '1'
113720010704
113730010704     C                   if        §lpfpr = '4' or §lpfpr = '3'
113740010704     C                             or §lpfpr = '5'
113750010704     C                   eval      okbolla = ' '
113760010704     C                   endif
113770010704
113780010704     C     endsrimmprep  endsr
113790020522      **********************************************************************
113800020522      * scelta stampante per immissione bolla
113810020522      **********************************************************************
113820020522     c     sr_scestp     begsr
113830020522
113840020522     C                   clear                   kpjbu
113850020522     c                   setoff                                       92
113860020522
113870020522      * Richiesta stampanti
113880020522     C                   if        conta = *zeros
113890030623     C                   clear                   trul90ds
113900030623      * se non serve non faccio vedere il video della scelta stampanti
113910030623     c                   If        wtrulvid <> *Blanks
113920030623     C                   movel     'X'           D90rse
113930030623     C                   movel     'X'           D90rsb
113940030623     c                   Else
113950030623     C                   movel     'S'           D90rse
113960030623     C                   movel     'S'           D90rsb
113970030623     c                   EndIf
113980030623     C                   call      'TRUL90R'
113990020522     C                   parm                    kpjba
114000030623     C                   parm                    trul90ds
114010020522      * F3=Fine
114020030623     C                   if        D90f3 = '1'
114030020522     C                   eval      *in92 = *on
114040020522     C                   goto      endstp
114050020522     C                   endif
114060030307
114070030307     C                   z-add     103           lung
114080020522
114090020522      * Ovrprtf Segnacolli
114100030623     C                   movea     D90pre        cm3(28)
114110030623     C                   movea     D90mde        cm3(50)
114120020522     C                   clear                   comman
114130020522     C                   movea     cm3           comman
114140020522     C                   CALL      'QCMDEXC'                            92
114150020522     C                   PARM                    comman
114160020522     C                   PARM                    lung
114170020522     C   92              goto      endstp
114180030623
114190030623      * Bolle - Nuovo modulo
114200030623
114210030623     c                   Z-Add     110           lung
114220030623
114230030623      * Ovrprtf Bolle A4
114240030623     c                   Movea     D90prb4       cma4(30)
114250030623     c                   Movea     D90mdb4       cma4(52)
114260030623     c                   Movea     wdatibolA4    cma4(96)
114270030623     c                   Clear                   Comman
114280030623     c                   Movea     cmA4          Comman
114290030623     c                   Call      'QCMDEXC'                            92
114300030623     c                   Parm                    Comman
114310030623     c                   Parm                    Lung
114320030623     c   92              goto      endstp
114330030623
114340030623      * Ovrprtf Bolle A5
114350030623     c                   Movea     D90prb5       cmA5(30)
114360030623     c                   Movea     D90mdb5       cmA5(52)
114370030623     c                   Movea     wdatibolA5    cma5(96)
114380030623     c                   Clear                   Comman
114390030623     c                   Movea     cmA5          Comman
114400030623     c                   Call      'QCMDEXC'                            92
114410030623     c                   Parm                    Comman
114420030623     c                   Parm                    Lung
114430030623     c   92              goto      endstp
114440020522
114450030623      * se non faccio vedere il video non aggiorno il contatore
114460030623     c                   If        wtrulvid = *Blanks
114470020522     C                   eval      conta = conta +1
114480030623     c                   EndIf
114490020522     C                   endif
114500020522
114510020522     c     endstp        endsr
114520001019      **********************************************************************
114530001023      * SCRIVE storico variazioni
114540001019      **********************************************************************
114550001019     C     Sr_Storico    BEGSR
114560001019
114570001019     C                   clear                   fnorv000
114580001019     C                   eval      ORVpoe = ORMpoe
114590001019     C                   eval      ORVnsr = ORMnsr
114600001019     C                   eval      ORVnor = ORMnor
114610001019     C                   eval      ORVnrv = ORMnrv
114620001019     C                   eval      ORVtor = ORMtor
114630001019     C                   eval      ORVcor = ORMcor
114640001019     C                   eval      ORVrso = ORMrso
114650001019     C                   eval      ORVino = ORMino
114660001019     C                   eval      ORVcao = ORMcao
114670001019     C                   eval      ORVloo = ORMloo
114680001019     C                   eval      ORVpro = ORMpro
114690001019     C                   eval      ORVnao = ORMnao
114700001019     C                   eval      ORVcra = ORMcra
114710001019     C                   eval      ORVrsr = ORMrsr
114720001019     C                   eval      ORVinr = ORMinr
114730001019     C                   eval      ORVcar = ORMcar
114740001019     C                   eval      ORVlor = ORMlor
114750001019     C                   eval      ORVprr = ORMprr
114760001019     C                   eval      ORVnar = ORMnar
114770001019     C                   eval      ORVrer = ORMrer
114780001019     C                   eval      ORVter = ORMter
114790001019     C                   eval      ORVcrc = ORMcrc
114800001019     C                   eval      ORVrsc = ORMrsc
114810001019     C                   eval      ORVinc = ORMinc
114820001019     C                   eval      ORVloc = ORMloc
114830001019     C                   eval      ORVcac = ORMcac
114840001019     C                   eval      ORVprc = ORMprc
114850001019     C                   eval      ORVnac = ORMnac
114860001019     C                   eval      ORVffd = ORMffd
114870001019     C                   eval      ORVdar = ORMdar
114880001019     C                   eval      ORVorr = ORMorr
114890001019     C                   eval      ORVnam = ORMnam
114900001019     C                   eval      ORVncl = ORMncl
114910001019     C                   eval      ORVpkg = ORMpkg
114920001019     C                   eval      ORVvlm = ORMvlm
114930001019     C                   eval      ORVbnc = ORMbnc
114940001019     C                   eval      ORVblc = ORMblc
114950001019     C                   eval      ORVatt = ORMatt
114960001019     C                   eval      ORVmtc = ORMmtc
114970001019     C                   eval      ORVpag = ORMpag
114980001019     C                   eval      ORVksc = ORMksc
114990001019     C                   eval      ORVctr = ORMctr
115000001024     C                   eval      ORVddt = ORMddt
115010001019     C                   eval      ORVpor = ORMpor
115020130416     C                   eval      ORVzor = ORMzor
115030001019     C                   eval      ORVpoc = ORMpoc
115040001019     C                   eval      ORVno1 = ORMno1
115050001019     C                   eval      ORVno2 = ORMno2
115060001019     C                   eval      ORVcst = ORMcst
115070001019     C                   eval      ORVvcs = ORMvcs
115080001019     C                   eval      ORVfcs = ORMfcs
115090001019     C                   eval      ORVdfo = ORMdfo
115100001019     C                   eval      ORVofo = ORMofo
115110001019     C                   eval      ORVfao = ORMfao
115120001027     C                   eval      ORVsto = ORMsto
115130001019     C                   eval      ORVdst = ORMdst
115140001019     C                   eval      ORVnpg = ORMnpg
115150001019     C                   eval      ORVndc = ORMndc
115160001019     C                   eval      ORVddc = ORMddc
115170001019     C                   eval      ORVstp = ORMstp
115180001027     C                   eval      ORVrfa = ORMrfa
115190070706     C                   eval      ORVeti = ORMeti
115200001103     C                   eval      ORVsto = ORMsto
115210010119     C                   eval      ORVspi = ORMspi
115220001019     C                   eval      ORVflo = ORMflo
115230010116     C                   z-add     oggi_aammgg   ORVdtv
115240001019     C                   movel     w0140         ORVorv
115250001019     C                   movel     knmus         ORVutv
115260040908     c                   Eval      OrvDtt = oggi_aammgg
115270161115     c                   eval      ORVrmp = ORMrmp
115280001019     C                   write     fnorv000
115290131024
115300131024      /free
115310131024       //?Scrivo anche le estensioni
115320131024         IF  wOkestensione;
115330131024           setll    (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115340131024           reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115350140520           DOW  not %eof(FNORE01L);
115360131024             clear FNORVE00;
115370131024             ORVEpoe = OREpoe;
115380131024             ORVEnsr = OREnsr;
115390131024             ORVEnor = OREnor;
115400131024             ORVEnrv = OREnrv;
115410131024             ORVEdtv = ORVdtv;
115420131024             ORVEorv = ORVorv;
115430131024             ORVEutv = ORVutv;
115440131024             ORVEtrc = OREtrc;
115450131024             ORVEdati = OREdati;
115460131024             write FNORVE00;
115470140520             reade(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORE01L;
115480140520           ENDDO;
115490131024         ENDIF;
115500131024      /end-free
115510001019
115520001019     C                   endsr
115530001019      **********************************************************************
115540001023      * SCRIVE proposte variazioni
115550001019      **********************************************************************
115560001103     C     Sr_CarProposteBEGSR
115570140227
115580140227      /free
115590140227       //?Se è stata fatta una proposta di nota AUT devo aggiornare FNORP
115600140227       //?scritto da FIORT1R
115610140227         IF  wF17write;
115620140227           chain (kpoe:knsr:knor:knrv:OORT1dim:OORT1oim) FNORP01L;
115630140227           IF  %found(FNORP01L);
115640140227             exsr move_proposte;
115650140227             update fnorp000;
115660140227           ENDIF;
115670140227         ENDIF;
115680140227      /end-free
115690001019
115700140227     c                   IF        not wF17write
115710010116     C                   clear                   fnorp000
115720010116     C                   eval      ORPpoe = v1cpoe
115730010116     C                   eval      ORPnsr = v1nsr
115740010116     C                   eval      ORPnor = v1nor
115750010116     C                   eval      ORPnrv = v1nrv
115760140227     c                   exsr      move_proposte
115770140227     C                   z-add     oggi_aammgg   ORPdtv
115780140227     C                   movel     w0140         ORPorv
115790140227     C                   movel     knmus         ORPutv
115800001019     C                   write     fnorp000
115810140227     c                   ENDIF
115820131024
115830131024      /free
115840131024       //?Scrivo anche le estensioni
115850131024         IF  v1OraAMda <> 0 or v1OraAMa <> 0 or
115860131024             v1OraPMda <> 0 or v1OraPMa <> 0;
115870131024           §OREoramda = v1OraAMda;
115880131024           §OREorama  = v1OraAMa;
115890131024           §OREorapda = v1OraPMda;
115900131024           §OREorapa  = v1OraPMa;
115910131024           clear FNORPE00;
115920131024           ORPEpoe = ORPpoe;
115930131024           ORPEnsr = ORPnsr;
115940131024           ORPEnor = ORPnor;
115950131024           ORPEnrv = ORPnrv;
115960131024           ORPEdtv = ORPdtv;
115970131024           ORPEorv = ORPorv;
115980131024           ORPEutv = ORPutv;
115990131024           ORPEtrc = 'O ';
116000131024           ORPEdati = dOREorari;
116010131024           write FNORPE00;
116020131024         ENDIF;
116030161020         //?RCD DT - Data Pronta Merce
116040161027         clear dOREdt;
116050161027         §OREdpm = inv_v1dpm;
116060170605         §OREdar = %editc(DarCalcolata:'X');
116070170605         IF  wOkPosticipa;
116080170605           §OREposd = 'S';
116090170605         ENDIF;
116100170605         IF  wAnticipa;
116110170605           §OREant = 'S';
116120170605         ENDIF;
116130170605         IF  Anticipato;
116140170605           §OREmod = 'S';
116150170605         ENDIF;
116160170605         §OREgga = %editc(ggAnticipo:'X');
116170161020         clear fnorpe00;
116180161020         ORPEpoe = ORPpoe;
116190161020         ORPEnsr = ORPnsr;
116200161020         ORPEnor = ORPnor;
116210161020         ORPEnrv = ORPnrv;
116220161020         ORPEdtv = ORPdtv;
116230161020         ORPEorv = ORPorv;
116240161020         ORPEutv = ORPutv;
116250161020         ORPEtrc = 'DT';
116260161027         ORPEdati = dOREdt;
116270161020         write FNORPE00;
116280160406         IF  w04sms <> *blanks;
116290160311           clear FNORPE00;
116300160311           ORPEpoe = ORPpoe;
116310160311           ORPEnsr = ORPnsr;
116320160311           ORPEnor = ORPnor;
116330160311           ORPEnrv = ORPnrv;
116340160311           ORPEdtv = ORPdtv;
116350160311           ORPEorv = ORPorv;
116360160311           ORPEutv = ORPutv;
116370160311           ORPEtrc = 'SC';
116380160406           ORPEdati = w04sms;
116390160311           write FNORPE00;
116400160311         ENDIF;
116410160406         IF  w04mail <> *blanks;
116420160311           clear FNORPE00;
116430160311           ORPEpoe = ORPpoe;
116440160311           ORPEnsr = ORPnsr;
116450160311           ORPEnor = ORPnor;
116460160311           ORPEnrv = ORPnrv;
116470160311           ORPEdtv = ORPdtv;
116480160311           ORPEorv = ORPorv;
116490160311           ORPEutv = ORPutv;
116500160311           ORPEtrc = 'MC';
116510160406           ORPEdati = w04mail;
116520160311           write FNORPE00;
116530160311         ENDIF;
116540131025         IF  w03sms <> *blanks;
116550140505           §OREsms = w03sms;
116560131024           clear FNORPE00;
116570131024           ORPEpoe = ORPpoe;
116580131024           ORPEnsr = ORPnsr;
116590131024           ORPEnor = ORPnor;
116600131024           ORPEnrv = ORPnrv;
116610131024           ORPEdtv = ORPdtv;
116620131024           ORPEorv = ORPorv;
116630131024           ORPEutv = ORPutv;
116640131024           ORPEtrc = 'S ';
116650140505           ORPEdati = dOREsms;
116660131024           write FNORPE00;
116670131024         ENDIF;
116680131025         IF  w03mail <> *blanks;
116690131024           clear FNORPE00;
116700131024           ORPEpoe = ORPpoe;
116710131024           ORPEnsr = ORPnsr;
116720131024           ORPEnor = ORPnor;
116730131024           ORPEnrv = ORPnrv;
116740131024           ORPEdtv = ORPdtv;
116750131024           ORPEorv = ORPorv;
116760131024           ORPEutv = ORPutv;
116770131024           ORPEtrc = 'MA';
116780131025           ORPEdati = w03mail;
116790131024           write FNORPE00;
116800131024         ENDIF;
116810131024      /end-free
116820001019
116830001019     C                   endsr
116840140227
116850140227      *------------------------------------------------------------------------*
116860140227      *  Muovo i campi del video al file delle proposte
116870140227      *------------------------------------------------------------------------*
116880140227     c     move_proposte begsr
116890140227
116900140227     C                   eval      ORPtor = v1ctor
116910140227     C                   eval      ds_v1cor1 = v1cor1
116920140227     C                   eval      ds_v1cor2 = v1cor2
116930140227     C                   eval      ds_v1cor3 = v1cor3
116940140227     C                   eval      ORPcor = ds_v1ccor
116950140227     C                   eval      ORPrso = v1rso
116960140227     C                   eval      ORPino = v1ino
116970140227     C                   eval      ORPcao = v1cao
116980140227     C                   movel(p)  sav_v1loo35   ORPloo
116990140227     C                   eval      ORPpro = v1pro
117000140227     C                   eval      ORPnao = v1nao
117010140227     C                   eval      ds_v1cra1 = v1cra1
117020140227     C                   eval      ds_v1cra2 = v1cra2
117030140227     C                   eval      ds_v1cra3 = v1cra3
117040140227     C                   eval      ORPcra = ds_v1ccra
117050140227     C                   eval      ORPrsr = v1rsr
117060140227     C                   eval      ORPinr = v1inr
117070140227     C                   eval      ORPcar = v1car
117080140227     C                   movel(p)  v1lor         ORPlor
117090140227     C                   eval      ORPprr = v1prr
117100140227     C                   eval      ORPnar = v1nar
117110140227     C                   eval      ORPrer = v1rer
117120140227     C                   eval      ORPter = v1ter
117130140227     C                   eval      ds_v1crc1 = v1crc1
117140140227     C                   eval      ds_v1crc2 = v1crc2
117150140227     C                   eval      ds_v1crc3 = v1crc3
117160140227     C                   eval      ORPcrc = ds_v1ccrc
117170140227     C                   eval      ORPrsc = v1rsc
117180140227     C                   eval      ORPinc = v1inc
117190140227     C                   movel(p)  sav_v1loc35   ORPloc
117200140227     C                   eval      ORPcac = v1cac
117210140227     C                   eval      ORPprc = v1prc
117220140227     C                   eval      ORPnac = v1nac
117230140227     C                   eval      ORPffd = v1ffd
117240140227      * Inversione data ritiro
117250140227     C                   clear                   wlbdat
117260140227     C                   z-add     v1dar         G02dat
117270140227     C                   movel     *blanks       G02err
117280140227     C                   call      'XSRDA8'
117290140227     C                   parm                    wlbdat
117300140227     C                   z-add     G02inv        ORPdar
117310140227
117320140227     C                   eval      ORPorr = v1orr
117330140227     C                   eval      ORPnam = v1nam
117340140227     C                   eval      ORPncl = v1ncl
117350140227     C                   eval      ORPpkg = v1pkg
117360140227     C                   eval      ORPvlm = v1vlm
117370140227     C                   eval      ORPbnc = v1bnc
117380140227     C                   eval      ORPblc = v1blc
117390140227     C                   eval      ORPatt = v1att
117400140227     C                   eval      ORPmtc = v1mtc
117410140227     C                   eval      ORPpag = v1pag
117420140227     C                   eval      ORPksc = v1ksc
117430140227     C                   eval      ORPctr = v1ctr
117440140227     C                   eval      ORPddt = v1ddt
117450140227     C                   movel     v1cpor        ORPpor
117460140227     C                   movel     v1cpoc        ORPpoc
117470140227     C                   eval      ORPno1 = v1not1
117480140227     C                   eval      ORPno2 = v1not2
117490140227     C                   eval      ORPrfa = v1rfa
117500140227     C                   move      v1sto         ORPsto
117510140227     C                   eval      ORPcst = v1cst
117520140227     C                   eval      ORPvcs = v1vcs
117530140227     C                   eval      ORPfcs = v1fcs
117540140227     C                   eval      ORPdfo = v1dfo
117550140227     C                   eval      ORPofo = v1ofo
117560140227     C                   eval      ORPfao = v1fao
117570140227     C                   eval      ORPdst = v1dst
117580140227     C                   eval      ORPnpg = v1npg
117590140227     C                   eval      ORPndc = v1ndc
117600140227     C                   eval      ORPddc = v1ddc
117610140227     C                   eval      ORPstp = v1stp
117620140227     C                   eval      ORPtap = v1tap
117630140227     C                   eval      ORPeti = v1eti
117640140227     C                   eval      ORPspi = v1spi
117650140227     C                   eval      ORPcau = v3cau
117660140227      * Se forzato blocco destinatario devo pulire il relativo flag
117670140227     C                   if        §ormfd = 'S'
117680140227     C                   if        v1ctor <> 'S'
117690140227     C                   clear                   §ormfd
117700140227     C                   else
117710140227     C                   if        v1rsc = *blanks or ds_v1ccrc = *zeros
117720140227     C                   clear                   §ormfd
117730140227     C                   endif
117740140227     C                   endif
117750140227     C                   endif
117760140227      * ORM commissionato
117770140227     c                   Eval      §orcom = v1com
117780140227
117790140227      * in base al tipo orm e a chi paga imposto il campo §orpfb
117800140227      * imposto 'F' se paga mittente per orm singolo o prepagato
117810140227      * imposto 'A' se NON paga mittente
117820140227     c                   Clear                   §orPfb
117830140227     c                   Select
117840140227     c                   When      OrpPag = 'M'
117850140227     c                   If        OrpTor = 'P' or OrpTor = 'S'
117860140227     c                   Eval      §orPfb = 'F'
117870140227     c                   EndIf
117880140227     c                   Other
117890140227     c                   Eval      §orPfb = 'A'
117900140227     c                   EndSl
117910140227
117920140227     C                   eval      ORPflo = dorm01
117930140227
117940140227     c                   Eval      OrpDtt = oggi_aammgg
117950140227      * Mette sempre in positivo i campi delle quantità
117960140227     C                   mllzo     1             ORPorr
117970140227     C                   mllzo     1             ORPncl
117980140227     C                   mllzo     1             ORPpkg
117990140227     C                   mllzo     1             ORPvlm
118000140227     C                   mllzo     1             ORPbnc
118010140227     C                   mllzo     1             ORPblc
118020140227     C                   mllzo     1             ORPatt
118030140227     C                   mllzo     1             ORPmtc
118040161115      /free
118050161115         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
118060161115         //?ipotetico ritiro da effettuare al mattino
118070161115         IF  Orariestesi;
118080161115           ORPrmp = 'M';
118090161115         ENDIF;
118100161116         //?Se ORM da Internet non lo memorizzo
118110161116         IF  ORMtco = 'I';
118120161116           clear ORPrmp;
118130161116         ENDIF;
118140161115      /end-free
118150161115
118160140227     c                   endsr
118170131011
118180131011      *------------------------------------------------------------------------*
118190131011      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
118200131011      *------------------------------------------------------------------------*
118210131011     c     sr_f24        begsr
118220131011
118230131011     c                   Select
118240131011
118250131011     c                   When      $loop = 2 and $rig =2  or
118260131011     c                             $loop = 3 and $rig =3
118270131011     c                   Movea     rigatf1       vzfd01
118280131011     c                   Z-add     1             $rig
118290131011
118300131011     c                   If        $loop = 2
118310131011     c                   Eval      vzfd02= cf2412
118320131011     c                   Else
118330131011     c                   Eval      vzfd02= cf2413
118340131011     c                   EndIf
118350131011
118360131011     c                   When      $loop = 2 and $rig =1 or
118370131011     c                             $loop = 3 and $rig =1
118380131011     c                   Movea     rigatf2       vzfd01
118390131011     c                   Z-add     2             $rig
118400131011
118410131011     c                   If        $loop = 2
118420131011     c                   Eval      vzfd02 = cf2422
118430131011     c                   Else
118440131011     c                   Eval      vzfd02 = cf2423
118450131011     c                   EndIf
118460131011
118470131011     c                   When      $loop = 3 and $rig =2
118480131011     c                   Movea     rigatf3       vzfd01
118490131011     c                   Z-add     3             $rig
118500131011     c                   Eval      vzfd02 = cf2433
118510131011     c                   EndSl
118520131011
118530131011     c                   EndSr
118540131011
118550131011      *------------------------------------------------------------------------*
118560131011      *  Imposto descrizione tasti funzione
118570131011      *------------------------------------------------------------------------*
118580131011     c     Sr_Tastifun   BegSr
118590140130
118600140130      /free
118610140130       //?Routine per abilitazione tasti funzione riga fissa
118620140130         exsr Abi_fxx;
118630140130      /end-free
118640131011
118650131011      * Conta i caratteri riempiti dalle RigaTf1
118660131011     c                   Clear                   $z
118670131011      * Conta i caratteri riempiti dalle RigaTf2
118680131011     c                   Clear                   $k
118690131011      * Conta i caratteri riempiti dalle RigaTf3
118700131011     c                   Clear                   $j
118710131011      * Conta la posizione del campone da cui partire per impostare
118720131011      * la descrizione del tasto funzione
118730131011     c                   Clear                   $y
118740131011
118750131011      * Stringhe che contengono le descrizioni dei tasti funzione
118760131011     c                   Reset                   rigatf1
118770131011     c                   Reset                   rigatf2
118780131011     c                   Reset                   rigatf3
118790131011
118800131011      * F2 - Dati DPD
118810131011     c                   if        *in02 and not *in10 and
118820131011     c                             *in18 and not *in17
118830131011     c                   reset                   $tf
118840131011     c                   movea     tf02          $tf
118850131011     c                   exsr      rie_$tf
118860131011     c                   endif
118870131011      * F11 - Volume
118880131011     c                   if        not *in10 and not *in26
118890131011     c                   reset                   $tf
118900131011     c                   movea     tf11          $tf
118910131011     c                   exsr      rie_$tf
118920131011     c                   endif
118930131011      * F6 - Conferma
118940131011     c                   if        *in01 or *in02 or *in03
118950131011     c                   reset                   $tf
118960131011     c                   movea     tf06          $tf
118970131011     c                   exsr      rie_$tf
118980131011     c                   endif
118990160406      * F13 - Conferma Prenotazione Ritiro
119000160406     c                   if        *in88
119010160406     c                   reset                   $tf
119020160406     c                   movea     tf13          $tf
119030160406     c                   exsr      rie_$tf
119040160406     c                   endif
119050131011      * F14 - Orari Servizio
119060131108     c                   if        not *in10 and
119070131025     c                             not *in17
119080131011     c                   reset                   $tf
119090131011     c                   movea     tf14          $tf
119100131011     c                   exsr      rie_$tf
119110131011     c                   endif
119120131011      * F20 - Simulazione
119130131011     c                   if        *in01
119140131011     c                   reset                   $tf
119150131011     c                   movea     tf20          $tf
119160131011     c                   exsr      rie_$tf
119170131011     c                   endif
119180131011      * F16 - Chiusura
119190140918     c                   if        *in98
119200131011     c                   reset                   $tf
119210131011     c                   movea     tf16          $tf
119220131011     c                   exsr      rie_$tf
119230131011     c                   endif
119240131011      * F19 - Proposte variazione
119250131011     c                   if        *in57 and not *in10
119260131011     c                   reset                   $tf
119270131011     c                   movea     tf19          $tf
119280131011     c                   exsr      rie_$tf
119290131011     c                   endif
119300131011      * F21 - Dirottamento
119310131011     c                   if        *in19 and not *in10 and
119320131011     c                             not *in26
119330131011     c                   reset                   $tf
119340131011     c                   movea     tf21          $tf
119350131011     c                   exsr      rie_$tf
119360131011     c                   endif
119370131011
119380131011      * Pulisco la stringa
119390131011     c                   Eval      $h = 1
119400131011     c                   For       $h by 1 to 62
119410131011     c                   If        rigatf1($h) = '#'
119420131011     c                   Clear                   rigatf1($h)
119430131011     c                   EndIf
119440131011     c                   EndFor
119450131011     c                   Eval      $h = 1
119460131011     c                   For       $h by 1 to 62
119470131011     c                   If        rigatf2($h) = '#'
119480131011     c                   Clear                   rigatf2($h)
119490131011     c                   EndIf
119500131011     c                   EndFor
119510131011     c                   Eval      $h = 1
119520131011     c                   For       $h by 1 to 62
119530131011     c                   If        rigatf3($h) = '#'
119540131011     c                   Clear                   rigatf3($h)
119550131011     c                   EndIf
119560131011     c                   EndFor
119570131011
119580131011      * Imposto la descrizione del tasto funzione F24 e segnalo
119590131011      * quante righe ho riempito e quale devo emettere
119600131011     c                   Select
119610131011
119620131011     c                   When      $k <> *Zeros and $y <> *Zeros
119630131011     c                   Eval      $loop = 3
119640131011     c                   Eval      vzfd02 = cf2413
119650131011
119660131011     c                   When      $k <> *Zeros and $y = *Zeros
119670131011     c                   Eval      $loop = 2
119680131011     c                   Eval      vzfd02 = cf2412
119690131011
119700131011     c                   Other
119710131011
119720131011     c                   Eval      $loop = 1
119730131011     c                   Clear                   vzfd02
119740131011     c                   EndSl
119750131011
119760131011      * Prima riga di tasti funzione
119770131011     c                   Movea     rigatf1       vzfd01
119780131011     c                   Eval      $rig = 1
119790131212
119800131212      /free
119810131212        *in80 = *off;
119820131212        *in83 = *off;
119830131212       //?Imposto tasto funzione in HI se presente dati
119840131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
119850131216             V1mtc > 0 or V1sto > 0 or V1ffd <> *blanks;
119860131212           *in80 = *on;
119870131212         ENDIF;
119880160513         IF  ((*in01 or *in03) and
119890160513              (w03sms <> *blanks or W03mail <> *blanks)) or
119900160513               walert;
119910131212           *in83 = *on;
119920131212         ENDIF;
119930131212      /end-free
119940131011
119950131011     c                   EndSr
119960140130
119970140130      /free
119980140130       //--------------------------------------------------------------------
119990140130       //?Abilitazione tasti funzione riga fissa.
120000140130       //--------------------------------------------------------------------
120010140130       BEGSR Abi_Fxx;
120020140130
120030140130         *in97 = *off;
120040140130       //?Abilito il tasto F17-Note AUT solo se sono in manutenzione
120050140227       //?e se NON è conferma proposta di variazione
120060140317           IF  *in02 and §RMfpr <> 'P';
120070140204             *in97 = *on;
120080140204           ENDIF;
120090140918
120100140918         *in98 = *off;
120110140918       //?Attivo F16 Chiusura ORM se
120120140918       //?non è copia/immissione ORM
120130140918       //?non è già chiusura ORM
120140140918       //?non siamo in conferma proposta di variazione
120150140918       //?se ORM non è in fase 420
120160140918         IF  not *in10 and not *in03 and not *in01 and not *in81 and
120170140918             not OrmFase420;
120180140918           *in98 = *on;
120190140918         ENDIF;
120200140130
120210140130       ENDSR;
120220140130      /end-free
120230131011
120240131011      *------------------------------------------------------------------------*
120250131011      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
120260131011      *------------------------------------------------------------------------*
120270131011     c     Rie_$tf       BegSr
120280131011      *
120290131011     c                   Eval      $x = 1
120300131011     c     '#'           Lookup    $tf($x)                                30
120310131011     c                   Add       $x            $z
120320131011     c                   If        $Z <= 63
120330131011     c                   Eval      $j = $Z - $x + 1
120340131011     c                   Movea     $tf           rigatf1($j)
120350131011     c                   Else
120360131011     c                   Add       $x            $k
120370131011     c                   If        $k <= 63
120380131011     c                   Eval      $j = $K - $x + 1
120390131011     c                   Movea     $tf           rigatf2($j)
120400131011     c                   Else
120410131011     c                   Add       $x            $y
120420131011     c                   If        $y <= 63
120430131011     c                   Eval      $j = $y - $x + 1
120440131011     c                   Movea     $tf           rigatf3($j)
120450131011     c                   EndIf
120460131011     c                   EndIf
120470131011     c                   EndIf
120480131011
120490131011     c                   EndSr
120500140422
120510140422      /free
120520140422       //--------------------------------------------------------------------
120530140422       //?Controllo se Alert o meno.
120540140422       //--------------------------------------------------------------------
120550140422       BEGSR AlertORM;
120560140422
120570140424         walert = *off;
120580140422         clear §OREfimo;
120590140422         clear §OREfiso;
120600140515
120610140515         exsr VerificaAlert;
120620140422
120630140515       //?Se OK alert, sono in conferma da VAO e
120640140515       //?ci sono i dati dell'alert li controllo
120650140515         IF  wOKalert and §RMfpr = 'C' and
120660140515            (w03mail <> *blanks or w03sms <> *blanks);
120670140424           exsr CtrDatiF04;
120680140424           IF  *in28;
120690140424             leavesr;
120700140424           ENDIF;
120710140424         ENDIF;
120720140422
120730140422       //?Imposto il flag ORM alert via SMS
120740140515         IF  w03sms <> *blanks;
120750140515           IF  not wOKalert;
120760140515             §OREfiso = 'N';
120770140515           ELSE;
120780140515             SELECT;
120790140422         //?Imposto in base al flag VAO alert via SMS
120800140422         //?per 'N' no alert
120810140423             WHEN  §OREfisv = 'N';
120820140423               §OREfiso = 'N';
120830140422         //?per 'X' alert solo se data ritiro > di oggi
120840140423             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
120850140423               §OREfiso = 'S';
120860140423             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
120870140423               §OREfiso = 'N';
120880140422         //?no 'N' e no 'X' alert calcolato
120890140423             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
120900140515               IF  inv_V1dar > oggi_aammgg;
120910140515                 §OREfiso = 'S';
120920140515               ELSE;
120930140515                 §OREfiso = 'N';
120940140515               ENDIF;
120950140515             ENDSL;
120960140515           ENDIF;
120970140422         ENDIF;
120980140422
120990140422       //?Imposto il flag ORM alert via mail
121000140515         IF  w03mail<> *blanks;
121010140515           IF  not wOKalert;
121020140515             §OREfimo = 'N';
121030140515           ELSE;
121040140515             SELECT;
121050140422         //?Imposto in base al flag VAO alert via mail
121060140422         //?per 'N' no alert
121070140507             WHEN  §OREfimv = 'N';
121080140423               §OREfimo = 'N';
121090140422         //?per 'X' alert solo se data ritiro > di oggi
121100140423             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
121110140423               §OREfimo = 'S';
121120140423             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
121130140423               §OREfimo = 'N';
121140140422         //?no 'N' e no 'X' alert calcolato
121150140423             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
121160140515               IF  inv_V1dar > oggi_aammgg;
121170140515                 §OREfimo = 'S';
121180140515               ELSE;
121190140515                 §OREfimo = 'N';
121200140515               ENDIF;
121210140515             ENDSL;
121220140515           ENDIF;
121230140422         ENDIF;
121240140422
121250140422       //?imposto flag per OK alert da mandare
121260140422         IF  §OREfiso = 'S' or §OREfimo = 'S';
121270140422           walert = *on;
121280140422         ENDIF;
121290140422
121300140422       ENDSR;
121310140515
121320140515       //--------------------------------------------------------------------
121330140515       //?Verifico se OK alert.
121340140515       //--------------------------------------------------------------------
121350140515       BEGSR VerificaAlert;
121360140515
121370140515         wOKalert = *off;
121380140515
121390140515         SELECT;
121400140515
121410140515       //?Se mittente codificato e filiale mittente ha ntw estero
121420140515       //?o mittente NON codificato con nazione estera
121430140515       //?NO alert e NON memorizzo i dati su FNORE
121440140515         WHEN  (ds_V1Ccra > 0 and
121450140515               (m_§OGntw = 'DPD' or m_§OGntw = 'EEX' or m_§OGntw = 'FED')) or
121460140515               (ds_V1Ccra = 0 and V1nar <> *blanks);
121470140515           clear w03sms;
121480140515           clear w03mail;
121490140515
121500140515         //?Se ORM NON commissionato NO alert
121510140515         WHEN  V1com <> 'S';
121520140515         //?Se ORM prepagato NO alert
121530140515         WHEN  V1Ctor = 'P';
121540140515         //?Se Ordinante non codificato NO alert
121550140515         WHEN  ds_V1Ccor = *zeros or
121560140515               ds_V1cor2 = 9999 or ds_V1cor2 = 8888;
121570140515         //?Controllo se peso/volume/bancali superano i limit massimi da DFT
121580140515         //?NO alert
121590140515         WHEN  (d§DFTpkga > 0 and V1pkg > d§DFTpkga) or
121600140515               (d§DFTvlma > 0 and V1vlm > d§DFTvlma) or
121610140515               (d§DFTbnca > 0 and V1bnc > d§DFTbnca);
121620140515         OTHER;
121630140515           wOKalert = *on;
121640140515         ENDSL;
121650140515
121660140515       ENDSR;
121670140505
121680140505       //--------------------------------------------------------------------
121690140505       //?Calcolo data ritiro Alert.
121700140505       //--------------------------------------------------------------------
121710140505       BEGSR DataRitiroAlert;
121720140515
121730140515       //?data ritiro video > oggi non faccio niente
121740140515         dataEUR = %date(V1dar:*eur);
121750140515         dataISO = dataeur;
121760140515         wdar = %dec(dataISO);
121770140515         IF  wdar > oggi_aammgg;
121780140515           leavesr;
121790140515         ENDIF;
121800140515
121810140515       //?se non ci sono i presupposti per inviare alert non faccio niente
121820140515         IF  not wOKalert;
121830140515           leavesr;
121840140515         ENDIF;
121850140515
121860140515       //?se i presupposti ci sono allora devo ricalcolare la data ritiro
121870140515       //?ma solo se i flag da VAO non sono 'X' o 'N'
121880140710         IF  (§OREfisv <> 'X' and §OREfisv <> 'N' and w03sms <> *blanks) or
121890140710             (§OREfimv <> 'X' and §OREfimv <> 'N' and w03mail <> *blanks);
121900140519           IF  V1Cpor > *zeros and num_V1Cpor = *zeros;
121910140519             num_v1Cpor = %dec(V1Cpor:3:0);
121920140519           ENDIF;
121930140527         //?se conferma ORM da VAO e NON ho ancora il POR lo prendo da O95lna
121940140529         //?se impostato
121950140527         //?capita perchè c'è un errore nel controllo del cap del mittente
121960140529           IF  §RMfpr = 'C' and num_V1Cpor = 0 and sav_O95lna > 0;
121970140527             num_V1Cpor = sav_O95lna;
121980140527           ENDIF;
121990140529         //?Calcolo la data solo se ho già la filiale ritiro
122000140529           IF  num_V1Cpor > 0;
122010140515           exsr sr_calpikup;
122020140515           v1dar = new_v1dar;
122030140515           dataEUR = %date(V1dar:*eur);
122040140515           dataISO = dataeur;
122050140515           inv_v1dar = %dec(dataISO);
122060140529           ENDIF;
122070140515         ENDIF;
122080140505
122090140505       ENDSR;
122100160406
122110160406       //--------------------------------------------------------------------
122120160406       //?Controllo se Conferma Prenotazione o meno.
122130160406       //--------------------------------------------------------------------
122140160406       BEGSR ConfermaORM;
122150160406
122160160406         clear §OREfmco;
122170160406         clear §OREfsco;
122180160406
122190160406       //?Se sono in conferma da VAO ed è stata richiesta la conferma
122200160406       //?controllo i dati
122210160406         IF  wConferma and §RMfpr = 'C' and
122220160406            (w04mail <> *blanks or w04sms <> *blanks);
122230160406           exsr CtrDatiF04;
122240160406           IF  *in28;
122250160406             leavesr;
122260160406           ENDIF;
122270160406         ENDIF;
122280160406
122290160406       //?Imposto il flag ORM alert via SMS
122300160406         IF  w03sms <> *blanks;
122310160406           IF  not wOKalert;
122320160406             §OREfiso = 'N';
122330160406           ELSE;
122340160406             SELECT;
122350160406         //?Imposto in base al flag VAO alert via SMS
122360160406         //?per 'N' no alert
122370160406             WHEN  §OREfisv = 'N';
122380160406               §OREfiso = 'N';
122390160406         //?per 'X' alert solo se data ritiro > di oggi
122400160406             WHEN  §OREfisv = 'X' and inv_v1dar > oggi_aammgg;
122410160406               §OREfiso = 'S';
122420160406             WHEN  §OREfisv = 'X' and inv_v1dar = oggi_aammgg;
122430160406               §OREfiso = 'N';
122440160406         //?no 'N' e no 'X' alert calcolato
122450160406             WHEN  §OREfisv <> 'N' and §OREfisv <> 'X';
122460160406               IF  inv_V1dar > oggi_aammgg;
122470160406                 §OREfiso = 'S';
122480160406               ELSE;
122490160406                 §OREfiso = 'N';
122500160406               ENDIF;
122510160406             ENDSL;
122520160406           ENDIF;
122530160406         ENDIF;
122540160406
122550160406       //?Imposto il flag ORM alert via mail
122560160406         IF  w03mail<> *blanks;
122570160406           IF  not wOKalert;
122580160406             §OREfimo = 'N';
122590160406           ELSE;
122600160406             SELECT;
122610160406         //?Imposto in base al flag VAO alert via mail
122620160406         //?per 'N' no alert
122630160406             WHEN  §OREfimv = 'N';
122640160406               §OREfimo = 'N';
122650160406         //?per 'X' alert solo se data ritiro > di oggi
122660160406             WHEN  §OREfimv = 'X' and inv_v1dar > oggi_aammgg;
122670160406               §OREfimo = 'S';
122680160406             WHEN  §OREfimv = 'X' and inv_v1dar = oggi_aammgg;
122690160406               §OREfimo = 'N';
122700160406         //?no 'N' e no 'X' alert calcolato
122710160406             WHEN  §OREfimv <> 'N' and §OREfimv <> 'X';
122720160406               IF  inv_V1dar > oggi_aammgg;
122730160406                 §OREfimo = 'S';
122740160406               ELSE;
122750160406                 §OREfimo = 'N';
122760160406               ENDIF;
122770160406             ENDSL;
122780160406           ENDIF;
122790160406         ENDIF;
122800160406
122810160406       //?imposto flag per OK alert da mandare
122820160406         IF  §OREfiso = 'S' or §OREfimo = 'S';
122830160406           walert = *on;
122840160406         ENDIF;
122850160406
122860160406       ENDSR;
122870140422
122880140422       //--------------------------------------------------------------------
122890140422       //?Scrivo file spia Alert.
122900140422       //--------------------------------------------------------------------
122910140422       BEGSR wrtspia;
122920140422
122930151008         IF  walert;
122940140422         clear FISAO000;
122950140422         SAOdaori = %timestamp();
122960140422         SAOpoe = ORMpoe;
122970140422         SAOnsr = ORMnsr;
122980140422         SAOnor = ORMnor;
122990140422         SAOnrv = ORMnrv;
123000140422         SAOtipa = 'AVV';
123010140422         SAOsts = '0';
123020140422         write FISAO000;
123030151008         ENDIF;
123040151008
123050151008         IF  wconferma;
123060151021           clear FISAO000;
123070151021           SAOdaori = %timestamp();
123080151021           SAOpoe = ORMpoe;
123090151021           SAOnsr = ORMnsr;
123100151021           SAOnor = ORMnor;
123110151021           SAOnrv = ORMnrv;
123120151021           SAOtipa = 'CON';
123130151021           SAOsts = '0';
123140151021           write FISAO000;
123150151008         ENDIF;
123160140422
123170140422       ENDSR;
123180140506
123190140506       //--------------------------------------------------------------------
123200140506       //?Imposto la frase per NON TELEFONARE
123210140506       //?se ORM commissionato con Alert
123220140506       //--------------------------------------------------------------------
123230140506       BEGSR CommissionatoAlert;
123240140506
123250140506         clear wtel01;
123260140506         clear wpos;
123270140506         wpos = (%len(wtel01) - %len(cCommAlert)) / 2;
123280140506         %subst(wtel01:wpos:%len(cCommAlert)) = cCommAlert;
123290140506
123300140506         clear wtel02;
123310140506         clear wpos;
123320140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123330140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123340140506
123350140506       ENDSR;
123360140506
123370140506       //--------------------------------------------------------------------
123380140506       //?Imposto la frase per NON TELEFONARE
123390140506       //?se ORM NON commissionato dirottato
123400140506       //--------------------------------------------------------------------
123410140506       BEGSR DirottaNoComm;
123420140506
123430140506         clear wtel01;
123440140506         clear wpos;
123450140506         wpos = (%len(wtel01) - %len(cDirottaNoComm)) / 2;
123460140506         %subst(wtel01:wpos:%len(cDirottaNoComm)) = cDirottaNoComm;
123470140506
123480140506         clear wtel02;
123490140506         clear wpos;
123500140506         wpos = (%len(wtel02) - %len(cNoTelefona)) / 2;
123510140506         %subst(wtel02:wpos:%len(cNoTelefona)) = cNoTelefona;
123520140506
123530140506       ENDSR;
123540140506
123550140506       //--------------------------------------------------------------------
123560140506       //?Imposto la frase per NON TELEFONARE
123570140506       //?se ORM Con preavviso via mail (CEVA) dirottato
123580140506       //--------------------------------------------------------------------
123590140506       BEGSR DirottaPreavviso;
123600140506
123610140506         clear wtel01;
123620140506         clear wpos;
123630140506         wpos = (%len(wtel01) - %len(cDirottaPreavv)) / 2;
123640140506         %subst(wtel01:wpos:%len(cDirottaPreavv)) = cDirottaPreavv;
123650140506
123660140506         clear wtel02;
123670140506         clear wpos;
123680140506         wpos = (%len(wtel02) - %len(cGiaAvvisato)) / 2;
123690140506         %subst(wtel02:wpos:%len(cGiaAvvisato)) = cGiaAvvisato;
123700140506
123710140506       ENDSR;
123720151106
123730160406       //--------------------------------------------------------------------
123740160406       //?Recupero i dati per Conferma Prenotazione da anagrafica clienti.
123750160406       //--------------------------------------------------------------------
123760160406       BEGSR RecDatiConf;
123770160406
123780160406         clear wCodAcre;
123790160406       //?dall'ordinante se presente
123800160406         IF  ds_V1Ccor > 0;
123810160406           wCodAcre = ds_V1Ccor;
123820160406         ELSE;
123830160406         //?dal mittente se ordinante non presente
123840160406           IF  V1rso = *blanks and ds_V1Ccra > 0;
123850160406             wCodAcre = ds_V1Ccra;
123860160406           ENDIF;
123870160406         ENDIF;
123880160406       //?Ho recuperato un codice quindi aggancio l'angrafica
123890160406          IF  wCodAcre = 0;
123900160406            leavesr;
123910160406          ENDIF;
123920160406          clear dACR01;
123930160406          chain(n) (wCodAcre) FNACR01L;
123940160406          IF  %found(FNACR01L);
123950160406            dACR01 = ACRmai;
123960160406          ENDIF;
123970160406         //?Se il cliente ha il flag di invio conferma
123980160406         //?imposto i dati a video
123990160406          IF  §ACRfmar <> 'S';
124000160406            leavesr;
124010160406          ENDIF;
124020160406          IF  w04mail = *blanks;
124030160406            chain(n) (wCodAcre:'MC') FNACRE1L;
124040160406            IF  %found(FNACRE1L);
124050160406              w04mail = ACREdati;
124060160406            ENDIF;
124070160406          ENDIF;
124080160406          IF  w04sms = *blanks;
124090160406            chain(n) (wCodAcre:'SC') FNACRE1L;
124100160406            IF  %found(FNACRE1L);
124110160406              w04sms = ACREdati;
124120160406            ENDIF;
124130160406          ENDIF;
124140160406
124150160406       ENDSR;
124160160406
124170151106       //--------------------------------------------------------------------
124180151106       //?Aggiorno Anagrafica clienti ritiro Ordinante x mail conferma.
124190151106       //--------------------------------------------------------------------
124200151106       BEGSR aggACR;
124210160505
124220160505       //?Se ORM con Ordinante YOOX non aggiorno
124230160505         IF  ORMcor = 17732000;
124240160505           leavesr;
124250160505         ENDIF;
124260151106
124270151109         chain(e) ORMcor FNACR01L;
124280161118         IF  not  %error;
124290151106         IF  %found(FNACR01L);
124300151109           dACR01 = ACRmai;
124310160322           IF  §OREfmco = 'S' or §OREfsco = 'S';
124320160322             §ACRfmar = 'S';
124330160322           ELSE;
124340151109             clear §ACRfmar;
124350160322           ENDIF;
124360151109           ACRmai = dACR01;
124370151109           update FNACR000;
124380151106         ENDIF;
124390161118         ENDIF;
124400151109
124410151109         chain(e) (ORMcor:'MC') FNACRE1L;
124420161118         IF  not %error;
124430151109         IF  %found(FNACRE1L);
124440160322           IF  sav_mailconf <> *blanks and §ACRfmar = 'S';
124450151109             ACREdati = sav_mailconf;
124460151109             update FNACRE00;
124470151109           ELSE;
124480151109             delete FNACRE00;
124490151109           ENDIF;
124500151109         ENDIF;
124510160112         IF  not %found(FNACRE1L) and sav_mailconf <> *blanks and
124520160322             §ACRfmar = 'S';
124530151109           clear FNACRE00;
124540151109           ACREcro = ORMcor;
124550151109           ACREtrc = 'MC';
124560151109           ACREdati = sav_mailconf;
124570151109           write FNACRE00;
124580151109         ENDIF;
124590161118         ENDIF;
124600160318
124610160318         chain(e) (ORMcor:'SC') FNACRE1L;
124620161118         IF  not %error;
124630160318         IF  %found(FNACRE1L);
124640160322           IF  sav_smsconf <> *blanks and §ACRfmar = 'S';
124650160318             ACREdati = sav_smsconf;
124660160318             update FNACRE00;
124670160318           ELSE;
124680160318             delete FNACRE00;
124690160318           ENDIF;
124700160318         ENDIF;
124710160318         IF  not %found(FNACRE1L) and sav_smsconf <> *blanks and
124720160322             §ACRfmar = 'S';
124730160318           clear FNACRE00;
124740160318           ACREcro = ORMcor;
124750160318           ACREtrc = 'SC';
124760160318           ACREdati = sav_smsconf;
124770160318           write FNACRE00;
124780160318         ENDIF;
124790161118         ENDIF;
124800160712
124810161118       //?Cerco il codice utente da memorizzare su FNACRE
124820160712         clear ORIidc;
124830160712         IF  §ORMpg > *zeros;
124840160712           ORIprg = %dec(§ORMpg:7:0);
124850160712           chain (ORIprg) TIORI01L;
124860160712         ENDIF;
124870161118
124880161118       //?Memorizzo chi ha fatto la richiesta
124890161118         clear dOREana;
124900161118         §OREtipo = 'I';
124910161118         §OREjob = §OREorip;
124920161118         §OREute = ORIidc;
124930161118         §OREdtv = %dec(%date());
124940161118         §OREorv = %dec(%time());
124950161118         chain(e) (ORMcor:'AA') FNACRE1L;
124960161118         IF  not %error;
124970161118           IF  %found(FNACRE1L);
124980161118             ACREdati = dOREana;
124990161118             update FNACRE00;
125000161118           ENDIF;
125010161118           IF  not %found(FNACRE1L);
125020161118             clear FNACRE00;
125030161118             ACREcro = ORMcor;
125040161118             ACREtrc = 'AA';
125050161118             ACREdati = dOREana;
125060161118             write FNACRE00;
125070161118           ENDIF;
125080161118         ENDIF;
125090151106
125100151106       ENDSR;
125110161115
125120161115       //--------------------------------------------------------------------
125130161115       //?Imposto i dati necessari alla visualizzazione orari servizio.
125140161115       //--------------------------------------------------------------------
125150161115       BEGSR DatiPerOrari;
125160161115
125170161115       //?Imposto la data per calcolo data ritiro = oggi
125180161115         DataCalcolo = Oggi_aammgg;
125190161115       //?Se data pronta merce > oggi la data calcolo è la data pronta merce
125200161115         IF  inv_V1dpm > 0 and inv_V1dpm > Oggi_aammgg;
125210161115           DataCalcolo = inv_V1dpm;
125220161115         ENDIF;
125230161115       //?Cerco giorno della settimana della data calcolo data ritiro
125240161115         dataiso = %date(DataCalcolo);
125250161115         clear wDay_Of_Week;
125260161115         exec sql
125270161115         set :wDay_Of_Week = dayofweek_iso(:dataiso);
125280161115
125290161115       //?Aggancio la tabella GPD con filiale ritiro ORM
125300161115         clear TIBS02ds;
125310161115         clear dGPD;
125320161115         T02mod = 'C';
125330161115         T02cod = 'GPD';
125340161115         T02sif = KNSIF;
125350161115         T02ke1 = %editc(num_V1cpor:'X');
125360161115         TNTBE_RicercaControllo (kpjba : tibs02ds);
125370161115         IF  T02err = *blanks;
125380161115           dGPD = T02uni;
125390161115         ELSE;
125400161115         //?Non trovo GPD con filiale ritiro
125410161115         //?la cerco con filiale generica 999
125420161115           clear TIBS02ds;
125430161115           T02mod = 'C';
125440161115           T02cod = 'GPD';
125450161115           T02sif = KNSIF;
125460161115           T02ke1 = '999';
125470161115           TNTBE_RicercaControllo (kpjba : tibs02ds);
125480161115           IF  T02err = *blanks;
125490161115             dGPD = T02uni;
125500161115           ENDIF;
125510161115         ENDIF;
125520161115
125530161115       ENDSR;
125540140422
125550140422      /end-free
125560131011
125570001010      *****************************************************************
125580001010      * ROUTINE INIZIALE
125590001010      *****************************************************************
125600001010     C     *INZSR        BEGSR
125610001009
125620001010     C     *ENTRY        PLIST
125630001010     C                   PARM                    KPJBA
125640001023     C                   PARM                    FIOR05DS
125650001011
125660001011     C                   movel     TIT_A         VTCtit
125670001009
125680150306     c                   IF        kpjbu <> *blanks
125690001026     C                   movel(p)  kpjbu         parm01
125700150306     c                   ELSE
125710150306     c                   clear                   parm01
125720150306     c                   ENDIF
125730001016
125740060203     C                   Z-ADD     1             CODUT             1 0
125750060203
125760060203      * Reperisco dati job
125770150713     c                   IF        §RMfpr <> 'K'
125780060203     c                   exsr      DatiJob
125790150713     c                   open      FIOR05D
125800150713     c                   ENDIF
125810150713
125820150713      /free
125830160713
125840160713       //?Determino la libreria
125850160713         IF  %subst(knsif : 7 : 1) = 'P';
125860160713           wLib = 'GAITRAGRPS';
125870160713         ELSE;
125880160713           wLib = 'GAITRAGRU';
125890160713         ENDIF;
125900160713       //?Apro i file
125910160713         wFLib = %trim(wLib) + '/TIORI01L';
125920160713         open  TIORI01L;
125930160713
125940150713       //?Se richiamato da pgm in ascolto (conferma automatica ORM da file/internet)
125950150713         IF  §RMfpr = 'K';
125960150713       //?imposto DUTpou = filiale emissione
125970150713           DUTpou = §RMpoe;
125980150713       //?carico la SKpog con la filiale emissione
125990150713           SKpog(1) = %editc(§RMpoe:'X');
126000150713       //?il flag di tipo richiamo diventa 'C'
126010150713           §RMfpr = 'C';
126020150713       //?Conferm automatica
126030150713           ConfAuto = *on;
126040150713       //?Utente batch
126050150713           knmus = 'BATCH';
126060150713         ENDIF;
126070150713      /end-free
126080001018
126090041014      * salvo il n. di distinta quando è proposta di chiusura
126100041014     c                   If        §rmtla = 'C' and §rmfpr = 'P' and
126110041014     c                             pndc > *Zeros
126120041014     c                   Eval      savndc = pndc
126130041014     c                   Eval      savddc = pddc
126140060516     c                   Eval      savfgs = pfvvfgs
126150041014     c                   EndIf
126160150304      * imposto il n. ORM quando è proposta variazione
126170001106     C                   if        §RMfpr = 'P'
126180001106     C                   eval      ppoe = §RMpoe
126190001106     C                   eval      pnsr = §RMnsr
126200001106     C                   eval      pnor = §RMnor
126210001106     C                   eval      pnrv = §RMnrv
126220001106     C                   endif
126230110420
126240110420      /free
126250110420       //?Imposto flag per arrivo da conferma proposta di variazione
126260110420       *in81 = *off;
126270110420       IF  §rmfpr = 'P';
126280110420         *in81 = *on;
126290110420       ENDIF;
126300110420      /end-free
126310001023
126320150304     C                   movel     DUTpou        kazorg
126330150310     C                   clear                   network
126340150612     C                   clear                   og143
126350001019     C     kazorg        chain     azorg01L
126360001016     C                   if        %found(azorg01l)
126370150612     C                   movel     orgde3        OG143
126380150612     c                   eval      network = §OGntw
126390001016     C                   endif
126400150609      * Carico £6
126410150609     C                   CLEAR                   trul06ds
126420150609     C                   MOVE      '£6'          D06COD
126430150609     C                   MOVEL     DUTpou        D06KEY
126440150609     C                   MOVEL     trul06ds      KPJBU
126450150609     C                   CALL      'TRUL06R'
126460150609     C                   PARM                    KPJBA
126470150609     C                   MOVEL     KPJBU         trul06ds
126480150609     C                   MOVEA     LIN           L6
126490061023
126500150305      * carico sk dei p.o. DPD
126510061023     c                   clear                   xx
126520061023     c     *loval        setll     azorg01l
126530061023     c                   do        *hival
126540061023     c                   read      azorg01l
126550061023     c                   if        %eof(azorg01l)
126560061023     c                   leave
126570061023     c                   endif
126580061023     c                   if        orgfva <> *blanks  or
126590061023     c                             (orgfag <> 'F' and orgfag <> 'A')
126600061023     c                   iter
126610061023     c                   endif
126620061023     c                   eval      og143 = orgde3
126630061023     c                   if        §ogntw = 'DPD'
126640061023     c                   add       1             xx
126650061023     c                   eval      skpodpd(xx) = orgfil
126660061023     c                   endif
126670061023     c                   enddo
126680081126
126690081126      * tabella 3I x DPD
126700081126     c                   clear                   ds3idp
126710081126     c                   eval      kcod = '3I'
126720081126     c                   eval      kkey = 'DPD'
126730081126     c     ktab          chain     tabel00f
126740081126     c                   if        %found(tabel00f)
126750081126     c                             and tblflg = *blanks
126760081126     c                   eval      ds3idp = tbluni
126770081126     c                   endif
126780001009
126790001023      * reperisce data e ora
126800001010     C                   TIME                    W0140
126810001010      * UDATE IN GGMMAAAA
126820010116     C                   MOVE      W0140         oggi_ggmmaa
126830001010      * UDATE IN AAAAMMGG
126840010124     C                   Z-ADD     oggi_ggmmaa   G02DAT
126850001010     C                   MOVEL     *BLANK        G02ERR
126860001010     C                   CALL      'XSRDA8'
126870001215     C                   PARM                    WLBDAT
126880010116     C                   Z-ADD     G02INV        oggi_aammgg
126890010907
126900010907     C                   movel     oggi_aammgg   oggi_anno
126910010907     C                   add       1             oggi_anno
126920001016
126930001009     C     Kfnor         klist
126940001011     C                   kfld                    kpoe
126950001016     C                   kfld                    knsr
126960001009     C                   kfld                    knor
126970001016     C                   kfld                    knrv
126980001109
126990001109     C     Kfnor1        klist
127000001109     C                   kfld                    kpoe
127010001109     C                   kfld                    knsr
127020001109     C                   kfld                    knor
127030001018
127040001026     C     Kfnorn        klist
127050001018     C                   kfld                    kpoe
127060001018     C                   kfld                    knsr
127070001018     C                   kfld                    knor
127080001018     C                   kfld                    knrv
127090001026     C                   kfld                    kdai
127100001026     C                   kfld                    kori
127110001018     C                   kfld                    kfar
127120001018     C                   kfld                    kprg
127130001026
127140001026     C     Kfnorn1       klist
127150001026     C                   kfld                    kpoe
127160001026     C                   kfld                    knsr
127170001026     C                   kfld                    knor
127180001026     C                   kfld                    knrv
127190001026     C                   kfld                    kdai
127200001026     C                   kfld                    kori
127210001026     C                   kfld                    kfar
127220070806
127230070806     c     kors          klist
127240070806     c                   kfld                    v1cpos
127250070806     c                   kfld                    v1ors
127260001009
127270010118     C     Ktab          klist
127280010119     C                   kfld                    codut
127290010118     C                   kfld                    kcod
127300010118     C                   kfld                    kkey
127310001010
127320001010     C     Ktntam        klist
127330001010     C                   kfld                    kksc
127340001010     C                   kfld                    kctr
127350010122
127360010122     C     Ktnta1        klist
127370010122     C                   kfld                    kksc
127380010122     C                   kfld                    kctr
127390010122     C                   kfld                    kprg
127400001215
127410001215     C     kazcln        klist
127420001215     C                   kfld                    ktfp
127430001215     C                   kfld                    ktfa
127440001215     C                   kfld                    kann
127450001215     C                   kfld                    kmes
127460001009
127470040426     c     kOrm08        klist
127480040426     c                   kfld                    ds_v1ccra
127490040426     c                   kfld                    inv_v1dar
127500040930
127510040930     c     kCnaco        klist
127520040930     c                   kfld                    codut
127530040930     c                   kfld                    kci
127540040930     c                   kfld                    kksc
127550070123
127560070301     c     kfidst        klist
127570070301     c                   kfld                    kdstnpg
127580080226     c                   kfld                    kdstnfv
127590080226     c                   kfld                    kdstfgs
127600071023
127610071023     c     kacr1         klist
127620071023     c                   kfld                    ds_v1ccra
127630071023     c                   kfld                    num_v1cpor
127640071031     c                   kfld                    kacr1ain
127650090206
127660090206     c     kfnora        klist
127670090206     c                   kfld                    ormpoe
127680090206     c                   kfld                    ormnsr
127690090206     c                   kfld                    ormnor
127700090206     c                   kfld                    ormnrv
127710090206     c                   kfld                    ormfao
127720090206     c                   kfld                    ormdfo
127730090206     c                   kfld                    ormofo
127740150615
127750150615     c                   eval      *in04 = *off
127760040426
127770001009     C                   select
127780010124      * Immissione
127790141212     C                   when      psce = '1' or psce = 'E'
127800001009     C                   movel     mod(1)        v1mod
127810001009     C                   eval      *in01 = *on
127820150615     c                   IF        §RMtla = *blanks
127830150615     c                   eval      *in04 = *on
127840150615     c                   ENDIF
127850010124      * Manutenzione
127860001009     C                   when      psce = '2'
127870001009     C                   movel     mod(2)        v1mod
127880001009     C                   eval      *in02 = *on
127890010124      * Copia
127900001109     C                   when      psce = '3'
127910001109     C                   movel     mod(1)        v1mod
127920010124     C                   eval      *in03 = *on
127930010124      * Conferma
127940010124     C                   when      psce = 'C'
127950090211     C                   movel     mod(7)        v1mod
127960010124     C                   eval      *in02 = *on
127970010124      * Richiamato x immissione
127980001025     C                   when      §RMtla <> *blanks
127990010124     C                             and §RMfpr <> 'P' and §RMfpr <> 'O'
128000001023     C                   movel     mod(1)        v1mod
128010001025     C                   eval      *in01 = *on
128020010124      * Richiamato x manutenzione
128030001106     C                   when      §RMtla <> *blanks
128040001106     C                             and §RMfpr = 'P'
128050001106     C                   movel     mod(2)        v1mod
128060001106     C                   eval      *in02 = *on
128070001009     C                   endsl
128080001009
128090001011     C                   eval      kpoe = ppoe
128100001016     C                   eval      knsr = pnsr
128110001011     C                   eval      knor = pnor
128120001016     C                   eval      knrv = pnrv
128130001011
128140001025     C                   eval      v1cpoe = ppoe
128150001016     C                   eval      v1nsr  = pnsr
128160001025     C                   eval      v1nor  = pnor
128170001025     C                   eval      v1nrv  = pnrv
128180150305
128190150305      * Recupero i dati di default prima con la filiale emissione
128200150305     C                   clear                   ddft
128210150305     C                   clear                   TIBS02DS
128220150305     C                   movel     'C'           T02mod
128230150305     C                   movel     knsif         t02sif
128240150305     C                   movel     'DFT'         t02cod
128250150305     C                   movel(p)  v1cpoe        T02ke1
128260150305     C                   call      'TIBS02R'
128270150305     C                   parm                    KPJBA
128280150305     C                   parm                    TIBS02DS
128290150305     C                   if        t02err =  *blanks
128300150305     C                   movel     t02uni        ddft
128310150305     C                   else
128320150305      * se non tropo provo con 999 generico
128330150305     C                   clear                   TIBS02DS
128340150305     C                   movel     'C'           T02mod
128350150305     C                   movel     knsif         t02sif
128360150305     C                   movel     'DFT'         t02cod
128370150305     C                   movel(p)  '999'         T02ke1
128380150305     C                   call      'TIBS02R'
128390150305     C                   parm                    KPJBA
128400150305     C                   parm                    TIBS02DS
128410150305     C                   if        t02err =  *blanks
128420150305     C                   movel     t02uni        ddft
128430150305     C                   endif
128440150305     C                   endif
128450150305
128460150305      * se i dati relativi al peso e volume sono a zero li imposto a di dft io
128470150305      * caso che non dovrebbe capitare ma se capita nella routine 'pesvol' il
128480150305      * programma si spacca
128490150305     c                   if        d§dftpkg = *zeros
128500150305     c                   eval      d§dftpkg = 200
128510150305     c                   endif
128520150305     c                   if        d§dftbnc = *zeros
128530150305     c                   eval      d§dftbnc = 1
128540150305     c                   endif
128550140318
128560140318      /free
128570140422       //?Carico i Tipi rcd FNORE che NON sono da copiare pari pari da VAOE
128580140422         clear dORE;
128590140422         clear xx;
128600140422         kTBEcod = 'ORE';
128610140422         setll kTBEcod tntbe01l;
128620140422         reade kTBEcod tntbe01l;
128630140422         DOW not %eof(tntbe01l);
128640140422           dORE = TBEUni;
128650140422           IF  §OREvar = 'S';
128660140422             xx += 1;
128670140423             skTRCvao(xx) = %subst(TBEke1:1:2);
128680140422           ENDIF;
128690140422           reade kTBEcod tntbe01l;
128700140422         ENDDO;
128710131022      /end-free
128720001006
128730001006     C                   endsr
128740060203
128750060203      *---------------------------------------------------------------*
128760060203      * Reperimento Dati del job (Utente/Operativi)                   *
128770060203      *---------------------------------------------------------------*
128780060203     c     DatiJob       BEGSR
128790060203      *
128800060203     c     *dtaara       define    §azute        azuteds
128810060203     c     *dtaara       define    §datiute      ddatiute
128820060203      *
128830060203     c                   in(E)     *dtaara
128840060203     c                   IF        %ERROR or RSUT = *blanks
128850060203     c                   clear                   Tibs34Ds
128860060203     c                   call      'TIBS34R'
128870060203     c                   parm                    Tibs34Ds
128880060203     c                   in        *dtaara
128890060203     c                   ENDIF
128900150305
128910150305      * abilitazione utente
128920150305     c                   clear                   wabi
128930150305     c                   eval      dUTE01 = UTEfaf
128940150305     c                   SELECT
128950150305      * carica abilitazioni del profilo
128960150305     c                   WHEN      §UTEorm <> *blanks
128970150305     c                   eval      wabi = §UTEorm
128980150305     c                   WHEN      UTEaut <> *blanks
128990150305     c                   eval      wabi = UTEaut
129000150305      * se non c'è l'abilitazione
129010150305      * --> se 1° livello, abilitazioni al terminal
129020150305      *     se 2° livello, abilitazioni al punto operativo
129030150305      *     se sede no abilitazioni
129040150305     c                   OTHER
129050150305     c                   IF        DUTlpo = '1'
129060150305     c                   eval      wabi   = 'TP'
129070150305     c                   ENDIF
129080150305     c                   IF        DUTlpo = '2'
129090150305     c                   eval      wabi   = 'PO'
129100150305     c                   ENDIF
129110150305     c                   ENDSL
129120150305
129130150305      * Reperimento delle filiali gestibili dall'utente
129140150305     c                   clear                   TRUL31DS
129150150305     c                   eval      I31abi = wabi
129160150305     c                   eval      I31cdi = DUTdis
129170150305     c                   eval      I31car = DUTare
129180150305     c                   eval      I31cpo = DUTpou
129190150305     c                   call      'TRUL31R'
129200150305     c                   parm                    KPJBA
129210150305     c                   parm                    TRUL31DS
129220150305     c                   IF        O31pog > *zeros
129230150305     c                   movea     O31pog        SKpog
129240150305     c                   ENDIF
129250060203      *
129260060203     c                   ENDSR
129270001010      *---------------------------------------------------------------------------------------------
129280001010** MSG  Lungh. 78                                                            *
129290010420Inserire mittente                                                                   1
129300010420Codice mittente non valido                                                          2
129310010213Ordinante non valido                                                                3
129320001010Immettere i colli                                                                   4
129330001010Immettere il peso                                                                   5
129340001010Immettere il volume                                                                 6
129350001010Immettere i bancali                                                                 7
129360001010Immettere il bilico                                                                 8
129370001010Immettere l'autotreno                                                               9
129380001010Immettere la motrice                                                                10
129390070418Inserire Filiale di ritiro                                                          11
129400131016Ora pronta merce maggiore dell'orario di fine servizio                              12
129410001010Tipo ordine non valido                                                              13
129420010202Immettere la cusale di chiusura                                                     14
129430010202Causale chiusura errata                                                             15
129440001010Codice tassazione o codice tariffa errati                                           16
129450070418Filiale consegna errata                                                             17
129460010118Destinatario non valido                                                             18
129470001010Attenzione !! Modificata Località                                                   19
129480001010Attenzione !! Modificato CAP                                                        20
129490001010Attenzione !! Modificata Provincia                                                  21
129500001010Attenzione !! Modificata Nazione                                                    22
129510030609Pagamento a carico dell'ordinante...inserire un codice valido                       23
129520001012Ora errata                                                                          24
129530001012Immettere i colli ------> ENTER x forzare                                           25
129540001012Immettere il peso ------> ENTER x forzare                                           26
129550001012Immettere il volume ------> ENTER x forzare                                         27
129560001012Immettere i bancali ------> ENTER x forzare                                         28
129570001012Immettere il bilico ------> ENTER x forzare                                         29
129580001012Immettere l'autotreno ------> ENTER x forzare                                       30
129590001012Immettere la motrice ------> ENTER x forzare                                        31
129600131016Ritiro no in giornata, superato l'orario di fine servizio                           32
129610070418Filiale ritiro errata                                                               33
129620001016Immettere data ritiro                                                               34
129630001016Data ritiro errata                                                                  35
129640070418La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica        36
129650010205Immettere almeno una quantità (Kg. Volume Bancali)                                  37
129660131107ORM multiplo non possibile se presente il destinatario                              38
129670010118Dati del destinatario incompleti                                                    39
129680010420Non è possibile variare il mittente                                                 40
129690070130ATTENZIONE!! Già avuto esito da PDA. Enter x forzare                                41
129700001103Non è stato modificato nulla                                                        42
129710070913Giro Ritiro errato                                                                  43
129720001215Data chiamata errata                                                                44
129730001215Data chiamata obbligatoria                                                          45
129740131107Orari di apertura incongruenti                                                      46
129750001215Ora chiamata obbligatoria                                                           47
129760010213ORM prepagato non possibile, inserire prima il prepagato!                           48
129770081007ORM IMPORT DPD automatico, non si può chiudere con causale "80"                     49
129780010223Immettere destinatario se ORM prepagato                                             50
129790161108Data ritiro immessa inferiore a quella calcolata.                                   51
129800010126Immettere l'ora del ritiro                                                          52
129810010202Priorità non trovata in tabella                                                     53
129820070913Per ORM prepagato immettere colli e peso                                            54
129830140424Immettere un indirizzo e-mail valido                                                55
129840010420Per ORM prepagato deve pagare il mittente                                           56
129850010215Inserire il referente                                                               57
129860010215Inserire il telefono                                                                58
129870040630Non immettere il codice tariffa se non immesso il codice cliente                    59
129880140424Immettere un numero di telefono valido                                              60
129890010223Immettere il destinatario. Orm singolo con pagamento a carico destinatario          61
129900070418ATTENZIONE!! Filiale ritiro diversa da quella calcolata                             62
129910010906ATTENZIONE!! data ritiro ricalcolata!                                               63
129920030806Causale chiusura non gestibile                                                      64
129930010305Data ritiro inferiore alla data immissione ORM                                      65
129940170303ATTENZIONE!! La data ritiro immessa è un giorno festivo                             66
129950010413ATTENZIONE!! fare F6 x confermare                                                   67
129960010906ATTENZIONE!! Volume troppo alto. Ricalcolare.                                       68
129970010502ATTENZIONE!! Mittente BLOCCATO                                                      69
129980070921ATTENZIONE!! Modificato codice mittente e/o codice cliente tariffa                  70
129990010507ATTENZIONE!! Numeratore ORM impegnato riprovare                                     71
130000010508ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                            72
130010140424Messaggi in sospeso su altre videate                                                73
130020011114Richiesta forzatura del destinatario ma l'ORM non è singolo                         74
130030011114Richiesta forzatura del destinatario ma i dati sono incompleti o errati             75
130040030808Se Mittente e Ordinante sono uguali impostare che paga il Mittente                  76
130050040419Non è possibile dirottare l'ORM                                                     77
130060070418Modificare la filiale ritiro                                                        78
130070070418La filiale ritiro non ha la proc.ORM attiva                                         79
130080040628ORM xxx-xx-xxxxxxx-xx da ritirare lo stesso giorno per lo stesso mittente           80
130090131202ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!        81
130100040727ATTENZIONE!! Ordinante BLOCCATO                                                     82
130110040727ATTENZIONE!! Destinatario BLOCCATO                                                  83
130120050120Pagamento a carico del destinatario...inserire un codice valido                     84
130130050128ATTENZIONE!! Le note immesse sono state sovrascritte                                85
130140050323ATTENZIONE!! Non esiste legame tra il mittente e l'ordinante. VERIFICARE!!!         86
130150050324ATTENZIONE!! ORM PREPAGATO non locale deve essere un ORM commissionato              87
130160050420Prima confermare le proposte di variazione                                          88
130170070418ATTENZIONE!! Filiale ritiro non gestibile                                           89
130180080422ATTENZIONE!! Per ORM commissionati a DPD il ritiro deve essere di 1 collo           90
130190140331Immettere la natura della merce.                                                    91
130200081126ATTENZIONE!! ORM Export DPD peso maggiore del limite consentito                     92
130210081126ATTENZIONE!! ORM Export DPD destinatario obbligatorio                               93
130220110120ATTENZIONE!! Presenti incongurenze fra Cap, Prov, Loc: modificare l'anagrafica      94
130230110610Per ORM Export EEX non è possibile la consegna all'estero                           95
130240110610Per ORM Export DPD non è possibile la consegna a                                    96
130250150615Tipo Comunicazione ORM non valido.                                                  97
130260160329ATTENZIONE!! Il giro è stato variato a seguito delle modifiche effettuate           98
130270010508** MOD  Lungh. 15                                                            *
130280001013  IMMISSIONE                                                                        1
130290001013 MANUTENZIONE                                                                       2
130300001009VISUALIZZAZIONE                                                                     3
130310001013   ANNULLATO                                                                        4
130320001106   CHIUSURA                                                                         5
130330040628 DIROTTAMENTO                                                                       6
130340090211   CONFERMA                                                                         7
130350010704** CM3
130360030611OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('ETICH     ')
130370160229 ovrscope(*calllvl) SHARE(*YES)
130380030623** cmA4
130390030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130400030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130410030623**  cmA5
130420030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
130430030623 SHARE(*YES)          USRDTA('xxxxxxxxx')
130440131011** TF02
130450131211F2=Dati DPD  #
130460131014** TF06
130470131211F6=Conferma  #
130480131011** TF11
130490131211F11=Volume  #
130500160405** TF13
130510160405F13=Conf.Prenotazione  #
130520131011** TF14
130530131211F14=OrariSr  #
130540131011** TF16
130550131211F16=Chiusura  #
130560131011** TF19
130570131211F19=Proposte  #
130580131011** TF20
130590131211F20=Simulazione  #
130600131011** TF21
130610131211F21=Dirottamento  #
