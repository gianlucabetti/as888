000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400150310      *  Nome programma:  FNlRU6R
000500150311      *  Descrizione   :  Interrogazione Bolle Unica - Selezione
000600150310      *  Data creazione:  2015 da FNLRI6R (Int. Bolle Arrivi)
000700940712      *------------------------------------------------------------------------*
000800991105     H DECEDIT('0,') DATEDIT(*DMY/)
000900150331     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR')
001000940712      *
001100150310     FFNLRU6D   CF   E             WORKSTN INFDS(CA02DS)
001200131107     F                                     SFILE(LRI6S02:NR2)
001300160311     FFNLRU6DA  CF   E             WORKSTN INFDS(CA02DSA)
001400160311     F                                     SFILE(LRI6S02A:NR2)
001500160311     F                                     USROPN
001700050405     Ftigcp21l  IF   E           K DISK
001800040218     FAZORG01L  IF   E           K DISK
001900911024     FTABEL00F  IF   E           K DISK
002000160502     FFIAPD01L  IF   E           K DISK
002100940712     F*.......... B O L L E   A R R I V I
002200940712     FFNARB01L  IF   E           K DISK
002300120330     FFNARB05L  IF   E           K DISK    RENAME(FNARB000:FNARB005)
002500020910     FFNARB78L  IF   E           K DISK    RENAME(FNARB000:FNARB078)
002600060216     FFNARB54L  IF   E           K DISK    RENAME(FnARB000:FNARB054)
002700060216     FFiAR401L  IF   E           K DISK
002701171023     FFipnd03l  if   E           K DISK
002800060216     FFiAR403L  IF   E           K DISK    RENAME(FiAR4000:FiAR4003)
002900150331     Ffiar501l  IF   E           K DISK
003000991026     FFIAR601L  IF   E           K DISK
003400940712     F*.......... D E T T A G L I O   C O L L I
003500940713     FFNART01L  IF   E           K DISK
003700150318     FFiART27L  IF   E           K DISK
003701161014     FFiART01L  IF   E           K DISK    rename(fiart000:fiart01)
003800161014     FFiarS03L  IF   E           K DISK
003801161014     FFiarS01L  IF   E           K DISK    rename(fiars000:fiars01)
004200940712     F*.......... B O L L E   P A R T E N Z E
004300150319     FFNBLP01L  IF   E           K DISK    prefix(ARB:3)
004400150318     FFNBLP26L  IF   E           K DISK    RENAME(FNBLP000:FNBLP026)
004500150318     f                                     prefix(ARB:3)
004600170224     F**FNblp32L  IF   E           K DISK    RENAME(FNblp000:FNblp032)
004700170224     f**                                     prefix(ARB:3)
004800150318     FFNblp33L  IF   E           K DISK    RENAME(FNblp000:FNblp033)
004900150318     f                                     prefix(ARB:3)
005000150326     FFNblp31L  IF   E           K DISK    RENAME(FNblp000:FNblp031)
005100150326     f                                     prefix(ARB:3)
005900010508     F*.......... O. R. M.
006000060216     FFiAR404L  if   e           k DISK    RENAME(FiAR4000:FiAR4004)
006700131114     F
006800131114     Ffipct02l  IF   E           K DISK
006801161011     Ffnevb04l  IF   E           K DISK
006802161012     Ffnevb22l  IF   E           K DISK
006900040218      *------------------------------------------------------------------------*
007000060130     D $pog            s              3    dim(250)
007100060130     D $DIG            s              1    dim(20)
007200060130     D $ARG            s              3    dim(50)
007201161116     D $pogp           s              3    dim(250)
007202161116     D $poga           s              3    dim(250)
007300060130     d
007800150331     D MSG             S             78    DIM(37) CTDATA PERRCD(1)
008400030612      *
008500991222     D CNM             S              1    DIM(11) CTDATA PERRCD(11)
010700911024     D*
010800131111     D C1              S              1    DIM(30)
010900941212     D L1              S              3  0 DIM(30)
011000060613     D k11             S              1    DIM(11)
011200921102     D SEG             S              1    DIM(15)
011300930705     D KCA             S              1    DIM(50)
011400940912     D DCA             S             44    DIM(50)
012800030527     D*
012900060130     D cbodop          S              2    DIM(25)
013000160503     D*
013001160523     d************************
013002160523     c* Parametri FNLRU6R :
013003160519     c* Campi di input:  PA1FLG, PA1GIA, PA1RMA, PA1XCO
013004160519     c* campi di output: V2CAAS,COMLNP,V2CNRS,V2CNSP,PA1F03
013005160523     d*
013100160523     D PARAMU6DS       DS
013101160523     d* CAMPI DI OUTPUT:
013102160523     d* . Key spedizione
013200940714     D  V2CAAS                14     17  0
013300940712     D  COMLNP                18     20  0
013400940714     D  V2CNRS                21     22  0
013500940714     D  V2CNSP                23     29  0
013501160523     d* . Flag '1'=Premuto F3=Fine
013600940712     D  PA1F03                30     30
013701160523     d* CAMPI DI INPUT:
013702160701     d* . Modalità di richiamo
013703160701     D  PA1FLG                31     31
013801160523     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F14=Giac.
013802160523     d*                                in interrogazione della bolla ed evitare così
013803160523     d*                                errore di chiamata ricorsiva
013804160519     D  PA1GIA               144    144
013805160523     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
015000110929     d  pa1rma               149    163
015100110929     d  pa1xco               164    164
015101160523     d* non utilizzato
015102160523     d  pa1flo               165    256
015103160523     d**********************
015104160523     d* Parametri FNLRU6R1:
015105160523     d* Campi di input:  V2CAAS,COMLNP,V2CNRS,V2CNSP,PA1FLG,PA1GIA, PA1TLA
015106160523     d* campi di output: PA1F03, PA1RMA, PA1XCO
015107160523
015109160606     D PARAMU6ds1      DS
015113160523     D  V2CAAS_1              14     17  0
015114160523     D  COMLNP_1              18     20  0
015115160523     D  V2CNRS_1              21     22  0
015116160523     D  V2CNSP_1              23     29  0
015119160523     D  PA1F03_1              30     30
015122160523     D  PA1FLG_1              31     31
015126160523     D  PA1GIA_1             144    144
015129160523     d  pa1rma_1             149    163
015130160523     d  pa1xco_1             164    164
015131160523     d  pa1tla_1             165    165
015132161124     d** OUTPUT - Codice di errore 1=Sped. non trovata
015135161124     d  pa1err_1             166    166
015136161124     d  pa1flo_1             167    256
015800940802     D* RICHIAMO PGM INTERROGAZ.BOLLE PARTENZE            - FNLS04R -
015900940721     D* RICHIAMO PGM INTERROGAZ.BOLLE TRANSITO            - FNLR68R -
016000161010     D*PARAM2          DS
016100161010     D* PA2FLG                31     31
016200161010     D* PA2BOL                62     62
020700940712     D                 DS
020800940712     D  MM                     1      2  0
020900940712     D  GG                     3      4  0
021000940712     D  MMGG                   1      4  0
021100901116     D                 DS
021200940713     D  V1CLNA                 1      3  0
021300940713     D  V1CLP1                 4      6  0
021400940713     D  V1CDTD                 7     14  0
021500940713     D  V1CDTA                15     22  0
021600940712     D  SEL1                   1     22  0
021700901116     D                 DS
021800940713     D  V1CLP2                 1      3  0
021900940713     D  V1CNRS                 4      5  0
022000940713     D  V1CNSP                 6     12  0
022100901116     D  SEL2                   1     12  0
022200940712     D                 DS
022300131111     D  V1CRSD                 1     30
022400131111     D  V1CRSM                31     60
022500131111     d  v1cind                61     90
022600131111     d  v1clod                91    120
022700131111     d  v1cnzd               121    123
022800131111     d  v1cprd               124    125
022900131111     D  SEL3                   1    125
023000010508     D                 DS
023100010508     D  V1Cpoe                 1      3  0
023200010508     D  V1CNsr                 4      5  0
023300010508     D  V1CNor                 6     12  0
023400010508     D  V1CNrv                13     14  0
023500010508     D  SEL4                   1     14  0
023600990119     D                 DS
023700160614     D**V2CTSP                 1      1
023800161010     D  V2CGCd                 1      2
023900161010     D  V2CTMC                 3      3
024000161010     D  V2CFLA                 1      3
024900131111     D
025000131111     D                 DS
025100160922     D  v1cbarcode             1     15  0
025200131111     D  b_aas                  1      2  0
025300131111     D  b_lnp                  3      5  0
025400131111     D  b_nrs                  6      7  0
025500131111     D  b_nsp                  8     14  0
026600150319     D                 DS
026700150319     D  V1cem1                 1     40
026800150319     D  V1cem2                41     70
026900150319     D  V1ceml                 1     70
027000131111     d
027100930705     D WLBDAT          DS
027200940712     D  G02DAT                 1      8  0
027300940712     D  G02INV                 9     16  0
027400940712     D  G02ERR                17     17
027500940712     D  G02TGI                18     22  0
027600900517     D CA02DS          DS
027700900517     D  PRIMA                378    379B 0
027800160311     D CA02DSA         DS
027900160311     D  PRIMAA               378    379B 0
028000950125     D                 DS
028500040218      *?definizione oggetto reclamo come spedizione?
028600131121     d ds_OggSped      ds                  inz
028700131121     d  wARBlnp                       3s 0 inz
028800131121     d  wARBnrs                       2s 0 inz
028900131121     d  wARBnsp                       7s 0 inz
029000131121     d  wARBaas                       4s 0 inz
029100040218      *
029200040218     d Status         sds
029300040218     d  SDSprm           *parms
029400991027      *
029500040216     D KPJBA         E DS
029600150310     d FNLRU6DS      e ds
030100040216      *
031200991027     D DS3A          E DS
031300940909     D DS7O          E DS
031500131108     D DS2A          E DS
031600020627
031700091214     d ds15          e ds
031800020627     d dspr          e ds
031900090604     d ds5e          e ds
032000020627
032100991217     D OG143         E DS
032200070403     D OG148         E DS
032400171023     D*DSBL4I        E DS
032401161012     D DSBL4L        E DS
032600971219     D DSARBD        E DS                  INZ
032700941014     D DSARBK        E DS                  INZ
032800941014     D DSARBG        E DS                  INZ
032900991011     D DARBT         E DS                  INZ
033500130920
033600131114     d fipctcetds    e ds
034000130920
034700941014      *
034800030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
034900030612     D TRUL90DS      E DS
035000941014      *
035100941212     D* DS PER TRUL06R - CARICAMENTO £X
035200991027     D TRUL06DS      E DS
035300991027     D  LIN                    1     90  0 DIM(30)
035400941212      *
035500941014     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
035600991027     D FNLR66DS      E DS
035700060224      *
035800060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
035900060224     D fnlv55ds      E DS
036000941014      *
036100941014     D* DS PER FNLR48R - MANUTENZIONE BOLLE ARRIVI
036200991027     D FNLR48DS      E DS
038000941212      *
038001161003     d Fnlri1ds      e ds
038002161003
038700130927     d dAr5emd       e ds
038701161013     d dAr5gen       e ds
038800030121     d Tibs02Ds      e ds
038900060130     d Azuteds       e ds                  extname(Azute00f)
039000060130     d dDatiute      e ds
039100060130     d dLat          e ds
039200060130     d dute01        e ds
039300060130     D TRUL31DS      E DS
039400060130     D TRUL31DS2     E DS
039500060130     D TIBS34DS      E DS
039700141010      // -?Reperimento Disposizioni di Consegna da WEB
039800141105     d FNLRY09DS2    e ds                  inz
039900040809      * ds per il controllo della presenza di CA per la spedizione richiesta
040000040809     d FIDN12DS      e ds
040100040809     d  skKey                 26    305    dim(20)
040200040809     d  skAnn                306    325    dim(20)
040300040809     d  skDca                326    485  0 dim(20)
040400040809     d  skFca                486    545  0 dim(20)
040500040809     d  skDch                546    705  0 dim(20)
040600040809     d  skCch                706    745    dim(20)
040700150331
040800150331     d dsemail       e ds
040900040809     d*
041400131111
041500131111       // -?File bolle arrivi
041600131111     d FNARBds       e ds                  extname(FNARB00F)
041700150319       // -?File bolle partenze
041800150319     d FNBLPds       e ds                  extname(FNBLP00F)
041801161116
041802161116      // - Controllo NPR e Recupero ORM da NPR
041803161116     d FIORC0I0      e ds                  QUALIFIED INZ(*EXTDFT)
041804161116     d  idLingua     e                     INZ(FIORC00_ID_LINGUA_IT)
041805161116     d FIORC0O0      e ds                  QUALIFIED INZ(*EXTDFT)
041806161116     d FIORC0M0      e ds                  QUALIFIED INZ(*EXTDFT)
041807161116     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
041808161116     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
041809161116     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
041810161116     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
041900131111     d
042000131111     d wEnd            s               n   inz(*off)
042100040809
042200991027      *
042700060130     D savkpjbu        S                   LIKE(kpjbu)
042800060130     D kscfil          S                   LIKE(arblna)
042900171023     D*War4aas         S                   LIKE(AR4aas)
043000171023     D*War4lnp         S                   LIKE(AR4lnp)
043100171023     D*War4nrs         S                   LIKE(AR4nrs)
043200171023     D*War4nsp         S                   LIKE(AR4nsp)
043201171023     D K_aas           S                   LIKE(BLPaas)
043202171023     D K_lnp           S                   LIKE(BLPlnp)
043203171023     D K_nrs           S                   LIKE(BLPnrs)
043204171023     D K_nsp           S                   LIKE(BLPnsp)
044300160502     d kpdpdr          S                   LIKE(APDpdr)
044400160502     d kpdtip          S                   LIKE(APDtip)
044500131114     D arbrsduni       S             70
044600131114     D arbdsp          S              8  0
044800150319     D W0070           S              7  0
045000150331     D W0030           S              3  0
045400131209     D WopenC          s              1
045500150319     D WopenC2         s              1
045501170224     D WopenC3         s              1
045600020225     d lnantw          s                   like(§ogntw)
045700020225     d lnpntw          s                   like(§ogntw)
045800020910     d Wifp            s                   Like(Arbifp) Inz
045900060130     d wabi            s                   like(UteAut)
045901161021     d*wkcc            s                   like(dutkci)
045902161011     d wdcm            s                   like(arbdcm)
045903161011     d whmc            s              4
045904161011     d wmic            s              1
045905161011     d kcev            s                   like(evbcev)
045906161011     d kdev            s                   like(evbdev)
045907161012     d w_ntwlna        s              1
046000161012     d w_lnaIt         s              1
046001161012     d wdata           s              8  0
046002161012     d w_fdcons        s              1
046300020628
046500020628     D ordina          S              1
046600020628     D comando         S              1
047200110930     d comrma          s                   like(arbrma)
047700131209     d AP              s              3  0
048000131209     d w060a           s             60
048200141010     d $Fine           s              1    inz(*off)
048201161115     d $esci           s              1    inz(*off)
048300150331     d lenrag          s              2  0
048301161014     d len             s              2  0
048400160506     d w_arr           s              1
048500160509     d w_cev           s                   like(§pctcmc)
048600160509     d w_dev           s                   like(§pctdata)
048601160719     d w_ilru6sel      s                   like(ilru6sel)
048602160719     d dataeur         s               d   datfmt(*eur)
048603160909     d w_v1ctel        s                   like(v1ctel)
048604160909     d w_§ar5eml       s                   like(§ar5eml)
048605161013     d wtel_ok         s              1
048606161013     d wteld_ok        s              1
048607161014     d w_ars           s              1
048608161108     d num3            s              3  0
048609161116     d wel4            s             14
048610161123     d v1cdtd_w        s                   like(v1cdtd)
048611161123     d v1cdta_w        s                   like(v1cdta)
048612161124     d w_nrigs         s              2  0
048613161013
048700141010       // -?Reperimento Disposizioni di consegna da WEB
048800141010     d fnlry09r        pr                  extpgm('FNLRY09R')
048900141013     d  kpjba                              likeds(kpjba)
049000131111     d
049100131111       // -?Stringa SQL da eseguire
049200131111     d wSQL            s           2048    Varying        inz
049300030527     D*
049400030527     D wrkoggiiso      s               d
049500160311     d dsp_mod         s             10I 0
049501161115     d comlp1          s                   like(v1clp1)
049502161124     d wsflpieno       s              1
049503161125     d posw            s              5  0
049504161125     d posa            s              5  0
049600991027      *
049700930705     D* DEFINIZIONI COSTANTI
049800930705     D*
050000941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
050100150318     D                                     LE FILIALE *')
050200150318     D CMANUT          C                   CONST('** MANUTENZIONE  BOL-
050300150318     D                                     LE ARRIVI **')
050301160909
050302160909     D minu            C                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto inglese
050303160909     D maiu            C                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO INGLESE
050900110705     d
051600150327
051700150327     D digits          c                   '0123456789'
051701161116      /copy gaitrasrc/srcconst,FIORC00R
051800150331
051900150331      // ? PROTOTIPI ?
051902161116      /copy gaitrasrc/srcprotopi,FIORC00R
051903161116      /copy gaitrasrc/srcprotopr,FIORC00R
052000150331      /copy gaitrasrc/srcprotopr,ubchkcel
052100150331      /copy gaitrasrc/srcprotopi,ubchkcel
052200160311      /copy gaitrasrc/srcprotopr,QsnQryModS
052300150327
052400150318     ifiart000
052500150318     i              artnfv                      artnfvi
052501161014     ifiart01
052502161014     i              artnfv                      artnfvi
052600150318     i
052700900518     C****************************************************************
052800900518     C*  RIEPILOGO INDICATORI
052900900518     C***************************************************************
053000911023     C* 01    - SELEZIONE PER LINEA ARRIVO
053100911023     C* 02    - SELEZIONE PER LINEA PARTENZA
053700911023     C* 07    - PARZIALIZZ. SEGNACOLLO DAL
053900131114     C* 09    - scelta in altre parzializz
054100911023     C* 11    - PARZIALIZZ. MITTENTE
054101160719     c* 12    - Condizionamento tasto funzionale F1=Parzializza
054300961111     C* 13    - SPEDIZIONE IN CONSEGNA IN EVIDENZA IN SUBFILE
054400911023     C* 14    - PARZIALIZZ. DESTINATARIO
054500940318     C* 15    - DATA DAL UGUALE A DATA AL (SCELTO UN SOLO GIORNO)
054800911023     C* 18/21 - STATO SPEDIZIONI SELEZIONATO
055200010508     C* 25    - SELEZIONE PER NUMERO O.R.M.
055201161116     C* 26    - SELEZIONE PER NUMERO Prenotazione ritiro
055500941028     C* 28    - VISUALIZZA MSG ERRORE
055600160518     C* 29    - errore parzializzazione su cliente mittente
055700911023     C* 30/31 - LETTURA COMODO
055800911023     C* 32    - BOLLA LETTA DA SCARTARE
055801160718     C* 33    - OFF SFLDSPCTL
056000911023     C* 34    - LETTURA SUBFILE
056100160718     C* 35    - OFF SFLDSP
056200911129     C* 36    - LETTURE
056300911023     C* 37    - ROLL UP
056301160615     C* 38    - Errore selezione codice cliente nella videta "altre parzializzazioni"
056400920203     C* 39    - ABILITAZIONE TASTO DI COMANDO INT. BOLLE PARTENZE
056500160518     C* 40-60 - ERRORI
056700131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
056800161013     C* 62    - ALTA INTENSITA' PER SPED "Bloccate"
056900040218     C* 63    - Abilita F1  = Aggiunta in reclamo
057100160518     c* 65    - Errore indirizzo email
057101161117     c* 67    - pos. cursore campo stato consegna
057400131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
057500930505     C*         PREVISTA E LE ORE DI RITARDO
057501161116     C* 69    - pos cursore numero prenotazione ritiro
059100970925     C* 84    - PARZIALIZZ. SEGNACOLLO AL
059400111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
059700920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
059900160518     C* 92    - Errore campo parzializzazine telefono destinatario
060200991222     C* 97    - MANUTENZIONE BOLLE ARRIVI
060500900518     C*****************************************************************
060600000000     C     *ENTRY        PLIST
060700000000     C                   PARM                    KPJBA
060800150310     c                   parm                    FNLRU6DS
060900131210     c
061000150408     c                   if        sdsprm>1
061100150402     c                   clear                   olru6err
061200150402     c                   clear                   olru6msg
061201160518     c                   clear                   olru6tor
061202160518     c                   clear                   olru6ogg
061203160719     c* parametro di work
061204160719     c                   eval      w_ilru6sel=ilru6sel
061300150408     c                   endif
061400150402
061500131210      /free
061600131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
061700131210      /end-free
061800980609     C**
061900160523     C                   MOVEL     KPJBU         PARAMU6DS
062100940713     C*
062200060130     C                   Z-ADD     1             CODUT             1 0
062300060130     c                   Movel     rsut          V1cdsa
062500941227     C*
062700160519     C****               MOVEL     V1CDSA        PA1DSA
062800940713     C*-------------------------------------------------------------------------
062900911024     C                   EXSR      CARTAB
063000131111
063100131111     c                   eval      Wend=*off
063200110930     c*
063300111005     c                   setoff                                       87
063400110930     c                   if        pa1flg='W'
063500110930     c                   eval      comrma=pa1rma
063600111005     c                   eval      *in87=*on
063700110930     c                   endif
064100161013     C***                MOVE      'N'           V1CAFF
064200941215     C                   MOVEL     '? '          V1CCVB
064300941215     C                   CLEAR                   V1DCVB
064400941014     C* TESTATA
064500941014     C                   MOVEL     CINTER        V1CTES
064600161108     c***                clear                   v1clna
064700940317      *
064800940712      * IMPOSTO LA LINEA DI ARRIVO
064900060217     c* Non la imposto se richiamato con key specifica
065000161108if  1C***  PA1FLG        IFNE      '1'
065100161108     C***  PA1FLG        ANDNE     '3'
065200161108     C***  PA1FLG        ANDNE     'C'
065300161108     C***  PA1FLG        ANDNE     'N'
065400161108     C***  PA1FLG        ANDNE     'P'
065500161108     C***  PA1FLG        ANDNE     'E'
065600161108     C***  PA1FLG        ANDNE     'W'
065700060217     c* o se vengo da int bolle normale (per assistenza)
065800161108     c***  pa1flg        andne     ' '
065900161108     c***  pa1flg        andne     '4'
066000161108     c***                z-add     dutpou        v1clna
066100020424     c*
066200161108     C***                MOVE      V1CLNA        NUM3              3 0
066300161108     C***  NUM3          CHAIN     AZORG01L                           90
066400161108     C**N90              MOVEL     ORGDES        V1CDEA
066500161108e   1C****               ENDIF
066600930702     C*
066700920703     C                   EXSR      PUL01
066800940809     C                   MOVEL     'S'           WRIEM             1
066900930702     C**
067000930702     C* PGM RICHIAMATO
067100920326     C**
067200940713     C** PA1FLG =   - NON RICHIAMATO
067300920423     C**        = 1 - RICHIAMATO CON   KEY SPECIFICA
067400920423     C**        = 2 - RICHIAMATO SENZA KEY SPECIFICA PER   SELEZ BOLLA
067500920423     C**        = 3 - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
067600920423     C**        = 4 - RICHIAMATO SENZA KEY SPECIFICA SENZA SELEZ BOLLA
067700990129     C**        = C - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F2=C.A
067800941014     C**        = M - MANUTENZIONE BOLLE ARRIVI
067900960903     C**        = N - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F7=ANOMA
068000960903     C**        = P - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
068100960903     C**              E DISABILITAZ F7=ANOMALIE
068200961121     C**        = E - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F8=EV.SEGNACOLLI
068300110930     C**        = W - RICHIAMATO SENZA KEY SPECIFICA per "PerfectOrder"
068400960903     C*
068500160719      *?Abilito F1=Aggiunta in Reclami?e F1=Parzializza
068600150401      * (se richiamato da pgm. R.A ......)
068700040220     c                   eval      *in63  = *off
068701160719     c                   eval      *in12  = *off
068800160719     c***                if            SDSprm >  1  and ilru6ra='S'
068801160719     c                   if            SDSprm >  1
068802160719     c                   if        ilru6ra = 'S'
068900150401     c                             and ELRU6INS = ' '
069000040220     c                   eval      *in63  = *on
069100060301     c                   clear                   v1clna
069200060301     c                   clear                   v1cdea
069201160719     c                   endif
069202160719     c                   if        w_ilru6sel='S'
069203160719     c                   eval      *in12=*on
069204160719     c                   endif
069300111003     c                   endif
070300960903     C*
070400160519if  1C***  PA1FLG        IFEQ      '1'
070500160519     C***  PA1FLG        OREQ      'N'
070600160519     C***  PA1FLG        OREQ      '3'
070700160519     C***  PA1FLG        OREQ      'P'
070800160519     C***  PA1FLG        OREQ      'E'
070900160519     C***  PA1FLG        OREQ      'C'
071000920703     C*
071100160519     C*                  MOVEL     V2CAAS        V1CAAS
071200160519     C*                  MOVEL     COMLNP        V1CLP2
071300160519     C*                  MOVEL     V2CNRS        V1CNRS
071400160519     C*                  MOVEL     V2CNSP        V1CNSP
071500920703     C* SE NO PASSATA KEY SPECIFICA SI PUO' SELEZIONARE
071600940713     C     PA1FLG        IFEQ      '1'
071700960903     C     PA1FLG        OREQ      'N'
071800961121     C     PA1FLG        OREQ      'E'
071900990129     C     PA1FLG        OREQ      'C'
071901160519     c* Asterisco la seguente specifica: il numero spedizione sarà per forza = 0
071902160519     c* altrimenti non sare in questo pgm ma nel fnlru6r1
072000160519     c****               if        v1cnsp=0
072100920703     C                   SETON                                        61
072200160519     C****               ENDIF
072300160519     C****               ENDIF
072400040217x   1C                   ELSE
072500110929     c* Se richiamata con "W" devo saltare tutta la gestione della
072600110929     c* prima videata e devo passare subito al caricamento del sfl
072700110929   1ac     pa1flg        ifne      'W'
072800911213     C*
072900940713     C     PA1FLG        IFEQ      '2'
073000911213     C                   SETON                                        61
073100040217     C                   ENDIF
073200920203     C*
073400040217if  2C     PA1FLG        IFEQ      ' '
073500920203     C                   SETON                                        39
073600040217e   2C                   ENDIF
073700941014     C*
073800000616     C* MANUTENZIONE BOLLE ARRIVI 97=ON
073900040217if  2C     PA1FLG        IFEQ      'M'
074000030612      *
074100941014     C                   SETON                                        97
074200030612     C                   MOVEL     CMANUT        V1CTES
074300070420     c
074400040217e   2C                   ENDIF
074500000703     C*
074600941209     C     INIZIO        TAG
074700131114     C***                EXSR      PUL01
074800131114
074900131114     c* se cursore aperto --> lo chiudo
075000131209     c*****              if        Wend=*on  or *in14 or *in11
075100131209     c                   if        WopenC='1'
075200131114     c                   exsr      closecursor
075300131114     c                   endif
075400150319     c                   if        WopenC2='1'
075500150319     c                   exsr      closecursorBlp
075600150319     c                   endif
075601170224     c                   if        WopenC3='1'
075602170224     c                   exsr      closecursorRMA
075603170224     c                   endif
075700911025     C*
075800900509     C     FOR01         TAG
075900941209     C*
076000040217if  2C     WRIEM         IFEQ      'S'
076100131107     C                   WRITE     LRI6T01
076200940720     C                   MOVEL     *BLANKS       WRIEM
076300040217e   2C                   ENDIF
076400150402     c* solo se devo emettere il video
076500160719     c                   if        sdsprm=1 or w_ILRU6SEL=' '
076600131107     C                   EXFMT     LRI6D01
076700150402     c                   endif
076800940713     C*
076900940713     C                   SETOFF                                       28
077000940715     C                   MOVEL     *BLANKS       WRIEM
077100940713     C*
077200980603     C** F3 - FINE LAVORO
077300040217if  2C     *INKC         IFEQ      '1'
077500980603     C                   GOTO      FINE
077600040217e   2C                   ENDIF
077601161003     c**
077602161003     C** F16 - Localizzazione filiale chi sono BRT
077603161003     C   KQ              DO
077604161003     c                   if        v1cnot<>*blanks
077605161003     C                   clear                   fnlri1ds
077606161003     c                   movel     v1cnot        ii1chi
077607161003     C                   MOVEL     fnlri1ds      KPJBU
077608161003     C                   CALL      'FNLRI1R'
077609161003     C                   PARM                    KPJBA
077610161003     C                   MOVEL     KPJBU         fnlri1ds
077611161003     c                   if        oi1err='1'
077612161003     c                   movel     oi1msg        v1cmsg
077613161003     C                   SETON                                        289062
077614161003     c                   endif
077615161003     c                   else
077616161003     c                   movel     msg(30)       v1cmsg
077617161003     C                   SETON                                        289062
077618161003     c                   endif
077619161003     C                   MOVEL     'S'           WRIEM
077620161003     C                   GOTO      FOR01
077621161003     C                   END
077700131115     C**
077800131115     C** F5 - Altre parzializzazioni
077900131115     c                   if        *inke=*on
078000131115     c                   exsr      AltrePar
078100131115     C                   MOVEL     'S'           WRIEM
078200131115     C                   GOTO      FOR01
078300131115     c                   endif
078400131114     C**
078500131114     C**
078600131114     C** F10- pulizia di tutti i campi di scelta
078700131114if  2C     *INKj         IFEQ      *ON
078800131114     C                   EXSR      pul01
078900131114     C                   MOVEL     'S'           WRIEM
079000131114     C                   GOTO      FOR01
079100131114e   2C                   ENDIF
079200930922     C**
079300980603     C** F14- INTERROGAZIONE BOLLE TRANSITO
079400161010do  2C** KN              DO
079500161010     C**                 MOVEL     'T'           PA2BOL
079600161010     C**                 MOVEL     PARAM2        KPJBU
079700161010     C**                 CALL      'FNLR68R'
079800161010     C**                 PARM                    KPJBA
079900161010     C**                 MOVEL     'S'           WRIEM
080000161010     C***                GOTO      FOR01
080100161010e   2C***                ENDDO
080200911025     C*
080300110929e  1ac                   endif
080400040217e   1C                   ENDIF
080500900509     C* CONTROLLI FORMATO1
080600110930     c     pa1flg        ifne      'W'
080700900509     C                   EXSR      CONTR
080800150402     c                   if        *in90
080801161104     c* SE non era richiesto il passaggio dalla prima videata ma riscontrati errori
080802161104     c* emetto comunque la prima videata ma senza segnalare errori faccio cioè come
080803161104     c* se entrassi per la prima volta nella videata
080804161104     c                   if        sdsprm>1 and w_ilru6sel='S'
080805161104     c                   exsr      sr_inzId1
080806161104     c                   eval      w_ilru6sel=*blanks
080807161104     c                   setoff                                       28
080808161104     c                   endif
080900161104     c*****              if        sdsprm=1 or w_ILRU6SEL=' '
080901160719     C                   MOVEL     'S'           WRIEM
081000150402     C                   GOTO      FOR01
081100160718     c*                  else
081200160718     c*                  eval      olru6err='E'
081300160718     c*                  eval      olru6msg=v1cmsg
081400160718     c*                  goto      fine
081500161104     c***                endif
081600150402     c                   endif
081700110929     c                   endif
081800911025     C**
081900911025     C** P E R   N U M E R O   S P E D I Z I O N E
082000131111     c**  o      B A R C O D E
082100040217if  1C     SEL2          IFNE      0
082200131111     c     v1cbarcode    orgt      0
082300150316     C                   EXSR      SINGOL
082400971223     C     WINIZ         IFEQ      '1'
082500971223     C                   GOTO      INIZIO
082600971223     C                   ENDIF
082700971223     C     WFINE         IFEQ      '1'
082800971223     C                   GOTO      FINE
082900971223     C                   ENDIF
083000040217e   1C                   ENDIF
083100911025     C**
083200911025     C**  P E R   S U B F I L E
083300150508     C                   EXSR      ELAB
083400941014     C*
083500901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
083600040217if  1C     NR2           IFEQ      0
083700160719     C****               MOVEL     MSG(1)        V1CMSG
083800160719     C****               SETON                                        28
083801161104     c                   if        sdsprm>1 and w_ilru6sel='S'
083802161104     c                   movel     'S'           wriem
083803161104     c                   eval      w_ilru6sel=*blanks
083804161104     C                   GOTO      for01
083805161104     c                   endif
083900131115     c                   select
084000131115     c                   when      v1crpe<>*blanks
084100131115     C                   SETON                                        58
084200131115     c                   when      v1crmn>0 or v1crma<>*blanks
084300131127     C                   SETON                                        68
084400131115     c                   when      v1cnot<>*blanks
084500131115     C                   SETON                                        62
084600131115     c                   when      v1cprd<>*blanks
084700131115     C                   SETON                                        50
084800131115     c                   other
084900131115     C                   SETON                                        51
085000131115     c                   endsl
085100160719     c                   if        sdsprm=1 or w_ILRU6SEL=' '
085101160719     C                   MOVEL     MSG(1)        V1CMSG
085102160719     C                   SETON                                        28
085103160719     c                   movel     'S'           wriem
085200900516     C                   GOTO      FOR01
085300160718     c*                  else
085400160718     c*                  eval      olru6err='E'
085500160718     c*                  eval      olru6msg=v1cmsg
085600160718     c*                  goto      fine
085700150402     c                   endif
085800040217e   1C                   ENDIF
085900971223     C**
086000040217if  1C     V1CRPE        IFNE      *BLANKS
086100010509     C     NR2           ANDEQ     1
086200010509     c     *in25         oreq      *on
086300971223     C     NR2           ANDEQ     1
086301161116     c     *in26         oreq      *on
086302161116     C     NR2           ANDEQ     1
086400111011     c     v1crmn        orgt      *zeros
086500111011     C     NR2           ANDEQ     1
086600111011     c     v1crma        orne      *blanks
086700111011     C     NR2           ANDEQ     1
086800131111     c     v1cnot        orne      *blanks
086900131111     C     NR2           ANDEQ     1
087000111011     C                   MOVEL     v2clnp        COMLNP
087100150507
087200150507     c                   clear                   fnarbds
087300150507     c                   exsr      sr_inzsoloblp
087400150507     c
087500150507     c     karb          chain     fnblp01l
087600150507     c     karb          chain     fnarb01l
087700971223     C                   EXSR      SINGOL
087800040217     C     WINIZ         IFEQ      '1'
087900971223     C                   GOTO      INIZIO
088000040217     C                   ENDIF
088100040217     C     WFINE         IFEQ      '1'
088200971223     C                   GOTO      FINE
088300040217     C                   ENDIF
088400040217e   1C                   ENDIF
088500930922     C**
088600900518     C     FORCT2        TAG
088601160718     c                   if        nr2=0
088602160718     c                   eval      *in35=*on
088603160719     c                   setoff                                       28
088604160719     c                   clear                   v1cmsg
088605160718     c                   endif
088700160311     C                   IF        dsp_mod = 1
088800160311     C                   WRITE     LRI6T01A
088900160311     C                   WRITE     LRI6Z02A
089000160311     c                   else
089100131107     C                   WRITE     LRI6T01
089200131107     C                   WRITE     LRI6Z02
089300160311     c                   endif
089400900515     C** COMANDI SUBFILE
089500900515     C     FORCT         TAG
089501161124      * se i record scritti nel sfl supera il limite max previsto dal sistema
089502161124      * emetto msg di errore
089503161124     c                   if        wsflpieno<>*blanks
089504161124     c                   Eval      *In59 = *On
089505161124     c                   Eval      *In28 = *On
089506161124     c                   Eval      V1cMsg = Msg(27)
089507161124     c                   if        dsp_mod= 1
089509161124     c     rec2          Chain     LRi6S02a                           34
089510161124     c                   else
089511161124     c     rec2          Chain     LRi6S02                            34
089512161124     c                   endif
089513161124     c                   Eval      *In36 = *On
089514161124     c                   if        dsp_mod= 1
089515161124     c                   Update    LRi6s02a
089516161124     c                   else
089517161124     c                   Update    LRi6s02
089518161124     c                   endif
089519161124     c                   Eval      *In36 = *Off
089520161124     c                   Eval      *In59 = *Off
089521161124     c                   endif
089522161124
089600160311     C                   IF        dsp_mod = 1
089700160311     C                   EXFMT     LRI6C02A
089800160311     c                   else
089900131107     C                   EXFMT     LRI6C02
090000160311     c                   endif
090100940923     C                   SETOFF                                       28
090200941109     C                   CLEAR                   PA1F03
090300980603     C* F3 - FINE
090400980603     C     *INKC         IFEQ      '1'
090600980603     C                   GOTO      FINE
090700980603     C                   ENDIF
090800930922     C*
090900980603     C* F12 - RITORNO
091000040217if  1C     *INKL         IFEQ      *ON
091100110930     c     pa1flg        cabeq     'W'           fine
091200940713     C     PA1FLG        IFEQ      '3'
091300960903     C     PA1FLG        OREQ      'P'
091400940713     C     PA1FLG        OREQ      '1'
091500960903     C     PA1FLG        OREQ      'N'
091600961121     C     PA1FLG        OREQ      'E'
091700990129     C     PA1FLG        OREQ      'C'
091800940714     C                   Z-ADD     0             V2CAAS
091900920702     C                   Z-ADD     0             COMLNP
092000940714     C                   Z-ADD     0             V2CNRS
092100940714     C                   Z-ADD     0             V2CNSP
092200930922     C                   ENDIF
092202160719     c
092300160719     c                   if        sdsprm=1 or ILRU6SEL=' '
092400160428     c                   eval      wriem='S'
092500930922     C                   GOTO      INIZIO
092600150402     c                   else
092700161104     c****               goto      fine
092701161104     c                   movel     'S'           wriem
092702161104     c                   eval      w_ilru6sel=*blanks
092703161104     c                   goto      inizio
092800150402     c                   endif
092900040217e   1C                   ENDIF
092901160719     c* F1 = Parzializza
092902160719     c                   if        *in12=*on and *inka=*on
092903160719     c                   movel     'S'           wriem
092904161104     c****               eval      *in50=*on
092905160719     c                   eval      w_ilru6sel=*blanks
092906161104     c                   goto      inizio
092907160719     c                   endif
093000900515     C* ROLLUP
093100040217if  1C     *IN37         IFEQ      *ON
093200940713     C   30              DO
093300940713     C                   MOVEL     MSG(15)       V1CMSG
093400940713     C                   SETON                                        28
093500940713     C                   ENDDO
093600930922     C  N30              EXSR      CARSF
093700940713     C                   Z-ADD     COMOD         REC2
093800930922     C                   GOTO      FORCT
093900040217e   1C                   ENDIF
094000900515     C*
094100900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
094101160719     c                   if        not *in35
094400160311     C                   IF        dsp_mod = 1
094401160909     C                   Z-ADD     PRIMAA        REC2
094500160311     C                   READC     LRI6S02A                               34
094600160311     c                   else
094601160909     C                   Z-ADD     PRIMA         REC2
094700131107     C                   READC     LRI6S02                                34
094800160311     c                   endif
094900911023     C* VISUALIZZO BOLLA
095000040217do  1C     *IN34         DOWEQ     *OFF
095100150317     C                   MOVEL     *BLANKS       V2CFLG            1
095200911023     C*
095300040217if  2C     V2CSCE        IFEQ      '5'
095400940714     C                   Z-ADD     V2CLNP        COMLNP
095500150507     c                   clear                   fnarbds
095600150507     c                   exsr      sr_inzsoloblp
095700150507     C     KARB          CHAIN     FNblp01L                           31
095800150507     C     KARB          CHAIN     FNARB01L                           31
095900930518     C*
096000940714     C                   MOVEL     *BLANKS       V2CSCE
096201161011     c                   if         %subst(v2ccon:1:7)='IN CONS'
096300930518     C                   SETON                                        13
096400161013     C                   endif
096401161013     c                   if        v2hi62='1'
097100160509     c                   seton                                        62
097101161013     c                   else
097102161013     c                   setoff                                       62
097200160509     c                   endif
098000941014     C*
098100160311     C                   IF        dsp_mod = 1
098200160311     C                   UPDATE    LRI6S02A
098300160311     c                   else
098400131107     C                   UPDATE    LRI6S02
098500160311     c                   endif
098509160606     c                   eval      PARAMU6ds1=paramu6ds
098510160523     c                   clear                   pa1tla_1
098511160606     c                   eval      kpjbu=paramu6ds1
098700911023     C*
098800150317     c                   if        sdsprm>1
098900150311     c                   call      'FNLRU6R1'
099000150311     c                   parm                    kpjba
099100150401     c                   parm                    fnlru6ds
099200150317     c                   else
099300150317     c                   call      'FNLRU6R1'
099400150317     c                   parm                    kpjba
099500150317     c                   endif
099600160606     c                   eval      paramu6ds1=kpjbu
099700150410     c                   select
099800160523     c                   when      pa1f03_1='1'
099900150410     c                   eval      v2cflg='2'
100000160523     c                   when      pa1f03_1='S'
100100150410     c                   eval      v2cflg='3'
100200150410     c                   endsl
100300150318     c                   movel     'S'           wriem
100400911023     C*
100500040217x   2C                   ELSE
100600941014     C*
100700941014     C* 97 ON - MANUTENZIONE BOLLE
100800040217if  3C     V2CSCE        IFEQ      '1'
100900941014     C*
101000040217if  4C     *IN97         IFEQ      *ON
101100060227     c                   movel     v2clna        w003a             3
101200060224     C     w003a         LOOKUP    $pog                                   31
101300060130    2c                   if        not *in31 and d66pocl='S'
101400060130     c     v2ccbo        lookup    cbodop                                 31
101500060130    3c                   if        not *in31
101600060130     c                   movel     v2cksc        kscfil
101700060130   x3c                   else
101800060130     c* Se bolla doppia prendo il ksc dall'AR6
101900060130     C                   Z-ADD     v2cAAS        KAAS
102000060130     C                   Z-ADD     v2cLNP        KLNP
102100060130     C                   Z-ADD     v2cNRS        KNRS
102200060130     C                   Z-ADD     v2cNSP        KNSP
102300060130     C                   MOVEL     '2'           KTRC
102400060130     C     KAR61         CHAIN     FIAR6000                           31
102500060130if  4C                   IF          %FOUND(fiar601l)
102600060130     c                   movel     ar6ksc        kscfil
102700060130     c                   else
102800060130     c                   movel     999           kscfil
102900060130    4c                   endif
103000060130    3c                   endif
103100130314     c*
103200060224     c                   movel     kscfil        w003a
103300060224     c     w003a         lookup    $pog                                   31
103400060130    2c                   endif
103500130314     c*
103600040217if  5C     *IN31         IFEQ      *OFF
103700941212     C                   MOVEL     MSG(19)       V1CMSG
103800941212     C                   SETON                                        599028
103900941212     C                   MOVEL     '2'           V2CFLG
104000130314     c*
104100040217x   5C                   ELSE
104200141010
104300130314     c*
104400991027     C                   CLEAR                   FNLR48DS
104500060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
104600060224     c*  di arrivo
104700060224     c                   clear                   fnlv55ds
104800060224     c                   movel     v2clna        d55lin
104900060224     c                   movel     '6'           d55tpt
105000060224     C                   time                    wrkoggiiso
105100060224     C     *iso          move      wrkoggiiso    d55drf
105200060224     c                   call      'FNLV55R'
105300060224     c                   parm                    fnlv55ds
105400060224     c                   movel     d55tfa        d48fgs
105500130314     c*
105600941014     C                   MOVEL     V2CAAS        D48AAS
105700941014     C                   MOVEL     V2CLNP        D48LNP
105800941014     C                   MOVEL     V2CNRS        D48NRS
105900941014     C                   MOVEL     V2CNSP        D48NSP
106000141010     c* routine per verifica dispo da esaminare
106100141010     c                   exsr      sr_wdwdc
106200141010
106300941014     C                   MOVEL     V2CCBO        D48CBO
106400941014     C                   MOVEL     V1CCVB        D48CVB
106500950616     C                   MOVEL     'V'           D48FFR
106600941227     C                   MOVEL     'A'           D48TBO
106700950607     C                   MOVEL     'S'           D48F12
106800941014     C*
106900991027     C                   MOVEL     FNLR48DS      KPJBU
107000941014     C                   CALL      'FNLR48R'
107100941014     C                   PARM                    KPJBA
107200941212     C                   PARM                    DSARBD
107300941212     C                   PARM                    DSARBK
107400991011     C                   PARM                    DARBT
107500941212     C                   PARM                    DSARBG
107600030612     C                   PARM                    trul90ds
107700941212     C*
107800991027     C                   MOVEL     KPJBU         FNLR48DS
107900941212     C*
108000941014     C* ERRORE IN MANUTENZIONE BOLLE
108100040217     C     D48ERR        IFEQ      '1'
108200941014     C                   MOVEL     D48MSG        V1CMSG
108300941014     C                   SETON                                        592890
108400941014     C                   MOVEL     '2'           V2CFLG
108500040217     C                   ENDIF
108600950114     C*
108700950114     C* SE NON C'E' ERRORE AGGIORNO IL SFL CON I DATI DELLA BOLLA VAR
108701161229     c* Non testo il 90 perchè poi non si spegne
108800161229     C***  *IN90         IFEQ      *OFF
108801161229     C     D48ERR        IFne      '1'
108900950120     C                   Z-ADD     V2CLNP        COMLNP
108901161229     c                   clear                   fnarbds
108902161229     c                   exsr      sr_inzsoloblp
108903161229     c     karb          chain     fnblp01l
109000161229     C     KARB          CHAIN     FNARB01L
109100950114     C                   MOVEL     ARBRSM        V2CRSM
109200160311     C                   MOVEL     ARBRSM        V2CRSMa
109300950114     C                   MOVEL     ARBRSD        V2CRSD
109400160311     C                   MOVEL     ARBRSD        V2CRSDA
109500131114     c                   if        v1cind=*blanks
109600131114     c                   eval      v2cdest=arblod
109700160311     c                   eval      v2cdesta=arblod
109800131114     c                   else
109900131114     c                   eval      v2cdest=arbind
110000160311     c                   eval      v2cdesta=arbind
110100131114     c                   endif
110200131114     c
110300040217     C                   ENDIF
110400040217e   5C                   ENDIF
110500941014     C*
110600040217x   4C                   ELSE
110700941014     C*
110800911023     C* SELEZIONATA BOLLA: PASSO KEY ED ESCO DA PGM
110900040217     C     *IN61         IFEQ      *ON
111000940714     C                   Z-ADD     V2CLNP        COMLNP
111100940714     C                   MOVEL     '3'           V2CFLG
111200040220      *?- Richiamato da pgm. FIDNA1R?
111300150401if  6c                   if        SDSprm  > 1 and ilru6ra='S'
111400131111     c                   eval      wARBlnp  = V2Clnp
111500131111     c                   eval      wARBnrs  = V2Cnrs
111600131111     c                   eval      wARBnsp  = V2Cnsp
111700040220     c                   eval      wARBaas = V2Caas
111800150401     c* ????????????
111900150402     c* ????????????     clear                   FIDNA1ds
112000150401     c                   eval      ELRU6INS= 'S'
112100150401     c                   eval      OLRU6TOR= 'S'
112200150401     c                   eval      OLRU6OGG= ds_OggSped
112300040220e   6c                   endif
112400040217     C                   ENDIF
112500040217e   4C                   ENDIF
112600040217e   3C                   ENDIF
112700930518     C*
112800040217do  3C  N61              DO
112900940714     C                   MOVEL     *BLANKS       V2CSCE
113101161011     c                   if         %subst(v2ccon:1:7)='IN CONS'
113200930518     C                   SETON                                        13
113300161013     C                   endif
113301161013     c                   if        v2hi62='1'
113805161011     c                   seton                                        62
113806161013     c                   else
113807161013     c                   setoff                                       62
113900040217     C                   END
114100160311     C                   IF        dsp_mod = 1
114200160311     C                   UPDATE    LRI6S02A
114300160311     c                   else
114400131107     C                   UPDATE    LRI6S02
114500160311     c                   endif
114600040217e   3C                   ENDDO
114700930518     C*
114800040217e   2C                   ENDIF
114900911022     C*
115000941014     C                   SETOFF                                       136259
115100911022     C*
115200911022     C* SE FLAG 2 TORNO AL SUBFILE
115300040217     C     V2CFLG        IFNE      *BLANKS
115400900530     C                   SETON                                        34
115500900530     C                   ELSE
115600160311     C                   IF        dsp_mod = 1
115700160311     C                   READC     LRI6S02A                               34
115800160311     c                   else
115900131107     C                   READC     LRI6S02                                34
116000160311     c                   endif
116100040217     C                   ENDIF
116200040217e   1C                   ENDDO
116201160719     c                   endif
116300941014     C*
116400911023     C* FLG =  3 ==> FILE PGM E PASSO KEY
116500940714     C     V2CFLG        IFEQ      '3'
116600911023     C                   GOTO      FINE
116700911023     C                   ELSE
116800911023     C* FLG >< 2 ==> TORNO A SUBFILE
116900911023     C                   GOTO      FORCT2
117000040217     C                   ENDIF
117100911023     C*
117200000000     C     FINE          TAG
117300930702     C*
117400160314
117500941014     C     *IN97         IFEQ      *ON
117600941014     C                   MOVEL     'C'           D66TLA
117700991027     C                   MOVEL     FNLR66DS      KPJBU
117800941014     C                   CALL      'FNLR66R'
117900941014     C                   PARM                    KPJBA
118000040217     C                   ENDIF
118700930504     C*
118800941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
118900941109     C     *INKC         IFEQ      *ON
119000941109     C                   Z-ADD     *ZERO         V2CAAS
119100941109     C                   Z-ADD     *ZERO         COMLNP
119200941109     C                   Z-ADD     *ZERO         V2CNRS
119300941109     C                   Z-ADD     *ZERO         V2CNSP
119400941109     C                   MOVEL     '1'           PA1F03
119500941109     C                   ENDIF
119600980609     C**
119700941212     C* CHIUDO I PGM
119800941212     C* SOLO PER PGM DI MANUTENZIONE
119900941212     C     *IN97         IFEQ      *ON
120000941212     C                   MOVEL     'C'           D66TLA
120100941212     C                   CALL      'FNLR66R'
120200991027     C                   PARM                    FNLR66DS
120300060224     c
120400060224     c                   clear                   fnlv55ds
120500060224     C                   MOVEL     'C'           D55TLA
120600060227     C                   CALL      'FNLV55R'
120700060224     C                   PARM                    FNLv55DS
120800941212     C*
120900941212     C                   MOVEL     'C'           D48TLA
121000991027     C                   MOVEL     FNLR48DS      KPJBU
121100941212     C                   CALL      'FNLR48R'
121200941212     C                   PARM                    KPJBA
121300941212     C*
121400141105     c                   eval      ilry09tla= 'C'
121500141105     c                   eval      kpjbu=fnlry09ds2
121600141105     c                   call      'FNLRY09R'
121700141105     c                   parm                    kpjba
121800941212     C                   ENDIF
121900980603     C**
122100160523     C                   MOVEL     PARAMu6ds     KPJBU
122200941212     C*
122300040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
122400040810     c                   clear                   fidn12ds
122500040810     c                   eval      i12tla = 'C'
122600040810     c                   call      'FIDN12R'
122700040810     c                   parm                    fidn12ds
122701160519
122702160519     c* Richiamo il FNLRU6R1 in modalità "solo chiusura"
122703160523     c                   eval      pa1tla_1='C'
122704160606     c                   eval      kpjbu=paramu6ds1
122705160519     c                   call      'FNLRU6R1'
122706160519     c                   parm                    kpjba
122708160523     c                   eval      kpjbu=paramu6ds
124200141105
124300040810     c*
124400000000     C                   SETON                                        LR
124700940802     C**************************************************************************
124800940802     C*    CARICO TABELLE
124900940802     C**************************************************************************
125000911024     C     CARTAB        BEGSR
125100941212     C***
125200941212     C* CARICO TABELLA FILIALI GESTITE £1
125300941212     C***
125400950122     C                   CLEAR                   L1
125500991027     C                   CLEAR                   TRUL06DS
125600941212     C                   MOVE      '£1'          D06COD
125700941212     C                   MOVEL     SIMFEL        D06KEY
125800941212     C                   MOVEL     'L'           D06TLA
125900991027     C                   MOVEL     TRUL06DS      KPJBU
126000941212     C*
126100941212     C                   CALL      'TRUL06R'
126200941212     C                   PARM                    KPJBA
126300991027     C                   MOVEL     KPJBU         TRUL06DS
126400941212     C                   MOVEA     LIN           L1
126500930705     C*
126600930705     C* CARICO TABELLA 7O
126700930705     C                   MOVEL     '7O'          COD
126800941212     C                   Z-ADD     1             C                 3 0
126900940713     C     KTAB2         SETLL     TABEL
127000940713     C     KTAB2         READE     TABEL                                  30
127100930705     C*
127200930705     C     *IN30         DOWEQ     *OFF
127300930705     C     TBLFLG        IFEQ      ' '
127400930705     C                   MOVEL     TBLKEY        KCA(C)
127500930705     C                   MOVEL     TBLUNI        DCA(C)
127600930705     C                   ADD       1             C
127700930705     C                   END
127800940713     C     KTAB2         READE     TABEL                                  30
127900930705     C                   END
129900060130     C*
130000060130     C* CARICO IN SCHIERA LA TABELLA 3a per cod bolla doppi
130001160517     C                   Z-ADD     *ZERO         WA                3 0
130200060130     C                   CLEAR                   cbodop
130300060130     C                   MOVEL     '3A'          COD
130400060130     C     KTAB2         SETLL     TABEL00F
130500160518     C     KTAB2         READE     TABEL00F
130600160518     C                   DOW       not %eof(tabel00f)
130700060130     c                   movel     tbluni        ds3a
130800060130     c                   if        §3atb2<>*blanks
130900060130     C                   ADD       1             WA
131000060130     C                   MOVEL     TBLKEY        cbodop(wa)
131100060130     c                   endif
131200060130     c
131300160518     C     KTAB2         READE     TABEL00F
131400060130     C                   ENDDO
131500020329      *
134100940802     C*
134200911024     C                   ENDSR
134300940802     C**************************************************************************
134400940802     C*    PULIZIA CAMPI FORMATO 1
134500940802     C**************************************************************************
134600900514     C     PUL01         BEGSR
134700131114     c
134800131114     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
134900131114     C                   MOVEL     ' '           V1CSSP
135000161108     c****               if        pa1flg='2' or pa1flg='4' or pa1flg='M'
135100161108     c****               else
135200131114     C                   MOVEL     *ZEROS        V1CLNA
135300161108     c****               endif
135400131115     c                   clear                   v1cdea
135500131115     c                   clear                   v1cdep
135600131114     C                   MOVEL     *ZEROS        V1CLP1
135700131114     C                   MOVEL     *ZEROS        V1CDTD
135800131114     C                   MOVEL     *ZEROS        V1CDTA
135900131114     c                   clear                   v1cnzd
136000131114     c                   clear                   v1cprd
136100161104     c                   movel     'X'           v1cxind
136200131114     c                   clear                   v1cind
136300131114     c                   clear                   v1clod
136400161104     c                   movel     'X'           v1cxlod
136500131114     C                   MOVE      *BLANKS       V1CRSD
136600161104     C                   movel     'X'           V1CXRSD
136700131114     C                   MOVE      *BLANKS       V1CRSM
136800161104     C                   movel     'X'           V1CXRSM
136900131114     C                   MOVEL     *ZEROS        NDC
137000131114     C                   MOVEL     *ZEROS        NSS
137100940802     C*
137300060613     C                   CLEAR                   V1CRPE
137400010509     C                   CLEAR                   V1Cpoe
137500010509     C                   CLEAR                   V1Cnsr
137600010509     C                   CLEAR                   V1Cnor
137700010509     C                   CLEAR                   V1Cnrv
137701161116     C                   CLEAR                   V1Cnprr
137800110930     C                   CLEAR                   V1Crmn
137900110930     C                   CLEAR                   V1Crma
138000131115     C                   CLEAR                   V1Cdan
138100940713     C                   MOVEL     *ZEROS        V1CLP2
138200940713     C                   MOVEL     *ZEROS        V1CNRS
138300940713     C                   MOVEL     *ZEROS        V1CNSP
138400000103     C                   z-add     UYEAR         V1CAAS
138500000103     c     uyear         iflt      60
138600000103     c                   movel     20            v1caas
138700000103     c                   else
138800940713     C                   MOVEL     19            V1CAAS
138900000103     c                   endif
139000940713     C                   MOVEL     *BLANKS       V1CTSP
139100131114     C                   MOVEL     *BLANKS       V01DTSP
139200940713     C                   MOVEL     *BLANKS       V1CGIA
139300940713     C                   MOVEL     *ZEROS        V1CPSD
139400940713     C                   MOVEL     99            V1CPSA
139500940713     C                   MOVEL     *ZEROS        V1CSCA
139600940713     C                   MOVEL     *ZEROS        V1CSCD
139700940923     C                   MOVEL     *ZERO         V1CFLS
139800161013     C****               MOVE      'N'           V1CAFF
139900961112     C                   CLEAR                   V1CFBR
140000131111     C                   CLEAR                   V1Cbarcode
140100131111     C                   CLEAR                   V1CNOT
140200040216     c                   clear                   V1Ccbo
140300940713     C                   Z-ADD     0             NR2
140400070420     C****               MOVE      'N'           V1CSNP            1
140500070420     c                   clear                   v1csfd
140600150507     c****               clear                   v1cft1
140700150327     c                   clear                   v1cklp
140800150327     c                   clear                   v1cksc
140900150327     c                   clear                   v1crag
141000150327     c                   clear                   v1cem1
141100150327     c                   clear                   v1cem2
141200150327     c                   clear                   v1ctel
141201161013     c                   clear                   v1cref
141202161013     c                   clear                   w_v1ctel
141203161117     c                   clear                   v1cstco
141204161125     c                   clear                   v1cter
141300941212     C                   SETOFF                                       09
141400131111     c
141500131111     c* se cursore aperto --> lo chiudo
141600131209     c***                if        Wend=*on  or *in14 or *in11
141700131209     c                   if        WopenC='1'
141800131111     c                   exsr      closecursor
141900131111     c                   endif
142000150319     c                   if        WopenC2='1'
142100150319     c                   exsr      closecursorBlp
142200150319     c                   endif
142201170224     c                   if        WopenC3='1'
142202170224     c                   exsr      closecursorRMA
142203170224     c                   endif
142300150223     c* se provengo da gestojne r.a. mi posiziono su brtcode
142400161011     c                   if            SDSprm >  1  and  ilru6ra='S'
142500161011     c                             and ELRU6INS  = ' '
142600150223     c                   seton                                        60
142700161011     c                   endif
142800150401     c*
142900150401     c* imposto campi del video come da parametri ricevuti se passati
143000150401     c                   if            SDSprm >  1
143100150402     c                   eval          v1cbarcode=ILRU6BRTCO
143200150402     c                   eval          v1cssp=ILRU6SSP
143300150402     c                   eval          v1clna=ILRU6LNA
143302161109     C     ilru6lna      CHAIN     AZORG01L
143303161109     c                   if        %found(azorg01l)
143304161109     C                   MOVEL     ORGDES        V1CDEA
143305161109e   1C                   endif
143400150402     c                   eval          v1cnzd=ILRU6NZD
143500150402     c                   eval          v1cprd=ILRU6PRD
143600150402     c                   eval          v1crsd=ILRU6RSD
143700150402     c                   eval          v1cxrsd=ILRU6XRSD
143701161117     c                   select
143702161117     c                   when          v1cxrsd=*blanks
143704161104     c                   eval          v1cxrsd='X'
143705161117     c                   when          v1cxrsd='N'
143706161117     c                   clear                   v1cxrsd
143707161117     c                   endsl
143800150402     c                   eval          v1cind =ILRU6IND
143900150402     c                   eval          v1cxind=ILRU6XIND
143901161117     c                   select
143902161117     c                   when          v1cxind=*blanks
143904161104     c                   eval          v1cxind='X'
143905161117     c                   when          v1cxind='N'
143906161117     c                   clear                   v1cxind
143907161117     c                   endsl
144000150402     c                   eval          v1clod =ILRU6LOD
144100150402     c                   eval          v1cxlod=ILRU6XLOD
144101161117     c                   select
144102161117     c                   when          v1cxlod=*blanks
144103161104     c                   eval          v1cxlod='X'
144104161117     c                   when          v1cxlod='N'
144105161117     c                   clear                   v1cxlod
144106161117     c                   endsl
144200150402     c                   eval          v1crsm =ILRU6RSM
144300150402     c                   eval          v1cxrsm=ILRU6XRSM
144301161117     c                   select
144302161117     c                   when          v1cxrsm=*blanks
144303161104     c                   eval          v1cxrsm='X'
144304161117     c                   when          v1cxrsm='N'
144305161117     c                   clear                   v1cxrsm
144306161117     c                   endsl
144400150402     c                   eval          v1clp1 =ILRU6LP1
144401160719     c                   clear                   v1cdtd
144402160719     c                   clear                   v1cdta
144601160719     c                   if            ilru6dtd>0
144602160719     c                   eval          dataeur=%date(ilru6dtd:*iso)
144603160719     c                   eval          v1cdtd=%dec( dataeur )
144604160719     c                   endif
144605160719     c                   if            ilru6dta>0
144606160719     c                   eval          dataeur=%date(ilru6dta:*iso)
144607160719     c                   eval          v1cdta=%dec( dataeur )
144608160719     c                   endif
144700150402     c                   eval          v1cnot =ILRU6NOT
144701161013     c                   eval          v1cxnot=ILRU6Xnot
144800150402     c                   eval          v1crpe =ILRU6RPE
144900150402     c                   eval          v1cpoe =ILRU6POE
145000150402     c                   eval          v1cnsr =ILRU6NSR
145100150402     c                   eval          v1cnor =ILRU6NOR
145200150402     c                   eval          v1cnrv =ILRU6NRV
145201161117     c                   eval          v1cnprr=ILRU6NPRR
145300150402     c                   eval          v1crmn =ILRU6RMN
145400150402     c                   eval          v1crma =ILRU6RMA
145401161103     c                   if        ilru6psd<>*blanks
145402161103     c                   monitor
145500161103     c                   move      ILRU6PSD      v1cpsd
145501161103     c                   on-error
145502161103     C                   MOVEL     *ZEROS        V1CPSD
145503161103     c                   endmon
145504161103     c                   endif
145505161103     c                   if        ilru6psa<>*blanks
145506161103     c                   monitor
145600161103     c                   move      ILRU6PSA      v1cpsa
145601161103     c                   on-error
145602161103     C                   MOVEL     99            V1CPSA
145603161103     c                   endmon
145604161103     c                   endif
145700150402     c                   eval          v1csfd =ILRU6SFD
145800150402     c                   eval          v1cscd =ILRU6SCD
145900150402     c                   eval          v1csca =ILRU6SCA
146000150402     c                   eval          v1cfls =ILRU6FLS
146100150402     c                   eval          v1ctsp =ILRU6TSP
146200150402     c                   eval          v1cdan =ILRU6DAN
146300150402     c* Affidabilità S/N
146400150402     c* se non passato mantengo il default a "N"  sopra impostato
146500161013     c***                if            ilru6aff<>*blanks
146600161013     c***                eval          v1caff =ILRU6AFF
146700161013     c***                endif
146800150402     c                   eval          v1cfbr =ILRU6FBR
146900150402     c                   eval          v1ccbo =ILRU6CBO
147000150402     c                   eval          v1cgia =ILRU6GIA
147001161117     c* Stato consegna lo riempio solo se stato spedizone=blank
147002161117     c                   if            v1cssp=*blanks
147003161117     c                   eval          v1cstco=ILRU6stco
147004161117     c                   endif
147100150402     c                   eval          w0070  =ILRU6CCM
147200150402     c                   movel     w0070         v1cklp
147300150402     c                   move      w0070         v1cksc
147301161117     c                   if        v1cksc=0
147302161117     c                   clear                   v1cklp
147303161117     c                   endif
147400150402     c                   eval          v1crag =ILRU6RAG
147500150402     c                   eval          v1ceml =ILRU6EML
147600150402     c                   eval          v1ctel =ILRU6TEL
147601161013     c                   eval          v1cref =ILRU6ref
147602161125     c                   if            ilru6timp = 'X'
147603161125     c                   eval          v1cter ='T'
147604161125     c                   endif
147605180308     c* imposto in dicatore per evidenziare che sono presenti parametri nela videata
147606180308     c* "AltreParz"
147607180308     c                   exsr      sr_IndAP
147700150401     c                   endif
147800930705     C*
147900900514     C                   ENDSR
147901161104     C**************************************************************************
147902161104     C*    Spegnimento indicatori di errore D01
147903161104     C**************************************************************************
147904161104     C     sr_InzId1     BEGSR
147905161104     C                   SETOFF                                       900102
147906161104     C                   SETOFF                                       140784
147907161104     C                   SETOFF                                           11
147908161104     C                   SETOFF                                       154046
147909161104     C                   SETOFF                                       414243
147910161104     C                   SETOFF                                       444548
147911161104     C                   SETOFF                                       495152
147912161104     C                   SETOFF                                       552547
147913161116     C                   SETOFF                                           60
147914161104     c                   setoff                                       575058
147915161116     c                   setoff                                       686226
147916161117     c                   setoff                                       6967
147917161104     c                   clear                   h1riga
147918161104     c                   clear                   h1colo
147919161104     c                   endsr
148000940802     C**************************************************************************
148100940802     C*    CONTROLLI FORMATO1
148200940802     C**************************************************************************
148300900509     C     CONTR         BEGSR
148400940317      *
148500940713     C                   MOVE      *BLANKS       V1CDEA
148600940713     C                   MOVE      *BLANKS       V1CDEP
148700110930     c                   movel     v1crma        comrma
148701161116     c                   clear                   wel4
148702161123     c                   clear                   v1cdtd_w
148703161123     c                   clear                   v1cdta_w
148800020709
148900131121     c******             clear                   v2dlna
149000940317      *
150301161104     c                   exsr      sr_InzId1
150400950121     C*
150500950121     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
150600950121     C     V1CAAS        IFLT      100
150700950121     C     V1CAAS        IFGT      60
150800950121     C                   ADD       1900          V1CAAS
150900950121     C                   ELSE
151000950121     C                   ADD       2000          V1CAAS
151100950121     C                   ENDIF
151200950121     C                   ENDIF
151300950121     C*
151400950121     C* SE VARIAZIONE CONTROLLO CAL CAUSALE
151500950121     C   97              DO
151600991027     C                   CLEAR                   FNLR66DS
151700950121     C     '?'           SCAN      V1CCVB                                 90
151800950121     C* RICERCA
151900950121    1C     *IN90         IFEQ      *ON
152000991027     C                   CLEAR                   FNLR66DS
152100950121     C                   MOVEL     'V'           D66GES
152200950121     C                   MOVEL     'A'           D66TBO
152300950121     C*
152400950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
152500950121    2C     V1CNSP        IFGT      0
152600950121     C                   MOVEL     V1CNSP        D66NSP
152700950121     C                   MOVEL     V1CAAS        D66AAS
152800950121     C                   MOVEL     V1CLP2        D66LNP
152900950121     C                   MOVEL     V1CNRS        D66NRS
153000950121    2C                   ENDIF
153100950121     C*
153200160311     c                   if        %subst(knmus:1:3)='EDP'
153300160127     c                   eval      d66bnp='S'
153400160311     c                   endif
153500991027     C                   MOVEL     FNLR66DS      KPJBU
153600950121     C                   CALL      'FNLR66R'
153700950121     C                   PARM                    KPJBA
153800991027     C                   MOVEL     KPJBU         FNLR66DS
153900950121     C                   MOVEL     D66CVB        V1CCVB
154000950121     C                   MOVEL     D66DES        V1DCVB
154100950121     C                   MOVEL     'S'           WRIEM
154200950203     C                   SETON                                        4790
154300950121     C*
154400950121     C                   GOTO      ENDCTR
154500950121   X1C                   ELSE
154600950121     C* CONTROLLO
154700991027     C                   CLEAR                   FNLR66DS
154800950121     C                   MOVEL     'C'           D66GES
154900950121     C                   MOVEL     'A'           D66TBO
155000950121     C                   MOVEL     V1CCVB        D66CVB
155100950121    2C     V1CNSP        IFGT      0
155200950121     C                   MOVEL     V1CNSP        D66NSP
155300950121     C                   MOVEL     V1CAAS        D66AAS
155400950121     C                   MOVEL     V1CLP2        D66LNP
155500950121     C                   MOVEL     V1CNRS        D66NRS
155600950121    2C                   ENDIF
155700950121     C*
155800160311     c                   if        %subst(knmus:1:3)='EDP'
155900160127     c                   eval      d66bnp='S'
156000160311     c                   endif
156100991027     C                   MOVEL     FNLR66DS      KPJBU
156200950121     C                   CALL      'FNLR66R'
156300950121     C                   PARM                    KPJBA
156400991027     C                   MOVEL     KPJBU         FNLR66DS
156500950121     C                   MOVEL     D66CVB        V1CCVB
156600950121     C                   MOVEL     D66DES        V1DCVB
156700950121     C*
156800950121     C* ERRORE
156900950121    2C     D66MSG        IFNE      *BLANKS
157000950121     C                   MOVEL     D66MSG        V1CMSG
157100950121     C                   SETON                                        284790
157200950121     C                   GOTO      ENDCTR
157300950121     C*
157400950121   X2C                   ELSE
157500950121     C* DECODIFICA
157600950121     C                   MOVEL     D66DES        V1DCVB
157700950121    2C                   ENDIF
157800950121    1C                   ENDIF
157900950121     C                   ENDDO
158000950102     C*
158100131113     C* immettere almeno una selezione
158200161013     C*    SEL1          IFEQ      0
158300161013     C*    SEL2          ANDEQ     0
158400161013     C*    SEL3          ANDEQ     *BLANKS
158500161013     C*    SEL4          ANDEQ     *zeros
158600131113     c
158700161013     C*    V1CRPE        ANDEQ     *BLANKS
158800161013     c*    v1crmn        andeq     *zeros
158900161013     c*    v1crma        andeq     *blanks
159000161013     c*    v1cnot        andeq     *blanks
159100161013     c*    v1cbarcode    andeq     *zeros
159101161013     c                   if        sel1=0 and
159102161013     c                             sel2=0 and
159103161014     c                             sel3=*blanks and
159104161014     c                             sel4=*zeros and
159105161116     c                             v1cnprr=*blanks and
159106161013     c                             v1crpe=*blanks and
159107161013     c                             v1crmn=*zeros  and
159108161013     c                             v1crma=*blanks and
159109161013     c                             (v1cnot=*blanks or v1cxnot='X') and
159110161013     c                             v1cbarcode= *zeros
159200940713     C                   MOVEL     MSG(3)        V1CMSG
159300940713     C                   SETON                                        412890
159400901115     C                   GOTO      ENDCTR
159500901115     C                   END
159600940318      *
159700901115     C* DATA SPEDIZIONE
159800900514     C* LINEA ARRIVO
159900131113      * per destinatario o mittente sono obbligatori
160000020627      * la nazione e la provincia del destinatario
160100131113    1C     SEL2          IFEQ      0
160200131113    2c     sel4          ifeq      0
160201161116    2c     v1cnprr       andeq     *blanks
160300020225     c                   clear                   lnantw
160400131113    3C     V1CLNA        IFNE      0
160500940713     C                   MOVEL     V1CLNA        NUM3
160600940713     C     NUM3          CHAIN     AZORG01L                           41
160700131113    4C   41              DO
160800940713     C                   MOVEL     MSG(2)        V1CMSG
160900940713     C                   SETON                                        28  90
161000940713     C                   GOTO      ENDCTR
161100131113    4C                   ENDDO
161200940713     C                   MOVEL     ORGDES        V1CDEA
161300131121     C*******            MOVEL     ORGDES        V2DLNA
161400991222     C                   MOVEL     ORGDE3        og143
161500020225     c                   eval      lnantw = §ogntw
161600900514     C                   SETON                                        01
161700131113    3C                   END
161800131113    2C                   END
161900131113    c
162000020626      * selezione per destinatario o mittente
162100020626     c                   if        sel3 <> *blanks
162200020628     c                   exsr      ContrDatiDes
162300020628     c                   if        *in90 = *on                                  ricerca
162400020628     c                   goto      endctr
162500020628     c                   endif
162600020628     c                   if        *in28 = *on                                  errore
162700020628     c                   eval      *in90 = *on
162800020628     c                   goto      endctr
162900020628     c                   endif
163000020626     c                   endif
163100901115     C* LINEA PARTENZA
163200131111     c                   clear                   lnpntw
163300940713     C     V1CLP1        IFNE      0
163301161115     c     v1clp1        ifne      999
163400940713     C                   MOVEL     V1CLP1        NUM3
163500940713     C     NUM3          CHAIN     AZORG01L                           42
163600940713     C   42              DO
163700940713     C                   MOVEL     MSG(5)        V1CMSG
163800940713     C                   SETON                                        28  90
163900940713     C                   GOTO      ENDCTR
164000940713     C                   ENDDO
164100940713     C                   MOVEL     ORGDES        V1CDEP
164200131121     C****               MOVEL     ORGDES        V2DLNP
164300000703     C                   movel     orgde3        og143
164400050304     c                   eval      lnpntw = §ogntw
164401161115     c                   endif
164500901115     C                   SETON                                        02
164600901115     C                   END
164700940413      *
164800971223      * E' OBBLIGATORIA LNA O LNP
164900161013     C*    V1CLNA        IFEQ      *ZERO
165000161013     C*    V1CLP1        ANDEQ     *ZERO
165100161013     C*    V1CRPE        ANDEQ     *BLANKS
165200161013     c*    sel4          andeq     0
165300161013     c*    sel3          andeq     *blanks
165400161013     c*    v1crmn        andeq     *zeros
165500161013     c*    v1crma        andeq     *blanks
165600161013     c*    v1cnot        andeq     *blanks
165700161013     c*    v1cbarcode    andeq     *zeros
165701161013     C                   if        V1CLNA=*ZERO and
165702161013     C                             V1CLP1=*ZERO and
165703161013     C                             V1CRPE=*BLANKS  and
165704161013     c                             sel4=0 and
165705161116     c                             v1cnprr=*blanks and
165706161013     c                             sel3=*blanks and
165707161013     c                             v1crmn=*zeros and
165708161013     c                             v1crma=*blanks  and
165709161013     c                             (v1cnot=*blanks  or v1cxnot='X')  and
165710161013     c                             v1cbarcode=*zeros
165800161021     C***                MOVEL     MSG(4)        V1CMSG
165801161021     C                   MOVEL     MSG(3)        V1CMSG
165900940713     C                   SETON                                        412890
166000940712     C                   GOTO      ENDCTR
166100940712     C                   ENDIF
166200940413     C*
166300911023     C* DATA DAL OBBLIGATORIA
167001161013     C                   if        V1CDTD=0 and
167002161013     C                             v1CRPE=*BLANKS and
167003161013     C                             sel4=0 and
167004161116     C                             v1cnprr=*blanks and
167005161013     C                             v1crmn=0 and
167006161013     C                             v1crma=*blanks and
167007161013     C                             (v1cnot=*blanks or v1cxnot='X') and
167008161013     c                             v1cbarcode=*zeros
167100131113     c
167200131113     c*********          if        v1cssp <> *blanks or
167300131113     c
167400161125    3c                   if        (v1crsd = *blanks and v1crsm = *blanks and
167500131111     c                              v1cind = *blanks and v1clod = *blanks and
167600131111     c                             sel3 <> *blanks and *in01) or
167601161125     c                             (v1crsd <> *blanks and v1cprd= *blanks and
167602161125     c                              v1cnzd = *blanks) or
167700131111     c                               sel3 = *blanks
167701161123     c* Se Data obbligatoria, anzichè dare errore preimposto il range
167702161123     c* fisso  01/01/2000 - 31/12/2039
167703161125     c* In manutenzione continuo a dare errore
167704161125    4c                   if        v1cdta=0 and not *in97
167708161128     c***                          and (sel3 = *blanks or
167709161128     c***                               (v1crsd=*blanks and v1crsm=*blanks and
167710161128     c***                                v1cind=*blanks and v1clod=*blanks and
167711161128     c***                                *in01) or
167712161128     c***                               (v1cprd=*blanks and v1cnzd=*blanks and
167713161128     c***                               v1crsd<>*blanks and v1cxrsd<>'X'))
167714161123     c                   eval      v1cdtd_W=20000101
167715161123     c                   eval      v1cdta_W=20391231
167716161125   x4c                   else
167717161125     c* No impostazione data dal/al automatica se:
167718161125     c* presente "data al" OPPURE
167719161128     c* in manutenzione
167800161123     C                   MOVEL     MSG(8)        V1CMSG
167900161123     C                   SETON                                        285190
167901161128    5c***                if        v1cprd=*blanks and v1cnzd=*blanks and
167902161128     c***                          v1crsd<>*blanks
167903161128     c***                eval      v1cmsg=%trim(v1cmsg) +
167904161128     c***                          ' OPPURE Naz/Prov Destinatario'
167905161128    5c***                endif
168000161123     C                   GOTO      ENDCTR
168006161125    4c                   endif
168100940712     C                   ENDIF
168200911023     C                   END
168300911023     C*
168400940712     C                   Z-ADD     *ZERO         COMDTD
168500940712     C                   Z-ADD     *ZERO         COMAAD
168600940712     C                   Z-ADD     *ZERO         COMMGD
168700940713     C     V1CDTD        IFNE      *ZERO
168800940713     C                   MOVE      V1CDTD        G02DAT
168900930705     C                   MOVEL     *BLANK        G02ERR
169000940713     C                   CALL      'XSRDA8'
169100930705     C                   PARM                    WLBDAT
169200930705     C     G02ERR        IFEQ      '1'
169300940713     C                   MOVEL     MSG(8)        V1CMSG
169400940713     C                   SETON                                        285190
169500930705     C                   GOTO      ENDCTR
169600930705     C                   ELSE
169700940713     C                   Z-ADD     G02DAT        V1CDTD
169800940713     C                   MOVE      G02INV        COMDTD            8 0
169900940713     C                   MOVEL     G02INV        COMAAD            4 0
170000930705     C                   MOVE      G02INV        COMMGD            4 0
170100930705     C                   END
170200940712     C                   ENDIF
170300900515     C* DATA SPEDIZIONE AL
170400940712     C                   Z-ADD     *ZERO         COMDTA
170500940713     C     V1CDTA        IFNE      0
170600940713     C                   MOVE      V1CDTA        G02DAT
170700930705     C                   MOVEL     *BLANK        G02ERR
170800940713     C                   CALL      'XSRDA8'
170900930705     C                   PARM                    WLBDAT
171000930705     C     G02ERR        IFEQ      '1'
171100941215     C                   MOVEL     MSG(8)        V1CMSG
171200940713     C                   SETON                                        285290
171300930705     C                   GOTO      ENDCTR
171400930705     C                   ELSE
171500940713     C                   Z-ADD     G02DAT        V1CDTA
171600940713     C                   MOVE      G02INV        COMDTA            8 0
171700900515     C                   END
171800930705     C*
171900900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
172000911023     C     COMDTA        IFLT      COMDTD
172100940713     C                   MOVEL     MSG(10)       V1CMSG
172200940713     C                   SETON                                        285152
172300940713     C                   SETON                                        90
172400900515     C                   GOTO      ENDCTR
172500900515     C                   END
172600940318      *
172700940712     C                   ELSE
172800940712      *
172900940712      * SE NON IMMESSA LA DATA FINALE, LA PONGO UGUALE A QUELLA INIZIALE
173000940712     C                   Z-ADD     COMDTD        COMDTA
173100940318      *
173200900515     C                   END
173300940318      *
173400940712      * VERIFICO SE LE DUE DATE SONO COINCIDENTI
173500131115     c                   if        comdtd>0
173600940712     C     COMDTD        COMP      COMDTA                                 15
173700161123     c                   endif
173702161123     c* se non presenti le date a video utilizzo quelle impostate in automatico
173703161123     c* quando necessario
173704161123     c                   if        v1cdtd_w>0
173705161123     c                   z-add     v1cdtd_w      comdtd
173706161123     c                   movel     v1cdtd_w      comaad
173707161123     c                   move      v1cdtd_w      commgd
173708161123     c                   z-add     v1cdta_w      comdta
173709161123     c                   endif
173800901115     C***
173900911024     C* GIACENTI
174000911024     C* SCELTE INCONGRUENTI
174100940713     C     V1CGIA        IFEQ      'G'
174200940713     C     V1CSSP        IFEQ      '2'
174300940713     C     V1CSSP        OREQ      '3'
174400150508     C     V1CSSP        OREQ      'N'
174500940713     C                   MOVEL     MSG(14)       V1CMSG
174600940713     C                   SETON                                        285690
174700911024     C                   GOTO      ENDCTR
174800911024     C                   END
174900911024     C                   END
174901161117     C* Stato consegna C=Consegnate N=non consegnate
174902161117     c* Errore se non richiesta la visualizzazione di TUTTE le sped.
174903161117     C* SCELTE INCONGRUENTI
174904161117     C     V1Cstco       IFne      *blanks
174905161117     C     V1CSSP        andne     *blanks
174906161117     C                   MOVEL     MSG(14)       v1cMSG
174907161117     C                   SETON                                        286790
174908161117     C                   GOTO      ENDCTR
174909161117     C                   END
175000150331     c* Bolle partite
175100150331     c* scelte incongruenti
175200150507     c****               if        v1cft1='N' and v1cssp<>' '
175300150507     C****               MOVEL     MSG(14)       v1cmsg
175400150507     C****               SETON                                        286990
175500150507     C****               GOTO      endctr
175600150507     c****               endif
175700901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
175800901115     C*  SE IMMESSO
175900940713     C     V1CPSD        IFGT      V1CPSA
176000940713     C                   MOVEL     MSG(11)       V1CMSG
176100940713     C                   SETON                                        284890
176200901115     C                   GOTO      ENDCTR
176300901115     C                   END
176400901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
176500940713     C     V1CSCA        IFGT      0
176600940713     C     V1CSCD        ANDGT     V1CSCA
176700940713     C                   MOVEL     MSG(12)       V1CMSG
176800940713     C                   SETON                                        284990
176900901115     C                   GOTO      ENDCTR
177000901115     C                   END
177100901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
177200940713     C     V1CSCA        IFGT      0
177300940713     C     V1CSCD        ANDEQ     0
177400940713     C                   MOVEL     MSG(13)       V1CMSG
177500940713     C                   SETON                                        284990
177600901115     C                   GOTO      ENDCTR
177700901115     C                   END
177800940713     C     V1CSCD        COMP      0                                  07
177900970925     C     V1CSCA        COMP      0                                  84
178000940923     C*
178100940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
178200940923     C     V1CFLS        IFNE      *ZERO
178300940923     C     V1CFLS        CHAIN     AZORG01L                           46
178400941014     C     *IN46         IFEQ      *ON
178500941014     C     *IN46         OREQ      *OFF
178600941014     C     ORGFVA        ANDNE     *BLANKS
178700941014     C                   SETON                                        284690
178800941014     C                   MOVEL     MSG(17)       V1CMSG
178900941014     C                   GOTO      ENDCTR
179000940923     C                   ENDIF
179100941014     C                   ENDIF
179200940923     C*
179300901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
179400940713     C     V1CRSM        IFNE      *BLANKS
179500901115     C                   SETON                                        11
179600940713     C                   MOVEA     V1CRSM        C1
179700131111     C                   Z-ADD     30            XX                2 0
179800930702     C     C1(XX)        DOWEQ     *BLANKS
179900930702     C                   SUB       1             XX
180000901116     C                   END
180100901116     C                   END
180200901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
180300940713     C     V1CRSD        IFNE      *BLANKS
180400901115     C                   SETON                                        14
180500940713     C                   MOVEA     V1CRSD        C1
180600131111     C                   Z-ADD     30            ZZ                2 0
180700930702     C     C1(ZZ)        DOWEQ     *BLANKS
180800930702     C                   SUB       1             ZZ
180900901116     C                   END
181000901115     C                   END
181100131113     C* località DESTINATARIO: CERCO PRIMO BLANKS
181200131113     C     V1Clod        IFNE      *BLANKS
181300131113     C                   SETON                                        14
181400131113     C                   MOVEA     V1Clod        C1
181500131113     C                   Z-ADD     30            LL                2 0
181600131113     C     C1(LL)        DOWEQ     *BLANKS
181700131113     C                   SUB       1             LL
181800131113     C                   END
181900131113     C                   END
182000131113     C* Indirizzo DESTINATARIO: CERCO PRIMO BLANKS
182100131113     C     V1Cind        IFNE      *BLANKS
182200131113     C                   SETON                                        14
182300131113     C                   MOVEA     V1Cind        C1
182400131113     C                   Z-ADD     30            DD                2 0
182500131113     C     C1(DD)        DOWEQ     *BLANKS
182600131113     C                   SUB       1             DD
182700131113     C                   END
182800131113     C                   END
182900131113     c
183000131113     c* 14 on anche per indirizzo o località  DESTINAT
183100131113     c                   if        not *in11 and
183200131113     c                              v1clod<>*blanks or v1cind<>*blanks
183300131113     c                   seton                                        14
183400131113     c                   endif
183500131113     c
183600131113      * Nazione provincia destinatario senza ragioni sociali
183700060613      * imposto per destinatario se non è stata impostata la lna  e la linea
183800060613      * partenza
183900060613     c                   if        v1cprd <> *blanks and
184000020628     c                             not *in11 and not *in14 and not *in01
184100060613     c                   eval      *in14 = *on
184200020628     c                   endif
184300060613     c                   if         v1cnzd <> *blanks and v1cprd = *blanks and
184400060613     c                             not *in11 and not *in14 and not *in01
184500060613     c                             and not *in02
184600060613     c                   eval      *in14 = *on
184700060613     c                   endif
184800901115     C                   END
184900901115     C*
185000901115     C** NUMERO SPEDIZIONE
185100901115     C** LINEA PARTENZA
185200140117     C***  SEL2          IFNE      0
185300140117     c                   if        sel2<> 0 and v1cbarcode<=0
185400940713     C                   Z-ADD     V1CLP2        NUM3
185500940713     C     NUM3          CHAIN     AZORG01L                           45
185600940713     C   45              DO
185700940713     C                   MOVEL     MSG(5)        V1CMSG
185800940713     C                   SETON                                        28  90
185900940713     C                   GOTO      ENDCTR
186000940713     C                   ENDDO
186100911023     C* ANNO SPEDIZIONE
186200940713     C     V1CAAS        IFEQ      0
186300940713     C                   MOVEL     MSG(7)        V1CMSG
186400940713     C                   SETON                                        285590
186500911023     C                   GOTO      ENDCTR
186600911023     C                   END
186700150507     c                   clear                   fnarbds
186800150507     c                   exsr      sr_inzsoloblp
186900150507     C     KLNPX         CHAIN     FNblp01L
187000150423     C     KLNPX         CHAIN     FNARB01L
187100030527     C*
187200150423     C                   if        not %found(fnblp01l) and not %found(fnarb01l)
187300940713     C                   MOVEL     MSG(6)        V1CMSG
187400160922     C                   SETON                                        284490
187500940713     C                   GOTO      ENDCTR
187600150423     C                   ENDif
187700160419      *manutenzione
187800160419     c                   if        *in97
187900160419     c                   if        (d66utp=*blanks and not %found(fnarb01l)) or
188000160419     c                             (d66utp='P' and not %found(fnblp01l))
188100160419     C                   MOVEL     MSG(6)        V1CMSG
188200160922     C                   SETON                                        284490
188300160419     C                   GOTO      ENDCTR
188400160419     c                   endif
188500160419     c                   endif
188600901115     C*
188700901115     C                   END
188800131111     c***
188900131113     C* B A R C O D E   L D V
189000131111     c***
189100131111     c                   if        v1cbarcode>0
189200150507     c                   clear                   fnarbds
189300150507     c                   exsr      sr_inzsoloblp
189400131111     c                   z-add     b_aas         arbaas
189500131111     C     b_aas         IFLT      100
189600131111     C     b_aas         IFGT      60
189700131111     C                   ADD       1900          arbAAS
189800131111     C                   ELSE
189900131111     C                   ADD       2000          arbAAS
190000131111     C                   ENDIF
190100131111     C                   ENDIF
190200131111     c                   z-add     b_lnp         arblnp
190300131111     c                   z-add     b_nrs         arbnrs
190400131111     c                   z-add     b_nsp         arbnsp
190500150507     C     Karb2         CHAIN     FNblp01L
190600131114     C     Karb2         CHAIN     FNARB01L
190700131111     C*
190800150423     C                   if        not%found(fnarb01l) and not %found(fnblp01l)
190900131111     C                   MOVEL     MSG(32)       V1CMSG
191000131111     C                   SETON                                        286090
191100131111     C                   GOTO      ENDCTR
191200131111     C                   endif
191300160419      *manutenzione
191400160419     c                   if        *in97
191500160419     c                   if        (d66utp=*blanks and not %found(fnarb01l)) or
191600160419     c                             (d66utp='P' and not %found(fnblp01l))
191700160419     C                   MOVEL     MSG(32)       V1CMSG
191800160419     C                   SETON                                        286090
191900160419     C                   GOTO      ENDCTR
192000160419     c                   endif
192100160419     c                   endif
192200131111     C                   endif
192201161116      ***
192202161116      * N U M E R O   P R E N O T A Z I O N E    R I T I R O
192203161116      ***
192204161116     c     v1cnprr       ifne      *blanks
192206161116     C     sel2          andeq     0
192207161116     c                   exsr      NumeroNPR
192208161116     c                   if        *in28
192209161116     c                   goto      endctr
192210161116     c                   endif
192211161116     C                   MOVEL     'M'           WTRC
192213161116     c                   eval      wel4=%editc(FIORC0O0.poe:'X')+
192214161116     c                                  %editc(FIORC0O0.nsr:'X')+
192215161116     c                                  %editc(FIORC0O0.nor:'X')+
192216161116     c                                  %editc(FIORC0O0.nrv:'X')
192217161116     C                   MOVEL(P)  wEL4          WN14             14
192218161116     C     kar44         chain     fiar404l                           69
192219161116     C     *in69         ifeq      *on
192220161116     C                   movel     msg(20)       v1cmsg
192221161116     C                   seton                                        28  90
192222161116     C                   goto      endctr
192223161116     C                   endif
192226161116      *
192227161116     C                   seton                                        26
192228161116      *
192229161116     C                   endif
192300010508      ***
192400010508      * N U M E R O   O R M
192500010508      ***
192600010508     C     sel4          ifne      0
192700010508     C     sel2          andeq     0
192701161116     C     v1cnprr       andeq     *blanks
194500010508      *
194600010508     C                   seton                                        25
194700010508      *
194800010508     C                   endif
194900131111     c
195000901115     C*
195100911023     C* TIPO SPEDIZIONE ESPRESSO
195200131114     C                   MOVEL     *BLANKS       V01DTSP
195300940713     C     V1CTSP        IFNE      ' '
195400940715     C     V1CTSP        IFEQ      '?'
195500940715     C                   CALL      'X§TABER'
195600040217     C                   PARM      CodUt         §KUT              1 0
195700040217     C                   PARM      '5E'          COD
195800040217     C                   PARM      *blanks       KEY
195900040217     C                   PARM      *blanks       §DES             30
196000940715     C                   MOVEL     KEY           V1CTSP
196100940715     C                   SETON                                        90
196200940715     C                   MOVEL     'S'           WRIEM
196300940715     C                   ENDIF
196400940715     C*
196500090604     c                   clear                   ds5e
196600940715     C                   MOVEL     '5E'          COD
196700940715     C                   MOVEL     *BLANKS       KEY
196800940715     C                   MOVEL     V1CTSP        KEY
196900940715     C     KTAB1         CHAIN     TABEL00F                           43
197000941014     C     *IN43         IFEQ      *ON
197100941014     C     *IN43         OREQ      *OFF
197200950221     C     TBLFLG        ANDNE     ' '
197300940715     C                   MOVEL     MSG(16)       V1CMSG
197400941014     C                   SETON                                        284390
197500940715     C                   GOTO      ENDCTR
197600941014     C                   ELSE
197700090604     C                   MOVEL     TBLUNI        ds5e
197800131114     C                   MOVEL     §5ed08        V01DTSP
197900991222     C                   ENDIF
198000940715     C                   ENDIF
198100040216      *
198200991222     c*
198300991222     c* Se richiesta linea di arrivo dpd il riferimemnto immesso deve
198400060613     c*  essere composto soltanto da numeri se <=11
198500060613     C*  alfanumerico       se=14
198600050121     c                   clear                   wnodpd            1
198700060613    1c                   if        lnantw = 'DPD' or lnpntw = 'DPD' or
198800050304     c                             lnpntw = *blanks or lnantw = *blanks
198900060613    2c                   if        v1crpe<>*blanks
199000060613    3c                   select
199100060613     c* ok se lungo 14
199200060613     c                   when      %subst(v1crpe:15:1)=' '  and
199300060613     c                             %subst(v1crpe:14:1)<>' '
199400060613     c* ok se lungo 11 ma tutto numerico
199500060613     c                   when      %subst(v1crpe:12:4)=*blanks and
199600060613     c                             %subst(v1crpe:11:1)<>' '
199700060613     C                   MOVEA     V1CRPe        K11
199800060613     C                   Z-ADD     1             YY                2 0
199900060613     c
200000060613    4C                   DO        11            YY
200100060613     C     K11(YY)       LOOKUP    CNM                                    30
200200060613    5C     *IN30         IFEQ      *OFF
200300060613     c* Se non è tutto numerico non è DPD
200400060613     c                   eval      wnodpd = '1'
200500060613     c                   leave
200600060613    5c                   endif
200700060613    4c                   enddo
200800060613   x3c                   other
200900060613     c
201000060613     c* se non è lungo 11 o 14 non è DPD
201100060613     c                   eval      wnodpd = '1'
201200060613    3c                   endsl
201300060613     c
201400060613    3c                   if        wnodpd='1'
201500060613    4c     lnantw        ifeq      'DPD'
201600060613     c     lnpntw        oreq      'DPD'
201700060613     C                   MOVEL     MSG(18)       V1CMSG
201800060613     C                   SETON                                        285890
201900060613     C                   GOTO      ENDCTR
202000060613    4C                   ENDIF
202100060613    3C                   ENDIF
202200060613    2c                   endif
202300061117   x1c                   else
202400061117     c                   eval      wnodpd = '1'
202500060613    1c                   endif
202600941014     C*
202700911023     C** FILE STORICO
202800940713     C     V1CSSP        COMP      '1'                                    18
202900940713     C     V1CSSP        COMP      '2'                                    19
203000940713     C     V1CSSP        COMP      '3'                                    20
203100940713     C     V1CSSP        COMP      ' '                                    21
203200150508     c  n21v1cssp        comp      'N'                                    21
203300920911     C*
203400940712      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
203500161116      *    1°    NUMERO O.R.M. / pren.rit( 25 ON / 26 ON)
203600010508      *    2°    MITTENTE                ( 11 ON )
203700010508      *    3°    DESTINATARIO            ( 14 ON )
203800161115      *    4°    LINEA DI PARTENZA <> 999( 02 ON )
203900010508      *    5°    LINEA DI ARRIVO         ( 01 ON )
203901161115      *    6°    LINEA DI Partenza 999   ( 02 ON )
204000940712      *
204001161116     c                   if        *in25 or *in26
204100161116     C                   setoff                                       111402
204200161116     C                   setoff                                       01
204201161116     c                   endif
204300131113     C   14              SETOFF                                       020111
204400131113     C   11              SETOFF                                       0201
204500161115     C** 02              SETOFF                                       01
204501161115     c* 02 on  --> lnp>0
204502161115     c                   if        *in02
204503161115     c                   if        v1clp1<>999
204504161115     c                   setoff                                       01
204505161115     c                   else
204506161115     c                   if        *in01
204507161115     c                   setoff                                       02
204508161115     c                   endif
204509161115     c                   endif
204510161115     c                   endif
204600991222     c*se lna dpd e immesso riferimento partner estero, se meno di 12
204700060518     c*  caratteri, calcolo il 12 --> ora non più: il check digit per la
204800060518     c*  ricerca non serve più
204900991222     C                   CLEAR                   krpe
205000050121    1c                   if        v1crpe <> *blanks and (lnantw='DPD'
205100050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
205200060613     C                   movel     V1CRpe        krpe             14
205300991222     C                   ENDIF
205400911023     C*
205500900514     C     ENDCTR        ENDSR
205600020628      *------------------------------------------------------------------------*
205700020628      *    Controlla i dati nazione e provincia destinatario
205800020628      *------------------------------------------------------------------------*
205900020628     c     ContrDatiDes  begsr
205901161125     c* se ricerca per destinatario non è più obbligatorio inserire provincia/nazione
206000020628     c                   if        v1cnzd = *blanks and v1cprd = *blanks
206001161125     c                             and v1crsd=*blanks
206100020628     c                   eval      v1cmsg = msg(23)
206200160720     c                   eval      *in50 = *on
206300020628     c                   eval      *in28 = *on
206400020628     c                   goto      EndContr
206500020628     c                   endif
206600020628      * ricerca tabella nazione
206700020628     c     '?'           scan      v1cnzd                                 90
206800020628     c                   movel     '15'          cod
206900020628     c                   if        *in90
207000020628     c                   call      'TRUL19R'
207100020628     c                   parm                    cod
207200020628     c                   parm                    ordina
207300020628     c                   parm                    key
207400020628     c                   parm                    comando
207500020628     c                   movel     key           v1cnzd
207600020628     c                   eval      *in57 = *on
207700020628     c                   movel     'S'           wriem
207800020628     c                   goto      EndContr
207900020628     c                   endif
208000020628      * controllo nazione destinatario
208100020628     c                   clear                   ds15
208200020628     c                   movel     '15'          cod
208300020628     c                   clear                   key
208400020628     c                   movel     v1cnzd        key
208500020628     c     ktab1         chain     tabel00f
208600020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
208700020628     c                   eval      v1cmsg = msg(24)
208800020628     c                   eval      *in57 = *on
208900020628     c                   eval      *in28 = *on
209000020628     c                   goto      EndContr
209100020628     c                   endif
209200020628     c                   movel     tbluni        ds15
209300060613     c* Se cambiata nazione, clearo forzatura msg se nazine estera
209800020628      * per nazione italia la provincia è obbligatoria
209801161125      * se non immessa la ragione sociale del destinatario
209900020628     c                   if        §15ita = 'I' and v1cprd = *blanks
209901161125     c                             and v1crsd=*blanks
210000020628     c                   eval      v1cmsg = msg(25)
210100020628     c                   eval      *in50 = *on
210200020628     c                   eval      *in28 = *on
210300020628     c                   goto      EndContr
210400020628     c                   endif
210500020628      * ricerca tabella provincia
210600020628     c     '?'           scan      v1cprd                                 90
210700020628     c                   movel     'PR'          cod
210800020628     c                   if        *in90
210900020628     c                   call      'TRUL19R'
211000020628     c                   parm                    cod
211100020628     c                   parm                    ordina
211200020628     c                   parm                    key
211300020628     c                   parm                    comando
211400020628     c                   movel     key           v1cprd
211500020628     c                   eval      *in50 = *on
211600020628     c                   movel     'S'           wriem
211700020628     c                   goto      EndContr
211800020628     c                   endif
211900020628      * controllo provincia destinatario
212000020628     c                   if        v1cprd <> *blanks and §15ita = 'I'
212100020628     c                   clear                   dspr
212200020628     c                   movel     'PR'          cod
212300020628     c                   clear                   key
212400020628     c                   movel     v1cprd        key
212500020628     c     ktab1         chain     tabel00f
212600020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
212700020628     c                   eval      v1cmsg = msg(25)
212800020628     c                   eval      *in50 = *on
212900020628     c                   eval      *in28 = *on
213000020628     c                   goto      EndContr
213100020628     c                   endif
213200020628     c                   movel     tbluni        dspr
213300020628      * se nazione italia la provincia deve essere italia
213400020628      * nella tabella ci sono anche le provincie estere e per sapere se ok
213500020628      * faccio il controllo con il codice tassazione per estero
213600020628      * se vuoto vuol dire che è provincia estera
213700020628     c                   if        §15ita = 'I' and §prcte = *blanks
213800020628     c                   eval      v1cmsg = msg(25)
213900020628     c                   eval      *in50 = *on
214000020628     c                   eval      *in28 = *on
214100020628     c                   goto      EndContr
214200020628     c                   endif
214300020628     c                   endif
214400020628
214500020628     c     EndContr      endsr
214600940318      **************************************************************************
214700940318      *    ELABORA BOLLE DA VISUALIZZARE
214800940318      **************************************************************************
214900900515     C     ELAB          BEGSR
215000940318      *
215100160718     C                   SETON                                        3533
215200160311     C                   IF        dsp_mod = 1
215300160311     C                   WRITE     LRI6C02A
215400160311     c                   else
215500131114     C                   WRITE     LRI6C02
215600160311     c                   endif
215700160718     C                   SETOFF                                       3533
215800131114     C*
215900940713     C                   Z-ADD     1             REC2
216000911024     C                   Z-ADD     1             C
216100131111     c                   eval      wEnd = *off
216200131115     c                   clear                   readFile          3
216201161124     c                   clear                   wsflpieno
216300131113     c
216400131113     C                   Z-ADD     COMAAD        $AAS
216500131113     C                   Z-ADD     COMMGD        $MGS
216600131111     c
216700131111    1c                   select
216800010508     c* O.R.M.
216900131111    1c     *in25         wheneq    *on
216901161116    1c     *in26         oreq      *on
217000010508     C                   MOVEL     'M'           WTRC
217100161116     C   25              MOVEL(P)  SEL4          WN14             14
217101161116     C   26              MOVEL(P)  wEL4          WN14             14
217200060216     C     Kar44         SETLL     FiAR404L
217300131111     c
217400131111     c***
217500131111     C*  RIFERIMENTO P A R T N E R   E S T E R O
217600131111     c***
217700131111    1C     V1CRPE        whenne    *BLANKS
217800050121     c                   clear                   wgiro             1
217900050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
218000971223     C                   MOVEL(P)  V1CRPE        WN30             30
218100991222     C                   MOVEL     'E'           WTRC
218101171023     C     KAR4          SETLL     FiAR403L
218200991222     c                   else
218300991222     c* D P D
218400171023     C***                MOVEL(P)  krpe          WN30             30
218600171023     C***                MOVEL     'I'           WTRC
218601171023     c     krpe          setll     fipnd03l
218700991222     c                   endif
218800991222     c**
218900171023     C***  KAR4          SETLL     FiAR403L
219000991222     c**
219100161013     c* C H I   S O N O  (non per contenuto)
219200131111     c**
219300131111    1C     V1CNOT        whenne    *BLANKS
219301161013     c     v1cxnot       andeq     *blanks
219302161013     c
219400161014     C     KarS          SETLL     FIarS03L
219500131111     c* R N N
219600131111    1c     v1crmn        whengt    *zeros
219700150318     c     v1crmn        setll     fnblp33l
219800131115     c                   eval      Readfile='DSP'
219900131111     c* R M A
220000131111    1c     comrma        whenne    *blanks
220100170224     c**   comrma        setll     fnblp32l
220101170224     c                   exsr      PresqlRMA
220102170224     c                   exsr      OpencursorRMA
220200170224     c****               eval      Readfile='DSP'
220300131111     c
220400131113      * RSD/RSM/IND/LOD o solo NAZ/PRV       -----------------------------------
220500110930     c
220600131113     c                   when      *in14  OR *IN11
220700150508     c                   if        v1cssp=*blanks or v1cssp='N'
220800150319     c                   exsr      PresqlBLP
220900150319     c                   exsr      OpencursorBlp
221000150319     c                   else
221100131111     c                   exsr      Presql
221200131111     c                   exsr      Opencursor
221300150319     c                   endif
221400131111   x1c                   other
221500131111     c
221600971223      *
221700940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1)  -----------------------------------
221800971223    2C   18              DO
221900940713     C     V1CLNA        IFNE      *ZERO
222000940713     C                   Z-ADD     V1CLNA        COMLNA
222100940712     C                   ELSE
222101161116     c                   move      $poga(1)      comlna
222200161115     C****               Z-ADD     L1(1)         COMLNA
222300940712     C                   ENDIF
222301161115     C     V1CLp1        IFNE      999
222302161115     C                   Z-ADD     V1Clp1        comlp1
222303161115     c                   else
222304161115     c                   clear                   comlp1
222306161115     C                   ENDIF
222400020910     C     K1ARB         SETLL     FNARB78L
222500131115     c                   eval      Readfile='DSP'
222600971223    2C                   ENDDO
222700940318      *
222800940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
222900971223    2C   19              DO
223000940713     C     V1CLNA        IFNE      *ZERO
223100120330     C     V1CLNA        SETLL     FNARB05L
223200940712     C                   ELSE
223300120330     C     *LOVAL        SETLL     FNARB05L
223400940712     C                   ENDIF
223500971223    2C                   ENDDO
223600940318      *
223700940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
223800971223    2C   20              DO
223900940713     C     V1CLNA        IFNE      *ZERO
224000940713     C                   Z-ADD     V1CLNA        COMLNA
224100940712     C                   ELSE
224101161116     c                   move      $poga(1)      comlna
224200161115     C***                Z-ADD     L1(1)         COMLNA
224300940712     C                   ENDIF
224301161115     C     V1CLp1        IFNE      999
224302161115     C                   Z-ADD     V1Clp1        comlp1
224303161115     c                   else
224304161115     c                   clear                   comlp1
224305161115     C                   ENDIF
224400940713     C     K4ARB         SETLL     FNARB54L
224500131115     c                   eval      Readfile='DCM'
224600971223    2C                   END
224700940318      *
224800150508      * TUTTE  (21 ON)  (V1CSSP=' ') oppure "NON PARTITE" (V1CSSP='N') ---------
224900971223    2C   21              DO
225000131113     c
225100131113     c* per LINEA DI PARTENZA
225200131113    3C     *IN02         IFEQ      *ON
225201161115     C     V1CLp1        IFNE      999
225202161115     C                   Z-ADD     V1Clp1        COMLp1
225203161115     C                   ELSE
225204161116     c                   move      $pogp(1)      comlp1
225206161115     C                   ENDIF
225207161115     c                   exsr      sr_posiz
227700131113    3C                   END
227800131113     c*
227900131113     c* Per LINEA DI ARRIVO
228000150318     C   01K8blp         SETLL     FNblp26L
228100131115     c                   eval      Readfile='DSP'
228200131111     c
228300971223    2C                   ENDDO
228400131111    1C                   ENDSL
228500940318      *
228600911023     C                   Z-ADD     0             COMOD
228700911023     C                   EXSR      CARSF
228800940318      *
228900901116     C                   ENDSR
228901161115      **************************************************************************
228902161115      *    Gestione posizionamento per linea di partenza
228903161115      **************************************************************************
228904161115     c     sr_posiz      begsr
228905161115     C                   SELECT
228906161115     C* Serie = 0 e segnacollo dal diverso da 0 => i segnacolli sono
228907161115     C* sequenziali
228908161115     C     V1CPSD        WHENEQ    *ZEROS
228909161115     C     V1CSCD        ANDNE     *ZEROS
228910161115     C                   Z-ADD     V1CSCD        COMSCD
228911161115     C     K6blp         SETLL     FNblp31l                               31
228912161115     c                   eval      Readfile='DSP'
228913161115     C     *IN31         IFEQ      *OFF
228914161115     C* Non trovata bolla col numero segnacollo specificato: il
228915161115     C* segnacollo non è quello iniziale della bolla e quindi è sicura-
228916161115     C* mente incluso nella spedizione precedente.
228917161115     C     K6blp1        READPE    FNblp31L                               31
228918161115     C  n31              Z-ADD     ARBNCD        COMSCD
228919161115     C     K6blp         SETLL     FNblp31l
228920161115     C                   END
228921161115     c
228922161115     C* Non immesso segnacollo dal oppure serie diversa da zero (segna-
228923161115     C* colli non sequenziali) mi posiziono con segnacollo = 0
228924161115     C                   OTHER
228925161115     C                   MOVE      *ZEROS        COMSCD
228926161115     C     K6blp         SETLL     FNblp31L
228927161115     c                   eval      Readfile='DSP'
228928161115     C                   ENDSL
228929161115     c                   endsr
229000940318      **************************************************************************
229100940318      *    CARICO SUBFILE IN AVANTI
229200940318      **************************************************************************
229300900515     C     CARSF         BEGSR
229400940318      *
229500900518     C                   Z-ADD     1             B                 2 0
229600940713     C                   Z-ADD     COMOD         NR2
229700171023     C***                clear                   War4AAS
229800171023     C****               clear                   War4LNP
229900171023     C****               clear                   War4NRS
230000171023     C****               clear                   War4NSP
230200000228      *
230300161124    1C     B             DOWLE     w_nrigs
230301160523     C                   clear                   W_arr
230400060216     c                   setoff                                       32
230500131111
230600131113    2c                   select
230700131111     c***
230800131111     c* O R M
230900131111     c***
231000131111     c     *in25         wheneq    *on
231001161116     c     *in26         oreq      *on
231100060216     c     kar44         reade     fiar404l                               30
231200150507     c
231300150507     c                   clear                   fnarbds
231400150507     c                   exsr      sr_inzsoloblp
231500150318     C  N30KARB4         CHAIN     FNblp01L
231600150318     C  N30KARB4         CHAIN     FNARB01L
231700150507     c  N30              if        not %found(fnblp01l) and
231800150318     c                             not %found(fnarb01l)
231900150318     c                   seton                                        32
232000160506     c                   else
232100160506     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
232200160506     c* viene pulito dalla tassazione porti assegnati
232300160506     c*
232400160506     c                   if        %found(fnarb01l)
232500160506     c                   eval      w_arr='S'
232600160506     c                   endif
232700150318     c                   endif
232800131111     C***
232900131111      * R I F .   p a r t n e r
233000131111     C***
233100131111     C     V1CRPE        whenne    *BLANKS
233200050121     c     leggar4       tag
233300000228      *
233400050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
233500050304     c                             and wgiro <> '2'
233600060216     C     KAR4          READE     FiAR403L                               30
233601171023     c                   if        not *in30
233602171023     C                   EVAL      k_aas=ar4aas
233603171023     C                   EVAL      k_lnp=ar4lnp
233604171023     C                   EVAL      k_nrs=ar4nrs
233605171023     C                   EVAL      k_nsp=ar4nsp
233606171023     c                   endif
233700991222     c                   else
233800000228      * DPD
233900171023     C***  wtrc          READE     FiAR403L                               30
233901171023     C     krpe          READE     Fipnd03l                               30
234000171023     C                   If        *in30 = *off
234100171023     c***                movel     ar4n30        dsbl4i
234200000228      *
234300171023     c***                Select
234400000228      * Se non trovo n.parcel e selezionati 0 rcd o più di 1: fine lettura
234500171023     c***                When       §b4ipn <> krpe  and  nr2 <> 1
234600171023     C***                SETON                                            30
234700000228      * Se non trovo n.parcel e è selezionato 1 rcd:  fine lettura e
234800060216      *                                               riposiziono FiAR4
234900171023     c***                When       §b4ipn<> krpe
235000171023     C***                SETON                                            30
235100171023     C***  KAR4W         CHAIN     FiAR401L
235200000228      * Se ho trovato il n.parcel ed è il 1o memorizzo key sped
235300171023     c***                When       NR2 = *zeros
235400171023     C***                Z-ADD     AR4AAS        War4AAS
235500171023     C***                Z-ADD     AR4LNP        War4LNP
235600171023     C***                Z-ADD     AR4NRS        War4NRS
235700171023     C***                Z-ADD     AR4NSP        War4NSP
235800171023     c***                Endsl
235801171023     C                   EVAL      k_aas=PNDaas
235802171023     C                   EVAL      k_lnp=PNDlnp
235803171023     C                   EVAL      k_nrs=PNDnrs
235804171023     C                   EVAL      k_nsp=PNDnsp
235900171023     c                   Endif
236000000228      *
236100991222     c                   endif
236200991222     c
236300050304     c                   if        *in30 and wgiro <> '2' and
236400050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
236500050304     c                             wnodpd = *blanks
236600050121     c                   eval      wgiro = '2'
236700171023     C***                MOVEL     'I'           WTRC
236800171023     C****               MOVEL(P)  KRPe          WN30             30
236900171023     C**** Kar4          SETLL     Fiar403l
236901171023     c     krpe          setll     fipnd03l
237000050121     c                   goto      leggar4
237100050121     c                   endif
237200061117     c*
237300061117     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
237400061117     c* faccio tentaivo di ricerca con rif.truccato
237500171023     c***                if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
237600171023     c***                          and %subst(krpe:12:1)=' '
237700171023     C***                eval      krpe='0'+%subst(krpe:1:3)+'99'
237800171023     c***                          +%subst(krpe:4:8)
237900171023     C***                MOVEL(P)  KRPe          WN30             30
238000171023     C***  Kar4          SETLL     Fiar403L
238100171023     c***                goto      leggar4
238200171023     c***                endif
238300050121     c*
238400150507     c                   clear                   fnarbds
238500150507     c                   exsr      sr_inzsoloblp
238600171023     C* N30KARB4         CHAIN     FNblp01L
238700171023     C* N30KARB4         CHAIN     FNARB01L
238701171023     C  N30KEYRPE        CHAIN     FNblp01L
238702171023     C  N30KEYRPE        CHAIN     FNARB01L
238800150507     c  n30              if        not %found(fnblp01l) and
238900150318     c                             not %found(fnarb01l)
239000150318     c                   seton                                        32
239100160506     c                   else
239200160506     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
239300160506     c* viene pulito dalla tassazione porti assegnati
239400160506     c                   if        %found(fnarb01l)
239500160506     c                   eval      w_arr='S'
239600160506     c                   endif
239700150318     c                   endif
239800131111     c**
239900131111     c* C H I   S O N O
240000131111     c**
240100131111    1C     V1CNOT        whenne    *BLANKS
240101161013     c     v1cxnot       andeq     *blanks
240200161014     C     'C'           reade     FIarS03L                               30
240300131111     c                   if        not *in30
240301161014     c                   eval      len=%checkr(' ':v1cnot)
240302161014     c                   if        %subst(v1cnot:1:len)<>%subst(arsnot:1:len)
240303161014     c                   seton                                        30
240304161014     c                   else
240400150318     C***! KarSXT        CHAIN     FNarT27L                           32
240500150318     C     KarSXT        CHAIN     FiarT27L                           32
240600150507     c                   clear                   fnarbds
240700150507     c                   exsr      sr_inzsoloblp
240800150318     c  n32KARTXP        CHAIN     FNblp01L
240900150318     C  n32KARTXP        CHAIN     FNarb01L
241000150318     c                   if        *in32 or
241100150318     c                             (not %found(fnblp01l) and
241200150318     c                              not %found(fnarb01l)  )
241300131111     C                   SETON                                        30
241400160506     c                   else
241500160506     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
241600160506     c* viene pulito dalla tassazione porti assegnati
241700160506     c                   if        %found(fnarb01l)
241800160506     c                   eval      w_arr='S'
241900160506     c                   endif
242000131111    3C                   ENDIF
242001161014     c                   endif
242100131111    2C                   ENDIF
242200110930     c*
242300131111     c* Interrogazione per R M N
242400131111    1C     v1crmn        whengt    *zeros
242500150507     c                   clear                   fnarbds
242600150318     c     v1crmn        reade     fnblp33l                               30
242700150318     c                   if        not %eof(fnblp33l)
242800150318     c     karb2         chain     fnarb01l
242900160506     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
243000160506     c* viene pulito dalla tassazione porti assegnati
243100160506     c                   if        %found(fnarb01l)
243200160506     c                   eval      w_arr='S'
243300160506     c                   endif
243400150318     c                   endif
243500940318      *
243600131111     c* Interrogazione per R M A
243700131111    1C     comrma        whenne    *BLANKS
243701170224     c                   if        wend=*off
243702170224     c                   exsr      readcursorRMA
243703170224     c                   endif
243704170224     c                   if        Wend=*on
243705170224     c                   seton                                        30
243706170224     c                   endif
243800170224     c**                 clear                   fnarbds
243900170224     c**   comrma        reade     fnblp32l                               30
244000170224     c**                 if        not %eof(fnblp32l)
244100170224     c**   karb2         chain     fnarb01l
244200160506     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
244300160506     c* viene pulito dalla tassazione porti assegnati
244400170224     c**                 if        %found(fnarb01l)
244500170224     c**                 eval      w_arr='S'
244600170224     c**                 endif
244700170224     c**                 endif
244800131111     c
244900131111      * RSD/RSM/IND/LOD                      -----------------------------------
245000131111     c
245100131113     c                   when      *IN14 OR *IN11
245200131113
245300131111     c                   if        Wend=*off
245400150508     c                   if        v1cssp=*blanks or v1cssp='N'
245500150319     c                   exsr      ReadcursorBlp
245600150319     c                   else
245700131111     c                   exsr      Readcursor
245800150319     c                   endif
245900131111     c                   endif
246000131111     c
246100131111     c                   if        Wend=*on
246200131111     c                   seton                                        30
246300131111     c                   endif
246400111005     c*
246500131111     C                   other
246600940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
246700940318     C   18              DO
246800940713     C     V1CLNA        IFNE      *ZERO
246900150507     c                   exsr      sr_inzsoloblp
247000020910     C   15K2ARB         READE     FNARB78L                               30
247100020910     C  N15K3ARB         READE     FNARB78L                               30
247200160506     c  N30              eval      w_arr='S'
247300940712      *
247400940712     C                   ELSE
247500940712      *
247600950214     C                   SETON                                            31
247700950214     C                   SETOFF                                           30
247800950214     C     *IN31         DOWEQ     *ON
247900150507     c                   exsr      sr_inzsoloblp
248000020910     C   15K2ARB         READE     FNARB78L                               31
248100020910     C  N15K3ARB         READE     FNARB78L                               31
248200940318     C   31              ADD       1             C
248201161117     C   31              if        c<=%elem($poga) and
248202161117     C                             $poga(C)<>*blanks and $poga(C)<>*zeros
248203161116     C                   move      $poga(C)      COMLNA
248300161115     C** 31L1(C)         IFNE      0
248400161115     C***                Z-ADD     L1(C)         COMLNA
248500020910     C     K1ARB         SETLL     FNARB78L
248600940318     C                   ELSE
248700950214     C                   SETOFF                                       31
248800940318     C                   SETON                                        30
248900940318     C                   ENDIF
249000940318     C                   ENDDO
249100160506     c  N30              eval      w_arr='S'
249200940318      *
249300940712     C                   ENDIF
249400940318     C                   ENDDO
249500940318      *
249600940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
249700150507     c   19              DO
249800150507     c                   exsr      sr_inzsoloblp
249900940713     C     V1CLNA        IFNE      *ZERO
250000120330     C     V1CLNA        READE     FNARB05L                               30
250100940712     C                   ELSE
250200120330     C                   READ      FNARB05L                               30
250300940712     C                   ENDIF
250400160506     c  N30              eval      w_arr='S'
250500940712     C                   ENDDO
250600940318      *
250700940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
250800940318     C   20              DO
250900940713     C     V1CLNA        IFNE      *ZERO
251000150507     c                   exsr      sr_inzsoloblp
251100940712     C   15K5ARB         READE     FNARB54L                               30
251200940713     C  N15V1CLNA        READE     FNARB54L                               30
251300160506     c  N30              eval      w_arr='S'
251400940712      *
251500940712     C                   ELSE
251600940712      *
251700940712     C                   SETON                                            31
251800940712     C     *IN31         DOWEQ     '1'
251900150507     c                   exsr      sr_inzsoloblp
252000940712     C  N36
252100940712     CAN 15K5ARB         READE     FNARB54L                               31
252200940712     C  N36
252300940712     CANN15COMLNA        READE     FNARB54L                               31
252400060216     C  N31              SETOFF                                       30
252500980120     C   31              ADD       1             C
252501161117     C   31              if        c<=%elem($poga) and
252502161117     C                             $poga(C)<>*blanks and $poga(C)<>*zeros
252503161116     C                   move      $poga(C)      COMLNA
252504161115     c
252600161115     C** 31L1(C)         IFNE      0
252700161115     C**                 Z-ADD     L1(C)         COMLNA
252800940712     C     K4ARB         SETLL     FNARB54L
252900940318     C                   SETOFF                                       36
253000940318     C                   ELSE
253100940318     C                   SETON                                        30
253200940318     C                   SETOFF                                       31
253300940318     C                   ENDIF
253400940318     C                   ENDDO
253500160506     c  N30              eval      w_arr='S'
253600940318      *
253700940712     C                   ENDIF
253800940318     C                   ENDDO
253900940318      *
254000150508      * TUTTE  (21 ON)  (V1CSSP=' ')  oppure NON PARTITE (V1CSSP='N') ----------
254100940318     C   21              DO
254200150507     c                   clear                   fnarbds
254201161115     c* Selezione per LINEA DI ARRIVO
254300940712     C   01
254400150318     CAN 15K8blp         READE     FNblp26L                               30
254500940713     C   01
254600150318     CANN15V1CLNA        READE     FNblp26L                               30
254700150318     c                   if        *in01 and not %eof(fnblp26l)
254800150318     c     karb2         chain     fnarb01l
254900160506     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
255000160506     c* viene pulito dalla tassazione porti assegnati
255100160506     c                   if        %found(fnarb01l)
255200160506     c                   eval      w_arr='S'
255300160506     c                   endif
255400150318     c                   endif
255401161115     c* Selezione per LINEA DI PARTENZA
255402161115    4c                   if        *in02
255403161115     c* Linea di partenza <>999
255404161115    5c                   if        v1clp1<>999
255405161115     c                   z-add     v1clp1        comlp1
255600161115     C   15K7blp         READE     FNblp31l                               30
255800161115     C  N15COMLP1        READE     FNblp31L                               30
255900161115    6c                   if        not %eof(fnblp31l)
256000150326     c     karb2         chain     fnarb01l
256100160506     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
256200160506     c* viene pulito dalla tassazione porti assegnati
256300161115    7c                   if        %found(fnarb01l)
256400160506     c                   eval      w_arr='S'
256500161115    7c                   endif
256600161115    6c                   endif
256601161115   x5c                   else
256602161115     c* Linea di partenza = 999 --> loop di lettura con filiali dell'area
256603161115     c                   eval      $esci=*off
256604161115   5ac                   dow       $esci=*off
256605161115     C   15K7blp         READE     FNblp31l                               30
256606161115     C  N15COMLP1        READE     FNblp31L                               30
256607161115    6c                   if        %eof(fnblp31l)
256608161115     c                   add       1             c
256609161117     c                   if        c<=%elem($pogp) and
256610161117    7c                             $pogp(c)<>*blanks and $pogp(c)<>*zeros
256611161116     c                   move      $pogp(c)      comlp1
256612161115     c                   exsr      sr_posiz
256613161115   x7c                   else
256614161115     c                   seton                                        30
256615161115     c                   eval      $esci=*on
256616161115    7c                   endif
256617161115   x6c                   else
256618161115     c                   eval      $esci=*on
256619161115    6c                   endif
256620161115   5ac                   enddo
256621161115    6c                   if        not *in30
256622161115     c     karb2         chain     fnarb01l
256623161115     c* memorizzo se la bolla è già in arrivo (partita). Il flag Ft1 in alcuni casi
256624161115     c* viene pulito dalla tassazione porti assegnati
256625161115    7c                   if        %found(fnarb01l)
256626161115     c                   eval      w_arr='S'
256627161115    7c                   endif
256628161115    6c                   endif
256629161115    5c                   endif
256630161115    4c                   endif
256700131113     c                   ENDDO
256800131113     c                   endsl
256900940318      *
257000060216     c  n32
257100060216     CanN30              EXSR      CONFR
257200900515     C  N30
257300060216     CANN32              EXSR      EMISS
257400900515     C  N30              END
257500131113     c
257600940713     C                   Z-ADD     NR2           COMOD             4 0
257700930518     C                   SETOFF                                       1362
257800131113     c
257900131209     c** 30              if        Wend=*on
258000131209     c   30              if        WopenC='1'
258100131113     c                   exsr      closecursor
258200131113     c                   endif
258300150319     c   30              if        WopenC2='1'
258400150319     c                   exsr      closecursorBlp
258500150319     c                   endif
258501170224     c   30              if        WopenC3='1'
258502170224     c                   exsr      closecursorRMA
258503170224     c                   endif
258600940318      *
258700900515     C                   ENDSR
258800940317      **************************************************************************
258900940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
259000940317      **************************************************************************
259100900515     C     CONFR         BEGSR
259200940317      *
259300911024     C                   SETOFF                                       3236
259400131111     c                   z-add     arbmgs        arbdsp
259500131111     c                   movel     arbaas        arbdsp
259600131111     c
259700110930     c* Se richiamata per Perfect Order devo scartare la bolla se xco diverso da
259800110930     c* quello passato
259900110930     c                   if        pa1flg='W' and arbxco<>pa1xco
260000110930     c                   seton                                        32
260100110930     c                   goto      endcon
260200110930     c                   endif
260300940318      *
260400940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
260500940712      * DEVO CONTROLLARE LA DATA SPEDIZIONE (ARBDSP)
260600971223    1C   18              DO
260601161116     c* Mi disinteresso della data se richiesta interrogazione per orm o nr. prenotaz.Ritiro
260602161116     c                   if        sel4=*zeros and v1cnprr=*blanks
260700131115     c                   if        comdtd>0 and *in15
260800131115    2C     ARBDSP        IFNE      COMDTD
260900940713     C     ARBDCM        ANDEQ     *ZERO
261000131115     c
261100131115    3C                   IF        v1clna<>0 and readfile='DSP'
261200940712     C                   SETON                                            30
261300131115     c                   endif
261400131115
261500131115     C                   SETON                                            32
261600911023     C                   GOTO      ENDCON
261700971223    2C                   END
261800131115     c                   endif
261900940318      *
262000131115     c                   if        comdtd>0 and not *in15
262100131115    2C     ARBDSP        IFGT      COMDTA
262200940713     C     ARBDCM        ANDEQ     *ZERO
262300131115     c* si potrebe uscire anche se lettura per riferimenti mittente
262400131115     c*  ma se la linea di arrivo =0 non lo fa ora .... si può sistemare
262500131115     c*  per far leggere meno
262600131115    3C                   IF        v1clna<>0 and readfile='DSP'
262700131115     C                   SETON                                            30
262800131115     c                   endif
262900131115
263000940712     C                   SETON                                            32
263100911023     C                   GOTO      ENDCON
263200971223    2C                   END
263300131115     c                   endif
263400131115     c
263401161116     c                   endif
263500131115    1C                   END
263600940318      *
263700940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
263800940712      * DEVO CONTROLLARE LA DATA DISTINTA (ARBDDC)
263900971223    1C   19              DO
263901161116     c* Mi disinteresso  se richiesta interrogazione per orm o nr. prenotaz.Ritiro
263902161116     c                   if        sel4=*zeros and v1cnprr=*blanks
263903161116
264000960527     C     ARBFBC        IFNE      *BLANKS
264100960527     C                   SETON                                        32
264200960527     C                   GOTO      ENDCON
264300960527     C                   END
264400960527     C*
264500131115     c                   if        comdtd>0 and *in15
264600131115     C     ARBDDC        IFNE      COMDTD
264700911024     C                   SETON                                            32
264800911024     C                   GOTO      ENDCON
264900911024     C                   END
265000131115     C                   ENDif
265100940318      *
265200131115     c                   if        comdtd>0 and not *in15
265300131115     C     ARBDDC        IFLT      COMDTD
265400940712     C     ARBDDC        ORGT      COMDTA
265500911024     C                   SETON                                            32
265600911024     C                   GOTO      ENDCON
265700911024     C                   END
265800131115     C                   ENDif
265900940318      *
266000940321      * SE NON C'E' NUMERO DISTINTA NON E' IN CONSEGNA
266100911024     C     ARBNDC        IFEQ      0
266200911024     C                   SETON                                            32
266300911024     C                   GOTO      ENDCON
266400911024     C                   END
266401161116
266402161116     c                   endif
266403161116
266500971223    1C                   END
266600940318      *
266700940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
266800940712      *  DEVO CONTROLLARE LA DATA CONSEGNA (ARBDCM)
266900971223    1C   20              DO
266901161116     c* Mi disinteresso  se richiesta interrogazione per orm o nr. prenotaz.Ritiro
266902161116     c                   if        sel4=*zeros and v1cnprr=*blanks
266903161116
267000131115     c                   if        comdtd>0 and *in15
267100131115    2C     ARBDCM        IFNE      COMDTD
267200131115    3C                   IF        v1clna<>0 and readfile='DCM'
267300131115     C                   SETON                                            30
267400131115     c                   endif
267500131115
267600940712     C                   SETON                                            32
267700911024     C                   GOTO      ENDCON
267800971223    2C                   END
267900131115     c                   endif
268000940318      *
268100131115     c                   if        comdtd>0 and not *in15
268200131212     C     ARBDCM        IFLT      COMDTD
268300131212     C                   SETON                                            32
268400131212     C                   GOTO      ENDCON
268500131212     C                   END
268600131212     c
268700131115    2C     ARBDCM        IFGT      COMDTA
268800131115    3C                   IF        v1clna<>0 and readfile='DCM'
268900131115     C                   SETON                                            30
269000131115     c                   endif
269100131115
269200940712     C                   SETON                                            32
269300911024     C                   GOTO      ENDCON
269400971223    2C                   END
269500131115     c                   endif
269600131115     c
269700131115      * SE NON C'E' data consegna  NON E' consegnata
269800131115     C     ARBDCM        IFEQ      0
269900131115     C                   SETON                                            32
270000131115     C                   GOTO      ENDCON
270100131115     C                   ENDIF
270101161116
270102161116     c                   endif
270200131115    1C                   END
270300940329      * ------------------------------------------------------------------------
270400940712      * HO SPOSTATO IL CONFRONTO CON MITTENTE E DESTINATARIO PRIMA DEL BLOCCO
270500940712      * CON 21 ON PERCHE' POSSO ACCENDERE IL 30 E ANDARE A FINE CICLO QUANDO
270600940712      * CAMBIA LA RAG.SOCIALE (EVITO DELLE LETTURE)
270700940712      *
270800940712      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
270900940712      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
271000940713      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
271100940713      * con V1CRSM.
271200060613     C                   MOVE      *BLANKS       $SALTA            1
271300940712      *
271400940712      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
271500131113     c                   if        not *in11 and not *in14
271600131113    1C     V1CRSD        IFNE      *BLANKS
271700131113     c
271800131113    2c     v1cxrsd       ifeq      ' '
271900131113     C     ZZ            SUBST(P)  ARBRSD        COMRAG
272000940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
272100131113    3C     COMRAG        IFNE      V1CRSD
272200131113     C                   MOVE      'S'           $SALTA
272300131113    3C                   ENDIF
272400131113     c*
272500131113   x2c                   else
272600131114     C                   MOVEL     ARBAAS        KAAS
272700131114     C                   MOVEL     ARBLNP        KLNP
272800131114     C                   MOVEL     ARBNRS        KNRS
272900131114     C                   MOVEL     ARBNSP        KNSP
273000131114     C                   MOVEL     'D'           KTRC
273100131114     C     KAR41         CHAIN     FiAR401L
273200131114     c                   if        %found(fiar401l)
273300131114     c                   eval      arbrsduni=arbrsd+ar4not
273400131114     c                   else
273500131114     c                   eval      arbrsduni=arbrsd
273600131114     c                   endif
273700131114     c
273800131114    3c                   if        %scan(%trimr(v1crsd):arbrsdUNI) = 0
273900131113     C                   MOVE      'S'           $SALTA
274000131113    3c                   endif
274100131113    2C                   ENDIF
274200131113    1C                   ENDIF
274300131113     c
274400131113      *??  I N D I R I Z Z O     D E S T I N A T A R I O  ?
274500131113    1C     V1Cind        IFNE      *BLANKS
274600131113     c
274700131113    2c     v1cxind       ifeq      ' '
274800131113     C     DD            SUBST(P)  ARBind        COMRAG           30
274900131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
275000131113    3C     COMRAG        IFNE      V1Cind
275100131113     C                   MOVE      'S'           $SALTA
275200131113    3C                   ENDIF
275300131113     c*
275400131113   x2c                   else
275500131113    3c                   if        %scan(%trimr(v1cind):arbind) = 0
275600131113     C                   MOVE      'S'           $SALTA
275700131113    3c                   endif
275800131113    2C                   ENDIF
275900131113    1C                   ENDIF
276000131113      *??  L O C A L I T A'      D E S T I N A T A R I O  ?
276100131113    1C     V1Clod        IFNE      *BLANKS
276200131113     c
276300131113    2c     v1cxlod       ifeq      ' '
276400131113     C     LL            SUBST(P)  ARBlod        COMRAG
276500131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
276600131113    3C     COMRAG        IFNE      V1Clod
276700131113     C                   MOVE      'S'           $SALTA
276800131113    3C                   ENDIF
276900131113     c*
277000131113   x2c                   else
277100131113    3c                   if        %scan(%trimr(v1clod):arblod) = 0
277200131113     C                   MOVE      'S'           $SALTA
277300131113    3c                   endif
277400131113    2C                   ENDIF
277500131113    1C                   ENDIF
277600940414      *
277700940712      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
277800131113    1C     V1CRSM        IFNE      *BLANKS
277900131113    2c     v1cxrsm       ifeq      ' '
278000940712     C     XX            SUBST(P)  ARBRSM        COMRAG
278100940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
278200131113    3C     COMRAG        IFne      V1CRSM
278300940712     C                   MOVE      'S'           $SALTA
278400131113    3C                   ENDIF
278500131113   x2c                   else
278600131113    3c                   if        %scan(%trimr(v1crsm):arbrsm) = 0
278700131113     C                   MOVE      'S'           $SALTA
278800131113    3c                   endif
278900131113    2C                   ENDIF
279000131113    1C                   ENDIF
279100131113    1C                   ENDIF
279101161013      *??  C H I   S O N O
279102161013    1C     V1CNOT        IFNE      *BLANKS
279103161014    2c     v1cxnot       ifeq      ' '
279104161014     c                   eval      len=%checkr(' ':v1cnot)
279105161014     c                   if        %subst(v1cnot:1:len)<>%subst(arsnot:1:len)
279106161014     C                   MOVE      'S'           $SALTA
279108161014     c                   endif
279109161013   x2c                   else
279110161014     c                   clear                   w_ars
279111161014     c     kart1         setll     fiart01l
279112161014     c     kart1         reade     fiart01l
279113161014    3c                   dow       not %eof(fiart01l)
279114161014     c     kars1         chain     fiars01l
279115161014    4c                   if        not %found(fiars01l) or
279116161014     c                             %scan(%trimr(v1cnot):arsnot) = 0
279118161014   x4c                   else
279119161014     c* trovato un chi sono valido
279120161014     c                   eval      w_ars='1'
279121161014    4c                   endif
279122161014     c     kart1         reade     fiart01l
279123161014    3c                   enddo
279124161014     c* Scarto se non trovati chi sono che soddisfinio la richiesta
279125161014    3c                   if        w_ars=*blanks
279126161014     C                   MOVE      'S'           $SALTA
279127161014    3c                   endif
279128161013    2C                   ENDIF
279129161013    1C                   ENDIF
279200940712      *
279300131115     c                   if        $SALTA='S'
279400131115     c                   seton                                        32
279500131115     C                   goto      ENDCON
279600131115     c                   endif
279700940318      *
279800150508      * TUTTE  (21 ON)  (V1CSSP=' ')  oppure NON PARTITE (V1CSSP='N') ----------
279900971223    1C   21              DO
279901161116     c* Mi disinteresso se richiesta interrogazione per orm o nr. prenotaz.Ritiro
279902161116     c                   if        sel4=*zeros and v1cnprr=*blanks
280000940712     C     ARBDSP        IFLT      COMDTD
280100940712     C     COMDTD        ANDNE     *ZERO
280200940712     C                   SETON                                            32
280300911024     C                   GOTO      ENDCON
280400911024     C                   END
280500940318      *
280600940321     C     ARBDSP        IFGT      COMDTA
280700940712     C     COMDTA        ANDNE     *ZERO
280800940712     C                   SETON                                            32
280900161116     c                   if        readfile='DSP' and v1clp1<>999
281000131115     C                   SETON                                            30
281100131115     c                   endif
281200131115     c
281300911024     C                   GOTO      ENDCON
281400131115     C                   ENDif
281401161117     c* Se richieste TUTTE le spedizioni
281402161117     c* verifico lo Stato Consegna se richiesto
281403161117    2c                   if        v1cssp=*blanks and v1cstco<>*blanks
281404161117    3c                   select
281405161117     c* DATA CONSEGNA MERCE>0  --> spedizione consegnata
281406161117     c                   when      arbdcm>0
281407161117     c* scarto se richieste le non consegnate
281408161117    4c                   if        v1cstco='N'
281409161117     c                   seton                                        32
281410161118     C                   GOTO      ENDCON
281411161117    4c                   endif
281412161117     c* DATA CONSEGNA MERCE=0 e SPED NON IN DISTINTA
281413161117     c*  scarto se richieste le consegnate
281414161117     c                   when      arbndc=0
281415161117    4c                   if        v1cstco='C'
281416161117     c                   seton                                        32
281417161118     C                   GOTO      ENDCON
281418161117    4c                   endif
281419161117     c                   other
281420161117     C* DATA CONSEGNA MERCE=0 e SPED     IN DISTINT
281421161117     C* Verifico se consegnata (Da PDA oppure Fermo deposito in distinta fittizia)
281423161117     c                   exsr      sr_VerCon
281426161117     c                   if        ((w_cev='  C' or W_fdcons='S') and
281427161117     c                             v1cstco='N')
281428161117     c                             or
281429161117     c                             (w_cev<>'  C' and W_fdcons<>'S' and
281430161117     c                             v1cstco='C')
281431161117     c                   seton                                        32
281432161118     C                   GOTO      ENDCON
281433161117     c                   endif
281450161117    3c                   endsl
281451161117    2c                   endif
281452161116
281453161116     c                   endif
281500131115     c
281600971223    1C                   ENDDO
281700150508     c* Bolle Non partite
281701161116     c* Mi disinteresso se richiesta interrogazione per orm o nr. prenotaz.Ritiro
281702161116     c                   if        sel4=*zeros and v1cnprr=*blanks
281800160506     c                   if        v1cssp='N' and (w_arr<>*blanks or
281900161031     c                                           (arbft2 <>' ' and arbft2<>'N'))
282000150508     c                   seton                                        32
282100150508     c                   goto      endcon
282200150508     c                   endif
282201161116     c                   endif
282300971223     C**
282400940317      *
282500940317      * PER OGNI SELEZIONE  ----------------------------------------------------
282600940317      *
282700940317      * TESTO LA LINEA DI ARRIVO
282800971223     C     V1CRPE        IFEQ      *BLANKS
282900971223     C     ARBLNA        ANDNE     V1CLNA
283000940713     C     V1CLNA        ANDNE     *ZERO
283100920911     C                   SETON                                            32
283200920911     C                   GOTO      ENDCON
283300971223     C                   ENDIF
283400020628      * testo la nazione/provincia del destinatario
283500060613     c                   if        v1cprd <> *blanks  and
283600020628     c                             (arbnzd <> v1cnzd  or arbprd <> v1cprd)
283700020628     c                   eval      *in32 = *on
283800020628     c                   goto      endcon
283900020628     c                   endif
284000060613     c                   if        (v1cnzd <> *blanks and v1cprd = *blanks) and
284100060613     c                             arbnzd <> v1cnzd
284200060613     c                   eval      *in32 = *on
284300060613     c                   goto      endcon
284400060613     c                   endif
284500020628
284600940317      *
284700940712      * VERIFICO ANCHE LA LNP POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
284800940713     C     ARBLNP        IFNE      V1CLP1
284900940713     C     V1CLP1        ANDNE     *ZERO
284901161115     C                   if        v1clp1<>999  or
284902161116     c                             %lookup(%editc(arblnp:'X'):$pogp)=0
285000940712     C                   SETON                                            32
285100940712     C                   GOTO      ENDCON
285101161115     c                   endif
285200940712     C                   END
285202161115     c
285300940923     C* FILIALE SEGNACOLLI
285400940926     C* SE NON VI SONO COLLI, SALTARE QUESTO CONTROLLO
285500940926     C     ARBNCD        IFNE      *ZERO
285600940926     C     ARBNCA        ORNE      *ZERO
285700940926     C     ARBFNS        OREQ      'S'
285800161125     C**   ARBFLS        ORNE      *ZERO
285900940923     C     V1CFLS        IFNE      *ZERO
286000940923     C     V1CFLS        ANDNE     ARBFLS
286100940923     C                   SETON                                            32
286200940923     C                   GOTO      ENDCON
286300940926     C                   ENDIF
286400940926     C                   ENDIF
286402161125     C* BOLLE A TERRA:  se   DUC = 0  OPPURE  DUC > DSP
286404161125     C     V1CTER        IFEQ      'T'
286406161125     C* ESCLUDO TUTTE LE BOLLE SENZA COLLI
286408161125     C     arbNCD        ifeq      *ZERO
286409161125     C     arbNCA        ANDEQ     *ZERO
286410161125     C     arbFNS        ANDEQ     *BLANKS
286411161125     C                   SETON                                            32
286412161125     C                   GOTO      ENDCON
286413161125     C                   ENDIF
286414161125     C*
286432161125     c                   if        arbduc >0        and
286433161125     c                             arbduc <= arbdsp
286434161125     C                   SETON                                            32
286435161125     C                   GOTO      ENDCON
286436161125     c                   endif
286437161125     C*
286438161125     C                   ENDIF
286500961112     C* RESO / FRANCO FRONTIERA
286600961112     C     V1CFBR        IFNE      ' '
286700961112     C                   MOVEL     V1CFBR        WFBR
286800961112     C     V1CFBR        IFEQ      'R'
286900961112     C                   MOVEL     'S'           WFBR
287000961112     C                   ENDIF
287100961112     C     ARBFBR        IFNE      WFBR
287200961112     C                   SETON                                            32
287300961112     C                   GOTO      ENDCON
287400961112     C                   ENDIF
287500961112     C                   ENDIF
287600040216      * Tipo bolla
287700040216     c                   if            V1Ccbo <> *blanks
287800040216     c                             and V1Ccbo <> ARBcbo
287900040216     c                   eval      *in32 = *on
288000040216     c                   goto      EndCon
288100040216     c                   endif
288200900515     C* TIPO SPEDIZIONE
288300940715     C     ARBTSP        IFNE      V1CTSP
288400940715     C     V1CTSP        ANDNE     *BLANKS
288500900515     C                   SETON                                            32
288600900515     C                   GOTO      ENDCON
288700900515     C                   END
288800911023     C* SPEDIZIONI GIACENTI
288900980603     C     V1CGIA        IFEQ      'G'
289000930702     C     ARBFBC        ANDNE     'G'
289100911023     C                   SETON                                            32
289200911023     C                   GOTO      ENDCON
289300911023     C                   END
289400980603     C     V1CGIA        IFEQ      'N'
289500930702     C     ARBFBC        ANDEQ     'G'
289600911023     C                   SETON                                            32
289700911023     C                   GOTO      ENDCON
289800911023     C                   END
289900070420     c*
290000070420     c* SPEDIZIONI IN FERMO DEPOSITO
290100070420     C                   IF        v1csfd='S' and arbffd<>'S'
290200070420     C                   SETON                                            32
290300070420     C                   GOTO      ENDCON
290400070420     C                   END
290500070420     c
290600900518     C* SERIE DAL + AL
290700940713     C     ARBNRS        IFLT      V1CPSD
290800900518     C                   SETON                                            32
290900900518     C                   GOTO      ENDCON
291000900518     C                   END
291100940713     C     ARBNRS        IFGT      V1CPSA
291200900515     C                   SETON                                            32
291300900515     C                   GOTO      ENDCON
291400900515     C                   END
291500900518     C* SEGNACOLLO DAL SOLO
291600930702     C     *IN07         IFEQ      *ON
291700970925     C     *IN84         ANDEQ     *OFF
291800161102     C     KART          SETLL     FIART01L                               31
291900930702     C  N31              SETON                                        32
292000930702     C  N31              GOTO      ENDCON
292100930702     C                   END
292200900515     C* SEGNACOLLO DAL + AL
292300970925     C   84              DO
292400910827     C                   SETON                                        32
292500161102     C     KARB2         SETLL     FIART01L
292600161102     C     KARB2         READE     FIART01L                               31
292700910827     C     *IN31         DOWEQ     '0'
292800940713     C     ARTNSC        IFGE      V1CSCD
292900940713     C     ARTNSC        ANDLE     V1CSCA
293000910827     C                   SETON                                        31
293100910827     C                   SETOFF                                       32
293200910827     C                   END
293300910827     C*
293400161102     C  N31KARB2         READE     FIART01L                               31
293500910827     C                   END
293600910827     C*
293700910827     C   32              GOTO      ENDCON
293800910827     C                   END
293900990119     C*
294000990119     C* NUMERO CA
294100990119    1C     V1CDAN        IFNE      ' '
294200040809     C                   SETON                                        32
294300040809      *
294400040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
294500040809      * legate alla spedizione che passo
294600040809     c                   clear                   fidn12ds
294700040809     c                   eval      i12aas = arbAAS
294800040809     c                   eval      i12lnp = arbLNP
294900040809     c                   eval      i12nrs = arbNRS
295000040809     c                   eval      i12nsp = arbNSP
295100040809     c                   eval      i12fel = 'E'
295200040809     c                   eval      i12fan = 'E'
295300040809     c*
295400040809     c                   call      'FIDN12R'
295500040809     c                   parm                    fidn12ds
295600040809     c*
295700040809     c                   setoff                                       31
295800040809     c*
295900040809     c* se non ci sono errori
296000040809     c                   if        o12err <> ' '
296100040809     c                   seton                                        31
296200040809     c                   else
296300040809     c* se numero di CA trovate maggiore di zero
296400040809     c                   if        o12nca = 0
296500040809     c                   seton                                        31
296600040809     c                   else
296700040809     c                   z-add     1             ii                2 0
296800040809     c                   dow       ii <= o12nca and *in31 = *off
296900040809     C* APERTA
297000040809     C     skdch(ii)     IFEQ      0
297100040809     C     V1CDAN        ANDEQ     'A'
297200040809     C                   SETOFF                                       32
297300040809     C                   SETON                                        31
297400040809     C                   ENDIF
297500040809     C* CHIUSE
297600040809     C     skdch(ii)     IFGT      0
297700040809     C     V1CDAN        ANDEQ     'C'
297800040809     C                   SETOFF                                       32
297900040809     C                   SETON                                        31
298000040809     C                   ENDIF
298100040809     C* CA APERTA O CHIUSA
298200040809     C     V1CDAN        IFEQ      'E'
298300040809     C                   SETOFF                                       32
298400040809     C                   SETON                                        31
298500040809     C                   ENDIF
298600040809     c*
298700040809     c                   add       1             ii
298800040809     c                   enddo
298900040809     c*
299000040809     c                   seton                                        31
299100040809     c*
299200040809     c                   endif
299300040809     c                   endif
299400040809     c**
299500990119    3C                   ENDIF
299600150507     c******             if        (v1cft1='P' and  arbft1=*blanks) or
299700150507     c******                       (v1cft1='N' and  arbft1<>*blanks)
299800150507     c******             seton                                        32
299900150507     c******             goto      endcon
300000150507     c******             endif
300100150331     c* Cliente mittente
300200150331     c                   if        v1cksc>0
300300150331     c                   movel     v1cklp        w0070
300400150331     c                   move      v1cksc        w0070
300500150331     c                   if        w0070<>arbksc
300600150331     c                   seton                                        32
300700150331     c                   endif
300800150331     c   32              if        w0070=arbccm
300900150331     c                   setoff                                       32
301000150331     c                   endif
301100150331     c   32              goto      endcon
301200150331     c                   endif
301300150331     c* ragione sociale mittente
301400150331     c                   if        v1crag<>*blanks
301500150331     c* verifico la lunghezza effettiva della stringa immessa
301600150331     c                   eval      lenrag=%checkr(' ':v1crag)
301700150331     c                   if        %subst(v1crag:1:lenrag)<>
301800150331     c                             %subst(arbrsm:1:lenrag)
301900150331     c                   seton                                        32
302000150331     c                   goto      endcon
302100150331     c                   endif
302200150331     c                   endif
302201161013     c* Reperimento Referente consegna e Telefono destintario
302202161013     c                   if        v1cref<>*blanks or v1ctel<>*blanks
302203161013     C                   MOVEL     ARBAAS        KAAS
302204161013     C                   MOVEL     ARBLNP        KLNP
302205161013     C                   MOVEL     ARBNRS        KNRS
302206161013     C                   MOVEL     ARBNSP        KNSP
302207161013     C                   MOVEL     'GEN'         KTRd
302208161013     c                   clear                   dar5gen
302209161013     c     kar51         chain     fiar501l
302210161013     c                   if        %found(fiar501l)
302211161013     c                   eval      dar5gen=ar5uni
302212161013     c                   endif
302213161013     c                   endif
302214161013     c* reperimento:
302300150331     c* indirizzo email
302400150331     c* numero cellulare
302500150331     c                   if        v1ceml<>*blanks or v1ctel<>*blanks
302600150331     C                   MOVEL     ARBAAS        KAAS
302700150331     C                   MOVEL     ARBLNP        KLNP
302800150331     C                   MOVEL     ARBNRS        KNRS
302900150331     C                   MOVEL     ARBNSP        KNSP
303000150331     C                   MOVEL     'EMD'         KTRd
303100150331     c                   clear                   dar5emd
303200150331     c     kar51         chain     fiar501l
303300150331     c                   if        %found(fiar501l)
303400150331     c                   eval      dar5emd=ar5uni
303500150331     c                   endif
303501161013     c                   endif
303503160909     c* Indirizzo email se richiesto
303504160909    1c                   if        v1ceml<>*blanks
303505160909    2c                   if        §ar5eml=*blanks
303506160909     c                   seton                                        32
303507160909     c                   goto      endcon
303508160909    2c                   endif
303509160909    2c                   if        v1ceml<>§ar5eml
303510160909     C     minu:maiu     XLATE     §ar5eml       w_§ar5eml                      *Minuscolo -> MAIUSC
303511160909    3c                   if        v1ceml<>w_§ar5eml
303512160909     c                   seton                                        32
303513160909     c                   goto      endcon
303514160909    3c                   endif
303515160909    2c                   endif
303516160909    1c                   endif
303517161013     c* Telefono se richiesto
304100161013    1c                   if        v1ctel<>*blanks
304101161013     c                   clear                   wtel_ok
304102161013     c                   clear                   wteld_ok
304103161013     c* telefono non presente in bolla
304104161013    2c                   if        §ar5tel=*blanks and §ar5teld=*blanks
304105160909     c                   seton                                        32
304106160909     c                   goto      endcon
304107161013    2c                   endif
304108161013     c* Presente telefono dest. (trd GEN) e diverso dal telefono richiesto
304109161013    2c                   if        §ar5teld<>*blanks and §ar5teld<>v1ctel
304110161013     c* normalizzo il numero trovato nel caso fosse un cellulare
304111161013      /free
304112161013       pInCell = %trim(§ar5teld);
304113161013       pOutErr = *blanks;
304114161013       if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
304115161013           // Numero di cellulare KO
304116161013          wteld_ok='n';
304117161013       else;
304118161013          if w_v1ctel<>poutcell ;
304119161013             wteld_ok='n'   ;
304120161013          endif;
304121161013       endif;
304122161013      /end-free
304123161013     c                   if        wteld_ok='n'
304124161013     c* imposto a no ok anche il cell se vuoto
304125161013     c                   if        §ar5tel=*blanks
304126161013     c                   eval      wtel_ok='n'
304127161013     c                   endif
304128161014     c                   endif
304129161013    2c                   endif
304130161013     c* Presente telefono cell. (trd EMD) e diverso dal telefono richiesto
304131161013    2c                   if        §ar5tel <>*blanks and §ar5tel<>v1ctel
304132161013     c* normalizzo il numero di cellulare trovato per confrontarlo
304133161013     c* col cellulare normalizzato immesso a video
304134161013      /free
304135161013       pInCell = %trim(§ar5tel);
304136161013       pOutErr = *blanks;
304137161013       if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
304138161013           // Numero di cellulare KO
304139161013          wtel_ok='n';
304140161013       else;
304141161013          if w_v1ctel<>poutcell ;
304142161013             wtel_ok='n'   ;
304143161013          endif;
304144161013       endif;
304145161013      /end-free
304146161013     c                   if        wtel_ok='n'
304147161013     c* imposto a no ok anche il tel. dest. se vuoto
304148161013     c                   if        §ar5teld=*blanks
304149161013     c                   eval      wteld_ok='n'
304150161013     c                   endif
304151161013    3c                   endif
304152161013    2c                   endif
304153161013     c* Scarto la bolla se nessun telefono corrisponde a quello richiesto
304154161013     c                   if        wtel_ok='n' and wteld_ok='n'
304155161013     c                   seton                                        32
304156161013     c                   goto      endcon
304157161013     c                   endif
304158161013    1c                   endif
304159161013
304160161013     c* Referente se richiesto
304161161013    1c                   if        v1cref<>*blanks and v1cref<>§ar5ref
304163161018     c                             and %scan(%trimr(v1cref):§ar5ref) = 0
304164161013     c                   seton                                        32
304165161013     c                   goto      endcon
304166161013     c                   endif
304700150331
304800030527     C*
304900940317     C     ENDCON        ENDSR
305000131114     C**************************************************************************
305100131114     C* Atre parzializzazioni
305200131114     C**************************************************************************
305300131114     C     AltrePar      BEGSR
305400131114     c     forw12        TAG
305500131114     c                   exfmt     LRI6W12
305600131114     c
305700131114     c                   setoff                                       285690
305800160615     c***                setoff                                       4069
305801160615     c                   setoff                                       40
305900150327     c                   setoff                                       659229
306000161116     c                   setoff                                       385354
306001161117     c                   setoff                                       67
306100131114     c
306200131114     C* GIACENTI
306300131114     C* SCELTE INCONGRUENTI
306400131114     C     V1CGIA        IFEQ      'G'
306500131114     C     V1CSSP        IFEQ      '2'
306600131114     C     V1CSSP        OREQ      '3'
306700150508     C     V1CSSP        OREQ      'N'
306800131114     C                   MOVEL     MSG(14)       w12MSG
306900131114     C                   SETON                                        285690
307000131114     C                   GOTO      forw12
307100131114     C                   END
307200131114     C                   END
307300131114      * Tipo bolla
307400131114if  1c                   if        V1Ccbo <> *blanks
307500131114     c                   eval      *in31  =  *off
307600131114     c     '?'           scan      V1Ccbo                                 31
307700131114if  2c                   if        *in31 = *on
307800131114     c                   call      'X§TABER'
307900131114     c                   parm      CodUt         §KUT
308000131114     c                   parm      '3A'          COD
308100131114     c                   parm      *blanks       KEY
308200131114     c                   parm      *blanks       §DES
308300131114     c                   movel     KEY           V1Ccbo
308400131114     c                   eval      *in90 = *on
308500131114     c                   goto      forw12
308600131114e   2c                   endif
308700131114      *
308800131114     c                   movel     '3A'          COD
308900131114     c                   movel(p)  V1Ccbo        KEY
309000131114     c     KTAB1         chain     TABEL
309100131114if  2c                   if        NOT %found(TABEL00F)
309200131114     c                             or  TBLflg <> *blanks
309300131114     c                   movel     Msg(9)        w12msg
309400131114     c                   seton                                        284090
309500131114     c                   goto      forw12
309600131114e   2c                   endif
309700131114e   1c                   endif
309701161117     C* Stato consegna C=Consegnate N=non consegnate
309702161117     c* Errore se non richiesta la visualizzazione di TUTTE le sped.
309703161117     C* SCELTE INCONGRUENTI
309704161117     C     V1Cstco       IFne      *blanks
309705161117     C     V1CSSP        andne     *blanks
309708161117     C                   MOVEL     MSG(14)       w12MSG
309709161117     C                   SETON                                        286790
309710161117     C                   GOTO      forw12
309711161117     C                   END
309800150319     c* Bolle partite
309900150507     c*****              if        v1cft1='N' and v1cssp<>' '
310000150507     C*****              MOVEL     MSG(14)       w12MSG
310100150507     C*****              SETON                                        286990
310200150507     C****               GOTO      forw12
310300150507     c****               endif
310400150319     c* cliente mittente  in alternativa a ragione sociale
310500161031     c                   if        (  (v1cklp<>*blanks and v1cklp<>*zeros and
310501161031     c                                 %scan('?':v1cklp)<=0)
310502161021     c                             or v1cksc>0)
310600150319     c                             and v1crag<>*blanks
310700150319     C                   MOVEL     MSG(33)       w12MSG
310800160518     C                   SETON                                        282990
310900150319     C                   GOTO      forw12
311000150319     c                   endif
311100150319     c* Ricerca con ?
311200150327    1c                   if        %scan('?':v1cklp)>0
311300150319      * PAXSTA = 9  ESCLUDO ANNULLATI
311400150319     C                   Z-ADD     9             PAXSTA
311600150319     C                   MOVEL     V1Cdsa        PAXDUT           30
311700150319     C                   MOVEL     *BLANKS       DESCR            48
311800161021     C                   MOVEL     V1CRAG        DESCR
311900160519     C                   Z-ADD     dutKCI        wkcc
312000161021     C***                MOVEL     V1CKLP        PAXFLR
312001161021     C                   MOVEL     *blanks       PAXFLR
312100150319     C                   CLEAR                   PAXDIT
312200150319     C                   Z-ADD     1             PAXNUM
312300150319     C                   CALL      'XALFA3BR'
312400150319     C                   PARM                    PAXDUT
312500150319     C                   PARM                    CODUT
312600150319     C                   PARM                    DESCR
312700161021     C                   PARM                    wkcc              4 0
312800150319     C                   PARM                    PAXSTA            1 0
312900150319     C                   PARM                    PAXFLR           90
313000150319     C                   PARM                    PAXDIT            3
313100150319     C                   PARM                    PAXNUM            2 0
313200150319     C                   PARM                    PAXKCM           80
313300150319     C                   PARM                    PAXKSM          140
313400150319     C                   PARM                    PAXKDM           60
313500150327    2C     PAXSTA        IFNE      -1
313600150319     C                   MOVEL     PAXKSM        w0070
313700150319     c                   movel     w0070         v1cklp
313800150319     C                   MOVE      w0070         V1CKSc
313900150319     C*****              MOVEL     DESCR         V1CRAG
313901161021     c                   clear                   v1crag
314000150319     C******             ELSE
314100150319     C******             CLEAR                   V1CRAG
314200150327    2C                   ENDIF
314300150319     c                   goto      forw12
314400150327    1c                   endif
314500150327     c* controllo il cliente mittente se immesso
314600161031    1c                   if        v1cklp<>*blanks and v1cklp<>*zeros
314700150327     c* deve contenere solo numeri
314800150327    2c                   if        %check(digits:v1cklp)>0
314900150327     C                   MOVEL     MSG(34)       w12MSG
315000150327     C                   SETON                                        282990
315100150327     C                   GOTO      forw12
315200150327    2c                   endif
315300150327     c* la linea del codice cliente deve essere esistente
315400150327     c                   move      v1cklp        w0030
315500150327     c     w0030         chain     azorg01l
315600150327    2c                   if        not %found(azorg01l)
315700150327     C                   MOVEL     MSG(34)       w12MSG
315800150327     C                   SETON                                        282990
315900150327     C                   GOTO      forw12
316000150327    2c                   endif
316100150327     c* errore se cod cliente non immesso
316200150327    2c                   if        v1cksc=0
316300150327     C                   MOVEL     MSG(35)       w12MSG
316400150327     C                   SETON                                        283890
316500150327     C                   GOTO      forw12
316600150327    2c                   endif
316700150327   x1c                   else
316800150327     c                   if        v1cksc>0
316900150327     C                   MOVEL     MSG(34)       w12MSG
317000150327     C                   SETON                                        282990
317100150327     C                   GOTO      forw12
317200150327     c                   endif
317300150327    1c                   endif
317400150319     c* ragione sociale mittente
317401161116     C     sel4          ifne      0
317402161116     C     sel2          andeq     0
317403161116     c     v1cnprr       andeq     *blanks
317404161116      * P.O. EMISSIONE
317405161116     C                   z-add     v1cpoe        num3
317406161116     C     num3          chain     azorg01l                           53
317407161116     C     *in53         ifeq      *on
317408161116     C                   movel     msg(21)       w12msg
317409161116     C                   seton                                        28  90
317410161116     C                   goto      forw12
317411161116     C                   endif
317412161116      * N.ORM
317413161116     C                   MOVEL     'M'           WTRC
317414161116     C                   MOVEL(P)  SEL4          WN14             14
317415161116     C     kar44         chain     fiar404l                           54
317416161116     C     *in54         ifeq      *on
317417161116     C                   movel     msg(22)       w12msg
317418161116     C                   seton                                        28  90
317419161116     C                   goto      forw12
317420161116     C                   endif
317421161116      *
317422161116     C                   seton                                        25
317423161116      *
317424161116     C                   endif
317500150319     c* indirizzo email
317600150330     c                   if        v1ceml<>*blanks
317700150330     c                   Clear                   dsemail
317800150330     c                   Eval      §emlindi = v1ceml
317900150330     c                   Call      'XEMAIL'
318000150330     c                   Parm                    dsemail
318100150330      * Errore
318200150330     c                   If        §emlerro <> '0'
318300150330     C                   MOVEL     MSG(36)       w12MSG
318400150330     C                   SETON                                        286590
318500150330     C                   GOTO      forw12
318600150330     c                   endif
318700150330     c                   endif
318800161013     c* numero cellulare  / telefono destinatario
318900150330     c                   if        v1ctel<>*blanks
319000150330      /free
319100150330       pInCell = %trim(v1ctel);
319200150330       pOutErr = *blanks;
319300150330       if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
319400150330           // Numero di cellulare KO
319500161013           //w12msg=msg(37);
319600161013           //*in28=*on;
319700161013           //*in92=*on;
319800161013           //*in90=*on;
319900161013           // /end-free
320000161013     C*                   GOTO      forw12
320100161013           // /free
320200161013           // endif;
320201161013       else;
320202160909           // Memorizzo il numero di cellulare normalizzato
320203160909           w_v1ctel=pOutCell;
320204161013       endif;
320300150330      /end-free
320400150330     c                   endif
320401180308     c                   exsr      sr_IndAP
321600131114     C*
321700131114     C                   ENDSR
321701180308     C**************************************************************************
321702180308     C* Impostazione indicatore per evidenziare scritta "F5=AltreParz"
321703180308     C**************************************************************************
321704180308     C     sr_IndAP      BEGSR
321705180308     c****               if        v1cgia<>' ' or v1cdan<>' ' or v1caff<>'N'
321706180308     c                   if        v1cgia<>' ' or v1cdan<>' '
321707180308     c                             or v1cfbr<>' ' or v1ccbo<>*blanks
321708180308     c****                         or v1cft1<>' '
321709180308     c                             or v1cksc>0 or v1crag<>*blanks
321710180308     c                             or v1ceml<>*blanks or v1ctel<>*blanks
321711180308     c                             or v1cref<>*blanks
321712180308     c                             or sel4<>0
321713180308     c                             or v1cstco<>*blanks
321714180308     c                   seton                                        09
321715180308     c                   else
321716180308     c                   setoff                                       09
321717180308     c                   endif
321718180308     C                   ENDSR
321800940802     C**************************************************************************
321900940802     C* EMISSIONE SUBFILE
322000940802     C**************************************************************************
322100900515     C     EMISS         BEGSR
322200940802     C*
322300930407     C                   SETOFF                                       1362
322400940714     C                   MOVEL     *BLANKS       V2CSCE
322500131121     c**                 if        *in87=*off and not *in02
322600131121     C**                 MOVEL     ARBLNP        V2CLIN
322700131121     c**                 endif
322800131121     c**                 if        *in87 or *in02
322900131121     C**                 MOVEL     ARBLNA        V2CLIN
323000131121     c**                 endif
323100940714     C                   MOVEL     ARBLNP        V2CLNP
323200941212     C                   MOVEL     ARBLNA        V2CLNA
323300161011     c                   eval      v2clin=%editc(arblnp:'X') + '/' +
323400161011     c                                    %editc(arblna:'X')
323500911023     C                   MOVEL     ARBMGS        MMGG
323600940714     C                   MOVEL     GG            V2CDSP
323700940714     C                   MOVE      MM            V2CDSP
323800940714     C                   MOVE      ARBAAS        V2CAAS
323900950114     C                   MOVEL     ARBNSP        V2CNSP
324000900516     C*
324100060130     C                   MOVEL     ARBksc        V2Cksc
324200060130     C                   MOVEL     ARBcbo        V2Ccbo
324300060130     C                   MOVEL     ARBRSM        V2CRSM
324400160311     C                   MOVEL     ARBRSM        V2CRSMa
324500940714     C                   MOVEL     ARBRSD        V2CRSD
324600160311     C                   MOVEL     ARBRSD        V2CRSDa
324700131114     c                   if        v1cind=*blanks
324800131114     c                   eval      v2cdest=arblod
324900160311     c                   eval      v2cdesta=arblod
325000131114     c                   else
325100131114     c                   eval      v2cdest=arbind
325200160311     c                   eval      v2cdesta=arbind
325300131114     c                   endif
325400131114     c                   if        arbrma<>*blanks
325500131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z')) +'/'+
325600131114     c                                     %trim(arbrma)
325700131114     c                   else
325800131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z'))
325900131114     c                   endif
326000160314     c                   eval      v2crifn=arbrmn
326100160314     c                   eval      v2crifa=arbrma
326200131114     C**                 Z-ADD     ARBRMN        V2CRMN
326300131114     C**                 MOVEL     ARBRMA        V2CRMA
326400940714     C                   MOVEL     ARBNRS        V2CNRS
326500940923     C                   MOVEL     ARBFLS        V2CFLS
326600940714     C                   MOVEL     *BLANKS       V2CFLA
326700990119     C* SPEDIZIONE IN DANNO
326800990119     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
326900990119     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
327000990119     C                   SELECT
327100990119     C     V1CDAN        WHENEQ    'C'
327200161027     C                   MOVEL     'Dc'          V2CGCD
327300990119     C     V1CDAN        WHENEQ    'A'
327400161027     C                   MOVEL     'D*'          V2CGCD
327500990119     C**
327600990119     C                   OTHER
327700040809      *
327800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
327900040809      * legate alla spedizione che passo
328000040809     c                   clear                   fidn12ds
328100040809     c                   eval      i12aas = arbAAS
328200040809     c                   eval      i12lnp = arbLNP
328300040809     c                   eval      i12nrs = arbNRS
328400040809     c                   eval      i12nsp = arbNSP
328500040809     c                   eval      i12fel = 'E'
328600040809     c                   eval      i12fan = 'E'
328700040809     c*
328800040809     c                   call      'FIDN12R'
328900040809     c                   parm                    fidn12ds
329000040809     c*
329100040809     c                   setoff                                       31
329200040809     c*
329300040809     c* se non ci sono errori
329400040809     c                   if        o12err <> ' '
329500040809     c                   seton                                        31
329600040809     c                   else
329700040809     c* se numero di CA trovate maggiore di zero
329800040809     c                   if        o12nca = 0
329900040809     c                   seton                                        31
330000040809     c                   else
330100040809     c                   z-add     1             ii                2 0
330200040809     c                   dow       ii <= o12nca and *in31 = *off
330300040809     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
330400040809     C     skdch(ii)     IFEQ      0
330500161013     C                   MOVEL     'D*'          V2CGCD
330600040809     C                   SETON                                            31
330700040809     C                   ELSE
330800040809     C* CHIUSA MA VADO AVANTI A LEGGERE
330900161013     C                   MOVEL     'Dc'          V2CGCD
331000040809     C                   ENDIF
331100040809     c*
331200040809     c                   add       1             ii
331300040809     c                   enddo
331400040809     c*
331500040809     c                   seton                                        31
331600040809     c*
331700040809     c                   endif
331800040809     c                   endif
331900990119     C**
332000990119     C                   ENDSL
332100921102     C* SEGNACOLLI
332200930702     C                   CLEAR                   SEG
332300921102     C                   MOVEL     ARBNCD        COMSCN            7
332400921102     C                   MOVEA     COMSCN        SEG(1)
332500921102     C     ARBNCA        IFNE      0
332600921102     C                   MOVEL     ARBNCA        COMSCN
332700921102     C                   MOVEA     COMSCN        SEG(9)
332800921102     C                   END
332900921102     C                   MOVEA     '/'           SEG(8)
333000921102     C* ELIMINO ZERO NON SIGNIFICATIVI
333100921102     C                   Z-ADD     1             Y
333200921102     C     SEG(Y)        DOWEQ     '0'
333300921102     C                   MOVE      ' '           SEG(Y)
333400921102     C                   ADD       1             Y
333500921102     C                   END
333600921102     C*
333700921102     C     ARBNCA        IFNE      0
333800921102     C                   Z-ADD     9             Y                 2 0
333900921102     C     SEG(Y)        DOWEQ     '0'
334000921102     C                   MOVE      ' '           SEG(Y)
334100921102     C                   ADD       1             Y
334200921102     C                   END
334300921102     C                   END
334400940714     C                   MOVEA     SEG           V2CSEG
334500911023     C*
334600911023     C* GIACENZA APERTA
334601161027     c* Se c'è c.a. vince quest'ultima sulle giacenze
334602161027     c                   if        v2cgcd=*blanks
334700941202     C                   Z-ADD     ARBAAS        KAAS
334800940713     C                   Z-ADD     ARBLNP        KLNP
334900940713     C                   Z-ADD     ARBNRS        KNRS
335000940713     C                   Z-ADD     ARBNSP        KNSP
335100050405     C     KGCA1         CHAIN     tigcp21l                           31
335200920410     C  N31GCPATB        IFEQ      ' '
335300920410     C     GCPDCG        IFNE      0
335400161011     C                   MOVE      'Gc'          V2CGCd
335500911023     C                   ELSE
335600161011     C                   MOVE      'G*'          V2CGCd
335700900515     C                   END
335800920410     C                   END
335801161027     c                   endif
335802160609     c                   if        arbtsp <>'C' and arbtsp<>'D'
335900990119     C                   MOVEL     ARBTSP        V2CTSP
335901160609     c                   else
335902160609     c                   clear                   v2ctsp
335903160609     c                   endif
336000930503     C*
336100161012     C* Stato Spedizione
336200940714     C                   MOVEL     *BLANKS       V2CCON
336202161012     c                   exsr      sr_StatoSped
336400160314     C***                MOVEL     *BLANKS       V2CFCC
357500911023     C*
357600940714     C                   MOVEL     ARBTMC        V2CTMC
357700900522     C*
357800900518     C                   ADD       1             B
357801161124     c                   if        nr2=*hival
357802161124     c                   eval      wsflpieno='1'
357803161124     c                   else
357900940713     C                   ADD       1             NR2
358000160311     C                   IF        dsp_mod = 1
358100160311     C                   WRITE     LRI6S02A
358200160311     c                   else
358300131107     C                   WRITE     LRI6S02
358400160311     c                   endif
358401161124     c                   endif
358500940720     C*
358600900515     C                   ENDSR
358601161117     C**************************************************************************
358602161117     C* Verifica se spedizione in distinta è consegnata se arbdcm=0
358603161117     C**************************************************************************
358604161117     C     sr_VerCon     BEGSR
358605161117     c                   clear                   w_fdcons
358606161117     c                   clear                   w_cev
358607161117     c                   clear                   w_dev
358608161117     c* Fermo Deposito in distinta
358609161117    3c                   if        arbffd='S'
358610161117     C                   movel     arbPdc        kpdpdr
358611161117     C                   move      'A'           kpdtip
358612161117     C     kfapd         chain     fiapd01L
358613161117    4c                   if        %found(fiapd01l) and apdpdd='S'
358614161117     c                   eval      w_fdcons='S'
358615161117    4c                   endif
358616161117    3c                   endif
358617161117     c* Sped. in distinta (no FD oppure distinta non fittizia)
358618161117    3c                   if        arbffd<>'S' or apdpdd<>'S'
358619161117     c                   move      arbndc        pctndc
358620161117     c                   move      arbifp        pctfgs
358621161117     c                   move      'CET'         ttrd
358622161117     c     kcet          setgt     fipct02l
358623161117     c     kcet          readpe    fipct02l
358624161117    4c                   if        not %eof(fipct02l)
358625161117     c                   movel     pctdati       fipctcetds
358626161117     c* consegnata
358627161117    5c                   if        §pctcmc = ' '
358628161117     c                   MOVE      '  C'         w_cev
358629161117     c                   else
358630161117     c* mancata consegna
358631161117     c                   movel     §PCTcmc       w_cev
358632161117    5c                   endif
358633161117     c                   z-add     §pctdata      w_dev
358634161117    4c                   endif
358635161117    3c                   endif
358636161117     c                   endsr
358637161012     C*
358638161012     C**************************************************************************
358639161012     C* Impostazione campo sfl "STATO SPEDIZIONE"
358640161012     C**************************************************************************
358641161012     C     sr_StatoSped  BEGSR
358642161117     c                   clear                   w_fdcons
358643161117     c                   clear                   w_cev
358644161117     c                   clear                   w_dev
358645161013     c                   clear                   v2hi62
358646161012
358647161012     c* OPERAZIONI PRELIMINARI
358648161012     c* Data corrente
358649161012     c                   eval      wdata=%dec(%date())
358650161012     c* Verifico presenza dati da PDA
358651161013     c* . . . Se spedizione in distinta, non consegnata e senza consegna anomala
358652161012    1c                   if        arbdcm=0 and arbcca=*blanks and arbndc>0
358653161117     c                   exsr      sr_VerCon
358654161012    1c                   endif
358655161012
358656161012     c* chaino record contatti
358657161013     c                   if        arbdcm=0
358658161012     c                   clear                   dsbl4l
358659161012     C                   Z-ADD     ARBAAS        KAAS
358660161012     C                   Z-ADD     ARBLNP        KLNP
358661161012     C                   Z-ADD     ARBNRS        KNRS
358662161012     C                   Z-ADD     ARBNSP        KNSP
358663161012     c                   eval      ktrc='L'
358664161012     c     kar41         chain     fiar401l
358665161012     c                   if        %found(fiar401l)
358666161012     c                   eval      dsbl4l=ar4not
358667161012     c                   endif
358668161013     c                   endif
358669161012     c* reperisco network della lna e memorizzo se bolla non export
358670161012     c                   clear                   w_ntwlna
358671161012     c                   clear                   w_lnaIt
358672161012     c     arblna        chain     azorg01l
358673161012     c                   if        %found(azorg01l)
358674161012     c                   eval      og143=orgde3
358675161012     c                   eval      w_ntwlna=§ogntw
358676161012     c                   endif
358677161012     c                   if        w_ntwlna<>'EEX' and w_ntwlna<>'DPD' and
358678161012     c                             w_ntwlna<>'FED'
358679161012     c                   eval      w_lnaIt='S'
358680161012     c                   endif
358681161018     c* Ds3a
358682161018     c                   eval      cod='3A'
358683161018     c                   eval      key=arbcbo
358684161018     c                   clear                   ds3a
358685161018     c     ktab1         chain     tabel00f
358686161018     c                   if        %found(tabel00f)
358687161018     c                   eval      ds3a=tbluni
358688161018     c                   endif
358689161018     c
358690161012     c* IMPOSTA CAMPO "STATO SPEDIZIONE"
358691161012     c                   select
358692161012     c* 1) Spedizione consegnata, no F.D., NO consegna anomala
358693161013     c                   when      arbdcm>0 and arbffd<>'S' and arbcca =*blanks
358694161012     c                   z-add     arbdcm        wdcm
358695161012     c                   move      arbhmc        whmc
358696161012     c                   exsr      sr_ImpoDtOraCo
358697161012     c                   when      arbffd<>'S' and arbcca =*blanks and
358698161012     c                             arbdcm=0 and w_cev='  C' and w_dev>0
358699161012     C                   MOVE      w_dev         wdcm
358700161012     c                   movel     §pctora       whmc
358701161012     c                   exsr      sr_ImpoDtOraCo
358702161013     c* 2) Spedizione con consegna anomala
358703161013     c                   when      arbcca<>*blanks
358704161116     C                   Z-ADD     1             Cc                3 0
358705161116     C     ARBCCA        LOOKUP    KCA(Cc)                                31
358706161116     C   31              MOVEL     DCA(Cc)       DS7O
358707161012     C*
358708161012     C*        NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
358709161012     C   31§7OVCA        IFEQ      'S'
358710161012     C     §7OVCA        ORNE      'S'
358711161012     C     ARBDCM        ANDEQ     *ZERO
358712161012     c                   if        §7odeb<>*blanks
358713161012     C                   MOVEL     §7ODEb        V2CCON
358714161012     c                   else
358715161012     C                   MOVEL     §7ODEi        V2CCON
358716161012     c                   endif
358717161012     C                   SETON                                        62
358718161013     C                   eval      v2hi62='1'
358719161012     C                   ELSE
358720161012     C*
358721161012     C     ARBDCM        IFNE      *ZERO
358722161012     c                   z-add     arbdcm        wdcm
358723161012     c                   move      arbhmc        whmc
358724161012     c                   exsr      sr_ImpoDtOraCo
358725161012     C                   ENDIF
358726161012     C                   ENDIF
358727161012     c* 3) Spedizione in FD consegnata (dcm>0 o distinta fittizia)
358728161012     c                   when      arbffd='S' and
358729161013     c                             (arbdcm>0 or w_fdcons='S' )
358730161012     c                   if        arbdcm>0
358731161012     c                   z-add     arbdcm        wdcm
358732161012     c                   move      arbhmc        whmc
358733161012     c                   exsr      sr_ImpoDtOraCo
358734161012     c                   else
358735161012     c                   z-add     arbddc        wdcm
358736161012     c                   clear                   whmc
358737161012     c                   exsr      sr_ImpoDtOraCo
358738161012     c                   endif
358739161012     c* 4) Spedizione "Da Partire"
358740161012     c                   when      w_arr =*blank
358741161031     c****                         and (arbft2=' ' or arbft2='N')
358742161031     c                             and arbft1=' '
358743161012     c                   eval      v2ccon='Da Partire'
358744161012     c* 5) Spedizione con C.A. aperta
358745161013     c                   when      v2cgcd='D*'
358746161012     c                   eval      v2ccon='C.A.'
358747161013     c                   seton                                        62
358748161013     C                   eval      v2hi62='1'
358749161012     c* 6) Blocco "B"
358750161012     c                   when      ARBFBC='B'
358751161012     c                   eval      v2ccon='BLOCCO'
358752161012     c                   seton                                        62
358753161013     C                   eval      v2hi62='1'
358754161012     c* 7) Blocco Giacenza
358755161012     c                   when      ARBFBC<> *blanks and arbfbc <> 'B' and
358756161012     c                             arbfbc<>'A' and arbfbc<>'*'
358757161012     c                   eval      v2ccon='GIAC. '
358758161012     c                   seton                                        62
358759161013     C                   eval      v2hi62='1'
358760161012     c* 8) Lasciato avviso
358761161012     c                   when      ARBFBC='A'
358762161012     c*       verifico il contatto
358763161012     c     karb2         setgt     fnevb22l
358764161012     c     karb2         readpe    fnevb22l
358765161012     c
358766161012     c                   if        not %eof(fnevb22l) and (evb2tel='S'
358767161012     c                             or  evb2tel='T')
358768161012     C                   eval      v2ccon='L.AVV.   *'
358769161012     c                   seton                                        62
358770161013     C                   eval      v2hi62='1'
358771161012     c                   else
358772161012     C                   eval      v2ccon='L.AVV.'
358773161012     c                   endif
358774161012     c* 9) Sospensione consegna
358775161012     c                   when      §b4lbge='S'
358776161012     C                   eval      v2ccon='BLOCCO'
358777161012     c                   seton                                        62
358778161013     C                   eval      v2hi62='1'
358779170119     c*    Bolle solo in partenza ma non "Da partire" (sono i recuperi
358780170119     c*    già scritti nel membro di trasmissione ma in attesa di
358781170119     c*    andare su arb con la prossima chiusura F.V. ma potrebbe
358782170119     c*    succedere anche in generale in caso di mancata ricezione)
358783170119     c*    in questo caso lo stato spedizione rimarrà vuoto
358784170119     c                   when      w_arr=' '
358785161013     c* Da qui in poi solo per bolle NON Export
358786161012     c                   when      w_lnaIt='S'
358787161012     c                   select
358788161012     c*10) Fermo Deposito da contattare/contattato
358789161012     c                   when      arbffd='S'
358790161012     c                   if        §b4ltfd='S' or
358791161012     c                             §b4ltfd='T'
358792161013     C                   eval      v2ccon='F.Dep    *'
358793161012     c                   seton                                        62
358794161013     C                   eval      v2hi62='1'
358795161012     c                   else
358796161013     C                   eval      v2ccon='F.Dep'
358797161012     c                   endif
358798161012     c* 11) Appuntamenti  e supermercati da contattare
358799161012     c* Appuntamento
358800161017     c                   when      (arbtc1='A' or arbtc2='A')  and
358801161012     c                             (§b4lbas='S' or §b4lbas='T')
358802161012     C                   eval      v2ccon='App      *'
358803161012     c                   seton                                        62
358804161013     C                   eval      v2hi62='1'
358805161012     c* Supermercato
358806161017     c                   when      (arbtc1='S' or arbtc2='S')  and
358807161012     c                             (§b4lbas='S' or §b4lbas='T')
358808161012     C                   eval      v2ccon='Sup      *'
358809161012     c                   seton                                        62
358810161013     C                   eval      v2hi62='1'
358811161012     c* 12) C/assegno da contattare
358812161012     c                   when      arbacc=' '
358813161012     C                   eval      v2ccon='C/Ass    *'
358814161012     c                   seton                                        62
358815161013     C                   eval      v2hi62='1'
358816161012     c* 13) Spedizione in consegna con evento di mancata consegna da PDA
358817161012     c                   when      w_cev<>'  C' and w_cev<>*blanks
358818161012     C                   MOVEL     '2A'          COD
358819161012     C                   MOVEL(p)  w_cev         KEY
358820161012     c                   clear                   ds2a
358821161012     C     KTAB1         CHAIN     TABEL00F
358822161012    3c                   if        %found(tabel00f)
358823161012     c                   eval      ds2a=tbluni
358824161012    3c                   endif
358825161012     C                   SETON                                        62
358826161013     C                   eval      v2hi62='1'
358827161012     c                   select
358828161012     c                   when      §2aftc='G'
358829161012     c                   eval      v2ccon='IN CONS G '
358830161012     c                   when      §2aftc='A'
358831161012     c                   eval      v2ccon='IN CONS LA'
358832161012     c                   when      §2aftc= *blank and §2atip='P'
358833161012     c                   eval      v2ccon='IN CONSPrz'
358834161012     c                   other
358835161018     c                   eval      v2ccon='IN CONS N '
358836161012     c                   endsl
358837161012     c* 14) In consegna con o senza MIC
358838161012     c                   when      arbndc>0
358839161012     c                   SETON                                        13
358840161012     c* cerco evento MIC per stabilire se effettivamente in consegna
358841161012     c                   exsr      sr_MIC
358842161013     c                   if        wmic  ='s'
358843161012     C                   eval      v2ccon='IN CONS   '
358844161012     c                   else
358845161012     C                   eval      v2ccon='IN CONS  *'
358846161013     c                   seton                                        62
358847161013     C                   eval      v2hi62='1'
358848161012     c                   endif
358849161012     c* 15) Consegna richiesta se > di udate
358850161018     c                   when      (arbtcr<>'D' and arbdcr>wdata) or
358851161018     c                             (arbtcr='D' and arbdcr>=wdata)
358852161018     C                   eval      v2ccon='Rich' + arbtcr +
358853161012     c                             %subst(%editc(arbdcr:'X'):7:2)+ '/' +
358854161012     c                             %subst(%editc(arbdcr:'X'):5:2)
358855161013     c                   seton                                        62
358856161013     C                   eval      v2hi62='1'
358857161012     c                   endsl
358858161012     c                   endsl
358859161012     C*
358973161012     c                   endsr
358974161011      **************************************************************************
358975161011      *     Imposta la dat/ora consegna nella colonna dello stato spedizione
358976161011      **************************************************************************
358977161011     C     sr_ImpoDtOraCoBEGSR
358978161011     C                   MOVE      wdcm          G02INV
358979161011     C                   Z-ADD     *ZEROS        G02DAT
358980161011     C                   MOVEL     '3'           G02ERR
358981161011     C                   CALL      'XSRDA8'
358982161011     C                   PARM                    WLBDAT
358983161011     C                   MOVE      G02DAT        $COMOD            8
358984161011     c                   eval      v2ccon =%subst($comod:1:2) + '/' +
358985161011     c                                     %subst($comod:3:2)
358986161011     c                   if        arbffd<>'S'
358987161011     c* Spedizione consegnata "normale" --> "GG/MM hh:mm"
358988161018     c                   if        whmc<>*blanks and whmc<>'0000' and
358989161018     c                             §3arbl<>'R'
358990161013     c                   eval      v2ccon=%trim(v2ccon) + ' ' + whmc
358991161013     c                   endif
358992161011     c                   else
358993161011     c* Fermo Deposito --> "GG/MMFDep"
358994161013     c                   eval      v2ccon=%trim(v2ccon) + ' F.D.'
358995161011     c                   endif
358996161011     c                   endsr
358997161011      **************************************************************************
358998161011      *     Cerca evento mic per impostazione stato spedizione
358999161011      **************************************************************************
359000161011     C     sr_MIC        BEGSR
359001161011     C                   clear                   wmic
359002161011     c                   eval      kcev='MIC'
359003161011     c                   eval      kdev=arbddc
359004161011     C     KEYEVB        SETLL     FNEVB04L
359005161011     C     keyevb        READe     FNEVB04L
359006161011    1C                   DOW       not %eof(fnevb04l) and wmic=' '
359007161011    2C                   if        evbatb<>'A' and
359008161011     c                             (evbnot=*blanks or
359009161011     c                              %int(%subst(evbnot:1:7))=arbndc )
359010161011     c                   eval      wmic='s'
359011161011    2c                   endif
359012161011     c
359013161011     C     keyevb        READe     FNEVB04L
359014161011    1C                   ENDDO                                                  <-- 1 ---
359015161011     c                   endsr
359016940408     C*
359017940321      **************************************************************************
359018940713      *     R O U T I N E      I N I Z I A L E
359019940713      **************************************************************************
359100940713     C     *INZSR        BEGSR
359200940713      *
359300940713     C     KARB          KLIST
359400940714     C                   KFLD                    V2CAAS
359500940713     C                   KFLD                    COMLNP            3 0
359600940714     C                   KFLD                    V2CNRS
359700940714     C                   KFLD                    V2CNSP
359800940713     C     KARB2         KLIST
359900940713     C                   KFLD                    ARBAAS
360000940713     C                   KFLD                    ARBLNP
360100940713     C                   KFLD                    ARBNRS
360200940713     C                   KFLD                    ARBNSP
360300971223     C     KARB4         KLIST
360400971223     C                   KFLD                    AR4AAS
360500971223     C                   KFLD                    AR4LNP
360600971223     C                   KFLD                    AR4NRS
360700971223     C                   KFLD                    AR4NSP
360701171023     C     KEYRPE        KLIST
360702171023     C                   KFLD                    K_AAS
360703171023     C                   KFLD                    K_LNP
360704171023     C                   KFLD                    K_NRS
360705171023     C                   KFLD                    K_NSP
360800171023     C*    KAR4W         KLIST
360900171023     C*                  KFLD                    wAR4AAS
361000171023     C*                  KFLD                    wAR4LNP
361100171023     C*                  KFLD                    wAR4NRS
361200171023     C*                  KFLD                    wAR4NSP
361300940713     C     KLNPX         KLIST
361400940713     C                   KFLD                    V1CAAS
361500940713     C                   KFLD                    V1CLP2
361600940713     C                   KFLD                    V1CNRS
361700940713     C                   KFLD                    V1CNSP
361800940713     C     K1ARB         KLIST
361900020910     c                   Kfld                    Wifp
362000940713     C                   KFLD                    NDC               7 0
362100940713     C                   KFLD                    NSS               3 0
362200940713     C                   KFLD                    COMLNA            3 0
362300940713     C                   KFLD                    COMAAD
362400940713     C                   KFLD                    COMMGD
362401161115     C                   KFLD                    COMLP1
362500161115     C***                KFLD                    V1CLP1
362600940713     C     K2ARB         KLIST
362700020910     c                   Kfld                    Wifp
362800940713     C                   KFLD                    NDC
362900940713     C                   KFLD                    NSS
363000940713     C                   KFLD                    COMLNA
363100940713     C                   KFLD                    COMAAD
363200940713     C                   KFLD                    COMMGD
363300940713     C     K3ARB         KLIST
363400020910     c                   Kfld                    Wifp
363500940713     C                   KFLD                    NDC
363600940713     C                   KFLD                    NSS
363700940713     C                   KFLD                    COMLNA
363800940713     C     K4ARB         KLIST
363900940713     C                   KFLD                    COMLNA
364000940713     C                   KFLD                    COMDTD
364100161115     C                   KFLD                    comlp1
364101161115     C****               KFLD                    V1CLP1
364200940713     C     K5ARB         KLIST
364300940713     C                   KFLD                    COMLNA
364400940713     C                   KFLD                    COMDTD
365800150326     C     K6blp         KLIST
365900161115     C                   KFLD                    COMLP1
366000150326     C                   KFLD                    COMAAD
366100150326     C                   KFLD                    COMMGD
366200150326     C                   KFLD                    V1CPSD
366300150326     C                   KFLD                    COMSCD
366400150326     C     K6blp1        KLIST
366500161115     C                   KFLD                    COMLP1
366600150326     C                   KFLD                    COMAAD
366700150326     C                   KFLD                    COMMGD
366800150326     C                   KFLD                    V1CPSD
366900150326     C     K7blp         KLIST
367000161115     C                   KFLD                    COMLP1
367100940713     C                   KFLD                    COMAAD
367200940713     C                   KFLD                    COMMGD
368000150318     C     K8blp         KLIST
368100150318     C                   KFLD                    V1CLNA
368200150318     C                   KFLD                    COMAAD
368300150318     C                   KFLD                    COMMGD
371300971223     C     KAR4          KLIST
371400971223     C                   KFLD                    WTRC
371500971223     C                   KFLD                    WN30
371600010508     C     KAR44         KLIST
371700010508     C                   KFLD                    WTRC
371800010508     C                   KFLD                    WN14
371900940713     C     KART          KLIST
372000940713     C                   KFLD                    ARBAAS
372100940713     C                   KFLD                    ARBLNP
372200940713     C                   KFLD                    ARBNRS
372300940713     C                   KFLD                    ARBNSP
372400940713     C                   KFLD                    V1CSCD
372500940713     C*
372600940713     C     KTAB1         KLIST
372700940713     C                   KFLD                    CODUT
372800940713     C                   KFLD                    COD
372900940713     C                   KFLD                    KEY
373000940713     C     KTAB2         KLIST
373100940713     C                   KFLD                    CODUT
373200940713     C                   KFLD                    COD
374900940713     C*
375000940719     C     KAR41         KLIST
375100940719     C                   KFLD                    KAAS
375200940719     C                   KFLD                    KLNP
375300940719     C                   KFLD                    KNRS
375400940719     C                   KFLD                    KNSP
375500940802     C                   KFLD                    KTRC
376100021213     C*
376200021213     C     KAR51         KLIST
376300021213     C                   KFLD                    KAAS
376400021213     C                   KFLD                    KLNP
376500021213     C                   KFLD                    KNRS
376600021213     C                   KFLD                    KNSP
376700021213     C                   KFLD                    KTRd              3
376800940719     C*
376900991026     C     KAR61         KLIST
377000940713     C                   KFLD                    KAAS
377100940713     C                   KFLD                    KLNP
377200940713     C                   KFLD                    KNRS
377300940713     C                   KFLD                    KNSP
377400991026     C                   KFLD                    KTRC
377500991026     C*
378100940713     C*
378200990715     C     KGCA1         KLIST
378300941202     C                   KFLD                    KAAS
378400940713     C                   KFLD                    KLNP
378500940713     C                   KFLD                    KNRS
378600940713     C                   KFLD                    KNSP
378700940713     C*
379300131114     C     Kcet          KLIST
379400140224     C                   KFLD                    pctfgs
379500131114     C                   KFLD                    pctNDC
379600131114     C                   KFLD                    arbpdc
379700131114     C                   KFLD                    TTRD              3
379800131114     C                   KFLD                    arbAAS
379900131114     C                   KFLD                    arbLNP
380000131114     C                   KFLD                    arbNRS
380100131114     C                   KFLD                    arbNSP
380101161011     c     keyevb        klist
380102161011     C                   KFLD                    ARBAAS
380103161011     C                   KFLD                    ARBLNP
380104161011     C                   KFLD                    ARBNRS
380105161011     C                   KFLD                    ARBNSP
380106161011     C                   KFLD                    kcev
380107161011     C                   KFLD                    kdev
380200950324     C*
381100131108     c
382000160502     C     Kfapd         klist
382100160502     C                   kfld                    kpdtip
382200160502     C                   kfld                    kpdpdr
382300131111     C     KarS          KLIST
382500131111     C                   KFLD                    KKTRC             1
382501161014     C                   KFLD                    V1CNOT
382600131111     C                   MOVE      'C'           KKTRC
382700131111     C     KarSXT        KLIST
382800131111     C                   KFLD                    arSFLS
382900131111     C                   KFLD                    arSLNA
383000131111     C                   KFLD                    arSNRS
383100131111     C                   KFLD                    arSNSC
383200131111     C     KartXP        KLIST
383300131111     C                   KFLD                    arTAAS
383400131111     C                   KFLD                    arTLNP
383500131111     C                   KFLD                    arTNRS
383600131111     C                   KFLD                    arTNSP
383601161014     C     KART1         KLIST
383602161014     C                   KFLD                    ARBAAS
383603161014     C                   KFLD                    ARBLNP
383604161014     C                   KFLD                    ARBNRS
383605161014     C                   KFLD                    ARBNSP
383607161014     C     KarS1         KLIST
383608161014     C                   KFLD                    artFLS
383609161014     C                   KFLD                    artLNA
383610161014     C                   KFLD                    artNRS
383611161014     C                   KFLD                    artNSC
383612161014     C                   KFLD                    KKTRC             1
383700040218      *
384700940713     C*
384800940713     C*-------------------------------------------------------------------------
384900940713     C*
385000991222     C     *LIKE         DEFINE    ARBAAS        WAAS
385100991027     C     *LIKE         DEFINE    ARBLNP        WLNP
385200991027     C     *LIKE         DEFINE    ARBNRS        WNRS
385300991027     C     *LIKE         DEFINE    ARBNSP        WNSP
385400160315     C**   *LIKE         DEFINE    FVVNPG        WNPG
385500160315     C**   *LIKE         DEFINE    FVVNFV        WNDC
385700991027     C     *LIKE         DEFINE    V1CSCD        COMSCD
385900971219     C     *LIKE         DEFINE    ARBAAS        KAAS
386000160315     C**   *LIKE         DEFINE    EVBAAS        KAAS$
386100940713     C     *LIKE         DEFINE    ARBLNP        KLNP
386200940713     C     *LIKE         DEFINE    ARBNRS        KNRS
386300940713     C     *LIKE         DEFINE    ARBNSP        KNSP
386400961112     C     *LIKE         DEFINE    ARBFBR        WFBR
386500940713     C     *LIKE         DEFINE    TBLCOD        COD
386600940713     C     *LIKE         DEFINE    TBLKEY        KEY
386700940713     C     *LIKE         DEFINE    ARBAAS        $AAS
386800940713     C     *LIKE         DEFINE    ARBMGS        $MGS
386900940719     C     *LIKE         DEFINE    AR4TRC        KTRC
387100971223     C     *LIKE         DEFINE    AR4TRC        WTRC
387200060130     C*-------------------------------------------------------------------------
387300060130     c
387400060130     c                   movel     kpjbu         savkpjbu
387500060130      *
387600060130     c     *dtaara       define    §azute        azuteds
387700060130     c     *dtaara       define    §datiute      ddatiute
387800060130     c                   in(E)     *dtaara
387900060130    1c                   If        %error  or RSUT = *blanks
388000060130     c                   Clear                   Tibs34ds
388100060130     c                   Call      'TIBS34R'
388200060130     c                   Parm                    Tibs34ds
388300060130     c                   In        *dtaara
388400060130    1c                   EndIf
388500060130
388600060130     c                   Clear                   wabi
388700060130     c                   Clear                   v1cmsg
388800060130     c                   Clear                   dLat
388900060130
389000060130      * Verifica errori e autorità profilo
389100060130s   1c                   Select
389200060130      * se ho errori nei dati utente esco dal pgm
389300060130w   1c                   When      DutErr = 'E'
389400060130     c                   GoTo      Fine
389500060130      * se non c'è l'abilitazione
389600060130      * --> se 1° livello, abilitazioni al terminal
389700060130      *     se 2° livello, abilitazioni al punto operativo
389800060130      *     se sede è impossibile ma se capita mando a fine il pgm
389900060130w   1c                   When      UteAut = *Blanks
390000060130if  2c                   If        DutLpo = '1'
390100060130     c                   Eval      wabi   = 'TP'
390200060130e   2c                   EndIf
390300060130if  2c                   If        DutLpo = '2'
390400060130     c                   Eval      wabi   = 'PO'
390500060130e   2c                   EndIf
390600060130if  2c                   If        DutLpo = 'S'
390700060130     c                   GoTo      Fine
390800060130e   2c                   EndIf
390900060130      * carica le abilitazioni del profilo
391000060130x   1c                   Other
391100060130     c                   Movel     UteFaf        Dute01
391200060131     c                   If        §Utembf <> *Blanks
391300060131     c                   Eval      wabi = §Utembf
391400060130     c                   Else
391500060130     c                   Eval      wabi = UteAut
391600060130     c                   EndIf
391700060130e   1c                   EndSl
391800060130
391900060130      * controllo se ok l'abilitazione dell'utente
392000060130     c                   Clear                   Tibs02ds
392100060130     c                   Eval      T02Mod = 'C'
392200060130     c                   Eval      T02Sif = knsif
392300060130     c                   Eval      T02Cod = 'LAT'
392400060130     c                   Movel(p)  wabi          T02Ke1
392500060130     c                   Call      'TIBS02R'
392600060130     c                   Parm                    kpjba
392700060130     c                   Parm                    Tibs02ds
392800060130if  1c                   If        T02Err = *Blanks
392900060130     c                   Eval      dLat = T02Uni
393000060130e   1c                   EndIf
393100060130      * errore o non abilitato
393200060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
393300060130     c                   Seton                                        28
393400060130     C                   movel     msg(26)       v1cmsg
393500060130   x1c                   Else
393600060130      * Reperimento dei P.O. gestibili dall'utente
393700060130     c                   clear                   TRUL31DS
393800060130     c                   clear                   TRUL31DS2
393900060130     c                   eval      I31abi = wabi
394000060130     c                   eval      I31cdi = DUTdis
394100060130     c                   eval      I31car = DUTare
394200060130     c                   eval      I31cpo = DUTpou
394300060130     c                   call      'TRUL31R'
394400060130     c                   parm                    KPJBA
394500060130     c                   parm                    TRUL31DS
394600060130     c                   parm                    TRUL31DS2
394700060130if  2c                   if        O31pog > *zeros
394800060130     c                   movea     O31pog        $pog
394900060130     c                   movea     O31arg        $arg
395000060130     c                   movea     O31dig        $dig
395100060130x   2c                   else
395200060130     c                   Seton                                        28
395300060130     c                   movel     MSG(26)       v1cmsg
395400060130e   2c                   endif
395401161116      * Caricamento dei P.O. dell'area  - Serve per interrogazione per
395402161116      * lnp=999 (filiali dell'area)
395403161116     c                   clear                   $pogp
395405161116     c                   clear                   TRUL31DS
395406161116     c                   clear                   TRUL31DS2
395407161116     c                   eval      I31abi = 'AO'
395408161116     c                   eval      I31cdi = DUTdis
395409161116     c                   eval      I31car = DUTare
395410161116     c                   eval      I31cpo = DUTpou
395411161116     c                   call      'TRUL31R'
395412161116     c                   parm                    KPJBA
395413161116     c                   parm                    TRUL31DS
395414161116     c                   parm                    TRUL31DS2
395415161116if  2c                   if        O31pog > *zeros
395416161116     c                   movea     O31pog        $pogp
395417161116     c                   endif
395418161116      * Caricamento di tutti i p.o      - Serve per interrogare le
395419161116      * sped. da consegnare e le spedizioni consegnate se non inserita la lna
395420161116     c                   clear                   $poga
395421161116     c                   clear                   TRUL31DS
395422161116     c                   clear                   TRUL31DS2
395423161116     c                   eval      I31abi = 'AZ'
395424161116     c                   eval      I31cdi = DUTdis
395425161116     c                   eval      I31car = DUTare
395426161116     c                   eval      I31cpo = DUTpou
395427161116     c                   call      'TRUL31R'
395428161116     c                   parm                    KPJBA
395429161116     c                   parm                    TRUL31DS
395430161116     c                   parm                    TRUL31DS2
395431161116if  2c                   if        O31pog > *zeros
395432161116     c                   movea     O31pog        $poga
395433161116     c                   endif
395500060130    1c                   endif
395600060130     c
395700060130     c                   movel     savkpjbu      kpjbu
395800971223     C*
395900971223     C* IMPOSTAZIONE CAMPI FISSI
396300950307     C                   MOVEL     *ZERO         COMLAN            3
396400160315     C**                 Z-ADD     4             WNPG
396500940713      *
396600940713     C                   Z-ADD     *ZERO         NR2               4 0
397000070403     c
397100070403     c* Verifico se profilo utente ha GEO attiva
397200131205     c* Per EDP sempre anche prr RAC999
397300131205     c                   if        %subst(knmus:1:3)='EDP' or
397400131205     c                             %subst(knmus:1:6)='RAC999'
397500070410     c                   eval      dutgeot='S'
397600070410     c                   else
397700070403     c     dutpou        chain     azorg01l
397800070403     c                   if        %found(azorg01l)
397900070403     c                   movel     orgde8        og148
398000070403     c                   else
398100070403     c                   clear                   og148
398200070403     c                   endif
398300070410     c
398400070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
398500070524     c                   movel     'S'           dutgeot           1
398600070410     c                   endif
398700070524     c                   endif
398800131007
398900131007      /free
399000131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
399100131007      /end-free
399400160311
399500160311     C* definisco se lo schermo dell'utente supporta o meno il 27X132
399600160311     C                   IF        not %open(FNLRU6DA)
399700160311     C                   EVAL      dsp_mod = QueryDisplayModeSupport
399800160311     C                              ('4':*omit :*omit :*omit)
399900160311     C                   IF        dsp_mod=0
399901161124     c                   z-add     18            w_nrigs
400000160311     C                   ELSE
400001161124     c                   z-add     20            w_nrigs
400100160311     C                   OPEN      FNLRU6DA
400200160311     C                   ENDIF
400300160311     C                   ENDIF
400400131007
400500940713      *
400600940713     C                   ENDSR
400700971223     C**
400800971223     C* ELABORAZIONE DI SINGOLA BOLLA --------------------------------*
400900971223     C     SINGOL        BEGSR
401000971223     C                   CLEAR                   WINIZ
401100971223     C                   CLEAR                   WFINE
401200971223     C*
401300971223     C* 97 ON - MANUTENZIONE BOLLA
401400060130    1C     *IN97         IFEQ      *ON
401500971223     C*
401600060224     c                   movel     arblna        w003a
401700060224     c     w003a         lookup    $pog                                   31
401800060130     c* Se la causale variazione bolle prevede anche il p.o. cliente
401900131111     c*    controllo anche questo
402000060130    2c                   if        not *in31 and d66pocl='S'
402100060130     c     arbcbo        lookup    cbodop                                 31
402200060130    3c                   if        not *in31
402300060130     c                   movel     arbksc        kscfil
402400060130   x3c                   else
402500060130     c* Se bolla doppia prendo il ksc dall'AR6
402600060130     C                   Z-ADD     ARBAAS        KAAS
402700060130     C                   Z-ADD     ARBLNP        KLNP
402800060130     C                   Z-ADD     ARBNRS        KNRS
402900060130     C                   Z-ADD     ARBNSP        KNSP
403000060130     C                   MOVEL     '2'           KTRC
403100060130     C     KAR61         CHAIN     FIAR6000                           31
403200060130if  4C                   IF          %FOUND(fiar601l)
403300060130     c                   movel     ar6ksc        kscfil
403400060130     c                   else
403500060130     c                   movel     999           kscfil
403600060130    4c                   endif
403700060130    3c                   endif
403800060130     c
403900060224     c                   movel     kscfil        w003a
404000060224     c     w003a         lookup    $pog                                   31
404100060130    2c                   endif
404200060130     c
404300060130     C     *IN31         IFEQ      *OFF
404400971223     C                   MOVEL     MSG(19)       V1CMSG
404500971223     C                   SETON                                        459028
404600150402     c* La manutenzione non è richiamabile da un altro pgm e quindi
404700150402     c* non gestisco la parte asteriscata e in caso di errore emetto
404800150402     c* la prima videata
404900150402     c****               if        sdsprm=1 or ILRU6SEL=' '
405000971223     C                   GOTO      FOR01
405100150402     c****               else
405200150402     c****               eval      olru6err='E'
405300150402     c****               eval      olru6msg=v1cmsg
405400150402     C****               GOTO      fine
405500150402     c****               endif
405600971223     C                   ELSE
405700971223     C*
405800991027     C                   CLEAR                   FNLR48DS
405900060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
406000060224     c*  di arrivo
406100060224     c                   clear                   fnlv55ds
406200060224     c                   movel     arblna        d55lin
406300060224     c                   movel     '6'           d55tpt
406400060224     C                   time                    wrkoggiiso
406500060224     C     *iso          move      wrkoggiiso    d55drf
406600060224     c                   call      'FNLV55R'
406700060224     c                   parm                    fnlv55ds
406800060224     c                   movel     d55tfa        d48fgs
406900060224     c
407000131203     c                   select
407100140117     C     v1cbarcode    whengt    0
407200140117     C                   MOVEL     ARbAAS        D48AAS
407300140117     C                   MOVEL     ARbLNP        D48LNP
407400140117     C                   MOVEL     ARbNRS        D48NRS
407500140117     C                   MOVEL     ARbNSP        D48NSP
407600131203     C     SEL2          whenne    0
407700980403     C                   MOVEL     V1CAAS        D48AAS
407800980403     C                   MOVEL     V1CLP2        D48LNP
407900980403     C                   MOVEL     V1CNRS        D48NRS
408000980403     C                   MOVEL     V1CNSP        D48NSP
408100131203     C                   other
408200111011     C*                  MOVEL     AR4AAS        D48AAS
408300111011     C*                  MOVEL     AR4LNP        D48LNP
408400111011     C*                  MOVEL     AR4NRS        D48NRS
408500111011     C*                  MOVEL     AR4NSP        D48NSP
408600111011     C                   MOVEL     v2cAAS        D48AAS
408700111011     C                   MOVEL     v2clnp        D48LNP
408800111011     C                   MOVEL     v2cNRS        D48NRS
408900111011     C                   MOVEL     v2cNSP        D48NSP
409000131203     C                   ENDsl
409100141010     c* routine per verifica dispo da esaminare
409200141010     c                   exsr      sr_wdwdc
409300141010
409400971223     C                   MOVEL     V1CCVB        D48CVB
409500971223     C                   MOVEL     ARBCBO        D48CBO
409600971223     C                   MOVEL     'V'           D48FFR
409700971223     C                   MOVEL     'A'           D48TBO
409800971223     C                   MOVEL     'S'           D48F12
409900991027     C                   MOVEL     FNLR48DS      KPJBU
410000971223     C*
410100971223     C                   CALL      'FNLR48R'
410200971223     C                   PARM                    KPJBA
410300971223     C                   PARM                    DSARBD
410400971223     C                   PARM                    DSARBK
410500991011     C                   PARM                    DARBT
410600971223     C                   PARM                    DSARBG
410700030612     C                   PARM                    trul90ds
410800971223     C*
410900991027     C                   MOVEL     KPJBU         FNLR48DS
411000060307     c                   movel     'S'           wriem
411100971223     C* ERRORE
411200971223     C     D48ERR        IFEQ      '1'
411300971223     C                   MOVEL     D48MSG        V1CMSG
411400971223     C                   SETON                                        284790
411500971223     C                   GOTO      FOR01
411600971223     C                   ENDIF
411700971223     C                   ENDIF
411800971223     C*
411900971223   X2C                   ELSE
412000131111   c
412100971223     C* VISUALIZZAZIONE BOLLA
412200131111     c                   select
412300131111     C     v1cbarcode    whengt    0
412400131111     C                   MOVEL     arbAAS        V2CAAS
412500131111     C                   MOVEL     arblnp        COMLNP
412600131111     C                   MOVEL     arbNRS        V2CNRS
412700131111     C                   MOVEL     arbNSP        V2CNSP
412800140117     C     SEL2          whengt    0
412900140117     C                   MOVEL     V1CAAS        V2CAAS
413000140117     C                   MOVEL     V1CLP2        COMLNP
413100140117     C                   MOVEL     V1CNRS        V2CNRS
413200140117     C                   MOVEL     V1CNSP        V2CNSP
413300131111     C                   ENDSL
413400131111     c
413500150316     C****               EXSR      BOLLA
413600971223     C*
413700150317     c****               EXSR      VISUAL
413701160523     c*Parametri per FNLRU6R1:
413702160523     c*Campi di input:  V2CAAS,COMLNP,V2CNRS,V2CNSP,PA1FLG,PA1GIA, PA1TLA
413703160523     c*campi di output: PA1F03, PA1RMA, PA1XCO
413704160606     c                   eval      PARAMU6ds1=paramu6ds
413705160523     c                   clear                   pa1tla_1
413706160606     c                   eval      kpjbu=paramu6ds1
413900150317     c                   if        sdsprm>1
414000150319     c                   call      'FNLRU6R1'
414100150316     c                   parm                    kpjba
414200150401     c                   parm                    fnlru6ds
414300150317     c                   else
414400150317     c                   call      'FNLRU6R1'
414500150317     c                   parm                    kpjba
414600150317     c                   endif
414701160606     c                   eval      paramu6ds1=kpjbu
414800150410
414900971223    2C                   ENDIF
415000150318     c                   movel     'S'           wriem
415100180308     C* L'unico caso in cui bisogna andare a INIZIO è quando:
415101180308     c* non è stata fatta ricerca con key specifica
415102180308     c* non è stata effettuata alcuna selezione di bolla
415103180308     c* il pgm non è richiamato in modalità di bypass videata di selezione bolla
415104180308     c* In tutti gli altri casi deve andare a FINE
415200971223    2C     PA1FLG        IFNE      '1'
415300971223     C     PA1FLG        ANDNE     'N'
415400971223     C     PA1FLG        ANDNE     '3'
415500971223     C     PA1FLG        ANDNE     'P'
415600971223     C     PA1FLG        ANDNE     'E'
415700990129     C     PA1FLG        ANDNE     'C'
415702180308    3c                   if        pa1f03_1<>'S'  and
415703180308     c                             (sdsprm=1 or ilru6sel=' ')
415900971223     C                   MOVEL     '1'           WINIZ             1
416000180308   x3c                   else
416100150402     C                   MOVEL     '1'           WFINE
416200180308    3c                   endif
416300180308   x2C                   ELSE
416400971223     C                   MOVEL     '1'           WFINE             1
416500971223    2C                   END
416600971223    2C                   ENDSR
416700131111      **************************************************************************
416800131111      *   Prepara SQL per rsd ind lod rsm
416900131111      **************************************************************************
417000131111     C     Presql        BEGSR
417100131111     //free
417200160127      // per ora non lo faccio più  eseguire il join per problemi di prestazione
417300160127      //  la ricerca viene fatta solo sulla ragione sociale di ARB primi 35 caratteri
417400160127      // if v1cxrsd='X'  ;
417500131114
417600160127      // wSQL ='SELECT fnarb00f.*  +
417700160127      //           from FNARB00F left outer join FIAR400F on arbaas=ar4aas +
417800160127      //       and arblnp=ar4lnp and arbnrs=ar4nrs and arbnsp=ar4nsp  +
417900160127      //        and ar4trc=''D'' where arbnzd='''+ v1cnzd +''' '  ;
418000160127      //        else  ;
418100131114
418101161125        if v1cnzd=*blanks and v1cprd=*blanks;
418102161125         wSQL ='SELECT * from FNARB00F   where ';
418103161125        else ;
418200131114         wSQL ='SELECT * from FNARB00F   where arbnzd='''+ v1cnzd +''' '  ;
418300160127      // endif  ;
418400131114
418500131114       if v1cnzd=*blanks   ;
418600131114         wSQL +=' and arbprd=''' + v1cprd + ''' '   ;
418700131111       endif  ;
418701161125
418702161125        endif ;
418800131111
418900131115       if (v1cssp=' ' or v1cssp='1')  ;
419000131115        if comdtd>0 and comdta>comdtd  ;
419100131115           wSQL += ' and (arbaas*10000)+arbmgs>=' + %editc(comdtd:'X')+ ' and +
419200131115                        (arbaas*10000)+arbmgs<=' + %editc(comdta:'X')  ;
419300131115        endif  ;
419400131115        if comdtd>0 and (comdta=0 or comdta=comdtd)      ;
419500131115           wSQL += 'and arbaas=' + %editc(comaad:'X')+ ' and arbmgs=' +
419600131115                                %editc(commgd:'X')  ;
419700131115        endif  ;
419800131115       endif  ;
419900131111
420000131111       // se voglio le "IN CONSEGNA", parzializzo
420100131111       if v1cssp='2'  ;
420200131113           wSQL += ' and (arbdcm=0 or arbica='' '' or arbica=''R'' +
420300131113                     or arbicc='' '' or arbicc=''R'')'  ;
420400131111       endif  ;
420500131114
420600131114       // Se voglio le "DA CONSEGNARE", parzializzo
420700131111       if v1cssp='1'  ;
420800131111           wSQL += ' and arbifp=0 and arbndc=0 and arbnss=0' ;
420900131111       endif  ;
421000131111
421100131114       // Se voglio le "CONSEGNATE", parzializzo
421200131111       if v1cssp='3'  ;
421300131111           wSQL += ' and arbdcm>0' ;
421400131111       endif  ;
421500131111
421600131111       // linea di arrivo
421700131111       if v1clna>0  ;
421800131111           wSQL += ' and arblna=' + %editc(v1clna:'X') ;
421900131111       endif  ;
422000131111
422100131111        if v1crsd<>*blanks  ;
422200131111
422300131111         if v1cxrsd =' '   ;
422400131114           wSQL += ' and arbrsd    like(''';
422500131111         else  ;
422600160127           wSQL += ' and arbrsd    like(''%';
422700160127          // non esegue più il joi
422800160127          // wSQL += ' and concat(arbrsd, ifnull(ar4not, '' '')) like(''%'  ;
422900131111         endif ;
423000131111
423100131209         w060a=v1crsd  ;
423200131209         exsr APICI  ;
423300131209
423400131209           wSQL += w060a ;
423500131111           wSQL  = %trim(wSQL) + '%'')' ;
423600131111         endif ;
423700131111
423800131111        if v1cIND<>*blanks  ;
423900131111
424000131111         if v1cxind =' '   ;
424100131111           wSQL += ' and arbind like(''';
424200131111         else  ;
424300131113           wSQL += ' and arbind like (''%' ;
424400131111         endif ;
424500131111
424600131209         w060a=v1cind  ;
424700131209         exsr APICI  ;
424800131209           wSQL += w060a ;
424900131111           wSQL  = %trim(wSQL) + '%'')' ;
425000131111         endif ;
425100131111
425200131111        if v1cLOD<>*blanks  ;
425300131111
425400131111         if v1cxlod =' '   ;
425500131111           wSQL += ' and arblod like(''';
425600131111         else  ;
425700131113           wSQL += ' and arblod like (''%' ;
425800131111         endif ;
425900131111
426000131209         w060a=v1clod  ;
426100131209         exsr APICI  ;
426200131209           wSQL += w060a ;
426300131111           wSQL  = %trim(wSQL) + '%'')' ;
426400131111         endif ;
426500131111
426600131111        if v1cRSM<>*blanks  ;
426700131111
426800131111         if v1cxrsm =' '   ;
426900131111           wSQL += ' and arbrsm like(''';
427000131111         else  ;
427100131113           wSQL += ' and arbrsm like (''%' ;
427200131111         endif ;
427300131111
427400131209         w060a=v1crsm  ;
427500131209         exsr APICI  ;
427600131209           wSQL += w060a ;
427700131115           wSQL  = %trim(wSQL) + '%'')' ;
427800131111         endif ;
427900131115
427901161128        if v1cnzd=*blanks and v1cprd=*blanks;
427902161128           wSQL += ' order by  arbaas, arbmgs'  ;
427903161128        else;
428000131115           wSQL += ' order by arbnzd, arbprd, arbaas, arbmgs'  ;
428001161128        endif;
428002161125
428003161125        // se dopo la "WHERE" è presente "and" lo tolgo
428004161125
428005161125           if v1cprd=*blanks and v1cnzd=*blanks;
428006161125               posw=%scan('where':wSQL) ;
428007161125               posa=%scan('and':wSQL:posw);
428008161125               %subst(wSQL:posa:3)=*blanks;
428009161125           endif  ;
428100131111
428200131111        ENDSR  ;
428300150319      **************************************************************************
428400150319      *   Prepara SQL per rsd ind lod rsm
428500150319      **************************************************************************
428600150319     C     PresqlBLP     BEGSR
428700150319     //free
428800160429        //  if v1cxrsd='X'  ;
428900150319
429000160429        //  wSQL ='SELECT fnblp00f.*  +
429100160429        //            from FNBLP00F left outer join FIAR400F on blpaas=ar4aas +
429200160429        //        and blplnp=ar4lnp and blpnrs=ar4nrs and blpnsp=ar4nsp  +
429300160429        //         and ar4trc=''D'' where blpnzd='''+ v1cnzd +''' '  ;
429400160429        //         else  ;
429500150319
429501161125        if v1cnzd=*blanks and v1cprd=*blanks;
429502161125         wSQL ='SELECT * from FNBLP00F   where ';
429503161125        else ;
429600150319         wSQL ='SELECT * from FNBLP00F   where blpnzd='''+ v1cnzd +''' '  ;
429700160429        //  endif  ;
429800150319
429900150319       if v1cnzd=*blanks   ;
430000150319         wSQL +=' and blpprd=''' + v1cprd + ''' '   ;
430100150319       endif  ;
430101161125        endif;
430200150319
430300150319        if comdtd>0 and comdta>comdtd  ;
430400150319           wSQL += ' and (blpaas*10000)+blpmgs>=' + %editc(comdtd:'X')+ ' and +
430500150319                        (blpaas*10000)+blpmgs<=' + %editc(comdta:'X')  ;
430600150319        endif  ;
430700150319        if comdtd>0 and (comdta=0 or comdta=comdtd)      ;
430800150319           wSQL += 'and blpaas=' + %editc(comaad:'X')+ ' and blpmgs=' +
430900150319                                %editc(commgd:'X')  ;
431000150319        endif  ;
431100150319
431200150319
431300150319       // linea di arrivo
431400150319       if v1clna>0  ;
431500150319           wSQL += ' and blplna=' + %editc(v1clna:'X') ;
431600150319       endif  ;
431700150319
431800150319        if v1crsd<>*blanks  ;
431900150319
432000150319         if v1cxrsd =' '   ;
432100150319           wSQL += ' and blprsd    like(''';
432200150319         else  ;
432300160429           wSQL += ' and blprsd    like(''%';
432400160429          // non esegue più il joi
432500160429          // wSQL += ' and concat(blprsd, ifnull(ar4not, '' '')) like(''%'  ;
432600150319         endif ;
432700150319
432800150319         w060a=v1crsd  ;
432900150319         exsr APICI  ;
433000150319
433100150319           wSQL += w060a ;
433200150319           wSQL  = %trim(wSQL) + '%'')' ;
433300150319         endif ;
433400150319
433500150319        if v1cIND<>*blanks  ;
433600150319
433700150319         if v1cxind =' '   ;
433800150319           wSQL += ' and blpind like(''';
433900150319         else  ;
434000150319           wSQL += ' and blpind like (''%' ;
434100150319         endif ;
434200150319
434300150319         w060a=v1cind  ;
434400150319         exsr APICI  ;
434500150319           wSQL += w060a ;
434600150319           wSQL  = %trim(wSQL) + '%'')' ;
434700150319         endif ;
434800150319
434900150319        if v1cLOD<>*blanks  ;
435000150319
435100150319         if v1cxlod =' '   ;
435200150319           wSQL += ' and blplod like(''';
435300150319         else  ;
435400150319           wSQL += ' and blplod like (''%' ;
435500150319         endif ;
435600150319
435700150319         w060a=v1clod  ;
435800150319         exsr APICI  ;
435900150319           wSQL += w060a ;
436000150319           wSQL  = %trim(wSQL) + '%'')' ;
436100150319         endif ;
436200150319
436300150319        if v1cRSM<>*blanks  ;
436400150319
436500150319         if v1cxrsm =' '   ;
436600150319           wSQL += ' and blprsm like(''';
436700150319         else  ;
436800150319           wSQL += ' and blprsm like (''%' ;
436900150319         endif ;
437000150319
437100150319         w060a=v1crsm  ;
437200150319         exsr APICI  ;
437300150319           wSQL += w060a ;
437400150319           wSQL  = %trim(wSQL) + '%'')' ;
437500150319         endif ;
437600150319
437601161128        if v1cnzd=*blanks and v1cprd=*blanks;
437602161128           wSQL += ' order by  blpaas, blpmgs'  ;
437603161128        else;
437700150319           wSQL += ' order by blpnzd, blpprd, blpaas, blpmgs'  ;
437701161128        endif;
437702161125        // se dopo la "WHERE" è presente "and" lo tolgo
437703161125
437704161125           if v1cprd=*blanks and v1cnzd=*blanks;
437705161125               posw=%scan('where':wSQL) ;
437706161125               posa=%scan('and':wSQL:posw);
437707161125               %subst(wSQL:posa:3)=*blanks;
437708161125           endif  ;
437800150319
437900150319        ENDSR  ;
437901170224      **************************************************************************
437902170224      *   Prepara SQL per rma
437903170224      **************************************************************************
437904170224       begsr presqlRMA;
437905170224     //free
437913170224
437915170224         wSQL ='SELECT * from FNBLP00F where ucase(blprma)='''+ comrma +''' ';
437917170224
438012170224        ENDSR  ;
438013131111       //--------------------------------------------------------------
438100131111       //?Apertura cursore.                                            ?
438200131111       //--------------------------------------------------------------
438300131111       BEGSR  OpenCursor;
438400131111
438500131111         wEnd = *off;
438600131111
438700131111       //?Dichiarazione cursore?
438800131111         exec sql
438900131111         PREPARE S1 from :wSQL;
439000131209
439100131209         IF sqlcode < 0;
439200131209           wEnd = *on;
439300131209           leavesr        ;
439400131209         ENDIF;
439500131209
439600131111         exec sql
439700131111         DECLARE ARB  cursor for S1;
439800131111
439900131111       //?Apertura del cursore
440000131111         exec sql
440100131111         OPEN ARB;
440200131111
440300131111         IF sqlcode < 0;
440400131111           wEnd = *on;
440500131209           leavesr        ;
440600131111         ENDIF;
440700131209
440800131209           WopenC='1'     ;
440900131209
441000131111
441100131111       ENDSR;
441200150319       //--------------------------------------------------------------
441300150319       //?Apertura cursore.                                            ?
441400150319       //--------------------------------------------------------------
441500150319       BEGSR  OpenCursorBlp;
441600150319
441700150319         wEnd = *off;
441800150319
441900150319       //?Dichiarazione cursore?
442000150319         exec sql
442100150319         PREPARE S2 from :wSQL;
442200150319
442300150319         IF sqlcode < 0;
442400150319           wEnd = *on;
442500150319           leavesr        ;
442600150319         ENDIF;
442700150319
442800150319         exec sql
442900150319         DECLARE BLP  cursor for S2;
443000150319
443100150319       //?Apertura del cursore
443200150319         exec sql
443300150319         OPEN BLP;
443400150319
443500150319         IF sqlcode < 0;
443600150319           wEnd = *on;
443700150319           leavesr        ;
443800150319         ENDIF;
443900150319
444000150319           WopenC2='1'     ;
444100150319
444200150319
444300150319       ENDSR;
444301170224       //--------------------------------------------------------------
444302170224       //?Apertura cursore.                                            ?
444303170224       //--------------------------------------------------------------
444304170224       BEGSR  OpenCursorRMA;
444305170224
444306170224         wEnd = *off;
444307170224
444308170224       //?Dichiarazione cursore?
444309170224         exec sql
444310170224         PREPARE S3 from :wSQL;
444311170224
444312170224         IF sqlcode < 0;
444313170224           wEnd = *on;
444314170224           leavesr        ;
444315170224         ENDIF;
444316170224
444317170224         exec sql
444318170224         DECLARE RMA  cursor for S3;
444319170224
444320170224       //?Apertura del cursore
444321170224         exec sql
444322170224         OPEN RMA;
444323170224
444324170224         IF sqlcode < 0;
444325170224           wEnd = *on;
444326170224           leavesr        ;
444327170224         ENDIF;
444328170224
444329170224           WopenC3='1'     ;
444330170224
444331170224
444332170224       ENDSR;
444400131209       //--------------------------------------------------------------
444500131209       //?Raddoppio apici nella stringa                                ?
444600131209       //--------------------------------------------------------------
444700131209       BEGSR  Apici     ;
444800131209       ap=1  ;
444900131209       dow ap <=60  ;
445000131209       if %subst(w060a:ap:1)=''''  ;
445100131209       w060a=%replace('''''':w060a:ap:1)  ;
445200131209       ap= ap   +1       ;
445300131209       endif  ;
445400131209
445500131209       ap= ap   +1       ;
445600131209       enddo  ;
445700131209
445800131209       ENDSR;
445900131111       //--------------------------------------------------------------
446000131111       //?Chiusura cursore.
446100131111       //--------------------------------------------------------------
446200131111       BEGSR CloseCursor;
446300131111
446400131111       //?Chiusura cursore SQL?
446500131111         exec sql
446600131111           close ARB;
446700131209           clear WopenC   ;
446800131209           Wend=*off      ;
446900131111
447000131111       ENDSR;
447100150319       //--------------------------------------------------------------
447200150319       //?Chiusura cursore.
447300150319       //--------------------------------------------------------------
447400150319       BEGSR CloseCursorBlp;
447500150319
447600150319       //?Chiusura cursore SQL?
447700150319         exec sql
447800150319           close BLP;
447900150319           clear WopenC2  ;
448000150319           Wend=*off      ;
448100150319
448200150319       ENDSR;
448201170224       //--------------------------------------------------------------
448202170224       //?Chiusura cursore.
448203170224       //--------------------------------------------------------------
448204170224       BEGSR CloseCursorRMA;
448205170224
448206170224       //?Chiusura cursore SQL?
448207170224         exec sql
448208170224           close RMA;
448209170224           clear WopenC3  ;
448210170224           Wend=*off      ;
448211170224
448212170224       ENDSR;
448300131111       //--------------------------------------------------------------
448400131111       //?Lettura cursore.                                             ?
448500131111       //--------------------------------------------------------------
448600131111       BEGSR  ReadCursor;
448700131111
448800131111         clear  FNARBds;
448900150507         exsr   sr_inzsoloblp;
449000131113         *in30=*off    ;
449100131113         *in32=*off    ;
449200131111
449300131111         exec sql
449400131111         FETCH next from ARB into :FNARBds;
449500131111
449600131111         IF sqlcod = 100 or sqlcod < 0;
449700131111           wEnd = *on;
449800160506         else;
449900160506           w_arr='S'    ;
450000131111         ENDIF;
450100131111
450200131111       ENDSR;
450300150319       //--------------------------------------------------------------
450400150319       //?Lettura cursore.                                             ?
450500150319       //--------------------------------------------------------------
450600150319       BEGSR  ReadCursorBlp;
450700150319
450800150319         clear  FNBLPds;
450900150319         *in30=*off    ;
451000150319         *in32=*off    ;
451100150319
451200150319         exec sql
451300150319         FETCH next from BLP into :FNBLPds;
451400150319
451500150319         IF sqlcod = 100 or sqlcod < 0;
451600150319           wEnd = *on;
451700150319         else;
451800150507         clear  FNARBds;
451900150507         exsr   sr_inzsoloblp;
452000150319         chain (blpaas:blplnp:blpnrs:blpnsp) fnblp01l   ;
452100150319         chain (blpaas:blplnp:blpnrs:blpnsp) fnarb01l   ;
452200160506         if %found(fnarb01l);
452300160506            w_arr='S'    ;
452400160506         endif;
452500150319         ENDIF;
452600150319
452700150319       ENDSR;
452701170224       //--------------------------------------------------------------
452702170224       //?Lettura cursore.                                             ?
452703170224       //--------------------------------------------------------------
452704170224       BEGSR  ReadCursorRMA;
452705170224
452706170224         clear  FNBLPds;
452707170224         *in30=*off    ;
452708170224         *in32=*off    ;
452709170224
452710170224         exec sql
452711170224         FETCH next from RMA into :FNBLPds;
452712170224
452713170224         IF sqlcod = 100 or sqlcod < 0;
452714170224           wEnd = *on;
452715170224         else;
452716170224         clear  FNARBds;
452717170224         exsr   sr_inzsoloblp;
452718170224         chain (blpaas:blplnp:blpnrs:blpnsp) fnblp01l   ;
452719170224         chain (blpaas:blplnp:blpnrs:blpnsp) fnarb01l   ;
452720170224         if %found(fnarb01l);
452721170224            w_arr='S'    ;
452722170224         endif;
452723170224         ENDIF;
452724170224
452725170224       ENDSR;
452817141010       //--------------------------------------------------------------
452900141010       //?Window per f19=Dispo di consegna da esaminare
453000141010       //--------------------------------------------------------------
453100141010       BEGSR sr_wdwdc   ;
453200141010          $fine=*off;
453300141105          clear fnlry09ds2;
453400141010          ilry09tch='C';
453500141010          ILRY09AAS=d48aas ;
453600141010          ILRY09LNP=d48LNP;
453700141010          ILRY09NRS=d48NRS ;
453800141010          ILRY09NSP=d48NSP ;
453900141105          kpjbu=fnlry09ds2;
454000141105          fnlry09r(kpjba);
454100141105          fnlry09ds2=kpjbu;
454200141010          if olry09esit='1' ;
454300141010             dow $fine=*off;
454301170110              IF dsp_mod = 1    ;
454400170110                exfmt lri6wddca;
454401170110              else;
454402170110                exfmt lri6wddc;
454403170110              endif;
454500141010                   if *inkt;
454600141105                       clear fnlry09ds2;
454700141010                       ilry09tch='E';
454800141010                       ILRY09AAS=d48aas ;
454900141010                       ILRY09LNP=d48LNP;
455000141010                       ILRY09NRS=d48NRS ;
455100141010                       ILRY09NSP=d48NSP ;
455200141105                       ILRY09DF06='S';
455300141105                       kpjbu=fnlry09ds2;
455400141105                       fnlry09r(kpjba);
455500141010                       $fine=*on;
455600141010                   endif;
455700141010             enddo;
455800141010          endif ;
455900141010
456000141010       ENDSR;
456100131111
456200150507       //--------------------------------------------------------------
456300150507       //?Pulizia campi di blp non presenti su arb
456400150507       //--------------------------------------------------------------
456500150507       BEGSR sr_inzsoloblp;
456600150507          clear arbflp;
456700150507          clear arbdim;
456800150507          clear arbsop;
456900150507          clear arbfst;
457000150507          clear arbctm;
457100150507          clear arbdrt;
457200150507          clear arbnrt;
457300150507          clear arbfpp;
457400150507          clear arbpdr;
457500150507          clear arbdse;
457600150507       ENDSR;
457601161116       //--------------------------------------------------------------
457602161116       //?Controllo NPR.
457603161116       //--------------------------------------------------------------
457604161116       BEGSR NumeroNPR;
457605161116
457606161116         prmRqsOpCode = FIORC00_RQSOPCODE_GET;
457607161116         prmRqsFormato = FIORC0I0.formato;
457608161116         prmRqsSize = %size(FIORC0I0);
457609161116         prmRpyFormato = FIORC0O0.formato;
457610161116         prmRpySize = %size(FIORC0O0);
457611161116         prmRpyMsgFormato  = FIORC0M0.formato;
457612161116         prmRpyMsgSize = %size(FIORC0M0);
457613161116
457614161116         clear FIORC0I0.unificante;
457615161116         FIORC0I0.npr = v1cnprr;
457616161116
457617161116       //?Richiamo FIORC00R
457618161116         FIORC00_GetOrm (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
457619161116                         prmRqsFormato:FIORC0I0:prmRqsSize:
457620161116                         prmRpyFormato:FIORC0O0:prmRpySize:
457621161116                         prmRpyMsgFormato:FIORC0M0:prmRpyMsgSize);
457622161116
457623161116         IF  FIORC0M0.nrmsg > 0;
457624161116           *in69=*on;
457625161116           *in90=*on;
457626161116           *in28=*on;
457627161116           v1cmsg = %trim(FIORC0M0.skTextMsg(01));
457628161116           leavesr;
457629161116         ENDIF;
457630161116
457644161116
457645161116       ENDSR;
457700131111
457800131111
457900131111      /end-free
458000131111
458100940713      **************************************************************************
459800940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
459900940713Nessuna bolla da visualizzare                                                  1
460000940713Immettere linea di arrivo valida                                               2
460100131114Effettuare ulteriori parzializzazioni                                          3
460200940713Immettere linea di arrivo o linea di partenza                                  4
460300940713Immettere linea di partenza valida                                             5
460400940713Numero spedizione inesistente                                                  6
460500940713Immettere anno spedizione                                                      7
460600940713Immettere data formalmente valida                                              8
460700040216Immettere tipo bolla valido                                                    9
460800940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
460900940713Immettere numero di serie iniziale minore o uguale a quello finale            11
461000940713Immettere numero segnacollo iniziale minore o uguale a quello finale          12
461100940713Immettere numero segnacollo finale, se immesso quello iniziale                13
461200131114Scelta incongruente tra stato spedizioni  e richiesta                         14
461300940713Fine scorrimento                                                              15
461400940715Immettere tipo servizio bolle valido                                          16
461500940923Immettere filiale segnacollo valida                                           17
461600060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico a 14 alfanum
461700941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
461701161116Non trovate bolle per il Numero di Prenotazione richiesto                     20
461900010508Immettere p.o. di emissione valido                                            21
462000010510Non trovate bolle per l' O.R.M. richiesto                                     22
462100020626Immettere la nazione e la provincia del destinatario                          23
462200020627Nazione destinatario errata                                                   24
462300020627Provincia destinatario errata                                                 25
462400060130Utente non abilitato alla Variazione bolle                                    26
462401161124Ampiezza massima subfile raggiunta. Non sono visualizzate tutte le spedizioni 27
462600160615                                                                              28
462700160518                                                                              29
462800161003Immettere il Cod. CHI SONO per attivare la ricerca                            30
462900160518                                                                              31
463000131111non trovata bolla per il barcde digitato                                      32
463100150319Immettere il Codice OPPURE la Ragione Sociale                                 33
463200150327Filiale cliente inesistente o errata                                          34
463300150327Immettere codice cliente esistente e non annullato                            35
463400150330Indirizzo Email errato                                                        36
463500150331Numero di Cellulare errato                                                    37
464800030612** cnm
464900991222 0123456789
