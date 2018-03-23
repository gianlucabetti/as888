000100040603     H DECEDIT('0,') DATEDIT(*ymD.)
000200950711     H* FNLR31R *-----------------------------------------------------*
000300170125     H*        - INVENTARIO MAGAZZINO  I.M.A. - IMG          Esegue
000400930629     H*---------------------------------------------------------------*
000500911015     FTABEL00F  IF   E           K DISK
000600911015     FAZORG01L  IF   E           K DISK
000700161025     FTigcp21L  uF   E           K DISK
000800021203     FFIAPD01L  IF   E           K DISK
000900040427     FFNARB05L  IF   E           K DISK    infds(fnarbinf)
001000040427     Ffnarb00f  UF   E             DISK    rename(fnarb000:fnarbf00)
001100941017     FFNARB01L  IF   E           K DISK
001200941017     F                                     RENAME(FNARB000:FNARB001)
001300090522     FFNblp01l  IF   E           K DISK    prefix(arb:3)
001400090522     FFNblt27l  IF   E           K DISK    prefix(art:3)
001500040603     FfIAR501l  IF   E           K DISK
001600040428     FFNART01L  UF   E           K DISK
001700081014     FFNART27L  iF   E           K DISK    RENAME(FNARt000:FNARt027)
001800941017     FFNLBL02L  IF   E           K DISK
001900070118     FFNBRV01L  IF   E           K DISK
002000070418RM*  FFNBRV07L  IF   E           K DISK    RENAME(FNBRV000:FNBRV007)
002100140618RM*  FFNBRV05L  IF   E           K DISK    RENAME(FNBRV000:FNBRV005)
002200081014     FFNANM02L  uF A E           K DISK
002300941017     FFNFVV01L  UF   E           K DISK
002400990212     FFNDCD02L  IF   E           K DISK
002500990212     FFNDCT01L  IF   E           K DISK
002600990212     F                                     RENAME(FNDCT000:FNDCT001)
002700950316     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002800950316     F                                     USROPN
002900941017     D*
003000161017     D STA             S             66    DIM(22) CTDATA PERRCD(1)             SKIERE STAMPA
003100101215     D L6              S              3  0 DIM(30)                              FIL da elaborare
003200101215     D Lspu            S              3  0 DIM(30)                              FIL GEST ARRIV
003300071227     D NPS             S              2    DIM(90)                              LETT BARCOD
003400071227     D CPS             S              7  0 DIM(90)                              COLLI LETTI
003500941019     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
003600161017     D CMDD            S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
003700050511     D zone            S              2    DIM(200)                             COMANDO QCMDEXC
003800160503     D zone12          S              2    DIM(200)                             COMANDO QCMDEXC
003900101220     d NPGARR          s              2    dim(10)
004000101220     D*
004100040427     D fnarbinf        ds
004200040427     D  fnarbnrri            397    400i 0
004300941017     D PARAM1          DS
004400911015     D  COMNFV                 1      5  0
004500911015     D  VIDSN                  6      6
004600941028     D* FILIALE DI CUI GESTIRE LE LINEE IN ARRIVO
004700920813     D  VIDFIL                 7      9  0
004800941028     D* FILIALE APERTURA FOGLIO
004900941031     D  VIDFGS                10     12  0
005000950810     D* TIPO INVENTARIO --> G-GIACENZE A-ARRIVI
005100950810     D  VIDTIN                13     13
005200160502     D*
005300160502     D  PAR_lib               14     14
005400060508     D* linea di arrivo
005500060508     D  vidlna                15     17  0
005600070418     D* solo per IMA - elabora colli mancanti di spedizioni in distinta
005700070418     D  vidmdc                18     18
005800101215     D* solo colli spuntati alla lna finale            - solo IMA
005900101215     D  parimaa               19     19
006000160502     D* TIPO zona consegna--> P=parcel   M=messaggerie  P1/P2=Parcel 1° e 2° turno
006100160502     D  PARTpm1               20     21
006200160502     D  PARTpm2               22     23
006300950810     D*** PARAMETRI PER RICERCA FASE ANOMALIA    - TRUL11R -
006400950810     D PARAM5          DS
006500950810     D  PA5TLA                 1      1
006600950810     D  PA5NPG                 2      2
006700950810     D  PA5SPG                 3      3
006800950810     D  PA5FAS                 4      4
006900911018     D                 DS
007000941018     D  COMAAS                 1      4  0
007100941018     D  ARBMGS                 5      8  0
007200941018     D  ARBDSP                 1      8  0
007300911018     D                 DS
007400941018     D  ARBAAS                 1      4  0
007500941018     D  ARBLNP                 5      7  0
007600941018     D  ARBNRS                 8      9  0
007700941018     D  ARBNSP                10     16  0
007800941018     D  ARBSPE                 1     16  0
007900950810     D                 DS
008000950810     D  SAVAAS                 1      4  0
008100950810     D  SAVLNP                 5      7  0
008200950810     D  SAVNRS                 8      9  0
008300950810     D  SAVNSP                10     16  0
008400950810     D  SAVSPE                 1     16  0
008500911015     D WLBDAT          DS
008600941017     D  G02DAT                 1      8  0
008700941017     D  G02INV                 9     16  0
008800941017     D  G02ERR                17     17
008900941017     D  G02TGI                18     22  0
009000990212     D                 DS
009100990212     D  DCTDCA                 1      8  0
009200990212     D  DCTAAC                 1      4  0
009300990212     D  DCTMGC                 5      8  0
009400941019     D* DS PER TRUL06R - CARICAMENTO £X
009500941019     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009600941019     D  LIN                    1     90  0
009700941019     D                                     DIM(30)                              FIL. COMODO
009800031219     D* REPERISCE DATI FOGLI
009900031219     D DSLV53        E DS                  EXTNAME(FNLV53DS)
010000960530     D KPJBA         E DS
010100161130     d
010200161130     d TRULVPODS     e ds
010300161130     D  skFilOk               16    765    dim(250)
010400161130     d
010500101202      *
010600101202      * - Parametri per richiamo al pgm di controllo profilo utenti
010700101202     d TIBS34ds      e ds
010800101202      * - Ds di riferimento al file esterno AzUte00F
010900101202     d AZUTEds       e ds                  ExtName(AzUte00F)
011000101202      * - Ds per dati organigramma
011100101202     d dDatiUte      e ds
011200101202      *
011300170213     D og148         E DS
011400170213     D DS7B          E DS
011500161017     D DS7C          E DS
011600101220     D DS7N          E DS
011700920318     D DS3A          E DS
011800050511     D DS03          E DS
011900040603     D DAR5GEN       E DS
012000100706     D DVPO          E DS
012100990212     D DTAD            DS
012200990212     D  §TARGR                26     26
012300990303     D DCCH            DS
012400990303     D  §CHDES                 1     50
012500990303     D  §CHIMA                53     53
012600070118     D RECBRV        E DS                  EXTNAME(FNBRV00F)
012700070118     D WSAVB         E DS                  EXTNAME(FNBRV00F)
012800031219     d                                     prefix(S_)
012900990212     D DSBS02        E DS                  EXTNAME(TIBS02DS)
013000160927     D tigcp00ds     E DS                  EXTNAME(tigcp00F)
013100040427      * DS Esterna x gestione allocazione ed invio messaggi
013200040427     D TRUL82DS      E DS
013300081014      * DS Esterna x controllo colli doppi
013400081014     D FNLRV7DS      E DS
013500160927      * DS Esterna x controllo colli in IMG
013600160927     D FNLGI1DS      E DS
013700040806      * ds per il controllo della presenza di CA per la spedizione richiesta
013800040806     d FIDN12DS      e ds
013900040806     d  skKey                 26    305    dim(20)
014000040806     d  skAnn                306    325    dim(20)
014100040806     d  skDca                326    485  0 dim(20)
014200101202     d**skFca                486    545  0 dim(20)
014300040806     d  skDch                546    705  0 dim(20)
014400040806     d  skCch                706    745    dim(20)
014500101202      *
014600040806     d dsKey           ds
014700040806     d  dsaac                         4  0
014800040806     d  dsfil                         3  0
014900040806     d  dsnca                         7  0
015000040806      *
015100101202      *
015200021203      *
015300101220     d WnpgArr         s              2
015400101220     d Trov            s              2  0
015500090522     d Nobolla         S              1
015600170125     d winGiac         S              1
015700090522     d uno             S              1
015800081014     d statipo         S             21
015900160503     d statipo2        S             21
016000101215     d staimaa         S             44
016100070418     d stamdc          S              2
016200160503     d indMP           S              4  0
016300160503     d ind12           S              4  0
016400170213     d iddgdopo        S              1    inz
016500050511     d WSPG            S                   LIKE(FVVSPG)
016600040108     d kpdpdr          S                   LIKE(APDpdr)
016700031219     d wdfv            S                   LIKE(d53dfv)
016800031219     d kpdtip          S                   LIKE(APDtip)
016900040603     d ktrd            S                   LIKE(ar5trd) inz('GEN')
017000161018     d ecce_descr      S             25
017100100706     d dataEUR         s               d   datfmt(*eur)
017200100706     d DataISO         s               d   datfmt(*iso)
017300101202     d*
017400040428     D msgjob          C                   CONST('La Manut.Bolle sta bloccando -
017500040428     D                                     l''elabor. Dell''IMA:USCIRE SUBITO D-
017600040428     D                                     A QUESTO LAVORO')
017700050511     D CPARCEL         C                   CONST('     P A R C E L     ')
017800050511     D CMessag         C                   CONST('M E S S A G G E R I E')
017900160503     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
018000160503     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
018100101215     D CsoloLNA        C                   CONST('Solo colli spuntati alla line-
018200101215     d                                     a arrivo finale')
018300911018     C*--------------------------------------------------------------- *
018400911018     C*                  INDICATORI                                    *
018500911018     C*--------------------------------------------------------------- *
018600911018     C* 01    - ESITONO ANOMALIE DI MANCANZA
018700950810     C* 02    - CHIESTA CHIUSURA  FOGLIO
018800911018     C* 03    - ESISTE SERIE
018900911018     C* 04    - SEGNACOLLI NON SEQUENZIALI
019000911018     C* 05    - ON CAMBIATA SPEDIZIONE
019100920325     C* 06    - NON C'E' COD PADRONCINO CONSEGNA
019200920708     C* 07    - MANCA DATA ARRIVO : SPEDIZIONE FORZATA
019300920813     C* 08    - BOLLA GESTITA
019400170124     C* 11    - e 10 ON --> IPG  ma ora eliminato
019500950810     C* 10    - ON --> IMG  OFF --> IMA
019600081014     C* 12    - CONTROLLO COLLI DOPPI
019700950810     C* 13    - NON TROVATA SPUNTA IN IMA MA  IN ALTRO FOGLIO NELLA
019800950810     C*         STESSA DATA
019900950810     C* 14    - NON TROVATA SPUNTA IN IMA
020000031219     C* 15    - TROVATA SPUNTA SU ALTRO FOGLIO MA DEVO CREARE L'ANOMAL
020100100706     C* 16    - collo in IMA ma spuntato in IMA più di XX volte...da tabella VPO
020200161018     C* 18    - collo eccedente
020300170124     C**** 20    - FILIALE partita con procedura 2 gg in IMA
020400170124     C* 22    - FILIALE partita con nuova quadratura IMA (e non più IPG)
020500050405     C* 24    - STAMPA ubicazione collo se c'e'
020600050405     C* 25    - STAMPA se c'e' c.a.
020700050405     C* 26    - STAMPARE DATA CHIUSURA C.A.
020800941017     C* 30    - LETTURA FNARB
020900941017     C* 31    - LETTURA FNART
021000911018     C* 32/34 - COMODO
021100911018     C*---------------------------------------------------------------*
021200900622     C     *ENTRY        PLIST
021300900622     C                   PARM                    KPJBA
021400941017     C                   MOVEL     KPJBU         PARAM1
021500000126     C                   Z-ADD     COMNFV        WNFV
021600900828     C*-----------------------------------------------------
021700950810     C* IMPOSTAZIONI INIZIALI
021800950810     C                   EXSR      IMPO
021900950810     C*-----------------------------------------------------
022000170124     c                   setoff                                       11
022100161018     c* il dly mi serve solo per il debug in batch
022200161017     C                   Z-ADD     80            LUNG             15 5
022300161017     C                   MOVEL     *BLANKS       COMMAN           80
022400161017     C                   MOVEA     VIDTIN        CMD(35)
022500161017     C                   MOVEA     CMDd          COMMAN
022600161017     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
022700161017     C                   PARM                    COMMAN
022800161017     C                   PARM                    LUNG
022900161017
023000941019     C                   Z-ADD     80            LUNG             15 5
023100941019     C                   MOVEL     *BLANKS       COMMAN           80
023200950810     C                   MOVEA     VIDTIN        CMD(35)
023300941019     C                   MOVEA     CMD           COMMAN
023400170124     c**                 if        *in11
023500170124     c**                 eval      %subst(comman:31:6)='I.P.G.'
023600170124     c**                 endif
023700941019     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
023800941019     C                   PARM                    COMMAN
023900941019     C                   PARM                    LUNG
024000950810     C*
024100950316RM*  C                   OPEN      PRTF198
024200980128     C                   SETON                                        OF
024300911015     C*
024400040108     C* CONSIDERO SOLO i p.o. di £6
024500941019     C                   Z-ADD     1             WA                3 0          CONTATORE
024600040108    1C     L6(WA)        DOWNE     *ZERO
024700941019     C*
024800040108     C                   Z-ADD     L6(WA)        KLNA                           LINEA ARRIVO
024900161017     C*
025000031218     C     KLNA          SETLL     FNARB05L
025100040427     C     KLNA          READE     FNARB05L                               30
025200950810     C*
025300950810    3C     *IN30         DOWEQ     *OFF
025400050511     c* Se ima messaggerie o parcel, escludo le zone non di interesse
025500160503     c                   eval      indMP=0
025600160503     c                   eval      ind12=0
025700160503     c
025800160503     c                   if        wselzona='S'
025900160503     c                   movel     arbznc        alfzona           2
026000160503
026100160503     c                   if        wparc='S' or wmess='S'
026200160503     c***  alfzona       lookup    zone                                   35
026300160503     c                   eval      indMP =%lookup(alfzona:zone)
026400050511     c                   endif
026500160503     c                   if        wparc1='S' or wparc2='S'
026600160503     c                   eval      ind12 =%lookup(alfzona:zone12)
026700160503     c                   endif
026800160503
026900160503     c                   endif
027000160503   3ac                   if        wselzona=' ' or  indmp>0 or  ind12>0
027100920813     C*
027200920318     C* CHAIN CODICE BOLLE PER ESCLUDERE I RECUPERI
027300920318     C                   MOVEL     '3A'          COD
027400950810     C                   MOVEL(P)  ARBCBO        KEY
027500941018     C     KTAB          CHAIN     TABEL00F                           35
027600920318     C  N35              MOVEL     TBLUNI        DS3A
027700920318     C   35              MOVEL     'F1'          §3ATB1
027800920318     C*
027900950810     C* ESCLUDO:  LE BLOCCATE CON "B"/
028000930518     C*           QUELLE IN CONSEGNA / QUELLE CONSEGNATE / I RECUPERI
028100161025    4C     ARBdcm        ifeq      *zeros
028200950810     C     §3ARBL        ANDNE     'R'
028300950810     C     ARBFBC        ANDNE     'B'
028400000512     C*
028500960516     C* CONTROLLO SE E' GIACENTE
028600960516     C                   CLEAR                   WGIAC
028700960516     C     ARBFBC        IFEQ      'G'
028800161025     C     KGCA          CHAIN(n)  Tigcp21L                           35
028900970721     C* SE C'E' LA GIACENZA E NON E' A FASE 35 LA ESCLUDO
029000970721     C  N35GCPFAS        IFLT      35
029100050331     C     GCPFAS        oreq      36
029200170124     c* la merce è in giacenza : verifico con driver
029300160927     c                   clear                   fnlgi1ds
029400160927     c                   eval      ILGI1AAS=arbaas
029500160927     c                   eval      ILGI1lnp=arblnp
029600160927     c                   eval      ILGI1nrs=arbnrs
029700160927     c                   eval      ILGI1nsp=arbnsp
029800161018    3c                   select
029900161018     c                   when      arbpkc>0 and arbncp=arbncl
030000161018     c                   eval      ilgi1pkf=arbpkc
030100161018     c                   when      arbpkc>0 and arbncp<arbncl
030200161018     c                             and arbpkc>arbpkf
030300161018     c                   eval      ilgi1pkf=arbpkc
030400161018     c                   other
030500161018     c                   eval      ilgi1pkf=arbpkf
030600161018    3c                   endsl
030700160927     c                   movel     fnlgi1ds      kpjbu
030800160927     c                   call      'FNLGI1R'
030900160927     c                   parm                    kpjba
031000160927     c                   parm                    tigcp00ds
031100161007     c                   movel     kpjbu         fnlgi1ds
031200160927
031300170125     c* per IMG: sped portata nel giorno a mag. giac DEVE essere in giacenza
031400170125     c*          e quindi elaborata con mancanza
031500170125   5ac   10              if        olgi1AMG='Y' or olgi1inmg='S'
031600970721     C                   MOVEL     'S'           WGIAC             1
031700160823     c                   endif
031800170125   5ac   10              if        olgi1AMG='S'
031900170125     C                   MOVEL     'D'           WGIAC             1
032000170125     c                   endif
032100170125     c* per IMA: devo elaborare sped da portare a mag giac o portate nel giorno
032200161024     c* se la bolla è da portare a mag giacenza OGGI, devo considerarla anche in IMA
032300161024     c*  e se manca dare segnalazione ma NON creando l'anomalia
032400170125   5ac  n10              if        olgi1AMG='S' or olgi1AMG='Y'
032500161024     C                   MOVEL     'D'           WGIAC             1
032600161024     c                   endif
032700170125   5ac  n10              if        olgi1inmg='S'
032800170125     C                   MOVEL     'S'           WGIAC             1
032900170125     c                   endif
033000161201     c*
033100170124     c* se non è partito con NEW IMA considero  la spedizione GIACENTE  come prima
033200170201   5ac*****              if        not *in22 and olgi1AMG='S'
033300170201   5ac                   if        not *in22 and WGIAC='D'
033400161201     C                   MOVEL     'S'           WGIAC             1
033500161201     c                   endif
033600161201     c
033700170124     c* IPG elaboro le giacenze da portare a mag giac OGGI ( non facciamo più)
033800170124   5ac** 11              if        olgi1AMG='S' or olgi1AMG='Y'
033900170124     C**                 MOVEL     'D'           WGIAC             1
034000170124     c**                 endif
034100160823     c
034200960516     C                   ENDIF
034300970721     C                   ENDIF
034400950810     C*
034500161025     C* SE IMA --> ESCLUDO LE GIACENTI ma includo le bolle che devono essere portate a mag giac
034600170124     c*            se partita la procedura NEWIMA
034700950810    5C     *IN10         IFEQ      *OFF
034800960516     C     WGIAC         ANDNE     'S'
034900170124     c     *in22         andeq     *on
035000170124     c***  *in20         andeq     *on
035100161130
035200161130    5C     *IN10         orEQ      *OFF
035300161130     C     WGIAC         ANDeq     ' '
035400170124     c     *in22         andeq     *off
035500170124     c**** *in20         andeq     *off
035600950810     C*
035700950810     C* SE IMG --> INCLUDO SOLO LE GIACENTI
035800161026     C*        --> INCLUDO anche le bolle da portare a mag.giac. ma senza stampare il mancante
035900950810     C     *IN10         OREQ      *ON
036000161025     C     *IN11         andeq     *Off
036100161026     C     WGIAC         ANDne     ' '
036200161025
036300170124     C* SE IPG --> INCLUDO SOLO quelle da portare a mag giac ( eliminato)
036400170124     C**   *IN10         OREQ      *ON
036500170124     C**   *IN11         andeq     *ON
036600170124     C**   WGIAC         ANDEQ     'D'
036700921013     C*
036800950317     C* ESCLUDO  SE BOLLA MAMMA E UNA DELLE FIGLIE E' IN CONSEGNA
036900960516     C*     O SE SONO TUTTE BLOCCATE
037000950810     C                   Z-ADD     ARBSPE        SAVSPE
037100960516     C*
037200960516     C                   CLEAR                   WESFI                          ESISTE FIGL
037300960516     C                   MOVEL     'S'           WFIBLO            1            FIGLIE BLOCC
037400950317     C                   SETOFF                                       36
037500950810     C     KSAV          SETLL     FNLBL02L
037600950810     C     KSAV          READE     FNLBL02L                               35
037700960516     C**
037800950810    6C     *IN35         DOWEQ     *OFF
037900950317     C*
038000950317     C     KLBL          CHAIN     FNARB01L                           35
038100950810    7C     *IN35         IFEQ      *OFF
038200960516     C                   MOVEL     'S'           WESFI             1
038300960516     C* FIGLIA IN CONSEGNA
038400960516    8C     ARBNDC        IFNE      0
038500950317     C                   SETON                                        3536
038600960516   X8C                   ELSE
038700960516     C*
038800960516     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
038900960516    9C     ARBFBC        IFNE      'B'
039000960516     C                   MOVEL     'N'           WFIBLO
039100960516    9C                   ENDIF
039200960516    8C                   ENDIF
039300960516    7C                   ENDIF
039400950317     C*
039500950810     C  N36KSAV          READE     FNLBL02L                               35
039600950810    6C                   ENDDO
039700960516     C*
039800960516     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
039900960516     C  N36WESFI         IFEQ      'S'
040000960516     C     WFIBLO        ANDEQ     'S'
040100960516     C                   SETON                                        36
040200960516     C                   ENDIF
040300070418     c
040400070418     c* Per 36 off - controllo in IMA se richiesti anche i colli mancanti
040500070418     c*              di spedizioni in distinta
040600070418     c  n36              if        not *in10 and vidmdc='N' and arbndc>0
040700070418     c                   seton                                        36
040800070418     c                   endif
040900911204     C*
041000950810    6C  N36              DO
041100950317     C*
041200040427     C     KSAV          CHAIN     FNARB01L                           31
041300950810     C                   MOVEL     ARBAMA        SAVAMA            1 0
041400040428     c                   clear                   wchain            1
041500161025     c                   clear                   wesspu            1
041600911018     C* LEGGO DETTAGLIO
041700941017     C     KART          SETLL     FNART01L
041800040428     C  N10
041900040428     CAN 02KART          READE     FNART01L                               31
042000040428     C   10
042100040428     CORN02KART          READE(N)  FNART01L                               31
042200911018     C*
042300950810    7C     *IN31         DOWEQ     *OFF
042400911204     C                   SETOFF                                       34
042500950810     C*
042600950810     C* ESCLUDO  SE IL COLLO E' CONSEGNATO (CONSEGNA PARZIALE)
042700070418     C     ARTDCM        IFEQ      0
042800950810     C  N10              EXSR      ELAIMA
042900950810     C   10              EXSR      ELAIMG
043000950810    8C                   ENDIF
043100930629     C*
043200040428     C  N10
043300040428     CAN 02KART          READE     FNART01L                               31
043400040428     C   10
043500040428     CORN02KART          READE(N)  FNART01L                               31
043600950810    7C                   ENDDO
043700170124     c* per IMA  e IMG aggiorno la giacenza con la data entrata a mag giac se spuntato
043800170124     c*       (IPG viene eliminato quindi uso ima al suo posto)
043900170125     c*  almeno un collo e data = 0  solo in fase "definitivo"
044000170321     c* per le filiali che fanno l'IDD il giorno dopo, aggiorno anche per ima provvisorio
044100170125     c*
044200170124    7c*****              if        *in22 and *in10 and wesspu='S' and
044300170321    7c*****              if        *in22 and *in02 and wesspu='S' and
044400170321     c
044500170321    7c                   if        *in22 and wgiac<>' ' and wesspu='S' and
044600161025     C                             olgi1amg='S'
044700170321     c                   if        *in02 or iddgdopo='S'
044800161025     C     KGCA          CHAIN     Tigcp21L
044900161025    8c                   if        %found(tigcp21l)
045000161025     C                   MOVE      dateu         GCPDLMa
045100161025     C                   EXCEPT    AGG5
045200161025    8C                   ENDIF
045300170321     c
045400161025    7c                   endif
045500170321    7c                   endif
045600920318     C*
045700950810    6C                   ENDDO
045800950810    5C                   ENDIF
045900161025     c
046000000512    4C                   ENDIF
046100050511   3aC                   ENDIF
046200911018     C*
046300040427     C     KLNA          READE     FNARB05L                               30
046400950810    3C                   ENDDO
046500941019     C*
046600040108     C                   ADD       1             WA                             CONTATORE L6
046700950810    1C                   ENDDO
046800161017     c
046900920813     C*
047000920813     C* RIEPILOGO SPUNTE PER PISTOLA
047100920813     C                   EXSR      RIEPIL
047200911018     C*
047300060508     C* SE NON TROVATE DISCORDANZE e non fatte parzializzazioni
047400060508     c*  se non ho parzii
047500070418     c* 18/04/07 - IMA chiude solo se richiesta forzatura chiusura
047600960530     C     *IN01         IFEQ      *OFF
047700070418     C     *IN10         andeq     *ON
047800161017     C     *IN11         andeq     *Off
047900060508     c
048000160503     c     wselzona      andeq     ' '
048100060508     c
048200060508     c     vidlna        andeq     0
048300060508     c
048400060508     C*  O FORZATA LA CHIUSURA METTO FLAG
048500911018     C     VIDSN         OREQ      'S'
048600060508     c
048700941017     C     KFVV          CHAIN     FNFVV01L                           32
048800920813     C  N32              MOVEL     'S'           FVVFCF
048900941017     C  N32              UPDATE    FNFVV000
049000911018     C                   END
049100050511     C*
049200081014     C* CHIUSURA PGM
049300990212     C                   CLEAR                   DSBS02
049400990212     C                   MOVEL     'C'           T02TLA
049500990212     C                   CALL      'TIBS02R'
049600990212     C                   PARM                    KPJBA
049700990212     C                   PARM                    DSBS02
049800031219     c
049900031219     C                   CLEAR                   DSLV53
050000031219     C                   MOVEL     'C'           D53TLA
050100031219     C                   CALL      'FNLV53R'
050200031219     C                   PARM                    DSLV53
050300040806     C*
050400040806     C                   MOVEL     'C'           I12TLA
050500040806     C                   CALL      'FIDN12R'
050600040806     C                   PARM                    FIDN12DS
050700140430     C*
050800140430     c                   clear                   fnlrv7ds
050900140430     C                   MOVEL     'C'           ILRV7TLA
051000140430     c                   eval      kpjbu= fnlrv7ds
051100140430     C                   CALL      'FNLRV7R'
051200140430     C                   PARM                    KPJBA
051300160927
051400160927     c                   clear                   fnlgi1ds
051500160927     c                   eval      ILGI1TLA= 'C'
051600160927     c                   movel     fnlgi1ds      kpjbu
051700160927     c                   call      'FNLGI1R'
051800160927     c                   parm                    kpjba
051900160927
052000040806      *
052100000000     C                   SETON                                        LR
052200031219     C**************************************************************************
052300031219     C* CONTROLLO SE SEGNACOLLO MANCANTE
052400031219     C**************************************************************************
052500031219     C     CTRART        BEGSR
052600031219     C*
052700031219     C                   SETOFF                                       131415
052800100706     C                   SETOFF                                       3316
052900161025     C                   MOVE      *BLANKS       STADSF           17
053000031219     C*
053100070118     C     KBRV1         SETLL     FNBRV01L                               33
053200070418     c
053300161017     c* se non ho trovato spunta IMx e spedizione in distinta, cerco
053400070418     c*  la categoria 4 del giorno
053500070418    1c                   if        not *in33 and arbndc>0 and vidmdc='S'
053600100706     C                   Z-ADD     4             conNPG
053700140618     C     KBRV4         chain     FNBRV05L
053800140618    2c                   if        %found(fnbrv05l)
053900070418     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
054000070418     c                   clear                   dslv53
054100070418     C                   z-add     brvnFV        D53NFV
054200070418     C                   MOVEL     brvNPG        D53NPG
054300070418     C                   MOVEL     brvFGS        D53FGS
054400070418     C                   CALL      'FNLV53R'
054500070418     C                   PARM                    DSLV53
054600070418     C* ERRORE
054700070418     c*  se sparato nella stessa data: ok
054800070418     c*  e sparato in altra data, rientro nei giri normali di controllo
054900070418     c*  spunte
055000070418    3c                   if        d53err=' ' and d53dfv=savdfv
055100070418     c                   seton                                        33
055200070418    3c                   endif
055300070418    2c                   endif
055400070418    1c                   endif
055500070418     c
055600031219     C* 14 ON - SPUNTA NON TROVATA
055700031219    1C     *IN33         IFEQ      *OFF
055800070418     c
055900031219     C                   SETON                                        14
056000031219     c*
056100031219     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
056200031219     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
056300031219     c*  immissione + alta
056400031219     C                   exsr      RICSPU
056500031219     C
056600031219     C* SE SPUNTA TROVATA CON DATA > STAMPO MA NON CREO ANOMALIA
056700170213     c
056800170213     c* Se filiale che fa l'IDD il giorno dopo e trovo spunta IMA con data >
056900170213     c*  non stampo nemmeno e dico che la spunta è presente
057000170213     c*  non è detto che questo vada bene per la nuova procedura IMG. Da verificare con
057100170213     c*  DLO (es Salerno)
057200170213    2c                   if        not *in10 and iddgdopo='S' and
057300170213     c                             wdfv>savdfv and wspg='A'
057400170213     c                   setoff                                       14
057500170213   x2c                   else
057600170213     c
057700170213    3C                   IF        WDFV>SAVDFV
057800031219     C                   EVAL      *IN13=*ON
057900170213    3C                   ENDIF
058000031219     C
058100031219     C* SE SPUNTA TROVATA CON DATA = STAMPO E CREO ANOMALIA SOLTANTO SE
058200031219     C*  CATEGORIA 2 O 6
058300170213    3C                   IF        WDFV=SAVDFV
058400031219     C                   EVAL      *IN13=*ON
058500170213    4C                   IF        BRVNPG=2
058600031219     C                   EVAL      *IN15=*ON
058700170213    4C                   ENDIF
058800170213    3C                   ENDIF
058900170213    2C                   ENDIF
059000031219     c*
059100031219    1C                   END
059200960523     C*
059300031219     C
059400031219    1C                   IF        WDFV>=SAVDFV
059500960523     C* GIRO LA DATA
059600031219     C                   MOVE      WDFV          G02INV
059700960523     C                   MOVE      *ZEROS        G02DAT
059800960523     C                   MOVEL     3             G02ERR
059900960523     C                   CALL      'XSRDA8'
060000960523     C                   PARM                    WLBDAT
060100960523     C                   MOVEL     G02DAT        ST2DFV            4 0
060200960523     C**
060300960523     C* DECODIFICO LA FILIALE GESTIONE
060400031219     C     BRVFGS        CHAIN     AZORG01L                           32
060500031219    2C  N32ORGDE5        IFNE      *BLANKS
060600960523     C                   MOVEL     ORGDE5        ST2FGS            8
060700960523     C                   ELSE
060800960523     C                   MOVEL     ORGDES        ST2FGS            8
060900031219    2C                   ENDIF
061000960523     C**
061100960523     C* DECODIFICO LA CATEGORIA SPUNTA
061200031219    2C     BRVNPG        IFEQ      3
061300161017     C                   MOVEL     '7B'          COD
061400161017     C                   MOVEL(P)  wspg          KEY
061500161017     C     KTAB          CHAIN     TABEL                              32
061600161017     C  N32              MOVEL     TBLUNI        ds7b
061700161017     C   32              CLEAR                   ds7b
061800161017     C                   MOVEL     §7bdesb       ST2CAT            3
061900031219   X2C                   ELSE
062000960523     C                   MOVEL     '7N'          COD
062100031219     C                   MOVEL(P)  BRVNPG        KEY
062200960523     C     KTAB          CHAIN     TABEL                              32
062300960523     C  N32              MOVEL     TBLUNI        ST2CAT
062400960523     C   32              CLEAR                   ST2CAT
062500031219    2C                   ENDIF
062600031219     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
062700031219    2C                   IF        BRVNPG=4
062800031219     C                   Z-ADD     BRVNFV        WNFV5
062900031219     C     KFVV2         CHAIN(N)  FNFVV000                           32
063000161026     C  N32              eval      stadsf=fvvdsf
063100031219    2C                   ENDIF
063200031219    1C                   ENDIF
063300960523     C*
063400100706     c* Se trovata spunta, segnalo se sparata xx volte in IMA
063500100706     c*  solo per bolle in IMA più vecchie di ....
063600100707     c* Escludo bolle in distinta
063700150113     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
063800100707     c                   if        not *in14 and not *in10
063900100706     c                             and arbndc=0 and arbdsp<datapresIMA
064000100706     c
064100100706     c* conto quante volte presente in ima
064200100706     C                   Z-ADD     3             conNPG
064300100706     c                   clear                   conta             4 0
064400140618     C     KBRV4         setll     FNBRV05L
064500140618     C     KBRV4         reade     FNBRV05L
064600100706     c
064700140618    2c                   dow       not %eof(fnbrv05l)
064800100706     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
064900100706     c                   clear                   dslv53
065000100706     C                   z-add     brvnFV        D53NFV
065100100706     C                   MOVEL     brvNPG        D53NPG
065200100706     C                   MOVEL     brvFGS        D53FGS
065300100706     C                   CALL      'FNLV53R'
065400100706     C                   PARM                    DSLV53
065500100706     c
065600100706     c                   if        d53spg='A'
065700100706     c                   eval      conta=conta+1
065800100706     c                   endif
065900100706     c
066000140618     C     KBRV4         reade     FNBRV05L
066100100706     c                   enddo
066200100706     c
066300100706     c                   if        conta>=§vpoggima
066400100706     c                   seton                                        16
066500100706     c                   endif
066600100706     c
066700100706     c
066800100706     c                   endif
066900100706     c
067000960523     C                   ENDSR
067100960523     C*
067200031219     C**************************************************************************
067300031219     C* cerco spunta del collo in qualsiasi categoria
067400031219     C**************************************************************************
067500031219     C     RICSPU        BEGSR
067600031219     C                   CLEAR                   Wimm
067700031219     C                   CLEAR                   WSAVB
067800031219     C                   CLEAR                   WDFV
067900031219     C                   CLEAR                   WTROV
068000040108     C                   CLEAR                   WSPG
068100070118RM*  C     Kartd         CHAIN     FNBRV07l                           33
068200031219RM*  C*
068300031219RM* 1C     *IN33         DOWEQ     *OFF
068400031219     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
068500031219     c                   clear                   dslv53
068600031219     C                   z-add     brvnFV        D53NFV
068700031219     C                   MOVEL     brvNPG        D53NPG
068800031219     C                   MOVEL     brvFGS        D53FGS
068900070118     C****               MOVEL     brvfle        D53flf
069000031219     C                   CALL      'FNLV53R'
069100031219     C                   PARM                    DSLV53
069200031219     C* ERRORE
069300031219    2C     D53ERR        IFeq      ' '
069400070118     c                   movel     brvdfs        brvimm           14 0
069500031219     c                   move      brvhms        brvimm
069600031219    3c                   select
069700031219     c* Se maggiore memorizzo
069800031219     c                   when      d53dfv>wdfv
069900031219     C                   MOVEL     RECBRV        WSAVB
070000070118     C                   MOVEL     brvimm        Wimm             14 0
070100031219     C                   MOVEL     d53DFV        WDFV
070200040108     C                   MOVEL     d53spg        WSPG
070300031219     C                   MOVEL     'S'           WTROV             1
070400031219     c
070500031219     c* Se uguale tengo la data ora immissione + alta
070600031219     c                   when      d53dfv=wdfv  and brvimm>wimm
070700070118     C                   MOVEL     brvimm        Wimm             14 0
070800031219     C                   MOVEL     RECBRV        WSAVB
070900031219     C                   MOVEL     d53DFV        WDFV
071000040108     C                   MOVEL     d53spg        WSPG
071100031219     C                   MOVEL     'S'           WTROV             1
071200031219     C                   eval      wimm=brvimm
071300040129     c* Se stessa data e ora immissione, tengo cat 3 o 4
071400101202     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
071500070118     C                   MOVEL     brvimm        Wimm             14 0
071600040129     C                   MOVEL     RECBRV        WSAVB
071700040129     C                   MOVEL     d53DFV        WDFV
071800040129     C                   MOVEL     d53spg        WSPG
071900040129     C                   MOVEL     'S'           WTROV             1
072000040129     C                   eval      wimm=brvimm
072100031219    3c                   endsl
072200031219    2c                   endif
072300031219     c
072400070118RM*  C     Kartd         reade     FNBRV07l                               33
072500031219    1c                   enddo
072600031219     c
072700031219     C     WTROV         IFEQ      'S'
072800031219     C                   MOVEL     WSAVB         RECBRV
072900031219     C                   ENDIF
073000031219     c                   endsr
073100941017     C**************************************************************************
073200941017     C* IMPOSTO CAMPI FISSI
073300941017     C**************************************************************************
073400911018     C     IMPO          BEGSR
073500921022     C*
073600101202     C                   Z-ADD     1             CODUT             1 0
073700921022     C                   Z-ADD     3             IMANPG
073800950810     C                   CLEAR                   SAVSPE
073900950810     C                   CLEAR                   COMSPE
074000921022     C                   TIME                    UTIME             6 0
074100960927     C                   TIME                    W0140            14 0
074200960927     C                   MOVE      W0140         UDATE8            8 0
074300101202      *
074400101202      * Reperimento dati utente/aziendali
074500101202     c                   exsr      sr_DatiJob
074600161130     c
074700161130     c* per ora solo le filiali abilitate all'attesa in IMA hanno ima e img modificato
074800170124     c**                 clear                   trulvpods
074900170124     c**                 eval      ivpoke1 = 'DECOFIIMG'
075000170124     c**                 call      'TRULVPOR'
075100170124     c**                 parm                    trulvpods
075200161130
075300170124     c**                 if         %lookup('999':skFilOk)>0    or
075400170124     c**                            %lookup(%editc(vidfil:'X'):skFilOk)>0
075500170124     c**                 seton                                        20
075600170124     c**                 endif
075700161130     c
075800170124     c* Solo se filiale partita con NUOVO IMA devo aggiornare la data entrata mag giac
075900170124     c* ima e IMG modificati sempre per NEW IMA e non per i 2 gg in ima
076000161130     c                   clear                   trulvpods
076100170124     c                   eval      ivpoke1 = 'DECOFIIPG'
076200161130     c                   call      'TRULVPOR'
076300161130     c                   parm                    trulvpods
076400161130
076500161130     c                   if         %lookup('999':skFilOk)>0    or
076600161130     c                              %lookup(%editc(vidfil:'X'):skFilOk)>0
076700161130     c                   seton                                        22
076800161130     c                   endif
076900161130     c
077000170124     C* 11 e 10 ON - IPG (eliminato)
077100170124     c                   select
077200170124     c***                when      vidtin='Z'
077300170124     C***                SETON                                        1011
077400161017     C* 10 ON - IMG
077500161017     C                   when      vidtin='G'
077600950810     C                   SETON                                        10
077700161014     C                   ENDsl
077800921022     C*
077900921022     C* CHAIN E DATA DEL FOGLIO IMA
078000941017     C     KFVV          CHAIN(N)  FNFVV01L                           32
078100921022     C*
078200941018     C                   Z-ADD     FVVDFV        G02INV                         AAAA/MM/GG
078300941018     C                   Z-ADD     *ZERO         G02DAT
078400941018     C                   MOVEL     '3'           G02ERR
078500941018     C                   CALL      'XSRDA8'
078600941018     C                   PARM                    WLBDAT
078700941018     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
078800950316     C*
078900950317RM*  C                   Z-ADD     FVVDFV        SAVDFV
079000950317RM*  C                   Z-ADD     FVVFGS        SAVFGS
079100931228     C*
079200941028     C* DECODIFICA FILIALE DI APERTURA FOGLIO
079300941018     C     FVVFGS        CHAIN     AZORG                              32
079400931228     C  N32              MOVEL     ORGDES        DESCDU           20
079500000512     C*
079600000512     C                   Z-ADD     15            ANMCAA
079700000512     C                   MOVEL     '7C'          COD
079800000512     C                   MOVEL(P)  ANMCAA        KEY
079900000512     C     KTAB          CHAIN     TABEL00F                           35
080000000512     C  N35              MOVEL     TBLUNI        DS7C
080100000512     C   35              MOVEL     *BLANKS       DS7C
080200911018     C*
080300931228     C* 02 ON -  D E F I N I T I V O
080400911018     C     VIDSN         IFEQ      'S'
080500911018     C                   SETON                                        02
080600911018     C*
080700921022     C* PRENDO FASE ANOMALIA
080800950810     C                   CLEAR                   PARAM5
080900950810     C                   MOVEL     'L'           PA5TLA
081000950810     C                   MOVEL     3             PA5NPG
081100950810     C                   MOVEL     VIDTIN        PA5SPG
081200950810     C                   CALL      'TRUL11R'
081300950810     C                   PARM                    PARAM5
081400920409     C                   END
081500920813     C*
081600920813     C* DECODIFICA FILIALE IN GESTIONE
081700920813     C     VIDFIL        CHAIN     AZORG                              32
081800920813     C  N32              MOVEL     ORGDES        DESFIL           20
081900170213     c* Verifico se fa l'IDD il giorno dopo
082000170213     c                   movel     orgde8        og148
082100170213     c*
082200170213     c                   if        §OGEID='S'
082300170213     c                   eval      iddgdopo='S'
082400170213     c                   endif
082500941019     C*
082600101215     C* CARICO L6
082700101215     C                   CLEAR                   DSUL06
082800101215     C                   MOVE      '£6'          D06COD
082900101215     C                   MOVEL     VIDFIL        D06KEY
083000101215     C                   MOVEL     'L'           D06TLA
083100101215     C                   MOVEL     DSUL06        KPJBU
083200101215     C                   CALL      'TRUL06R'
083300101215     C                   PARM                    KPJBA
083400101215     C                   MOVEL     KPJBU         DSUL06
083500101215     C                   MOVEA     LIN           Lspu
083600060508     c*
083700101215     C*  Se non parzializzata la linea di arrivo, elaboro L6
083800161014     c*  altrimenti elaboro solo lei
083900060508     c                   if        vidlna=0
084000101215     C                   MOVEA     LIN           L6
084100060508     c                   else
084200060508     c                   clear                   L6
084300060508     c                   movel     vidlna        l6(1)
084400060508     c
084500060508     c* Decodifica
084600060508     C     VIDlna        CHAIN     AZORG                              32
084700060508     C  N32              MOVEL     ORGDES        DESlna           20
084800060508     c                   movel     vidlna        stalna            3
084900060508     c                   endif
085000070418     c
085100070418     c* Elabora anche spedizioni in distinta
085200070418     c                   if        vidmdc='S'
085300070418     c                   eval      stamdc='SI'
085400070418     c                   else
085500070418     c                   eval      stamdc='NO'
085600070418     c                   endif
085700101215     c* solo colli spuntati alla linea di arrivo finale
085800101215     c                   if        parimaa='S'
085900101215     c                   eval      staimaa=csololna
086000101215     c                   endif
086100160503     c* Verifico le richieste delle zone di consegna
086200160503     c                   clear                   wParc             1
086300160503     c                   clear                   wParc1            1
086400160503     c                   clear                   wParc2            1
086500160503     c                   clear                   wMess             1
086600160503     c                   clear                   wSelzona          1
086700160503     c                   if        partpm1='P ' or partpm2='P '
086800160503     c                   eval      wparc='S'
086900160503     c                   eval      wselzona='S'
087000160503     c                   endif
087100160503     c                   if        partpm1='M ' or partpm2='M '
087200160503     c                   eval      wMess='S'
087300160503     c                   eval      wselzona='S'
087400160503     c                   endif
087500160503     c                   if        partpm1='P1' or partpm2='P1'
087600160503     c                   eval      wParc1='S'
087700160503     c                   eval      wselzona='S'
087800160503     c                   endif
087900160503     c                   if        partpm1='P2' or partpm2='P2'
088000160503     c                   eval      wParc2='S'
088100160503     c                   eval      wselzona='S'
088200160503     c                   endif
088300060508     c
088400050510     c* se richiesta soltanto zone parcel o messagerie, carico in
088500050510     c*  schiera
088600160503     c                   if        wparc='S' and wmess='S'
088700160503     c                   clear                   wparc
088800160503     c                   clear                   wmess
088900160503     c                   clear                   wselzona
089000160503     c                   endif
089100160503     c                   if        wparc1='S' and wparc2='S'
089200160503     c                   clear                   wparc1
089300160503     c                   clear                   wparc2
089400160503     c                   eval      wparc='S'
089500160503     c                   endif
089600160503     c
089700160503    1c                   if        wselzona='S'
089800050511     c                   clear                   xx                4 0
089900160502     c                   clear                   yy                4 0
090000050510     c                   movel     vidfil        alffil            3
090100050511     c                   movel     vidfil        wpo
090200050510     c                   movel(P)  '03'          cod
090300050510     c                   movel(P)  vidfil        key
090400050510     c     ktab          setll     tabel
090500050510     c     ktab2         reade     tabel
090600160502    2c                   dow       not %eof(tabel00f) and  wpo=alffil
090700160502    3c                   if        tblflg=' '
090800050510     c                   movel     tblkey        wpo               3
090900160502    4c                   if        wpo=alffil
091000050511     c                   movel     tbluni        ds03
091100160502
091200160502     c* parcel/messaggerie
091300160503    5c                   if        wparc='S' or wmess='S'
091400160502
091500160503    6c                   if        (wmess='S' and §03tpm='M') or
091600160503     c                             (wparc='S' and §03tpm<>'M')
091700050511     c                   add       1             xx
091800050511     c                   movel     tblkey        w005a             5
091900050511     c                   move      w005a         zone(xx)
092000160502    6c                   endif
092100160502    5c                   endif
092200160502     c* parcel 1° e 2° turno
092300160503    5c                   if        (wparc1='S' or wparc2='S')
092400160503     c                             and §03tpm<>'M'
092500160502
092600160503    6c                   if        (wparc2='S' and §03turno='2') or
092700160503     c                             (wparc1='S' and §03turno<>'2')
092800160503     c                   add       1             yy
092900160502     c                   movel     tblkey        w005a             5
093000160503     c                   move      w005a         zone12(yy)
093100160502    6c                   endif
093200160502    5c                   endif
093300160502     c
093400160502    4c                   endif
093500160502    3c                   endif
093600160502     c
093700050511     c     ktab2         reade     tabel
093800160502    2c                   enddo
093900160502    1c                   endif
094000050511     c
094100160503     c                   clear                   statipo
094200160503     c                   clear                   statipo2
094300160503     c                   if        partpm1='P ' or partpm2='P '
094400050511     c                   eval      statipo=cparcel
094500050511     c                   endif
094600050511     c
094700160503     c                   if        partpm1='M ' or partpm2='M '
094800160503     c                   if        statipo=*blanks
094900050511     c                   eval      statipo=cmessag
095000160503     c                   else
095100160503     c                   eval      statipo2=cmessag
095200160503     c                   endif
095300050511     c                   endif
095400160503     c                   if        partpm1='P1' or partpm2='P1'
095500160503     c                   if        statipo=*blanks
095600160503     c                   eval      statipo=cparc1
095700160503     c                   else
095800160503     c                   eval      statipo2=cparc1
095900160503     c                   endif
096000160503     c                   endif
096100160503     c                   if        partpm1='P2' or partpm2='P2'
096200160503     c                   if        statipo=*blanks
096300160503     c                   eval      statipo=cparc2
096400160503     c                   else
096500160503     c                   eval      statipo2=cparc2
096600160503     c                   endif
096700160503     c                   endif
096800941017     C*
096900100706      * Aggancio tabella VPO recpord VPO
097000100706     c                   Clear                   Dvpo
097100100706     c                   Clear                   dsbs02
097200100706     c                   Eval      T02Mod = 'C'
097300100706     c                   Eval      T02tla = 'L'
097400100706     c                   Eval      T02Sif = knsif
097500100706     c                   Eval      T02Cod = 'VPO'
097600100706     c                   Movel(p)  'VPO'         T02Ke1
097700100706     c                   Call      'TIBS02R'
097800100706     c                   Parm                    Kpjba
097900100706     c                   Parm                    dsbs02
098000100706     c                   If        T02Err = *Blanks
098100100706     c                   Movel     T02Uni        Dvpo
098200100706     c                   EndIf
098300100706     c
098400100706     c     *eur          move      udate8        dataeur
098500100706     c                   movel     dataeur       dataiso
098600161017     c
098700161017     c                   movel     dataiso       dateu             8 0
098800161017
098900100706     c                   if        §vpoggima>0
099000100706     c                   subdur    §vpoggima:*d  dataiso
099100100706     c                   endif
099200100706     c
099300100706     c                   movel     dataiso       datapresIMA       8 0
099400100706
099500911018     C                   ENDSR
099600941017     C**************************************************************************
099700941017     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
099800941017     C**************************************************************************
099900911018     C     ERRORE        BEGSR
100000941017     C*
100100911015     C                   SETON                                        01
100200040603     c
100300040603     c     kar5          chain     fiar501l
100400040603     c                   if        %found(fiar501l)
100500040603     c                   movel     ar5uni        dar5gen
100600040603     c                   else
100700040603     c                   clear                   dar5gen
100800040603     c                   endif
100900911018     C* STAMPA ANOMALIA
101000040428     C                   EXSR      STAMPA
101100081014     c
101200100706     c* non creo anomalia se collo doppio o solo segnalazione
101300161017    0c                   if        not *in12 and not *in16 and not *in11
101400161018     c                             and not *in18
101500170124     c* in IMA e IMG la creo solo se non è da portare a mag giac  se partito con NEW IMA
101600161025     c                             and wgiac<>'D'
101700911015     C*
101800950707     C* SOLO SE IMA DEFINITIVO  CREO ANOMALIA
101900950608    1C     VIDSN         IFEQ      'S'
102000950810     C* NON DEVO CREARE:
102100950810     C* 1) SE C'E' LA FORZATURA E MANCA LA DATA DI ARRIVO
102200950810     C*    ANCHE SE STAMPO LO STESSO
102300950810     C* 2) SE LA SPUNTA E' STATA TROVATA IN ALTRA CATEGORIA DEL GIORNO
102400960419     C*    3 O 4 NON DISABILITO NEMMENO LA BOLLA
102500960419     C* 3) SE LA SPUNTA E' STATA TROVATA IN ARRIVO CREO L'ANOMLIA
102600950608    2C     ARTDAM        IFGT      0
102700960419   2AC     *IN13         IFEQ      *OFF
102800031219     C     *IN15         OREQ      *ON
102900081014i    C*
103000081104     C     KANM          SETLL     FNANM02L                               35
103100081014    3c                   if        not *in35
103200081014     c* Se anomalia chiusa non in pratica idd e data chiusura <data ima
103300081014     c*  la ricreo
103400081104    4c***                if        anmdch>0 and anmdch<savdfv and
103500081104     c***                          anmcch<>'PR'
103600081104     c***                delete    fnanm000
103700081104     c***                seton                                        35
103800081104    4c***                endif
103900081104    3c***                endif
104000081014     c
104100081104    3C***                if        *in35
104200081104     C*
104300081104     C                   MOVEL     *BLANKS       ANMATB
104400081104     C                   MOVEL     PA5FAS        ANMFSA
104500160503     c*  data salvata del foglio IMA perchè richaino in update
104600100506     C                   Z-ADD     SAVdfv        ANMDAO
104700081104     C                   Z-ADD     15            ANMcaa
104800081104     C                   MOVEL     *BLANKS       ANMCCH
104900081104     C                   MOVEL     VIDFGS        ANMCDU
105000081104     C                   Z-ADD     COMNFV        ANMNFV
105100081104     C                   MOVEL     VIDFGS        ANMFLE
105200081104     C                   MOVE      *BLANKS       ANMFLO
105300081104     C                   MOVEL     *BLANKS       ANMFT1
105400081104     C                   MOVEL     *ZEROS        ANMFL1
105500081104     C                   MOVEL     *BLANKS       ANMFT2
105600081104     C                   MOVEL     *ZEROS        ANMFL2
105700081104     C*
105800081104     C                   MOVEL     §7CTAN        ANMTAN
105900081104     C                   MOVEL     §7CSAN        ANMSAN
106000081104     C                   MOVEL     §7CAIE        ANMAIE
106100081104     C                   Z-ADD     §7CLID        ANMLID
106200081104     C                   MOVE      *ZEROS        ANMDUE
106300081104     C                   MOVE      *ZEROS        ANMNPG
106400081104     C                   MOVE      *ZEROS        ANMFGS
106500081104     C                   Z-ADD     0             ANMDCH
106600081104     C                   MOVEL     ' '           ANMFSC
106700081104     C* CHIUSURA AUTOMATICA
106800081104     C     §7CCHA        IFEQ      'S'
106900100506     C                   Z-ADD     SAVDFV        ANMDCH
107000081104     C                   MOVEL     PA5FAS        ANMFSC
107100081104     C                   END
107200081104     C                   MOVE      *BLANKS       ANMFT3
107300081104     C                   MOVE      *ZEROS        ANMFL3
107400081104     C                   MOVE      *BLANKS       ANMFT4
107500081104     C                   MOVE      999           ANMFL4
107600081104     C                   MOVE      *ZEROS        ANMDTR
107700081104     C*
107800911018     C                   MOVEL     ARTFLS        ANMFLS
107900911018     C                   MOVEL     ARTLNA        ANMLNA
108000911018     C                   MOVEL     ARTNRS        ANMNRS
108100941018     C                   MOVEL     ARTNSC        ANMSCN
108200941018     C                   Z-ADD     ARTAAS        ANMAAS
108300911018     C                   MOVEL     ARTNSP        ANMNSP
108400911018     C                   MOVEL     ARTLNP        ANMLNP
108500970206     C                   MOVEL     ARTNAP        ANMNPS
108600040603     c
108700040603     c* imposto data e ora di creazione
108800050707     C                   TIME                    W0140            14 0
108900050707     C                   MOVE      W0140         G02DAT
109000050707     C                   Z-ADD     *ZERO         G02INV
109100050707     C                   MOVEL     *BLANKS       G02ERR
109200050707     C                   CALL      'XSRDA8'
109300050707     C                   PARM                    WLBDAT
109400050707     C                   Z-ADD     G02INV        WDATA             6 0
109500050707     c**                 time                    wora              6 0
109600050707     c**                 z-add     *date         wdata             6 0
109700050707     C
109800050707     c                   movel     w0140         wora4             4 0
109900040603     c                   movel     wdata         anmflo
110000040603     c                   move      wora4         anmflo
110100040603     c
110200040603     c* imposto se flag di valore
110300040603     c                   movel     §ar5bva       anmft4
110400040603     c
110500040603     c
110600920409     C* ESTERNA
110700950608    4C     §7CAIE        IFEQ      'E'
110800950608     C                   MOVEL     ARTLNP        ANMFL1
110900950810    4C                   ENDIF
111000920409     C*
111100990511     C                   WRITE     FNANM000
111200081014    3C                   endif
111300081014     c
111400000512   2AC                   ENDIF
111500960419    2C                   ENDIF
111600920318     C*
111700000512     C* DISABILITO LA BOLLA SOLO SE HO CREATO ANOMALIA
111800000512     C* NO POSTE SEMPRE, LNP POSTE SOLO SE PREVOSTO
111900950810     C* SOLO SE NON HO TROVATO UN'ALTRA SPUNTA IN DATA DEL FOGLIO IMA
112000960419    2C     *IN10         IFEQ      *OFF
112100960419     C*
112200960419    3C     *IN13         IFEQ      *OFF
112300031219     C     *IN15         OREQ      *ON
112400000512     C*
112500040428    5c     wchain        ifeq      *blanks
112600040428     c     fnarbnrri     chain(e)  fnarb00f
112700040428    6c                   if        %error
112800040428     C* Valorizzo la DS di procedura
112900040428     C                   CLEAR                   TRUL82DS
113000040428     c                   eval      ul82§rrn = fnarbnrri
113100040428     C                   EVAL      UL82§FIL = 'FNARB00F'
113200040428     C                   EVAL      UL82§WIN = 'N'
113300040428     C                   EVAL      UL82§F7  = 'S'
113400040428     C                   EVAL      UL82§num = 5
113500040428     C                   EVAL      UL82§att = 60
113600040428     C                   EVAL      UL82§mss = msgjob
113700040428     C* Effettuo la chiamata al *PGM d utilità
113800040428     C                   CALL(e)   'TRUL82R'
113900040428     C                   PARM                    TRUL82DS
114000040428     C     fnarbnrri     chain     fnarb00f
114100040428    6c                   endif
114200040428     C                   Z-ADD     0             ARBAMA
114300040428     c                   update    fnarbf00
114400040428     c                   move      '1'           wchain
114500040428    5c                   endif
114600930629     C* AGGIORNO IL COLLO CON UNA 'D' DI DISABILITAZIONE
114700941221     C                   MOVEL     'S'           ARTDAB                         'D'=DISABILITATO
114800000512    3C                   ENDIF
114900960419    2C                   ENDIF
115000950810     C*
115100040428     C  N10              UPDATE    FNART000
115200950810    1C                   ENDIF
115300081014    0C                   ENDIF
115400911015     C*
115500911015     C                   ENDSR
115600950810     C**************************************************************************
115700950810     C* ELABORAZIONE DELL'IMA
115800950810     C**************************************************************************
115900950810     C     ELAIMA        BEGSR
116000950810     C*
116100950810     C* N O N   C O N T R O L L O :
116200950810     C*          1) SE NO ABILITAZIONE FORZATA E SENZA DATA ARRIVO
116300990212    1C     SAVAMA        IFNE      3
116400950810     C     ARTDAM        ANDEQ     0
116500950810     C                   SETON                                        34
116600990212    1C                   ENDIF
116700950810     C*
116800950810     C*          2) SE DATA ARRIVO MAGGIORE DATA FOGLIO IMA
116900990212    1C  N34ARTDAM        IFNE      0
117000950810     C     ARTDAM        ANDGT     SAVDFV
117100950810     C                   SETON                                        34
117200990212    1C                   ENDIF
117300950810     C*
117400990212     C*          3) SE COLLO SI TROVA IN DCD E RESO O DISTRUTTO PER
117500990212     C*             AVARIA O COLLO MANCANTE
117600990212     C     KARTD         CHAIN     FNDCD02L                           31
117700990212    1C     *IN31         IFEQ      *OFF
117800990212     C*
117900990303     C     DCDATB        ANDEQ     ' '
118000990212     C* CHAIN TESTATA PER CONTROLLARE LA DATA DI CHIUSURA
118100990212     C     KDCD          CHAIN     FNDCT01L                           32
118200050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
118300050221     c                   If        Not *In32 and dctdt2 > 0
118400050221     c                   Eval      *In32 = *On
118500050221     c                   EndIf
118600990303     C*
118700990303    2C     *IN32         IFEQ      *OFF
118800990303     C* SE E' CHIUSA CONTROLLO SE IL RECORD DEVE ESSERE COMPRESO
118900990303     C*  IN IMA, PER ALCUNI TIPI ANOMALIA
119000990303     C                   MOVEL     'S'           §CHIMA
119100990303    3C     DCTCCH        IFNE      *BLANKS
119200990303     C                   CLEAR                   DSBS02
119300990303     C                   MOVEL     'C'           T02MOD
119400990303     C                   MOVEL     KNSIF         T02SIF
119500990303     C                   MOVEL     'CCH'         T02COD
119600990303     C                   MOVEL     DCTCCH        T02KE1
119700990303     C                   CALL      'TIBS02R'
119800990303     C                   PARM                    KPJBA
119900990303     C                   PARM                    DSBS02
120000990303     C*
120100990303    4C     T02ERR        IFEQ      ' '
120200990303     C                   MOVEL     T02UNI        DCCH
120300990303    4C                   ENDIF
120400990303    3C                   ENDIF
120500990303     C**
120600990303     C** SE NON DEVE ESSERE COMPRESO:
120700990303    3C     §CHIMA        IFNE      'S'
120800990303     C     DCTDCH        OREQ      0
120900990212     C** SE ENTRAMBI APERTI O CHIUSI CON STESSA DATA CONTROLLO IL TIPO
121000990212     C**  ANOMALIA PER DECIDERE SE TENERE O ESCLUDERE IL COLLO
121100990212     C**  NON CONSIDERO LA CAUSALE PAREGGIO CHE SIGNIFICA CHE IL COLLO
121200990212     C*  C'E' SEMPRE
121300990212    4C     DCTDCH        IFEQ      DCDDCH
121400990212     C     DCDDCH        OREQ      0
121500990212     C**
121600990212     C                   CLEAR                   DSBS02
121700990212     C                   MOVEL     'C'           T02MOD
121800990212     C                   MOVEL     KNSIF         T02SIF
121900990212     C                   MOVEL     'TAD'         T02COD
122000990212     C                   MOVEL     DCTTAD        T02KE1
122100990212     C                   CALL      'TIBS02R'
122200990212     C                   PARM                    KPJBA
122300990212     C                   PARM                    DSBS02
122400990212     C**
122500990212    5C     T02ERR        IFEQ      ' '
122600990212     C                   MOVEL     T02UNI        DTAD
122700990212     C                   ELSE
122800990212     C                   MOVEL     'A'           §TARGR
122900990212    5C                   ENDIF
123000990212    C**
123100990212     C* SE MANCANZE
123200990212    5C     §TARGR        IFEQ      'M'
123300990212     C* O AVARIA RESA
123400990212     C     §TARGR        OREQ      'V'
123500990212     C     DCTDSD        ANDEQ     'R'
123600990212     C* O AVARIA DISTRUTTA
123700990212     C     §TARGR        OREQ      'V'
123800990212     C     DCTDSD        ANDEQ     'D'
123900990212     C*                    --> ESCLUDO
124000990212     C                   SETON                                        34
124100990212    5C                   ENDIF
124200990212    4C                   ENDIF
124300990212    3C                   ENDIF
124400990212    2C                   ENDIF
124500990212    1C                   ENDIF
124600101215     c
124700101215     c* Se opzione solo spuntato alla linea di arrivo finale, controllo
124800101215     c                   if        parimaa='S'
124900101215     c                   setoff                                       37
125000101215RM*  C     Kartd         setll     FNBRV07l
125100101215RM*  C     Kartd         reade     FNBRV07l
125200101215     c                   dow       not %eof(fnbrv07l)
125300101215     c     brvfgs        lookup    Lspu                                   37
125400101215     c                   if        *in37
125500101220     c* Se la filiale che spunta è giusta verifico che sia categoria di "arrivo"
125600101220     c                   clear                   dslv53
125700101220     C                   z-add     brvnFV        D53NFV
125800101220     C                   MOVEL     brvNPG        D53NPG
125900101220     C                   MOVEL     brvFGS        D53FGS
126000101220     C                   CALL      'FNLV53R'
126100101220     C                   PARM                    DSLV53
126200101220     C*
126300101220    2C     D53ERR        IFeq      ' '
126400101220     c*
126500101220     c                   if        d53def='S'
126600101220     c                   eval      wnpgarr=%editc(d53npg:'X')+'D'
126700101220     c                   else
126800101220     c                   eval      wnpgarr=%editc(d53npg:'X')+d53spg
126900101220     c                   endif
127000101220     c
127100101220     c                   eval      TROV=%lookup   (wnpgarr: NPGARR)
127200101220     c                   if        TROV>0
127300101215     c                   leave
127400101220     c                   endif
127500101215     c                   endif
127600101220     c                   endif
127700101220     c
127800101220     c                   setoff                                       37
127900101220RM*  C     Kartd         reade     FNBRV07l
128000101215     c
128100101215     c                   enddo
128200101215     c
128300101215     c* se non trovata nessuna spunta, NON elaboro
128400101215     c                   if        not *in37
128500101215     c                   seton                                        34
128600101215     c                   endif
128700101215     c
128800101215     c                   endif
128900990212    C**
129000170125     c** se collo mancante di bolla da portare a mag gaicenza e collo spuntato in
129100170125     c*  IMG nel giorno --> non stampo
129200170125     c
129300100706     c                   if        not *in34
129400170125     c                   clear                   winGiac
129500100706     C                   EXSR      CTRART
129600950810     C*
129700950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
129800170125     c*         se si tratta di merce da portare a IMG e collo spuntato solo in IMG
129900170125     c*         non stampo
130000170125     c                   if        wgiac='D' and *in14 and
130100170125     c                             wspg='G' and wdfv=savdfv
130200170125     c                   eval      winGiac='S'
130300170125     c                   endif
130400170213     c
130500170125
130600100706     C* 16 ON -     TROVATO SEGNACOLLO IN IMA più di xxx volte
130700170125     c                   if        *in14 and wingiac=' '  or *in16
130800170125     C                   EXSR      ERRORE
130900170125     c                   endif
131000170125
131100170125     c                   if        not *in14
131200170125     c                   eval      Wesspu='S'
131300170125     c                   endif
131400100706     c                   endif
131500950810     C*
131600950810     C                   ENDSR
131700950810     C**************************************************************************
131800950810     C* ELABORAZIONE DELL'IMG
131900950810     C**************************************************************************
132000950810     C     ELAIMG        BEGSR
132100950810     C                   EXSR      CTRART
132200950810     C*
132300170124     C* 14 ON - NON TROVATO SEGNACOLLO IN IMG
132400161025     c                   if        *in14
132500161026     c* In caso di bolla con giacenza e merce da portare a mag giac.
132600161026     c*  non stampo nulla per IMG
132700170124     c***                if        *in11 or wgiac<>'D'
132800170124     c                   if        wgiac<>'D'
132900161025     C                   EXSR      ERRORE
133000161026     c                   endif
133100161026     c
133200161025     c                   else
133300161025     c                   eval      Wesspu='S'
133400161025     c                   endif
133500161025
133600950810     C                   ENDSR
133700941017     C**************************************************************************
133800941017     C* STAMPO DISCORDANZA
133900941017     C**************************************************************************
134000900621     C     STAMPA        BEGSR
134100050405     c                   setoff                                       242526
134200941017     C*
134300911015     C* STAMPA TESTATA
134400911015     C   OF              DO
134500911018     C                   SETON                                        05
134600911018     C                   EXCEPT    TES
134700920813     C                   EXCEPT    TES2
134800911018     C                   SETOFF                                       OF
134900911018     C                   END
135000090522     c* Se non trovata la bolla (per colli doppi)
135100090522    1c                   if        NoBolla='S'
135200090522     c                   except    DetNObo
135300161018     C   12              EXCEPT    DETdoppio
135400161018     C   18              EXCEPT    DETeccede
135500090522   x1c                   else
135600911018     C*
135700950810     C     COMSPE        IFNE      ARBSPE
135800911018     C                   SETON                                        05
135900950810     C                   MOVEL     ARBSPE        COMSPE
136000950810     C                   ENDIF
136100911018     C* DATA SPEDIZIONE
136200911018     C   05              DO
136300941018     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
136400941018     C                   MOVE      ARBDSP        G02INV                         AAAA/MM/GG
136500911018     C                   MOVE      *ZEROS        G02DAT
136600911018     C                   MOVEL     3             G02ERR
136700941017     C                   CALL      'XSRDA8'
136800911018     C                   PARM                    WLBDAT
136900941018     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
137000941018     C*
137100941018     C* CODICE PADRONCINO CONSEGNA DECODIFICA
137200021203     C*    ARBPDC        CHAIN     FNAPD01L                           06
137300021205     C                   move      arbpdc        kpdpdr
137400021203     C                   move      'A'           kpdtip
137500021203     C     kfapd         chain     fiapd01L                           06
137600941018     C  N06              MOVEL     APDRSC        DESCPC            9
137700100707     c
137800100707     c* se non c'e' paadroncino consegna scrivo se Fermo deposito
137900100707     c                   if        *in06
138000100707     C                   MOVEL     *BLANKS       DESCPC
138100100707     c                   if        arbffd='S'
138200100707     c                   eval      descpc='FDeposito'
138300100707     c                   endif
138400100707     c                   endif
138500920409     C*
138600911018     C* SEGNACOLLI NON CONSECUTIVI
138700911018     C     ARBFNS        COMP      'S'                                    04
138800911018     C* SERIE>< 0
138900911018     C     ARBNRS        COMP      0                                  0303
139000911018     C                   END
139100920409     C*
139200920708     C* 07 ON - MANCA DATA ARRIVO: SPEDIZIONE FORZATA A 3
139300920708     C     ARTDAM        IFEQ      0
139400920708     C                   SETON                                        07
139500920708     C                   ELSE
139600920708     C                   SETOFF                                       07
139700920409     C* DATA ARRIVO MERCE  SEGNACOLLO
139800941018     C                   MOVE      ARTDAM        G02INV                         AAAA/MM/GG
139900920409     C                   MOVE      *ZEROS        G02DAT
140000920409     C                   MOVEL     3             G02ERR
140100941017     C                   CALL      'XSRDA8'
140200920409     C                   PARM                    WLBDAT
140300941018     C                   Z-ADD     G02DAT        STADAM                         GG/MM/AAAA
140400920708     C                   END
140500040603     C* MITTENTE E DESTINATARIO DI 17 E 20
140600040603     C                   MOVEL     ARBRSM        STARSM           17
140700920707     C                   MOVEL     ARBRSD        STARSD           20
140800040603     c* collo di valore
140900040603     c                   movel     §ar5bva       stabva            1
141000161025     c* se si tratta di bolla da portare a mag giac, riempi il campo STADSF con relativa scritta
141100161025     c                   if        not *in11 and wgiac='D'
141200161025     c                   clear                   stadsf
141300161025     c                   eval      stadsf='Portare a MagGIAC'
141400161025     c                   endif
141500161025     c
141600161026     c                   clear                   stahms            4 0
141700170222     c   13              if        brvhms>0
141800170222     c                   eval      stahms=brvhms/100
141900170222     c                   endif
142000910522     C*
142100900622     C* STAMPO DETTAGLIO
142200100706     C                   EXCEPT    DET
142300100707     c                   select
142400100707     c                   when      *in12
142500100706     C                   EXCEPT    DETdoppio
142600100707     c                   when      *in16
142700100707     C                   EXCEPT    DETspuIMA
142800161018     c                   when      *in18
142900161018     C                   EXCEPT    DETeccede
143000100707     c                   other
143100100706     C                   EXCEPT    DETerr
143200100707     c                   endsl
143300070418     c
143400070418     c* se in distinta , stampo
143500090522    2c                   if        not *in12
143600081014
143700100706    3c                   if        *in05 and arbndc>0 and not *in16
143800070418     C                   MOVE      ARbddc        G02INV                         AAAA/MM/GG
143900070418     C                   MOVE      *ZEROS        G02DAT
144000070418     C                   MOVEL     3             G02ERR
144100070418     C                   CALL      'XSRDA8'
144200070418     C                   PARM                    WLBDAT
144300070418     C                   Z-ADD     G02DAT        STAddc            8 0          GG/MM/AAAA
144400070418     C                   EXCEPT    DETndc
144500090522    3c                   endif
144600050405     c
144700161017     c* Se c'e' ubicazione collo, per 05 on lo stampo
144800050405     c                   if        *in05 and §ar5zng<>*blanks
144900050405     c                   seton                                        24
145000050405     c                   endif
145100050405     c
145200100707     C                   SETOFF                                       05
145300050405     c
145400990212     C* STAMPO TUTTE LE C.A. PRESENTI SULLA SPEDIZIONE
145500040806      * Sostituisco la lettura del file FNDTC02 con
145600040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
145700040806      * legate alla spedizione che passo
145800040806      *
145900040806     c                   clear                   fidn12ds
146000040806     c                   eval      i12aas = arbaas
146100040806     c                   eval      i12lnp = arblnp
146200040806     c                   eval      i12nrs = arbnrs
146300040806     c                   eval      i12nsp = arbnsp
146400040806     c                   eval      i12fel = 'E'
146500040806     c                   eval      i12fan = 'E'
146600040806      *
146700040806     c                   call      'FIDN12R'
146800040806     c                   parm                    fidn12ds
146900040806      *
147000040806      * se non ci sono errori
147100090522    3c                   if        o12err = ' '
147200040806      * se numero di CA trovate maggiore di zero
147300090522    4c                   if        o12nca > 0
147400100706     c
147500050405     c                   seton                                        25
147600040806     c
147700040806     c                   z-add     *zeros        ii                2 0
147800100706    6c     1             do        o12nca        ii
147900040806     c                   movea     skkey(ii)     dskey
148000040806     C                   Z-ADD     dsfil         dctfil
148100040806     C                   Z-ADD     dsnca         dctnca
148200040806     C                   Z-ADD     skdca(ii)     dctdca
148300040806     C                   Z-ADD     skdch(ii)     dctdch
148400040806     C                   movea     skcch(ii)     dctcch
148500040806     c                   exsr      preca
148600050405     c                   except    detca
148700050405     c
148800050405     c                   setoff                                       24
148900100706    6C                   ENDDO
149000040806      *
149100100706    4c                   endif
149200090522    3c                   endif
149300050405     c
149400050405     c* Stampo ubicazione collo se non ancora stampata
149500090522    3c                   if        *in24
149600050405     c                   except    detca
149700090522     c                   setoff                                       24
149800090522    3c                   endif
149900090522     c                   setoff                                       25
150000081014     c
150100090522    2c                   endif
150200090522    1c                   endif
150300900622     C*
150400900622     C                   ENDSR
150500990212     C**************************************************************************
150600990212     C* PREPARO I DATI DELLA C.A.
150700990212     C**************************************************************************
150800990212     C     PRECA         BEGSR
150900990212     C                   SETOFF                                       26
151000990303     C                   CLEAR                   STACCH
151100990212     C* DATA APERTURA C.A.
151200990212     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
151300990212     C                   Z-ADD     *ZERO         G02DAT
151400990212     C                   MOVEL     '3'           G02ERR
151500990212     C                   CALL      'XSRDA8'
151600990212     C                   PARM                    WLBDAT
151700990212     C                   MOVE      G02DAT        W0020             2 0
151800990212     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
151900990212     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
152000990303     C** DATA CHIUSURA C.A.
152100990212     C                   CLEAR                   STACHC
152200990303    1C     DCTDCH        IFGT      0
152300990212     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
152400990212     C                   Z-ADD     *ZERO         G02DAT
152500990212     C                   MOVEL     '3'           G02ERR
152600990212     C                   CALL      'XSRDA8'
152700990212     C                   PARM                    WLBDAT
152800990212     C                   MOVE      G02DAT        W0020             2 0
152900990212     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
153000990212     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
153100990212     C                   SETON                                        26
153200990303     C* CAUSALE CHIUSURA
153300990303    2C     DCTCCH        IFNE      *BLANKS
153400990303     C                   CLEAR                   DCCH
153500990303     C                   CLEAR                   DSBS02
153600990303     C                   MOVEL     'C'           T02MOD
153700990303     C                   MOVEL     KNSIF         T02SIF
153800990303     C                   MOVEL     'CCH'         T02COD
153900990303     C                   MOVEL     DCTCCH        T02KE1
154000990303     C                   CALL      'TIBS02R'
154100990303     C                   PARM                    KPJBA
154200990303     C                   PARM                    DSBS02
154300990303     C*
154400990303    3C     T02ERR        IFEQ      ' '
154500990303     C                   MOVEL     T02UNI        DCCH
154600990303    3C                   ENDIF
154700990303     C                   MOVEL     §CHDES        STACCH           12
154800990303    2C                   ENDIF
154900990303    1C                   ENDIF
155000990212     C                   ENDSR
155100941017     C**************************************************************************
155200161018     C* RIEPILOGO SPUNTE PER PISTOLA e controllo se colli doppi o eccedenti
155300941017     C**************************************************************************
155400920813     C     RIEPIL        BEGSR
155500081014     C                   SETON                                        12
155600941017     C*
155700920813     C                   Z-ADD     1             C
155800920813     C* CONTO SPUNTE PER PISTOLA
155900070118     C     KBRV          SETLL     FNBRV01L
156000070118     C     KBRV          READE     FNBRV01L                               30
156100920813     C*
156200161018    1C     *IN30         DOWEQ     *OFF
156300161018    2c                   if        brvtsu<>*blanks
156400081014     c                   clear                   fnlrv7ds
156500081014     c                   eval      ilrv7fls=brvlnp
156600081014     c                   eval      ilrv7lna=brvlna
156700081014     c                   eval      ilrv7nrs=brvnrs
156800081014     c                   eval      ilrv7nsc=brvnsc
156900081014     c                   eval      kpjbu= fnlrv7ds
157000081014     c                   call      'FNLRV7R'
157100081014     c                   parm                    kpjba
157200081014     c
157300081014     c                   eval      fnlrv7ds=kpjbu
157400161018    3c                   if        olrv7nric>=3 and olrv7part='S'
157500081014     c* Se ho già stampato qualche cosa, lascio la prima volta una
157600081014     c*  riga vuota
157700161018    4c                   if        *in01 and not *inOF and uno=' '
157800081014     c                   except    DETvuota
157900161018    4c                   endif
158000161018     c                   exsr      Cerca_bolla
158100081014     c
158200081014     c                   exsr      ERRORE
158300081014     c                   eval      UNO='S'
158400161018    3c                   endif
158500081014     c
158600161018    2c                   endif
158700161018     c
158800170124     c*      per  IMG  stampo i colli eccedenti ( IPG non esiste più)
158900170124     C*  ( SE partiti con procedura)
159000161201     c                   if        *in10 and *in22
159100161201     c                   exsr      Eccedenti
159200161201     c                   endif
159300081014     c
159400941019     C                   Z-ADD     1             B                 2 0
159500920813     C                   MOVEL     BRVNPS        COMNPS            2
159600920813     C     COMNPS        LOOKUP    NPS(B)                                 31
159700920813     C  N31              MOVEL     BRVNPS        NPS(C)
159800920813     C  N31              Z-ADD     C             B
159900941019     C  N31              ADD       1             C                 2 0
160000920813     C*
160100920813     C                   ADD       1             CPS(B)
160200920813     C*
160300070118     C     KBRV          READE     FNBRV01L                               30
160400161018    1C                   ENDDO
160500081014     C
160600081014     C                   SETOFF                                       12
160700161018     c
160800081014     C*
160900081014     C* SE NESSUNA ANOMALIA STAMPO TUTTO O.K.
161000081014     C  N01              EXCEPT    TES
161100081014     C  N01              EXCEPT    OK
161200081014     C  N01              SETOFF                                       OF
161300920813     C**
161400920813     C* SOMMO TUTTI I COLLI
161500920813     C                   XFOOT     CPS           TOTSPU            9 0
161600920813     C**
161700920813     C* STAMPA SPUNTE
161800920813     C* TESTATA
161900920813     C   OF              EXCEPT    TES
162000920813     C   OF              SETOFF                                       OF
162100920813     C*
162200920813     C                   EXCEPT    TOT1
162300920813     C*
162400920813     C                   Z-ADD     1             C
162500920813     C     CPS(C)        DOWGT     0
162600920813     C   OF              DO
162700920813     C                   EXCEPT    TES
162800920813     C                   EXCEPT    TOT1
162900920813     C                   SETOFF                                       OF
163000920813     C                   END
163100920813     C*
163200920813     C                   EXCEPT    TOT2
163300920813     C                   ADD       1             C
163400920813     C                   END
163500920813     C*
163600920813     C                   EXCEPT    TOT3
163700941017     C*
163800920813     C                   ENDSR
163900161018     C**************************************************************************
164000161018     C*   da spunta cerco la relativa bolla
164100161018     C**************************************************************************
164200161018     c     Cerca_bolla   BEGSR
164300161018     c                   clear                   nobolla
164400161018     c* chain la bolla e stampo collo doppio
164500161018     c     kartB         chain     fnart27l
164600161018    2c                   if        %found(fnart27l)
164700161018     c     karb          chain     fnarb01l
164800161018   x2c                   else
164900161018     c     kartB         chain     fnblt27l
165000161018    3c                   if        %found(fnart27l)
165100161018     c     karb          chain     fnblp01l
165200161018   x3c                   else
165300161018     c                   eval      Nobolla='S'
165400161018    3c                   endif
165500161018    2c                   endif
165600161018     c                   ENDSR
165700161018     C**************************************************************************
165800161018     C*   Ricerca colli eccedenti
165900161018     C**************************************************************************
166000161018     C     Eccedenti     BEGSR
166100161018     c                   setoff                                       12
166200161018     c                   seton                                        18
166300161018     c                   clear                   ecce_descr
166400161018     c
166500161018     c                   exsr      Cerca_bolla
166600161018     c* Se manca bolla --> sempre eccedente
166700161018    1c                   if        nobolla='S'
166800161018    4c                   if        *in01 and not *inOF and uno=' '
166900161018     c                   except    DETvuota
167000161018    4c                   endif
167100161018     c                   exsr      ERRORE
167200161018     c                   eval      UNO='S'
167300161018   x1c                   else
167400161018     c* Richiamo driver per verificare se la bolla è a mag giac oggi
167500161018     c                   clear                   fnlgi1ds
167600161018     c                   eval      ILGI1AAS=arbaas
167700161018     c                   eval      ILGI1lnp=arblnp
167800161018     c                   eval      ILGI1nrs=arbnrs
167900161018     c                   eval      ILGI1nsp=arbnsp
168000161018    3c                   select
168100161018     c                   when      arbpkc>0 and arbncp=arbncl
168200161018     c                   eval      ilgi1pkf=arbpkc
168300161018     c                   when      arbpkc>0 and arbncp<arbncl
168400161018     c                             and arbpkc>arbpkf
168500161018     c                   eval      ilgi1pkf=arbpkc
168600161018     c                   other
168700161018     c                   eval      ilgi1pkf=arbpkf
168800161018    3c                   endsl
168900161018     c                   movel     fnlgi1ds      kpjbu
169000161018     c                   call      'FNLGI1R'
169100161018     c                   parm                    kpjba
169200161018     c                   movel     kpjbu         fnlgi1ds
169300161201     c* Per IPG --> corretto se il flag da portare a mag giac = "Y"
169400161025     c* Per IMG --> corretto se il flag da portare a mag giac = "Y"  o già in giac
169500161026     c*         --> ignoro anche se deve uscire da mag gaicenza, ma non ancora effettuat
169600161025    2c                   if        (*in11 and olgi1AMG<>'Y'and
169700161025     c                              olgi1AMG<>'S') or
169800161025     c                             (not *in11 and olgi1AMG<>'Y' and
169900161025     c                             olgi1AMG<>'S'  and
170000161026     c                             olgi1inmg<>'S' and olgi1outmg<>'S')
170100161018     c*
170200161018     c                   select
170300161025     c                   when      olgi1inmg<>' '
170400161018     c                   eval      ecce_descr=' -già in Giacenza'
170500161026     c                   when      arbndc>0  and arbdcm=0
170600161018     c                   eval      ecce_descr=' -di bolla in distinta '
170700161025     c                   when      OLGI1OUTMG<>' '
170800161025     c                   eval      ecce_descr=' -non più giacente'
170900161018     c                   other
171000161025     c                   eval      ecce_descr=' -di colli IMA'
171100161018     c                   endsl
171200161018
171300161018    4c                   if        *in01 and not *inOF and uno=' '
171400161018     c                   except    DETvuota
171500161018    4c                   endif
171600161018
171700161018     c                   exsr      ERRORE
171800161018     c                   eval      UNO='S'
171900161018    2c                   endif
172000161025    1c                   endif
172100161018     c
172200161018     c                   setoff                                       18
172300161018     c                   seton                                        12
172400161018     C                   ENDSR
172500941017     C**************************************************************************
172600941017     C*   R O U T I N E     I N I Z I A L E
172700941017     C**************************************************************************
172800941017     C     *INZSR        BEGSR
172900941017     C*
173000161017     C* DEFINIZIONE CHIAVI
173100000126     C     KBRV          KLIST
173200000126     C                   KFLD                    IMANPG
173300000126     C                   KFLD                    WNFV
173400000126     C                   KFLD                    VIDFGS
173500070118     C     KBRV1         KLIST                                                  FNBRV01L
173600941019     C                   KFLD                    IMANPG                         ..CATEGORIA
173700000126     C                   KFLD                    WNFV                           ..NR. FOGLIO
173800950316RM*  C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
173900941019     C                   KFLD                    ARTFLS                         ..LNP
174000941018     C                   KFLD                    ARTLNA                         ..LNA
174100941018     C                   KFLD                    ARTNRS                         ..SERIE
174200941018     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
174300070418     C     KBRV4         KLIST                                                  FNBRV01L
174400070418     C                   KFLD                    CONNPG                         ..CATEGORIA
174500070418     C                   KFLD                    ARTFLS                         ..LNP
174600070418     C                   KFLD                    ARTLNA                         ..LNA
174700070418     C                   KFLD                    ARTNRS                         ..SERIE
174800070418     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
174900070418     C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
175000990212RM*  C     KARTD         KLIST
175100990212RM*  C                   KFLD                    ARTFLS
175200990212RM*  C                   KFLD                    ARTLNA
175300990212RM*  C                   KFLD                    ARTNRS
175400990212RM*  C                   KFLD                    ARTNSC
175500081014     C     KARTb         KLIST
175600081014     C                   KFLD                    brvlnp
175700081014     C                   KFLD                    brvlnA
175800081014     C                   KFLD                    brvNRS
175900081014     C                   KFLD                    brvNSC
176000941017     C     KFVV          KLIST                                                  *
176100941019     C                   KFLD                    IMANPG                         *
176200941017     C                   KFLD                    COMNFV                         *
176300941028     C                   KFLD                    VIDFGS                         *
176400950316RM*  C     KFVV2         KLIST
176500950316RM*  C                   KFLD                    BRVNPG
176600000126RM*  C                   KFLD                    WNFV5
176700950316RM*  C                   KFLD                    BRVFGS
176800941017     C     KANM          KLIST                                                  FNANM02L
176900941017     C                   KFLD                    ARTFLS                         *
177000941017     C                   KFLD                    ARTLNA                         *
177100941017     C                   KFLD                    ARTNRS                         *
177200941018     C                   KFLD                    ARTNSC                         *
177300941017     C                   KFLD                    ANMCAA                         *
177400941017     C     KART          KLIST                                                  *
177500941017     C                   KFLD                    ARBAAS                         *
177600941017     C                   KFLD                    ARBLNP                         *
177700941017     C                   KFLD                    ARBNRS                         *
177800941017     C                   KFLD                    ARBNSP                         *
177900081014     C     KARB          KLIST                                                  *
178000081014     C                   KFLD                    ARtAAS                         *
178100081014     C                   KFLD                    ARtLNP                         *
178200081014     C                   KFLD                    ARtNRS                         *
178300081014     C                   KFLD                    ARtNSP                         *
178400040603     C     Kar5          KLIST                                                  *
178500040603     C                   KFLD                    ARBAAS                         *
178600040603     C                   KFLD                    ARBLNP                         *
178700040603     C                   KFLD                    ARBNRS                         *
178800040603     C                   KFLD                    ARBNSP                         *
178900040603     C                   KFLD                    ktrd                           *
179000970721     C     KGCA          KLIST                                                  *
179100970721     C                   KFLD                    ARBAAS                         *
179200970721     C                   KFLD                    ARBLNP                         *
179300970721     C                   KFLD                    ARBNRS                         *
179400970721     C                   KFLD                    ARBNSP                         *
179500990212     C                   KFLD                    KFRG
179600950810     C     KSAV          KLIST                                                  *
179700950810     C                   KFLD                    SAVAAS                         *
179800950810     C                   KFLD                    SAVLNP                         *
179900950810     C                   KFLD                    SAVNRS                         *
180000950810     C                   KFLD                    SAVNSP                         *
180100941017     C     KLBL          KLIST                                                  *
180200941017     C                   KFLD                    LBLAAN                         *
180300941017     C                   KFLD                    LBLLPN                         *
180400941017     C                   KFLD                    LBLNRN                         *
180500941017     C                   KFLD                    LBLNSN                         *
180600941017     C     KTAB          KLIST
180700941017     C                   KFLD                    CODUT
180800941017     C                   KFLD                    COD
180900941017     C                   KFLD                    KEY
181000050510     C     KTAB2         KLIST
181100050510     C                   KFLD                    CODUT
181200050510     C                   KFLD                    COD
181300990212     C     KDCD          KLIST
181400990212     C                   KFLD                    DCDAAC                         *
181500990212     C                   KFLD                    DCDFIL                         *
181600990212     C                   KFLD                    DCDNCA                         *
181700021203
181800021203     C     Kfapd         klist
181900021203     C                   kfld                    kpdtip
182000021203     C                   kfld                    kpdpdr
182100941017     C*
182200970721     C     *LIKE         DEFINE    GCPFRG        KFRG
182300970721     C                   MOVEL     0             KFRG
182400941017     C     *LIKE         DEFINE    TBLCOD        COD
182500941017     C     *LIKE         DEFINE    TBLKEY        KEY
182600941019     C     *LIKE         DEFINE    BRVNPG        IMANPG                         CATEGORIA FOGLIO
182700070418     C     *LIKE         DEFINE    BRVNPG        conNPG                         CATEGORIA FOGLIO
182800031218     C     *LIKE         DEFINE    BRVNFV        WNFV
182900031218     C     *LIKE         DEFINE    FVVNFV        WNFV5
183000941018     C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO VIAGGIO
183100941018     C     *LIKE         DEFINE    ARTDAM        STADAM                         DATA ARRIVO MERCE
183200941018     C     *LIKE         DEFINE    ARBSPE        COMSPE
183300941019     C     *LIKE         DEFINE    ARBLNA        KLNA                           LINEA ARRIVO
183400031218     C     *LIKE         DEFINE    FVVFGS        SAVFGS
183500950317RM*  C     *LIKE         DEFINE    FVVDFV        SAVDFV
183600941017     C*
183700101220      /free
183800101220       // Carico le categorie con ambito Arrivo
183900101220       xx=2   ;
184000101220       // Alla posizine 1 metto al defluenza
184100101220       npgarr(1)='1D'  ;
184200101220
184300101220       setll   (1:'7N') tabel00f   ;
184400101220       reade   (1:'7N') tabel00f   ;
184500101220
184600101220       dow not %eof(tabel00f)      ;
184700101220       if tblflg=' '               ;
184800101220       ds7n=tbluni                 ;
184900101220
185000101220       if §7ntbo<>' '              ;
185100101220
185200101220          if §7ntbo='A'               ;
185300101220          npgArr(xx)=tblkey;
185400101220          xx=xx+1  ;
185500101220          endif  ;
185600101220       else   ;
185700101220
185800101220         if  §7ntb1='A'                         ;
185900101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc1   ;
186000101220         xx=xx+1  ;
186100101220         endif                                  ;
186200101220         if  §7ntb2='A'                         ;
186300101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc2   ;
186400101220         xx=xx+1  ;
186500101220         endif                                  ;
186600101220         if  §7ntb3='A'                         ;
186700101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc3   ;
186800101220         xx=xx+1  ;
186900101220         endif                                  ;
187000101220         if  §7ntb4='A'                         ;
187100101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc4   ;
187200101220         xx=xx+1  ;
187300101220         endif                                  ;
187400101220         if  §7ntb5='A'                         ;
187500101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc5   ;
187600101220         xx=xx+1  ;
187700101220         endif                                  ;
187800101220         if  §7ntb6='A'                         ;
187900101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc6   ;
188000101220         xx=xx+1  ;
188100101220         endif                                  ;
188200101220
188300101220       endif                       ;
188400101220       endif                       ;
188500101220
188600101220       reade   (1:'7N') tabel00f   ;
188700101220       enddo                       ;
188800101220      /end-free
188900101220
189000941017     C                   ENDSR
189100101202
189200101202      *---------------------------------------------------------------*
189300101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
189400101202      *---------------------------------------------------------------*
189500101202     c     sr_DatiJob    BEGSR
189600101202      *
189700101202     c     *dtaara       define    §azute        AZUTEds
189800101202     c     *dtaara       define    §datiute      dDatiUte
189900101202      *
190000101202     c                   in(E)     *dtaara
190100101202     c                   IF        %Error or RSUT = *blanks
190200101202     c                   clear                   TIBS34ds
190300101202     c                   call      'TIBS34R'
190400101202     c                   parm                    TIBS34ds
190500101202     c                   in        *dtaara
190600101202     c                   ENDIF
190700101202      *
190800101202     c                   ENDSR
190900941017     C**************************************************************************
191000161028     Otigcp000  E            AGG5
191100161017     O                       GCPDLMa
191200950316RM*  OPRTF198   E            TES              02
191300911015     O                       STA(1)              66
191400950810     O              N10      STA(2)             132
191500161017     O               10N11   STA(18)            132
191600170124     O*********      10 11   STA(22)            132
191700950320     O                       STA(17)            198
191800900626     O                       RSUT                20
191900960927     O                       UDATE8             181 '  /  /    '
192000950320     O                       PAGE          Z    198
192100920813     O          E            TES         1
192200160503     o                       statipo             67
192300160503     o                       statipo2           130
192400050511     O                       UTIME              179 '  :  :  '
192500931228     O               02      STA(4)              70
192600931228     O          E    02      TES         0  0
192700931228     O                       STA(4)              70
192800931228     O          E            TES         2
192900911015     O                       STA(3)              66
193000931228     O                       STA(16)            132
193100060508     O                       STA(20)            198
193200161018     O              n11                          13 'I.M. .'
193300161018     O              n11      VIDTIN              12
193400170124     O*********      11                          13 'I.P.G.'
193500060508RM*  O                       SAVFGS              21
193600060508     O                       DESCDU              42
193700920813     O                       COMNFV        Z     56
193800950317     O                       STADFV        8     73
193900960927     O                       VIDFIL             116
194000960927     O                       DESFIL             137
194100060508     O                       stalna             177
194200060508     O                       DESlna             198
194300070418     O          E   n10      TES         1
194400070418     O                       STA(21)            198
194500070418     O                       stamdc             188
194600101215     O                       staimaa            146
194700920813     O          E            TES2        2
194800911018     O                       STA(5)              66
194900911018     O                       STA(8)             132
195000040621     O                       STA(19)            198
195100920813     O          E            TES2        1  1
195200911018     O                       STA(6)              66
195300911018     O                       STA(9)             132
195400911015     O*
195500090522     O          E            DETnoBO     1
195600090522     O                       brvLNP        Z      8
195700090522     O                       brvNRS        Z     11
195800090522     O                       brvLNA        Z     29
195900090522     O                       brvZNC              87
196000090522     O                       brvnps             122
196100090522     O                       brvNSC        Z    131
196200090522     O
196300090522     O          E            DET         1
196400090522     O                  05   ARBAAS        Z
196500090522     O                  05   ARBLNP        Z   +  1
196600090522     O                  05 03ARBNRS        Z   +  1
196700090522     O                  05 03                  +  0 '-'
196800090522     O                  05   ARBNSP        Z   +  0
196900090522     O                  05   STADSP            +  1 '  /  '
197000090522     O                  05   ARBLNA        Z   +  1
197100090522     O                  05   STARSM            +  1
197200090522     O                  05   STARSD            +  1
197300090522     O                  05N04ARBNCD        Z   +  1
197400090522     O                  05N04                  +  0 '/'
197500090522     O                  05N04ARBNCA        Z   +  0
197600090522     O                  05 04                    84 'NON SEQUENZIALI'
197700090522     O                  05   ARBZNC            +  1
197800090522     O                  05   stabva            +  0
197900090522     O                  05   ARBTMC            +  1
198000090522     O                  05N06ARBPDC        Z   +  1
198100090522     O                  05N06                  +  0 '-'
198200100707     O                  05   DESCPC            +  0
198300090522     O              N07      STADAM            +  1 '  /  /    '
198400090522     O              N07      ARTNAP            +  1
198500090522     O              N07      ARTNSC        Z   +  2
198600090522     O               07                         131 'ABILIT.MERCE FORZATA'
198700081014     O* ERRORI:
198800950411     O*
198900081014     O          E            DETERR      0  0
199000031219     O                    n13                   155 'NON SPUNTATO IN DATA '
199100031219     O                    n13STADFV        8   +  1
199200950316     O*
199300040621     O                     13brvFGS             137
199400040621     O                     13                   138 '-'
199500040621     O                     13ST2FGS             146
199600161025     O                     13                   149 'in'
199700070118     O                     13ST2CAT             153
199800070118     O                     13brvNPG             155
199900070118     O                     13                   156 '-'
200000070118     O                     13brvNFV        Z    162
200100161025     O                     13                   165 'il'
200200070118     O                     13ST2DFV             171 '  /  '
200300161026     O**                   13                   175 'ore'
200400161026     O                     13staHMS             178 '  :  '
200500161130     O                       STADSF             198
200600070418     O          E            DETndc      1
200700070418     O                                           87 'In distinta:'
200800070418     O                       ARBndc        2  +   1
200900070418     O                                        +   2 'del'
201000070418     O                       staddc            +  1 '  /  /    '
201100081014     o* Collo doppio
201200081014     O          E            DETdoppio   0  0
201300161018     O                                          153 'COLLO   D O P P I O'
201400161018     o* Collo eccedente
201500161018     O          E            DETeccede   0  0
201600161018     O                                          153 'COLLO   E C C E D E'
201700161018     O                                          159 ' N T E'
201800161018     O                       ecce_descr         185
201900100706     o* Collo presente da XX giorni in IMA
202000100706     O          E            DETspuIMA   0  0
202100100707     O                                          160 '   COLLO SPUNTATO PER '
202200100707     O                                        +   1 'ALMENO '
202300100706     O                       §VPOGGIMA     2  +   1
202400100707     O                                        +   1 'GIORNI in I.M.A.'
202500081014     o* riga vuota
202600081014     O          E            DETvuota    2
202700081014     O                                          157 ' '
202800050405     O* STAMPO NUMERO C.A.e ubicazione collo
202900990212     O          E            DETCA       1
203000050405     O               24                          87 'Ubicazione collo:'
203100050405     O               24      §ar5zng           +  2
203200050405     O               25                         138 'C.A.'
203300050405     O               25      DCTFIL            +  1
203400050405     O               25      DCTNCA        Z   +  1
203500050405     O               25                        +  1 'del'
203600050405     O               25      STADCA            +  1 '  /  /  '
203700990212     O                     26                  +  1 'chiusa il'
203800990212     O                     26STACHC            +  1 '  /  /  '
203900990303     O                     26DCTCCH            +  1
204000990303     O                     26                  +  0 '-'
204100990303     O                     26STACCH            +  0
204200920813     O          E            OK          3
204300920813     O                       STA(11)             66
204400920813     O          E            OK          0  0
204500920813     O                       STA(11)             66
204600920813     O*
204700920813     O          E            TOT1        3  1
204800920813     O                       STA(13)             66
204900920813     O          E            TOT2           1
205000920813     O                       NPS(C)              33
205100920813     O                       CPS(C)        2     58
205200920813     O          E            TOT3           1
205300920813     O                       STA(14)             66
205400920813     O          E            TOT3           1
205500920813     O                       STA(15)             66
205600920813     O                       TOTSPU        2     58
205700920813     O*
205800920813     O          E            TOT3        3
205900920813     O                       STA(12)             66
206000920813     O          E            TOT3        0  0
206100920813     O                       STA(12)             66
206200941017     O**************************************************************************
206300950316** STA
206400060508XXXXXXXXXXXXXXXXXXXX                                                 1
206500950320 ** I.M.A. : INVENTARIO MAGAZZINO ARRIVI **                          2
206600161018FOGLIO        di  XXX-XXXXXXXXXXXXXXXXXXXX    N.ro XXXXX   del XX/   3
206700931228                                       *** D E F I N I T I V O ***   4
206800040603    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO        5
206900941019ANNO LNP SER NUMERO  DATA ARR                                        6
207000040603XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx  7
207100040603     SEGNACOLLI    ZON ULT  AUTOTRASPORT.   A R R I V O SEGNACOLLO   8
207200040603      DAL /   AL   ARR CONS   CONSEGNA       DATA   PIST MANCANTE    9
207300040603XX XXXXXXX/XXXXXXX XXV X XXXXXXX-XXXXXXXXX XX/XX/XXXX XX  XXXXXXX   10
207400920813             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
207500920813 **  FINE  STAMPA  **
207600920813   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI      13
207700920813                        ---------------------------------------     14
207800920813               TOTALE                                               15
207900060508XX/XXXX           Punto Operativo in Gestione: XXX-xxxxxxxxxxxxxxx  16
208000950320                         FNLR31R                          PAG.XXXX  17
208100950810 ** I.M.G. : INVENTARIO MAGAZZINO GIACENZE **                       18
208200040621 S  P  U  N  T  A  T  O      D  A                                   19
208300060508xxxxx           Linea Arrivo Selezionata: XXX-xxxxxxxxxxxxxxxxxxxx  20
208400070418                Elabora anche spedizioni in distinta: XX            21
208500161031 ** I.P.G. : QUADRATURA COLLI A MAGAZZINO GIACENZE **               22
208600161017**
208700950316OVRPRTF FILE(PRTF198) USRDTA('I.M.A.')
208800161017**
208900161017DLYJOB DLY(01)
