000100040603     H DECEDIT('0,') DATEDIT(*ymD.)
000200950711     H* FNLR31R *-----------------------------------------------------*
000300161031     H*        - INVENTARIO MAGAZZINO  I.M.A. - IMG - IPG    Esegue
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
011300161017     D DS7B          E DS
011400161017     D DS7C          E DS
011500101220     D DS7N          E DS
011600920318     D DS3A          E DS
011700050511     D DS03          E DS
011800040603     D DAR5GEN       E DS
011900100706     D DVPO          E DS
012000990212     D DTAD            DS
012100990212     D  §TARGR                26     26
012200990303     D DCCH            DS
012300990303     D  §CHDES                 1     50
012400990303     D  §CHIMA                53     53
012500070118     D RECBRV        E DS                  EXTNAME(FNBRV00F)
012600070118     D WSAVB         E DS                  EXTNAME(FNBRV00F)
012700031219     d                                     prefix(S_)
012800990212     D DSBS02        E DS                  EXTNAME(TIBS02DS)
012900160927     D tigcp00ds     E DS                  EXTNAME(tigcp00F)
013000040427      * DS Esterna x gestione allocazione ed invio messaggi
013100040427     D TRUL82DS      E DS
013200081014      * DS Esterna x controllo colli doppi
013300081014     D FNLRV7DS      E DS
013400160927      * DS Esterna x controllo colli in IMG
013500160927     D FNLGI1DS      E DS
013600040806      * ds per il controllo della presenza di CA per la spedizione richiesta
013700040806     d FIDN12DS      e ds
013800040806     d  skKey                 26    305    dim(20)
013900040806     d  skAnn                306    325    dim(20)
014000040806     d  skDca                326    485  0 dim(20)
014100101202     d**skFca                486    545  0 dim(20)
014200040806     d  skDch                546    705  0 dim(20)
014300040806     d  skCch                706    745    dim(20)
014400101202      *
014500040806     d dsKey           ds
014600040806     d  dsaac                         4  0
014700040806     d  dsfil                         3  0
014800040806     d  dsnca                         7  0
014900040806      *
015000101202      *
015100021203      *
015200101220     d WnpgArr         s              2
015300101220     d Trov            s              2  0
015400090522     d Nobolla         S              1
015500090522     d uno             S              1
015600081014     d statipo         S             21
015700160503     d statipo2        S             21
015800101215     d staimaa         S             44
015900070418     d stamdc          S              2
016000160503     d indMP           S              4  0
016100160503     d ind12           S              4  0
016200050511     d WSPG            S                   LIKE(FVVSPG)
016300040108     d kpdpdr          S                   LIKE(APDpdr)
016400031219     d wdfv            S                   LIKE(d53dfv)
016500031219     d kpdtip          S                   LIKE(APDtip)
016600040603     d ktrd            S                   LIKE(ar5trd) inz('GEN')
016700161018     d ecce_descr      S             25
016800100706     d dataEUR         s               d   datfmt(*eur)
016900100706     d DataISO         s               d   datfmt(*iso)
017000101202     d*
017100040428     D msgjob          C                   CONST('La Manut.Bolle sta bloccando -
017200040428     D                                     l''elabor. Dell''IMA:USCIRE SUBITO D-
017300040428     D                                     A QUESTO LAVORO')
017400050511     D CPARCEL         C                   CONST('     P A R C E L     ')
017500050511     D CMessag         C                   CONST('M E S S A G G E R I E')
017600160503     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
017700160503     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
017800101215     D CsoloLNA        C                   CONST('Solo colli spuntati alla line-
017900101215     d                                     a arrivo finale')
018000911018     C*--------------------------------------------------------------- *
018100911018     C*                  INDICATORI                                    *
018200911018     C*--------------------------------------------------------------- *
018300911018     C* 01    - ESITONO ANOMALIE DI MANCANZA
018400950810     C* 02    - CHIESTA CHIUSURA  FOGLIO
018500911018     C* 03    - ESISTE SERIE
018600911018     C* 04    - SEGNACOLLI NON SEQUENZIALI
018700911018     C* 05    - ON CAMBIATA SPEDIZIONE
018800920325     C* 06    - NON C'E' COD PADRONCINO CONSEGNA
018900920708     C* 07    - MANCA DATA ARRIVO : SPEDIZIONE FORZATA
019000920813     C* 08    - BOLLA GESTITA
019100161017     C* 11    - e 10 ON --> ZPG
019200950810     C* 10    - ON --> IMG  OFF --> IMA
019300081014     C* 12    - CONTROLLO COLLI DOPPI
019400950810     C* 13    - NON TROVATA SPUNTA IN IMA MA  IN ALTRO FOGLIO NELLA
019500950810     C*         STESSA DATA
019600950810     C* 14    - NON TROVATA SPUNTA IN IMA
019700031219     C* 15    - TROVATA SPUNTA SU ALTRO FOGLIO MA DEVO CREARE L'ANOMAL
019800100706     C* 16    - collo in IMA ma spuntato in IMA più di XX volte...da tabella VPO
019900161018     C* 18    - collo eccedente
020000161130     C* 20    - FILIALE partita con procedura 2 gg in IMA
020100161130     C* 22    - FILIALE partita con quadratura IPG
020200050405     C* 24    - STAMPA ubicazione collo se c'e'
020300050405     C* 25    - STAMPA se c'e' c.a.
020400050405     C* 26    - STAMPARE DATA CHIUSURA C.A.
020500941017     C* 30    - LETTURA FNARB
020600941017     C* 31    - LETTURA FNART
020700911018     C* 32/34 - COMODO
020800911018     C*---------------------------------------------------------------*
020900900622     C     *ENTRY        PLIST
021000900622     C                   PARM                    KPJBA
021100941017     C                   MOVEL     KPJBU         PARAM1
021200000126     C                   Z-ADD     COMNFV        WNFV
021300900828     C*-----------------------------------------------------
021400950810     C* IMPOSTAZIONI INIZIALI
021500950810     C                   EXSR      IMPO
021600950810     C*-----------------------------------------------------
021700161018     c* il dly mi serve solo per il debug in batch
021800161017     C                   Z-ADD     80            LUNG             15 5
021900161017     C                   MOVEL     *BLANKS       COMMAN           80
022000161017     C                   MOVEA     VIDTIN        CMD(35)
022100161017     C                   MOVEA     CMDd          COMMAN
022200161017     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
022300161017     C                   PARM                    COMMAN
022400161017     C                   PARM                    LUNG
022500161017
022600941019     C                   Z-ADD     80            LUNG             15 5
022700941019     C                   MOVEL     *BLANKS       COMMAN           80
022800950810     C                   MOVEA     VIDTIN        CMD(35)
022900941019     C                   MOVEA     CMD           COMMAN
023000161018     c                   if        *in11
023100161031     c                   eval      %subst(comman:31:6)='I.P.G.'
023200161018     c                   endif
023300941019     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
023400941019     C                   PARM                    COMMAN
023500941019     C                   PARM                    LUNG
023600950810     C*
023700950316RM*  C                   OPEN      PRTF198
023800980128     C                   SETON                                        OF
023900911015     C*
024000040108     C* CONSIDERO SOLO i p.o. di £6
024100941019     C                   Z-ADD     1             WA                3 0          CONTATORE
024200040108    1C     L6(WA)        DOWNE     *ZERO
024300941019     C*
024400040108     C                   Z-ADD     L6(WA)        KLNA                           LINEA ARRIVO
024500161017     C*
024600031218     C     KLNA          SETLL     FNARB05L
024700040427     C     KLNA          READE     FNARB05L                               30
024800950810     C*
024900950810    3C     *IN30         DOWEQ     *OFF
025000050511     c* Se ima messaggerie o parcel, escludo le zone non di interesse
025100160503     c                   eval      indMP=0
025200160503     c                   eval      ind12=0
025300160503     c
025400160503     c                   if        wselzona='S'
025500160503     c                   movel     arbznc        alfzona           2
025600160503
025700160503     c                   if        wparc='S' or wmess='S'
025800160503     c***  alfzona       lookup    zone                                   35
025900160503     c                   eval      indMP =%lookup(alfzona:zone)
026000050511     c                   endif
026100160503     c                   if        wparc1='S' or wparc2='S'
026200160503     c                   eval      ind12 =%lookup(alfzona:zone12)
026300160503     c                   endif
026400160503
026500160503     c                   endif
026600160503   3ac                   if        wselzona=' ' or  indmp>0 or  ind12>0
026700920813     C*
026800920318     C* CHAIN CODICE BOLLE PER ESCLUDERE I RECUPERI
026900920318     C                   MOVEL     '3A'          COD
027000950810     C                   MOVEL(P)  ARBCBO        KEY
027100941018     C     KTAB          CHAIN     TABEL00F                           35
027200920318     C  N35              MOVEL     TBLUNI        DS3A
027300920318     C   35              MOVEL     'F1'          §3ATB1
027400920318     C*
027500950810     C* ESCLUDO:  LE BLOCCATE CON "B"/
027600930518     C*           QUELLE IN CONSEGNA / QUELLE CONSEGNATE / I RECUPERI
027700161025    4C     ARBdcm        ifeq      *zeros
027800950810     C     §3ARBL        ANDNE     'R'
027900950810     C     ARBFBC        ANDNE     'B'
028000000512     C*
028100960516     C* CONTROLLO SE E' GIACENTE
028200960516     C                   CLEAR                   WGIAC
028300960516     C     ARBFBC        IFEQ      'G'
028400161025     C     KGCA          CHAIN(n)  Tigcp21L                           35
028500970721     C* SE C'E' LA GIACENZA E NON E' A FASE 35 LA ESCLUDO
028600970721     C  N35GCPFAS        IFLT      35
028700050331     C     GCPFAS        oreq      36
028800160927     c* la merce è in giacenza solo dopo 2/3 giorni: verifico con driver
028900160927     c                   clear                   fnlgi1ds
029000160927     c                   eval      ILGI1AAS=arbaas
029100160927     c                   eval      ILGI1lnp=arblnp
029200160927     c                   eval      ILGI1nrs=arbnrs
029300160927     c                   eval      ILGI1nsp=arbnsp
029400161018    3c                   select
029500161018     c                   when      arbpkc>0 and arbncp=arbncl
029600161018     c                   eval      ilgi1pkf=arbpkc
029700161018     c                   when      arbpkc>0 and arbncp<arbncl
029800161018     c                             and arbpkc>arbpkf
029900161018     c                   eval      ilgi1pkf=arbpkc
030000161018     c                   other
030100161018     c                   eval      ilgi1pkf=arbpkf
030200161018    3c                   endsl
030300160927     c                   movel     fnlgi1ds      kpjbu
030400160927     c                   call      'FNLGI1R'
030500160927     c                   parm                    kpjba
030600160927     c                   parm                    tigcp00ds
030700161007     c                   movel     kpjbu         fnlgi1ds
030800160927
030900161024   5ac                   if        olgi1AMG='Y' or olgi1inmg='S'
031000970721     C                   MOVEL     'S'           WGIAC             1
031100160823     c                   endif
031200161024     c* se la bolla è da portare a mag giacenza OGGI, devo considerarla anche in IMA
031300161024     c*  e se manca dare segnalazione ma NON creando l'anomalia
031400161025   5ac  n11              if        olgi1AMG='S'
031500161024     C                   MOVEL     'D'           WGIAC             1
031600161024     c                   endif
031700161201     c*
031800161201     c* se non è partito con IPG considero  la spedizione GIACENTE  come prima
031900161201   5ac  n11              if        not *in22 and olgi1AMG='S'
032000161201     C                   MOVEL     'S'           WGIAC             1
032100161201     c                   endif
032200161201     c
032300161031     c* IPG elaboro le giacenze da portare a mag gac OGGI
032400161025   5ac   11              if        olgi1AMG='S' or olgi1AMG='Y'
032500161025     C                   MOVEL     'D'           WGIAC             1
032600161025     c                   endif
032700160823     c
032800960516     C                   ENDIF
032900970721     C                   ENDIF
033000950810     C*
033100161025     C* SE IMA --> ESCLUDO LE GIACENTI ma includo le bolle che devono essere portate a mag giac
033200161130     c*            se partita la procedura 2 gg in IMA
033300950810    5C     *IN10         IFEQ      *OFF
033400960516     C     WGIAC         ANDNE     'S'
033500161130     c     *in20         andeq     *on
033600161130
033700161130    5C     *IN10         orEQ      *OFF
033800161130     C     WGIAC         ANDeq     ' '
033900161130     c     *in20         andeq     *off
034000950810     C*
034100950810     C* SE IMG --> INCLUDO SOLO LE GIACENTI
034200161026     C*        --> INCLUDO anche le bolle da portare a mag.giac. ma senza stampare il mancante
034300950810     C     *IN10         OREQ      *ON
034400161025     C     *IN11         andeq     *Off
034500161026     C     WGIAC         ANDne     ' '
034600161025
034700161031     C* SE IPG --> INCLUDO SOLO quelle da portare a mag giac
034800161025     C     *IN10         OREQ      *ON
034900161025     C     *IN11         andeq     *ON
035000161025     C     WGIAC         ANDEQ     'D'
035100921013     C*
035200950317     C* ESCLUDO  SE BOLLA MAMMA E UNA DELLE FIGLIE E' IN CONSEGNA
035300960516     C*     O SE SONO TUTTE BLOCCATE
035400950810     C                   Z-ADD     ARBSPE        SAVSPE
035500960516     C*
035600960516     C                   CLEAR                   WESFI                          ESISTE FIGL
035700960516     C                   MOVEL     'S'           WFIBLO            1            FIGLIE BLOCC
035800950317     C                   SETOFF                                       36
035900950810     C     KSAV          SETLL     FNLBL02L
036000950810     C     KSAV          READE     FNLBL02L                               35
036100960516     C**
036200950810    6C     *IN35         DOWEQ     *OFF
036300950317     C*
036400950317     C     KLBL          CHAIN     FNARB01L                           35
036500950810    7C     *IN35         IFEQ      *OFF
036600960516     C                   MOVEL     'S'           WESFI             1
036700960516     C* FIGLIA IN CONSEGNA
036800960516    8C     ARBNDC        IFNE      0
036900950317     C                   SETON                                        3536
037000960516   X8C                   ELSE
037100960516     C*
037200960516     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
037300960516    9C     ARBFBC        IFNE      'B'
037400960516     C                   MOVEL     'N'           WFIBLO
037500960516    9C                   ENDIF
037600960516    8C                   ENDIF
037700960516    7C                   ENDIF
037800950317     C*
037900950810     C  N36KSAV          READE     FNLBL02L                               35
038000950810    6C                   ENDDO
038100960516     C*
038200960516     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
038300960516     C  N36WESFI         IFEQ      'S'
038400960516     C     WFIBLO        ANDEQ     'S'
038500960516     C                   SETON                                        36
038600960516     C                   ENDIF
038700070418     c
038800070418     c* Per 36 off - controllo in IMA se richiesti anche i colli mancanti
038900070418     c*              di spedizioni in distinta
039000070418     c  n36              if        not *in10 and vidmdc='N' and arbndc>0
039100070418     c                   seton                                        36
039200070418     c                   endif
039300911204     C*
039400950810    6C  N36              DO
039500950317     C*
039600040427     C     KSAV          CHAIN     FNARB01L                           31
039700950810     C                   MOVEL     ARBAMA        SAVAMA            1 0
039800040428     c                   clear                   wchain            1
039900161025     c                   clear                   wesspu            1
040000911018     C* LEGGO DETTAGLIO
040100941017     C     KART          SETLL     FNART01L
040200040428     C  N10
040300040428     CAN 02KART          READE     FNART01L                               31
040400040428     C   10
040500040428     CORN02KART          READE(N)  FNART01L                               31
040600911018     C*
040700950810    7C     *IN31         DOWEQ     *OFF
040800911204     C                   SETOFF                                       34
040900950810     C*
041000950810     C* ESCLUDO  SE IL COLLO E' CONSEGNATO (CONSEGNA PARZIALE)
041100070418     C     ARTDCM        IFEQ      0
041200950810     C  N10              EXSR      ELAIMA
041300950810     C   10              EXSR      ELAIMG
041400950810    8C                   ENDIF
041500930629     C*
041600040428     C  N10
041700040428     CAN 02KART          READE     FNART01L                               31
041800040428     C   10
041900040428     CORN02KART          READE(N)  FNART01L                               31
042000950810    7C                   ENDDO
042100161031     c* per IPG  e IMG aggiorno la giacenza con la data entrata a mag giac se spuntato
042200161026     c*  almeno un collo e data = 0
042300161201    7c                   if        *in22 and *in10 and wesspu='S' and
042400161025     C                             olgi1amg='S'
042500161025     C     KGCA          CHAIN     Tigcp21L
042600161025    8c                   if        %found(tigcp21l)
042700161025     C                   MOVE      dateu         GCPDLMa
042800161025     C                   EXCEPT    AGG5
042900161028     c
043000161025    8C                   ENDIF
043100161025    7c                   endif
043200920318     C*
043300950810    6C                   ENDDO
043400950810    5C                   ENDIF
043500161025     c
043600000512    4C                   ENDIF
043700050511   3aC                   ENDIF
043800911018     C*
043900040427     C     KLNA          READE     FNARB05L                               30
044000950810    3C                   ENDDO
044100941019     C*
044200040108     C                   ADD       1             WA                             CONTATORE L6
044300950810    1C                   ENDDO
044400161017     c
044500920813     C*
044600920813     C* RIEPILOGO SPUNTE PER PISTOLA
044700920813     C                   EXSR      RIEPIL
044800911018     C*
044900060508     C* SE NON TROVATE DISCORDANZE e non fatte parzializzazioni
045000060508     c*  se non ho parzii
045100070418     c* 18/04/07 - IMA chiude solo se richiesta forzatura chiusura
045200960530     C     *IN01         IFEQ      *OFF
045300070418     C     *IN10         andeq     *ON
045400161017     C     *IN11         andeq     *Off
045500060508     c
045600160503     c     wselzona      andeq     ' '
045700060508     c
045800060508     c     vidlna        andeq     0
045900060508     c
046000060508     C*  O FORZATA LA CHIUSURA METTO FLAG
046100911018     C     VIDSN         OREQ      'S'
046200060508     c
046300941017     C     KFVV          CHAIN     FNFVV01L                           32
046400920813     C  N32              MOVEL     'S'           FVVFCF
046500941017     C  N32              UPDATE    FNFVV000
046600911018     C                   END
046700050511     C*
046800081014     C* CHIUSURA PGM
046900990212     C                   CLEAR                   DSBS02
047000990212     C                   MOVEL     'C'           T02TLA
047100990212     C                   CALL      'TIBS02R'
047200990212     C                   PARM                    KPJBA
047300990212     C                   PARM                    DSBS02
047400031219     c
047500031219     C                   CLEAR                   DSLV53
047600031219     C                   MOVEL     'C'           D53TLA
047700031219     C                   CALL      'FNLV53R'
047800031219     C                   PARM                    DSLV53
047900040806     C*
048000040806     C                   MOVEL     'C'           I12TLA
048100040806     C                   CALL      'FIDN12R'
048200040806     C                   PARM                    FIDN12DS
048300140430     C*
048400140430     c                   clear                   fnlrv7ds
048500140430     C                   MOVEL     'C'           ILRV7TLA
048600140430     c                   eval      kpjbu= fnlrv7ds
048700140430     C                   CALL      'FNLRV7R'
048800140430     C                   PARM                    KPJBA
048900160927
049000160927     c                   clear                   fnlgi1ds
049100160927     c                   eval      ILGI1TLA= 'C'
049200160927     c                   movel     fnlgi1ds      kpjbu
049300160927     c                   call      'FNLGI1R'
049400160927     c                   parm                    kpjba
049500160927
049600040806      *
049700000000     C                   SETON                                        LR
049800031219     C**************************************************************************
049900031219     C* CONTROLLO SE SEGNACOLLO MANCANTE
050000031219     C**************************************************************************
050100031219     C     CTRART        BEGSR
050200031219     C*
050300031219     C                   SETOFF                                       131415
050400100706     C                   SETOFF                                       3316
050500161025     C                   MOVE      *BLANKS       STADSF           17
050600031219     C*
050700070118     C     KBRV1         SETLL     FNBRV01L                               33
050800070418     c
050900161017     c* se non ho trovato spunta IMx e spedizione in distinta, cerco
051000070418     c*  la categoria 4 del giorno
051100070418    1c                   if        not *in33 and arbndc>0 and vidmdc='S'
051200100706     C                   Z-ADD     4             conNPG
051300140618     C     KBRV4         chain     FNBRV05L
051400140618    2c                   if        %found(fnbrv05l)
051500070418     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
051600070418     c                   clear                   dslv53
051700070418     C                   z-add     brvnFV        D53NFV
051800070418     C                   MOVEL     brvNPG        D53NPG
051900070418     C                   MOVEL     brvFGS        D53FGS
052000070418     C                   CALL      'FNLV53R'
052100070418     C                   PARM                    DSLV53
052200070418     C* ERRORE
052300070418     c*  se sparato nella stessa data: ok
052400070418     c*  e sparato in altra data, rientro nei giri normali di controllo
052500070418     c*  spunte
052600070418    3c                   if        d53err=' ' and d53dfv=savdfv
052700070418     c                   seton                                        33
052800070418    3c                   endif
052900070418    2c                   endif
053000070418    1c                   endif
053100070418     c
053200031219     C* 14 ON - SPUNTA NON TROVATA
053300031219    1C     *IN33         IFEQ      *OFF
053400070418     c
053500031219     C                   SETON                                        14
053600031219     c*
053700031219     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
053800031219     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
053900031219     c*  immissione + alta
054000031219     C                   exsr      RICSPU
054100031219     C
054200031219     C* SE SPUNTA TROVATA CON DATA > STAMPO MA NON CREO ANOMALIA
054300031219     C                   IF        WDFV>SAVDFV
054400031219     C                   EVAL      *IN13=*ON
054500031219     C                   ENDIF
054600031219     C
054700031219     C* SE SPUNTA TROVATA CON DATA = STAMPO E CREO ANOMALIA SOLTANTO SE
054800031219     C*  CATEGORIA 2 O 6
054900031219     C                   IF        WDFV=SAVDFV
055000031219     C                   EVAL      *IN13=*ON
055100101202     C                   IF        BRVNPG=2
055200031219     C                   EVAL      *IN15=*ON
055300031219     C                   ENDIF
055400031219     C                   ENDIF
055500031219     c*
055600031219    1C                   END
055700960523     C*
055800031219     C
055900031219    1C                   IF        WDFV>=SAVDFV
056000960523     C* GIRO LA DATA
056100031219     C                   MOVE      WDFV          G02INV
056200960523     C                   MOVE      *ZEROS        G02DAT
056300960523     C                   MOVEL     3             G02ERR
056400960523     C                   CALL      'XSRDA8'
056500960523     C                   PARM                    WLBDAT
056600960523     C                   MOVEL     G02DAT        ST2DFV            4 0
056700960523     C**
056800960523     C* DECODIFICO LA FILIALE GESTIONE
056900031219     C     BRVFGS        CHAIN     AZORG01L                           32
057000031219    2C  N32ORGDE5        IFNE      *BLANKS
057100960523     C                   MOVEL     ORGDE5        ST2FGS            8
057200960523     C                   ELSE
057300960523     C                   MOVEL     ORGDES        ST2FGS            8
057400031219    2C                   ENDIF
057500960523     C**
057600960523     C* DECODIFICO LA CATEGORIA SPUNTA
057700031219    2C     BRVNPG        IFEQ      3
057800161017     C                   MOVEL     '7B'          COD
057900161017     C                   MOVEL(P)  wspg          KEY
058000161017     C     KTAB          CHAIN     TABEL                              32
058100161017     C  N32              MOVEL     TBLUNI        ds7b
058200161017     C   32              CLEAR                   ds7b
058300161017     C                   MOVEL     §7bdesb       ST2CAT            3
058400031219   X2C                   ELSE
058500960523     C                   MOVEL     '7N'          COD
058600031219     C                   MOVEL(P)  BRVNPG        KEY
058700960523     C     KTAB          CHAIN     TABEL                              32
058800960523     C  N32              MOVEL     TBLUNI        ST2CAT
058900960523     C   32              CLEAR                   ST2CAT
059000031219    2C                   ENDIF
059100031219     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
059200031219    2C                   IF        BRVNPG=4
059300031219     C                   Z-ADD     BRVNFV        WNFV5
059400031219     C     KFVV2         CHAIN(N)  FNFVV000                           32
059500161026     C  N32              eval      stadsf=fvvdsf
059600031219    2C                   ENDIF
059700031219    1C                   ENDIF
059800960523     C*
059900100706     c* Se trovata spunta, segnalo se sparata xx volte in IMA
060000100706     c*  solo per bolle in IMA più vecchie di ....
060100100707     c* Escludo bolle in distinta
060200150113     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
060300100707     c                   if        not *in14 and not *in10
060400100706     c                             and arbndc=0 and arbdsp<datapresIMA
060500100706     c
060600100706     c* conto quante volte presente in ima
060700100706     C                   Z-ADD     3             conNPG
060800100706     c                   clear                   conta             4 0
060900140618     C     KBRV4         setll     FNBRV05L
061000140618     C     KBRV4         reade     FNBRV05L
061100100706     c
061200140618    2c                   dow       not %eof(fnbrv05l)
061300100706     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
061400100706     c                   clear                   dslv53
061500100706     C                   z-add     brvnFV        D53NFV
061600100706     C                   MOVEL     brvNPG        D53NPG
061700100706     C                   MOVEL     brvFGS        D53FGS
061800100706     C                   CALL      'FNLV53R'
061900100706     C                   PARM                    DSLV53
062000100706     c
062100100706     c                   if        d53spg='A'
062200100706     c                   eval      conta=conta+1
062300100706     c                   endif
062400100706     c
062500140618     C     KBRV4         reade     FNBRV05L
062600100706     c                   enddo
062700100706     c
062800100706     c                   if        conta>=§vpoggima
062900100706     c                   seton                                        16
063000100706     c                   endif
063100100706     c
063200100706     c
063300100706     c                   endif
063400100706     c
063500960523     C                   ENDSR
063600960523     C*
063700031219     C**************************************************************************
063800031219     C* cerco spunta del collo in qualsiasi categoria
063900031219     C**************************************************************************
064000031219     C     RICSPU        BEGSR
064100031219     C                   CLEAR                   Wimm
064200031219     C                   CLEAR                   WSAVB
064300031219     C                   CLEAR                   WDFV
064400031219     C                   CLEAR                   WTROV
064500040108     C                   CLEAR                   WSPG
064600070118RM*  C     Kartd         CHAIN     FNBRV07l                           33
064700031219RM*  C*
064800031219RM* 1C     *IN33         DOWEQ     *OFF
064900031219     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
065000031219     c                   clear                   dslv53
065100031219     C                   z-add     brvnFV        D53NFV
065200031219     C                   MOVEL     brvNPG        D53NPG
065300031219     C                   MOVEL     brvFGS        D53FGS
065400070118     C****               MOVEL     brvfle        D53flf
065500031219     C                   CALL      'FNLV53R'
065600031219     C                   PARM                    DSLV53
065700031219     C* ERRORE
065800031219    2C     D53ERR        IFeq      ' '
065900070118     c                   movel     brvdfs        brvimm           14 0
066000031219     c                   move      brvhms        brvimm
066100031219    3c                   select
066200031219     c* Se maggiore memorizzo
066300031219     c                   when      d53dfv>wdfv
066400031219     C                   MOVEL     RECBRV        WSAVB
066500070118     C                   MOVEL     brvimm        Wimm             14 0
066600031219     C                   MOVEL     d53DFV        WDFV
066700040108     C                   MOVEL     d53spg        WSPG
066800031219     C                   MOVEL     'S'           WTROV             1
066900031219     c
067000031219     c* Se uguale tengo la data ora immissione + alta
067100031219     c                   when      d53dfv=wdfv  and brvimm>wimm
067200070118     C                   MOVEL     brvimm        Wimm             14 0
067300031219     C                   MOVEL     RECBRV        WSAVB
067400031219     C                   MOVEL     d53DFV        WDFV
067500040108     C                   MOVEL     d53spg        WSPG
067600031219     C                   MOVEL     'S'           WTROV             1
067700031219     C                   eval      wimm=brvimm
067800040129     c* Se stessa data e ora immissione, tengo cat 3 o 4
067900101202     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
068000070118     C                   MOVEL     brvimm        Wimm             14 0
068100040129     C                   MOVEL     RECBRV        WSAVB
068200040129     C                   MOVEL     d53DFV        WDFV
068300040129     C                   MOVEL     d53spg        WSPG
068400040129     C                   MOVEL     'S'           WTROV             1
068500040129     C                   eval      wimm=brvimm
068600031219    3c                   endsl
068700031219    2c                   endif
068800031219     c
068900070118RM*  C     Kartd         reade     FNBRV07l                               33
069000031219    1c                   enddo
069100031219     c
069200031219     C     WTROV         IFEQ      'S'
069300031219     C                   MOVEL     WSAVB         RECBRV
069400031219     C                   ENDIF
069500031219     c                   endsr
069600941017     C**************************************************************************
069700941017     C* IMPOSTO CAMPI FISSI
069800941017     C**************************************************************************
069900911018     C     IMPO          BEGSR
070000921022     C*
070100101202     C                   Z-ADD     1             CODUT             1 0
070200921022     C                   Z-ADD     3             IMANPG
070300950810     C                   CLEAR                   SAVSPE
070400950810     C                   CLEAR                   COMSPE
070500921022     C                   TIME                    UTIME             6 0
070600960927     C                   TIME                    W0140            14 0
070700960927     C                   MOVE      W0140         UDATE8            8 0
070800101202      *
070900101202      * Reperimento dati utente/aziendali
071000101202     c                   exsr      sr_DatiJob
071100161130     c
071200161130     c* per ora solo le filiali abilitate all'attesa in IMA hanno ima e img modificato
071300161130     c                   clear                   trulvpods
071400161130     c                   eval      ivpoke1 = 'DECOFIIMG'
071500161130     c                   call      'TRULVPOR'
071600161130     c                   parm                    trulvpods
071700161130
071800161130     c                   if         %lookup('999':skFilOk)>0    or
071900161130     c                              %lookup(%editc(vidfil:'X'):skFilOk)>0
072000161130     c                   seton                                        20
072100161130     c                   endif
072200161130     c
072300161130     c* Solo se filiale partita con IPG devo aggiornare la data entrata mag giac
072400161130     c                   clear                   trulvpods
072500161130     c                   eval      ivpoke1 = 'DECOFIIPG'
072600161130     c                   call      'TRULVPOR'
072700161130     c                   parm                    trulvpods
072800161130
072900161130     c                   if         %lookup('999':skFilOk)>0    or
073000161130     c                              %lookup(%editc(vidfil:'X'):skFilOk)>0
073100161130     c                   seton                                        22
073200161130     c                   endif
073300161130     c
073400161031     C* 11 e 10 ON - IPG
073500161014     c                   select
073600161014     c                   when      vidtin='Z'
073700161017     C                   SETON                                        1011
073800161017     C* 10 ON - IMG
073900161017     C                   when      vidtin='G'
074000950810     C                   SETON                                        10
074100161014     C                   ENDsl
074200921022     C*
074300921022     C* CHAIN E DATA DEL FOGLIO IMA
074400941017     C     KFVV          CHAIN(N)  FNFVV01L                           32
074500921022     C*
074600941018     C                   Z-ADD     FVVDFV        G02INV                         AAAA/MM/GG
074700941018     C                   Z-ADD     *ZERO         G02DAT
074800941018     C                   MOVEL     '3'           G02ERR
074900941018     C                   CALL      'XSRDA8'
075000941018     C                   PARM                    WLBDAT
075100941018     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
075200950316     C*
075300950317RM*  C                   Z-ADD     FVVDFV        SAVDFV
075400950317RM*  C                   Z-ADD     FVVFGS        SAVFGS
075500931228     C*
075600941028     C* DECODIFICA FILIALE DI APERTURA FOGLIO
075700941018     C     FVVFGS        CHAIN     AZORG                              32
075800931228     C  N32              MOVEL     ORGDES        DESCDU           20
075900000512     C*
076000000512     C                   Z-ADD     15            ANMCAA
076100000512     C                   MOVEL     '7C'          COD
076200000512     C                   MOVEL(P)  ANMCAA        KEY
076300000512     C     KTAB          CHAIN     TABEL00F                           35
076400000512     C  N35              MOVEL     TBLUNI        DS7C
076500000512     C   35              MOVEL     *BLANKS       DS7C
076600911018     C*
076700931228     C* 02 ON -  D E F I N I T I V O
076800911018     C     VIDSN         IFEQ      'S'
076900911018     C                   SETON                                        02
077000911018     C*
077100921022     C* PRENDO FASE ANOMALIA
077200950810     C                   CLEAR                   PARAM5
077300950810     C                   MOVEL     'L'           PA5TLA
077400950810     C                   MOVEL     3             PA5NPG
077500950810     C                   MOVEL     VIDTIN        PA5SPG
077600950810     C                   CALL      'TRUL11R'
077700950810     C                   PARM                    PARAM5
077800920409     C                   END
077900920813     C*
078000920813     C* DECODIFICA FILIALE IN GESTIONE
078100920813     C     VIDFIL        CHAIN     AZORG                              32
078200920813     C  N32              MOVEL     ORGDES        DESFIL           20
078300941019     C*
078400101215     C* CARICO L6
078500101215     C                   CLEAR                   DSUL06
078600101215     C                   MOVE      '£6'          D06COD
078700101215     C                   MOVEL     VIDFIL        D06KEY
078800101215     C                   MOVEL     'L'           D06TLA
078900101215     C                   MOVEL     DSUL06        KPJBU
079000101215     C                   CALL      'TRUL06R'
079100101215     C                   PARM                    KPJBA
079200101215     C                   MOVEL     KPJBU         DSUL06
079300101215     C                   MOVEA     LIN           Lspu
079400060508     c*
079500101215     C*  Se non parzializzata la linea di arrivo, elaboro L6
079600161014     c*  altrimenti elaboro solo lei
079700060508     c                   if        vidlna=0
079800101215     C                   MOVEA     LIN           L6
079900060508     c                   else
080000060508     c                   clear                   L6
080100060508     c                   movel     vidlna        l6(1)
080200060508     c
080300060508     c* Decodifica
080400060508     C     VIDlna        CHAIN     AZORG                              32
080500060508     C  N32              MOVEL     ORGDES        DESlna           20
080600060508     c                   movel     vidlna        stalna            3
080700060508     c                   endif
080800070418     c
080900070418     c* Elabora anche spedizioni in distinta
081000070418     c                   if        vidmdc='S'
081100070418     c                   eval      stamdc='SI'
081200070418     c                   else
081300070418     c                   eval      stamdc='NO'
081400070418     c                   endif
081500101215     c* solo colli spuntati alla linea di arrivo finale
081600101215     c                   if        parimaa='S'
081700101215     c                   eval      staimaa=csololna
081800101215     c                   endif
081900160503     c* Verifico le richieste delle zone di consegna
082000160503     c                   clear                   wParc             1
082100160503     c                   clear                   wParc1            1
082200160503     c                   clear                   wParc2            1
082300160503     c                   clear                   wMess             1
082400160503     c                   clear                   wSelzona          1
082500160503     c                   if        partpm1='P ' or partpm2='P '
082600160503     c                   eval      wparc='S'
082700160503     c                   eval      wselzona='S'
082800160503     c                   endif
082900160503     c                   if        partpm1='M ' or partpm2='M '
083000160503     c                   eval      wMess='S'
083100160503     c                   eval      wselzona='S'
083200160503     c                   endif
083300160503     c                   if        partpm1='P1' or partpm2='P1'
083400160503     c                   eval      wParc1='S'
083500160503     c                   eval      wselzona='S'
083600160503     c                   endif
083700160503     c                   if        partpm1='P2' or partpm2='P2'
083800160503     c                   eval      wParc2='S'
083900160503     c                   eval      wselzona='S'
084000160503     c                   endif
084100060508     c
084200050510     c* se richiesta soltanto zone parcel o messagerie, carico in
084300050510     c*  schiera
084400160503     c                   if        wparc='S' and wmess='S'
084500160503     c                   clear                   wparc
084600160503     c                   clear                   wmess
084700160503     c                   clear                   wselzona
084800160503     c                   endif
084900160503     c                   if        wparc1='S' and wparc2='S'
085000160503     c                   clear                   wparc1
085100160503     c                   clear                   wparc2
085200160503     c                   eval      wparc='S'
085300160503     c                   endif
085400160503     c
085500160503    1c                   if        wselzona='S'
085600050511     c                   clear                   xx                4 0
085700160502     c                   clear                   yy                4 0
085800050510     c                   movel     vidfil        alffil            3
085900050511     c                   movel     vidfil        wpo
086000050510     c                   movel(P)  '03'          cod
086100050510     c                   movel(P)  vidfil        key
086200050510     c     ktab          setll     tabel
086300050510     c     ktab2         reade     tabel
086400160502    2c                   dow       not %eof(tabel00f) and  wpo=alffil
086500160502    3c                   if        tblflg=' '
086600050510     c                   movel     tblkey        wpo               3
086700160502    4c                   if        wpo=alffil
086800050511     c                   movel     tbluni        ds03
086900160502
087000160502     c* parcel/messaggerie
087100160503    5c                   if        wparc='S' or wmess='S'
087200160502
087300160503    6c                   if        (wmess='S' and §03tpm='M') or
087400160503     c                             (wparc='S' and §03tpm<>'M')
087500050511     c                   add       1             xx
087600050511     c                   movel     tblkey        w005a             5
087700050511     c                   move      w005a         zone(xx)
087800160502    6c                   endif
087900160502    5c                   endif
088000160502     c* parcel 1° e 2° turno
088100160503    5c                   if        (wparc1='S' or wparc2='S')
088200160503     c                             and §03tpm<>'M'
088300160502
088400160503    6c                   if        (wparc2='S' and §03turno='2') or
088500160503     c                             (wparc1='S' and §03turno<>'2')
088600160503     c                   add       1             yy
088700160502     c                   movel     tblkey        w005a             5
088800160503     c                   move      w005a         zone12(yy)
088900160502    6c                   endif
089000160502    5c                   endif
089100160502     c
089200160502    4c                   endif
089300160502    3c                   endif
089400160502     c
089500050511     c     ktab2         reade     tabel
089600160502    2c                   enddo
089700160502    1c                   endif
089800050511     c
089900160503     c                   clear                   statipo
090000160503     c                   clear                   statipo2
090100160503     c                   if        partpm1='P ' or partpm2='P '
090200050511     c                   eval      statipo=cparcel
090300050511     c                   endif
090400050511     c
090500160503     c                   if        partpm1='M ' or partpm2='M '
090600160503     c                   if        statipo=*blanks
090700050511     c                   eval      statipo=cmessag
090800160503     c                   else
090900160503     c                   eval      statipo2=cmessag
091000160503     c                   endif
091100050511     c                   endif
091200160503     c                   if        partpm1='P1' or partpm2='P1'
091300160503     c                   if        statipo=*blanks
091400160503     c                   eval      statipo=cparc1
091500160503     c                   else
091600160503     c                   eval      statipo2=cparc1
091700160503     c                   endif
091800160503     c                   endif
091900160503     c                   if        partpm1='P2' or partpm2='P2'
092000160503     c                   if        statipo=*blanks
092100160503     c                   eval      statipo=cparc2
092200160503     c                   else
092300160503     c                   eval      statipo2=cparc2
092400160503     c                   endif
092500160503     c                   endif
092600941017     C*
092700100706      * Aggancio tabella VPO recpord VPO
092800100706     c                   Clear                   Dvpo
092900100706     c                   Clear                   dsbs02
093000100706     c                   Eval      T02Mod = 'C'
093100100706     c                   Eval      T02tla = 'L'
093200100706     c                   Eval      T02Sif = knsif
093300100706     c                   Eval      T02Cod = 'VPO'
093400100706     c                   Movel(p)  'VPO'         T02Ke1
093500100706     c                   Call      'TIBS02R'
093600100706     c                   Parm                    Kpjba
093700100706     c                   Parm                    dsbs02
093800100706     c                   If        T02Err = *Blanks
093900100706     c                   Movel     T02Uni        Dvpo
094000100706     c                   EndIf
094100100706     c
094200100706     c     *eur          move      udate8        dataeur
094300100706     c                   movel     dataeur       dataiso
094400161017     c
094500161017     c                   movel     dataiso       dateu             8 0
094600161017
094700100706     c                   if        §vpoggima>0
094800100706     c                   subdur    §vpoggima:*d  dataiso
094900100706     c                   endif
095000100706     c
095100100706     c                   movel     dataiso       datapresIMA       8 0
095200100706
095300911018     C                   ENDSR
095400941017     C**************************************************************************
095500941017     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
095600941017     C**************************************************************************
095700911018     C     ERRORE        BEGSR
095800941017     C*
095900911015     C                   SETON                                        01
096000040603     c
096100040603     c     kar5          chain     fiar501l
096200040603     c                   if        %found(fiar501l)
096300040603     c                   movel     ar5uni        dar5gen
096400040603     c                   else
096500040603     c                   clear                   dar5gen
096600040603     c                   endif
096700911018     C* STAMPA ANOMALIA
096800040428     C                   EXSR      STAMPA
096900081014     c
097000100706     c* non creo anomalia se collo doppio o solo segnalazione
097100161017    0c                   if        not *in12 and not *in16 and not *in11
097200161018     c                             and not *in18
097300161201     c* in IMA e IMG la creo solo se non è da portare a mag giac  se partito con IPG
097400161025     c                             and wgiac<>'D'
097500911015     C*
097600950707     C* SOLO SE IMA DEFINITIVO  CREO ANOMALIA
097700950608    1C     VIDSN         IFEQ      'S'
097800950810     C* NON DEVO CREARE:
097900950810     C* 1) SE C'E' LA FORZATURA E MANCA LA DATA DI ARRIVO
098000950810     C*    ANCHE SE STAMPO LO STESSO
098100950810     C* 2) SE LA SPUNTA E' STATA TROVATA IN ALTRA CATEGORIA DEL GIORNO
098200960419     C*    3 O 4 NON DISABILITO NEMMENO LA BOLLA
098300960419     C* 3) SE LA SPUNTA E' STATA TROVATA IN ARRIVO CREO L'ANOMLIA
098400950608    2C     ARTDAM        IFGT      0
098500960419   2AC     *IN13         IFEQ      *OFF
098600031219     C     *IN15         OREQ      *ON
098700081014i    C*
098800081104     C     KANM          SETLL     FNANM02L                               35
098900081014    3c                   if        not *in35
099000081014     c* Se anomalia chiusa non in pratica idd e data chiusura <data ima
099100081014     c*  la ricreo
099200081104    4c***                if        anmdch>0 and anmdch<savdfv and
099300081104     c***                          anmcch<>'PR'
099400081104     c***                delete    fnanm000
099500081104     c***                seton                                        35
099600081104    4c***                endif
099700081104    3c***                endif
099800081014     c
099900081104    3C***                if        *in35
100000081104     C*
100100081104     C                   MOVEL     *BLANKS       ANMATB
100200081104     C                   MOVEL     PA5FAS        ANMFSA
100300160503     c*  data salvata del foglio IMA perchè richaino in update
100400100506     C                   Z-ADD     SAVdfv        ANMDAO
100500081104     C                   Z-ADD     15            ANMcaa
100600081104     C                   MOVEL     *BLANKS       ANMCCH
100700081104     C                   MOVEL     VIDFGS        ANMCDU
100800081104     C                   Z-ADD     COMNFV        ANMNFV
100900081104     C                   MOVEL     VIDFGS        ANMFLE
101000081104     C                   MOVE      *BLANKS       ANMFLO
101100081104     C                   MOVEL     *BLANKS       ANMFT1
101200081104     C                   MOVEL     *ZEROS        ANMFL1
101300081104     C                   MOVEL     *BLANKS       ANMFT2
101400081104     C                   MOVEL     *ZEROS        ANMFL2
101500081104     C*
101600081104     C                   MOVEL     §7CTAN        ANMTAN
101700081104     C                   MOVEL     §7CSAN        ANMSAN
101800081104     C                   MOVEL     §7CAIE        ANMAIE
101900081104     C                   Z-ADD     §7CLID        ANMLID
102000081104     C                   MOVE      *ZEROS        ANMDUE
102100081104     C                   MOVE      *ZEROS        ANMNPG
102200081104     C                   MOVE      *ZEROS        ANMFGS
102300081104     C                   Z-ADD     0             ANMDCH
102400081104     C                   MOVEL     ' '           ANMFSC
102500081104     C* CHIUSURA AUTOMATICA
102600081104     C     §7CCHA        IFEQ      'S'
102700100506     C                   Z-ADD     SAVDFV        ANMDCH
102800081104     C                   MOVEL     PA5FAS        ANMFSC
102900081104     C                   END
103000081104     C                   MOVE      *BLANKS       ANMFT3
103100081104     C                   MOVE      *ZEROS        ANMFL3
103200081104     C                   MOVE      *BLANKS       ANMFT4
103300081104     C                   MOVE      999           ANMFL4
103400081104     C                   MOVE      *ZEROS        ANMDTR
103500081104     C*
103600911018     C                   MOVEL     ARTFLS        ANMFLS
103700911018     C                   MOVEL     ARTLNA        ANMLNA
103800911018     C                   MOVEL     ARTNRS        ANMNRS
103900941018     C                   MOVEL     ARTNSC        ANMSCN
104000941018     C                   Z-ADD     ARTAAS        ANMAAS
104100911018     C                   MOVEL     ARTNSP        ANMNSP
104200911018     C                   MOVEL     ARTLNP        ANMLNP
104300970206     C                   MOVEL     ARTNAP        ANMNPS
104400040603     c
104500040603     c* imposto data e ora di creazione
104600050707     C                   TIME                    W0140            14 0
104700050707     C                   MOVE      W0140         G02DAT
104800050707     C                   Z-ADD     *ZERO         G02INV
104900050707     C                   MOVEL     *BLANKS       G02ERR
105000050707     C                   CALL      'XSRDA8'
105100050707     C                   PARM                    WLBDAT
105200050707     C                   Z-ADD     G02INV        WDATA             6 0
105300050707     c**                 time                    wora              6 0
105400050707     c**                 z-add     *date         wdata             6 0
105500050707     C
105600050707     c                   movel     w0140         wora4             4 0
105700040603     c                   movel     wdata         anmflo
105800040603     c                   move      wora4         anmflo
105900040603     c
106000040603     c* imposto se flag di valore
106100040603     c                   movel     §ar5bva       anmft4
106200040603     c
106300040603     c
106400920409     C* ESTERNA
106500950608    4C     §7CAIE        IFEQ      'E'
106600950608     C                   MOVEL     ARTLNP        ANMFL1
106700950810    4C                   ENDIF
106800920409     C*
106900990511     C                   WRITE     FNANM000
107000081014    3C                   endif
107100081014     c
107200000512   2AC                   ENDIF
107300960419    2C                   ENDIF
107400920318     C*
107500000512     C* DISABILITO LA BOLLA SOLO SE HO CREATO ANOMALIA
107600000512     C* NO POSTE SEMPRE, LNP POSTE SOLO SE PREVOSTO
107700950810     C* SOLO SE NON HO TROVATO UN'ALTRA SPUNTA IN DATA DEL FOGLIO IMA
107800960419    2C     *IN10         IFEQ      *OFF
107900960419     C*
108000960419    3C     *IN13         IFEQ      *OFF
108100031219     C     *IN15         OREQ      *ON
108200000512     C*
108300040428    5c     wchain        ifeq      *blanks
108400040428     c     fnarbnrri     chain(e)  fnarb00f
108500040428    6c                   if        %error
108600040428     C* Valorizzo la DS di procedura
108700040428     C                   CLEAR                   TRUL82DS
108800040428     c                   eval      ul82§rrn = fnarbnrri
108900040428     C                   EVAL      UL82§FIL = 'FNARB00F'
109000040428     C                   EVAL      UL82§WIN = 'N'
109100040428     C                   EVAL      UL82§F7  = 'S'
109200040428     C                   EVAL      UL82§num = 5
109300040428     C                   EVAL      UL82§att = 60
109400040428     C                   EVAL      UL82§mss = msgjob
109500040428     C* Effettuo la chiamata al *PGM d utilità
109600040428     C                   CALL(e)   'TRUL82R'
109700040428     C                   PARM                    TRUL82DS
109800040428     C     fnarbnrri     chain     fnarb00f
109900040428    6c                   endif
110000040428     C                   Z-ADD     0             ARBAMA
110100040428     c                   update    fnarbf00
110200040428     c                   move      '1'           wchain
110300040428    5c                   endif
110400930629     C* AGGIORNO IL COLLO CON UNA 'D' DI DISABILITAZIONE
110500941221     C                   MOVEL     'S'           ARTDAB                         'D'=DISABILITATO
110600000512    3C                   ENDIF
110700960419    2C                   ENDIF
110800950810     C*
110900040428     C  N10              UPDATE    FNART000
111000950810    1C                   ENDIF
111100081014    0C                   ENDIF
111200911015     C*
111300911015     C                   ENDSR
111400950810     C**************************************************************************
111500950810     C* ELABORAZIONE DELL'IMA
111600950810     C**************************************************************************
111700950810     C     ELAIMA        BEGSR
111800950810     C*
111900950810     C* N O N   C O N T R O L L O :
112000950810     C*          1) SE NO ABILITAZIONE FORZATA E SENZA DATA ARRIVO
112100990212    1C     SAVAMA        IFNE      3
112200950810     C     ARTDAM        ANDEQ     0
112300950810     C                   SETON                                        34
112400990212    1C                   ENDIF
112500950810     C*
112600950810     C*          2) SE DATA ARRIVO MAGGIORE DATA FOGLIO IMA
112700990212    1C  N34ARTDAM        IFNE      0
112800950810     C     ARTDAM        ANDGT     SAVDFV
112900950810     C                   SETON                                        34
113000990212    1C                   ENDIF
113100950810     C*
113200990212     C*          3) SE COLLO SI TROVA IN DCD E RESO O DISTRUTTO PER
113300990212     C*             AVARIA O COLLO MANCANTE
113400990212     C     KARTD         CHAIN     FNDCD02L                           31
113500990212    1C     *IN31         IFEQ      *OFF
113600990212     C*
113700990303     C     DCDATB        ANDEQ     ' '
113800990212     C* CHAIN TESTATA PER CONTROLLARE LA DATA DI CHIUSURA
113900990212     C     KDCD          CHAIN     FNDCT01L                           32
114000050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
114100050221     c                   If        Not *In32 and dctdt2 > 0
114200050221     c                   Eval      *In32 = *On
114300050221     c                   EndIf
114400990303     C*
114500990303    2C     *IN32         IFEQ      *OFF
114600990303     C* SE E' CHIUSA CONTROLLO SE IL RECORD DEVE ESSERE COMPRESO
114700990303     C*  IN IMA, PER ALCUNI TIPI ANOMALIA
114800990303     C                   MOVEL     'S'           §CHIMA
114900990303    3C     DCTCCH        IFNE      *BLANKS
115000990303     C                   CLEAR                   DSBS02
115100990303     C                   MOVEL     'C'           T02MOD
115200990303     C                   MOVEL     KNSIF         T02SIF
115300990303     C                   MOVEL     'CCH'         T02COD
115400990303     C                   MOVEL     DCTCCH        T02KE1
115500990303     C                   CALL      'TIBS02R'
115600990303     C                   PARM                    KPJBA
115700990303     C                   PARM                    DSBS02
115800990303     C*
115900990303    4C     T02ERR        IFEQ      ' '
116000990303     C                   MOVEL     T02UNI        DCCH
116100990303    4C                   ENDIF
116200990303    3C                   ENDIF
116300990303     C**
116400990303     C** SE NON DEVE ESSERE COMPRESO:
116500990303    3C     §CHIMA        IFNE      'S'
116600990303     C     DCTDCH        OREQ      0
116700990212     C** SE ENTRAMBI APERTI O CHIUSI CON STESSA DATA CONTROLLO IL TIPO
116800990212     C**  ANOMALIA PER DECIDERE SE TENERE O ESCLUDERE IL COLLO
116900990212     C**  NON CONSIDERO LA CAUSALE PAREGGIO CHE SIGNIFICA CHE IL COLLO
117000990212     C*  C'E' SEMPRE
117100990212    4C     DCTDCH        IFEQ      DCDDCH
117200990212     C     DCDDCH        OREQ      0
117300990212     C**
117400990212     C                   CLEAR                   DSBS02
117500990212     C                   MOVEL     'C'           T02MOD
117600990212     C                   MOVEL     KNSIF         T02SIF
117700990212     C                   MOVEL     'TAD'         T02COD
117800990212     C                   MOVEL     DCTTAD        T02KE1
117900990212     C                   CALL      'TIBS02R'
118000990212     C                   PARM                    KPJBA
118100990212     C                   PARM                    DSBS02
118200990212     C**
118300990212    5C     T02ERR        IFEQ      ' '
118400990212     C                   MOVEL     T02UNI        DTAD
118500990212     C                   ELSE
118600990212     C                   MOVEL     'A'           §TARGR
118700990212    5C                   ENDIF
118800990212    C**
118900990212     C* SE MANCANZE
119000990212    5C     §TARGR        IFEQ      'M'
119100990212     C* O AVARIA RESA
119200990212     C     §TARGR        OREQ      'V'
119300990212     C     DCTDSD        ANDEQ     'R'
119400990212     C* O AVARIA DISTRUTTA
119500990212     C     §TARGR        OREQ      'V'
119600990212     C     DCTDSD        ANDEQ     'D'
119700990212     C*                    --> ESCLUDO
119800990212     C                   SETON                                        34
119900990212    5C                   ENDIF
120000990212    4C                   ENDIF
120100990212    3C                   ENDIF
120200990212    2C                   ENDIF
120300990212    1C                   ENDIF
120400101215     c
120500101215     c* Se opzione solo spuntato alla linea di arrivo finale, controllo
120600101215     c                   if        parimaa='S'
120700101215     c                   setoff                                       37
120800101215RM*  C     Kartd         setll     FNBRV07l
120900101215RM*  C     Kartd         reade     FNBRV07l
121000101215     c                   dow       not %eof(fnbrv07l)
121100101215     c     brvfgs        lookup    Lspu                                   37
121200101215     c                   if        *in37
121300101220     c* Se la filiale che spunta è giusta verifico che sia categoria di "arrivo"
121400101220     c                   clear                   dslv53
121500101220     C                   z-add     brvnFV        D53NFV
121600101220     C                   MOVEL     brvNPG        D53NPG
121700101220     C                   MOVEL     brvFGS        D53FGS
121800101220     C                   CALL      'FNLV53R'
121900101220     C                   PARM                    DSLV53
122000101220     C*
122100101220    2C     D53ERR        IFeq      ' '
122200101220     c*
122300101220     c                   if        d53def='S'
122400101220     c                   eval      wnpgarr=%editc(d53npg:'X')+'D'
122500101220     c                   else
122600101220     c                   eval      wnpgarr=%editc(d53npg:'X')+d53spg
122700101220     c                   endif
122800101220     c
122900101220     c                   eval      TROV=%lookup   (wnpgarr: NPGARR)
123000101220     c                   if        TROV>0
123100101215     c                   leave
123200101220     c                   endif
123300101215     c                   endif
123400101220     c                   endif
123500101220     c
123600101220     c                   setoff                                       37
123700101220RM*  C     Kartd         reade     FNBRV07l
123800101215     c
123900101215     c                   enddo
124000101215     c
124100101215     c* se non trovata nessuna spunta, NON elaboro
124200101215     c                   if        not *in37
124300101215     c                   seton                                        34
124400101215     c                   endif
124500101215     c
124600101215     c                   endif
124700990212    C**
124800100706     c                   if        not *in34
124900100706     C                   EXSR      CTRART
125000950810     C*
125100950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
125200100706     C* 16 ON -     TROVATO SEGNACOLLO IN IMA più di xxx volte
125300100706     C   14
125400100706     Cor 16              EXSR      ERRORE
125500100706     c                   endif
125600950810     C*
125700950810     C                   ENDSR
125800950810     C**************************************************************************
125900950810     C* ELABORAZIONE DELL'IMG
126000950810     C**************************************************************************
126100950810     C     ELAIMG        BEGSR
126200950810     C                   EXSR      CTRART
126300950810     C*
126400161031     C* 14 ON - NON TROVATO SEGNACOLLO IN IMG o IPG
126500161025     c                   if        *in14
126600161026     c* In caso di bolla con giacenza e merce da portare a mag giac.
126700161026     c*  non stampo nulla per IMG
126800161026     c                   if        *in11 or wgiac<>'D'
126900161025     C                   EXSR      ERRORE
127000161026     c                   endif
127100161026     c
127200161025     c                   else
127300161025     c                   eval      Wesspu='S'
127400161025     c                   endif
127500161025
127600950810     C                   ENDSR
127700941017     C**************************************************************************
127800941017     C* STAMPO DISCORDANZA
127900941017     C**************************************************************************
128000900621     C     STAMPA        BEGSR
128100050405     c                   setoff                                       242526
128200941017     C*
128300911015     C* STAMPA TESTATA
128400911015     C   OF              DO
128500911018     C                   SETON                                        05
128600911018     C                   EXCEPT    TES
128700920813     C                   EXCEPT    TES2
128800911018     C                   SETOFF                                       OF
128900911018     C                   END
129000090522     c* Se non trovata la bolla (per colli doppi)
129100090522    1c                   if        NoBolla='S'
129200090522     c                   except    DetNObo
129300161018     C   12              EXCEPT    DETdoppio
129400161018     C   18              EXCEPT    DETeccede
129500090522   x1c                   else
129600911018     C*
129700950810     C     COMSPE        IFNE      ARBSPE
129800911018     C                   SETON                                        05
129900950810     C                   MOVEL     ARBSPE        COMSPE
130000950810     C                   ENDIF
130100911018     C* DATA SPEDIZIONE
130200911018     C   05              DO
130300941018     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
130400941018     C                   MOVE      ARBDSP        G02INV                         AAAA/MM/GG
130500911018     C                   MOVE      *ZEROS        G02DAT
130600911018     C                   MOVEL     3             G02ERR
130700941017     C                   CALL      'XSRDA8'
130800911018     C                   PARM                    WLBDAT
130900941018     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
131000941018     C*
131100941018     C* CODICE PADRONCINO CONSEGNA DECODIFICA
131200021203     C*    ARBPDC        CHAIN     FNAPD01L                           06
131300021205     C                   move      arbpdc        kpdpdr
131400021203     C                   move      'A'           kpdtip
131500021203     C     kfapd         chain     fiapd01L                           06
131600941018     C  N06              MOVEL     APDRSC        DESCPC            9
131700100707     c
131800100707     c* se non c'e' paadroncino consegna scrivo se Fermo deposito
131900100707     c                   if        *in06
132000100707     C                   MOVEL     *BLANKS       DESCPC
132100100707     c                   if        arbffd='S'
132200100707     c                   eval      descpc='FDeposito'
132300100707     c                   endif
132400100707     c                   endif
132500920409     C*
132600911018     C* SEGNACOLLI NON CONSECUTIVI
132700911018     C     ARBFNS        COMP      'S'                                    04
132800911018     C* SERIE>< 0
132900911018     C     ARBNRS        COMP      0                                  0303
133000911018     C                   END
133100920409     C*
133200920708     C* 07 ON - MANCA DATA ARRIVO: SPEDIZIONE FORZATA A 3
133300920708     C     ARTDAM        IFEQ      0
133400920708     C                   SETON                                        07
133500920708     C                   ELSE
133600920708     C                   SETOFF                                       07
133700920409     C* DATA ARRIVO MERCE  SEGNACOLLO
133800941018     C                   MOVE      ARTDAM        G02INV                         AAAA/MM/GG
133900920409     C                   MOVE      *ZEROS        G02DAT
134000920409     C                   MOVEL     3             G02ERR
134100941017     C                   CALL      'XSRDA8'
134200920409     C                   PARM                    WLBDAT
134300941018     C                   Z-ADD     G02DAT        STADAM                         GG/MM/AAAA
134400920708     C                   END
134500040603     C* MITTENTE E DESTINATARIO DI 17 E 20
134600040603     C                   MOVEL     ARBRSM        STARSM           17
134700920707     C                   MOVEL     ARBRSD        STARSD           20
134800040603     c* collo di valore
134900040603     c                   movel     §ar5bva       stabva            1
135000161025     c* se si tratta di bolla da portare a mag giac, riempi il campo STADSF con relativa scritta
135100161025     c                   if        not *in11 and wgiac='D'
135200161025     c                   clear                   stadsf
135300161025     c                   eval      stadsf='Portare a MagGIAC'
135400161025     c                   endif
135500161025     c
135600161026     c                   clear                   stahms            4 0
135700161026     c                   eval      stahms=brvhms/100
135800910522     C*
135900900622     C* STAMPO DETTAGLIO
136000100706     C                   EXCEPT    DET
136100100707     c                   select
136200100707     c                   when      *in12
136300100706     C                   EXCEPT    DETdoppio
136400100707     c                   when      *in16
136500100707     C                   EXCEPT    DETspuIMA
136600161018     c                   when      *in18
136700161018     C                   EXCEPT    DETeccede
136800100707     c                   other
136900100706     C                   EXCEPT    DETerr
137000100707     c                   endsl
137100070418     c
137200070418     c* se in distinta , stampo
137300090522    2c                   if        not *in12
137400081014
137500100706    3c                   if        *in05 and arbndc>0 and not *in16
137600070418     C                   MOVE      ARbddc        G02INV                         AAAA/MM/GG
137700070418     C                   MOVE      *ZEROS        G02DAT
137800070418     C                   MOVEL     3             G02ERR
137900070418     C                   CALL      'XSRDA8'
138000070418     C                   PARM                    WLBDAT
138100070418     C                   Z-ADD     G02DAT        STAddc            8 0          GG/MM/AAAA
138200070418     C                   EXCEPT    DETndc
138300090522    3c                   endif
138400050405     c
138500161017     c* Se c'e' ubicazione collo, per 05 on lo stampo
138600050405     c                   if        *in05 and §ar5zng<>*blanks
138700050405     c                   seton                                        24
138800050405     c                   endif
138900050405     c
139000100707     C                   SETOFF                                       05
139100050405     c
139200990212     C* STAMPO TUTTE LE C.A. PRESENTI SULLA SPEDIZIONE
139300040806      * Sostituisco la lettura del file FNDTC02 con
139400040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
139500040806      * legate alla spedizione che passo
139600040806      *
139700040806     c                   clear                   fidn12ds
139800040806     c                   eval      i12aas = arbaas
139900040806     c                   eval      i12lnp = arblnp
140000040806     c                   eval      i12nrs = arbnrs
140100040806     c                   eval      i12nsp = arbnsp
140200040806     c                   eval      i12fel = 'E'
140300040806     c                   eval      i12fan = 'E'
140400040806      *
140500040806     c                   call      'FIDN12R'
140600040806     c                   parm                    fidn12ds
140700040806      *
140800040806      * se non ci sono errori
140900090522    3c                   if        o12err = ' '
141000040806      * se numero di CA trovate maggiore di zero
141100090522    4c                   if        o12nca > 0
141200100706     c
141300050405     c                   seton                                        25
141400040806     c
141500040806     c                   z-add     *zeros        ii                2 0
141600100706    6c     1             do        o12nca        ii
141700040806     c                   movea     skkey(ii)     dskey
141800040806     C                   Z-ADD     dsfil         dctfil
141900040806     C                   Z-ADD     dsnca         dctnca
142000040806     C                   Z-ADD     skdca(ii)     dctdca
142100040806     C                   Z-ADD     skdch(ii)     dctdch
142200040806     C                   movea     skcch(ii)     dctcch
142300040806     c                   exsr      preca
142400050405     c                   except    detca
142500050405     c
142600050405     c                   setoff                                       24
142700100706    6C                   ENDDO
142800040806      *
142900100706    4c                   endif
143000090522    3c                   endif
143100050405     c
143200050405     c* Stampo ubicazione collo se non ancora stampata
143300090522    3c                   if        *in24
143400050405     c                   except    detca
143500090522     c                   setoff                                       24
143600090522    3c                   endif
143700090522     c                   setoff                                       25
143800081014     c
143900090522    2c                   endif
144000090522    1c                   endif
144100900622     C*
144200900622     C                   ENDSR
144300990212     C**************************************************************************
144400990212     C* PREPARO I DATI DELLA C.A.
144500990212     C**************************************************************************
144600990212     C     PRECA         BEGSR
144700990212     C                   SETOFF                                       26
144800990303     C                   CLEAR                   STACCH
144900990212     C* DATA APERTURA C.A.
145000990212     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
145100990212     C                   Z-ADD     *ZERO         G02DAT
145200990212     C                   MOVEL     '3'           G02ERR
145300990212     C                   CALL      'XSRDA8'
145400990212     C                   PARM                    WLBDAT
145500990212     C                   MOVE      G02DAT        W0020             2 0
145600990212     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
145700990212     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
145800990303     C** DATA CHIUSURA C.A.
145900990212     C                   CLEAR                   STACHC
146000990303    1C     DCTDCH        IFGT      0
146100990212     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
146200990212     C                   Z-ADD     *ZERO         G02DAT
146300990212     C                   MOVEL     '3'           G02ERR
146400990212     C                   CALL      'XSRDA8'
146500990212     C                   PARM                    WLBDAT
146600990212     C                   MOVE      G02DAT        W0020             2 0
146700990212     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
146800990212     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
146900990212     C                   SETON                                        26
147000990303     C* CAUSALE CHIUSURA
147100990303    2C     DCTCCH        IFNE      *BLANKS
147200990303     C                   CLEAR                   DCCH
147300990303     C                   CLEAR                   DSBS02
147400990303     C                   MOVEL     'C'           T02MOD
147500990303     C                   MOVEL     KNSIF         T02SIF
147600990303     C                   MOVEL     'CCH'         T02COD
147700990303     C                   MOVEL     DCTCCH        T02KE1
147800990303     C                   CALL      'TIBS02R'
147900990303     C                   PARM                    KPJBA
148000990303     C                   PARM                    DSBS02
148100990303     C*
148200990303    3C     T02ERR        IFEQ      ' '
148300990303     C                   MOVEL     T02UNI        DCCH
148400990303    3C                   ENDIF
148500990303     C                   MOVEL     §CHDES        STACCH           12
148600990303    2C                   ENDIF
148700990303    1C                   ENDIF
148800990212     C                   ENDSR
148900941017     C**************************************************************************
149000161018     C* RIEPILOGO SPUNTE PER PISTOLA e controllo se colli doppi o eccedenti
149100941017     C**************************************************************************
149200920813     C     RIEPIL        BEGSR
149300081014     C                   SETON                                        12
149400941017     C*
149500920813     C                   Z-ADD     1             C
149600920813     C* CONTO SPUNTE PER PISTOLA
149700070118     C     KBRV          SETLL     FNBRV01L
149800070118     C     KBRV          READE     FNBRV01L                               30
149900920813     C*
150000161018    1C     *IN30         DOWEQ     *OFF
150100161018    2c                   if        brvtsu<>*blanks
150200081014     c                   clear                   fnlrv7ds
150300081014     c                   eval      ilrv7fls=brvlnp
150400081014     c                   eval      ilrv7lna=brvlna
150500081014     c                   eval      ilrv7nrs=brvnrs
150600081014     c                   eval      ilrv7nsc=brvnsc
150700081014     c                   eval      kpjbu= fnlrv7ds
150800081014     c                   call      'FNLRV7R'
150900081014     c                   parm                    kpjba
151000081014     c
151100081014     c                   eval      fnlrv7ds=kpjbu
151200161018    3c                   if        olrv7nric>=3 and olrv7part='S'
151300081014     c* Se ho già stampato qualche cosa, lascio la prima volta una
151400081014     c*  riga vuota
151500161018    4c                   if        *in01 and not *inOF and uno=' '
151600081014     c                   except    DETvuota
151700161018    4c                   endif
151800161018     c                   exsr      Cerca_bolla
151900081014     c
152000081014     c                   exsr      ERRORE
152100081014     c                   eval      UNO='S'
152200161018    3c                   endif
152300081014     c
152400161018    2c                   endif
152500161018     c
152600161201     c* solo per  IPG e IMG  stampo i colli eccedenti
152700161201     C*  ( SE partiti cion procedura)
152800161201     c                   if        *in10 and *in22
152900161201     c                   exsr      Eccedenti
153000161201     c                   endif
153100081014     c
153200941019     C                   Z-ADD     1             B                 2 0
153300920813     C                   MOVEL     BRVNPS        COMNPS            2
153400920813     C     COMNPS        LOOKUP    NPS(B)                                 31
153500920813     C  N31              MOVEL     BRVNPS        NPS(C)
153600920813     C  N31              Z-ADD     C             B
153700941019     C  N31              ADD       1             C                 2 0
153800920813     C*
153900920813     C                   ADD       1             CPS(B)
154000920813     C*
154100070118     C     KBRV          READE     FNBRV01L                               30
154200161018    1C                   ENDDO
154300081014     C
154400081014     C                   SETOFF                                       12
154500161018     c
154600081014     C*
154700081014     C* SE NESSUNA ANOMALIA STAMPO TUTTO O.K.
154800081014     C  N01              EXCEPT    TES
154900081014     C  N01              EXCEPT    OK
155000081014     C  N01              SETOFF                                       OF
155100920813     C**
155200920813     C* SOMMO TUTTI I COLLI
155300920813     C                   XFOOT     CPS           TOTSPU            9 0
155400920813     C**
155500920813     C* STAMPA SPUNTE
155600920813     C* TESTATA
155700920813     C   OF              EXCEPT    TES
155800920813     C   OF              SETOFF                                       OF
155900920813     C*
156000920813     C                   EXCEPT    TOT1
156100920813     C*
156200920813     C                   Z-ADD     1             C
156300920813     C     CPS(C)        DOWGT     0
156400920813     C   OF              DO
156500920813     C                   EXCEPT    TES
156600920813     C                   EXCEPT    TOT1
156700920813     C                   SETOFF                                       OF
156800920813     C                   END
156900920813     C*
157000920813     C                   EXCEPT    TOT2
157100920813     C                   ADD       1             C
157200920813     C                   END
157300920813     C*
157400920813     C                   EXCEPT    TOT3
157500941017     C*
157600920813     C                   ENDSR
157700161018     C**************************************************************************
157800161018     C*   da spunta cerco la relativa bolla
157900161018     C**************************************************************************
158000161018     c     Cerca_bolla   BEGSR
158100161018     c                   clear                   nobolla
158200161018     c* chain la bolla e stampo collo doppio
158300161018     c     kartB         chain     fnart27l
158400161018    2c                   if        %found(fnart27l)
158500161018     c     karb          chain     fnarb01l
158600161018   x2c                   else
158700161018     c     kartB         chain     fnblt27l
158800161018    3c                   if        %found(fnart27l)
158900161018     c     karb          chain     fnblp01l
159000161018   x3c                   else
159100161018     c                   eval      Nobolla='S'
159200161018    3c                   endif
159300161018    2c                   endif
159400161018     c                   ENDSR
159500161018     C**************************************************************************
159600161018     C*   Ricerca colli eccedenti
159700161018     C**************************************************************************
159800161018     C     Eccedenti     BEGSR
159900161018     c                   setoff                                       12
160000161018     c                   seton                                        18
160100161018     c                   clear                   ecce_descr
160200161018     c
160300161018     c                   exsr      Cerca_bolla
160400161018     c* Se manca bolla --> sempre eccedente
160500161018    1c                   if        nobolla='S'
160600161018    4c                   if        *in01 and not *inOF and uno=' '
160700161018     c                   except    DETvuota
160800161018    4c                   endif
160900161018     c                   exsr      ERRORE
161000161018     c                   eval      UNO='S'
161100161018   x1c                   else
161200161018     c* Richiamo driver per verificare se la bolla è a mag giac oggi
161300161018     c                   clear                   fnlgi1ds
161400161018     c                   eval      ILGI1AAS=arbaas
161500161018     c                   eval      ILGI1lnp=arblnp
161600161018     c                   eval      ILGI1nrs=arbnrs
161700161018     c                   eval      ILGI1nsp=arbnsp
161800161018    3c                   select
161900161018     c                   when      arbpkc>0 and arbncp=arbncl
162000161018     c                   eval      ilgi1pkf=arbpkc
162100161018     c                   when      arbpkc>0 and arbncp<arbncl
162200161018     c                             and arbpkc>arbpkf
162300161018     c                   eval      ilgi1pkf=arbpkc
162400161018     c                   other
162500161018     c                   eval      ilgi1pkf=arbpkf
162600161018    3c                   endsl
162700161018     c                   movel     fnlgi1ds      kpjbu
162800161018     c                   call      'FNLGI1R'
162900161018     c                   parm                    kpjba
163000161018     c                   movel     kpjbu         fnlgi1ds
163100161201     c* Per IPG --> corretto se il flag da portare a mag giac = "Y"
163200161025     c* Per IMG --> corretto se il flag da portare a mag giac = "Y"  o già in giac
163300161026     c*         --> ignoro anche se deve uscire da mag gaicenza, ma non ancora effettuat
163400161025    2c                   if        (*in11 and olgi1AMG<>'Y'and
163500161025     c                              olgi1AMG<>'S') or
163600161025     c                             (not *in11 and olgi1AMG<>'Y' and
163700161025     c                             olgi1AMG<>'S'  and
163800161026     c                             olgi1inmg<>'S' and olgi1outmg<>'S')
163900161018     c*
164000161018     c                   select
164100161025     c                   when      olgi1inmg<>' '
164200161018     c                   eval      ecce_descr=' -già in Giacenza'
164300161026     c                   when      arbndc>0  and arbdcm=0
164400161018     c                   eval      ecce_descr=' -di bolla in distinta '
164500161025     c                   when      OLGI1OUTMG<>' '
164600161025     c                   eval      ecce_descr=' -non più giacente'
164700161018     c                   other
164800161025     c                   eval      ecce_descr=' -di colli IMA'
164900161018     c                   endsl
165000161018
165100161018    4c                   if        *in01 and not *inOF and uno=' '
165200161018     c                   except    DETvuota
165300161018    4c                   endif
165400161018
165500161018     c                   exsr      ERRORE
165600161018     c                   eval      UNO='S'
165700161018    2c                   endif
165800161025    1c                   endif
165900161018     c
166000161018     c                   setoff                                       18
166100161018     c                   seton                                        12
166200161018     C                   ENDSR
166300941017     C**************************************************************************
166400941017     C*   R O U T I N E     I N I Z I A L E
166500941017     C**************************************************************************
166600941017     C     *INZSR        BEGSR
166700941017     C*
166800161017     C* DEFINIZIONE CHIAVI
166900000126     C     KBRV          KLIST
167000000126     C                   KFLD                    IMANPG
167100000126     C                   KFLD                    WNFV
167200000126     C                   KFLD                    VIDFGS
167300070118     C     KBRV1         KLIST                                                  FNBRV01L
167400941019     C                   KFLD                    IMANPG                         ..CATEGORIA
167500000126     C                   KFLD                    WNFV                           ..NR. FOGLIO
167600950316RM*  C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
167700941019     C                   KFLD                    ARTFLS                         ..LNP
167800941018     C                   KFLD                    ARTLNA                         ..LNA
167900941018     C                   KFLD                    ARTNRS                         ..SERIE
168000941018     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
168100070418     C     KBRV4         KLIST                                                  FNBRV01L
168200070418     C                   KFLD                    CONNPG                         ..CATEGORIA
168300070418     C                   KFLD                    ARTFLS                         ..LNP
168400070418     C                   KFLD                    ARTLNA                         ..LNA
168500070418     C                   KFLD                    ARTNRS                         ..SERIE
168600070418     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
168700070418     C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
168800990212RM*  C     KARTD         KLIST
168900990212RM*  C                   KFLD                    ARTFLS
169000990212RM*  C                   KFLD                    ARTLNA
169100990212RM*  C                   KFLD                    ARTNRS
169200990212RM*  C                   KFLD                    ARTNSC
169300081014     C     KARTb         KLIST
169400081014     C                   KFLD                    brvlnp
169500081014     C                   KFLD                    brvlnA
169600081014     C                   KFLD                    brvNRS
169700081014     C                   KFLD                    brvNSC
169800941017     C     KFVV          KLIST                                                  *
169900941019     C                   KFLD                    IMANPG                         *
170000941017     C                   KFLD                    COMNFV                         *
170100941028     C                   KFLD                    VIDFGS                         *
170200950316RM*  C     KFVV2         KLIST
170300950316RM*  C                   KFLD                    BRVNPG
170400000126RM*  C                   KFLD                    WNFV5
170500950316RM*  C                   KFLD                    BRVFGS
170600941017     C     KANM          KLIST                                                  FNANM02L
170700941017     C                   KFLD                    ARTFLS                         *
170800941017     C                   KFLD                    ARTLNA                         *
170900941017     C                   KFLD                    ARTNRS                         *
171000941018     C                   KFLD                    ARTNSC                         *
171100941017     C                   KFLD                    ANMCAA                         *
171200941017     C     KART          KLIST                                                  *
171300941017     C                   KFLD                    ARBAAS                         *
171400941017     C                   KFLD                    ARBLNP                         *
171500941017     C                   KFLD                    ARBNRS                         *
171600941017     C                   KFLD                    ARBNSP                         *
171700081014     C     KARB          KLIST                                                  *
171800081014     C                   KFLD                    ARtAAS                         *
171900081014     C                   KFLD                    ARtLNP                         *
172000081014     C                   KFLD                    ARtNRS                         *
172100081014     C                   KFLD                    ARtNSP                         *
172200040603     C     Kar5          KLIST                                                  *
172300040603     C                   KFLD                    ARBAAS                         *
172400040603     C                   KFLD                    ARBLNP                         *
172500040603     C                   KFLD                    ARBNRS                         *
172600040603     C                   KFLD                    ARBNSP                         *
172700040603     C                   KFLD                    ktrd                           *
172800970721     C     KGCA          KLIST                                                  *
172900970721     C                   KFLD                    ARBAAS                         *
173000970721     C                   KFLD                    ARBLNP                         *
173100970721     C                   KFLD                    ARBNRS                         *
173200970721     C                   KFLD                    ARBNSP                         *
173300990212     C                   KFLD                    KFRG
173400950810     C     KSAV          KLIST                                                  *
173500950810     C                   KFLD                    SAVAAS                         *
173600950810     C                   KFLD                    SAVLNP                         *
173700950810     C                   KFLD                    SAVNRS                         *
173800950810     C                   KFLD                    SAVNSP                         *
173900941017     C     KLBL          KLIST                                                  *
174000941017     C                   KFLD                    LBLAAN                         *
174100941017     C                   KFLD                    LBLLPN                         *
174200941017     C                   KFLD                    LBLNRN                         *
174300941017     C                   KFLD                    LBLNSN                         *
174400941017     C     KTAB          KLIST
174500941017     C                   KFLD                    CODUT
174600941017     C                   KFLD                    COD
174700941017     C                   KFLD                    KEY
174800050510     C     KTAB2         KLIST
174900050510     C                   KFLD                    CODUT
175000050510     C                   KFLD                    COD
175100990212     C     KDCD          KLIST
175200990212     C                   KFLD                    DCDAAC                         *
175300990212     C                   KFLD                    DCDFIL                         *
175400990212     C                   KFLD                    DCDNCA                         *
175500021203
175600021203     C     Kfapd         klist
175700021203     C                   kfld                    kpdtip
175800021203     C                   kfld                    kpdpdr
175900941017     C*
176000970721     C     *LIKE         DEFINE    GCPFRG        KFRG
176100970721     C                   MOVEL     0             KFRG
176200941017     C     *LIKE         DEFINE    TBLCOD        COD
176300941017     C     *LIKE         DEFINE    TBLKEY        KEY
176400941019     C     *LIKE         DEFINE    BRVNPG        IMANPG                         CATEGORIA FOGLIO
176500070418     C     *LIKE         DEFINE    BRVNPG        conNPG                         CATEGORIA FOGLIO
176600031218     C     *LIKE         DEFINE    BRVNFV        WNFV
176700031218     C     *LIKE         DEFINE    FVVNFV        WNFV5
176800941018     C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO VIAGGIO
176900941018     C     *LIKE         DEFINE    ARTDAM        STADAM                         DATA ARRIVO MERCE
177000941018     C     *LIKE         DEFINE    ARBSPE        COMSPE
177100941019     C     *LIKE         DEFINE    ARBLNA        KLNA                           LINEA ARRIVO
177200031218     C     *LIKE         DEFINE    FVVFGS        SAVFGS
177300950317RM*  C     *LIKE         DEFINE    FVVDFV        SAVDFV
177400941017     C*
177500101220      /free
177600101220       // Carico le categorie con ambito Arrivo
177700101220       xx=2   ;
177800101220       // Alla posizine 1 metto al defluenza
177900101220       npgarr(1)='1D'  ;
178000101220
178100101220       setll   (1:'7N') tabel00f   ;
178200101220       reade   (1:'7N') tabel00f   ;
178300101220
178400101220       dow not %eof(tabel00f)      ;
178500101220       if tblflg=' '               ;
178600101220       ds7n=tbluni                 ;
178700101220
178800101220       if §7ntbo<>' '              ;
178900101220
179000101220          if §7ntbo='A'               ;
179100101220          npgArr(xx)=tblkey;
179200101220          xx=xx+1  ;
179300101220          endif  ;
179400101220       else   ;
179500101220
179600101220         if  §7ntb1='A'                         ;
179700101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc1   ;
179800101220         xx=xx+1  ;
179900101220         endif                                  ;
180000101220         if  §7ntb2='A'                         ;
180100101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc2   ;
180200101220         xx=xx+1  ;
180300101220         endif                                  ;
180400101220         if  §7ntb3='A'                         ;
180500101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc3   ;
180600101220         xx=xx+1  ;
180700101220         endif                                  ;
180800101220         if  §7ntb4='A'                         ;
180900101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc4   ;
181000101220         xx=xx+1  ;
181100101220         endif                                  ;
181200101220         if  §7ntb5='A'                         ;
181300101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc5   ;
181400101220         xx=xx+1  ;
181500101220         endif                                  ;
181600101220         if  §7ntb6='A'                         ;
181700101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc6   ;
181800101220         xx=xx+1  ;
181900101220         endif                                  ;
182000101220
182100101220       endif                       ;
182200101220       endif                       ;
182300101220
182400101220       reade   (1:'7N') tabel00f   ;
182500101220       enddo                       ;
182600101220      /end-free
182700101220
182800941017     C                   ENDSR
182900101202
183000101202      *---------------------------------------------------------------*
183100101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
183200101202      *---------------------------------------------------------------*
183300101202     c     sr_DatiJob    BEGSR
183400101202      *
183500101202     c     *dtaara       define    §azute        AZUTEds
183600101202     c     *dtaara       define    §datiute      dDatiUte
183700101202      *
183800101202     c                   in(E)     *dtaara
183900101202     c                   IF        %Error or RSUT = *blanks
184000101202     c                   clear                   TIBS34ds
184100101202     c                   call      'TIBS34R'
184200101202     c                   parm                    TIBS34ds
184300101202     c                   in        *dtaara
184400101202     c                   ENDIF
184500101202      *
184600101202     c                   ENDSR
184700941017     C**************************************************************************
184800161028     Otigcp000  E            AGG5
184900161017     O                       GCPDLMa
185000950316RM*  OPRTF198   E            TES              02
185100911015     O                       STA(1)              66
185200950810     O              N10      STA(2)             132
185300161017     O               10N11   STA(18)            132
185400161017     O               10 11   STA(22)            132
185500950320     O                       STA(17)            198
185600900626     O                       RSUT                20
185700960927     O                       UDATE8             181 '  /  /    '
185800950320     O                       PAGE          Z    198
185900920813     O          E            TES         1
186000160503     o                       statipo             67
186100160503     o                       statipo2           130
186200050511     O                       UTIME              179 '  :  :  '
186300931228     O               02      STA(4)              70
186400931228     O          E    02      TES         0  0
186500931228     O                       STA(4)              70
186600931228     O          E            TES         2
186700911015     O                       STA(3)              66
186800931228     O                       STA(16)            132
186900060508     O                       STA(20)            198
187000161018     O              n11                          13 'I.M. .'
187100161018     O              n11      VIDTIN              12
187200161031     O               11                          13 'I.P.G.'
187300060508RM*  O                       SAVFGS              21
187400060508     O                       DESCDU              42
187500920813     O                       COMNFV        Z     56
187600950317     O                       STADFV        8     73
187700960927     O                       VIDFIL             116
187800960927     O                       DESFIL             137
187900060508     O                       stalna             177
188000060508     O                       DESlna             198
188100070418     O          E   n10      TES         1
188200070418     O                       STA(21)            198
188300070418     O                       stamdc             188
188400101215     O                       staimaa            146
188500920813     O          E            TES2        2
188600911018     O                       STA(5)              66
188700911018     O                       STA(8)             132
188800040621     O                       STA(19)            198
188900920813     O          E            TES2        1  1
189000911018     O                       STA(6)              66
189100911018     O                       STA(9)             132
189200911015     O*
189300090522     O          E            DETnoBO     1
189400090522     O                       brvLNP        Z      8
189500090522     O                       brvNRS        Z     11
189600090522     O                       brvLNA        Z     29
189700090522     O                       brvZNC              87
189800090522     O                       brvnps             122
189900090522     O                       brvNSC        Z    131
190000090522     O
190100090522     O          E            DET         1
190200090522     O                  05   ARBAAS        Z
190300090522     O                  05   ARBLNP        Z   +  1
190400090522     O                  05 03ARBNRS        Z   +  1
190500090522     O                  05 03                  +  0 '-'
190600090522     O                  05   ARBNSP        Z   +  0
190700090522     O                  05   STADSP            +  1 '  /  '
190800090522     O                  05   ARBLNA        Z   +  1
190900090522     O                  05   STARSM            +  1
191000090522     O                  05   STARSD            +  1
191100090522     O                  05N04ARBNCD        Z   +  1
191200090522     O                  05N04                  +  0 '/'
191300090522     O                  05N04ARBNCA        Z   +  0
191400090522     O                  05 04                    84 'NON SEQUENZIALI'
191500090522     O                  05   ARBZNC            +  1
191600090522     O                  05   stabva            +  0
191700090522     O                  05   ARBTMC            +  1
191800090522     O                  05N06ARBPDC        Z   +  1
191900090522     O                  05N06                  +  0 '-'
192000100707     O                  05   DESCPC            +  0
192100090522     O              N07      STADAM            +  1 '  /  /    '
192200090522     O              N07      ARTNAP            +  1
192300090522     O              N07      ARTNSC        Z   +  2
192400090522     O               07                         131 'ABILIT.MERCE FORZATA'
192500081014     O* ERRORI:
192600950411     O*
192700081014     O          E            DETERR      0  0
192800031219     O                    n13                   155 'NON SPUNTATO IN DATA '
192900031219     O                    n13STADFV        8   +  1
193000950316     O*
193100040621     O                     13brvFGS             137
193200040621     O                     13                   138 '-'
193300040621     O                     13ST2FGS             146
193400161025     O                     13                   149 'in'
193500070118     O                     13ST2CAT             153
193600070118     O                     13brvNPG             155
193700070118     O                     13                   156 '-'
193800070118     O                     13brvNFV        Z    162
193900161025     O                     13                   165 'il'
194000070118     O                     13ST2DFV             171 '  /  '
194100161026     O**                   13                   175 'ore'
194200161026     O                     13staHMS             178 '  :  '
194300161130     O                       STADSF             198
194400070418     O          E            DETndc      1
194500070418     O                                           87 'In distinta:'
194600070418     O                       ARBndc        2  +   1
194700070418     O                                        +   2 'del'
194800070418     O                       staddc            +  1 '  /  /    '
194900081014     o* Collo doppio
195000081014     O          E            DETdoppio   0  0
195100161018     O                                          153 'COLLO   D O P P I O'
195200161018     o* Collo eccedente
195300161018     O          E            DETeccede   0  0
195400161018     O                                          153 'COLLO   E C C E D E'
195500161018     O                                          159 ' N T E'
195600161018     O                       ecce_descr         185
195700100706     o* Collo presente da XX giorni in IMA
195800100706     O          E            DETspuIMA   0  0
195900100707     O                                          160 '   COLLO SPUNTATO PER '
196000100707     O                                        +   1 'ALMENO '
196100100706     O                       §VPOGGIMA     2  +   1
196200100707     O                                        +   1 'GIORNI in I.M.A.'
196300081014     o* riga vuota
196400081014     O          E            DETvuota    2
196500081014     O                                          157 ' '
196600050405     O* STAMPO NUMERO C.A.e ubicazione collo
196700990212     O          E            DETCA       1
196800050405     O               24                          87 'Ubicazione collo:'
196900050405     O               24      §ar5zng           +  2
197000050405     O               25                         138 'C.A.'
197100050405     O               25      DCTFIL            +  1
197200050405     O               25      DCTNCA        Z   +  1
197300050405     O               25                        +  1 'del'
197400050405     O               25      STADCA            +  1 '  /  /  '
197500990212     O                     26                  +  1 'chiusa il'
197600990212     O                     26STACHC            +  1 '  /  /  '
197700990303     O                     26DCTCCH            +  1
197800990303     O                     26                  +  0 '-'
197900990303     O                     26STACCH            +  0
198000920813     O          E            OK          3
198100920813     O                       STA(11)             66
198200920813     O          E            OK          0  0
198300920813     O                       STA(11)             66
198400920813     O*
198500920813     O          E            TOT1        3  1
198600920813     O                       STA(13)             66
198700920813     O          E            TOT2           1
198800920813     O                       NPS(C)              33
198900920813     O                       CPS(C)        2     58
199000920813     O          E            TOT3           1
199100920813     O                       STA(14)             66
199200920813     O          E            TOT3           1
199300920813     O                       STA(15)             66
199400920813     O                       TOTSPU        2     58
199500920813     O*
199600920813     O          E            TOT3        3
199700920813     O                       STA(12)             66
199800920813     O          E            TOT3        0  0
199900920813     O                       STA(12)             66
200000941017     O**************************************************************************
200100950316** STA
200200060508XXXXXXXXXXXXXXXXXXXX                                                 1
200300950320 ** I.M.A. : INVENTARIO MAGAZZINO ARRIVI **                          2
200400161018FOGLIO        di  XXX-XXXXXXXXXXXXXXXXXXXX    N.ro XXXXX   del XX/   3
200500931228                                       *** D E F I N I T I V O ***   4
200600040603    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO        5
200700941019ANNO LNP SER NUMERO  DATA ARR                                        6
200800040603XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx  7
200900040603     SEGNACOLLI    ZON ULT  AUTOTRASPORT.   A R R I V O SEGNACOLLO   8
201000040603      DAL /   AL   ARR CONS   CONSEGNA       DATA   PIST MANCANTE    9
201100040603XX XXXXXXX/XXXXXXX XXV X XXXXXXX-XXXXXXXXX XX/XX/XXXX XX  XXXXXXX   10
201200920813             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
201300920813 **  FINE  STAMPA  **
201400920813   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI      13
201500920813                        ---------------------------------------     14
201600920813               TOTALE                                               15
201700060508XX/XXXX           Punto Operativo in Gestione: XXX-xxxxxxxxxxxxxxx  16
201800950320                         FNLR31R                          PAG.XXXX  17
201900950810 ** I.M.G. : INVENTARIO MAGAZZINO GIACENZE **                       18
202000040621 S  P  U  N  T  A  T  O      D  A                                   19
202100060508xxxxx           Linea Arrivo Selezionata: XXX-xxxxxxxxxxxxxxxxxxxx  20
202200070418                Elabora anche spedizioni in distinta: XX            21
202300161031 ** I.P.G. : QUADRATURA COLLI A MAGAZZINO GIACENZE **               22
202400161017**
202500950316OVRPRTF FILE(PRTF198) USRDTA('I.M.A.')
202600161017**
202700161017DLYJOB DLY(01)
