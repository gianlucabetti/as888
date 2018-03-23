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
015500170125     d winGiac         S              1
015600090522     d uno             S              1
015700081014     d statipo         S             21
015800160503     d statipo2        S             21
015900101215     d staimaa         S             44
016000070418     d stamdc          S              2
016100160503     d indMP           S              4  0
016200160503     d ind12           S              4  0
016300050511     d WSPG            S                   LIKE(FVVSPG)
016400040108     d kpdpdr          S                   LIKE(APDpdr)
016500031219     d wdfv            S                   LIKE(d53dfv)
016600031219     d kpdtip          S                   LIKE(APDtip)
016700040603     d ktrd            S                   LIKE(ar5trd) inz('GEN')
016800161018     d ecce_descr      S             25
016900100706     d dataEUR         s               d   datfmt(*eur)
017000100706     d DataISO         s               d   datfmt(*iso)
017100101202     d*
017200040428     D msgjob          C                   CONST('La Manut.Bolle sta bloccando -
017300040428     D                                     l''elabor. Dell''IMA:USCIRE SUBITO D-
017400040428     D                                     A QUESTO LAVORO')
017500050511     D CPARCEL         C                   CONST('     P A R C E L     ')
017600050511     D CMessag         C                   CONST('M E S S A G G E R I E')
017700160503     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
017800160503     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
017900101215     D CsoloLNA        C                   CONST('Solo colli spuntati alla line-
018000101215     d                                     a arrivo finale')
018100911018     C*--------------------------------------------------------------- *
018200911018     C*                  INDICATORI                                    *
018300911018     C*--------------------------------------------------------------- *
018400911018     C* 01    - ESITONO ANOMALIE DI MANCANZA
018500950810     C* 02    - CHIESTA CHIUSURA  FOGLIO
018600911018     C* 03    - ESISTE SERIE
018700911018     C* 04    - SEGNACOLLI NON SEQUENZIALI
018800911018     C* 05    - ON CAMBIATA SPEDIZIONE
018900920325     C* 06    - NON C'E' COD PADRONCINO CONSEGNA
019000920708     C* 07    - MANCA DATA ARRIVO : SPEDIZIONE FORZATA
019100920813     C* 08    - BOLLA GESTITA
019200170124     C* 11    - e 10 ON --> IPG  ma ora eliminato
019300950810     C* 10    - ON --> IMG  OFF --> IMA
019400081014     C* 12    - CONTROLLO COLLI DOPPI
019500950810     C* 13    - NON TROVATA SPUNTA IN IMA MA  IN ALTRO FOGLIO NELLA
019600950810     C*         STESSA DATA
019700950810     C* 14    - NON TROVATA SPUNTA IN IMA
019800031219     C* 15    - TROVATA SPUNTA SU ALTRO FOGLIO MA DEVO CREARE L'ANOMAL
019900100706     C* 16    - collo in IMA ma spuntato in IMA più di XX volte...da tabella VPO
020000161018     C* 18    - collo eccedente
020100170124     C**** 20    - FILIALE partita con procedura 2 gg in IMA
020200170124     C* 22    - FILIALE partita con nuova quadratura IMA (e non più IPG)
020300050405     C* 24    - STAMPA ubicazione collo se c'e'
020400050405     C* 25    - STAMPA se c'e' c.a.
020500050405     C* 26    - STAMPARE DATA CHIUSURA C.A.
020600941017     C* 30    - LETTURA FNARB
020700941017     C* 31    - LETTURA FNART
020800911018     C* 32/34 - COMODO
020900911018     C*---------------------------------------------------------------*
021000900622     C     *ENTRY        PLIST
021100900622     C                   PARM                    KPJBA
021200941017     C                   MOVEL     KPJBU         PARAM1
021300000126     C                   Z-ADD     COMNFV        WNFV
021400900828     C*-----------------------------------------------------
021500950810     C* IMPOSTAZIONI INIZIALI
021600950810     C                   EXSR      IMPO
021700950810     C*-----------------------------------------------------
021800170124     c                   setoff                                       11
021900161018     c* il dly mi serve solo per il debug in batch
022000161017     C                   Z-ADD     80            LUNG             15 5
022100161017     C                   MOVEL     *BLANKS       COMMAN           80
022200161017     C                   MOVEA     VIDTIN        CMD(35)
022300161017     C                   MOVEA     CMDd          COMMAN
022400161017     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
022500161017     C                   PARM                    COMMAN
022600161017     C                   PARM                    LUNG
022700161017
022800941019     C                   Z-ADD     80            LUNG             15 5
022900941019     C                   MOVEL     *BLANKS       COMMAN           80
023000950810     C                   MOVEA     VIDTIN        CMD(35)
023100941019     C                   MOVEA     CMD           COMMAN
023200170124     c**                 if        *in11
023300170124     c**                 eval      %subst(comman:31:6)='I.P.G.'
023400170124     c**                 endif
023500941019     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
023600941019     C                   PARM                    COMMAN
023700941019     C                   PARM                    LUNG
023800950810     C*
023900950316RM*  C                   OPEN      PRTF198
024000980128     C                   SETON                                        OF
024100911015     C*
024200040108     C* CONSIDERO SOLO i p.o. di £6
024300941019     C                   Z-ADD     1             WA                3 0          CONTATORE
024400040108    1C     L6(WA)        DOWNE     *ZERO
024500941019     C*
024600040108     C                   Z-ADD     L6(WA)        KLNA                           LINEA ARRIVO
024700161017     C*
024800031218     C     KLNA          SETLL     FNARB05L
024900040427     C     KLNA          READE     FNARB05L                               30
025000950810     C*
025100950810    3C     *IN30         DOWEQ     *OFF
025200050511     c* Se ima messaggerie o parcel, escludo le zone non di interesse
025300160503     c                   eval      indMP=0
025400160503     c                   eval      ind12=0
025500160503     c
025600160503     c                   if        wselzona='S'
025700160503     c                   movel     arbznc        alfzona           2
025800160503
025900160503     c                   if        wparc='S' or wmess='S'
026000160503     c***  alfzona       lookup    zone                                   35
026100160503     c                   eval      indMP =%lookup(alfzona:zone)
026200050511     c                   endif
026300160503     c                   if        wparc1='S' or wparc2='S'
026400160503     c                   eval      ind12 =%lookup(alfzona:zone12)
026500160503     c                   endif
026600160503
026700160503     c                   endif
026800160503   3ac                   if        wselzona=' ' or  indmp>0 or  ind12>0
026900920813     C*
027000920318     C* CHAIN CODICE BOLLE PER ESCLUDERE I RECUPERI
027100920318     C                   MOVEL     '3A'          COD
027200950810     C                   MOVEL(P)  ARBCBO        KEY
027300941018     C     KTAB          CHAIN     TABEL00F                           35
027400920318     C  N35              MOVEL     TBLUNI        DS3A
027500920318     C   35              MOVEL     'F1'          §3ATB1
027600920318     C*
027700950810     C* ESCLUDO:  LE BLOCCATE CON "B"/
027800930518     C*           QUELLE IN CONSEGNA / QUELLE CONSEGNATE / I RECUPERI
027900161025    4C     ARBdcm        ifeq      *zeros
028000950810     C     §3ARBL        ANDNE     'R'
028100950810     C     ARBFBC        ANDNE     'B'
028200000512     C*
028300960516     C* CONTROLLO SE E' GIACENTE
028400960516     C                   CLEAR                   WGIAC
028500960516     C     ARBFBC        IFEQ      'G'
028600161025     C     KGCA          CHAIN(n)  Tigcp21L                           35
028700970721     C* SE C'E' LA GIACENZA E NON E' A FASE 35 LA ESCLUDO
028800970721     C  N35GCPFAS        IFLT      35
028900050331     C     GCPFAS        oreq      36
029000170124     c* la merce è in giacenza : verifico con driver
029100160927     c                   clear                   fnlgi1ds
029200160927     c                   eval      ILGI1AAS=arbaas
029300160927     c                   eval      ILGI1lnp=arblnp
029400160927     c                   eval      ILGI1nrs=arbnrs
029500160927     c                   eval      ILGI1nsp=arbnsp
029600161018    3c                   select
029700161018     c                   when      arbpkc>0 and arbncp=arbncl
029800161018     c                   eval      ilgi1pkf=arbpkc
029900161018     c                   when      arbpkc>0 and arbncp<arbncl
030000161018     c                             and arbpkc>arbpkf
030100161018     c                   eval      ilgi1pkf=arbpkc
030200161018     c                   other
030300161018     c                   eval      ilgi1pkf=arbpkf
030400161018    3c                   endsl
030500160927     c                   movel     fnlgi1ds      kpjbu
030600160927     c                   call      'FNLGI1R'
030700160927     c                   parm                    kpjba
030800160927     c                   parm                    tigcp00ds
030900161007     c                   movel     kpjbu         fnlgi1ds
031000160927
031100170125     c* per IMG: sped portata nel giorno a mag. giac DEVE essere in giacenza
031200170125     c*          e quindi elaborata con mancanza
031300170125   5ac   10              if        olgi1AMG='Y' or olgi1inmg='S'
031400970721     C                   MOVEL     'S'           WGIAC             1
031500160823     c                   endif
031600170125   5ac   10              if        olgi1AMG='S'
031700170125     C                   MOVEL     'D'           WGIAC             1
031800170125     c                   endif
031900170125     c* per IMA: devo elaborare sped da portare a mag giac o portate nel giorno
032000161024     c* se la bolla è da portare a mag giacenza OGGI, devo considerarla anche in IMA
032100161024     c*  e se manca dare segnalazione ma NON creando l'anomalia
032200170125   5ac  n10              if        olgi1AMG='S' or olgi1AMG='Y'
032300161024     C                   MOVEL     'D'           WGIAC             1
032400161024     c                   endif
032500170125   5ac  n10              if        olgi1inmg='S'
032600170125     C                   MOVEL     'S'           WGIAC             1
032700170125     c                   endif
032800161201     c*
032900170124     c* se non è partito con NEW IMA considero  la spedizione GIACENTE  come prima
033000170201   5ac*****              if        not *in22 and olgi1AMG='S'
033100170201   5ac                   if        not *in22 and WGIAC='D'
033200161201     C                   MOVEL     'S'           WGIAC             1
033300161201     c                   endif
033400161201     c
033500170124     c* IPG elaboro le giacenze da portare a mag giac OGGI ( non facciamo più)
033600170124   5ac** 11              if        olgi1AMG='S' or olgi1AMG='Y'
033700170124     C**                 MOVEL     'D'           WGIAC             1
033800170124     c**                 endif
033900160823     c
034000960516     C                   ENDIF
034100970721     C                   ENDIF
034200950810     C*
034300161025     C* SE IMA --> ESCLUDO LE GIACENTI ma includo le bolle che devono essere portate a mag giac
034400170124     c*            se partita la procedura NEWIMA
034500950810    5C     *IN10         IFEQ      *OFF
034600960516     C     WGIAC         ANDNE     'S'
034700170124     c     *in22         andeq     *on
034800170124     c***  *in20         andeq     *on
034900161130
035000161130    5C     *IN10         orEQ      *OFF
035100161130     C     WGIAC         ANDeq     ' '
035200170124     c     *in22         andeq     *off
035300170124     c**** *in20         andeq     *off
035400950810     C*
035500950810     C* SE IMG --> INCLUDO SOLO LE GIACENTI
035600161026     C*        --> INCLUDO anche le bolle da portare a mag.giac. ma senza stampare il mancante
035700950810     C     *IN10         OREQ      *ON
035800161025     C     *IN11         andeq     *Off
035900161026     C     WGIAC         ANDne     ' '
036000161025
036100170124     C* SE IPG --> INCLUDO SOLO quelle da portare a mag giac ( eliminato)
036200170124     C**   *IN10         OREQ      *ON
036300170124     C**   *IN11         andeq     *ON
036400170124     C**   WGIAC         ANDEQ     'D'
036500921013     C*
036600950317     C* ESCLUDO  SE BOLLA MAMMA E UNA DELLE FIGLIE E' IN CONSEGNA
036700960516     C*     O SE SONO TUTTE BLOCCATE
036800950810     C                   Z-ADD     ARBSPE        SAVSPE
036900960516     C*
037000960516     C                   CLEAR                   WESFI                          ESISTE FIGL
037100960516     C                   MOVEL     'S'           WFIBLO            1            FIGLIE BLOCC
037200950317     C                   SETOFF                                       36
037300950810     C     KSAV          SETLL     FNLBL02L
037400950810     C     KSAV          READE     FNLBL02L                               35
037500960516     C**
037600950810    6C     *IN35         DOWEQ     *OFF
037700950317     C*
037800950317     C     KLBL          CHAIN     FNARB01L                           35
037900950810    7C     *IN35         IFEQ      *OFF
038000960516     C                   MOVEL     'S'           WESFI             1
038100960516     C* FIGLIA IN CONSEGNA
038200960516    8C     ARBNDC        IFNE      0
038300950317     C                   SETON                                        3536
038400960516   X8C                   ELSE
038500960516     C*
038600960516     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
038700960516    9C     ARBFBC        IFNE      'B'
038800960516     C                   MOVEL     'N'           WFIBLO
038900960516    9C                   ENDIF
039000960516    8C                   ENDIF
039100960516    7C                   ENDIF
039200950317     C*
039300950810     C  N36KSAV          READE     FNLBL02L                               35
039400950810    6C                   ENDDO
039500960516     C*
039600960516     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
039700960516     C  N36WESFI         IFEQ      'S'
039800960516     C     WFIBLO        ANDEQ     'S'
039900960516     C                   SETON                                        36
040000960516     C                   ENDIF
040100070418     c
040200070418     c* Per 36 off - controllo in IMA se richiesti anche i colli mancanti
040300070418     c*              di spedizioni in distinta
040400070418     c  n36              if        not *in10 and vidmdc='N' and arbndc>0
040500070418     c                   seton                                        36
040600070418     c                   endif
040700911204     C*
040800950810    6C  N36              DO
040900950317     C*
041000040427     C     KSAV          CHAIN     FNARB01L                           31
041100950810     C                   MOVEL     ARBAMA        SAVAMA            1 0
041200040428     c                   clear                   wchain            1
041300161025     c                   clear                   wesspu            1
041400911018     C* LEGGO DETTAGLIO
041500941017     C     KART          SETLL     FNART01L
041600040428     C  N10
041700040428     CAN 02KART          READE     FNART01L                               31
041800040428     C   10
041900040428     CORN02KART          READE(N)  FNART01L                               31
042000911018     C*
042100950810    7C     *IN31         DOWEQ     *OFF
042200911204     C                   SETOFF                                       34
042300950810     C*
042400950810     C* ESCLUDO  SE IL COLLO E' CONSEGNATO (CONSEGNA PARZIALE)
042500070418     C     ARTDCM        IFEQ      0
042600950810     C  N10              EXSR      ELAIMA
042700950810     C   10              EXSR      ELAIMG
042800950810    8C                   ENDIF
042900930629     C*
043000040428     C  N10
043100040428     CAN 02KART          READE     FNART01L                               31
043200040428     C   10
043300040428     CORN02KART          READE(N)  FNART01L                               31
043400950810    7C                   ENDDO
043500170124     c* per IMA  e IMG aggiorno la giacenza con la data entrata a mag giac se spuntato
043600170124     c*       (IPG viene eliminato quindi uso ima al suo posto)
043700170125     c*  almeno un collo e data = 0  solo in fase "definitivo"
043800170125     c*
043900170124    7c*****              if        *in22 and *in10 and wesspu='S' and
044000170125    7c                   if        *in22 and *in02 and wesspu='S' and
044100161025     C                             olgi1amg='S'
044200161025     C     KGCA          CHAIN     Tigcp21L
044300161025    8c                   if        %found(tigcp21l)
044400161025     C                   MOVE      dateu         GCPDLMa
044500161025     C                   EXCEPT    AGG5
044600161028     c
044700161025    8C                   ENDIF
044800161025    7c                   endif
044900920318     C*
045000950810    6C                   ENDDO
045100950810    5C                   ENDIF
045200161025     c
045300000512    4C                   ENDIF
045400050511   3aC                   ENDIF
045500911018     C*
045600040427     C     KLNA          READE     FNARB05L                               30
045700950810    3C                   ENDDO
045800941019     C*
045900040108     C                   ADD       1             WA                             CONTATORE L6
046000950810    1C                   ENDDO
046100161017     c
046200920813     C*
046300920813     C* RIEPILOGO SPUNTE PER PISTOLA
046400920813     C                   EXSR      RIEPIL
046500911018     C*
046600060508     C* SE NON TROVATE DISCORDANZE e non fatte parzializzazioni
046700060508     c*  se non ho parzii
046800070418     c* 18/04/07 - IMA chiude solo se richiesta forzatura chiusura
046900960530     C     *IN01         IFEQ      *OFF
047000070418     C     *IN10         andeq     *ON
047100161017     C     *IN11         andeq     *Off
047200060508     c
047300160503     c     wselzona      andeq     ' '
047400060508     c
047500060508     c     vidlna        andeq     0
047600060508     c
047700060508     C*  O FORZATA LA CHIUSURA METTO FLAG
047800911018     C     VIDSN         OREQ      'S'
047900060508     c
048000941017     C     KFVV          CHAIN     FNFVV01L                           32
048100920813     C  N32              MOVEL     'S'           FVVFCF
048200941017     C  N32              UPDATE    FNFVV000
048300911018     C                   END
048400050511     C*
048500081014     C* CHIUSURA PGM
048600990212     C                   CLEAR                   DSBS02
048700990212     C                   MOVEL     'C'           T02TLA
048800990212     C                   CALL      'TIBS02R'
048900990212     C                   PARM                    KPJBA
049000990212     C                   PARM                    DSBS02
049100031219     c
049200031219     C                   CLEAR                   DSLV53
049300031219     C                   MOVEL     'C'           D53TLA
049400031219     C                   CALL      'FNLV53R'
049500031219     C                   PARM                    DSLV53
049600040806     C*
049700040806     C                   MOVEL     'C'           I12TLA
049800040806     C                   CALL      'FIDN12R'
049900040806     C                   PARM                    FIDN12DS
050000140430     C*
050100140430     c                   clear                   fnlrv7ds
050200140430     C                   MOVEL     'C'           ILRV7TLA
050300140430     c                   eval      kpjbu= fnlrv7ds
050400140430     C                   CALL      'FNLRV7R'
050500140430     C                   PARM                    KPJBA
050600160927
050700160927     c                   clear                   fnlgi1ds
050800160927     c                   eval      ILGI1TLA= 'C'
050900160927     c                   movel     fnlgi1ds      kpjbu
051000160927     c                   call      'FNLGI1R'
051100160927     c                   parm                    kpjba
051200160927
051300040806      *
051400000000     C                   SETON                                        LR
051500031219     C**************************************************************************
051600031219     C* CONTROLLO SE SEGNACOLLO MANCANTE
051700031219     C**************************************************************************
051800031219     C     CTRART        BEGSR
051900031219     C*
052000031219     C                   SETOFF                                       131415
052100100706     C                   SETOFF                                       3316
052200161025     C                   MOVE      *BLANKS       STADSF           17
052300031219     C*
052400070118     C     KBRV1         SETLL     FNBRV01L                               33
052500070418     c
052600161017     c* se non ho trovato spunta IMx e spedizione in distinta, cerco
052700070418     c*  la categoria 4 del giorno
052800070418    1c                   if        not *in33 and arbndc>0 and vidmdc='S'
052900100706     C                   Z-ADD     4             conNPG
053000140618     C     KBRV4         chain     FNBRV05L
053100140618    2c                   if        %found(fnbrv05l)
053200070418     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
053300070418     c                   clear                   dslv53
053400070418     C                   z-add     brvnFV        D53NFV
053500070418     C                   MOVEL     brvNPG        D53NPG
053600070418     C                   MOVEL     brvFGS        D53FGS
053700070418     C                   CALL      'FNLV53R'
053800070418     C                   PARM                    DSLV53
053900070418     C* ERRORE
054000070418     c*  se sparato nella stessa data: ok
054100070418     c*  e sparato in altra data, rientro nei giri normali di controllo
054200070418     c*  spunte
054300070418    3c                   if        d53err=' ' and d53dfv=savdfv
054400070418     c                   seton                                        33
054500070418    3c                   endif
054600070418    2c                   endif
054700070418    1c                   endif
054800070418     c
054900031219     C* 14 ON - SPUNTA NON TROVATA
055000031219    1C     *IN33         IFEQ      *OFF
055100070418     c
055200031219     C                   SETON                                        14
055300031219     c*
055400031219     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
055500031219     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
055600031219     c*  immissione + alta
055700031219     C                   exsr      RICSPU
055800031219     C
055900031219     C* SE SPUNTA TROVATA CON DATA > STAMPO MA NON CREO ANOMALIA
056000031219     C                   IF        WDFV>SAVDFV
056100031219     C                   EVAL      *IN13=*ON
056200031219     C                   ENDIF
056300031219     C
056400031219     C* SE SPUNTA TROVATA CON DATA = STAMPO E CREO ANOMALIA SOLTANTO SE
056500031219     C*  CATEGORIA 2 O 6
056600031219     C                   IF        WDFV=SAVDFV
056700031219     C                   EVAL      *IN13=*ON
056800101202     C                   IF        BRVNPG=2
056900031219     C                   EVAL      *IN15=*ON
057000031219     C                   ENDIF
057100031219     C                   ENDIF
057200031219     c*
057300031219    1C                   END
057400960523     C*
057500031219     C
057600031219    1C                   IF        WDFV>=SAVDFV
057700960523     C* GIRO LA DATA
057800031219     C                   MOVE      WDFV          G02INV
057900960523     C                   MOVE      *ZEROS        G02DAT
058000960523     C                   MOVEL     3             G02ERR
058100960523     C                   CALL      'XSRDA8'
058200960523     C                   PARM                    WLBDAT
058300960523     C                   MOVEL     G02DAT        ST2DFV            4 0
058400960523     C**
058500960523     C* DECODIFICO LA FILIALE GESTIONE
058600031219     C     BRVFGS        CHAIN     AZORG01L                           32
058700031219    2C  N32ORGDE5        IFNE      *BLANKS
058800960523     C                   MOVEL     ORGDE5        ST2FGS            8
058900960523     C                   ELSE
059000960523     C                   MOVEL     ORGDES        ST2FGS            8
059100031219    2C                   ENDIF
059200960523     C**
059300960523     C* DECODIFICO LA CATEGORIA SPUNTA
059400031219    2C     BRVNPG        IFEQ      3
059500161017     C                   MOVEL     '7B'          COD
059600161017     C                   MOVEL(P)  wspg          KEY
059700161017     C     KTAB          CHAIN     TABEL                              32
059800161017     C  N32              MOVEL     TBLUNI        ds7b
059900161017     C   32              CLEAR                   ds7b
060000161017     C                   MOVEL     §7bdesb       ST2CAT            3
060100031219   X2C                   ELSE
060200960523     C                   MOVEL     '7N'          COD
060300031219     C                   MOVEL(P)  BRVNPG        KEY
060400960523     C     KTAB          CHAIN     TABEL                              32
060500960523     C  N32              MOVEL     TBLUNI        ST2CAT
060600960523     C   32              CLEAR                   ST2CAT
060700031219    2C                   ENDIF
060800031219     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
060900031219    2C                   IF        BRVNPG=4
061000031219     C                   Z-ADD     BRVNFV        WNFV5
061100031219     C     KFVV2         CHAIN(N)  FNFVV000                           32
061200161026     C  N32              eval      stadsf=fvvdsf
061300031219    2C                   ENDIF
061400031219    1C                   ENDIF
061500960523     C*
061600100706     c* Se trovata spunta, segnalo se sparata xx volte in IMA
061700100706     c*  solo per bolle in IMA più vecchie di ....
061800100707     c* Escludo bolle in distinta
061900150113     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
062000100707     c                   if        not *in14 and not *in10
062100100706     c                             and arbndc=0 and arbdsp<datapresIMA
062200100706     c
062300100706     c* conto quante volte presente in ima
062400100706     C                   Z-ADD     3             conNPG
062500100706     c                   clear                   conta             4 0
062600140618     C     KBRV4         setll     FNBRV05L
062700140618     C     KBRV4         reade     FNBRV05L
062800100706     c
062900140618    2c                   dow       not %eof(fnbrv05l)
063000100706     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
063100100706     c                   clear                   dslv53
063200100706     C                   z-add     brvnFV        D53NFV
063300100706     C                   MOVEL     brvNPG        D53NPG
063400100706     C                   MOVEL     brvFGS        D53FGS
063500100706     C                   CALL      'FNLV53R'
063600100706     C                   PARM                    DSLV53
063700100706     c
063800100706     c                   if        d53spg='A'
063900100706     c                   eval      conta=conta+1
064000100706     c                   endif
064100100706     c
064200140618     C     KBRV4         reade     FNBRV05L
064300100706     c                   enddo
064400100706     c
064500100706     c                   if        conta>=§vpoggima
064600100706     c                   seton                                        16
064700100706     c                   endif
064800100706     c
064900100706     c
065000100706     c                   endif
065100100706     c
065200960523     C                   ENDSR
065300960523     C*
065400031219     C**************************************************************************
065500031219     C* cerco spunta del collo in qualsiasi categoria
065600031219     C**************************************************************************
065700031219     C     RICSPU        BEGSR
065800031219     C                   CLEAR                   Wimm
065900031219     C                   CLEAR                   WSAVB
066000031219     C                   CLEAR                   WDFV
066100031219     C                   CLEAR                   WTROV
066200040108     C                   CLEAR                   WSPG
066300070118RM*  C     Kartd         CHAIN     FNBRV07l                           33
066400031219RM*  C*
066500031219RM* 1C     *IN33         DOWEQ     *OFF
066600031219     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
066700031219     c                   clear                   dslv53
066800031219     C                   z-add     brvnFV        D53NFV
066900031219     C                   MOVEL     brvNPG        D53NPG
067000031219     C                   MOVEL     brvFGS        D53FGS
067100070118     C****               MOVEL     brvfle        D53flf
067200031219     C                   CALL      'FNLV53R'
067300031219     C                   PARM                    DSLV53
067400031219     C* ERRORE
067500031219    2C     D53ERR        IFeq      ' '
067600070118     c                   movel     brvdfs        brvimm           14 0
067700031219     c                   move      brvhms        brvimm
067800031219    3c                   select
067900031219     c* Se maggiore memorizzo
068000031219     c                   when      d53dfv>wdfv
068100031219     C                   MOVEL     RECBRV        WSAVB
068200070118     C                   MOVEL     brvimm        Wimm             14 0
068300031219     C                   MOVEL     d53DFV        WDFV
068400040108     C                   MOVEL     d53spg        WSPG
068500031219     C                   MOVEL     'S'           WTROV             1
068600031219     c
068700031219     c* Se uguale tengo la data ora immissione + alta
068800031219     c                   when      d53dfv=wdfv  and brvimm>wimm
068900070118     C                   MOVEL     brvimm        Wimm             14 0
069000031219     C                   MOVEL     RECBRV        WSAVB
069100031219     C                   MOVEL     d53DFV        WDFV
069200040108     C                   MOVEL     d53spg        WSPG
069300031219     C                   MOVEL     'S'           WTROV             1
069400031219     C                   eval      wimm=brvimm
069500040129     c* Se stessa data e ora immissione, tengo cat 3 o 4
069600101202     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
069700070118     C                   MOVEL     brvimm        Wimm             14 0
069800040129     C                   MOVEL     RECBRV        WSAVB
069900040129     C                   MOVEL     d53DFV        WDFV
070000040129     C                   MOVEL     d53spg        WSPG
070100040129     C                   MOVEL     'S'           WTROV             1
070200040129     C                   eval      wimm=brvimm
070300031219    3c                   endsl
070400031219    2c                   endif
070500031219     c
070600070118RM*  C     Kartd         reade     FNBRV07l                               33
070700031219    1c                   enddo
070800031219     c
070900031219     C     WTROV         IFEQ      'S'
071000031219     C                   MOVEL     WSAVB         RECBRV
071100031219     C                   ENDIF
071200031219     c                   endsr
071300941017     C**************************************************************************
071400941017     C* IMPOSTO CAMPI FISSI
071500941017     C**************************************************************************
071600911018     C     IMPO          BEGSR
071700921022     C*
071800101202     C                   Z-ADD     1             CODUT             1 0
071900921022     C                   Z-ADD     3             IMANPG
072000950810     C                   CLEAR                   SAVSPE
072100950810     C                   CLEAR                   COMSPE
072200921022     C                   TIME                    UTIME             6 0
072300960927     C                   TIME                    W0140            14 0
072400960927     C                   MOVE      W0140         UDATE8            8 0
072500101202      *
072600101202      * Reperimento dati utente/aziendali
072700101202     c                   exsr      sr_DatiJob
072800161130     c
072900161130     c* per ora solo le filiali abilitate all'attesa in IMA hanno ima e img modificato
073000170124     c**                 clear                   trulvpods
073100170124     c**                 eval      ivpoke1 = 'DECOFIIMG'
073200170124     c**                 call      'TRULVPOR'
073300170124     c**                 parm                    trulvpods
073400161130
073500170124     c**                 if         %lookup('999':skFilOk)>0    or
073600170124     c**                            %lookup(%editc(vidfil:'X'):skFilOk)>0
073700170124     c**                 seton                                        20
073800170124     c**                 endif
073900161130     c
074000170124     c* Solo se filiale partita con NUOVO IMA devo aggiornare la data entrata mag giac
074100170124     c* ima e IMG modificati sempre per NEW IMA e non per i 2 gg in ima
074200161130     c                   clear                   trulvpods
074300170124     c                   eval      ivpoke1 = 'DECOFIIPG'
074400161130     c                   call      'TRULVPOR'
074500161130     c                   parm                    trulvpods
074600161130
074700161130     c                   if         %lookup('999':skFilOk)>0    or
074800161130     c                              %lookup(%editc(vidfil:'X'):skFilOk)>0
074900161130     c                   seton                                        22
075000161130     c                   endif
075100161130     c
075200170124     C* 11 e 10 ON - IPG (eliminato)
075300170124     c                   select
075400170124     c***                when      vidtin='Z'
075500170124     C***                SETON                                        1011
075600161017     C* 10 ON - IMG
075700161017     C                   when      vidtin='G'
075800950810     C                   SETON                                        10
075900161014     C                   ENDsl
076000921022     C*
076100921022     C* CHAIN E DATA DEL FOGLIO IMA
076200941017     C     KFVV          CHAIN(N)  FNFVV01L                           32
076300921022     C*
076400941018     C                   Z-ADD     FVVDFV        G02INV                         AAAA/MM/GG
076500941018     C                   Z-ADD     *ZERO         G02DAT
076600941018     C                   MOVEL     '3'           G02ERR
076700941018     C                   CALL      'XSRDA8'
076800941018     C                   PARM                    WLBDAT
076900941018     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
077000950316     C*
077100950317RM*  C                   Z-ADD     FVVDFV        SAVDFV
077200950317RM*  C                   Z-ADD     FVVFGS        SAVFGS
077300931228     C*
077400941028     C* DECODIFICA FILIALE DI APERTURA FOGLIO
077500941018     C     FVVFGS        CHAIN     AZORG                              32
077600931228     C  N32              MOVEL     ORGDES        DESCDU           20
077700000512     C*
077800000512     C                   Z-ADD     15            ANMCAA
077900000512     C                   MOVEL     '7C'          COD
078000000512     C                   MOVEL(P)  ANMCAA        KEY
078100000512     C     KTAB          CHAIN     TABEL00F                           35
078200000512     C  N35              MOVEL     TBLUNI        DS7C
078300000512     C   35              MOVEL     *BLANKS       DS7C
078400911018     C*
078500931228     C* 02 ON -  D E F I N I T I V O
078600911018     C     VIDSN         IFEQ      'S'
078700911018     C                   SETON                                        02
078800911018     C*
078900921022     C* PRENDO FASE ANOMALIA
079000950810     C                   CLEAR                   PARAM5
079100950810     C                   MOVEL     'L'           PA5TLA
079200950810     C                   MOVEL     3             PA5NPG
079300950810     C                   MOVEL     VIDTIN        PA5SPG
079400950810     C                   CALL      'TRUL11R'
079500950810     C                   PARM                    PARAM5
079600920409     C                   END
079700920813     C*
079800920813     C* DECODIFICA FILIALE IN GESTIONE
079900920813     C     VIDFIL        CHAIN     AZORG                              32
080000920813     C  N32              MOVEL     ORGDES        DESFIL           20
080100941019     C*
080200101215     C* CARICO L6
080300101215     C                   CLEAR                   DSUL06
080400101215     C                   MOVE      '£6'          D06COD
080500101215     C                   MOVEL     VIDFIL        D06KEY
080600101215     C                   MOVEL     'L'           D06TLA
080700101215     C                   MOVEL     DSUL06        KPJBU
080800101215     C                   CALL      'TRUL06R'
080900101215     C                   PARM                    KPJBA
081000101215     C                   MOVEL     KPJBU         DSUL06
081100101215     C                   MOVEA     LIN           Lspu
081200060508     c*
081300101215     C*  Se non parzializzata la linea di arrivo, elaboro L6
081400161014     c*  altrimenti elaboro solo lei
081500060508     c                   if        vidlna=0
081600101215     C                   MOVEA     LIN           L6
081700060508     c                   else
081800060508     c                   clear                   L6
081900060508     c                   movel     vidlna        l6(1)
082000060508     c
082100060508     c* Decodifica
082200060508     C     VIDlna        CHAIN     AZORG                              32
082300060508     C  N32              MOVEL     ORGDES        DESlna           20
082400060508     c                   movel     vidlna        stalna            3
082500060508     c                   endif
082600070418     c
082700070418     c* Elabora anche spedizioni in distinta
082800070418     c                   if        vidmdc='S'
082900070418     c                   eval      stamdc='SI'
083000070418     c                   else
083100070418     c                   eval      stamdc='NO'
083200070418     c                   endif
083300101215     c* solo colli spuntati alla linea di arrivo finale
083400101215     c                   if        parimaa='S'
083500101215     c                   eval      staimaa=csololna
083600101215     c                   endif
083700160503     c* Verifico le richieste delle zone di consegna
083800160503     c                   clear                   wParc             1
083900160503     c                   clear                   wParc1            1
084000160503     c                   clear                   wParc2            1
084100160503     c                   clear                   wMess             1
084200160503     c                   clear                   wSelzona          1
084300160503     c                   if        partpm1='P ' or partpm2='P '
084400160503     c                   eval      wparc='S'
084500160503     c                   eval      wselzona='S'
084600160503     c                   endif
084700160503     c                   if        partpm1='M ' or partpm2='M '
084800160503     c                   eval      wMess='S'
084900160503     c                   eval      wselzona='S'
085000160503     c                   endif
085100160503     c                   if        partpm1='P1' or partpm2='P1'
085200160503     c                   eval      wParc1='S'
085300160503     c                   eval      wselzona='S'
085400160503     c                   endif
085500160503     c                   if        partpm1='P2' or partpm2='P2'
085600160503     c                   eval      wParc2='S'
085700160503     c                   eval      wselzona='S'
085800160503     c                   endif
085900060508     c
086000050510     c* se richiesta soltanto zone parcel o messagerie, carico in
086100050510     c*  schiera
086200160503     c                   if        wparc='S' and wmess='S'
086300160503     c                   clear                   wparc
086400160503     c                   clear                   wmess
086500160503     c                   clear                   wselzona
086600160503     c                   endif
086700160503     c                   if        wparc1='S' and wparc2='S'
086800160503     c                   clear                   wparc1
086900160503     c                   clear                   wparc2
087000160503     c                   eval      wparc='S'
087100160503     c                   endif
087200160503     c
087300160503    1c                   if        wselzona='S'
087400050511     c                   clear                   xx                4 0
087500160502     c                   clear                   yy                4 0
087600050510     c                   movel     vidfil        alffil            3
087700050511     c                   movel     vidfil        wpo
087800050510     c                   movel(P)  '03'          cod
087900050510     c                   movel(P)  vidfil        key
088000050510     c     ktab          setll     tabel
088100050510     c     ktab2         reade     tabel
088200160502    2c                   dow       not %eof(tabel00f) and  wpo=alffil
088300160502    3c                   if        tblflg=' '
088400050510     c                   movel     tblkey        wpo               3
088500160502    4c                   if        wpo=alffil
088600050511     c                   movel     tbluni        ds03
088700160502
088800160502     c* parcel/messaggerie
088900160503    5c                   if        wparc='S' or wmess='S'
089000160502
089100160503    6c                   if        (wmess='S' and §03tpm='M') or
089200160503     c                             (wparc='S' and §03tpm<>'M')
089300050511     c                   add       1             xx
089400050511     c                   movel     tblkey        w005a             5
089500050511     c                   move      w005a         zone(xx)
089600160502    6c                   endif
089700160502    5c                   endif
089800160502     c* parcel 1° e 2° turno
089900160503    5c                   if        (wparc1='S' or wparc2='S')
090000160503     c                             and §03tpm<>'M'
090100160502
090200160503    6c                   if        (wparc2='S' and §03turno='2') or
090300160503     c                             (wparc1='S' and §03turno<>'2')
090400160503     c                   add       1             yy
090500160502     c                   movel     tblkey        w005a             5
090600160503     c                   move      w005a         zone12(yy)
090700160502    6c                   endif
090800160502    5c                   endif
090900160502     c
091000160502    4c                   endif
091100160502    3c                   endif
091200160502     c
091300050511     c     ktab2         reade     tabel
091400160502    2c                   enddo
091500160502    1c                   endif
091600050511     c
091700160503     c                   clear                   statipo
091800160503     c                   clear                   statipo2
091900160503     c                   if        partpm1='P ' or partpm2='P '
092000050511     c                   eval      statipo=cparcel
092100050511     c                   endif
092200050511     c
092300160503     c                   if        partpm1='M ' or partpm2='M '
092400160503     c                   if        statipo=*blanks
092500050511     c                   eval      statipo=cmessag
092600160503     c                   else
092700160503     c                   eval      statipo2=cmessag
092800160503     c                   endif
092900050511     c                   endif
093000160503     c                   if        partpm1='P1' or partpm2='P1'
093100160503     c                   if        statipo=*blanks
093200160503     c                   eval      statipo=cparc1
093300160503     c                   else
093400160503     c                   eval      statipo2=cparc1
093500160503     c                   endif
093600160503     c                   endif
093700160503     c                   if        partpm1='P2' or partpm2='P2'
093800160503     c                   if        statipo=*blanks
093900160503     c                   eval      statipo=cparc2
094000160503     c                   else
094100160503     c                   eval      statipo2=cparc2
094200160503     c                   endif
094300160503     c                   endif
094400941017     C*
094500100706      * Aggancio tabella VPO recpord VPO
094600100706     c                   Clear                   Dvpo
094700100706     c                   Clear                   dsbs02
094800100706     c                   Eval      T02Mod = 'C'
094900100706     c                   Eval      T02tla = 'L'
095000100706     c                   Eval      T02Sif = knsif
095100100706     c                   Eval      T02Cod = 'VPO'
095200100706     c                   Movel(p)  'VPO'         T02Ke1
095300100706     c                   Call      'TIBS02R'
095400100706     c                   Parm                    Kpjba
095500100706     c                   Parm                    dsbs02
095600100706     c                   If        T02Err = *Blanks
095700100706     c                   Movel     T02Uni        Dvpo
095800100706     c                   EndIf
095900100706     c
096000100706     c     *eur          move      udate8        dataeur
096100100706     c                   movel     dataeur       dataiso
096200161017     c
096300161017     c                   movel     dataiso       dateu             8 0
096400161017
096500100706     c                   if        §vpoggima>0
096600100706     c                   subdur    §vpoggima:*d  dataiso
096700100706     c                   endif
096800100706     c
096900100706     c                   movel     dataiso       datapresIMA       8 0
097000100706
097100911018     C                   ENDSR
097200941017     C**************************************************************************
097300941017     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
097400941017     C**************************************************************************
097500911018     C     ERRORE        BEGSR
097600941017     C*
097700911015     C                   SETON                                        01
097800040603     c
097900040603     c     kar5          chain     fiar501l
098000040603     c                   if        %found(fiar501l)
098100040603     c                   movel     ar5uni        dar5gen
098200040603     c                   else
098300040603     c                   clear                   dar5gen
098400040603     c                   endif
098500911018     C* STAMPA ANOMALIA
098600040428     C                   EXSR      STAMPA
098700081014     c
098800100706     c* non creo anomalia se collo doppio o solo segnalazione
098900161017    0c                   if        not *in12 and not *in16 and not *in11
099000161018     c                             and not *in18
099100170124     c* in IMA e IMG la creo solo se non è da portare a mag giac  se partito con NEW IMA
099200161025     c                             and wgiac<>'D'
099300911015     C*
099400950707     C* SOLO SE IMA DEFINITIVO  CREO ANOMALIA
099500950608    1C     VIDSN         IFEQ      'S'
099600950810     C* NON DEVO CREARE:
099700950810     C* 1) SE C'E' LA FORZATURA E MANCA LA DATA DI ARRIVO
099800950810     C*    ANCHE SE STAMPO LO STESSO
099900950810     C* 2) SE LA SPUNTA E' STATA TROVATA IN ALTRA CATEGORIA DEL GIORNO
100000960419     C*    3 O 4 NON DISABILITO NEMMENO LA BOLLA
100100960419     C* 3) SE LA SPUNTA E' STATA TROVATA IN ARRIVO CREO L'ANOMLIA
100200950608    2C     ARTDAM        IFGT      0
100300960419   2AC     *IN13         IFEQ      *OFF
100400031219     C     *IN15         OREQ      *ON
100500081014i    C*
100600081104     C     KANM          SETLL     FNANM02L                               35
100700081014    3c                   if        not *in35
100800081014     c* Se anomalia chiusa non in pratica idd e data chiusura <data ima
100900081014     c*  la ricreo
101000081104    4c***                if        anmdch>0 and anmdch<savdfv and
101100081104     c***                          anmcch<>'PR'
101200081104     c***                delete    fnanm000
101300081104     c***                seton                                        35
101400081104    4c***                endif
101500081104    3c***                endif
101600081014     c
101700081104    3C***                if        *in35
101800081104     C*
101900081104     C                   MOVEL     *BLANKS       ANMATB
102000081104     C                   MOVEL     PA5FAS        ANMFSA
102100160503     c*  data salvata del foglio IMA perchè richaino in update
102200100506     C                   Z-ADD     SAVdfv        ANMDAO
102300081104     C                   Z-ADD     15            ANMcaa
102400081104     C                   MOVEL     *BLANKS       ANMCCH
102500081104     C                   MOVEL     VIDFGS        ANMCDU
102600081104     C                   Z-ADD     COMNFV        ANMNFV
102700081104     C                   MOVEL     VIDFGS        ANMFLE
102800081104     C                   MOVE      *BLANKS       ANMFLO
102900081104     C                   MOVEL     *BLANKS       ANMFT1
103000081104     C                   MOVEL     *ZEROS        ANMFL1
103100081104     C                   MOVEL     *BLANKS       ANMFT2
103200081104     C                   MOVEL     *ZEROS        ANMFL2
103300081104     C*
103400081104     C                   MOVEL     §7CTAN        ANMTAN
103500081104     C                   MOVEL     §7CSAN        ANMSAN
103600081104     C                   MOVEL     §7CAIE        ANMAIE
103700081104     C                   Z-ADD     §7CLID        ANMLID
103800081104     C                   MOVE      *ZEROS        ANMDUE
103900081104     C                   MOVE      *ZEROS        ANMNPG
104000081104     C                   MOVE      *ZEROS        ANMFGS
104100081104     C                   Z-ADD     0             ANMDCH
104200081104     C                   MOVEL     ' '           ANMFSC
104300081104     C* CHIUSURA AUTOMATICA
104400081104     C     §7CCHA        IFEQ      'S'
104500100506     C                   Z-ADD     SAVDFV        ANMDCH
104600081104     C                   MOVEL     PA5FAS        ANMFSC
104700081104     C                   END
104800081104     C                   MOVE      *BLANKS       ANMFT3
104900081104     C                   MOVE      *ZEROS        ANMFL3
105000081104     C                   MOVE      *BLANKS       ANMFT4
105100081104     C                   MOVE      999           ANMFL4
105200081104     C                   MOVE      *ZEROS        ANMDTR
105300081104     C*
105400911018     C                   MOVEL     ARTFLS        ANMFLS
105500911018     C                   MOVEL     ARTLNA        ANMLNA
105600911018     C                   MOVEL     ARTNRS        ANMNRS
105700941018     C                   MOVEL     ARTNSC        ANMSCN
105800941018     C                   Z-ADD     ARTAAS        ANMAAS
105900911018     C                   MOVEL     ARTNSP        ANMNSP
106000911018     C                   MOVEL     ARTLNP        ANMLNP
106100970206     C                   MOVEL     ARTNAP        ANMNPS
106200040603     c
106300040603     c* imposto data e ora di creazione
106400050707     C                   TIME                    W0140            14 0
106500050707     C                   MOVE      W0140         G02DAT
106600050707     C                   Z-ADD     *ZERO         G02INV
106700050707     C                   MOVEL     *BLANKS       G02ERR
106800050707     C                   CALL      'XSRDA8'
106900050707     C                   PARM                    WLBDAT
107000050707     C                   Z-ADD     G02INV        WDATA             6 0
107100050707     c**                 time                    wora              6 0
107200050707     c**                 z-add     *date         wdata             6 0
107300050707     C
107400050707     c                   movel     w0140         wora4             4 0
107500040603     c                   movel     wdata         anmflo
107600040603     c                   move      wora4         anmflo
107700040603     c
107800040603     c* imposto se flag di valore
107900040603     c                   movel     §ar5bva       anmft4
108000040603     c
108100040603     c
108200920409     C* ESTERNA
108300950608    4C     §7CAIE        IFEQ      'E'
108400950608     C                   MOVEL     ARTLNP        ANMFL1
108500950810    4C                   ENDIF
108600920409     C*
108700990511     C                   WRITE     FNANM000
108800081014    3C                   endif
108900081014     c
109000000512   2AC                   ENDIF
109100960419    2C                   ENDIF
109200920318     C*
109300000512     C* DISABILITO LA BOLLA SOLO SE HO CREATO ANOMALIA
109400000512     C* NO POSTE SEMPRE, LNP POSTE SOLO SE PREVOSTO
109500950810     C* SOLO SE NON HO TROVATO UN'ALTRA SPUNTA IN DATA DEL FOGLIO IMA
109600960419    2C     *IN10         IFEQ      *OFF
109700960419     C*
109800960419    3C     *IN13         IFEQ      *OFF
109900031219     C     *IN15         OREQ      *ON
110000000512     C*
110100040428    5c     wchain        ifeq      *blanks
110200040428     c     fnarbnrri     chain(e)  fnarb00f
110300040428    6c                   if        %error
110400040428     C* Valorizzo la DS di procedura
110500040428     C                   CLEAR                   TRUL82DS
110600040428     c                   eval      ul82§rrn = fnarbnrri
110700040428     C                   EVAL      UL82§FIL = 'FNARB00F'
110800040428     C                   EVAL      UL82§WIN = 'N'
110900040428     C                   EVAL      UL82§F7  = 'S'
111000040428     C                   EVAL      UL82§num = 5
111100040428     C                   EVAL      UL82§att = 60
111200040428     C                   EVAL      UL82§mss = msgjob
111300040428     C* Effettuo la chiamata al *PGM d utilità
111400040428     C                   CALL(e)   'TRUL82R'
111500040428     C                   PARM                    TRUL82DS
111600040428     C     fnarbnrri     chain     fnarb00f
111700040428    6c                   endif
111800040428     C                   Z-ADD     0             ARBAMA
111900040428     c                   update    fnarbf00
112000040428     c                   move      '1'           wchain
112100040428    5c                   endif
112200930629     C* AGGIORNO IL COLLO CON UNA 'D' DI DISABILITAZIONE
112300941221     C                   MOVEL     'S'           ARTDAB                         'D'=DISABILITATO
112400000512    3C                   ENDIF
112500960419    2C                   ENDIF
112600950810     C*
112700040428     C  N10              UPDATE    FNART000
112800950810    1C                   ENDIF
112900081014    0C                   ENDIF
113000911015     C*
113100911015     C                   ENDSR
113200950810     C**************************************************************************
113300950810     C* ELABORAZIONE DELL'IMA
113400950810     C**************************************************************************
113500950810     C     ELAIMA        BEGSR
113600950810     C*
113700950810     C* N O N   C O N T R O L L O :
113800950810     C*          1) SE NO ABILITAZIONE FORZATA E SENZA DATA ARRIVO
113900990212    1C     SAVAMA        IFNE      3
114000950810     C     ARTDAM        ANDEQ     0
114100950810     C                   SETON                                        34
114200990212    1C                   ENDIF
114300950810     C*
114400950810     C*          2) SE DATA ARRIVO MAGGIORE DATA FOGLIO IMA
114500990212    1C  N34ARTDAM        IFNE      0
114600950810     C     ARTDAM        ANDGT     SAVDFV
114700950810     C                   SETON                                        34
114800990212    1C                   ENDIF
114900950810     C*
115000990212     C*          3) SE COLLO SI TROVA IN DCD E RESO O DISTRUTTO PER
115100990212     C*             AVARIA O COLLO MANCANTE
115200990212     C     KARTD         CHAIN     FNDCD02L                           31
115300990212    1C     *IN31         IFEQ      *OFF
115400990212     C*
115500990303     C     DCDATB        ANDEQ     ' '
115600990212     C* CHAIN TESTATA PER CONTROLLARE LA DATA DI CHIUSURA
115700990212     C     KDCD          CHAIN     FNDCT01L                           32
115800050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
115900050221     c                   If        Not *In32 and dctdt2 > 0
116000050221     c                   Eval      *In32 = *On
116100050221     c                   EndIf
116200990303     C*
116300990303    2C     *IN32         IFEQ      *OFF
116400990303     C* SE E' CHIUSA CONTROLLO SE IL RECORD DEVE ESSERE COMPRESO
116500990303     C*  IN IMA, PER ALCUNI TIPI ANOMALIA
116600990303     C                   MOVEL     'S'           §CHIMA
116700990303    3C     DCTCCH        IFNE      *BLANKS
116800990303     C                   CLEAR                   DSBS02
116900990303     C                   MOVEL     'C'           T02MOD
117000990303     C                   MOVEL     KNSIF         T02SIF
117100990303     C                   MOVEL     'CCH'         T02COD
117200990303     C                   MOVEL     DCTCCH        T02KE1
117300990303     C                   CALL      'TIBS02R'
117400990303     C                   PARM                    KPJBA
117500990303     C                   PARM                    DSBS02
117600990303     C*
117700990303    4C     T02ERR        IFEQ      ' '
117800990303     C                   MOVEL     T02UNI        DCCH
117900990303    4C                   ENDIF
118000990303    3C                   ENDIF
118100990303     C**
118200990303     C** SE NON DEVE ESSERE COMPRESO:
118300990303    3C     §CHIMA        IFNE      'S'
118400990303     C     DCTDCH        OREQ      0
118500990212     C** SE ENTRAMBI APERTI O CHIUSI CON STESSA DATA CONTROLLO IL TIPO
118600990212     C**  ANOMALIA PER DECIDERE SE TENERE O ESCLUDERE IL COLLO
118700990212     C**  NON CONSIDERO LA CAUSALE PAREGGIO CHE SIGNIFICA CHE IL COLLO
118800990212     C*  C'E' SEMPRE
118900990212    4C     DCTDCH        IFEQ      DCDDCH
119000990212     C     DCDDCH        OREQ      0
119100990212     C**
119200990212     C                   CLEAR                   DSBS02
119300990212     C                   MOVEL     'C'           T02MOD
119400990212     C                   MOVEL     KNSIF         T02SIF
119500990212     C                   MOVEL     'TAD'         T02COD
119600990212     C                   MOVEL     DCTTAD        T02KE1
119700990212     C                   CALL      'TIBS02R'
119800990212     C                   PARM                    KPJBA
119900990212     C                   PARM                    DSBS02
120000990212     C**
120100990212    5C     T02ERR        IFEQ      ' '
120200990212     C                   MOVEL     T02UNI        DTAD
120300990212     C                   ELSE
120400990212     C                   MOVEL     'A'           §TARGR
120500990212    5C                   ENDIF
120600990212    C**
120700990212     C* SE MANCANZE
120800990212    5C     §TARGR        IFEQ      'M'
120900990212     C* O AVARIA RESA
121000990212     C     §TARGR        OREQ      'V'
121100990212     C     DCTDSD        ANDEQ     'R'
121200990212     C* O AVARIA DISTRUTTA
121300990212     C     §TARGR        OREQ      'V'
121400990212     C     DCTDSD        ANDEQ     'D'
121500990212     C*                    --> ESCLUDO
121600990212     C                   SETON                                        34
121700990212    5C                   ENDIF
121800990212    4C                   ENDIF
121900990212    3C                   ENDIF
122000990212    2C                   ENDIF
122100990212    1C                   ENDIF
122200101215     c
122300101215     c* Se opzione solo spuntato alla linea di arrivo finale, controllo
122400101215     c                   if        parimaa='S'
122500101215     c                   setoff                                       37
122600101215RM*  C     Kartd         setll     FNBRV07l
122700101215RM*  C     Kartd         reade     FNBRV07l
122800101215     c                   dow       not %eof(fnbrv07l)
122900101215     c     brvfgs        lookup    Lspu                                   37
123000101215     c                   if        *in37
123100101220     c* Se la filiale che spunta è giusta verifico che sia categoria di "arrivo"
123200101220     c                   clear                   dslv53
123300101220     C                   z-add     brvnFV        D53NFV
123400101220     C                   MOVEL     brvNPG        D53NPG
123500101220     C                   MOVEL     brvFGS        D53FGS
123600101220     C                   CALL      'FNLV53R'
123700101220     C                   PARM                    DSLV53
123800101220     C*
123900101220    2C     D53ERR        IFeq      ' '
124000101220     c*
124100101220     c                   if        d53def='S'
124200101220     c                   eval      wnpgarr=%editc(d53npg:'X')+'D'
124300101220     c                   else
124400101220     c                   eval      wnpgarr=%editc(d53npg:'X')+d53spg
124500101220     c                   endif
124600101220     c
124700101220     c                   eval      TROV=%lookup   (wnpgarr: NPGARR)
124800101220     c                   if        TROV>0
124900101215     c                   leave
125000101220     c                   endif
125100101215     c                   endif
125200101220     c                   endif
125300101220     c
125400101220     c                   setoff                                       37
125500101220RM*  C     Kartd         reade     FNBRV07l
125600101215     c
125700101215     c                   enddo
125800101215     c
125900101215     c* se non trovata nessuna spunta, NON elaboro
126000101215     c                   if        not *in37
126100101215     c                   seton                                        34
126200101215     c                   endif
126300101215     c
126400101215     c                   endif
126500990212    C**
126600170125     c** se collo mancante di bolla da portare a mag gaicenza e collo spuntato in
126700170125     c*  IMG nel giorno --> non stampo
126800170125     c
126900100706     c                   if        not *in34
127000170125     c                   clear                   winGiac
127100100706     C                   EXSR      CTRART
127200950810     C*
127300950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
127400170125     c*         se si tratta di merce da portare a IMG e collo spuntato solo in IMG
127500170125     c*         non stampo
127600170125     c                   if        wgiac='D' and *in14 and
127700170125     c                             wspg='G' and wdfv=savdfv
127800170125     c                   eval      winGiac='S'
127900170125     c                   endif
128000170125
128100100706     C* 16 ON -     TROVATO SEGNACOLLO IN IMA più di xxx volte
128200170125     c                   if        *in14 and wingiac=' '  or *in16
128300170125     C                   EXSR      ERRORE
128400170125     c                   endif
128500170125
128600170125     c                   if        not *in14
128700170125     c                   eval      Wesspu='S'
128800170125     c                   endif
128900100706     c                   endif
129000950810     C*
129100950810     C                   ENDSR
129200950810     C**************************************************************************
129300950810     C* ELABORAZIONE DELL'IMG
129400950810     C**************************************************************************
129500950810     C     ELAIMG        BEGSR
129600950810     C                   EXSR      CTRART
129700950810     C*
129800170124     C* 14 ON - NON TROVATO SEGNACOLLO IN IMG
129900161025     c                   if        *in14
130000161026     c* In caso di bolla con giacenza e merce da portare a mag giac.
130100161026     c*  non stampo nulla per IMG
130200170124     c***                if        *in11 or wgiac<>'D'
130300170124     c                   if        wgiac<>'D'
130400161025     C                   EXSR      ERRORE
130500161026     c                   endif
130600161026     c
130700161025     c                   else
130800161025     c                   eval      Wesspu='S'
130900161025     c                   endif
131000161025
131100950810     C                   ENDSR
131200941017     C**************************************************************************
131300941017     C* STAMPO DISCORDANZA
131400941017     C**************************************************************************
131500900621     C     STAMPA        BEGSR
131600050405     c                   setoff                                       242526
131700941017     C*
131800911015     C* STAMPA TESTATA
131900911015     C   OF              DO
132000911018     C                   SETON                                        05
132100911018     C                   EXCEPT    TES
132200920813     C                   EXCEPT    TES2
132300911018     C                   SETOFF                                       OF
132400911018     C                   END
132500090522     c* Se non trovata la bolla (per colli doppi)
132600090522    1c                   if        NoBolla='S'
132700090522     c                   except    DetNObo
132800161018     C   12              EXCEPT    DETdoppio
132900161018     C   18              EXCEPT    DETeccede
133000090522   x1c                   else
133100911018     C*
133200950810     C     COMSPE        IFNE      ARBSPE
133300911018     C                   SETON                                        05
133400950810     C                   MOVEL     ARBSPE        COMSPE
133500950810     C                   ENDIF
133600911018     C* DATA SPEDIZIONE
133700911018     C   05              DO
133800941018     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
133900941018     C                   MOVE      ARBDSP        G02INV                         AAAA/MM/GG
134000911018     C                   MOVE      *ZEROS        G02DAT
134100911018     C                   MOVEL     3             G02ERR
134200941017     C                   CALL      'XSRDA8'
134300911018     C                   PARM                    WLBDAT
134400941018     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
134500941018     C*
134600941018     C* CODICE PADRONCINO CONSEGNA DECODIFICA
134700021203     C*    ARBPDC        CHAIN     FNAPD01L                           06
134800021205     C                   move      arbpdc        kpdpdr
134900021203     C                   move      'A'           kpdtip
135000021203     C     kfapd         chain     fiapd01L                           06
135100941018     C  N06              MOVEL     APDRSC        DESCPC            9
135200100707     c
135300100707     c* se non c'e' paadroncino consegna scrivo se Fermo deposito
135400100707     c                   if        *in06
135500100707     C                   MOVEL     *BLANKS       DESCPC
135600100707     c                   if        arbffd='S'
135700100707     c                   eval      descpc='FDeposito'
135800100707     c                   endif
135900100707     c                   endif
136000920409     C*
136100911018     C* SEGNACOLLI NON CONSECUTIVI
136200911018     C     ARBFNS        COMP      'S'                                    04
136300911018     C* SERIE>< 0
136400911018     C     ARBNRS        COMP      0                                  0303
136500911018     C                   END
136600920409     C*
136700920708     C* 07 ON - MANCA DATA ARRIVO: SPEDIZIONE FORZATA A 3
136800920708     C     ARTDAM        IFEQ      0
136900920708     C                   SETON                                        07
137000920708     C                   ELSE
137100920708     C                   SETOFF                                       07
137200920409     C* DATA ARRIVO MERCE  SEGNACOLLO
137300941018     C                   MOVE      ARTDAM        G02INV                         AAAA/MM/GG
137400920409     C                   MOVE      *ZEROS        G02DAT
137500920409     C                   MOVEL     3             G02ERR
137600941017     C                   CALL      'XSRDA8'
137700920409     C                   PARM                    WLBDAT
137800941018     C                   Z-ADD     G02DAT        STADAM                         GG/MM/AAAA
137900920708     C                   END
138000040603     C* MITTENTE E DESTINATARIO DI 17 E 20
138100040603     C                   MOVEL     ARBRSM        STARSM           17
138200920707     C                   MOVEL     ARBRSD        STARSD           20
138300040603     c* collo di valore
138400040603     c                   movel     §ar5bva       stabva            1
138500161025     c* se si tratta di bolla da portare a mag giac, riempi il campo STADSF con relativa scritta
138600161025     c                   if        not *in11 and wgiac='D'
138700161025     c                   clear                   stadsf
138800161025     c                   eval      stadsf='Portare a MagGIAC'
138900161025     c                   endif
139000161025     c
139100161026     c                   clear                   stahms            4 0
139200161026     c                   eval      stahms=brvhms/100
139300910522     C*
139400900622     C* STAMPO DETTAGLIO
139500100706     C                   EXCEPT    DET
139600100707     c                   select
139700100707     c                   when      *in12
139800100706     C                   EXCEPT    DETdoppio
139900100707     c                   when      *in16
140000100707     C                   EXCEPT    DETspuIMA
140100161018     c                   when      *in18
140200161018     C                   EXCEPT    DETeccede
140300100707     c                   other
140400100706     C                   EXCEPT    DETerr
140500100707     c                   endsl
140600070418     c
140700070418     c* se in distinta , stampo
140800090522    2c                   if        not *in12
140900081014
141000100706    3c                   if        *in05 and arbndc>0 and not *in16
141100070418     C                   MOVE      ARbddc        G02INV                         AAAA/MM/GG
141200070418     C                   MOVE      *ZEROS        G02DAT
141300070418     C                   MOVEL     3             G02ERR
141400070418     C                   CALL      'XSRDA8'
141500070418     C                   PARM                    WLBDAT
141600070418     C                   Z-ADD     G02DAT        STAddc            8 0          GG/MM/AAAA
141700070418     C                   EXCEPT    DETndc
141800090522    3c                   endif
141900050405     c
142000161017     c* Se c'e' ubicazione collo, per 05 on lo stampo
142100050405     c                   if        *in05 and §ar5zng<>*blanks
142200050405     c                   seton                                        24
142300050405     c                   endif
142400050405     c
142500100707     C                   SETOFF                                       05
142600050405     c
142700990212     C* STAMPO TUTTE LE C.A. PRESENTI SULLA SPEDIZIONE
142800040806      * Sostituisco la lettura del file FNDTC02 con
142900040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
143000040806      * legate alla spedizione che passo
143100040806      *
143200040806     c                   clear                   fidn12ds
143300040806     c                   eval      i12aas = arbaas
143400040806     c                   eval      i12lnp = arblnp
143500040806     c                   eval      i12nrs = arbnrs
143600040806     c                   eval      i12nsp = arbnsp
143700040806     c                   eval      i12fel = 'E'
143800040806     c                   eval      i12fan = 'E'
143900040806      *
144000040806     c                   call      'FIDN12R'
144100040806     c                   parm                    fidn12ds
144200040806      *
144300040806      * se non ci sono errori
144400090522    3c                   if        o12err = ' '
144500040806      * se numero di CA trovate maggiore di zero
144600090522    4c                   if        o12nca > 0
144700100706     c
144800050405     c                   seton                                        25
144900040806     c
145000040806     c                   z-add     *zeros        ii                2 0
145100100706    6c     1             do        o12nca        ii
145200040806     c                   movea     skkey(ii)     dskey
145300040806     C                   Z-ADD     dsfil         dctfil
145400040806     C                   Z-ADD     dsnca         dctnca
145500040806     C                   Z-ADD     skdca(ii)     dctdca
145600040806     C                   Z-ADD     skdch(ii)     dctdch
145700040806     C                   movea     skcch(ii)     dctcch
145800040806     c                   exsr      preca
145900050405     c                   except    detca
146000050405     c
146100050405     c                   setoff                                       24
146200100706    6C                   ENDDO
146300040806      *
146400100706    4c                   endif
146500090522    3c                   endif
146600050405     c
146700050405     c* Stampo ubicazione collo se non ancora stampata
146800090522    3c                   if        *in24
146900050405     c                   except    detca
147000090522     c                   setoff                                       24
147100090522    3c                   endif
147200090522     c                   setoff                                       25
147300081014     c
147400090522    2c                   endif
147500090522    1c                   endif
147600900622     C*
147700900622     C                   ENDSR
147800990212     C**************************************************************************
147900990212     C* PREPARO I DATI DELLA C.A.
148000990212     C**************************************************************************
148100990212     C     PRECA         BEGSR
148200990212     C                   SETOFF                                       26
148300990303     C                   CLEAR                   STACCH
148400990212     C* DATA APERTURA C.A.
148500990212     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
148600990212     C                   Z-ADD     *ZERO         G02DAT
148700990212     C                   MOVEL     '3'           G02ERR
148800990212     C                   CALL      'XSRDA8'
148900990212     C                   PARM                    WLBDAT
149000990212     C                   MOVE      G02DAT        W0020             2 0
149100990212     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
149200990212     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
149300990303     C** DATA CHIUSURA C.A.
149400990212     C                   CLEAR                   STACHC
149500990303    1C     DCTDCH        IFGT      0
149600990212     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
149700990212     C                   Z-ADD     *ZERO         G02DAT
149800990212     C                   MOVEL     '3'           G02ERR
149900990212     C                   CALL      'XSRDA8'
150000990212     C                   PARM                    WLBDAT
150100990212     C                   MOVE      G02DAT        W0020             2 0
150200990212     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
150300990212     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
150400990212     C                   SETON                                        26
150500990303     C* CAUSALE CHIUSURA
150600990303    2C     DCTCCH        IFNE      *BLANKS
150700990303     C                   CLEAR                   DCCH
150800990303     C                   CLEAR                   DSBS02
150900990303     C                   MOVEL     'C'           T02MOD
151000990303     C                   MOVEL     KNSIF         T02SIF
151100990303     C                   MOVEL     'CCH'         T02COD
151200990303     C                   MOVEL     DCTCCH        T02KE1
151300990303     C                   CALL      'TIBS02R'
151400990303     C                   PARM                    KPJBA
151500990303     C                   PARM                    DSBS02
151600990303     C*
151700990303    3C     T02ERR        IFEQ      ' '
151800990303     C                   MOVEL     T02UNI        DCCH
151900990303    3C                   ENDIF
152000990303     C                   MOVEL     §CHDES        STACCH           12
152100990303    2C                   ENDIF
152200990303    1C                   ENDIF
152300990212     C                   ENDSR
152400941017     C**************************************************************************
152500161018     C* RIEPILOGO SPUNTE PER PISTOLA e controllo se colli doppi o eccedenti
152600941017     C**************************************************************************
152700920813     C     RIEPIL        BEGSR
152800081014     C                   SETON                                        12
152900941017     C*
153000920813     C                   Z-ADD     1             C
153100920813     C* CONTO SPUNTE PER PISTOLA
153200070118     C     KBRV          SETLL     FNBRV01L
153300070118     C     KBRV          READE     FNBRV01L                               30
153400920813     C*
153500161018    1C     *IN30         DOWEQ     *OFF
153600161018    2c                   if        brvtsu<>*blanks
153700081014     c                   clear                   fnlrv7ds
153800081014     c                   eval      ilrv7fls=brvlnp
153900081014     c                   eval      ilrv7lna=brvlna
154000081014     c                   eval      ilrv7nrs=brvnrs
154100081014     c                   eval      ilrv7nsc=brvnsc
154200081014     c                   eval      kpjbu= fnlrv7ds
154300081014     c                   call      'FNLRV7R'
154400081014     c                   parm                    kpjba
154500081014     c
154600081014     c                   eval      fnlrv7ds=kpjbu
154700161018    3c                   if        olrv7nric>=3 and olrv7part='S'
154800081014     c* Se ho già stampato qualche cosa, lascio la prima volta una
154900081014     c*  riga vuota
155000161018    4c                   if        *in01 and not *inOF and uno=' '
155100081014     c                   except    DETvuota
155200161018    4c                   endif
155300161018     c                   exsr      Cerca_bolla
155400081014     c
155500081014     c                   exsr      ERRORE
155600081014     c                   eval      UNO='S'
155700161018    3c                   endif
155800081014     c
155900161018    2c                   endif
156000161018     c
156100170124     c*      per  IMG  stampo i colli eccedenti ( IPG non esiste più)
156200170124     C*  ( SE partiti con procedura)
156300161201     c                   if        *in10 and *in22
156400161201     c                   exsr      Eccedenti
156500161201     c                   endif
156600081014     c
156700941019     C                   Z-ADD     1             B                 2 0
156800920813     C                   MOVEL     BRVNPS        COMNPS            2
156900920813     C     COMNPS        LOOKUP    NPS(B)                                 31
157000920813     C  N31              MOVEL     BRVNPS        NPS(C)
157100920813     C  N31              Z-ADD     C             B
157200941019     C  N31              ADD       1             C                 2 0
157300920813     C*
157400920813     C                   ADD       1             CPS(B)
157500920813     C*
157600070118     C     KBRV          READE     FNBRV01L                               30
157700161018    1C                   ENDDO
157800081014     C
157900081014     C                   SETOFF                                       12
158000161018     c
158100081014     C*
158200081014     C* SE NESSUNA ANOMALIA STAMPO TUTTO O.K.
158300081014     C  N01              EXCEPT    TES
158400081014     C  N01              EXCEPT    OK
158500081014     C  N01              SETOFF                                       OF
158600920813     C**
158700920813     C* SOMMO TUTTI I COLLI
158800920813     C                   XFOOT     CPS           TOTSPU            9 0
158900920813     C**
159000920813     C* STAMPA SPUNTE
159100920813     C* TESTATA
159200920813     C   OF              EXCEPT    TES
159300920813     C   OF              SETOFF                                       OF
159400920813     C*
159500920813     C                   EXCEPT    TOT1
159600920813     C*
159700920813     C                   Z-ADD     1             C
159800920813     C     CPS(C)        DOWGT     0
159900920813     C   OF              DO
160000920813     C                   EXCEPT    TES
160100920813     C                   EXCEPT    TOT1
160200920813     C                   SETOFF                                       OF
160300920813     C                   END
160400920813     C*
160500920813     C                   EXCEPT    TOT2
160600920813     C                   ADD       1             C
160700920813     C                   END
160800920813     C*
160900920813     C                   EXCEPT    TOT3
161000941017     C*
161100920813     C                   ENDSR
161200161018     C**************************************************************************
161300161018     C*   da spunta cerco la relativa bolla
161400161018     C**************************************************************************
161500161018     c     Cerca_bolla   BEGSR
161600161018     c                   clear                   nobolla
161700161018     c* chain la bolla e stampo collo doppio
161800161018     c     kartB         chain     fnart27l
161900161018    2c                   if        %found(fnart27l)
162000161018     c     karb          chain     fnarb01l
162100161018   x2c                   else
162200161018     c     kartB         chain     fnblt27l
162300161018    3c                   if        %found(fnart27l)
162400161018     c     karb          chain     fnblp01l
162500161018   x3c                   else
162600161018     c                   eval      Nobolla='S'
162700161018    3c                   endif
162800161018    2c                   endif
162900161018     c                   ENDSR
163000161018     C**************************************************************************
163100161018     C*   Ricerca colli eccedenti
163200161018     C**************************************************************************
163300161018     C     Eccedenti     BEGSR
163400161018     c                   setoff                                       12
163500161018     c                   seton                                        18
163600161018     c                   clear                   ecce_descr
163700161018     c
163800161018     c                   exsr      Cerca_bolla
163900161018     c* Se manca bolla --> sempre eccedente
164000161018    1c                   if        nobolla='S'
164100161018    4c                   if        *in01 and not *inOF and uno=' '
164200161018     c                   except    DETvuota
164300161018    4c                   endif
164400161018     c                   exsr      ERRORE
164500161018     c                   eval      UNO='S'
164600161018   x1c                   else
164700161018     c* Richiamo driver per verificare se la bolla è a mag giac oggi
164800161018     c                   clear                   fnlgi1ds
164900161018     c                   eval      ILGI1AAS=arbaas
165000161018     c                   eval      ILGI1lnp=arblnp
165100161018     c                   eval      ILGI1nrs=arbnrs
165200161018     c                   eval      ILGI1nsp=arbnsp
165300161018    3c                   select
165400161018     c                   when      arbpkc>0 and arbncp=arbncl
165500161018     c                   eval      ilgi1pkf=arbpkc
165600161018     c                   when      arbpkc>0 and arbncp<arbncl
165700161018     c                             and arbpkc>arbpkf
165800161018     c                   eval      ilgi1pkf=arbpkc
165900161018     c                   other
166000161018     c                   eval      ilgi1pkf=arbpkf
166100161018    3c                   endsl
166200161018     c                   movel     fnlgi1ds      kpjbu
166300161018     c                   call      'FNLGI1R'
166400161018     c                   parm                    kpjba
166500161018     c                   movel     kpjbu         fnlgi1ds
166600161201     c* Per IPG --> corretto se il flag da portare a mag giac = "Y"
166700161025     c* Per IMG --> corretto se il flag da portare a mag giac = "Y"  o già in giac
166800161026     c*         --> ignoro anche se deve uscire da mag gaicenza, ma non ancora effettuat
166900161025    2c                   if        (*in11 and olgi1AMG<>'Y'and
167000161025     c                              olgi1AMG<>'S') or
167100161025     c                             (not *in11 and olgi1AMG<>'Y' and
167200161025     c                             olgi1AMG<>'S'  and
167300161026     c                             olgi1inmg<>'S' and olgi1outmg<>'S')
167400161018     c*
167500161018     c                   select
167600161025     c                   when      olgi1inmg<>' '
167700161018     c                   eval      ecce_descr=' -già in Giacenza'
167800161026     c                   when      arbndc>0  and arbdcm=0
167900161018     c                   eval      ecce_descr=' -di bolla in distinta '
168000161025     c                   when      OLGI1OUTMG<>' '
168100161025     c                   eval      ecce_descr=' -non più giacente'
168200161018     c                   other
168300161025     c                   eval      ecce_descr=' -di colli IMA'
168400161018     c                   endsl
168500161018
168600161018    4c                   if        *in01 and not *inOF and uno=' '
168700161018     c                   except    DETvuota
168800161018    4c                   endif
168900161018
169000161018     c                   exsr      ERRORE
169100161018     c                   eval      UNO='S'
169200161018    2c                   endif
169300161025    1c                   endif
169400161018     c
169500161018     c                   setoff                                       18
169600161018     c                   seton                                        12
169700161018     C                   ENDSR
169800941017     C**************************************************************************
169900941017     C*   R O U T I N E     I N I Z I A L E
170000941017     C**************************************************************************
170100941017     C     *INZSR        BEGSR
170200941017     C*
170300161017     C* DEFINIZIONE CHIAVI
170400000126     C     KBRV          KLIST
170500000126     C                   KFLD                    IMANPG
170600000126     C                   KFLD                    WNFV
170700000126     C                   KFLD                    VIDFGS
170800070118     C     KBRV1         KLIST                                                  FNBRV01L
170900941019     C                   KFLD                    IMANPG                         ..CATEGORIA
171000000126     C                   KFLD                    WNFV                           ..NR. FOGLIO
171100950316RM*  C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
171200941019     C                   KFLD                    ARTFLS                         ..LNP
171300941018     C                   KFLD                    ARTLNA                         ..LNA
171400941018     C                   KFLD                    ARTNRS                         ..SERIE
171500941018     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
171600070418     C     KBRV4         KLIST                                                  FNBRV01L
171700070418     C                   KFLD                    CONNPG                         ..CATEGORIA
171800070418     C                   KFLD                    ARTFLS                         ..LNP
171900070418     C                   KFLD                    ARTLNA                         ..LNA
172000070418     C                   KFLD                    ARTNRS                         ..SERIE
172100070418     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
172200070418     C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
172300990212RM*  C     KARTD         KLIST
172400990212RM*  C                   KFLD                    ARTFLS
172500990212RM*  C                   KFLD                    ARTLNA
172600990212RM*  C                   KFLD                    ARTNRS
172700990212RM*  C                   KFLD                    ARTNSC
172800081014     C     KARTb         KLIST
172900081014     C                   KFLD                    brvlnp
173000081014     C                   KFLD                    brvlnA
173100081014     C                   KFLD                    brvNRS
173200081014     C                   KFLD                    brvNSC
173300941017     C     KFVV          KLIST                                                  *
173400941019     C                   KFLD                    IMANPG                         *
173500941017     C                   KFLD                    COMNFV                         *
173600941028     C                   KFLD                    VIDFGS                         *
173700950316RM*  C     KFVV2         KLIST
173800950316RM*  C                   KFLD                    BRVNPG
173900000126RM*  C                   KFLD                    WNFV5
174000950316RM*  C                   KFLD                    BRVFGS
174100941017     C     KANM          KLIST                                                  FNANM02L
174200941017     C                   KFLD                    ARTFLS                         *
174300941017     C                   KFLD                    ARTLNA                         *
174400941017     C                   KFLD                    ARTNRS                         *
174500941018     C                   KFLD                    ARTNSC                         *
174600941017     C                   KFLD                    ANMCAA                         *
174700941017     C     KART          KLIST                                                  *
174800941017     C                   KFLD                    ARBAAS                         *
174900941017     C                   KFLD                    ARBLNP                         *
175000941017     C                   KFLD                    ARBNRS                         *
175100941017     C                   KFLD                    ARBNSP                         *
175200081014     C     KARB          KLIST                                                  *
175300081014     C                   KFLD                    ARtAAS                         *
175400081014     C                   KFLD                    ARtLNP                         *
175500081014     C                   KFLD                    ARtNRS                         *
175600081014     C                   KFLD                    ARtNSP                         *
175700040603     C     Kar5          KLIST                                                  *
175800040603     C                   KFLD                    ARBAAS                         *
175900040603     C                   KFLD                    ARBLNP                         *
176000040603     C                   KFLD                    ARBNRS                         *
176100040603     C                   KFLD                    ARBNSP                         *
176200040603     C                   KFLD                    ktrd                           *
176300970721     C     KGCA          KLIST                                                  *
176400970721     C                   KFLD                    ARBAAS                         *
176500970721     C                   KFLD                    ARBLNP                         *
176600970721     C                   KFLD                    ARBNRS                         *
176700970721     C                   KFLD                    ARBNSP                         *
176800990212     C                   KFLD                    KFRG
176900950810     C     KSAV          KLIST                                                  *
177000950810     C                   KFLD                    SAVAAS                         *
177100950810     C                   KFLD                    SAVLNP                         *
177200950810     C                   KFLD                    SAVNRS                         *
177300950810     C                   KFLD                    SAVNSP                         *
177400941017     C     KLBL          KLIST                                                  *
177500941017     C                   KFLD                    LBLAAN                         *
177600941017     C                   KFLD                    LBLLPN                         *
177700941017     C                   KFLD                    LBLNRN                         *
177800941017     C                   KFLD                    LBLNSN                         *
177900941017     C     KTAB          KLIST
178000941017     C                   KFLD                    CODUT
178100941017     C                   KFLD                    COD
178200941017     C                   KFLD                    KEY
178300050510     C     KTAB2         KLIST
178400050510     C                   KFLD                    CODUT
178500050510     C                   KFLD                    COD
178600990212     C     KDCD          KLIST
178700990212     C                   KFLD                    DCDAAC                         *
178800990212     C                   KFLD                    DCDFIL                         *
178900990212     C                   KFLD                    DCDNCA                         *
179000021203
179100021203     C     Kfapd         klist
179200021203     C                   kfld                    kpdtip
179300021203     C                   kfld                    kpdpdr
179400941017     C*
179500970721     C     *LIKE         DEFINE    GCPFRG        KFRG
179600970721     C                   MOVEL     0             KFRG
179700941017     C     *LIKE         DEFINE    TBLCOD        COD
179800941017     C     *LIKE         DEFINE    TBLKEY        KEY
179900941019     C     *LIKE         DEFINE    BRVNPG        IMANPG                         CATEGORIA FOGLIO
180000070418     C     *LIKE         DEFINE    BRVNPG        conNPG                         CATEGORIA FOGLIO
180100031218     C     *LIKE         DEFINE    BRVNFV        WNFV
180200031218     C     *LIKE         DEFINE    FVVNFV        WNFV5
180300941018     C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO VIAGGIO
180400941018     C     *LIKE         DEFINE    ARTDAM        STADAM                         DATA ARRIVO MERCE
180500941018     C     *LIKE         DEFINE    ARBSPE        COMSPE
180600941019     C     *LIKE         DEFINE    ARBLNA        KLNA                           LINEA ARRIVO
180700031218     C     *LIKE         DEFINE    FVVFGS        SAVFGS
180800950317RM*  C     *LIKE         DEFINE    FVVDFV        SAVDFV
180900941017     C*
181000101220      /free
181100101220       // Carico le categorie con ambito Arrivo
181200101220       xx=2   ;
181300101220       // Alla posizine 1 metto al defluenza
181400101220       npgarr(1)='1D'  ;
181500101220
181600101220       setll   (1:'7N') tabel00f   ;
181700101220       reade   (1:'7N') tabel00f   ;
181800101220
181900101220       dow not %eof(tabel00f)      ;
182000101220       if tblflg=' '               ;
182100101220       ds7n=tbluni                 ;
182200101220
182300101220       if §7ntbo<>' '              ;
182400101220
182500101220          if §7ntbo='A'               ;
182600101220          npgArr(xx)=tblkey;
182700101220          xx=xx+1  ;
182800101220          endif  ;
182900101220       else   ;
183000101220
183100101220         if  §7ntb1='A'                         ;
183200101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc1   ;
183300101220         xx=xx+1  ;
183400101220         endif                                  ;
183500101220         if  §7ntb2='A'                         ;
183600101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc2   ;
183700101220         xx=xx+1  ;
183800101220         endif                                  ;
183900101220         if  §7ntb3='A'                         ;
184000101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc3   ;
184100101220         xx=xx+1  ;
184200101220         endif                                  ;
184300101220         if  §7ntb4='A'                         ;
184400101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc4   ;
184500101220         xx=xx+1  ;
184600101220         endif                                  ;
184700101220         if  §7ntb5='A'                         ;
184800101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc5   ;
184900101220         xx=xx+1  ;
185000101220         endif                                  ;
185100101220         if  §7ntb6='A'                         ;
185200101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc6   ;
185300101220         xx=xx+1  ;
185400101220         endif                                  ;
185500101220
185600101220       endif                       ;
185700101220       endif                       ;
185800101220
185900101220       reade   (1:'7N') tabel00f   ;
186000101220       enddo                       ;
186100101220      /end-free
186200101220
186300941017     C                   ENDSR
186400101202
186500101202      *---------------------------------------------------------------*
186600101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
186700101202      *---------------------------------------------------------------*
186800101202     c     sr_DatiJob    BEGSR
186900101202      *
187000101202     c     *dtaara       define    §azute        AZUTEds
187100101202     c     *dtaara       define    §datiute      dDatiUte
187200101202      *
187300101202     c                   in(E)     *dtaara
187400101202     c                   IF        %Error or RSUT = *blanks
187500101202     c                   clear                   TIBS34ds
187600101202     c                   call      'TIBS34R'
187700101202     c                   parm                    TIBS34ds
187800101202     c                   in        *dtaara
187900101202     c                   ENDIF
188000101202      *
188100101202     c                   ENDSR
188200941017     C**************************************************************************
188300161028     Otigcp000  E            AGG5
188400161017     O                       GCPDLMa
188500950316RM*  OPRTF198   E            TES              02
188600911015     O                       STA(1)              66
188700950810     O              N10      STA(2)             132
188800161017     O               10N11   STA(18)            132
188900170124     O*********      10 11   STA(22)            132
189000950320     O                       STA(17)            198
189100900626     O                       RSUT                20
189200960927     O                       UDATE8             181 '  /  /    '
189300950320     O                       PAGE          Z    198
189400920813     O          E            TES         1
189500160503     o                       statipo             67
189600160503     o                       statipo2           130
189700050511     O                       UTIME              179 '  :  :  '
189800931228     O               02      STA(4)              70
189900931228     O          E    02      TES         0  0
190000931228     O                       STA(4)              70
190100931228     O          E            TES         2
190200911015     O                       STA(3)              66
190300931228     O                       STA(16)            132
190400060508     O                       STA(20)            198
190500161018     O              n11                          13 'I.M. .'
190600161018     O              n11      VIDTIN              12
190700170124     O*********      11                          13 'I.P.G.'
190800060508RM*  O                       SAVFGS              21
190900060508     O                       DESCDU              42
191000920813     O                       COMNFV        Z     56
191100950317     O                       STADFV        8     73
191200960927     O                       VIDFIL             116
191300960927     O                       DESFIL             137
191400060508     O                       stalna             177
191500060508     O                       DESlna             198
191600070418     O          E   n10      TES         1
191700070418     O                       STA(21)            198
191800070418     O                       stamdc             188
191900101215     O                       staimaa            146
192000920813     O          E            TES2        2
192100911018     O                       STA(5)              66
192200911018     O                       STA(8)             132
192300040621     O                       STA(19)            198
192400920813     O          E            TES2        1  1
192500911018     O                       STA(6)              66
192600911018     O                       STA(9)             132
192700911015     O*
192800090522     O          E            DETnoBO     1
192900090522     O                       brvLNP        Z      8
193000090522     O                       brvNRS        Z     11
193100090522     O                       brvLNA        Z     29
193200090522     O                       brvZNC              87
193300090522     O                       brvnps             122
193400090522     O                       brvNSC        Z    131
193500090522     O
193600090522     O          E            DET         1
193700090522     O                  05   ARBAAS        Z
193800090522     O                  05   ARBLNP        Z   +  1
193900090522     O                  05 03ARBNRS        Z   +  1
194000090522     O                  05 03                  +  0 '-'
194100090522     O                  05   ARBNSP        Z   +  0
194200090522     O                  05   STADSP            +  1 '  /  '
194300090522     O                  05   ARBLNA        Z   +  1
194400090522     O                  05   STARSM            +  1
194500090522     O                  05   STARSD            +  1
194600090522     O                  05N04ARBNCD        Z   +  1
194700090522     O                  05N04                  +  0 '/'
194800090522     O                  05N04ARBNCA        Z   +  0
194900090522     O                  05 04                    84 'NON SEQUENZIALI'
195000090522     O                  05   ARBZNC            +  1
195100090522     O                  05   stabva            +  0
195200090522     O                  05   ARBTMC            +  1
195300090522     O                  05N06ARBPDC        Z   +  1
195400090522     O                  05N06                  +  0 '-'
195500100707     O                  05   DESCPC            +  0
195600090522     O              N07      STADAM            +  1 '  /  /    '
195700090522     O              N07      ARTNAP            +  1
195800090522     O              N07      ARTNSC        Z   +  2
195900090522     O               07                         131 'ABILIT.MERCE FORZATA'
196000081014     O* ERRORI:
196100950411     O*
196200081014     O          E            DETERR      0  0
196300031219     O                    n13                   155 'NON SPUNTATO IN DATA '
196400031219     O                    n13STADFV        8   +  1
196500950316     O*
196600040621     O                     13brvFGS             137
196700040621     O                     13                   138 '-'
196800040621     O                     13ST2FGS             146
196900161025     O                     13                   149 'in'
197000070118     O                     13ST2CAT             153
197100070118     O                     13brvNPG             155
197200070118     O                     13                   156 '-'
197300070118     O                     13brvNFV        Z    162
197400161025     O                     13                   165 'il'
197500070118     O                     13ST2DFV             171 '  /  '
197600161026     O**                   13                   175 'ore'
197700161026     O                     13staHMS             178 '  :  '
197800161130     O                       STADSF             198
197900070418     O          E            DETndc      1
198000070418     O                                           87 'In distinta:'
198100070418     O                       ARBndc        2  +   1
198200070418     O                                        +   2 'del'
198300070418     O                       staddc            +  1 '  /  /    '
198400081014     o* Collo doppio
198500081014     O          E            DETdoppio   0  0
198600161018     O                                          153 'COLLO   D O P P I O'
198700161018     o* Collo eccedente
198800161018     O          E            DETeccede   0  0
198900161018     O                                          153 'COLLO   E C C E D E'
199000161018     O                                          159 ' N T E'
199100161018     O                       ecce_descr         185
199200100706     o* Collo presente da XX giorni in IMA
199300100706     O          E            DETspuIMA   0  0
199400100707     O                                          160 '   COLLO SPUNTATO PER '
199500100707     O                                        +   1 'ALMENO '
199600100706     O                       §VPOGGIMA     2  +   1
199700100707     O                                        +   1 'GIORNI in I.M.A.'
199800081014     o* riga vuota
199900081014     O          E            DETvuota    2
200000081014     O                                          157 ' '
200100050405     O* STAMPO NUMERO C.A.e ubicazione collo
200200990212     O          E            DETCA       1
200300050405     O               24                          87 'Ubicazione collo:'
200400050405     O               24      §ar5zng           +  2
200500050405     O               25                         138 'C.A.'
200600050405     O               25      DCTFIL            +  1
200700050405     O               25      DCTNCA        Z   +  1
200800050405     O               25                        +  1 'del'
200900050405     O               25      STADCA            +  1 '  /  /  '
201000990212     O                     26                  +  1 'chiusa il'
201100990212     O                     26STACHC            +  1 '  /  /  '
201200990303     O                     26DCTCCH            +  1
201300990303     O                     26                  +  0 '-'
201400990303     O                     26STACCH            +  0
201500920813     O          E            OK          3
201600920813     O                       STA(11)             66
201700920813     O          E            OK          0  0
201800920813     O                       STA(11)             66
201900920813     O*
202000920813     O          E            TOT1        3  1
202100920813     O                       STA(13)             66
202200920813     O          E            TOT2           1
202300920813     O                       NPS(C)              33
202400920813     O                       CPS(C)        2     58
202500920813     O          E            TOT3           1
202600920813     O                       STA(14)             66
202700920813     O          E            TOT3           1
202800920813     O                       STA(15)             66
202900920813     O                       TOTSPU        2     58
203000920813     O*
203100920813     O          E            TOT3        3
203200920813     O                       STA(12)             66
203300920813     O          E            TOT3        0  0
203400920813     O                       STA(12)             66
203500941017     O**************************************************************************
203600950316** STA
203700060508XXXXXXXXXXXXXXXXXXXX                                                 1
203800950320 ** I.M.A. : INVENTARIO MAGAZZINO ARRIVI **                          2
203900161018FOGLIO        di  XXX-XXXXXXXXXXXXXXXXXXXX    N.ro XXXXX   del XX/   3
204000931228                                       *** D E F I N I T I V O ***   4
204100040603    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO        5
204200941019ANNO LNP SER NUMERO  DATA ARR                                        6
204300040603XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx  7
204400040603     SEGNACOLLI    ZON ULT  AUTOTRASPORT.   A R R I V O SEGNACOLLO   8
204500040603      DAL /   AL   ARR CONS   CONSEGNA       DATA   PIST MANCANTE    9
204600040603XX XXXXXXX/XXXXXXX XXV X XXXXXXX-XXXXXXXXX XX/XX/XXXX XX  XXXXXXX   10
204700920813             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
204800920813 **  FINE  STAMPA  **
204900920813   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI      13
205000920813                        ---------------------------------------     14
205100920813               TOTALE                                               15
205200060508XX/XXXX           Punto Operativo in Gestione: XXX-xxxxxxxxxxxxxxx  16
205300950320                         FNLR31R                          PAG.XXXX  17
205400950810 ** I.M.G. : INVENTARIO MAGAZZINO GIACENZE **                       18
205500040621 S  P  U  N  T  A  T  O      D  A                                   19
205600060508xxxxx           Linea Arrivo Selezionata: XXX-xxxxxxxxxxxxxxxxxxxx  20
205700070418                Elabora anche spedizioni in distinta: XX            21
205800161031 ** I.P.G. : QUADRATURA COLLI A MAGAZZINO GIACENZE **               22
205900161017**
206000950316OVRPRTF FILE(PRTF198) USRDTA('I.M.A.')
206100161017**
206200161017DLYJOB DLY(01)
