000100040603     H DECEDIT('0,') DATEDIT(*ymD.)
000200950711     H* FNLR31R *-----------------------------------------------------*
000300170330     H*        - INVENTARIO MAGAZZINO  I.M.A. - I.M.G          Esegue
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
003700170331     D zone            S              5    DIM(200)                             COMANDO QCMDEXC
003800170331     D zone12          S              5    DIM(200)                             COMANDO QCMDEXC
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
011300170512     D TRULTASDS     E DS
011400170512     D og148         E DS
011500170323     D og150         E DS
011600170213     D DS7B          E DS
011700161017     D DS7C          E DS
011800101220     D DS7N          E DS
011900920318     D DS3A          E DS
012000050511     D DS03          E DS
012100040603     D DAR5GEN       E DS
012200100706     D DVPO          E DS
012300990212     D DTAD            DS
012400990212     D  §TARGR                26     26
012500990303     D DCCH            DS
012600990303     D  §CHDES                 1     50
012700990303     D  §CHIMA                53     53
012800070118     D RECBRV        E DS                  EXTNAME(FNBRV00F)
012900070118     D WSAVB         E DS                  EXTNAME(FNBRV00F)
013000031219     d                                     prefix(S_)
013100170329     D WSAVr         E DS                  EXTNAME(FNBRV00F)
013200170329     d                                     prefix(r_)
013300990212     D DSBS02        E DS                  EXTNAME(TIBS02DS)
013400160927     D tigcp00ds     E DS                  EXTNAME(tigcp00F)
013500040427      * DS Esterna x gestione allocazione ed invio messaggi
013600040427     D TRUL82DS      E DS
013700081014      * DS Esterna x controllo colli doppi
013800081014     D FNLRV7DS      E DS
013900160927      * DS Esterna x controllo colli in IMG
014000160927     D FNLGI1DS      E DS
014100040806      * ds per il controllo della presenza di CA per la spedizione richiesta
014200040806     d FIDN12DS      e ds
014300040806     d  skKey                 26    305    dim(20)
014400040806     d  skAnn                306    325    dim(20)
014500040806     d  skDca                326    485  0 dim(20)
014600101202     d**skFca                486    545  0 dim(20)
014700040806     d  skDch                546    705  0 dim(20)
014800040806     d  skCch                706    745    dim(20)
014900101202      *
015000040806     d dsKey           ds
015100040806     d  dsaac                         4  0
015200040806     d  dsfil                         3  0
015300040806     d  dsnca                         7  0
015400040806      *
015500101202      *
015600021203      *
015700101220     d WnpgArr         s              2
015800170330     d Wnpg            s                   like(brvnpg)
015900101220     d Trov            s              2  0
016000090522     d Nobolla         S              1
016100170330     d wstampa         S              1
016200170125     d winGiac         S              1
016300090522     d uno             S              1
016400081014     d statipo         S             21
016500160503     d statipo2        S             21
016600101215     d staimaa         S             44
016700070418     d stamdc          S              2
016800160503     d indMP           S              4  0
016900160503     d ind12           S              4  0
017000170213     d iddgdopo        S              1    inz
017100170323     d picking         S              1    inz
017200050511     d WSPG            S                   LIKE(FVVSPG)
017300040108     d kpdpdr          S                   LIKE(APDpdr)
017400031219     d wdfv            S                   LIKE(d53dfv)
017500031219     d kpdtip          S                   LIKE(APDtip)
017600040603     d ktrd            S                   LIKE(ar5trd) inz('GEN')
017700161018     d ecce_descr      S             25
017800100706     d dataEUR         s               d   datfmt(*eur)
017900100706     d DataISO         s               d   datfmt(*iso)
018000101202     d*
018100040428     D msgjob          C                   CONST('La Manut.Bolle sta bloccando -
018200040428     D                                     l''elabor. Dell''IMA:USCIRE SUBITO D-
018300040428     D                                     A QUESTO LAVORO')
018400050511     D CPARCEL         C                   CONST('     P A R C E L     ')
018500050511     D CMessag         C                   CONST('M E S S A G G E R I E')
018600160503     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
018700160503     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
018800101215     D CsoloLNA        C                   CONST('Solo colli spuntati alla line-
018900101215     d                                     a arrivo finale')
019000911018     C*--------------------------------------------------------------- *
019100911018     C*                  INDICATORI                                    *
019200911018     C*--------------------------------------------------------------- *
019300911018     C* 01    - ESITONO ANOMALIE DI MANCANZA
019400950810     C* 02    - CHIESTA CHIUSURA  FOGLIO
019500911018     C* 03    - ESISTE SERIE
019600911018     C* 04    - SEGNACOLLI NON SEQUENZIALI
019700911018     C* 05    - ON CAMBIATA SPEDIZIONE
019800920325     C* 06    - NON C'E' COD PADRONCINO CONSEGNA
019900920708     C* 07    - MANCA DATA ARRIVO : SPEDIZIONE FORZATA
020000920813     C* 08    - BOLLA GESTITA
020100170124     C* 11    - e 10 ON --> IPG  ma ora eliminato
020200950810     C* 10    - ON --> IMG  OFF --> IMA
020300081014     C* 12    - CONTROLLO COLLI DOPPI
020400950810     C* 13    - NON TROVATA SPUNTA IN IMA MA  IN ALTRO FOGLIO NELLA
020500950810     C*         STESSA DATA
020600950810     C* 14    - NON TROVATA SPUNTA IN IMA
020700031219     C* 15    - TROVATA SPUNTA SU ALTRO FOGLIO MA DEVO CREARE L'ANOMAL
020800100706     C* 16    - collo in IMA ma spuntato in IMA più di XX volte...da tabella VPO
020900161018     C* 18    - collo eccedente
021000170124     C* 22    - FILIALE partita con nuova quadratura IMA (e non più IPG)
021100170323     c*         e non ha il picking la nuova procedura non può essere fatta quindi
021200170323     c*         l'indicatore viene sempre spento
021300050405     C* 24    - STAMPA ubicazione collo se c'e'
021400050405     C* 25    - STAMPA se c'e' c.a.
021500050405     C* 26    - STAMPARE DATA CHIUSURA C.A.
021600941017     C* 30    - LETTURA FNARB
021700941017     C* 31    - LETTURA FNART
021800911018     C* 32/34 - COMODO
021900911018     C*---------------------------------------------------------------*
022000900622     C     *ENTRY        PLIST
022100900622     C                   PARM                    KPJBA
022200941017     C                   MOVEL     KPJBU         PARAM1
022300000126     C                   Z-ADD     COMNFV        WNFV
022400900828     C*-----------------------------------------------------
022500950810     C* IMPOSTAZIONI INIZIALI
022600950810     C                   EXSR      IMPO
022700950810     C*-----------------------------------------------------
022800170124     c                   setoff                                       11
022900161018     c* il dly mi serve solo per il debug in batch
023000161017     C                   Z-ADD     80            LUNG             15 5
023100161017     C                   MOVEL     *BLANKS       COMMAN           80
023200161017     C                   MOVEA     VIDTIN        CMD(35)
023300161017     C                   MOVEA     CMDd          COMMAN
023400161017     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
023500161017     C                   PARM                    COMMAN
023600161017     C                   PARM                    LUNG
023700161017
023800941019     C                   Z-ADD     80            LUNG             15 5
023900941019     C                   MOVEL     *BLANKS       COMMAN           80
024000950810     C                   MOVEA     VIDTIN        CMD(35)
024100941019     C                   MOVEA     CMD           COMMAN
024200170124     c**                 if        *in11
024300170124     c**                 eval      %subst(comman:31:6)='I.P.G.'
024400170124     c**                 endif
024500941019     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
024600941019     C                   PARM                    COMMAN
024700941019     C                   PARM                    LUNG
024800950810     C*
024900950316RM*  C                   OPEN      PRTF198
025000980128     C                   SETON                                        OF
025100911015     C*
025200040108     C* CONSIDERO SOLO i p.o. di £6
025300941019     C                   Z-ADD     1             WA                3 0          CONTATORE
025400040108    1C     L6(WA)        DOWNE     *ZERO
025500941019     C*
025600040108     C                   Z-ADD     L6(WA)        KLNA                           LINEA ARRIVO
025700161017     C*
025800031218     C     KLNA          SETLL     FNARB05L
025900040427     C     KLNA          READE     FNARB05L                               30
026000950810     C*
026100950810    3C     *IN30         DOWEQ     *OFF
026200050511     c* Se ima messaggerie o parcel, escludo le zone non di interesse
026300160503     c                   eval      indMP=0
026400160503     c                   eval      ind12=0
026500160503     c
026600160503     c                   if        wselzona='S'
026700170331     c
026800170331     c* Eseguo la ricerca per linea/zona per tenere in considerazione le filiali in £6
026900170331     c                   movel     arblna        alfzona           5
027000170331     c                   move      arbznc        alfzona
027100160503
027200160503     c                   if        wparc='S' or wmess='S'
027300160503     c***  alfzona       lookup    zone                                   35
027400160503     c                   eval      indMP =%lookup(alfzona:zone)
027500050511     c                   endif
027600160503     c                   if        wparc1='S' or wparc2='S'
027700160503     c                   eval      ind12 =%lookup(alfzona:zone12)
027800160503     c                   endif
027900160503
028000160503     c                   endif
028100160503   3ac                   if        wselzona=' ' or  indmp>0 or  ind12>0
028200920813     C*
028300920318     C* CHAIN CODICE BOLLE PER ESCLUDERE I RECUPERI
028400920318     C                   MOVEL     '3A'          COD
028500950810     C                   MOVEL(P)  ARBCBO        KEY
028600941018     C     KTAB          CHAIN     TABEL00F                           35
028700920318     C  N35              MOVEL     TBLUNI        DS3A
028800920318     C   35              MOVEL     'F1'          §3ATB1
028900920318     C*
029000950810     C* ESCLUDO:  LE BLOCCATE CON "B"/
029100930518     C*           QUELLE IN CONSEGNA / QUELLE CONSEGNATE / I RECUPERI
029200161025    4C     ARBdcm        ifeq      *zeros
029300950810     C     §3ARBL        ANDNE     'R'
029400950810     C     ARBFBC        ANDNE     'B'
029500000512     C*
029600960516     C* CONTROLLO SE E' GIACENTE
029700960516     C                   CLEAR                   WGIAC
029800960516     C     ARBFBC        IFEQ      'G'
029900161025     C     KGCA          CHAIN(n)  Tigcp21L                           35
030000970721     C* SE C'E' LA GIACENZA E NON E' A FASE 35 LA ESCLUDO
030100970721     C  N35GCPFAS        IFLT      35
030200050331     C     GCPFAS        oreq      36
030300170124     c* la merce è in giacenza : verifico con driver
030400160927     c                   clear                   fnlgi1ds
030500160927     c                   eval      ILGI1AAS=arbaas
030600160927     c                   eval      ILGI1lnp=arblnp
030700160927     c                   eval      ILGI1nrs=arbnrs
030800160927     c                   eval      ILGI1nsp=arbnsp
030900161018    3c                   select
031000161018     c                   when      arbpkc>0 and arbncp=arbncl
031100161018     c                   eval      ilgi1pkf=arbpkc
031200161018     c                   when      arbpkc>0 and arbncp<arbncl
031300161018     c                             and arbpkc>arbpkf
031400161018     c                   eval      ilgi1pkf=arbpkc
031500161018     c                   other
031600161018     c                   eval      ilgi1pkf=arbpkf
031700161018    3c                   endsl
031800170323     c                   movel     vidfil        ilgi1fgs
031900170323     c
032000170323     c* visto che l'ho già calcolato, lo passo al driver
032100170323     c                   if        picking<>' '
032200170323     c                   movel     picking       ILGI1PICK
032300170323     c                   else
032400170323     c                   movel     'N'           ILGI1PICK
032500170323     c                   endif
032600170323     c
032700160927     c                   movel     fnlgi1ds      kpjbu
032800160927     c                   call      'FNLGI1R'
032900160927     c                   parm                    kpjba
033000160927     c                   parm                    tigcp00ds
033100161007     c                   movel     kpjbu         fnlgi1ds
033200160927
033300170125     c* per IMG: sped portata nel giorno a mag. giac DEVE essere in giacenza
033400170125     c*          e quindi elaborata con mancanza
033500170125   5ac   10              if        olgi1AMG='Y' or olgi1inmg='S'
033600970721     C                   MOVEL     'S'           WGIAC             1
033700160823     c                   endif
033800170125   5ac   10              if        olgi1AMG='S'
033900170125     C                   MOVEL     'D'           WGIAC             1
034000170125     c                   endif
034100170125     c* per IMA: devo elaborare sped da portare a mag giac o portate nel giorno
034200161024     c* se la bolla è da portare a mag giacenza OGGI, devo considerarla anche in IMA
034300161024     c*  e se manca dare segnalazione ma NON creando l'anomalia
034400170125   5ac  n10              if        olgi1AMG='S' or olgi1AMG='Y'
034500161024     C                   MOVEL     'D'           WGIAC             1
034600161024     c                   endif
034700170125   5ac  n10              if        olgi1inmg='S'
034800170125     C                   MOVEL     'S'           WGIAC             1
034900170125     c                   endif
035000161201     c*
035100170124     c* se non è partito con NEW IMA considero  la spedizione GIACENTE  come prima
035200170201   5ac*****              if        not *in22 and olgi1AMG='S'
035300170201   5ac                   if        not *in22 and WGIAC='D'
035400161201     C                   MOVEL     'S'           WGIAC             1
035500161201     c                   endif
035600161201     c
035700170124     c* IPG elaboro le giacenze da portare a mag giac OGGI ( non facciamo più)
035800170124   5ac** 11              if        olgi1AMG='S' or olgi1AMG='Y'
035900170124     C**                 MOVEL     'D'           WGIAC             1
036000170124     c**                 endif
036100160823     c
036200960516     C                   ENDIF
036300970721     C                   ENDIF
036400950810     C*
036500161025     C* SE IMA --> ESCLUDO LE GIACENTI ma includo le bolle che devono essere portate a mag giac
036600170124     c*            se partita la procedura NEWIMA
036700950810    5C     *IN10         IFEQ      *OFF
036800960516     C     WGIAC         ANDNE     'S'
036900170124     c     *in22         andeq     *on
037000170124     c***  *in20         andeq     *on
037100161130
037200161130    5C     *IN10         orEQ      *OFF
037300161130     C     WGIAC         ANDeq     ' '
037400170124     c     *in22         andeq     *off
037500170124     c**** *in20         andeq     *off
037600950810     C*
037700950810     C* SE IMG --> INCLUDO SOLO LE GIACENTI
037800161026     C*        --> INCLUDO anche le bolle da portare a mag.giac. ma senza stampare il mancante
037900950810     C     *IN10         OREQ      *ON
038000161025     C     *IN11         andeq     *Off
038100161026     C     WGIAC         ANDne     ' '
038200161025
038300170124     C* SE IPG --> INCLUDO SOLO quelle da portare a mag giac ( eliminato)
038400170124     C**   *IN10         OREQ      *ON
038500170124     C**   *IN11         andeq     *ON
038600170124     C**   WGIAC         ANDEQ     'D'
038700921013     C*
038800950317     C* ESCLUDO  SE BOLLA MAMMA E UNA DELLE FIGLIE E' IN CONSEGNA
038900960516     C*     O SE SONO TUTTE BLOCCATE
039000950810     C                   Z-ADD     ARBSPE        SAVSPE
039100960516     C*
039200960516     C                   CLEAR                   WESFI                          ESISTE FIGL
039300960516     C                   MOVEL     'S'           WFIBLO            1            FIGLIE BLOCC
039400950317     C                   SETOFF                                       36
039500950810     C     KSAV          SETLL     FNLBL02L
039600950810     C     KSAV          READE     FNLBL02L                               35
039700960516     C**
039800950810    6C     *IN35         DOWEQ     *OFF
039900950317     C*
040000950317     C     KLBL          CHAIN     FNARB01L                           35
040100950810    7C     *IN35         IFEQ      *OFF
040200960516     C                   MOVEL     'S'           WESFI             1
040300960516     C* FIGLIA IN CONSEGNA
040400960516    8C     ARBNDC        IFNE      0
040500950317     C                   SETON                                        3536
040600960516   X8C                   ELSE
040700960516     C*
040800960516     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
040900960516    9C     ARBFBC        IFNE      'B'
041000960516     C                   MOVEL     'N'           WFIBLO
041100960516    9C                   ENDIF
041200960516    8C                   ENDIF
041300960516    7C                   ENDIF
041400950317     C*
041500950810     C  N36KSAV          READE     FNLBL02L                               35
041600950810    6C                   ENDDO
041700960516     C*
041800960516     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
041900960516     C  N36WESFI         IFEQ      'S'
042000960516     C     WFIBLO        ANDEQ     'S'
042100960516     C                   SETON                                        36
042200960516     C                   ENDIF
042300070418     c
042400070418     c* Per 36 off - controllo in IMA se richiesti anche i colli mancanti
042500070418     c*              di spedizioni in distinta
042600070418     c  n36              if        not *in10 and vidmdc='N' and arbndc>0
042700070418     c                   seton                                        36
042800070418     c                   endif
042900911204     C*
043000950810    6C  N36              DO
043100950317     C*
043200040427     C     KSAV          CHAIN     FNARB01L                           31
043300950810     C                   MOVEL     ARBAMA        SAVAMA            1 0
043400040428     c                   clear                   wchain            1
043500161025     c                   clear                   wesspu            1
043600911018     C* LEGGO DETTAGLIO
043700941017     C     KART          SETLL     FNART01L
043800040428     C  N10
043900040428     CAN 02KART          READE     FNART01L                               31
044000040428     C   10
044100040428     CORN02KART          READE(N)  FNART01L                               31
044200911018     C*
044300950810    7C     *IN31         DOWEQ     *OFF
044400911204     C                   SETOFF                                       34
044500950810     C*
044600950810     C* ESCLUDO  SE IL COLLO E' CONSEGNATO (CONSEGNA PARZIALE)
044700070418     C     ARTDCM        IFEQ      0
044800950810     C  N10              EXSR      ELAIMA
044900950810     C   10              EXSR      ELAIMG
045000950810    8C                   ENDIF
045100930629     C*
045200040428     C  N10
045300040428     CAN 02KART          READE     FNART01L                               31
045400040428     C   10
045500040428     CORN02KART          READE(N)  FNART01L                               31
045600950810    7C                   ENDDO
045700170124     c* per IMA  e IMG aggiorno la giacenza con la data entrata a mag giac se spuntato
045800170124     c*       (IPG viene eliminato quindi uso ima al suo posto)
045900170125     c*  almeno un collo e data = 0  solo in fase "definitivo"
046000170321     c* per le filiali che fanno l'IDD il giorno dopo, aggiorno anche per ima provvisorio
046100170125     c*
046200170124    7c*****              if        *in22 and *in10 and wesspu='S' and
046300170321    7c*****              if        *in22 and *in02 and wesspu='S' and
046400170321     c
046500170321    7c                   if        *in22 and wgiac<>' ' and wesspu='S' and
046600161025     C                             olgi1amg='S'
046700170321     c                   if        *in02 or iddgdopo='S'
046800161025     C     KGCA          CHAIN     Tigcp21L
046900161025    8c                   if        %found(tigcp21l)
047000161025     C                   MOVE      dateu         GCPDLMa
047100161025     C                   EXCEPT    AGG5
047200170512     c* richiamo pgm per attribuire competenze aut
047300170512     c                   clear                   trultasds
047400170512     c                   eval      TTASAAS=arbaas
047500170512     c                   eval      TTASlnp=arblnp
047600170512     c                   eval      TTASnrs=arbnrs
047700170512     c                   eval      TTASnsp=arbnsp
047800170512     c                   movel     trulTASds     KPJBU
047900170512     c                   call      'TRULTASR'
048000170512     c                   parm                    kpjba
048100161025    8C                   ENDIF
048200170321     c
048300161025    7c                   endif
048400170321    7c                   endif
048500920318     C*
048600950810    6C                   ENDDO
048700950810    5C                   ENDIF
048800161025     c
048900000512    4C                   ENDIF
049000050511   3aC                   ENDIF
049100911018     C*
049200040427     C     KLNA          READE     FNARB05L                               30
049300950810    3C                   ENDDO
049400941019     C*
049500040108     C                   ADD       1             WA                             CONTATORE L6
049600950810    1C                   ENDDO
049700161017     c
049800920813     C*
049900920813     C* RIEPILOGO SPUNTE PER PISTOLA
050000920813     C                   EXSR      RIEPIL
050100911018     C*
050200060508     C* SE NON TROVATE DISCORDANZE e non fatte parzializzazioni
050300060508     c*  se non ho parzii
050400070418     c* 18/04/07 - IMA chiude solo se richiesta forzatura chiusura
050500960530     C     *IN01         IFEQ      *OFF
050600070418     C     *IN10         andeq     *ON
050700161017     C     *IN11         andeq     *Off
050800060508     c
050900160503     c     wselzona      andeq     ' '
051000060508     c
051100060508     c     vidlna        andeq     0
051200060508     c
051300060508     C*  O FORZATA LA CHIUSURA METTO FLAG
051400911018     C     VIDSN         OREQ      'S'
051500060508     c
051600941017     C     KFVV          CHAIN     FNFVV01L                           32
051700920813     C  N32              MOVEL     'S'           FVVFCF
051800941017     C  N32              UPDATE    FNFVV000
051900911018     C                   END
052000050511     C*
052100081014     C* CHIUSURA PGM
052200990212     C                   CLEAR                   DSBS02
052300990212     C                   MOVEL     'C'           T02TLA
052400990212     C                   CALL      'TIBS02R'
052500990212     C                   PARM                    KPJBA
052600990212     C                   PARM                    DSBS02
052700031219     c
052800031219     C                   CLEAR                   DSLV53
052900031219     C                   MOVEL     'C'           D53TLA
053000031219     C                   CALL      'FNLV53R'
053100031219     C                   PARM                    DSLV53
053200040806     C*
053300040806     C                   MOVEL     'C'           I12TLA
053400040806     C                   CALL      'FIDN12R'
053500040806     C                   PARM                    FIDN12DS
053600140430     C*
053700140430     c                   clear                   fnlrv7ds
053800140430     C                   MOVEL     'C'           ILRV7TLA
053900140430     c                   eval      kpjbu= fnlrv7ds
054000140430     C                   CALL      'FNLRV7R'
054100140430     C                   PARM                    KPJBA
054200160927
054300160927     c                   clear                   fnlgi1ds
054400160927     c                   eval      ILGI1TLA= 'C'
054500160927     c                   movel     fnlgi1ds      kpjbu
054600160927     c                   call      'FNLGI1R'
054700160927     c                   parm                    kpjba
054800160927
054900040806      *
055000000000     C                   SETON                                        LR
055100031219     C**************************************************************************
055200031219     C* CONTROLLO SE SEGNACOLLO MANCANTE
055300031219     C**************************************************************************
055400031219     C     CTRART        BEGSR
055500031219     C*
055600031219     C                   SETOFF                                       131415
055700100706     C                   SETOFF                                       3316
055800161025     C                   MOVE      *BLANKS       STADSF           17
055900031219     C*
056000070118     C     KBRV1         SETLL     FNBRV01L                               33
056100070418     c
056200161017     c* se non ho trovato spunta IMx e spedizione in distinta, cerco
056300070418     c*  la categoria 4 del giorno
056400070418    1c                   if        not *in33 and arbndc>0 and vidmdc='S'
056500100706     C                   Z-ADD     4             conNPG
056600140618     C     KBRV4         chain     FNBRV05L
056700140618    2c                   if        %found(fnbrv05l)
056800070418     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
056900070418     c                   clear                   dslv53
057000070418     C                   z-add     brvnFV        D53NFV
057100070418     C                   MOVEL     brvNPG        D53NPG
057200070418     C                   MOVEL     brvFGS        D53FGS
057300070418     C                   CALL      'FNLV53R'
057400070418     C                   PARM                    DSLV53
057500070418     C* ERRORE
057600070418     c*  se sparato nella stessa data: ok
057700070418     c*  e sparato in altra data, rientro nei giri normali di controllo
057800070418     c*  spunte
057900070418    3c                   if        d53err=' ' and d53dfv=savdfv
058000070418     c                   seton                                        33
058100070418    3c                   endif
058200070418    2c                   endif
058300070418    1c                   endif
058400070418     c
058500031219     C* 14 ON - SPUNTA NON TROVATA
058600031219    1C     *IN33         IFEQ      *OFF
058700070418     c
058800031219     C                   SETON                                        14
058900031219     c*
059000031219     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
059100031219     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
059200031219     c*  immissione + alta
059300031219     C                   exsr      RICSPU
059400031219     C
059500031219     C* SE SPUNTA TROVATA CON DATA > STAMPO MA NON CREO ANOMALIA
059600170213     c
059700170213     c* Se filiale che fa l'IDD il giorno dopo e trovo spunta IMA con data >
059800170213     c*  non stampo nemmeno e dico che la spunta è presente
059900170213     c*  non è detto che questo vada bene per la nuova procedura IMG. Da verificare con
060000170213     c*  DLO (es Salerno)
060100170213    2c                   if        not *in10 and iddgdopo='S' and
060200170213     c                             wdfv>savdfv and wspg='A'
060300170213     c                   setoff                                       14
060400170213   x2c                   else
060500170213     c
060600170213    3C                   IF        WDFV>SAVDFV
060700031219     C                   EVAL      *IN13=*ON
060800170213    3C                   ENDIF
060900031219     C
061000031219     C* SE SPUNTA TROVATA CON DATA = STAMPO E CREO ANOMALIA SOLTANTO SE
061100031219     C*  CATEGORIA 2 O 6
061200170213    3C                   IF        WDFV=SAVDFV
061300031219     C                   EVAL      *IN13=*ON
061400170213    4C                   IF        BRVNPG=2
061500031219     C                   EVAL      *IN15=*ON
061600170213    4C                   ENDIF
061700170213    3C                   ENDIF
061800170213    2C                   ENDIF
061900031219     c*
062000031219    1C                   END
062100960523     C*
062200031219     C
062300031219    1C                   IF        WDFV>=SAVDFV
062400960523     C* GIRO LA DATA
062500031219     C                   MOVE      WDFV          G02INV
062600960523     C                   MOVE      *ZEROS        G02DAT
062700960523     C                   MOVEL     3             G02ERR
062800960523     C                   CALL      'XSRDA8'
062900960523     C                   PARM                    WLBDAT
063000960523     C                   MOVEL     G02DAT        ST2DFV            4 0
063100960523     C**
063200960523     C* DECODIFICO LA FILIALE GESTIONE
063300031219     C     BRVFGS        CHAIN     AZORG01L                           32
063400031219    2C  N32ORGDE5        IFNE      *BLANKS
063500960523     C                   MOVEL     ORGDE5        ST2FGS            8
063600960523     C                   ELSE
063700960523     C                   MOVEL     ORGDES        ST2FGS            8
063800031219    2C                   ENDIF
063900960523     C**
064000960523     C* DECODIFICO LA CATEGORIA SPUNTA
064100031219    2C     BRVNPG        IFEQ      3
064200161017     C                   MOVEL     '7B'          COD
064300161017     C                   MOVEL(P)  wspg          KEY
064400161017     C     KTAB          CHAIN     TABEL                              32
064500161017     C  N32              MOVEL     TBLUNI        ds7b
064600161017     C   32              CLEAR                   ds7b
064700161017     C                   MOVEL     §7bdesb       ST2CAT            3
064800031219   X2C                   ELSE
064900960523     C                   MOVEL     '7N'          COD
065000031219     C                   MOVEL(P)  BRVNPG        KEY
065100960523     C     KTAB          CHAIN     TABEL                              32
065200960523     C  N32              MOVEL     TBLUNI        ST2CAT
065300960523     C   32              CLEAR                   ST2CAT
065400031219    2C                   ENDIF
065500031219     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
065600031219    2C                   IF        BRVNPG=4
065700031219     C                   Z-ADD     BRVNFV        WNFV5
065800031219     C     KFVV2         CHAIN(N)  FNFVV000                           32
065900161026     C  N32              eval      stadsf=fvvdsf
066000031219    2C                   ENDIF
066100031219    1C                   ENDIF
066200960523     C*
066300100706     c* Se trovata spunta, segnalo se sparata xx volte in IMA
066400100706     c*  solo per bolle in IMA più vecchie di ....
066500100707     c* Escludo bolle in distinta
066600150113     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
066700100707     c                   if        not *in14 and not *in10
066800100706     c                             and arbndc=0 and arbdsp<datapresIMA
066900100706     c
067000100706     c* conto quante volte presente in ima
067100100706     C                   Z-ADD     3             conNPG
067200100706     c                   clear                   conta             4 0
067300140618     C     KBRV4         setll     FNBRV05L
067400140618     C     KBRV4         reade     FNBRV05L
067500100706     c
067600140618    2c                   dow       not %eof(fnbrv05l)
067700100706     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
067800100706     c                   clear                   dslv53
067900100706     C                   z-add     brvnFV        D53NFV
068000100706     C                   MOVEL     brvNPG        D53NPG
068100100706     C                   MOVEL     brvFGS        D53FGS
068200100706     C                   CALL      'FNLV53R'
068300100706     C                   PARM                    DSLV53
068400100706     c
068500100706     c                   if        d53spg='A'
068600100706     c                   eval      conta=conta+1
068700100706     c                   endif
068800100706     c
068900140618     C     KBRV4         reade     FNBRV05L
069000100706     c                   enddo
069100100706     c
069200100706     c                   if        conta>=§vpoggima
069300100706     c                   seton                                        16
069400100706     c                   endif
069500100706     c
069600100706     c
069700100706     c                   endif
069800100706     c
069900960523     C                   ENDSR
070000960523     C*
070100031219     C**************************************************************************
070200031219     C* cerco spunta del collo in qualsiasi categoria
070300031219     C**************************************************************************
070400031219     C     RICSPU        BEGSR
070500031219     C                   CLEAR                   Wimm
070600031219     C                   CLEAR                   WSAVB
070700031219     C                   CLEAR                   WDFV
070800031219     C                   CLEAR                   WTROV
070900040108     C                   CLEAR                   WSPG
071000170330     C                   CLEAR                   Wnpg
071100070118RM*  C     Kartd         CHAIN     FNBRV07l                           33
071200031219RM*  C*
071300031219RM* 1C     *IN33         DOWEQ     *OFF
071400031219     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
071500031219     c                   clear                   dslv53
071600031219     C                   z-add     brvnFV        D53NFV
071700031219     C                   MOVEL     brvNPG        D53NPG
071800031219     C                   MOVEL     brvFGS        D53FGS
071900070118     C****               MOVEL     brvfle        D53flf
072000031219     C                   CALL      'FNLV53R'
072100031219     C                   PARM                    DSLV53
072200031219     C* ERRORE
072300031219    2C     D53ERR        IFeq      ' '
072400070118     c                   movel     brvdfs        brvimm           14 0
072500031219     c                   move      brvhms        brvimm
072600031219    3c                   select
072700031219     c* Se maggiore memorizzo
072800031219     c                   when      d53dfv>wdfv
072900031219     C                   MOVEL     RECBRV        WSAVB
073000070118     C                   MOVEL     brvimm        Wimm             14 0
073100031219     C                   MOVEL     d53DFV        WDFV
073200040108     C                   MOVEL     d53spg        WSPG
073300170330     C                   MOVEL     brvnpg        wNPG
073400031219     C                   MOVEL     'S'           WTROV             1
073500031219     c
073600031219     c* Se uguale tengo la data ora immissione + alta
073700031219     c                   when      d53dfv=wdfv  and brvimm>wimm
073800070118     C                   MOVEL     brvimm        Wimm             14 0
073900031219     C                   MOVEL     RECBRV        WSAVB
074000031219     C                   MOVEL     d53DFV        WDFV
074100040108     C                   MOVEL     d53spg        WSPG
074200170330     C                   MOVEL     brvnpg        wNPG
074300031219     C                   MOVEL     'S'           WTROV             1
074400031219     C                   eval      wimm=brvimm
074500040129     c* Se stessa data e ora immissione, tengo cat 3 o 4
074600101202     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
074700070118     C                   MOVEL     brvimm        Wimm             14 0
074800040129     C                   MOVEL     RECBRV        WSAVB
074900040129     C                   MOVEL     d53DFV        WDFV
075000040129     C                   MOVEL     d53spg        WSPG
075100040129     C                   MOVEL     'S'           WTROV             1
075200170330     C                   MOVEL     brvnpg        wNPG
075300040129     C                   eval      wimm=brvimm
075400031219    3c                   endsl
075500031219    2c                   endif
075600031219     c
075700070118RM*  C     Kartd         reade     FNBRV07l                               33
075800031219    1c                   enddo
075900031219     c
076000031219     C     WTROV         IFEQ      'S'
076100031219     C                   MOVEL     WSAVB         RECBRV
076200031219     C                   ENDIF
076300031219     c                   endsr
076400941017     C**************************************************************************
076500941017     C* IMPOSTO CAMPI FISSI
076600941017     C**************************************************************************
076700911018     C     IMPO          BEGSR
076800921022     C*
076900101202     C                   Z-ADD     1             CODUT             1 0
077000921022     C                   Z-ADD     3             IMANPG
077100950810     C                   CLEAR                   SAVSPE
077200950810     C                   CLEAR                   COMSPE
077300921022     C                   TIME                    UTIME             6 0
077400960927     C                   TIME                    W0140            14 0
077500960927     C                   MOVE      W0140         UDATE8            8 0
077600101202      *
077700101202      * Reperimento dati utente/aziendali
077800101202     c                   exsr      sr_DatiJob
077900161130     c
078000161130     c* per ora solo le filiali abilitate all'attesa in IMA hanno ima e img modificato
078100170124     c**                 clear                   trulvpods
078200170124     c**                 eval      ivpoke1 = 'DECOFIIMG'
078300170124     c**                 call      'TRULVPOR'
078400170124     c**                 parm                    trulvpods
078500161130
078600170124     c**                 if         %lookup('999':skFilOk)>0    or
078700170124     c**                            %lookup(%editc(vidfil:'X'):skFilOk)>0
078800170124     c**                 seton                                        20
078900170124     c**                 endif
079000161130     c
079100161130     c
079200170124     C* 11 e 10 ON - IPG (eliminato)
079300170124     c                   select
079400170124     c***                when      vidtin='Z'
079500170124     C***                SETON                                        1011
079600161017     C* 10 ON - IMG
079700161017     C                   when      vidtin='G'
079800950810     C                   SETON                                        10
079900161014     C                   ENDsl
080000921022     C*
080100921022     C* CHAIN E DATA DEL FOGLIO IMA
080200941017     C     KFVV          CHAIN(N)  FNFVV01L                           32
080300921022     C*
080400941018     C                   Z-ADD     FVVDFV        G02INV                         AAAA/MM/GG
080500941018     C                   Z-ADD     *ZERO         G02DAT
080600941018     C                   MOVEL     '3'           G02ERR
080700941018     C                   CALL      'XSRDA8'
080800941018     C                   PARM                    WLBDAT
080900941018     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
081000950316     C*
081100950317RM*  C                   Z-ADD     FVVDFV        SAVDFV
081200950317RM*  C                   Z-ADD     FVVFGS        SAVFGS
081300931228     C*
081400941028     C* DECODIFICA FILIALE DI APERTURA FOGLIO
081500941018     C     FVVFGS        CHAIN     AZORG                              32
081600931228     C  N32              MOVEL     ORGDES        DESCDU           20
081700000512     C*
081800000512     C                   Z-ADD     15            ANMCAA
081900000512     C                   MOVEL     '7C'          COD
082000000512     C                   MOVEL(P)  ANMCAA        KEY
082100000512     C     KTAB          CHAIN     TABEL00F                           35
082200000512     C  N35              MOVEL     TBLUNI        DS7C
082300000512     C   35              MOVEL     *BLANKS       DS7C
082400911018     C*
082500931228     C* 02 ON -  D E F I N I T I V O
082600911018     C     VIDSN         IFEQ      'S'
082700911018     C                   SETON                                        02
082800911018     C*
082900921022     C* PRENDO FASE ANOMALIA
083000950810     C                   CLEAR                   PARAM5
083100950810     C                   MOVEL     'L'           PA5TLA
083200950810     C                   MOVEL     3             PA5NPG
083300950810     C                   MOVEL     VIDTIN        PA5SPG
083400950810     C                   CALL      'TRUL11R'
083500950810     C                   PARM                    PARAM5
083600920409     C                   END
083700920813     C*
083800920813     C* DECODIFICA FILIALE IN GESTIONE
083900920813     C     VIDFIL        CHAIN     AZORG                              32
084000920813     C  N32              MOVEL     ORGDES        DESFIL           20
084100170323     c
084200170213     c* Verifico se fa l'IDD il giorno dopo
084300170213     c                   movel     orgde8        og148
084400170213     c*
084500170213     c                   if        §OGEID='S'
084600170213     c                   eval      iddgdopo='S'
084700170213     c                   endif
084800170323     c
084900170323     c* Verifico se ha il picking
085000170323     c                   movel     orgdf0        og150
085100170323     c*
085200170323     c                   if        §OGPCK='S'
085300170323     c                   eval      picking ='S'
085400170323     c                   endif
085500170323
085600170323     c* Solo se filiale partita con NUOVO IMA devo aggiornare la data entrata mag giac
085700170323     c* ima e IMG modificati sempre per NEW IMA e non per i 2 gg in ima
085800170323     c                   clear                   trulvpods
085900170323     c                   eval      ivpoke1 = 'DECOFIIPG'
086000170323     c                   call      'TRULVPOR'
086100170323     c                   parm                    trulvpods
086200170323
086300170323     c                   if         %lookup('999':skFilOk)>0    or
086400170323     c                              %lookup(%editc(vidfil:'X'):skFilOk)>0
086500170323     c                   seton                                        22
086600170323     c                   endif
086700170323     c*
086800170323     c* Chi non ha il picking non può fare la nuova procedura
086900170323     c                   if        picking=' '
087000170323     c                   setoff                                       22
087100170323     c                   endif
087200941019     C*
087300101215     C* CARICO L6
087400101215     C                   CLEAR                   DSUL06
087500101215     C                   MOVE      '£6'          D06COD
087600101215     C                   MOVEL     VIDFIL        D06KEY
087700101215     C                   MOVEL     'L'           D06TLA
087800101215     C                   MOVEL     DSUL06        KPJBU
087900101215     C                   CALL      'TRUL06R'
088000101215     C                   PARM                    KPJBA
088100101215     C                   MOVEL     KPJBU         DSUL06
088200101215     C                   MOVEA     LIN           Lspu
088300060508     c*
088400101215     C*  Se non parzializzata la linea di arrivo, elaboro L6
088500161014     c*  altrimenti elaboro solo lei
088600060508     c                   if        vidlna=0
088700101215     C                   MOVEA     LIN           L6
088800060508     c                   else
088900060508     c                   clear                   L6
089000060508     c                   movel     vidlna        l6(1)
089100060508     c
089200060508     c* Decodifica
089300060508     C     VIDlna        CHAIN     AZORG                              32
089400060508     C  N32              MOVEL     ORGDES        DESlna           20
089500060508     c                   movel     vidlna        stalna            3
089600060508     c                   endif
089700070418     c
089800070418     c* Elabora anche spedizioni in distinta
089900070418     c                   if        vidmdc='S'
090000070418     c                   eval      stamdc='SI'
090100070418     c                   else
090200070418     c                   eval      stamdc='NO'
090300070418     c                   endif
090400101215     c* solo colli spuntati alla linea di arrivo finale
090500101215     c                   if        parimaa='S'
090600101215     c                   eval      staimaa=csololna
090700101215     c                   endif
090800160503     c* Verifico le richieste delle zone di consegna
090900160503     c                   clear                   wParc             1
091000160503     c                   clear                   wParc1            1
091100160503     c                   clear                   wParc2            1
091200160503     c                   clear                   wMess             1
091300160503     c                   clear                   wSelzona          1
091400160503     c                   if        partpm1='P ' or partpm2='P '
091500160503     c                   eval      wparc='S'
091600160503     c                   eval      wselzona='S'
091700160503     c                   endif
091800160503     c                   if        partpm1='M ' or partpm2='M '
091900160503     c                   eval      wMess='S'
092000160503     c                   eval      wselzona='S'
092100160503     c                   endif
092200160503     c                   if        partpm1='P1' or partpm2='P1'
092300160503     c                   eval      wParc1='S'
092400160503     c                   eval      wselzona='S'
092500160503     c                   endif
092600160503     c                   if        partpm1='P2' or partpm2='P2'
092700160503     c                   eval      wParc2='S'
092800160503     c                   eval      wselzona='S'
092900160503     c                   endif
093000060508     c
093100050510     c* se richiesta soltanto zone parcel o messagerie, carico in
093200050510     c*  schiera
093300160503     c                   if        wparc='S' and wmess='S'
093400160503     c                   clear                   wparc
093500160503     c                   clear                   wmess
093600160503     c                   clear                   wselzona
093700160503     c                   endif
093800160503     c                   if        wparc1='S' and wparc2='S'
093900160503     c                   clear                   wparc1
094000160503     c                   clear                   wparc2
094100160503     c                   eval      wparc='S'
094200160503     c                   endif
094300160503     c
094400160503    1c                   if        wselzona='S'
094500050511     c                   clear                   xx                4 0
094600160502     c                   clear                   yy                4 0
094700170331     c                   clear                   zz                4 0
094800170331     c                   eval      zz=1
094900170331     c
095000170331     c* Carico le zone per tutte le linee della £6
095100170331   1ac                   dow       l6(zz)>0
095200170331     c                   movel     l6(zz)        alffil            3
095300170331     c                   movel     l6(zz)        wpo
095400050510     c                   movel(P)  '03'          cod
095500170331     c                   movel(P)  l6(zz)        key
095600050510     c     ktab          setll     tabel
095700050510     c     ktab2         reade     tabel
095800160502    2c                   dow       not %eof(tabel00f) and  wpo=alffil
095900160502    3c                   if        tblflg=' '
096000050510     c                   movel     tblkey        wpo               3
096100160502    4c                   if        wpo=alffil
096200050511     c                   movel     tbluni        ds03
096300160502
096400160502     c* parcel/messaggerie
096500160503    5c                   if        wparc='S' or wmess='S'
096600160502
096700160503    6c                   if        (wmess='S' and §03tpm='M') or
096800160503     c                             (wparc='S' and §03tpm<>'M')
096900050511     c                   add       1             xx
097000050511     c                   movel     tblkey        w005a             5
097100050511     c                   move      w005a         zone(xx)
097200160502    6c                   endif
097300160502    5c                   endif
097400160502     c* parcel 1° e 2° turno
097500160503    5c                   if        (wparc1='S' or wparc2='S')
097600160503     c                             and §03tpm<>'M'
097700160502
097800160503    6c                   if        (wparc2='S' and §03turno='2') or
097900160503     c                             (wparc1='S' and §03turno<>'2')
098000160503     c                   add       1             yy
098100160502     c                   movel     tblkey        w005a             5
098200160503     c                   move      w005a         zone12(yy)
098300160502    6c                   endif
098400160502    5c                   endif
098500160502     c
098600160502    4c                   endif
098700160502    3c                   endif
098800160502     c
098900050511     c     ktab2         reade     tabel
099000160502    2c                   enddo
099100170331     c
099200170331     c                   add       1             zz
099300170331   1ac                   enddo
099400160502    1c                   endif
099500050511     c
099600160503     c                   clear                   statipo
099700160503     c                   clear                   statipo2
099800160503     c                   if        partpm1='P ' or partpm2='P '
099900050511     c                   eval      statipo=cparcel
100000050511     c                   endif
100100050511     c
100200160503     c                   if        partpm1='M ' or partpm2='M '
100300160503     c                   if        statipo=*blanks
100400050511     c                   eval      statipo=cmessag
100500160503     c                   else
100600160503     c                   eval      statipo2=cmessag
100700160503     c                   endif
100800050511     c                   endif
100900160503     c                   if        partpm1='P1' or partpm2='P1'
101000160503     c                   if        statipo=*blanks
101100160503     c                   eval      statipo=cparc1
101200160503     c                   else
101300160503     c                   eval      statipo2=cparc1
101400160503     c                   endif
101500160503     c                   endif
101600160503     c                   if        partpm1='P2' or partpm2='P2'
101700160503     c                   if        statipo=*blanks
101800160503     c                   eval      statipo=cparc2
101900160503     c                   else
102000160503     c                   eval      statipo2=cparc2
102100160503     c                   endif
102200160503     c                   endif
102300941017     C*
102400100706      * Aggancio tabella VPO recpord VPO
102500100706     c                   Clear                   Dvpo
102600100706     c                   Clear                   dsbs02
102700100706     c                   Eval      T02Mod = 'C'
102800100706     c                   Eval      T02tla = 'L'
102900100706     c                   Eval      T02Sif = knsif
103000100706     c                   Eval      T02Cod = 'VPO'
103100100706     c                   Movel(p)  'VPO'         T02Ke1
103200100706     c                   Call      'TIBS02R'
103300100706     c                   Parm                    Kpjba
103400100706     c                   Parm                    dsbs02
103500100706     c                   If        T02Err = *Blanks
103600100706     c                   Movel     T02Uni        Dvpo
103700100706     c                   EndIf
103800100706     c
103900100706     c     *eur          move      udate8        dataeur
104000100706     c                   movel     dataeur       dataiso
104100161017     c
104200161017     c                   movel     dataiso       dateu             8 0
104300161017
104400100706     c                   if        §vpoggima>0
104500100706     c                   subdur    §vpoggima:*d  dataiso
104600100706     c                   endif
104700100706     c
104800100706     c                   movel     dataiso       datapresIMA       8 0
104900100706
105000911018     C                   ENDSR
105100941017     C**************************************************************************
105200941017     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
105300941017     C**************************************************************************
105400911018     C     ERRORE        BEGSR
105500941017     C*
105600911015     C                   SETON                                        01
105700040603     c
105800040603     c     kar5          chain     fiar501l
105900040603     c                   if        %found(fiar501l)
106000040603     c                   movel     ar5uni        dar5gen
106100040603     c                   else
106200040603     c                   clear                   dar5gen
106300040603     c                   endif
106400911018     C* STAMPA ANOMALIA
106500040428     C                   EXSR      STAMPA
106600081014     c
106700100706     c* non creo anomalia se collo doppio o solo segnalazione
106800161017    0c                   if        not *in12 and not *in16 and not *in11
106900161018     c                             and not *in18
107000170329     c* in IMA la creo solo se non è da portare a mag giac  se partito con NEW IMA
107100170329     c*  in IMG  sempre perchè rischio di perdere la segnalaziione in IDD se mancante
107200170329     c                              and (wgiac<>'D' or *in10)
107300911015     C*
107400950707     C* SOLO SE IMA DEFINITIVO  CREO ANOMALIA
107500950608    1C     VIDSN         IFEQ      'S'
107600950810     C* NON DEVO CREARE:
107700950810     C* 1) SE C'E' LA FORZATURA E MANCA LA DATA DI ARRIVO
107800950810     C*    ANCHE SE STAMPO LO STESSO
107900950810     C* 2) SE LA SPUNTA E' STATA TROVATA IN ALTRA CATEGORIA DEL GIORNO
108000960419     C*    3 O 4 NON DISABILITO NEMMENO LA BOLLA
108100960419     C* 3) SE LA SPUNTA E' STATA TROVATA IN ARRIVO CREO L'ANOMLIA
108200950608    2C     ARTDAM        IFGT      0
108300960419   2AC     *IN13         IFEQ      *OFF
108400031219     C     *IN15         OREQ      *ON
108500081014i    C*
108600081104     C     KANM          SETLL     FNANM02L                               35
108700081014    3c                   if        not *in35
108800081014     c* Se anomalia chiusa non in pratica idd e data chiusura <data ima
108900081014     c*  la ricreo
109000081104    4c***                if        anmdch>0 and anmdch<savdfv and
109100081104     c***                          anmcch<>'PR'
109200081104     c***                delete    fnanm000
109300081104     c***                seton                                        35
109400081104    4c***                endif
109500081104    3c***                endif
109600081014     c
109700081104    3C***                if        *in35
109800081104     C*
109900081104     C                   MOVEL     *BLANKS       ANMATB
110000081104     C                   MOVEL     PA5FAS        ANMFSA
110100160503     c*  data salvata del foglio IMA perchè richaino in update
110200100506     C                   Z-ADD     SAVdfv        ANMDAO
110300081104     C                   Z-ADD     15            ANMcaa
110400081104     C                   MOVEL     *BLANKS       ANMCCH
110500081104     C                   MOVEL     VIDFGS        ANMCDU
110600081104     C                   Z-ADD     COMNFV        ANMNFV
110700081104     C                   MOVEL     VIDFGS        ANMFLE
110800081104     C                   MOVE      *BLANKS       ANMFLO
110900081104     C                   MOVEL     *BLANKS       ANMFT1
111000081104     C                   MOVEL     *ZEROS        ANMFL1
111100081104     C                   MOVEL     *BLANKS       ANMFT2
111200081104     C                   MOVEL     *ZEROS        ANMFL2
111300081104     C*
111400081104     C                   MOVEL     §7CTAN        ANMTAN
111500081104     C                   MOVEL     §7CSAN        ANMSAN
111600081104     C                   MOVEL     §7CAIE        ANMAIE
111700081104     C                   Z-ADD     §7CLID        ANMLID
111800081104     C                   MOVE      *ZEROS        ANMDUE
111900081104     C                   MOVE      *ZEROS        ANMNPG
112000081104     C                   MOVE      *ZEROS        ANMFGS
112100081104     C                   Z-ADD     0             ANMDCH
112200081104     C                   MOVEL     ' '           ANMFSC
112300081104     C* CHIUSURA AUTOMATICA
112400081104     C     §7CCHA        IFEQ      'S'
112500100506     C                   Z-ADD     SAVDFV        ANMDCH
112600081104     C                   MOVEL     PA5FAS        ANMFSC
112700081104     C                   END
112800081104     C                   MOVE      *BLANKS       ANMFT3
112900081104     C                   MOVE      *ZEROS        ANMFL3
113000081104     C                   MOVE      *BLANKS       ANMFT4
113100081104     C                   MOVE      999           ANMFL4
113200081104     C                   MOVE      *ZEROS        ANMDTR
113300081104     C*
113400911018     C                   MOVEL     ARTFLS        ANMFLS
113500911018     C                   MOVEL     ARTLNA        ANMLNA
113600911018     C                   MOVEL     ARTNRS        ANMNRS
113700941018     C                   MOVEL     ARTNSC        ANMSCN
113800941018     C                   Z-ADD     ARTAAS        ANMAAS
113900911018     C                   MOVEL     ARTNSP        ANMNSP
114000911018     C                   MOVEL     ARTLNP        ANMLNP
114100970206     C                   MOVEL     ARTNAP        ANMNPS
114200040603     c
114300040603     c* imposto data e ora di creazione
114400050707     C                   TIME                    W0140            14 0
114500050707     C                   MOVE      W0140         G02DAT
114600050707     C                   Z-ADD     *ZERO         G02INV
114700050707     C                   MOVEL     *BLANKS       G02ERR
114800050707     C                   CALL      'XSRDA8'
114900050707     C                   PARM                    WLBDAT
115000050707     C                   Z-ADD     G02INV        WDATA             6 0
115100050707     c**                 time                    wora              6 0
115200050707     c**                 z-add     *date         wdata             6 0
115300050707     C
115400050707     c                   movel     w0140         wora4             4 0
115500040603     c                   movel     wdata         anmflo
115600040603     c                   move      wora4         anmflo
115700040603     c
115800040603     c* imposto se flag di valore
115900040603     c                   movel     §ar5bva       anmft4
116000040603     c
116100040603     c
116200920409     C* ESTERNA
116300950608    4C     §7CAIE        IFEQ      'E'
116400950608     C                   MOVEL     ARTLNP        ANMFL1
116500950810    4C                   ENDIF
116600920409     C*
116700990511     C                   WRITE     FNANM000
116800081014    3C                   endif
116900081014     c
117000000512   2AC                   ENDIF
117100960419    2C                   ENDIF
117200920318     C*
117300000512     C* DISABILITO LA BOLLA SOLO SE HO CREATO ANOMALIA
117400000512     C* NO POSTE SEMPRE, LNP POSTE SOLO SE PREVOSTO
117500950810     C* SOLO SE NON HO TROVATO UN'ALTRA SPUNTA IN DATA DEL FOGLIO IMA
117600960419    2C     *IN10         IFEQ      *OFF
117700960419     C*
117800960419    3C     *IN13         IFEQ      *OFF
117900031219     C     *IN15         OREQ      *ON
118000000512     C*
118100040428    5c     wchain        ifeq      *blanks
118200040428     c     fnarbnrri     chain(e)  fnarb00f
118300040428    6c                   if        %error
118400040428     C* Valorizzo la DS di procedura
118500040428     C                   CLEAR                   TRUL82DS
118600040428     c                   eval      ul82§rrn = fnarbnrri
118700040428     C                   EVAL      UL82§FIL = 'FNARB00F'
118800040428     C                   EVAL      UL82§WIN = 'N'
118900040428     C                   EVAL      UL82§F7  = 'S'
119000040428     C                   EVAL      UL82§num = 5
119100040428     C                   EVAL      UL82§att = 60
119200040428     C                   EVAL      UL82§mss = msgjob
119300040428     C* Effettuo la chiamata al *PGM d utilità
119400040428     C                   CALL(e)   'TRUL82R'
119500040428     C                   PARM                    TRUL82DS
119600040428     C     fnarbnrri     chain     fnarb00f
119700040428    6c                   endif
119800040428     C                   Z-ADD     0             ARBAMA
119900040428     c                   update    fnarbf00
120000040428     c                   move      '1'           wchain
120100040428    5c                   endif
120200930629     C* AGGIORNO IL COLLO CON UNA 'D' DI DISABILITAZIONE
120300941221     C                   MOVEL     'S'           ARTDAB                         'D'=DISABILITATO
120400000512    3C                   ENDIF
120500960419    2C                   ENDIF
120600950810     C*
120700040428     C  N10              UPDATE    FNART000
120800950810    1C                   ENDIF
120900081014    0C                   ENDIF
121000911015     C*
121100911015     C                   ENDSR
121200950810     C**************************************************************************
121300950810     C* ELABORAZIONE DELL'IMA
121400950810     C**************************************************************************
121500950810     C     ELAIMA        BEGSR
121600950810     C*
121700950810     C* N O N   C O N T R O L L O :
121800950810     C*          1) SE NO ABILITAZIONE FORZATA E SENZA DATA ARRIVO
121900990212    1C     SAVAMA        IFNE      3
122000950810     C     ARTDAM        ANDEQ     0
122100950810     C                   SETON                                        34
122200990212    1C                   ENDIF
122300950810     C*
122400950810     C*          2) SE DATA ARRIVO MAGGIORE DATA FOGLIO IMA
122500990212    1C  N34ARTDAM        IFNE      0
122600950810     C     ARTDAM        ANDGT     SAVDFV
122700950810     C                   SETON                                        34
122800990212    1C                   ENDIF
122900950810     C*
123000990212     C*          3) SE COLLO SI TROVA IN DCD E RESO O DISTRUTTO PER
123100990212     C*             AVARIA O COLLO MANCANTE
123200990212     C     KARTD         CHAIN     FNDCD02L                           31
123300990212    1C     *IN31         IFEQ      *OFF
123400990212     C*
123500990303     C     DCDATB        ANDEQ     ' '
123600990212     C* CHAIN TESTATA PER CONTROLLARE LA DATA DI CHIUSURA
123700990212     C     KDCD          CHAIN     FNDCT01L                           32
123800050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
123900050221     c                   If        Not *In32 and dctdt2 > 0
124000050221     c                   Eval      *In32 = *On
124100050221     c                   EndIf
124200990303     C*
124300990303    2C     *IN32         IFEQ      *OFF
124400990303     C* SE E' CHIUSA CONTROLLO SE IL RECORD DEVE ESSERE COMPRESO
124500990303     C*  IN IMA, PER ALCUNI TIPI ANOMALIA
124600990303     C                   MOVEL     'S'           §CHIMA
124700990303    3C     DCTCCH        IFNE      *BLANKS
124800990303     C                   CLEAR                   DSBS02
124900990303     C                   MOVEL     'C'           T02MOD
125000990303     C                   MOVEL     KNSIF         T02SIF
125100990303     C                   MOVEL     'CCH'         T02COD
125200990303     C                   MOVEL     DCTCCH        T02KE1
125300990303     C                   CALL      'TIBS02R'
125400990303     C                   PARM                    KPJBA
125500990303     C                   PARM                    DSBS02
125600990303     C*
125700990303    4C     T02ERR        IFEQ      ' '
125800990303     C                   MOVEL     T02UNI        DCCH
125900990303    4C                   ENDIF
126000990303    3C                   ENDIF
126100990303     C**
126200990303     C** SE NON DEVE ESSERE COMPRESO:
126300990303    3C     §CHIMA        IFNE      'S'
126400990303     C     DCTDCH        OREQ      0
126500990212     C** SE ENTRAMBI APERTI O CHIUSI CON STESSA DATA CONTROLLO IL TIPO
126600990212     C**  ANOMALIA PER DECIDERE SE TENERE O ESCLUDERE IL COLLO
126700990212     C**  NON CONSIDERO LA CAUSALE PAREGGIO CHE SIGNIFICA CHE IL COLLO
126800990212     C*  C'E' SEMPRE
126900990212    4C     DCTDCH        IFEQ      DCDDCH
127000990212     C     DCDDCH        OREQ      0
127100990212     C**
127200990212     C                   CLEAR                   DSBS02
127300990212     C                   MOVEL     'C'           T02MOD
127400990212     C                   MOVEL     KNSIF         T02SIF
127500990212     C                   MOVEL     'TAD'         T02COD
127600990212     C                   MOVEL     DCTTAD        T02KE1
127700990212     C                   CALL      'TIBS02R'
127800990212     C                   PARM                    KPJBA
127900990212     C                   PARM                    DSBS02
128000990212     C**
128100990212    5C     T02ERR        IFEQ      ' '
128200990212     C                   MOVEL     T02UNI        DTAD
128300990212     C                   ELSE
128400990212     C                   MOVEL     'A'           §TARGR
128500990212    5C                   ENDIF
128600990212    C**
128700990212     C* SE MANCANZE
128800990212    5C     §TARGR        IFEQ      'M'
128900990212     C* O AVARIA RESA
129000990212     C     §TARGR        OREQ      'V'
129100990212     C     DCTDSD        ANDEQ     'R'
129200990212     C* O AVARIA DISTRUTTA
129300990212     C     §TARGR        OREQ      'V'
129400990212     C     DCTDSD        ANDEQ     'D'
129500990212     C*                    --> ESCLUDO
129600990212     C                   SETON                                        34
129700990212    5C                   ENDIF
129800990212    4C                   ENDIF
129900990212    3C                   ENDIF
130000990212    2C                   ENDIF
130100990212    1C                   ENDIF
130200101215     c
130300101215     c* Se opzione solo spuntato alla linea di arrivo finale, controllo
130400101215     c                   if        parimaa='S'
130500101215     c                   setoff                                       37
130600101215RM*  C     Kartd         setll     FNBRV07l
130700101215RM*  C     Kartd         reade     FNBRV07l
130800101215     c                   dow       not %eof(fnbrv07l)
130900101215     c     brvfgs        lookup    Lspu                                   37
131000101215     c                   if        *in37
131100101220     c* Se la filiale che spunta è giusta verifico che sia categoria di "arrivo"
131200101220     c                   clear                   dslv53
131300101220     C                   z-add     brvnFV        D53NFV
131400101220     C                   MOVEL     brvNPG        D53NPG
131500101220     C                   MOVEL     brvFGS        D53FGS
131600101220     C                   CALL      'FNLV53R'
131700101220     C                   PARM                    DSLV53
131800101220     C*
131900101220    2C     D53ERR        IFeq      ' '
132000101220     c*
132100101220     c                   if        d53def='S'
132200101220     c                   eval      wnpgarr=%editc(d53npg:'X')+'D'
132300101220     c                   else
132400101220     c                   eval      wnpgarr=%editc(d53npg:'X')+d53spg
132500101220     c                   endif
132600101220     c
132700101220     c                   eval      TROV=%lookup   (wnpgarr: NPGARR)
132800101220     c                   if        TROV>0
132900101215     c                   leave
133000101220     c                   endif
133100101215     c                   endif
133200101220     c                   endif
133300101220     c
133400101220     c                   setoff                                       37
133500101220RM*  C     Kartd         reade     FNBRV07l
133600101215     c
133700101215     c                   enddo
133800101215     c
133900101215     c* se non trovata nessuna spunta, NON elaboro
134000101215     c                   if        not *in37
134100101215     c                   seton                                        34
134200101215     c                   endif
134300101215     c
134400101215     c                   endif
134500990212    C**
134600170125     c** se collo mancante di bolla da portare a mag gaicenza e collo spuntato in
134700170125     c*  IMG nel giorno --> non stampo
134800170125     c
134900100706     c                   if        not *in34
135000170125     c                   clear                   winGiac
135100100706     C                   EXSR      CTRART
135200950810     C*
135300950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
135400170125     c*         se si tratta di merce da portare a IMG e collo spuntato solo in IMG
135500170125     c*         non stampo
135600170125     c                   if        wgiac='D' and *in14 and
135700170125     c                             wspg='G' and wdfv=savdfv
135800170125     c                   eval      winGiac='S'
135900170125     c                   endif
136000170213     c
136100170125
136200100706     C* 16 ON -     TROVATO SEGNACOLLO IN IMA più di xxx volte
136300170125     c                   if        *in14 and wingiac=' '  or *in16
136400170125     C                   EXSR      ERRORE
136500170125     c                   endif
136600170125
136700170125     c                   if        not *in14
136800170125     c                   eval      Wesspu='S'
136900170125     c                   endif
137000100706     c                   endif
137100950810     C*
137200950810     C                   ENDSR
137300950810     C**************************************************************************
137400950810     C* ELABORAZIONE DELL'IMG
137500950810     C**************************************************************************
137600950810     C     ELAIMG        BEGSR
137700950810     C                   EXSR      CTRART
137800950810     C*
137900170124     C* 14 ON - NON TROVATO SEGNACOLLO IN IMG
138000161025     c                   if        *in14
138100161026     c* In caso di bolla con giacenza e merce da portare a mag giac.
138200161026     c*  non stampo nulla per IMG
138300170124     c***                if        *in11 or wgiac<>'D'
138400170124     c                   if        wgiac<>'D'
138500161025     C                   EXSR      ERRORE
138600161026     c                   endif
138700161026     c
138800161025     c                   else
138900161025     c                   eval      Wesspu='S'
139000161025     c                   endif
139100161025
139200950810     C                   ENDSR
139300941017     C**************************************************************************
139400941017     C* STAMPO DISCORDANZA
139500941017     C**************************************************************************
139600900621     C     STAMPA        BEGSR
139700050405     c                   setoff                                       242526
139800941017     C*
139900911015     C* STAMPA TESTATA
140000911015     C   OF              DO
140100911018     C                   SETON                                        05
140200911018     C                   EXCEPT    TES
140300920813     C                   EXCEPT    TES2
140400911018     C                   SETOFF                                       OF
140500911018     C                   END
140600090522     c* Se non trovata la bolla (per colli doppi)
140700090522    1c                   if        NoBolla='S'
140800090522     c                   except    DetNObo
140900161018     C   12              EXCEPT    DETdoppio
141000161018     C   18              EXCEPT    DETeccede
141100090522   x1c                   else
141200911018     C*
141300950810     C     COMSPE        IFNE      ARBSPE
141400911018     C                   SETON                                        05
141500950810     C                   MOVEL     ARBSPE        COMSPE
141600950810     C                   ENDIF
141700911018     C* DATA SPEDIZIONE
141800911018     C   05              DO
141900941018     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
142000941018     C                   MOVE      ARBDSP        G02INV                         AAAA/MM/GG
142100911018     C                   MOVE      *ZEROS        G02DAT
142200911018     C                   MOVEL     3             G02ERR
142300941017     C                   CALL      'XSRDA8'
142400911018     C                   PARM                    WLBDAT
142500941018     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
142600941018     C*
142700941018     C* CODICE PADRONCINO CONSEGNA DECODIFICA
142800021203     C*    ARBPDC        CHAIN     FNAPD01L                           06
142900021205     C                   move      arbpdc        kpdpdr
143000021203     C                   move      'A'           kpdtip
143100021203     C     kfapd         chain     fiapd01L                           06
143200941018     C  N06              MOVEL     APDRSC        DESCPC            9
143300100707     c
143400100707     c* se non c'e' paadroncino consegna scrivo se Fermo deposito
143500100707     c                   if        *in06
143600100707     C                   MOVEL     *BLANKS       DESCPC
143700100707     c                   if        arbffd='S'
143800100707     c                   eval      descpc='FDeposito'
143900100707     c                   endif
144000100707     c                   endif
144100920409     C*
144200911018     C* SEGNACOLLI NON CONSECUTIVI
144300911018     C     ARBFNS        COMP      'S'                                    04
144400911018     C* SERIE>< 0
144500911018     C     ARBNRS        COMP      0                                  0303
144600911018     C                   END
144700920409     C*
144800920708     C* 07 ON - MANCA DATA ARRIVO: SPEDIZIONE FORZATA A 3
144900920708     C     ARTDAM        IFEQ      0
145000920708     C                   SETON                                        07
145100920708     C                   ELSE
145200920708     C                   SETOFF                                       07
145300920409     C* DATA ARRIVO MERCE  SEGNACOLLO
145400941018     C                   MOVE      ARTDAM        G02INV                         AAAA/MM/GG
145500920409     C                   MOVE      *ZEROS        G02DAT
145600920409     C                   MOVEL     3             G02ERR
145700941017     C                   CALL      'XSRDA8'
145800920409     C                   PARM                    WLBDAT
145900941018     C                   Z-ADD     G02DAT        STADAM                         GG/MM/AAAA
146000920708     C                   END
146100040603     C* MITTENTE E DESTINATARIO DI 17 E 20
146200040603     C                   MOVEL     ARBRSM        STARSM           17
146300920707     C                   MOVEL     ARBRSD        STARSD           20
146400040603     c* collo di valore
146500040603     c                   movel     §ar5bva       stabva            1
146600161025     c* se si tratta di bolla da portare a mag giac, riempi il campo STADSF con relativa scritta
146700161025     c                   if        not *in11 and wgiac='D'
146800161025     c                   clear                   stadsf
146900161025     c                   eval      stadsf='Portare a MagGIAC'
147000161025     c                   endif
147100161025     c
147200161026     c                   clear                   stahms            4 0
147300170222     c   13              if        brvhms>0
147400170222     c                   eval      stahms=brvhms/100
147500170222     c                   endif
147600910522     C*
147700900622     C* STAMPO DETTAGLIO
147800100706     C                   EXCEPT    DET
147900100707     c                   select
148000100707     c                   when      *in12
148100100706     C                   EXCEPT    DETdoppio
148200100707     c                   when      *in16
148300100707     C                   EXCEPT    DETspuIMA
148400161018     c                   when      *in18
148500161018     C                   EXCEPT    DETeccede
148600100707     c                   other
148700100706     C                   EXCEPT    DETerr
148800100707     c                   endsl
148900070418     c
149000070418     c* se in distinta , stampo
149100090522    2c                   if        not *in12
149200081014
149300100706    3c                   if        *in05 and arbndc>0 and not *in16
149400070418     C                   MOVE      ARbddc        G02INV                         AAAA/MM/GG
149500070418     C                   MOVE      *ZEROS        G02DAT
149600070418     C                   MOVEL     3             G02ERR
149700070418     C                   CALL      'XSRDA8'
149800070418     C                   PARM                    WLBDAT
149900070418     C                   Z-ADD     G02DAT        STAddc            8 0          GG/MM/AAAA
150000070418     C                   EXCEPT    DETndc
150100090522    3c                   endif
150200050405     c
150300161017     c* Se c'e' ubicazione collo, per 05 on lo stampo
150400050405     c                   if        *in05 and §ar5zng<>*blanks
150500050405     c                   seton                                        24
150600050405     c                   endif
150700050405     c
150800100707     C                   SETOFF                                       05
150900050405     c
151000990212     C* STAMPO TUTTE LE C.A. PRESENTI SULLA SPEDIZIONE
151100040806      * Sostituisco la lettura del file FNDTC02 con
151200040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
151300040806      * legate alla spedizione che passo
151400040806      *
151500040806     c                   clear                   fidn12ds
151600040806     c                   eval      i12aas = arbaas
151700040806     c                   eval      i12lnp = arblnp
151800040806     c                   eval      i12nrs = arbnrs
151900040806     c                   eval      i12nsp = arbnsp
152000040806     c                   eval      i12fel = 'E'
152100040806     c                   eval      i12fan = 'E'
152200040806      *
152300040806     c                   call      'FIDN12R'
152400040806     c                   parm                    fidn12ds
152500040806      *
152600040806      * se non ci sono errori
152700090522    3c                   if        o12err = ' '
152800040806      * se numero di CA trovate maggiore di zero
152900090522    4c                   if        o12nca > 0
153000100706     c
153100050405     c                   seton                                        25
153200040806     c
153300040806     c                   z-add     *zeros        ii                2 0
153400100706    6c     1             do        o12nca        ii
153500040806     c                   movea     skkey(ii)     dskey
153600040806     C                   Z-ADD     dsfil         dctfil
153700040806     C                   Z-ADD     dsnca         dctnca
153800040806     C                   Z-ADD     skdca(ii)     dctdca
153900040806     C                   Z-ADD     skdch(ii)     dctdch
154000040806     C                   movea     skcch(ii)     dctcch
154100040806     c                   exsr      preca
154200050405     c                   except    detca
154300050405     c
154400050405     c                   setoff                                       24
154500100706    6C                   ENDDO
154600040806      *
154700100706    4c                   endif
154800090522    3c                   endif
154900050405     c
155000050405     c* Stampo ubicazione collo se non ancora stampata
155100090522    3c                   if        *in24
155200050405     c                   except    detca
155300090522     c                   setoff                                       24
155400090522    3c                   endif
155500090522     c                   setoff                                       25
155600081014     c
155700090522    2c                   endif
155800090522    1c                   endif
155900900622     C*
156000900622     C                   ENDSR
156100990212     C**************************************************************************
156200990212     C* PREPARO I DATI DELLA C.A.
156300990212     C**************************************************************************
156400990212     C     PRECA         BEGSR
156500990212     C                   SETOFF                                       26
156600990303     C                   CLEAR                   STACCH
156700990212     C* DATA APERTURA C.A.
156800990212     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
156900990212     C                   Z-ADD     *ZERO         G02DAT
157000990212     C                   MOVEL     '3'           G02ERR
157100990212     C                   CALL      'XSRDA8'
157200990212     C                   PARM                    WLBDAT
157300990212     C                   MOVE      G02DAT        W0020             2 0
157400990212     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
157500990212     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
157600990303     C** DATA CHIUSURA C.A.
157700990212     C                   CLEAR                   STACHC
157800990303    1C     DCTDCH        IFGT      0
157900990212     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
158000990212     C                   Z-ADD     *ZERO         G02DAT
158100990212     C                   MOVEL     '3'           G02ERR
158200990212     C                   CALL      'XSRDA8'
158300990212     C                   PARM                    WLBDAT
158400990212     C                   MOVE      G02DAT        W0020             2 0
158500990212     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
158600990212     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
158700990212     C                   SETON                                        26
158800990303     C* CAUSALE CHIUSURA
158900990303    2C     DCTCCH        IFNE      *BLANKS
159000990303     C                   CLEAR                   DCCH
159100990303     C                   CLEAR                   DSBS02
159200990303     C                   MOVEL     'C'           T02MOD
159300990303     C                   MOVEL     KNSIF         T02SIF
159400990303     C                   MOVEL     'CCH'         T02COD
159500990303     C                   MOVEL     DCTCCH        T02KE1
159600990303     C                   CALL      'TIBS02R'
159700990303     C                   PARM                    KPJBA
159800990303     C                   PARM                    DSBS02
159900990303     C*
160000990303    3C     T02ERR        IFEQ      ' '
160100990303     C                   MOVEL     T02UNI        DCCH
160200990303    3C                   ENDIF
160300990303     C                   MOVEL     §CHDES        STACCH           12
160400990303    2C                   ENDIF
160500990303    1C                   ENDIF
160600990212     C                   ENDSR
160700941017     C**************************************************************************
160800161018     C* RIEPILOGO SPUNTE PER PISTOLA e controllo se colli doppi o eccedenti
160900941017     C**************************************************************************
161000920813     C     RIEPIL        BEGSR
161100081014     C                   SETON                                        12
161200941017     C*
161300920813     C                   Z-ADD     1             C
161400920813     C* CONTO SPUNTE PER PISTOLA
161500070118     C     KBRV          SETLL     FNBRV01L
161600070118     C     KBRV          READE     FNBRV01L                               30
161700920813     C*
161800161018    1C     *IN30         DOWEQ     *OFF
161900161018    2c                   if        brvtsu<>*blanks
162000081014     c                   clear                   fnlrv7ds
162100081014     c                   eval      ilrv7fls=brvlnp
162200081014     c                   eval      ilrv7lna=brvlna
162300081014     c                   eval      ilrv7nrs=brvnrs
162400081014     c                   eval      ilrv7nsc=brvnsc
162500081014     c                   eval      kpjbu= fnlrv7ds
162600081014     c                   call      'FNLRV7R'
162700081014     c                   parm                    kpjba
162800081014     c
162900081014     c                   eval      fnlrv7ds=kpjbu
163000161018    3c                   if        olrv7nric>=3 and olrv7part='S'
163100081014     c* Se ho già stampato qualche cosa, lascio la prima volta una
163200081014     c*  riga vuota
163300161018    4c                   if        *in01 and not *inOF and uno=' '
163400081014     c                   except    DETvuota
163500161018    4c                   endif
163600161018     c                   exsr      Cerca_bolla
163700081014     c
163800081014     c                   exsr      ERRORE
163900081014     c                   eval      UNO='S'
164000161018    3c                   endif
164100081014     c
164200161018    2c                   endif
164300161018     c
164400170124     c*      per  IMG  stampo i colli eccedenti ( IPG non esiste più)
164500170124     C*  ( SE partiti con procedura)
164600161201     c                   if        *in10 and *in22
164700161201     c                   exsr      Eccedenti
164800161201     c                   endif
164900081014     c
165000941019     C                   Z-ADD     1             B                 2 0
165100920813     C                   MOVEL     BRVNPS        COMNPS            2
165200920813     C     COMNPS        LOOKUP    NPS(B)                                 31
165300920813     C  N31              MOVEL     BRVNPS        NPS(C)
165400920813     C  N31              Z-ADD     C             B
165500941019     C  N31              ADD       1             C                 2 0
165600920813     C*
165700920813     C                   ADD       1             CPS(B)
165800920813     C*
165900070118     C     KBRV          READE     FNBRV01L                               30
166000161018    1C                   ENDDO
166100081014     C
166200081014     C                   SETOFF                                       12
166300161018     c
166400081014     C*
166500081014     C* SE NESSUNA ANOMALIA STAMPO TUTTO O.K.
166600081014     C  N01              EXCEPT    TES
166700081014     C  N01              EXCEPT    OK
166800081014     C  N01              SETOFF                                       OF
166900920813     C**
167000920813     C* SOMMO TUTTI I COLLI
167100920813     C                   XFOOT     CPS           TOTSPU            9 0
167200920813     C**
167300920813     C* STAMPA SPUNTE
167400920813     C* TESTATA
167500920813     C   OF              EXCEPT    TES
167600920813     C   OF              SETOFF                                       OF
167700920813     C*
167800920813     C                   EXCEPT    TOT1
167900920813     C*
168000920813     C                   Z-ADD     1             C
168100920813     C     CPS(C)        DOWGT     0
168200920813     C   OF              DO
168300920813     C                   EXCEPT    TES
168400920813     C                   EXCEPT    TOT1
168500920813     C                   SETOFF                                       OF
168600920813     C                   END
168700920813     C*
168800920813     C                   EXCEPT    TOT2
168900920813     C                   ADD       1             C
169000920813     C                   END
169100920813     C*
169200920813     C                   EXCEPT    TOT3
169300941017     C*
169400920813     C                   ENDSR
169500161018     C**************************************************************************
169600161018     C*   da spunta cerco la relativa bolla
169700161018     C**************************************************************************
169800161018     c     Cerca_bolla   BEGSR
169900161018     c                   clear                   nobolla
170000161018     c* chain la bolla e stampo collo doppio
170100161018     c     kartB         chain     fnart27l
170200161018    2c                   if        %found(fnart27l)
170300161018     c     karb          chain     fnarb01l
170400161018   x2c                   else
170500161018     c     kartB         chain     fnblt27l
170600161018    3c                   if        %found(fnart27l)
170700161018     c     karb          chain     fnblp01l
170800161018   x3c                   else
170900161018     c                   eval      Nobolla='S'
171000161018    3c                   endif
171100161018    2c                   endif
171200161018     c                   ENDSR
171300161018     C**************************************************************************
171400161018     C*   Ricerca colli eccedenti
171500161018     C**************************************************************************
171600161018     C     Eccedenti     BEGSR
171700161018     c                   setoff                                       12
171800161018     c                   seton                                        18
171900161018     c                   clear                   ecce_descr
172000161018     c
172100161018     c                   exsr      Cerca_bolla
172200161018     c* Se manca bolla --> sempre eccedente
172300161018    1c                   if        nobolla='S'
172400161018    4c                   if        *in01 and not *inOF and uno=' '
172500161018     c                   except    DETvuota
172600161018    4c                   endif
172700161018     c                   exsr      ERRORE
172800161018     c                   eval      UNO='S'
172900161018   x1c                   else
173000161018     c* Richiamo driver per verificare se la bolla è a mag giac oggi
173100161018     c                   clear                   fnlgi1ds
173200161018     c                   eval      ILGI1AAS=arbaas
173300161018     c                   eval      ILGI1lnp=arblnp
173400161018     c                   eval      ILGI1nrs=arbnrs
173500161018     c                   eval      ILGI1nsp=arbnsp
173600161018    3c                   select
173700161018     c                   when      arbpkc>0 and arbncp=arbncl
173800161018     c                   eval      ilgi1pkf=arbpkc
173900161018     c                   when      arbpkc>0 and arbncp<arbncl
174000161018     c                             and arbpkc>arbpkf
174100161018     c                   eval      ilgi1pkf=arbpkc
174200161018     c                   other
174300161018     c                   eval      ilgi1pkf=arbpkf
174400161018    3c                   endsl
174500161018     c                   movel     fnlgi1ds      kpjbu
174600161018     c                   call      'FNLGI1R'
174700161018     c                   parm                    kpjba
174800161018     c                   movel     kpjbu         fnlgi1ds
174900161201     c* Per IPG --> corretto se il flag da portare a mag giac = "Y"
175000161025     c* Per IMG --> corretto se il flag da portare a mag giac = "Y"  o già in giac
175100161026     c*         --> ignoro anche se deve uscire da mag gaicenza, ma non ancora effettuat
175200170329    2c***                if        (*in11 and olgi1AMG<>'Y'and
175300170329     c***                           olgi1AMG<>'S') or
175400170329     c***                          (not *in11 and olgi1AMG<>'Y' and
175500170329     c                   if        (olgi1AMG<>'Y' and
175600161025     c                             olgi1AMG<>'S'  and
175700161026     c                             olgi1inmg<>'S' and olgi1outmg<>'S')
175800161018     c*
175900161018     c                   select
176000170329     c**                 when      olgi1inmg<>' '
176100170329     c**                 eval      ecce_descr=' -già in Giacenza'
176200161026     c                   when      arbndc>0  and arbdcm=0
176300161018     c                   eval      ecce_descr=' -di bolla in distinta '
176400161025     c                   when      OLGI1OUTMG<>' '
176500161025     c                   eval      ecce_descr=' -non più giacente'
176600161018     c                   other
176700161025     c                   eval      ecce_descr=' -di colli IMA'
176800161018     c                   endsl
176900170329     c
177000170330     c* se trovo una spunta con data  o ora MAGGIORE "tolgo" la eccedenza
177100170330     c* salvo il record di BRV chesto leggendo
177200170329     C                   MOVEL     RECBRV        WSAVr
177300170330     c
177400170330     c                   exsr      RicSpu
177500170330     c
177600170330     C                   MOVEL     WSAVr         RECBRV
177700170330     c
177800170330     c                   movel     brvdfs        brvimm           14 0
177900170330     c                   move      brvhms        brvimm
178000170330
178100170330     c* considero soltanto spunte cat 4 e IMA
178200170330     c                   clear                   wstampa
178300170330    3c                   if        wspg='A' or wnpg=4
178400170330    4c                   if        wdfv>savdfv  or
178500170330     c                             (wdfv=savdfv and wimm>brvimm)
178600170330     c                   eval      wstampa='N'
178700170330    4c                   endif
178800170330    3c                   endif
178900170329     c*
179000170330    3c                   if        wstampa<>'N'
179100161018    4c                   if        *in01 and not *inOF and uno=' '
179200161018     c                   except    DETvuota
179300161018    4c                   endif
179400161018
179500161018     c                   exsr      ERRORE
179600161018     c                   eval      UNO='S'
179700170330    3c                   endif
179800170330    2c                   endif
179900161025    1c                   endif
180000161018     c
180100161018     c                   setoff                                       18
180200161018     c                   seton                                        12
180300161018     C                   ENDSR
180400941017     C**************************************************************************
180500941017     C*   R O U T I N E     I N I Z I A L E
180600941017     C**************************************************************************
180700941017     C     *INZSR        BEGSR
180800941017     C*
180900161017     C* DEFINIZIONE CHIAVI
181000000126     C     KBRV          KLIST
181100000126     C                   KFLD                    IMANPG
181200000126     C                   KFLD                    WNFV
181300000126     C                   KFLD                    VIDFGS
181400070118     C     KBRV1         KLIST                                                  FNBRV01L
181500941019     C                   KFLD                    IMANPG                         ..CATEGORIA
181600000126     C                   KFLD                    WNFV                           ..NR. FOGLIO
181700950316RM*  C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
181800941019     C                   KFLD                    ARTFLS                         ..LNP
181900941018     C                   KFLD                    ARTLNA                         ..LNA
182000941018     C                   KFLD                    ARTNRS                         ..SERIE
182100941018     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
182200070418     C     KBRV4         KLIST                                                  FNBRV01L
182300070418     C                   KFLD                    CONNPG                         ..CATEGORIA
182400070418     C                   KFLD                    ARTFLS                         ..LNP
182500070418     C                   KFLD                    ARTLNA                         ..LNA
182600070418     C                   KFLD                    ARTNRS                         ..SERIE
182700070418     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
182800070418     C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
182900990212RM*  C     KARTD         KLIST
183000990212RM*  C                   KFLD                    ARTFLS
183100990212RM*  C                   KFLD                    ARTLNA
183200990212RM*  C                   KFLD                    ARTNRS
183300990212RM*  C                   KFLD                    ARTNSC
183400081014     C     KARTb         KLIST
183500081014     C                   KFLD                    brvlnp
183600081014     C                   KFLD                    brvlnA
183700081014     C                   KFLD                    brvNRS
183800081014     C                   KFLD                    brvNSC
183900941017     C     KFVV          KLIST                                                  *
184000941019     C                   KFLD                    IMANPG                         *
184100941017     C                   KFLD                    COMNFV                         *
184200941028     C                   KFLD                    VIDFGS                         *
184300950316RM*  C     KFVV2         KLIST
184400950316RM*  C                   KFLD                    BRVNPG
184500000126RM*  C                   KFLD                    WNFV5
184600950316RM*  C                   KFLD                    BRVFGS
184700941017     C     KANM          KLIST                                                  FNANM02L
184800941017     C                   KFLD                    ARTFLS                         *
184900941017     C                   KFLD                    ARTLNA                         *
185000941017     C                   KFLD                    ARTNRS                         *
185100941018     C                   KFLD                    ARTNSC                         *
185200941017     C                   KFLD                    ANMCAA                         *
185300941017     C     KART          KLIST                                                  *
185400941017     C                   KFLD                    ARBAAS                         *
185500941017     C                   KFLD                    ARBLNP                         *
185600941017     C                   KFLD                    ARBNRS                         *
185700941017     C                   KFLD                    ARBNSP                         *
185800081014     C     KARB          KLIST                                                  *
185900081014     C                   KFLD                    ARtAAS                         *
186000081014     C                   KFLD                    ARtLNP                         *
186100081014     C                   KFLD                    ARtNRS                         *
186200081014     C                   KFLD                    ARtNSP                         *
186300040603     C     Kar5          KLIST                                                  *
186400040603     C                   KFLD                    ARBAAS                         *
186500040603     C                   KFLD                    ARBLNP                         *
186600040603     C                   KFLD                    ARBNRS                         *
186700040603     C                   KFLD                    ARBNSP                         *
186800040603     C                   KFLD                    ktrd                           *
186900970721     C     KGCA          KLIST                                                  *
187000970721     C                   KFLD                    ARBAAS                         *
187100970721     C                   KFLD                    ARBLNP                         *
187200970721     C                   KFLD                    ARBNRS                         *
187300970721     C                   KFLD                    ARBNSP                         *
187400990212     C                   KFLD                    KFRG
187500950810     C     KSAV          KLIST                                                  *
187600950810     C                   KFLD                    SAVAAS                         *
187700950810     C                   KFLD                    SAVLNP                         *
187800950810     C                   KFLD                    SAVNRS                         *
187900950810     C                   KFLD                    SAVNSP                         *
188000941017     C     KLBL          KLIST                                                  *
188100941017     C                   KFLD                    LBLAAN                         *
188200941017     C                   KFLD                    LBLLPN                         *
188300941017     C                   KFLD                    LBLNRN                         *
188400941017     C                   KFLD                    LBLNSN                         *
188500941017     C     KTAB          KLIST
188600941017     C                   KFLD                    CODUT
188700941017     C                   KFLD                    COD
188800941017     C                   KFLD                    KEY
188900050510     C     KTAB2         KLIST
189000050510     C                   KFLD                    CODUT
189100050510     C                   KFLD                    COD
189200990212     C     KDCD          KLIST
189300990212     C                   KFLD                    DCDAAC                         *
189400990212     C                   KFLD                    DCDFIL                         *
189500990212     C                   KFLD                    DCDNCA                         *
189600021203
189700021203     C     Kfapd         klist
189800021203     C                   kfld                    kpdtip
189900021203     C                   kfld                    kpdpdr
190000941017     C*
190100970721     C     *LIKE         DEFINE    GCPFRG        KFRG
190200970721     C                   MOVEL     0             KFRG
190300941017     C     *LIKE         DEFINE    TBLCOD        COD
190400941017     C     *LIKE         DEFINE    TBLKEY        KEY
190500941019     C     *LIKE         DEFINE    BRVNPG        IMANPG                         CATEGORIA FOGLIO
190600070418     C     *LIKE         DEFINE    BRVNPG        conNPG                         CATEGORIA FOGLIO
190700031218     C     *LIKE         DEFINE    BRVNFV        WNFV
190800031218     C     *LIKE         DEFINE    FVVNFV        WNFV5
190900941018     C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO VIAGGIO
191000941018     C     *LIKE         DEFINE    ARTDAM        STADAM                         DATA ARRIVO MERCE
191100941018     C     *LIKE         DEFINE    ARBSPE        COMSPE
191200941019     C     *LIKE         DEFINE    ARBLNA        KLNA                           LINEA ARRIVO
191300031218     C     *LIKE         DEFINE    FVVFGS        SAVFGS
191400950317RM*  C     *LIKE         DEFINE    FVVDFV        SAVDFV
191500941017     C*
191600101220      /free
191700101220       // Carico le categorie con ambito Arrivo
191800101220       xx=2   ;
191900101220       // Alla posizine 1 metto al defluenza
192000101220       npgarr(1)='1D'  ;
192100101220
192200101220       setll   (1:'7N') tabel00f   ;
192300101220       reade   (1:'7N') tabel00f   ;
192400101220
192500101220       dow not %eof(tabel00f)      ;
192600101220       if tblflg=' '               ;
192700101220       ds7n=tbluni                 ;
192800101220
192900101220       if §7ntbo<>' '              ;
193000101220
193100101220          if §7ntbo='A'               ;
193200101220          npgArr(xx)=tblkey;
193300101220          xx=xx+1  ;
193400101220          endif  ;
193500101220       else   ;
193600101220
193700101220         if  §7ntb1='A'                         ;
193800101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc1   ;
193900101220         xx=xx+1  ;
194000101220         endif                                  ;
194100101220         if  §7ntb2='A'                         ;
194200101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc2   ;
194300101220         xx=xx+1  ;
194400101220         endif                                  ;
194500101220         if  §7ntb3='A'                         ;
194600101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc3   ;
194700101220         xx=xx+1  ;
194800101220         endif                                  ;
194900101220         if  §7ntb4='A'                         ;
195000101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc4   ;
195100101220         xx=xx+1  ;
195200101220         endif                                  ;
195300101220         if  §7ntb5='A'                         ;
195400101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc5   ;
195500101220         xx=xx+1  ;
195600101220         endif                                  ;
195700101220         if  §7ntb6='A'                         ;
195800101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc6   ;
195900101220         xx=xx+1  ;
196000101220         endif                                  ;
196100101220
196200101220       endif                       ;
196300101220       endif                       ;
196400101220
196500101220       reade   (1:'7N') tabel00f   ;
196600101220       enddo                       ;
196700101220      /end-free
196800101220
196900941017     C                   ENDSR
197000101202
197100101202      *---------------------------------------------------------------*
197200101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
197300101202      *---------------------------------------------------------------*
197400101202     c     sr_DatiJob    BEGSR
197500101202      *
197600101202     c     *dtaara       define    §azute        AZUTEds
197700101202     c     *dtaara       define    §datiute      dDatiUte
197800101202      *
197900101202     c                   in(E)     *dtaara
198000101202     c                   IF        %Error or RSUT = *blanks
198100101202     c                   clear                   TIBS34ds
198200101202     c                   call      'TIBS34R'
198300101202     c                   parm                    TIBS34ds
198400101202     c                   in        *dtaara
198500101202     c                   ENDIF
198600101202      *
198700101202     c                   ENDSR
198800941017     C**************************************************************************
198900161028     Otigcp000  E            AGG5
199000161017     O                       GCPDLMa
199100950316RM*  OPRTF198   E            TES              02
199200911015     O                       STA(1)              66
199300950810     O              N10      STA(2)             132
199400161017     O               10N11   STA(18)            132
199500170124     O*********      10 11   STA(22)            132
199600950320     O                       STA(17)            198
199700900626     O                       RSUT                20
199800960927     O                       UDATE8             181 '  /  /    '
199900950320     O                       PAGE          Z    198
200000920813     O          E            TES         1
200100160503     o                       statipo             67
200200160503     o                       statipo2           130
200300050511     O                       UTIME              179 '  :  :  '
200400931228     O               02      STA(4)              70
200500931228     O          E    02      TES         0  0
200600931228     O                       STA(4)              70
200700931228     O          E            TES         2
200800911015     O                       STA(3)              66
200900931228     O                       STA(16)            132
201000060508     O                       STA(20)            198
201100161018     O              n11                          13 'I.M. .'
201200161018     O              n11      VIDTIN              12
201300170124     O*********      11                          13 'I.P.G.'
201400060508RM*  O                       SAVFGS              21
201500060508     O                       DESCDU              42
201600920813     O                       COMNFV        Z     56
201700950317     O                       STADFV        8     73
201800960927     O                       VIDFIL             116
201900960927     O                       DESFIL             137
202000060508     O                       stalna             177
202100060508     O                       DESlna             198
202200070418     O          E   n10      TES         1
202300070418     O                       STA(21)            198
202400070418     O                       stamdc             188
202500101215     O                       staimaa            146
202600920813     O          E            TES2        2
202700911018     O                       STA(5)              66
202800911018     O                       STA(8)             132
202900040621     O                       STA(19)            198
203000920813     O          E            TES2        1  1
203100911018     O                       STA(6)              66
203200911018     O                       STA(9)             132
203300911015     O*
203400090522     O          E            DETnoBO     1
203500090522     O                       brvLNP        Z      8
203600090522     O                       brvNRS        Z     11
203700090522     O                       brvLNA        Z     29
203800090522     O                       brvZNC              87
203900090522     O                       brvnps             122
204000090522     O                       brvNSC        Z    131
204100090522     O
204200090522     O          E            DET         1
204300090522     O                  05   ARBAAS        Z
204400090522     O                  05   ARBLNP        Z   +  1
204500090522     O                  05 03ARBNRS        Z   +  1
204600090522     O                  05 03                  +  0 '-'
204700090522     O                  05   ARBNSP        Z   +  0
204800090522     O                  05   STADSP            +  1 '  /  '
204900090522     O                  05   ARBLNA        Z   +  1
205000090522     O                  05   STARSM            +  1
205100090522     O                  05   STARSD            +  1
205200090522     O                  05N04ARBNCD        Z   +  1
205300090522     O                  05N04                  +  0 '/'
205400090522     O                  05N04ARBNCA        Z   +  0
205500090522     O                  05 04                    84 'NON SEQUENZIALI'
205600090522     O                  05   ARBZNC            +  1
205700090522     O                  05   stabva            +  0
205800090522     O                  05   ARBTMC            +  1
205900090522     O                  05N06ARBPDC        Z   +  1
206000090522     O                  05N06                  +  0 '-'
206100100707     O                  05   DESCPC            +  0
206200090522     O              N07      STADAM            +  1 '  /  /    '
206300090522     O              N07      ARTNAP            +  1
206400090522     O              N07      ARTNSC        Z   +  2
206500090522     O               07                         131 'ABILIT.MERCE FORZATA'
206600081014     O* ERRORI:
206700950411     O*
206800081014     O          E            DETERR      0  0
206900031219     O                    n13                   155 'NON SPUNTATO IN DATA '
207000031219     O                    n13STADFV        8   +  1
207100950316     O*
207200040621     O                     13brvFGS             137
207300040621     O                     13                   138 '-'
207400040621     O                     13ST2FGS             146
207500161025     O                     13                   149 'in'
207600070118     O                     13ST2CAT             153
207700070118     O                     13brvNPG             155
207800070118     O                     13                   156 '-'
207900070118     O                     13brvNFV        Z    162
208000161025     O                     13                   165 'il'
208100070118     O                     13ST2DFV             171 '  /  '
208200161026     O**                   13                   175 'ore'
208300161026     O                     13staHMS             178 '  :  '
208400161130     O                       STADSF             198
208500070418     O          E            DETndc      1
208600070418     O                                           87 'In distinta:'
208700070418     O                       ARBndc        2  +   1
208800070418     O                                        +   2 'del'
208900070418     O                       staddc            +  1 '  /  /    '
209000081014     o* Collo doppio
209100081014     O          E            DETdoppio   0  0
209200161018     O                                          153 'COLLO   D O P P I O'
209300161018     o* Collo eccedente
209400161018     O          E            DETeccede   0  0
209500161018     O                                          153 'COLLO   E C C E D E'
209600161018     O                                          159 ' N T E'
209700161018     O                       ecce_descr         185
209800100706     o* Collo presente da XX giorni in IMA
209900100706     O          E            DETspuIMA   0  0
210000100707     O                                          160 '   COLLO SPUNTATO PER '
210100100707     O                                        +   1 'ALMENO '
210200100706     O                       §VPOGGIMA     2  +   1
210300100707     O                                        +   1 'GIORNI in I.M.A.'
210400081014     o* riga vuota
210500081014     O          E            DETvuota    2
210600081014     O                                          157 ' '
210700050405     O* STAMPO NUMERO C.A.e ubicazione collo
210800990212     O          E            DETCA       1
210900050405     O               24                          87 'Ubicazione collo:'
211000050405     O               24      §ar5zng           +  2
211100050405     O               25                         138 'C.A.'
211200050405     O               25      DCTFIL            +  1
211300050405     O               25      DCTNCA        Z   +  1
211400050405     O               25                        +  1 'del'
211500050405     O               25      STADCA            +  1 '  /  /  '
211600990212     O                     26                  +  1 'chiusa il'
211700990212     O                     26STACHC            +  1 '  /  /  '
211800990303     O                     26DCTCCH            +  1
211900990303     O                     26                  +  0 '-'
212000990303     O                     26STACCH            +  0
212100920813     O          E            OK          3
212200920813     O                       STA(11)             66
212300920813     O          E            OK          0  0
212400920813     O                       STA(11)             66
212500920813     O*
212600920813     O          E            TOT1        3  1
212700920813     O                       STA(13)             66
212800920813     O          E            TOT2           1
212900920813     O                       NPS(C)              33
213000920813     O                       CPS(C)        2     58
213100920813     O          E            TOT3           1
213200920813     O                       STA(14)             66
213300920813     O          E            TOT3           1
213400920813     O                       STA(15)             66
213500920813     O                       TOTSPU        2     58
213600920813     O*
213700920813     O          E            TOT3        3
213800920813     O                       STA(12)             66
213900920813     O          E            TOT3        0  0
214000920813     O                       STA(12)             66
214100941017     O**************************************************************************
214200950316** STA
214300060508XXXXXXXXXXXXXXXXXXXX                                                 1
214400950320 ** I.M.A. : INVENTARIO MAGAZZINO ARRIVI **                          2
214500161018FOGLIO        di  XXX-XXXXXXXXXXXXXXXXXXXX    N.ro XXXXX   del XX/   3
214600931228                                       *** D E F I N I T I V O ***   4
214700040603    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO        5
214800941019ANNO LNP SER NUMERO  DATA ARR                                        6
214900040603XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx  7
215000040603     SEGNACOLLI    ZON ULT  AUTOTRASPORT.   A R R I V O SEGNACOLLO   8
215100040603      DAL /   AL   ARR CONS   CONSEGNA       DATA   PIST MANCANTE    9
215200040603XX XXXXXXX/XXXXXXX XXV X XXXXXXX-XXXXXXXXX XX/XX/XXXX XX  XXXXXXX   10
215300920813             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
215400920813 **  FINE  STAMPA  **
215500920813   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI      13
215600920813                        ---------------------------------------     14
215700920813               TOTALE                                               15
215800060508XX/XXXX           Punto Operativo in Gestione: XXX-xxxxxxxxxxxxxxx  16
215900950320                         FNLR31R                          PAG.XXXX  17
216000950810 ** I.M.G. : INVENTARIO MAGAZZINO GIACENZE **                       18
216100040621 S  P  U  N  T  A  T  O      D  A                                   19
216200060508xxxxx           Linea Arrivo Selezionata: XXX-xxxxxxxxxxxxxxxxxxxx  20
216300070418                Elabora anche spedizioni in distinta: XX            21
216400161031 ** I.P.G. : QUADRATURA COLLI A MAGAZZINO GIACENZE **               22
216500161017**
216600950316OVRPRTF FILE(PRTF198) USRDTA('I.M.A.')
216700161017**
216800161017DLYJOB DLY(01)
