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
011300170213     D og148         E DS
011400170323     D og150         E DS
011500170213     D DS7B          E DS
011600161017     D DS7C          E DS
011700101220     D DS7N          E DS
011800920318     D DS3A          E DS
011900050511     D DS03          E DS
012000040603     D DAR5GEN       E DS
012100100706     D DVPO          E DS
012200990212     D DTAD            DS
012300990212     D  §TARGR                26     26
012400990303     D DCCH            DS
012500990303     D  §CHDES                 1     50
012600990303     D  §CHIMA                53     53
012700070118     D RECBRV        E DS                  EXTNAME(FNBRV00F)
012800070118     D WSAVB         E DS                  EXTNAME(FNBRV00F)
012900031219     d                                     prefix(S_)
013000170329     D WSAVr         E DS                  EXTNAME(FNBRV00F)
013100170329     d                                     prefix(r_)
013200990212     D DSBS02        E DS                  EXTNAME(TIBS02DS)
013300160927     D tigcp00ds     E DS                  EXTNAME(tigcp00F)
013400040427      * DS Esterna x gestione allocazione ed invio messaggi
013500040427     D TRUL82DS      E DS
013600081014      * DS Esterna x controllo colli doppi
013700081014     D FNLRV7DS      E DS
013800160927      * DS Esterna x controllo colli in IMG
013900160927     D FNLGI1DS      E DS
014000040806      * ds per il controllo della presenza di CA per la spedizione richiesta
014100040806     d FIDN12DS      e ds
014200040806     d  skKey                 26    305    dim(20)
014300040806     d  skAnn                306    325    dim(20)
014400040806     d  skDca                326    485  0 dim(20)
014500101202     d**skFca                486    545  0 dim(20)
014600040806     d  skDch                546    705  0 dim(20)
014700040806     d  skCch                706    745    dim(20)
014800101202      *
014900040806     d dsKey           ds
015000040806     d  dsaac                         4  0
015100040806     d  dsfil                         3  0
015200040806     d  dsnca                         7  0
015300040806      *
015400101202      *
015500021203      *
015600101220     d WnpgArr         s              2
015700170330     d Wnpg            s                   like(brvnpg)
015800101220     d Trov            s              2  0
015900090522     d Nobolla         S              1
016000170330     d wstampa         S              1
016100170125     d winGiac         S              1
016200090522     d uno             S              1
016300081014     d statipo         S             21
016400160503     d statipo2        S             21
016500101215     d staimaa         S             44
016600070418     d stamdc          S              2
016700160503     d indMP           S              4  0
016800160503     d ind12           S              4  0
016900170213     d iddgdopo        S              1    inz
017000170323     d picking         S              1    inz
017100050511     d WSPG            S                   LIKE(FVVSPG)
017200040108     d kpdpdr          S                   LIKE(APDpdr)
017300031219     d wdfv            S                   LIKE(d53dfv)
017400031219     d kpdtip          S                   LIKE(APDtip)
017500040603     d ktrd            S                   LIKE(ar5trd) inz('GEN')
017600161018     d ecce_descr      S             25
017700100706     d dataEUR         s               d   datfmt(*eur)
017800100706     d DataISO         s               d   datfmt(*iso)
017900101202     d*
018000040428     D msgjob          C                   CONST('La Manut.Bolle sta bloccando -
018100040428     D                                     l''elabor. Dell''IMA:USCIRE SUBITO D-
018200040428     D                                     A QUESTO LAVORO')
018300050511     D CPARCEL         C                   CONST('     P A R C E L     ')
018400050511     D CMessag         C                   CONST('M E S S A G G E R I E')
018500160503     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
018600160503     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
018700101215     D CsoloLNA        C                   CONST('Solo colli spuntati alla line-
018800101215     d                                     a arrivo finale')
018900911018     C*--------------------------------------------------------------- *
019000911018     C*                  INDICATORI                                    *
019100911018     C*--------------------------------------------------------------- *
019200911018     C* 01    - ESITONO ANOMALIE DI MANCANZA
019300950810     C* 02    - CHIESTA CHIUSURA  FOGLIO
019400911018     C* 03    - ESISTE SERIE
019500911018     C* 04    - SEGNACOLLI NON SEQUENZIALI
019600911018     C* 05    - ON CAMBIATA SPEDIZIONE
019700920325     C* 06    - NON C'E' COD PADRONCINO CONSEGNA
019800920708     C* 07    - MANCA DATA ARRIVO : SPEDIZIONE FORZATA
019900920813     C* 08    - BOLLA GESTITA
020000170124     C* 11    - e 10 ON --> IPG  ma ora eliminato
020100950810     C* 10    - ON --> IMG  OFF --> IMA
020200081014     C* 12    - CONTROLLO COLLI DOPPI
020300950810     C* 13    - NON TROVATA SPUNTA IN IMA MA  IN ALTRO FOGLIO NELLA
020400950810     C*         STESSA DATA
020500950810     C* 14    - NON TROVATA SPUNTA IN IMA
020600031219     C* 15    - TROVATA SPUNTA SU ALTRO FOGLIO MA DEVO CREARE L'ANOMAL
020700100706     C* 16    - collo in IMA ma spuntato in IMA più di XX volte...da tabella VPO
020800161018     C* 18    - collo eccedente
020900170124     C* 22    - FILIALE partita con nuova quadratura IMA (e non più IPG)
021000170323     c*         e non ha il picking la nuova procedura non può essere fatta quindi
021100170323     c*         l'indicatore viene sempre spento
021200050405     C* 24    - STAMPA ubicazione collo se c'e'
021300050405     C* 25    - STAMPA se c'e' c.a.
021400050405     C* 26    - STAMPARE DATA CHIUSURA C.A.
021500941017     C* 30    - LETTURA FNARB
021600941017     C* 31    - LETTURA FNART
021700911018     C* 32/34 - COMODO
021800911018     C*---------------------------------------------------------------*
021900900622     C     *ENTRY        PLIST
022000900622     C                   PARM                    KPJBA
022100941017     C                   MOVEL     KPJBU         PARAM1
022200000126     C                   Z-ADD     COMNFV        WNFV
022300900828     C*-----------------------------------------------------
022400950810     C* IMPOSTAZIONI INIZIALI
022500950810     C                   EXSR      IMPO
022600950810     C*-----------------------------------------------------
022700170124     c                   setoff                                       11
022800161018     c* il dly mi serve solo per il debug in batch
022900161017     C                   Z-ADD     80            LUNG             15 5
023000161017     C                   MOVEL     *BLANKS       COMMAN           80
023100161017     C                   MOVEA     VIDTIN        CMD(35)
023200161017     C                   MOVEA     CMDd          COMMAN
023300161017     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
023400161017     C                   PARM                    COMMAN
023500161017     C                   PARM                    LUNG
023600161017
023700941019     C                   Z-ADD     80            LUNG             15 5
023800941019     C                   MOVEL     *BLANKS       COMMAN           80
023900950810     C                   MOVEA     VIDTIN        CMD(35)
024000941019     C                   MOVEA     CMD           COMMAN
024100170124     c**                 if        *in11
024200170124     c**                 eval      %subst(comman:31:6)='I.P.G.'
024300170124     c**                 endif
024400941019     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
024500941019     C                   PARM                    COMMAN
024600941019     C                   PARM                    LUNG
024700950810     C*
024800950316RM*  C                   OPEN      PRTF198
024900980128     C                   SETON                                        OF
025000911015     C*
025100040108     C* CONSIDERO SOLO i p.o. di £6
025200941019     C                   Z-ADD     1             WA                3 0          CONTATORE
025300040108    1C     L6(WA)        DOWNE     *ZERO
025400941019     C*
025500040108     C                   Z-ADD     L6(WA)        KLNA                           LINEA ARRIVO
025600161017     C*
025700031218     C     KLNA          SETLL     FNARB05L
025800040427     C     KLNA          READE     FNARB05L                               30
025900950810     C*
026000950810    3C     *IN30         DOWEQ     *OFF
026100050511     c* Se ima messaggerie o parcel, escludo le zone non di interesse
026200160503     c                   eval      indMP=0
026300160503     c                   eval      ind12=0
026400160503     c
026500160503     c                   if        wselzona='S'
026600170331     c
026700170331     c* Eseguo la ricerca per linea/zona per tenere in considerazione le filiali in £6
026800170331     c                   movel     arblna        alfzona           5
026900170331     c                   move      arbznc        alfzona
027000160503
027100160503     c                   if        wparc='S' or wmess='S'
027200160503     c***  alfzona       lookup    zone                                   35
027300160503     c                   eval      indMP =%lookup(alfzona:zone)
027400050511     c                   endif
027500160503     c                   if        wparc1='S' or wparc2='S'
027600160503     c                   eval      ind12 =%lookup(alfzona:zone12)
027700160503     c                   endif
027800160503
027900160503     c                   endif
028000160503   3ac                   if        wselzona=' ' or  indmp>0 or  ind12>0
028100920813     C*
028200920318     C* CHAIN CODICE BOLLE PER ESCLUDERE I RECUPERI
028300920318     C                   MOVEL     '3A'          COD
028400950810     C                   MOVEL(P)  ARBCBO        KEY
028500941018     C     KTAB          CHAIN     TABEL00F                           35
028600920318     C  N35              MOVEL     TBLUNI        DS3A
028700920318     C   35              MOVEL     'F1'          §3ATB1
028800920318     C*
028900950810     C* ESCLUDO:  LE BLOCCATE CON "B"/
029000930518     C*           QUELLE IN CONSEGNA / QUELLE CONSEGNATE / I RECUPERI
029100161025    4C     ARBdcm        ifeq      *zeros
029200950810     C     §3ARBL        ANDNE     'R'
029300950810     C     ARBFBC        ANDNE     'B'
029400000512     C*
029500960516     C* CONTROLLO SE E' GIACENTE
029600960516     C                   CLEAR                   WGIAC
029700960516     C     ARBFBC        IFEQ      'G'
029800161025     C     KGCA          CHAIN(n)  Tigcp21L                           35
029900970721     C* SE C'E' LA GIACENZA E NON E' A FASE 35 LA ESCLUDO
030000970721     C  N35GCPFAS        IFLT      35
030100050331     C     GCPFAS        oreq      36
030200170124     c* la merce è in giacenza : verifico con driver
030300160927     c                   clear                   fnlgi1ds
030400160927     c                   eval      ILGI1AAS=arbaas
030500160927     c                   eval      ILGI1lnp=arblnp
030600160927     c                   eval      ILGI1nrs=arbnrs
030700160927     c                   eval      ILGI1nsp=arbnsp
030800161018    3c                   select
030900161018     c                   when      arbpkc>0 and arbncp=arbncl
031000161018     c                   eval      ilgi1pkf=arbpkc
031100161018     c                   when      arbpkc>0 and arbncp<arbncl
031200161018     c                             and arbpkc>arbpkf
031300161018     c                   eval      ilgi1pkf=arbpkc
031400161018     c                   other
031500161018     c                   eval      ilgi1pkf=arbpkf
031600161018    3c                   endsl
031700170323     c                   movel     vidfil        ilgi1fgs
031800170323     c
031900170323     c* visto che l'ho già calcolato, lo passo al driver
032000170323     c                   if        picking<>' '
032100170323     c                   movel     picking       ILGI1PICK
032200170323     c                   else
032300170323     c                   movel     'N'           ILGI1PICK
032400170323     c                   endif
032500170323     c
032600160927     c                   movel     fnlgi1ds      kpjbu
032700160927     c                   call      'FNLGI1R'
032800160927     c                   parm                    kpjba
032900160927     c                   parm                    tigcp00ds
033000161007     c                   movel     kpjbu         fnlgi1ds
033100160927
033200170125     c* per IMG: sped portata nel giorno a mag. giac DEVE essere in giacenza
033300170125     c*          e quindi elaborata con mancanza
033400170125   5ac   10              if        olgi1AMG='Y' or olgi1inmg='S'
033500970721     C                   MOVEL     'S'           WGIAC             1
033600160823     c                   endif
033700170125   5ac   10              if        olgi1AMG='S'
033800170125     C                   MOVEL     'D'           WGIAC             1
033900170125     c                   endif
034000170125     c* per IMA: devo elaborare sped da portare a mag giac o portate nel giorno
034100161024     c* se la bolla è da portare a mag giacenza OGGI, devo considerarla anche in IMA
034200161024     c*  e se manca dare segnalazione ma NON creando l'anomalia
034300170125   5ac  n10              if        olgi1AMG='S' or olgi1AMG='Y'
034400161024     C                   MOVEL     'D'           WGIAC             1
034500161024     c                   endif
034600170125   5ac  n10              if        olgi1inmg='S'
034700170125     C                   MOVEL     'S'           WGIAC             1
034800170125     c                   endif
034900161201     c*
035000170124     c* se non è partito con NEW IMA considero  la spedizione GIACENTE  come prima
035100170201   5ac*****              if        not *in22 and olgi1AMG='S'
035200170201   5ac                   if        not *in22 and WGIAC='D'
035300161201     C                   MOVEL     'S'           WGIAC             1
035400161201     c                   endif
035500161201     c
035600170124     c* IPG elaboro le giacenze da portare a mag giac OGGI ( non facciamo più)
035700170124   5ac** 11              if        olgi1AMG='S' or olgi1AMG='Y'
035800170124     C**                 MOVEL     'D'           WGIAC             1
035900170124     c**                 endif
036000160823     c
036100960516     C                   ENDIF
036200970721     C                   ENDIF
036300950810     C*
036400161025     C* SE IMA --> ESCLUDO LE GIACENTI ma includo le bolle che devono essere portate a mag giac
036500170124     c*            se partita la procedura NEWIMA
036600950810    5C     *IN10         IFEQ      *OFF
036700960516     C     WGIAC         ANDNE     'S'
036800170124     c     *in22         andeq     *on
036900170124     c***  *in20         andeq     *on
037000161130
037100161130    5C     *IN10         orEQ      *OFF
037200161130     C     WGIAC         ANDeq     ' '
037300170124     c     *in22         andeq     *off
037400170124     c**** *in20         andeq     *off
037500950810     C*
037600950810     C* SE IMG --> INCLUDO SOLO LE GIACENTI
037700161026     C*        --> INCLUDO anche le bolle da portare a mag.giac. ma senza stampare il mancante
037800950810     C     *IN10         OREQ      *ON
037900161025     C     *IN11         andeq     *Off
038000161026     C     WGIAC         ANDne     ' '
038100161025
038200170124     C* SE IPG --> INCLUDO SOLO quelle da portare a mag giac ( eliminato)
038300170124     C**   *IN10         OREQ      *ON
038400170124     C**   *IN11         andeq     *ON
038500170124     C**   WGIAC         ANDEQ     'D'
038600921013     C*
038700950317     C* ESCLUDO  SE BOLLA MAMMA E UNA DELLE FIGLIE E' IN CONSEGNA
038800960516     C*     O SE SONO TUTTE BLOCCATE
038900950810     C                   Z-ADD     ARBSPE        SAVSPE
039000960516     C*
039100960516     C                   CLEAR                   WESFI                          ESISTE FIGL
039200960516     C                   MOVEL     'S'           WFIBLO            1            FIGLIE BLOCC
039300950317     C                   SETOFF                                       36
039400950810     C     KSAV          SETLL     FNLBL02L
039500950810     C     KSAV          READE     FNLBL02L                               35
039600960516     C**
039700950810    6C     *IN35         DOWEQ     *OFF
039800950317     C*
039900950317     C     KLBL          CHAIN     FNARB01L                           35
040000950810    7C     *IN35         IFEQ      *OFF
040100960516     C                   MOVEL     'S'           WESFI             1
040200960516     C* FIGLIA IN CONSEGNA
040300960516    8C     ARBNDC        IFNE      0
040400950317     C                   SETON                                        3536
040500960516   X8C                   ELSE
040600960516     C*
040700960516     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
040800960516    9C     ARBFBC        IFNE      'B'
040900960516     C                   MOVEL     'N'           WFIBLO
041000960516    9C                   ENDIF
041100960516    8C                   ENDIF
041200960516    7C                   ENDIF
041300950317     C*
041400950810     C  N36KSAV          READE     FNLBL02L                               35
041500950810    6C                   ENDDO
041600960516     C*
041700960516     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
041800960516     C  N36WESFI         IFEQ      'S'
041900960516     C     WFIBLO        ANDEQ     'S'
042000960516     C                   SETON                                        36
042100960516     C                   ENDIF
042200070418     c
042300070418     c* Per 36 off - controllo in IMA se richiesti anche i colli mancanti
042400070418     c*              di spedizioni in distinta
042500070418     c  n36              if        not *in10 and vidmdc='N' and arbndc>0
042600070418     c                   seton                                        36
042700070418     c                   endif
042800911204     C*
042900950810    6C  N36              DO
043000950317     C*
043100040427     C     KSAV          CHAIN     FNARB01L                           31
043200950810     C                   MOVEL     ARBAMA        SAVAMA            1 0
043300040428     c                   clear                   wchain            1
043400161025     c                   clear                   wesspu            1
043500911018     C* LEGGO DETTAGLIO
043600941017     C     KART          SETLL     FNART01L
043700040428     C  N10
043800040428     CAN 02KART          READE     FNART01L                               31
043900040428     C   10
044000040428     CORN02KART          READE(N)  FNART01L                               31
044100911018     C*
044200950810    7C     *IN31         DOWEQ     *OFF
044300911204     C                   SETOFF                                       34
044400950810     C*
044500950810     C* ESCLUDO  SE IL COLLO E' CONSEGNATO (CONSEGNA PARZIALE)
044600070418     C     ARTDCM        IFEQ      0
044700950810     C  N10              EXSR      ELAIMA
044800950810     C   10              EXSR      ELAIMG
044900950810    8C                   ENDIF
045000930629     C*
045100040428     C  N10
045200040428     CAN 02KART          READE     FNART01L                               31
045300040428     C   10
045400040428     CORN02KART          READE(N)  FNART01L                               31
045500950810    7C                   ENDDO
045600170124     c* per IMA  e IMG aggiorno la giacenza con la data entrata a mag giac se spuntato
045700170124     c*       (IPG viene eliminato quindi uso ima al suo posto)
045800170125     c*  almeno un collo e data = 0  solo in fase "definitivo"
045900170321     c* per le filiali che fanno l'IDD il giorno dopo, aggiorno anche per ima provvisorio
046000170125     c*
046100170124    7c*****              if        *in22 and *in10 and wesspu='S' and
046200170321    7c*****              if        *in22 and *in02 and wesspu='S' and
046300170321     c
046400170321    7c                   if        *in22 and wgiac<>' ' and wesspu='S' and
046500161025     C                             olgi1amg='S'
046600170321     c                   if        *in02 or iddgdopo='S'
046700161025     C     KGCA          CHAIN     Tigcp21L
046800161025    8c                   if        %found(tigcp21l)
046900161025     C                   MOVE      dateu         GCPDLMa
047000161025     C                   EXCEPT    AGG5
047100161025    8C                   ENDIF
047200170321     c
047300161025    7c                   endif
047400170321    7c                   endif
047500920318     C*
047600950810    6C                   ENDDO
047700950810    5C                   ENDIF
047800161025     c
047900000512    4C                   ENDIF
048000050511   3aC                   ENDIF
048100911018     C*
048200040427     C     KLNA          READE     FNARB05L                               30
048300950810    3C                   ENDDO
048400941019     C*
048500040108     C                   ADD       1             WA                             CONTATORE L6
048600950810    1C                   ENDDO
048700161017     c
048800920813     C*
048900920813     C* RIEPILOGO SPUNTE PER PISTOLA
049000920813     C                   EXSR      RIEPIL
049100911018     C*
049200060508     C* SE NON TROVATE DISCORDANZE e non fatte parzializzazioni
049300060508     c*  se non ho parzii
049400070418     c* 18/04/07 - IMA chiude solo se richiesta forzatura chiusura
049500960530     C     *IN01         IFEQ      *OFF
049600070418     C     *IN10         andeq     *ON
049700161017     C     *IN11         andeq     *Off
049800060508     c
049900160503     c     wselzona      andeq     ' '
050000060508     c
050100060508     c     vidlna        andeq     0
050200060508     c
050300060508     C*  O FORZATA LA CHIUSURA METTO FLAG
050400911018     C     VIDSN         OREQ      'S'
050500060508     c
050600941017     C     KFVV          CHAIN     FNFVV01L                           32
050700920813     C  N32              MOVEL     'S'           FVVFCF
050800941017     C  N32              UPDATE    FNFVV000
050900911018     C                   END
051000050511     C*
051100081014     C* CHIUSURA PGM
051200990212     C                   CLEAR                   DSBS02
051300990212     C                   MOVEL     'C'           T02TLA
051400990212     C                   CALL      'TIBS02R'
051500990212     C                   PARM                    KPJBA
051600990212     C                   PARM                    DSBS02
051700031219     c
051800031219     C                   CLEAR                   DSLV53
051900031219     C                   MOVEL     'C'           D53TLA
052000031219     C                   CALL      'FNLV53R'
052100031219     C                   PARM                    DSLV53
052200040806     C*
052300040806     C                   MOVEL     'C'           I12TLA
052400040806     C                   CALL      'FIDN12R'
052500040806     C                   PARM                    FIDN12DS
052600140430     C*
052700140430     c                   clear                   fnlrv7ds
052800140430     C                   MOVEL     'C'           ILRV7TLA
052900140430     c                   eval      kpjbu= fnlrv7ds
053000140430     C                   CALL      'FNLRV7R'
053100140430     C                   PARM                    KPJBA
053200160927
053300160927     c                   clear                   fnlgi1ds
053400160927     c                   eval      ILGI1TLA= 'C'
053500160927     c                   movel     fnlgi1ds      kpjbu
053600160927     c                   call      'FNLGI1R'
053700160927     c                   parm                    kpjba
053800160927
053900040806      *
054000000000     C                   SETON                                        LR
054100031219     C**************************************************************************
054200031219     C* CONTROLLO SE SEGNACOLLO MANCANTE
054300031219     C**************************************************************************
054400031219     C     CTRART        BEGSR
054500031219     C*
054600031219     C                   SETOFF                                       131415
054700100706     C                   SETOFF                                       3316
054800161025     C                   MOVE      *BLANKS       STADSF           17
054900031219     C*
055000070118     C     KBRV1         SETLL     FNBRV01L                               33
055100070418     c
055200161017     c* se non ho trovato spunta IMx e spedizione in distinta, cerco
055300070418     c*  la categoria 4 del giorno
055400070418    1c                   if        not *in33 and arbndc>0 and vidmdc='S'
055500100706     C                   Z-ADD     4             conNPG
055600140618     C     KBRV4         chain     FNBRV05L
055700140618    2c                   if        %found(fnbrv05l)
055800070418     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
055900070418     c                   clear                   dslv53
056000070418     C                   z-add     brvnFV        D53NFV
056100070418     C                   MOVEL     brvNPG        D53NPG
056200070418     C                   MOVEL     brvFGS        D53FGS
056300070418     C                   CALL      'FNLV53R'
056400070418     C                   PARM                    DSLV53
056500070418     C* ERRORE
056600070418     c*  se sparato nella stessa data: ok
056700070418     c*  e sparato in altra data, rientro nei giri normali di controllo
056800070418     c*  spunte
056900070418    3c                   if        d53err=' ' and d53dfv=savdfv
057000070418     c                   seton                                        33
057100070418    3c                   endif
057200070418    2c                   endif
057300070418    1c                   endif
057400070418     c
057500031219     C* 14 ON - SPUNTA NON TROVATA
057600031219    1C     *IN33         IFEQ      *OFF
057700070418     c
057800031219     C                   SETON                                        14
057900031219     c*
058000031219     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
058100031219     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
058200031219     c*  immissione + alta
058300031219     C                   exsr      RICSPU
058400031219     C
058500031219     C* SE SPUNTA TROVATA CON DATA > STAMPO MA NON CREO ANOMALIA
058600170213     c
058700170213     c* Se filiale che fa l'IDD il giorno dopo e trovo spunta IMA con data >
058800170213     c*  non stampo nemmeno e dico che la spunta è presente
058900170213     c*  non è detto che questo vada bene per la nuova procedura IMG. Da verificare con
059000170213     c*  DLO (es Salerno)
059100170213    2c                   if        not *in10 and iddgdopo='S' and
059200170213     c                             wdfv>savdfv and wspg='A'
059300170213     c                   setoff                                       14
059400170213   x2c                   else
059500170213     c
059600170213    3C                   IF        WDFV>SAVDFV
059700031219     C                   EVAL      *IN13=*ON
059800170213    3C                   ENDIF
059900031219     C
060000031219     C* SE SPUNTA TROVATA CON DATA = STAMPO E CREO ANOMALIA SOLTANTO SE
060100031219     C*  CATEGORIA 2 O 6
060200170213    3C                   IF        WDFV=SAVDFV
060300031219     C                   EVAL      *IN13=*ON
060400170213    4C                   IF        BRVNPG=2
060500031219     C                   EVAL      *IN15=*ON
060600170213    4C                   ENDIF
060700170213    3C                   ENDIF
060800170213    2C                   ENDIF
060900031219     c*
061000031219    1C                   END
061100960523     C*
061200031219     C
061300031219    1C                   IF        WDFV>=SAVDFV
061400960523     C* GIRO LA DATA
061500031219     C                   MOVE      WDFV          G02INV
061600960523     C                   MOVE      *ZEROS        G02DAT
061700960523     C                   MOVEL     3             G02ERR
061800960523     C                   CALL      'XSRDA8'
061900960523     C                   PARM                    WLBDAT
062000960523     C                   MOVEL     G02DAT        ST2DFV            4 0
062100960523     C**
062200960523     C* DECODIFICO LA FILIALE GESTIONE
062300031219     C     BRVFGS        CHAIN     AZORG01L                           32
062400031219    2C  N32ORGDE5        IFNE      *BLANKS
062500960523     C                   MOVEL     ORGDE5        ST2FGS            8
062600960523     C                   ELSE
062700960523     C                   MOVEL     ORGDES        ST2FGS            8
062800031219    2C                   ENDIF
062900960523     C**
063000960523     C* DECODIFICO LA CATEGORIA SPUNTA
063100031219    2C     BRVNPG        IFEQ      3
063200161017     C                   MOVEL     '7B'          COD
063300161017     C                   MOVEL(P)  wspg          KEY
063400161017     C     KTAB          CHAIN     TABEL                              32
063500161017     C  N32              MOVEL     TBLUNI        ds7b
063600161017     C   32              CLEAR                   ds7b
063700161017     C                   MOVEL     §7bdesb       ST2CAT            3
063800031219   X2C                   ELSE
063900960523     C                   MOVEL     '7N'          COD
064000031219     C                   MOVEL(P)  BRVNPG        KEY
064100960523     C     KTAB          CHAIN     TABEL                              32
064200960523     C  N32              MOVEL     TBLUNI        ST2CAT
064300960523     C   32              CLEAR                   ST2CAT
064400031219    2C                   ENDIF
064500031219     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
064600031219    2C                   IF        BRVNPG=4
064700031219     C                   Z-ADD     BRVNFV        WNFV5
064800031219     C     KFVV2         CHAIN(N)  FNFVV000                           32
064900161026     C  N32              eval      stadsf=fvvdsf
065000031219    2C                   ENDIF
065100031219    1C                   ENDIF
065200960523     C*
065300100706     c* Se trovata spunta, segnalo se sparata xx volte in IMA
065400100706     c*  solo per bolle in IMA più vecchie di ....
065500100707     c* Escludo bolle in distinta
065600150113     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
065700100707     c                   if        not *in14 and not *in10
065800100706     c                             and arbndc=0 and arbdsp<datapresIMA
065900100706     c
066000100706     c* conto quante volte presente in ima
066100100706     C                   Z-ADD     3             conNPG
066200100706     c                   clear                   conta             4 0
066300140618     C     KBRV4         setll     FNBRV05L
066400140618     C     KBRV4         reade     FNBRV05L
066500100706     c
066600140618    2c                   dow       not %eof(fnbrv05l)
066700100706     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
066800100706     c                   clear                   dslv53
066900100706     C                   z-add     brvnFV        D53NFV
067000100706     C                   MOVEL     brvNPG        D53NPG
067100100706     C                   MOVEL     brvFGS        D53FGS
067200100706     C                   CALL      'FNLV53R'
067300100706     C                   PARM                    DSLV53
067400100706     c
067500100706     c                   if        d53spg='A'
067600100706     c                   eval      conta=conta+1
067700100706     c                   endif
067800100706     c
067900140618     C     KBRV4         reade     FNBRV05L
068000100706     c                   enddo
068100100706     c
068200100706     c                   if        conta>=§vpoggima
068300100706     c                   seton                                        16
068400100706     c                   endif
068500100706     c
068600100706     c
068700100706     c                   endif
068800100706     c
068900960523     C                   ENDSR
069000960523     C*
069100031219     C**************************************************************************
069200031219     C* cerco spunta del collo in qualsiasi categoria
069300031219     C**************************************************************************
069400031219     C     RICSPU        BEGSR
069500031219     C                   CLEAR                   Wimm
069600031219     C                   CLEAR                   WSAVB
069700031219     C                   CLEAR                   WDFV
069800031219     C                   CLEAR                   WTROV
069900040108     C                   CLEAR                   WSPG
070000170330     C                   CLEAR                   Wnpg
070100070118RM*  C     Kartd         CHAIN     FNBRV07l                           33
070200031219RM*  C*
070300031219RM* 1C     *IN33         DOWEQ     *OFF
070400031219     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
070500031219     c                   clear                   dslv53
070600031219     C                   z-add     brvnFV        D53NFV
070700031219     C                   MOVEL     brvNPG        D53NPG
070800031219     C                   MOVEL     brvFGS        D53FGS
070900070118     C****               MOVEL     brvfle        D53flf
071000031219     C                   CALL      'FNLV53R'
071100031219     C                   PARM                    DSLV53
071200031219     C* ERRORE
071300031219    2C     D53ERR        IFeq      ' '
071400070118     c                   movel     brvdfs        brvimm           14 0
071500031219     c                   move      brvhms        brvimm
071600031219    3c                   select
071700031219     c* Se maggiore memorizzo
071800031219     c                   when      d53dfv>wdfv
071900031219     C                   MOVEL     RECBRV        WSAVB
072000070118     C                   MOVEL     brvimm        Wimm             14 0
072100031219     C                   MOVEL     d53DFV        WDFV
072200040108     C                   MOVEL     d53spg        WSPG
072300170330     C                   MOVEL     brvnpg        wNPG
072400031219     C                   MOVEL     'S'           WTROV             1
072500031219     c
072600031219     c* Se uguale tengo la data ora immissione + alta
072700031219     c                   when      d53dfv=wdfv  and brvimm>wimm
072800070118     C                   MOVEL     brvimm        Wimm             14 0
072900031219     C                   MOVEL     RECBRV        WSAVB
073000031219     C                   MOVEL     d53DFV        WDFV
073100040108     C                   MOVEL     d53spg        WSPG
073200170330     C                   MOVEL     brvnpg        wNPG
073300031219     C                   MOVEL     'S'           WTROV             1
073400031219     C                   eval      wimm=brvimm
073500040129     c* Se stessa data e ora immissione, tengo cat 3 o 4
073600101202     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
073700070118     C                   MOVEL     brvimm        Wimm             14 0
073800040129     C                   MOVEL     RECBRV        WSAVB
073900040129     C                   MOVEL     d53DFV        WDFV
074000040129     C                   MOVEL     d53spg        WSPG
074100040129     C                   MOVEL     'S'           WTROV             1
074200170330     C                   MOVEL     brvnpg        wNPG
074300040129     C                   eval      wimm=brvimm
074400031219    3c                   endsl
074500031219    2c                   endif
074600031219     c
074700070118RM*  C     Kartd         reade     FNBRV07l                               33
074800031219    1c                   enddo
074900031219     c
075000031219     C     WTROV         IFEQ      'S'
075100031219     C                   MOVEL     WSAVB         RECBRV
075200031219     C                   ENDIF
075300031219     c                   endsr
075400941017     C**************************************************************************
075500941017     C* IMPOSTO CAMPI FISSI
075600941017     C**************************************************************************
075700911018     C     IMPO          BEGSR
075800921022     C*
075900101202     C                   Z-ADD     1             CODUT             1 0
076000921022     C                   Z-ADD     3             IMANPG
076100950810     C                   CLEAR                   SAVSPE
076200950810     C                   CLEAR                   COMSPE
076300921022     C                   TIME                    UTIME             6 0
076400960927     C                   TIME                    W0140            14 0
076500960927     C                   MOVE      W0140         UDATE8            8 0
076600101202      *
076700101202      * Reperimento dati utente/aziendali
076800101202     c                   exsr      sr_DatiJob
076900161130     c
077000161130     c* per ora solo le filiali abilitate all'attesa in IMA hanno ima e img modificato
077100170124     c**                 clear                   trulvpods
077200170124     c**                 eval      ivpoke1 = 'DECOFIIMG'
077300170124     c**                 call      'TRULVPOR'
077400170124     c**                 parm                    trulvpods
077500161130
077600170124     c**                 if         %lookup('999':skFilOk)>0    or
077700170124     c**                            %lookup(%editc(vidfil:'X'):skFilOk)>0
077800170124     c**                 seton                                        20
077900170124     c**                 endif
078000161130     c
078100161130     c
078200170124     C* 11 e 10 ON - IPG (eliminato)
078300170124     c                   select
078400170124     c***                when      vidtin='Z'
078500170124     C***                SETON                                        1011
078600161017     C* 10 ON - IMG
078700161017     C                   when      vidtin='G'
078800950810     C                   SETON                                        10
078900161014     C                   ENDsl
079000921022     C*
079100921022     C* CHAIN E DATA DEL FOGLIO IMA
079200941017     C     KFVV          CHAIN(N)  FNFVV01L                           32
079300921022     C*
079400941018     C                   Z-ADD     FVVDFV        G02INV                         AAAA/MM/GG
079500941018     C                   Z-ADD     *ZERO         G02DAT
079600941018     C                   MOVEL     '3'           G02ERR
079700941018     C                   CALL      'XSRDA8'
079800941018     C                   PARM                    WLBDAT
079900941018     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
080000950316     C*
080100950317RM*  C                   Z-ADD     FVVDFV        SAVDFV
080200950317RM*  C                   Z-ADD     FVVFGS        SAVFGS
080300931228     C*
080400941028     C* DECODIFICA FILIALE DI APERTURA FOGLIO
080500941018     C     FVVFGS        CHAIN     AZORG                              32
080600931228     C  N32              MOVEL     ORGDES        DESCDU           20
080700000512     C*
080800000512     C                   Z-ADD     15            ANMCAA
080900000512     C                   MOVEL     '7C'          COD
081000000512     C                   MOVEL(P)  ANMCAA        KEY
081100000512     C     KTAB          CHAIN     TABEL00F                           35
081200000512     C  N35              MOVEL     TBLUNI        DS7C
081300000512     C   35              MOVEL     *BLANKS       DS7C
081400911018     C*
081500931228     C* 02 ON -  D E F I N I T I V O
081600911018     C     VIDSN         IFEQ      'S'
081700911018     C                   SETON                                        02
081800911018     C*
081900921022     C* PRENDO FASE ANOMALIA
082000950810     C                   CLEAR                   PARAM5
082100950810     C                   MOVEL     'L'           PA5TLA
082200950810     C                   MOVEL     3             PA5NPG
082300950810     C                   MOVEL     VIDTIN        PA5SPG
082400950810     C                   CALL      'TRUL11R'
082500950810     C                   PARM                    PARAM5
082600920409     C                   END
082700920813     C*
082800920813     C* DECODIFICA FILIALE IN GESTIONE
082900920813     C     VIDFIL        CHAIN     AZORG                              32
083000920813     C  N32              MOVEL     ORGDES        DESFIL           20
083100170323     c
083200170213     c* Verifico se fa l'IDD il giorno dopo
083300170213     c                   movel     orgde8        og148
083400170213     c*
083500170213     c                   if        §OGEID='S'
083600170213     c                   eval      iddgdopo='S'
083700170213     c                   endif
083800170323     c
083900170323     c* Verifico se ha il picking
084000170323     c                   movel     orgdf0        og150
084100170323     c*
084200170323     c                   if        §OGPCK='S'
084300170323     c                   eval      picking ='S'
084400170323     c                   endif
084500170323
084600170323     c* Solo se filiale partita con NUOVO IMA devo aggiornare la data entrata mag giac
084700170323     c* ima e IMG modificati sempre per NEW IMA e non per i 2 gg in ima
084800170323     c                   clear                   trulvpods
084900170323     c                   eval      ivpoke1 = 'DECOFIIPG'
085000170323     c                   call      'TRULVPOR'
085100170323     c                   parm                    trulvpods
085200170323
085300170323     c                   if         %lookup('999':skFilOk)>0    or
085400170323     c                              %lookup(%editc(vidfil:'X'):skFilOk)>0
085500170323     c                   seton                                        22
085600170323     c                   endif
085700170323     c*
085800170323     c* Chi non ha il picking non può fare la nuova procedura
085900170323     c                   if        picking=' '
086000170323     c                   setoff                                       22
086100170323     c                   endif
086200941019     C*
086300101215     C* CARICO L6
086400101215     C                   CLEAR                   DSUL06
086500101215     C                   MOVE      '£6'          D06COD
086600101215     C                   MOVEL     VIDFIL        D06KEY
086700101215     C                   MOVEL     'L'           D06TLA
086800101215     C                   MOVEL     DSUL06        KPJBU
086900101215     C                   CALL      'TRUL06R'
087000101215     C                   PARM                    KPJBA
087100101215     C                   MOVEL     KPJBU         DSUL06
087200101215     C                   MOVEA     LIN           Lspu
087300060508     c*
087400101215     C*  Se non parzializzata la linea di arrivo, elaboro L6
087500161014     c*  altrimenti elaboro solo lei
087600060508     c                   if        vidlna=0
087700101215     C                   MOVEA     LIN           L6
087800060508     c                   else
087900060508     c                   clear                   L6
088000060508     c                   movel     vidlna        l6(1)
088100060508     c
088200060508     c* Decodifica
088300060508     C     VIDlna        CHAIN     AZORG                              32
088400060508     C  N32              MOVEL     ORGDES        DESlna           20
088500060508     c                   movel     vidlna        stalna            3
088600060508     c                   endif
088700070418     c
088800070418     c* Elabora anche spedizioni in distinta
088900070418     c                   if        vidmdc='S'
089000070418     c                   eval      stamdc='SI'
089100070418     c                   else
089200070418     c                   eval      stamdc='NO'
089300070418     c                   endif
089400101215     c* solo colli spuntati alla linea di arrivo finale
089500101215     c                   if        parimaa='S'
089600101215     c                   eval      staimaa=csololna
089700101215     c                   endif
089800160503     c* Verifico le richieste delle zone di consegna
089900160503     c                   clear                   wParc             1
090000160503     c                   clear                   wParc1            1
090100160503     c                   clear                   wParc2            1
090200160503     c                   clear                   wMess             1
090300160503     c                   clear                   wSelzona          1
090400160503     c                   if        partpm1='P ' or partpm2='P '
090500160503     c                   eval      wparc='S'
090600160503     c                   eval      wselzona='S'
090700160503     c                   endif
090800160503     c                   if        partpm1='M ' or partpm2='M '
090900160503     c                   eval      wMess='S'
091000160503     c                   eval      wselzona='S'
091100160503     c                   endif
091200160503     c                   if        partpm1='P1' or partpm2='P1'
091300160503     c                   eval      wParc1='S'
091400160503     c                   eval      wselzona='S'
091500160503     c                   endif
091600160503     c                   if        partpm1='P2' or partpm2='P2'
091700160503     c                   eval      wParc2='S'
091800160503     c                   eval      wselzona='S'
091900160503     c                   endif
092000060508     c
092100050510     c* se richiesta soltanto zone parcel o messagerie, carico in
092200050510     c*  schiera
092300160503     c                   if        wparc='S' and wmess='S'
092400160503     c                   clear                   wparc
092500160503     c                   clear                   wmess
092600160503     c                   clear                   wselzona
092700160503     c                   endif
092800160503     c                   if        wparc1='S' and wparc2='S'
092900160503     c                   clear                   wparc1
093000160503     c                   clear                   wparc2
093100160503     c                   eval      wparc='S'
093200160503     c                   endif
093300160503     c
093400160503    1c                   if        wselzona='S'
093500050511     c                   clear                   xx                4 0
093600160502     c                   clear                   yy                4 0
093700170331     c                   clear                   zz                4 0
093800170331     c                   eval      zz=1
093900170331     c
094000170331     c* Carico le zone per tutte le linee della £6
094100170331   1ac                   dow       l6(zz)>0
094200170331     c                   movel     l6(zz)        alffil            3
094300170331     c                   movel     l6(zz)        wpo
094400050510     c                   movel(P)  '03'          cod
094500170331     c                   movel(P)  l6(zz)        key
094600050510     c     ktab          setll     tabel
094700050510     c     ktab2         reade     tabel
094800160502    2c                   dow       not %eof(tabel00f) and  wpo=alffil
094900160502    3c                   if        tblflg=' '
095000050510     c                   movel     tblkey        wpo               3
095100160502    4c                   if        wpo=alffil
095200050511     c                   movel     tbluni        ds03
095300160502
095400160502     c* parcel/messaggerie
095500160503    5c                   if        wparc='S' or wmess='S'
095600160502
095700160503    6c                   if        (wmess='S' and §03tpm='M') or
095800160503     c                             (wparc='S' and §03tpm<>'M')
095900050511     c                   add       1             xx
096000050511     c                   movel     tblkey        w005a             5
096100050511     c                   move      w005a         zone(xx)
096200160502    6c                   endif
096300160502    5c                   endif
096400160502     c* parcel 1° e 2° turno
096500160503    5c                   if        (wparc1='S' or wparc2='S')
096600160503     c                             and §03tpm<>'M'
096700160502
096800160503    6c                   if        (wparc2='S' and §03turno='2') or
096900160503     c                             (wparc1='S' and §03turno<>'2')
097000160503     c                   add       1             yy
097100160502     c                   movel     tblkey        w005a             5
097200160503     c                   move      w005a         zone12(yy)
097300160502    6c                   endif
097400160502    5c                   endif
097500160502     c
097600160502    4c                   endif
097700160502    3c                   endif
097800160502     c
097900050511     c     ktab2         reade     tabel
098000160502    2c                   enddo
098100170331     c
098200170331     c                   add       1             zz
098300170331   1ac                   enddo
098400160502    1c                   endif
098500050511     c
098600160503     c                   clear                   statipo
098700160503     c                   clear                   statipo2
098800160503     c                   if        partpm1='P ' or partpm2='P '
098900050511     c                   eval      statipo=cparcel
099000050511     c                   endif
099100050511     c
099200160503     c                   if        partpm1='M ' or partpm2='M '
099300160503     c                   if        statipo=*blanks
099400050511     c                   eval      statipo=cmessag
099500160503     c                   else
099600160503     c                   eval      statipo2=cmessag
099700160503     c                   endif
099800050511     c                   endif
099900160503     c                   if        partpm1='P1' or partpm2='P1'
100000160503     c                   if        statipo=*blanks
100100160503     c                   eval      statipo=cparc1
100200160503     c                   else
100300160503     c                   eval      statipo2=cparc1
100400160503     c                   endif
100500160503     c                   endif
100600160503     c                   if        partpm1='P2' or partpm2='P2'
100700160503     c                   if        statipo=*blanks
100800160503     c                   eval      statipo=cparc2
100900160503     c                   else
101000160503     c                   eval      statipo2=cparc2
101100160503     c                   endif
101200160503     c                   endif
101300941017     C*
101400100706      * Aggancio tabella VPO recpord VPO
101500100706     c                   Clear                   Dvpo
101600100706     c                   Clear                   dsbs02
101700100706     c                   Eval      T02Mod = 'C'
101800100706     c                   Eval      T02tla = 'L'
101900100706     c                   Eval      T02Sif = knsif
102000100706     c                   Eval      T02Cod = 'VPO'
102100100706     c                   Movel(p)  'VPO'         T02Ke1
102200100706     c                   Call      'TIBS02R'
102300100706     c                   Parm                    Kpjba
102400100706     c                   Parm                    dsbs02
102500100706     c                   If        T02Err = *Blanks
102600100706     c                   Movel     T02Uni        Dvpo
102700100706     c                   EndIf
102800100706     c
102900100706     c     *eur          move      udate8        dataeur
103000100706     c                   movel     dataeur       dataiso
103100161017     c
103200161017     c                   movel     dataiso       dateu             8 0
103300161017
103400100706     c                   if        §vpoggima>0
103500100706     c                   subdur    §vpoggima:*d  dataiso
103600100706     c                   endif
103700100706     c
103800100706     c                   movel     dataiso       datapresIMA       8 0
103900100706
104000911018     C                   ENDSR
104100941017     C**************************************************************************
104200941017     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
104300941017     C**************************************************************************
104400911018     C     ERRORE        BEGSR
104500941017     C*
104600911015     C                   SETON                                        01
104700040603     c
104800040603     c     kar5          chain     fiar501l
104900040603     c                   if        %found(fiar501l)
105000040603     c                   movel     ar5uni        dar5gen
105100040603     c                   else
105200040603     c                   clear                   dar5gen
105300040603     c                   endif
105400911018     C* STAMPA ANOMALIA
105500040428     C                   EXSR      STAMPA
105600081014     c
105700100706     c* non creo anomalia se collo doppio o solo segnalazione
105800161017    0c                   if        not *in12 and not *in16 and not *in11
105900161018     c                             and not *in18
106000170329     c* in IMA la creo solo se non è da portare a mag giac  se partito con NEW IMA
106100170329     c*  in IMG  sempre perchè rischio di perdere la segnalaziione in IDD se mancante
106200170329     c                              and (wgiac<>'D' or *in10)
106300911015     C*
106400950707     C* SOLO SE IMA DEFINITIVO  CREO ANOMALIA
106500950608    1C     VIDSN         IFEQ      'S'
106600950810     C* NON DEVO CREARE:
106700950810     C* 1) SE C'E' LA FORZATURA E MANCA LA DATA DI ARRIVO
106800950810     C*    ANCHE SE STAMPO LO STESSO
106900950810     C* 2) SE LA SPUNTA E' STATA TROVATA IN ALTRA CATEGORIA DEL GIORNO
107000960419     C*    3 O 4 NON DISABILITO NEMMENO LA BOLLA
107100960419     C* 3) SE LA SPUNTA E' STATA TROVATA IN ARRIVO CREO L'ANOMLIA
107200950608    2C     ARTDAM        IFGT      0
107300960419   2AC     *IN13         IFEQ      *OFF
107400031219     C     *IN15         OREQ      *ON
107500081014i    C*
107600081104     C     KANM          SETLL     FNANM02L                               35
107700081014    3c                   if        not *in35
107800081014     c* Se anomalia chiusa non in pratica idd e data chiusura <data ima
107900081014     c*  la ricreo
108000081104    4c***                if        anmdch>0 and anmdch<savdfv and
108100081104     c***                          anmcch<>'PR'
108200081104     c***                delete    fnanm000
108300081104     c***                seton                                        35
108400081104    4c***                endif
108500081104    3c***                endif
108600081014     c
108700081104    3C***                if        *in35
108800081104     C*
108900081104     C                   MOVEL     *BLANKS       ANMATB
109000081104     C                   MOVEL     PA5FAS        ANMFSA
109100160503     c*  data salvata del foglio IMA perchè richaino in update
109200100506     C                   Z-ADD     SAVdfv        ANMDAO
109300081104     C                   Z-ADD     15            ANMcaa
109400081104     C                   MOVEL     *BLANKS       ANMCCH
109500081104     C                   MOVEL     VIDFGS        ANMCDU
109600081104     C                   Z-ADD     COMNFV        ANMNFV
109700081104     C                   MOVEL     VIDFGS        ANMFLE
109800081104     C                   MOVE      *BLANKS       ANMFLO
109900081104     C                   MOVEL     *BLANKS       ANMFT1
110000081104     C                   MOVEL     *ZEROS        ANMFL1
110100081104     C                   MOVEL     *BLANKS       ANMFT2
110200081104     C                   MOVEL     *ZEROS        ANMFL2
110300081104     C*
110400081104     C                   MOVEL     §7CTAN        ANMTAN
110500081104     C                   MOVEL     §7CSAN        ANMSAN
110600081104     C                   MOVEL     §7CAIE        ANMAIE
110700081104     C                   Z-ADD     §7CLID        ANMLID
110800081104     C                   MOVE      *ZEROS        ANMDUE
110900081104     C                   MOVE      *ZEROS        ANMNPG
111000081104     C                   MOVE      *ZEROS        ANMFGS
111100081104     C                   Z-ADD     0             ANMDCH
111200081104     C                   MOVEL     ' '           ANMFSC
111300081104     C* CHIUSURA AUTOMATICA
111400081104     C     §7CCHA        IFEQ      'S'
111500100506     C                   Z-ADD     SAVDFV        ANMDCH
111600081104     C                   MOVEL     PA5FAS        ANMFSC
111700081104     C                   END
111800081104     C                   MOVE      *BLANKS       ANMFT3
111900081104     C                   MOVE      *ZEROS        ANMFL3
112000081104     C                   MOVE      *BLANKS       ANMFT4
112100081104     C                   MOVE      999           ANMFL4
112200081104     C                   MOVE      *ZEROS        ANMDTR
112300081104     C*
112400911018     C                   MOVEL     ARTFLS        ANMFLS
112500911018     C                   MOVEL     ARTLNA        ANMLNA
112600911018     C                   MOVEL     ARTNRS        ANMNRS
112700941018     C                   MOVEL     ARTNSC        ANMSCN
112800941018     C                   Z-ADD     ARTAAS        ANMAAS
112900911018     C                   MOVEL     ARTNSP        ANMNSP
113000911018     C                   MOVEL     ARTLNP        ANMLNP
113100970206     C                   MOVEL     ARTNAP        ANMNPS
113200040603     c
113300040603     c* imposto data e ora di creazione
113400050707     C                   TIME                    W0140            14 0
113500050707     C                   MOVE      W0140         G02DAT
113600050707     C                   Z-ADD     *ZERO         G02INV
113700050707     C                   MOVEL     *BLANKS       G02ERR
113800050707     C                   CALL      'XSRDA8'
113900050707     C                   PARM                    WLBDAT
114000050707     C                   Z-ADD     G02INV        WDATA             6 0
114100050707     c**                 time                    wora              6 0
114200050707     c**                 z-add     *date         wdata             6 0
114300050707     C
114400050707     c                   movel     w0140         wora4             4 0
114500040603     c                   movel     wdata         anmflo
114600040603     c                   move      wora4         anmflo
114700040603     c
114800040603     c* imposto se flag di valore
114900040603     c                   movel     §ar5bva       anmft4
115000040603     c
115100040603     c
115200920409     C* ESTERNA
115300950608    4C     §7CAIE        IFEQ      'E'
115400950608     C                   MOVEL     ARTLNP        ANMFL1
115500950810    4C                   ENDIF
115600920409     C*
115700990511     C                   WRITE     FNANM000
115800081014    3C                   endif
115900081014     c
116000000512   2AC                   ENDIF
116100960419    2C                   ENDIF
116200920318     C*
116300000512     C* DISABILITO LA BOLLA SOLO SE HO CREATO ANOMALIA
116400000512     C* NO POSTE SEMPRE, LNP POSTE SOLO SE PREVOSTO
116500950810     C* SOLO SE NON HO TROVATO UN'ALTRA SPUNTA IN DATA DEL FOGLIO IMA
116600960419    2C     *IN10         IFEQ      *OFF
116700960419     C*
116800960419    3C     *IN13         IFEQ      *OFF
116900031219     C     *IN15         OREQ      *ON
117000000512     C*
117100040428    5c     wchain        ifeq      *blanks
117200040428     c     fnarbnrri     chain(e)  fnarb00f
117300040428    6c                   if        %error
117400040428     C* Valorizzo la DS di procedura
117500040428     C                   CLEAR                   TRUL82DS
117600040428     c                   eval      ul82§rrn = fnarbnrri
117700040428     C                   EVAL      UL82§FIL = 'FNARB00F'
117800040428     C                   EVAL      UL82§WIN = 'N'
117900040428     C                   EVAL      UL82§F7  = 'S'
118000040428     C                   EVAL      UL82§num = 5
118100040428     C                   EVAL      UL82§att = 60
118200040428     C                   EVAL      UL82§mss = msgjob
118300040428     C* Effettuo la chiamata al *PGM d utilità
118400040428     C                   CALL(e)   'TRUL82R'
118500040428     C                   PARM                    TRUL82DS
118600040428     C     fnarbnrri     chain     fnarb00f
118700040428    6c                   endif
118800040428     C                   Z-ADD     0             ARBAMA
118900040428     c                   update    fnarbf00
119000040428     c                   move      '1'           wchain
119100040428    5c                   endif
119200930629     C* AGGIORNO IL COLLO CON UNA 'D' DI DISABILITAZIONE
119300941221     C                   MOVEL     'S'           ARTDAB                         'D'=DISABILITATO
119400000512    3C                   ENDIF
119500960419    2C                   ENDIF
119600950810     C*
119700040428     C  N10              UPDATE    FNART000
119800950810    1C                   ENDIF
119900081014    0C                   ENDIF
120000911015     C*
120100911015     C                   ENDSR
120200950810     C**************************************************************************
120300950810     C* ELABORAZIONE DELL'IMA
120400950810     C**************************************************************************
120500950810     C     ELAIMA        BEGSR
120600950810     C*
120700950810     C* N O N   C O N T R O L L O :
120800950810     C*          1) SE NO ABILITAZIONE FORZATA E SENZA DATA ARRIVO
120900990212    1C     SAVAMA        IFNE      3
121000950810     C     ARTDAM        ANDEQ     0
121100950810     C                   SETON                                        34
121200990212    1C                   ENDIF
121300950810     C*
121400950810     C*          2) SE DATA ARRIVO MAGGIORE DATA FOGLIO IMA
121500990212    1C  N34ARTDAM        IFNE      0
121600950810     C     ARTDAM        ANDGT     SAVDFV
121700950810     C                   SETON                                        34
121800990212    1C                   ENDIF
121900950810     C*
122000990212     C*          3) SE COLLO SI TROVA IN DCD E RESO O DISTRUTTO PER
122100990212     C*             AVARIA O COLLO MANCANTE
122200990212     C     KARTD         CHAIN     FNDCD02L                           31
122300990212    1C     *IN31         IFEQ      *OFF
122400990212     C*
122500990303     C     DCDATB        ANDEQ     ' '
122600990212     C* CHAIN TESTATA PER CONTROLLARE LA DATA DI CHIUSURA
122700990212     C     KDCD          CHAIN     FNDCT01L                           32
122800050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
122900050221     c                   If        Not *In32 and dctdt2 > 0
123000050221     c                   Eval      *In32 = *On
123100050221     c                   EndIf
123200990303     C*
123300990303    2C     *IN32         IFEQ      *OFF
123400990303     C* SE E' CHIUSA CONTROLLO SE IL RECORD DEVE ESSERE COMPRESO
123500990303     C*  IN IMA, PER ALCUNI TIPI ANOMALIA
123600990303     C                   MOVEL     'S'           §CHIMA
123700990303    3C     DCTCCH        IFNE      *BLANKS
123800990303     C                   CLEAR                   DSBS02
123900990303     C                   MOVEL     'C'           T02MOD
124000990303     C                   MOVEL     KNSIF         T02SIF
124100990303     C                   MOVEL     'CCH'         T02COD
124200990303     C                   MOVEL     DCTCCH        T02KE1
124300990303     C                   CALL      'TIBS02R'
124400990303     C                   PARM                    KPJBA
124500990303     C                   PARM                    DSBS02
124600990303     C*
124700990303    4C     T02ERR        IFEQ      ' '
124800990303     C                   MOVEL     T02UNI        DCCH
124900990303    4C                   ENDIF
125000990303    3C                   ENDIF
125100990303     C**
125200990303     C** SE NON DEVE ESSERE COMPRESO:
125300990303    3C     §CHIMA        IFNE      'S'
125400990303     C     DCTDCH        OREQ      0
125500990212     C** SE ENTRAMBI APERTI O CHIUSI CON STESSA DATA CONTROLLO IL TIPO
125600990212     C**  ANOMALIA PER DECIDERE SE TENERE O ESCLUDERE IL COLLO
125700990212     C**  NON CONSIDERO LA CAUSALE PAREGGIO CHE SIGNIFICA CHE IL COLLO
125800990212     C*  C'E' SEMPRE
125900990212    4C     DCTDCH        IFEQ      DCDDCH
126000990212     C     DCDDCH        OREQ      0
126100990212     C**
126200990212     C                   CLEAR                   DSBS02
126300990212     C                   MOVEL     'C'           T02MOD
126400990212     C                   MOVEL     KNSIF         T02SIF
126500990212     C                   MOVEL     'TAD'         T02COD
126600990212     C                   MOVEL     DCTTAD        T02KE1
126700990212     C                   CALL      'TIBS02R'
126800990212     C                   PARM                    KPJBA
126900990212     C                   PARM                    DSBS02
127000990212     C**
127100990212    5C     T02ERR        IFEQ      ' '
127200990212     C                   MOVEL     T02UNI        DTAD
127300990212     C                   ELSE
127400990212     C                   MOVEL     'A'           §TARGR
127500990212    5C                   ENDIF
127600990212    C**
127700990212     C* SE MANCANZE
127800990212    5C     §TARGR        IFEQ      'M'
127900990212     C* O AVARIA RESA
128000990212     C     §TARGR        OREQ      'V'
128100990212     C     DCTDSD        ANDEQ     'R'
128200990212     C* O AVARIA DISTRUTTA
128300990212     C     §TARGR        OREQ      'V'
128400990212     C     DCTDSD        ANDEQ     'D'
128500990212     C*                    --> ESCLUDO
128600990212     C                   SETON                                        34
128700990212    5C                   ENDIF
128800990212    4C                   ENDIF
128900990212    3C                   ENDIF
129000990212    2C                   ENDIF
129100990212    1C                   ENDIF
129200101215     c
129300101215     c* Se opzione solo spuntato alla linea di arrivo finale, controllo
129400101215     c                   if        parimaa='S'
129500101215     c                   setoff                                       37
129600101215RM*  C     Kartd         setll     FNBRV07l
129700101215RM*  C     Kartd         reade     FNBRV07l
129800101215     c                   dow       not %eof(fnbrv07l)
129900101215     c     brvfgs        lookup    Lspu                                   37
130000101215     c                   if        *in37
130100101220     c* Se la filiale che spunta è giusta verifico che sia categoria di "arrivo"
130200101220     c                   clear                   dslv53
130300101220     C                   z-add     brvnFV        D53NFV
130400101220     C                   MOVEL     brvNPG        D53NPG
130500101220     C                   MOVEL     brvFGS        D53FGS
130600101220     C                   CALL      'FNLV53R'
130700101220     C                   PARM                    DSLV53
130800101220     C*
130900101220    2C     D53ERR        IFeq      ' '
131000101220     c*
131100101220     c                   if        d53def='S'
131200101220     c                   eval      wnpgarr=%editc(d53npg:'X')+'D'
131300101220     c                   else
131400101220     c                   eval      wnpgarr=%editc(d53npg:'X')+d53spg
131500101220     c                   endif
131600101220     c
131700101220     c                   eval      TROV=%lookup   (wnpgarr: NPGARR)
131800101220     c                   if        TROV>0
131900101215     c                   leave
132000101220     c                   endif
132100101215     c                   endif
132200101220     c                   endif
132300101220     c
132400101220     c                   setoff                                       37
132500101220RM*  C     Kartd         reade     FNBRV07l
132600101215     c
132700101215     c                   enddo
132800101215     c
132900101215     c* se non trovata nessuna spunta, NON elaboro
133000101215     c                   if        not *in37
133100101215     c                   seton                                        34
133200101215     c                   endif
133300101215     c
133400101215     c                   endif
133500990212    C**
133600170125     c** se collo mancante di bolla da portare a mag gaicenza e collo spuntato in
133700170125     c*  IMG nel giorno --> non stampo
133800170125     c
133900100706     c                   if        not *in34
134000170125     c                   clear                   winGiac
134100100706     C                   EXSR      CTRART
134200950810     C*
134300950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
134400170125     c*         se si tratta di merce da portare a IMG e collo spuntato solo in IMG
134500170125     c*         non stampo
134600170125     c                   if        wgiac='D' and *in14 and
134700170125     c                             wspg='G' and wdfv=savdfv
134800170125     c                   eval      winGiac='S'
134900170125     c                   endif
135000170213     c
135100170125
135200100706     C* 16 ON -     TROVATO SEGNACOLLO IN IMA più di xxx volte
135300170125     c                   if        *in14 and wingiac=' '  or *in16
135400170125     C                   EXSR      ERRORE
135500170125     c                   endif
135600170125
135700170125     c                   if        not *in14
135800170125     c                   eval      Wesspu='S'
135900170125     c                   endif
136000100706     c                   endif
136100950810     C*
136200950810     C                   ENDSR
136300950810     C**************************************************************************
136400950810     C* ELABORAZIONE DELL'IMG
136500950810     C**************************************************************************
136600950810     C     ELAIMG        BEGSR
136700950810     C                   EXSR      CTRART
136800950810     C*
136900170124     C* 14 ON - NON TROVATO SEGNACOLLO IN IMG
137000161025     c                   if        *in14
137100161026     c* In caso di bolla con giacenza e merce da portare a mag giac.
137200161026     c*  non stampo nulla per IMG
137300170124     c***                if        *in11 or wgiac<>'D'
137400170124     c                   if        wgiac<>'D'
137500161025     C                   EXSR      ERRORE
137600161026     c                   endif
137700161026     c
137800161025     c                   else
137900161025     c                   eval      Wesspu='S'
138000161025     c                   endif
138100161025
138200950810     C                   ENDSR
138300941017     C**************************************************************************
138400941017     C* STAMPO DISCORDANZA
138500941017     C**************************************************************************
138600900621     C     STAMPA        BEGSR
138700050405     c                   setoff                                       242526
138800941017     C*
138900911015     C* STAMPA TESTATA
139000911015     C   OF              DO
139100911018     C                   SETON                                        05
139200911018     C                   EXCEPT    TES
139300920813     C                   EXCEPT    TES2
139400911018     C                   SETOFF                                       OF
139500911018     C                   END
139600090522     c* Se non trovata la bolla (per colli doppi)
139700090522    1c                   if        NoBolla='S'
139800090522     c                   except    DetNObo
139900161018     C   12              EXCEPT    DETdoppio
140000161018     C   18              EXCEPT    DETeccede
140100090522   x1c                   else
140200911018     C*
140300950810     C     COMSPE        IFNE      ARBSPE
140400911018     C                   SETON                                        05
140500950810     C                   MOVEL     ARBSPE        COMSPE
140600950810     C                   ENDIF
140700911018     C* DATA SPEDIZIONE
140800911018     C   05              DO
140900941018     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
141000941018     C                   MOVE      ARBDSP        G02INV                         AAAA/MM/GG
141100911018     C                   MOVE      *ZEROS        G02DAT
141200911018     C                   MOVEL     3             G02ERR
141300941017     C                   CALL      'XSRDA8'
141400911018     C                   PARM                    WLBDAT
141500941018     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
141600941018     C*
141700941018     C* CODICE PADRONCINO CONSEGNA DECODIFICA
141800021203     C*    ARBPDC        CHAIN     FNAPD01L                           06
141900021205     C                   move      arbpdc        kpdpdr
142000021203     C                   move      'A'           kpdtip
142100021203     C     kfapd         chain     fiapd01L                           06
142200941018     C  N06              MOVEL     APDRSC        DESCPC            9
142300100707     c
142400100707     c* se non c'e' paadroncino consegna scrivo se Fermo deposito
142500100707     c                   if        *in06
142600100707     C                   MOVEL     *BLANKS       DESCPC
142700100707     c                   if        arbffd='S'
142800100707     c                   eval      descpc='FDeposito'
142900100707     c                   endif
143000100707     c                   endif
143100920409     C*
143200911018     C* SEGNACOLLI NON CONSECUTIVI
143300911018     C     ARBFNS        COMP      'S'                                    04
143400911018     C* SERIE>< 0
143500911018     C     ARBNRS        COMP      0                                  0303
143600911018     C                   END
143700920409     C*
143800920708     C* 07 ON - MANCA DATA ARRIVO: SPEDIZIONE FORZATA A 3
143900920708     C     ARTDAM        IFEQ      0
144000920708     C                   SETON                                        07
144100920708     C                   ELSE
144200920708     C                   SETOFF                                       07
144300920409     C* DATA ARRIVO MERCE  SEGNACOLLO
144400941018     C                   MOVE      ARTDAM        G02INV                         AAAA/MM/GG
144500920409     C                   MOVE      *ZEROS        G02DAT
144600920409     C                   MOVEL     3             G02ERR
144700941017     C                   CALL      'XSRDA8'
144800920409     C                   PARM                    WLBDAT
144900941018     C                   Z-ADD     G02DAT        STADAM                         GG/MM/AAAA
145000920708     C                   END
145100040603     C* MITTENTE E DESTINATARIO DI 17 E 20
145200040603     C                   MOVEL     ARBRSM        STARSM           17
145300920707     C                   MOVEL     ARBRSD        STARSD           20
145400040603     c* collo di valore
145500040603     c                   movel     §ar5bva       stabva            1
145600161025     c* se si tratta di bolla da portare a mag giac, riempi il campo STADSF con relativa scritta
145700161025     c                   if        not *in11 and wgiac='D'
145800161025     c                   clear                   stadsf
145900161025     c                   eval      stadsf='Portare a MagGIAC'
146000161025     c                   endif
146100161025     c
146200161026     c                   clear                   stahms            4 0
146300170222     c   13              if        brvhms>0
146400170222     c                   eval      stahms=brvhms/100
146500170222     c                   endif
146600910522     C*
146700900622     C* STAMPO DETTAGLIO
146800100706     C                   EXCEPT    DET
146900100707     c                   select
147000100707     c                   when      *in12
147100100706     C                   EXCEPT    DETdoppio
147200100707     c                   when      *in16
147300100707     C                   EXCEPT    DETspuIMA
147400161018     c                   when      *in18
147500161018     C                   EXCEPT    DETeccede
147600100707     c                   other
147700100706     C                   EXCEPT    DETerr
147800100707     c                   endsl
147900070418     c
148000070418     c* se in distinta , stampo
148100090522    2c                   if        not *in12
148200081014
148300100706    3c                   if        *in05 and arbndc>0 and not *in16
148400070418     C                   MOVE      ARbddc        G02INV                         AAAA/MM/GG
148500070418     C                   MOVE      *ZEROS        G02DAT
148600070418     C                   MOVEL     3             G02ERR
148700070418     C                   CALL      'XSRDA8'
148800070418     C                   PARM                    WLBDAT
148900070418     C                   Z-ADD     G02DAT        STAddc            8 0          GG/MM/AAAA
149000070418     C                   EXCEPT    DETndc
149100090522    3c                   endif
149200050405     c
149300161017     c* Se c'e' ubicazione collo, per 05 on lo stampo
149400050405     c                   if        *in05 and §ar5zng<>*blanks
149500050405     c                   seton                                        24
149600050405     c                   endif
149700050405     c
149800100707     C                   SETOFF                                       05
149900050405     c
150000990212     C* STAMPO TUTTE LE C.A. PRESENTI SULLA SPEDIZIONE
150100040806      * Sostituisco la lettura del file FNDTC02 con
150200040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
150300040806      * legate alla spedizione che passo
150400040806      *
150500040806     c                   clear                   fidn12ds
150600040806     c                   eval      i12aas = arbaas
150700040806     c                   eval      i12lnp = arblnp
150800040806     c                   eval      i12nrs = arbnrs
150900040806     c                   eval      i12nsp = arbnsp
151000040806     c                   eval      i12fel = 'E'
151100040806     c                   eval      i12fan = 'E'
151200040806      *
151300040806     c                   call      'FIDN12R'
151400040806     c                   parm                    fidn12ds
151500040806      *
151600040806      * se non ci sono errori
151700090522    3c                   if        o12err = ' '
151800040806      * se numero di CA trovate maggiore di zero
151900090522    4c                   if        o12nca > 0
152000100706     c
152100050405     c                   seton                                        25
152200040806     c
152300040806     c                   z-add     *zeros        ii                2 0
152400100706    6c     1             do        o12nca        ii
152500040806     c                   movea     skkey(ii)     dskey
152600040806     C                   Z-ADD     dsfil         dctfil
152700040806     C                   Z-ADD     dsnca         dctnca
152800040806     C                   Z-ADD     skdca(ii)     dctdca
152900040806     C                   Z-ADD     skdch(ii)     dctdch
153000040806     C                   movea     skcch(ii)     dctcch
153100040806     c                   exsr      preca
153200050405     c                   except    detca
153300050405     c
153400050405     c                   setoff                                       24
153500100706    6C                   ENDDO
153600040806      *
153700100706    4c                   endif
153800090522    3c                   endif
153900050405     c
154000050405     c* Stampo ubicazione collo se non ancora stampata
154100090522    3c                   if        *in24
154200050405     c                   except    detca
154300090522     c                   setoff                                       24
154400090522    3c                   endif
154500090522     c                   setoff                                       25
154600081014     c
154700090522    2c                   endif
154800090522    1c                   endif
154900900622     C*
155000900622     C                   ENDSR
155100990212     C**************************************************************************
155200990212     C* PREPARO I DATI DELLA C.A.
155300990212     C**************************************************************************
155400990212     C     PRECA         BEGSR
155500990212     C                   SETOFF                                       26
155600990303     C                   CLEAR                   STACCH
155700990212     C* DATA APERTURA C.A.
155800990212     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
155900990212     C                   Z-ADD     *ZERO         G02DAT
156000990212     C                   MOVEL     '3'           G02ERR
156100990212     C                   CALL      'XSRDA8'
156200990212     C                   PARM                    WLBDAT
156300990212     C                   MOVE      G02DAT        W0020             2 0
156400990212     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
156500990212     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
156600990303     C** DATA CHIUSURA C.A.
156700990212     C                   CLEAR                   STACHC
156800990303    1C     DCTDCH        IFGT      0
156900990212     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
157000990212     C                   Z-ADD     *ZERO         G02DAT
157100990212     C                   MOVEL     '3'           G02ERR
157200990212     C                   CALL      'XSRDA8'
157300990212     C                   PARM                    WLBDAT
157400990212     C                   MOVE      G02DAT        W0020             2 0
157500990212     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
157600990212     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
157700990212     C                   SETON                                        26
157800990303     C* CAUSALE CHIUSURA
157900990303    2C     DCTCCH        IFNE      *BLANKS
158000990303     C                   CLEAR                   DCCH
158100990303     C                   CLEAR                   DSBS02
158200990303     C                   MOVEL     'C'           T02MOD
158300990303     C                   MOVEL     KNSIF         T02SIF
158400990303     C                   MOVEL     'CCH'         T02COD
158500990303     C                   MOVEL     DCTCCH        T02KE1
158600990303     C                   CALL      'TIBS02R'
158700990303     C                   PARM                    KPJBA
158800990303     C                   PARM                    DSBS02
158900990303     C*
159000990303    3C     T02ERR        IFEQ      ' '
159100990303     C                   MOVEL     T02UNI        DCCH
159200990303    3C                   ENDIF
159300990303     C                   MOVEL     §CHDES        STACCH           12
159400990303    2C                   ENDIF
159500990303    1C                   ENDIF
159600990212     C                   ENDSR
159700941017     C**************************************************************************
159800161018     C* RIEPILOGO SPUNTE PER PISTOLA e controllo se colli doppi o eccedenti
159900941017     C**************************************************************************
160000920813     C     RIEPIL        BEGSR
160100081014     C                   SETON                                        12
160200941017     C*
160300920813     C                   Z-ADD     1             C
160400920813     C* CONTO SPUNTE PER PISTOLA
160500070118     C     KBRV          SETLL     FNBRV01L
160600070118     C     KBRV          READE     FNBRV01L                               30
160700920813     C*
160800161018    1C     *IN30         DOWEQ     *OFF
160900161018    2c                   if        brvtsu<>*blanks
161000081014     c                   clear                   fnlrv7ds
161100081014     c                   eval      ilrv7fls=brvlnp
161200081014     c                   eval      ilrv7lna=brvlna
161300081014     c                   eval      ilrv7nrs=brvnrs
161400081014     c                   eval      ilrv7nsc=brvnsc
161500081014     c                   eval      kpjbu= fnlrv7ds
161600081014     c                   call      'FNLRV7R'
161700081014     c                   parm                    kpjba
161800081014     c
161900081014     c                   eval      fnlrv7ds=kpjbu
162000161018    3c                   if        olrv7nric>=3 and olrv7part='S'
162100081014     c* Se ho già stampato qualche cosa, lascio la prima volta una
162200081014     c*  riga vuota
162300161018    4c                   if        *in01 and not *inOF and uno=' '
162400081014     c                   except    DETvuota
162500161018    4c                   endif
162600161018     c                   exsr      Cerca_bolla
162700081014     c
162800081014     c                   exsr      ERRORE
162900081014     c                   eval      UNO='S'
163000161018    3c                   endif
163100081014     c
163200161018    2c                   endif
163300161018     c
163400170124     c*      per  IMG  stampo i colli eccedenti ( IPG non esiste più)
163500170124     C*  ( SE partiti con procedura)
163600161201     c                   if        *in10 and *in22
163700161201     c                   exsr      Eccedenti
163800161201     c                   endif
163900081014     c
164000941019     C                   Z-ADD     1             B                 2 0
164100920813     C                   MOVEL     BRVNPS        COMNPS            2
164200920813     C     COMNPS        LOOKUP    NPS(B)                                 31
164300920813     C  N31              MOVEL     BRVNPS        NPS(C)
164400920813     C  N31              Z-ADD     C             B
164500941019     C  N31              ADD       1             C                 2 0
164600920813     C*
164700920813     C                   ADD       1             CPS(B)
164800920813     C*
164900070118     C     KBRV          READE     FNBRV01L                               30
165000161018    1C                   ENDDO
165100081014     C
165200081014     C                   SETOFF                                       12
165300161018     c
165400081014     C*
165500081014     C* SE NESSUNA ANOMALIA STAMPO TUTTO O.K.
165600081014     C  N01              EXCEPT    TES
165700081014     C  N01              EXCEPT    OK
165800081014     C  N01              SETOFF                                       OF
165900920813     C**
166000920813     C* SOMMO TUTTI I COLLI
166100920813     C                   XFOOT     CPS           TOTSPU            9 0
166200920813     C**
166300920813     C* STAMPA SPUNTE
166400920813     C* TESTATA
166500920813     C   OF              EXCEPT    TES
166600920813     C   OF              SETOFF                                       OF
166700920813     C*
166800920813     C                   EXCEPT    TOT1
166900920813     C*
167000920813     C                   Z-ADD     1             C
167100920813     C     CPS(C)        DOWGT     0
167200920813     C   OF              DO
167300920813     C                   EXCEPT    TES
167400920813     C                   EXCEPT    TOT1
167500920813     C                   SETOFF                                       OF
167600920813     C                   END
167700920813     C*
167800920813     C                   EXCEPT    TOT2
167900920813     C                   ADD       1             C
168000920813     C                   END
168100920813     C*
168200920813     C                   EXCEPT    TOT3
168300941017     C*
168400920813     C                   ENDSR
168500161018     C**************************************************************************
168600161018     C*   da spunta cerco la relativa bolla
168700161018     C**************************************************************************
168800161018     c     Cerca_bolla   BEGSR
168900161018     c                   clear                   nobolla
169000161018     c* chain la bolla e stampo collo doppio
169100161018     c     kartB         chain     fnart27l
169200161018    2c                   if        %found(fnart27l)
169300161018     c     karb          chain     fnarb01l
169400161018   x2c                   else
169500161018     c     kartB         chain     fnblt27l
169600161018    3c                   if        %found(fnart27l)
169700161018     c     karb          chain     fnblp01l
169800161018   x3c                   else
169900161018     c                   eval      Nobolla='S'
170000161018    3c                   endif
170100161018    2c                   endif
170200161018     c                   ENDSR
170300161018     C**************************************************************************
170400161018     C*   Ricerca colli eccedenti
170500161018     C**************************************************************************
170600161018     C     Eccedenti     BEGSR
170700161018     c                   setoff                                       12
170800161018     c                   seton                                        18
170900161018     c                   clear                   ecce_descr
171000161018     c
171100161018     c                   exsr      Cerca_bolla
171200161018     c* Se manca bolla --> sempre eccedente
171300161018    1c                   if        nobolla='S'
171400161018    4c                   if        *in01 and not *inOF and uno=' '
171500161018     c                   except    DETvuota
171600161018    4c                   endif
171700161018     c                   exsr      ERRORE
171800161018     c                   eval      UNO='S'
171900161018   x1c                   else
172000161018     c* Richiamo driver per verificare se la bolla è a mag giac oggi
172100161018     c                   clear                   fnlgi1ds
172200161018     c                   eval      ILGI1AAS=arbaas
172300161018     c                   eval      ILGI1lnp=arblnp
172400161018     c                   eval      ILGI1nrs=arbnrs
172500161018     c                   eval      ILGI1nsp=arbnsp
172600161018    3c                   select
172700161018     c                   when      arbpkc>0 and arbncp=arbncl
172800161018     c                   eval      ilgi1pkf=arbpkc
172900161018     c                   when      arbpkc>0 and arbncp<arbncl
173000161018     c                             and arbpkc>arbpkf
173100161018     c                   eval      ilgi1pkf=arbpkc
173200161018     c                   other
173300161018     c                   eval      ilgi1pkf=arbpkf
173400161018    3c                   endsl
173500161018     c                   movel     fnlgi1ds      kpjbu
173600161018     c                   call      'FNLGI1R'
173700161018     c                   parm                    kpjba
173800161018     c                   movel     kpjbu         fnlgi1ds
173900161201     c* Per IPG --> corretto se il flag da portare a mag giac = "Y"
174000161025     c* Per IMG --> corretto se il flag da portare a mag giac = "Y"  o già in giac
174100161026     c*         --> ignoro anche se deve uscire da mag gaicenza, ma non ancora effettuat
174200170329    2c***                if        (*in11 and olgi1AMG<>'Y'and
174300170329     c***                           olgi1AMG<>'S') or
174400170329     c***                          (not *in11 and olgi1AMG<>'Y' and
174500170329     c                   if        (olgi1AMG<>'Y' and
174600161025     c                             olgi1AMG<>'S'  and
174700161026     c                             olgi1inmg<>'S' and olgi1outmg<>'S')
174800161018     c*
174900161018     c                   select
175000170329     c**                 when      olgi1inmg<>' '
175100170329     c**                 eval      ecce_descr=' -già in Giacenza'
175200161026     c                   when      arbndc>0  and arbdcm=0
175300161018     c                   eval      ecce_descr=' -di bolla in distinta '
175400161025     c                   when      OLGI1OUTMG<>' '
175500161025     c                   eval      ecce_descr=' -non più giacente'
175600161018     c                   other
175700161025     c                   eval      ecce_descr=' -di colli IMA'
175800161018     c                   endsl
175900170329     c
176000170330     c* se trovo una spunta con data  o ora MAGGIORE "tolgo" la eccedenza
176100170330     c* salvo il record di BRV chesto leggendo
176200170329     C                   MOVEL     RECBRV        WSAVr
176300170330     c
176400170330     c                   exsr      RicSpu
176500170330     c
176600170330     C                   MOVEL     WSAVr         RECBRV
176700170330     c
176800170330     c                   movel     brvdfs        brvimm           14 0
176900170330     c                   move      brvhms        brvimm
177000170330
177100170330     c* considero soltanto spunte cat 4 e IMA
177200170330     c                   clear                   wstampa
177300170330    3c                   if        wspg='A' or wnpg=4
177400170330    4c                   if        wdfv>savdfv  or
177500170330     c                             (wdfv=savdfv and wimm>brvimm)
177600170330     c                   eval      wstampa='N'
177700170330    4c                   endif
177800170330    3c                   endif
177900170329     c*
178000170330    3c                   if        wstampa<>'N'
178100161018    4c                   if        *in01 and not *inOF and uno=' '
178200161018     c                   except    DETvuota
178300161018    4c                   endif
178400161018
178500161018     c                   exsr      ERRORE
178600161018     c                   eval      UNO='S'
178700170330    3c                   endif
178800170330    2c                   endif
178900161025    1c                   endif
179000161018     c
179100161018     c                   setoff                                       18
179200161018     c                   seton                                        12
179300161018     C                   ENDSR
179400941017     C**************************************************************************
179500941017     C*   R O U T I N E     I N I Z I A L E
179600941017     C**************************************************************************
179700941017     C     *INZSR        BEGSR
179800941017     C*
179900161017     C* DEFINIZIONE CHIAVI
180000000126     C     KBRV          KLIST
180100000126     C                   KFLD                    IMANPG
180200000126     C                   KFLD                    WNFV
180300000126     C                   KFLD                    VIDFGS
180400070118     C     KBRV1         KLIST                                                  FNBRV01L
180500941019     C                   KFLD                    IMANPG                         ..CATEGORIA
180600000126     C                   KFLD                    WNFV                           ..NR. FOGLIO
180700950316RM*  C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
180800941019     C                   KFLD                    ARTFLS                         ..LNP
180900941018     C                   KFLD                    ARTLNA                         ..LNA
181000941018     C                   KFLD                    ARTNRS                         ..SERIE
181100941018     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
181200070418     C     KBRV4         KLIST                                                  FNBRV01L
181300070418     C                   KFLD                    CONNPG                         ..CATEGORIA
181400070418     C                   KFLD                    ARTFLS                         ..LNP
181500070418     C                   KFLD                    ARTLNA                         ..LNA
181600070418     C                   KFLD                    ARTNRS                         ..SERIE
181700070418     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
181800070418     C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
181900990212RM*  C     KARTD         KLIST
182000990212RM*  C                   KFLD                    ARTFLS
182100990212RM*  C                   KFLD                    ARTLNA
182200990212RM*  C                   KFLD                    ARTNRS
182300990212RM*  C                   KFLD                    ARTNSC
182400081014     C     KARTb         KLIST
182500081014     C                   KFLD                    brvlnp
182600081014     C                   KFLD                    brvlnA
182700081014     C                   KFLD                    brvNRS
182800081014     C                   KFLD                    brvNSC
182900941017     C     KFVV          KLIST                                                  *
183000941019     C                   KFLD                    IMANPG                         *
183100941017     C                   KFLD                    COMNFV                         *
183200941028     C                   KFLD                    VIDFGS                         *
183300950316RM*  C     KFVV2         KLIST
183400950316RM*  C                   KFLD                    BRVNPG
183500000126RM*  C                   KFLD                    WNFV5
183600950316RM*  C                   KFLD                    BRVFGS
183700941017     C     KANM          KLIST                                                  FNANM02L
183800941017     C                   KFLD                    ARTFLS                         *
183900941017     C                   KFLD                    ARTLNA                         *
184000941017     C                   KFLD                    ARTNRS                         *
184100941018     C                   KFLD                    ARTNSC                         *
184200941017     C                   KFLD                    ANMCAA                         *
184300941017     C     KART          KLIST                                                  *
184400941017     C                   KFLD                    ARBAAS                         *
184500941017     C                   KFLD                    ARBLNP                         *
184600941017     C                   KFLD                    ARBNRS                         *
184700941017     C                   KFLD                    ARBNSP                         *
184800081014     C     KARB          KLIST                                                  *
184900081014     C                   KFLD                    ARtAAS                         *
185000081014     C                   KFLD                    ARtLNP                         *
185100081014     C                   KFLD                    ARtNRS                         *
185200081014     C                   KFLD                    ARtNSP                         *
185300040603     C     Kar5          KLIST                                                  *
185400040603     C                   KFLD                    ARBAAS                         *
185500040603     C                   KFLD                    ARBLNP                         *
185600040603     C                   KFLD                    ARBNRS                         *
185700040603     C                   KFLD                    ARBNSP                         *
185800040603     C                   KFLD                    ktrd                           *
185900970721     C     KGCA          KLIST                                                  *
186000970721     C                   KFLD                    ARBAAS                         *
186100970721     C                   KFLD                    ARBLNP                         *
186200970721     C                   KFLD                    ARBNRS                         *
186300970721     C                   KFLD                    ARBNSP                         *
186400990212     C                   KFLD                    KFRG
186500950810     C     KSAV          KLIST                                                  *
186600950810     C                   KFLD                    SAVAAS                         *
186700950810     C                   KFLD                    SAVLNP                         *
186800950810     C                   KFLD                    SAVNRS                         *
186900950810     C                   KFLD                    SAVNSP                         *
187000941017     C     KLBL          KLIST                                                  *
187100941017     C                   KFLD                    LBLAAN                         *
187200941017     C                   KFLD                    LBLLPN                         *
187300941017     C                   KFLD                    LBLNRN                         *
187400941017     C                   KFLD                    LBLNSN                         *
187500941017     C     KTAB          KLIST
187600941017     C                   KFLD                    CODUT
187700941017     C                   KFLD                    COD
187800941017     C                   KFLD                    KEY
187900050510     C     KTAB2         KLIST
188000050510     C                   KFLD                    CODUT
188100050510     C                   KFLD                    COD
188200990212     C     KDCD          KLIST
188300990212     C                   KFLD                    DCDAAC                         *
188400990212     C                   KFLD                    DCDFIL                         *
188500990212     C                   KFLD                    DCDNCA                         *
188600021203
188700021203     C     Kfapd         klist
188800021203     C                   kfld                    kpdtip
188900021203     C                   kfld                    kpdpdr
189000941017     C*
189100970721     C     *LIKE         DEFINE    GCPFRG        KFRG
189200970721     C                   MOVEL     0             KFRG
189300941017     C     *LIKE         DEFINE    TBLCOD        COD
189400941017     C     *LIKE         DEFINE    TBLKEY        KEY
189500941019     C     *LIKE         DEFINE    BRVNPG        IMANPG                         CATEGORIA FOGLIO
189600070418     C     *LIKE         DEFINE    BRVNPG        conNPG                         CATEGORIA FOGLIO
189700031218     C     *LIKE         DEFINE    BRVNFV        WNFV
189800031218     C     *LIKE         DEFINE    FVVNFV        WNFV5
189900941018     C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO VIAGGIO
190000941018     C     *LIKE         DEFINE    ARTDAM        STADAM                         DATA ARRIVO MERCE
190100941018     C     *LIKE         DEFINE    ARBSPE        COMSPE
190200941019     C     *LIKE         DEFINE    ARBLNA        KLNA                           LINEA ARRIVO
190300031218     C     *LIKE         DEFINE    FVVFGS        SAVFGS
190400950317RM*  C     *LIKE         DEFINE    FVVDFV        SAVDFV
190500941017     C*
190600101220      /free
190700101220       // Carico le categorie con ambito Arrivo
190800101220       xx=2   ;
190900101220       // Alla posizine 1 metto al defluenza
191000101220       npgarr(1)='1D'  ;
191100101220
191200101220       setll   (1:'7N') tabel00f   ;
191300101220       reade   (1:'7N') tabel00f   ;
191400101220
191500101220       dow not %eof(tabel00f)      ;
191600101220       if tblflg=' '               ;
191700101220       ds7n=tbluni                 ;
191800101220
191900101220       if §7ntbo<>' '              ;
192000101220
192100101220          if §7ntbo='A'               ;
192200101220          npgArr(xx)=tblkey;
192300101220          xx=xx+1  ;
192400101220          endif  ;
192500101220       else   ;
192600101220
192700101220         if  §7ntb1='A'                         ;
192800101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc1   ;
192900101220         xx=xx+1  ;
193000101220         endif                                  ;
193100101220         if  §7ntb2='A'                         ;
193200101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc2   ;
193300101220         xx=xx+1  ;
193400101220         endif                                  ;
193500101220         if  §7ntb3='A'                         ;
193600101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc3   ;
193700101220         xx=xx+1  ;
193800101220         endif                                  ;
193900101220         if  §7ntb4='A'                         ;
194000101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc4   ;
194100101220         xx=xx+1  ;
194200101220         endif                                  ;
194300101220         if  §7ntb5='A'                         ;
194400101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc5   ;
194500101220         xx=xx+1  ;
194600101220         endif                                  ;
194700101220         if  §7ntb6='A'                         ;
194800101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc6   ;
194900101220         xx=xx+1  ;
195000101220         endif                                  ;
195100101220
195200101220       endif                       ;
195300101220       endif                       ;
195400101220
195500101220       reade   (1:'7N') tabel00f   ;
195600101220       enddo                       ;
195700101220      /end-free
195800101220
195900941017     C                   ENDSR
196000101202
196100101202      *---------------------------------------------------------------*
196200101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
196300101202      *---------------------------------------------------------------*
196400101202     c     sr_DatiJob    BEGSR
196500101202      *
196600101202     c     *dtaara       define    §azute        AZUTEds
196700101202     c     *dtaara       define    §datiute      dDatiUte
196800101202      *
196900101202     c                   in(E)     *dtaara
197000101202     c                   IF        %Error or RSUT = *blanks
197100101202     c                   clear                   TIBS34ds
197200101202     c                   call      'TIBS34R'
197300101202     c                   parm                    TIBS34ds
197400101202     c                   in        *dtaara
197500101202     c                   ENDIF
197600101202      *
197700101202     c                   ENDSR
197800941017     C**************************************************************************
197900161028     Otigcp000  E            AGG5
198000161017     O                       GCPDLMa
198100950316RM*  OPRTF198   E            TES              02
198200911015     O                       STA(1)              66
198300950810     O              N10      STA(2)             132
198400161017     O               10N11   STA(18)            132
198500170124     O*********      10 11   STA(22)            132
198600950320     O                       STA(17)            198
198700900626     O                       RSUT                20
198800960927     O                       UDATE8             181 '  /  /    '
198900950320     O                       PAGE          Z    198
199000920813     O          E            TES         1
199100160503     o                       statipo             67
199200160503     o                       statipo2           130
199300050511     O                       UTIME              179 '  :  :  '
199400931228     O               02      STA(4)              70
199500931228     O          E    02      TES         0  0
199600931228     O                       STA(4)              70
199700931228     O          E            TES         2
199800911015     O                       STA(3)              66
199900931228     O                       STA(16)            132
200000060508     O                       STA(20)            198
200100161018     O              n11                          13 'I.M. .'
200200161018     O              n11      VIDTIN              12
200300170124     O*********      11                          13 'I.P.G.'
200400060508RM*  O                       SAVFGS              21
200500060508     O                       DESCDU              42
200600920813     O                       COMNFV        Z     56
200700950317     O                       STADFV        8     73
200800960927     O                       VIDFIL             116
200900960927     O                       DESFIL             137
201000060508     O                       stalna             177
201100060508     O                       DESlna             198
201200070418     O          E   n10      TES         1
201300070418     O                       STA(21)            198
201400070418     O                       stamdc             188
201500101215     O                       staimaa            146
201600920813     O          E            TES2        2
201700911018     O                       STA(5)              66
201800911018     O                       STA(8)             132
201900040621     O                       STA(19)            198
202000920813     O          E            TES2        1  1
202100911018     O                       STA(6)              66
202200911018     O                       STA(9)             132
202300911015     O*
202400090522     O          E            DETnoBO     1
202500090522     O                       brvLNP        Z      8
202600090522     O                       brvNRS        Z     11
202700090522     O                       brvLNA        Z     29
202800090522     O                       brvZNC              87
202900090522     O                       brvnps             122
203000090522     O                       brvNSC        Z    131
203100090522     O
203200090522     O          E            DET         1
203300090522     O                  05   ARBAAS        Z
203400090522     O                  05   ARBLNP        Z   +  1
203500090522     O                  05 03ARBNRS        Z   +  1
203600090522     O                  05 03                  +  0 '-'
203700090522     O                  05   ARBNSP        Z   +  0
203800090522     O                  05   STADSP            +  1 '  /  '
203900090522     O                  05   ARBLNA        Z   +  1
204000090522     O                  05   STARSM            +  1
204100090522     O                  05   STARSD            +  1
204200090522     O                  05N04ARBNCD        Z   +  1
204300090522     O                  05N04                  +  0 '/'
204400090522     O                  05N04ARBNCA        Z   +  0
204500090522     O                  05 04                    84 'NON SEQUENZIALI'
204600090522     O                  05   ARBZNC            +  1
204700090522     O                  05   stabva            +  0
204800090522     O                  05   ARBTMC            +  1
204900090522     O                  05N06ARBPDC        Z   +  1
205000090522     O                  05N06                  +  0 '-'
205100100707     O                  05   DESCPC            +  0
205200090522     O              N07      STADAM            +  1 '  /  /    '
205300090522     O              N07      ARTNAP            +  1
205400090522     O              N07      ARTNSC        Z   +  2
205500090522     O               07                         131 'ABILIT.MERCE FORZATA'
205600081014     O* ERRORI:
205700950411     O*
205800081014     O          E            DETERR      0  0
205900031219     O                    n13                   155 'NON SPUNTATO IN DATA '
206000031219     O                    n13STADFV        8   +  1
206100950316     O*
206200040621     O                     13brvFGS             137
206300040621     O                     13                   138 '-'
206400040621     O                     13ST2FGS             146
206500161025     O                     13                   149 'in'
206600070118     O                     13ST2CAT             153
206700070118     O                     13brvNPG             155
206800070118     O                     13                   156 '-'
206900070118     O                     13brvNFV        Z    162
207000161025     O                     13                   165 'il'
207100070118     O                     13ST2DFV             171 '  /  '
207200161026     O**                   13                   175 'ore'
207300161026     O                     13staHMS             178 '  :  '
207400161130     O                       STADSF             198
207500070418     O          E            DETndc      1
207600070418     O                                           87 'In distinta:'
207700070418     O                       ARBndc        2  +   1
207800070418     O                                        +   2 'del'
207900070418     O                       staddc            +  1 '  /  /    '
208000081014     o* Collo doppio
208100081014     O          E            DETdoppio   0  0
208200161018     O                                          153 'COLLO   D O P P I O'
208300161018     o* Collo eccedente
208400161018     O          E            DETeccede   0  0
208500161018     O                                          153 'COLLO   E C C E D E'
208600161018     O                                          159 ' N T E'
208700161018     O                       ecce_descr         185
208800100706     o* Collo presente da XX giorni in IMA
208900100706     O          E            DETspuIMA   0  0
209000100707     O                                          160 '   COLLO SPUNTATO PER '
209100100707     O                                        +   1 'ALMENO '
209200100706     O                       §VPOGGIMA     2  +   1
209300100707     O                                        +   1 'GIORNI in I.M.A.'
209400081014     o* riga vuota
209500081014     O          E            DETvuota    2
209600081014     O                                          157 ' '
209700050405     O* STAMPO NUMERO C.A.e ubicazione collo
209800990212     O          E            DETCA       1
209900050405     O               24                          87 'Ubicazione collo:'
210000050405     O               24      §ar5zng           +  2
210100050405     O               25                         138 'C.A.'
210200050405     O               25      DCTFIL            +  1
210300050405     O               25      DCTNCA        Z   +  1
210400050405     O               25                        +  1 'del'
210500050405     O               25      STADCA            +  1 '  /  /  '
210600990212     O                     26                  +  1 'chiusa il'
210700990212     O                     26STACHC            +  1 '  /  /  '
210800990303     O                     26DCTCCH            +  1
210900990303     O                     26                  +  0 '-'
211000990303     O                     26STACCH            +  0
211100920813     O          E            OK          3
211200920813     O                       STA(11)             66
211300920813     O          E            OK          0  0
211400920813     O                       STA(11)             66
211500920813     O*
211600920813     O          E            TOT1        3  1
211700920813     O                       STA(13)             66
211800920813     O          E            TOT2           1
211900920813     O                       NPS(C)              33
212000920813     O                       CPS(C)        2     58
212100920813     O          E            TOT3           1
212200920813     O                       STA(14)             66
212300920813     O          E            TOT3           1
212400920813     O                       STA(15)             66
212500920813     O                       TOTSPU        2     58
212600920813     O*
212700920813     O          E            TOT3        3
212800920813     O                       STA(12)             66
212900920813     O          E            TOT3        0  0
213000920813     O                       STA(12)             66
213100941017     O**************************************************************************
213200950316** STA
213300060508XXXXXXXXXXXXXXXXXXXX                                                 1
213400950320 ** I.M.A. : INVENTARIO MAGAZZINO ARRIVI **                          2
213500161018FOGLIO        di  XXX-XXXXXXXXXXXXXXXXXXXX    N.ro XXXXX   del XX/   3
213600931228                                       *** D E F I N I T I V O ***   4
213700040603    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO        5
213800941019ANNO LNP SER NUMERO  DATA ARR                                        6
213900040603XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx  7
214000040603     SEGNACOLLI    ZON ULT  AUTOTRASPORT.   A R R I V O SEGNACOLLO   8
214100040603      DAL /   AL   ARR CONS   CONSEGNA       DATA   PIST MANCANTE    9
214200040603XX XXXXXXX/XXXXXXX XXV X XXXXXXX-XXXXXXXXX XX/XX/XXXX XX  XXXXXXX   10
214300920813             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
214400920813 **  FINE  STAMPA  **
214500920813   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI      13
214600920813                        ---------------------------------------     14
214700920813               TOTALE                                               15
214800060508XX/XXXX           Punto Operativo in Gestione: XXX-xxxxxxxxxxxxxxx  16
214900950320                         FNLR31R                          PAG.XXXX  17
215000950810 ** I.M.G. : INVENTARIO MAGAZZINO GIACENZE **                       18
215100040621 S  P  U  N  T  A  T  O      D  A                                   19
215200060508xxxxx           Linea Arrivo Selezionata: XXX-xxxxxxxxxxxxxxxxxxxx  20
215300070418                Elabora anche spedizioni in distinta: XX            21
215400161031 ** I.P.G. : QUADRATURA COLLI A MAGAZZINO GIACENZE **               22
215500161017**
215600950316OVRPRTF FILE(PRTF198) USRDTA('I.M.A.')
215700161017**
215800161017DLYJOB DLY(01)
