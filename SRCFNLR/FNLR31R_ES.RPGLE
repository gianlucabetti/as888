000100040603     H DECEDIT('0,') DATEDIT(*ymD.)
000200950711     H* FNLR31R *-----------------------------------------------------*
000300161014     H*        - INVENTARIO MAGAZZINO  I.M.A. - IMG - CPG    Esegue
000400930629     H*---------------------------------------------------------------*
000500161017     ftigcp26l  uf   e           k disk    RENAME(tigcp000:tigcp026)
000600161017     f                                     usropn
000700911015     FTABEL00F  IF   E           K DISK
000800911015     FAZORG01L  IF   E           K DISK
000900050331     FTigcp21L  IF   E           K DISK
001000021203     FFIAPD01L  IF   E           K DISK
001100040427     FFNARB05L  IF   E           K DISK    infds(fnarbinf)
001200040427     Ffnarb00f  UF   E             DISK    rename(fnarb000:fnarbf00)
001300941017     FFNARB01L  IF   E           K DISK
001400941017     F                                     RENAME(FNARB000:FNARB001)
001500090522     FFNblp01l  IF   E           K DISK    prefix(arb:3)
001600090522     FFNblt27l  IF   E           K DISK    prefix(art:3)
001700040603     FfIAR501l  IF   E           K DISK
001800040428     FFNART01L  UF   E           K DISK
001900081014     FFNART27L  iF   E           K DISK    RENAME(FNARt000:FNARt027)
002000941017     FFNLBL02L  IF   E           K DISK
002100070118     FFNBRV01L  IF   E           K DISK
002200070418RM*  FFNBRV07L  IF   E           K DISK    RENAME(FNBRV000:FNBRV007)
002300140618RM*  FFNBRV05L  IF   E           K DISK    RENAME(FNBRV000:FNBRV005)
002400081014     FFNANM02L  uF A E           K DISK
002500941017     FFNFVV01L  UF   E           K DISK
002600990212     FFNDCD02L  IF   E           K DISK
002700990212     FFNDCT01L  IF   E           K DISK
002800990212     F                                     RENAME(FNDCT000:FNDCT001)
002900950316     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
003000950316     F                                     USROPN
003100941017     D*
003200161017     D STA             S             66    DIM(22) CTDATA PERRCD(1)             SKIERE STAMPA
003300101215     D L6              S              3  0 DIM(30)                              FIL da elaborare
003400101215     D Lspu            S              3  0 DIM(30)                              FIL GEST ARRIV
003500071227     D NPS             S              2    DIM(90)                              LETT BARCOD
003600071227     D CPS             S              7  0 DIM(90)                              COLLI LETTI
003700941019     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
003800161017     D CMDD            S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
003900050511     D zone            S              2    DIM(200)                             COMANDO QCMDEXC
004000160503     D zone12          S              2    DIM(200)                             COMANDO QCMDEXC
004100101220     d NPGARR          s              2    dim(10)
004200101220     D*
004300040427     D fnarbinf        ds
004400040427     D  fnarbnrri            397    400i 0
004500941017     D PARAM1          DS
004600911015     D  COMNFV                 1      5  0
004700911015     D  VIDSN                  6      6
004800941028     D* FILIALE DI CUI GESTIRE LE LINEE IN ARRIVO
004900920813     D  VIDFIL                 7      9  0
005000941028     D* FILIALE APERTURA FOGLIO
005100941031     D  VIDFGS                10     12  0
005200950810     D* TIPO INVENTARIO --> G-GIACENZE A-ARRIVI
005300950810     D  VIDTIN                13     13
005400160502     D*
005500160502     D  PAR_lib               14     14
005600060508     D* linea di arrivo
005700060508     D  vidlna                15     17  0
005800070418     D* solo per IMA - elabora colli mancanti di spedizioni in distinta
005900070418     D  vidmdc                18     18
006000101215     D* solo colli spuntati alla lna finale            - solo IMA
006100101215     D  parimaa               19     19
006200160502     D* TIPO zona consegna--> P=parcel   M=messaggerie  P1/P2=Parcel 1° e 2° turno
006300160502     D  PARTpm1               20     21
006400160502     D  PARTpm2               22     23
006500950810     D*** PARAMETRI PER RICERCA FASE ANOMALIA    - TRUL11R -
006600950810     D PARAM5          DS
006700950810     D  PA5TLA                 1      1
006800950810     D  PA5NPG                 2      2
006900950810     D  PA5SPG                 3      3
007000950810     D  PA5FAS                 4      4
007100911018     D                 DS
007200941018     D  COMAAS                 1      4  0
007300941018     D  ARBMGS                 5      8  0
007400941018     D  ARBDSP                 1      8  0
007500911018     D                 DS
007600941018     D  ARBAAS                 1      4  0
007700941018     D  ARBLNP                 5      7  0
007800941018     D  ARBNRS                 8      9  0
007900941018     D  ARBNSP                10     16  0
008000941018     D  ARBSPE                 1     16  0
008100950810     D                 DS
008200950810     D  SAVAAS                 1      4  0
008300950810     D  SAVLNP                 5      7  0
008400950810     D  SAVNRS                 8      9  0
008500950810     D  SAVNSP                10     16  0
008600950810     D  SAVSPE                 1     16  0
008700911015     D WLBDAT          DS
008800941017     D  G02DAT                 1      8  0
008900941017     D  G02INV                 9     16  0
009000941017     D  G02ERR                17     17
009100941017     D  G02TGI                18     22  0
009200990212     D                 DS
009300990212     D  DCTDCA                 1      8  0
009400990212     D  DCTAAC                 1      4  0
009500990212     D  DCTMGC                 5      8  0
009600941019     D* DS PER TRUL06R - CARICAMENTO £X
009700941019     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009800941019     D  LIN                    1     90  0
009900941019     D                                     DIM(30)                              FIL. COMODO
010000031219     D* REPERISCE DATI FOGLI
010100031219     D DSLV53        E DS                  EXTNAME(FNLV53DS)
010200960530     D KPJBA         E DS
010300101202      *
010400101202      * - Parametri per richiamo al pgm di controllo profilo utenti
010500101202     d TIBS34ds      e ds
010600101202      * - Ds di riferimento al file esterno AzUte00F
010700101202     d AZUTEds       e ds                  ExtName(AzUte00F)
010800101202      * - Ds per dati organigramma
010900101202     d dDatiUte      e ds
011000101202      *
011100161017     D DS7B          E DS
011200161017     D DS7C          E DS
011300101220     D DS7N          E DS
011400920318     D DS3A          E DS
011500050511     D DS03          E DS
011600040603     D DAR5GEN       E DS
011700100706     D DVPO          E DS
011800990212     D DTAD            DS
011900990212     D  §TARGR                26     26
012000990303     D DCCH            DS
012100990303     D  §CHDES                 1     50
012200990303     D  §CHIMA                53     53
012300070118     D RECBRV        E DS                  EXTNAME(FNBRV00F)
012400070118     D WSAVB         E DS                  EXTNAME(FNBRV00F)
012500031219     d                                     prefix(S_)
012600990212     D DSBS02        E DS                  EXTNAME(TIBS02DS)
012700160927     D tigcp00ds     E DS                  EXTNAME(tigcp00F)
012800040427      * DS Esterna x gestione allocazione ed invio messaggi
012900040427     D TRUL82DS      E DS
013000081014      * DS Esterna x controllo colli doppi
013100081014     D FNLRV7DS      E DS
013200160927      * DS Esterna x controllo colli in IMG
013300160927     D FNLGI1DS      E DS
013400040806      * ds per il controllo della presenza di CA per la spedizione richiesta
013500040806     d FIDN12DS      e ds
013600040806     d  skKey                 26    305    dim(20)
013700040806     d  skAnn                306    325    dim(20)
013800040806     d  skDca                326    485  0 dim(20)
013900101202     d**skFca                486    545  0 dim(20)
014000040806     d  skDch                546    705  0 dim(20)
014100040806     d  skCch                706    745    dim(20)
014200101202      *
014300040806     d dsKey           ds
014400040806     d  dsaac                         4  0
014500040806     d  dsfil                         3  0
014600040806     d  dsnca                         7  0
014700040806      *
014800101202      *
014900021203      *
015000101220     d WnpgArr         s              2
015100101220     d Trov            s              2  0
015200090522     d Nobolla         S              1
015300090522     d uno             S              1
015400081014     d statipo         S             21
015500160503     d statipo2        S             21
015600101215     d staimaa         S             44
015700070418     d stamdc          S              2
015800160503     d indMP           S              4  0
015900160503     d ind12           S              4  0
016000050511     d WSPG            S                   LIKE(FVVSPG)
016100040108     d kpdpdr          S                   LIKE(APDpdr)
016200031219     d wdfv            S                   LIKE(d53dfv)
016300031219     d kpdtip          S                   LIKE(APDtip)
016400040603     d ktrd            S                   LIKE(ar5trd) inz('GEN')
016500161018     d ecce_descr      S             25
016600100706     d dataEUR         s               d   datfmt(*eur)
016700100706     d DataISO         s               d   datfmt(*iso)
016800101202     d*
016900040428     D msgjob          C                   CONST('La Manut.Bolle sta bloccando -
017000040428     D                                     l''elabor. Dell''IMA:USCIRE SUBITO D-
017100040428     D                                     A QUESTO LAVORO')
017200050511     D CPARCEL         C                   CONST('     P A R C E L     ')
017300050511     D CMessag         C                   CONST('M E S S A G G E R I E')
017400160503     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
017500160503     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
017600101215     D CsoloLNA        C                   CONST('Solo colli spuntati alla line-
017700101215     d                                     a arrivo finale')
017800911018     C*--------------------------------------------------------------- *
017900911018     C*                  INDICATORI                                    *
018000911018     C*--------------------------------------------------------------- *
018100911018     C* 01    - ESITONO ANOMALIE DI MANCANZA
018200950810     C* 02    - CHIESTA CHIUSURA  FOGLIO
018300911018     C* 03    - ESISTE SERIE
018400911018     C* 04    - SEGNACOLLI NON SEQUENZIALI
018500911018     C* 05    - ON CAMBIATA SPEDIZIONE
018600920325     C* 06    - NON C'E' COD PADRONCINO CONSEGNA
018700920708     C* 07    - MANCA DATA ARRIVO : SPEDIZIONE FORZATA
018800920813     C* 08    - BOLLA GESTITA
018900161017     C* 11    - e 10 ON --> ZPG
019000950810     C* 10    - ON --> IMG  OFF --> IMA
019100081014     C* 12    - CONTROLLO COLLI DOPPI
019200950810     C* 13    - NON TROVATA SPUNTA IN IMA MA  IN ALTRO FOGLIO NELLA
019300950810     C*         STESSA DATA
019400950810     C* 14    - NON TROVATA SPUNTA IN IMA
019500031219     C* 15    - TROVATA SPUNTA SU ALTRO FOGLIO MA DEVO CREARE L'ANOMAL
019600100706     C* 16    - collo in IMA ma spuntato in IMA più di XX volte...da tabella VPO
019700161018     C* 18    - collo eccedente
019800050405     C* 24    - STAMPA ubicazione collo se c'e'
019900050405     C* 25    - STAMPA se c'e' c.a.
020000050405     C* 26    - STAMPARE DATA CHIUSURA C.A.
020100941017     C* 30    - LETTURA FNARB
020200941017     C* 31    - LETTURA FNART
020300911018     C* 32/34 - COMODO
020400911018     C*---------------------------------------------------------------*
020500900622     C     *ENTRY        PLIST
020600900622     C                   PARM                    KPJBA
020700941017     C                   MOVEL     KPJBU         PARAM1
020800000126     C                   Z-ADD     COMNFV        WNFV
020900900828     C*-----------------------------------------------------
021000950810     C* IMPOSTAZIONI INIZIALI
021100950810     C                   EXSR      IMPO
021200950810     C*-----------------------------------------------------
021300161018     c* il dly mi serve solo per il debug in batch
021400161017     C                   Z-ADD     80            LUNG             15 5
021500161017     C                   MOVEL     *BLANKS       COMMAN           80
021600161017     C                   MOVEA     VIDTIN        CMD(35)
021700161017     C                   MOVEA     CMDd          COMMAN
021800161017     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
021900161017     C                   PARM                    COMMAN
022000161017     C                   PARM                    LUNG
022100161017
022200941019     C                   Z-ADD     80            LUNG             15 5
022300941019     C                   MOVEL     *BLANKS       COMMAN           80
022400950810     C                   MOVEA     VIDTIN        CMD(35)
022500941019     C                   MOVEA     CMD           COMMAN
022600161018     c                   if        *in11
022700161018     c                   eval      %subst(comman:31:6)='C.P.G.'
022800161018     c                   endif
022900941019     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
023000941019     C                   PARM                    COMMAN
023100941019     C                   PARM                    LUNG
023200950810     C*
023300950316RM*  C                   OPEN      PRTF198
023400980128     C                   SETON                                        OF
023500161017     c
023600161017     c* Se si tratta di quadratura ZPG eseguo routine di lettura diversa
023700911015     C*
023800040108     C* CONSIDERO SOLO i p.o. di £6
023900941019     C                   Z-ADD     1             WA                3 0          CONTATORE
024000040108    1C     L6(WA)        DOWNE     *ZERO
024100941019     C*
024200040108     C                   Z-ADD     L6(WA)        KLNA                           LINEA ARRIVO
024300161017   1ac                   if        *in11
024400161017     c                   exsr      LeggiGIAC
024500161017  z1ac                   else
024600161017     C*
024700031218     C     KLNA          SETLL     FNARB05L
024800040427     C     KLNA          READE     FNARB05L                               30
024900950810     C*
025000950810    3C     *IN30         DOWEQ     *OFF
025100050511     c* Se ima messaggerie o parcel, escludo le zone non di interesse
025200160503     c                   eval      indMP=0
025300160503     c                   eval      ind12=0
025400160503     c
025500160503     c                   if        wselzona='S'
025600160503     c                   movel     arbznc        alfzona           2
025700160503
025800160503     c                   if        wparc='S' or wmess='S'
025900160503     c***  alfzona       lookup    zone                                   35
026000160503     c                   eval      indMP =%lookup(alfzona:zone)
026100050511     c                   endif
026200160503     c                   if        wparc1='S' or wparc2='S'
026300160503     c                   eval      ind12 =%lookup(alfzona:zone12)
026400160503     c                   endif
026500160503
026600160503     c                   endif
026700160503   3ac                   if        wselzona=' ' or  indmp>0 or  ind12>0
026800920813     C*
026900920318     C* CHAIN CODICE BOLLE PER ESCLUDERE I RECUPERI
027000920318     C                   MOVEL     '3A'          COD
027100950810     C                   MOVEL(P)  ARBCBO        KEY
027200941018     C     KTAB          CHAIN     TABEL00F                           35
027300920318     C  N35              MOVEL     TBLUNI        DS3A
027400920318     C   35              MOVEL     'F1'          §3ATB1
027500920318     C*
027600950810     C* ESCLUDO:  LE BLOCCATE CON "B"/
027700930518     C*           QUELLE IN CONSEGNA / QUELLE CONSEGNATE / I RECUPERI
027800070418     C     ARBdcm        ifeq      *zeros
027900950810     C     §3ARBL        ANDNE     'R'
028000950810     C     ARBFBC        ANDNE     'B'
028100000512     C*
028200960516     C* CONTROLLO SE E' GIACENTE
028300960516     C                   CLEAR                   WGIAC
028400960516     C     ARBFBC        IFEQ      'G'
028500050331     C     KGCA          CHAIN     Tigcp21L                           35
028600970721     C* SE C'E' LA GIACENZA E NON E' A FASE 35 LA ESCLUDO
028700970721     C  N35GCPFAS        IFLT      35
028800050331     C     GCPFAS        oreq      36
028900160927     c* la merce è in giacenza solo dopo 2/3 giorni: verifico con driver
029000160927     c                   clear                   fnlgi1ds
029100160927     c                   eval      ILGI1AAS=arbaas
029200160927     c                   eval      ILGI1lnp=arblnp
029300160927     c                   eval      ILGI1nrs=arbnrs
029400160927     c                   eval      ILGI1nsp=arbnsp
029500161018    3c                   select
029600161018     c                   when      arbpkc>0 and arbncp=arbncl
029700161018     c                   eval      ilgi1pkf=arbpkc
029800161018     c                   when      arbpkc>0 and arbncp<arbncl
029900161018     c                             and arbpkc>arbpkf
030000161018     c                   eval      ilgi1pkf=arbpkc
030100161018     c                   other
030200161018     c                   eval      ilgi1pkf=arbpkf
030300161018    3c                   endsl
030400160927     c                   movel     fnlgi1ds      kpjbu
030500160927     c                   call      'FNLGI1R'
030600160927     c                   parm                    kpjba
030700160927     c                   parm                    tigcp00ds
030800161007     c                   movel     kpjbu         fnlgi1ds
030900160927
031000161024   5ac                   if        olgi1AMG='Y' or olgi1inmg='S'
031100970721     C                   MOVEL     'S'           WGIAC             1
031200160823     c                   endif
031300161024     c* se la bolla è da portare a mag giacenza OGGI, devo considerarla anche in IMA
031400161024     c*  e se manca dare segnalazione ma NON creando l'anomalia
031500161024   5ac                   if        olgi1AMG='S'
031600161024     C                   MOVEL     'D'           WGIAC             1
031700161024     c                   endif
031800160823     c
031900960516     C                   ENDIF
032000970721     C                   ENDIF
032100950810     C*
032200950810     C* SE IMA --> ESCLUDO LE GIACENTI
032300950810    5C     *IN10         IFEQ      *OFF
032400960516     C     WGIAC         ANDNE     'S'
032500950810     C*
032600950810     C* SE IMG --> INCLUDO SOLO LE GIACENTI
032700950810     C     *IN10         OREQ      *ON
032800960516     C     WGIAC         ANDEQ     'S'
032900070418     c* escludo anche quelle in distinta in IMG
033000070418     c     arbndc        andeq     0
033100921013     C*
033200950317     C* ESCLUDO  SE BOLLA MAMMA E UNA DELLE FIGLIE E' IN CONSEGNA
033300960516     C*     O SE SONO TUTTE BLOCCATE
033400950810     C                   Z-ADD     ARBSPE        SAVSPE
033500960516     C*
033600960516     C                   CLEAR                   WESFI                          ESISTE FIGL
033700960516     C                   MOVEL     'S'           WFIBLO            1            FIGLIE BLOCC
033800950317     C                   SETOFF                                       36
033900950810     C     KSAV          SETLL     FNLBL02L
034000950810     C     KSAV          READE     FNLBL02L                               35
034100960516     C**
034200950810    6C     *IN35         DOWEQ     *OFF
034300950317     C*
034400950317     C     KLBL          CHAIN     FNARB01L                           35
034500950810    7C     *IN35         IFEQ      *OFF
034600960516     C                   MOVEL     'S'           WESFI             1
034700960516     C* FIGLIA IN CONSEGNA
034800960516    8C     ARBNDC        IFNE      0
034900950317     C                   SETON                                        3536
035000960516   X8C                   ELSE
035100960516     C*
035200960516     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
035300960516    9C     ARBFBC        IFNE      'B'
035400960516     C                   MOVEL     'N'           WFIBLO
035500960516    9C                   ENDIF
035600960516    8C                   ENDIF
035700960516    7C                   ENDIF
035800950317     C*
035900950810     C  N36KSAV          READE     FNLBL02L                               35
036000950810    6C                   ENDDO
036100960516     C*
036200960516     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
036300960516     C  N36WESFI         IFEQ      'S'
036400960516     C     WFIBLO        ANDEQ     'S'
036500960516     C                   SETON                                        36
036600960516     C                   ENDIF
036700070418     c
036800070418     c* Per 36 off - controllo in IMA se richiesti anche i colli mancanti
036900070418     c*              di spedizioni in distinta
037000070418     c  n36              if        not *in10 and vidmdc='N' and arbndc>0
037100070418     c                   seton                                        36
037200070418     c                   endif
037300911204     C*
037400950810    6C  N36              DO
037500950317     C*
037600040427     C     KSAV          CHAIN     FNARB01L                           31
037700950810     C                   MOVEL     ARBAMA        SAVAMA            1 0
037800040428     c                   clear                   wchain            1
037900911018     C* LEGGO DETTAGLIO
038000941017     C     KART          SETLL     FNART01L
038100040428     C  N10
038200040428     CAN 02KART          READE     FNART01L                               31
038300040428     C   10
038400040428     CORN02KART          READE(N)  FNART01L                               31
038500911018     C*
038600950810    7C     *IN31         DOWEQ     *OFF
038700911204     C                   SETOFF                                       34
038800950810     C*
038900950810     C* ESCLUDO  SE IL COLLO E' CONSEGNATO (CONSEGNA PARZIALE)
039000070418     C     ARTDCM        IFEQ      0
039100950810     C  N10              EXSR      ELAIMA
039200950810     C   10              EXSR      ELAIMG
039300950810    8C                   ENDIF
039400930629     C*
039500040428     C  N10
039600040428     CAN 02KART          READE     FNART01L                               31
039700040428     C   10
039800040428     CORN02KART          READE(N)  FNART01L                               31
039900950810    7C                   ENDDO
040000920318     C*
040100950810    6C                   ENDDO
040200950810    5C                   ENDIF
040300000512    4C                   ENDIF
040400050511   3aC                   ENDIF
040500911018     C*
040600040427     C     KLNA          READE     FNARB05L                               30
040700950810    3C                   ENDDO
040800161017     c
040900161017   1ac                   endif
041000941019     C*
041100040108     C                   ADD       1             WA                             CONTATORE L6
041200950810    1C                   ENDDO
041300161017     c
041400920813     C*
041500920813     C* RIEPILOGO SPUNTE PER PISTOLA
041600920813     C                   EXSR      RIEPIL
041700911018     C*
041800060508     C* SE NON TROVATE DISCORDANZE e non fatte parzializzazioni
041900060508     c*  se non ho parzii
042000070418     c* 18/04/07 - IMA chiude solo se richiesta forzatura chiusura
042100960530     C     *IN01         IFEQ      *OFF
042200070418     C     *IN10         andeq     *ON
042300161017     C     *IN11         andeq     *Off
042400060508     c
042500160503     c     wselzona      andeq     ' '
042600060508     c
042700060508     c     vidlna        andeq     0
042800060508     c
042900060508     C*  O FORZATA LA CHIUSURA METTO FLAG
043000911018     C     VIDSN         OREQ      'S'
043100060508     c
043200941017     C     KFVV          CHAIN     FNFVV01L                           32
043300920813     C  N32              MOVEL     'S'           FVVFCF
043400941017     C  N32              UPDATE    FNFVV000
043500911018     C                   END
043600050511     C*
043700081014     C* CHIUSURA PGM
043800990212     C                   CLEAR                   DSBS02
043900990212     C                   MOVEL     'C'           T02TLA
044000990212     C                   CALL      'TIBS02R'
044100990212     C                   PARM                    KPJBA
044200990212     C                   PARM                    DSBS02
044300031219     c
044400031219     C                   CLEAR                   DSLV53
044500031219     C                   MOVEL     'C'           D53TLA
044600031219     C                   CALL      'FNLV53R'
044700031219     C                   PARM                    DSLV53
044800040806     C*
044900040806     C                   MOVEL     'C'           I12TLA
045000040806     C                   CALL      'FIDN12R'
045100040806     C                   PARM                    FIDN12DS
045200140430     C*
045300140430     c                   clear                   fnlrv7ds
045400140430     C                   MOVEL     'C'           ILRV7TLA
045500140430     c                   eval      kpjbu= fnlrv7ds
045600140430     C                   CALL      'FNLRV7R'
045700140430     C                   PARM                    KPJBA
045800160927
045900160927     c                   clear                   fnlgi1ds
046000160927     c                   eval      ILGI1TLA= 'C'
046100160927     c                   movel     fnlgi1ds      kpjbu
046200160927     c                   call      'FNLGI1R'
046300160927     c                   parm                    kpjba
046400160927
046500040806      *
046600000000     C                   SETON                                        LR
046700031219     C**************************************************************************
046800031219     C* CONTROLLO SE SEGNACOLLO MANCANTE
046900031219     C**************************************************************************
047000031219     C     CTRART        BEGSR
047100031219     C*
047200031219     C                   SETOFF                                       131415
047300100706     C                   SETOFF                                       3316
047400031219     C                   MOVE      *BLANKS       STADSF
047500031219     C*
047600070118     C     KBRV1         SETLL     FNBRV01L                               33
047700070418     c
047800161017     c* se non ho trovato spunta IMx e spedizione in distinta, cerco
047900070418     c*  la categoria 4 del giorno
048000070418    1c                   if        not *in33 and arbndc>0 and vidmdc='S'
048100100706     C                   Z-ADD     4             conNPG
048200140618     C     KBRV4         chain     FNBRV05L
048300140618    2c                   if        %found(fnbrv05l)
048400070418     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
048500070418     c                   clear                   dslv53
048600070418     C                   z-add     brvnFV        D53NFV
048700070418     C                   MOVEL     brvNPG        D53NPG
048800070418     C                   MOVEL     brvFGS        D53FGS
048900070418     C                   CALL      'FNLV53R'
049000070418     C                   PARM                    DSLV53
049100070418     C* ERRORE
049200070418     c*  se sparato nella stessa data: ok
049300070418     c*  e sparato in altra data, rientro nei giri normali di controllo
049400070418     c*  spunte
049500070418    3c                   if        d53err=' ' and d53dfv=savdfv
049600070418     c                   seton                                        33
049700070418    3c                   endif
049800070418    2c                   endif
049900070418    1c                   endif
050000070418     c
050100031219     C* 14 ON - SPUNTA NON TROVATA
050200031219    1C     *IN33         IFEQ      *OFF
050300070418     c
050400031219     C                   SETON                                        14
050500031219     c*
050600031219     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
050700031219     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
050800031219     c*  immissione + alta
050900031219     C                   exsr      RICSPU
051000031219     C
051100031219     C* SE SPUNTA TROVATA CON DATA > STAMPO MA NON CREO ANOMALIA
051200031219     C                   IF        WDFV>SAVDFV
051300031219     C                   EVAL      *IN13=*ON
051400031219     C                   ENDIF
051500031219     C
051600031219     C* SE SPUNTA TROVATA CON DATA = STAMPO E CREO ANOMALIA SOLTANTO SE
051700031219     C*  CATEGORIA 2 O 6
051800031219     C                   IF        WDFV=SAVDFV
051900031219     C                   EVAL      *IN13=*ON
052000101202     C                   IF        BRVNPG=2
052100031219     C                   EVAL      *IN15=*ON
052200031219     C                   ENDIF
052300031219     C                   ENDIF
052400031219     c*
052500031219    1C                   END
052600960523     C*
052700031219     C
052800031219    1C                   IF        WDFV>=SAVDFV
052900960523     C* GIRO LA DATA
053000031219     C                   MOVE      WDFV          G02INV
053100960523     C                   MOVE      *ZEROS        G02DAT
053200960523     C                   MOVEL     3             G02ERR
053300960523     C                   CALL      'XSRDA8'
053400960523     C                   PARM                    WLBDAT
053500960523     C                   MOVEL     G02DAT        ST2DFV            4 0
053600960523     C**
053700960523     C* DECODIFICO LA FILIALE GESTIONE
053800031219     C     BRVFGS        CHAIN     AZORG01L                           32
053900031219    2C  N32ORGDE5        IFNE      *BLANKS
054000960523     C                   MOVEL     ORGDE5        ST2FGS            8
054100960523     C                   ELSE
054200960523     C                   MOVEL     ORGDES        ST2FGS            8
054300031219    2C                   ENDIF
054400960523     C**
054500960523     C* DECODIFICO LA CATEGORIA SPUNTA
054600031219    2C     BRVNPG        IFEQ      3
054700161017     C                   MOVEL     '7B'          COD
054800161017     C                   MOVEL(P)  wspg          KEY
054900161017     C     KTAB          CHAIN     TABEL                              32
055000161017     C  N32              MOVEL     TBLUNI        ds7b
055100161017     C   32              CLEAR                   ds7b
055200161017     C                   MOVEL     §7bdesb       ST2CAT            3
055300031219   X2C                   ELSE
055400960523     C                   MOVEL     '7N'          COD
055500031219     C                   MOVEL(P)  BRVNPG        KEY
055600960523     C     KTAB          CHAIN     TABEL                              32
055700960523     C  N32              MOVEL     TBLUNI        ST2CAT
055800960523     C   32              CLEAR                   ST2CAT
055900031219    2C                   ENDIF
056000031219     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
056100031219    2C                   IF        BRVNPG=4
056200031219     C                   Z-ADD     BRVNFV        WNFV5
056300031219     C     KFVV2         CHAIN(N)  FNFVV000                           32
056400070118RM*  C  N32              MOVEL     FVVDSF        STADSF           13
056500031219    2C                   ENDIF
056600031219    1C                   ENDIF
056700960523     C*
056800100706     c* Se trovata spunta, segnalo se sparata xx volte in IMA
056900100706     c*  solo per bolle in IMA più vecchie di ....
057000100707     c* Escludo bolle in distinta
057100150113     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
057200100707     c                   if        not *in14 and not *in10
057300100706     c                             and arbndc=0 and arbdsp<datapresIMA
057400100706     c
057500100706     c* conto quante volte presente in ima
057600100706     C                   Z-ADD     3             conNPG
057700100706     c                   clear                   conta             4 0
057800140618     C     KBRV4         setll     FNBRV05L
057900140618     C     KBRV4         reade     FNBRV05L
058000100706     c
058100140618    2c                   dow       not %eof(fnbrv05l)
058200100706     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
058300100706     c                   clear                   dslv53
058400100706     C                   z-add     brvnFV        D53NFV
058500100706     C                   MOVEL     brvNPG        D53NPG
058600100706     C                   MOVEL     brvFGS        D53FGS
058700100706     C                   CALL      'FNLV53R'
058800100706     C                   PARM                    DSLV53
058900100706     c
059000100706     c                   if        d53spg='A'
059100100706     c                   eval      conta=conta+1
059200100706     c                   endif
059300100706     c
059400140618     C     KBRV4         reade     FNBRV05L
059500100706     c                   enddo
059600100706     c
059700100706     c                   if        conta>=§vpoggima
059800100706     c                   seton                                        16
059900100706     c                   endif
060000100706     c
060100100706     c
060200100706     c                   endif
060300100706     c
060400960523     C                   ENDSR
060500960523     C*
060600031219     C**************************************************************************
060700031219     C* cerco spunta del collo in qualsiasi categoria
060800031219     C**************************************************************************
060900031219     C     RICSPU        BEGSR
061000031219     C                   CLEAR                   Wimm
061100031219     C                   CLEAR                   WSAVB
061200031219     C                   CLEAR                   WDFV
061300031219     C                   CLEAR                   WTROV
061400040108     C                   CLEAR                   WSPG
061500070118RM*  C     Kartd         CHAIN     FNBRV07l                           33
061600031219RM*  C*
061700031219RM* 1C     *IN33         DOWEQ     *OFF
061800031219     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
061900031219     c                   clear                   dslv53
062000031219     C                   z-add     brvnFV        D53NFV
062100031219     C                   MOVEL     brvNPG        D53NPG
062200031219     C                   MOVEL     brvFGS        D53FGS
062300070118     C****               MOVEL     brvfle        D53flf
062400031219     C                   CALL      'FNLV53R'
062500031219     C                   PARM                    DSLV53
062600031219     C* ERRORE
062700031219    2C     D53ERR        IFeq      ' '
062800070118     c                   movel     brvdfs        brvimm           14 0
062900031219     c                   move      brvhms        brvimm
063000031219    3c                   select
063100031219     c* Se maggiore memorizzo
063200031219     c                   when      d53dfv>wdfv
063300031219     C                   MOVEL     RECBRV        WSAVB
063400070118     C                   MOVEL     brvimm        Wimm             14 0
063500031219     C                   MOVEL     d53DFV        WDFV
063600040108     C                   MOVEL     d53spg        WSPG
063700031219     C                   MOVEL     'S'           WTROV             1
063800031219     c
063900031219     c* Se uguale tengo la data ora immissione + alta
064000031219     c                   when      d53dfv=wdfv  and brvimm>wimm
064100070118     C                   MOVEL     brvimm        Wimm             14 0
064200031219     C                   MOVEL     RECBRV        WSAVB
064300031219     C                   MOVEL     d53DFV        WDFV
064400040108     C                   MOVEL     d53spg        WSPG
064500031219     C                   MOVEL     'S'           WTROV             1
064600031219     C                   eval      wimm=brvimm
064700040129     c* Se stessa data e ora immissione, tengo cat 3 o 4
064800101202     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
064900070118     C                   MOVEL     brvimm        Wimm             14 0
065000040129     C                   MOVEL     RECBRV        WSAVB
065100040129     C                   MOVEL     d53DFV        WDFV
065200040129     C                   MOVEL     d53spg        WSPG
065300040129     C                   MOVEL     'S'           WTROV             1
065400040129     C                   eval      wimm=brvimm
065500031219    3c                   endsl
065600031219    2c                   endif
065700031219     c
065800070118RM*  C     Kartd         reade     FNBRV07l                               33
065900031219    1c                   enddo
066000031219     c
066100031219     C     WTROV         IFEQ      'S'
066200031219     C                   MOVEL     WSAVB         RECBRV
066300031219     C                   ENDIF
066400031219     c                   endsr
066500941017     C**************************************************************************
066600941017     C* IMPOSTO CAMPI FISSI
066700941017     C**************************************************************************
066800911018     C     IMPO          BEGSR
066900921022     C*
067000101202     C                   Z-ADD     1             CODUT             1 0
067100921022     C                   Z-ADD     3             IMANPG
067200950810     C                   CLEAR                   SAVSPE
067300950810     C                   CLEAR                   COMSPE
067400921022     C                   TIME                    UTIME             6 0
067500960927     C                   TIME                    W0140            14 0
067600960927     C                   MOVE      W0140         UDATE8            8 0
067700101202      *
067800101202      * Reperimento dati utente/aziendali
067900101202     c                   exsr      sr_DatiJob
068000101202      *
068100161017     C* 11 e 10 ON - cpg
068200161014     c                   select
068300161014     c                   when      vidtin='Z'
068400161017     C                   SETON                                        1011
068500161017     c                   open      tigcp26l
068600161017     C* 10 ON - IMG
068700161017     C                   when      vidtin='G'
068800950810     C                   SETON                                        10
068900161014     C                   ENDsl
069000921022     C*
069100921022     C* CHAIN E DATA DEL FOGLIO IMA
069200941017     C     KFVV          CHAIN(N)  FNFVV01L                           32
069300921022     C*
069400941018     C                   Z-ADD     FVVDFV        G02INV                         AAAA/MM/GG
069500941018     C                   Z-ADD     *ZERO         G02DAT
069600941018     C                   MOVEL     '3'           G02ERR
069700941018     C                   CALL      'XSRDA8'
069800941018     C                   PARM                    WLBDAT
069900941018     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
070000950316     C*
070100950317RM*  C                   Z-ADD     FVVDFV        SAVDFV
070200950317RM*  C                   Z-ADD     FVVFGS        SAVFGS
070300931228     C*
070400941028     C* DECODIFICA FILIALE DI APERTURA FOGLIO
070500941018     C     FVVFGS        CHAIN     AZORG                              32
070600931228     C  N32              MOVEL     ORGDES        DESCDU           20
070700000512     C*
070800000512     C                   Z-ADD     15            ANMCAA
070900000512     C                   MOVEL     '7C'          COD
071000000512     C                   MOVEL(P)  ANMCAA        KEY
071100000512     C     KTAB          CHAIN     TABEL00F                           35
071200000512     C  N35              MOVEL     TBLUNI        DS7C
071300000512     C   35              MOVEL     *BLANKS       DS7C
071400911018     C*
071500931228     C* 02 ON -  D E F I N I T I V O
071600911018     C     VIDSN         IFEQ      'S'
071700911018     C                   SETON                                        02
071800911018     C*
071900921022     C* PRENDO FASE ANOMALIA
072000950810     C                   CLEAR                   PARAM5
072100950810     C                   MOVEL     'L'           PA5TLA
072200950810     C                   MOVEL     3             PA5NPG
072300950810     C                   MOVEL     VIDTIN        PA5SPG
072400950810     C                   CALL      'TRUL11R'
072500950810     C                   PARM                    PARAM5
072600920409     C                   END
072700920813     C*
072800920813     C* DECODIFICA FILIALE IN GESTIONE
072900920813     C     VIDFIL        CHAIN     AZORG                              32
073000920813     C  N32              MOVEL     ORGDES        DESFIL           20
073100941019     C*
073200101215     C* CARICO L6
073300101215     C                   CLEAR                   DSUL06
073400101215     C                   MOVE      '£6'          D06COD
073500101215     C                   MOVEL     VIDFIL        D06KEY
073600101215     C                   MOVEL     'L'           D06TLA
073700101215     C                   MOVEL     DSUL06        KPJBU
073800101215     C                   CALL      'TRUL06R'
073900101215     C                   PARM                    KPJBA
074000101215     C                   MOVEL     KPJBU         DSUL06
074100101215     C                   MOVEA     LIN           Lspu
074200060508     c*
074300101215     C*  Se non parzializzata la linea di arrivo, elaboro L6
074400161014     c*  altrimenti elaboro solo lei
074500060508     c                   if        vidlna=0
074600101215     C                   MOVEA     LIN           L6
074700060508     c                   else
074800060508     c                   clear                   L6
074900060508     c                   movel     vidlna        l6(1)
075000060508     c
075100060508     c* Decodifica
075200060508     C     VIDlna        CHAIN     AZORG                              32
075300060508     C  N32              MOVEL     ORGDES        DESlna           20
075400060508     c                   movel     vidlna        stalna            3
075500060508     c                   endif
075600070418     c
075700070418     c* Elabora anche spedizioni in distinta
075800070418     c                   if        vidmdc='S'
075900070418     c                   eval      stamdc='SI'
076000070418     c                   else
076100070418     c                   eval      stamdc='NO'
076200070418     c                   endif
076300101215     c* solo colli spuntati alla linea di arrivo finale
076400101215     c                   if        parimaa='S'
076500101215     c                   eval      staimaa=csololna
076600101215     c                   endif
076700160503     c* Verifico le richieste delle zone di consegna
076800160503     c                   clear                   wParc             1
076900160503     c                   clear                   wParc1            1
077000160503     c                   clear                   wParc2            1
077100160503     c                   clear                   wMess             1
077200160503     c                   clear                   wSelzona          1
077300160503     c                   if        partpm1='P ' or partpm2='P '
077400160503     c                   eval      wparc='S'
077500160503     c                   eval      wselzona='S'
077600160503     c                   endif
077700160503     c                   if        partpm1='M ' or partpm2='M '
077800160503     c                   eval      wMess='S'
077900160503     c                   eval      wselzona='S'
078000160503     c                   endif
078100160503     c                   if        partpm1='P1' or partpm2='P1'
078200160503     c                   eval      wParc1='S'
078300160503     c                   eval      wselzona='S'
078400160503     c                   endif
078500160503     c                   if        partpm1='P2' or partpm2='P2'
078600160503     c                   eval      wParc2='S'
078700160503     c                   eval      wselzona='S'
078800160503     c                   endif
078900060508     c
079000050510     c* se richiesta soltanto zone parcel o messagerie, carico in
079100050510     c*  schiera
079200160503     c                   if        wparc='S' and wmess='S'
079300160503     c                   clear                   wparc
079400160503     c                   clear                   wmess
079500160503     c                   clear                   wselzona
079600160503     c                   endif
079700160503     c                   if        wparc1='S' and wparc2='S'
079800160503     c                   clear                   wparc1
079900160503     c                   clear                   wparc2
080000160503     c                   eval      wparc='S'
080100160503     c                   endif
080200160503     c
080300160503    1c                   if        wselzona='S'
080400050511     c                   clear                   xx                4 0
080500160502     c                   clear                   yy                4 0
080600050510     c                   movel     vidfil        alffil            3
080700050511     c                   movel     vidfil        wpo
080800050510     c                   movel(P)  '03'          cod
080900050510     c                   movel(P)  vidfil        key
081000050510     c     ktab          setll     tabel
081100050510     c     ktab2         reade     tabel
081200160502    2c                   dow       not %eof(tabel00f) and  wpo=alffil
081300160502    3c                   if        tblflg=' '
081400050510     c                   movel     tblkey        wpo               3
081500160502    4c                   if        wpo=alffil
081600050511     c                   movel     tbluni        ds03
081700160502
081800160502     c* parcel/messaggerie
081900160503    5c                   if        wparc='S' or wmess='S'
082000160502
082100160503    6c                   if        (wmess='S' and §03tpm='M') or
082200160503     c                             (wparc='S' and §03tpm<>'M')
082300050511     c                   add       1             xx
082400050511     c                   movel     tblkey        w005a             5
082500050511     c                   move      w005a         zone(xx)
082600160502    6c                   endif
082700160502    5c                   endif
082800160502     c* parcel 1° e 2° turno
082900160503    5c                   if        (wparc1='S' or wparc2='S')
083000160503     c                             and §03tpm<>'M'
083100160502
083200160503    6c                   if        (wparc2='S' and §03turno='2') or
083300160503     c                             (wparc1='S' and §03turno<>'2')
083400160503     c                   add       1             yy
083500160502     c                   movel     tblkey        w005a             5
083600160503     c                   move      w005a         zone12(yy)
083700160502    6c                   endif
083800160502    5c                   endif
083900160502     c
084000160502    4c                   endif
084100160502    3c                   endif
084200160502     c
084300050511     c     ktab2         reade     tabel
084400160502    2c                   enddo
084500160502    1c                   endif
084600050511     c
084700160503     c                   clear                   statipo
084800160503     c                   clear                   statipo2
084900160503     c                   if        partpm1='P ' or partpm2='P '
085000050511     c                   eval      statipo=cparcel
085100050511     c                   endif
085200050511     c
085300160503     c                   if        partpm1='M ' or partpm2='M '
085400160503     c                   if        statipo=*blanks
085500050511     c                   eval      statipo=cmessag
085600160503     c                   else
085700160503     c                   eval      statipo2=cmessag
085800160503     c                   endif
085900050511     c                   endif
086000160503     c                   if        partpm1='P1' or partpm2='P1'
086100160503     c                   if        statipo=*blanks
086200160503     c                   eval      statipo=cparc1
086300160503     c                   else
086400160503     c                   eval      statipo2=cparc1
086500160503     c                   endif
086600160503     c                   endif
086700160503     c                   if        partpm1='P2' or partpm2='P2'
086800160503     c                   if        statipo=*blanks
086900160503     c                   eval      statipo=cparc2
087000160503     c                   else
087100160503     c                   eval      statipo2=cparc2
087200160503     c                   endif
087300160503     c                   endif
087400941017     C*
087500100706      * Aggancio tabella VPO recpord VPO
087600100706     c                   Clear                   Dvpo
087700100706     c                   Clear                   dsbs02
087800100706     c                   Eval      T02Mod = 'C'
087900100706     c                   Eval      T02tla = 'L'
088000100706     c                   Eval      T02Sif = knsif
088100100706     c                   Eval      T02Cod = 'VPO'
088200100706     c                   Movel(p)  'VPO'         T02Ke1
088300100706     c                   Call      'TIBS02R'
088400100706     c                   Parm                    Kpjba
088500100706     c                   Parm                    dsbs02
088600100706     c                   If        T02Err = *Blanks
088700100706     c                   Movel     T02Uni        Dvpo
088800100706     c                   EndIf
088900100706     c
089000100706     c     *eur          move      udate8        dataeur
089100100706     c                   movel     dataeur       dataiso
089200161017     c
089300161017     c                   movel     dataiso       dateu             8 0
089400161017
089500100706     c                   if        §vpoggima>0
089600100706     c                   subdur    §vpoggima:*d  dataiso
089700100706     c                   endif
089800100706     c
089900100706     c                   movel     dataiso       datapresIMA       8 0
090000100706
090100911018     C                   ENDSR
090200941017     C**************************************************************************
090300941017     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
090400941017     C**************************************************************************
090500911018     C     ERRORE        BEGSR
090600941017     C*
090700911015     C                   SETON                                        01
090800040603     c
090900040603     c     kar5          chain     fiar501l
091000040603     c                   if        %found(fiar501l)
091100040603     c                   movel     ar5uni        dar5gen
091200040603     c                   else
091300040603     c                   clear                   dar5gen
091400040603     c                   endif
091500911018     C* STAMPA ANOMALIA
091600040428     C                   EXSR      STAMPA
091700081014     c
091800100706     c* non creo anomalia se collo doppio o solo segnalazione
091900161017    0c                   if        not *in12 and not *in16 and not *in11
092000161018     c                             and not *in18
092100911015     C*
092200950707     C* SOLO SE IMA DEFINITIVO  CREO ANOMALIA
092300950608    1C     VIDSN         IFEQ      'S'
092400950810     C* NON DEVO CREARE:
092500950810     C* 1) SE C'E' LA FORZATURA E MANCA LA DATA DI ARRIVO
092600950810     C*    ANCHE SE STAMPO LO STESSO
092700950810     C* 2) SE LA SPUNTA E' STATA TROVATA IN ALTRA CATEGORIA DEL GIORNO
092800960419     C*    3 O 4 NON DISABILITO NEMMENO LA BOLLA
092900960419     C* 3) SE LA SPUNTA E' STATA TROVATA IN ARRIVO CREO L'ANOMLIA
093000950608    2C     ARTDAM        IFGT      0
093100960419   2AC     *IN13         IFEQ      *OFF
093200031219     C     *IN15         OREQ      *ON
093300081014i    C*
093400081104     C     KANM          SETLL     FNANM02L                               35
093500081014    3c                   if        not *in35
093600081014     c* Se anomalia chiusa non in pratica idd e data chiusura <data ima
093700081014     c*  la ricreo
093800081104    4c***                if        anmdch>0 and anmdch<savdfv and
093900081104     c***                          anmcch<>'PR'
094000081104     c***                delete    fnanm000
094100081104     c***                seton                                        35
094200081104    4c***                endif
094300081104    3c***                endif
094400081014     c
094500081104    3C***                if        *in35
094600081104     C*
094700081104     C                   MOVEL     *BLANKS       ANMATB
094800081104     C                   MOVEL     PA5FAS        ANMFSA
094900160503     c*  data salvata del foglio IMA perchè richaino in update
095000100506     C                   Z-ADD     SAVdfv        ANMDAO
095100081104     C                   Z-ADD     15            ANMcaa
095200081104     C                   MOVEL     *BLANKS       ANMCCH
095300081104     C                   MOVEL     VIDFGS        ANMCDU
095400081104     C                   Z-ADD     COMNFV        ANMNFV
095500081104     C                   MOVEL     VIDFGS        ANMFLE
095600081104     C                   MOVE      *BLANKS       ANMFLO
095700081104     C                   MOVEL     *BLANKS       ANMFT1
095800081104     C                   MOVEL     *ZEROS        ANMFL1
095900081104     C                   MOVEL     *BLANKS       ANMFT2
096000081104     C                   MOVEL     *ZEROS        ANMFL2
096100081104     C*
096200081104     C                   MOVEL     §7CTAN        ANMTAN
096300081104     C                   MOVEL     §7CSAN        ANMSAN
096400081104     C                   MOVEL     §7CAIE        ANMAIE
096500081104     C                   Z-ADD     §7CLID        ANMLID
096600081104     C                   MOVE      *ZEROS        ANMDUE
096700081104     C                   MOVE      *ZEROS        ANMNPG
096800081104     C                   MOVE      *ZEROS        ANMFGS
096900081104     C                   Z-ADD     0             ANMDCH
097000081104     C                   MOVEL     ' '           ANMFSC
097100081104     C* CHIUSURA AUTOMATICA
097200081104     C     §7CCHA        IFEQ      'S'
097300100506     C                   Z-ADD     SAVDFV        ANMDCH
097400081104     C                   MOVEL     PA5FAS        ANMFSC
097500081104     C                   END
097600081104     C                   MOVE      *BLANKS       ANMFT3
097700081104     C                   MOVE      *ZEROS        ANMFL3
097800081104     C                   MOVE      *BLANKS       ANMFT4
097900081104     C                   MOVE      999           ANMFL4
098000081104     C                   MOVE      *ZEROS        ANMDTR
098100081104     C*
098200911018     C                   MOVEL     ARTFLS        ANMFLS
098300911018     C                   MOVEL     ARTLNA        ANMLNA
098400911018     C                   MOVEL     ARTNRS        ANMNRS
098500941018     C                   MOVEL     ARTNSC        ANMSCN
098600941018     C                   Z-ADD     ARTAAS        ANMAAS
098700911018     C                   MOVEL     ARTNSP        ANMNSP
098800911018     C                   MOVEL     ARTLNP        ANMLNP
098900970206     C                   MOVEL     ARTNAP        ANMNPS
099000040603     c
099100040603     c* imposto data e ora di creazione
099200050707     C                   TIME                    W0140            14 0
099300050707     C                   MOVE      W0140         G02DAT
099400050707     C                   Z-ADD     *ZERO         G02INV
099500050707     C                   MOVEL     *BLANKS       G02ERR
099600050707     C                   CALL      'XSRDA8'
099700050707     C                   PARM                    WLBDAT
099800050707     C                   Z-ADD     G02INV        WDATA             6 0
099900050707     c**                 time                    wora              6 0
100000050707     c**                 z-add     *date         wdata             6 0
100100050707     C
100200050707     c                   movel     w0140         wora4             4 0
100300040603     c                   movel     wdata         anmflo
100400040603     c                   move      wora4         anmflo
100500040603     c
100600040603     c* imposto se flag di valore
100700040603     c                   movel     §ar5bva       anmft4
100800040603     c
100900040603     c
101000920409     C* ESTERNA
101100950608    4C     §7CAIE        IFEQ      'E'
101200950608     C                   MOVEL     ARTLNP        ANMFL1
101300950810    4C                   ENDIF
101400920409     C*
101500990511     C                   WRITE     FNANM000
101600081014    3C                   endif
101700081014     c
101800000512   2AC                   ENDIF
101900960419    2C                   ENDIF
102000920318     C*
102100000512     C* DISABILITO LA BOLLA SOLO SE HO CREATO ANOMALIA
102200000512     C* NO POSTE SEMPRE, LNP POSTE SOLO SE PREVOSTO
102300950810     C* SOLO SE NON HO TROVATO UN'ALTRA SPUNTA IN DATA DEL FOGLIO IMA
102400960419    2C     *IN10         IFEQ      *OFF
102500960419     C*
102600960419    3C     *IN13         IFEQ      *OFF
102700031219     C     *IN15         OREQ      *ON
102800000512     C*
102900040428    5c     wchain        ifeq      *blanks
103000040428     c     fnarbnrri     chain(e)  fnarb00f
103100040428    6c                   if        %error
103200040428     C* Valorizzo la DS di procedura
103300040428     C                   CLEAR                   TRUL82DS
103400040428     c                   eval      ul82§rrn = fnarbnrri
103500040428     C                   EVAL      UL82§FIL = 'FNARB00F'
103600040428     C                   EVAL      UL82§WIN = 'N'
103700040428     C                   EVAL      UL82§F7  = 'S'
103800040428     C                   EVAL      UL82§num = 5
103900040428     C                   EVAL      UL82§att = 60
104000040428     C                   EVAL      UL82§mss = msgjob
104100040428     C* Effettuo la chiamata al *PGM d utilità
104200040428     C                   CALL(e)   'TRUL82R'
104300040428     C                   PARM                    TRUL82DS
104400040428     C     fnarbnrri     chain     fnarb00f
104500040428    6c                   endif
104600040428     C                   Z-ADD     0             ARBAMA
104700040428     c                   update    fnarbf00
104800040428     c                   move      '1'           wchain
104900040428    5c                   endif
105000930629     C* AGGIORNO IL COLLO CON UNA 'D' DI DISABILITAZIONE
105100941221     C                   MOVEL     'S'           ARTDAB                         'D'=DISABILITATO
105200000512    3C                   ENDIF
105300960419    2C                   ENDIF
105400950810     C*
105500040428     C  N10              UPDATE    FNART000
105600950810    1C                   ENDIF
105700081014    0C                   ENDIF
105800911015     C*
105900911015     C                   ENDSR
106000950810     C**************************************************************************
106100950810     C* ELABORAZIONE DELL'IMA
106200950810     C**************************************************************************
106300950810     C     ELAIMA        BEGSR
106400950810     C*
106500950810     C* N O N   C O N T R O L L O :
106600950810     C*          1) SE NO ABILITAZIONE FORZATA E SENZA DATA ARRIVO
106700990212    1C     SAVAMA        IFNE      3
106800950810     C     ARTDAM        ANDEQ     0
106900950810     C                   SETON                                        34
107000990212    1C                   ENDIF
107100950810     C*
107200950810     C*          2) SE DATA ARRIVO MAGGIORE DATA FOGLIO IMA
107300990212    1C  N34ARTDAM        IFNE      0
107400950810     C     ARTDAM        ANDGT     SAVDFV
107500950810     C                   SETON                                        34
107600990212    1C                   ENDIF
107700950810     C*
107800990212     C*          3) SE COLLO SI TROVA IN DCD E RESO O DISTRUTTO PER
107900990212     C*             AVARIA O COLLO MANCANTE
108000990212     C     KARTD         CHAIN     FNDCD02L                           31
108100990212    1C     *IN31         IFEQ      *OFF
108200990212     C*
108300990303     C     DCDATB        ANDEQ     ' '
108400990212     C* CHAIN TESTATA PER CONTROLLARE LA DATA DI CHIUSURA
108500990212     C     KDCD          CHAIN     FNDCT01L                           32
108600050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
108700050221     c                   If        Not *In32 and dctdt2 > 0
108800050221     c                   Eval      *In32 = *On
108900050221     c                   EndIf
109000990303     C*
109100990303    2C     *IN32         IFEQ      *OFF
109200990303     C* SE E' CHIUSA CONTROLLO SE IL RECORD DEVE ESSERE COMPRESO
109300990303     C*  IN IMA, PER ALCUNI TIPI ANOMALIA
109400990303     C                   MOVEL     'S'           §CHIMA
109500990303    3C     DCTCCH        IFNE      *BLANKS
109600990303     C                   CLEAR                   DSBS02
109700990303     C                   MOVEL     'C'           T02MOD
109800990303     C                   MOVEL     KNSIF         T02SIF
109900990303     C                   MOVEL     'CCH'         T02COD
110000990303     C                   MOVEL     DCTCCH        T02KE1
110100990303     C                   CALL      'TIBS02R'
110200990303     C                   PARM                    KPJBA
110300990303     C                   PARM                    DSBS02
110400990303     C*
110500990303    4C     T02ERR        IFEQ      ' '
110600990303     C                   MOVEL     T02UNI        DCCH
110700990303    4C                   ENDIF
110800990303    3C                   ENDIF
110900990303     C**
111000990303     C** SE NON DEVE ESSERE COMPRESO:
111100990303    3C     §CHIMA        IFNE      'S'
111200990303     C     DCTDCH        OREQ      0
111300990212     C** SE ENTRAMBI APERTI O CHIUSI CON STESSA DATA CONTROLLO IL TIPO
111400990212     C**  ANOMALIA PER DECIDERE SE TENERE O ESCLUDERE IL COLLO
111500990212     C**  NON CONSIDERO LA CAUSALE PAREGGIO CHE SIGNIFICA CHE IL COLLO
111600990212     C*  C'E' SEMPRE
111700990212    4C     DCTDCH        IFEQ      DCDDCH
111800990212     C     DCDDCH        OREQ      0
111900990212     C**
112000990212     C                   CLEAR                   DSBS02
112100990212     C                   MOVEL     'C'           T02MOD
112200990212     C                   MOVEL     KNSIF         T02SIF
112300990212     C                   MOVEL     'TAD'         T02COD
112400990212     C                   MOVEL     DCTTAD        T02KE1
112500990212     C                   CALL      'TIBS02R'
112600990212     C                   PARM                    KPJBA
112700990212     C                   PARM                    DSBS02
112800990212     C**
112900990212    5C     T02ERR        IFEQ      ' '
113000990212     C                   MOVEL     T02UNI        DTAD
113100990212     C                   ELSE
113200990212     C                   MOVEL     'A'           §TARGR
113300990212    5C                   ENDIF
113400990212    C**
113500990212     C* SE MANCANZE
113600990212    5C     §TARGR        IFEQ      'M'
113700990212     C* O AVARIA RESA
113800990212     C     §TARGR        OREQ      'V'
113900990212     C     DCTDSD        ANDEQ     'R'
114000990212     C* O AVARIA DISTRUTTA
114100990212     C     §TARGR        OREQ      'V'
114200990212     C     DCTDSD        ANDEQ     'D'
114300990212     C*                    --> ESCLUDO
114400990212     C                   SETON                                        34
114500990212    5C                   ENDIF
114600990212    4C                   ENDIF
114700990212    3C                   ENDIF
114800990212    2C                   ENDIF
114900990212    1C                   ENDIF
115000101215     c
115100101215     c* Se opzione solo spuntato alla linea di arrivo finale, controllo
115200101215     c                   if        parimaa='S'
115300101215     c                   setoff                                       37
115400101215RM*  C     Kartd         setll     FNBRV07l
115500101215RM*  C     Kartd         reade     FNBRV07l
115600101215     c                   dow       not %eof(fnbrv07l)
115700101215     c     brvfgs        lookup    Lspu                                   37
115800101215     c                   if        *in37
115900101220     c* Se la filiale che spunta è giusta verifico che sia categoria di "arrivo"
116000101220     c                   clear                   dslv53
116100101220     C                   z-add     brvnFV        D53NFV
116200101220     C                   MOVEL     brvNPG        D53NPG
116300101220     C                   MOVEL     brvFGS        D53FGS
116400101220     C                   CALL      'FNLV53R'
116500101220     C                   PARM                    DSLV53
116600101220     C*
116700101220    2C     D53ERR        IFeq      ' '
116800101220     c*
116900101220     c                   if        d53def='S'
117000101220     c                   eval      wnpgarr=%editc(d53npg:'X')+'D'
117100101220     c                   else
117200101220     c                   eval      wnpgarr=%editc(d53npg:'X')+d53spg
117300101220     c                   endif
117400101220     c
117500101220     c                   eval      TROV=%lookup   (wnpgarr: NPGARR)
117600101220     c                   if        TROV>0
117700101215     c                   leave
117800101220     c                   endif
117900101215     c                   endif
118000101220     c                   endif
118100101220     c
118200101220     c                   setoff                                       37
118300101220RM*  C     Kartd         reade     FNBRV07l
118400101215     c
118500101215     c                   enddo
118600101215     c
118700101215     c* se non trovata nessuna spunta, NON elaboro
118800101215     c                   if        not *in37
118900101215     c                   seton                                        34
119000101215     c                   endif
119100101215     c
119200101215     c                   endif
119300990212    C**
119400100706     c                   if        not *in34
119500100706     C                   EXSR      CTRART
119600950810     C*
119700950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
119800100706     C* 16 ON -     TROVATO SEGNACOLLO IN IMA più di xxx volte
119900100706     C   14
120000100706     Cor 16              EXSR      ERRORE
120100100706     c                   endif
120200950810     C*
120300950810     C                   ENDSR
120400950810     C**************************************************************************
120500950810     C* ELABORAZIONE DELL'IMG
120600950810     C**************************************************************************
120700950810     C     ELAIMG        BEGSR
120800950810     C                   EXSR      CTRART
120900950810     C*
121000950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
121100950810     C   14              EXSR      ERRORE
121200950810     C                   ENDSR
121300941017     C**************************************************************************
121400941017     C* STAMPO DISCORDANZA
121500941017     C**************************************************************************
121600900621     C     STAMPA        BEGSR
121700050405     c                   setoff                                       242526
121800941017     C*
121900911015     C* STAMPA TESTATA
122000911015     C   OF              DO
122100911018     C                   SETON                                        05
122200911018     C                   EXCEPT    TES
122300920813     C                   EXCEPT    TES2
122400911018     C                   SETOFF                                       OF
122500911018     C                   END
122600090522     c* Se non trovata la bolla (per colli doppi)
122700090522    1c                   if        NoBolla='S'
122800090522     c                   except    DetNObo
122900161018     C   12              EXCEPT    DETdoppio
123000161018     C   18              EXCEPT    DETeccede
123100090522   x1c                   else
123200911018     C*
123300950810     C     COMSPE        IFNE      ARBSPE
123400911018     C                   SETON                                        05
123500950810     C                   MOVEL     ARBSPE        COMSPE
123600950810     C                   ENDIF
123700911018     C* DATA SPEDIZIONE
123800911018     C   05              DO
123900941018     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
124000941018     C                   MOVE      ARBDSP        G02INV                         AAAA/MM/GG
124100911018     C                   MOVE      *ZEROS        G02DAT
124200911018     C                   MOVEL     3             G02ERR
124300941017     C                   CALL      'XSRDA8'
124400911018     C                   PARM                    WLBDAT
124500941018     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
124600941018     C*
124700941018     C* CODICE PADRONCINO CONSEGNA DECODIFICA
124800021203     C*    ARBPDC        CHAIN     FNAPD01L                           06
124900021205     C                   move      arbpdc        kpdpdr
125000021203     C                   move      'A'           kpdtip
125100021203     C     kfapd         chain     fiapd01L                           06
125200941018     C  N06              MOVEL     APDRSC        DESCPC            9
125300100707     c
125400100707     c* se non c'e' paadroncino consegna scrivo se Fermo deposito
125500100707     c                   if        *in06
125600100707     C                   MOVEL     *BLANKS       DESCPC
125700100707     c                   if        arbffd='S'
125800100707     c                   eval      descpc='FDeposito'
125900100707     c                   endif
126000100707     c                   endif
126100920409     C*
126200911018     C* SEGNACOLLI NON CONSECUTIVI
126300911018     C     ARBFNS        COMP      'S'                                    04
126400911018     C* SERIE>< 0
126500911018     C     ARBNRS        COMP      0                                  0303
126600911018     C                   END
126700920409     C*
126800920708     C* 07 ON - MANCA DATA ARRIVO: SPEDIZIONE FORZATA A 3
126900920708     C     ARTDAM        IFEQ      0
127000920708     C                   SETON                                        07
127100920708     C                   ELSE
127200920708     C                   SETOFF                                       07
127300920409     C* DATA ARRIVO MERCE  SEGNACOLLO
127400941018     C                   MOVE      ARTDAM        G02INV                         AAAA/MM/GG
127500920409     C                   MOVE      *ZEROS        G02DAT
127600920409     C                   MOVEL     3             G02ERR
127700941017     C                   CALL      'XSRDA8'
127800920409     C                   PARM                    WLBDAT
127900941018     C                   Z-ADD     G02DAT        STADAM                         GG/MM/AAAA
128000920708     C                   END
128100040603     C* MITTENTE E DESTINATARIO DI 17 E 20
128200040603     C                   MOVEL     ARBRSM        STARSM           17
128300920707     C                   MOVEL     ARBRSD        STARSD           20
128400040603     c* collo di valore
128500040603     c                   movel     §ar5bva       stabva            1
128600910522     C*
128700900622     C* STAMPO DETTAGLIO
128800100706     C                   EXCEPT    DET
128900100707     c                   select
129000100707     c                   when      *in12
129100100706     C                   EXCEPT    DETdoppio
129200100707     c                   when      *in16
129300100707     C                   EXCEPT    DETspuIMA
129400161018     c                   when      *in18
129500161018     C                   EXCEPT    DETeccede
129600100707     c                   other
129700100706     C                   EXCEPT    DETerr
129800100707     c                   endsl
129900070418     c
130000070418     c* se in distinta , stampo
130100090522    2c                   if        not *in12
130200081014
130300100706    3c                   if        *in05 and arbndc>0 and not *in16
130400070418     C                   MOVE      ARbddc        G02INV                         AAAA/MM/GG
130500070418     C                   MOVE      *ZEROS        G02DAT
130600070418     C                   MOVEL     3             G02ERR
130700070418     C                   CALL      'XSRDA8'
130800070418     C                   PARM                    WLBDAT
130900070418     C                   Z-ADD     G02DAT        STAddc            8 0          GG/MM/AAAA
131000070418     C                   EXCEPT    DETndc
131100090522    3c                   endif
131200050405     c
131300161017     c* Se c'e' ubicazione collo, per 05 on lo stampo
131400050405     c                   if        *in05 and §ar5zng<>*blanks
131500050405     c                   seton                                        24
131600050405     c                   endif
131700050405     c
131800100707     C                   SETOFF                                       05
131900050405     c
132000990212     C* STAMPO TUTTE LE C.A. PRESENTI SULLA SPEDIZIONE
132100040806      * Sostituisco la lettura del file FNDTC02 con
132200040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
132300040806      * legate alla spedizione che passo
132400040806      *
132500040806     c                   clear                   fidn12ds
132600040806     c                   eval      i12aas = arbaas
132700040806     c                   eval      i12lnp = arblnp
132800040806     c                   eval      i12nrs = arbnrs
132900040806     c                   eval      i12nsp = arbnsp
133000040806     c                   eval      i12fel = 'E'
133100040806     c                   eval      i12fan = 'E'
133200040806      *
133300040806     c                   call      'FIDN12R'
133400040806     c                   parm                    fidn12ds
133500040806      *
133600040806      * se non ci sono errori
133700090522    3c                   if        o12err = ' '
133800040806      * se numero di CA trovate maggiore di zero
133900090522    4c                   if        o12nca > 0
134000100706     c
134100050405     c                   seton                                        25
134200040806     c
134300040806     c                   z-add     *zeros        ii                2 0
134400100706    6c     1             do        o12nca        ii
134500040806     c                   movea     skkey(ii)     dskey
134600040806     C                   Z-ADD     dsfil         dctfil
134700040806     C                   Z-ADD     dsnca         dctnca
134800040806     C                   Z-ADD     skdca(ii)     dctdca
134900040806     C                   Z-ADD     skdch(ii)     dctdch
135000040806     C                   movea     skcch(ii)     dctcch
135100040806     c                   exsr      preca
135200050405     c                   except    detca
135300050405     c
135400050405     c                   setoff                                       24
135500100706    6C                   ENDDO
135600040806      *
135700100706    4c                   endif
135800090522    3c                   endif
135900050405     c
136000050405     c* Stampo ubicazione collo se non ancora stampata
136100090522    3c                   if        *in24
136200050405     c                   except    detca
136300090522     c                   setoff                                       24
136400090522    3c                   endif
136500090522     c                   setoff                                       25
136600081014     c
136700090522    2c                   endif
136800090522    1c                   endif
136900900622     C*
137000900622     C                   ENDSR
137100990212     C**************************************************************************
137200990212     C* PREPARO I DATI DELLA C.A.
137300990212     C**************************************************************************
137400990212     C     PRECA         BEGSR
137500990212     C                   SETOFF                                       26
137600990303     C                   CLEAR                   STACCH
137700990212     C* DATA APERTURA C.A.
137800990212     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
137900990212     C                   Z-ADD     *ZERO         G02DAT
138000990212     C                   MOVEL     '3'           G02ERR
138100990212     C                   CALL      'XSRDA8'
138200990212     C                   PARM                    WLBDAT
138300990212     C                   MOVE      G02DAT        W0020             2 0
138400990212     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
138500990212     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
138600990303     C** DATA CHIUSURA C.A.
138700990212     C                   CLEAR                   STACHC
138800990303    1C     DCTDCH        IFGT      0
138900990212     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
139000990212     C                   Z-ADD     *ZERO         G02DAT
139100990212     C                   MOVEL     '3'           G02ERR
139200990212     C                   CALL      'XSRDA8'
139300990212     C                   PARM                    WLBDAT
139400990212     C                   MOVE      G02DAT        W0020             2 0
139500990212     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
139600990212     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
139700990212     C                   SETON                                        26
139800990303     C* CAUSALE CHIUSURA
139900990303    2C     DCTCCH        IFNE      *BLANKS
140000990303     C                   CLEAR                   DCCH
140100990303     C                   CLEAR                   DSBS02
140200990303     C                   MOVEL     'C'           T02MOD
140300990303     C                   MOVEL     KNSIF         T02SIF
140400990303     C                   MOVEL     'CCH'         T02COD
140500990303     C                   MOVEL     DCTCCH        T02KE1
140600990303     C                   CALL      'TIBS02R'
140700990303     C                   PARM                    KPJBA
140800990303     C                   PARM                    DSBS02
140900990303     C*
141000990303    3C     T02ERR        IFEQ      ' '
141100990303     C                   MOVEL     T02UNI        DCCH
141200990303    3C                   ENDIF
141300990303     C                   MOVEL     §CHDES        STACCH           12
141400990303    2C                   ENDIF
141500990303    1C                   ENDIF
141600990212     C                   ENDSR
141700941017     C**************************************************************************
141800161018     C* RIEPILOGO SPUNTE PER PISTOLA e controllo se colli doppi o eccedenti
141900941017     C**************************************************************************
142000920813     C     RIEPIL        BEGSR
142100081014     C                   SETON                                        12
142200941017     C*
142300920813     C                   Z-ADD     1             C
142400920813     C* CONTO SPUNTE PER PISTOLA
142500070118     C     KBRV          SETLL     FNBRV01L
142600070118     C     KBRV          READE     FNBRV01L                               30
142700920813     C*
142800161018    1C     *IN30         DOWEQ     *OFF
142900161018    2c                   if        brvtsu<>*blanks
143000081014     c                   clear                   fnlrv7ds
143100081014     c                   eval      ilrv7fls=brvlnp
143200081014     c                   eval      ilrv7lna=brvlna
143300081014     c                   eval      ilrv7nrs=brvnrs
143400081014     c                   eval      ilrv7nsc=brvnsc
143500081014     c                   eval      kpjbu= fnlrv7ds
143600081014     c                   call      'FNLRV7R'
143700081014     c                   parm                    kpjba
143800081014     c
143900081014     c                   eval      fnlrv7ds=kpjbu
144000161018    3c                   if        olrv7nric>=3 and olrv7part='S'
144100081014     c* Se ho già stampato qualche cosa, lascio la prima volta una
144200081014     c*  riga vuota
144300161018    4c                   if        *in01 and not *inOF and uno=' '
144400081014     c                   except    DETvuota
144500161018    4c                   endif
144600161018     c                   exsr      Cerca_bolla
144700081014     c
144800081014     c                   exsr      ERRORE
144900081014     c                   eval      UNO='S'
145000161018    3c                   endif
145100081014     c
145200161018    2c                   endif
145300161018     c
145400161018     c* solo per ora per C P G stampo i colli eccedenti
145500161018     c   11              exsr      Eccedenti
145600081014     c
145700941019     C                   Z-ADD     1             B                 2 0
145800920813     C                   MOVEL     BRVNPS        COMNPS            2
145900920813     C     COMNPS        LOOKUP    NPS(B)                                 31
146000920813     C  N31              MOVEL     BRVNPS        NPS(C)
146100920813     C  N31              Z-ADD     C             B
146200941019     C  N31              ADD       1             C                 2 0
146300920813     C*
146400920813     C                   ADD       1             CPS(B)
146500920813     C*
146600070118     C     KBRV          READE     FNBRV01L                               30
146700161018    1C                   ENDDO
146800081014     C
146900081014     C                   SETOFF                                       12
147000161018     c
147100081014     C*
147200081014     C* SE NESSUNA ANOMALIA STAMPO TUTTO O.K.
147300081014     C  N01              EXCEPT    TES
147400081014     C  N01              EXCEPT    OK
147500081014     C  N01              SETOFF                                       OF
147600920813     C**
147700920813     C* SOMMO TUTTI I COLLI
147800920813     C                   XFOOT     CPS           TOTSPU            9 0
147900920813     C**
148000920813     C* STAMPA SPUNTE
148100920813     C* TESTATA
148200920813     C   OF              EXCEPT    TES
148300920813     C   OF              SETOFF                                       OF
148400920813     C*
148500920813     C                   EXCEPT    TOT1
148600920813     C*
148700920813     C                   Z-ADD     1             C
148800920813     C     CPS(C)        DOWGT     0
148900920813     C   OF              DO
149000920813     C                   EXCEPT    TES
149100920813     C                   EXCEPT    TOT1
149200920813     C                   SETOFF                                       OF
149300920813     C                   END
149400920813     C*
149500920813     C                   EXCEPT    TOT2
149600920813     C                   ADD       1             C
149700920813     C                   END
149800920813     C*
149900920813     C                   EXCEPT    TOT3
150000941017     C*
150100920813     C                   ENDSR
150200161018     C**************************************************************************
150300161018     C*   da spunta cerco la relativa bolla
150400161018     C**************************************************************************
150500161018     c     Cerca_bolla   BEGSR
150600161018     c                   clear                   nobolla
150700161018     c* chain la bolla e stampo collo doppio
150800161018     c     kartB         chain     fnart27l
150900161018    2c                   if        %found(fnart27l)
151000161018     c     karb          chain     fnarb01l
151100161018   x2c                   else
151200161018     c     kartB         chain     fnblt27l
151300161018    3c                   if        %found(fnart27l)
151400161018     c     karb          chain     fnblp01l
151500161018   x3c                   else
151600161018     c                   eval      Nobolla='S'
151700161018    3c                   endif
151800161018    2c                   endif
151900161018     c                   ENDSR
152000161018     C**************************************************************************
152100161018     C*   Ricerca colli eccedenti
152200161018     C**************************************************************************
152300161018     C     Eccedenti     BEGSR
152400161018     c                   setoff                                       12
152500161018     c                   seton                                        18
152600161018     c                   clear                   ecce_descr
152700161018     c
152800161018     c                   exsr      Cerca_bolla
152900161018     c* Se manca bolla --> sempre eccedente
153000161018    1c                   if        nobolla='S'
153100161018    4c                   if        *in01 and not *inOF and uno=' '
153200161018     c                   except    DETvuota
153300161018    4c                   endif
153400161018     c                   exsr      ERRORE
153500161018     c                   eval      UNO='S'
153600161018   x1c                   else
153700161018     c* Richiamo driver per verificare se la bolla è a mag giac oggi
153800161018     c                   clear                   fnlgi1ds
153900161018     c                   eval      ILGI1AAS=arbaas
154000161018     c                   eval      ILGI1lnp=arblnp
154100161018     c                   eval      ILGI1nrs=arbnrs
154200161018     c                   eval      ILGI1nsp=arbnsp
154300161018    3c                   select
154400161018     c                   when      arbpkc>0 and arbncp=arbncl
154500161018     c                   eval      ilgi1pkf=arbpkc
154600161018     c                   when      arbpkc>0 and arbncp<arbncl
154700161018     c                             and arbpkc>arbpkf
154800161018     c                   eval      ilgi1pkf=arbpkc
154900161018     c                   other
155000161018     c                   eval      ilgi1pkf=arbpkf
155100161018    3c                   endsl
155200161018     c                   movel     fnlgi1ds      kpjbu
155300161018     c                   call      'FNLGI1R'
155400161018     c                   parm                    kpjba
155500161018     c                   movel     kpjbu         fnlgi1ds
155600161018    2c                   if        olgi1AMG<>'Y'
155700161018     c*
155800161018     c                   select
155900161018     c                   when      olgi1inmg<>' ' or olgi1AMG<>' '
156000161018     c                   eval      ecce_descr=' -già in Giacenza'
156100161018     c                   when      OLGI1OUTMG<>' '
156200161018     c                   eval      ecce_descr=' -non più giacente'
156300161018     c                   when      arbndc>0
156400161018     c                   eval      ecce_descr=' -di bolla in distinta '
156500161018     c                   other
156600161018     c                   eval      ecce_descr=' -di bolla in IMA'
156700161018     c                   endsl
156800161018
156900161018    4c                   if        *in01 and not *inOF and uno=' '
157000161018     c                   except    DETvuota
157100161018    4c                   endif
157200161018
157300161018     c                   exsr      ERRORE
157400161018     c                   eval      UNO='S'
157500161018    2c                   endif
157600161018
157700161018    1c                   endif
157800161018     c
157900161018     c                   setoff                                       18
158000161018     c                   seton                                        12
158100161018     C                   ENDSR
158200161017     C**************************************************************************
158300161017     C*   Elaborazione per C P G
158400161017     C**************************************************************************
158500161017     C     LeggiGIAC     BEGSR
158600161017      * questo pgm produce soltanto un tabulato di quadratura e non una lista di giacenza
158700161017      *
158800161017      * il programma prevede la stessa lettura di giacenze del pgm FNLG86R per cui
158900161017      *  si devono modoficare entrambi se cambiano le logiche
159000161017     C*
159100161017     c                   z-add     10            fas
159200161017      * lettura giacenze
159300161017      *
159400161017     c     kgcp          setll     tigcp26l
159500161017    1c                   do        *hival
159600161017     c     klna          reade     tigcp26l                             99
159700161017      *allocazione
159800161017    2c                   if        *in99
159900161017     c     klna          reade(n)  tigcp26l
160000161017    3c                   if        %eof
160100161017     c                   leave
160200161017    3c                   end
160300161017     c                   iter
160400161017    2c                   endif
160500161017      *
160600161017    2c                   if        %eof
160700161017     c                   leave
160800161017    2c                   end
160900161017      * la fase non può essere maggiore di 30
161000161017    2c                   if        gcpfas >= 30
161100161017     c                   leave
161200161017    2c                   end
161300161017
161400161017      *solo progressivo 0
161500161017    2c                   if        gcpfrg <> 0
161600161017     c                   iter
161700161017    2c                   end
161800161017     c*
161900161017     c                   clear                   fnlgi1ds
162000161017     c                   eval      ilgi1agc=gcpagc
162100161017     c                   eval      ilgi1fgc=gcpfgc
162200161017     c                   eval      ilgi1ngc=gcpngc
162300161017     c                   movel     fnlgi1ds      kpjbu
162400161017     c                   call      'FNLGI1R'
162500161017     c                   parm                    kpjba
162600161017     c                   parm                    tigcp00ds
162700161017     c                   movel     kpjbu         fnlgi1ds
162800161017
162900161017      *merce da portare in magazzino
163000161017    2c                   if        olgi1amg='S' or olgi1amg='Y'
163100161017     c* Verifico se il collo è stato spuntanto nel foglio indicato
163200161017      *
163300161017     C* PRELEVO DATI BOLLA
163400161017     C     KARB_g        CHAIN     FNARB01L
163500161017    3c                   if        %found(fnarb01l)
163600161017     C* LEGGO DETTAGLIO
163700161017     C     KART          SETLL     FNART01L
163800161017     C     KART          READE(n)  FNART01L                               31
163900161017     C*
164000161017    4C     *IN31         DOWEQ     *OFF
164100161017     C                   SETOFF                                       34
164200161017     C*
164300161017     C* ESCLUDO  SE IL COLLO E' CONSEGNATO
164400161017    5C     ARTDCM        IFEQ      0
164500161017     C                   EXSR      CTRART
164600161017     C* 14 ON - NON TROVATO SEGNACOLLO IN CPG
164700161017     C   14              EXSR      ERRORE
164800161017    5C                   ENDIF
164900161017     C*
165000161017     C     KART          READE     FNART01L                               31
165100161017    4C                   ENDDO
165200161017     C*
165300161017     C* AGGIORNO DATA DI STAMPA SE NON È  "Y"
165400161017    4C                   IF        olgi1amg='S'
165500161017     C                   MOVE      dateu         GCPDLMa
165600161017     C                   EXCEPT    AGG5
165700161017    4C                   ENDIF
165800161017
165900161017    3C                   ENDIF
166000161017     C*
166100161017    2C                   ENDif
166200161017     C*
166300161017    1c                   enddo
166400161017     c*
166500161017     C                   ENDSR
166600941017     C**************************************************************************
166700941017     C*   R O U T I N E     I N I Z I A L E
166800941017     C**************************************************************************
166900941017     C     *INZSR        BEGSR
167000941017     C*
167100161017     C* DEFINIZIONE CHIAVI
167200161017     C     KGCp          KLIST
167300161017     C                   KFLD                    klna
167400161017     C                   KFLD                    FAS               3 0
167500000126     C     KBRV          KLIST
167600000126     C                   KFLD                    IMANPG
167700000126     C                   KFLD                    WNFV
167800000126     C                   KFLD                    VIDFGS
167900070118     C     KBRV1         KLIST                                                  FNBRV01L
168000941019     C                   KFLD                    IMANPG                         ..CATEGORIA
168100000126     C                   KFLD                    WNFV                           ..NR. FOGLIO
168200950316RM*  C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
168300941019     C                   KFLD                    ARTFLS                         ..LNP
168400941018     C                   KFLD                    ARTLNA                         ..LNA
168500941018     C                   KFLD                    ARTNRS                         ..SERIE
168600941018     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
168700070418     C     KBRV4         KLIST                                                  FNBRV01L
168800070418     C                   KFLD                    CONNPG                         ..CATEGORIA
168900070418     C                   KFLD                    ARTFLS                         ..LNP
169000070418     C                   KFLD                    ARTLNA                         ..LNA
169100070418     C                   KFLD                    ARTNRS                         ..SERIE
169200070418     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
169300070418     C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
169400990212RM*  C     KARTD         KLIST
169500990212RM*  C                   KFLD                    ARTFLS
169600990212RM*  C                   KFLD                    ARTLNA
169700990212RM*  C                   KFLD                    ARTNRS
169800990212RM*  C                   KFLD                    ARTNSC
169900081014     C     KARTb         KLIST
170000081014     C                   KFLD                    brvlnp
170100081014     C                   KFLD                    brvlnA
170200081014     C                   KFLD                    brvNRS
170300081014     C                   KFLD                    brvNSC
170400941017     C     KFVV          KLIST                                                  *
170500941019     C                   KFLD                    IMANPG                         *
170600941017     C                   KFLD                    COMNFV                         *
170700941028     C                   KFLD                    VIDFGS                         *
170800950316RM*  C     KFVV2         KLIST
170900950316RM*  C                   KFLD                    BRVNPG
171000000126RM*  C                   KFLD                    WNFV5
171100950316RM*  C                   KFLD                    BRVFGS
171200941017     C     KANM          KLIST                                                  FNANM02L
171300941017     C                   KFLD                    ARTFLS                         *
171400941017     C                   KFLD                    ARTLNA                         *
171500941017     C                   KFLD                    ARTNRS                         *
171600941018     C                   KFLD                    ARTNSC                         *
171700941017     C                   KFLD                    ANMCAA                         *
171800941017     C     KART          KLIST                                                  *
171900941017     C                   KFLD                    ARBAAS                         *
172000941017     C                   KFLD                    ARBLNP                         *
172100941017     C                   KFLD                    ARBNRS                         *
172200941017     C                   KFLD                    ARBNSP                         *
172300081014     C     KARB          KLIST                                                  *
172400081014     C                   KFLD                    ARtAAS                         *
172500081014     C                   KFLD                    ARtLNP                         *
172600081014     C                   KFLD                    ARtNRS                         *
172700081014     C                   KFLD                    ARtNSP                         *
172800040603     C     Kar5          KLIST                                                  *
172900040603     C                   KFLD                    ARBAAS                         *
173000040603     C                   KFLD                    ARBLNP                         *
173100040603     C                   KFLD                    ARBNRS                         *
173200040603     C                   KFLD                    ARBNSP                         *
173300040603     C                   KFLD                    ktrd                           *
173400970721     C     KGCA          KLIST                                                  *
173500970721     C                   KFLD                    ARBAAS                         *
173600970721     C                   KFLD                    ARBLNP                         *
173700970721     C                   KFLD                    ARBNRS                         *
173800970721     C                   KFLD                    ARBNSP                         *
173900990212     C                   KFLD                    KFRG
174000950810     C     KSAV          KLIST                                                  *
174100950810     C                   KFLD                    SAVAAS                         *
174200950810     C                   KFLD                    SAVLNP                         *
174300950810     C                   KFLD                    SAVNRS                         *
174400950810     C                   KFLD                    SAVNSP                         *
174500941017     C     KLBL          KLIST                                                  *
174600941017     C                   KFLD                    LBLAAN                         *
174700941017     C                   KFLD                    LBLLPN                         *
174800941017     C                   KFLD                    LBLNRN                         *
174900941017     C                   KFLD                    LBLNSN                         *
175000161017     C     KARB_G        KLIST
175100161017     C                   KFLD                    GCPAASa
175200161017     C                   KFLD                    GCPLNPa
175300161017     C                   KFLD                    GCPNRSa
175400161017     C                   KFLD                    GCPNSPa
175500941017     C     KTAB          KLIST
175600941017     C                   KFLD                    CODUT
175700941017     C                   KFLD                    COD
175800941017     C                   KFLD                    KEY
175900050510     C     KTAB2         KLIST
176000050510     C                   KFLD                    CODUT
176100050510     C                   KFLD                    COD
176200990212     C     KDCD          KLIST
176300990212     C                   KFLD                    DCDAAC                         *
176400990212     C                   KFLD                    DCDFIL                         *
176500990212     C                   KFLD                    DCDNCA                         *
176600021203
176700021203     C     Kfapd         klist
176800021203     C                   kfld                    kpdtip
176900021203     C                   kfld                    kpdpdr
177000941017     C*
177100970721     C     *LIKE         DEFINE    GCPFRG        KFRG
177200970721     C                   MOVEL     0             KFRG
177300941017     C     *LIKE         DEFINE    TBLCOD        COD
177400941017     C     *LIKE         DEFINE    TBLKEY        KEY
177500941019     C     *LIKE         DEFINE    BRVNPG        IMANPG                         CATEGORIA FOGLIO
177600070418     C     *LIKE         DEFINE    BRVNPG        conNPG                         CATEGORIA FOGLIO
177700031218     C     *LIKE         DEFINE    BRVNFV        WNFV
177800031218     C     *LIKE         DEFINE    FVVNFV        WNFV5
177900941018     C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO VIAGGIO
178000941018     C     *LIKE         DEFINE    ARTDAM        STADAM                         DATA ARRIVO MERCE
178100941018     C     *LIKE         DEFINE    ARBSPE        COMSPE
178200941019     C     *LIKE         DEFINE    ARBLNA        KLNA                           LINEA ARRIVO
178300031218     C     *LIKE         DEFINE    FVVFGS        SAVFGS
178400950317RM*  C     *LIKE         DEFINE    FVVDFV        SAVDFV
178500941017     C*
178600101220      /free
178700101220       // Carico le categorie con ambito Arrivo
178800101220       xx=2   ;
178900101220       // Alla posizine 1 metto al defluenza
179000101220       npgarr(1)='1D'  ;
179100101220
179200101220       setll   (1:'7N') tabel00f   ;
179300101220       reade   (1:'7N') tabel00f   ;
179400101220
179500101220       dow not %eof(tabel00f)      ;
179600101220       if tblflg=' '               ;
179700101220       ds7n=tbluni                 ;
179800101220
179900101220       if §7ntbo<>' '              ;
180000101220
180100101220          if §7ntbo='A'               ;
180200101220          npgArr(xx)=tblkey;
180300101220          xx=xx+1  ;
180400101220          endif  ;
180500101220       else   ;
180600101220
180700101220         if  §7ntb1='A'                         ;
180800101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc1   ;
180900101220         xx=xx+1  ;
181000101220         endif                                  ;
181100101220         if  §7ntb2='A'                         ;
181200101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc2   ;
181300101220         xx=xx+1  ;
181400101220         endif                                  ;
181500101220         if  §7ntb3='A'                         ;
181600101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc3   ;
181700101220         xx=xx+1  ;
181800101220         endif                                  ;
181900101220         if  §7ntb4='A'                         ;
182000101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc4   ;
182100101220         xx=xx+1  ;
182200101220         endif                                  ;
182300101220         if  §7ntb5='A'                         ;
182400101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc5   ;
182500101220         xx=xx+1  ;
182600101220         endif                                  ;
182700101220         if  §7ntb6='A'                         ;
182800101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc6   ;
182900101220         xx=xx+1  ;
183000101220         endif                                  ;
183100101220
183200101220       endif                       ;
183300101220       endif                       ;
183400101220
183500101220       reade   (1:'7N') tabel00f   ;
183600101220       enddo                       ;
183700101220      /end-free
183800101220
183900941017     C                   ENDSR
184000101202
184100101202      *---------------------------------------------------------------*
184200101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
184300101202      *---------------------------------------------------------------*
184400101202     c     sr_DatiJob    BEGSR
184500101202      *
184600101202     c     *dtaara       define    §azute        AZUTEds
184700101202     c     *dtaara       define    §datiute      dDatiUte
184800101202      *
184900101202     c                   in(E)     *dtaara
185000101202     c                   IF        %Error or RSUT = *blanks
185100101202     c                   clear                   TIBS34ds
185200101202     c                   call      'TIBS34R'
185300101202     c                   parm                    TIBS34ds
185400101202     c                   in        *dtaara
185500101202     c                   ENDIF
185600101202      *
185700101202     c                   ENDSR
185800941017     C**************************************************************************
185900161017     Otigcp026  E            AGG5
186000161017     O                       GCPDLMa
186100950316RM*  OPRTF198   E            TES              02
186200911015     O                       STA(1)              66
186300950810     O              N10      STA(2)             132
186400161017     O               10N11   STA(18)            132
186500161017     O               10 11   STA(22)            132
186600950320     O                       STA(17)            198
186700900626     O                       RSUT                20
186800960927     O                       UDATE8             181 '  /  /    '
186900950320     O                       PAGE          Z    198
187000920813     O          E            TES         1
187100160503     o                       statipo             67
187200160503     o                       statipo2           130
187300050511     O                       UTIME              179 '  :  :  '
187400931228     O               02      STA(4)              70
187500931228     O          E    02      TES         0  0
187600931228     O                       STA(4)              70
187700931228     O          E            TES         2
187800911015     O                       STA(3)              66
187900931228     O                       STA(16)            132
188000060508     O                       STA(20)            198
188100161018     O              n11                          13 'I.M. .'
188200161018     O              n11      VIDTIN              12
188300161018     O               11                          13 'C.P.G.'
188400060508RM*  O                       SAVFGS              21
188500060508     O                       DESCDU              42
188600920813     O                       COMNFV        Z     56
188700950317     O                       STADFV        8     73
188800960927     O                       VIDFIL             116
188900960927     O                       DESFIL             137
189000060508     O                       stalna             177
189100060508     O                       DESlna             198
189200070418     O          E   n10      TES         1
189300070418     O                       STA(21)            198
189400070418     O                       stamdc             188
189500101215     O                       staimaa            146
189600920813     O          E            TES2        2
189700911018     O                       STA(5)              66
189800911018     O                       STA(8)             132
189900040621     O                       STA(19)            198
190000920813     O          E            TES2        1  1
190100911018     O                       STA(6)              66
190200911018     O                       STA(9)             132
190300911015     O*
190400090522     O          E            DETnoBO     1
190500090522     O                       brvLNP        Z      8
190600090522     O                       brvNRS        Z     11
190700090522     O                       brvLNA        Z     29
190800090522     O                       brvZNC              87
190900090522     O                       brvnps             122
191000090522     O                       brvNSC        Z    131
191100090522     O
191200090522     O          E            DET         1
191300090522     O                  05   ARBAAS        Z
191400090522     O                  05   ARBLNP        Z   +  1
191500090522     O                  05 03ARBNRS        Z   +  1
191600090522     O                  05 03                  +  0 '-'
191700090522     O                  05   ARBNSP        Z   +  0
191800090522     O                  05   STADSP            +  1 '  /  '
191900090522     O                  05   ARBLNA        Z   +  1
192000090522     O                  05   STARSM            +  1
192100090522     O                  05   STARSD            +  1
192200090522     O                  05N04ARBNCD        Z   +  1
192300090522     O                  05N04                  +  0 '/'
192400090522     O                  05N04ARBNCA        Z   +  0
192500090522     O                  05 04                    84 'NON SEQUENZIALI'
192600090522     O                  05   ARBZNC            +  1
192700090522     O                  05   stabva            +  0
192800090522     O                  05   ARBTMC            +  1
192900090522     O                  05N06ARBPDC        Z   +  1
193000090522     O                  05N06                  +  0 '-'
193100100707     O                  05   DESCPC            +  0
193200090522     O              N07      STADAM            +  1 '  /  /    '
193300090522     O              N07      ARTNAP            +  1
193400090522     O              N07      ARTNSC        Z   +  2
193500090522     O               07                         131 'ABILIT.MERCE FORZATA'
193600081014     O* ERRORI:
193700950411     O*
193800081014     O          E            DETERR      0  0
193900031219     O                    n13                   155 'NON SPUNTATO IN DATA '
194000031219     O                    n13STADFV        8   +  1
194100950316     O*
194200040621     O                     13brvFGS             137
194300040621     O                     13                   138 '-'
194400040621     O                     13ST2FGS             146
194500040621     O                     13                   149 'IN'
194600070118     O                     13ST2CAT             153
194700070118     O                     13brvNPG             155
194800070118     O                     13                   156 '-'
194900070118     O                     13brvNFV        Z    162
195000070118     O                     13                   165 'IL'
195100070118     O                     13ST2DFV             171 '  /  '
195200070118     O                     13                   175 'ORE'
195300070118     O                     13brvHMS             184 '  :  :  '
195400960523     O                     13STADSF             198
195500070418     O          E            DETndc      1
195600070418     O                                           87 'In distinta:'
195700070418     O                       ARBndc        2  +   1
195800070418     O                                        +   2 'del'
195900070418     O                       staddc            +  1 '  /  /    '
196000081014     o* Collo doppio
196100081014     O          E            DETdoppio   0  0
196200161018     O                                          153 'COLLO   D O P P I O'
196300161018     o* Collo eccedente
196400161018     O          E            DETeccede   0  0
196500161018     O                                          153 'COLLO   E C C E D E'
196600161018     O                                          159 ' N T E'
196700161018     O                       ecce_descr         185
196800100706     o* Collo presente da XX giorni in IMA
196900100706     O          E            DETspuIMA   0  0
197000100707     O                                          160 '   COLLO SPUNTATO PER '
197100100707     O                                        +   1 'ALMENO '
197200100706     O                       §VPOGGIMA     2  +   1
197300100707     O                                        +   1 'GIORNI in I.M.A.'
197400081014     o* riga vuota
197500081014     O          E            DETvuota    2
197600081014     O                                          157 ' '
197700050405     O* STAMPO NUMERO C.A.e ubicazione collo
197800990212     O          E            DETCA       1
197900050405     O               24                          87 'Ubicazione collo:'
198000050405     O               24      §ar5zng           +  2
198100050405     O               25                         138 'C.A.'
198200050405     O               25      DCTFIL            +  1
198300050405     O               25      DCTNCA        Z   +  1
198400050405     O               25                        +  1 'del'
198500050405     O               25      STADCA            +  1 '  /  /  '
198600990212     O                     26                  +  1 'chiusa il'
198700990212     O                     26STACHC            +  1 '  /  /  '
198800990303     O                     26DCTCCH            +  1
198900990303     O                     26                  +  0 '-'
199000990303     O                     26STACCH            +  0
199100920813     O          E            OK          3
199200920813     O                       STA(11)             66
199300920813     O          E            OK          0  0
199400920813     O                       STA(11)             66
199500920813     O*
199600920813     O          E            TOT1        3  1
199700920813     O                       STA(13)             66
199800920813     O          E            TOT2           1
199900920813     O                       NPS(C)              33
200000920813     O                       CPS(C)        2     58
200100920813     O          E            TOT3           1
200200920813     O                       STA(14)             66
200300920813     O          E            TOT3           1
200400920813     O                       STA(15)             66
200500920813     O                       TOTSPU        2     58
200600920813     O*
200700920813     O          E            TOT3        3
200800920813     O                       STA(12)             66
200900920813     O          E            TOT3        0  0
201000920813     O                       STA(12)             66
201100941017     O**************************************************************************
201200950316** STA
201300060508XXXXXXXXXXXXXXXXXXXX                                                 1
201400950320 ** I.M.A. : INVENTARIO MAGAZZINO ARRIVI **                          2
201500161018FOGLIO        di  XXX-XXXXXXXXXXXXXXXXXXXX    N.ro XXXXX   del XX/   3
201600931228                                       *** D E F I N I T I V O ***   4
201700040603    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO        5
201800941019ANNO LNP SER NUMERO  DATA ARR                                        6
201900040603XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx  7
202000040603     SEGNACOLLI    ZON ULT  AUTOTRASPORT.   A R R I V O SEGNACOLLO   8
202100040603      DAL /   AL   ARR CONS   CONSEGNA       DATA   PIST MANCANTE    9
202200040603XX XXXXXXX/XXXXXXX XXV X XXXXXXX-XXXXXXXXX XX/XX/XXXX XX  XXXXXXX   10
202300920813             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
202400920813 **  FINE  STAMPA  **
202500920813   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI      13
202600920813                        ---------------------------------------     14
202700920813               TOTALE                                               15
202800060508XX/XXXX           Punto Operativo in Gestione: XXX-xxxxxxxxxxxxxxx  16
202900950320                         FNLR31R                          PAG.XXXX  17
203000950810 ** I.M.G. : INVENTARIO MAGAZZINO GIACENZE **                       18
203100040621 S  P  U  N  T  A  T  O      D  A                                   19
203200060508xxxxx           Linea Arrivo Selezionata: XXX-xxxxxxxxxxxxxxxxxxxx  20
203300070418                Elabora anche spedizioni in distinta: XX            21
203400161017 ** C.P.G. : QUADRATURA COLLI A MAGAZZINO GIACENZE **               22
203500161017**
203600950316OVRPRTF FILE(PRTF198) USRDTA('I.M.A.')
203700161017**
203800161017DLYJOB DLY(01)
