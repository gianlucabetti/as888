000100040603     H DECEDIT('0,') DATEDIT(*ymD.)
000200950711     H* FNLR31R *-----------------------------------------------------*
000300930629     H*        - INVENTARIO MAGAZZINO  ARRIVI  I.M.A. -     ESEGUE    *
000400930629     H*---------------------------------------------------------------*
000500911015     FTABEL00F  IF   E           K DISK
000600911015     FAZORG01L  IF   E           K DISK
000700050331     FTigcp21L  IF   E           K DISK
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
003000070418     D STA             S             66    DIM(21) CTDATA PERRCD(1)             SKIERE STAMPA
003100101215     D L6              S              3  0 DIM(30)                              FIL da elaborare
003200101215     D Lspu            S              3  0 DIM(30)                              FIL GEST ARRIV
003300071227     D NPS             S              2    DIM(90)                              LETT BARCOD
003400071227     D CPS             S              7  0 DIM(90)                              COLLI LETTI
003500941019     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
003600050511     D zone            S              2    DIM(200)                             COMANDO QCMDEXC
003700160503     D zone12          S              2    DIM(200)                             COMANDO QCMDEXC
003800101220     d NPGARR          s              2    dim(10)
003900101220     D*
004000040427     D fnarbinf        ds
004100040427     D  fnarbnrri            397    400i 0
004200941017     D PARAM1          DS
004300911015     D  COMNFV                 1      5  0
004400911015     D  VIDSN                  6      6
004500941028     D* FILIALE DI CUI GESTIRE LE LINEE IN ARRIVO
004600920813     D  VIDFIL                 7      9  0
004700941028     D* FILIALE APERTURA FOGLIO
004800941031     D  VIDFGS                10     12  0
004900950810     D* TIPO INVENTARIO --> G-GIACENZE A-ARRIVI
005000950810     D  VIDTIN                13     13
005100160502     D*
005200160502     D  PAR_lib               14     14
005300060508     D* linea di arrivo
005400060508     D  vidlna                15     17  0
005500070418     D* solo per IMA - elabora colli mancanti di spedizioni in distinta
005600070418     D  vidmdc                18     18
005700101215     D* solo colli spuntati alla lna finale            - solo IMA
005800101215     D  parimaa               19     19
005900160502     D* TIPO zona consegna--> P=parcel   M=messaggerie  P1/P2=Parcel 1° e 2° turno
006000160502     D  PARTpm1               20     21
006100160502     D  PARTpm2               22     23
006200950810     D*** PARAMETRI PER RICERCA FASE ANOMALIA    - TRUL11R -
006300950810     D PARAM5          DS
006400950810     D  PA5TLA                 1      1
006500950810     D  PA5NPG                 2      2
006600950810     D  PA5SPG                 3      3
006700950810     D  PA5FAS                 4      4
006800911018     D                 DS
006900941018     D  COMAAS                 1      4  0
007000941018     D  ARBMGS                 5      8  0
007100941018     D  ARBDSP                 1      8  0
007200911018     D                 DS
007300941018     D  ARBAAS                 1      4  0
007400941018     D  ARBLNP                 5      7  0
007500941018     D  ARBNRS                 8      9  0
007600941018     D  ARBNSP                10     16  0
007700941018     D  ARBSPE                 1     16  0
007800950810     D                 DS
007900950810     D  SAVAAS                 1      4  0
008000950810     D  SAVLNP                 5      7  0
008100950810     D  SAVNRS                 8      9  0
008200950810     D  SAVNSP                10     16  0
008300950810     D  SAVSPE                 1     16  0
008400911015     D WLBDAT          DS
008500941017     D  G02DAT                 1      8  0
008600941017     D  G02INV                 9     16  0
008700941017     D  G02ERR                17     17
008800941017     D  G02TGI                18     22  0
008900990212     D                 DS
009000990212     D  DCTDCA                 1      8  0
009100990212     D  DCTAAC                 1      4  0
009200990212     D  DCTMGC                 5      8  0
009300941019     D* DS PER TRUL06R - CARICAMENTO £X
009400941019     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009500941019     D  LIN                    1     90  0
009600941019     D                                     DIM(30)                              FIL. COMODO
009700031219     D* REPERISCE DATI FOGLI
009800031219     D DSLV53        E DS                  EXTNAME(FNLV53DS)
009900960530     D KPJBA         E DS
010000101202      *
010100101202      * - Parametri per richiamo al pgm di controllo profilo utenti
010200101202     d TIBS34ds      e ds
010300101202      * - Ds di riferimento al file esterno AzUte00F
010400101202     d AZUTEds       e ds                  ExtName(AzUte00F)
010500101202      * - Ds per dati organigramma
010600101202     d dDatiUte      e ds
010700101202      *
010800911018     D DS7C          E DS
010900101220     D DS7N          E DS
011000920318     D DS3A          E DS
011100050511     D DS03          E DS
011200040603     D DAR5GEN       E DS
011300100706     D DVPO          E DS
011400990212     D DTAD            DS
011500990212     D  §TARGR                26     26
011600990303     D DCCH            DS
011700990303     D  §CHDES                 1     50
011800990303     D  §CHIMA                53     53
011900070118     D RECBRV        E DS                  EXTNAME(FNBRV00F)
012000070118     D WSAVB         E DS                  EXTNAME(FNBRV00F)
012100031219     d                                     prefix(S_)
012200990212     D DSBS02        E DS                  EXTNAME(TIBS02DS)
012300040427      * DS Esterna x gestione allocazione ed invio messaggi
012400040427     D TRUL82DS      E DS
012500081014      * DS Esterna x controllo colli doppi
012600081014     D FNLRV7DS      E DS
012700040806      * ds per il controllo della presenza di CA per la spedizione richiesta
012800040806     d FIDN12DS      e ds
012900040806     d  skKey                 26    305    dim(20)
013000040806     d  skAnn                306    325    dim(20)
013100040806     d  skDca                326    485  0 dim(20)
013200101202     d**skFca                486    545  0 dim(20)
013300040806     d  skDch                546    705  0 dim(20)
013400040806     d  skCch                706    745    dim(20)
013500101202      *
013600040806     d dsKey           ds
013700040806     d  dsaac                         4  0
013800040806     d  dsfil                         3  0
013900040806     d  dsnca                         7  0
014000040806      *
014100101202      *
014200021203      *
014300101220     d WnpgArr         s              2
014400101220     d Trov            s              2  0
014500090522     d Nobolla         S              1
014600090522     d uno             S              1
014700081014     d statipo         S             21
014800160503     d statipo2        S             21
014900101215     d staimaa         S             44
015000070418     d stamdc          S              2
015100160503     d indMP           S              4  0
015200160503     d ind12           S              4  0
015300050511     d WSPG            S                   LIKE(FVVSPG)
015400040108     d kpdpdr          S                   LIKE(APDpdr)
015500031219     d wdfv            S                   LIKE(d53dfv)
015600031219     d kpdtip          S                   LIKE(APDtip)
015700040603     d ktrd            S                   LIKE(ar5trd) inz('GEN')
015800100706     d dataEUR         s               d   datfmt(*eur)
015900100706     d DataISO         s               d   datfmt(*iso)
016000101202     d*
016100040428     D msgjob          C                   CONST('La Manut.Bolle sta bloccando -
016200040428     D                                     l''elabor. Dell''IMA:USCIRE SUBITO D-
016300040428     D                                     A QUESTO LAVORO')
016400050511     D CPARCEL         C                   CONST('     P A R C E L     ')
016500050511     D CMessag         C                   CONST('M E S S A G G E R I E')
016600160503     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
016700160503     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
016800101215     D CsoloLNA        C                   CONST('Solo colli spuntati alla line-
016900101215     d                                     a arrivo finale')
017000911018     C*--------------------------------------------------------------- *
017100911018     C*                  INDICATORI                                    *
017200911018     C*--------------------------------------------------------------- *
017300911018     C* 01    - ESITONO ANOMALIE DI MANCANZA
017400950810     C* 02    - CHIESTA CHIUSURA  FOGLIO
017500911018     C* 03    - ESISTE SERIE
017600911018     C* 04    - SEGNACOLLI NON SEQUENZIALI
017700911018     C* 05    - ON CAMBIATA SPEDIZIONE
017800920325     C* 06    - NON C'E' COD PADRONCINO CONSEGNA
017900920708     C* 07    - MANCA DATA ARRIVO : SPEDIZIONE FORZATA
018000920813     C* 08    - BOLLA GESTITA
018100950810     C* 10    - ON --> IMG  OFF --> IMA
018200081014     C* 12    - CONTROLLO COLLI DOPPI
018300950810     C* 13    - NON TROVATA SPUNTA IN IMA MA  IN ALTRO FOGLIO NELLA
018400950810     C*         STESSA DATA
018500950810     C* 14    - NON TROVATA SPUNTA IN IMA
018600031219     C* 15    - TROVATA SPUNTA SU ALTRO FOGLIO MA DEVO CREARE L'ANOMAL
018700100706     C* 16    - collo in IMA ma spuntato in IMA più di XX volte...da tabella VPO
018800050405     C* 24    - STAMPA ubicazione collo se c'e'
018900050405     C* 25    - STAMPA se c'e' c.a.
019000050405     C* 26    - STAMPARE DATA CHIUSURA C.A.
019100941017     C* 30    - LETTURA FNARB
019200941017     C* 31    - LETTURA FNART
019300911018     C* 32/34 - COMODO
019400911018     C*---------------------------------------------------------------*
019500900622     C     *ENTRY        PLIST
019600900622     C                   PARM                    KPJBA
019700941017     C                   MOVEL     KPJBU         PARAM1
019800000126     C                   Z-ADD     COMNFV        WNFV
019900900828     C*-----------------------------------------------------
020000950810     C* IMPOSTAZIONI INIZIALI
020100950810     C                   EXSR      IMPO
020200950810     C*-----------------------------------------------------
020300941019     C                   Z-ADD     80            LUNG             15 5
020400941019     C                   MOVEL     *BLANKS       COMMAN           80
020500950810     C                   MOVEA     VIDTIN        CMD(35)
020600941019     C                   MOVEA     CMD           COMMAN
020700941019     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
020800941019     C                   PARM                    COMMAN
020900941019     C                   PARM                    LUNG
021000950810     C*
021100950316RM*  C                   OPEN      PRTF198
021200980128     C                   SETON                                        OF
021300911015     C*
021400040108     C* CONSIDERO SOLO i p.o. di £6
021500941019     C                   Z-ADD     1             WA                3 0          CONTATORE
021600040108    1C     L6(WA)        DOWNE     *ZERO
021700941019     C*
021800941019     C*
021900040108     C                   Z-ADD     L6(WA)        KLNA                           LINEA ARRIVO
022000031218     C     KLNA          SETLL     FNARB05L
022100040427     C     KLNA          READE     FNARB05L                               30
022200950810     C*
022300950810    3C     *IN30         DOWEQ     *OFF
022400050511     c* Se ima messaggerie o parcel, escludo le zone non di interesse
022500160503     c                   eval      indMP=0
022600160503     c                   eval      ind12=0
022700160503     c
022800160503     c                   if        wselzona='S'
022900160503     c                   movel     arbznc        alfzona           2
023000160503
023100160503     c                   if        wparc='S' or wmess='S'
023200160503     c***  alfzona       lookup    zone                                   35
023300160503     c                   eval      indMP =%lookup(alfzona:zone)
023400050511     c                   endif
023500160503     c                   if        wparc1='S' or wparc2='S'
023600160503     c                   eval      ind12 =%lookup(alfzona:zone12)
023700160503     c                   endif
023800160503
023900160503     c                   endif
024000160503   3ac                   if        wselzona=' ' or  indmp>0 or  ind12>0
024100920813     C*
024200920318     C* CHAIN CODICE BOLLE PER ESCLUDERE I RECUPERI
024300920318     C                   MOVEL     '3A'          COD
024400950810     C                   MOVEL(P)  ARBCBO        KEY
024500941018     C     KTAB          CHAIN     TABEL00F                           35
024600920318     C  N35              MOVEL     TBLUNI        DS3A
024700920318     C   35              MOVEL     'F1'          §3ATB1
024800920318     C*
024900950810     C* ESCLUDO:  LE BLOCCATE CON "B"/
025000930518     C*           QUELLE IN CONSEGNA / QUELLE CONSEGNATE / I RECUPERI
025100070418     C     ARBdcm        ifeq      *zeros
025200950810     C     §3ARBL        ANDNE     'R'
025300950810     C     ARBFBC        ANDNE     'B'
025400000512     C*
025500960516     C* CONTROLLO SE E' GIACENTE
025600960516     C                   CLEAR                   WGIAC
025700960516     C     ARBFBC        IFEQ      'G'
025800050331     C     KGCA          CHAIN     Tigcp21L                           35
025900970721     C* SE C'E' LA GIACENZA E NON E' A FASE 35 LA ESCLUDO
026000970721     C  N35GCPFAS        IFLT      35
026100050331     C     GCPFAS        oreq      36
026200970721     C                   MOVEL     'S'           WGIAC             1
026300960516     C                   ENDIF
026400970721     C                   ENDIF
026500950810     C*
026600950810     C* SE IMA --> ESCLUDO LE GIACENTI
026700950810    5C     *IN10         IFEQ      *OFF
026800960516     C     WGIAC         ANDNE     'S'
026900950810     C*
027000950810     C* SE IMG --> INCLUDO SOLO LE GIACENTI
027100950810     C     *IN10         OREQ      *ON
027200960516     C     WGIAC         ANDEQ     'S'
027300070418     c* escludo anche quelle in distinta in IMG
027400070418     c     arbndc        andeq     0
027500921013     C*
027600950317     C* ESCLUDO  SE BOLLA MAMMA E UNA DELLE FIGLIE E' IN CONSEGNA
027700960516     C*     O SE SONO TUTTE BLOCCATE
027800950810     C                   Z-ADD     ARBSPE        SAVSPE
027900960516     C*
028000960516     C                   CLEAR                   WESFI                          ESISTE FIGL
028100960516     C                   MOVEL     'S'           WFIBLO            1            FIGLIE BLOCC
028200950317     C                   SETOFF                                       36
028300950810     C     KSAV          SETLL     FNLBL02L
028400950810     C     KSAV          READE     FNLBL02L                               35
028500960516     C**
028600950810    6C     *IN35         DOWEQ     *OFF
028700950317     C*
028800950317     C     KLBL          CHAIN     FNARB01L                           35
028900950810    7C     *IN35         IFEQ      *OFF
029000960516     C                   MOVEL     'S'           WESFI             1
029100960516     C* FIGLIA IN CONSEGNA
029200960516    8C     ARBNDC        IFNE      0
029300950317     C                   SETON                                        3536
029400960516   X8C                   ELSE
029500960516     C*
029600960516     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
029700960516    9C     ARBFBC        IFNE      'B'
029800960516     C                   MOVEL     'N'           WFIBLO
029900960516    9C                   ENDIF
030000960516    8C                   ENDIF
030100960516    7C                   ENDIF
030200950317     C*
030300950810     C  N36KSAV          READE     FNLBL02L                               35
030400950810    6C                   ENDDO
030500960516     C*
030600960516     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
030700960516     C  N36WESFI         IFEQ      'S'
030800960516     C     WFIBLO        ANDEQ     'S'
030900960516     C                   SETON                                        36
031000960516     C                   ENDIF
031100070418     c
031200070418     c* Per 36 off - controllo in IMA se richiesti anche i colli mancanti
031300070418     c*              di spedizioni in distinta
031400070418     c  n36              if        not *in10 and vidmdc='N' and arbndc>0
031500070418     c                   seton                                        36
031600070418     c                   endif
031700911204     C*
031800950810    6C  N36              DO
031900950317     C*
032000040427     C     KSAV          CHAIN     FNARB01L                           31
032100950810     C                   MOVEL     ARBAMA        SAVAMA            1 0
032200040428     c                   clear                   wchain            1
032300911018     C* LEGGO DETTAGLIO
032400941017     C     KART          SETLL     FNART01L
032500040428     C  N10
032600040428     CAN 02KART          READE     FNART01L                               31
032700040428     C   10
032800040428     CORN02KART          READE(N)  FNART01L                               31
032900911018     C*
033000950810    7C     *IN31         DOWEQ     *OFF
033100911204     C                   SETOFF                                       34
033200950810     C*
033300950810     C* ESCLUDO  SE IL COLLO E' CONSEGNATO (CONSEGNA PARZIALE)
033400070418     C     ARTDCM        IFEQ      0
033500950810     C  N10              EXSR      ELAIMA
033600950810     C   10              EXSR      ELAIMG
033700950810    8C                   ENDIF
033800930629     C*
033900040428     C  N10
034000040428     CAN 02KART          READE     FNART01L                               31
034100040428     C   10
034200040428     CORN02KART          READE(N)  FNART01L                               31
034300950810    7C                   ENDDO
034400920318     C*
034500950810    6C                   ENDDO
034600950810    5C                   ENDIF
034700000512    4C                   ENDIF
034800050511   3aC                   ENDIF
034900911018     C*
035000040427     C     KLNA          READE     FNARB05L                               30
035100950810    3C                   ENDDO
035200941019     C*
035300040108     C                   ADD       1             WA                             CONTATORE L6
035400950810    1C                   ENDDO
035500920813     C*
035600920813     C* RIEPILOGO SPUNTE PER PISTOLA
035700920813     C                   EXSR      RIEPIL
035800911018     C*
035900060508     C* SE NON TROVATE DISCORDANZE e non fatte parzializzazioni
036000060508     c*  se non ho parzii
036100070418     c* 18/04/07 - IMA chiude solo se richiesta forzatura chiusura
036200960530     C     *IN01         IFEQ      *OFF
036300070418     C     *IN10         andeq     *ON
036400060508     c
036500160503     c     wselzona      andeq     ' '
036600060508     c
036700060508     c     vidlna        andeq     0
036800060508     c
036900060508     C*  O FORZATA LA CHIUSURA METTO FLAG
037000911018     C     VIDSN         OREQ      'S'
037100060508     c
037200941017     C     KFVV          CHAIN     FNFVV01L                           32
037300920813     C  N32              MOVEL     'S'           FVVFCF
037400941017     C  N32              UPDATE    FNFVV000
037500911018     C                   END
037600050511     C*
037700081014     C* CHIUSURA PGM
037800990212     C                   CLEAR                   DSBS02
037900990212     C                   MOVEL     'C'           T02TLA
038000990212     C                   CALL      'TIBS02R'
038100990212     C                   PARM                    KPJBA
038200990212     C                   PARM                    DSBS02
038300031219     c
038400031219     C                   CLEAR                   DSLV53
038500031219     C                   MOVEL     'C'           D53TLA
038600031219     C                   CALL      'FNLV53R'
038700031219     C                   PARM                    DSLV53
038800040806     C*
038900040806     C                   MOVEL     'C'           I12TLA
039000040806     C                   CALL      'FIDN12R'
039100040806     C                   PARM                    FIDN12DS
039200140430     C*
039300140430     c                   clear                   fnlrv7ds
039400140430     C                   MOVEL     'C'           ILRV7TLA
039500140430     c                   eval      kpjbu= fnlrv7ds
039600140430     C                   CALL      'FNLRV7R'
039700140430     C                   PARM                    KPJBA
039800040806      *
039900000000     C                   SETON                                        LR
040000031219     C**************************************************************************
040100031219     C* CONTROLLO SE SEGNACOLLO MANCANTE
040200031219     C**************************************************************************
040300031219     C     CTRART        BEGSR
040400031219     C*
040500031219     C                   SETOFF                                       131415
040600100706     C                   SETOFF                                       3316
040700031219     C                   MOVE      *BLANKS       STADSF
040800031219     C*
040900070118     C     KBRV1         SETLL     FNBRV01L                               33
041000070418     c
041100070418     c* se non ho trovato spunta IMA e spedizione in distinta, cerco
041200070418     c*  la categoria 4 del giorno
041300070418    1c                   if        not *in33 and arbndc>0 and vidmdc='S'
041400100706     C                   Z-ADD     4             conNPG
041500140618     C     KBRV4         chain     FNBRV05L
041600140618    2c                   if        %found(fnbrv05l)
041700070418     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
041800070418     c                   clear                   dslv53
041900070418     C                   z-add     brvnFV        D53NFV
042000070418     C                   MOVEL     brvNPG        D53NPG
042100070418     C                   MOVEL     brvFGS        D53FGS
042200070418     C****               MOVEL     brvfle        D53flf
042300070418     C                   CALL      'FNLV53R'
042400070418     C                   PARM                    DSLV53
042500070418     C* ERRORE
042600070418     c*  se sparato nella stessa data: ok
042700070418     c*  e sparato in altra data, rientro nei giri normali di controllo
042800070418     c*  spunte
042900070418    3c                   if        d53err=' ' and d53dfv=savdfv
043000070418     c                   seton                                        33
043100070418    3c                   endif
043200070418    2c                   endif
043300070418    1c                   endif
043400070418     c
043500031219     C* 14 ON - SPUNTA NON TROVATA
043600031219    1C     *IN33         IFEQ      *OFF
043700070418     c
043800031219     C                   SETON                                        14
043900031219     c*
044000031219     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
044100031219     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
044200031219     c*  immissione + alta
044300031219     C                   exsr      RICSPU
044400031219     C
044500031219     C* SE SPUNTA TROVATA CON DATA > STAMPO MA NON CREO ANOMALIA
044600031219     C                   IF        WDFV>SAVDFV
044700031219     C                   EVAL      *IN13=*ON
044800031219     C                   ENDIF
044900031219     C
045000031219     C* SE SPUNTA TROVATA CON DATA = STAMPO E CREO ANOMALIA SOLTANTO SE
045100031219     C*  CATEGORIA 2 O 6
045200031219     C                   IF        WDFV=SAVDFV
045300031219     C                   EVAL      *IN13=*ON
045400101202     C                   IF        BRVNPG=2
045500031219     C                   EVAL      *IN15=*ON
045600031219     C                   ENDIF
045700031219     C                   ENDIF
045800031219     c*
045900031219    1C                   END
046000960523     C*
046100031219     C
046200031219    1C                   IF        WDFV>=SAVDFV
046300960523     C* GIRO LA DATA
046400031219     C                   MOVE      WDFV          G02INV
046500960523     C                   MOVE      *ZEROS        G02DAT
046600960523     C                   MOVEL     3             G02ERR
046700960523     C                   CALL      'XSRDA8'
046800960523     C                   PARM                    WLBDAT
046900960523     C                   MOVEL     G02DAT        ST2DFV            4 0
047000960523     C**
047100960523     C* DECODIFICO LA FILIALE GESTIONE
047200031219     C     BRVFGS        CHAIN     AZORG01L                           32
047300031219    2C  N32ORGDE5        IFNE      *BLANKS
047400960523     C                   MOVEL     ORGDE5        ST2FGS            8
047500960523     C                   ELSE
047600960523     C                   MOVEL     ORGDES        ST2FGS            8
047700031219    2C                   ENDIF
047800960523     C**
047900960523     C* DECODIFICO LA CATEGORIA SPUNTA
048000031219    2C     BRVNPG        IFEQ      3
048100960523     C                   MOVEL     'IMA'         ST2CAT            3
048200040108     C                   MOVE      WSPG          ST2CAT
048300031219   X2C                   ELSE
048400960523     C                   MOVEL     '7N'          COD
048500031219     C                   MOVEL(P)  BRVNPG        KEY
048600960523     C     KTAB          CHAIN     TABEL                              32
048700960523     C  N32              MOVEL     TBLUNI        ST2CAT
048800960523     C   32              CLEAR                   ST2CAT
048900031219    2C                   ENDIF
049000031219     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
049100031219    2C                   IF        BRVNPG=4
049200031219     C                   Z-ADD     BRVNFV        WNFV5
049300031219     C     KFVV2         CHAIN(N)  FNFVV000                           32
049400070118RM*  C  N32              MOVEL     FVVDSF        STADSF           13
049500031219    2C                   ENDIF
049600031219    1C                   ENDIF
049700960523     C*
049800100706     c* Se trovata spunta, segnalo se sparata xx volte in IMA
049900100706     c*  solo per bolle in IMA più vecchie di ....
050000100707     c* Escludo bolle in distinta
050100150113     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
050200100707     c                   if        not *in14 and not *in10
050300100706     c                             and arbndc=0 and arbdsp<datapresIMA
050400100706     c
050500100706     c* conto quante volte presente in ima
050600100706     C                   Z-ADD     3             conNPG
050700100706     c                   clear                   conta             4 0
050800140618     C     KBRV4         setll     FNBRV05L
050900140618     C     KBRV4         reade     FNBRV05L
051000100706     c
051100140618    2c                   dow       not %eof(fnbrv05l)
051200100706     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
051300100706     c                   clear                   dslv53
051400100706     C                   z-add     brvnFV        D53NFV
051500100706     C                   MOVEL     brvNPG        D53NPG
051600100706     C                   MOVEL     brvFGS        D53FGS
051700100706     C                   CALL      'FNLV53R'
051800100706     C                   PARM                    DSLV53
051900100706     c
052000100706     c                   if        d53spg='A'
052100100706     c                   eval      conta=conta+1
052200100706     c                   endif
052300100706     c
052400140618     C     KBRV4         reade     FNBRV05L
052500100706     c                   enddo
052600100706     c
052700100706     c                   if        conta>=§vpoggima
052800100706     c                   seton                                        16
052900100706     c                   endif
053000100706     c
053100100706     c
053200100706     c                   endif
053300100706     c
053400960523     C                   ENDSR
053500960523     C*
053600031219     C**************************************************************************
053700031219     C* cerco spunta del collo in qualsiasi categoria
053800031219     C**************************************************************************
053900031219     C     RICSPU        BEGSR
054000031219     C                   CLEAR                   Wimm
054100031219     C                   CLEAR                   WSAVB
054200031219     C                   CLEAR                   WDFV
054300031219     C                   CLEAR                   WTROV
054400040108     C                   CLEAR                   WSPG
054500070118RM*  C     Kartd         CHAIN     FNBRV07l                           33
054600031219RM*  C*
054700031219RM* 1C     *IN33         DOWEQ     *OFF
054800031219     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
054900031219     c                   clear                   dslv53
055000031219     C                   z-add     brvnFV        D53NFV
055100031219     C                   MOVEL     brvNPG        D53NPG
055200031219     C                   MOVEL     brvFGS        D53FGS
055300070118     C****               MOVEL     brvfle        D53flf
055400031219     C                   CALL      'FNLV53R'
055500031219     C                   PARM                    DSLV53
055600031219     C* ERRORE
055700031219    2C     D53ERR        IFeq      ' '
055800070118     c                   movel     brvdfs        brvimm           14 0
055900031219     c                   move      brvhms        brvimm
056000031219    3c                   select
056100031219     c* Se maggiore memorizzo
056200031219     c                   when      d53dfv>wdfv
056300031219     C                   MOVEL     RECBRV        WSAVB
056400070118     C                   MOVEL     brvimm        Wimm             14 0
056500031219     C                   MOVEL     d53DFV        WDFV
056600040108     C                   MOVEL     d53spg        WSPG
056700031219     C                   MOVEL     'S'           WTROV             1
056800031219     c
056900031219     c* Se uguale tengo la data ora immissione + alta
057000031219     c                   when      d53dfv=wdfv  and brvimm>wimm
057100070118     C                   MOVEL     brvimm        Wimm             14 0
057200031219     C                   MOVEL     RECBRV        WSAVB
057300031219     C                   MOVEL     d53DFV        WDFV
057400040108     C                   MOVEL     d53spg        WSPG
057500031219     C                   MOVEL     'S'           WTROV             1
057600031219     C                   eval      wimm=brvimm
057700040129     c* Se stessa data e ora immissione, tengo cat 3 o 4
057800101202     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
057900070118     C                   MOVEL     brvimm        Wimm             14 0
058000040129     C                   MOVEL     RECBRV        WSAVB
058100040129     C                   MOVEL     d53DFV        WDFV
058200040129     C                   MOVEL     d53spg        WSPG
058300040129     C                   MOVEL     'S'           WTROV             1
058400040129     C                   eval      wimm=brvimm
058500031219    3c                   endsl
058600031219    2c                   endif
058700031219     c
058800070118RM*  C     Kartd         reade     FNBRV07l                               33
058900031219    1c                   enddo
059000031219     c
059100031219     C     WTROV         IFEQ      'S'
059200031219     C                   MOVEL     WSAVB         RECBRV
059300031219     C                   ENDIF
059400031219     c                   endsr
059500941017     C**************************************************************************
059600941017     C* IMPOSTO CAMPI FISSI
059700941017     C**************************************************************************
059800911018     C     IMPO          BEGSR
059900921022     C*
060000101202     C                   Z-ADD     1             CODUT             1 0
060100921022     C                   Z-ADD     3             IMANPG
060200950810     C                   CLEAR                   SAVSPE
060300950810     C                   CLEAR                   COMSPE
060400921022     C                   TIME                    UTIME             6 0
060500960927     C                   TIME                    W0140            14 0
060600960927     C                   MOVE      W0140         UDATE8            8 0
060700101202      *
060800101202      * Reperimento dati utente/aziendali
060900101202     c                   exsr      sr_DatiJob
061000101202      *
061100950810     C* 10 ON - IMG
061200950810     C     VIDTIN        IFEQ      'G'
061300950810     C                   SETON                                        10
061400950810     C                   ENDIF
061500921022     C*
061600921022     C* CHAIN E DATA DEL FOGLIO IMA
061700941017     C     KFVV          CHAIN(N)  FNFVV01L                           32
061800921022     C*
061900941018     C                   Z-ADD     FVVDFV        G02INV                         AAAA/MM/GG
062000941018     C                   Z-ADD     *ZERO         G02DAT
062100941018     C                   MOVEL     '3'           G02ERR
062200941018     C                   CALL      'XSRDA8'
062300941018     C                   PARM                    WLBDAT
062400941018     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
062500950316     C*
062600950317RM*  C                   Z-ADD     FVVDFV        SAVDFV
062700950317RM*  C                   Z-ADD     FVVFGS        SAVFGS
062800931228     C*
062900941028     C* DECODIFICA FILIALE DI APERTURA FOGLIO
063000941018     C     FVVFGS        CHAIN     AZORG                              32
063100931228     C  N32              MOVEL     ORGDES        DESCDU           20
063200000512     C*
063300000512     C                   Z-ADD     15            ANMCAA
063400000512     C                   MOVEL     '7C'          COD
063500000512     C                   MOVEL(P)  ANMCAA        KEY
063600000512     C     KTAB          CHAIN     TABEL00F                           35
063700000512     C  N35              MOVEL     TBLUNI        DS7C
063800000512     C   35              MOVEL     *BLANKS       DS7C
063900911018     C*
064000931228     C* 02 ON -  D E F I N I T I V O
064100911018     C     VIDSN         IFEQ      'S'
064200911018     C                   SETON                                        02
064300911018     C*
064400921022     C* PRENDO FASE ANOMALIA
064500950810     C                   CLEAR                   PARAM5
064600950810     C                   MOVEL     'L'           PA5TLA
064700950810     C                   MOVEL     3             PA5NPG
064800950810     C                   MOVEL     VIDTIN        PA5SPG
064900950810     C                   CALL      'TRUL11R'
065000950810     C                   PARM                    PARAM5
065100920409     C                   END
065200920813     C*
065300920813     C* DECODIFICA FILIALE IN GESTIONE
065400920813     C     VIDFIL        CHAIN     AZORG                              32
065500920813     C  N32              MOVEL     ORGDES        DESFIL           20
065600941019     C*
065700101215     C* CARICO L6
065800101215     C                   CLEAR                   DSUL06
065900101215     C                   MOVE      '£6'          D06COD
066000101215     C                   MOVEL     VIDFIL        D06KEY
066100101215     C                   MOVEL     'L'           D06TLA
066200101215     C                   MOVEL     DSUL06        KPJBU
066300101215     C                   CALL      'TRUL06R'
066400101215     C                   PARM                    KPJBA
066500101215     C                   MOVEL     KPJBU         DSUL06
066600101215     C                   MOVEA     LIN           Lspu
066700060508     c*
066800101215     C*  Se non parzializzata la linea di arrivo, elaboro L6
066900101215     c*  amtrimenti elaboro solo lei
067000060508     c                   if        vidlna=0
067100101215     C                   MOVEA     LIN           L6
067200060508     c                   else
067300060508     c                   clear                   L6
067400060508     c                   movel     vidlna        l6(1)
067500060508     c
067600060508     c* Decodifica
067700060508     C     VIDlna        CHAIN     AZORG                              32
067800060508     C  N32              MOVEL     ORGDES        DESlna           20
067900060508     c                   movel     vidlna        stalna            3
068000060508     c                   endif
068100070418     c
068200070418     c* Elabora anche spedizioni in distinta
068300070418     c                   if        vidmdc='S'
068400070418     c                   eval      stamdc='SI'
068500070418     c                   else
068600070418     c                   eval      stamdc='NO'
068700070418     c                   endif
068800101215     c* solo colli spuntati alla linea di arrivo finale
068900101215     c                   if        parimaa='S'
069000101215     c                   eval      staimaa=csololna
069100101215     c                   endif
069200160503     c* Verifico le richieste delle zone di consegna
069300160503     c                   clear                   wParc             1
069400160503     c                   clear                   wParc1            1
069500160503     c                   clear                   wParc2            1
069600160503     c                   clear                   wMess             1
069700160503     c                   clear                   wSelzona          1
069800160503     c                   if        partpm1='P ' or partpm2='P '
069900160503     c                   eval      wparc='S'
070000160503     c                   eval      wselzona='S'
070100160503     c                   endif
070200160503     c                   if        partpm1='M ' or partpm2='M '
070300160503     c                   eval      wMess='S'
070400160503     c                   eval      wselzona='S'
070500160503     c                   endif
070600160503     c                   if        partpm1='P1' or partpm2='P1'
070700160503     c                   eval      wParc1='S'
070800160503     c                   eval      wselzona='S'
070900160503     c                   endif
071000160503     c                   if        partpm1='P2' or partpm2='P2'
071100160503     c                   eval      wParc2='S'
071200160503     c                   eval      wselzona='S'
071300160503     c                   endif
071400060508     c
071500050510     c* se richiesta soltanto zone parcel o messagerie, carico in
071600050510     c*  schiera
071700160503     c                   if        wparc='S' and wmess='S'
071800160503     c                   clear                   wparc
071900160503     c                   clear                   wmess
072000160503     c                   clear                   wselzona
072100160503     c                   endif
072200160503     c                   if        wparc1='S' and wparc2='S'
072300160503     c                   clear                   wparc1
072400160503     c                   clear                   wparc2
072500160503     c                   eval      wparc='S'
072600160503     c                   endif
072700160503     c
072800160503    1c                   if        wselzona='S'
072900050511     c                   clear                   xx                4 0
073000160502     c                   clear                   yy                4 0
073100050510     c                   movel     vidfil        alffil            3
073200050511     c                   movel     vidfil        wpo
073300050510     c                   movel(P)  '03'          cod
073400050510     c                   movel(P)  vidfil        key
073500050510     c     ktab          setll     tabel
073600050510     c     ktab2         reade     tabel
073700160502    2c                   dow       not %eof(tabel00f) and  wpo=alffil
073800160502    3c                   if        tblflg=' '
073900050510     c                   movel     tblkey        wpo               3
074000160502    4c                   if        wpo=alffil
074100050511     c                   movel     tbluni        ds03
074200160502
074300160502     c* parcel/messaggerie
074400160503    5c                   if        wparc='S' or wmess='S'
074500160502
074600160503    6c                   if        (wmess='S' and §03tpm='M') or
074700160503     c                             (wparc='S' and §03tpm<>'M')
074800050511     c                   add       1             xx
074900050511     c                   movel     tblkey        w005a             5
075000050511     c                   move      w005a         zone(xx)
075100160502    6c                   endif
075200160502    5c                   endif
075300160502     c* parcel 1° e 2° turno
075400160503    5c                   if        (wparc1='S' or wparc2='S')
075500160503     c                             and §03tpm<>'M'
075600160502
075700160503    6c                   if        (wparc2='S' and §03turno='2') or
075800160503     c                             (wparc1='S' and §03turno<>'2')
075900160503     c                   add       1             yy
076000160502     c                   movel     tblkey        w005a             5
076100160503     c                   move      w005a         zone12(yy)
076200160502    6c                   endif
076300160502    5c                   endif
076400160502     c
076500160502    4c                   endif
076600160502    3c                   endif
076700160502     c
076800050511     c     ktab2         reade     tabel
076900160502    2c                   enddo
077000160502    1c                   endif
077100050511     c
077200160503     c                   clear                   statipo
077300160503     c                   clear                   statipo2
077400160503     c                   if        partpm1='P ' or partpm2='P '
077500050511     c                   eval      statipo=cparcel
077600050511     c                   endif
077700050511     c
077800160503     c                   if        partpm1='M ' or partpm2='M '
077900160503     c                   if        statipo=*blanks
078000050511     c                   eval      statipo=cmessag
078100160503     c                   else
078200160503     c                   eval      statipo2=cmessag
078300160503     c                   endif
078400050511     c                   endif
078500160503     c                   if        partpm1='P1' or partpm2='P1'
078600160503     c                   if        statipo=*blanks
078700160503     c                   eval      statipo=cparc1
078800160503     c                   else
078900160503     c                   eval      statipo2=cparc1
079000160503     c                   endif
079100160503     c                   endif
079200160503     c                   if        partpm1='P2' or partpm2='P2'
079300160503     c                   if        statipo=*blanks
079400160503     c                   eval      statipo=cparc2
079500160503     c                   else
079600160503     c                   eval      statipo2=cparc2
079700160503     c                   endif
079800160503     c                   endif
079900941017     C*
080000100706      * Aggancio tabella VPO recpord VPO
080100100706     c                   Clear                   Dvpo
080200100706     c                   Clear                   dsbs02
080300100706     c                   Eval      T02Mod = 'C'
080400100706     c                   Eval      T02tla = 'L'
080500100706     c                   Eval      T02Sif = knsif
080600100706     c                   Eval      T02Cod = 'VPO'
080700100706     c                   Movel(p)  'VPO'         T02Ke1
080800100706     c                   Call      'TIBS02R'
080900100706     c                   Parm                    Kpjba
081000100706     c                   Parm                    dsbs02
081100100706     c                   If        T02Err = *Blanks
081200100706     c                   Movel     T02Uni        Dvpo
081300100706     c                   EndIf
081400100706     c
081500100706     c     *eur          move      udate8        dataeur
081600100706     c                   movel     dataeur       dataiso
081700100706     c                   if        §vpoggima>0
081800100706     c                   subdur    §vpoggima:*d  dataiso
081900100706     c                   endif
082000100706     c
082100100706     c                   movel     dataiso       datapresIMA       8 0
082200100706
082300911018     C                   ENDSR
082400941017     C**************************************************************************
082500941017     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
082600941017     C**************************************************************************
082700911018     C     ERRORE        BEGSR
082800941017     C*
082900911015     C                   SETON                                        01
083000040603     c
083100040603     c     kar5          chain     fiar501l
083200040603     c                   if        %found(fiar501l)
083300040603     c                   movel     ar5uni        dar5gen
083400040603     c                   else
083500040603     c                   clear                   dar5gen
083600040603     c                   endif
083700911018     C* STAMPA ANOMALIA
083800040428     C                   EXSR      STAMPA
083900081014     c
084000100706     c* non creo anomalia se collo doppio o solo segnalazione
084100100706    0c                   if        not *in12 and not *in16
084200911015     C*
084300950707     C* SOLO SE IMA DEFINITIVO  CREO ANOMALIA
084400950608    1C     VIDSN         IFEQ      'S'
084500950810     C* NON DEVO CREARE:
084600950810     C* 1) SE C'E' LA FORZATURA E MANCA LA DATA DI ARRIVO
084700950810     C*    ANCHE SE STAMPO LO STESSO
084800950810     C* 2) SE LA SPUNTA E' STATA TROVATA IN ALTRA CATEGORIA DEL GIORNO
084900960419     C*    3 O 4 NON DISABILITO NEMMENO LA BOLLA
085000960419     C* 3) SE LA SPUNTA E' STATA TROVATA IN ARRIVO CREO L'ANOMLIA
085100950608    2C     ARTDAM        IFGT      0
085200960419   2AC     *IN13         IFEQ      *OFF
085300031219     C     *IN15         OREQ      *ON
085400081014i    C*
085500081104     C     KANM          SETLL     FNANM02L                               35
085600081014    3c                   if        not *in35
085700081014     c* Se anomalia chiusa non in pratica idd e data chiusura <data ima
085800081014     c*  la ricreo
085900081104    4c***                if        anmdch>0 and anmdch<savdfv and
086000081104     c***                          anmcch<>'PR'
086100081104     c***                delete    fnanm000
086200081104     c***                seton                                        35
086300081104    4c***                endif
086400081104    3c***                endif
086500081014     c
086600081104    3C***                if        *in35
086700081104     C*
086800081104     C                   MOVEL     *BLANKS       ANMATB
086900081104     C                   MOVEL     PA5FAS        ANMFSA
087000160503     c*  data salvata del foglio IMA perchè richaino in update
087100100506     C                   Z-ADD     SAVdfv        ANMDAO
087200081104     C                   Z-ADD     15            ANMcaa
087300081104     C                   MOVEL     *BLANKS       ANMCCH
087400081104     C                   MOVEL     VIDFGS        ANMCDU
087500081104     C                   Z-ADD     COMNFV        ANMNFV
087600081104     C                   MOVEL     VIDFGS        ANMFLE
087700081104     C                   MOVE      *BLANKS       ANMFLO
087800081104     C                   MOVEL     *BLANKS       ANMFT1
087900081104     C                   MOVEL     *ZEROS        ANMFL1
088000081104     C                   MOVEL     *BLANKS       ANMFT2
088100081104     C                   MOVEL     *ZEROS        ANMFL2
088200081104     C*
088300081104     C                   MOVEL     §7CTAN        ANMTAN
088400081104     C                   MOVEL     §7CSAN        ANMSAN
088500081104     C                   MOVEL     §7CAIE        ANMAIE
088600081104     C                   Z-ADD     §7CLID        ANMLID
088700081104     C                   MOVE      *ZEROS        ANMDUE
088800081104     C                   MOVE      *ZEROS        ANMNPG
088900081104     C                   MOVE      *ZEROS        ANMFGS
089000081104     C                   Z-ADD     0             ANMDCH
089100081104     C                   MOVEL     ' '           ANMFSC
089200081104     C* CHIUSURA AUTOMATICA
089300081104     C     §7CCHA        IFEQ      'S'
089400100506     C                   Z-ADD     SAVDFV        ANMDCH
089500081104     C                   MOVEL     PA5FAS        ANMFSC
089600081104     C                   END
089700081104     C                   MOVE      *BLANKS       ANMFT3
089800081104     C                   MOVE      *ZEROS        ANMFL3
089900081104     C                   MOVE      *BLANKS       ANMFT4
090000081104     C                   MOVE      999           ANMFL4
090100081104     C                   MOVE      *ZEROS        ANMDTR
090200081104     C*
090300911018     C                   MOVEL     ARTFLS        ANMFLS
090400911018     C                   MOVEL     ARTLNA        ANMLNA
090500911018     C                   MOVEL     ARTNRS        ANMNRS
090600941018     C                   MOVEL     ARTNSC        ANMSCN
090700941018     C                   Z-ADD     ARTAAS        ANMAAS
090800911018     C                   MOVEL     ARTNSP        ANMNSP
090900911018     C                   MOVEL     ARTLNP        ANMLNP
091000970206     C                   MOVEL     ARTNAP        ANMNPS
091100040603     c
091200040603     c* imposto data e ora di creazione
091300050707     C                   TIME                    W0140            14 0
091400050707     C                   MOVE      W0140         G02DAT
091500050707     C                   Z-ADD     *ZERO         G02INV
091600050707     C                   MOVEL     *BLANKS       G02ERR
091700050707     C                   CALL      'XSRDA8'
091800050707     C                   PARM                    WLBDAT
091900050707     C                   Z-ADD     G02INV        WDATA             6 0
092000050707     c**                 time                    wora              6 0
092100050707     c**                 z-add     *date         wdata             6 0
092200050707     C
092300050707     c                   movel     w0140         wora4             4 0
092400040603     c                   movel     wdata         anmflo
092500040603     c                   move      wora4         anmflo
092600040603     c
092700040603     c* imposto se flag di valore
092800040603     c                   movel     §ar5bva       anmft4
092900040603     c
093000040603     c
093100920409     C* ESTERNA
093200950608    4C     §7CAIE        IFEQ      'E'
093300950608     C                   MOVEL     ARTLNP        ANMFL1
093400950810    4C                   ENDIF
093500920409     C*
093600990511     C                   WRITE     FNANM000
093700081014    3C                   endif
093800081014     c
093900000512   2AC                   ENDIF
094000960419    2C                   ENDIF
094100920318     C*
094200000512     C* DISABILITO LA BOLLA SOLO SE HO CREATO ANOMALIA
094300000512     C* NO POSTE SEMPRE, LNP POSTE SOLO SE PREVOSTO
094400950810     C* SOLO SE NON HO TROVATO UN'ALTRA SPUNTA IN DATA DEL FOGLIO IMA
094500960419    2C     *IN10         IFEQ      *OFF
094600960419     C*
094700960419    3C     *IN13         IFEQ      *OFF
094800031219     C     *IN15         OREQ      *ON
094900000512     C*
095000040428    5c     wchain        ifeq      *blanks
095100040428     c     fnarbnrri     chain(e)  fnarb00f
095200040428    6c                   if        %error
095300040428     C* Valorizzo la DS di procedura
095400040428     C                   CLEAR                   TRUL82DS
095500040428     c                   eval      ul82§rrn = fnarbnrri
095600040428     C                   EVAL      UL82§FIL = 'FNARB00F'
095700040428     C                   EVAL      UL82§WIN = 'N'
095800040428     C                   EVAL      UL82§F7  = 'S'
095900040428     C                   EVAL      UL82§num = 5
096000040428     C                   EVAL      UL82§att = 60
096100040428     C                   EVAL      UL82§mss = msgjob
096200040428     C* Effettuo la chiamata al *PGM d utilità
096300040428     C                   CALL(e)   'TRUL82R'
096400040428     C                   PARM                    TRUL82DS
096500040428     C     fnarbnrri     chain     fnarb00f
096600040428    6c                   endif
096700040428     C                   Z-ADD     0             ARBAMA
096800040428     c                   update    fnarbf00
096900040428     c                   move      '1'           wchain
097000040428    5c                   endif
097100930629     C* AGGIORNO IL COLLO CON UNA 'D' DI DISABILITAZIONE
097200941221     C                   MOVEL     'S'           ARTDAB                         'D'=DISABILITATO
097300000512    3C                   ENDIF
097400960419    2C                   ENDIF
097500950810     C*
097600040428     C  N10              UPDATE    FNART000
097700950810    1C                   ENDIF
097800081014    0C                   ENDIF
097900911015     C*
098000911015     C                   ENDSR
098100950810     C**************************************************************************
098200950810     C* ELABORAZIONE DELL'IMA
098300950810     C**************************************************************************
098400950810     C     ELAIMA        BEGSR
098500950810     C*
098600950810     C* N O N   C O N T R O L L O :
098700950810     C*          1) SE NO ABILITAZIONE FORZATA E SENZA DATA ARRIVO
098800990212    1C     SAVAMA        IFNE      3
098900950810     C     ARTDAM        ANDEQ     0
099000950810     C                   SETON                                        34
099100990212    1C                   ENDIF
099200950810     C*
099300950810     C*          2) SE DATA ARRIVO MAGGIORE DATA FOGLIO IMA
099400990212    1C  N34ARTDAM        IFNE      0
099500950810     C     ARTDAM        ANDGT     SAVDFV
099600950810     C                   SETON                                        34
099700990212    1C                   ENDIF
099800950810     C*
099900990212     C*          3) SE COLLO SI TROVA IN DCD E RESO O DISTRUTTO PER
100000990212     C*             AVARIA O COLLO MANCANTE
100100990212     C     KARTD         CHAIN     FNDCD02L                           31
100200990212    1C     *IN31         IFEQ      *OFF
100300990212     C*
100400990303     C     DCDATB        ANDEQ     ' '
100500990212     C* CHAIN TESTATA PER CONTROLLARE LA DATA DI CHIUSURA
100600990212     C     KDCD          CHAIN     FNDCT01L                           32
100700050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
100800050221     c                   If        Not *In32 and dctdt2 > 0
100900050221     c                   Eval      *In32 = *On
101000050221     c                   EndIf
101100990303     C*
101200990303    2C     *IN32         IFEQ      *OFF
101300990303     C* SE E' CHIUSA CONTROLLO SE IL RECORD DEVE ESSERE COMPRESO
101400990303     C*  IN IMA, PER ALCUNI TIPI ANOMALIA
101500990303     C                   MOVEL     'S'           §CHIMA
101600990303    3C     DCTCCH        IFNE      *BLANKS
101700990303     C                   CLEAR                   DSBS02
101800990303     C                   MOVEL     'C'           T02MOD
101900990303     C                   MOVEL     KNSIF         T02SIF
102000990303     C                   MOVEL     'CCH'         T02COD
102100990303     C                   MOVEL     DCTCCH        T02KE1
102200990303     C                   CALL      'TIBS02R'
102300990303     C                   PARM                    KPJBA
102400990303     C                   PARM                    DSBS02
102500990303     C*
102600990303    4C     T02ERR        IFEQ      ' '
102700990303     C                   MOVEL     T02UNI        DCCH
102800990303    4C                   ENDIF
102900990303    3C                   ENDIF
103000990303     C**
103100990303     C** SE NON DEVE ESSERE COMPRESO:
103200990303    3C     §CHIMA        IFNE      'S'
103300990303     C     DCTDCH        OREQ      0
103400990212     C** SE ENTRAMBI APERTI O CHIUSI CON STESSA DATA CONTROLLO IL TIPO
103500990212     C**  ANOMALIA PER DECIDERE SE TENERE O ESCLUDERE IL COLLO
103600990212     C**  NON CONSIDERO LA CAUSALE PAREGGIO CHE SIGNIFICA CHE IL COLLO
103700990212     C*  C'E' SEMPRE
103800990212    4C     DCTDCH        IFEQ      DCDDCH
103900990212     C     DCDDCH        OREQ      0
104000990212     C**
104100990212     C                   CLEAR                   DSBS02
104200990212     C                   MOVEL     'C'           T02MOD
104300990212     C                   MOVEL     KNSIF         T02SIF
104400990212     C                   MOVEL     'TAD'         T02COD
104500990212     C                   MOVEL     DCTTAD        T02KE1
104600990212     C                   CALL      'TIBS02R'
104700990212     C                   PARM                    KPJBA
104800990212     C                   PARM                    DSBS02
104900990212     C**
105000990212    5C     T02ERR        IFEQ      ' '
105100990212     C                   MOVEL     T02UNI        DTAD
105200990212     C                   ELSE
105300990212     C                   MOVEL     'A'           §TARGR
105400990212    5C                   ENDIF
105500990212    C**
105600990212     C* SE MANCANZE
105700990212    5C     §TARGR        IFEQ      'M'
105800990212     C* O AVARIA RESA
105900990212     C     §TARGR        OREQ      'V'
106000990212     C     DCTDSD        ANDEQ     'R'
106100990212     C* O AVARIA DISTRUTTA
106200990212     C     §TARGR        OREQ      'V'
106300990212     C     DCTDSD        ANDEQ     'D'
106400990212     C*                    --> ESCLUDO
106500990212     C                   SETON                                        34
106600990212    5C                   ENDIF
106700990212    4C                   ENDIF
106800990212    3C                   ENDIF
106900990212    2C                   ENDIF
107000990212    1C                   ENDIF
107100101215     c
107200101215     c* Se opzione solo spuntato alla linea di arrivo finale, controllo
107300101215     c                   if        parimaa='S'
107400101215     c                   setoff                                       37
107500101215RM*  C     Kartd         setll     FNBRV07l
107600101215RM*  C     Kartd         reade     FNBRV07l
107700101215     c                   dow       not %eof(fnbrv07l)
107800101215     c     brvfgs        lookup    Lspu                                   37
107900101215     c                   if        *in37
108000101220     c* Se la filiale che spunta è giusta verifico che sia categoria di "arrivo"
108100101220     c                   clear                   dslv53
108200101220     C                   z-add     brvnFV        D53NFV
108300101220     C                   MOVEL     brvNPG        D53NPG
108400101220     C                   MOVEL     brvFGS        D53FGS
108500101220     C                   CALL      'FNLV53R'
108600101220     C                   PARM                    DSLV53
108700101220     C*
108800101220    2C     D53ERR        IFeq      ' '
108900101220     c*
109000101220     c                   if        d53def='S'
109100101220     c                   eval      wnpgarr=%editc(d53npg:'X')+'D'
109200101220     c                   else
109300101220     c                   eval      wnpgarr=%editc(d53npg:'X')+d53spg
109400101220     c                   endif
109500101220     c
109600101220     c                   eval      TROV=%lookup   (wnpgarr: NPGARR)
109700101220     c                   if        TROV>0
109800101215     c                   leave
109900101220     c                   endif
110000101215     c                   endif
110100101220     c                   endif
110200101220     c
110300101220     c                   setoff                                       37
110400101220RM*  C     Kartd         reade     FNBRV07l
110500101215     c
110600101215     c                   enddo
110700101215     c
110800101215     c* se non trovata nessuna spunta, NON elaboro
110900101215     c                   if        not *in37
111000101215     c                   seton                                        34
111100101215     c                   endif
111200101215     c
111300101215     c                   endif
111400990212    C**
111500100706     c                   if        not *in34
111600100706     C                   EXSR      CTRART
111700950810     C*
111800950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
111900100706     C* 16 ON -     TROVATO SEGNACOLLO IN IMA più di xxx volte
112000100706     C   14
112100100706     Cor 16              EXSR      ERRORE
112200100706     c                   endif
112300950810     C*
112400950810     C                   ENDSR
112500950810     C**************************************************************************
112600950810     C* ELABORAZIONE DELL'IMG
112700950810     C**************************************************************************
112800950810     C     ELAIMG        BEGSR
112900950810     C                   EXSR      CTRART
113000950810     C*
113100950810     C* 14 ON - NON TROVATO SEGNACOLLO IN IMA
113200950810     C   14              EXSR      ERRORE
113300950810     C                   ENDSR
113400941017     C**************************************************************************
113500941017     C* STAMPO DISCORDANZA
113600941017     C**************************************************************************
113700900621     C     STAMPA        BEGSR
113800050405     c                   setoff                                       242526
113900941017     C*
114000911015     C* STAMPA TESTATA
114100911015     C   OF              DO
114200911018     C                   SETON                                        05
114300911018     C                   EXCEPT    TES
114400920813     C                   EXCEPT    TES2
114500911018     C                   SETOFF                                       OF
114600911018     C                   END
114700090522     c* Se non trovata la bolla (per colli doppi)
114800090522    1c                   if        NoBolla='S'
114900090522     c                   except    DetNObo
115000090522     C                   EXCEPT    DETdoppio
115100090522   x1c                   else
115200911018     C*
115300950810     C     COMSPE        IFNE      ARBSPE
115400911018     C                   SETON                                        05
115500950810     C                   MOVEL     ARBSPE        COMSPE
115600950810     C                   ENDIF
115700911018     C* DATA SPEDIZIONE
115800911018     C   05              DO
115900941018     C                   Z-ADD     ARBAAS        COMAAS                         ANNO (4 CIFRE)
116000941018     C                   MOVE      ARBDSP        G02INV                         AAAA/MM/GG
116100911018     C                   MOVE      *ZEROS        G02DAT
116200911018     C                   MOVEL     3             G02ERR
116300941017     C                   CALL      'XSRDA8'
116400911018     C                   PARM                    WLBDAT
116500941018     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
116600941018     C*
116700941018     C* CODICE PADRONCINO CONSEGNA DECODIFICA
116800021203     C*    ARBPDC        CHAIN     FNAPD01L                           06
116900021205     C                   move      arbpdc        kpdpdr
117000021203     C                   move      'A'           kpdtip
117100021203     C     kfapd         chain     fiapd01L                           06
117200941018     C  N06              MOVEL     APDRSC        DESCPC            9
117300100707     c
117400100707     c* se non c'e' paadroncino consegna scrivo se Fermo deposito
117500100707     c                   if        *in06
117600100707     C                   MOVEL     *BLANKS       DESCPC
117700100707     c                   if        arbffd='S'
117800100707     c                   eval      descpc='FDeposito'
117900100707     c                   endif
118000100707     c                   endif
118100920409     C*
118200911018     C* SEGNACOLLI NON CONSECUTIVI
118300911018     C     ARBFNS        COMP      'S'                                    04
118400911018     C* SERIE>< 0
118500911018     C     ARBNRS        COMP      0                                  0303
118600911018     C                   END
118700920409     C*
118800920708     C* 07 ON - MANCA DATA ARRIVO: SPEDIZIONE FORZATA A 3
118900920708     C     ARTDAM        IFEQ      0
119000920708     C                   SETON                                        07
119100920708     C                   ELSE
119200920708     C                   SETOFF                                       07
119300920409     C* DATA ARRIVO MERCE  SEGNACOLLO
119400941018     C                   MOVE      ARTDAM        G02INV                         AAAA/MM/GG
119500920409     C                   MOVE      *ZEROS        G02DAT
119600920409     C                   MOVEL     3             G02ERR
119700941017     C                   CALL      'XSRDA8'
119800920409     C                   PARM                    WLBDAT
119900941018     C                   Z-ADD     G02DAT        STADAM                         GG/MM/AAAA
120000920708     C                   END
120100040603     C* MITTENTE E DESTINATARIO DI 17 E 20
120200040603     C                   MOVEL     ARBRSM        STARSM           17
120300920707     C                   MOVEL     ARBRSD        STARSD           20
120400040603     c* collo di valore
120500040603     c                   movel     §ar5bva       stabva            1
120600910522     C*
120700900622     C* STAMPO DETTAGLIO
120800100706     C                   EXCEPT    DET
120900100707     c                   select
121000100707     c                   when      *in12
121100100706     C                   EXCEPT    DETdoppio
121200100707     c                   when      *in16
121300100707     C                   EXCEPT    DETspuIMA
121400100707     c                   other
121500100706     C                   EXCEPT    DETerr
121600100707     c                   endsl
121700070418     c
121800070418     c* se in distinta , stampo
121900090522    2c                   if        not *in12
122000081014
122100100706    3c                   if        *in05 and arbndc>0 and not *in16
122200070418     C                   MOVE      ARbddc        G02INV                         AAAA/MM/GG
122300070418     C                   MOVE      *ZEROS        G02DAT
122400070418     C                   MOVEL     3             G02ERR
122500070418     C                   CALL      'XSRDA8'
122600070418     C                   PARM                    WLBDAT
122700070418     C                   Z-ADD     G02DAT        STAddc            8 0          GG/MM/AAAA
122800070418     C                   EXCEPT    DETndc
122900090522    3c                   endif
123000050405     c
123100050405     c* Se c'e' ubicazine collo, per 05 on lo stampo
123200050405     c                   if        *in05 and §ar5zng<>*blanks
123300050405     c                   seton                                        24
123400050405     c                   endif
123500050405     c
123600100707     C                   SETOFF                                       05
123700050405     c
123800990212     C* STAMPO TUTTE LE C.A. PRESENTI SULLA SPEDIZIONE
123900040806      * Sostituisco la lettura del file FNDTC02 con
124000040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
124100040806      * legate alla spedizione che passo
124200040806      *
124300040806     c                   clear                   fidn12ds
124400040806     c                   eval      i12aas = arbaas
124500040806     c                   eval      i12lnp = arblnp
124600040806     c                   eval      i12nrs = arbnrs
124700040806     c                   eval      i12nsp = arbnsp
124800040806     c                   eval      i12fel = 'E'
124900040806     c                   eval      i12fan = 'E'
125000040806      *
125100040806     c                   call      'FIDN12R'
125200040806     c                   parm                    fidn12ds
125300040806      *
125400040806      * se non ci sono errori
125500090522    3c                   if        o12err = ' '
125600040806      * se numero di CA trovate maggiore di zero
125700090522    4c                   if        o12nca > 0
125800100706     c
125900050405     c                   seton                                        25
126000040806     c
126100040806     c                   z-add     *zeros        ii                2 0
126200100706    6c     1             do        o12nca        ii
126300040806     c                   movea     skkey(ii)     dskey
126400040806     C                   Z-ADD     dsfil         dctfil
126500040806     C                   Z-ADD     dsnca         dctnca
126600040806     C                   Z-ADD     skdca(ii)     dctdca
126700040806     C                   Z-ADD     skdch(ii)     dctdch
126800040806     C                   movea     skcch(ii)     dctcch
126900040806     c                   exsr      preca
127000050405     c                   except    detca
127100050405     c
127200050405     c                   setoff                                       24
127300100706    6C                   ENDDO
127400040806      *
127500100706    4c                   endif
127600090522    3c                   endif
127700050405     c
127800050405     c* Stampo ubicazione collo se non ancora stampata
127900090522    3c                   if        *in24
128000050405     c                   except    detca
128100090522     c                   setoff                                       24
128200090522    3c                   endif
128300090522     c                   setoff                                       25
128400081014     c
128500090522    2c                   endif
128600090522    1c                   endif
128700900622     C*
128800900622     C                   ENDSR
128900990212     C**************************************************************************
129000990212     C* PREPARO I DATI DELLA C.A.
129100990212     C**************************************************************************
129200990212     C     PRECA         BEGSR
129300990212     C                   SETOFF                                       26
129400990303     C                   CLEAR                   STACCH
129500990212     C* DATA APERTURA C.A.
129600990212     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
129700990212     C                   Z-ADD     *ZERO         G02DAT
129800990212     C                   MOVEL     '3'           G02ERR
129900990212     C                   CALL      'XSRDA8'
130000990212     C                   PARM                    WLBDAT
130100990212     C                   MOVE      G02DAT        W0020             2 0
130200990212     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
130300990212     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
130400990303     C** DATA CHIUSURA C.A.
130500990212     C                   CLEAR                   STACHC
130600990303    1C     DCTDCH        IFGT      0
130700990212     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
130800990212     C                   Z-ADD     *ZERO         G02DAT
130900990212     C                   MOVEL     '3'           G02ERR
131000990212     C                   CALL      'XSRDA8'
131100990212     C                   PARM                    WLBDAT
131200990212     C                   MOVE      G02DAT        W0020             2 0
131300990212     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
131400990212     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
131500990212     C                   SETON                                        26
131600990303     C* CAUSALE CHIUSURA
131700990303    2C     DCTCCH        IFNE      *BLANKS
131800990303     C                   CLEAR                   DCCH
131900990303     C                   CLEAR                   DSBS02
132000990303     C                   MOVEL     'C'           T02MOD
132100990303     C                   MOVEL     KNSIF         T02SIF
132200990303     C                   MOVEL     'CCH'         T02COD
132300990303     C                   MOVEL     DCTCCH        T02KE1
132400990303     C                   CALL      'TIBS02R'
132500990303     C                   PARM                    KPJBA
132600990303     C                   PARM                    DSBS02
132700990303     C*
132800990303    3C     T02ERR        IFEQ      ' '
132900990303     C                   MOVEL     T02UNI        DCCH
133000990303    3C                   ENDIF
133100990303     C                   MOVEL     §CHDES        STACCH           12
133200990303    2C                   ENDIF
133300990303    1C                   ENDIF
133400990212     C                   ENDSR
133500941017     C**************************************************************************
133600081014     C* RIEPILOGO SPUNTE PER PISTOLA e controllo se colli doppi
133700941017     C**************************************************************************
133800920813     C     RIEPIL        BEGSR
133900081014     C                   SETON                                        12
134000941017     C*
134100920813     C                   Z-ADD     1             C
134200920813     C* CONTO SPUNTE PER PISTOLA
134300070118     C     KBRV          SETLL     FNBRV01L
134400070118     C     KBRV          READE     FNBRV01L                               30
134500920813     C*
134600950810     C     *IN30         DOWEQ     *OFF
134700081014     c                   if        brvtsu<>*blanks
134800081014     c                   clear                   fnlrv7ds
134900081014     c                   eval      ilrv7fls=brvlnp
135000081014     c                   eval      ilrv7lna=brvlna
135100081014     c                   eval      ilrv7nrs=brvnrs
135200081014     c                   eval      ilrv7nsc=brvnsc
135300081014     c                   eval      kpjbu= fnlrv7ds
135400081014     c                   call      'FNLRV7R'
135500081014     c                   parm                    kpjba
135600081014     c
135700081014     c                   eval      fnlrv7ds=kpjbu
135800081014     c                   if        olrv7nric>=3 and olrv7part='S'
135900081014     c* Se ho già stampato qualche cosa, lascio la prima volta una
136000081014     c*  riga vuota
136100081014     c                   if        *in01 and not *inOF and uno=' '
136200081014     c                   except    DETvuota
136300081014     c                   endif
136400090522     c                   clear                   nobolla
136500081014     c* chain la bolla e stampo collo doppio
136600081014     c     kartB         chain     fnart27l
136700090522    2c                   if        %found(fnart27l)
136800081014     c     karb          chain     fnarb01l
136900090522   x2c                   else
137000090522     c     kartB         chain     fnblt27l
137100090522    3c                   if        %found(fnart27l)
137200090522     c     karb          chain     fnblp01l
137300090522   x3c                   else
137400090522     c                   eval      Nobolla='S'
137500090522    3c                   endif
137600090522    2c                   endif
137700081014     c
137800081014     c                   exsr      ERRORE
137900081014     c                   eval      UNO='S'
138000081014     c                   endif
138100081014     c
138200081014     c                   endif
138300081014     c
138400941019     C                   Z-ADD     1             B                 2 0
138500920813     C                   MOVEL     BRVNPS        COMNPS            2
138600920813     C     COMNPS        LOOKUP    NPS(B)                                 31
138700920813     C  N31              MOVEL     BRVNPS        NPS(C)
138800920813     C  N31              Z-ADD     C             B
138900941019     C  N31              ADD       1             C                 2 0
139000920813     C*
139100920813     C                   ADD       1             CPS(B)
139200920813     C*
139300070118     C     KBRV          READE     FNBRV01L                               30
139400950810     C                   ENDDO
139500081014     C
139600081014     C                   SETOFF                                       12
139700081014     C*
139800081014     C* SE NESSUNA ANOMALIA STAMPO TUTTO O.K.
139900081014     C  N01              EXCEPT    TES
140000081014     C  N01              EXCEPT    OK
140100081014     C  N01              SETOFF                                       OF
140200920813     C**
140300920813     C* SOMMO TUTTI I COLLI
140400920813     C                   XFOOT     CPS           TOTSPU            9 0
140500920813     C**
140600920813     C* STAMPA SPUNTE
140700920813     C* TESTATA
140800920813     C   OF              EXCEPT    TES
140900920813     C   OF              SETOFF                                       OF
141000920813     C*
141100920813     C                   EXCEPT    TOT1
141200920813     C*
141300920813     C                   Z-ADD     1             C
141400920813     C     CPS(C)        DOWGT     0
141500920813     C   OF              DO
141600920813     C                   EXCEPT    TES
141700920813     C                   EXCEPT    TOT1
141800920813     C                   SETOFF                                       OF
141900920813     C                   END
142000920813     C*
142100920813     C                   EXCEPT    TOT2
142200920813     C                   ADD       1             C
142300920813     C                   END
142400920813     C*
142500920813     C                   EXCEPT    TOT3
142600941017     C*
142700920813     C                   ENDSR
142800941017     C**************************************************************************
142900941017     C*   R O U T I N E     I N I Z I A L E
143000941017     C**************************************************************************
143100941017     C     *INZSR        BEGSR
143200941017     C*
143300000126     C     KBRV          KLIST
143400000126     C                   KFLD                    IMANPG
143500000126     C                   KFLD                    WNFV
143600000126     C                   KFLD                    VIDFGS
143700070118     C     KBRV1         KLIST                                                  FNBRV01L
143800941019     C                   KFLD                    IMANPG                         ..CATEGORIA
143900000126     C                   KFLD                    WNFV                           ..NR. FOGLIO
144000950316RM*  C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
144100941019     C                   KFLD                    ARTFLS                         ..LNP
144200941018     C                   KFLD                    ARTLNA                         ..LNA
144300941018     C                   KFLD                    ARTNRS                         ..SERIE
144400941018     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
144500070418     C     KBRV4         KLIST                                                  FNBRV01L
144600070418     C                   KFLD                    CONNPG                         ..CATEGORIA
144700070418     C                   KFLD                    ARTFLS                         ..LNP
144800070418     C                   KFLD                    ARTLNA                         ..LNA
144900070418     C                   KFLD                    ARTNRS                         ..SERIE
145000070418     C                   KFLD                    ARTNSC                         ..SEGNACOLLO
145100070418     C                   KFLD                    SAVFGS                         ..FIL.GESTIONE
145200990212RM*  C     KARTD         KLIST
145300990212RM*  C                   KFLD                    ARTFLS
145400990212RM*  C                   KFLD                    ARTLNA
145500990212RM*  C                   KFLD                    ARTNRS
145600990212RM*  C                   KFLD                    ARTNSC
145700081014     C     KARTb         KLIST
145800081014     C                   KFLD                    brvlnp
145900081014     C                   KFLD                    brvlnA
146000081014     C                   KFLD                    brvNRS
146100081014     C                   KFLD                    brvNSC
146200941017     C     KFVV          KLIST                                                  *
146300941019     C                   KFLD                    IMANPG                         *
146400941017     C                   KFLD                    COMNFV                         *
146500941028     C                   KFLD                    VIDFGS                         *
146600950316RM*  C     KFVV2         KLIST
146700950316RM*  C                   KFLD                    BRVNPG
146800000126RM*  C                   KFLD                    WNFV5
146900950316RM*  C                   KFLD                    BRVFGS
147000941017     C     KANM          KLIST                                                  FNANM02L
147100941017     C                   KFLD                    ARTFLS                         *
147200941017     C                   KFLD                    ARTLNA                         *
147300941017     C                   KFLD                    ARTNRS                         *
147400941018     C                   KFLD                    ARTNSC                         *
147500941017     C                   KFLD                    ANMCAA                         *
147600941017     C     KART          KLIST                                                  *
147700941017     C                   KFLD                    ARBAAS                         *
147800941017     C                   KFLD                    ARBLNP                         *
147900941017     C                   KFLD                    ARBNRS                         *
148000941017     C                   KFLD                    ARBNSP                         *
148100081014     C     KARB          KLIST                                                  *
148200081014     C                   KFLD                    ARtAAS                         *
148300081014     C                   KFLD                    ARtLNP                         *
148400081014     C                   KFLD                    ARtNRS                         *
148500081014     C                   KFLD                    ARtNSP                         *
148600040603     C     Kar5          KLIST                                                  *
148700040603     C                   KFLD                    ARBAAS                         *
148800040603     C                   KFLD                    ARBLNP                         *
148900040603     C                   KFLD                    ARBNRS                         *
149000040603     C                   KFLD                    ARBNSP                         *
149100040603     C                   KFLD                    ktrd                           *
149200970721     C     KGCA          KLIST                                                  *
149300970721     C                   KFLD                    ARBAAS                         *
149400970721     C                   KFLD                    ARBLNP                         *
149500970721     C                   KFLD                    ARBNRS                         *
149600970721     C                   KFLD                    ARBNSP                         *
149700990212     C                   KFLD                    KFRG
149800950810     C     KSAV          KLIST                                                  *
149900950810     C                   KFLD                    SAVAAS                         *
150000950810     C                   KFLD                    SAVLNP                         *
150100950810     C                   KFLD                    SAVNRS                         *
150200950810     C                   KFLD                    SAVNSP                         *
150300941017     C     KLBL          KLIST                                                  *
150400941017     C                   KFLD                    LBLAAN                         *
150500941017     C                   KFLD                    LBLLPN                         *
150600941017     C                   KFLD                    LBLNRN                         *
150700941017     C                   KFLD                    LBLNSN                         *
150800941017     C     KTAB          KLIST
150900941017     C                   KFLD                    CODUT
151000941017     C                   KFLD                    COD
151100941017     C                   KFLD                    KEY
151200050510     C     KTAB2         KLIST
151300050510     C                   KFLD                    CODUT
151400050510     C                   KFLD                    COD
151500990212     C     KDCD          KLIST
151600990212     C                   KFLD                    DCDAAC                         *
151700990212     C                   KFLD                    DCDFIL                         *
151800990212     C                   KFLD                    DCDNCA                         *
151900021203
152000021203     C     Kfapd         klist
152100021203     C                   kfld                    kpdtip
152200021203     C                   kfld                    kpdpdr
152300941017     C*
152400970721     C     *LIKE         DEFINE    GCPFRG        KFRG
152500970721     C                   MOVEL     0             KFRG
152600941017     C     *LIKE         DEFINE    TBLCOD        COD
152700941017     C     *LIKE         DEFINE    TBLKEY        KEY
152800941019     C     *LIKE         DEFINE    BRVNPG        IMANPG                         CATEGORIA FOGLIO
152900070418     C     *LIKE         DEFINE    BRVNPG        conNPG                         CATEGORIA FOGLIO
153000031218     C     *LIKE         DEFINE    BRVNFV        WNFV
153100031218     C     *LIKE         DEFINE    FVVNFV        WNFV5
153200941018     C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO VIAGGIO
153300941018     C     *LIKE         DEFINE    ARTDAM        STADAM                         DATA ARRIVO MERCE
153400941018     C     *LIKE         DEFINE    ARBSPE        COMSPE
153500941019     C     *LIKE         DEFINE    ARBLNA        KLNA                           LINEA ARRIVO
153600031218     C     *LIKE         DEFINE    FVVFGS        SAVFGS
153700950317RM*  C     *LIKE         DEFINE    FVVDFV        SAVDFV
153800941017     C*
153900101220      /free
154000101220       // Carico le categorie con ambito Arrivo
154100101220       xx=2   ;
154200101220       // Alla posizine 1 metto al defluenza
154300101220       npgarr(1)='1D'  ;
154400101220
154500101220       setll   (1:'7N') tabel00f   ;
154600101220       reade   (1:'7N') tabel00f   ;
154700101220
154800101220       dow not %eof(tabel00f)      ;
154900101220       if tblflg=' '               ;
155000101220       ds7n=tbluni                 ;
155100101220
155200101220       if §7ntbo<>' '              ;
155300101220
155400101220          if §7ntbo='A'               ;
155500101220          npgArr(xx)=tblkey;
155600101220          xx=xx+1  ;
155700101220          endif  ;
155800101220       else   ;
155900101220
156000101220         if  §7ntb1='A'                         ;
156100101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc1   ;
156200101220         xx=xx+1  ;
156300101220         endif                                  ;
156400101220         if  §7ntb2='A'                         ;
156500101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc2   ;
156600101220         xx=xx+1  ;
156700101220         endif                                  ;
156800101220         if  §7ntb3='A'                         ;
156900101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc3   ;
157000101220         xx=xx+1  ;
157100101220         endif                                  ;
157200101220         if  §7ntb4='A'                         ;
157300101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc4   ;
157400101220         xx=xx+1  ;
157500101220         endif                                  ;
157600101220         if  §7ntb5='A'                         ;
157700101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc5   ;
157800101220         xx=xx+1  ;
157900101220         endif                                  ;
158000101220         if  §7ntb6='A'                         ;
158100101220         npgarr(xx)=%subst(tblkey:1:1)+§7nsc6   ;
158200101220         xx=xx+1  ;
158300101220         endif                                  ;
158400101220
158500101220       endif                       ;
158600101220       endif                       ;
158700101220
158800101220       reade   (1:'7N') tabel00f   ;
158900101220       enddo                       ;
159000101220      /end-free
159100101220
159200941017     C                   ENDSR
159300101202
159400101202      *---------------------------------------------------------------*
159500101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
159600101202      *---------------------------------------------------------------*
159700101202     c     sr_DatiJob    BEGSR
159800101202      *
159900101202     c     *dtaara       define    §azute        AZUTEds
160000101202     c     *dtaara       define    §datiute      dDatiUte
160100101202      *
160200101202     c                   in(E)     *dtaara
160300101202     c                   IF        %Error or RSUT = *blanks
160400101202     c                   clear                   TIBS34ds
160500101202     c                   call      'TIBS34R'
160600101202     c                   parm                    TIBS34ds
160700101202     c                   in        *dtaara
160800101202     c                   ENDIF
160900101202      *
161000101202     c                   ENDSR
161100941017     C**************************************************************************
161200950316RM*  OPRTF198   E            TES              02
161300911015     O                       STA(1)              66
161400950810     O              N10      STA(2)             132
161500950810     O               10      STA(18)            132
161600950320     O                       STA(17)            198
161700900626     O                       RSUT                20
161800960927     O                       UDATE8             181 '  /  /    '
161900950320     O                       PAGE          Z    198
162000920813     O          E            TES         1
162100160503     o                       statipo             67
162200160503     o                       statipo2           130
162300050511     O                       UTIME              179 '  :  :  '
162400931228     O               02      STA(4)              70
162500931228     O          E    02      TES         0  0
162600931228     O                       STA(4)              70
162700931228     O          E            TES         2
162800911015     O                       STA(3)              66
162900931228     O                       STA(16)            132
163000060508     O                       STA(20)            198
163100060508     O                       VIDTIN              12
163200060508RM*  O                       SAVFGS              21
163300060508     O                       DESCDU              42
163400920813     O                       COMNFV        Z     56
163500950317     O                       STADFV        8     73
163600960927     O                       VIDFIL             116
163700960927     O                       DESFIL             137
163800060508     O                       stalna             177
163900060508     O                       DESlna             198
164000070418     O          E   n10      TES         1
164100070418     O                       STA(21)            198
164200070418     O                       stamdc             188
164300101215     O                       staimaa            146
164400920813     O          E            TES2        2
164500911018     O                       STA(5)              66
164600911018     O                       STA(8)             132
164700040621     O                       STA(19)            198
164800920813     O          E            TES2        1  1
164900911018     O                       STA(6)              66
165000911018     O                       STA(9)             132
165100911015     O*
165200090522     O          E            DETnoBO     1
165300090522     O                       brvLNP        Z      8
165400090522     O                       brvNRS        Z     11
165500090522     O                       brvLNA        Z     29
165600090522     O                       brvZNC              87
165700090522     O                       brvnps             122
165800090522     O                       brvNSC        Z    131
165900090522     O
166000090522     O          E            DET         1
166100090522     O                  05   ARBAAS        Z
166200090522     O                  05   ARBLNP        Z   +  1
166300090522     O                  05 03ARBNRS        Z   +  1
166400090522     O                  05 03                  +  0 '-'
166500090522     O                  05   ARBNSP        Z   +  0
166600090522     O                  05   STADSP            +  1 '  /  '
166700090522     O                  05   ARBLNA        Z   +  1
166800090522     O                  05   STARSM            +  1
166900090522     O                  05   STARSD            +  1
167000090522     O                  05N04ARBNCD        Z   +  1
167100090522     O                  05N04                  +  0 '/'
167200090522     O                  05N04ARBNCA        Z   +  0
167300090522     O                  05 04                    84 'NON SEQUENZIALI'
167400090522     O                  05   ARBZNC            +  1
167500090522     O                  05   stabva            +  0
167600090522     O                  05   ARBTMC            +  1
167700090522     O                  05N06ARBPDC        Z   +  1
167800090522     O                  05N06                  +  0 '-'
167900100707     O                  05   DESCPC            +  0
168000090522     O              N07      STADAM            +  1 '  /  /    '
168100090522     O              N07      ARTNAP            +  1
168200090522     O              N07      ARTNSC        Z   +  2
168300090522     O               07                         131 'ABILIT.MERCE FORZATA'
168400081014     O* ERRORI:
168500950411     O*
168600081014     O          E            DETERR      0  0
168700031219     O                    n13                   155 'NON SPUNTATO IN DATA '
168800031219     O                    n13STADFV        8   +  1
168900950316     O*
169000040621     O                     13brvFGS             137
169100040621     O                     13                   138 '-'
169200040621     O                     13ST2FGS             146
169300040621     O                     13                   149 'IN'
169400070118     O                     13ST2CAT             153
169500070118     O                     13brvNPG             155
169600070118     O                     13                   156 '-'
169700070118     O                     13brvNFV        Z    162
169800070118     O                     13                   165 'IL'
169900070118     O                     13ST2DFV             171 '  /  '
170000070118     O                     13                   175 'ORE'
170100070118     O                     13brvHMS             184 '  :  :  '
170200960523     O                     13STADSF             198
170300070418     O          E            DETndc      1
170400070418     O                                           87 'In distinta:'
170500070418     O                       ARBndc        2  +   1
170600070418     O                                        +   2 'del'
170700070418     O                       staddc            +  1 '  /  /    '
170800081014     o* Collo doppio
170900081014     O          E            DETdoppio   0  0
171000081014     O                                          157 'C O L L O   D O P P I O'
171100100706     o* Collo presente da XX giorni in IMA
171200100706     O          E            DETspuIMA   0  0
171300100707     O                                          160 '   COLLO SPUNTATO PER '
171400100707     O                                        +   1 'ALMENO '
171500100706     O                       §VPOGGIMA     2  +   1
171600100707     O                                        +   1 'GIORNI in I.M.A.'
171700081014     o* riga vuota
171800081014     O          E            DETvuota    2
171900081014     O                                          157 ' '
172000050405     O* STAMPO NUMERO C.A.e ubicazione collo
172100990212     O          E            DETCA       1
172200050405     O               24                          87 'Ubicazione collo:'
172300050405     O               24      §ar5zng           +  2
172400050405     O               25                         138 'C.A.'
172500050405     O               25      DCTFIL            +  1
172600050405     O               25      DCTNCA        Z   +  1
172700050405     O               25                        +  1 'del'
172800050405     O               25      STADCA            +  1 '  /  /  '
172900990212     O                     26                  +  1 'chiusa il'
173000990212     O                     26STACHC            +  1 '  /  /  '
173100990303     O                     26DCTCCH            +  1
173200990303     O                     26                  +  0 '-'
173300990303     O                     26STACCH            +  0
173400920813     O          E            OK          3
173500920813     O                       STA(11)             66
173600920813     O          E            OK          0  0
173700920813     O                       STA(11)             66
173800920813     O*
173900920813     O          E            TOT1        3  1
174000920813     O                       STA(13)             66
174100920813     O          E            TOT2           1
174200920813     O                       NPS(C)              33
174300920813     O                       CPS(C)        2     58
174400920813     O          E            TOT3           1
174500920813     O                       STA(14)             66
174600920813     O          E            TOT3           1
174700920813     O                       STA(15)             66
174800920813     O                       TOTSPU        2     58
174900920813     O*
175000920813     O          E            TOT3        3
175100920813     O                       STA(12)             66
175200920813     O          E            TOT3        0  0
175300920813     O                       STA(12)             66
175400941017     O**************************************************************************
175500950316** STA
175600060508XXXXXXXXXXXXXXXXXXXX                                                 1
175700950320 ** I.M.A. : INVENTARIO MAGAZZINO ARRIVI **                          2
175800060508FOGLIO I.M.A. di  XXX-XXXXXXXXXXXXXXXXXXXX    N.ro XXXXX   del XX/   3
175900931228                                       *** D E F I N I T I V O ***   4
176000040603    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO        5
176100941019ANNO LNP SER NUMERO  DATA ARR                                        6
176200040603XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx  7
176300040603     SEGNACOLLI    ZON ULT  AUTOTRASPORT.   A R R I V O SEGNACOLLO   8
176400040603      DAL /   AL   ARR CONS   CONSEGNA       DATA   PIST MANCANTE    9
176500040603XX XXXXXXX/XXXXXXX XXV X XXXXXXX-XXXXXXXXX XX/XX/XXXX XX  XXXXXXX   10
176600920813             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
176700920813 **  FINE  STAMPA  **
176800920813   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI      13
176900920813                        ---------------------------------------     14
177000920813               TOTALE                                               15
177100060508XX/XXXX           Punto Operativo in Gestione: XXX-xxxxxxxxxxxxxxx  16
177200950320                         FNLR31R                          PAG.XXXX  17
177300950810 ** I.M.G. : INVENTARIO MAGAZZINO GIACENZE **                       18
177400040621 S  P  U  N  T  A  T  O      D  A                                   19
177500060508xxxxx           Linea Arrivo Selezionata: XXX-xxxxxxxxxxxxxxxxxxxx  20
177600070418                Elabora anche spedizioni in distinta: XX            21
177700941019**
177800950316OVRPRTF FILE(PRTF198) USRDTA('I.M.A.')
