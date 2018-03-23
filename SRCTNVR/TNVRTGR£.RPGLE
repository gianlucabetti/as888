000100921203     H DECEDIT('0,') DATEDIT(*DMY.)
000200990628     H* FNLGT2R *----------------------------------------------------*
000300921203     H*---------*                                                    *
000400921203     H*  RICEZIONE GIACENZE APERTE IN ARRIVO                         *
000500921203     H*--------------------------------------------------------------*
000600990628     H*  01 = LETTURA FIGCA00R/FIGNA00R                              *
000700940224     H*  09 = NON UTILIZZATO                                         *
000800990628     H*  10 = LOOP DI LETTURA FIGCP01L/FIGNP01L                      *
000900940224     H*  20 = INDICATORE DI COMODO                                   *
001000940224     H*  33 = CHAIN SU TABELLA 3K                                    *
001100940224     H*  34 = CHAIN SU FLBLP25C                                      *
001200990628     H*  40 = CHAIN SU FIGCP01L X AGGIORNAMENTO PROGGR. GAICENZA     *
001300990628     H*  50 = GESTIONE FIGCA SE ON/GESTIONE FIGNA SE OFF             *
001400990628     H*  55 = CHAIN FIGCP01L                                         *
001500940224     H*--------------------------------------------------------------*
001600990628     FFIGCA00R  UF   E             DISK    USROPN
001700990628     FFIGCP00F  O    E             DISK
001800990628     F                                     RENAME(FIGCP000:FIGCP0F)
001900990628     FFIGCP01L  UF   E           K DISK
002000990628     F                                     RENAME(FIGCP000:FIGCP1L)
002100031030     FFIGNP01L  UF a E           K DISK
002200990628     FFIGNA00R  UF   E             DISK    USROPN
002300990628     F                                     RENAME(FIGNP000:FIGNA000)
002400990628     FFIGNP00F  O    E             DISK
002500990628     F                                     RENAME(FIGNP000:FIGNP001)
002600941013     FCMRIC01L  UF   E           K DISK
002700941013     FFNBLP01L  UF   E           K DISK
002800040116     FFNar901L  iF   E           K DISK
002900911017     FTABEL00F  IF   E           K DISK
003000040304     FFNcls01L  iF   E           K DISK
003100040304     Ftitgc01L  iF   E           K DISK
003200941013     H*--------------------------------------------------------------*
003300900604     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
003400900604     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
003500900604     D CMD4            S             55    DIM(1) CTDATA PERRCD(1)
003600911018     D CMD11           S             55    DIM(1) CTDATA PERRCD(1)
003700911018     D CMD13           S             55    DIM(1) CTDATA PERRCD(1)
003800911018     D CMD14           S             55    DIM(1) CTDATA PERRCD(1)
003900031030     D DIS             S              1    DIM(50) CTDATA PERRCD(50)
004000031030     D ARR             S              1    DIM(50) CTDATA PERRCD(50)
004100941013     H*--------------------------------------------------------------*
004200000000     D KPJBA         E DS
004300900604     D  LIBSYS                92    101
004400900604     D  LEGFIL               483    492
004500900604     D  MBRFIL               493    502
004600000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
004700990628     D FNLGT2        E DS                  EXTNAME(FNLGT2DS)
004800990628     D SAV           E DS                  EXTNAME(FIGCA00R)
004900911016     D                 DS
005000990628     D  SALVA                  1    202
005100921203     D DS3K          E DS
005200941013     D DS7Q          E DS
005300040115     D DS2z          E DS
005400040116     D DS3a          E DS
005500040831     D DS2f          E DS
005600040304     D fnlv51ds      E DS
005700040304     D trul27ds      E DS
005800040831     d* codici com.giac.
005900040831     D C2F             S              1    DIM(50)
006000941013     D WLBDA8          DS
006100941013     D  G02DAT                 1      8  0
006200941013     D  G02INV                 9     16  0
006300941013     D  G02ERR                17     17
006400941013     D  G02TGI                18     22  0
006500000000     C*---------------------------------------------------------------*
006600900604     C* APERTURA MEMBRO DA TRASCODIFICARE
006700900604     C                   EXSR      CAEXA
006800000000     C*---------------------------------------------------------------*
006900900604     C* LETTURA
007000000000     C     SU            TAG
007100990628     C                   READ      FIGCA00R                               01
007200911018     C   01              GOTO      FIN
007300911016     C* MI SALVO IL RECORD LETTO
007400940125     C                   MOVEL     SAV           SALVA
007500950117     C                   Z-ADD     0             CODCLI            7 0
007600941013     C                   Z-ADD     GCPDDM        SAVDDM            8 0
007700990628     C                   Z-ADD     GCPDED        SAVDED            8 0
007800040517     C                   Z-ADD     GCPDcg        SAVDcg            8 0
007900941013     C                   MOVE      GCPDIS        SAVDIS            1
008000920211     C                   MOVE      GCPPSG        SAVPSG            1
008100920211     C                   MOVE      GCPASG        SAVASG            1
008200920211     C                   MOVE      GCPNPC        SAVNPC            1
008300920211     C                   MOVE      GCPVCS        SAVVCS            1
008400941013     C                   Z-ADD     GCPCAS        SAVCAS           13 3
008500941013     C                   MOVE      GCPVCA        SAVVCA            3
008600920211     C                   MOVE      GCPPPC        SAVPPC            1
008700920211     C                   MOVE      GCPAPC        SAVAPC            1
008800941013     C                   Z-ADD     GCPDUR        SAVDUR            8 0
008900990628     C                   Z-ADD     GCPDTC        SAVDTC            8 0
009000990628     C                   MOVE      GCPFTC        SAVFTC            1
009100920331     C* MI SALVO NUOVI DATI GIACENZA PER VERIFICARE SE CAMBIATO RI-
009200920331     C* SPETTO ALLA PARTENZA
009300920331     C                   Z-ADD     GCPNGC        CNTNGC            7 0
009400920331     C                   Z-ADD     GCPFGC        CNTFGC            3 0
009500941013     C                   Z-ADD     GCPAGC        CNTAGC            4 0
009600950209     C                   Z-ADD     GCPMGC        CNTMGC            4 0
009700911015     C* CONTROLLO SE LA GIACENZA RICEVUTA E' UNA RIAPERTURA, IN QUESTO
009800911015     C* CASO QUELLA GIA' PRESENTE DIVENTA CON PROGRESSIVO + 1 RISPETTO
009900911016     C* A QUELLI GIA' PRESENTI POI MODIFICO ANCHE PROGRESSIVO NOTE
010000911104     C                   Z-ADD     GCPAAS        AAS
010100911104     C                   Z-ADD     GCPLNP        LNP
010200911104     C                   Z-ADD     GCPNRS        NRS
010300911104     C                   Z-ADD     GCPNSP        NSP
010400911104     C*
010500911016     C                   Z-ADD     0             FRG
010600031030     C                   Z-ADD     0             CNT
010700031030      * -------------------------------------------
010800031030      *  Fase 10 -> Apertura giacenza in arrivo
010900031030      * -------------------------------------------
011000911216     C     GCPFAS        IFEQ      10
011100031030      *
011200950117     C                   CLEAR                   DS7Q
011300941013     C* SE PREVISTO IN TABELLA INVIO INFORMAZIONI BOLLE GIACENTI AL CLIENTI
011400941013     C* SFLEGGO LA BOLLA
011500941013     C     KGIA1         CHAIN     FNBLP01L                           34
011600941013     C     *IN34         IFEQ      '0'
011700941013     C                   MOVEL     '3K'          COD
011800941013     C                   MOVEL     *BLANKS       KEY
011900941013     C     BLPCCM        IFNE      0
012000941013     C                   MOVEL     BLPCCM        KEY
012100941013     C                   ELSE
012200941013     C                   MOVEL     BLPKSC        KEY
012300941013     C                   END
012400941013     C     KTAB          CHAIN     TABEL                              33
012500950419     C*
012600941013     C  N33              DO
012700941013     C                   MOVEL     TBLUNI        DS3K
012800941013     C     §3KCQ5        IFEQ      'S'
012900941013     C  N34              MOVEL     *BLANKS       BLPFT3
013000941013     C  N34              EXCEPT    AGGI
013100941013     C                   END
013200941013     C                   END
013300950117     C*  Se è una apertura prendo il codice cliente dalla bolla
013400950117     C     GCPDUR        IFEQ      0
013500950117     C     BLPCCM        IFEQ      0
013600950117     C                   Z-ADD     BLPKSC        CODCLI
013700950117     C                   ELSE
013800950117     C                   Z-ADD     BLPCCM        CODCLI
013900950117     C                   END
014000950117     C                   END
014100941013     C*
014200941013     C*  Se sulla bolla ho impostato un codice di particolarità giacenza
014300941013     C*  mi posiziono sulla tabella 7Q per verificare se devo aprire giac.
014400941013     C                   Z-ADD     0             §7Q5RP
014500030911      *FORZO LA PARTICOLARITÀ GR SE NON C'È PARTICOLARITÀ PER
014600030911      *FARE ESEGUIRE IL RESO DOPO LE 2 RIAPERTURE CONSENTITE
014700030911     C     BLPGGA        IFEQ      *BLANKS
014800030911     C                   MOVE      'GR'          BLPGGA
014900030911     C                   ENDIF
015000030911      *
015100941013     C     BLPGGA        IFNE      *BLANKS
015200941013     C                   MOVE      '7Q'          COD
015300941013     C                   MOVEL     *BLANKS       KEY
015400941013     C                   MOVEL     BLPGGA        KEY
015500941013     C     KTAB          CHAIN     TABEL00F                           33
015600941013     C     *IN33         IFEQ      '0'
015700941013     C                   MOVEL     TBLUNI        DS7Q
015800941013     C                   END
015900941013     C                   END
016000950117     C*
016100950117     C                   END
016200941013     C*
016300990628     C     KGIA          CHAIN     FIGCP01L                           10
016400950209     C     *IN10         IFEQ      '0'
016500950213     C     GCPSCM        IFNE      0
016600950213     C                   Z-ADD     GCPSCM        CODCLI
016700950213     C                   END
016800950209     C     GCPNGC        IFNE      CNTNGC
016900950209     C     GCPFGC        ORNE      CNTFGC
017000950209     C     GCPAGC        ORNE      CNTAGC
017100950209     C                   EXCEPT    LIBER
017200990628     C                   CLEAR                   FNLGT2
017300990628     C                   Z-ADD     CNTAGC        DT2NAG
017400990628     C                   Z-ADD     CNTFGC        DT2NFG
017500990628     C                   Z-ADD     CNTNGC        DT2NNG
017600990628     C                   Z-ADD     CNTMGC        DT2NMG
017700990628     C                   Z-ADD     GCPAGC        DT2VAG
017800990628     C                   Z-ADD     GCPFGC        DT2VFG
017900990628     C                   Z-ADD     GCPNGC        DT2VNG
018000950209     C                   MOVEL     'S'           WCAL91
018100990628     C                   MOVEL     FNLGT2        KPJBU
018200990628     C                   CALL      'FNLGT2R1'
018300950209     C                   PARM                    KPJBA
018400950209     C                   ELSE
018500911015     C     *IN10         DOWEQ     '0'
018600950411     C  N10              ADD       1             CNT               2 0
018700990628     C     KGIA1         READE     FIGCP01L                               10
018800911016     C                   END
018900950209     C*
019000950209     C                   END
019100950209     C                   END
019200031027     C*
019300941013     C*  Controllo se posso riaprire giacenza
019400031027     C**** §7Q5RP**      IFEQ      0
019500031027     C**** CNT*****      ORLT      §7Q5RP
019600031027     C*************
019700031027     C*
019800911016     C     CNT           IFNE      0
019900031030     C*
020000911016     C                   Z-ADD     0             FRG
020100990628     C     KGIA          CHAIN     FIGCP01L                           40
020200031030     C*
020300031030     C*  SE CNT = 15 DELETO ULTIMA GIAC. E IMPOSTO CNT A 14
020400000403     C     CNT           IFEQ      15
020500031030     C*
020600990628     C  N40              DELETE    FIGCP1L
020700961105     C                   SUB       1             CNT
020800961105     C* DELETO ANCHE RELATIVE NOTE
020900961105     C                   Z-ADD     0             FRG
021000990628     C     KNOT          DELETE    FIGNP01L                           10
021100961105     C     *IN10         DOWEQ     '0'
021200990628     C     KNOT          DELETE    FIGNP01L                           10
021300961105     C                   END
021400961105     C*
021500961105     C                   ELSE
021600031030     C*
021700911016     C     1             ADD       CNT           GCPFRG
021800990722     C                   Z-ADD     0             GCPDTR
021900990722     C                   MOVEL     *BLANKS       GCPFTR
022000031030      *
022100031031     c                   clear                   dis_arrivo        1
022200031031     C*
022300031031     C* SE LE DISPOSIZIONI ERANO STATE IMMESSE IN ARRIVO
022400031031     C     SAVDUR        IFlt      GCPDLD
022500031031     C     GCPDLD        OREQ      0
022600031031     C     GCPDIS        ORNE      SAVDIS
022700031031     C     GCPPSG        ORNE      SAVPSG
022800031031     C     GCPNPC        ORNE      SAVNPC
022900031031     C     GCPVCS        ORNE      SAVVCS
023000031031     C     GCPPPC        ORNE      SAVPPC
023100031031     c                   move      'S'           dis_arrivo
023200031031     c                   end
023300031030      *
023400920211     C* SE LE DISPOSIZIONI ERANO STATE IMMESSE IN ARRIVO CARICO ORA
023500920211     C* IN PARTENZA LA DATA DISPOSIONI MITTENTE
023600940316     C* OPPURE DIVERSE IN ARRIVO
023700941013     C     SAVDUR        IFLE      GCPDLD
023800920410     C     GCPDLD        OREQ      0
023900941013     C     GCPDIS        ORNE      SAVDIS
024000940316     C     GCPPSG        ORNE      SAVPSG
024100940316     C     GCPNPC        ORNE      SAVNPC
024200940316     C     GCPVCS        ORNE      SAVVCS
024300940316     C     GCPPPC        ORNE      SAVPPC
024400920211     C*
024500990628     C                   Z-ADD     SAVDTC        GCPDTC
024600990628     C                   MOVE      SAVFTC        GCPFTC
024700941013     C                   Z-ADD     SAVDED        GCPDED
024800941013     C                   Z-ADD     SAVDDM        GCPDDM
024900941013     C                   MOVE      SAVDIS        GCPDIS
025000920211     C                   MOVE      SAVPSG        GCPPSG
025100920211     C                   MOVE      SAVASG        GCPASG
025200920211     C                   MOVE      SAVNPC        GCPNPC
025300920211     C                   MOVE      SAVVCS        GCPVCS
025400941013     C                   MOVE      SAVVCA        GCPVCA
025500941013     C                   Z-ADD     SAVCAS        GCPCAS
025600920211     C                   MOVE      SAVPPC        GCPPPC
025700920211     C                   MOVE      SAVAPC        GCPAPC
025800920211     C*
025900920211     C                   END
026000921102     C  N40              EXCEPT    AGG
026100950412     C*
026200950412     C* RISISTEMO PROGRESSIVO RIAPERTURA GIAC. NELLE NOTE
026300920305     C                   Z-ADD     0             FRG
026400031030     C                   Z-ADD     0             GNPFRG
026500990628     C     KNOT          SETLL     FIGNP01L                             10
026600031030      *
026700920305     C     *IN10         DOWEQ     '0'
026800990628     C     KNOT          READE     FIGNP01L                               10
026900031030      *
027000960208     C     *IN10         IFEQ      '0'
027100960208     C     GNPTPN        ANDNE     'I'
027200960208     C     1             ADD       CNT           GNPFRG
027300990628     C                   UPDATE    FIGNP000
027400920305     C                   END
027500031030      *
027600960208     C                   END
027700031030      *
027800031030     C* >>>>>>>>
027900031030      * se ho preso l'ultimo progressivo delle note
028000031030      *  devo memorizzare le eventuali disposizioni immesse
028100031030      *  dall'arrivo.
028200031031     C     dis_Arrivo    Ifne      *blank
028300031030     c                   exsr      forza_note
028400031030     c                   end
028500031030     C* >>>>>>>>
028600031030     C*
028700961105     C                   END
028800911016     C                   END
028900920703     C*
029000950117     C                   MOVEL     SALVA         SAV
029100950117     C     CODCLI        IFNE      0
029200950117     C                   Z-ADD     CODCLI        GCPSCM
029300950117     C                   END
029400040115      * routine di verifica se disposizioni da arrivo forzate per data cons.
029500040115      * richiesta
029600040322     c                   clear                   azzer
029700040517      * il campo è quello della data chiusura perchè viene forzato per la
029800040517      * trasmissione rischiava in caso di riapertura di contenere il valore
029900040517      * del progressivo precedente
030000040517     c*                  if        gcpded > 0
030100040517     c                   if        savdcg > 0
030200040115     c                   exsr      datarich
030300040322     c                   endif
030400040115      * forzo se data consegna richiesta le disposizioni
030500040115     c                   if        azzer <> *blank
030600040827      * verifico se cliente vario
030700040827     c                   move      gcpscm        cod4              4 0
030800040924      * forzo tipo comunicazione se non impostato
030900040924     c                   if        gcptcm = *blank
031000040924     c                   move      'P'           gcptcm
031100040924     c                   endif
031200040827      * se cliente vario non imposto la data disposizioni in modo che resti
031300040831      * in evidenza alla filiale oppure se per il cliente è gestito un tipo
031400040831      * comunicazione = *blank come tipo invio
031500040924     c                   if        cod4  = 8888 or cod4 = 9999
031600040827     C                   z-add     0             GCPDDM
031700040827     c                   else
031800040827     C                   z-add     dateu         GCPDDM
031900040827     c                   endif
032000040827
032100040517     C                   z-add     savdcg        GCPDed
032200040115     C                   z-add     15            GCPfas
032300040518     C                   z-add     0             GCPDcg
032400040115     C                   MOVE      'C'           GCPDIS
032500040115     c                   else
032600920211     C                   Z-ADD     0             GCPDDM
032700941013     C                   Z-ADD     0             GCPDED
032800040115     C                   MOVE      *BLANKS       GCPDIS
032900040116     C                   MOVE      *BLANKS       GCPPSG
033000040116     C                   MOVE      *BLANKS       GCPASG
033100040116     C                   MOVE      *BLANKS       GCPPPC
033200040116     C                   MOVE      *BLANKS       GCPAPC
033300040115     c                   endif
033400040115
033500040115     C                   Z-ADD     0             GCPFRG
033600960315     C                   MOVEL     'N'           GCPFST
033700920211     C                   MOVE      *BLANKS       GCPNPC
033800920211     C                   MOVE      *BLANKS       GCPVCS
033900941013     C                   MOVE      *BLANKS       GCPVCA
034000950314     C                   MOVE      *BLANKS       GCPTFG
034100941013     C                   Z-ADD     0             GCPCAS
034200920427     C* SE L'APERTURA ERA STATA FATTA LA PRIMA VOLTA IN PARTENZA
034300920427     C* MUOVO DATA APERTURA ARRIVO IN DATA RIAPERTURA E VECCHIA DATA
034400930519     C* AZZERO GCPDLM DATA LETT.CHIUSURA PERCHE IN ARRIVO VIENE UTILIZZATA
034500930519     C* PER STAMPA MATERIALE DA PRELEVARE /PORTARE AL MAG GIAC.
034600930519     C                   Z-ADD     0             GCPDLM
034700990629     C                   Z-ADD     0             GCPDTR
034800990629     C                   MOVEL     *BLANKS       GCPFTR
034900031027      *
035000031027      * Se si supera il limite presente in tabella (7Q) Particolarità
035100031027     C     §7Q5RP        ifNE      *Zeros
035200031028     C     CNT           andGE     §7Q5RP
035300031027     C                   Z-ADD     dateu         gcpDMM
035400031027     C                   Z-ADD     26            gcpFAS
035500031027     C                   MOVE      'R'           gcpDIS
035600031027     C                   MOVE      'R'           gcpTCM
035700040705     C                   Z-ADD     0             gcpddm
035800040705     C                   Z-ADD     0             gcpded
035900031027     c                   end
036000031027      *
036100990628     C                   WRITE     FIGCP0F
036200031027      *
036300031027     C***********        ELSE
036400031027      ***********
036500031027     C** Se non posso riaprire giacenza aggiorno quella in essere impostando
036600031027     C** la fase 26
036700031027     C**** KGIA**        CHAIN     FIGCP01L                           40
036800031027     C***********        Z-ADD     26            GCPFAS
036900031027     C***********        Z-ADD     0             GCPDLD
037000031027     C***********        Z-ADD     0             GCPDDM
037100031027     C***********        Z-ADD     0             GCPDED
037200031027     C***********        MOVE      ' '           GCPDIS
037300031027     C***********        MOVE      ' '           GCPPSG
037400031027     C***********        MOVE      ' '           GCPASG
037500031027     C***********        MOVE      ' '           GCPNPC
037600031027     C***********        MOVE      ' '           GCPVCS
037700031027     C***********        Z-ADD     0             GCPCAS
037800031027     C***********        MOVE      *BLANKS       GCPVCA
037900031027     C***********        MOVE      ' '           GCPPPC
038000031027     C***********        MOVE      ' '           GCPAPC
038100031027     C***********        Z-ADD     0             GCPDTR
038200031027     C***********        MOVEL     *BLANKS       GCPFTR
038300031027     C* N40******        EXCEPT    AGG3
038400031027      ***********
038500031027     C***********        END
038600941013     C*
038700941013     C                   END
038800031027     C*****
038900031027     C* chiude la Fase 10 <<----
039000031027     C*****
039100921006     C                   SETOFF                                       55
039200911107     C* SE LA FASE E' 38 ALLA PARTENZA DIVENTA 40
039300941227     C     GCPFAS        IFEQ      38
039400941227     C     GCPFAS        OREQ      39
039500920204     C                   Z-ADD     40            APP               3 0
039600911218     C*
039700930309     C                   Z-ADD     0             DATRIC
039800941013     C     GCPDIS        IFEQ      'R'
039900911218     C                   Z-ADD     50            APP
040000950118     C                   Z-ADD     DATEU         DATRIC            8 0
040100911218     C                   END
040200911218     C*
040300990628     C     KGIA          CHAIN     FIGCP01L                           55
040400950412     C*
040500950412     C* METTO NELLE NOTE EVENTUALI DISPOSIZIONI IN ARRIVO DISCORDANTI
040600950412     C* DA QUELLE PRESENTI NELLA GIACENZA ATTUALE.
040700950412     C* E' IL CASO DI DISPOSIZIONI IMMESSE DALL'ARRIVO.
040800950412     C                   EXSR      NOTPRE
040900950412     C*
041000950210     C                   Z-ADD     GCPDBG        SAVDBG            8 0
041100941013     C                   MOVE      GCPDIS        §DIS              1
041200921006     C                   MOVEL     SALVA         SAV
041300940120     C                   Z-ADD     APP           GCPFAS
041400950120     C     GCPDIS        IFEQ      'R'
041500940120     C                   Z-ADD     DATRIC        GCPDBG
041600950210     C                   ELSE
041700950210     C                   Z-ADD     SAVDBG        GCPDBG
041800950120     C                   END
041900941013     C     GCPDIS        IFEQ      'R'
042000941014     C     §DIS          ANDEQ     'C'
042100940120     C                   Z-ADD     9             GCPTFT
042200990629     C                   Z-ADD     0             GCPDTR
042300990629     C                   MOVEL     *BLANKS       GCPFTR
042400940120     C  N55              EXCEPT    AGG2
042500940120     C                   ELSE
042600950309     C     GCPDIS        IFNE      'R'
042700950309     C     §DIS          ANDEQ     'R'
042800990628     C                   MOVEL     *BLANKS       GCPDIV
042900950309     C                   Z-ADD     0             GCPTFT
043000950309     C                   Z-ADD     0             GCPSGS
043100950309     C                   Z-ADD     0             GCPSGR
043200950309     C                   Z-ADD     0             GCPSGD
043300950309     C                   Z-ADD     0             GCPSGA
043400950309     C                   Z-ADD     0             GCPDSD
043500950309     C                   Z-ADD     0             GCPDSA
043600950309     C                   Z-ADD     0             GCPGGS
043700950309     C                   Z-ADD     0             GCPSGF
043800950309     C                   Z-ADD     0             GCPNBW
043900990629     C                   Z-ADD     0             GCPDTR
044000990629     C                   MOVEL     *BLANKS       GCPFTR
044100950309     C  N55              EXCEPT    AGG4
044200950309     C                   ELSE
044300990629     C                   Z-ADD     0             GCPDTR
044400990629     C                   MOVEL     *BLANKS       GCPFTR
044500940120     C  N55              EXCEPT    AGG1
044600911213     C                   END
044700950309     C                   END
044800940120     C                   END
044900911016     C*
045000900604     C                   ADD       1             NUREC             7 0
045100990628     C  N55              DELETE    FIGCP000
045200000000     C                   GOTO      SU
045300911018     C     FIN           TAG
045400900604     C*
045500900604     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
045600900604     C*
045700900604     C                   EXSR      CAEXB
045800900604     C*
045900911018     C* ESPLOSIONE ARCHIVIO NOTE GIACENZE
046000911018     C*
046100911018     C                   SETON                                            50
046200911104     C                   EXSR      CAEXA
046300911018     C                   Z-ADD     0             NUREC
046400911018     C     SUSU          TAG
046500990628     C                   READ      FIGNA00R                               01
046600911018     C   01              GOTO      FINE
046700921102     C                   MOVEL     'R'           GNPFTF
046800921102     C                   Z-ADD     DATEU         GNPDTF
046900990628     C                   WRITE     FIGNP001
047000911018     C                   ADD       1             NUREC
047100990628     C                   DELETE    FIGNA000
047200911018     C                   GOTO      SUSU
047300911018     C     FINE          TAG
047400911018     C*
047500900604     C* AGGIORNA ARCHIVIO DI LOG
047600900604     C*
047700900604     C     LEGFIL        CHAIN     CMRIC01L                           20
047800900604     C  N20              DO
047900900604     C                   Z-ADD     NUREC         RICNR1
048000900604     C                   MOVE      '1'           RICFL2
048100900604     C                   UPDATE    CMRIC000
048200900604     C                   MOVEL     RICPRR        LEGFIL
048300900604     C                   END
048400900604     C     STOP          TAG
048500950209     C**---------------------------------------------------------
048600950209     C*  Se ho richiamato almeno una volta il pgm, 91R1
048700950209     C*  lo richiamo per eseguire DLTOVR e CLOSE
048800950209     C     WCAL91        IFEQ      'S'
048900990628     C                   CLEAR                   FNLGT2
049000990628     C                   MOVEL     'U'           DT2TPC
049100990628     C                   MOVEL     FNLGT2        KPJBU
049200990628     C                   CALL      'FNLGT2R1'
049300950209     C                   PARM                    KPJBA
049400950209     C                   END
049500040115
049600000000     C                   SETON                                        LR
049700040115
049800040115     C**---------------------------------------------------------
049900040115     c     datarich      begsr
050000040115     C**---------------------------------------------------------
050100040304     c                   exsr      comunica
050200040115     C                   MOVE      '2Z'          COD
050300040115     C                   MOVEL     *BLANKS       KEY
050400040115     C                   MOVEL     gcpcmc        KEY
050500040115     C     KTAB          CHAIN     TABEL00F                           33
050600040115     C     *IN33         IFEQ      '0'
050700040115     C                   MOVEL     TBLUNI        DS2z
050800040115     c                   if        §2zdcr <> *blank
050900040115     c                   move      'X'           azzer             1
051000040116      * controllo il tipo bolla per l'attribuzione delle spese e provvigioni
051100040116     C                   MOVE      '3A'          COD
051200040116     C                   MOVEL     *BLANKS       KEY
051300040116     C                   MOVEL     blpcbo        KEY
051400040116     C     KTAB          CHAIN     TABEL00F                           33
051500040116     C     *IN33         IFEQ      '0'
051600040116     C                   MOVEL     TBLUNI        DS3a
051700040116     C                   MOVEL     §3ATB1        PORT              1
051800040116      * spese trasporto
051900040116     c                   if        port = 'F'
052000040116     c                   move      'M'           gcppsg
052100040116     c                   move      'S'           gcpasg
052200040116     c                   else
052300040116     c                   move      'D'           gcppsg
052400040116     c                   move      'S'           gcpasg
052500040116     c                   endif
052600040116      * provv. contrassegno
052700040116     c     kgia1         chain     fnar901l
052800040116     c                   if        %found(fnar901l)
052900040116     c                   if        port = 'F'
053000040116     c                   if        §3atb2 <> *blank
053100040116     c                   move      'D'           gcpppc
053200040116     c                   move      'S'           gcpapc
053300040116     c                   else
053400040116     c                   move      'M'           gcpppc
053500040116     c                   move      'S'           gcpapc
053600040116     c                   endif
053700040116     c                   else
053800040116     c                   move      'D'           gcpppc
053900040116     c                   move      'S'           gcpapc
054000040116     c                   endif
054100040116      *no contrassegno
054200040116     c                   else
054300040116     c                   clear                   gcpppc
054400040116     c                   clear                   gcpapc
054500040116     c                   endif
054600040116
054700040116     C                   END
054800040115     C                   END
054900040115     C                   END
055000040115     c                   endsr
055100040304     C**---------------------------------------------------------
055200040304     c     comunica      begsr
055300040304     C**---------------------------------------------------------
055400040304     c                   move      'N'           trovato           1
055500040304     c                   move      gcpscm        appo              4 0
055600040304     C                   CLEAR                   TRUL27ds
055700040304     C     APPO          IFNE      8888
055800040304     C     APPO          ANDNE     9999
055900040304     C     APPO          ANDNE     *ZEROS
056000040304     C                   MOVEL     'L'           I27TLA
056100040304     C                   MOVEL     BLPTSP        I27TSP
056200040304     C                   MOVE      BLPLNA        I27LNA
056300040304     C                   MOVE      BLPLNP        I27LNP
056400040304     C                   MOVE      BLPctr        I27ctb
056500040304     C                   MOVE      gcpscm        I27CLI
056600040304     C                   movel     blpaas        i27dta
056700040304     C                   move      blpmgs        i27dta
056800040304     C                   CALL      'TRUL27R'
056900040304     C                   PARM                    TRUL27ds
057000040304     C                   END
057100040304     C     APPO          IFNE      8888
057200040304     C     APPO          ANDNE     9999
057300040304     C     APPO          ANDNE     *ZEROS
057400040304     C*  Cerco progressivo tariffa in vigore su TNTAM
057500040304     C                   CLEAR                   fnlv51ds
057600040304     C                   MOVE      BLPCTR        D51CTR
057700040304     C                   MOVEL     'P'           D51TBO
057800040304     C                   movel     blpaas        D51DSP
057900040304     C                   move      blpmgs        D51DSP
058000040304     C                   MOVE      gcpscm        D51KSC
058100040304     C                   z-add     151           D51KCI
058200040304     C                   MOVEL     O27FIE        D51FIE
058300040304     C                   MOVEL     BLPTSP        D51TSP
058400040304     C                   MOVEL     'S'           D51CON
058500040304     C                   CALL      'FNLV51R'
058600040304     C                   PARM                    fnlv51ds
058700040304     C*
058800040304     C     D51ERR        IFEQ      *BLANKS
058900040304     C     D51TKS        ANDEQ     'S'
059000040304     C*  Se ho trovato tariffa in vigore mi posiziono sul file
059100040304     C*  delle tariffe giacenze con il proggressivo giusto
059200040304     C                   MOVE      D51PRG        KPRG
059300040304     C                   MOVE      D51CTR        KCTR
059400040304     C     KTGC          CHAIN     TITGC01L                           31
059500040304     C     *IN31         IFEQ      '0'
059600040304     C     TGCTCM        ANDNE     *BLANKS
059700040304     C                   MOVE      TGCTCM        gcpTCM
059800040304     C                   MOVE      'S'           trovato
059900040304     C                   END
060000040304     C                   END
060100040304     C                   END
060200040304     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
060300040304     C*  lo cerco sull'anagrafica cliente
060400040304     C     trovato       IFEQ      'N'
060500040304     C                   MOVEL     *BLANKS       CLSFLO
060600040304     C     gcpscm        CHAIN     FNCLS01L                           31
060700040304     C     CLSFLO        IFNE      *BLANKS
060800040304     C                   MOVE      CLSFLO        gcpTCM
060900040304     C                   END
061000040304     C                   END
061100040304     C*
061200040304     c                   endsr
061300900604     C**---------------------------------------------------------
061400990628     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI FIGCP00R/FIGNA00R
061500900604     C**---------------------------------------------------------
061600900604     C     CAEXA         BEGSR
061700900604     C                   MOVE      ')'           VAR11            11
061800900604     C                   MOVEL     MBRFIL        VAR11
061900900604     C                   MOVE      '))'          VAR12            12
062000900604     C                   MOVEL     MBRFIL        VAR12
062100900604     C                   Z-ADD     55            LUNG             15 5
062200900604     C*
062300900604     C                   SETOFF                                       20
062400911018     C  N50              MOVE      VAR11         CMD1
062500911018     C   50              MOVE      VAR11         CMD11
062600900604     C                   MOVEL     *BLANKS       COMMAN
062700911018     C  N50              MOVEA     CMD1(1)       COMMAN           80
062800911018     C   50              MOVEA     CMD11(1)      COMMAN           80
062900900604     C                   CALL      'QCMDEXC'                            20
063000900604     C                   PARM                    COMMAN
063100900604     C                   PARM                    LUNG
063200900604     C   20              GOTO      STOP
063300900604     C** CONTROLLO SE ESISTE IL MEMBRO
063400900604     C** SE NON ESISTE CHIUDO IL PROGRAMMA
063500900604     C                   SETOFF                                       20
063600911018     C  N50              MOVE      VAR11         CMD3
063700911018     C   50              MOVE      VAR11         CMD13
063800900604     C                   MOVEL     *BLANKS       COMMAN
063900911018     C  N50              MOVEA     CMD3(1)       COMMAN
064000911018     C   50              MOVEA     CMD13(1)      COMMAN
064100900604     C                   CALL      'QCMDEXC'                            20
064200900604     C                   PARM                    COMMAN
064300900604     C                   PARM                    LUNG
064400900604     C   20              GOTO      STOP
064500900604     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
064600900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
064700990628     C  N50              OPEN      FIGCA00R
064800990628     C   50              OPEN      FIGNA00R
064900900604     C                   ENDSR
065000911018     C**------------------------------------------------------------
065100990628     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI FIGCA00R/FIGNA00R
065200911018     C**------------------------------------------------------------
065300900604     C     CAEXB         BEGSR
065400900604     C                   MOVE      ')'           VAR11            11
065500900604     C                   MOVEL     MBRFIL        VAR11
065600900604     C                   MOVE      '))'          VAR12            12
065700900604     C                   MOVEL     MBRFIL        VAR12
065800900604     C                   Z-ADD     55            LUNG             15 5
065900900604     C*
066000990628     C  N50              CLOSE     FIGCA00R
066100990628     C   50              CLOSE     FIGNA00R
066200900627     C*
066300900604     C                   SETOFF                                       20
066400930630     C  N50
066500930630     CAN LR
066600930630     CANNLR              MOVE      VAR11         CMD4
066700930630     C   50
066800930630     CAN LR
066900930630     CANNLR              MOVE      VAR11         CMD14
067000930630     C** RIORGANIZZA IL MEMBRO
067100900604     C                   ENDSR
067200950412     C*----------------------------------------------------------
067300950412     C*  SCRITTURA, NELLE NOTE, DELLA GIACENZA "PRECEDENTE"
067400950412     C*----------------------------------------------------------
067500950412     C     NOTPRE        BEGSR
067600950412     C*
067700950412     C* SE VARIATO UNO DEI SEGUENTI FLAG, SCRIVO UN RECORD NOTE
067800950412     C     GCPDIS        IFNE      SAVDIS
067900950412     C     GCPDIS        ANDNE     *BLANKS
068000950412     C     GCPPSG        ORNE      SAVPSG
068100950412     C     GCPPSG        ANDNE     *BLANKS
068200950412     C     GCPASG        ORNE      SAVASG
068300950412     C     GCPASG        ANDNE     *BLANKS
068400950412     C     GCPNPC        ORNE      SAVNPC
068500950412     C     GCPNPC        ANDNE     *BLANKS
068600950412     C     GCPPPC        ORNE      SAVPPC
068700950412     C     GCPPPC        ANDNE     *BLANKS
068800950412     C     GCPAPC        ORNE      SAVAPC
068900950412     C     GCPAPC        ANDNE     *BLANKS
069000950412     C*
069100950412     C* DEVO RILEVARE IL NR.PROGRASSIVO PER LE NOTE
069200950412     C                   Z-ADD     20            KFAS
069300990628     C     KNOT2         SETGT     FIGNP01L
069400990628     C     KNOT2         READPE    FIGNP01L                               10
069500950412     C  N10              ADD       1             GNPNPR
069600950412     C   10              Z-ADD     1             GNPNPR
069700950412     C*
069800950412     C                   Z-ADD     GCPAGC        GNPAGC
069900950412     C                   Z-ADD     GCPFGC        GNPFGC
070000950412     C                   Z-ADD     GCPNGC        GNPNGC
070100950412     C                   Z-ADD     GCPFRG        GNPFRG
070200950412     C                   MOVEA     GCPDIS        DIS(13)
070300950412     C                   MOVEA     GCPPSG        DIS(37)
070400950412     C                   MOVEA     GCPASG        DIS(35)
070500950412     C                   MOVEA     GCPNPC        DIS(21)
070600950412     C                   MOVEA     GCPPPC        DIS(50)
070700950412     C                   MOVEA     GCPAPC        DIS(48)
070800950412     C                   MOVEA     DIS           GNPDMC
070900950412     C                   MOVE      'I'           GNPTPN
071000950412     C                   Z-ADD     20            GNPFAS
071100950412     C                   MOVE      *ALL'9'       GNPDTF
071200950412     C                   MOVE      'T'           GNPFTF
071300990628     C                   WRITE     FIGNP001
071400950412     C                   ENDIF
071500950412     C*
071600950412     C                   ENDSR
071700031030     C*----------------------------------------------------------
071800031030     C*  SCRITTURA, NOTE IMMESSE IN ARRIVO
071900031030     C*----------------------------------------------------------
072000031030     C     Forza_NOTE    BEGSR
072100031030     C*
072200031030     C* DEVO RILEVARE IL NR.PROGRASSIVO PER LE NOTE
072300031030     C                   z-add     gcpagc        Kagc3
072400031030     C                   z-add     gcpfgc        KFGC3
072500031030     C                   z-add     gcpngc        KNGC3
072600031030     C                   z-add     gnpFRG        KFRG3
072700031030     C                   Z-ADD     20            KFAS3
072800031030     C     KNOT3         SETGT     FIGNP01L
072900031030     C     KNOT3         READPE    FIGNP01L                               10
073000031030     C*
073100031030     C  N10              ADD       1             GNPNPR
073200031030     C   10              Z-ADD     1             GNPNPR
073300031030     C*
073400031030     C                   Z-ADD     GCPAGC        GNPAGC
073500031030     C                   Z-ADD     GCPFGC        GNPFGC
073600031030     C                   Z-ADD     GCPNGC        GNPNGC
073700031030     C                   Z-ADD     GCPFRG        GNPFRG
073800031030     C                   MOVEA     savDIS        ARR(13)
073900031030     C                   MOVEA     savPSG        ARR(37)
074000031030     C                   MOVEA     savASG        ARR(35)
074100031030     C                   MOVEA     savNPC        ARR(21)
074200031030     C                   MOVEA     savPPC        ARR(50)
074300031030     C                   MOVEA     savAPC        ARR(48)
074400031030     C                   MOVEA     ARR           GNPDMC
074500031030     C                   MOVE      'I'           GNPTPN
074600031030     C                   Z-ADD     20            GNPFAS
074700031030     C                   MOVE      *ALL'9'       GNPDTF
074800031030     C                   MOVE      'T'           GNPFTF
074900031030     C                   WRITE     FIGNP001
075000031030     C*
075100031030     C                   ENDSR
075200941013     C*------------------------------------------------------------*
075300941013     C*  OPERAZIONI INIZIALI                          -------------*
075400941013     C*------------------------------------------------------------*
075500941013     C     *INZSR        BEGSR
075600941013     C*
075700941013     C     *ENTRY        PLIST
075800941013     C                   PARM                    KPJBA
075900941013     C* COMPONGO LA DATA ROVESCIATA
076000941013     C                   TIME                    WHHDAT           14 0
076100941013     C                   MOVE      WHHDAT        G02DAT
076200941013     C                   MOVE      *ZEROS        G02INV
076300941013     C                   MOVEL     ' '           G02ERR
076400941013     C                   CALL      'XSRDA8'
076500941013     C                   PARM                    WLBDA8
076600941013     C                   Z-ADD     G02INV        DATEU             8 0
076700941013     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
076800941013     C* ASSUMO IL PRIMO MEMBRO DEL FILE
076900941013     C                   Z-ADD     1             CODUT
077000941013     C                   CALL      'XPARUT'
077100941013     C                   PARM                    UTEDSE
077200941013     C     CODUT         CABEQ     -1            STOP
077300941013     C                   MOVEL     RAGUT         RSUT             20
077400041001     C*
077500941013     C* DEFINIZIONE CHIAVI
077600941013     C*
077700941013     C     KGIA          KLIST
077800941013     C                   KFLD                    AAS               4 0
077900941013     C                   KFLD                    LNP               3 0
078000941013     C                   KFLD                    NRS               2 0
078100941013     C                   KFLD                    NSP               7 0
078200990628     C                   KFLD                    FRG               2 0
078300941013     C*
078400941013     C     KGIA1         KLIST
078500941013     C                   KFLD                    AAS
078600941013     C                   KFLD                    LNP
078700941013     C                   KFLD                    NRS
078800941013     C                   KFLD                    NSP
078900941013     C*
079000941013     C     KNOT          KLIST
079100941013     C                   KFLD                    GCPAGC
079200941013     C                   KFLD                    GCPFGC
079300941013     C                   KFLD                    GCPNGC
079400941013     C                   KFLD                    FRG
079500950412     C*
079600990628     C     KNOT2         KLIST
079700950412     C                   KFLD                    GCPAGC
079800950412     C                   KFLD                    GCPFGC
079900950412     C                   KFLD                    GCPNGC
080000950412     C                   KFLD                    GCPFRG
080100950412     C                   KFLD                    KFAS
080200941013     C*
080300031030     C     KNOT3         KLIST
080400031030     C                   KFLD                    Kagc3
080500031030     C                   KFLD                    KFGC3
080600031030     C                   KFLD                    KNGC3
080700031030     C                   KFLD                    KFRG3
080800031030     C                   KFLD                    KFAS3
080900031030     C*
081000941013     C     KTAB          KLIST
081100941013     C                   KFLD                    UT                1 0
081200941013     C                   KFLD                    COD               2
081300941013     C                   KFLD                    KEY               8
081400941013     C                   MOVEL     '1'           UT
081500950209     C                   MOVEL     'N'           WCAL91            1
081600040831     C*
081700040831     C     KTBL2         KLIST
081800040831     C                   KFLD                    UT
081900040831     C                   KFLD                    COD
082000040831     C*
082100040304     C     KTGC          KLIST
082200040304     C                   KFLD                    gcpscm
082300040304     C                   KFLD                    KCTR
082400040304     C                   KFLD                    KPRG
082500040304     C*
082600950412     C*
082700040304     C     *LIKE         DEFINE    TGCPRG        KPRG
082800040304     C     *LIKE         DEFINE    TGCCTR        KCTR
082900950412     C     *LIKE         DEFINE    GCPFAS        KFAS
083000031030     C     *LIKE         DEFINE    GNPagc        Kagc3
083100031030     C     *LIKE         DEFINE    GNPFGC        KFGC3
083200031030     C     *LIKE         DEFINE    GNPNGC        KNGC3
083300031030     C     *LIKE         DEFINE    GNPFRG        KFRG3
083400031030     C     *LIKE         DEFINE    GNPFAS        KFAS3
083500941013     C*
083600040831     C* Carico tabella 2f
083700040831     C                   MOVEA     *HIVAL        C2F
083800040831     C                   Z-ADD     0             X                 2 0
083900040831     C                   MOVEL     '2F'          COD
084000040831     C     KTBL2         CHAIN     TABEL                              30
084100040831     C     *IN30         DOWEQ     '0'
084200040831     C     X             ANDLT     50
084300040831     C     TBLFLG        IFEQ      ' '
084400040831     C                   MOVEL     TBLUNI        DS2F
084500040831      * solo se non prevede nessuna comunicazione lo carico
084600040831     c                   if        §2Fftp = *blank
084700040831     C                   ADD       1             X
084800040831     C                   MOVEL     TBLKEY        C2F(X)
084900040831     c                   end
085000040831     C                   END
085100040831     C     KTBL2         READE     TABEL                                  30
085200040831     C                   END
085300941013     C                   ENDSR
085400920331     C*
085500941013     OFNBLP000  E            AGGI
085600921203     O                       BLPFT3
085700990628     OFIGCP1L   E            LIBER
085800990628     OFIGCP1L   E            AGG
085900911016     O                       GCPFRG
086000920211     O                       GCPDDM
086100941013     O                       GCPDED
086200941013     O                       GCPDIS
086300920211     O                       GCPPSG
086400920211     O                       GCPASG
086500920211     O                       GCPNPC
086600920211     O                       GCPVCS
086700941013     O                       GCPVCA
086800941013     O                       GCPCAS
086900920211     O                       GCPPPC
087000920211     O                       GCPAPC
087100941013     O                       GCPDUR
087200990629     O                       GCPDTR
087300990629     O                       GCPFTR
087400990628     OFIGCP1L   E            AGG1
087500911213     O                       GCPFAS
087600941013     O                       GCPDIS
087700911213     O                       GCPPSG
087800911213     O                       GCPASG
087900911213     O                       GCPNPC
088000911213     O                       GCPVCS
088100941013     O                       GCPVCA
088200941013     O                       GCPCAS
088300911213     O                       GCPPPC
088400911213     O                       GCPAPC
088500911213     O                       GCPDDM
088600941013     O                       GCPDED
088700911213     O                       GCPDCG
088800911213     O                       GCPCFG
088900911213     O                       GCPVCS
089000911213     O                       GCPDTR
089100911213     O                       GCPFTR
089200911213     O                       GCPSTA
089300911213     O                       GCPDTF
089400911213     O                       GCPFTF
089500930309     O                       GCPDBG
089600990628     OFIGCP1L   E            AGG2
089700940120     O                       GCPFAS
089800941013     O                       GCPDIS
089900940120     O                       GCPPSG
090000940120     O                       GCPASG
090100940120     O                       GCPNPC
090200940120     O                       GCPVCS
090300941013     O                       GCPVCA
090400941013     O                       GCPCAS
090500940120     O                       GCPPPC
090600940120     O                       GCPAPC
090700940120     O                       GCPDDM
090800941013     O                       GCPDED
090900940120     O                       GCPDCG
091000940120     O                       GCPCFG
091100940120     O                       GCPVCS
091200940120     O                       GCPDTR
091300940120     O                       GCPFTR
091400940120     O                       GCPSTA
091500940120     O                       GCPDTF
091600940120     O                       GCPFTF
091700940120     O                       GCPDBG
091800940120     O                       GCPTFT
091900031027     O******
092000031027     O******FIGCP1L   E            AGG3
092100031027     O******                       GCPFRG
092200031027     O******                       GCPDDM
092300031027     O******                       GCPDED
092400031027     O******                       GCPDIS
092500031027     O******                       GCPPSG
092600031027     O******                       GCPASG
092700031027     O******                       GCPNPC
092800031027     O******                       GCPVCS
092900031027     O******                       GCPVCA
093000031027     O******                       GCPCAS
093100031027     O******                       GCPPPC
093200031027     O******                       GCPAPC
093300031027     O******                       GCPDUR
093400031027     O******                       GCPFAS
093500031027     O******                       GCPDLD
093600031027     O******                       GCPDTR
093700031027     O******                       GCPFTR
093800031027     O******
093900990628     OFIGCP1L   E            AGG4
094000950309     O                       GCPFAS
094100950309     O                       GCPDIS
094200950309     O                       GCPPSG
094300950309     O                       GCPASG
094400950309     O                       GCPNPC
094500950309     O                       GCPVCS
094600950309     O                       GCPVCA
094700950309     O                       GCPCAS
094800950309     O                       GCPPPC
094900950309     O                       GCPAPC
095000950309     O                       GCPDDM
095100950309     O                       GCPDED
095200950309     O                       GCPDCG
095300950309     O                       GCPCFG
095400950309     O                       GCPVCS
095500950309     O                       GCPDTR
095600950309     O                       GCPFTR
095700950309     O                       GCPSTA
095800950309     O                       GCPDTF
095900950309     O                       GCPFTF
096000950309     O                       GCPDBG
096100950309     O                       GCPTFT
096200990628     O                       GCPDIV
096300990628     O                       GCPSGS
096400950309     O                       GCPSGR
096500950309     O                       GCPSGD
096600950309     O                       GCPSGA
096700950309     O                       GCPDSD
096800950309     O                       GCPDSA
096900950309     O                       GCPGGS
097000950309     O                       GCPSGF
097100950309     O                       GCPNBW
097200950412**         CMD1
097300990628CHKOBJ OBJ(FIGCA00R) OBJTYPE(*FILE)     MBR(M234567890)
097400900604**         CMD3
097500990628OVRDBF FILE(FIGCA00R)                   MBR(M234567890)
097600900604**         CMD4
097700990628RGZPFM FILE(FIGCA00R)                   MBR(M234567890)
097800911018**         CMD11
097900990628CHKOBJ OBJ(FIGNA00R) OBJTYPE(*FILE)     MBR(M234567890)
098000911018**         CMD13
098100990628OVRDBF FILE(FIGNA00R)                   MBR(M234567890)
098200911018**         CMD14
098300990628RGZPFM FILE(FIGNA00R)                   MBR(M234567890)
098400950411**  5678901234567890123456789012345678901234567890  DISPOSIZIONI PRECEDENTI
098500950412Disp.Prec.: X porto X spese giac. X-X prov.c/a X-X
098600031030**  5678901234567890123456789012345678901234567890  DISPOSIZIONI ARRIVO
098700031107Disp.Mod..: X porto X spese giac. X-X prov.c/a X-X
