000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941207     H* FNLG31R *----------------------------------------------------*
000300900418     H*                                                              *
000400900510     H*             - VISUALIZZA SINGOLA GIACENZA                    *
000500900418     H*  03 FINE LAVORO                                              *
000600900418     H*  21 IND. COMODO                                              *
000700900418     H*                                                              *
000800900418     H*--------------------------------------------------------------*
000900941207     FFNLG10D   CF   E             WORKSTN
001000941209     F                                     SFILE(LG10S02:NRR)
001100050307     Ftigcp02L  IF   E           K DISK
001200050307     Ftigcp01L  IF   E           K DISK
001300050307     F                                     RENAME(tigcp000:tigcpS)
001400050411     Ftigcp51L  IF   E           K DISK    USROPN
001500050411     F                                     RENAME(tigcp000:tigcpa)
001600050415     Ftigcp52L  IF   E           K DISK    USROPN
001700050415     F                                     RENAME(tigcp000:tigcpa2)
001800050307     FtiGAP01L  IF   E           K DISK
001900050307     FtiGNP01L  IF   E           K DISK
002000941209      *
002100900510     FTABEL00F  IF   E           K DISK
002200900510     FAZORG01L  IF   E           K DISK
002300941209     FFNBLP01L  IF   E           K DISK    USROPN
002400010709     F***FNSTB01LIF  E           K        DISK                           UC
002500990929     FFIAR601L  IF   E           K DISK    USROPN
002600941209     FFNARB01L  IF   E           K DISK    USROPN
002700060213     FFiAR401L  IF   E           K DISK    usropn
002800051115     FFIAR901L  IF   E           K DISK    USROPN
002900941212     FFNLBL01L  IF   E           K DISK    USROPN
003000990830     FTITAS30C  IF   E           K DISK    USROPN
003100990830     FTITAA30C  IF   E           K DISK    USROPN
003200990830     FTITA430C  IF   E           K DISK    USROPN
003300990830     FTNCSB03L  IF   E           K DISK    USROPN
003400040126     Ffiar501L  IF   E           K DISK    usropn
003500990830     D*--------------------------------------------------------------*
003600900509     D KSC§            S              1    DIM(7)
003700990830     D*--------------------------------------------------------------*
003800970108     D FNLG21        E DS                  EXTNAME(FNLG21DS)
003900970624     D FNLG03        E DS                  EXTNAME(FNLG03DS)
004000990830     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004100990830     D CNACO         E DS                  EXTNAME(CNACO00F)
004200990830     D CNIND         E DS                  EXTNAME(CNIND00F)
004300990830     D CNCLP         E DS                  EXTNAME(CNCLP00F)
004400990830     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004500900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004600900418     D  TCU                  398    697
004700941209     D                                     DIM(50)
004800900418     D  KCU                  698    847P 0
004900900509     D                                     DIM(50)
005000900509     D                                     PACKEVEN
005100900509     D TCUDS           DS
005200900509     D  F1                     1      1
005300900509     D  F3                     3      3
005400900509     D  F2                     2      2
005500900509     D  F4                     4      4
005600900509     D  F56                    5      6
005700900418     D CNCR80        E DS
005800900510     D DS3A          E DS
005900941212     D DS5E          E DS
006000911219     D DS2D          E DS
006100911219     D DS2E          E DS
006200941209     D DSTB69        E DS                  EXTNAME(DS7PQRS)
006300900418     D KPJBA         E DS
006400960124     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
006500941209     D FNLG30        E DS                  EXTNAME(FNLG30DS)
006600040126     D Dar5gen       E DS
006700051011     D dgcpflr       E DS
006800160121     D dta4a         E DS
006900941209     D WLBDA8          DS
007000941209     D  G08DAT                 1      8  0
007100941209     D  G08INV                 9     16  0
007200941209     D  G08ERR                17     17
007300941209     D  G08TGI                18     22  0
007400941209     D                SDS
007500941209     D  §PGM                   1     10
007600020927      *parametri per reperimento coda
007700020927     d finep           s              1
007800020927     d codpo           s              3s 0
007900020927     d tpcoda          s              1
008000020927     d nomcoda         s             10
008100020927     d libcoda         s             10
008200020927     d esito           s              1
008300990830     I*--------------------------------------------------------------*
008400990830     ITITASP00      12
008500990830     ITITAS000      01
008600990830     ITITAS010      10
008700990830     C*--------------------------------------------------------------*
008800900510     C*  CARICO DATI A VIDEO
008900900510     C*
009000941209     C     D30NGC        IFEQ      *ZEROS
009100941209     C                   Z-ADD     D30AAS        §AAS
009200941209     C                   Z-ADD     D30LNP        §LNP
009300941209     C                   Z-ADD     D30NRS        §NRS
009400941209     C                   Z-ADD     D30NSP        §NSP
009500050411     C     SIMFEL        IFEQ      0
009600050411     C     KSPE          CHAIN     tigcp51L                           21
009700050411     c                   else
009800050411     C     KSPE          CHAIN     tigcp01L                           21
009900050411     c                   end
010000900619     C                   ELSE
010100050415     C     simfel        ifeq      0
010200050415     C     KGIA          CHAIN     tigcp52L                           21
010300050415     C                   else
010400050307     C     KGIA          CHAIN     tigcp02L                           21
010500050415     C                   endif
010600900619     C                   END
010700941213     C     *IN21         IFEQ      *ON
010800941213     C                   MOVEL     ' '           D30F03
010900941213     C                   MOVEL     '1'           D30F12
011000941213     C                   MOVEL     '1'           D30ERR
011100941213     C                   MOVEL     FNLG30        KPJBU
011200941213     C                   GOTO      FINE
011300941213     C                   ENDIF
011400900510     C                   EXSR      GCPVID
011500970624     C* CONTROLLO SE ESISTONO NOTE APERTURA
011600970624     C     D30POA        IFEQ      'A'
011700970624     C                   EXSR      CTRAPE
011800970624     C                   END
011900900418     C*
012000900418     C     EMFG01        TAG
012100950117     C                   SETOFF                                       14
012200950117     C*------------------------------------------------------------*
012300950117     C*  Se filiale di apertura giacenza diversa da linea di arrivo
012400950117     C*  (bolla originale) ---> Reverse Immage su VLNA
012500950117     C     VLNA          COMP      VFGC                               0707
012600950117     C*------------------------------------------------------------*
012700941209     C                   EXFMT     LG10D01
012800900418     C*                    ------------
012900941212     C*  CMD 12  RITORNO
013000941212     C     *INKL         IFEQ      '1'
013100941212     C                   MOVEL     D30POA        SAVPOA            1
013200941212     C                   CLEAR                   FNLG30
013300941212     C                   MOVEL     ' '           D30F03
013400941212     C                   MOVEL     '1'           D30F12
013500941212     C                   MOVEL     SAVPOA        D30POA
013600941212     C                   MOVEL     FNLG30        KPJBU
013700941212     C                   GOTO      FINE
013800941212     C                   ENDIF
013900960124     C*  VISUALIZZA DESTINATARIO ORIGINALE
014000960124     C     *INKM         IFEQ      '1'
014100960124     C                   MOVEL     'N'           WRITOR            1
014200960124     C     WRITOR        DOWEQ     'N'
014300960124     C                   EXSR      GESD06
014400960124     C                   END
014500960124     C                   GOTO      EMFG01
014600960124     C                   END
014700900418     C*
014800970624     C*  CMD 11  NOTE APERTURA
014900970624     C   KK              EXSR      CALL03
015000970624     C   KK              GOTO      EMFG01
015100911118     C*  CMD 14  GESTIONE NOTE
015200970108     C   KN              EXSR      CTRFAX
015300970108     C   KN              EXSR      GESNOT
015400941213     C   KL              GOTO      EMFG01
015500941209     C* INTERROGO LA PRTICOLARITà GIACENZE
015600941209     C     '?'           SCAN      V1CRPG                                 32
015700941209     C     *IN32         IFEQ      '1'
015800941213     C                   CLEAR                   DSTB69
015900941209     C                   MOVEL     'S'           DS7RIC
016000941209     C                   MOVEL     V1CGGA        DS7COD
016100941209     C                   MOVEL     'V'           DS7GES
016200941209     C                   MOVEL     DSTB69        KPJBU
016300941209     C                   CALL      'TRTB69R'
016400941209     C                   PARM                    KPJBA
016500941209     C                   MOVEL     ' '           V1CRPG
016600941209     C                   GOTO      EMFG01
016700941209     C                   END
016800911118     C*
016900911119     C*  CMD 04  GESTIONE STORICO
017000941212     C     *INKD         IFEQ      *ON
017100941212     C                   MOVEL     'S'           D30RIC
017200941209     C                   Z-ADD     GCPAGC        D30AGC
017300941209     C                   Z-ADD     GCPFGC        D30FGC
017400941209     C                   Z-ADD     GCPNGC        D30NGC
017500941212     C                   CLEAR                   D30F03
017600941212     C                   CLEAR                   D30F12
017700941213     C                   CLEAR                   D30ERR
017800050307     c                   if        d30poa <> 'A'
017900000403     C                   Z-ADD     GCPAAS        D30AAS
018000000403     C                   Z-ADD     GCPLNP        D30LNP
018100000403     C                   Z-ADD     GCPNRS        D30NRS
018200000403     C                   Z-ADD     GCPNSP        D30NSP
018300050307     c                   else
018400050307     C                   Z-ADD     GCPAASa       D30AAS
018500050307     C                   Z-ADD     GCPLNPa       D30LNP
018600050307     C                   Z-ADD     GCPNRSa       D30NRS
018700050307     C                   Z-ADD     GCPNSPa       D30NSP
018800050307     c                   end
018900000403     C                   MOVE      GCPFRG        D30FRG
019000941209     C                   MOVEL     FNLG30        KPJBU
019100941212     C                   GOTO      FINE
019200941212     C                   ENDIF
019300911119     C*
019400941212     C     EMFG04        TAG
019500941209     C                   WRITE     LG10D03
019600941209     C                   EXFMT     LG10D04
019700900510     C*                    ____________
019800941212     C*  CMD 21  STATO DELLA GIACENZA
019900941212     C     *INKV         IFEQ      *ON
020000941212     C                   EXFMT     LG10D05
020100941212     C                   GOTO      EMFG04
020200941212     C                   ENDIF
020300941209     C     *INKL         CABEQ     '1'           EMFG01
020400941212     C*  CMD 03  FINE LAVORO
020500950112     C*  O ENTER FINE LAVORO
020600941212     C     *INKC         IFEQ      '1'
020700950112     C     *INKC         OREQ      '0'
020800950112     C     *INKL         ANDEQ     '0'
020900950112     C     *INKV         ANDEQ     '0'
021000941212     C                   MOVEL     D30POA        SAVPOA            1
021100941212     C                   CLEAR                   FNLG30
021200941212     C                   MOVEL     '1'           D30F03
021300941212     C                   MOVEL     SAVPOA        D30POA
021400941212     C                   MOVEL     FNLG30        KPJBU
021500941212     C                   GOTO      FINE
021600941212     C                   ENDIF
021700900418     C*
021800900418     C     FINE          TAG
021900900418     C                   SETON                                        LR
022000900418     C/EJECT
022100900510     C/SPACE 3
022200900510     C*---------------------------------------------------------------*
022300900510     C*   G C P V I D                                                 *
022400900510     C*---------------------------------------------------------------*
022500900510     C     GCPVID        BEGSR
022600990830     C*
022700050307     C     GCPriap       comp      *blank                             4242
022800050307     c                   if        d30poa = 'A'
022900050307     C                   Z-ADD     GCPLNPa       §LNP
023000050307     C                   Z-ADD     GCPAASa       §AAS
023100050307     C                   Z-ADD     GCPNRSa       §NRS
023200050307     C                   Z-ADD     GCPNSPa       §NSP
023300050307     c                   else
023400050307     C                   Z-ADD     GCPLNP        §LNP
023500050307     C                   Z-ADD     GCPAAS        §AAS
023600050307     C                   Z-ADD     GCPNRS        §NRS
023700050307     C                   Z-ADD     GCPNSP        §NSP
023800050307     c                   endif
023900040126      * rekord GEN riferimenti ubicazione in magazzino giacenze
024000040209     c                   if        d30poa = 'A'
024100040126     C                   clear                   dar5gen
024200040126     C                   move      'GEN'         trcar5
024300040126     c     kar5          chain     fiar501l
024400040126     c                   if        %found
024500040126     C                   MOVEL     ar5uni        dar5gen
024600040126     c                   endif
024700040126     c                   movel     §ar5zng       vubima
024800040209     c                   endif
024900990830     C* INTERROGAZIONE GIACENZE SEDE
025000990830     C     *IN93         IFEQ      '1'
025100990830     C                   EXSR      TASVID
025200990830     C                   ELSE
025300941209     C     *IN80         IFEQ      '0'
025400941209     C     KSPE          CHAIN     FNBLP01L                           21
025500950117     C     *IN21         IFEQ      '0'
025600950117     C                   EXSR      BLPVID
025700950117     C                   END
025800941209     C                   ELSE
025900941209     C     KSPE          CHAIN     FNARB01L                           21
026000941209     C  N21              EXSR      ARBVID
026100941209     C                   ENDIF
026200990830     C                   ENDIF
026300050307     c                   if        d30poa = 'A'
026400050307     C                   MOVE      GCPLNPa       VLNP
026500050307     C                   MOVE      GCPAASa       VAAS
026600050307     C                   MOVE      GCPNRSa       VNRS
026700050307     C                   MOVE      GCPNSPa       VNSP
026800050307     C                   MOVE      GCPLNPa       V6CLNP
026900050307     C                   MOVE      GCPAASa       V6CAAS
027000050307     C                   MOVE      GCPNRSa       V6CNRS
027100050307     C                   MOVE      GCPNSPa       V6CNSP
027200050307     c                   else
027300900510     C                   MOVE      GCPLNP        VLNP
027400900510     C                   MOVE      GCPAAS        VAAS
027500900510     C                   MOVE      GCPNRS        VNRS
027600900510     C                   MOVE      GCPNSP        VNSP
027700960124     C                   MOVE      GCPLNP        V6CLNP
027800960124     C                   MOVE      GCPAAS        V6CAAS
027900960124     C                   MOVE      GCPNRS        V6CNRS
028000960124     C                   MOVE      GCPNSP        V6CNSP
028100050307     c                   endif
028200050307     C                   MOVE      GCPSCM        VSCM
028300060808     C                   clear                   V1cchim
028400060808     c                   if        GCPchiaut = 'N'
028500060808     C                   MOVE      'S'           V1cchim
028600060808     c                   endif
028700050307     C                   MOVE      VSCM          §QUATR            4
028800941209      * SE SONO IN PARTENZA VIS. IL MITTENTE DALLA GIACENZA
028900911121     C  N80§QUATR        IFEQ      *ALL'8'
029000900510     C     §QUATR        OREQ      *ALL'9'
029100900510     C* MITTENTE DA APPOGGIO
029200050307     C     KSPE          CHAIN     tigap01L                           21
029300900510     C     *IN21         IFEQ      '0'
029400900510     C                   MOVEL     GAPRSM        VRSM
029500900510     C                   MOVEL     GAPINM        VINM
029600900510     C                   MOVEL     GAPLOM        VLOM
029700900510     C                   MOVEL     GAPPRM        VPRM
029800900510     C                   MOVEL     GAPCAM        VCAM
029900941209     C                   MOVEL     GAPNZM        V1CNZM
030000900510     C                   END
030100900510     C                   ELSE
030200990830      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030300990830     C                   CLEAR                   TIBS69
030400990830     C                   CLEAR                   CNACO
030500990830     C                   CLEAR                   CNIND
030600990830     C                   CLEAR                   CNCLP
030700990830     C                   CLEAR                   FNCLS
030800990830     C                   MOVEL     ' '           I69TLA
030900990830     C                   MOVEL     KNSIF         I69SIF
031000990830     C                   Z-ADD     VSCM          I69KAC
031100990830     C                   Z-ADD     VSCM          I69KIN
031200990830     C                   CALL      'TIBS69R'
031300990830     C                   PARM                    TIBS69
031400990830     C                   PARM                    CNACO
031500990830     C                   PARM                    CNIND
031600990830     C                   PARM                    CNCLP
031700990830     C                   PARM                    FNCLS
031800990830     C     O69ERR        IFEQ      *BLANK
031900990830     C                   MOVEL     ACORAG        VRSM
032000990830     C                   MOVEL     INDVIA        VINM
032100990830     C                   MOVEL     INDCIT        VLOM
032200990830     C                   MOVEL     INDPRV        VPRM
032300990830     C                   MOVEL     INDCAE        VCAM
032400990830     C                   MOVEL     INDSTA        V1CNZM
032500990830     C                   ELSE
032600990830     C                   MOVE      *ALL'?'       VRSM
032700990830     C                   MOVE      *ALL'?'       VINM
032800990830     C                   MOVE      *ALL'?'       VLOM
032900990830     C                   MOVE      *ALL'?'       VPRM
033000990830     C                   END
033100900510     C                   END
033200941212     C* STATO AVANZAMENTO GIACENZA
033300941212     C                   MOVEL     GCPFAS        V5CFAS
033400941212     C                   MOVEL     '2Y'          TBLCOD
033500941212     C                   MOVEL     *BLANK        TBLKEY
033600941212     C                   MOVEL     GCPFAS        TBLKEY
033700941212     C     KTAB          CHAIN     TABEL                              21
033800941212     C   21              MOVEL     *ALL'?'       V5DFAS
033900941212     C  N21              MOVEL     TBLUNI        V5DFAS
034000900510     C* CODICE MOTIVO GIACENZA
034100900510     C                   MOVE      GCPCMC        VCMC
034200900626     C                   MOVEL     *BLANKS       DCMC
034300900626     C     GCPCMC        IFNE      *BLANKS
034400900510     C                   MOVEL     '2A'          TBLCOD
034500900510     C                   MOVEL     *BLANK        TBLKEY
034600900510     C                   MOVEL     VCMC          TBLKEY
034700900510     C     KTAB          CHAIN     TABEL                              21
034800900510     C   21              MOVEL     *ALL'?'       DCMC
034900900510     C  N21              MOVEL     TBLUNI        DCMC
035000900626     C                   END
035100911118     C* PRLEVO EVENTUALE ULTERIORE MOTIVO
035200911118     C                   Z-ADD     GCPFRG        §FRG
035300911118     C                   Z-ADD     10            §FAS
035400050307     C     KNOT          CHAIN     tignp01L                           45
035500911118     C  N45              MOVEL     GNPDMC        VDMC
035600911118     C   45              MOVEL     *BLANKS       VDMC
035700900510     C* LUOGO DEPOSITO MERCE
035800900510     C                   MOVE      GCPCCR        VCCR
035900911118     C                   MOVEL     *BLANKS       DCCR
036000900626     C     GCPCCR        IFNE      *ZEROS
036100900626     C                   MOVEL     '2B'          TBLCOD
036200900626     C                   MOVEL     *BLANK        TBLKEY
036300900626     C                   MOVEL     VCCR          TBLKEY
036400900626     C     KTAB          CHAIN     TABEL                              21
036500911118     C   21              MOVEL     *ALL'?'       DCCR
036600911118     C  N21              MOVEL     TBLUNI        DCCR
036700900626     C* SE NON LO TROVO NELLA TABELLA  LO CERCO IN ORGANIGARMMA
036800900510     C     *IN21         IFEQ      '1'
036900900626     C     VCCR          CHAIN     AZORG01L                           21
037000900626     C  N21ORGFVA        COMP      '*'                                    21
037100911118     C   21              MOVEL     *ALL'?'       DCCR
037200911118     C  N21              MOVEL     *BLANK        DCCR
037300911118     C  N21              MOVEL     ORGDES        DCCR
037400900510     C                   END
037500900626     C                   END
037600050408     C* SE SONO IN ARRIVO CERCO NEI LEGAMI LA BOLLA ORIGINALE
037700941212     C                   SETOFF                                       81
037800941212     C     *IN80         IFEQ      *ON
037900941212      *
038000050408     c                   if        gcpaas <> gcpaasa or gcplnp <> gcplnpa or
038100050408     c                             gcpnrs <> gcpnrsa or gcpnsp <> gcpnspa
038200941212     C                   SETON                                        81
038300050307     C                   MOVE      gcpaas        V5CAAO
038400050307     C                   MOVE      gcplnp        V5CLPO
038500050307     C                   MOVE      gcpnrs        V5CNRO
038600050307     C                   MOVE      gcpnsp        V5CNSO
038700050408     c                   endif
038800941212     C*
038900941212     C                   ENDIF
039000941209     C* NR. E DATA COMUNICAZ. = DATA DIST.PADRONC.
039100900510     C                   MOVE      GCPNRC        VNRC
039200941209     C                   MOVE      GCPDXD        G08INV
039300941209     C                   MOVEL     '3'           G08ERR
039400941209     C                   CALL      'XSRDA8'
039500941209     C                   PARM                    WLBDA8
039600941209     C                   Z-ADD     G08DAT        VDXD
039700941212     C                   Z-ADD     G08DAT        V5CDXD
039800960124     C                   MOVE      GCPAGC        V6CAGC
039900960124     C                   MOVE      GCPFGC        V6CFGC
040000960124     C                   MOVE      GCPNGC        V6CNGC
040100900510     C* DATA APERTURA GIACENZA
040200941209     C                   MOVEL     GCPAGC        G08INV
040300941209     C                   MOVE      GCPMGC        G08INV
040400941209     C                   MOVEL     '3'           G08ERR
040500941209     C                   CALL      'XSRDA8'
040600941209     C                   PARM                    WLBDA8
040700941209     C                   Z-ADD     G08DAT        VDTG
040800941212     C                   Z-ADD     G08DAT        V5CDTG
040900900510     C                   MOVE      GCPNGC        VNGC
041000900510     C* FIL. CHE HA APERTO LA GIACENZA
041100900510     C                   MOVE      GCPFGC        VFGC
041200900510     C     GCPFGC        CHAIN     AZORG01L                           21
041300900510     C  N21ORGFVA        COMP      '*'                                    21
041400900510     C   21              MOVEL     *ALL'?'       DFGC
041500900510     C  N21              MOVEL     ORGDES        DFGC
041600900510     C*
041700900510     C* DATA ULTIMA RIAPERTURA
041800050408     c                   if        gcpriap = 'R'
041900941209     C                   MOVE      GCPDUR        G08INV
042000941209     C                   MOVEL     '3'           G08ERR
042100941209     C                   CALL      'XSRDA8'
042200941209     C                   PARM                    WLBDA8
042300941209     C                   Z-ADD     G08DAT        VDIG
042400941212     C                   Z-ADD     G08DAT        V5CDIG
042500050408     c                   endif
042600941212     C* DATA STAMPA LETTERA APERTURA
042700941212     C                   MOVE      GCPDMM        G08INV
042800941212     C                   MOVEL     '3'           G08ERR
042900941212     C                   CALL      'XSRDA8'
043000941212     C                   PARM                    WLBDA8
043100941212     C                   Z-ADD     G08DAT        V5CDMM
043200941212     C* TIPO COMUN. APERTURA
043300960314     C                   MOVEL     GCPFST        V1CFST
043400960314     C                   MOVEL     GCPTCM        V5CTCM
043500941212     C                   MOVEL     '2F'          TBLCOD
043600941212     C                   MOVEL     *BLANK        TBLKEY
043700941212     C                   MOVEL     GCPTCM        TBLKEY
043800941212     C     KTAB          CHAIN     TABEL                              21
043900941212     C  N21              MOVEL     TBLUNI        V5DTCM
044000051011     C* appuntamento
044100051011     C                   MOVEL     GCPflr        dgcpflr
044200051011     c                   movel     D§GCPAPP      v1capp
044300051011     C* DATA IMMISS. DISPOSIZIONI
044400941209     C                   MOVE      GCPDDM        G08INV
044500941209     C                   MOVEL     '3'           G08ERR
044600941209     C                   CALL      'XSRDA8'
044700941209     C                   PARM                    WLBDA8
044800941209     C                   Z-ADD     G08DAT        VDDM
044900941212     C                   Z-ADD     G08DAT        V5CDDM
045000941209     C* DATA ESEGUIBILTà  DISPOSIZIONI (SE NON IMMESSE=DATA IMMISS.)
045100941209     C     GCPDED        IFGT      0
045200941209     C                   MOVE      GCPDED        G08INV
045300941209     C                   MOVEL     '3'           G08ERR
045400941209     C                   CALL      'XSRDA8'
045500941209     C                   PARM                    WLBDA8
045600941209     C                   Z-ADD     G08DAT        V1CDED
045700941212     C                   Z-ADD     G08DAT        V5CDED
045800941209     C                   ELSE
045900051011     c                   if        D§GCPAPP = 'A'
046000051011     c                   clear                   v1cded
046100051011     c                   else
046200941209     C                   Z-ADD     VDDM          V1CDED
046300941212     C                   Z-ADD     VDDM          V5CDED
046400941209     C                   ENDIF
046500051011     C                   ENDIF
046600941209     C* DATA TRASMISSIONE DISPOSIZIONI ALL'ARRIVO
046700941209     C                   MOVE      GCPDLD        G08INV
046800941209     C                   MOVEL     '3'           G08ERR
046900941209     C                   CALL      'XSRDA8'
047000941209     C                   PARM                    WLBDA8
047100941212     C                   Z-ADD     G08DAT        V5CDLD
047200911122     C*
047300911122     C* DATI DISPOSIZIONI
047400911122     C*
047500941209     C                   MOVEL     GCPDIS        VFRI
047600911219     C                   MOVEL     *BLANK        DFRI
047700941209     C     GCPDIS        IFNE      *BLANK
047800911219     C                   MOVEL     '2D'          TBLCOD
047900911219     C                   MOVEL     *BLANK        TBLKEY
048000911219     C                   MOVEL     VFRI          TBLKEY
048100911219     C     KTAB          CHAIN     TABEL                              21
048200911219     C   21              MOVEL     *ALL'?'       DFRI
048300911219     C  N21              MOVEL     TBLUNI        DS2D
048400911219     C  N21              MOVEL     §2DDES        DFRI
048500911219     C                   END
048600941209     C* DISPOSIZIONI:
048700911122     C                   MOVEL     GCPPSG        VPSG
048800911219     C                   MOVEL     *BLANK        DPSG
048900911219     C     GCPPSG        IFNE      *BLANK
049000911219     C                   MOVEL     '2E'          TBLCOD
049100911219     C                   MOVEL     *BLANK        TBLKEY
049200911219     C                   MOVEL     VPSG          TBLKEY
049300911219     C     KTAB          CHAIN     TABEL                              21
049400911219     C   21              MOVEL     *ALL'?'       DPSG
049500911219     C  N21              MOVEL     TBLUNI        DS2E
049600911219     C  N21              MOVEL     §2EDES        DPSG
049700911219     C                   END
049800911122     C                   MOVEL     GCPASG        VASG
049900911122     C                   MOVEL     GCPNPC        VNPC
050000911219     C                   MOVEL     *BLANK        DNPC
050100911219     C     GCPNPC        IFNE      *BLANK
050200911219     C     GCPNPC        COMP      'A'                                    21
050300911219     C   21              MOVEL     'ASSEGNAT'    DNPC
050400911219     C   21              MOVE      'O '          DNPC
050500911219     C  N21              MOVEL     'FRANCO  '    DNPC
050600911219     C  N21              MOVE      '  '          DNPC
050700911219     C                   END
050800911122     C                   MOVEL     GCPVCS        VVCS
050900941209     C                   Z-ADD     GCPCAS        VNCA
051000941209     C                   MOVE      GCPVCA        V1CNDV
051100911122     C                   MOVEL     GCPPPC        VPPC
051200911219     C                   MOVEL     *BLANK        DPPC
051300911219     C     GCPPPC        IFNE      *BLANK
051400911219     C                   MOVEL     '2E'          TBLCOD
051500911219     C                   MOVEL     *BLANK        TBLKEY
051600911219     C                   MOVEL     VPPC          TBLKEY
051700911219     C     KTAB          CHAIN     TABEL                              21
051800911219     C   21              MOVEL     *ALL'?'       DPPC
051900911219     C  N21              MOVEL     TBLUNI        DS2E
052000911219     C  N21              MOVEL     §2EDES        DPPC
052100911219     C                   END
052200911122     C                   MOVEL     GCPAPC        VAPC
052300051013     c* apertura automatica
052400051013     c                   if        gcpapeaut <> *blank
052500051013     c                   seton                                        61
052600051013     C                   MOVE      GCPapeaut     V5Capeaut
052700051013     c                   else
052800051013     C                   clear                   V5Capeaut
052900051013     c                   setoff                                       61
053000051013     c                   endif
053100051013     c* disposizioni automatiche
053200051013     c                   movel     gcpflr        dgcpflr
053300051013     c                   if        d§gcpdisa <> *blank
053400051013     c                   seton                                        62
053500051013     C                   MOVE      d§gcpdisa     V5Cdisaut
053600051013     c                   else
053700051013     C                   clear                   V5Cdisaut
053800051013     c                   setoff                                       62
053900051013     c                   endif
054000170307     c* disposizioni immesse da chi ?
054100170324     c                   if        D§GCPUTEW  = 'D' or gcpfas = 31
054200170307     C                   eval      vdischi = 'Destinatario'
054300170307     c                   else
054400170307     C                   eval      vdischi = 'Mittente'
054500170307     c                   endif
054600911122     C*
054700960208     C                   SETOFF                                       94
054800960208     C                   Z-ADD     20            §FAS
054900050307     C     KNOT          SETLL     tIGNP01L                               21
055000960208     C   21              SETON                                        94
055100911210     C* SE NON è CHIUSA SALTO DATI DI FATTURAZIONE E NON SONO
055200911210     C* STATI FATTI ADDEBITI IN DISPOSIZ.
055300141111     C*mm  GCPDCG        IFEQ      *ZERO
055400141111     C*mm  GCPNBW        ANDEQ     *ZEROS
055500141111     C*mm                GOTO      ENDGCP
055600141111     C*mm                END
055700900510     C*
055800900510     C* CAMPI X ESITAZIONE GIACENZA / FATTURAZIONE
055900900510     C* ---------------------------
056000900510     C* TIPO FATTURAZIONE
056100900510     C                   Z-ADD     GCPTFT        VTFT
056200900510     C* CODICE FINE  GIACENZA
056300900626     C                   MOVEL     *BLANKS       DCFG
056400900510     C     GCPCFG        IFNE      *BLANK
056500900510     C                   MOVEL     GCPCFG        VCFG
056600900510     C                   MOVEL     '2C'          TBLCOD
056700900510     C                   MOVEL     *BLANK        TBLKEY
056800900510     C                   MOVEL     VCFG          TBLKEY
056900900510     C     KTAB          CHAIN     TABEL                              21
057000900510     C   21              MOVEL     *ALL'?'       DCFG
057100900510     C  N21              MOVEL     TBLUNI        DCFG
057200900510     C                   END
057300941209     C* DATA FINE GIACENZA IN ARRIVO
057400941209     C                   MOVE      GCPDCG        G08INV
057500941209     C                   MOVEL     '3'           G08ERR
057600990830     C                   CALL      'XSRDA8'
057700941209     C                   PARM                    WLBDA8
057800941209     C                   Z-ADD     G08DAT        VDCG
057900941212     C                   Z-ADD     G08DAT        V5CDCG
058000900510     C* DATA SOSTA DAL
058100941209     C                   MOVE      GCPDSD        G08INV
058200941209     C                   MOVEL     '3'           G08ERR
058300941209     C                   CALL      'XSRDA8'
058400941209     C                   PARM                    WLBDA8
058500941209     C                   Z-ADD     G08DAT        VDSD
058600900510     C* DATA SOSTA AL
058700941209     C                   MOVE      GCPDSA        G08INV
058800941209     C                   MOVEL     '3'           G08ERR
058900941209     C                   CALL      'XSRDA8'
059000941209     C                   PARM                    WLBDA8
059100941209     C                   Z-ADD     G08DAT        VDSA
059200900510     C                   Z-ADD     GCPGGS        VGGS
059300900510     C                   Z-ADD     GCPSGF        VSGF
059400900510     C                   Z-ADD     GCPSGS        VTSGS
059500900510     C                   Z-ADD     GCPSGR        VSGR
059600900510     C                   Z-ADD     GCPSGD        VSGD
059700920424     C                   Z-ADD     GCPSGA        VSGA
059800990628     C                   MOVEL     GCPDIV        V4CDIV
059900990628     C* SE NON C'E' DIVISA IMPOSTA ITL
060000990628     C     GCPDIV        IFEQ      *BLANKS
060100990628     C                   MOVEL     'ITL'         GCPDIV
060200990628     C                   END
060300941209     C* DATA CREAZ. BOLLA / IN ARRIVO= DATA ESECUZIONE DISPOSIZ.
060400050408     C                   MOVE      GCPDeda       G08INV
060500941209     C                   MOVEL     '3'           G08ERR
060600941209     C                   CALL      'XSRDA8'
060700941209     C                   PARM                    WLBDA8
060800050408     C                   Z-ADD     G08DAT        V5CDeda
060900941209     C                   Z-ADD     G08DAT        VDBG
061000000201     C* REPERISCO LA DATA DELLA BOLLA CHAINANDO LA SPEDIZIONE
061100000201     C                   Z-ADD     0             VDBG
061200000201     C     GCPNBW        IFNE      0
061300000201     C     GCPNBW        ANDNE     9999999
061400000207     C* SE NON SONO IN SEDE
061500000207     C     *IN93         ANDEQ     '0'
061600000201     C     *IN80         IFEQ      '0'
061700000201     C     KBOLW         CHAIN     FNBLP01L                           31
061800000201     C     *IN31         IFEQ      '0'
061900000201     C                   MOVEL     BLPAAS        G08INV
062000000201     C                   MOVE      BLPMGS        G08INV
062100000201     C                   Z-ADD     0             G08DAT
062200000201     C                   MOVEL     '3'           G08ERR
062300000201     C                   CALL      'XSRDA8'
062400000201     C                   PARM                    WLBDA8
062500000201     C                   MOVE      G08DAT        VDBG
062600000201     C     KSPE          CHAIN     FNBLP01L                           21
062700000201     C                   END
062800000201     C                   ELSE
062900050307     C     KBOLW2        CHAIN     FNARB01L                           31
063000000201     C     *IN31         IFEQ      '0'
063100000201     C                   MOVEL     ARBAAS        G08INV
063200000201     C                   MOVE      ARBMGS        G08INV
063300000201     C                   Z-ADD     0             G08DAT
063400000201     C                   MOVEL     '3'           G08ERR
063500000201     C                   CALL      'XSRDA8'
063600000201     C                   PARM                    WLBDA8
063700000201     C                   MOVE      G08DAT        VDBG
063800000201     C     KSPE          CHAIN     FNARB01L                           21
063900000201     C                   END
064000000201     C                   END
064100000201     C                   END
064200941209      *
064300050609     c*                  if        d30poa = 'A'
064400050609     C*                  Z-ADD     GCPLPWa       VLNW
064500050609     C*                  Z-ADD     GCPASWa       VASW
064600050609     C*                  Z-ADD     GCPNSWa       VNSW
064700050609     C*                  Z-ADD     GCPNBWa       VNBW
064800050609     c*                  else
064900900510     C                   Z-ADD     GCPLPW        VLNW
065000900510     C                   Z-ADD     GCPASW        VASW
065100900510     C                   Z-ADD     GCPNSW        VNSW
065200900510     C                   Z-ADD     GCPNBW        VNBW
065300050609     c*                  endif
065400900510     C*
065500941212     C* DATA STAMPA COMUNIC. CHIUSURA
065600941212     C                   MOVE      GCPDLM        G08INV
065700941212     C                   MOVEL     '3'           G08ERR
065800941212     C                   CALL      'XSRDA8'
065900941212     C                   PARM                    WLBDA8
066000941212     C                   Z-ADD     G08DAT        V5CDLM
066100941212     C*
066200050531     C* DATA chiusura in partenza
066300050531     C                   MOVE      GCPDbg        G08INV
066400050531     C                   MOVEL     '3'           G08ERR
066500050531     C                   CALL      'XSRDA8'
066600050531     C                   PARM                    WLBDA8
066700050531     C                   Z-ADD     G08DAT        V5CDbg
066800050531     C*
066900050531     C* DATA STAMPA merce portata in magazzino
067000050531     C                   MOVE      GCPDLMa       G08INV
067100050531     C                   MOVEL     '3'           G08ERR
067200050531     C                   CALL      'XSRDA8'
067300050531     C                   PARM                    WLBDA8
067400050531     C                   Z-ADD     G08DAT        V5CDLMa
067500050531     C*
067600050531     C* DATA STAMPA merce prelevata da magazzino
067700050531     C                   MOVE      GCPDuma       G08INV
067800050531     C                   MOVEL     '3'           G08ERR
067900050531     C                   CALL      'XSRDA8'
068000050531     C                   PARM                    WLBDA8
068100050531     C                   Z-ADD     G08DAT        V5CDuma
068200050609     C                   MOVE      GCPaswa       V5Caswa
068300050609     C                   MOVE      GCPlpwa       V5Clpwa
068400050609     C                   MOVE      GCPnswa       V5Cnswa
068500050609     C                   MOVE      GCPnbwa       V5Cnbwa
068600050531     C*
068700941209     C                   MOVEL     GCPTFG        V4CTFG
068800941212     C                   MOVEL     GCPTFG        V5CTFG
068900941209     C                   MOVEL     '2H'          TBLCOD
069000941209     C                   MOVEL     *BLANK        TBLKEY
069100941209     C                   MOVEL     GCPTFG        TBLKEY
069200941209     C     KTAB          CHAIN     TABEL                              21
069300941209     C  N21              MOVEL     TBLUNI        V4DTFG
069400941212     C  N21              MOVEL     TBLUNI        V5DTFG
069500941209     C*
069600941209     C* CLIENTE A CUI SI E' FATTURATO LE SPESE GIAC. (DALLA BOLLA)
069700900510     C*
069800901220     C                   MOVEL     *BLANK        VRSDB
069900901220     C                   MOVEL     *BLANK        VINDB
070000901220     C                   MOVEL     *BLANK        VLODB
070100901220     C                   MOVEL     *BLANK        VPRDB
070200941212     C                   MOVE      *BLANK        VKSC
070300941212     C                   MOVE      *BLANK        VCADB
070400901220     C                   MOVE      *ZEROS        VFIV
070500901220     C                   MOVE      *ZEROS        VNFT
070600901220     C                   MOVE      *ZEROS        VDFT
070700990830      * BOLLA ADDEBITO SPESE ...
070800911211     C     GCPNBW        IFNE      *ZEROS
070900941209     C     *IN80         ANDEQ     '0'
071000990830     C* SE SONO IN SEDE
071100990830     C     *IN93         IFEQ      '1'
071200990830     C* REPERISCO DA CHAIN IN TNTAS DATI BOLLA DI RECUPERO
071300990830     C                   EXSR      TASREC
071400990830     C* ... O IN P.O. PARTENZA
071500990830     C                   ELSE
071600900510     C                   Z-ADD     GCPLPW        §LNP
071700900510     C                   Z-ADD     GCPASW        §AAS
071800900510     C                   Z-ADD     GCPNSW        §NRS
071900900510     C                   Z-ADD     GCPNBW        §NSP
072000941209     C     KSPE          CHAIN     FNBLP01L                           21
072100941209     C     *IN21         IFEQ      '0'
072200941209     C                   MOVE      BLPKSC        VKSC
072300941209     C                   MOVEL     BLPRSM        VRSDB
072400941209     C                   MOVEL     BLPINM        VINDB
072500941209     C                   MOVEL     BLPLOM        VLODB
072600941209     C                   MOVEL     BLPPRM        VPRDB
072700941209     C                   MOVEL     BLPCAM        VCADB
072800941209     C                   MOVEL     BLPNZM        V3CNZM
072900990929     C* Imposto tipo record '1' X aggancio su file Tassazione
073000990929     C* per reperire dati fattura della bolla di recupero
073100990929     C                   MOVEL     '1'           KTRC
073200990929     C     KAR6          CHAIN     FIAR601L                           21
073300941209     C     *IN21         IFEQ      '0'
073400990929     C                   Z-ADD     AR6FIV        VFIV
073500990929     C                   Z-ADD     AR6NFT        VNFT
073600990929     C                   Z-ADD     AR6DFT        G08INV
073700941209     C                   MOVEL     '3'           G08ERR
073800941209     C                   CALL      'XSRDA8'
073900941209     C                   PARM                    WLBDA8
074000941209     C                   Z-ADD     G08DAT        VDFT
074100941209     C                   ELSE
074200950117     C                   CLEAR                   VFIV
074300950117     C                   CLEAR                   VNFT
074400950117     C                   CLEAR                   VDFT
074500941209     C                   ENDIF
074600950117     C                   ENDIF
074700941209     C                   ENDIF
074800990830     C                   ENDIF
074900960208     C*
075000900510     C     ENDGCP        ENDSR
075100990830     C*---------------------------------------------------------------*
075200990830     C*   T A S R E C                                                 *
075300990830     C*---------------------------------------------------------------*
075400990830     C     TASREC        BEGSR
075500990830     C*
075600990830     C* AZZERO I DATI ANAGRAFICI DELLA BOLLA DI RECUPERO PRIMA DEL CHAIN
075700990830     C* IN TITAS/TAA
075800990830     C                   CLEAR                   VKSC
075900990830     C                   CLEAR                   VRSDB
076000990830     C                   CLEAR                   VINDB
076100990830     C                   CLEAR                   VLODB
076200990830     C                   CLEAR                   VPRDB
076300990830     C                   CLEAR                   VCADB
076400990830     C                   CLEAR                   V3CNZM
076500990830     C                   CLEAR                   VFIV
076600990830     C                   CLEAR                   VNFT
076700990830     C                   CLEAR                   VDFT
076800990830     C* AGGANCIO TITAS
076900990830     C                   Z-ADD     GCPLPW        KLNP
077000990830     C                   Z-ADD     GCPASW        KAAS
077100990830     C                   Z-ADD     GCPNSW        KNRS
077200990830     C                   Z-ADD     GCPNBW        KNSP
077300990830     C     KTAS          CHAIN     TITAS30C                           21
077400990830     C     *IN21         IFEQ      '0'
077500990830     C                   MOVE      TASCCM        VKSC
077600990830     C                   Z-ADD     TASFIV        VFIV
077700990830     C                   Z-ADD     TASNFT        VNFT
077800990830     C                   Z-ADD     TASDFT        G08INV
077900990830     C                   MOVEL     '3'           G08ERR
078000990830     C                   CALL      'XSRDA8'
078100990830     C                   PARM                    WLBDA8
078200990830     C                   Z-ADD     G08DAT        VDFT
078300990830     C* CONTROLLO SE CLIENTE CODIFICATO
078400990830     C                   MOVE      TASCCM        §QUATR
078500990830      * ... SE NO REPERISCO DECODIFICA IN TITAS
078600990830     C     §QUATR        IFEQ      *ALL'8'
078700990830     C                   MOVE      'M'           KTRC
078800990830     C     KTAA          CHAIN     TITAA30C                           21
078900990830     C     *IN21         IFEQ      '0'
079000990830     C                   MOVEL     TAARSC        VRSDB
079100990830     C                   MOVEL     TAAIND        VINDB
079200990830     C                   MOVEL     TAALOC        VLODB
079300990830     C                   MOVEL     TAAPRV        VPRDB
079400990830     C                   MOVEL     TAACAP        VCADB
079500990830     C                   MOVEL     TAANAZ        V3CNZM
079600990830     C                   ENDIF
079700990830      * ... ALTRIMENTI IN CNACO
079800990830     C                   ELSE
079900990830     C                   CLEAR                   TIBS69
080000990830     C                   CLEAR                   CNACO
080100990830     C                   CLEAR                   CNIND
080200990830     C                   CLEAR                   CNCLP
080300990830     C                   CLEAR                   FNCLS
080400990830     C                   MOVEL     ' '           I69TLA
080500990830     C                   MOVEL     KNSIF         I69SIF
080600990830     C                   Z-ADD     TASCCM        I69KAC
080700990830     C                   Z-ADD     TASCCM        I69KIN
080800990830     C                   CALL      'TIBS69R'
080900990830     C                   PARM                    TIBS69
081000990830     C                   PARM                    CNACO
081100990830     C                   PARM                    CNIND
081200990830     C                   PARM                    CNCLP
081300990830     C                   PARM                    FNCLS
081400990830     C     O69ERR        IFEQ      *BLANK
081500990830     C                   MOVEL     ACORAG        VRSDB
081600990830     C                   MOVEL     INDVIA        VINDB
081700990830     C                   MOVEL     INDCIT        VLODB
081800990830     C                   MOVEL     INDPRV        VPRDB
081900990830     C                   MOVEL     INDCAE        VCADB
082000990830     C                   MOVEL     INDSTA        V3CNZM
082100990830     C                   ENDIF
082200990830     C*
082300990830     C                   ENDIF
082400990831     C                   ENDIF
082500990830     C*
082600990830     C                   ENDSR
082700990830     C*---------------------------------------------------------------*
082800990830     C*   T A S V I D                                                 *
082900990830     C*---------------------------------------------------------------*
083000990830     C     TASVID        BEGSR
083100990830     C*
083200990830     C                   SETOFF                                       011012
083300990831     C                   Z-ADD     GCPLNP        KLNP
083400990831     C                   Z-ADD     GCPAAS        KAAS
083500990831     C                   Z-ADD     GCPNRS        KNRS
083600990831     C                   Z-ADD     GCPNSP        KNSP
083700990830     C     KTAS          CHAIN     TITAS30C                           21
083800990830     C* IMPOSTO DATI A VIDEO
083900990830     C                   MOVE      TASMGS        W040              6 0
084000990830     C                   MOVEL     W040          VDSP
084100990830     C                   MOVE      TASMGS        §GG               2 0
084200990830     C                   MOVEL     §GG           VDSP
084300990830     C                   MOVE      TASAAS        VDSP
084400990830     C* TIPO BOLLA
084500990830     C                   MOVEL     TASCBO        VTBL
084600990830     C                   MOVEL     '3A'          TBLCOD
084700990830     C                   MOVEL     *BLANK        TBLKEY
084800990830     C                   MOVEL     VTBL          TBLKEY
084900990830     C     KTAB          CHAIN     TABEL                              21
085000990830     C   21              MOVEL     *ALL'?'       DTBL
085100990830     C  N21              MOVEL     TBLUNI        DS3A
085200990830     C  N21              MOVEL     §3ADES        DTBL
085300990830     C* TIPO SPEDIZIONE
085400990830     C                   MOVEL     *BLANKS       DTSP
085500990830     C                   MOVEL     '5E'          TBLCOD
085600990830     C                   MOVEL     *BLANK        TBLKEY
085700990830     C                   MOVEL     TASTSP        TBLKEY
085800990830     C     KTAB          CHAIN     TABEL                              21
085900990830     C  N21              MOVEL     TBLUNI        DS5E
086000990830     C  N21              MOVEL     §5EDES        DTSP
086100990830     C* LINEA ARRIVO
086200990830     C                   MOVEL     TASLNA        VLNA
086300990830     C     VLNA          CHAIN     AZORG01L                           21
086400990830     C  N21ORGFVA        COMP      '*'                                    21
086500990830     C   21              MOVEL     *ALL'?'       DLNA
086600990830     C  N21              MOVEL     ORGDES        DLNA
086700990830     C*
086800160121     C****               MOVEL     TASNAS        VNAS
086900990830     C                   Z-ADD     TASNCL        VNCL
087000990830     C                   Z-ADD     TASPKF        VPKG
087100990830     C                   Z-ADD     TASVLF        VVOL
087200990830     C                   MOVEL     TASRSD        VRSD
087300990830     C                   MOVEL     *BLANKS       V1CRD2
087400990830     C                   MOVEL     TASIND        VIND
087500990830     C                   MOVEL     TASLOD        VLOD
087600990830     C                   MOVEL     TASPRD        VPRD
087700990830     C                   MOVEL     TASCAD        VCAD
087800990830     C                   MOVEL     TASNZD        V1CNZD
087900990830     C* MITTENTE
088000990830     C                   MOVE      TASCCM        VSCM
088100990830     C* CONTROLLO SE CLIENTE CODIFICATO
088200990830     C                   MOVE      TASCCM        §QUATR
088300990830      * ... SE NO REPERISCO DECODIFICA IN TITAS
088400990830     C     §QUATR        IFEQ      *ALL'8'
088500990830     C                   MOVE      'M'           KTRC
088600990830     C     KTAA          CHAIN     TITAA30C                           21
088700990830     C     *IN21         IFEQ      '0'
088800990830     C                   MOVEL     TAARSC        VRSM
088900990830     C                   MOVEL     TAAIND        VINM
089000990830     C                   MOVEL     TAALOC        VLOM
089100990830     C                   MOVEL     TAAPRV        VPRM
089200990830     C                   MOVEL     TAACAP        VCAM
089300990830     C                   MOVEL     TAANAZ        V1CNZM
089400990830     C                   ENDIF
089500990830      * ... ALTRIMENTI IN CNACO
089600990830     C                   ELSE
089700990830     C                   CLEAR                   TIBS69
089800990830     C                   CLEAR                   CNACO
089900990830     C                   CLEAR                   CNIND
090000990830     C                   CLEAR                   CNCLP
090100990830     C                   CLEAR                   FNCLS
090200990830     C                   MOVEL     ' '           I69TLA
090300990830     C                   MOVEL     KNSIF         I69SIF
090400990830     C                   Z-ADD     TASCCM        I69KAC
090500990830     C                   Z-ADD     TASCCM        I69KIN
090600990830     C                   CALL      'TIBS69R'
090700990830     C                   PARM                    TIBS69
090800990830     C                   PARM                    CNACO
090900990830     C                   PARM                    CNIND
091000990830     C                   PARM                    CNCLP
091100990830     C                   PARM                    FNCLS
091200990830     C     O69ERR        IFEQ      *BLANK
091300990830     C                   MOVEL     ACORAG        VRSM
091400990830     C                   MOVEL     INDVIA        VINM
091500990830     C                   MOVEL     INDCIT        VLOM
091600990830     C                   MOVEL     INDPRV        VPRM
091700990830     C                   MOVEL     INDCAE        VCAM
091800990830     C                   MOVEL     INDSTA        V1CNZM
091900990830     C                   ENDIF
092000990830     C*
092100990830     C                   ENDIF
092200990830     C                   Z-ADD     TASRMN        VRIF
092300990830     C* REPERISCO RIFERIMENTO MITTENTE ALFABETICO
092400160121     c*           e natura merce
092500990830     C                   MOVEL     'A'           KTRC
092600990830     C     KTA4          CHAIN     TITA430C                           21
092700160121     c  n12              movel     ta4not        dta4a
092800160121     c   12              clear                   dta4a
092900160121     C**N12              MOVEL     TA4NOT        V1CRMA
093000160121     C                   MOVEL     §ta4arma      V1CRMA
093100160121     C                   MOVEL     §ta4anas      Vnas
093200990830      * GESTIONE PARTICOLARITA' GIACENZE
093300990830     C                   MOVEL     TASGGA        V1CGGA
093400990830      * CONTRASSEGNO
093500990830     C     KCSB          CHAIN     TNCSB03L                           21
093600990830     C  N21              Z-ADD     CSBCAS        VASS
093700990830     C  N21              MOVEL     CSBVCA        V1CDIV
093800990830     C   21              CLEAR                   VASS
093900990830     C   21              CLEAR                   V1CDIV
094000990830     C*
094100990830     C                   ENDSR
094200900510     C*---------------------------------------------------------------*
094300900510     C*   B L P V I D                                                 *
094400900510     C*---------------------------------------------------------------*
094500900510     C     BLPVID        BEGSR
094600941209     C                   MOVE      BLPMGS        W040              6 0
094700941209     C                   MOVEL     W040          VDSP
094800900510     C                   MOVE      BLPMGS        §GG               2 0
094900900510     C                   MOVEL     §GG           VDSP
095000900510     C                   MOVE      BLPAAS        VDSP
095100900510     C* TIPO BOLLA
095200941209     C                   MOVEL     BLPCBO        VTBL
095300900510     C                   MOVEL     '3A'          TBLCOD
095400900510     C                   MOVEL     *BLANK        TBLKEY
095500941209     C                   MOVEL     VTBL          TBLKEY
095600900510     C     KTAB          CHAIN     TABEL                              21
095700900510     C   21              MOVEL     *ALL'?'       DTBL
095800900510     C  N21              MOVEL     TBLUNI        DS3A
095900900510     C  N21              MOVEL     §3ADES        DTBL
096000990929     C  N21              MOVEL     §3ATB1        WPORTO            1
096100941213     C* TIPO SPEDIZIONE
096200941213     C                   MOVEL     *BLANKS       DTSP
096300941213     C                   MOVEL     '5E'          TBLCOD
096400941213     C                   MOVEL     *BLANK        TBLKEY
096500941213     C                   MOVEL     BLPTSP        TBLKEY
096600941213     C     KTAB          CHAIN     TABEL                              21
096700941213     C  N21              MOVEL     TBLUNI        DS5E
096800941213     C  N21              MOVEL     §5EDES        DTSP
096900900510     C* LINEA ARRIVO
097000900510     C                   MOVEL     BLPLNA        VLNA
097100900510     C     VLNA          CHAIN     AZORG01L                           21
097200900510     C  N21ORGFVA        COMP      '*'                                    21
097300900510     C   21              MOVEL     *ALL'?'       DLNA
097400900510     C  N21              MOVEL     ORGDES        DLNA
097500900510     C*
097600900510     C                   MOVEL     BLPNAS        VNAS
097700900510     C                   Z-ADD     BLPNCL        VNCL
097800941209     C                   Z-ADD     BLPPKF        VPKG
097900941209     C                   Z-ADD     BLPVLF        VVOL
098000900510     C* DESTINATARIO
098100960124     C                   MOVEL     'D'           §TRC
098200060213     C     KAR4          CHAIN     Fiar401L                           21
098300960124     C* Controllo se esiste destinatario finale
098400960124     C                   SETOFF                                       13
098500960124     C                   CLEAR                   FNLSN0
098600960124     C                   Z-ADD     BLPAAS        DN0AAS
098700960124     C                   Z-ADD     BLPLNP        DN0LNP
098800960124     C                   Z-ADD     BLPNRS        DN0NRS
098900960124     C                   Z-ADD     BLPNSP        DN0NSP
099000960124     C                   MOVEL     *BLANKS       DN0TRC
099100960125     C                   Z-ADD     GCPDUR        DN0DAG
099200960124     C                   MOVEL     KPJBU         WSVKPJ          256
099300960124     C                   MOVEL     FNLSN0        KPJBU
099400960124     C                   CALL      'FNLSN0R'
099500960124     C                   PARM                    KPJBA
099600960124     C                   MOVEL     KPJBU         FNLSN0
099700960124     C                   MOVEL     WSVKPJ        KPJBU
099800960124     C* Destinatario
099900960124     C     DN0TRV        IFEQ      'S'
100000960124     C                   SETON                                        13
100100960124     C                   MOVEL     DN0RSD        VRSD
100200960124     C                   MOVEL     DN0RS2        V1CRD2
100300960124     C                   MOVEL     DN0IND        VIND
100400960124     C                   MOVEL     DN0LOD        VLOD
100500960124     C                   MOVEL     DN0PRD        VPRD
100600960124     C                   MOVEL     DN0CAD        VCAD
100700960124     C                   MOVEL     DN0NZD        V1CNZD
100800960124     C                   MOVEL     BLPRSD        V6CRSD
100900060213     C                   MOVEL     ar4NOT        V6CDE2
101000960124     C                   MOVEL     BLPIND        V6CIND
101100960124     C                   MOVEL     BLPLOD        V6CLOD
101200960124     C                   MOVEL     BLPPRD        V6CPRD
101300960124     C                   MOVEL     BLPCAD        V6CCAD
101400960124     C                   MOVEL     BLPNZD        V6CNZD
101500960124     C                   ELSE
101600900510     C                   MOVEL     BLPRSD        VRSD
101700060213     C  N21              MOVEL     ar4NOT        V1CRD2
101800941209     C   21              MOVEL     *BLANKS       V1CRD2
101900941209      *
102000900510     C                   MOVEL     BLPIND        VIND
102100900510     C                   MOVEL     BLPLOD        VLOD
102200900510     C                   MOVEL     BLPPRD        VPRD
102300900510     C                   MOVEL     BLPCAD        VCAD
102400941209     C                   MOVEL     BLPNZD        V1CNZD
102500960124     C                   END
102600900510     C* MITTENTE
102700900510     C     BLPCCM        IFNE      *ZEROS
102800900510     C                   MOVE      BLPCCM        VSCM
102900900510     C                   ELSE
103000900510     C                   MOVE      BLPKSC        VSCM
103100900510     C                   END
103200900510     C                   MOVEL     BLPRSM        VRSM
103300900510     C                   MOVEL     BLPINM        VINM
103400900510     C                   MOVEL     BLPLOM        VLOM
103500900510     C                   MOVEL     BLPPRM        VPRM
103600900510     C                   MOVEL     BLPCAM        VCAM
103700941209     C                   MOVEL     BLPNZM        V1CNZM
103800921014     C                   Z-ADD     BLPRMN        VRIF
103900941209     C                   MOVEL     BLPRMA        V1CRMA
104000941209      * GESTIONE PARTICOLARITA' GIACENZE
104100941209     C                   MOVEL     BLPGGA        V1CGGA
104200941209      * CONTRASSEGNO
104300051115     C     KSPE          CHAIN     Fiar901L                           21
104400051115     C  N21              Z-ADD     ar9CAS        VASS
104500051115     C  N21              MOVEL     ar9VCA        V1CDIV
104600941209     C   21              CLEAR                   VASS
104700941209     C   21              CLEAR                   V1CDIV
104800941209      * TASSAZIONE
104900990929     C     WPORTO        IFEQ      'F'
105000990929     C                   MOVEL     '1'           KTRC
105100990929     C     KAR6          CHAIN     FIAR601L                           21
105200990929     C                   ELSE
105300990929     C                   SETON                                        21
105400990929     C                   ENDIF
105500941209     C     *IN21         IFEQ      '0'
105600990929     C                   Z-ADD     AR6IFT        VIFT
105700991119     C                   MOVEL     AR6DIV        V1CDFA
105800941209     C                   ELSE
105900941209     C                   CLEAR                   VIFT
106000991119     C                   CLEAR                   V1CDFA
106100941209     C                   ENDIF
106200900510     C                   ENDSR
106300911118     C*---------------------------------------------------------------*
106400911118     C     ARBVID        BEGSR
106500941213     C                   MOVE      ARBMGS        W040              6 0
106600941209     C                   MOVEL     W040          VDSP
106700911118     C                   MOVE      ARBMGS        §GG               2 0
106800911118     C                   MOVEL     §GG           VDSP
106900911118     C                   MOVE      ARBAAS        VDSP
107000911118     C* TIPO SPEDIZIONE
107100911118     C                   MOVEL     *BLANKS       DTSP
107200941212     C                   MOVEL     '5E'          TBLCOD
107300941212     C                   MOVEL     *BLANK        TBLKEY
107400941212     C                   MOVEL     ARBTSP        TBLKEY
107500941212     C     KTAB          CHAIN     TABEL                              21
107600941212     C  N21              MOVEL     TBLUNI        DS5E
107700941212     C  N21              MOVEL     §5EDES        DTSP
107800911118     C* TIPO BOLLA
107900941209     C                   MOVEL     ARBCBO        VTBL
108000911118     C                   MOVEL     '3A'          TBLCOD
108100911118     C                   MOVEL     *BLANK        TBLKEY
108200911118     C                   MOVEL     VTBL          TBLKEY
108300911118     C     KTAB          CHAIN     TABEL                              21
108400911118     C   21              MOVEL     *ALL'?'       DTBL
108500911118     C  N21              MOVEL     TBLUNI        DS3A
108600911118     C  N21              MOVEL     §3ADES        DTBL
108700990929     C  N21              MOVEL     §3ATB1        WPORTO            1
108800911118     C* LINEA ARRIVO
108900911118     C                   MOVEL     ARBLNA        VLNA
109000911118     C     VLNA          CHAIN     AZORG01L                           21
109100911118     C  N21ORGFVA        COMP      '*'                                    21
109200911118     C   21              MOVEL     *ALL'?'       DLNA
109300911118     C  N21              MOVEL     ORGDES        DLNA
109400911118     C*
109500911118     C                   MOVEL     ARBNAS        VNAS
109600911118     C                   Z-ADD     ARBNCL        VNCL
109700941209     C                   Z-ADD     ARBVLF        VVOL
109800941209     C                   Z-ADD     ARBPKF        VPKG
109900911118     C* DESTINATARIO
110000911118     C                   MOVEL     ARBRSD        VRSD
110100941209     C                   MOVEL     'D'           §TRC
110200060213     C     KAR4          CHAIN     FiAR401L                           21
110300941209     C  N21              MOVEL     AR4NOT        V1CRD2
110400941209     C   21              MOVEL     *BLANKS       V1CRD2
110500941209      *
110600911118     C                   MOVEL     ARBIND        VIND
110700911118     C                   MOVEL     ARBLOD        VLOD
110800911118     C                   MOVEL     ARBPRD        VPRD
110900911118     C                   MOVEL     ARBCAD        VCAD
111000941209     C                   MOVEL     ARBNZD        V1CNZD
111100911118     C* MITTENTE
111200911118     C     ARBCCM        IFNE      *ZEROS
111300911118     C                   MOVEL     ARBCCM        VSCM
111400911118     C                   ELSE
111500911118     C                   MOVEL     ARBKSC        VSCM
111600911118     C                   END
111700930324     C                   MOVEL     ARBRMN        VRIF
111800941209     C                   MOVEL     ARBRMA        V1CRMA
111900911118     C                   MOVEL     ARBRSM        VRSM
112000911118     C                   MOVEL     ARBINM        VINM
112100911118     C                   MOVEL     ARBLOM        VLOM
112200911118     C                   MOVEL     ARBPRM        VPRM
112300911118     C                   MOVEL     ARBCAM        VCAM
112400941209     C                   MOVEL     ARBNZM        V1CNZM
112500941209      * PARTICOLATITA GIACENZA
112600941209     C                   MOVEL     ARBGGA        V1CGGA
112700911118     C* SE SIAMO IN RIAPERTURA METTO COD. MANCATA CONSEGNA
112800911118     C* CHE C'E SULLA BOLLA
112900911118     C   42              MOVEL     ARBCMC        VCMC
113000911118     C* LUOGO DEPOSITO MERCE  PROPONGO FILIALE ARRIVO DELLA BOLLA
113100911118     C                   MOVE      ARBLNA        VCCR
113200911118     C* DATA COMUNICAZIONE INTERNA = DATA DIST. PAD. SE IN ARRIVO
113300941209     C                   Z-ADD     ARBDDC        G08INV
113400941209     C                   MOVE      *ZEROS        G08DAT
113500941209     C                   MOVEL     *BLANK        G08ERR
113600941209     C                   CALL      'XSRDA8'
113700941209     C                   PARM                    WLBDA8
113800941209     C                   Z-ADD     G08DAT        VDXD
113900941209      * CONTRASSEGNO
114000051115     C     KSPE          CHAIN     FIAR901L                           21
114100941209     C  N21              Z-ADD     AR9CAS        VASS
114200941209     C  N21              MOVEL     AR9VCA        V1CDIV
114300941209     C   21              CLEAR                   VASS
114400941209     C   21              CLEAR                   V1CDIV
114500941209      * TASSAZIONE
114600990929     C     WPORTO        IFEQ      'A'
114700990929     C                   MOVEL     '1'           KTRC
114800990929     C     KAR6          CHAIN     FIAR601L                           21
114900990929     C                   ELSE
115000990929     C                   SETON                                        21
115100990929     C                   ENDIF
115200941209     C     *IN21         IFEQ      '0'
115300941209     C                   Z-ADD     AR6IFT        VIFT
115400991119     C                   MOVEL     AR6DIV        V1CDFA
115500941209     C                   ELSE
115600941209     C                   CLEAR                   VIFT
115700991119     C                   CLEAR                   V1CDFA
115800941209     C                   ENDIF
115900911118     C                   ENDSR
116000960124     C*-----------------------------------------------------*
116100960124     C*  Gestione della sesta videata
116200960124     C*-----------------------------------------------------*
116300960124     C     GESD06        BEGSR
116400960124     C*
116500960124     C                   EXFMT     LG10D06
116600960124     C*                    ------------
116700960124     C*  Ritorno
116800960124     C     *INKL         IFEQ      '1'
116900960124     C                   MOVEL     'S'           WRITOR
117000960124     C                   END
117100960124     C*
117200960124     C                   ENDSR
117300970108     C*---------------------------------------------------------------*
117400970108     C     CTRFAX        BEGSR
117500970108     C*
117600970108     C                   Z-ADD     35            §FAS
117700970108     C                   SETOFF                                       12
117800050307     C     KNOT          SETLL     tIGNP01L                               12
117900970108     C*
118000970108     C                   ENDSR
118100970624     C*---------------------------------------------------------------*
118200970624     C     CTRAPE        BEGSR
118300970624     C*
118400970624     C                   Z-ADD     12            §FAS
118500970624     C                   SETOFF                                       11
118600050307     C     KNOT          SETLL     tIGNP01L                               11
118700970624     C*
118800970624     C                   ENDSR
118900911118     C*---------------------------------------------------------------*
119000911118     C     GESNOT        BEGSR
119100911118     C                   EXSR      PULIZ
119200911119     C                   Z-ADD     20            §FAS
119300050307     C     KNOT          SETLL     tIGNP01L
119400911118     C     *IN21         DOUEQ     '1'
119500050307     C     KNOT          READE     tIGNP01L                               21
119600911118     C     *IN21         IFEQ      '0'
119700911125     C                   MOVEL     GNPDMC        VDMC1
119800911118     C                   MOVEL     GNPTPN        VTPN
119900911118     C                   MOVEL     GNPNPR        VNPR
120000911118     C                   ADD       1             NRR               3 0
120100941209     C                   WRITE     LG10S02
120200951026     C                   END
120300951026     C                   END
120400960208     C     NRR           DIV       10            WRESTO            2 0
120500960208     C                   MVR                     §NR               2 0
120600960208     C     10            SUB       §NR           §NR
120700941212     C                   DO        §NR
120800941212     C                   MOVEL     *BLANK        VDMC1
120900941212     C                   MOVEL     *BLANK        VTPN
121000941212     C                   ADD       1             VNPR
121100941212     C                   ADD       1             NRR
121200941212     C                   WRITE     LG10S02
121300941212     C                   END
121400960208     C                   Z-ADD     1             NR2KEY
121500911118     C*
121600911118     C     STRNOT        TAG
121700941209     C                   WRITE     LG10Z02
121800941209     C                   EXFMT     LG10C02
121900970108     C* RICHIAMO VISUALIZZAZIONE FAX
122000970108     C     *INKM         IFEQ      '1'
122100020927      * richiamo pgm reperimento coda fax
122200020927     c                   move      'L'           finep
122300020927     c                   z-add     simpou        codpo
122400020927     c                   move      '2'           tpcoda
122500020927     c                   clear                   nomcoda
122600020927     c                   clear                   libcoda
122700020927     c                   clear                   esito
122800020927     c                   call      'TRULOUTQ'
122900020927     c                   parm                    finep
123000020927     c                   parm                    codpo
123100020927     c                   parm                    tpcoda
123200020927     c                   parm                    nomcoda
123300020927     c                   parm                    libcoda
123400020927     c                   parm                    esito
123500020927     c                   if        esito <> '0' or nomcoda = *blank
123600020927     c                   eval      nomcoda = 'QDKT'
123700020927     c                   endif
123800020927     c
123900970108     C                   CLEAR                   FNLG21
124000970108     C                   Z-ADD     GCPAAS        D21AAS
124100970108     C                   Z-ADD     GCPLNP        D21LNP
124200970108     C                   Z-ADD     GCPNRS        D21NRS
124300970108     C                   Z-ADD     GCPNSP        D21NSP
124400970108     C                   Z-ADD     GCPAGC        D21AGC
124500970108     C                   Z-ADD     GCPFGC        D21FGC
124600970108     C                   Z-ADD     GCPNGC        D21NGC
124700970108     C                   MOVEL     'G'           D21TPR
124800970108     C                   MOVEL     'V'           D21VIS
124900970108     C                   MOVEL     GCPDIS        D21DIS
125000970418     C                   MOVEL     GCPFRG        D21FRG
125100970108     C*  Richiamo videata x fax
125200070720     C*  SE SEDE OVRDBF PER FIAR401L
125300070720     c                   if        *in93 = '1'
125400070720     c                   call      'FNLG21C2'
125500070720     C                   PARM      '1'           FLAGC2            1
125600070720     C                   ENDIF
125700020927     C                   MOVE      nomcoda       D21PRF
125800970108     C                   MOVEL     FNLG21        KPJBU
125900970108     C                   CALL      'FNLG21C'
126000970108     C                   PARM                    KPJBA
126100970108     C                   END
126200070720     C*  SE SEDE OVRDBF PER FIAR401L
126300070720     c                   if        *in93 = '1'
126400070720     c                   call      'FNLG21C2'
126500070720     C                   PARM      '2'           FLAGC2
126600070720     C                   ENDIF
126700970108     C*
126800941209     C   KL              GOTO      ENDNOT
126900941209     C                   GOTO      STRNOT
127000911118     C     ENDNOT        ENDSR
127100970624     C*---------------------------------------------------------------*
127200970624     C*   CALL03: Richiamo pgm per visualizzazione note apertura      *
127300970624     C*---------------------------------------------------------------*
127400970624     C     CALL03        BEGSR
127500970624     C*
127600970624     C* Imposto parametri
127700970624     C                   CLEAR                   FNLG03
127800970624     C                   Z-ADD     VLNP          D03LNP
127900970624     C                   Z-ADD     VNRS          D03NRS
128000970624     C                   Z-ADD     VNSP          D03NSP
128100970624     C                   Z-ADD     VDSP          D03DSP
128200970624     C                   MOVEL     VTBL          D03CBO
128300970624     C                   MOVEL     DTBL          D03DBO
128400970624     C                   Z-ADD     VFGC          D03FGC
128500970624     C                   MOVEL     DFGC          D03DFG
128600970624     C                   Z-ADD     VNGC          D03NGC
128700970624     C                   Z-ADD     VDTG          D03DGC
128800970624     C                   Z-ADD     VDIG          D03DUR
128900970624     C                   Z-ADD     VNRC          D03NRC
129000970624     C                   Z-ADD     VDXD          D03DXD
129100970624     C                   MOVEL     VCCR          D03CCR
129200970624     C                   MOVEL     DCCR          D03DCR
129300970624     C                   MOVE      VSCM          D03KSC
129400970624     C                   Z-ADD     VRIF          D03RMN
129500970624     C                   MOVEL     V1CRMA        D03RMA
129600970624     C                   MOVEL     VRSM          D03RSM
129700970624     C                   MOVEL     VINM          D03INM
129800970624     C                   MOVEL     VCAM          D03CAM
129900970624     C                   MOVEL     VLOM          D03LOM
130000970624     C                   MOVEL     VPRM          D03PRM
130100970624     C                   MOVEL     V1CNZM        D03NZM
130200970624     C                   MOVEL     VRSD          D03RSD
130300970624     C                   MOVEL     V1CRD2        D03RS2
130400970624     C                   MOVEL     VIND          D03IND
130500970624     C                   MOVEL     VCAD          D03CAD
130600970624     C                   MOVEL     VLOD          D03LOD
130700970624     C                   MOVEL     VPRD          D03PRD
130800970624     C                   MOVEL     V1CNZD        D03NZD
130900970624     C                   MOVEL     VCMC          D03CMC
131000970625     C                   MOVEL     DCMC          D03CMD
131100970624     C                   MOVEL     VDMC          D03DMC
131200970624     C                   MOVEL     'V'           D03TPV
131300970624     C                   CALL      'FNLG03R'
131400970624     C                   PARM                    FNLG03
131500970624     C*
131600970624     C                   ENDSR
131700970624     C*---------------------------------------------------------------*
131800911118     C* PULIZIA SUBFILE
131900911118     C     PULIZ         BEGSR
132000911118     C                   SETON                                        70
132100941209     C                   WRITE     LG10C02
132200911118     C                   Z-ADD     0             NRR
132300911118     C                   SETOFF                                       70
132400911118     C                   ENDSR
132500941209     C*---------------------------------------------------------------*
132600941209     C*   I N Z S R                                                   *
132700941209     C*---------------------------------------------------------------*
132800941209     C     *INZSR        BEGSR
132900990830     C*
133000941209     C     *ENTRY        PLIST
133100941209     C                   PARM                    KPJBA
133200941209     C*
133300941209     C                   MOVEL     KPJBU         FNLG30
133400941209     C*
133500941209     C                   Z-ADD     1             CODUT
133600941209     C                   CALL      'X§PARUT'
133700941209     C                   PARM                    UTEDSE
133800941209     C     CODUT         CABEQ     -1            FINE
133900941212     C                   MOVEL     RAGUT         RSUT             20
134000941209     C                   MOVEL     §PGM          VPROGR
134100990831     C*-                                                   *
134200990831     C                   MOVEL     REC80         CNCR80
134300941209      *
134400941209     C     D30FRG        IFNE      *BLANK
134500941209     C                   MOVE      D30FRG        §FRG
134600941209     C                   END
134700941209      *
134800941209     C                   SETON                                        9076
134900950103     C                   SETOFF                                       85
135000941209     C     D30RIC        COMP      'S'                                    87
135100950103     C   87              SETON                                        85
135200941209     C*                                                    DISABILITA
135300941209     C*                                                    L'INT STORIC
135400990830      * SEDE
135500990830     C     SIMFEL        IFEQ      0
135600990830     C                   SETON                                        93
135700050411     C                   OPEN      TIgcp51l
135800050415     C                   OPEN      TIgcp52l
135900050411     C                   OPEN      TITAS30C
136000990830     C                   OPEN      TITAA30C
136100990830     C                   OPEN      TITA430C
136200990830     C                   OPEN      TNCSB03L
136300990901     C                   MOVEL     '  DI SED'    V1CGES
136400990901     C                   MOVE      'E  '         V1CGES
136500990830     C                   ELSE
136600941209     C* VERIFICO SE SIAMO IN PARTENZA O ARRIVO
136700941209     C     D30POA        IFEQ      'A'
136800941209     C                   SETON                                        80
136900941209     C                   MOVEL     '   ARRIV'    V1CGES
137000941209     C                   MOVE      'O  '         V1CGES
137100941209     C                   OPEN      FNARB01L
137200990929     C                   OPEN      FIAR601L
137300051115     C                   OPEN      FIAR901L
137400060213     C                   OPEN      FIAR401L
137500941212     C                   OPEN      FNLBL01L
137600040126     C                   OPEN      FIAR501L
137700941209     C                   ELSE
137800941209     C                   MOVEL     ' PARTENZ'    V1CGES
137900941209     C                   MOVE      'A  '         V1CGES
138000941209     C                   OPEN      FNBLP01L
138100990929     C                   OPEN      FIAR601L
138200051115     C                   OPEN      Fiar901L
138300060213     C                   OPEN      FIAR401L
138400040126     C                   OPEN      FIAR501L
138500941209     C                   ENDIF
138600990830     C                   ENDIF
138700990830     C*
138800941209     C*--- RICERCA CAPOCONTI------------------------------------------*
138900941209     C                   DO        50            X                 2 0
139000941209     C                   MOVE      TCU(X)        TCUDS
139100941209     C     F56           CABNE     'CG'          END1
139200941209     C     F4            COMP      '1'                                    21
139300941209     C** 1 CLIENTI   21
139400941209     C     F3            COMP      '0'                                242425
139500941209     C** 0 ITALIA   25
139600941209     C** 1 ESTERO   24
139700941209     C   21
139800941209     CAN 25              Z-ADD     KCU(X)        KCI               4 0
139900941209     C     END1          TAG
140000941209     C                   END
140100941209     C*---------------------------------------------------------------*
140200941209     C*§
140300941209     C*  KLIST
140400941209     C     KGIA          KLIST
140500941209     C                   KFLD                    D30AGC
140600941209     C                   KFLD                    D30FGC
140700941209     C                   KFLD                    D30NGC
140800941209     C                   KFLD                    §FRG
140900000201     C     KBOLW         KLIST
141000000201     C                   KFLD                    GCPASW
141100000201     C                   KFLD                    GCPLPW
141200000201     C                   KFLD                    GCPNSW
141300000201     C                   KFLD                    GCPNBW
141400941209     C*
141500050307     C     KBOLW2        KLIST
141600050307     C                   KFLD                    GCPASWa
141700050307     C                   KFLD                    GCPLPWa
141800050307     C                   KFLD                    GCPNSWa
141900050307     C                   KFLD                    GCPNBWa
142000050307     C*
142100941209     C     KSPE          KLIST
142200941209     C                   KFLD                    §AAS
142300941209     C                   KFLD                    §LNP
142400941209     C                   KFLD                    §NRS
142500941209     C                   KFLD                    §NSP
142600990929     C     KAR6          KLIST
142700990929     C                   KFLD                    §AAS
142800990929     C                   KFLD                    §LNP
142900990929     C                   KFLD                    §NRS
143000990929     C                   KFLD                    §NSP
143100990929     C                   KFLD                    KTRC
143200941209     C*
143300941209     C     KAR4          KLIST
143400941209     C                   KFLD                    §AAS
143500941209     C                   KFLD                    §LNP
143600941209     C                   KFLD                    §NRS
143700941209     C                   KFLD                    §NSP
143800941209     C                   KFLD                    §TRC
143900990831     C     KTAS          KLIST
144000990831     C                   KFLD                    KAAS
144100990831     C                   KFLD                    KLNP
144200990831     C                   KFLD                    KNRS
144300990831     C                   KFLD                    KNSP
144400990831     C     KCSB          KLIST
144500990831     C                   KFLD                    KAAS
144600990831     C                   KFLD                    KLNP
144700990831     C                   KFLD                    KNRS
144800990831     C                   KFLD                    KNSP
144900990831     C     KTAA          KLIST
145000990831     C                   KFLD                    KAAS
145100990831     C                   KFLD                    KLNP
145200990831     C                   KFLD                    KNRS
145300990831     C                   KFLD                    KNSP
145400990831     C                   KFLD                    KTRC
145500990831     C     KTA4          KLIST
145600990831     C                   KFLD                    KAAS
145700990831     C                   KFLD                    KLNP
145800990831     C                   KFLD                    KNRS
145900990831     C                   KFLD                    KNSP
146000990831     C                   KFLD                    KTRC
146100941209     C*
146200941209     C     KTAB          KLIST
146300941209     C                   KFLD                    CODUT
146400941209     C                   KFLD                    TBLCOD
146500941209     C                   KFLD                    TBLKEY
146600941209     C*
146700941209     C     KNOT          KLIST
146800941209     C                   KFLD                    GCPAGC
146900941209     C                   KFLD                    GCPFGC
147000941209     C                   KFLD                    GCPNGC
147100990628     C                   KFLD                    §FRG              2 0
147200941209     C                   KFLD                    §FAS              3 0
147300941209     C*
147400941209     C     KNOR          KLIST
147500941209     C                   KFLD                    GCPAGC
147600941209     C                   KFLD                    GCPFGC
147700941209     C                   KFLD                    GCPNGC
147800941209     C                   KFLD                    §FRG
147900941209     C                   KFLD                    §FAS
148000941209     C                   KFLD                    VNPR
148100040126     C*
148200040126     C     Kar5          KLIST
148300040126     C                   KFLD                    §AAS
148400040126     C                   KFLD                    §LNP
148500040126     C                   KFLD                    §NRS
148600040126     C                   KFLD                    §NSP
148700040126     C                   KFLD                    trcar5
148800941209     C*
148900941209     C                   SETON                                        90
149000941209     C*
149100040126     C     *LIKE         DEFINE    ar5trd        trcar5
149200941209     C     *LIKE         DEFINE    GCPAAS        §AAS
149300941209     C     *LIKE         DEFINE    GCPLNP        §LNP
149400941209     C     *LIKE         DEFINE    GCPNRS        §NRS
149500941209     C     *LIKE         DEFINE    GCPNSP        §NSP
149600941209     C     *LIKE         DEFINE    ACOKSC        §KSC
149700941209     C     *LIKE         DEFINE    AR4TRC        §TRC
149800990831     C     *LIKE         DEFINE    TASAAS        KAAS
149900990831     C     *LIKE         DEFINE    TASLNP        KLNP
150000990831     C     *LIKE         DEFINE    TASNRS        KNRS
150100990831     C     *LIKE         DEFINE    TASNSP        KNSP
150200990831     C     *LIKE         DEFINE    TAATRC        KTRC
150300941209     C                   MOVEL     *ZEROS        G08DAT
150400941209     C                   MOVEL     *ZEROS        G08TGI
150500941209     C                   MOVE      *ZEROS        G08INV
150600941209     C                   MOVEL     *BLANK        G08ERR
150700941209     C*
150800941209     C                   ENDSR
