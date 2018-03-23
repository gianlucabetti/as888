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
006701160121     D dta4a         E DS
006800941209     D WLBDA8          DS
006900941209     D  G08DAT                 1      8  0
007000941209     D  G08INV                 9     16  0
007100941209     D  G08ERR                17     17
007200941209     D  G08TGI                18     22  0
007300941209     D                SDS
007400941209     D  §PGM                   1     10
007500020927      *parametri per reperimento coda
007600020927     d finep           s              1
007700020927     d codpo           s              3s 0
007800020927     d tpcoda          s              1
007900020927     d nomcoda         s             10
008000020927     d libcoda         s             10
008100020927     d esito           s              1
008200990830     I*--------------------------------------------------------------*
008300990830     ITITASP00      12
008400990830     ITITAS000      01
008500990830     ITITAS010      10
008600990830     C*--------------------------------------------------------------*
008700900510     C*  CARICO DATI A VIDEO
008800900510     C*
008900941209     C     D30NGC        IFEQ      *ZEROS
009000941209     C                   Z-ADD     D30AAS        §AAS
009100941209     C                   Z-ADD     D30LNP        §LNP
009200941209     C                   Z-ADD     D30NRS        §NRS
009300941209     C                   Z-ADD     D30NSP        §NSP
009400050411     C     SIMFEL        IFEQ      0
009500050411     C     KSPE          CHAIN     tigcp51L                           21
009600050411     c                   else
009700050411     C     KSPE          CHAIN     tigcp01L                           21
009800050411     c                   end
009900900619     C                   ELSE
010000050415     C     simfel        ifeq      0
010100050415     C     KGIA          CHAIN     tigcp52L                           21
010200050415     C                   else
010300050307     C     KGIA          CHAIN     tigcp02L                           21
010400050415     C                   endif
010500900619     C                   END
010600941213     C     *IN21         IFEQ      *ON
010700941213     C                   MOVEL     ' '           D30F03
010800941213     C                   MOVEL     '1'           D30F12
010900941213     C                   MOVEL     '1'           D30ERR
011000941213     C                   MOVEL     FNLG30        KPJBU
011100941213     C                   GOTO      FINE
011200941213     C                   ENDIF
011300900510     C                   EXSR      GCPVID
011400970624     C* CONTROLLO SE ESISTONO NOTE APERTURA
011500970624     C     D30POA        IFEQ      'A'
011600970624     C                   EXSR      CTRAPE
011700970624     C                   END
011800900418     C*
011900900418     C     EMFG01        TAG
012000950117     C                   SETOFF                                       14
012100950117     C*------------------------------------------------------------*
012200950117     C*  Se filiale di apertura giacenza diversa da linea di arrivo
012300950117     C*  (bolla originale) ---> Reverse Immage su VLNA
012400950117     C     VLNA          COMP      VFGC                               0707
012500950117     C*------------------------------------------------------------*
012600941209     C                   EXFMT     LG10D01
012700900418     C*                    ------------
012800941212     C*  CMD 12  RITORNO
012900941212     C     *INKL         IFEQ      '1'
013000941212     C                   MOVEL     D30POA        SAVPOA            1
013100941212     C                   CLEAR                   FNLG30
013200941212     C                   MOVEL     ' '           D30F03
013300941212     C                   MOVEL     '1'           D30F12
013400941212     C                   MOVEL     SAVPOA        D30POA
013500941212     C                   MOVEL     FNLG30        KPJBU
013600941212     C                   GOTO      FINE
013700941212     C                   ENDIF
013800960124     C*  VISUALIZZA DESTINATARIO ORIGINALE
013900960124     C     *INKM         IFEQ      '1'
014000960124     C                   MOVEL     'N'           WRITOR            1
014100960124     C     WRITOR        DOWEQ     'N'
014200960124     C                   EXSR      GESD06
014300960124     C                   END
014400960124     C                   GOTO      EMFG01
014500960124     C                   END
014600900418     C*
014700970624     C*  CMD 11  NOTE APERTURA
014800970624     C   KK              EXSR      CALL03
014900970624     C   KK              GOTO      EMFG01
015000911118     C*  CMD 14  GESTIONE NOTE
015100970108     C   KN              EXSR      CTRFAX
015200970108     C   KN              EXSR      GESNOT
015300941213     C   KL              GOTO      EMFG01
015400941209     C* INTERROGO LA PRTICOLARITà GIACENZE
015500941209     C     '?'           SCAN      V1CRPG                                 32
015600941209     C     *IN32         IFEQ      '1'
015700941213     C                   CLEAR                   DSTB69
015800941209     C                   MOVEL     'S'           DS7RIC
015900941209     C                   MOVEL     V1CGGA        DS7COD
016000941209     C                   MOVEL     'V'           DS7GES
016100941209     C                   MOVEL     DSTB69        KPJBU
016200941209     C                   CALL      'TRTB69R'
016300941209     C                   PARM                    KPJBA
016400941209     C                   MOVEL     ' '           V1CRPG
016500941209     C                   GOTO      EMFG01
016600941209     C                   END
016700911118     C*
016800911119     C*  CMD 04  GESTIONE STORICO
016900941212     C     *INKD         IFEQ      *ON
017000941212     C                   MOVEL     'S'           D30RIC
017100941209     C                   Z-ADD     GCPAGC        D30AGC
017200941209     C                   Z-ADD     GCPFGC        D30FGC
017300941209     C                   Z-ADD     GCPNGC        D30NGC
017400941212     C                   CLEAR                   D30F03
017500941212     C                   CLEAR                   D30F12
017600941213     C                   CLEAR                   D30ERR
017700050307     c                   if        d30poa <> 'A'
017800000403     C                   Z-ADD     GCPAAS        D30AAS
017900000403     C                   Z-ADD     GCPLNP        D30LNP
018000000403     C                   Z-ADD     GCPNRS        D30NRS
018100000403     C                   Z-ADD     GCPNSP        D30NSP
018200050307     c                   else
018300050307     C                   Z-ADD     GCPAASa       D30AAS
018400050307     C                   Z-ADD     GCPLNPa       D30LNP
018500050307     C                   Z-ADD     GCPNRSa       D30NRS
018600050307     C                   Z-ADD     GCPNSPa       D30NSP
018700050307     c                   end
018800000403     C                   MOVE      GCPFRG        D30FRG
018900941209     C                   MOVEL     FNLG30        KPJBU
019000941212     C                   GOTO      FINE
019100941212     C                   ENDIF
019200911119     C*
019300941212     C     EMFG04        TAG
019400941209     C                   WRITE     LG10D03
019500941209     C                   EXFMT     LG10D04
019600900510     C*                    ____________
019700941212     C*  CMD 21  STATO DELLA GIACENZA
019800941212     C     *INKV         IFEQ      *ON
019900941212     C                   EXFMT     LG10D05
020000941212     C                   GOTO      EMFG04
020100941212     C                   ENDIF
020200941209     C     *INKL         CABEQ     '1'           EMFG01
020300941212     C*  CMD 03  FINE LAVORO
020400950112     C*  O ENTER FINE LAVORO
020500941212     C     *INKC         IFEQ      '1'
020600950112     C     *INKC         OREQ      '0'
020700950112     C     *INKL         ANDEQ     '0'
020800950112     C     *INKV         ANDEQ     '0'
020900941212     C                   MOVEL     D30POA        SAVPOA            1
021000941212     C                   CLEAR                   FNLG30
021100941212     C                   MOVEL     '1'           D30F03
021200941212     C                   MOVEL     SAVPOA        D30POA
021300941212     C                   MOVEL     FNLG30        KPJBU
021400941212     C                   GOTO      FINE
021500941212     C                   ENDIF
021600900418     C*
021700900418     C     FINE          TAG
021800900418     C                   SETON                                        LR
021900900418     C/EJECT
022000900510     C/SPACE 3
022100900510     C*---------------------------------------------------------------*
022200900510     C*   G C P V I D                                                 *
022300900510     C*---------------------------------------------------------------*
022400900510     C     GCPVID        BEGSR
022500990830     C*
022600050307     C     GCPriap       comp      *blank                             4242
022700050307     c                   if        d30poa = 'A'
022800050307     C                   Z-ADD     GCPLNPa       §LNP
022900050307     C                   Z-ADD     GCPAASa       §AAS
023000050307     C                   Z-ADD     GCPNRSa       §NRS
023100050307     C                   Z-ADD     GCPNSPa       §NSP
023200050307     c                   else
023300050307     C                   Z-ADD     GCPLNP        §LNP
023400050307     C                   Z-ADD     GCPAAS        §AAS
023500050307     C                   Z-ADD     GCPNRS        §NRS
023600050307     C                   Z-ADD     GCPNSP        §NSP
023700050307     c                   endif
023800040126      * rekord GEN riferimenti ubicazione in magazzino giacenze
023900040209     c                   if        d30poa = 'A'
024000040126     C                   clear                   dar5gen
024100040126     C                   move      'GEN'         trcar5
024200040126     c     kar5          chain     fiar501l
024300040126     c                   if        %found
024400040126     C                   MOVEL     ar5uni        dar5gen
024500040126     c                   endif
024600040126     c                   movel     §ar5zng       vubima
024700040209     c                   endif
024800990830     C* INTERROGAZIONE GIACENZE SEDE
024900990830     C     *IN93         IFEQ      '1'
025000990830     C                   EXSR      TASVID
025100990830     C                   ELSE
025200941209     C     *IN80         IFEQ      '0'
025300941209     C     KSPE          CHAIN     FNBLP01L                           21
025400950117     C     *IN21         IFEQ      '0'
025500950117     C                   EXSR      BLPVID
025600950117     C                   END
025700941209     C                   ELSE
025800941209     C     KSPE          CHAIN     FNARB01L                           21
025900941209     C  N21              EXSR      ARBVID
026000941209     C                   ENDIF
026100990830     C                   ENDIF
026200050307     c                   if        d30poa = 'A'
026300050307     C                   MOVE      GCPLNPa       VLNP
026400050307     C                   MOVE      GCPAASa       VAAS
026500050307     C                   MOVE      GCPNRSa       VNRS
026600050307     C                   MOVE      GCPNSPa       VNSP
026700050307     C                   MOVE      GCPLNPa       V6CLNP
026800050307     C                   MOVE      GCPAASa       V6CAAS
026900050307     C                   MOVE      GCPNRSa       V6CNRS
027000050307     C                   MOVE      GCPNSPa       V6CNSP
027100050307     c                   else
027200900510     C                   MOVE      GCPLNP        VLNP
027300900510     C                   MOVE      GCPAAS        VAAS
027400900510     C                   MOVE      GCPNRS        VNRS
027500900510     C                   MOVE      GCPNSP        VNSP
027600960124     C                   MOVE      GCPLNP        V6CLNP
027700960124     C                   MOVE      GCPAAS        V6CAAS
027800960124     C                   MOVE      GCPNRS        V6CNRS
027900960124     C                   MOVE      GCPNSP        V6CNSP
028000050307     c                   endif
028100050307     C                   MOVE      GCPSCM        VSCM
028200060808     C                   clear                   V1cchim
028300060808     c                   if        GCPchiaut = 'N'
028400060808     C                   MOVE      'S'           V1cchim
028500060808     c                   endif
028600050307     C                   MOVE      VSCM          §QUATR            4
028700941209      * SE SONO IN PARTENZA VIS. IL MITTENTE DALLA GIACENZA
028800911121     C  N80§QUATR        IFEQ      *ALL'8'
028900900510     C     §QUATR        OREQ      *ALL'9'
029000900510     C* MITTENTE DA APPOGGIO
029100050307     C     KSPE          CHAIN     tigap01L                           21
029200900510     C     *IN21         IFEQ      '0'
029300900510     C                   MOVEL     GAPRSM        VRSM
029400900510     C                   MOVEL     GAPINM        VINM
029500900510     C                   MOVEL     GAPLOM        VLOM
029600900510     C                   MOVEL     GAPPRM        VPRM
029700900510     C                   MOVEL     GAPCAM        VCAM
029800941209     C                   MOVEL     GAPNZM        V1CNZM
029900900510     C                   END
030000900510     C                   ELSE
030100990830      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030200990830     C                   CLEAR                   TIBS69
030300990830     C                   CLEAR                   CNACO
030400990830     C                   CLEAR                   CNIND
030500990830     C                   CLEAR                   CNCLP
030600990830     C                   CLEAR                   FNCLS
030700990830     C                   MOVEL     ' '           I69TLA
030800990830     C                   MOVEL     KNSIF         I69SIF
030900990830     C                   Z-ADD     VSCM          I69KAC
031000990830     C                   Z-ADD     VSCM          I69KIN
031100990830     C                   CALL      'TIBS69R'
031200990830     C                   PARM                    TIBS69
031300990830     C                   PARM                    CNACO
031400990830     C                   PARM                    CNIND
031500990830     C                   PARM                    CNCLP
031600990830     C                   PARM                    FNCLS
031700990830     C     O69ERR        IFEQ      *BLANK
031800990830     C                   MOVEL     ACORAG        VRSM
031900990830     C                   MOVEL     INDVIA        VINM
032000990830     C                   MOVEL     INDCIT        VLOM
032100990830     C                   MOVEL     INDPRV        VPRM
032200990830     C                   MOVEL     INDCAE        VCAM
032300990830     C                   MOVEL     INDSTA        V1CNZM
032400990830     C                   ELSE
032500990830     C                   MOVE      *ALL'?'       VRSM
032600990830     C                   MOVE      *ALL'?'       VINM
032700990830     C                   MOVE      *ALL'?'       VLOM
032800990830     C                   MOVE      *ALL'?'       VPRM
032900990830     C                   END
033000900510     C                   END
033100941212     C* STATO AVANZAMENTO GIACENZA
033200941212     C                   MOVEL     GCPFAS        V5CFAS
033300941212     C                   MOVEL     '2Y'          TBLCOD
033400941212     C                   MOVEL     *BLANK        TBLKEY
033500941212     C                   MOVEL     GCPFAS        TBLKEY
033600941212     C     KTAB          CHAIN     TABEL                              21
033700941212     C   21              MOVEL     *ALL'?'       V5DFAS
033800941212     C  N21              MOVEL     TBLUNI        V5DFAS
033900900510     C* CODICE MOTIVO GIACENZA
034000900510     C                   MOVE      GCPCMC        VCMC
034100900626     C                   MOVEL     *BLANKS       DCMC
034200900626     C     GCPCMC        IFNE      *BLANKS
034300900510     C                   MOVEL     '2A'          TBLCOD
034400900510     C                   MOVEL     *BLANK        TBLKEY
034500900510     C                   MOVEL     VCMC          TBLKEY
034600900510     C     KTAB          CHAIN     TABEL                              21
034700900510     C   21              MOVEL     *ALL'?'       DCMC
034800900510     C  N21              MOVEL     TBLUNI        DCMC
034900900626     C                   END
035000911118     C* PRLEVO EVENTUALE ULTERIORE MOTIVO
035100911118     C                   Z-ADD     GCPFRG        §FRG
035200911118     C                   Z-ADD     10            §FAS
035300050307     C     KNOT          CHAIN     tignp01L                           45
035400911118     C  N45              MOVEL     GNPDMC        VDMC
035500911118     C   45              MOVEL     *BLANKS       VDMC
035600900510     C* LUOGO DEPOSITO MERCE
035700900510     C                   MOVE      GCPCCR        VCCR
035800911118     C                   MOVEL     *BLANKS       DCCR
035900900626     C     GCPCCR        IFNE      *ZEROS
036000900626     C                   MOVEL     '2B'          TBLCOD
036100900626     C                   MOVEL     *BLANK        TBLKEY
036200900626     C                   MOVEL     VCCR          TBLKEY
036300900626     C     KTAB          CHAIN     TABEL                              21
036400911118     C   21              MOVEL     *ALL'?'       DCCR
036500911118     C  N21              MOVEL     TBLUNI        DCCR
036600900626     C* SE NON LO TROVO NELLA TABELLA  LO CERCO IN ORGANIGARMMA
036700900510     C     *IN21         IFEQ      '1'
036800900626     C     VCCR          CHAIN     AZORG01L                           21
036900900626     C  N21ORGFVA        COMP      '*'                                    21
037000911118     C   21              MOVEL     *ALL'?'       DCCR
037100911118     C  N21              MOVEL     *BLANK        DCCR
037200911118     C  N21              MOVEL     ORGDES        DCCR
037300900510     C                   END
037400900626     C                   END
037500050408     C* SE SONO IN ARRIVO CERCO NEI LEGAMI LA BOLLA ORIGINALE
037600941212     C                   SETOFF                                       81
037700941212     C     *IN80         IFEQ      *ON
037800941212      *
037900050408     c                   if        gcpaas <> gcpaasa or gcplnp <> gcplnpa or
038000050408     c                             gcpnrs <> gcpnrsa or gcpnsp <> gcpnspa
038100941212     C                   SETON                                        81
038200050307     C                   MOVE      gcpaas        V5CAAO
038300050307     C                   MOVE      gcplnp        V5CLPO
038400050307     C                   MOVE      gcpnrs        V5CNRO
038500050307     C                   MOVE      gcpnsp        V5CNSO
038600050408     c                   endif
038700941212     C*
038800941212     C                   ENDIF
038900941209     C* NR. E DATA COMUNICAZ. = DATA DIST.PADRONC.
039000900510     C                   MOVE      GCPNRC        VNRC
039100941209     C                   MOVE      GCPDXD        G08INV
039200941209     C                   MOVEL     '3'           G08ERR
039300941209     C                   CALL      'XSRDA8'
039400941209     C                   PARM                    WLBDA8
039500941209     C                   Z-ADD     G08DAT        VDXD
039600941212     C                   Z-ADD     G08DAT        V5CDXD
039700960124     C                   MOVE      GCPAGC        V6CAGC
039800960124     C                   MOVE      GCPFGC        V6CFGC
039900960124     C                   MOVE      GCPNGC        V6CNGC
040000900510     C* DATA APERTURA GIACENZA
040100941209     C                   MOVEL     GCPAGC        G08INV
040200941209     C                   MOVE      GCPMGC        G08INV
040300941209     C                   MOVEL     '3'           G08ERR
040400941209     C                   CALL      'XSRDA8'
040500941209     C                   PARM                    WLBDA8
040600941209     C                   Z-ADD     G08DAT        VDTG
040700941212     C                   Z-ADD     G08DAT        V5CDTG
040800900510     C                   MOVE      GCPNGC        VNGC
040900900510     C* FIL. CHE HA APERTO LA GIACENZA
041000900510     C                   MOVE      GCPFGC        VFGC
041100900510     C     GCPFGC        CHAIN     AZORG01L                           21
041200900510     C  N21ORGFVA        COMP      '*'                                    21
041300900510     C   21              MOVEL     *ALL'?'       DFGC
041400900510     C  N21              MOVEL     ORGDES        DFGC
041500900510     C*
041600900510     C* DATA ULTIMA RIAPERTURA
041700050408     c                   if        gcpriap = 'R'
041800941209     C                   MOVE      GCPDUR        G08INV
041900941209     C                   MOVEL     '3'           G08ERR
042000941209     C                   CALL      'XSRDA8'
042100941209     C                   PARM                    WLBDA8
042200941209     C                   Z-ADD     G08DAT        VDIG
042300941212     C                   Z-ADD     G08DAT        V5CDIG
042400050408     c                   endif
042500941212     C* DATA STAMPA LETTERA APERTURA
042600941212     C                   MOVE      GCPDMM        G08INV
042700941212     C                   MOVEL     '3'           G08ERR
042800941212     C                   CALL      'XSRDA8'
042900941212     C                   PARM                    WLBDA8
043000941212     C                   Z-ADD     G08DAT        V5CDMM
043100941212     C* TIPO COMUN. APERTURA
043200960314     C                   MOVEL     GCPFST        V1CFST
043300960314     C                   MOVEL     GCPTCM        V5CTCM
043400941212     C                   MOVEL     '2F'          TBLCOD
043500941212     C                   MOVEL     *BLANK        TBLKEY
043600941212     C                   MOVEL     GCPTCM        TBLKEY
043700941212     C     KTAB          CHAIN     TABEL                              21
043800941212     C  N21              MOVEL     TBLUNI        V5DTCM
043900051011     C* appuntamento
044000051011     C                   MOVEL     GCPflr        dgcpflr
044100051011     c                   movel     D§GCPAPP      v1capp
044200051011     C* DATA IMMISS. DISPOSIZIONI
044300941209     C                   MOVE      GCPDDM        G08INV
044400941209     C                   MOVEL     '3'           G08ERR
044500941209     C                   CALL      'XSRDA8'
044600941209     C                   PARM                    WLBDA8
044700941209     C                   Z-ADD     G08DAT        VDDM
044800941212     C                   Z-ADD     G08DAT        V5CDDM
044900941209     C* DATA ESEGUIBILTà  DISPOSIZIONI (SE NON IMMESSE=DATA IMMISS.)
045000941209     C     GCPDED        IFGT      0
045100941209     C                   MOVE      GCPDED        G08INV
045200941209     C                   MOVEL     '3'           G08ERR
045300941209     C                   CALL      'XSRDA8'
045400941209     C                   PARM                    WLBDA8
045500941209     C                   Z-ADD     G08DAT        V1CDED
045600941212     C                   Z-ADD     G08DAT        V5CDED
045700941209     C                   ELSE
045800051011     c                   if        D§GCPAPP = 'A'
045900051011     c                   clear                   v1cded
046000051011     c                   else
046100941209     C                   Z-ADD     VDDM          V1CDED
046200941212     C                   Z-ADD     VDDM          V5CDED
046300941209     C                   ENDIF
046400051011     C                   ENDIF
046500941209     C* DATA TRASMISSIONE DISPOSIZIONI ALL'ARRIVO
046600941209     C                   MOVE      GCPDLD        G08INV
046700941209     C                   MOVEL     '3'           G08ERR
046800941209     C                   CALL      'XSRDA8'
046900941209     C                   PARM                    WLBDA8
047000941212     C                   Z-ADD     G08DAT        V5CDLD
047100911122     C*
047200911122     C* DATI DISPOSIZIONI
047300911122     C*
047400941209     C                   MOVEL     GCPDIS        VFRI
047500911219     C                   MOVEL     *BLANK        DFRI
047600941209     C     GCPDIS        IFNE      *BLANK
047700911219     C                   MOVEL     '2D'          TBLCOD
047800911219     C                   MOVEL     *BLANK        TBLKEY
047900911219     C                   MOVEL     VFRI          TBLKEY
048000911219     C     KTAB          CHAIN     TABEL                              21
048100911219     C   21              MOVEL     *ALL'?'       DFRI
048200911219     C  N21              MOVEL     TBLUNI        DS2D
048300911219     C  N21              MOVEL     §2DDES        DFRI
048400911219     C                   END
048500941209     C* DISPOSIZIONI:
048600911122     C                   MOVEL     GCPPSG        VPSG
048700911219     C                   MOVEL     *BLANK        DPSG
048800911219     C     GCPPSG        IFNE      *BLANK
048900911219     C                   MOVEL     '2E'          TBLCOD
049000911219     C                   MOVEL     *BLANK        TBLKEY
049100911219     C                   MOVEL     VPSG          TBLKEY
049200911219     C     KTAB          CHAIN     TABEL                              21
049300911219     C   21              MOVEL     *ALL'?'       DPSG
049400911219     C  N21              MOVEL     TBLUNI        DS2E
049500911219     C  N21              MOVEL     §2EDES        DPSG
049600911219     C                   END
049700911122     C                   MOVEL     GCPASG        VASG
049800911122     C                   MOVEL     GCPNPC        VNPC
049900911219     C                   MOVEL     *BLANK        DNPC
050000911219     C     GCPNPC        IFNE      *BLANK
050100911219     C     GCPNPC        COMP      'A'                                    21
050200911219     C   21              MOVEL     'ASSEGNAT'    DNPC
050300911219     C   21              MOVE      'O '          DNPC
050400911219     C  N21              MOVEL     'FRANCO  '    DNPC
050500911219     C  N21              MOVE      '  '          DNPC
050600911219     C                   END
050700911122     C                   MOVEL     GCPVCS        VVCS
050800941209     C                   Z-ADD     GCPCAS        VNCA
050900941209     C                   MOVE      GCPVCA        V1CNDV
051000911122     C                   MOVEL     GCPPPC        VPPC
051100911219     C                   MOVEL     *BLANK        DPPC
051200911219     C     GCPPPC        IFNE      *BLANK
051300911219     C                   MOVEL     '2E'          TBLCOD
051400911219     C                   MOVEL     *BLANK        TBLKEY
051500911219     C                   MOVEL     VPPC          TBLKEY
051600911219     C     KTAB          CHAIN     TABEL                              21
051700911219     C   21              MOVEL     *ALL'?'       DPPC
051800911219     C  N21              MOVEL     TBLUNI        DS2E
051900911219     C  N21              MOVEL     §2EDES        DPPC
052000911219     C                   END
052100911122     C                   MOVEL     GCPAPC        VAPC
052200051013     c* apertura automatica
052300051013     c                   if        gcpapeaut <> *blank
052400051013     c                   seton                                        61
052500051013     C                   MOVE      GCPapeaut     V5Capeaut
052600051013     c                   else
052700051013     C                   clear                   V5Capeaut
052800051013     c                   setoff                                       61
052900051013     c                   endif
053000051013     c* disposizioni automatiche
053100051013     c                   movel     gcpflr        dgcpflr
053200051013     c                   if        d§gcpdisa <> *blank
053300051013     c                   seton                                        62
053400051013     C                   MOVE      d§gcpdisa     V5Cdisaut
053500051013     c                   else
053600051013     C                   clear                   V5Cdisaut
053700051013     c                   setoff                                       62
053800051013     c                   endif
053900911122     C*
054000960208     C                   SETOFF                                       94
054100960208     C                   Z-ADD     20            §FAS
054200050307     C     KNOT          SETLL     tIGNP01L                               21
054300960208     C   21              SETON                                        94
054400911210     C* SE NON è CHIUSA SALTO DATI DI FATTURAZIONE E NON SONO
054500911210     C* STATI FATTI ADDEBITI IN DISPOSIZ.
054600141111     C*mm  GCPDCG        IFEQ      *ZERO
054700141111     C*mm  GCPNBW        ANDEQ     *ZEROS
054800141111     C*mm                GOTO      ENDGCP
054900141111     C*mm                END
055000900510     C*
055100900510     C* CAMPI X ESITAZIONE GIACENZA / FATTURAZIONE
055200900510     C* ---------------------------
055300900510     C* TIPO FATTURAZIONE
055400900510     C                   Z-ADD     GCPTFT        VTFT
055500900510     C* CODICE FINE  GIACENZA
055600900626     C                   MOVEL     *BLANKS       DCFG
055700900510     C     GCPCFG        IFNE      *BLANK
055800900510     C                   MOVEL     GCPCFG        VCFG
055900900510     C                   MOVEL     '2C'          TBLCOD
056000900510     C                   MOVEL     *BLANK        TBLKEY
056100900510     C                   MOVEL     VCFG          TBLKEY
056200900510     C     KTAB          CHAIN     TABEL                              21
056300900510     C   21              MOVEL     *ALL'?'       DCFG
056400900510     C  N21              MOVEL     TBLUNI        DCFG
056500900510     C                   END
056600941209     C* DATA FINE GIACENZA IN ARRIVO
056700941209     C                   MOVE      GCPDCG        G08INV
056800941209     C                   MOVEL     '3'           G08ERR
056900990830     C                   CALL      'XSRDA8'
057000941209     C                   PARM                    WLBDA8
057100941209     C                   Z-ADD     G08DAT        VDCG
057200941212     C                   Z-ADD     G08DAT        V5CDCG
057300900510     C* DATA SOSTA DAL
057400941209     C                   MOVE      GCPDSD        G08INV
057500941209     C                   MOVEL     '3'           G08ERR
057600941209     C                   CALL      'XSRDA8'
057700941209     C                   PARM                    WLBDA8
057800941209     C                   Z-ADD     G08DAT        VDSD
057900900510     C* DATA SOSTA AL
058000941209     C                   MOVE      GCPDSA        G08INV
058100941209     C                   MOVEL     '3'           G08ERR
058200941209     C                   CALL      'XSRDA8'
058300941209     C                   PARM                    WLBDA8
058400941209     C                   Z-ADD     G08DAT        VDSA
058500900510     C                   Z-ADD     GCPGGS        VGGS
058600900510     C                   Z-ADD     GCPSGF        VSGF
058700900510     C                   Z-ADD     GCPSGS        VTSGS
058800900510     C                   Z-ADD     GCPSGR        VSGR
058900900510     C                   Z-ADD     GCPSGD        VSGD
059000920424     C                   Z-ADD     GCPSGA        VSGA
059100990628     C                   MOVEL     GCPDIV        V4CDIV
059200990628     C* SE NON C'E' DIVISA IMPOSTA ITL
059300990628     C     GCPDIV        IFEQ      *BLANKS
059400990628     C                   MOVEL     'ITL'         GCPDIV
059500990628     C                   END
059600941209     C* DATA CREAZ. BOLLA / IN ARRIVO= DATA ESECUZIONE DISPOSIZ.
059700050408     C                   MOVE      GCPDeda       G08INV
059800941209     C                   MOVEL     '3'           G08ERR
059900941209     C                   CALL      'XSRDA8'
060000941209     C                   PARM                    WLBDA8
060100050408     C                   Z-ADD     G08DAT        V5CDeda
060200941209     C                   Z-ADD     G08DAT        VDBG
060300000201     C* REPERISCO LA DATA DELLA BOLLA CHAINANDO LA SPEDIZIONE
060400000201     C                   Z-ADD     0             VDBG
060500000201     C     GCPNBW        IFNE      0
060600000201     C     GCPNBW        ANDNE     9999999
060700000207     C* SE NON SONO IN SEDE
060800000207     C     *IN93         ANDEQ     '0'
060900000201     C     *IN80         IFEQ      '0'
061000000201     C     KBOLW         CHAIN     FNBLP01L                           31
061100000201     C     *IN31         IFEQ      '0'
061200000201     C                   MOVEL     BLPAAS        G08INV
061300000201     C                   MOVE      BLPMGS        G08INV
061400000201     C                   Z-ADD     0             G08DAT
061500000201     C                   MOVEL     '3'           G08ERR
061600000201     C                   CALL      'XSRDA8'
061700000201     C                   PARM                    WLBDA8
061800000201     C                   MOVE      G08DAT        VDBG
061900000201     C     KSPE          CHAIN     FNBLP01L                           21
062000000201     C                   END
062100000201     C                   ELSE
062200050307     C     KBOLW2        CHAIN     FNARB01L                           31
062300000201     C     *IN31         IFEQ      '0'
062400000201     C                   MOVEL     ARBAAS        G08INV
062500000201     C                   MOVE      ARBMGS        G08INV
062600000201     C                   Z-ADD     0             G08DAT
062700000201     C                   MOVEL     '3'           G08ERR
062800000201     C                   CALL      'XSRDA8'
062900000201     C                   PARM                    WLBDA8
063000000201     C                   MOVE      G08DAT        VDBG
063100000201     C     KSPE          CHAIN     FNARB01L                           21
063200000201     C                   END
063300000201     C                   END
063400000201     C                   END
063500941209      *
063600050609     c*                  if        d30poa = 'A'
063700050609     C*                  Z-ADD     GCPLPWa       VLNW
063800050609     C*                  Z-ADD     GCPASWa       VASW
063900050609     C*                  Z-ADD     GCPNSWa       VNSW
064000050609     C*                  Z-ADD     GCPNBWa       VNBW
064100050609     c*                  else
064200900510     C                   Z-ADD     GCPLPW        VLNW
064300900510     C                   Z-ADD     GCPASW        VASW
064400900510     C                   Z-ADD     GCPNSW        VNSW
064500900510     C                   Z-ADD     GCPNBW        VNBW
064600050609     c*                  endif
064700900510     C*
064800941212     C* DATA STAMPA COMUNIC. CHIUSURA
064900941212     C                   MOVE      GCPDLM        G08INV
065000941212     C                   MOVEL     '3'           G08ERR
065100941212     C                   CALL      'XSRDA8'
065200941212     C                   PARM                    WLBDA8
065300941212     C                   Z-ADD     G08DAT        V5CDLM
065400941212     C*
065500050531     C* DATA chiusura in partenza
065600050531     C                   MOVE      GCPDbg        G08INV
065700050531     C                   MOVEL     '3'           G08ERR
065800050531     C                   CALL      'XSRDA8'
065900050531     C                   PARM                    WLBDA8
066000050531     C                   Z-ADD     G08DAT        V5CDbg
066100050531     C*
066200050531     C* DATA STAMPA merce portata in magazzino
066300050531     C                   MOVE      GCPDLMa       G08INV
066400050531     C                   MOVEL     '3'           G08ERR
066500050531     C                   CALL      'XSRDA8'
066600050531     C                   PARM                    WLBDA8
066700050531     C                   Z-ADD     G08DAT        V5CDLMa
066800050531     C*
066900050531     C* DATA STAMPA merce prelevata da magazzino
067000050531     C                   MOVE      GCPDuma       G08INV
067100050531     C                   MOVEL     '3'           G08ERR
067200050531     C                   CALL      'XSRDA8'
067300050531     C                   PARM                    WLBDA8
067400050531     C                   Z-ADD     G08DAT        V5CDuma
067500050609     C                   MOVE      GCPaswa       V5Caswa
067600050609     C                   MOVE      GCPlpwa       V5Clpwa
067700050609     C                   MOVE      GCPnswa       V5Cnswa
067800050609     C                   MOVE      GCPnbwa       V5Cnbwa
067900050531     C*
068000941209     C                   MOVEL     GCPTFG        V4CTFG
068100941212     C                   MOVEL     GCPTFG        V5CTFG
068200941209     C                   MOVEL     '2H'          TBLCOD
068300941209     C                   MOVEL     *BLANK        TBLKEY
068400941209     C                   MOVEL     GCPTFG        TBLKEY
068500941209     C     KTAB          CHAIN     TABEL                              21
068600941209     C  N21              MOVEL     TBLUNI        V4DTFG
068700941212     C  N21              MOVEL     TBLUNI        V5DTFG
068800941209     C*
068900941209     C* CLIENTE A CUI SI E' FATTURATO LE SPESE GIAC. (DALLA BOLLA)
069000900510     C*
069100901220     C                   MOVEL     *BLANK        VRSDB
069200901220     C                   MOVEL     *BLANK        VINDB
069300901220     C                   MOVEL     *BLANK        VLODB
069400901220     C                   MOVEL     *BLANK        VPRDB
069500941212     C                   MOVE      *BLANK        VKSC
069600941212     C                   MOVE      *BLANK        VCADB
069700901220     C                   MOVE      *ZEROS        VFIV
069800901220     C                   MOVE      *ZEROS        VNFT
069900901220     C                   MOVE      *ZEROS        VDFT
070000990830      * BOLLA ADDEBITO SPESE ...
070100911211     C     GCPNBW        IFNE      *ZEROS
070200941209     C     *IN80         ANDEQ     '0'
070300990830     C* SE SONO IN SEDE
070400990830     C     *IN93         IFEQ      '1'
070500990830     C* REPERISCO DA CHAIN IN TNTAS DATI BOLLA DI RECUPERO
070600990830     C                   EXSR      TASREC
070700990830     C* ... O IN P.O. PARTENZA
070800990830     C                   ELSE
070900900510     C                   Z-ADD     GCPLPW        §LNP
071000900510     C                   Z-ADD     GCPASW        §AAS
071100900510     C                   Z-ADD     GCPNSW        §NRS
071200900510     C                   Z-ADD     GCPNBW        §NSP
071300941209     C     KSPE          CHAIN     FNBLP01L                           21
071400941209     C     *IN21         IFEQ      '0'
071500941209     C                   MOVE      BLPKSC        VKSC
071600941209     C                   MOVEL     BLPRSM        VRSDB
071700941209     C                   MOVEL     BLPINM        VINDB
071800941209     C                   MOVEL     BLPLOM        VLODB
071900941209     C                   MOVEL     BLPPRM        VPRDB
072000941209     C                   MOVEL     BLPCAM        VCADB
072100941209     C                   MOVEL     BLPNZM        V3CNZM
072200990929     C* Imposto tipo record '1' X aggancio su file Tassazione
072300990929     C* per reperire dati fattura della bolla di recupero
072400990929     C                   MOVEL     '1'           KTRC
072500990929     C     KAR6          CHAIN     FIAR601L                           21
072600941209     C     *IN21         IFEQ      '0'
072700990929     C                   Z-ADD     AR6FIV        VFIV
072800990929     C                   Z-ADD     AR6NFT        VNFT
072900990929     C                   Z-ADD     AR6DFT        G08INV
073000941209     C                   MOVEL     '3'           G08ERR
073100941209     C                   CALL      'XSRDA8'
073200941209     C                   PARM                    WLBDA8
073300941209     C                   Z-ADD     G08DAT        VDFT
073400941209     C                   ELSE
073500950117     C                   CLEAR                   VFIV
073600950117     C                   CLEAR                   VNFT
073700950117     C                   CLEAR                   VDFT
073800941209     C                   ENDIF
073900950117     C                   ENDIF
074000941209     C                   ENDIF
074100990830     C                   ENDIF
074200960208     C*
074300900510     C     ENDGCP        ENDSR
074400990830     C*---------------------------------------------------------------*
074500990830     C*   T A S R E C                                                 *
074600990830     C*---------------------------------------------------------------*
074700990830     C     TASREC        BEGSR
074800990830     C*
074900990830     C* AZZERO I DATI ANAGRAFICI DELLA BOLLA DI RECUPERO PRIMA DEL CHAIN
075000990830     C* IN TITAS/TAA
075100990830     C                   CLEAR                   VKSC
075200990830     C                   CLEAR                   VRSDB
075300990830     C                   CLEAR                   VINDB
075400990830     C                   CLEAR                   VLODB
075500990830     C                   CLEAR                   VPRDB
075600990830     C                   CLEAR                   VCADB
075700990830     C                   CLEAR                   V3CNZM
075800990830     C                   CLEAR                   VFIV
075900990830     C                   CLEAR                   VNFT
076000990830     C                   CLEAR                   VDFT
076100990830     C* AGGANCIO TITAS
076200990830     C                   Z-ADD     GCPLPW        KLNP
076300990830     C                   Z-ADD     GCPASW        KAAS
076400990830     C                   Z-ADD     GCPNSW        KNRS
076500990830     C                   Z-ADD     GCPNBW        KNSP
076600990830     C     KTAS          CHAIN     TITAS30C                           21
076700990830     C     *IN21         IFEQ      '0'
076800990830     C                   MOVE      TASCCM        VKSC
076900990830     C                   Z-ADD     TASFIV        VFIV
077000990830     C                   Z-ADD     TASNFT        VNFT
077100990830     C                   Z-ADD     TASDFT        G08INV
077200990830     C                   MOVEL     '3'           G08ERR
077300990830     C                   CALL      'XSRDA8'
077400990830     C                   PARM                    WLBDA8
077500990830     C                   Z-ADD     G08DAT        VDFT
077600990830     C* CONTROLLO SE CLIENTE CODIFICATO
077700990830     C                   MOVE      TASCCM        §QUATR
077800990830      * ... SE NO REPERISCO DECODIFICA IN TITAS
077900990830     C     §QUATR        IFEQ      *ALL'8'
078000990830     C                   MOVE      'M'           KTRC
078100990830     C     KTAA          CHAIN     TITAA30C                           21
078200990830     C     *IN21         IFEQ      '0'
078300990830     C                   MOVEL     TAARSC        VRSDB
078400990830     C                   MOVEL     TAAIND        VINDB
078500990830     C                   MOVEL     TAALOC        VLODB
078600990830     C                   MOVEL     TAAPRV        VPRDB
078700990830     C                   MOVEL     TAACAP        VCADB
078800990830     C                   MOVEL     TAANAZ        V3CNZM
078900990830     C                   ENDIF
079000990830      * ... ALTRIMENTI IN CNACO
079100990830     C                   ELSE
079200990830     C                   CLEAR                   TIBS69
079300990830     C                   CLEAR                   CNACO
079400990830     C                   CLEAR                   CNIND
079500990830     C                   CLEAR                   CNCLP
079600990830     C                   CLEAR                   FNCLS
079700990830     C                   MOVEL     ' '           I69TLA
079800990830     C                   MOVEL     KNSIF         I69SIF
079900990830     C                   Z-ADD     TASCCM        I69KAC
080000990830     C                   Z-ADD     TASCCM        I69KIN
080100990830     C                   CALL      'TIBS69R'
080200990830     C                   PARM                    TIBS69
080300990830     C                   PARM                    CNACO
080400990830     C                   PARM                    CNIND
080500990830     C                   PARM                    CNCLP
080600990830     C                   PARM                    FNCLS
080700990830     C     O69ERR        IFEQ      *BLANK
080800990830     C                   MOVEL     ACORAG        VRSDB
080900990830     C                   MOVEL     INDVIA        VINDB
081000990830     C                   MOVEL     INDCIT        VLODB
081100990830     C                   MOVEL     INDPRV        VPRDB
081200990830     C                   MOVEL     INDCAE        VCADB
081300990830     C                   MOVEL     INDSTA        V3CNZM
081400990830     C                   ENDIF
081500990830     C*
081600990830     C                   ENDIF
081700990831     C                   ENDIF
081800990830     C*
081900990830     C                   ENDSR
082000990830     C*---------------------------------------------------------------*
082100990830     C*   T A S V I D                                                 *
082200990830     C*---------------------------------------------------------------*
082300990830     C     TASVID        BEGSR
082400990830     C*
082500990830     C                   SETOFF                                       011012
082600990831     C                   Z-ADD     GCPLNP        KLNP
082700990831     C                   Z-ADD     GCPAAS        KAAS
082800990831     C                   Z-ADD     GCPNRS        KNRS
082900990831     C                   Z-ADD     GCPNSP        KNSP
083000990830     C     KTAS          CHAIN     TITAS30C                           21
083100990830     C* IMPOSTO DATI A VIDEO
083200990830     C                   MOVE      TASMGS        W040              6 0
083300990830     C                   MOVEL     W040          VDSP
083400990830     C                   MOVE      TASMGS        §GG               2 0
083500990830     C                   MOVEL     §GG           VDSP
083600990830     C                   MOVE      TASAAS        VDSP
083700990830     C* TIPO BOLLA
083800990830     C                   MOVEL     TASCBO        VTBL
083900990830     C                   MOVEL     '3A'          TBLCOD
084000990830     C                   MOVEL     *BLANK        TBLKEY
084100990830     C                   MOVEL     VTBL          TBLKEY
084200990830     C     KTAB          CHAIN     TABEL                              21
084300990830     C   21              MOVEL     *ALL'?'       DTBL
084400990830     C  N21              MOVEL     TBLUNI        DS3A
084500990830     C  N21              MOVEL     §3ADES        DTBL
084600990830     C* TIPO SPEDIZIONE
084700990830     C                   MOVEL     *BLANKS       DTSP
084800990830     C                   MOVEL     '5E'          TBLCOD
084900990830     C                   MOVEL     *BLANK        TBLKEY
085000990830     C                   MOVEL     TASTSP        TBLKEY
085100990830     C     KTAB          CHAIN     TABEL                              21
085200990830     C  N21              MOVEL     TBLUNI        DS5E
085300990830     C  N21              MOVEL     §5EDES        DTSP
085400990830     C* LINEA ARRIVO
085500990830     C                   MOVEL     TASLNA        VLNA
085600990830     C     VLNA          CHAIN     AZORG01L                           21
085700990830     C  N21ORGFVA        COMP      '*'                                    21
085800990830     C   21              MOVEL     *ALL'?'       DLNA
085900990830     C  N21              MOVEL     ORGDES        DLNA
086000990830     C*
086100160121     C****               MOVEL     TASNAS        VNAS
086200990830     C                   Z-ADD     TASNCL        VNCL
086300990830     C                   Z-ADD     TASPKF        VPKG
086400990830     C                   Z-ADD     TASVLF        VVOL
086500990830     C                   MOVEL     TASRSD        VRSD
086600990830     C                   MOVEL     *BLANKS       V1CRD2
086700990830     C                   MOVEL     TASIND        VIND
086800990830     C                   MOVEL     TASLOD        VLOD
086900990830     C                   MOVEL     TASPRD        VPRD
087000990830     C                   MOVEL     TASCAD        VCAD
087100990830     C                   MOVEL     TASNZD        V1CNZD
087200990830     C* MITTENTE
087300990830     C                   MOVE      TASCCM        VSCM
087400990830     C* CONTROLLO SE CLIENTE CODIFICATO
087500990830     C                   MOVE      TASCCM        §QUATR
087600990830      * ... SE NO REPERISCO DECODIFICA IN TITAS
087700990830     C     §QUATR        IFEQ      *ALL'8'
087800990830     C                   MOVE      'M'           KTRC
087900990830     C     KTAA          CHAIN     TITAA30C                           21
088000990830     C     *IN21         IFEQ      '0'
088100990830     C                   MOVEL     TAARSC        VRSM
088200990830     C                   MOVEL     TAAIND        VINM
088300990830     C                   MOVEL     TAALOC        VLOM
088400990830     C                   MOVEL     TAAPRV        VPRM
088500990830     C                   MOVEL     TAACAP        VCAM
088600990830     C                   MOVEL     TAANAZ        V1CNZM
088700990830     C                   ENDIF
088800990830      * ... ALTRIMENTI IN CNACO
088900990830     C                   ELSE
089000990830     C                   CLEAR                   TIBS69
089100990830     C                   CLEAR                   CNACO
089200990830     C                   CLEAR                   CNIND
089300990830     C                   CLEAR                   CNCLP
089400990830     C                   CLEAR                   FNCLS
089500990830     C                   MOVEL     ' '           I69TLA
089600990830     C                   MOVEL     KNSIF         I69SIF
089700990830     C                   Z-ADD     TASCCM        I69KAC
089800990830     C                   Z-ADD     TASCCM        I69KIN
089900990830     C                   CALL      'TIBS69R'
090000990830     C                   PARM                    TIBS69
090100990830     C                   PARM                    CNACO
090200990830     C                   PARM                    CNIND
090300990830     C                   PARM                    CNCLP
090400990830     C                   PARM                    FNCLS
090500990830     C     O69ERR        IFEQ      *BLANK
090600990830     C                   MOVEL     ACORAG        VRSM
090700990830     C                   MOVEL     INDVIA        VINM
090800990830     C                   MOVEL     INDCIT        VLOM
090900990830     C                   MOVEL     INDPRV        VPRM
091000990830     C                   MOVEL     INDCAE        VCAM
091100990830     C                   MOVEL     INDSTA        V1CNZM
091200990830     C                   ENDIF
091300990830     C*
091400990830     C                   ENDIF
091500990830     C                   Z-ADD     TASRMN        VRIF
091600990830     C* REPERISCO RIFERIMENTO MITTENTE ALFABETICO
091601160121     c*           e natura merce
091700990830     C                   MOVEL     'A'           KTRC
091800990830     C     KTA4          CHAIN     TITA430C                           21
091801160121     c  n12              movel     ta4not        dta4a
091802160121     c   12              clear                   dta4a
091900160121     C**N12              MOVEL     TA4NOT        V1CRMA
091901160121     C                   MOVEL     §ta4arma      V1CRMA
091902160121     C                   MOVEL     §ta4anas      Vnas
092000990830      * GESTIONE PARTICOLARITA' GIACENZE
092100990830     C                   MOVEL     TASGGA        V1CGGA
092200990830      * CONTRASSEGNO
092300990830     C     KCSB          CHAIN     TNCSB03L                           21
092400990830     C  N21              Z-ADD     CSBCAS        VASS
092500990830     C  N21              MOVEL     CSBVCA        V1CDIV
092600990830     C   21              CLEAR                   VASS
092700990830     C   21              CLEAR                   V1CDIV
092800990830     C*
092900990830     C                   ENDSR
093000900510     C*---------------------------------------------------------------*
093100900510     C*   B L P V I D                                                 *
093200900510     C*---------------------------------------------------------------*
093300900510     C     BLPVID        BEGSR
093400941209     C                   MOVE      BLPMGS        W040              6 0
093500941209     C                   MOVEL     W040          VDSP
093600900510     C                   MOVE      BLPMGS        §GG               2 0
093700900510     C                   MOVEL     §GG           VDSP
093800900510     C                   MOVE      BLPAAS        VDSP
093900900510     C* TIPO BOLLA
094000941209     C                   MOVEL     BLPCBO        VTBL
094100900510     C                   MOVEL     '3A'          TBLCOD
094200900510     C                   MOVEL     *BLANK        TBLKEY
094300941209     C                   MOVEL     VTBL          TBLKEY
094400900510     C     KTAB          CHAIN     TABEL                              21
094500900510     C   21              MOVEL     *ALL'?'       DTBL
094600900510     C  N21              MOVEL     TBLUNI        DS3A
094700900510     C  N21              MOVEL     §3ADES        DTBL
094800990929     C  N21              MOVEL     §3ATB1        WPORTO            1
094900941213     C* TIPO SPEDIZIONE
095000941213     C                   MOVEL     *BLANKS       DTSP
095100941213     C                   MOVEL     '5E'          TBLCOD
095200941213     C                   MOVEL     *BLANK        TBLKEY
095300941213     C                   MOVEL     BLPTSP        TBLKEY
095400941213     C     KTAB          CHAIN     TABEL                              21
095500941213     C  N21              MOVEL     TBLUNI        DS5E
095600941213     C  N21              MOVEL     §5EDES        DTSP
095700900510     C* LINEA ARRIVO
095800900510     C                   MOVEL     BLPLNA        VLNA
095900900510     C     VLNA          CHAIN     AZORG01L                           21
096000900510     C  N21ORGFVA        COMP      '*'                                    21
096100900510     C   21              MOVEL     *ALL'?'       DLNA
096200900510     C  N21              MOVEL     ORGDES        DLNA
096300900510     C*
096400900510     C                   MOVEL     BLPNAS        VNAS
096500900510     C                   Z-ADD     BLPNCL        VNCL
096600941209     C                   Z-ADD     BLPPKF        VPKG
096700941209     C                   Z-ADD     BLPVLF        VVOL
096800900510     C* DESTINATARIO
096900960124     C                   MOVEL     'D'           §TRC
097000060213     C     KAR4          CHAIN     Fiar401L                           21
097100960124     C* Controllo se esiste destinatario finale
097200960124     C                   SETOFF                                       13
097300960124     C                   CLEAR                   FNLSN0
097400960124     C                   Z-ADD     BLPAAS        DN0AAS
097500960124     C                   Z-ADD     BLPLNP        DN0LNP
097600960124     C                   Z-ADD     BLPNRS        DN0NRS
097700960124     C                   Z-ADD     BLPNSP        DN0NSP
097800960124     C                   MOVEL     *BLANKS       DN0TRC
097900960125     C                   Z-ADD     GCPDUR        DN0DAG
098000960124     C                   MOVEL     KPJBU         WSVKPJ          256
098100960124     C                   MOVEL     FNLSN0        KPJBU
098200960124     C                   CALL      'FNLSN0R'
098300960124     C                   PARM                    KPJBA
098400960124     C                   MOVEL     KPJBU         FNLSN0
098500960124     C                   MOVEL     WSVKPJ        KPJBU
098600960124     C* Destinatario
098700960124     C     DN0TRV        IFEQ      'S'
098800960124     C                   SETON                                        13
098900960124     C                   MOVEL     DN0RSD        VRSD
099000960124     C                   MOVEL     DN0RS2        V1CRD2
099100960124     C                   MOVEL     DN0IND        VIND
099200960124     C                   MOVEL     DN0LOD        VLOD
099300960124     C                   MOVEL     DN0PRD        VPRD
099400960124     C                   MOVEL     DN0CAD        VCAD
099500960124     C                   MOVEL     DN0NZD        V1CNZD
099600960124     C                   MOVEL     BLPRSD        V6CRSD
099700060213     C                   MOVEL     ar4NOT        V6CDE2
099800960124     C                   MOVEL     BLPIND        V6CIND
099900960124     C                   MOVEL     BLPLOD        V6CLOD
100000960124     C                   MOVEL     BLPPRD        V6CPRD
100100960124     C                   MOVEL     BLPCAD        V6CCAD
100200960124     C                   MOVEL     BLPNZD        V6CNZD
100300960124     C                   ELSE
100400900510     C                   MOVEL     BLPRSD        VRSD
100500060213     C  N21              MOVEL     ar4NOT        V1CRD2
100600941209     C   21              MOVEL     *BLANKS       V1CRD2
100700941209      *
100800900510     C                   MOVEL     BLPIND        VIND
100900900510     C                   MOVEL     BLPLOD        VLOD
101000900510     C                   MOVEL     BLPPRD        VPRD
101100900510     C                   MOVEL     BLPCAD        VCAD
101200941209     C                   MOVEL     BLPNZD        V1CNZD
101300960124     C                   END
101400900510     C* MITTENTE
101500900510     C     BLPCCM        IFNE      *ZEROS
101600900510     C                   MOVE      BLPCCM        VSCM
101700900510     C                   ELSE
101800900510     C                   MOVE      BLPKSC        VSCM
101900900510     C                   END
102000900510     C                   MOVEL     BLPRSM        VRSM
102100900510     C                   MOVEL     BLPINM        VINM
102200900510     C                   MOVEL     BLPLOM        VLOM
102300900510     C                   MOVEL     BLPPRM        VPRM
102400900510     C                   MOVEL     BLPCAM        VCAM
102500941209     C                   MOVEL     BLPNZM        V1CNZM
102600921014     C                   Z-ADD     BLPRMN        VRIF
102700941209     C                   MOVEL     BLPRMA        V1CRMA
102800941209      * GESTIONE PARTICOLARITA' GIACENZE
102900941209     C                   MOVEL     BLPGGA        V1CGGA
103000941209      * CONTRASSEGNO
103100051115     C     KSPE          CHAIN     Fiar901L                           21
103200051115     C  N21              Z-ADD     ar9CAS        VASS
103300051115     C  N21              MOVEL     ar9VCA        V1CDIV
103400941209     C   21              CLEAR                   VASS
103500941209     C   21              CLEAR                   V1CDIV
103600941209      * TASSAZIONE
103700990929     C     WPORTO        IFEQ      'F'
103800990929     C                   MOVEL     '1'           KTRC
103900990929     C     KAR6          CHAIN     FIAR601L                           21
104000990929     C                   ELSE
104100990929     C                   SETON                                        21
104200990929     C                   ENDIF
104300941209     C     *IN21         IFEQ      '0'
104400990929     C                   Z-ADD     AR6IFT        VIFT
104500991119     C                   MOVEL     AR6DIV        V1CDFA
104600941209     C                   ELSE
104700941209     C                   CLEAR                   VIFT
104800991119     C                   CLEAR                   V1CDFA
104900941209     C                   ENDIF
105000900510     C                   ENDSR
105100911118     C*---------------------------------------------------------------*
105200911118     C     ARBVID        BEGSR
105300941213     C                   MOVE      ARBMGS        W040              6 0
105400941209     C                   MOVEL     W040          VDSP
105500911118     C                   MOVE      ARBMGS        §GG               2 0
105600911118     C                   MOVEL     §GG           VDSP
105700911118     C                   MOVE      ARBAAS        VDSP
105800911118     C* TIPO SPEDIZIONE
105900911118     C                   MOVEL     *BLANKS       DTSP
106000941212     C                   MOVEL     '5E'          TBLCOD
106100941212     C                   MOVEL     *BLANK        TBLKEY
106200941212     C                   MOVEL     ARBTSP        TBLKEY
106300941212     C     KTAB          CHAIN     TABEL                              21
106400941212     C  N21              MOVEL     TBLUNI        DS5E
106500941212     C  N21              MOVEL     §5EDES        DTSP
106600911118     C* TIPO BOLLA
106700941209     C                   MOVEL     ARBCBO        VTBL
106800911118     C                   MOVEL     '3A'          TBLCOD
106900911118     C                   MOVEL     *BLANK        TBLKEY
107000911118     C                   MOVEL     VTBL          TBLKEY
107100911118     C     KTAB          CHAIN     TABEL                              21
107200911118     C   21              MOVEL     *ALL'?'       DTBL
107300911118     C  N21              MOVEL     TBLUNI        DS3A
107400911118     C  N21              MOVEL     §3ADES        DTBL
107500990929     C  N21              MOVEL     §3ATB1        WPORTO            1
107600911118     C* LINEA ARRIVO
107700911118     C                   MOVEL     ARBLNA        VLNA
107800911118     C     VLNA          CHAIN     AZORG01L                           21
107900911118     C  N21ORGFVA        COMP      '*'                                    21
108000911118     C   21              MOVEL     *ALL'?'       DLNA
108100911118     C  N21              MOVEL     ORGDES        DLNA
108200911118     C*
108300911118     C                   MOVEL     ARBNAS        VNAS
108400911118     C                   Z-ADD     ARBNCL        VNCL
108500941209     C                   Z-ADD     ARBVLF        VVOL
108600941209     C                   Z-ADD     ARBPKF        VPKG
108700911118     C* DESTINATARIO
108800911118     C                   MOVEL     ARBRSD        VRSD
108900941209     C                   MOVEL     'D'           §TRC
109000060213     C     KAR4          CHAIN     FiAR401L                           21
109100941209     C  N21              MOVEL     AR4NOT        V1CRD2
109200941209     C   21              MOVEL     *BLANKS       V1CRD2
109300941209      *
109400911118     C                   MOVEL     ARBIND        VIND
109500911118     C                   MOVEL     ARBLOD        VLOD
109600911118     C                   MOVEL     ARBPRD        VPRD
109700911118     C                   MOVEL     ARBCAD        VCAD
109800941209     C                   MOVEL     ARBNZD        V1CNZD
109900911118     C* MITTENTE
110000911118     C     ARBCCM        IFNE      *ZEROS
110100911118     C                   MOVEL     ARBCCM        VSCM
110200911118     C                   ELSE
110300911118     C                   MOVEL     ARBKSC        VSCM
110400911118     C                   END
110500930324     C                   MOVEL     ARBRMN        VRIF
110600941209     C                   MOVEL     ARBRMA        V1CRMA
110700911118     C                   MOVEL     ARBRSM        VRSM
110800911118     C                   MOVEL     ARBINM        VINM
110900911118     C                   MOVEL     ARBLOM        VLOM
111000911118     C                   MOVEL     ARBPRM        VPRM
111100911118     C                   MOVEL     ARBCAM        VCAM
111200941209     C                   MOVEL     ARBNZM        V1CNZM
111300941209      * PARTICOLATITA GIACENZA
111400941209     C                   MOVEL     ARBGGA        V1CGGA
111500911118     C* SE SIAMO IN RIAPERTURA METTO COD. MANCATA CONSEGNA
111600911118     C* CHE C'E SULLA BOLLA
111700911118     C   42              MOVEL     ARBCMC        VCMC
111800911118     C* LUOGO DEPOSITO MERCE  PROPONGO FILIALE ARRIVO DELLA BOLLA
111900911118     C                   MOVE      ARBLNA        VCCR
112000911118     C* DATA COMUNICAZIONE INTERNA = DATA DIST. PAD. SE IN ARRIVO
112100941209     C                   Z-ADD     ARBDDC        G08INV
112200941209     C                   MOVE      *ZEROS        G08DAT
112300941209     C                   MOVEL     *BLANK        G08ERR
112400941209     C                   CALL      'XSRDA8'
112500941209     C                   PARM                    WLBDA8
112600941209     C                   Z-ADD     G08DAT        VDXD
112700941209      * CONTRASSEGNO
112800051115     C     KSPE          CHAIN     FIAR901L                           21
112900941209     C  N21              Z-ADD     AR9CAS        VASS
113000941209     C  N21              MOVEL     AR9VCA        V1CDIV
113100941209     C   21              CLEAR                   VASS
113200941209     C   21              CLEAR                   V1CDIV
113300941209      * TASSAZIONE
113400990929     C     WPORTO        IFEQ      'A'
113500990929     C                   MOVEL     '1'           KTRC
113600990929     C     KAR6          CHAIN     FIAR601L                           21
113700990929     C                   ELSE
113800990929     C                   SETON                                        21
113900990929     C                   ENDIF
114000941209     C     *IN21         IFEQ      '0'
114100941209     C                   Z-ADD     AR6IFT        VIFT
114200991119     C                   MOVEL     AR6DIV        V1CDFA
114300941209     C                   ELSE
114400941209     C                   CLEAR                   VIFT
114500991119     C                   CLEAR                   V1CDFA
114600941209     C                   ENDIF
114700911118     C                   ENDSR
114800960124     C*-----------------------------------------------------*
114900960124     C*  Gestione della sesta videata
115000960124     C*-----------------------------------------------------*
115100960124     C     GESD06        BEGSR
115200960124     C*
115300960124     C                   EXFMT     LG10D06
115400960124     C*                    ------------
115500960124     C*  Ritorno
115600960124     C     *INKL         IFEQ      '1'
115700960124     C                   MOVEL     'S'           WRITOR
115800960124     C                   END
115900960124     C*
116000960124     C                   ENDSR
116100970108     C*---------------------------------------------------------------*
116200970108     C     CTRFAX        BEGSR
116300970108     C*
116400970108     C                   Z-ADD     35            §FAS
116500970108     C                   SETOFF                                       12
116600050307     C     KNOT          SETLL     tIGNP01L                               12
116700970108     C*
116800970108     C                   ENDSR
116900970624     C*---------------------------------------------------------------*
117000970624     C     CTRAPE        BEGSR
117100970624     C*
117200970624     C                   Z-ADD     12            §FAS
117300970624     C                   SETOFF                                       11
117400050307     C     KNOT          SETLL     tIGNP01L                               11
117500970624     C*
117600970624     C                   ENDSR
117700911118     C*---------------------------------------------------------------*
117800911118     C     GESNOT        BEGSR
117900911118     C                   EXSR      PULIZ
118000911119     C                   Z-ADD     20            §FAS
118100050307     C     KNOT          SETLL     tIGNP01L
118200911118     C     *IN21         DOUEQ     '1'
118300050307     C     KNOT          READE     tIGNP01L                               21
118400911118     C     *IN21         IFEQ      '0'
118500911125     C                   MOVEL     GNPDMC        VDMC1
118600911118     C                   MOVEL     GNPTPN        VTPN
118700911118     C                   MOVEL     GNPNPR        VNPR
118800911118     C                   ADD       1             NRR               3 0
118900941209     C                   WRITE     LG10S02
119000951026     C                   END
119100951026     C                   END
119200960208     C     NRR           DIV       10            WRESTO            2 0
119300960208     C                   MVR                     §NR               2 0
119400960208     C     10            SUB       §NR           §NR
119500941212     C                   DO        §NR
119600941212     C                   MOVEL     *BLANK        VDMC1
119700941212     C                   MOVEL     *BLANK        VTPN
119800941212     C                   ADD       1             VNPR
119900941212     C                   ADD       1             NRR
120000941212     C                   WRITE     LG10S02
120100941212     C                   END
120200960208     C                   Z-ADD     1             NR2KEY
120300911118     C*
120400911118     C     STRNOT        TAG
120500941209     C                   WRITE     LG10Z02
120600941209     C                   EXFMT     LG10C02
120700970108     C* RICHIAMO VISUALIZZAZIONE FAX
120800970108     C     *INKM         IFEQ      '1'
120900020927      * richiamo pgm reperimento coda fax
121000020927     c                   move      'L'           finep
121100020927     c                   z-add     simpou        codpo
121200020927     c                   move      '2'           tpcoda
121300020927     c                   clear                   nomcoda
121400020927     c                   clear                   libcoda
121500020927     c                   clear                   esito
121600020927     c                   call      'TRULOUTQ'
121700020927     c                   parm                    finep
121800020927     c                   parm                    codpo
121900020927     c                   parm                    tpcoda
122000020927     c                   parm                    nomcoda
122100020927     c                   parm                    libcoda
122200020927     c                   parm                    esito
122300020927     c                   if        esito <> '0' or nomcoda = *blank
122400020927     c                   eval      nomcoda = 'QDKT'
122500020927     c                   endif
122600020927     c
122700970108     C                   CLEAR                   FNLG21
122800970108     C                   Z-ADD     GCPAAS        D21AAS
122900970108     C                   Z-ADD     GCPLNP        D21LNP
123000970108     C                   Z-ADD     GCPNRS        D21NRS
123100970108     C                   Z-ADD     GCPNSP        D21NSP
123200970108     C                   Z-ADD     GCPAGC        D21AGC
123300970108     C                   Z-ADD     GCPFGC        D21FGC
123400970108     C                   Z-ADD     GCPNGC        D21NGC
123500970108     C                   MOVEL     'G'           D21TPR
123600970108     C                   MOVEL     'V'           D21VIS
123700970108     C                   MOVEL     GCPDIS        D21DIS
123800970418     C                   MOVEL     GCPFRG        D21FRG
123900970108     C*  Richiamo videata x fax
124000070720     C*  SE SEDE OVRDBF PER FIAR401L
124100070720     c                   if        *in93 = '1'
124200070720     c                   call      'FNLG21C2'
124300070720     C                   PARM      '1'           FLAGC2            1
124400070720     C                   ENDIF
124500020927     C                   MOVE      nomcoda       D21PRF
124600970108     C                   MOVEL     FNLG21        KPJBU
124700970108     C                   CALL      'FNLG21C'
124800970108     C                   PARM                    KPJBA
124900970108     C                   END
125000070720     C*  SE SEDE OVRDBF PER FIAR401L
125100070720     c                   if        *in93 = '1'
125200070720     c                   call      'FNLG21C2'
125300070720     C                   PARM      '2'           FLAGC2
125400070720     C                   ENDIF
125500970108     C*
125600941209     C   KL              GOTO      ENDNOT
125700941209     C                   GOTO      STRNOT
125800911118     C     ENDNOT        ENDSR
125900970624     C*---------------------------------------------------------------*
126000970624     C*   CALL03: Richiamo pgm per visualizzazione note apertura      *
126100970624     C*---------------------------------------------------------------*
126200970624     C     CALL03        BEGSR
126300970624     C*
126400970624     C* Imposto parametri
126500970624     C                   CLEAR                   FNLG03
126600970624     C                   Z-ADD     VLNP          D03LNP
126700970624     C                   Z-ADD     VNRS          D03NRS
126800970624     C                   Z-ADD     VNSP          D03NSP
126900970624     C                   Z-ADD     VDSP          D03DSP
127000970624     C                   MOVEL     VTBL          D03CBO
127100970624     C                   MOVEL     DTBL          D03DBO
127200970624     C                   Z-ADD     VFGC          D03FGC
127300970624     C                   MOVEL     DFGC          D03DFG
127400970624     C                   Z-ADD     VNGC          D03NGC
127500970624     C                   Z-ADD     VDTG          D03DGC
127600970624     C                   Z-ADD     VDIG          D03DUR
127700970624     C                   Z-ADD     VNRC          D03NRC
127800970624     C                   Z-ADD     VDXD          D03DXD
127900970624     C                   MOVEL     VCCR          D03CCR
128000970624     C                   MOVEL     DCCR          D03DCR
128100970624     C                   MOVE      VSCM          D03KSC
128200970624     C                   Z-ADD     VRIF          D03RMN
128300970624     C                   MOVEL     V1CRMA        D03RMA
128400970624     C                   MOVEL     VRSM          D03RSM
128500970624     C                   MOVEL     VINM          D03INM
128600970624     C                   MOVEL     VCAM          D03CAM
128700970624     C                   MOVEL     VLOM          D03LOM
128800970624     C                   MOVEL     VPRM          D03PRM
128900970624     C                   MOVEL     V1CNZM        D03NZM
129000970624     C                   MOVEL     VRSD          D03RSD
129100970624     C                   MOVEL     V1CRD2        D03RS2
129200970624     C                   MOVEL     VIND          D03IND
129300970624     C                   MOVEL     VCAD          D03CAD
129400970624     C                   MOVEL     VLOD          D03LOD
129500970624     C                   MOVEL     VPRD          D03PRD
129600970624     C                   MOVEL     V1CNZD        D03NZD
129700970624     C                   MOVEL     VCMC          D03CMC
129800970625     C                   MOVEL     DCMC          D03CMD
129900970624     C                   MOVEL     VDMC          D03DMC
130000970624     C                   MOVEL     'V'           D03TPV
130100970624     C                   CALL      'FNLG03R'
130200970624     C                   PARM                    FNLG03
130300970624     C*
130400970624     C                   ENDSR
130500970624     C*---------------------------------------------------------------*
130600911118     C* PULIZIA SUBFILE
130700911118     C     PULIZ         BEGSR
130800911118     C                   SETON                                        70
130900941209     C                   WRITE     LG10C02
131000911118     C                   Z-ADD     0             NRR
131100911118     C                   SETOFF                                       70
131200911118     C                   ENDSR
131300941209     C*---------------------------------------------------------------*
131400941209     C*   I N Z S R                                                   *
131500941209     C*---------------------------------------------------------------*
131600941209     C     *INZSR        BEGSR
131700990830     C*
131800941209     C     *ENTRY        PLIST
131900941209     C                   PARM                    KPJBA
132000941209     C*
132100941209     C                   MOVEL     KPJBU         FNLG30
132200941209     C*
132300941209     C                   Z-ADD     1             CODUT
132400941209     C                   CALL      'X§PARUT'
132500941209     C                   PARM                    UTEDSE
132600941209     C     CODUT         CABEQ     -1            FINE
132700941212     C                   MOVEL     RAGUT         RSUT             20
132800941209     C                   MOVEL     §PGM          VPROGR
132900990831     C*-                                                   *
133000990831     C                   MOVEL     REC80         CNCR80
133100941209      *
133200941209     C     D30FRG        IFNE      *BLANK
133300941209     C                   MOVE      D30FRG        §FRG
133400941209     C                   END
133500941209      *
133600941209     C                   SETON                                        9076
133700950103     C                   SETOFF                                       85
133800941209     C     D30RIC        COMP      'S'                                    87
133900950103     C   87              SETON                                        85
134000941209     C*                                                    DISABILITA
134100941209     C*                                                    L'INT STORIC
134200990830      * SEDE
134300990830     C     SIMFEL        IFEQ      0
134400990830     C                   SETON                                        93
134500050411     C                   OPEN      TIgcp51l
134600050415     C                   OPEN      TIgcp52l
134700050411     C                   OPEN      TITAS30C
134800990830     C                   OPEN      TITAA30C
134900990830     C                   OPEN      TITA430C
135000990830     C                   OPEN      TNCSB03L
135100990901     C                   MOVEL     '  DI SED'    V1CGES
135200990901     C                   MOVE      'E  '         V1CGES
135300990830     C                   ELSE
135400941209     C* VERIFICO SE SIAMO IN PARTENZA O ARRIVO
135500941209     C     D30POA        IFEQ      'A'
135600941209     C                   SETON                                        80
135700941209     C                   MOVEL     '   ARRIV'    V1CGES
135800941209     C                   MOVE      'O  '         V1CGES
135900941209     C                   OPEN      FNARB01L
136000990929     C                   OPEN      FIAR601L
136100051115     C                   OPEN      FIAR901L
136200060213     C                   OPEN      FIAR401L
136300941212     C                   OPEN      FNLBL01L
136400040126     C                   OPEN      FIAR501L
136500941209     C                   ELSE
136600941209     C                   MOVEL     ' PARTENZ'    V1CGES
136700941209     C                   MOVE      'A  '         V1CGES
136800941209     C                   OPEN      FNBLP01L
136900990929     C                   OPEN      FIAR601L
137000051115     C                   OPEN      Fiar901L
137100060213     C                   OPEN      FIAR401L
137200040126     C                   OPEN      FIAR501L
137300941209     C                   ENDIF
137400990830     C                   ENDIF
137500990830     C*
137600941209     C*--- RICERCA CAPOCONTI------------------------------------------*
137700941209     C                   DO        50            X                 2 0
137800941209     C                   MOVE      TCU(X)        TCUDS
137900941209     C     F56           CABNE     'CG'          END1
138000941209     C     F4            COMP      '1'                                    21
138100941209     C** 1 CLIENTI   21
138200941209     C     F3            COMP      '0'                                242425
138300941209     C** 0 ITALIA   25
138400941209     C** 1 ESTERO   24
138500941209     C   21
138600941209     CAN 25              Z-ADD     KCU(X)        KCI               4 0
138700941209     C     END1          TAG
138800941209     C                   END
138900941209     C*---------------------------------------------------------------*
139000941209     C*§
139100941209     C*  KLIST
139200941209     C     KGIA          KLIST
139300941209     C                   KFLD                    D30AGC
139400941209     C                   KFLD                    D30FGC
139500941209     C                   KFLD                    D30NGC
139600941209     C                   KFLD                    §FRG
139700000201     C     KBOLW         KLIST
139800000201     C                   KFLD                    GCPASW
139900000201     C                   KFLD                    GCPLPW
140000000201     C                   KFLD                    GCPNSW
140100000201     C                   KFLD                    GCPNBW
140200941209     C*
140300050307     C     KBOLW2        KLIST
140400050307     C                   KFLD                    GCPASWa
140500050307     C                   KFLD                    GCPLPWa
140600050307     C                   KFLD                    GCPNSWa
140700050307     C                   KFLD                    GCPNBWa
140800050307     C*
140900941209     C     KSPE          KLIST
141000941209     C                   KFLD                    §AAS
141100941209     C                   KFLD                    §LNP
141200941209     C                   KFLD                    §NRS
141300941209     C                   KFLD                    §NSP
141400990929     C     KAR6          KLIST
141500990929     C                   KFLD                    §AAS
141600990929     C                   KFLD                    §LNP
141700990929     C                   KFLD                    §NRS
141800990929     C                   KFLD                    §NSP
141900990929     C                   KFLD                    KTRC
142000941209     C*
142100941209     C     KAR4          KLIST
142200941209     C                   KFLD                    §AAS
142300941209     C                   KFLD                    §LNP
142400941209     C                   KFLD                    §NRS
142500941209     C                   KFLD                    §NSP
142600941209     C                   KFLD                    §TRC
142700990831     C     KTAS          KLIST
142800990831     C                   KFLD                    KAAS
142900990831     C                   KFLD                    KLNP
143000990831     C                   KFLD                    KNRS
143100990831     C                   KFLD                    KNSP
143200990831     C     KCSB          KLIST
143300990831     C                   KFLD                    KAAS
143400990831     C                   KFLD                    KLNP
143500990831     C                   KFLD                    KNRS
143600990831     C                   KFLD                    KNSP
143700990831     C     KTAA          KLIST
143800990831     C                   KFLD                    KAAS
143900990831     C                   KFLD                    KLNP
144000990831     C                   KFLD                    KNRS
144100990831     C                   KFLD                    KNSP
144200990831     C                   KFLD                    KTRC
144300990831     C     KTA4          KLIST
144400990831     C                   KFLD                    KAAS
144500990831     C                   KFLD                    KLNP
144600990831     C                   KFLD                    KNRS
144700990831     C                   KFLD                    KNSP
144800990831     C                   KFLD                    KTRC
144900941209     C*
145000941209     C     KTAB          KLIST
145100941209     C                   KFLD                    CODUT
145200941209     C                   KFLD                    TBLCOD
145300941209     C                   KFLD                    TBLKEY
145400941209     C*
145500941209     C     KNOT          KLIST
145600941209     C                   KFLD                    GCPAGC
145700941209     C                   KFLD                    GCPFGC
145800941209     C                   KFLD                    GCPNGC
145900990628     C                   KFLD                    §FRG              2 0
146000941209     C                   KFLD                    §FAS              3 0
146100941209     C*
146200941209     C     KNOR          KLIST
146300941209     C                   KFLD                    GCPAGC
146400941209     C                   KFLD                    GCPFGC
146500941209     C                   KFLD                    GCPNGC
146600941209     C                   KFLD                    §FRG
146700941209     C                   KFLD                    §FAS
146800941209     C                   KFLD                    VNPR
146900040126     C*
147000040126     C     Kar5          KLIST
147100040126     C                   KFLD                    §AAS
147200040126     C                   KFLD                    §LNP
147300040126     C                   KFLD                    §NRS
147400040126     C                   KFLD                    §NSP
147500040126     C                   KFLD                    trcar5
147600941209     C*
147700941209     C                   SETON                                        90
147800941209     C*
147900040126     C     *LIKE         DEFINE    ar5trd        trcar5
148000941209     C     *LIKE         DEFINE    GCPAAS        §AAS
148100941209     C     *LIKE         DEFINE    GCPLNP        §LNP
148200941209     C     *LIKE         DEFINE    GCPNRS        §NRS
148300941209     C     *LIKE         DEFINE    GCPNSP        §NSP
148400941209     C     *LIKE         DEFINE    ACOKSC        §KSC
148500941209     C     *LIKE         DEFINE    AR4TRC        §TRC
148600990831     C     *LIKE         DEFINE    TASAAS        KAAS
148700990831     C     *LIKE         DEFINE    TASLNP        KLNP
148800990831     C     *LIKE         DEFINE    TASNRS        KNRS
148900990831     C     *LIKE         DEFINE    TASNSP        KNSP
149000990831     C     *LIKE         DEFINE    TAATRC        KTRC
149100941209     C                   MOVEL     *ZEROS        G08DAT
149200941209     C                   MOVEL     *ZEROS        G08TGI
149300941209     C                   MOVE      *ZEROS        G08INV
149400941209     C                   MOVEL     *BLANK        G08ERR
149500941209     C*
149600941209     C                   ENDSR
