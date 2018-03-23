000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950110     H* FNLR13R *----------------------------------------------------*
000300950109     H*        - RICEZIONE BOLLE TRANSITO                            *
000400000000     H*--------------------------------------------------------------*
000500020611     FFNBTP11L  UF A E           K DISK
000600020611     FFNBTT11L  UF A E           K DISK
000700990510     FFNANM02L  IF   E           K DISK
000800990510     F                                     INFDS(FNANM2)
000900130702     Ffnblp77l  IF   E           K DISK
001000130702     Ffnblt01l  IF   E           K DISK
001100080402     FTABEL00F  IF   E           K DISK
001200070123     FFNBRV09L  IF   E           K DISK
001300950110     FFNFGV01L  UF   E           K DISK
001400950110     FFNFVD01L  UF   E           K DISK
001500980213     FAZORG01L  IF   E           K DISK
001600950109     D*
001700021008     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001800021008     D CMD4            S             55    DIM(1) CTDATA PERRCD(1)
001900021008     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
002000970509     D CAT             S              1  0 DIM(20)                              CATEGORIE FOGLI
002100040322     D L1              S              3  0 DIM(30)
002200950109     D*
002300930201     D KPJBA         E DS
002400911001     D  LEGFIL               483    492
002500911001     D  MBRFIL               493    502
002600020809     D*
002700020809     D UT§DSE0F      E DS
002800020809     D CNCR80        E DS
002900020809     D FNLR33DS      E DS
003000020809     D* CALL A PGM DI REPERIMENTO DATI FOGLI
003100020809     D FNLV53DS      E DS
003200040322     D* DS PER TRUL06R - CARICAMENTO £X
003300040322     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003400040322     D  LIN                    1     90  0
003500040322     D                                     DIM(30)
003600070223     D FNLS45DS      E DS
003700070223     D DSbrv         E DS                  EXTNAME(fnbrv00f)
003800020809     D*
003900021015     D DS7N          E DS
004000020809     D DSSPP         E DS                  EXTNAME(FISPP00R)
004100020809     D*
004200970508     D PARAM           DS
004300970508     D  D21TLA                 1      1
004400991231     D  D21REC                 2     68
004500991231     D  D21ERR                69     69
004600991231     D  D21NOS                70     70
004700970819     D* NON SERVE PERCHE' QUI SIUAMO NELLE BOLLE TRANSITO MA SERVE
004800970819     D*  SOLO PER LE BOLLE ARRIVI PER FAR LANCIARE PGM FNLR27R
004900991231     D  D21ABI                71     71
005000950110     D WLBDAT          DS
005100950109     D  G02DAT                 1      8  0
005200950109     D  G02INV                 9     16  0
005300950109     D  G02ERR                17     17
005400950109     D  G02TGI                18     22  0
005500950110     D                 DS
005600950110     D* SCHIERA PER I TIPO SERVIZIO PRESENTI SUL RECORD DI FNFVD
005700950110     D  TSP                    1      6
005800950110     D                                     DIM(6)                               TIPO SERVIZIO
005900950110     D  FVDTS1                 1      1
006000950110     D  FVDTS2                 2      2
006100950110     D  FVDTS3                 3      3
006200950110     D  FVDTS4                 4      4
006300950110     D  FVDTS5                 5      5
006400950110     D  FVDTS6                 6      6
006500950110     D                 DS
006600950110     D* SCHIERA PER I VOLUMI PER TSP PRESENTI SUL RECORD DI FNFVD
006700950110     D  VOL                    1     54  4
006800950110     D                                     DIM(6)                               VOLUME PER TSP
006900950110     D  FVDVL1                 1      9  4
007000950110     D  FVDVL2                10     18  4
007100950110     D  FVDVL3                19     27  4
007200950110     D  FVDVL4                28     36  4
007300950110     D  FVDVL5                37     45  4
007400950110     D  FVDVL6                46     54  4
007500020809     D*
007600990510     D FNANM2          DS
007700960412     D  ANMNR2               397    400B 0
007800020809     D*
007900020809     D* DEFINIZIONE DI ALCUNI CAMPI
008000021015     D COD             S                   LIKE(TBLCOD)
008100020809     D KEY             S                   LIKE(TBLKEY)
008200020809     D PKGU            S                   LIKE(FVDKFA)
008300020809     D VOLU            S                   LIKE(FVDVL1)
008400020809     D WSPE            S                   LIKE(FVDSNA)
008500020809     D KNPG            S                   LIKE(BRVNPG)                         CATEGORIA FOGLIO
008600020809     D KLNP            S                   LIKE(BRVLNP)                                      LNP
008700020809     D KLNA            S                   LIKE(BRVLNA)                                      LNA
008800020809     D KNFV            S                   LIKE(FGVNFV)                         NR.FOGLIO
008900020809     D KTRC            S                   LIKE(FVDTRC)                         TIPO RECORD
009000020809     D COMNCA          S              8  0 INZ
009100020809     D COMNCD          S              8  0 INZ
009200020809     D WTIME           S             14  0 INZ                                  ORA & DATA
009300020809     D WDATE           S              8  0 INZ                                  GG/MM/AAAA
009400020809     D DATEU           S              8  0 INZ                                  AAAA/MM/GG
009500020809     D LUNG            S             15  5 INZ(80)
009600020809     D COMMAN          S             80    INZ
009700020809     D XB              S              2  0 INZ
009800020809     D WA              S              3  0 INZ
009900020809     D WDISG           S              1    INZ
010000021008     D VAR11           S             11    INZ
010100021008     D VAR12           S             12    INZ
010200020809     D WCA21           S              1    INZ
010300020809     D WFC21           S              1    INZ
010400020809     D WFC44           S              1    INZ
010500020809     D WSPU            S              1    INZ
010600940111     C*****************************************************************
010700911003     C*  RIEPILOGO INDICATORI
010800940111     C*****************************************************************
010900930201     C* 01/02 - INDICATORI DI TIPO RECROD
011000940111     C* 15    - LOKUP
011100921022     C* 20    - NON ALLOCATI DEGLI OGGETTI ESCO DA PROGRAMMA
011200950110     C* 31    - LETTURA FNBLPT00
011300940111     C* 32/35 - COMODO
011400911003     C*****************************************************************
011500000000     C     *ENTRY        PLIST
011600000000     C                   PARM                    KPJBA
011700950109     C*
011800940111     C                   CALL      'X§PARUT'
011900020809     C                   PARM                    UT§DSE0F
012000940111     C                   MOVEL     REC80         CNCR80
012100021015     c*
012200021016     C                   Z-ADD     SIMFEL        COMTFP            3 0
012300940111     C*---------------------------------------------------------------*
012400910322     C*
012500911011     C* GIRO DATA DEL GIORNO
012600020809     C                   TIME                    WTIME                          ORA & DATA
012700020809     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
012800950109     C                   Z-ADD     WDATE         G02DAT
012900950109     C                   MOVEL     *BLANKS       G02ERR
013000950109     C                   CALL      'XSRDA8'
013100950109     C                   PARM                    WLBDAT
013200020809     C                   Z-ADD     G02INV        DATEU                          AAAA/MM/GG
013300911011     C*
013400911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
013500911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
013600911001     C     MBRFIL        IFEQ      *BLANK
013700021008     C                   MOVEL     '*FIRST'      MBRFIL
013800911001     C                   END
013900921022     C*
014000940111     C* CARICAMENTO TABELLE
014100940111     C                   EXSR      CARTAB
014200911001     C*
014300130702     c                   z-add     28            ktfp              3 0
014400130702     c                   z-add     11347         knfv              5 0
014500130702     c     kblp77        klist
014600130702     c                   kfld                    ktfp
014700130702     c                   kfld                    knfv
014800130702     c
014900130702     C     kblp77        setll     FNBLP000                               31
015000130702     C     kblp77        reade     FNBLP000                               31
015100911003     C*
015200911001     C     *IN31         DOWEQ     '0'
015300130702     c
015400130702     c                   if        blplna=81 or blplna=82
015500130702     c                             or blplna=104 or blplna=113
015600130702     c                             or blplna=138
015700950109     C*
015800080402     C* VERIFICO SE c'e' gioà la bolla
015900020611     C     KBTP11        SETLL     FNBTP11L                               33
016000950109     C*
016100911003     C* 33 OFF - CREO BOLLA
016200911003     C  N33              EXSR      CRTBOL
016300130702     C                   ENDIF
016400130702     C
016500130702     C     kblp77        reade     FNBLP000                               31
016600911001     C*
016700911001     C                   END
016800970508     C**
016900970508     C* SE HO APERTO IL PGM LO DEVO CHIUDERE
017000970508     C     WFC44         IFEQ      'S'
017100070223     C                   MOVEL     'C'           dls45flg
017200070223     C                   CALL      'FNLS45R'
017300070223     c                   parm                    fnls45ds
017400070223     c                   parm                    dsbrv
017500970508     C                   ENDIF
017600970508     C**
017700970508     C     WFC21         IFEQ      'S'
017800970508     C                   MOVEL     'C'           D21TLA
017900970508     C                   MOVEL     PARAM         KPJBU
018000970509     C                   CALL      'FNLR21R'
018100970508     C                   PARM                    KPJBA
018200970508     C                   ENDIF
018300900911     C*
018400900409     C     FINE          TAG
018500020809     C                   EVAL      *INLR = *ON
018600950109     C**************************************************************************
018700950109     C*    CARICAMENTO TABELLE
018800950109     C**************************************************************************
018900940111     C     CARTAB        BEGSR
019000970509     C*****
019100970509     C***  CARICO CATEGORIE
019200970509     C*****
019300970509     C                   MOVEL     '7N'          COD
019400020809     C                   Z-ADD     0             XB
019500970509     C     KTAB2         SETLL     TABEL
019600970509     C     KTAB2         READE     TABEL                                  31
019700970509     C*
019800970509     C     *IN31         DOWEQ     '0'
019900970509     C     TBLFLG        IFEQ      ' '
020000970509     C*
020100020809     C                   ADD       1             XB
020200020809     C                   MOVEL     TBLKEY        CAT(XB)
020300970509     C                   END
020400970509     C*
020500970509     C     KTAB2         READE     TABEL                                  31
020600970509     C                   END
020700940111     C*****
020800970508     C***  RICERCO FASE CATEGORIA ARRIVI
020900940111     C*****
021000940111     C                   MOVEL     '7N'          COD
021100940111     C                   MOVEL     *BLANKS       KEY
021200940111     C                   MOVEL     2             KEY
021300940111     C     KTAB          CHAIN     TABEL                              31
021400940111     C  N31              MOVEL     TBLUNI        DS7N
021500040322     c* CARICO £1
021600040322     C                   CLEAR                   DSUL06
021700040322     C                   MOVE      '£1'          D06COD
021800040322     C                   MOVEL     SIMFEL        D06KEY
021900040322     C                   MOVEL     'L'           D06TLA
022000040322     C                   MOVEL     DSUL06        KPJBU
022100040322     C*
022200040322     C                   CALL      'TRUL06R'
022300040322     C                   PARM                    KPJBA
022400040322     C                   MOVEL     KPJBU         DSUL06
022500040322     C                   MOVEA     LIN           L1
022600940111     C*
022700940111     C                   ENDSR
022800950109     C**************************************************************************
022900950109     C*  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI
023000950109     C**************************************************************************
023100950109     C**************************************************************************
023200950109     C*  . HO IN MEMORIA LA BOLLA RICEVUTA
023300950109     C*  . SCRIVO I COLLI DELLA BOLLA TRANSITO
023400950109     C*  . CANCELLO I COLLI DELLA BOLLA RICEVUTA
023500950109     C*  . SCRIVO LA BOLLA TRANSITO
023600950109     C*  . CANCELLO LA BOLLA RICEVUTA
023700950109     C*  . VERIFICO SE C'E' DISGUIDO
023800950109     C**************************************************************************
023900911002     C     CRTBOL        BEGSR
024000911003     C*
024100950109     C* MUOVO I CAMPI DALLA BOLLA RICEVUTA ALLA BOLLA TRANSITO
024200950110     C                   CLEAR                   FNBTP000
024300950109     C                   Z-ADD     BLPAAS        BTPAAS
024400950109     C                   Z-ADD     BLPLNP        BTPLNP
024500950109     C                   Z-ADD     BLPNRS        BTPNRS
024600950109     C                   Z-ADD     BLPNSP        BTPNSP
024700950109     C                   Z-ADD     BLPMGS        BTPMGS
024800950110     C                   MOVEL     BLPCBO        BTPCBO
024900950110     C                   MOVEL     BLPFNS        BTPFNS
025000950109     C                   Z-ADD     BLPLNA        BTPLNA
025100950110     C                   Z-ADD     BLPZNC        BTPZNC
025200950109     C                   MOVEL     BLPRSM        BTPRSM
025300950109     C                   MOVEL     BLPRSD        BTPRSD
025400950110     C                   MOVEL     BLPTSP        BTPTSP
025500950109     C                   Z-ADD     BLPNCL        BTPNCL
025600950110     C                   Z-ADD     BLPPKB        BTPPKB
025700950110     C                   MOVEL     BLPFVB        BTPFVB
025800950110     C                   Z-ADD     BLPVLB        BTPVLB
025900950109     C                   Z-ADD     BLPFLS        BTPFLS
026000950109     C                   Z-ADD     BLPNCD        BTPNCD
026100950109     C                   Z-ADD     BLPNCA        BTPNCA
026200950110     C                   MOVEL     BLPXCO        BTPXCO
026300950109     C                   Z-ADD     BLPDBR        BTPDBR
026400950109     C                   Z-ADD     BLPNFV        BTPNFV
026500950109     C                   Z-ADD     BLPFLE        BTPFLE
026600950109     C                   Z-ADD     BLPTFP        BTPTFP
026700950109     C                   Z-ADD     BLPFEA        BTPFEA
026800950109     C                   Z-ADD     BLPTFA        BTPTFA
026900020607     C                   Z-ADD     SIMFEL        BTPFLP
027000950109     C                   MOVEL     'R'           BTPFT1                         'R'=RICEZIONE
027100950109     C                   Z-ADD     DATEU         BTPDT1
027200930201     C*
027300080402     c*  I segnacolli li prendo dal dettaglio bolle partenza
027400080402     C     KBLP          SETLL     FNBLt01l
027500080402     C     KBLP          READE     FNBLt01l                               32
027600911002     C     *IN32         DOWEQ     '0'
027700921022     C                   EXSR      CTRBTT
027800911015     C*
027900950808     C                   WRITE     FNBTT000                             99
028000911015     C*
028100080402     C     KBLP          READE     FNBLT01l                               32
028200080402     C                   ENDDO
028300911002     C*
028400911015     C* SCRIVO FILE
028500950808     C                   WRITE     FNBTP000                             99
028600940111     C*
028700921022     C* CONTROLLO EVENTUALE ANOMALIA DI DISGUIDO
028800950110     C     KBTT1         SETLL     FNBTT000
028900950110     C     KBTT1         READE     FNBTT000                               32
029000921022     C*
029100970508     C     *IN32         DOWEQ     *OFF
029200921022     C                   EXSR      CTRANM
029300950110     C     KBTT1         READE     FNBTT000                               32
029400970508     C                   ENDDO
029500911003     C*
029600911003     C                   ENDSR
029700950109     C**************************************************************************
029800950109     C*  RIEMPO I CAMPI DEL FILE DETTAGLIO BOLLE TRANSITO
029900950109     C**************************************************************************
030000920805     C     CTRBTT        BEGSR
030100950109     C*
030200950110     C                   CLEAR                   FNBTT000
030300950109     C                   Z-ADD     BLTAAS        BTTAAS
030400950109     C                   Z-ADD     BLTLNP        BTTLNP
030500950109     C                   Z-ADD     BLTNRS        BTTNRS
030600950109     C                   Z-ADD     BLTNSP        BTTNSP
030700950110     C                   Z-ADD     BLTNSC        BTTNSC
030800080402     C                   MOVEL     Bltlna        BTTLNA
030900080402     C                   MOVEL     Bltfls        BTTFLS
031000080402     C                   MOVEL     Bltvuc        BTTvuc
031100080402     C                   MOVEL     Bltfvc        BTTfvc
031200080402     C                   MOVEL     Bltpuc        BTTpuc
031300080402     C                   MOVEL     Bltfpc        BTTfpc
031400080402     c
031500950109     C                   MOVEL     BTPFLP        BTTFLP
031600950110     C                   MOVEL     'R'           BTTFTR                         'R'=RICEZIONE
031700950110     C                   Z-ADD     DATEU         BTTDTR                         DATA RICEZIONE
031800950109     C*
031900920805     C                   ENDSR
032000950109     C**************************************************************************
032100950109     C*  CONTROLLO SE ESISTEVA ANOMALIA
032200950109     C**************************************************************************
032300911015     C     CTRANM        BEGSR
032400970307     C                   CLEAR                   WDISG
032500970509     C                   CLEAR                   WSPU
032600970509     C                   CLEAR                   WCA21
032700970509     C                   CLEAR                   DSSPP
032800920805     C*
032900960520     C                   Z-ADD     200           COMCAA
033000040322     c     kanm1         setll     fnanm02l
033100040322     C     KANM1         reade     FNANM02L                               33
033200040322     c     *in33         doweq     *off
033300040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
033400040429     c**   anmfle        lookup    l1                                     33
033500040429    1C**   *IN33         IFEQ      *On
033600040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
033700040429     c*  se spara 2 livello deve rimanere l'anomalia200
033800040429     c                   if        anmfle=simfel
033900040429     C* CHIUDO ANOMALIA
034000020809     C                   MOVEL     'S'           WDISG
034100960415     C*
034200020809     C                   CLEAR                   FNLR33DS
034300960415     C                   Z-ADD     ANMNR2        D33NRR
034400960415     C                   MOVE      DATEU         D33DCH
034500960415     C                   MOVEL     §7NFS1        D33FSC
034600960415     C                   CALL      'FNLR33R'
034700020809     C                   PARM                    FNLR33DS
034800970307    1C                   ENDIF
034900040322     C     KANM1         reade     FNANM02L                               33
035000040322     c                   enddo
035100960520     C*
035200960520     C* CHAINO ANCHE L'ANOMALIA 55 CHE SE POSSO LA DELETO
035300960520     C                   Z-ADD     55            COMCAA
035400040429     c                   clear                   wtrov             1
035500040322     c     kanm1         setll     fnanm02l
035600040322     C     KANM1         reade     FNANM02L                               33
035700040322     c     *in33         doweq     *off
035800040429     c     wtrov         andeq     *blanks
035900040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
036000040429     c**   anmfle        lookup    l1                                     33
036100040429    1C**   *IN33         IFEQ      *OFF
036200040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
036300040429     c*  se spara 2 livello deve rimanere l'anomalia200
036400040429     c                   if        anmfle=simfel
036500040429     C* CHIUDO ANOMALIA
036600020809     C                   MOVEL     'S'           WDISG
036700040429     C                   MOVEL     'S'           Wtrov
036800020809     C                   CLEAR                   FNLR33DS
036900960520     C                   Z-ADD     ANMNR2        D33NRR
037000960520     C                   MOVE      DATEU         D33DCH
037100960520     C                   MOVEL     §7NFS1        D33FSC
037200960520     C                   MOVEL     'S'           D33MAC
037300960520     C                   CALL      'FNLR33R'
037400020809     C                   PARM                    FNLR33DS
037500040429     c                   else
037600040322     C     KANM1         reade     FNANM02L                               33
037700040429    1C                   ENDIF
037800040322     c                   enddo
037900040108     C*
038000040108     C* CHAINO ANCHE L'ANOMALIA 56 CHE SE POSSO LA DELETO
038100040108     C                   Z-ADD     56            COMCAA
038200040322     c                   clear                   wtrov
038300040322     c     kanm1         setll     fnanm02l
038400040322     C     KANM1         reade     FNANM02L                               33
038500040322     c     *in33         doweq     *off
038600040429     c     wtrov         andeq     *blanks
038700040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
038800040429     c**   anmfle        lookup    l1                                     33
038900040429    1C**   *IN33         IFEQ      *OFF
039000040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
039100040429     c*  se spara 2 livello deve rimanere l'anomalia200
039200040429     c                   if        anmfle=simfel
039300040429     C* CHIUDO ANOMALIA
039400040108     C                   MOVEL     'S'           WDISG
039500040429     C                   MOVEL     'S'           Wtrov
039600040108     C                   CLEAR                   FNLR33DS
039700040108     C                   Z-ADD     ANMNR2        D33NRR
039800040108     C                   MOVE      DATEU         D33DCH
039900040108     C                   MOVEL     §7NFS1        D33FSC
040000040108     C                   MOVEL     'S'           D33MAC
040100040108     C                   CALL      'FNLR33R'
040200040108     C                   PARM                    FNLR33DS
040300040429     c                   else
040400040322     C     KANM1         reade     FNANM02L                               33
040500040429    1C                   ENDIF
040600040322     c                   enddo
040700970307     C**
040800970307    1C     WDISG         IFEQ      'S'
040900970509     C*
041000970509     C* CERCO LE SPUNTE DEL COLLO
041100020809     C                   Z-ADD     1             XB
041200020809    2C     CAT(XB)       DOWNE     0
041300020809     C                   MOVEL     CAT(XB)       KNPG
041400970508     C**
041500070123     C     KBRV1         CHAIN     FNBRV09L                           33
041600970508     C**
041700970509     C* PRIMA DEVO AGGIORNARE TUTTE LE SPUNTE PARTENZA PER POI
041800970509     C*  RILASCIARE IL RECORD DI FNBTT
041900970509    3C     *IN33         DOWEQ     *OFF
042000970508     C* SPUNTA PARTENZA
042100970509    4C     BRVNPG        IFEQ      1                                            1=PARTENZE
042200970508     C**
042300970508     C* SE SPUNTA NON MIA RICHIAMO PGM PER AGGIORNARE LA DATA DI PART
042400970509    5C     BRVFLE        IFNE      SIMFEL
042500020809     C                   CLEAR                   FNLV53DS
042600970508     C                   MOVEL     'A'           D53TFO
042700991230     C                   Z-ADD     BRVNFV        D53NFV
042800970508     C                   MOVEL     BRVFGS        D53FGS
042900970508     C                   MOVEL     SIMFEL        D53FEL
043000970508     C                   CALL      'FNLV53R'
043100020809     C                   PARM                    FNLV53DS
043200970509     C*
043300970509    6C     D53ERR        IFEQ      ' '
043400970509     C     D53DFV        ANDGE     SPPDFV
043500970508     C                   CLEAR                   DSSPP
043600970508     C                   MOVEL     BRVLNP        SPPLNP
043700970508     C                   MOVEL     BRVLNA        SPPLNA
043800970508     C                   MOVEL     BRVNRS        SPPNRS
043900970508     C                   MOVEL     BRVNSC        SPPNSC
044000970508     C                   MOVEL     BRVZNC        SPPZNC
044100970508     C                   MOVEL     BRVNPS        SPPNPS
044200970508     C                   MOVEL     BRVNPG        SPPNPG
044300970508     C                   MOVEL     BRVFGS        SPPFGS
044400970508     C                   MOVEL     BRVNFV        SPPNFV
044500970508     C                   MOVEL     D53DFV        SPPDFV
044600970508     C                   MOVEL     BRVATR        SPPATR
044700970508     C                   MOVEL     BRVVUC        SPPVUC
044800970508     C                   MOVEL     BRVPUC        SPPPUC
044900970508     C                   MOVEL     BRVCAN        SPPCAN
045000970508     C*
045100970508     C* SPPFLP NON LO PASSO PERCHE' SERVE SOLO SE LA SPUNTA E' "A"
045200020610     C* 10/06/02: NON È PIÙ VERO ORA SPPFLP SERVE ANCHE QUANDO LA SPUNT
045300020610     C* E' BLANK SE SI TRATTA DI UNTA SPUNTA PER IL TRANSITO
045400020610     C     SPPATR        IFEQ      *BLANKS
045500020610     C                   Z-ADD     SIMFEL        SPPFLP
045600020610     C                   ENDIF
045700970508     C                   MOVEL     DSSPP         D21REC
045800970508     C                   MOVEL     ' '           D21TLA
045900020809     C                   MOVEL     'S'           WCA21
046000970509    6C                   ENDIF
046100970508     C**
046200970509   X5C                   ELSE
046300970508     C** SPUNTA DI SIMFEL
046400950110     C                   EXSR      AGGFGV
046500970508     C*
046600970509     C** SE NON SI TRATTA DI PISTOLA AUTOGENERATA AGGIORNO
046700101202    6C     BRVNPS        IFNE      98
046800101202     C     BRVNPS        ANDNE     96
046900970508     C                   EXSR      CHIAMO
047000970509    6C                   ENDIF
047100970509    5C                   ENDIF
047200970508     C**
047300970509   X4C                   ELSE
047400040322     c* Richiamo fnls44r solo se spunta dell'area di partenza del p.o.
047500040322     c* di transito (simfel)
047600040322     c* Infatti dando per scontato che i secondi livello in arrivo tota
047700040322     c* li o parziali non hanno transiti usiamo la £1 perchè di fatto
047800040322     c* solo p.o. con ter-arr = ter-part aggiornano le bolle transito
047900040322     c* (v. anche fnls44r)
048000040322     c     brvpes        lookup    l1                                     33
048100040322     c*
048200040322     C   33              EXSR      CHIAMO
048300040322     c*
048400970509    4C                   ENDIF
048500921022     C*
048600070123     C     KBRV1         READE     FNBRV09L                               33
048700970509    3C                   ENDDO
048800020809     C                   ADD       1             XB
048900970509    2C                   ENDDO
049000940328     C*
049100940328     C* AGGIORNO E RILASCIO RECORD
049200970509    2C     WSPU          IFEQ      ' '
049300970508     C                   UPDATE    FNBTT000
049400970509    2C                   ENDIF
049500940328     C*
049600970509    1C                   ENDIF
049700970509     C*
049800970509     C* COME ULTIMA COSA AGGIORNO LA DATA DI PARTENZA DEL FOGLIO
049900970509     C     WCA21         IFEQ      'S'
050000970819     C                   CLEAR                   D21NOS
050100970509     C                   MOVEL     PARAM         KPJBU
050200970509     C                   CALL      'FNLR21R'
050300970509     C                   PARM                    KPJBA
050400020809     C                   MOVEL     'S'           WFC21
050500970509    5C                   ENDIF
050600911015     C*
050700911015     C                   ENDSR
050800950109     C**************************************************************************
050900950109     C*  CHIAMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
051000950109     C**************************************************************************
051100921022     C     CHIAMO        BEGSR
051200940328     C*
051300940328     C* AGGIORNO E RILASCIO RECORD
051400970509     C     WSPU          IFEQ      ' '
051500950110     C                   UPDATE    FNBTT000
051600020809     C                   MOVEL     'S'           WSPU
051700970509     C                   ENDIF
051800921022     C*
051900070223     c                   clear                   fnls45ds
052000070223     c                   move      brvpes        dls45pes
052100070223     c                   movel     'S'           dls45ric
052200070223     c                   move      comtfp        dls45tfp
052300070223     c                   movel     'T'           dls45flg
052400070223     c                   call      'FNLS45R'
052500070223     c                   parm                    fnls45ds
052600070223     c                   parm                    dsbrv
052700950109     C*
052800020809     C                   MOVEL     'S'           WFC44
052900950110     C*
053000921022     C                   ENDSR
053100950109     C**************************************************************************
053200950109     C*    AGGIORNO FOGLIO VIAGGIO
053300950109     C**************************************************************************
053400940111     C     AGGFGV        BEGSR
053500950109     C*
053600940111     C                   CLEAR                   WSPE
053700940111     C*
053800940111     C* TROVATA SPUNTA PARTENZA
053900020809     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
054000020809     C     K02FGV01      chain     FNFGV01L                           34
054100940111     C*
054200940328    1C     *IN34         IFEQ      *OFF
054300940328     C* SOLO SE E' CHIUSO
054400940328    2C     FGVFCF        IFNE      *BLANKS
054500950110     C                   Z-ADD     FGVNFV        BTTNFV
054600950110     C                   Z-ADD     FGVDFV        BTTDUT                         DATA FOGLIO
054700950110     C                   Z-ADD     BRVNPS        BTTPUT
054800940328    2C                   ENDIF
054900940328   X1C                   ELSE
055000940328     C* SE NON TROVATO FOGLIO CME SE FOSSE CHIUSO
055100950110     C                   Z-ADD     DATEU         BTTDUT
055200940328     C                   MOVEL     'S'           FGVFCF
055300940328    1C                   ENDIF
055400940111     C*
055500940328     C* FOGLIO CHIUSO
055600940328    1C     FGVFCF        IFNE      *BLANKS
055700940328     C*
055800950110     C     KBTP1         CHAIN     FNBTP000                           35
055900950110    2C     BTPDUT        IFEQ      *ZERO
056000950110     C  N35              Z-ADD     BTTDUT        BTPDUT                         DATA USCITA TRANSITO
056100940111     C* SOMMO LA SPEDIZIONE DI UN COLLO "EX MANCA BOLLA"
056200940111     C                   Z-ADD     1             WSPE
056300940328    2C                   ENDIF
056400950206     C*
056500950206     C  N35              UPDATE    FNBTP000
056600940111     C*
056700940111     C* CALCOLO PESO E VOLUME UNITARI DI UN COLLO "EX MANCA BOLLA" (IL
056800940111     C*   COLLO L'HO GIA' SOMMATO AL MOMENTO DELLA CHIUSURA DEL FOGLIO)
056900940111     C                   EXSR      TOTFVD
057000940111     C*
057100950110     C  N34              UPDATE    FNFGV000
057200940328    1C                   ENDIF
057300940111     C*
057400940111     C                   ENDSR
057500950109     C**************************************************************************
057600950109     C*    CALCOLO PESO/VOLUME/SPEDIZIONI UNITARI
057700950110     C* IL FILE FNFVD SERVIRA' PER IL PROSPETTO NOLI (FNLS59R)
057800950109     C**************************************************************************
057900940111     C     TOTFVD        BEGSR
058000940111     C*
058100980213     C* IN FNFVD POSSO AVERE TRE RECORD PER LO STESSO FOGLIO:
058200950110     C* TRC:   "1" = DELL'AREA            "2" = TRANSITO
058300980213     C*                                   "3" = TRANSITO PARTENZA SDI
058400980216     C* DEVO CONTROLLARE LA LINEA DI PARTENZA DELLA BOLLA
058500980216     C* PER CONOSCERE LA DITTA ORIGINARIA SDI O BARTOLINI
058600980216     C                   MOVEL     '2'           KTRC                           '2'=TRANSITO PART.BA
058700980213     C* AGGANCIO L'ORGANIGRAMMA PER VERIFICARE SE E' SDI O BARTOLINI
058800980216     C     BTTLNP        CHAIN     AZORG01L                           35
058900980213     C  N35ORGDIT        IFEQ      'SDI'
059000980213     C                   MOVEL     '3'           KTRC                           '3'=TRANSITO PART.SD
059100980213     C                   END
059200980213     C*
059300950110     C                   Z-ADD     SIMFEL        KLNP                           LNP
059400950110     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
059500950110     C                   Z-ADD     BTPLNA        KLNA                           LNA
059600950110     C     KFVD1         CHAIN     FNFVD01L                           35
059700940111     C*
059800940111     C     *IN35         IFEQ      *OFF
059900940111     C*****
060000940111     C***  P E S O
060100940111     C*****
060200950110     C     BTPPKB        DIV(H)    BTPNCL        PKGU
060300940111     C*
060400950110     C* ADDIZIONO IN FNFVD
060500950110     C     BTPPKB        IFGT      3500
060600950110     C                   ADD       PKGU          FVDKFA                         *OLTRE 35 Q.
060700940111     C                   ELSE
060800950110     C                   ADD       PKGU          FVDKOA                         *FINO  35 Q.
060900940111     C                   ENDIF
061000940111     C*
061100940111     C*****
061200940111     C***  V O L U M E
061300940111     C*****
061400940111     C* SE IL VOLUME DEL COLLO NON E' INDICATO IN FLBTT, LO CALCOLO
061500950110     C*   PRENDENDOLO DA FNBTP
061600950110     C     BTTVUC        IFNE      0
061700950110     C                   Z-ADD(H)  BTTVUC        VOLU
061800940111     C*
061900940111     C                   ELSE
062000940111     C*
062100950110     C     BTPVLB        DIV(H)    BTPNCL        VOLU
062200940111     C                   ENDIF
062300940111     C*
062400950110     C* VEDO IN QUALE POSIZIONE (DEL RECORD) STA IL TIPO SERVIZIO
062500020809     C                   Z-ADD     1             WA
062600950110     C     BTPTSP        LOOKUP    TSP(WA)                                15     ON=TROVATO
062700950110     C  N15              ADD       VOLU          VOL(WA)
062800940111     C*
062900940111     C*****
063000940111     C***  S P E D I Z I O N I
063100940111     C*****
063200940111     C     WSPE          IFGT      0
063300950110     C                   ADD       WSPE          FVDSNA
063400940111     C                   ENDIF
063500940111     C*
063600940111     C* ABBLENCO FLAG E DATA TRASMISSIONE PER INVIO IN SEDE
063700940111     C                   MOVEL     ' '           FVDFTR
063800940111     C                   Z-ADD     0             FVDDTR
063900940111     C*
064000950110     C                   UPDATE    FNFVD000
064100940111     C                   ENDIF
064200940111     C*
064300940111     C                   ENDSR
064400950109     C**************************************************************************
064500950110     C*    R O U T I N E    I N I Z I A L E
064600950110     C**************************************************************************
064700950110     C     *INZSR        BEGSR
064800950110     C*
064900950110     C     KTAB          KLIST
065000950110     C                   KFLD                    CODUT
065100950110     C                   KFLD                    COD
065200950110     C                   KFLD                    KEY
065300950110     C     KTAB2         KLIST
065400950110     C                   KFLD                    CODUT
065500950110     C                   KFLD                    COD
065600970508     C     KBLP          KLIST
065700950110     C                   KFLD                    BLPAAS
065800950110     C                   KFLD                    BLPLNP
065900950110     C                   KFLD                    BLPNRS
066000950110     C                   KFLD                    BLPNSP
066100020611     C     KBTP11        KLIST
066200020611     C                   KFLD                    SIMFEL
066300020611     C                   KFLD                    BLPAAS
066400020611     C                   KFLD                    BLPLNP
066500020611     C                   KFLD                    BLPNRS
066600020611     C                   KFLD                    BLPNSP
066700020611     C     KBTP1         KLIST                                                  FNBTP11L
066800020611     C                   KFLD                    BTTFLP
066900950110     C                   KFLD                    BTTAAS
067000950110     C                   KFLD                    BTTLNP
067100950110     C                   KFLD                    BTTNRS
067200950110     C                   KFLD                    BTTNSP
067300020611     C     KBTT1         KLIST                                                  FNBTT11L
067400020611     C                   KFLD                    BTPFLP
067500950110     C                   KFLD                    BTPAAS
067600950110     C                   KFLD                    BTPLNP
067700950110     C                   KFLD                    BTPNRS
067800950110     C                   KFLD                    BTPNSP
067900040108     C     KANM1         KLIST                                                  FNANM02l
068000970508     C                   KFLD                    BTTFLS
068100020611     C                   KFLD                    BTTLNA
068200950110     C                   KFLD                    BTTNRS
068300950110     C                   KFLD                    BTTNSC
068400950110     C                   KFLD                    COMCAA            3 0
068500070123     C     KBRV1         KLIST                                                  FNBRV09L
068600970509     C                   KFLD                    KNPG
068700970509     C                   KFLD                    BTTFLS                         ..LNP
068800950110     C                   KFLD                    BTTLNA                         ..LNA
068900950110     C                   KFLD                    BTTNRS                         ..SERIE
069000950110     C                   KFLD                    BTTNSC                         ..SEGNACOLLO
069100950110     C     KFVD1         KLIST                                                  FNFVD01L
069200950110     C                   KFLD                    KLNP                           ..LNP
069300950110     C                   KFLD                    KNFV                           ..NR.FOGLIO
069400950110     C                   KFLD                    KLNA                           ..LNA
069500950110     C                   KFLD                    KTRC                           ..TIPO RECORD
069600020809     C     K02FGV01      klist                                                  FNFGV01L
069700020809     C                   kfld                    KNFV                           ..NR.FOGLIO
069800020809     C                   kfld                    BRVfgs
069900950110     C*
070000950110     C                   ENDSR
070100950110     C**************************************************************************
070200950110**         CMD1
070300021008ALCOBJ OBJ((FNBLPT0R *FILE *EXCL           M234567890))
070400911002**         CMD4
070500021008OVRDBF FILE(FNBLPT0R)                   MBR(M234567890)
070600911001**         CMD5
070700021008CHKOBJ OBJ(FNBLPT0R) OBJTYPE(*FILE)     MBR(M234567890)
