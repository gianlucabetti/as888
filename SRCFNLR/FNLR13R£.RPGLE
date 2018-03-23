000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950110     H* FNLR13R *----------------------------------------------------*
000300950109     H*        - RICEZIONE BOLLE TRANSITO                            *
000400000000     H*--------------------------------------------------------------*
000500950110     FFNBLPT0R  UF   E           K DISK    USROPN
000600020611     FFNBTP11L  UF A E           K DISK
000700020611     FFNBTT11L  UF A E           K DISK
000800990510     FFNANM02L  IF   E           K DISK
000900990510     F                                     INFDS(FNANM2)
001000080402     Ffnblt01l  IF   E           K DISK
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
014300911001     C* ALLOCO MEMBRI
014400911001     C                   EXSR      CAEX
014500911001     C   20              GOTO      FINE
014600911001     C*
014700950110     C                   READ      FNBLPT00                               31
014800911003     C*
014900911001     C     *IN31         DOWEQ     '0'
015000950109     C*
015100080402     C* VERIFICO SE c'e' gioà la bolla
015200020611     C     KBTP11        SETLL     FNBTP11L                               33
015300950109     C*
015400911011     C* 30 ON  - ERRORE CANCELLO E BASTA
015500080402     C   33              DELETE    FNBLPT00
015600911003     C* 33 OFF - CREO BOLLA
015700911003     C  N33              EXSR      CRTBOL
015800911001     C*
015900950110     C                   READ      FNBLPT00                               31
016000911001     C                   END
016100970508     C**
016200970508     C* SE HO APERTO IL PGM LO DEVO CHIUDERE
016300970508     C     WFC44         IFEQ      'S'
016400070223     C                   MOVEL     'C'           dls45flg
016500070223     C                   CALL      'FNLS45R'
016600070223     c                   parm                    fnls45ds
016700070223     c                   parm                    dsbrv
016800970508     C                   ENDIF
016900970508     C**
017000970508     C     WFC21         IFEQ      'S'
017100970508     C                   MOVEL     'C'           D21TLA
017200970508     C                   MOVEL     PARAM         KPJBU
017300970509     C                   CALL      'FNLR21R'
017400970508     C                   PARM                    KPJBA
017500970508     C                   ENDIF
017600900911     C*
017700900409     C     FINE          TAG
017800020809     C                   EVAL      *INLR = *ON
017900950109     C**************************************************************************
018000950109     C*    CARICAMENTO TABELLE
018100950109     C**************************************************************************
018200940111     C     CARTAB        BEGSR
018300970509     C*****
018400970509     C***  CARICO CATEGORIE
018500970509     C*****
018600970509     C                   MOVEL     '7N'          COD
018700020809     C                   Z-ADD     0             XB
018800970509     C     KTAB2         SETLL     TABEL
018900970509     C     KTAB2         READE     TABEL                                  31
019000970509     C*
019100970509     C     *IN31         DOWEQ     '0'
019200970509     C     TBLFLG        IFEQ      ' '
019300970509     C*
019400020809     C                   ADD       1             XB
019500020809     C                   MOVEL     TBLKEY        CAT(XB)
019600970509     C                   END
019700970509     C*
019800970509     C     KTAB2         READE     TABEL                                  31
019900970509     C                   END
020000940111     C*****
020100970508     C***  RICERCO FASE CATEGORIA ARRIVI
020200940111     C*****
020300940111     C                   MOVEL     '7N'          COD
020400940111     C                   MOVEL     *BLANKS       KEY
020500940111     C                   MOVEL     2             KEY
020600940111     C     KTAB          CHAIN     TABEL                              31
020700940111     C  N31              MOVEL     TBLUNI        DS7N
020800040322     c* CARICO £1
020900040322     C                   CLEAR                   DSUL06
021000040322     C                   MOVE      '£1'          D06COD
021100040322     C                   MOVEL     SIMFEL        D06KEY
021200040322     C                   MOVEL     'L'           D06TLA
021300040322     C                   MOVEL     DSUL06        KPJBU
021400040322     C*
021500040322     C                   CALL      'TRUL06R'
021600040322     C                   PARM                    KPJBA
021700040322     C                   MOVEL     KPJBU         DSUL06
021800040322     C                   MOVEA     LIN           L1
021900940111     C*
022000940111     C                   ENDSR
022100950109     C**************************************************************************
022200950109     C*  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI
022300950109     C**************************************************************************
022400911001     C     CAEX          BEGSR
022500950109     C*
022600020809     C                   MOVE      *BLANK        VAR11
022700020809     C                   MOVE      *BLANK        VAR12
022800911001     C                   MOVE      ')'           VAR11
022900021008     C                   MOVEL     MBRfil        VAR11
023000911001     C                   MOVE      '))'          VAR12
023100021008     C                   MOVEL     MBRfil        VAR12
023200021008     C                   Z-ADD     55            LUNG
023300911001     C*
023400911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
023500911001     C                   SETOFF                                       20
023600950110     C                   MOVE      VAR11         CMD5                            FNBLPT0R
023700911001     C                   MOVEL     *BLANKS       COMMAN
023800020809     C                   MOVEA     CMD5(1)       COMMAN
023900911001     C                   CALL      'QCMDEXC'                            20
024000911001     C                   PARM                    COMMAN
024100911001     C                   PARM                    LUNG
024200911002     C* 20 ON - ESCO
024300911002     C   20              GOTO      ENDCAE
024400911002     C* ALLOCO MEMBRO: SE NON RIESCO ESCO DA PROGRAMMA
024500911001     C                   SETOFF                                       20
024600911011     C                   MOVE      VAR12         CMD1
024700911001     C                   MOVEL     *BLANKS       COMMAN
024800911011     C                   MOVEA     CMD1(1)       COMMAN
024900911001     C                   CALL      'QCMDEXC'                            20
025000911001     C                   PARM                    COMMAN
025100911001     C                   PARM                    LUNG
025200911002     C*
025300911002     C   20              GOTO      ENDCAE
025400911001     C* OVRDBF
025500911002     C                   SETOFF                                       20
025600911011     C                   MOVE      VAR11         CMD4
025700911001     C                   MOVEL     *BLANKS       COMMAN
025800911011     C                   MOVEA     CMD4(1)       COMMAN
025900911002     C                   CALL      'QCMDEXC'                            20
026000911001     C                   PARM                    COMMAN
026100911001     C                   PARM                    LUNG
026200911002     C   20              GOTO      ENDCAE
026300911002     C*
026400950110     C                   OPEN      FNBLPT0R
026500911001     C*
026600911002     C     ENDCAE        ENDSR
026700950109     C**************************************************************************
026800950109     C*  . HO IN MEMORIA LA BOLLA RICEVUTA
026900950109     C*  . SCRIVO I COLLI DELLA BOLLA TRANSITO
027000950109     C*  . CANCELLO I COLLI DELLA BOLLA RICEVUTA
027100950109     C*  . SCRIVO LA BOLLA TRANSITO
027200950109     C*  . CANCELLO LA BOLLA RICEVUTA
027300950109     C*  . VERIFICO SE C'E' DISGUIDO
027400950109     C**************************************************************************
027500911002     C     CRTBOL        BEGSR
027600911003     C*
027700950109     C* MUOVO I CAMPI DALLA BOLLA RICEVUTA ALLA BOLLA TRANSITO
027800950110     C                   CLEAR                   FNBTP000
027900950109     C                   Z-ADD     BLPAAS        BTPAAS
028000950109     C                   Z-ADD     BLPLNP        BTPLNP
028100950109     C                   Z-ADD     BLPNRS        BTPNRS
028200950109     C                   Z-ADD     BLPNSP        BTPNSP
028300950109     C                   Z-ADD     BLPMGS        BTPMGS
028400950110     C                   MOVEL     BLPCBO        BTPCBO
028500950110     C                   MOVEL     BLPFNS        BTPFNS
028600950109     C                   Z-ADD     BLPLNA        BTPLNA
028700950110     C                   Z-ADD     BLPZNC        BTPZNC
028800950109     C                   MOVEL     BLPRSM        BTPRSM
028900950109     C                   MOVEL     BLPRSD        BTPRSD
029000950110     C                   MOVEL     BLPTSP        BTPTSP
029100950109     C                   Z-ADD     BLPNCL        BTPNCL
029200950110     C                   Z-ADD     BLPPKB        BTPPKB
029300950110     C                   MOVEL     BLPFVB        BTPFVB
029400950110     C                   Z-ADD     BLPVLB        BTPVLB
029500950109     C                   Z-ADD     BLPFLS        BTPFLS
029600950109     C                   Z-ADD     BLPNCD        BTPNCD
029700950109     C                   Z-ADD     BLPNCA        BTPNCA
029800950110     C                   MOVEL     BLPXCO        BTPXCO
029900950109     C                   Z-ADD     BLPDBR        BTPDBR
030000950109     C                   Z-ADD     BLPNFV        BTPNFV
030100950109     C                   Z-ADD     BLPFLE        BTPFLE
030200950109     C                   Z-ADD     BLPTFP        BTPTFP
030300950109     C                   Z-ADD     BLPFEA        BTPFEA
030400950109     C                   Z-ADD     BLPTFA        BTPTFA
030500020607     C                   Z-ADD     SIMFEL        BTPFLP
030600950109     C                   MOVEL     'R'           BTPFT1                         'R'=RICEZIONE
030700950109     C                   Z-ADD     DATEU         BTPDT1
030800930201     C*
030900080402     c*  I segnacolli li prendo dal dettaglio bolle partenza
031000080402     C     KBLP          SETLL     FNBLt01l
031100080402     C     KBLP          READE     FNBLt01l                               32
031200911002     C     *IN32         DOWEQ     '0'
031300921022     C                   EXSR      CTRBTT
031400911015     C*
031500950808     C                   WRITE     FNBTT000                             99
031600911015     C*
031700080402     C     KBLP          READE     FNBLT01l                               32
031800080402     C                   ENDDO
031900911002     C*
032000911015     C* SCRIVO FILE
032100950808     C                   WRITE     FNBTP000                             99
032200911003     C* DELETO
032300950110     C                   DELETE    FNBLPT00
032400940111     C*
032500921022     C* CONTROLLO EVENTUALE ANOMALIA DI DISGUIDO
032600950110     C     KBTT1         SETLL     FNBTT000
032700950110     C     KBTT1         READE     FNBTT000                               32
032800921022     C*
032900970508     C     *IN32         DOWEQ     *OFF
033000921022     C                   EXSR      CTRANM
033100950110     C     KBTT1         READE     FNBTT000                               32
033200970508     C                   ENDDO
033300911003     C*
033400911003     C                   ENDSR
033500950109     C**************************************************************************
033600950109     C*  RIEMPO I CAMPI DEL FILE DETTAGLIO BOLLE TRANSITO
033700950109     C**************************************************************************
033800920805     C     CTRBTT        BEGSR
033900950109     C*
034000950110     C                   CLEAR                   FNBTT000
034100950109     C                   Z-ADD     BLTAAS        BTTAAS
034200950109     C                   Z-ADD     BLTLNP        BTTLNP
034300950109     C                   Z-ADD     BLTNRS        BTTNRS
034400950109     C                   Z-ADD     BLTNSP        BTTNSP
034500950110     C                   Z-ADD     BLTNSC        BTTNSC
034600080402     C                   MOVEL     Bltlna        BTTLNA
034700080402     C                   MOVEL     Bltfls        BTTFLS
034800080402     C                   MOVEL     Bltvuc        BTTvuc
034900080402     C                   MOVEL     Bltfvc        BTTfvc
035000080402     C                   MOVEL     Bltpuc        BTTpuc
035100080402     C                   MOVEL     Bltfpc        BTTfpc
035200080402     c
035300950109     C                   MOVEL     BTPFLP        BTTFLP
035400950110     C                   MOVEL     'R'           BTTFTR                         'R'=RICEZIONE
035500950110     C                   Z-ADD     DATEU         BTTDTR                         DATA RICEZIONE
035600950109     C*
035700920805     C                   ENDSR
035800950109     C**************************************************************************
035900950109     C*  CONTROLLO SE ESISTEVA ANOMALIA
036000950109     C**************************************************************************
036100911015     C     CTRANM        BEGSR
036200970307     C                   CLEAR                   WDISG
036300970509     C                   CLEAR                   WSPU
036400970509     C                   CLEAR                   WCA21
036500970509     C                   CLEAR                   DSSPP
036600920805     C*
036700960520     C                   Z-ADD     200           COMCAA
036800040322     c     kanm1         setll     fnanm02l
036900040322     C     KANM1         reade     FNANM02L                               33
037000040322     c     *in33         doweq     *off
037100040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
037200040429     c**   anmfle        lookup    l1                                     33
037300040429    1C**   *IN33         IFEQ      *On
037400040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
037500040429     c*  se spara 2 livello deve rimanere l'anomalia200
037600040429     c                   if        anmfle=simfel
037700040429     C* CHIUDO ANOMALIA
037800020809     C                   MOVEL     'S'           WDISG
037900960415     C*
038000020809     C                   CLEAR                   FNLR33DS
038100960415     C                   Z-ADD     ANMNR2        D33NRR
038200960415     C                   MOVE      DATEU         D33DCH
038300960415     C                   MOVEL     §7NFS1        D33FSC
038400960415     C                   CALL      'FNLR33R'
038500020809     C                   PARM                    FNLR33DS
038600970307    1C                   ENDIF
038700040322     C     KANM1         reade     FNANM02L                               33
038800040322     c                   enddo
038900960520     C*
039000960520     C* CHAINO ANCHE L'ANOMALIA 55 CHE SE POSSO LA DELETO
039100960520     C                   Z-ADD     55            COMCAA
039200040429     c                   clear                   wtrov             1
039300040322     c     kanm1         setll     fnanm02l
039400040322     C     KANM1         reade     FNANM02L                               33
039500040322     c     *in33         doweq     *off
039600040429     c     wtrov         andeq     *blanks
039700040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
039800040429     c**   anmfle        lookup    l1                                     33
039900040429    1C**   *IN33         IFEQ      *OFF
040000040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
040100040429     c*  se spara 2 livello deve rimanere l'anomalia200
040200040429     c                   if        anmfle=simfel
040300040429     C* CHIUDO ANOMALIA
040400020809     C                   MOVEL     'S'           WDISG
040500040429     C                   MOVEL     'S'           Wtrov
040600020809     C                   CLEAR                   FNLR33DS
040700960520     C                   Z-ADD     ANMNR2        D33NRR
040800960520     C                   MOVE      DATEU         D33DCH
040900960520     C                   MOVEL     §7NFS1        D33FSC
041000960520     C                   MOVEL     'S'           D33MAC
041100960520     C                   CALL      'FNLR33R'
041200020809     C                   PARM                    FNLR33DS
041300040429     c                   else
041400040322     C     KANM1         reade     FNANM02L                               33
041500040429    1C                   ENDIF
041600040322     c                   enddo
041700040108     C*
041800040108     C* CHAINO ANCHE L'ANOMALIA 56 CHE SE POSSO LA DELETO
041900040108     C                   Z-ADD     56            COMCAA
042000040322     c                   clear                   wtrov
042100040322     c     kanm1         setll     fnanm02l
042200040322     C     KANM1         reade     FNANM02L                               33
042300040322     c     *in33         doweq     *off
042400040429     c     wtrov         andeq     *blanks
042500040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
042600040429     c**   anmfle        lookup    l1                                     33
042700040429    1C**   *IN33         IFEQ      *OFF
042800040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
042900040429     c*  se spara 2 livello deve rimanere l'anomalia200
043000040429     c                   if        anmfle=simfel
043100040429     C* CHIUDO ANOMALIA
043200040108     C                   MOVEL     'S'           WDISG
043300040429     C                   MOVEL     'S'           Wtrov
043400040108     C                   CLEAR                   FNLR33DS
043500040108     C                   Z-ADD     ANMNR2        D33NRR
043600040108     C                   MOVE      DATEU         D33DCH
043700040108     C                   MOVEL     §7NFS1        D33FSC
043800040108     C                   MOVEL     'S'           D33MAC
043900040108     C                   CALL      'FNLR33R'
044000040108     C                   PARM                    FNLR33DS
044100040429     c                   else
044200040322     C     KANM1         reade     FNANM02L                               33
044300040429    1C                   ENDIF
044400040322     c                   enddo
044500970307     C**
044600970307    1C     WDISG         IFEQ      'S'
044700970509     C*
044800970509     C* CERCO LE SPUNTE DEL COLLO
044900020809     C                   Z-ADD     1             XB
045000020809    2C     CAT(XB)       DOWNE     0
045100020809     C                   MOVEL     CAT(XB)       KNPG
045200970508     C**
045300070123     C     KBRV1         CHAIN     FNBRV09L                           33
045400970508     C**
045500970509     C* PRIMA DEVO AGGIORNARE TUTTE LE SPUNTE PARTENZA PER POI
045600970509     C*  RILASCIARE IL RECORD DI FNBTT
045700970509    3C     *IN33         DOWEQ     *OFF
045800970508     C* SPUNTA PARTENZA
045900970509    4C     BRVNPG        IFEQ      1                                            1=PARTENZE
046000970508     C**
046100970508     C* SE SPUNTA NON MIA RICHIAMO PGM PER AGGIORNARE LA DATA DI PART
046200970509    5C     BRVFLE        IFNE      SIMFEL
046300020809     C                   CLEAR                   FNLV53DS
046400970508     C                   MOVEL     'A'           D53TFO
046500991230     C                   Z-ADD     BRVNFV        D53NFV
046600970508     C                   MOVEL     BRVFGS        D53FGS
046700970508     C                   MOVEL     SIMFEL        D53FEL
046800970508     C                   CALL      'FNLV53R'
046900020809     C                   PARM                    FNLV53DS
047000970509     C*
047100970509    6C     D53ERR        IFEQ      ' '
047200970509     C     D53DFV        ANDGE     SPPDFV
047300970508     C                   CLEAR                   DSSPP
047400970508     C                   MOVEL     BRVLNP        SPPLNP
047500970508     C                   MOVEL     BRVLNA        SPPLNA
047600970508     C                   MOVEL     BRVNRS        SPPNRS
047700970508     C                   MOVEL     BRVNSC        SPPNSC
047800970508     C                   MOVEL     BRVZNC        SPPZNC
047900970508     C                   MOVEL     BRVNPS        SPPNPS
048000970508     C                   MOVEL     BRVNPG        SPPNPG
048100970508     C                   MOVEL     BRVFGS        SPPFGS
048200970508     C                   MOVEL     BRVNFV        SPPNFV
048300970508     C                   MOVEL     D53DFV        SPPDFV
048400970508     C                   MOVEL     BRVATR        SPPATR
048500970508     C                   MOVEL     BRVVUC        SPPVUC
048600970508     C                   MOVEL     BRVPUC        SPPPUC
048700970508     C                   MOVEL     BRVCAN        SPPCAN
048800970508     C*
048900970508     C* SPPFLP NON LO PASSO PERCHE' SERVE SOLO SE LA SPUNTA E' "A"
049000020610     C* 10/06/02: NON È PIÙ VERO ORA SPPFLP SERVE ANCHE QUANDO LA SPUNT
049100020610     C* E' BLANK SE SI TRATTA DI UNTA SPUNTA PER IL TRANSITO
049200020610     C     SPPATR        IFEQ      *BLANKS
049300020610     C                   Z-ADD     SIMFEL        SPPFLP
049400020610     C                   ENDIF
049500970508     C                   MOVEL     DSSPP         D21REC
049600970508     C                   MOVEL     ' '           D21TLA
049700020809     C                   MOVEL     'S'           WCA21
049800970509    6C                   ENDIF
049900970508     C**
050000970509   X5C                   ELSE
050100970508     C** SPUNTA DI SIMFEL
050200950110     C                   EXSR      AGGFGV
050300970508     C*
050400970509     C** SE NON SI TRATTA DI PISTOLA AUTOGENERATA AGGIORNO
050500101202    6C     BRVNPS        IFNE      98
050600101202     C     BRVNPS        ANDNE     96
050700970508     C                   EXSR      CHIAMO
050800970509    6C                   ENDIF
050900970509    5C                   ENDIF
051000970508     C**
051100970509   X4C                   ELSE
051200040322     c* Richiamo fnls44r solo se spunta dell'area di partenza del p.o.
051300040322     c* di transito (simfel)
051400040322     c* Infatti dando per scontato che i secondi livello in arrivo tota
051500040322     c* li o parziali non hanno transiti usiamo la £1 perchè di fatto
051600040322     c* solo p.o. con ter-arr = ter-part aggiornano le bolle transito
051700040322     c* (v. anche fnls44r)
051800040322     c     brvpes        lookup    l1                                     33
051900040322     c*
052000040322     C   33              EXSR      CHIAMO
052100040322     c*
052200970509    4C                   ENDIF
052300921022     C*
052400070123     C     KBRV1         READE     FNBRV09L                               33
052500970509    3C                   ENDDO
052600020809     C                   ADD       1             XB
052700970509    2C                   ENDDO
052800940328     C*
052900940328     C* AGGIORNO E RILASCIO RECORD
053000970509    2C     WSPU          IFEQ      ' '
053100970508     C                   UPDATE    FNBTT000
053200970509    2C                   ENDIF
053300940328     C*
053400970509    1C                   ENDIF
053500970509     C*
053600970509     C* COME ULTIMA COSA AGGIORNO LA DATA DI PARTENZA DEL FOGLIO
053700970509     C     WCA21         IFEQ      'S'
053800970819     C                   CLEAR                   D21NOS
053900970509     C                   MOVEL     PARAM         KPJBU
054000970509     C                   CALL      'FNLR21R'
054100970509     C                   PARM                    KPJBA
054200020809     C                   MOVEL     'S'           WFC21
054300970509    5C                   ENDIF
054400911015     C*
054500911015     C                   ENDSR
054600950109     C**************************************************************************
054700950109     C*  CHIAMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
054800950109     C**************************************************************************
054900921022     C     CHIAMO        BEGSR
055000940328     C*
055100940328     C* AGGIORNO E RILASCIO RECORD
055200970509     C     WSPU          IFEQ      ' '
055300950110     C                   UPDATE    FNBTT000
055400020809     C                   MOVEL     'S'           WSPU
055500970509     C                   ENDIF
055600921022     C*
055700070223     c                   clear                   fnls45ds
055800070223     c                   move      brvpes        dls45pes
055900070223     c                   movel     'S'           dls45ric
056000070223     c                   move      comtfp        dls45tfp
056100070223     c                   movel     'T'           dls45flg
056200070223     c                   call      'FNLS45R'
056300070223     c                   parm                    fnls45ds
056400070223     c                   parm                    dsbrv
056500950109     C*
056600020809     C                   MOVEL     'S'           WFC44
056700950110     C*
056800921022     C                   ENDSR
056900950109     C**************************************************************************
057000950109     C*    AGGIORNO FOGLIO VIAGGIO
057100950109     C**************************************************************************
057200940111     C     AGGFGV        BEGSR
057300950109     C*
057400940111     C                   CLEAR                   WSPE
057500940111     C*
057600940111     C* TROVATA SPUNTA PARTENZA
057700020809     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
057800020809     C     K02FGV01      chain     FNFGV01L                           34
057900940111     C*
058000940328    1C     *IN34         IFEQ      *OFF
058100940328     C* SOLO SE E' CHIUSO
058200940328    2C     FGVFCF        IFNE      *BLANKS
058300950110     C                   Z-ADD     FGVNFV        BTTNFV
058400950110     C                   Z-ADD     FGVDFV        BTTDUT                         DATA FOGLIO
058500950110     C                   Z-ADD     BRVNPS        BTTPUT
058600940328    2C                   ENDIF
058700940328   X1C                   ELSE
058800940328     C* SE NON TROVATO FOGLIO CME SE FOSSE CHIUSO
058900950110     C                   Z-ADD     DATEU         BTTDUT
059000940328     C                   MOVEL     'S'           FGVFCF
059100940328    1C                   ENDIF
059200940111     C*
059300940328     C* FOGLIO CHIUSO
059400940328    1C     FGVFCF        IFNE      *BLANKS
059500940328     C*
059600950110     C     KBTP1         CHAIN     FNBTP000                           35
059700950110    2C     BTPDUT        IFEQ      *ZERO
059800950110     C  N35              Z-ADD     BTTDUT        BTPDUT                         DATA USCITA TRANSITO
059900940111     C* SOMMO LA SPEDIZIONE DI UN COLLO "EX MANCA BOLLA"
060000940111     C                   Z-ADD     1             WSPE
060100940328    2C                   ENDIF
060200950206     C*
060300950206     C  N35              UPDATE    FNBTP000
060400940111     C*
060500940111     C* CALCOLO PESO E VOLUME UNITARI DI UN COLLO "EX MANCA BOLLA" (IL
060600940111     C*   COLLO L'HO GIA' SOMMATO AL MOMENTO DELLA CHIUSURA DEL FOGLIO)
060700940111     C                   EXSR      TOTFVD
060800940111     C*
060900950110     C  N34              UPDATE    FNFGV000
061000940328    1C                   ENDIF
061100940111     C*
061200940111     C                   ENDSR
061300950109     C**************************************************************************
061400950109     C*    CALCOLO PESO/VOLUME/SPEDIZIONI UNITARI
061500950110     C* IL FILE FNFVD SERVIRA' PER IL PROSPETTO NOLI (FNLS59R)
061600950109     C**************************************************************************
061700940111     C     TOTFVD        BEGSR
061800940111     C*
061900980213     C* IN FNFVD POSSO AVERE TRE RECORD PER LO STESSO FOGLIO:
062000950110     C* TRC:   "1" = DELL'AREA            "2" = TRANSITO
062100980213     C*                                   "3" = TRANSITO PARTENZA SDI
062200980216     C* DEVO CONTROLLARE LA LINEA DI PARTENZA DELLA BOLLA
062300980216     C* PER CONOSCERE LA DITTA ORIGINARIA SDI O BARTOLINI
062400980216     C                   MOVEL     '2'           KTRC                           '2'=TRANSITO PART.BA
062500980213     C* AGGANCIO L'ORGANIGRAMMA PER VERIFICARE SE E' SDI O BARTOLINI
062600980216     C     BTTLNP        CHAIN     AZORG01L                           35
062700980213     C  N35ORGDIT        IFEQ      'SDI'
062800980213     C                   MOVEL     '3'           KTRC                           '3'=TRANSITO PART.SD
062900980213     C                   END
063000980213     C*
063100950110     C                   Z-ADD     SIMFEL        KLNP                           LNP
063200950110     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
063300950110     C                   Z-ADD     BTPLNA        KLNA                           LNA
063400950110     C     KFVD1         CHAIN     FNFVD01L                           35
063500940111     C*
063600940111     C     *IN35         IFEQ      *OFF
063700940111     C*****
063800940111     C***  P E S O
063900940111     C*****
064000950110     C     BTPPKB        DIV(H)    BTPNCL        PKGU
064100940111     C*
064200950110     C* ADDIZIONO IN FNFVD
064300950110     C     BTPPKB        IFGT      3500
064400950110     C                   ADD       PKGU          FVDKFA                         *OLTRE 35 Q.
064500940111     C                   ELSE
064600950110     C                   ADD       PKGU          FVDKOA                         *FINO  35 Q.
064700940111     C                   ENDIF
064800940111     C*
064900940111     C*****
065000940111     C***  V O L U M E
065100940111     C*****
065200940111     C* SE IL VOLUME DEL COLLO NON E' INDICATO IN FLBTT, LO CALCOLO
065300950110     C*   PRENDENDOLO DA FNBTP
065400950110     C     BTTVUC        IFNE      0
065500950110     C                   Z-ADD(H)  BTTVUC        VOLU
065600940111     C*
065700940111     C                   ELSE
065800940111     C*
065900950110     C     BTPVLB        DIV(H)    BTPNCL        VOLU
066000940111     C                   ENDIF
066100940111     C*
066200950110     C* VEDO IN QUALE POSIZIONE (DEL RECORD) STA IL TIPO SERVIZIO
066300020809     C                   Z-ADD     1             WA
066400950110     C     BTPTSP        LOOKUP    TSP(WA)                                15     ON=TROVATO
066500950110     C  N15              ADD       VOLU          VOL(WA)
066600940111     C*
066700940111     C*****
066800940111     C***  S P E D I Z I O N I
066900940111     C*****
067000940111     C     WSPE          IFGT      0
067100950110     C                   ADD       WSPE          FVDSNA
067200940111     C                   ENDIF
067300940111     C*
067400940111     C* ABBLENCO FLAG E DATA TRASMISSIONE PER INVIO IN SEDE
067500940111     C                   MOVEL     ' '           FVDFTR
067600940111     C                   Z-ADD     0             FVDDTR
067700940111     C*
067800950110     C                   UPDATE    FNFVD000
067900940111     C                   ENDIF
068000940111     C*
068100940111     C                   ENDSR
068200950109     C**************************************************************************
068300950110     C*    R O U T I N E    I N I Z I A L E
068400950110     C**************************************************************************
068500950110     C     *INZSR        BEGSR
068600950110     C*
068700950110     C     KTAB          KLIST
068800950110     C                   KFLD                    CODUT
068900950110     C                   KFLD                    COD
069000950110     C                   KFLD                    KEY
069100950110     C     KTAB2         KLIST
069200950110     C                   KFLD                    CODUT
069300950110     C                   KFLD                    COD
069400970508     C     KBLP          KLIST
069500950110     C                   KFLD                    BLPAAS
069600950110     C                   KFLD                    BLPLNP
069700950110     C                   KFLD                    BLPNRS
069800950110     C                   KFLD                    BLPNSP
069900020611     C     KBTP11        KLIST
070000020611     C                   KFLD                    SIMFEL
070100020611     C                   KFLD                    BLPAAS
070200020611     C                   KFLD                    BLPLNP
070300020611     C                   KFLD                    BLPNRS
070400020611     C                   KFLD                    BLPNSP
070500020611     C     KBTP1         KLIST                                                  FNBTP11L
070600020611     C                   KFLD                    BTTFLP
070700950110     C                   KFLD                    BTTAAS
070800950110     C                   KFLD                    BTTLNP
070900950110     C                   KFLD                    BTTNRS
071000950110     C                   KFLD                    BTTNSP
071100020611     C     KBTT1         KLIST                                                  FNBTT11L
071200020611     C                   KFLD                    BTPFLP
071300950110     C                   KFLD                    BTPAAS
071400950110     C                   KFLD                    BTPLNP
071500950110     C                   KFLD                    BTPNRS
071600950110     C                   KFLD                    BTPNSP
071700040108     C     KANM1         KLIST                                                  FNANM02l
071800970508     C                   KFLD                    BTTFLS
071900020611     C                   KFLD                    BTTLNA
072000950110     C                   KFLD                    BTTNRS
072100950110     C                   KFLD                    BTTNSC
072200950110     C                   KFLD                    COMCAA            3 0
072300070123     C     KBRV1         KLIST                                                  FNBRV09L
072400970509     C                   KFLD                    KNPG
072500970509     C                   KFLD                    BTTFLS                         ..LNP
072600950110     C                   KFLD                    BTTLNA                         ..LNA
072700950110     C                   KFLD                    BTTNRS                         ..SERIE
072800950110     C                   KFLD                    BTTNSC                         ..SEGNACOLLO
072900950110     C     KFVD1         KLIST                                                  FNFVD01L
073000950110     C                   KFLD                    KLNP                           ..LNP
073100950110     C                   KFLD                    KNFV                           ..NR.FOGLIO
073200950110     C                   KFLD                    KLNA                           ..LNA
073300950110     C                   KFLD                    KTRC                           ..TIPO RECORD
073400020809     C     K02FGV01      klist                                                  FNFGV01L
073500020809     C                   kfld                    KNFV                           ..NR.FOGLIO
073600020809     C                   kfld                    BRVfgs
073700950110     C*
073800950110     C                   ENDSR
073900950110     C**************************************************************************
074000950110**         CMD1
074100021008ALCOBJ OBJ((FNBLPT0R *FILE *EXCL           M234567890))
074200911002**         CMD4
074300021008OVRDBF FILE(FNBLPT0R)                   MBR(M234567890)
074400911001**         CMD5
074500021008CHKOBJ OBJ(FNBLPT0R) OBJTYPE(*FILE)     MBR(M234567890)
