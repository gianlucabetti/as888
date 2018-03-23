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
001600160513     ffnblp01l  if   e           k disk    prefix(w_)
001700950109     D*
001800021008     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001900021008     D CMD4            S             55    DIM(1) CTDATA PERRCD(1)
002000021008     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
002100970509     D CAT             S              1  0 DIM(20)                              CATEGORIE FOGLI
002200040322     D L1              S              3  0 DIM(30)
002300950109     D*
002400930201     D KPJBA         E DS
002500911001     D  LEGFIL               483    492
002600911001     D  MBRFIL               493    502
002700020809     D*
002800020809     D UT§DSE0F      E DS
002900020809     D CNCR80        E DS
003000020809     D FNLR33DS      E DS
003100020809     D* CALL A PGM DI REPERIMENTO DATI FOGLI
003200020809     D FNLV53DS      E DS
003300040322     D* DS PER TRUL06R - CARICAMENTO £X
003400040322     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003500040322     D  LIN                    1     90  0
003600040322     D                                     DIM(30)
003700070223     D FNLS45DS      E DS
003800070223     D DSbrv         E DS                  EXTNAME(fnbrv00f)
003900020809     D*
004000021015     D DS7N          E DS
004100020809     D DSSPP         E DS                  EXTNAME(FISPP00R)
004200020809     D*
004300970508     D PARAM           DS
004400970508     D  D21TLA                 1      1
004500991231     D  D21REC                 2     68
004600991231     D  D21ERR                69     69
004700991231     D  D21NOS                70     70
004800970819     D* NON SERVE PERCHE' QUI SIUAMO NELLE BOLLE TRANSITO MA SERVE
004900970819     D*  SOLO PER LE BOLLE ARRIVI PER FAR LANCIARE PGM FNLR27R
005000991231     D  D21ABI                71     71
005100950110     D WLBDAT          DS
005200950109     D  G02DAT                 1      8  0
005300950109     D  G02INV                 9     16  0
005400950109     D  G02ERR                17     17
005500950109     D  G02TGI                18     22  0
005600950110     D                 DS
005700950110     D* SCHIERA PER I TIPO SERVIZIO PRESENTI SUL RECORD DI FNFVD
005800950110     D  TSP                    1      6
005900950110     D                                     DIM(6)                               TIPO SERVIZIO
006000950110     D  FVDTS1                 1      1
006100950110     D  FVDTS2                 2      2
006200950110     D  FVDTS3                 3      3
006300950110     D  FVDTS4                 4      4
006400950110     D  FVDTS5                 5      5
006500950110     D  FVDTS6                 6      6
006600950110     D                 DS
006700950110     D* SCHIERA PER I VOLUMI PER TSP PRESENTI SUL RECORD DI FNFVD
006800950110     D  VOL                    1     54  4
006900950110     D                                     DIM(6)                               VOLUME PER TSP
007000950110     D  FVDVL1                 1      9  4
007100950110     D  FVDVL2                10     18  4
007200950110     D  FVDVL3                19     27  4
007300950110     D  FVDVL4                28     36  4
007400950110     D  FVDVL5                37     45  4
007500950110     D  FVDVL6                46     54  4
007600020809     D*
007700990510     D FNANM2          DS
007800960412     D  ANMNR2               397    400B 0
007900020809     D*
008000020809     D* DEFINIZIONE DI ALCUNI CAMPI
008100021015     D COD             S                   LIKE(TBLCOD)
008200020809     D KEY             S                   LIKE(TBLKEY)
008300020809     D PKGU            S                   LIKE(FVDKFA)
008400020809     D VOLU            S                   LIKE(FVDVL1)
008500020809     D WSPE            S                   LIKE(FVDSNA)
008600020809     D KNPG            S                   LIKE(BRVNPG)                         CATEGORIA FOGLIO
008700020809     D KLNP            S                   LIKE(BRVLNP)                                      LNP
008800020809     D KLNA            S                   LIKE(BRVLNA)                                      LNA
008900020809     D KNFV            S                   LIKE(FGVNFV)                         NR.FOGLIO
009000020809     D KTRC            S                   LIKE(FVDTRC)                         TIPO RECORD
009100020809     D COMNCA          S              8  0 INZ
009200020809     D COMNCD          S              8  0 INZ
009300020809     D WTIME           S             14  0 INZ                                  ORA & DATA
009400020809     D WDATE           S              8  0 INZ                                  GG/MM/AAAA
009500020809     D DATEU           S              8  0 INZ                                  AAAA/MM/GG
009600020809     D LUNG            S             15  5 INZ(80)
009700020809     D COMMAN          S             80    INZ
009800020809     D XB              S              2  0 INZ
009900020809     D WA              S              3  0 INZ
010000020809     D WDISG           S              1    INZ
010100021008     D VAR11           S             11    INZ
010200021008     D VAR12           S             12    INZ
010300020809     D WCA21           S              1    INZ
010400020809     D WFC21           S              1    INZ
010500020809     D WFC44           S              1    INZ
010600020809     D WSPU            S              1    INZ
010700940111     C*****************************************************************
010800911003     C*  RIEPILOGO INDICATORI
010900940111     C*****************************************************************
011000930201     C* 01/02 - INDICATORI DI TIPO RECROD
011100940111     C* 15    - LOKUP
011200921022     C* 20    - NON ALLOCATI DEGLI OGGETTI ESCO DA PROGRAMMA
011300950110     C* 31    - LETTURA FNBLPT00
011400940111     C* 32/35 - COMODO
011500911003     C*****************************************************************
011600000000     C     *ENTRY        PLIST
011700000000     C                   PARM                    KPJBA
011800950109     C*
011900940111     C                   CALL      'X§PARUT'
012000020809     C                   PARM                    UT§DSE0F
012100940111     C                   MOVEL     REC80         CNCR80
012200021015     c*
012300021016     C                   Z-ADD     SIMFEL        COMTFP            3 0
012400940111     C*---------------------------------------------------------------*
012500910322     C*
012600911011     C* GIRO DATA DEL GIORNO
012700020809     C                   TIME                    WTIME                          ORA & DATA
012800020809     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
012900950109     C                   Z-ADD     WDATE         G02DAT
013000950109     C                   MOVEL     *BLANKS       G02ERR
013100950109     C                   CALL      'XSRDA8'
013200950109     C                   PARM                    WLBDAT
013300020809     C                   Z-ADD     G02INV        DATEU                          AAAA/MM/GG
013400911011     C*
013500911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
013600911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
013700911001     C     MBRFIL        IFEQ      *BLANK
013800021008     C                   MOVEL     '*FIRST'      MBRFIL
013900911001     C                   END
014000921022     C*
014100940111     C* CARICAMENTO TABELLE
014200940111     C                   EXSR      CARTAB
014300911001     C*
014400911001     C* ALLOCO MEMBRI
014500911001     C                   EXSR      CAEX
014600911001     C   20              GOTO      FINE
014700911001     C*
014800950110     C                   READ      FNBLPT00                               31
014900911003     C*
015000911001     C     *IN31         DOWEQ     '0'
015100950109     C*
015200080402     C* VERIFICO SE c'e' gioà la bolla
015300020611     C     KBTP11        SETLL     FNBTP11L                               33
015400950109     C*
015500911011     C* 30 ON  - ERRORE CANCELLO E BASTA
015600080402     C   33              DELETE    FNBLPT00
015700911003     C* 33 OFF - CREO BOLLA
015800911003     C  N33              EXSR      CRTBOL
015900911001     C*
016000950110     C                   READ      FNBLPT00                               31
016100911001     C                   END
016200970508     C**
016300970508     C* SE HO APERTO IL PGM LO DEVO CHIUDERE
016400970508     C     WFC44         IFEQ      'S'
016500070223     C                   MOVEL     'C'           dls45flg
016600070223     C                   CALL      'FNLS45R'
016700070223     c                   parm                    fnls45ds
016800070223     c                   parm                    dsbrv
016900970508     C                   ENDIF
017000970508     C**
017100970508     C     WFC21         IFEQ      'S'
017200970508     C                   MOVEL     'C'           D21TLA
017300970508     C                   MOVEL     PARAM         KPJBU
017400970509     C                   CALL      'FNLR21R'
017500970508     C                   PARM                    KPJBA
017600970508     C                   ENDIF
017700900911     C*
017800900409     C     FINE          TAG
017900020809     C                   EVAL      *INLR = *ON
018000950109     C**************************************************************************
018100950109     C*    CARICAMENTO TABELLE
018200950109     C**************************************************************************
018300940111     C     CARTAB        BEGSR
018400970509     C*****
018500970509     C***  CARICO CATEGORIE
018600970509     C*****
018700970509     C                   MOVEL     '7N'          COD
018800020809     C                   Z-ADD     0             XB
018900970509     C     KTAB2         SETLL     TABEL
019000970509     C     KTAB2         READE     TABEL                                  31
019100970509     C*
019200970509     C     *IN31         DOWEQ     '0'
019300970509     C     TBLFLG        IFEQ      ' '
019400970509     C*
019500020809     C                   ADD       1             XB
019600020809     C                   MOVEL     TBLKEY        CAT(XB)
019700970509     C                   END
019800970509     C*
019900970509     C     KTAB2         READE     TABEL                                  31
020000970509     C                   END
020100940111     C*****
020200970508     C***  RICERCO FASE CATEGORIA ARRIVI
020300940111     C*****
020400940111     C                   MOVEL     '7N'          COD
020500940111     C                   MOVEL     *BLANKS       KEY
020600940111     C                   MOVEL     2             KEY
020700940111     C     KTAB          CHAIN     TABEL                              31
020800940111     C  N31              MOVEL     TBLUNI        DS7N
020900040322     c* CARICO £1
021000040322     C                   CLEAR                   DSUL06
021100040322     C                   MOVE      '£1'          D06COD
021200040322     C                   MOVEL     SIMFEL        D06KEY
021300040322     C                   MOVEL     'L'           D06TLA
021400040322     C                   MOVEL     DSUL06        KPJBU
021500040322     C*
021600040322     C                   CALL      'TRUL06R'
021700040322     C                   PARM                    KPJBA
021800040322     C                   MOVEL     KPJBU         DSUL06
021900040322     C                   MOVEA     LIN           L1
022000940111     C*
022100940111     C                   ENDSR
022200950109     C**************************************************************************
022300950109     C*  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI
022400950109     C**************************************************************************
022500911001     C     CAEX          BEGSR
022600950109     C*
022700020809     C                   MOVE      *BLANK        VAR11
022800020809     C                   MOVE      *BLANK        VAR12
022900911001     C                   MOVE      ')'           VAR11
023000021008     C                   MOVEL     MBRfil        VAR11
023100911001     C                   MOVE      '))'          VAR12
023200021008     C                   MOVEL     MBRfil        VAR12
023300021008     C                   Z-ADD     55            LUNG
023400911001     C*
023500911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
023600911001     C                   SETOFF                                       20
023700950110     C                   MOVE      VAR11         CMD5                            FNBLPT0R
023800911001     C                   MOVEL     *BLANKS       COMMAN
023900020809     C                   MOVEA     CMD5(1)       COMMAN
024000911001     C                   CALL      'QCMDEXC'                            20
024100911001     C                   PARM                    COMMAN
024200911001     C                   PARM                    LUNG
024300911002     C* 20 ON - ESCO
024400911002     C   20              GOTO      ENDCAE
024500911002     C* ALLOCO MEMBRO: SE NON RIESCO ESCO DA PROGRAMMA
024600911001     C                   SETOFF                                       20
024700911011     C                   MOVE      VAR12         CMD1
024800911001     C                   MOVEL     *BLANKS       COMMAN
024900911011     C                   MOVEA     CMD1(1)       COMMAN
025000911001     C                   CALL      'QCMDEXC'                            20
025100911001     C                   PARM                    COMMAN
025200911001     C                   PARM                    LUNG
025300911002     C*
025400911002     C   20              GOTO      ENDCAE
025500911001     C* OVRDBF
025600911002     C                   SETOFF                                       20
025700911011     C                   MOVE      VAR11         CMD4
025800911001     C                   MOVEL     *BLANKS       COMMAN
025900911011     C                   MOVEA     CMD4(1)       COMMAN
026000911002     C                   CALL      'QCMDEXC'                            20
026100911001     C                   PARM                    COMMAN
026200911001     C                   PARM                    LUNG
026300911002     C   20              GOTO      ENDCAE
026400911002     C*
026500950110     C                   OPEN      FNBLPT0R
026600911001     C*
026700911002     C     ENDCAE        ENDSR
026800950109     C**************************************************************************
026900950109     C*  . HO IN MEMORIA LA BOLLA RICEVUTA
027000950109     C*  . SCRIVO I COLLI DELLA BOLLA TRANSITO
027100950109     C*  . CANCELLO I COLLI DELLA BOLLA RICEVUTA
027200950109     C*  . SCRIVO LA BOLLA TRANSITO
027300950109     C*  . CANCELLO LA BOLLA RICEVUTA
027400950109     C*  . VERIFICO SE C'E' DISGUIDO
027500950109     C**************************************************************************
027600911002     C     CRTBOL        BEGSR
027700911003     C*
027800950109     C* MUOVO I CAMPI DALLA BOLLA RICEVUTA ALLA BOLLA TRANSITO
027900950110     C                   CLEAR                   FNBTP000
028000950109     C                   Z-ADD     BLPAAS        BTPAAS
028100950109     C                   Z-ADD     BLPLNP        BTPLNP
028200950109     C                   Z-ADD     BLPNRS        BTPNRS
028300950109     C                   Z-ADD     BLPNSP        BTPNSP
028400950109     C                   Z-ADD     BLPMGS        BTPMGS
028500950110     C                   MOVEL     BLPCBO        BTPCBO
028600950110     C                   MOVEL     BLPFNS        BTPFNS
028700950109     C                   Z-ADD     BLPLNA        BTPLNA
028800950110     C                   Z-ADD     BLPZNC        BTPZNC
028900160513     c     kblp          chain     fnblp01l
029000160513     c                   if        %found(fnblp01l)
029100160513     c                   movel     w_blpznc      BTPznc
029200160513     c                   endif
029300950109     C                   MOVEL     BLPRSM        BTPRSM
029400950109     C                   MOVEL     BLPRSD        BTPRSD
029500950110     C                   MOVEL     BLPTSP        BTPTSP
029600950109     C                   Z-ADD     BLPNCL        BTPNCL
029700950110     C                   Z-ADD     BLPPKB        BTPPKB
029800950110     C                   MOVEL     BLPFVB        BTPFVB
029900950110     C                   Z-ADD     BLPVLB        BTPVLB
030000950109     C                   Z-ADD     BLPFLS        BTPFLS
030100950109     C                   Z-ADD     BLPNCD        BTPNCD
030200950109     C                   Z-ADD     BLPNCA        BTPNCA
030300950110     C                   MOVEL     BLPXCO        BTPXCO
030400950109     C                   Z-ADD     BLPDBR        BTPDBR
030500950109     C                   Z-ADD     BLPNFV        BTPNFV
030600950109     C                   Z-ADD     BLPFLE        BTPFLE
030700950109     C                   Z-ADD     BLPTFP        BTPTFP
030800950109     C                   Z-ADD     BLPFEA        BTPFEA
030900950109     C                   Z-ADD     BLPTFA        BTPTFA
031000020607     C                   Z-ADD     SIMFEL        BTPFLP
031100950109     C                   MOVEL     'R'           BTPFT1                         'R'=RICEZIONE
031200950109     C                   Z-ADD     DATEU         BTPDT1
031300930201     C*
031400080402     c*  I segnacolli li prendo dal dettaglio bolle partenza
031500080402     C     KBLP          SETLL     FNBLt01l
031600080402     C     KBLP          READE     FNBLt01l                               32
031700911002     C     *IN32         DOWEQ     '0'
031800921022     C                   EXSR      CTRBTT
031900911015     C*
032000950808     C                   WRITE     FNBTT000                             99
032100911015     C*
032200080402     C     KBLP          READE     FNBLT01l                               32
032300080402     C                   ENDDO
032400911002     C*
032500911015     C* SCRIVO FILE
032600950808     C                   WRITE     FNBTP000                             99
032700911003     C* DELETO
032800950110     C                   DELETE    FNBLPT00
032900940111     C*
033000921022     C* CONTROLLO EVENTUALE ANOMALIA DI DISGUIDO
033100950110     C     KBTT1         SETLL     FNBTT000
033200950110     C     KBTT1         READE     FNBTT000                               32
033300921022     C*
033400970508     C     *IN32         DOWEQ     *OFF
033500921022     C                   EXSR      CTRANM
033600950110     C     KBTT1         READE     FNBTT000                               32
033700970508     C                   ENDDO
033800911003     C*
033900911003     C                   ENDSR
034000950109     C**************************************************************************
034100950109     C*  RIEMPO I CAMPI DEL FILE DETTAGLIO BOLLE TRANSITO
034200950109     C**************************************************************************
034300920805     C     CTRBTT        BEGSR
034400950109     C*
034500950110     C                   CLEAR                   FNBTT000
034600950109     C                   Z-ADD     BLTAAS        BTTAAS
034700950109     C                   Z-ADD     BLTLNP        BTTLNP
034800950109     C                   Z-ADD     BLTNRS        BTTNRS
034900950109     C                   Z-ADD     BLTNSP        BTTNSP
035000950110     C                   Z-ADD     BLTNSC        BTTNSC
035100080402     C                   MOVEL     Bltlna        BTTLNA
035200080402     C                   MOVEL     Bltfls        BTTFLS
035300080402     C                   MOVEL     Bltvuc        BTTvuc
035400080402     C                   MOVEL     Bltfvc        BTTfvc
035500080402     C                   MOVEL     Bltpuc        BTTpuc
035600080402     C                   MOVEL     Bltfpc        BTTfpc
035700080402     c
035800950109     C                   MOVEL     BTPFLP        BTTFLP
035900950110     C                   MOVEL     'R'           BTTFTR                         'R'=RICEZIONE
036000950110     C                   Z-ADD     DATEU         BTTDTR                         DATA RICEZIONE
036100950109     C*
036200920805     C                   ENDSR
036300950109     C**************************************************************************
036400950109     C*  CONTROLLO SE ESISTEVA ANOMALIA
036500950109     C**************************************************************************
036600911015     C     CTRANM        BEGSR
036700970307     C                   CLEAR                   WDISG
036800970509     C                   CLEAR                   WSPU
036900970509     C                   CLEAR                   WCA21
037000970509     C                   CLEAR                   DSSPP
037100920805     C*
037200960520     C                   Z-ADD     200           COMCAA
037300040322     c     kanm1         setll     fnanm02l
037400040322     C     KANM1         reade     FNANM02L                               33
037500040322     c     *in33         doweq     *off
037600040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
037700040429     c**   anmfle        lookup    l1                                     33
037800040429    1C**   *IN33         IFEQ      *On
037900040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
038000040429     c*  se spara 2 livello deve rimanere l'anomalia200
038100040429     c                   if        anmfle=simfel
038200040429     C* CHIUDO ANOMALIA
038300020809     C                   MOVEL     'S'           WDISG
038400960415     C*
038500020809     C                   CLEAR                   FNLR33DS
038600960415     C                   Z-ADD     ANMNR2        D33NRR
038700960415     C                   MOVE      DATEU         D33DCH
038800960415     C                   MOVEL     §7NFS1        D33FSC
038900960415     C                   CALL      'FNLR33R'
039000020809     C                   PARM                    FNLR33DS
039100970307    1C                   ENDIF
039200040322     C     KANM1         reade     FNANM02L                               33
039300040322     c                   enddo
039400960520     C*
039500960520     C* CHAINO ANCHE L'ANOMALIA 55 CHE SE POSSO LA DELETO
039600960520     C                   Z-ADD     55            COMCAA
039700040429     c                   clear                   wtrov             1
039800040322     c     kanm1         setll     fnanm02l
039900040322     C     KANM1         reade     FNANM02L                               33
040000040322     c     *in33         doweq     *off
040100040429     c     wtrov         andeq     *blanks
040200040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
040300040429     c**   anmfle        lookup    l1                                     33
040400040429    1C**   *IN33         IFEQ      *OFF
040500040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
040600040429     c*  se spara 2 livello deve rimanere l'anomalia200
040700040429     c                   if        anmfle=simfel
040800040429     C* CHIUDO ANOMALIA
040900020809     C                   MOVEL     'S'           WDISG
041000040429     C                   MOVEL     'S'           Wtrov
041100020809     C                   CLEAR                   FNLR33DS
041200960520     C                   Z-ADD     ANMNR2        D33NRR
041300960520     C                   MOVE      DATEU         D33DCH
041400960520     C                   MOVEL     §7NFS1        D33FSC
041500960520     C                   MOVEL     'S'           D33MAC
041600960520     C                   CALL      'FNLR33R'
041700020809     C                   PARM                    FNLR33DS
041800040429     c                   else
041900040322     C     KANM1         reade     FNANM02L                               33
042000040429    1C                   ENDIF
042100040322     c                   enddo
042200040108     C*
042300040108     C* CHAINO ANCHE L'ANOMALIA 56 CHE SE POSSO LA DELETO
042400040108     C                   Z-ADD     56            COMCAA
042500040322     c                   clear                   wtrov
042600040322     c     kanm1         setll     fnanm02l
042700040322     C     KANM1         reade     FNANM02L                               33
042800040322     c     *in33         doweq     *off
042900040429     c     wtrov         andeq     *blanks
043000040322     c* L'anomalia è da chiudere solo se creata da p.o. della £1
043100040429     c**   anmfle        lookup    l1                                     33
043200040429    1C**   *IN33         IFEQ      *OFF
043300040429     c* Adesso l'anomalia si chiude solo se creata da anmfle=simfel
043400040429     c*  se spara 2 livello deve rimanere l'anomalia200
043500040429     c                   if        anmfle=simfel
043600040429     C* CHIUDO ANOMALIA
043700040108     C                   MOVEL     'S'           WDISG
043800040429     C                   MOVEL     'S'           Wtrov
043900040108     C                   CLEAR                   FNLR33DS
044000040108     C                   Z-ADD     ANMNR2        D33NRR
044100040108     C                   MOVE      DATEU         D33DCH
044200040108     C                   MOVEL     §7NFS1        D33FSC
044300040108     C                   MOVEL     'S'           D33MAC
044400040108     C                   CALL      'FNLR33R'
044500040108     C                   PARM                    FNLR33DS
044600040429     c                   else
044700040322     C     KANM1         reade     FNANM02L                               33
044800040429    1C                   ENDIF
044900040322     c                   enddo
045000970307     C**
045100970307    1C     WDISG         IFEQ      'S'
045200970509     C*
045300970509     C* CERCO LE SPUNTE DEL COLLO
045400020809     C                   Z-ADD     1             XB
045500020809    2C     CAT(XB)       DOWNE     0
045600020809     C                   MOVEL     CAT(XB)       KNPG
045700970508     C**
045800070123     C     KBRV1         CHAIN     FNBRV09L                           33
045900970508     C**
046000970509     C* PRIMA DEVO AGGIORNARE TUTTE LE SPUNTE PARTENZA PER POI
046100970509     C*  RILASCIARE IL RECORD DI FNBTT
046200970509    3C     *IN33         DOWEQ     *OFF
046300970508     C* SPUNTA PARTENZA
046400970509    4C     BRVNPG        IFEQ      1                                            1=PARTENZE
046500970508     C**
046600970508     C* SE SPUNTA NON MIA RICHIAMO PGM PER AGGIORNARE LA DATA DI PART
046700970509    5C     BRVFLE        IFNE      SIMFEL
046800020809     C                   CLEAR                   FNLV53DS
046900970508     C                   MOVEL     'A'           D53TFO
047000991230     C                   Z-ADD     BRVNFV        D53NFV
047100970508     C                   MOVEL     BRVFGS        D53FGS
047200970508     C                   MOVEL     SIMFEL        D53FEL
047300970508     C                   CALL      'FNLV53R'
047400020809     C                   PARM                    FNLV53DS
047500970509     C*
047600970509    6C     D53ERR        IFEQ      ' '
047700970509     C     D53DFV        ANDGE     SPPDFV
047800970508     C                   CLEAR                   DSSPP
047900970508     C                   MOVEL     BRVLNP        SPPLNP
048000970508     C                   MOVEL     BRVLNA        SPPLNA
048100970508     C                   MOVEL     BRVNRS        SPPNRS
048200970508     C                   MOVEL     BRVNSC        SPPNSC
048300970508     C                   MOVEL     BRVZNC        SPPZNC
048400970508     C                   MOVEL     BRVNPS        SPPNPS
048500970508     C                   MOVEL     BRVNPG        SPPNPG
048600970508     C                   MOVEL     BRVFGS        SPPFGS
048700970508     C                   MOVEL     BRVNFV        SPPNFV
048800970508     C                   MOVEL     D53DFV        SPPDFV
048900970508     C                   MOVEL     BRVATR        SPPATR
049000970508     C                   MOVEL     BRVVUC        SPPVUC
049100970508     C                   MOVEL     BRVPUC        SPPPUC
049200970508     C                   MOVEL     BRVCAN        SPPCAN
049300970508     C*
049400970508     C* SPPFLP NON LO PASSO PERCHE' SERVE SOLO SE LA SPUNTA E' "A"
049500020610     C* 10/06/02: NON È PIÙ VERO ORA SPPFLP SERVE ANCHE QUANDO LA SPUNT
049600020610     C* E' BLANK SE SI TRATTA DI UNTA SPUNTA PER IL TRANSITO
049700020610     C     SPPATR        IFEQ      *BLANKS
049800020610     C                   Z-ADD     SIMFEL        SPPFLP
049900020610     C                   ENDIF
050000970508     C                   MOVEL     DSSPP         D21REC
050100970508     C                   MOVEL     ' '           D21TLA
050200020809     C                   MOVEL     'S'           WCA21
050300970509    6C                   ENDIF
050400970508     C**
050500970509   X5C                   ELSE
050600970508     C** SPUNTA DI SIMFEL
050700950110     C                   EXSR      AGGFGV
050800970508     C*
050900970509     C** SE NON SI TRATTA DI PISTOLA AUTOGENERATA AGGIORNO
051000101202    6C     BRVNPS        IFNE      98
051100101202     C     BRVNPS        ANDNE     96
051200970508     C                   EXSR      CHIAMO
051300970509    6C                   ENDIF
051400970509    5C                   ENDIF
051500970508     C**
051600970509   X4C                   ELSE
051700040322     c* Richiamo fnls44r solo se spunta dell'area di partenza del p.o.
051800040322     c* di transito (simfel)
051900040322     c* Infatti dando per scontato che i secondi livello in arrivo tota
052000040322     c* li o parziali non hanno transiti usiamo la £1 perchè di fatto
052100040322     c* solo p.o. con ter-arr = ter-part aggiornano le bolle transito
052200040322     c* (v. anche fnls44r)
052300040322     c     brvpes        lookup    l1                                     33
052400040322     c*
052500040322     C   33              EXSR      CHIAMO
052600040322     c*
052700970509    4C                   ENDIF
052800921022     C*
052900070123     C     KBRV1         READE     FNBRV09L                               33
053000970509    3C                   ENDDO
053100020809     C                   ADD       1             XB
053200970509    2C                   ENDDO
053300940328     C*
053400940328     C* AGGIORNO E RILASCIO RECORD
053500970509    2C     WSPU          IFEQ      ' '
053600970508     C                   UPDATE    FNBTT000
053700970509    2C                   ENDIF
053800940328     C*
053900970509    1C                   ENDIF
054000970509     C*
054100970509     C* COME ULTIMA COSA AGGIORNO LA DATA DI PARTENZA DEL FOGLIO
054200970509     C     WCA21         IFEQ      'S'
054300970819     C                   CLEAR                   D21NOS
054400970509     C                   MOVEL     PARAM         KPJBU
054500970509     C                   CALL      'FNLR21R'
054600970509     C                   PARM                    KPJBA
054700020809     C                   MOVEL     'S'           WFC21
054800970509    5C                   ENDIF
054900911015     C*
055000911015     C                   ENDSR
055100950109     C**************************************************************************
055200950109     C*  CHIAMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
055300950109     C**************************************************************************
055400921022     C     CHIAMO        BEGSR
055500940328     C*
055600940328     C* AGGIORNO E RILASCIO RECORD
055700970509     C     WSPU          IFEQ      ' '
055800950110     C                   UPDATE    FNBTT000
055900020809     C                   MOVEL     'S'           WSPU
056000970509     C                   ENDIF
056100921022     C*
056200070223     c                   clear                   fnls45ds
056300070223     c                   move      brvpes        dls45pes
056400070223     c                   movel     'S'           dls45ric
056500070223     c                   move      comtfp        dls45tfp
056600070223     c                   movel     'T'           dls45flg
056700070223     c                   call      'FNLS45R'
056800070223     c                   parm                    fnls45ds
056900070223     c                   parm                    dsbrv
057000950109     C*
057100020809     C                   MOVEL     'S'           WFC44
057200950110     C*
057300921022     C                   ENDSR
057400950109     C**************************************************************************
057500950109     C*    AGGIORNO FOGLIO VIAGGIO
057600950109     C**************************************************************************
057700940111     C     AGGFGV        BEGSR
057800950109     C*
057900940111     C                   CLEAR                   WSPE
058000940111     C*
058100940111     C* TROVATA SPUNTA PARTENZA
058200020809     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
058300020809     C     K02FGV01      chain     FNFGV01L                           34
058400940111     C*
058500940328    1C     *IN34         IFEQ      *OFF
058600940328     C* SOLO SE E' CHIUSO
058700940328    2C     FGVFCF        IFNE      *BLANKS
058800950110     C                   Z-ADD     FGVNFV        BTTNFV
058900950110     C                   Z-ADD     FGVDFV        BTTDUT                         DATA FOGLIO
059000950110     C                   Z-ADD     BRVNPS        BTTPUT
059100940328    2C                   ENDIF
059200940328   X1C                   ELSE
059300940328     C* SE NON TROVATO FOGLIO CME SE FOSSE CHIUSO
059400950110     C                   Z-ADD     DATEU         BTTDUT
059500940328     C                   MOVEL     'S'           FGVFCF
059600940328    1C                   ENDIF
059700940111     C*
059800940328     C* FOGLIO CHIUSO
059900940328    1C     FGVFCF        IFNE      *BLANKS
060000940328     C*
060100950110     C     KBTP1         CHAIN     FNBTP000                           35
060200950110    2C     BTPDUT        IFEQ      *ZERO
060300950110     C  N35              Z-ADD     BTTDUT        BTPDUT                         DATA USCITA TRANSITO
060400940111     C* SOMMO LA SPEDIZIONE DI UN COLLO "EX MANCA BOLLA"
060500940111     C                   Z-ADD     1             WSPE
060600940328    2C                   ENDIF
060700950206     C*
060800950206     C  N35              UPDATE    FNBTP000
060900940111     C*
061000940111     C* CALCOLO PESO E VOLUME UNITARI DI UN COLLO "EX MANCA BOLLA" (IL
061100940111     C*   COLLO L'HO GIA' SOMMATO AL MOMENTO DELLA CHIUSURA DEL FOGLIO)
061200940111     C                   EXSR      TOTFVD
061300940111     C*
061400950110     C  N34              UPDATE    FNFGV000
061500940328    1C                   ENDIF
061600940111     C*
061700940111     C                   ENDSR
061800950109     C**************************************************************************
061900950109     C*    CALCOLO PESO/VOLUME/SPEDIZIONI UNITARI
062000950110     C* IL FILE FNFVD SERVIRA' PER IL PROSPETTO NOLI (FNLS59R)
062100950109     C**************************************************************************
062200940111     C     TOTFVD        BEGSR
062300940111     C*
062400980213     C* IN FNFVD POSSO AVERE TRE RECORD PER LO STESSO FOGLIO:
062500950110     C* TRC:   "1" = DELL'AREA            "2" = TRANSITO
062600980213     C*                                   "3" = TRANSITO PARTENZA SDI
062700980216     C* DEVO CONTROLLARE LA LINEA DI PARTENZA DELLA BOLLA
062800980216     C* PER CONOSCERE LA DITTA ORIGINARIA SDI O BARTOLINI
062900980216     C                   MOVEL     '2'           KTRC                           '2'=TRANSITO PART.BA
063000980213     C* AGGANCIO L'ORGANIGRAMMA PER VERIFICARE SE E' SDI O BARTOLINI
063100980216     C     BTTLNP        CHAIN     AZORG01L                           35
063200980213     C  N35ORGDIT        IFEQ      'SDI'
063300980213     C                   MOVEL     '3'           KTRC                           '3'=TRANSITO PART.SD
063400980213     C                   END
063500980213     C*
063600950110     C                   Z-ADD     SIMFEL        KLNP                           LNP
063700950110     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
063800950110     C                   Z-ADD     BTPLNA        KLNA                           LNA
063900950110     C     KFVD1         CHAIN     FNFVD01L                           35
064000940111     C*
064100940111     C     *IN35         IFEQ      *OFF
064200940111     C*****
064300940111     C***  P E S O
064400940111     C*****
064500950110     C     BTPPKB        DIV(H)    BTPNCL        PKGU
064600940111     C*
064700950110     C* ADDIZIONO IN FNFVD
064800950110     C     BTPPKB        IFGT      3500
064900950110     C                   ADD       PKGU          FVDKFA                         *OLTRE 35 Q.
065000940111     C                   ELSE
065100950110     C                   ADD       PKGU          FVDKOA                         *FINO  35 Q.
065200940111     C                   ENDIF
065300940111     C*
065400940111     C*****
065500940111     C***  V O L U M E
065600940111     C*****
065700940111     C* SE IL VOLUME DEL COLLO NON E' INDICATO IN FLBTT, LO CALCOLO
065800950110     C*   PRENDENDOLO DA FNBTP
065900950110     C     BTTVUC        IFNE      0
066000950110     C                   Z-ADD(H)  BTTVUC        VOLU
066100940111     C*
066200940111     C                   ELSE
066300940111     C*
066400950110     C     BTPVLB        DIV(H)    BTPNCL        VOLU
066500940111     C                   ENDIF
066600940111     C*
066700950110     C* VEDO IN QUALE POSIZIONE (DEL RECORD) STA IL TIPO SERVIZIO
066800020809     C                   Z-ADD     1             WA
066900950110     C     BTPTSP        LOOKUP    TSP(WA)                                15     ON=TROVATO
067000950110     C  N15              ADD       VOLU          VOL(WA)
067100940111     C*
067200940111     C*****
067300940111     C***  S P E D I Z I O N I
067400940111     C*****
067500940111     C     WSPE          IFGT      0
067600950110     C                   ADD       WSPE          FVDSNA
067700940111     C                   ENDIF
067800940111     C*
067900940111     C* ABBLENCO FLAG E DATA TRASMISSIONE PER INVIO IN SEDE
068000940111     C                   MOVEL     ' '           FVDFTR
068100940111     C                   Z-ADD     0             FVDDTR
068200940111     C*
068300950110     C                   UPDATE    FNFVD000
068400940111     C                   ENDIF
068500940111     C*
068600940111     C                   ENDSR
068700950109     C**************************************************************************
068800950110     C*    R O U T I N E    I N I Z I A L E
068900950110     C**************************************************************************
069000950110     C     *INZSR        BEGSR
069100950110     C*
069200950110     C     KTAB          KLIST
069300950110     C                   KFLD                    CODUT
069400950110     C                   KFLD                    COD
069500950110     C                   KFLD                    KEY
069600950110     C     KTAB2         KLIST
069700950110     C                   KFLD                    CODUT
069800950110     C                   KFLD                    COD
069900970508     C     KBLP          KLIST
070000950110     C                   KFLD                    BLPAAS
070100950110     C                   KFLD                    BLPLNP
070200950110     C                   KFLD                    BLPNRS
070300950110     C                   KFLD                    BLPNSP
070400020611     C     KBTP11        KLIST
070500020611     C                   KFLD                    SIMFEL
070600020611     C                   KFLD                    BLPAAS
070700020611     C                   KFLD                    BLPLNP
070800020611     C                   KFLD                    BLPNRS
070900020611     C                   KFLD                    BLPNSP
071000020611     C     KBTP1         KLIST                                                  FNBTP11L
071100020611     C                   KFLD                    BTTFLP
071200950110     C                   KFLD                    BTTAAS
071300950110     C                   KFLD                    BTTLNP
071400950110     C                   KFLD                    BTTNRS
071500950110     C                   KFLD                    BTTNSP
071600020611     C     KBTT1         KLIST                                                  FNBTT11L
071700020611     C                   KFLD                    BTPFLP
071800950110     C                   KFLD                    BTPAAS
071900950110     C                   KFLD                    BTPLNP
072000950110     C                   KFLD                    BTPNRS
072100950110     C                   KFLD                    BTPNSP
072200040108     C     KANM1         KLIST                                                  FNANM02l
072300970508     C                   KFLD                    BTTFLS
072400020611     C                   KFLD                    BTTLNA
072500950110     C                   KFLD                    BTTNRS
072600950110     C                   KFLD                    BTTNSC
072700950110     C                   KFLD                    COMCAA            3 0
072800070123     C     KBRV1         KLIST                                                  FNBRV09L
072900970509     C                   KFLD                    KNPG
073000970509     C                   KFLD                    BTTFLS                         ..LNP
073100950110     C                   KFLD                    BTTLNA                         ..LNA
073200950110     C                   KFLD                    BTTNRS                         ..SERIE
073300950110     C                   KFLD                    BTTNSC                         ..SEGNACOLLO
073400950110     C     KFVD1         KLIST                                                  FNFVD01L
073500950110     C                   KFLD                    KLNP                           ..LNP
073600950110     C                   KFLD                    KNFV                           ..NR.FOGLIO
073700950110     C                   KFLD                    KLNA                           ..LNA
073800950110     C                   KFLD                    KTRC                           ..TIPO RECORD
073900020809     C     K02FGV01      klist                                                  FNFGV01L
074000020809     C                   kfld                    KNFV                           ..NR.FOGLIO
074100020809     C                   kfld                    BRVfgs
074200950110     C*
074300950110     C                   ENDSR
074400950110     C**************************************************************************
074500950110**         CMD1
074600021008ALCOBJ OBJ((FNBLPT0R *FILE *EXCL           M234567890))
074700911002**         CMD4
074800021008OVRDBF FILE(FNBLPT0R)                   MBR(M234567890)
074900911001**         CMD5
075000021008CHKOBJ OBJ(FNBLPT0R) OBJTYPE(*FILE)     MBR(M234567890)
