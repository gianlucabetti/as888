000100040531     H DECEDIT('0,') DATEDIT(*YMD.)
000200950918     H* FNLR15R *----------------------------------------------------*
000300960705     H*     - AGGIORNAMENTO DATE DI PARTENZA LOCALI
000400000000     H*--------------------------------------------------------------*
000500000703     FTABEL00F  IF   E           K DISK
000600000703     FAZORG01L  IF   E           K DISK
000700031211     FFNblp01L  IF   E           K DISK
000800040531     FFiar501l  IF   E           K DISK
000900950918     FFNART27L  UF   E           K DISK
001000921022     F*
001100070123     FFNBRV02L  IF   E           K DISK
001200050223     FFNAGB01L  uf A E           K DISK
001300990510     FFNANM02L  IF A E           K DISK
001400990510     F                                     INFDS(FNANM2)
001500990510     FFNANM00F  UF   E             DISK
001600990510     F                                     RENAME(FNANM000:FNANMF)
001700911011     D*
001800950918     D L1              S              3  0 DIM(30)                              FIL GESTITE
001900970520     D*
002000990510     D FNANM2          DS
002100960712     D  ANMNR2               397    400B 0
002200950919     D* PARAMETRI PASSATI
002300950919     D PARAM           DS
002400121113     D* PARDTP CONTIENE IN REALTÀ  FGVDFV
002500950919     D  PARDTP                 1      8  0
002600950919     D  PARNFV                 9     13  0
002700950919     D  LEGFIL               237    246
002800950919     D  MBRFIL               247    256
002900950919     D  ALFFIL               248    250
003000951011     D* PARAMETRI, DATA BORDERO' PER PGM  - FNLR17R  -
003100951011     D PARAM2          DS
003200951011     D  PARDBR                 1      8  0
003300951011     D  PARDAM                 9     16  0
003400050707     D WLBDAT          DS
003500050707     D  G02DAT                 1      8  0
003600050707     D  G02INV                 9     16  0
003700050707     D  G02ERR                17     17
003800050707     D  G02TGI                18     22  0
003900000000     D KPJBA         E DS
004000950918     D                 DS
004100950918     D  ARTAAS                 1      4  0
004200950918     D  ARTLNP                 5      7  0
004300950918     D  ARTNRS                 8      9  0
004400950918     D  ARTNSP                10     16  0
004500950918     D  ARTSPE                 1     16  0
004600970520     D                 DS
004700970520     D  SAVAAS                 1      4  0
004800970520     D  SAVLNP                 5      7  0
004900970520     D  SAVNRS                 8      9  0
005000970520     D  SAVNSP                10     16  0
005100970520     D  SAVSPE                 1     16  0 INZ
005200950918     D* DS PER TRUL06R - CARICAMENTO £X
005300950918     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005400950918     D  LIN                    1     90  0
005500950918     D                                     DIM(30)                              FIL. COMODO
005600121115     d** trul09ds      e ds
005700121115     d**  TAR                   19    108  0 dim(30)
005800960712     D DS7C          E DS
005900960712     D CNCR80        E DS
006000911001     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006100121115     D OG143         E DS
006200070123     D DSBRVF        E DS                  EXTNAME(FNBRV00F)
006300040531     d dar5gen       E DS
006400040531     d dsbrv           s                   like(dsbrvf)
006500040531     d ktrd            s                   like(ar5trd) inz('GEN')
006600050223     d kagb            s                   like(agbagb) inz('DT')
006700050223     d savtbo          s                   like(agbtbo) inz('A')
006800911003     C*****************************************************************
006900000000     C     *ENTRY        PLIST
007000000000     C                   PARM                    KPJBA
007100031201     c                   PARM                    DSBRV
007200031202     c                   PARM                    FLG               1
007300031203     C* N.B.: Il secondo e terzo parametro non sono obbligatori
007400031203     c*       SE SI PASSA SOLO LA KPJBA il pgm legge le spunte del fogl
007500031203     c*       viaggio passato ed aggiorna la bolla e chiude in LR
007600031203     c*       SE SI PASSA ANCHE IL SECONDO PARAMETRO (record spunta)
007700031203     c*       il pgm salta il ciclo di lettura delle spunte ed effettua
007800031203     c*       direttamente gli aggiornamenti in base alla spunta ricevu
007900031203     c*       IL TERZO PARAMETRO può assumere i seguenti valori:
008000031203     c*       FLG = ' ' il pgm elabora la spunta passata nel secondo
008100031203     c*                 parametro (DSBRV) e chiude in RT
008200031203     c*       FLG = 'C' il pgm scrive l'ultimo record su FNAGB e chiude
008300031203     c*                 in LR
008400031203     c*
008500031203     c* Se richiamato con FLG='C' scrivo l'ultimo AGB e chiudo con lr
008600031205    0C                   IF        %PARMS > 1
008700031205    1c                   if        flg = 'C'
008800031203     C* ELABORO L'ULTIMO DATO MEMORIZZATO
008900031203     C                   EXSR      WRTAGB
009000031203     c                   seton                                        lr
009100031205     c                   goto      fine
009200031205    1c                   endif
009300031205    0c                   endif
009400031205     c
009500950919     C                   MOVEL     KPJBU         PARAM
009600000000     C                   Z-ADD     1             CODUT
009700911001     C                   CALL      'X§PARUT'
009800911001     C                   PARM                    UT§DSE
009900911001     C                   MOVEL     REC80         CNCR80
010000000111     C*
010100000111     C                   Z-ADD     PARNFV        KNFV
010200950918     C                   EXSR      OPERAZ
010300911011     C*
010400031201    0C                   IF        %PARMS < 2
010500920423     C* L O C A L E
010600911001     C*
010700950918     C                   Z-ADD     1             WNPG
010800911001     C*
010900070123     C     KBRV          SETLL     FNBRV000
011000070123     C     KBRV          READE     FNBRV000                               31
011100911003     C*
011200970127    2C     *IN31         DOWEQ     *OFF
011300950918     C* CONTROLLO LA LINEA DI ARRIVO LETTA SE E' LOCALE
011400031203     c* Siccome adesso ci sono f.v.p. non locali ma con lna da trattare come
011500031203     c* locali (es. fvp Modena per Bologna con lna nel fvp = 019) è necessa-
011600031203     c* rio fare lookup di brvlna in l1 per escludere dall'elaborazione
011700031203     c* tutte le lna che non si trovano sull'as di Simfel (nell'esempio
011800031203     c* bisogna escludere le spunte partenza da 18 per per 001)
011900031203     c* Occorre però aggiungere la lookup di lna su schiera dei p.o. di cui
012000031203     c* simfel è ter-arr che diventerà necessaria quando un secondo liv
012100031203     c* in arrivo su altro AS avrà lo stesso ogas del suo terminal di arrivo
012200031203     c* quando sarà tutto su as unico la prima lookup si potrebbe eliminare
012300031203     c* ma non è necessario:l'elaborazione funziona ugualmente
012400121115     c
012500121115     c* Es - eliminata lo lookup in £1 e in TAR ovvero terminal dia rrivo sus tesso OGAS
012600121115     c*  Adesso le linee secondi livello si possono mettere SOLO su un foglio locale o defluenza
012700121115     c*  quindi per forza tutte con lo stesso terminal di arrivo = simfel
012800121115     C**** BRVLNA        LOOKUP    L1                                     90
012900121115     C**n90BRVLNA        LOOKUP    TAR                                    90
013000950918     C*
013100950918     C* 1) NON E' LOCALE  --> LA SALTO E LEGGO LA PROSSIMA
013200121115    3C**   *IN90         IFEQ      *OFF
013300121115     C**   KBRV1         SETGT     FNBRV000
013400950918     C*
013500950918     C* 2)     E' LOCALE --> ELABORO
013600121115   X3C**                 ELSE
013700121115     c*
013800121115     c* escludo  le spunte errate
013900121115     c                   if        brvftr<>'L'
014000031201     c                   EXSR      AGGBOL
014100121115     c                   endif
014200951011     C*
014300121115    3C**                 ENDIF
014400950918     C*
014500070123     C     KBRV          READE     FNBRV000                               31
014600970127    2C                   ENDDO
014700970520     C*
014800970520     C* RICHIAMO LA SCRITTURA DI FNAGB PER L'ULTIMA BOLLA
014900970520     C                   EXSR      WRTAGB
015000950918     C*
015100031203    1C***                ENDIF
015200031201   x0c                   else
015300031201     c* Se il chiamante mi ha passato la spunta richiamo direttamente la
015400031201     c* routine di aggiornamento
015500031209     c                   movel     dsbrv         dsbrvf
015600031201     c                   exsr      AGGBOL
015700031201    0c                   endif
015800911016     C*
015900050224     c                   if        %parms < 2
016000000000     C                   SETON                                        LR
016100031201     c                   else
016200031201     c                   return
016300031201     c                   endif
016400031205     c     fine          tag
016500910322     C*
016600950918     C* OPERAZIONI INIZIALI ------------------------------------------*
016700950918     C     OPERAZ        BEGSR
016800950918     C**
016900950918     C* CARICO TABELLA L1
017000950918     C**
017100950918     C                   CLEAR                   DSUL06
017200950918     C                   MOVE      '£1'          D06COD
017300950918     C                   MOVEL     SIMFEL        D06KEY
017400121122     C                   MOVe      'S'           D06KEY
017500950918     C                   MOVEL     'L'           D06TLA
017600121113     c                   movel     pardtp        d06drf
017700950918     C                   MOVEL     DSUL06        KPJBU
017800950918     C*
017900950918     C                   CALL      'TRUL06R'
018000950918     C                   PARM                    KPJBA
018100950918     C                   MOVEL     KPJBU         DSUL06
018200950918     C                   MOVEA     LIN           L1
018300031201
018400031201      * Carico p.o. di cui SIMFEL è terminal di arrivo
018500031201
018600121115     c*****              clear                   trul09ds
018700121115     c*****              eval      i09mod = 'TFU'
018800121115     c*****              eval      i09tfa = simfel
018900121115     c****+              movel     pardtp        I09dta
019000121115     c*****              Call      'TRUL09R'
019100121115     c*****              parm                    trul09ds
019200950918     C**
019300950918     C* DEFINIZIONE CAMPI
019400950918     C**
019500960712     C     *LIKE         DEFINE    ANMFL1        WFL1
019600960712     C     *LIKE         DEFINE    ANMFL2        WFL2
019700960712     C     *LIKE         DEFINE    TBLCOD        COD
019800960712     C     *LIKE         DEFINE    TBLKEY        KEY
019900960712     C     *LIKE         DEFINE    BRVNPG        WNPG
020000000111     C     *LIKE         DEFINE    BRVNFV        KNFV
020100970520     C     *LIKE         DEFINE    AGBFVC        WFVC
020200970520     C     *LIKE         DEFINE    ARTFVC        WWFVC
020300990809     C     *LIKE         DEFINE    AGBFPC        WFPC
020400990809     C     *LIKE         DEFINE    ARTFPC        WWFPC
020500950918     C**
020600950918     C* KLIST
020700950918     C**
020800050223     C     KSAV          KLIST
020900050223     C                   KFLD                    SAVTBO
021000050223     C                   KFLD                    SAVAAS
021100050223     C                   KFLD                    SAVLNP
021200050223     C                   KFLD                    SAVNRS
021300050223     C                   KFLD                    SAVNSP
021400050223     C                   KFLD                    KAGB
021500950918     C     KBRV          KLIST
021600950918     C                   KFLD                    WNPG
021700000111     C                   KFLD                    KNFV
021800950918     C                   KFLD                    SIMFEL
021900950918     C     KBRV1         KLIST
022000950918     C                   KFLD                    WNPG
022100000111     C                   KFLD                    KNFV
022200950918     C                   KFLD                    SIMFEL
022300950918     C                   KFLD                    BRVLNA
022400950918     C     KART          KLIST
022500950918     C                   KFLD                    BRVLNP
022600950918     C                   KFLD                    BRVLNA
022700950918     C                   KFLD                    BRVNRS
022800950918     C                   KFLD                    BRVNSC
022900031211     C     kblp          KLIST
023000950918     C                   KFLD                    ARTAAS
023100950918     C                   KFLD                    ARTLNP
023200950918     C                   KFLD                    ARTNRS
023300950918     C                   KFLD                    ARTNSP
023400040531     C     kar5          KLIST
023500040531     C                   KFLD                    ARTAAS
023600040531     C                   KFLD                    ARTLNP
023700040531     C                   KFLD                    ARTNRS
023800040531     C                   KFLD                    ARTNSP
023900040531     C                   KFLD                    ktrd
024000000703      *
024100960712     C     KTAB          KLIST
024200960712     C                   KFLD                    CODUT
024300960712     C                   KFLD                    COD
024400960712     C                   KFLD                    KEY
024500000703     C*
024600000703     C* PRENDO LA DS7C DELLA ANOMALIA 05
024700000703     C                   MOVEL     '7C'          COD
024800000703     C                   MOVEL(P)  005           KEY
024900000703     C     KTAB          CHAIN     TABEL                              32
025000000703     C  N32              MOVEL     TBLUNI        DS7C
025100000703     C   32              CLEAR                   DS7C
025200000703      *
025300960712     C     KANM          KLIST
025400960712     C                   KFLD                    ARTFLS
025500960712     C                   KFLD                    ARTLNA
025600960712     C                   KFLD                    ARTNRS
025700960712     C                   KFLD                    ARTNSC
025800960712     C                   KFLD                    ANMCAA
025900911107     C                   ENDSR
026000031201     C* AGGIORNAMENTO BOLLA ------------------------------------------*
026100031201     c     AGGBOL        begsr
026200031201     C     KART          CHAIN     FNART000                           32
026300031201     C*
026400031201     C* SEGNACOLLO NON TROVATO  --> NON FACCIO NULLA
026500031201    4C     *IN32         IFEQ      *OFF
026600031201     C     ARTDFV        ANDEQ     0
026700031201     C* AGGIORNO ANCHE SE DATA RICEVUTA > DATA GIA' IMMESSA
026800031201    4C     *IN32         OREQ      *OFF
026900031201     C     ARTDFV        ANDLT     PARDTP
027000031201     C* AGGIORNO SOLTANTO SE LNP IN L1
027100031201     C     ARTLNP        LOOKUP    L1                                     34
027200031201     C**
027300031201   4AC     *IN34         IFEQ      *ON
027400031201     C*
027500031201     C* AGGIORNO LA DATA DI ARRIVO E LA DATA DI PARTENZA
027600031201    5C     ARTSPE        IFNE      SAVSPE
027700031201     C* SCRIVO LA BOLLA IN FNAGB (LA PRIMA VOLTA NON SCRIVO)
027800031201     C                   EXSR      WRTAGB
027900031201     C                   CLEAR                   WFVC
028000031201     C                   CLEAR                   WFPC
028100031211     C     KBLP          CHAIN     FNBLP000                           33
028200031201     C* CONTROLLO LA DATA BORDERO' DELLA SPEDIZIONE (SOLO LA 1 VOLTA)
028300031201     C     WUNO          IFEQ      ' '
028400031201     C                   EXSR      DATARR
028500031201     C                   MOVEL     '1'           WUNO              1
028600031201     C                   ENDIF
028700031201     C                   MOVEL     ARTSPE        SAVSPE
028800031201    5C                   ENDIF
028900031201     C*
029000031201     C* AGGIORNO IL COLLO
029100031201     C* DATA PARTENZA MERCE E NUMERO PISTOLA PARTENZA SOLO SE LOCALE
029200031201     C                   MOVEL     PARDTP        ARTDFV
029300031201     C                   MOVEL     BRVNPS        ARTNPP
029400031201     C                   MOVEL     PARNFV        ARTNFV
029500031201     C* AGGIORNO IL VOLUME CML
029600031201     C                   MOVEL     'P'           WWFVC
029700031201      * cerco network linea arrivo
029800031201     c                   Clear                   og143
029900031201     c     BrvLna        Chain     Azorg01l
030000031201     c                   If        %Found(Azorg01l)
030100031201     c                   Movel     OrgDe3        Og143
030200031201     c                   EndIf
030300031201
030400031201    5c                   SELECT
030500031201     c                   when      BrvVuc = *Zeros
030600031201      * NON SOSTITUISCO IL VOLUME ARRIVO SE LNA FEDEX
030700031201     C                   when      ARTFVC = 'A' and ARTVUC > *zeros
030800031201     C                                          and §Ogntw = 'FED'
030900031201     C* IL VOLUME ARRIVO O TRANSITO LO SOSTITUISCO SE QUELLO DELLA
031000031201     C* SPUNTA E' RILEVATO DALLA PARTENZA
031100031201     C                   when       ARTVUC = *zeros  or
031200031201     C                             (ARTVUC > *zeros and ARTFVC <> 'P')
031300031201     C                   MOVE      'S'           WFVC
031400031201     C                   Z-ADD     BRVVUC        ARTVUC                          VOLUME
031500031201     C                   MOVEL     WWFVC         ARTFVC                          ' '=DA CML
031600031201    5c                   Endsl
031700031201     C*
031800031201     C* AGGIORNO IL PESO CML
031900031201     C                   MOVEL     'P'           WWFPC
032000031201     C*
032100031201    5C     BRVPUC        IFGT      0                                             PESO CML
032200031201    6C     ARTPUC        IFEQ      0
032300031201     C* IL VOLUME ARRIVO O TRANSITO LO SOSTITUISCO SE QUELLO DELLA
032400031201     C* SPUNTA E' RILEVATO DALLA PARTENZA
032500031201     C     ARTPUC        ORNE      0
032600031201     C     ARTFPC        ANDNE     'P'
032700031201     C                   MOVE      'S'           WFPC
032800031201     C                   Z-ADD     BRVPUC        ARTPUC                          PESO
032900031201     C                   MOVEL     WWFPC         ARTFPC                          ' '=DA CML
033000031201    6C                   END
033100031201     C*
033200031201    5C                   END
033300031201     C***
033400031201     C*
033500031201     C* CREO L'ANOMALIA COLLO PARTITO NON ARRIVATO
033600031201    5C     ARTDAM        IFEQ      0
033700031201     C* SOLO SE IL TERMINAL DI ARRIVO E' SULLO STESSO AS
033800121115     C***  blptfa        LOOKUP    L1                                     98
033900031201     C* PER LNP = POSTE SOLO SE CONSENTITA L'ANOMALIA 005
034000121115     C** 98              DO
034100121115     C**   ARTLNP        CHAIN     AZORG01L                           38
034200121115     C**   *IN38         IFEQ      *OFF
034300121115     C**                 MOVEL     ORGDE3        OG143
034400031201     C**!!!§OGPT         IFEQ      'S'
034500121115     c**   §ogNtw        Ifeq      'PPT'
034600121115     C**   §7CCPT        ANDNE     'S'
034700121115     C**                 SETOFF                                       98
034800121115     C**                 ENDIF
034900121115     C**                 ENDIF
035000121115     C**                 ENDDO
035100031201      *
035200121115     C***98              EXSR      CRTANM
035300121115     c
035400121115     c* tolto indicatore 98 perchè ora la creo sempre l'anomalia siamo su As unico
035500121115     C                   EXSR      CRTANM
035600031201      *                    -----------
035700031201    5C                   ENDIF
035800031201     C*
035900031201     C                   UPDATE    FNART000
036000031201  X4AC                   ELSE
036100031201     C                   UNLOCK    FNART27L
036200031201   4AC                   ENDIF
036300041006   x4c                   else
036400041006     C                   UNLOCK    FNART27L
036500031201    4C                   ENDIF
036600031201     c                   endsr
036700970520     C**---------------------------------------------------------
036800970520     C** SCRITTURA FNAGB
036900970520     C**---------------------------------------------------------
037000970520     C     WRTAGB        BEGSR
037100970520     C     SAVSPE        IFGT      0
037200050223     C     KSAV          CHAIN     FNAGB01L                           32
037300050223     C   32              CLEAR                   FNAGB000
037400970520     C                   MOVEL     'A'           AGBTBO
037500970520     C                   MOVEL     'DT'          AGBAGB
037600970520     C                   MOVEL     SAVAAS        AGBAAS
037700970520     C                   MOVEL     SAVLNP        AGBLNP
037800970520     C                   MOVEL     SAVNRS        AGBNRS
037900970520     C                   MOVEL     SAVNSP        AGBNSP
038000970520     C                   MOVEL     'S'           AGBFBS
038100970520     C     WFVC          IFEQ      'S'
038200970520     C                   MOVEL     WFVC          AGBFVC
038300970520     C                   ENDIF
038400990809     C     WFPC          IFEQ      'S'
038500990809     C                   MOVEL     WFPC          AGBFPC
038600990809     C                   ENDIF
038700050224     C   32              WRITE     FNAGB000                             99
038800050223     C  N32              UPDATE    FNAGB000
038900970520     C                   END
039000970520     C                   ENDSR
039100960712     C**---------------------------------------------------------
039200960712     C** PRENDO LA IPOTETICA DATA DI ARRIVO DELLE BOLLE
039300960712     C**---------------------------------------------------------
039400960712     C     DATARR        BEGSR
039500960712     C                   CLEAR                   PARAM2
039600031211     C                   Z-ADD     blpdbr        PARDBR
039700960712     C                   MOVEL     PARAM2        KPJBU
039800960712     C                   CALL      'FNLR17R'
039900960712     C                   PARM                    KPJBA
040000960712     C*
040100960712     C                   MOVEL     KPJBU         PARAM2
040200960712     C*
040300960712     C                   ENDSR
040400960712     C**---------------------------------------------------------
040500960712     C** CREO L'ANOMALIA COLLO PARTITO NON ARRIVATO
040600960712     C**---------------------------------------------------------
040700960712     C     CRTANM        BEGSR
040800960712     C                   Z-ADD     5             ANMCAA
040900960712     C                   MOVEL     'S'           WELAB             1
041000960712     C                   CLEAR                   WANOM
041100960712     C*
041200990510     C     KANM          CHAIN     FNANM000                           32
041300960712     C*
041400960712     C* ELABORO SE NON TROVO L'ANOMALIA O IL FV E' DIVERSO
041500960712    1C     *IN32         DOWEQ     *OFF
041600960712     C* SE E' LO STESSO FOGLIO NON FACCIO NULLA
041700960712    2C     ANMNFV        IFEQ      PARNFV
041800960712     C     ANMCDU        ANDEQ     SIMFEL
041900960712     C                   MOVEL     'N'           WELAB
042000960712     C                   SETON                                        32
042100960712   X2C                   ELSE
042200960712     C* SALVO I CAMPI CHE SERVONO E MEMORIZZO CHE C'E' GIA' L'ANOMALIA
042300960712     C                   MOVEL     'S'           WANOM             1
042400960712     C                   Z-ADD     ANMNR2        WSANR2            9 0
042500960712     C*
042600990510     C     KANM          READE     FNANM000                               32
042700960712    2C                   ENDIF
042800960712    1C                   ENDDO
042900960712     C*
043000960712    1C     WELAB         IFEQ      'S'
043100960712     C* SE HO TROVATO UN'ALTRA ANOMALIA LA RICHAINO
043200960712    2C     WANOM         IFEQ      'S'
043300990510     C     WSANR2        CHAIN     FNANM00F                           32
043400960712    2C                   ENDIF
043500960712     C*
043600960712     C                   Z-ADD     ARTAAS        ANMAAS
043700960712     C                   Z-ADD     ARTLNP        ANMLNP
043800960712     C                   Z-ADD     ARTNSP        ANMNSP
043900960712     C*
044000960712     C                   MOVEL     ARTFLS        ANMFLS
044100960712     C                   MOVEL     ARTLNA        ANMLNA
044200960712     C                   MOVEL     ARTNRS        ANMNRS
044300960712     C                   MOVEL     ARTNSC        ANMSCN
044400960712     C                   Z-ADD     ARTNPP        ANMNPS
044500990510     C                   Z-ADD     PARNFV        ANMNFV
044600960712     C                   MOVEL     SIMFEL        ANMCDU
044700960712     C                   Z-ADD     PARDAM        ANMDAO
044800960712     C                   MOVEL     SIMFEL        ANMFLE
044900960712     C                   MOVEL     SIMFEL        WFL1
045000040531     C* IMPOSTO FALG COLLI DI VALORE
045100040531     C     KAR5          CHAIN     FIAR501L
045200040531     C                   IF        %FOUND(FIAR501L)
045300040531     C                   MOVEL     AR5UNI        DAR5GEN
045400040531     C                   ELSE
045500040531     C                   CLEAR                   DAR5GEN
045600040531     C                   ENDIF
045700040531     c*
045800040531     C                   MOVEL     §ar5bva       anmft4
045900040531     c                   movel     999           anmfl4
046000960712     C*
046100960712     C                   MOVEL     'I'           ANMFSA
046200960712     C                   MOVEL     §7CTAN        ANMTAN
046300960712     C                   MOVEL     §7CSAN        ANMSAN
046400960712     C                   MOVEL     §7CAIE        ANMAIE
046500960712     C                   Z-ADD     §7CLID        ANMLID
046600960712     C                   Z-ADD     0             ANMDCH
046700960712     C                   MOVEL     ' '           ANMFSC
046800960712     C                   MOVEL     '  '          ANMCCH
046900960712     C                   CLEAR                   ANMATB
047000960712     C* CHIUSURA AUTOMATICA
047100960712    2C     §7CCHA        IFEQ      'S'
047200960712     C                   Z-ADD     PARDAM        ANMDCH
047300960712     C                   MOVEL     'I'           ANMFSC
047400960712    2C                   END
047500960712     C                   Z-ADD     0             ANMFL1
047600960712     C                   Z-ADD     0             ANMFL2
047700960712     C* ESTERNA
047800960712    2C     §7CAIE        IFEQ      'E'
047900960712     C* VEDO A CHI TRASMETTERE
048000960712     C                   MOVEL     WFL1          ANMFL1
048100960712     C                   MOVEL     WFL2          ANMFL2
048200960712    2C                   ENDIF
048300050707     c*
048400050707     C                   TIME                    W0140            14 0
048500050707     C                   MOVE      W0140         G02DAT
048600050707     C                   Z-ADD     *ZERO         G02INV
048700050707     C                   MOVEL     *BLANKS       G02ERR
048800050707     C                   CALL      'XSRDA8'
048900050707     C                   PARM                    WLBDAT
049000050707     C                   Z-ADD     G02INV        WDATA             6 0
049100050707     C
049200050707     c                   movel     w0140         wora4             4 0
049300050707     c                   movel     wdata         anmflo
049400050707     c                   move      wora4         anmflo
049500960712     C*
049600960712     C*  SE CE N'E' UN'ALTRA, AGGIORNO
049700960712    2C     WANOM         IFEQ      'S'
049800990510     C                   UPDATE    FNANMF
049900960712     C**
050000960712   X2C                   ELSE
050100960712     C* ALTRIMENTI SCRIVO
050200960712     C                   CLEAR                   ANMFT1
050300960712     C                   CLEAR                   ANMFT2
050400990510     C                   CLEAR                   ANMDUE
050500990510     C                   CLEAR                   ANMNPG
050600990510     C                   CLEAR                   ANMFGS
050700990510     C                   CLEAR                   ANMFT3
050800990510     C                   CLEAR                   ANMFL3
050900040531     C* NON IMPOSTO LA ZONA
051000040531     C                   MOVEL     999           ANMFL4
051100990510     C                   CLEAR                   ANMDTR
051200990510     C                   WRITE     FNANM000
051300960712    2C                   ENDIF
051400960712    1C                   ENDIF
051500960712     C*
051600960712     C                   ENDSR
