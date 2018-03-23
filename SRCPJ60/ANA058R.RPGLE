000100020131 0334H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200020131B0334H*PARMS CVTOPT(*DATETIME)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940117      *
000500950411      *            GESTIONE VARIAZIONE ESTREMI FISCALI
000600950411      *           ------------------------------------
000700940223      *
000800940223      *
000900940223      * DOCUMENTI MICROANALISI :
001000940223      *
001100940223      * NOTE TECNICHE :
001200950301    > *  1) ELIMINARE LE SPECIFICHE RELATIVE AL COMMIT/ROLLBACK SE
001300950301    > *     QUESTO NON DEVE ESSERE GESTITO
001400941214      *
001500940223      * PERSONALIZZAZIONI
001600940223      *
001700940223      *
001800940223      *
001900940223      *               MAPPA INDICATORI STD
002000940223      *
002100940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002200950411      *  04           PROTECT TUTTI CAMPI CHIAVE TRANNE DATA ANN
002300950411      *               SE OPZ=ANNULLAMENTO OPPURE RCD GIA' ANNULLATO
002400940223      *  21           GENERICO OPERAZIONI I/O
002500940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002600940223      *  30           SFLDSP
002700940223      * N31           SFLCLR
002800940128      *  31           SFLDSPCTL
002900940128      *  32           SFLNXTCHG
003000940128      *  33           SFLEND
003100940128      *  39           OF PRTF
003200940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003300940317    > *  Specificare l'uso dei singoli indicatori
003400940315      *  50 <---> 98  ERRORI SU VIDEO
003500940317    > *  Specificare l'uso dei singoli indicatori
003600940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003900940128      *  99           INDIC. GENERALE DI ERRORE
004000940128     F*----------------------------------------------------*
004100950411     FANSOG01L  IF   E           K DISK
004200941215     F*------------
004300950411     FANIND01L  IF   E           K DISK
004400950411     F*------------
004500950411     FANCPI01L  UF A E           K DISK
004501100901R027 F*------------
004502100901R027 FSDGSO301L UF   E           K DISK
004600950411     F*------------
004700950411     FANA058D   CF   E             WORKSTN
004800950411     F                                     RENAME(A058D1:FMTD1)
004900950411     F                                     RENAME(A058T1:FMTT1)
005000950411     F                                     RENAME(A058Z1:FMTZ1)
005100950412     F                                     INFDS(DSFMT)
005200941215     F*------------
005300940128     D*----------------------------------------------------*
005400950404     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005500950404     D $ID             S             27    DIM(5) ALT($OZ)
005600940506     D* Schiere per la gestione dei tasti di funzione
005700940506     D $FM             S             27    DIM(24)
005800940506     D $FX             S             50    DIM(24)
005900940506     D $FC             S              1    DIM(24)
006000940506     D $FR             S              1    DIM(24)
006100940506     D* Video D1
006200940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006300940506     D $FX1            S             50    DIM(24) ALT($FM1)
006400940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006500940506     D $FR1            S              1    DIM(24) ALT($FC1)
006600950411     D*-------------
006700940127     D*-------------
006800940127     D* Passaggio Parametri
006900940127     D KPJBA         E DS
007000950301     D*-------------
007100950301     D* Reperimento Autorizzazioni
007200950301     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
007300950301     D*-------------
007400950410     D* Passaggio Autorizzazioni
007500950410     D DSAUT           DS           512
007600950410     D*-------------
007700960206     D* Ricerca legende
007800960206     D A964DS        E DS                  EXTNAME(ANA964DS)
007900960206     D* Controlli validità legende
008000960206     D X07DS         E DS                  EXTNAME(X07VALDS)
008100960206     D*-------------
008200960206     D* Maschera dati autorizzazioni
008300960206     D GSOGDS        E DS                  EXTNAME(ANGSOGDS)
008400950301     D*-------------
008500950126     D* Reperimento nome PGM
008600950126     D STATUS         SDS           333
008700950126     D  DSPGM            *PROC
008800950411     D  PARMS            *PARMS
008900950411     D*-------------
009000950411     D* EX-area dati
009100950411     D SICOM         E DS
009200950411     D*-------------
009300950411     D* DS gestione gruppo  o azienda
009400950411     D XPAOUT          DS          2000
009500941209     D*-------------
009600941209     D* Dati di ambiente ottenuti da XSOC
009700950411     D SOC001        E DS                  EXTNAME(XSOC001DS)
009800960206     D  XSCSDC       E                     EXTFLD(XSCSALDOC)
009900960206     D  XSCSDA       E                     EXTFLD(XSCSALDOA)
010000941209     D*-------------
010100941209     D* DS Interna per dati di output di XSOC
010200941209     D XSOCDS          DS          1000
010300940203     D*-------------
010400940201     D* Passaggio Parametri al PGM di manutenzione
010500950411     D A058DS        E DS                  EXTNAME(ANA058DS)
010501160503R224 X*ex R027   -------------
010502160503  "  X*ex R027   Parametri del PGM di manutenzione  dati aggiuntivi
010503160503  "  X*ex R027   SDGSD16DS     E DS                  inz
010504160503  "  X*ex R027    RETD16       E                     INZ('0')
010505160503  "  X*ex R027    OPRD16       E                     INZ('0')
010506160503  "  X*ex R027    ERRD16       E                     INZ('0')
010507160503R224 X*ex R027    CMTD16       E                     INZ('0')
010600950404     D*-------------
010700950411     D* Richiamo a XATB
010800950411     D XATBDS        E DS                  INZ
010900950411     D*-------------
011000950411     D* Parametri per ricerca codice ANATB00F
011100950411     D XTABDS        E DS                  INZ
011200950411     D*-------------
011300950411     D* DS esterna provincia
011400950411     D TABC17        E DS                  EXTNAME(ANGC17DS) INZ
011500950411     D*-------------
011600950411     D* DS esterna stato
011700950411     D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
011800950411     D*-------------
011900950411     D* DS esterna natura giuridica
012000950411     D TABG16        E DS                  EXTNAME(ANGG16DS) INZ
012100950411     D*-------------
012200940201     D DSFMT           DS
012300940506     D  $TASTO               369    369
012400940201     D  NRG                  370    370
012500940201     D  NCL                  371    371
012600950314     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
012700940201     D*-------------
012800950411     D* DS Esterna gestione messaggi per COMIT
012900950411     D XMSGDS        E DS
013000950411     D  STS          E                     EXTFLD(MSGSTS)
013100940721     D                                     DIM(333)
013200950411     D  JBA          E                     EXTFLD(MSGJBA)
013300940721     D                                     DIM(502)
013400950411     D*-------------
013500940201     D* posizione cursore
013600940201     D CURSOR          DS
013700940223     D  RIRI                   1      2B 0 INZ
013800940201     D  R$$                    2      2
013900940223     D  COCO                   3      4B 0 INZ
014000940201     D  C$$                    4      4
014100940225     D*-------------
014200940506     D* Tasti di funzione
014300940506     D F01             C                   CONST(X'31')
014400940506     D F02             C                   CONST(X'32')
014500940506     D F03             C                   CONST(X'33')
014600940506     D F04             C                   CONST(X'34')
014700940506     D F05             C                   CONST(X'35')
014800940506     D F06             C                   CONST(X'36')
014900940506     D F07             C                   CONST(X'37')
015000940506     D F08             C                   CONST(X'38')
015100940506     D F09             C                   CONST(X'39')
015200940506     D F10             C                   CONST(X'3A')
015300940506     D F11             C                   CONST(X'3B')
015400940506     D F12             C                   CONST(X'3C')
015500940506     D F13             C                   CONST(X'B1')
015600940506     D F14             C                   CONST(X'B2')
015700940506     D F15             C                   CONST(X'B3')
015800940506     D F16             C                   CONST(X'B4')
015900940506     D F17             C                   CONST(X'B5')
016000940506     D F18             C                   CONST(X'B6')
016100940506     D F19             C                   CONST(X'B7')
016200940506     D F20             C                   CONST(X'B8')
016300940506     D F21             C                   CONST(X'B9')
016400940506     D F22             C                   CONST(X'BA')
016500940506     D F23             C                   CONST(X'BB')
016600940506     D F24             C                   CONST(X'BC')
016700940506     D ENTER           C                   CONST(X'F1')
016800940506     D ROLDWN          C                   CONST(X'F4')
016900940506     D ROLLUP          C                   CONST(X'F5')
017000960206     D*-------------
017100960206     D REPFPI          C                   CONST('SOGFRCPIVA')
017200950331     D*-------------
017300950331     D* Sottotitolo di testata
017400950411     D SOTTIT          C                   CONST('PROMSG    *LIBL-
017500950411     D                                          COS0095')
017600000114A0735D ZDateISO        S               D
017700020201B0334D* data massima
017800020201  "  D $DT9            C                   CONST('9999-12-31')
017900020201  "  D*-------------
018000020131  "  D* Parametri per il driver dei controlli (PJXX0401R)
018100020131  "  D* (Sono definite IMPORT, in modo da non doverle
018200020131  "  D*  passare al modulo - intermedio - che legge i messaggi
018300020131  "  D*  dalla PgmQ riempita dal driver dei controlli e
018400020131  "  D*  risale ai campi errati)
018500020131  "  D** Campi da controllare
018600020131  "  D £Soggetto       S              8A   import
018700020131  "  D £RagSoc         S             44A   import
018800020131  "  D £Indirizzo      S             44A   import
018900020131  "  D £Provincia      S              2A   import
019000020131  "  D £Stato          S              4A   import
019100020131  "  D £CAP            S              9A   import
019200020131  "  D £Localita       S             34A   import
019300020131  "  D £NatGiur        S              2A   import
019400020131  "  D £FlagForz       S              1A   import
019500020131  "  D £CodFisc        S             20A   import
019600020131  "  D £PartIVA        S             20A   import
019700020131  "  D** Segnalazioni CdF, P.IVA doppi
019800020131  "  D £CdFDopp        S              1A   import
019900031209B0334D £PIVADopp       S              1A   import
020000031209B0657X** ex B0334 * £CdFSogOrig     S              1A   import
020100031209  "  X** ex B0334 * £PIVSogOrig     S              1A   import
020200031209  "  D £CdFSogOrig     s             20a   import
020300031209B0657D £PIVSogOrig     s             20a   import
020400031209B0334D** Decodifiche
020500020131  "  D £DesProvincia   S             20A   import
020600020131  "  D £DesStato       S             20A   import
020700020131  "  D £DesNatGiur     S             20A   import
020800020131  "  D £DesFlagForz    S             20A   import
020900020131  "  D** Flag che indica se i controlli sono relativi
021000020131  "  D** all'anagrafica variazioni dati fiscali
021100020131  "  D £CtrDatiFisc    S              1A   import
021200020131  "  D*-------------
021300020131  "  D* Parametri per il driver che legge la coda messaggi
021400020131  "  D* e risale ai campi errati (PJXX0402R)
021500020131  "  D*
021600020131  "  D* Dimensione della schiera dei campi errati
021700020131  "  D DimSk           C                   const(15)
021800020131  "  D* Schiera dei campi errati
021900020131  "  D SkCampiErr      S             14A   dim(DimSk)
022000020131  "  D* Indice per la schiera dei campi errati
022100020131  "  D Ind             S              2S 0
022200020131  "  D*-------------
022300020131  "  D* Campi per spegnere indicatori
022400020131  "  D Spegni5         C                   const('00000')
022500020131  "  D Spegni2         C                   const('00')
022600020131  "  D*-------------
022700020131  "  D* Parametri per la pulizia della coda messaggi
022800020131  "  D ClStkCounter    S              9B 0 inz(1)
022900020131  "  D ClStkEntry      S             10A   inz('*')
023000020131  "  D MsgKey          S              4A
023100020131  "  D MsgsToRmv       S             10A   inz('*ALL')
023200020201  "  D ApiErr          S              1A
023300020201  "  D* Passaggio Parametri al PGM ANA801R
023400020201  "  D A801DS        E DS                  EXTNAME(ANA801DS)
023500020201B0334D  ERR801       E                     INZ('0')
023600050506B0802D SavCdF          S                   like(d1Cdf)
023700050506B0802D SavIva          S                   like(d1Iva)
023800940128     I*----------------------------------------------------*
023900950411     I*-------------
024000950411     IANSOG000
024100950411     I              SOGSOGG                     SOGCOD
024200950411     I              SOGDTENTR                   SOGDAT
024300950411     I              SOGDTULMO                   SOGDAU
024400950411     I              SOGDTINVL                   SOGDAI
024500950411     I              SOGDTFIVL                   SOGDAF
024600950411     I              SOGDTNASC                   SOGDAN
024700950411     I              SOGDES                      SOGDES
024800950411     I              SOGDESAGG                   SOGDE1
024900950411     I              SOGCDFISC                   SOGCDF
025000950411     I              SOGPARTIVA                  SOGIVA
025100950411     I              SOGFRCPIVA                  SOGFPI
025200950411     I              SOGISCCIAA                  SOGCIA
025300950411     I              SOGSEX                      SOGSES
025400950411     I              SOGNATGIUR                  SOGNAT
025500950411     I              SOGCODPREC                  SOGCOP
025600950411     I              SOGABI                      SOGABI
025700950411     I              SOGCLFSOG                   SOGCLG
025800950411     I*-------------
025900950411     IANIND000
026000950411     I              INDSOGG                     INDCOD
026100950411     I              INDTPIND                    INDTII
026200950411     I              INDCDIND                    INDCOI
026300950411     I              INDDTULMO                   INDDAU
026400950411     I              INDDTINVL                   INDDAI
026500950411     I              INDDTFIVL                   INDDAF
026600950411     I              INDDESFIL                   INDDES
026700950411     I              INDDESAGG                   INDDE1
026800950411     I              INDINDRIZ                   INDVIA
026900950411     I              INDLOCALIT                  INDCIT
027000950411     I              INDCAP                      INDCAP
027100950411     I              INDPROV                     INDPRV
027200950411     I              INDSTATO                    INDSTA
027300950411     I              INDCAB                      INDCAB
027400950411     I              INDLINGUA                   INDLIN
027500950411     I              INDTELEX                    INDTLX
027600950411     I              INDTELEFON                  INDTEL
027700950411     I              INDTELEFAX                  INDTLF
027800950411     I*-------------
027900950411     I* Ridenominazione Campi file ANCPI00F
028000950411     IANCPI000
028100950411     I              CPISOGG                     CPICOD
028200950411     I              CPIDTFIVL                   CPIDTF
028300950411     I              CPIPARTIVA                  CPIIVA
028400950414     I              CPIFRCPIVA                  CPIFPI
028500950411     I              CPICDFISC                   CPICDF
028600950411     I              CPIDESSOG1                  CPIDE1
028700950411     I              CPIDESSOG2                  CPIDE2
028800950411     I              CPIISCCIAA                  CPICIA
028900950411     I              CPINATGIUR                  CPINAT
029000950411     I              CPIINDRIZ                   CPIVIA
029100950411     I              CPILOCALIT                  CPICIT
029200950411     I              CPICAP                      CPICAP
029300950411     I              CPIPROV                     CPIPRV
029400950411     I              CPISTATO                    CPISTA
029500941209     I*-------------
029600940127     C*----------------------------------------------------*
029700940127     C*                MAIN LINE PROGRAM
029800940127     C*----------------------------------------------------*
029900940127     C*
030000940223     C* inizializzazione variabili
030100940223     C                   EXSR      INZVAR
030200940223     C*
030300940223     C     $FINE         DOWEQ     *OFF
030400940202     C     $GEST         CASEQ     'D1'          GESD1
030500950411    >C*****      $GEST     CASEQ'D2'      GESD2
030600941027     C                   ENDCS
030700941027     C                   ENDDO
030800940201     C*
030900940325     C* fine programma
031000940325     C                   EXSR      ENDPGM
031100940325     C/EJECT
031200940325     C************************************************************
031300940325     C* RICERCHE
031400940325     C************************************************************
031500940325     C     ENDPGM        BEGSR
031600941214     C*
031700941214     C* se ho confermato l'uscita proseguo
031800941214     C     $FINE         IFEQ      *ON
031900940325     C*
032000940325     C* disallocazione rcd nel caso di modifica/annullamento
032100950411     C     OPZ058        IFEQ      '02'
032200950411     C     OPZ058        OREQ      '04'
032300950411     C                   UNLOCK    ANCPI01L                             22
032400941027     C                   ENDIF
032500940325     C*
032600940325     C* passaggio dati al pgm chiamante
032700950411     C*
032800950411     C                   MOVEL     A058DS        KPJBU
032900940325     C*
033000940325     C                   SETON                                            RT
033100940616     C                   RETURN
033200941214     C*
033300941214     C                   ENDIF
033400940325     C*
033500940325     C                   ENDSR
033600940325     C/EJECT
033700940201     C************************************************************
033800940201     C* RICERCHE
033900940201     C************************************************************
034000940127     C     SEARCH        BEGSR
034100940127     C*
034200940127     C* determino Riga/Colonna del cursore
034300940127     C                   MOVE      NRG           R$$
034400940127     C                   MOVE      NCL           C$$
034500940127     C                   Z-ADD     RIRI          CURR
034600940127     C                   Z-ADD     COCO          CURC
034700940127     C*
034800940224     C* I campi che al momento dell'F4 risultano protetti non devono
034900940224     C* essere abilitati alla funzione di ricerca
035000941027     C*
035100941027     C                   SELECT
035200950411     C*
035300950411     C* FMTD1  - PROVINCIA
035400950411     C     H1NMFL        WHENEQ    'D1PRV'
035500950411     C                   RESET                   XTABDS
035600950411     C                   MOVE      '01'          XTAOPZ
035700950411     C                   MOVE      XSCSOC        XTAAZI
035800950411     C                   MOVE      XSCLIN        XTALIN
035900950411     C                   Z-ADD     CURR          XTARIG
036000950411     C                   Z-ADD     CURC          XTACOL
036100950411     C                   CLEAR                   KPJBU
036200950411     C                   MOVEL     XTABDS        KPJBU
036300950411     C                   CALL      'ANGC17R'
036400950411     C                   PARM                    KPJBA
036500950411     C                   MOVEL     KPJBU         XTABDS
036600950411     C     XTAERR        IFNE      '1'
036700950411     C                   MOVE      XTAKEY        WWPRV
036800950411     C                   ELSE
036900950411     C                   MOVE      *BLANK        WWPRV
037000950411     C                   ENDIF
037100950411     C*
037200950411     C     WWPRV         IFNE      *BLANK
037300950411     C                   MOVEL     WWPRV         D1PRV
037400950411     C                   ENDIF
037500950411     C* imposto pos. del cursore
037600950411     C                   Z-ADD     CURR          H1RIGA
037700950411     C                   Z-ADD     CURC          H1COLO
037800020201     C*
037900020201B0334C* FMTD1  - C.A.P.
038000020201  "  C     H1NMFL        WhenEq    'D1CAP'
038100020201  "  C                   Reset                   A801DS
038200020201  "  C                   Move      '01'          OPZ801
038300020201  "  C                   Move      XscSoc        Societa801
038400020201  "  C                   Move      D1Sta         Stato801
038500020201  "  C                   Z-Add     CURR          XTARIG
038600020201  "  C                   Z-Add     CURC          XTACOL
038700020201  "  C                   Clear                   KPJBU
038800020201  "  C                   Movel     A801DS        KPJBU
038900020201  "  C                   Call      'ANA801R'
039000020201  "  C                   Parm                    KPJBA
039100020201  "  C                   Movel     KPJBU         A801DS
039200020201  "  C     RET801        IfNe      '2'
039300020201  "  C                   Move      CAP801        D1CAP
039400020201  "  C                   If        D1Sta  =  *blank
039500020201  "  C                   Eval      D1Sta  =  Stato801
039600020201  "  C                   Endif
039700020201  "  C                   If        D1Prv  =  *blank
039800020201  "  C                   Eval      D1Prv  =  Prov801
039900020201  "  C                   Endif
040000020201  "  C                   If        D1Cit  =  *blank
040100020201  "  C                   Eval      D1Cit  =  Localit801
040200020201  "  C                   Endif
040300020201B0334C                   ENDIF
040400950411     C*
040500950411     C* FMTD1  - STATO
040600950411     C     H1NMFL        WHENEQ    'D1STA'
040700950411     C                   RESET                   XTABDS
040800950411     C                   MOVE      '01'          XTAOPZ
040900950411     C                   MOVE      XSCSOC        XTAAZI
041000950411     C                   MOVE      XSCLIN        XTALIN
041100950411     C                   Z-ADD     CURR          XTARIG
041200950411     C                   Z-ADD     CURC          XTACOL
041300950411     C                   CLEAR                   KPJBU
041400950411     C                   MOVEL     XTABDS        KPJBU
041500950411     C                   CALL      'ANGG12R'
041600950411     C                   PARM                    KPJBA
041700950411     C                   MOVEL     KPJBU         XTABDS
041800950411     C     XTAERR        IFNE      '1'
041900950411     C                   MOVE      XTAKEY        WWSTA
042000950411     C                   ELSE
042100950411     C                   MOVE      *BLANK        WWSTA
042200950411     C                   ENDIF
042300950411     C*
042400950411     C     WWSTA         IFNE      *BLANK
042500950411     C                   MOVEL     WWSTA         D1STA
042600950411     C                   ENDIF
042700950411     C* imposto pos. del cursore
042800950411     C                   Z-ADD     CURR          H1RIGA
042900950411     C                   Z-ADD     CURC          H1COLO
043000950411     C*
043100950411     C* FMTD1  - NATURA GIURIDICA
043200950411     C     H1NMFL        WHENEQ    'D1NAT'
043300950411     C                   RESET                   XTABDS
043400950411     C                   MOVE      '01'          XTAOPZ
043500950411     C                   MOVE      XSCSOC        XTAAZI
043600950411     C                   MOVE      XSCLIN        XTALIN
043700950411     C                   Z-ADD     CURR          XTARIG
043800950411     C                   Z-ADD     CURC          XTACOL
043900950411     C                   CLEAR                   KPJBU
044000950411     C                   MOVEL     XTABDS        KPJBU
044100950411     C                   CALL      'ANGG16R'
044200950411     C                   PARM                    KPJBA
044300950411     C                   MOVEL     KPJBU         XTABDS
044400950411     C     XTAERR        IFNE      '1'
044500950411     C                   MOVE      XTAKEY        WWNAT
044600950411     C                   ELSE
044700950411     C                   MOVE      *BLANK        WWNAT
044800950411     C                   ENDIF
044900950411     C*
045000950411     C     WWNAT         IFNE      *BLANK
045100950411     C                   MOVEL     WWNAT         D1NAT
045200950411     C                   ENDIF
045300950411     C* imposto pos. del cursore
045400950411     C                   Z-ADD     CURR          H1RIGA
045500950411     C                   Z-ADD     CURC          H1COLO
045600960206     C*
045700960206     C* FMTD1  - LEGENDA FORZATURA CD FISC / P.IVA
045800960206     C     H1NMFL        WHENEQ    'D1FPI'
045900960206     C                   RESET                   A964DS
046000960206     C                   MOVEL     '01'          OPZ964
046100960206     C                   MOVEL     'ANSOG000'    TRC964
046200960206     C                   MOVEL     REPFPI        CAM964
046300960206     C                   MOVEL     A964DS        KPJBU
046400960206     C                   CALL      'ANA964R'
046500960206     C                   PARM                    KPJBA
046600960206     C                   MOVEL     KPJBU         A964DS
046700960206     C*
046800960206     C     VAL964        IFNE      *BLANK
046900960206     C                   MOVEL     VAL964        D1FPI
047000960206     C                   ENDIF
047100960206     C*
047200960206     C* imposto pos. del cursore
047300960206     C                   Z-ADD     CURR          H1RIGA
047400960206     C                   Z-ADD     CURC          H1COLO
047500940223     C*
047600940223     C* ricerca non abilitata in questa posizione
047700941027     C                   OTHER
047800940926     C* imposto pos. del cursore
047900940926     C                   Z-ADD     CURR          H1RIGA
048000940926     C                   Z-ADD     CURC          H1COLO
048100940926     C                   SETON                                        9899
048200940127     C*
048300941027     C                   ENDSL
048400940127     C*
048500940506     C                   MOVE      *HIVAL        $LASTV
048600940127     C                   ENDSR
048700940225     C/EJECT
048800940225     C************************************************************
048900940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
049000940225     C************************************************************
049100940225     C     GESPIE        BEGSR
049200940225     C*
049300940225     C                   SELECT
049400940225     C*
049500940506     C     $GEST         WHENEQ    'D1'
049600940506     C     $ULKD1        IFEQ      0
049700940506     C* riempimento opzioni
049800940506     C                   EXSR      F24D1
049900940506     C                   ELSE
050000940506     C                   WRITE     FMTZ1
050100940506     C                   ENDIF
050200950411     C                   ENDSL
050300940225     C*
050400940225     C                   ENDSR
050500940131     C/EJECT
050600940131     C************************************************************
050700940510     C* GESTIONE VIDEO RECORD D1
050800940131     C************************************************************
050900940127     C     GESD1         BEGSR
051000940225     C* inizializzazione videata
051100941117   B1C     $INZD1        IFEQ      *ON
051200940127     C                   EXSR      INZD1
051300940223     C                   MOVE      *OFF          $INZD1
051400941117   E1C                   ENDIF
051500940225     C* emissione piede videata se proveniente da altra
051600940225     C* salvataggio valore formato di provenienza
051700941117   B1C     $LASTV        IFNE      'D1'
051800940207     C                   WRITE     FMTT1
051900940225     C                   EXSR      GESPIE
052000940202     C                   MOVE      $LASTV        $LASTG
052100940202     C                   MOVE      'D1'          $LASTV
052200941117   E1C                   ENDIF
052300940707     C*
052400940707     C* se esistono errori sulla videata
052500940707     C* emetto la write del formato a indicatori spenti per vedere
052600940707     C* le eventuali decodifiche
052700941117   B1C     *IN99         IFEQ      *ON
052800940707     C     $GEST         ANDEQ     'D1'
052900940707     C                   MOVEA     *IN           WIN
053000940707     C                   MOVE      *ALL'0'       IN5098           49
053100940707     C                   MOVEA     IN5098        *IN(50)
053200940707     C                   WRITE     FMTD1
053300940707     C                   MOVEA     WIN           *IN
053400941117   E1C                   ENDIF
053500940131     C*
053600941117   B1C                   SELECT
053700940203     C* Annullamento
053800950411    >C     OPZ058        WHENEQ    '04'
053900940224     C* Visualizzazione
054000950411    >C     OPZ058        OREQ      '05'
054100940203     C                   WRITE     FMTD1
054200940203     C                   EXFMT     PROTECT
054300940224     C* Immissione/Modifica/Copia
054400941117   X1C                   OTHER
054500940203     C                   EXFMT     FMTD1
054600941117   E1C                   ENDSL
054700940506     C* controllo abilitazione tasti funzione
054800940506     C*
054900940506     C                   EXSR      CTRKD1
055000940506     C*
055100941117   B1C     *IN99         IFEQ      *OFF
055200940131     C*
055300941117   B2C                   SELECT
055400940916     C* F2=Decodifica
055500940916     C     $TASTO        WHENEQ    F02
055600940916     C                   EXSR      F02D1
055700940131     C* F3=Fine
055800940506     C     $TASTO        WHENEQ    F03
055900940309     C                   EXSR      F03D1
056000940131     C* F4=Ricerca
056100940506     C     $TASTO        WHENEQ    F04
056200940127     C                   EXSR      SEARCH
056300940224     C* F5=Refresh
056400940506     C     $TASTO        WHENEQ    F05
056500940309     C                   EXSR      F05D1
056600940223     C* F8=Successivo
056700940506     C     $TASTO        WHENEQ    F08
056800940309     C                   EXSR      F08D1
056900940131     C* F12=Ritorno
057000940506     C     $TASTO        WHENEQ    F12
057100940309     C                   EXSR      F12D1
057200940315     C* F24=Altri tasti
057300940506     C     $TASTO        WHENEQ    F24
057400940315     C                   EXSR      F24D1
057500940131     C*
057600941117   X2C                   OTHER
057700940127     C                   EXSR      CTRD1
057800010214B0334 *
057900010214  "  C* se non c'è stato nessun errore emetto eventualmente la finestra
058000010214  "  C* di avvertimento codici doppi
058100010214  "  C                   if        *in99 = *Off
058200010214  "  C                   exsr      MsgDopp
058300010214B0334C                   endif
058400940203     C*
058500950412     C     OPZ058        IFNE      '05'
058600941117   B3C     $TASTO        IFEQ      F06
058700940207     C     *IN99         ANDEQ     *OFF
058800940224     C* eseguo aggiornamento
058900941214     C                   EXSR      AGGANA
059000940224     C* eseguo operazioni del dopo-aggiornamento
059100941117   B4C     *IN99         IFEQ      *OFF
059200940318     C                   EXSR      GESAGG
059300950412     C                   ENDIF
059400950412   E4C                   ENDIF
059500941117   E3C                   ENDIF
059600940203     C*
059700941117   E2C                   ENDSL
059800940224     C*
059900941117   E1C                   ENDIF
060000941027     C*
060100940117     C                   ENDSR
060200940506     C/EJECT
060300010214B0334C***********************************************************
060400010214  "  C* PGM PER GESTIONE MESSAGGIO avviso di duplicità codici
060500010214  "  C***********************************************************
060600010214B0334C     MsgDopp       BEGSR
060700010214     C*
060800020131b0334x***                if        (CdFDopp   =   *On   or
060900020131  "  x***                           PIVADopp  =   *On       )
061000020131  "  C                   if        (£CdFDopp = *On   OR
061100020131B0334C                              £PIVADopp = *On)
061200010214     C*
061300010214     C                   Clear                   XMsgDs
061400010214     C*
061500010214     C* se tipo codice fiscale doppio
061600020131B0334X***                if        CdFDopp   =   *On
061700020131B0334C                   if        £CdFDopp = *On
061800010214     C* 1a riga
061900010214     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0698'
062000010214     C*
062100010214     C                   CALLB     'XRTVM'
062200010214     C                   PARM                    IDMSG            27
062300010214     C                   PARM                    TXTMSG          100
062400010214     C                   PARM                    ERRMSG            1
062500010214     C*
062600010214     C     ERRMSG        IFNE      *ON
062700010214     C                   Movel     TxtMsg        MsgMs1
062800010214     C                   ELSE
062900010214     C                   Movel     *ALL'?'       MsgMs1
063000010214     C                   ENDIF
063100010214     C*
063200010214     C                   endif
063300010214     C*
063400010214     C* se codice partita IVA doppio
063500020131B0334X***                if        PIVADopp  =  *On
063600020131B0334C                   if        £PIVADopp = *On
063700010214     c* 2a riga
063800010214     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0705'
063900010214     C*
064000010214     C                   CALLB     'XRTVM'
064100010214     C                   PARM                    IDMSG            27
064200010214     C                   PARM                    TXTMSG          100
064300010214     C                   PARM                    ERRMSG            1
064400010214     C*
064500010214     C     ERRMSG        IFNE      *ON
064600010214     C                   MOVEL     TXTMSG        MsgMs2
064700010214     C                   ELSE
064800010214     C                   MOVEL     *ALL'?'       MsgMs2
064900010214     C                   ENDIF
065000010214     C*
065100010214     C                   endif
065200010214     C*
065300010214     C* 3a riga (sia per codice fiscale che per partita IVA)
065400010214     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS0613'
065500010214     C*
065600010214     C                   CALLB     'XRTVM'
065700010214     C                   PARM                    IDMSG            27
065800010214     C                   PARM                    TXTMSG          100
065900010214     C                   PARM                    ERRMSG            1
066000010214     C*
066100010214     C     ERRMSG        IFNE      *ON
066200010214     C                   Movel     TxtMsg        MsgMs3
066300010214     C                   ELSE
066400010214     C                   Movel     *ALL'?'       MsgMs3
066500010214     C                   ENDIF
066600010214     C*
066700010214     C                   Z-Add     0             MsgMsg
066800010214     C                   Movel     'N'           MsgRsp
066900010214     C                   Movel     *Blanks       MsgTpR
067000010214     C                   Movel     'S'           MsgEmV
067100010214     C                   Z-add     10            MsgRgp
067200010214     C                   Movel     'N'           MsgLck
067300010214     C                   Movea     Status        Sts
067400010214     C                   Movel     'N'           MsgCnl
067500010214     C                   Movel     'N'           MsgVid
067600010214     C                   Movel     'N'           MsgOpe
067700010214     C                   Movel     'N'           MsgStp
067800010214     C                   Call      'XMSGR'
067900010214     C                   Parm                    XMsgDs
068000010214      *
068100010214      * ctl l'abilitaz. all'immisione CodFisc/PIva doppi, solo se
068200010214      * il CodFisc e/o la PIva non appartengono al soggetto a video
068300010214      * (PIvSogOrige/CdFSogOrig sono valorizzati solo se si sono
068400010214      *  reperiti dei codici doppi tra le variazioni fiscali)
068500020131B0334X*** ex-B0316       If        (CdfDopp = *On  AND
068600020131  "  X***      "                    D1Cod  <> CdFSogOrig)    OR
068700020131  "  X***      "                   (PIvaDopp = *On  AND
068800020131  "  X*** ex-B0316                  D1Cod  <> PIvSogOrig)
068900031211B0334 *
069000031211B0657X*** ex-B0334       If        (£CdfDopp = *On  AND
069100031211  "  X*** ex-B0334                  D1Sog <> £CdFSogOrig)    OR
069200031211  "  X*** ex-B0334                 (£PIvaDopp = *On  AND
069300031211  "  X*** ex-B0334                  D1Sog <> £PIvSogOrig)
069400031211  "   *
069500031216  "   * Pjxx0402r restituisce £CdfDopp o £PIvaDopp = *on e
069600031216  "   * PIvSogOrige/CdFSogOrig valorizzati se si sono
069700031216  "   *  reperiti dei codici doppi tra le variazioni fiscali -ANCPI-;
069800031216  "   * oppure  PIvSogOrige/CdFSogOrig sono vuoti se
069900031216  "   *  si sono reperiti dei codici doppi in angr. soggetti -ANSOG-.
070000031216  "   *
070100031216  "  C                   If        ( (£CdfDopp = *on and
070200031216  "  C                                £CdFSogOrig = *blank)
070300031216  "  C                               or
070400031216  "  C                               (£CdfDopp = *on and
070500031216  "  C                                £CdFSogOrig <> *blank and
070600031216  "  C                                £CdFSogOrig <> D1Sog      ) )
070700031216  "  C                             OR
070800031216  "  C                             ( (£PIvaDopp = *on  and
070900031216  "  C                                £PIvSogOrig = *blank)
071000031216  "  C                               or
071100031216  "  C                               (£PIvaDopp = *on  and
071200031216  "  C                                £PIvSogOrig <> *blank and
071300031216  "  C                                £PIvSogOrig <> D1Sog      ) )
071400031211B0657 *
071500010214     C*
071600010214     C* se l'utente NON è abilitato all'immissione di cd fiscale e/o
071700010214     C* partita IVA doppi
071800050506B0802C* MA solo se lo ha modificato o immesso
071900050506  "  c                   if        D1Iva <> SavIva or
072000050506B0802c                             D1Cdf <> SavCdf
072100010214     C                   IF        ImmDup = *off
072200010214     C* allora accendo l'errore generico, la segnalazione l'ha già data
072300010214     C* la finestra
072400010214     C                   SETON                                        9994
072500010214     C                   ENDIF
072600050506B0802C                   ENDIF
072700010214      *
072800010214     C                   EndIf
072900010214      *
073000010214     C*
073100010214     C                   endif
073200010214     C*
073300010214B0334C                   ENDSR
073400940506     C************************************************************
073500940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
073600940506     C************************************************************
073700940506     C     CTRKD1        BEGSR
073800941205     C*
073900940506     C                   SETOFF                                       99
074000940506     C                   MOVEA     $FC1          $FC
074100940506     C                   EXSR      CTRKEY
074200940506     C     *IN99         IFEQ      *ON
074300940506     C                   SETON                                        97
074400940506     C                   ENDIF
074500941205     C*
074600940506     C                   ENDSR
074700941209     C/EJECT
074800941209     C************************************************************
074900941209     C* INIZIALIZZAZIONE VIDEATA DATI
075000941209     C************************************************************
075100940127     C     INZD1         BEGSR
075200940131     C*
075300940131     C                   CLEAR                   FMTD1
075400940225     C*
075500940225     C                   Z-ADD     1             $PIED1
075600950102     C*
075700950412   B1C     OPZ058        IFNE      '01'
075800950411     C                   MOVEL     SOG058        CPICOD
075900950411     C                   MOVEL     DTF058        CPIDTF
076000940318     C*
076100941201   B2C     *IN22         DOUEQ     *OFF
076200950411     C     MSGRTN        ORNE      'R'
076300940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
076400950411   B>C     OPZ058        IFEQ      '02'
076500950411     C     OPZ058        OREQ      '04'
076600950411     C     K2CPI1        CHAIN     ANCPI000                           2122
076700940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
076800941201   X3C                   ELSE
076900950411     C     K2CPI1        CHAIN(N)  ANCPI000                           2122
077000941201   E3C                   ENDIF
077100940318     C*
077200940318     C* Gestione RCD allocato
077300941201   B3C     *IN22         IFEQ      *ON
077400950411     C                   MOVEL     'ANCPI00F'    MSGFIL
077500940318     C                   EXSR      MSGPGM
077600941201   E3C                   ENDIF
077700941201   E2C                   ENDDO
077800940224     C*
077900940224     C* se il rcd da gestire non viene trovato
078000940318     C* oppure è allocato da altro pgm
078100940224     C* ritorno al pgm guida avvertendolo dell'errore
078200941201   B2C     *IN21         IFEQ      *ON
078300940318     C     *IN22         OREQ      *ON
078400940224     C                   MOVE      *ON           $FINE
078500950411     C                   MOVE      '1'           ERR058
078600940224     C*
078700940224     C* altrimenti
078800941201   X2C                   ELSE
078900950411     C* alloco in modo esclusivo i     rcd
079000950411   B3C     OPZ058        IFEQ      '02'
079100950411     C     OPZ058        OREQ      '04'
079200950411     C                   UPDATE    ANCPI000
079300950411   E3C                   ENDIF
079400940224     C* riempio la videata
079500940224     C                   EXSR      RIED1
079600950411    >C** Aggiungere le routine di riempimento dei record succcessivi
079700950411    >C*****                EXSR RIED2
079800940224     C* richiamo routine dei ctrl per decodificarla
079900940614     C                   EXSR      CTRD1
080000950411    >C** Aggiungere le routine di controllo dei record succcessivi
080100950411    >C*                    EXSR CTRD2
080200940224     C                   MOVE      *ALL'0'       IN5098
080300940224     C                   MOVEA     IN5098        *IN(50)
080400940224     C* definizione indicatori su video
080500940224     C                   SETOFF                                       02
080600950411     C                   SETOFF                                       04
080700941201   B3C                   SELECT
080800950412     C     OPZ058        WHENEQ    '02'
080900940224     C                   SETON                                        02        PR campi key
081000950411     C     OPZ058        WHENEQ    '04'
081100950412     C                   SETON                                        02        PR campi key
081200950412     C     OPZ058        WHENEQ    '05'
081300941201   E3C                   ENDSL
081400940224     C*
081500941201   E2C                   ENDIF
081600940223     C*
081700950102   X1C                   ELSE
081800950102     C*
081900950102     C* riempio con i default di immissione
082000950403     C                   EXSR      RIEDEF
082100950404     C*
082200950404     C* imposto i campi chiave ricevuti anche se si è in immissione
082300950404     C* perchè potrebbero essere stati passati dei default di chiave
082400950414     C     D1FPI         IFEQ      *BLANK
082500970515     C                   MOVEL     '0'           D1FPI
082600950414     C                   END
082700950414     C*
082800950411     C                   MOVEL     SOG058        D1SOG
082900950411     C*
083000950412   E1C                   ENDIF
083100940224     C*
083200940117     C                   ENDSR
083300940207     C/EJECT
083400940207     C************************************************************
083500940510     C* RIEMPIMENTO VIDEATA  D1
083600940207     C************************************************************
083700940207     C     RIED1         BEGSR
083800940207     C*
083900950301     C* impostare i campi della videata dai campi del file
084000950301     C*
084100950411     C                   MOVEL     CPICOD        D1SOG
084200020131     C                   CALLb     'XDT4000'
084300950411     C                   PARM      CPIDTF        XDTAMG           10
084400950411     C                   PARM                    XDTGMA            6 0
084500950411     C                   PARM      2             XDTSTA            1 0
084600950411     C                   MOVEL     XDTGMA        D1DTF
084700950411     C*
084800950411     C                   MOVEL     CPIIVA        D1IVA
084900970515     c                   move      cpifpi        d1fpi
085000950411     C                   MOVEL     CPICDF        D1CDF
085100950411     C                   MOVEL     CPIDE1        D1DES1
085200950411     C                   MOVEL     CPIDE2        D1DES2
085300950411     C                   MOVEL     CPICIA        D1CIA
085400950411     C                   MOVEL     CPINAT        D1NAT
085500950411     C                   MOVEL     CPIVIA        D1VIA
085600950411     C                   MOVEL     CPICIT        D1CIT
085700950411     C                   MOVEL     CPICAP        D1CAP
085800950411     C                   MOVEL     CPIPRV        D1PRV
085900950411     C                   MOVEL     CPISTA        D1STA
086000050506     c*
086100050506B0802C                   If        opz058 <> '03'
086200050506  "  C                   eval      SavIva=d1Iva
086300050506  "  C                   eval      SavCdf=d1Cdf
086400050506B0802c                   endif
086500940207     C*
086600940207     C                   ENDSR
086700950403     C/EJECT
086800950403     C************************************************************
086900950403     C* RIEMPIMENTO CAMPI DAI DEFAULT
087000950403     C************************************************************
087100950403     C     RIEDEF        BEGSR
087200950403     C*
087300950411    >C* riempire i campi con i default recuperati dal codice di
087400950411    >C* riferimento
087500950412     C*
087600950412     C                   MOVEL     SOGIVA        D1IVA
087700950412     C                   MOVEL     SOGCDF        D1CDF
087800970515     c                   move      sogfpi        d1fpi
087900950412     C                   MOVEL     SOGDES        D1DES1
088000950412     C                   MOVEL     SOGDE1        D1DES2
088100950412     C                   MOVEL     SOGCIA        D1CIA
088200950412     C                   MOVEL     SOGNAT        D1NAT
088300950412     C                   MOVEL     INDVIA        D1VIA
088400950412     C                   MOVEL     INDCIT        D1CIT
088500950412     C                   MOVEL     INDCAP        D1CAP
088600950412     C                   MOVEL     INDPRV        D1PRV
088700950412     C                   MOVEL     INDSTA        D1STA
088800950412     C*
088900950412     C     DTF058        IFGT      *ZEROS
089000020201B0334C     DTF058        ANDNE     $DT9
089100020131     C                   CALLb     'XDT4000'
089200950412     C                   PARM      DTF058        XDTAMG           10
089300950412     C                   PARM                    XDTGMA            6 0
089400950412     C                   PARM      2             XDTSTA            1 0
089500950412     C                   MOVEL     XDTGMA        D1DTF
089600950412     C                   ELSE
089700000114A0735C*****              Z-ADD     UDATE         D1DTF
089800000114A0735C                   Z-ADD     ZDate         D1DTF
089900950412     C                   END
090000950412     C*
090100950403     C*
090200950403     C                   ENDSR
090300940916     C/EJECT
090400940916     C************************************************************
090500940916     C* GESTIONE F02 VIDEO D1
090600940916     C************************************************************
090700940916     C     F02D1         BEGSR
090800940916     C*
090900940916     C* per decodificare la videata richiamo la routine dei ctrl
091000940916     C* e poi disattivo gli eventuali messaggi di errore
091100940916     C                   EXSR      CTRD1
091200940916     C*
091300940916     C                   MOVE      *ALL'0'       IN5098
091400940916     C                   MOVEA     IN5098        *IN(50)
091500940916     C*
091600940916     C                   ENDSR
091700940309     C/EJECT
091800940309     C************************************************************
091900940309     C* GESTIONE F03 VIDEO D1
092000940309     C************************************************************
092100940309     C     F03D1         BEGSR
092200940309     C*
092300940309     C                   MOVE      *ON           $FINE
092400950411     C                   MOVE      '1'           RET058
092500940325     C* fine programma
092600940325     C                   EXSR      ENDPGM
092700940325     C*
092800940325     C                   ENDSR
092900940309     C/EJECT
093000940309     C************************************************************
093100940309     C* GESTIONE F05 VIDEO D1
093200940309     C************************************************************
093300940309     C     F05D1         BEGSR
093400940309     C*
093500940309     C                   MOVE      *ON           $INZD1
093600940309     C*
093700940309     C                   ENDSR
093800940309     C/EJECT
093900940309     C************************************************************
094000940309     C* GESTIONE F08 VIDEO D1
094100940309     C************************************************************
094200940309     C     F08D1         BEGSR
094300940309     C*
094400940309     C                   MOVE      *ON           $FINE
094500950411    >C                   MOVE      '0'           RET058
094600940309     C*
094700940309     C                   ENDSR
094800940309     C/EJECT
094900940309     C************************************************************
095000940309     C* GESTIONE F12 VIDEO D1
095100940309     C************************************************************
095200940309     C     F12D1         BEGSR
095300940309     C*
095400940309     C                   MOVE      *ON           $FINE
095500950411    >C                   MOVE      '2'           RET058
095600940309     C*
095700940309     C                   ENDSR
095800940309     C/EJECT
095922940309     C************************************************************
096000940309     C* GESTIONE F24 VIDEO D1
096100940309     C************************************************************
096200940309     C     F24D1         BEGSR
096300940506     C*
096400940506     C                   MOVEA     $FX1          $FX
096500940506     C                   MOVEA     $FC1          $FC
096600940506     C                   MOVEA     $FR1          $FR
096700940506     C                   Z-ADD     $ULKD1        $ULK
096800940506     C                   EXSR      RIEKEY
096900940506     C                   Z-ADD     $ULK          $ULKD1            3 0
097000940720     C                   MOVEA     $FC           $FC1
097100940506     C                   MOVEL     $KEY1         Z1KE1
097200060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
097300940506     C                   MOVEL     $KEY2         Z1KE2
097400940506     C                   WRITE     FMTZ1
097500940506     C*
097600940309     C                   ENDSR
097700940207     C/EJECT
097800940131     C************************************************************
097900940207     C* CONTROLLO VIDEATA
098000940131     C************************************************************
098100940127     C     CTRD1         BEGSR
098200940131     C*
098300940127     C                   SETOFF                                       99
098400940127     C* non imposto pos. del cursore
098500940127     C                   Z-ADD     0             H1RIGA
098600940127     C                   Z-ADD     0             H1COLO
098700940223     C*
098800940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
098900940223     C*
099000950411     C     OPZ058        IFNE      '04'
099100020201B0334X***  OPZ058        ORNE      '05'
099200020131      *
099300020131B0334 * Questa utility è richiamata dalla PJXX0402 che fa
099400020131  "   * da intermediario, restituendo la schiera dei campi
099500020131  "   * che sono stati trovati errati
099600020131  "  C                   eval      £Soggetto = D1Sog
099700020131  "  C                   eval      £RagSoc = D1Des1
099800020131  "  C                   eval      £Indirizzo = D1Via
099900020131  "  C                   eval      £Provincia = D1Prv
100000020131  "  C                   eval      £Stato = D1Sta
100100020131  "  C                   eval      £CAP = D1CAP
100200020131  "  C                   eval      £Localita = D1Cit
100300020131  "  C                   eval      £NatGiur = D1Nat
100400020131  "  C                   eval      £FlagForz = D1FPI
100500020131  "  C                   eval      £CodFisc = D1CdF
100600020131  "  C                   eval      £PartIVA = D1IVA
100700020131  "  C                   eval      £CdFDopp = *Off
100800031209B0334C                   eval      £PIVADopp = *Off
100900031209B0657X** ex B0334        eval      £CdFSogOrig = *Off
101000031209  "  X** ex B0334        eval      £PIVSogOrig = *Off
101100031209  "  C                   eval      £CdFSogOrig = *blank
101200031209B0657C                   eval      £PIVSogOrig = *blank
101300031209B0334C                   eval      £DesProvincia = *blank
101400020131  "  C                   eval      £DesStato = *blank
101500020131  "  C                   eval      £DesNatGiur = *blank
101600020131  "  C                   eval      £DesFlagForz = *blank
101700020131  "  C                   eval      £CtrDatiFisc = *Off
101800020131  "   *
101900020131  "  C                   callb     'PJXX0402R'
102000020308B0334C                   parm                    SkCampiErr
102100020308B0350 *
102200020308  "   * Reimposto il codice soggetto con quello allineato
102300020308  "  C                   eval      D1Sog = £Soggetto
102400020308B0350C
102500020308B0334 *
102600020131  "   * In base ai campi che sono stati trovati errati, si
102700020131  "   * accendono gli indicatori per il reverse image sui
102800020131  "   * campi video.
102900020131  "  C                   exsr      AccendiIndRI
103000020131  "  C*
103100020131  "  C* Si valorizzano inoltre le eventuali decodifiche
103200020131  "   * reperite dal modulo dei controlli
103300020131  "  C                   exsr      ValDecod
103400020131  "  C*
103500010214  "  X*** CTRL OBBLIGATORIETA' SOGGETTO
103600010214  "  X***  D1SOG         IFEQ      *BLANK
103700010214  "  X***                SETON                                        5099
103800010214  "  X***                ELSE
103900010214  "  X***                CALL      'XALLINEA'
104000010214  "  X***                PARM                    D1SOG
104100010214  "  X***                PARM      8             LEN               5 0
104200010214  "  X***                PARM                    XSCINP
104300010214  "  X***                ENDIF
104400010214  "  X***
104500010214  "  X*** CTRL OBBLIGATORIETA' RAGIONE SOCIALE
104600010214  "  X***  D1DES1        IFEQ      *BLANK
104700010214  "  X***                SETON                                        5199
104800010214  "  X***                ENDIF
104900010214  "  X***
105000010214  "  X*** CTRL OBBLIGATORIETA' INDIRIZZO
105100010214  "  X***  D1VIA         IFEQ      *BLANK
105200010214  "  X***                SETON                                        5299
105300010214  "  X***                ENDIF
105400010214  "  X***
105500010214  "  X*** CTRL OBBLIGATORIETA' LOCALITA'
105600010214  "  X***  D1CIT         IFEQ      *BLANK
105700010214  "  X***                SETON                                        5399
105800010214  "  X***                ENDIF
105900010214  "  X***
106000010214  "  X*** CTRL VALIDITA' PROVINCIA
106100010214  "  X***                MOVEL     *BLANK        D1PRVD
106200010214  "  X***  D1PRV         IFNE      *BLANK
106300010214  "  X***                MOVE      '1'           XTBRIC
106400010214  "  X***                MOVE      XSCSOC        XTBAZI
106500010214  "  X***                MOVE      *ZERO         XTBKEY
106600010214  "  X***                MOVE      'C17'         XTBCOD
106700010214  "  X***                MOVE      D1PRV         WRK06A            6
106800010214  "  X***                MOVEL     D1STA         WRK06A
106900010214  "  X***                MOVE      WRK06A        XTBKEY
107000010214  "  X***                CALL      'XATB'
107100010214  "  X***                PARM                    XATBDS
107200010214  "  X***  XTBERR        IFEQ      '0'
107300010214  "  X***                MOVEL     XTBUNI        TABC17
107400010214  "  X***                MOVEL     DESC17        D1PRVD
107500010214  "  X***                ELSE
107600010214  "  X***                SETON                                        6599
107700010214  "  X***                ENDIF
107800010214  "  X***                ENDIF
107900010214  "  X***
108000010214  "  X*** CTRL OBBLIGATORIETA' STATO
108100010214  "  X***  D1STA         IFEQ      *BLANK
108200010214  "  X***                SETON                                        5499
108300010214  "  X***
108400010214  "  X*** CTRL VALIDITA' STATO
108500010214  "  X***                ELSE
108600010214  "  X***                MOVEL     *BLANK        D1STAD
108700010214  "  X***                MOVE      '1'           XTBRIC
108800010214  "  X***                MOVE      XSCSOC        XTBAZI
108900010214  "  X***                MOVE      *ZERO         XTBKEY
109000010214  "  X***                MOVE      'G12'         XTBCOD
109100010214  "  X***                MOVE      D1STA         XTBKEY
109200010214  "  X***                CALL      'XATB'
109300010214  "  X***                PARM                    XATBDS
109400010214  "  X***  XTBERR        IFEQ      '0'
109500010214  "  X***                MOVEL     XTBUNI        TABG12
109600010214  "  X***                MOVEL     DESG12        D1STAD
109700010214  "  X***
109800010214  "  X*** CTRL NR.CARATTERI DEL CAP
109900010214  "  X***  D1CAP         IFNE      *BLANK
110000010214  "  X***  ' '           CHECKR    D1CAP         X
110100010214  "  X***  X             IFNE      CAPG12
110200010214  "  X***                SETON                                        7399
110300010214  "  X***                ENDIF
110400010214  "  X***                ENDIF
110500010214  "  X***
110600010214  "  X***                ELSE
110700010214  "  X***                SETON                                        6699
110800010214  "  X***                ENDIF
110900010214  "  X***                ENDIF
111000010214  "  X***
111100010214  "  X*** CTRL VALIDITA' FLAG FORZATURA CD FISC / P.IVA
111200010214  "  X***                MOVEL     *BLANK        D1FPID
111300010214  "  X***  D1FPI         IFNE      *BLANK
111400010214  "  X***                RESET                   X07DS
111500010214  "  X***                MOVE      '1'           X07RIC
111600010214  "  X***                MOVEL     'ANSOG000'    X07TRC
111700010214  "  X***                MOVEL     REPFPI        X07CAM
111800010214  "  X***                MOVEL     D1FPI         X07VAL
111900010214  "  X***                CALL      'X07VALR'
112000010214  "  X***                PARM                    X07DS
112100010214  "  X***  X07ERR        IFEQ      '0'
112200010214  "  X***                MOVEL     X07DES        D1FPID
112300010214  "  X***                ELSE
112400010214  "  X***                SETON                                        7299
112500010214  "  X***                ENDIF
112600010214  "  X***                ELSE
112700010214  "  X***                SETON                                          7299
112800010214  "  X***                ENDIF
112900010214  "  X***
113000010214  "  X*** CTRL VALIDITA' CODICE FISCALE
113100010214  "  X***  D1CDF         IFNE      *BLANK
113200010214  "  X*** SE NON RICHIESTA FORZATURA
113300010214  "  X***  D1FPI         ANDNE     '1'
113400010214  "  X***  D1FPI         ANDNE     '3'
113500010214  "  X***                CALL      'XCDFIS'
113600010214  "  X***                PARM                    D1CDF
113700010214  "  X***                PARM                    XSTATO            1 0
113800010214  "  X*** SE RILEVATO UN ERRORE
113900010214  "  X***  XSTATO        IFLT      0
114000010214  "  X*** CTRL VALIDITA' CD FISCALE COME PARTITA IVA
114100010214  "  X***                CALL      'XPARIVA'
114200010214  "  X***                PARM                    D1CDF
114300010214  "  X***                PARM                    XSTATO            1 0
114400010214  "  X***                PARM                    XScsoc
114500010214  "B3X***  XSTATO        IFLT      0
114600010214  "  X***                SETON                                        5799
114700010214  "  X***                ENDIF
114800010214  "  X***                ENDIF
114900010214  "  X***                ENDIF
115000010214  "  X***
115100010214  "  X*** SE NON RICHIESTA FORZATURA P.IVA
115200010214  "B1X***  D1FPI         IFNE      '2'
115300010214  "  X***  D1FPI         ANDNE     '3'
115400010214  "  X***
115500010214  "  X*** CTRL OBBLIGATORIETA' PARTITA IVA
115600010214  "  X***  D1IVA         IFEQ      *BLANK
115700010214  "  X***
115800010214  "  X***                SETON                                        5899
115900010214  "  X***
116000010214  "  X***                ELSE
116100010214  "  X***
116200010214  "  X*** CTRL VALIDITA' FORMALE PARTITA IVA
116300010214  "  X***                CALL      'XPARIVA'
116400010214  "  X***                PARM                    D1IVA
116500010214  "  X***                PARM                    XSTATO            1 0
116600010214  "  X***                PARM                    XScsoc
116700010214  "  X***  XSTATO        IFLT      0
116800010214  "  X***                SETON                                        6099
116900010214  "  X***                ENDIF
117000010214  "E2X***                ENDIF
117100010214  "E1X***                ENDIF
117200010214  "  X***
117300010214  "  X*** CTRL OBBLIGATORIETA' NATURA GIURIDICA
117400010214  "  X***                MOVEL     *BLANK        D1NATD
117500010214  "  X***  D1NAT         IFEQ      *BLANK
117600010214  "  X*** SOLO SE LO STATO DEL SOGGETTO E' LO STESSO DELLA SOCIETA'
117700010214  "  X***  D1STA         IFEQ      XSCSTA
117800010214  "  X***                SETON                                        6199
117900010214  "  X***                ENDIF
118000010214  "  X***
118100010214  "  X*** CTRL VALIDITA' NATURA GIURIDICA
118200010214  "  X***                ELSE
118300010214  "  X***                MOVE      '1'           XTBRIC
118400010214  "  X***                MOVE      XSCSOC        XTBAZI
118500010214  "  X***                MOVE      *ZERO         XTBKEY
118600010214  "  X***                MOVE      'G16'         XTBCOD
118700010214  "  X***                MOVE      D1NAT         XTBKEY
118800010214  "  X***                CALL      'XATB'
118900010214  "  X***                PARM                    XATBDS
119000010214  "  X***  XTBERR        IFEQ      '0'
119100010214  "  X***                MOVEL     XTBUNI        TABG16
119200010214  "  X***                MOVEL     DESG16        D1NATD
119300010214  "  X***                ELSE
119400010214  "  X***                SETON                                        6299
119500010214  "  X***                ENDIF
119600010214B0334X***                ENDIF
119700020131     c* queste specifiche sono state spostate
119800020131     c* qui
119900020131     c*   CTRL OBBLIOGATORIETA' DATA :
120000020131   B3c     D1DTF         IFEQ      *ZERO
120100020131     c                   SETON                                        6999
120200020131     c                   ELSE
120300020131     c*   CTRL VALIDITA' DATA
120400020131     c                   CALLb     'XDT4000'
120500020131     c                   PARM                    XDTAMG           10
120600020131     c                   PARM      D1DTF         XDTGMA            6 0
120700020131     c                   PARM      3             XDTSTA            1 0
120800020131   B4c     XDTSTA        IFNE      *ZERO
120900020131     c                   SETON                                        7099
121000020131   X4c                   ELSE
121100020131     c                   MOVE      XDTAMG        DTFAMG
121200020131   E4c                   ENDIF
121300020131     c                   ENDIF
121400010214     C*
121500950411     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
121600950411     C     OPZ058        IFEQ      '01'
121700950411     C     OPZ058        OREQ      '03'
121800950912     C* SOLO SE LA DATA FINE VALIDITA' E' CORRETTA
121900950912     C     *IN69         IFEQ      *OFF
122000950912     C     *IN70         ANDEQ     *OFF
122100950912     C                   MOVEL     D1SOG         CPICOD
122200950411     C                   MOVEL     DTFAMG        CPIDTF
122300950411     C     K2CPI1        SETLL     ANCPI000                               21
122400950411     C     *IN21         IFEQ      '1'
122500950411     C                   SETON                                        5999
122600950411     C                   ENDIF
122700950912     C                   ENDIF
122800950411     C                   ENDIF
122900950411     C*
123000950411     C*
123100950411     C                   ENDIF
123200940117     C                   ENDSR
123300020131B0334C************************************************************
123400020131  "  C* Accendo indicatori per il reverse image a video dei
123500020131  "  C* campi errati
123600020131  "  C************************************************************
123700020131B0334C     AccendiIndRI  begsr
123800020131      *
123900020131     C                   eval      Ind = 1
124000020131     C                   movea     Spegni5       *in(50)
124100020131     C                   movea     Spegni2       *in(57)
124200020131     C                   movea     Spegni2       *in(72)
124300020131     C                   setoff                                       6165
124400020131      *
124500020131     C* Per ogni campo trovato errato accendo un indicatore
124600020131     C                   dow       Ind <= DimSk
124700020131     C                   if        SkCampiErr(Ind) <> *blank
124800020131      *
124900020131     C                   select
125000020131      * soggetto
125100020131     C                   when      SkCampiErr(Ind) = 'SOGG'
125200020131     C                   seton                                        5099
125300020131      * ragione sociale
125400020131     C                   when      SkCampiErr(Ind) = '$RA1'
125500020131     C                   seton                                        5199
125600020131      * indirizzo
125700020131     C                   when      SkCampiErr(Ind) = 'INDIRIZ'
125800020131     C                   seton                                        5299
125900020131      * CAP
126000020131     C                   when      SkCampiErr(Ind) = 'CAPNUMC'
126100020131     C                   seton                                        7399
126200020131     C                   when      SkCampiErr(Ind) = 'CAP'
126300020131     C                   seton                                        7599
126400020131     C                   when      SkCampiErr(Ind) = 'CAPOBB'
126500020131     C                   seton                                        7899
126600020131      * località
126700020131     C                   when      SkCampiErr(Ind) = 'LOCALIT'
126800020131     C                   seton                                        5399
126900020131      * provincia
127000020131     C                   when      SkCampiErr(Ind) = 'PROV'
127100020131     C                   seton                                        6599
127200020131      * stato
127300020131     C                   when      SkCampiErr(Ind) = 'STATOOBB'
127400020131     C                   seton                                        5499
127500020131     C                   when      SkCampiErr(Ind) = 'STATO'
127600020131     C                   seton                                        6699
127700020131      * natura giuridica
127800020131     C                   when      SkCampiErr(Ind) = 'NATGIUROBB'
127900020131     C                   seton                                        6199
128000020131     C                   when      SkCampiErr(Ind) = 'NATGIUR'
128100020131     C                   seton                                        6299
128200020131      * codice fiscale
128300020131     C                   when      SkCampiErr(Ind) = 'CDFISC'
128400020131     C                   seton                                        5799
128500020131     C                   when      SkCampiErr(Ind) = 'CDFISCOBB'
128600020131     C                   seton                                        7499
128700020131      * partita IVA
128800020131     C                   when      SkCampiErr(Ind) = 'PARTIVAOBB'
128900020131     C                   seton                                        5899
129000020131     C                   when      SkCampiErr(Ind) = 'PARTIVA'
129100020131     C                   seton                                        6099
129200020131      * forzatura codice fiscale/partita IVA
129300020131     C                   when      SkCampiErr(Ind) = 'FRCPIVA'
129400020131     C                   seton                                        7299
129500020131      *
129600020131     C                   endsl
129700020131      *
129800020131      * al primo campo della schiera non valorizzato esco
129900020131      * dal ciclo
130000020131     C                   else
130100020131     C                   leave
130200020131     C                   endif
130300020131      *
130400020131     C                   eval      Ind = Ind + 1
130500020131      *
130600020131     C                   enddo
130700020131      *
130800020131B0334C                   endsr
130900020131B0334C************************************************************
131000020131  "  C* Valorizzo eventuali decodifiche reperite dal driver
131100020131  "  C* dei controlli (PJXX0401R)
131200020131  "  C************************************************************
131300020131B0334C     ValDecod      begsr
131400020131     C*
131500020131     C* Provincia
131600020131     C                   eval      D1PrvD = £DesProvincia
131700020131     C* Stato
131800020131     C                   eval      D1StaD = £DesStato
131900020131     C* Natura giuridica
132000020131     C                   eval      D1NatD = £DesnatGiur
132100020131     C* Forzatura codice fiscale/partita IVA
132200020131     C                   eval      D1FPID = £DesFlagForz
132300020131     C*
132400020131B0334C                   endsr
132500940207     C/EJECT
132600950411     C************************************************************
132700950411     C* AGGIORNAMENTO
132800950411     C************************************************************
132900950411     C     AGGANA        BEGSR
133000950411     C*
133100950411     C* se l'opzione non è di scrittura
133200950411     C* devo rileggere il rcd in questione perchè per allocarlo in
133300950411     C* modo esclusivo ho già fatto l'UPDATE
133400950411     C     OPZ058        IFNE      '01'
133500950411     C     OPZ058        ANDNE     '03'
133600950411     C                   MOVEL     SOG058        CPICOD
133700950411     C                   MOVEL     DTF058        CPIDTF
133800950411     C     K2CPI1        CHAIN     ANCPI000                           21
133900950411     C                   ENDIF
134000950411     C*
134100950411     C                   SELECT
134200950411     C     OPZ058        WHENEQ    '01'
134300950411     C     OPZ058        OREQ      '03'
134400950411     C                   EXSR      RIEANA
134500950411     C                   WRITE     ANCPI000                             22
134600950411     C* se rcd non scrivibile attivo errore
134700950411     C     *IN22         IFEQ      *ON
134800950411     C                   SETON                                        9599
134900950411     C                   ENDIF
135000950411     C     OPZ058        WHENEQ    '02'
135100950411     C******     OPZ058    OREQ '04'
135200950411     C                   EXSR      RIEANA
135300950411     C                   UPDATE    ANCPI000
135400950411     C     OPZ058        WHENEQ    '04'
135500950412     C                   DELETE    ANCPI000
135501100901D0525 *
135502100901R027 C                   Eval      SO3Sogg    = CPICod
135503100901  "  C                   Eval      SO3DtFiVl  = CPIDTF
135504100901  "  C                   Eval      SO3Sys     = 0
135505100901  "  C                   Eval      SO3NrAsInt = 0
135506100901  "  C     K04SO301      SETLL     SDGSO301L
135507160503R027 C     K04SO301      READE     SDGSO301L                              21
135508160503R224 X*ex R027           if        *in21 = *Off
135509160503R224 C                   DoW       *in21 = *Off
135510160503R027 C                   delete    SDGSO3000
135511160503R224 X*ex R027           endif
135512160503R224 C     K04SO301      READE     SDGSO301L                              21
135513160503R224 C                   endDo
135514100901R027 C*
135515100901R027 C***********        EXSR      CallDatiAgg
135600950411     C                   ENDSL
135700950411     C*
135800950411     C* richiama pgm User Exit
135900950411     C*
136000950411     C     USRERR        IFEQ      *BLANK
136100950411     C* imposta chiave univoca  per user exit
136200950411     C                   MOVE      XSCSOC        XUKSOC
136300950411     C                   MOVE      XSCDUN        XUKDUN
136400950411     C                   MOVEL     NOMPGM        XUKPGM
136500950411    >C                   MOVEL     CPICOD        XUKFL1
136600950411    >C                   MOVEL     *BLANK        XUKFL2
136700950411    >C                   MOVEL     *BLANK        XUKFL3
136800950411    >C                   MOVEL     XUKDS         KPJBU
136900950411     C     NOMPGM        CAT       'U':0         USREXT           10
137000950411     C                   CALL      USREXT                               21
137100950411     C                   PARM                    KPJBA
137200950411     C     *IN21         IFEQ      *ON
137300950411     C                   MOVE      *ON           USRERR            1
137400950411     C                   ENDIF
137500950411     C                   ENDIF
137600950411     C*
137700950411     C                   ENDSR
137800950411     C/EJECT
137801160503R224 X*ex R027 ***************************************************
137802160503  "  X*ex R027 Richiamo pgm per dati anagrafici aggiuntivi
137803160503  "  X*ex R027 ***************************************************
137804160503  "  X*ex R027 CallDatiAgg   BEGSR
137805160503  "  X*ex R027
137806160503  "  X*ex R027           RESET                   SDGSD16DS
137807160503  "  X*ex R027
137808160503  "  X*ex R027           MOVEL     '04'          OPZD16
137809160503  "  X*ex R027 CMT058        IFEQ      '1'
137810160503  "  X*ex R027           MOVEL     '1'           CMTD16
137811160503  "  X*ex R027           ELSE
137812160503  "  X*ex R027           MOVEL     '0'           CMTD16
137813160503  "  X*ex R027           ENDIF
137814160503  "  X*ex R027
137817160503  "  X*ex R027           Eval      SoggD16     = Sog058
137818160503  "  X*ex R027           MOVEL     DTF058        DtFiVlD16
137820160503  "  X*ex R027           Eval      SysD16      = 0
137821160503  "  X*ex R027           Eval      NrAsIntD16  = 0
137822160503  "  X*ex R027
137823160503  "  X*ex R027           MOVEL     SDGSD16DS     KPJBU
137824160503  "  X*ex R027
137825160503  "  X*ex R027           CALL      'SDGSD16R'
137826160503  "  X*ex R027           PARM                    KPJBA
137827160503  "  X*ex R027
137828160503  "  X*ex R027           MOVEL     KPJBU         SDGSD16DS
137829160503  "  X*ex R027
137830160503  "  X*ex R027 ritorno da PGM gestione
137831160503  "  X*ex R027           EXSR      RTND16
137832160503  "  X*ex R027
137833160503  "  X*ex R027           ENDSR
137834160503  "  X*ex R027 /EJECT
137835160503  "  X*ex R027 ***************************************************
137836160503  "  X*ex R027 GESTIONE RITORNO DA PGM DI GESTIONE
137837160503  "  X*ex R027****************************************************
137838160503  "  X*ex R027 RTND16        BEGSR
137839160503  "  X*ex R027
137840160503  "  X*ex R027 modo di ritorno
137841160503  "  X*ex R027
137842160503  "  X*ex R027           SELECT
137843160503  "  X*ex R027 questi modi di utilizzo dei valori di ritorno dal
137844160503  "  X*ex R027 pgm di manutenzione rcd di anagrafica sono delle
137845160503  "  X*ex R027 proposte, normalmente sempre valide, ma modificabili
137846160503  "  X*ex R027 per situazioni particolari
137847160503  "  X*ex R027 1 = F3
137848160503  "  X*ex R027 RETD16        WHENEQ    '1'
137849160503  "  X*ex R027           MOVE      *ON           $FINE                          fine pgm
137850160503  "  X*ex R027           MOVE      '1'           RET058
137851160503  "  X*ex R027 2 = F12
137852160503  "  X*ex R027 RETD16        WHENEQ    '2'
137853160503  "  X*ex R027 se il pgm di manutenzione ha prima una guida, è necessario anche
137854160503  "  X*ex R027 questo test, altrimenti prevale il retD04 e il sfl non viene
137855160503  "  X*ex R027 ricaricato
137856160503  "  X*ex R027 OPRD16        ANDNE     '1'
137857160503  "  X*ex R027
137858160503  "  X*ex R027           ENDSL
137859160503  "  X*ex R027
137860160503  "  X*ex R027 funzione non eseguibile per errore :
137861160503  "  X*ex R027           SELECT
137862160503  "  X*ex R027 1 = funzione richiamata chiusa in errore
137863160503  "  X*ex R027 ERRD16        WHENEQ    '1'
137864160503  "  X*ex R027           SETON                                        9399      emiss.SFLMSGID
137865160503  "  X*ex R027 2 = funzione richiamata senza la dovuta autorizzazione
137866160503  "  X*ex R027 ERRD16        WHENEQ    '2'
137867160503  "  X*ex R027           SETON                                        9499      emiss.SFLMSGID
137868160503  "  X*ex R027
137869160503  "  X*ex R027           ENDSL
137870160503  "  X*ex R027
137871160503  "  X*ex R027           ENDSR
137872160503R224 X*ex R027 /EJECT
137900940224     C************************************************************
138000940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
138100940224     C************************************************************
138200941214     C     RIEANA        BEGSR
138300940224     C*
138400950411     C                   CLEAR                   ANCPI000
138500950301     C*
138600950301     C* imposto i campi del file da quelli video
138700950301     C*
138800950411     C                   MOVEL     D1SOG         CPICOD
138900950411     C                   MOVEL     DTFAMG        CPIDTF
139000950411     C                   MOVEL     D1IVA         CPIIVA
139100970515     c                   move      d1fpi         cpifpi
139200950411     C                   MOVEL     D1CDF         CPICDF
139300950411     C                   MOVEL     D1DES1        CPIDE1
139400950411     C                   MOVEL     D1DES2        CPIDE2
139500950411     C                   MOVEL     D1CIA         CPICIA
139600950411     C                   MOVEL     D1NAT         CPINAT
139700950411     C                   MOVEL     D1VIA         CPIVIA
139800950411     C                   MOVEL     D1CIT         CPICIT
139900950411     C                   MOVEL     D1CAP         CPICAP
140000950411     C                   MOVEL     D1PRV         CPIPRV
140100950411     C                   MOVEL     D1STA         CPISTA
140200940207     C*
140300940207     C                   ENDSR
140400940224     C/EJECT
140500940224     C************************************************************
140600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
140700940224     C************************************************************
140800940224     C     GESAGG        BEGSR
140900940224     C*
141000940224     C* segnalo al pgm chiamante l'avvenuta conferma
141100950411     C                   MOVE      *ON           OPR058
141200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
141300950411     C                   MOVE      '0'           RET058
141400940224     C*
141500941027     C                   SELECT
141600950301     C*
141700950301     C* nel caso di immissione
141800950411     C     OPZ058        WHENEQ    '01'
141900950301     C* ripristino la videata iniziale
142000950912     C                   MOVE      *ON           $FINE
142100940224     C                   MOVE      *ON           $INZD1
142200941228     C                   MOVE      'D1'          $GEST
142201100901R027 C                   MOVEL     CPICOD        SOG058
142202100901R027 C                   MOVE      CPIDTF        DTF058
142300950301     C*
142400950301     C* nel caso di modifica
142500950411     C     OPZ058        WHENEQ    '02'
142600950301     C* ripasso i campi che devono essere reimpostati sul sfl
142700950411     C                   MOVEL     CPIIVA        IVA058
142800950411     C                   MOVEL     CPICDF        CDF058
142900950301     C* e ritorno al pgm chiamante
143000950301     C                   MOVE      *ON           $FINE
143001100901R027 C*
143002100901 "   C* nel caso di copia
143003100901 "   C     OPZ058        WHENEQ    '03'
143004100901 "   C                   MOVEL     CPICOD        SOG058
143005100901R027 C                   MOVE      CPIDTF        DTF058
143100950301     C*
143200950301     C* per tutte le altre opzioni
143300940224     C                   OTHER
143400950301     C* ritorno al pgm chiamante
143500940224     C                   MOVE      *ON           $FINE
143600941027     C                   ENDSL
143700940224     C*
143800940224     C                   ENDSR
143900941205     C/EJECT
144000941205     C************************************************************
144100941205     C* CONTROLLO ABILITAZIONE TASTI
144200941205     C************************************************************
144300941205     C     CTRKEY        BEGSR
144400941205     C*
144500941205     C                   SELECT
144600941205     C     $TASTO        WHENEQ    F01
144700941205     C     $FC(1)        IFEQ      '0'
144800941205     C                   SETON                                        99
144900941205     C                   ENDIF
145000941205     C     $TASTO        WHENEQ    F02
145100941205     C     $FC(2)        IFEQ      '0'
145200941205     C                   SETON                                        99
145300941205     C                   ENDIF
145400941205     C     $TASTO        WHENEQ    F03
145500941205     C     $FC(3)        IFEQ      '0'
145600941205     C                   SETON                                        99
145700941205     C                   ENDIF
145800941205     C     $TASTO        WHENEQ    F04
145900941205     C     $FC(4)        IFEQ      '0'
146000941205     C                   SETON                                        99
146100941205     C                   ENDIF
146200941205     C     $TASTO        WHENEQ    F05
146300941205     C     $FC(5)        IFEQ      '0'
146400941205     C                   SETON                                        99
146500941205     C                   ENDIF
146600941205     C     $TASTO        WHENEQ    F06
146700941205     C     $FC(6)        IFEQ      '0'
146800941205     C                   SETON                                        99
146900941205     C                   ENDIF
147000941205     C     $TASTO        WHENEQ    F07
147100941205     C     $FC(7)        IFEQ      '0'
147200941205     C                   SETON                                        99
147300941205     C                   ENDIF
147400941205     C     $TASTO        WHENEQ    F08
147500941205     C     $FC(8)        IFEQ      '0'
147600941205     C                   SETON                                        99
147700941205     C                   ENDIF
147800941205     C     $TASTO        WHENEQ    F09
147900941205     C     $FC(09)       IFEQ      '0'
148000941205     C                   SETON                                        99
148100941205     C                   ENDIF
148200941205     C     $TASTO        WHENEQ    F10
148300941205     C     $FC(10)       IFEQ      '0'
148400941205     C                   SETON                                        99
148500941205     C                   ENDIF
148600941205     C     $TASTO        WHENEQ    F11
148700941205     C     $FC(11)       IFEQ      '0'
148800941205     C                   SETON                                        99
148900941205     C                   ENDIF
149000941205     C     $TASTO        WHENEQ    F12
149100941205     C     $FC(12)       IFEQ      '0'
149200941205     C                   SETON                                        99
149300941205     C                   ENDIF
149400941205     C     $TASTO        WHENEQ    F13
149500941205     C     $FC(13)       IFEQ      '0'
149600941205     C                   SETON                                        99
149700941205     C                   ENDIF
149800941205     C     $TASTO        WHENEQ    F14
149900941205     C     $FC(14)       IFEQ      '0'
150000941205     C                   SETON                                        99
150100941205     C                   ENDIF
150200941205     C     $TASTO        WHENEQ    F15
150300941205     C     $FC(15)       IFEQ      '0'
150400941205     C                   SETON                                        99
150500941205     C                   ENDIF
150600941205     C     $TASTO        WHENEQ    F16
150700941205     C     $FC(16)       IFEQ      '0'
150800941205     C                   SETON                                        99
150900941205     C                   ENDIF
151000941205     C     $TASTO        WHENEQ    F17
151100941205     C     $FC(17)       IFEQ      '0'
151200941205     C                   SETON                                        99
151300941205     C                   ENDIF
151400941205     C     $TASTO        WHENEQ    F18
151500941205     C     $FC(18)       IFEQ      '0'
151600941205     C                   SETON                                        99
151700941205     C                   ENDIF
151800941205     C     $TASTO        WHENEQ    F19
151900941205     C     $FC(19)       IFEQ      '0'
152000941205     C                   SETON                                        99
152100941205     C                   ENDIF
152200941205     C     $TASTO        WHENEQ    F20
152300941205     C     $FC(20)       IFEQ      '0'
152400941205     C                   SETON                                        99
152500941205     C                   ENDIF
152600941205     C     $TASTO        WHENEQ    F21
152700941205     C     $FC(21)       IFEQ      '0'
152800941205     C                   SETON                                        99
152900941205     C                   ENDIF
153000941205     C     $TASTO        WHENEQ    F22
153100941205     C     $FC(22)       IFEQ      '0'
153200941205     C                   SETON                                        99
153300941205     C                   ENDIF
153400941205     C     $TASTO        WHENEQ    F23
153500941205     C     $FC(23)       IFEQ      '0'
153600941205     C                   SETON                                        99
153700941205     C                   ENDIF
153800941205     C     $TASTO        WHENEQ    F24
153900941205     C     $FC(24)       IFEQ      '0'
154000941205     C                   SETON                                        99
154100941205     C                   ENDIF
154200941205     C                   ENDSL
154300941205     C*
154400941205     C                   ENDSR
154500940318     C/EJECT
154600940318     C******************************************************
154700940322     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
154800940318     C******************************************************
154900940318     C     MSGPGM        BEGSR
155000940318     C*
155100940318     C                   Z-ADD     2             MSGMSG
155200940318     C                   MOVEL     'S'           MSGRSP
155300940318     C                   MOVEL     'CR'          MSGTPR
155400940318     C                   MOVEL     'S'           MSGEMV
155500940318     C                   Z-ADD     10            MSGRGP
155600940318     C                   MOVEL     'S'           MSGLCK
155700940318     C                   MOVEA     STATUS        STS
155800940318     C                   MOVEL     'N'           MSGCNL
155900940318     C                   MOVEL     'N'           MSGVID
156000940318     C                   MOVEL     'N'           MSGOPE
156100940318     C                   MOVEL     'N'           MSGSTP
156200940318     C                   CALL      'XMSGR'
156300940318     C                   PARM                    XMSGDS
156400940318     C*
156500940318     C                   ENDSR
156600940207     C/EJECT
156700940131     C************************************************************
156800940131     C* OPERAZIONI INIZIALI
156900940131     C************************************************************
157000940131     C     *INZSR        BEGSR
157100000114A0735C     *DMY          Move      Udate         ZDateISO
157200000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
157300940131     C*
157400940127     C* Reperimento parametri
157500940127     C*
157600940117     C     *ENTRY        PLIST
157700940117     C                   PARM                    KPJBA
157800950410     C                   PARM                    DSAUT
157900950410      *
158000950410     C     PARMS         IFLT      2
158100950410     C                   MOVE      *BLANK        AUTDS
158200950410     C                   ELSE
158300950410     C                   MOVEL     DSAUT         AUTDS
158400950410     C                   ENDIF
158500940506     C*
158600940506     C* Reperimento tasti di funzione
158700940506     C*
158800940506     C                   EXSR      REPKEY
158900940506     C*
159000940223     C                   ENDSR
159100940223     C/EJECT
159200940506     C************************************************************
159300940506     C* REPERIMENTO TASTI DI FUNZIONE
159400940506     C************************************************************
159500940506     C     REPKEY        BEGSR
159600940506     C                   MOVEA     $FM1          $FM
159700950906     C                   MOVEA     $FC1          $FC
159800940506     C                   EXSR      REPKKK
159900940506     C                   MOVEA     $FX           $FX1
160000940614     C*
160100940614     C                   ENDSR
160200940506     C/EJECT
160300940506     C************************************************************
160400940506     C* REPERIMENTO TASTI DI FUNZIONE
160500940506     C************************************************************
160600940506     C     REPKKK        BEGSR
160700940506     C*
160800940506     C                   CLEAR                   $FX
160900940506     C     1             DO        24            X
161000940506     C*
161100940506     C     $FM(X)        IFNE      *BLANK
161200950906     C     $FC(X)        ANDNE     '0'
161300940506     C*
161400940506     C                   MOVEL     $FM(X)        IDMSG
161500020131     C                   CALLB     'XRTVM'
161600940506     C                   PARM                    IDMSG            27
161700940506     C                   PARM                    TXTMSG          100
161800940506     C                   PARM                    ERRMSG            1
161900940506     C*
162000940506     C     ERRMSG        IFNE      *ON
162100940506     C                   MOVEL     TXTMSG        $FX(X)
162200940506     C                   ELSE
162300940506     C                   MOVEL     *ALL' '       $FX(X)
162400941027     C                   ENDIF
162500940506     C*
162600941027     C                   ENDIF
162700940506     C*
162800941027     C                   ENDDO
162900940506     C*
163000940506     C                   ENDSR
163100940509     C/EJECT
163200940223     C************************************************************
163300940223     C* INIZIALIZZAZIONE VARIABILI
163400940223     C************************************************************
163500940223     C     INZVAR        BEGSR
163600941215     C*
163700941215     C* Dati da PGM guida
163800941215     C*
163900950411    >C                   MOVEL     KPJBU         A058DS
164000941201     C*
164100941201     C* Reperimento dati società
164200941201     C*
164300941115    >C                   MOVEL     'SOC001'      TIPXSC
164400941115     C                   MOVEL     *BLANK        SOCXSC
164500941115     C                   EXSR      REPSOC
164600941115     C     RTNXSC        IFNE      '1'
164700941115     C                   MOVEL     XSOCDS        SOC001
164800941115     C                   ELSE
164900941115     C                   RETURN
165000941115     C                   ENDIF
165100950301     C*
165200950301     C* Reperimento autorizzazioni
165300950301     C*
165400950411     C*  nel caso l'anagrafica non gestisse le autorizzazioni
165500950411     C*   asteriscare la chiamata alla subroutine REPAUT
165600950301     C* le autorizzazioni potrebbero essere già state passate,
165700950301     C* nel qual caso non vanno reperite
165800950411     C     AUTDS         IFEQ      *BLANK
165900950411     C                   EXSR      REPAUT
166000950411     C                   ELSE
166100950411     C                   MOVEL     XCADDS        GSOGDS
166200950411     C                   ENDIF
166300950301     C*
166400950301     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
166500950301     C* le autorizzazione
166600950301     C*
166700950411     C                   EXSR      TSTAUT
166800950301     C*
166900950411     C                   MOVEL     *BLANK        XPARIC
167000950411     C                   MOVEL     XSCSOC        XPASOC
167100950411     C                   MOVEL     'SICOM   '    XPAPAR
167200950411     C                   MOVEL     *BLANK        XPAOUT
167300950411     C                   EXSR      REPPAR
167400950411     C     XPAERR        IFEQ      '0'
167500950411     C                   MOVEL     XPAOUT        SICOM
167600950411     C                   ELSE
167700950411     C                   RETURN
167800950411     C                   ENDIF
167900940715     C*
168000940715     C* Definizione dei tasti funzione utilizzabili a pgm
168100940715     C*
168200940715     C                   EXSR      DEFOPZ
168300940128     C*
168400940223     C* Variabili per gestione videate
168500940128     C*
168600940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
168700940223     C                   MOVE      *OFF          $FINE                          fine pgm
168800940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
168900940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
169000940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
169100940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
169200940127     C*
169300940223     C* Variabili appoggio
169400940127     C*
169500940224     C                   Z-ADD     0             CURR
169600940224     C                   Z-ADD     0             CURC
169700050506B0802C                   clear                   SavIva
169800050506B0802C                   clear                   SavCdf
169900940207     C*
170000940207     C* Valorizzazione campi univoci testate
170100950414     C                   MOVE      XSCON         SI
170200950414     C                   MOVE      XSCOFF        NO
170300940207     C*
170400940207     C                   CLEAR                   FMTT1
170500940207     C                   MOVEL     KNSIF         NOMSIF
170600941110     C                   MOVEL     XSCDSI        NOMDIT
170700941027     C                   MOVEL     DSPGM         NOMPGM
170800950331     C* recupero il tipo operazione
170900950331     C                   Z-ADD     1             X
171000950411     C     OPZ058        LOOKUP    $OZ(X)                                 21
171100950331     C     *IN21         IFEQ      *ON
171200950331     C                   MOVEL     $ID(X)        IDMSG
171300020131     C                   CALLB     'XRTVM'
171400950331     C                   PARM                    IDMSG            27
171500950331     C                   PARM                    TXTMSG          100
171600950331     C                   PARM                    ERRMSG            1
171700950331     C                   PARM      *ON           CTRMSG            1            centratura
171800950331     C                   PARM      30            LENMSG            3 0          lun output.
171900950331     C     ERRMSG        IFNE      *ON
172000950331     C                   MOVEL     TXTMSG        T1OPE
172100950331     C                   ELSE
172200950331     C                   MOVEL     *ALL'?'       T1OPE
172300950331     C                   ENDIF
172400950331     C                   ENDIF
172500950403     C* Reperisco e memorizzo default in caso di immissione
172600950403     C*
172700950411    >C* azzero tutti i campi di appoggio default
172800950411     C     OPZ058        IFEQ      '01'
172900950411    >C* costruisco la chiave univoca per il reperimento dei default
173000950411    >C                   MOVE      SOG058        SOGCOD
173100950411     C     SOGCOD        CHAIN     ANSOG01L                           21
173200950411    >C                   MOVE      SOG058        INDCOD
173300950411     C     INDCOD        CHAIN     ANIND01L                           21
173400950403     C     *IN21         IFEQ      *OFF
173500950403     C                   EXSR      REPDEF
173600950403     C                   ENDIF
173700950403     C                   ENDIF
173800950412     C*
173900020131B0334c                   eval      £CdFDopp  =    *Off
174000020131  "  c                   eval      £PIVADopp =    *Off
174100020131  "  c                   eval      £PIvSogOrig  = *Blank
174200020131B0334c                   eval      £CdFSogOrig  = *Blank
174300010214     C*
174400950412     C* Inizializzazione/Decodifica prima videata
174500950412     C*
174600950412     C                   EXSR      INZD1
174700950412     C                   MOVE      *OFF          $INZD1
174800950412     C*
174900940127     C*
175000940117     C                   ENDSR
175100941209     C/EJECT
175200941209     C************************************************************
175300941209     C* REPERIMENTO DATI SOCIETA'
175400941209     C************************************************************
175500941209     C     REPSOC        BEGSR
175600941209     C*
175700020131     C                   CALLB     'XSOC'
175800941209     C                   PARM                    TIPXSC            6
175900941209     C                   PARM                    SOCXSC            3
176000941209     C                   PARM                    CDSXSC            9 0
176100941209     C                   PARM                    MODXSC            3
176200941209     C                   PARM      *BLANKS       RTNXSC            1
176300941209     C                   PARM                    XSOCDS
176400941209     C                   PARM                    KPJBA
176500941209     C*
176600941209     C                   ENDSR
176700950301     C/EJECT
176800950301     C************************************************************
176900950301     C* REPERIMENTO AUTORIZZAZIONI UTENTE
177000950301     C************************************************************
177100950301     C     REPAUT        BEGSR
177200950301     C*
177300950301     C                   CLEAR                   AUTDS
177400950301     C*
177500950301     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
177600950301     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
177700950301     C                   Z-ADD     0             XCAGRP                         GRUPPO
177800950301     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
177900950411     C                   MOVEL     'ANGSOG'      XCAFNC                         FUNZIONE
178000950411     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
178100950301    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
178200950301    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
178300950301     C*
178400950301     C                   CALL      'XCHKAUT'
178500950301     C                   PARM                    AUTDS
178600950301     C*
178700950301     C* errori/non abilitazione
178800950301     C     XCARTN        IFNE      0
178900980818     C                   MOVE      '2'           Err058
179000980818     c                   MOVE      *ON           $FINE
179100950301     C                   EXSR      ENDPGM
179200950301     C                   ELSE
179300950411     C                   MOVEL     XCADDS        GSOGDS
179400950301     C                   ENDIF
179500950301     C*
179600950301     C                   ENDSR
179700950301     C/EJECT
179800950301     C************************************************************
179900950301     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
180000950301     C************************************************************
180100950301     C     TSTAUT        BEGSR
180200950301     C*
180300950301     C                   SELECT
180400950301     C*
180500950301     C* se richiamato in immissione
180600950411     C     OPZ058        WHENEQ    '01'
180700950301     C* ma l'utente non è abilitato
180800950411     C     IMMSOG        ANDEQ     *OFF
180900950301     C* esco da pgm avvertendolo
181000950411     C                   MOVE      '2'           ERR058
181100950301     C                   MOVE      *ON           $FINE
181200950301     C                   EXSR      ENDPGM
181300950301     C*
181400950301     C* se richiamato in modifica
181500950411     C     OPZ058        WHENEQ    '02'
181600950301     C* ma l'utente non è abilitato
181700950411     C     MANSOG        ANDEQ     *OFF
181800950301     C* testo se l'utente è abilitato almeno alla visualizzazione
181900950411     C     VISSOG        IFEQ      *ON
182000950411     C                   MOVE      '05'          OPZ058
182100950301     C                   ELSE
182200950301     C* altrimenti esco da pgm avvertendolo
182300950411     C                   MOVE      '2'           ERR058
182400950301     C                   MOVE      *ON           $FINE
182500950301     C                   EXSR      ENDPGM
182600950301     C                   ENDIF
182700950301     C*
182800950301     C* se richiamato in copia
182900950411     C     OPZ058        WHENEQ    '03'
183000950301     C* ma l'utente non è abilitato
183100950411     C     IMMSOG        ANDEQ     *OFF
183200950301     C* esco da pgm avvertendolo
183300950411     C                   MOVE      '2'           ERR058
183400950301     C                   MOVE      *ON           $FINE
183500950301     C                   EXSR      ENDPGM
183600950301     C*
183700950301     C* se richiamato in annullamento
183800950411     C     OPZ058        WHENEQ    '04'
183900950301     C* ma l'utente non è abilitato
184000950411     C     ANNSOG        ANDEQ     *OFF
184100950301     C* esco da pgm avvertendolo
184200950411     C                   MOVE      '2'           ERR058
184300950301     C                   MOVE      *ON           $FINE
184400950301     C                   EXSR      ENDPGM
184500950301     C*
184600950301     C* se richiamato in visualizzazione
184700950411     C     OPZ058        WHENEQ    '05'
184800950301     C* ma l'utente non è abilitato
184900950411     C     VISSOG        ANDEQ     *OFF
185000950301     C* esco da pgm avvertendolo
185100950411     C                   MOVE      '2'           ERR058
185200950301     C                   MOVE      *ON           $FINE
185300950301     C                   EXSR      ENDPGM
185400950301     C*
185500950301     C                   ENDSL
185600950301     C*
185700950301     C                   ENDSR
185800950301     C/EJECT
185900950301     C************************************************************
186000950301     C* REPERIMENTO PARAMETRI GESTIONE
186100950301     C************************************************************
186200950301     C     REPPAR        BEGSR
186300950301     C*
186400020131     C                   CALLB     'XPAR'
186500950301     C                   PARM                    XPASOC            3
186600950301     C                   PARM                    XPAPAR            8
186700950301     C                   PARM                    XPAOUT
186800950301     C                   PARM                    XPAERR            1
186900950301     C                   PARM                    XPARIC            1
187000950301     C*
187100950301     C                   ENDSR
187200940715     C/EJECT
187300940715     C************************************************************
187400940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
187500940715     C************************************************************
187600940715     C     DEFOPZ        BEGSR
187700940715     C*
187800940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
187900940715     C* con S o N a seconda della utilizzabilità o meno
188000940715     C* del tasto funzione correlato
188100940715     C*
188200940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
188300940715     C* di compilazione
188400940715     C                   RESET                   $FC1
188500940715     C                   RESET                   $FR1
188600940715     C*
188700940715     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
188800940715     C* invalido alcune opzioni
188900940715     C                   SELECT
189000940715     C* Se pgm richiamato in interrogazione disattivo :
189100950411     C     OPZ058        WHENEQ    '05'
189200940715     C* F4=Ricerca
189300940715     C                   Z-ADD     4             X
189400940720     C                   MOVE      '0'           $FC1(X)
189500940715     C* F5=Ripristino
189600940715     C                   Z-ADD     5             X
189700940720     C                   MOVE      '0'           $FC1(X)
189800940715     C* F6=Conferma
189900940715     C                   Z-ADD     06            X
190000940720     C                   MOVE      '0'           $FC1(X)
190100940715     C* Se pgm richiamato in annullamento disattivo :
190200950411     C     OPZ058        WHENEQ    '04'
190300940715     C* F4=Ricerca
190400940715     C                   Z-ADD     4             X
190500940720     C                   MOVE      '0'           $FC1(X)
190600940715     C* F5=Ripristino
190700940715     C                   Z-ADD     5             X
190800940720     C                   MOVE      '0'           $FC1(X)
190900940715     C*
191000940715     C                   OTHER
191100940715     C*
191200940715     C                   ENDSL
191300940715     C*
191400940715     C                   ENDSR
191500950403     C/EJECT
191600950403     C************************************************************
191700950403     C* REPERIMENTO CAMPI DI DEFAULT
191800950403     C************************************************************
191900950403     C     REPDEF        BEGSR
192000950403     C*
192100950411    >C* riempire dei campi di appoggio con tutti quei valori
192200950411    >C* che possono essere usati come default
192300950403     C*
192400950403     C                   ENDSR
192500940128     C************************************************************
192600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
192700940223     C************************************************************
192800940223     C     DEFCAM        BEGSR
192900940223     C*
193000940223     C* klist
193100940207     C*
193200950411     C* ANCPI01L
193300950411     C     K2CPI1        KLIST
193400950411     C                   KFLD                    CPICOD
193500950411     C                   KFLD                    CPIDTF
193501100901R027 C*
193502100901  "  C* SDGSO301L
193503100901  "  C     K04SO301      KLIST
193504100901  "  C                   KFLD                    SO3Sogg
193505100901  "  C                   KFLD                    SO3DtFiVL
193506100901  "  C                   KFLD                    SO3Sys
193507100901R027 C                   KFLD                    SO3NrAsint
193600940223     C*
193700940223     C* Variabili per gestione videate
193800940223     C*
193900940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
194000940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
194100940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
194200940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
194300940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
194400940225     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
194500940223     C*
194600940223     C* Variabili appoggio
194700950411     C*
194800950411     C     *LIKE         DEFINE    INDPRV        WWPRV
194900950411     C     *LIKE         DEFINE    INDSTA        WWSTA
195000950411     C     *LIKE         DEFINE    SOGNAT        WWNAT
195100950411     C     *LIKE         DEFINE    CPIDTF        DTFAMG
195200950414     C                   MOVE      *BLANK        SI                1
195300950414     C                   MOVE      *BLANK        NO                1
195400940223     C*
195500940223     C                   Z-ADD     0             CURR              2 0
195600940223     C                   Z-ADD     0             CURC              2 0
195700940223     C                   MOVE      *ZEROS        WIN              99
195800950411    >C* elenco tutti i campi di appoggio default
195900940223     C*
196000940223     C* Indici
196100940223     C*
196200940223     C                   Z-ADD     0             X                 3 0
196300940223     C                   Z-ADD     0             Y                 3 0
196400940127     C*
196500940117     C                   ENDSR
196600940509     C/EJECT
196700940509     C************************************************************
196800940509     C* RIEMPIMENTO TASTI FUNZIONE
196900940509     C************************************************************
197000940509     C     RIEKEY        BEGSR
197100940509     C*
197200940509     C* Input :
197300940509     C* - $FX = schiera con tutti i testi disponibili
197400940509     C* - $FC = schiere con i flag di validità associati ai testi
197500940509     C* - $FR = schiere con i flag di REVERSE IMAGE
197600940509     C* - $ULK = ultimo testo caricato
197700940722     C* - $L01 = Lunghezza prima riga
197800940722     C* - $L02 = Lunghezza seconda riga
197900940509     C* Output :
198000940509     C* - $ULK
198100940609     C* - $KEY1= 1° riga di tasti funzione
198200940609     C* - $KEY2= 2° riga di tasti funzione
198300940509     C*
198400020131     C                   CALLB     'XKEYMSG'
198500940509     C* input
198600940509     C                   PARM                    $FX
198700940509     C                   PARM                    $FC
198800940509     C                   PARM                    $FR
198900940509     C                   PARM                    $ULK              3 0
199000940509     C* output
199100940509     C                   PARM                    $KEY1            79
199200940509     C                   PARM                    $KEY2            79
199300940722    >C* input
199400941116    >C                   PARM      79            $L01              3 0
199500941116    >C                   PARM      79            $L02              3 0
199600940509     C*
199700060224A1148C                   PARM                    $ALLFUNCT       480
199800060224A1148C*
199900940509     C                   ENDSR
200000950906**
20010095040401PROMSG    *LIBL     COS0001
20020095040402PROMSG    *LIBL     COS0002
20030095040403PROMSG    *LIBL     COS0003
20040095040404PROMSG    *LIBL     COS0004
20050095040405PROMSG    *LIBL     COS0005
200600940509** TASTI DI FUNZIONE  D1
200700940509PROMSG    *LIBL     KEY0001  01
200800940509PROMSG    *LIBL     KEY0002  02
200900940509PROMSG    *LIBL     KEY0003  03
201000940509PROMSG    *LIBL     KEY0004  04
201100940509PROMSG    *LIBL     KEY0005  05
201200940509PROMSG    *LIBL     KEY0006  06
201300940509PROMSG    *LIBL     KEY0007  07
201400940509PROMSG    *LIBL     KEY0008  08
201500940509PROMSG    *LIBL     KEY0009  09
201600940509PROMSG    *LIBL     KEY0010  10
201700940509PROMSG    *LIBL     KEY0011  11
201800940509PROMSG    *LIBL     KEY0012  12
201900940509PROMSG    *LIBL     KEY0013  13
202000940509PROMSG    *LIBL     KEY0014  14
202100940509PROMSG    *LIBL     KEY0015  15
202200940509PROMSG    *LIBL     KEY0016  16
202300940509PROMSG    *LIBL     KEY0017  17
202400940509PROMSG    *LIBL     KEY0018  18
202500940509PROMSG    *LIBL     KEY0019  19
202600940509PROMSG    *LIBL     KEY0020  20
202700940509PROMSG    *LIBL     KEY0021  21
202701100831PROMSG    *LIBL     KEY0022  22
202900940509PROMSG    *LIBL     KEY0023  23
203000940509PROMSG    *LIBL     KEY0024  24
203100940509**  ABILITAZIONE D1
2032009407200N  01
2033009407200N  02
2034009407201N  03
2035009407201N  04
2036009407201N  05
2037009407201N  06
2038009407200N  07
2039009407201N  08
2040009407200N  09
2041009411160N  10
2042009407200N  11
2043009407201N  12
2044009407200N  13
2045009407200N  14
2046009407200N  15
2047009407200N  16
2048009407200N  17
2049009407200N  18
2050009407200N  19
2051009407200N  20
2052009407200N  21
2053001008310N  22
2054009509060N  23   Impostare a '1' se si utilizzano opzioni
2055009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
