000100140129     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
000200140129     H CVTOPT(*DATETIME)
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940127      *----------*
000500940317    > * STD002R  *
000600940127      *----------*
000700940117      *
000800940613      *            GESTIONE TABELLA
000900940223      *
001000940223      *
001100940615      * DOCUMENTI MICROANALISI :
001200940223      *
001300940223      * NOTE TECNICHE :
001400940223      *
001500940223      *
001600940614      * PERSONALIZZAZIONI
001700940223      *
001800940223      *
001900940223      *
002000940223      *               MAPPA INDICATORI STD
002100940223      *
002200940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002300940620      *  03           PROTECT TUTTI I CAMPI
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
004100980909$001 FANGY4ZD2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
004400020412     Fanope01l  IF   E           K DISK
004401060724     Fanatb02l  IF   E           K DISK
004402060724     Ftabel00f  IF   E           K DISK
004500020412     FANRCO99J  IF   E           K DISK
004600980909     FANent01L  IF   E           K DISK
004700980909     FANVCI01L  IF   E           K DISK
004800140129     F*ANVCI02L  IF   E           K DISK    RENAME(ANVCI000:ANVCI002)
004900980909     FAnRiv02L  IF   E           K DISK
005000940128     D*----------------------------------------------------*
005100940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005200940224     D $ID             S             27    DIM(5) ALT($OZ)
005300940506     D* Schiere per la gestione dei tasti di funzione
005400940506     D $FM             S             27    DIM(24)
005500940506     D $FX             S             50    DIM(24)
005600940506     D $FC             S              1    DIM(24)
005700940506     D $FR             S              1    DIM(24)
005800940506     D* Video D1
005900940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006000940506     D $FX1            S             50    DIM(24) ALT($FM1)
006100940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006200940506     D $FR1            S              1    DIM(24) ALT($FC1)
006300980909$003 D EXTDS         E DS                  EXTNAME(ANGY4ZDS) INZ
006400940127     D* Passaggio Parametri
006500940127     D KPJBA         E DS
006600980910     D* Richiamo a XTAB
006700980910    >D XTABDS        E DS                  INZ
006800980910     D*-------------
006900980910     D* Parametri
007000980910     D X06DS         E DS                  EXTNAME(X06G48DS)
007100980910     D  X06SOC       E                     EXTFLD(X06SOCIETA)
007200980910     D  X06CIV       E                     EXTFLD(X06CDIVA)
007300980910     D  X06DAT       E                     EXTFLD(X06DATA)
007400980910     D  X06LIN       E                     EXTFLD(X06LINGUA)
007500980909     D*-------------
007600980909     D* Ds x ricerche anagrafiche
007700060724     D angy4zds      E DS                  INZ prefix(£)
007701060724     D ana200ds      E DS                  INZ
007800980909     D ana250ds      E DS                  INZ
007900980909     D ana280ds      E DS                  INZ
008000980909     D ana064ds      E DS                  INZ
008100980910     D Ana032ds      E DS                  INZ
008200980910     D TABG48        E DS                  EXTNAME(ANGG48DS) INZ
008300941103     D*-------------
008400941103      * DS Società
008500941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008600960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
008700960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
008800941103     D*-------------
008900941103      * DS Interna per dati utente
009000941103     D XSOCDS          DS          1000
009100941103     D*-------------
009200940616     D XATBDS        E DS                  INZ
009300940318     D*-------------
009400020412     D* DS Esterna causali contabili
009500020412     D ANA070DS      E DS
009600940318     D* DS Esterna gestione messaggi per COMIT
009700940616     D XMSGDS        E DS                  INZ
009800940318     D  STS          E                     EXTFLD(MSGSTS)
009900940318     D                                     DIM(333)
010000940318     D  JBA          E                     EXTFLD(MSGJBA)
010100940318     D                                     DIM(502)
010200940203     D*-------------
010300940620     D* DS Esterna autorizzazioni tabelle
010400940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
010500940620     D*
010600940620     D* DS  restituzione autorizzazioni
010700940620     D* IND=indice
010800940620     D* GRU=man. di gruppo
010900940620     D* AZI=man. azienda
011000940712     D* ABI=abilit. ai riservati
011100941111     D AUTDS           DS
011200940620     D  MANIND                 1      1
011300940620     D  MANGRU                 2      2
011400940620     D  MANAZI                 3      3
011500940712     D  ABIRIS                 4      4
011600940620     D*-------------
011700940207     D* opzione richiesta
011800940614    >D D002DS          DS
011900940614     D  OPZ002                 1      2
012000940614     D* 0 = ritorno normale
012100940614     D* 1 = F3
012200940614     D* 2 = F12
012300940614     D  RET002                 3      3
012400940614     D* operazione eseguite dal pgm chiamato :
012500940614     D* 0 = nessuna operazione
012600940614     D* 1 = eseguito aggiornamento
012700940614     D  OPR002                 4      4
012800940614     D* funzione non eseguibile per errore :
012900940614     D* 0 = nessun errore
013000940614     D* 1 = funzione richiamata chiusa in errore
013100940614     D* Aggiungere eventuali altri codici di errore
013200940614     D  ERR002                 5      5
013300940614     D* Azienda
013400940614     D  AZI002                 6      8
013500940614     D* Lingua
013600940614     D  LIN002                 9      9
013700940614     D* Codice scelto
013800940614     D  KEY002                10     24
013900940620     D* Autorizzazione
014000940620     D  AUT002                25     25
014100970509     D* Unita
014200970509     D  UNI002                26     33
014300940614     D*-------------
014400940201     D*-------------
014500940201     D DSFMT           DS
014600940506     D  $TASTO               369    369
014700940201     D  NRG                  370    370
014800940201     D  NCL                  371    371
014900940201     D*-------------
015000940201     D* posizione cursore
015100940201     D CURSOR          DS
015200940223     D  RIRI                   1      2B 0 INZ
015300940201     D  R$$                    2      2
015400940223     D  COCO                   3      4B 0 INZ
015500940201     D  C$$                    4      4
015600940207     D*-------------
015700940207     D* Reperimento nome PGM
015800940207     D STATUS         SDS           333
015900940207     D  DSPGM            *PROC
016000940225     D*-------------
016100940225     D* COSTANTI
016200940225     D*-------------
016300940506     D*
016400940506     D* Tasti di funzione
016500940506     D F01             C                   CONST(X'31')
016600940506     D F02             C                   CONST(X'32')
016700940506     D F03             C                   CONST(X'33')
016800940506     D F04             C                   CONST(X'34')
016900940506     D F05             C                   CONST(X'35')
017000940506     D F06             C                   CONST(X'36')
017100940506     D F07             C                   CONST(X'37')
017200940506     D F08             C                   CONST(X'38')
017300940506     D F09             C                   CONST(X'39')
017400940506     D F10             C                   CONST(X'3A')
017500940506     D F11             C                   CONST(X'3B')
017600940506     D F12             C                   CONST(X'3C')
017700940506     D F13             C                   CONST(X'B1')
017800940506     D F14             C                   CONST(X'B2')
017900940506     D F15             C                   CONST(X'B3')
018000940506     D F16             C                   CONST(X'B4')
018100940506     D F17             C                   CONST(X'B5')
018200940506     D F18             C                   CONST(X'B6')
018300940506     D F19             C                   CONST(X'B7')
018400940506     D F20             C                   CONST(X'B8')
018500940506     D F21             C                   CONST(X'B9')
018600940506     D F22             C                   CONST(X'BA')
018700940506     D F23             C                   CONST(X'BB')
018800940506     D F24             C                   CONST(X'BC')
018900940506     D ENTER           C                   CONST(X'F1')
019000940506     D ROLDWN          C                   CONST(X'F4')
019100940506     D ROLLUP          C                   CONST(X'F5')
019200940506     D*-------------
019300940506     D*
019400941108     D DATA            C                   CONST('0001-01-01')
019500940128     I*----------------------------------------------------*
019600940615     ID1
019700980909$005 I              D1DES                       §4ZDES
019800980909$005 I              D1KCC                       §4ZKCC
019900980909$005 I              D1KSC                       §4ZKSC
020000980909$005 I              D1VOC                       §4ZVOC
020100980909$005 I              D1CDB                       §4ZCDB
020200980909$005 I              D1CDS                       §4ZCDS
020300980909$005 I              D1KCI                       §4ZKCI
020400980909$005 I              D1KSI                       §4ZKSI
020500980911$005 I              D1KCR                       §4ZKCR
020600980911$005 I              D1KSR                       §4ZKSR
020700980909$005 I              D1IVC                       §4ZIVC
020800980909$005 I              D1IVA                       §4ZIVA
020900020415$005 I              D1crc                       §4Zcrc
021000020415$005 I              D1crp                       §4Zcrp
021100020415$005 I              D1crr                       §4Zcrr
021200020527$005 I              D1crca                      §4Zcrca
021300020527$005 I              D1crpa                      §4Zcrpa
021400020527$005 I              D1crra                      §4Zcrra
021500020415$005 I              D1ta                        §4Zta
021600980909$005 I              D1FLG                       §4ZFLG
021700940127     I*
021800940127     C*----------------------------------------------------*
021900940127     C*                MAIN LINE PROGRAM
022000940127     C*----------------------------------------------------*
022100940127     C*
022200940223     C* inizializzazione variabili
022300940223     C                   EXSR      INZVAR
022400940223     C*
022500940223     C     $FINE         DOWEQ     *OFF
022600940202     C     $GEST         CASEQ     'D1'          GESD1
022700940117     C                   END
022800940117     C                   END
022900940201     C*
023000940325     C* fine programma
023100940325     C                   EXSR      ENDPGM
023200940325     C/EJECT
023300940325     C************************************************************
023400940325     C* RICERCHE
023500940325     C************************************************************
023600940325     C     ENDPGM        BEGSR
023700940325     C*
023800940614     C* disallocazione rcd nel caso di
023900971110$007 C                   MOVE      'Y4Z'         XTBCOD
024000940614    >C                   MOVE      '7'           XTBRIC
024100970214     C                   CALLB     'XATB'
024200940614     C                   PARM                    XATBDS
024300940325     C*
024400940325     C* passaggio dati al pgm chiamante
024500940715     C                   MOVE      *BLANKS       KPJBU
024600940325    >C                   MOVEL     D002DS        KPJBU
024700940325     C*
024800940325     C                   SETON                                            RT
024900940325     C*
025000940325     C                   ENDSR
025100940325     C/EJECT
025200940201     C************************************************************
025300940201     C* RICERCHE
025400940201     C************************************************************
025500940127     C     SEARCH        BEGSR
025600940127     C*
025700940127     C* determino Riga/Colonna del cursore
025800940127     C                   MOVE      NRG           R$$
025900940127     C                   MOVE      NCL           C$$
026000940127     C                   Z-ADD     RIRI          CURR
026100940127     C                   Z-ADD     COCO          CURC
026200940127     C*
026300940224     C* I campi che al momento dell'F4 risultano protetti non devono
026400940224     C* essere abilitati alla funzione di ricerca
026500940224     C*
0266009401271    C                   SELECT
026700980909     C* FMTD1  - capoconto
026800980909    >C     H1NMFL        WHEneq    'D1KCC'
026900980909     C                   RESET                   Ana200DS
027000980909     C                   MOVEL     '01'          OPZ200
027100980909     C                   clear                   KCC200
027200980909     C                   MOVEL     Ana200DS      KPJBU
027300980909     C                   CALL      'ANA200R'
027400980909     C                   PARM                    KPJBA
027500980909     C                   MOVEL     KPJBU         Ana200DS
027600980909     C     KCC200        IFNE      *BLANK
027700980909     C                   MOVEL     KCC200        §4Zkcc
027800980909     C                   ENDIF
027900980909     C* imposto pos. del cursore
028000980909     C                   Z-ADD     CURR          H1RIGA
028100980909     C                   Z-ADD     CURC          H1COLO
028200980909     C*
028300980909     C* Ricerca sottoconti (ANA250R)
028400980909     C     H1NMFL        WHENEQ    'D1KSC'
028500980909     C                   RESET                   ana250DS
028600980909     C                   MOVEL     '01'          OPZ250
028700980909     C                   MOVEL     §4Zkcc        KCC250
028800980909     C                   clear                   KSC250
028900980909     C                   MOVEL     ana250DS      KPJBU
029000980909     C                   CALL      'ANA250R'
029100980909     C                   PARM                    KPJBA
029200980909     C                   MOVEL     KPJBU         ana250DS
029300980909     C     KSC250        IFNE      *BLANK
029400980909     C                   MOVEL     KCC250        §4zkcc
029500980909     C                   MOVEL     KSC250        §4zksc
029600980909     C                   ENDIF
029700980909     C* imposto pos. del cursore
029800980909     C                   Z-ADD     CURR          H1RIGA
029900980909     C                   Z-ADD     CURC          H1COLO
030000980909     C*
030100980909     C* FMTD1  - codice voce
030200980909    >C     H1NMFL        WHEneq    'D1VOC'
030300980909     C                   RESET                   Ana280DS
030400980909     C                   MOVEL     '01'          OPZ280
030500980909     C                   clear                   VOC280
030600980909     C                   MOVEL     Ana280DS      KPJBU
030700980909     C                   CALL      'ANA280R'
030800980909     C                   PARM                    KPJBA
030900980909     C                   MOVEL     KPJBU         Ana280DS
031000980909     C     voc280        IFNE      *BLANK
031100980909     C                   MOVEL     VOC280        §4Zvoc
031200980909     C                   ENDIF
031300980909     C* imposto pos. del cursore
031400980909     C                   Z-ADD     CURR          H1RIGA
031500980909     C                   Z-ADD     CURC          H1COLO
031600980910     C*
031700980910     C* FMTD3  - RIFERIMENTO ENTITA'
031800980910     C     H1NMFL        WHENEQ    'D1CDB'
031900980910     C     H1NMFL        OREQ      'D1CDS'
032000980910    >C                   RESET                   ANA032DS
032100980910    >C                   MOVEL     '01'          OPZ032
032200980910     C                   MOVE      'S'           FSC032
032300980910     C                   MOVE      'CDC'         TPE032
032400980910    >C                   MOVEL     ANA032Ds      KPJBU
032500980910    >C                   CALL      'ANA032R'
032600980910     C                   PARM                    KPJBA
032700980910    >C                   MOVEL     KPJBU         ANA032DS
032800980910     C*
032900980910     C     ENT032        IFNE      *BLANKS
033000980910     C                   SELECT
033100980910     C     H1NMFL        WHENEQ    'D1CDB'
033200980910     C                   MOVEL     ENT032        §4ZCDB
033300980910     C     H1NMFL        WHENEQ    'D1CDS'
033400980910     C                   MOVEL     ENT032        §4ZCDS
033500980910     C                   ENDSL
033600980910     C                   ENDIF
033700980909     C* FMTD1  - codice iva
033800980909    >C     H1NMFL        WHEneq    'D1IVA'
033900980909     C                   RESET                   Ana064DS
034000980909     C                   MOVEL     '2'           TPR064
034100980909     C                   MOVEL     '01'          OPZ064
034200980909     C                   MOVEL     Ana064DS      KPJBU
034300980909     C                   CALL      'ANA064R'
034400980909     C                   PARM                    KPJBA
034500980909     C                   MOVEL     KPJBU         Ana064DS
034600980909     C     ali064        IFNE      *zeros
034700980909     C                   MOVEL     ali064        §4ZIVA
034800980909     C                   ENDIF
034900980909     C* imposto pos. del cursore
035000980909     C                   Z-ADD     CURR          H1RIGA
035100980909     C                   Z-ADD     CURC          H1COLO
035200980909     C*
035300980909     C* FMTD1  - codice esenzione
035400980909    >C     H1NMFL        WHEneq    'D1IVC'
035500980910     C                   RESET                   XTABDS
035600980910     C                   MOVE      XSCSOC        XTAAZI
035700980910     C                   MOVE      XSCLIN        XTALIN
035800980910    >C                   MOVE      *BLANK        WWIVA            13
035900980910     C                   MOVE      WWIVA         XTAKEY
036000980910     C                   MOVE      '01'          XTAOPZ
036100980910     C                   Z-ADD     CURR          XTARIG
036200980910     C                   Z-ADD     CURC          XTACOL
036300980910     C                   CLEAR                   KPJBU
036400980910     C                   MOVEL     XTABDS        KPJBU
036500980910     C                   CALL      'ANGG48R'
036600980910     C                   PARM                    KPJBA
036700980910     C                   MOVEL     KPJBU         XTABDS
036800980910     C     XTAERR        IFNE      '1'
036900980910     C                   MOVE      XTAKEY        WWIVA
037000980910     C                   ELSE
037100980910     C                   MOVE      *BLANK        WWIVA
037200980910     C                   ENDIF
037300980910    >C     WWIVA         IFNE      *BLANK
037400980910    >C                   MOVEL     WWIVA         §4ZIVC
037500980910    >C                   ENDIF
037600980909     C* imposto pos. del cursore
037700980909     C                   Z-ADD     CURR          H1RIGA
037800980909     C                   Z-ADD     CURC          H1COLO
037900980909     C* FMTD1  - capoconto IVA
038000980909    >C     H1NMFL        WHEneq    'D1KCI'
038100980909     C                   RESET                   Ana200DS
038200980909     C                   MOVEL     '01'          OPZ200
038300980909     C                   clear                   KCC200
038400980909     C                   MOVEL     Ana200DS      KPJBU
038500980909     C                   CALL      'ANA200R'
038600980909     C                   PARM                    KPJBA
038700980909     C                   MOVEL     KPJBU         Ana200DS
038800980909     C     KCC200        IFNE      *BLANK
038900980909     C                   MOVEL     KCC200        §4ZKCI
039000980909     C                   ENDIF
039100980909     C* imposto pos. del cursore
039200980909     C                   Z-ADD     CURR          H1RIGA
039300980909     C                   Z-ADD     CURC          H1COLO
039400980909     C*
039500980909     C* Ricerca sottoconti (ANA250R)
039600980909     C     H1NMFL        WHENEQ    'D1KSI'
039700980909     C                   RESET                   ana250DS
039800980909     C                   MOVEL     '01'          OPZ250
039900980909     C                   MOVEL     §4ZKCI        KCC250
040000980909     C                   clear                   KSC250
040100980909     C                   MOVEL     ana250DS      KPJBU
040200980909     C                   CALL      'ANA250R'
040300980909     C                   PARM                    KPJBA
040400980909     C                   MOVEL     KPJBU         ana250DS
040500980909     C     KSC250        IFNE      *BLANK
040600980909     C                   MOVEL     KCC250        §4ZKCI
040700980909     C                   MOVEL     KSC250        §4ZKSI
040800980909     C                   ENDIF
040900980909     C* imposto pos. del cursore
041000980909     C                   Z-ADD     CURR          H1RIGA
041100980909     C                   Z-ADD     CURC          H1COLO
041200980911     C* FMTD1  - capoconto FAtture da ricevere
041300980911    >C     H1NMFL        WHEneq    'D1KCR'
041400980911     C                   RESET                   Ana200DS
041500980911     C                   MOVEL     '01'          OPZ200
041600980911     C                   clear                   KCC200
041700980911     C                   MOVEL     Ana200DS      KPJBU
041800980911     C                   CALL      'ANA200R'
041900980911     C                   PARM                    KPJBA
042000980911     C                   MOVEL     KPJBU         Ana200DS
042100980911     C     KCC200        IFNE      *BLANK
042200980911     C                   MOVEL     KCC200        §4ZKCr
042300980911     C                   ENDIF
042400980911     C* imposto pos. del cursore
042500980911     C                   Z-ADD     CURR          H1RIGA
042600980911     C                   Z-ADD     CURC          H1COLO
042700980911     C*
042800980911     C* Ricerca sottoconti (ANA250R)
042900980911     C     H1NMFL        WHENEQ    'D1KSR'
043000980911     C                   RESET                   ana250DS
043100980911     C                   MOVEL     '01'          OPZ250
043200980911     C                   MOVEL     §4ZKCR        KCC250
043300980911     C                   clear                   KSC250
043400980911     C                   MOVEL     ana250DS      KPJBU
043500980911     C                   CALL      'ANA250R'
043600980911     C                   PARM                    KPJBA
043700980911     C                   MOVEL     KPJBU         ana250DS
043800980911     C     KSC250        IFNE      *BLANK
043900980911     C                   MOVEL     KCC250        §4ZKCR
044000980911     C                   MOVEL     KSC250        §4ZKSR
044100980911     C                   ENDIF
044200020412     C* imposto pos. del cursore
044300020412     C                   Z-ADD     CURR          H1RIGA
044400020412     C                   Z-ADD     CURC          H1COLO
044500020412     C*
044600020412     C* Ricerca causali contabili
044700020412     C                   WHEN      h1nmfl = 'D1CRC'or
044800020412     C                             h1nmfl = 'D1CRR'or
044900020527     C                             h1nmfl = 'D1CRP'or
045000020527     C                             h1nmfl = 'D1CRCA'or
045100020527     C                             h1nmfl = 'D1CRRA'or
045200020527     C                             h1nmfl = 'D1CRPA'
045300020412     C                   RESET                   ANA070DS
045400020412     C                   EVAL      OPZ070 = '01'
045500020412     C                   EVAL      Kpjbu = ANA070DS
045600020412     C                   CALL      'ANA070R'
045700020412     C                   PARM                    Kpjba
045800020412     C                   EVAL      ANA070DS = Kpjbu
045900020412      *
046000020412     C                   IF        RET070 = *BLANKS
046100020412     C                   SELECT
046200020412     C                   WHEN      H1NMFL = 'D1CRC'
046300020412     C                   EVAL      §4zcrc  = Cau070
046400020412     C                   WHEN      H1NMFL = 'D1CRR'
046500020412     C                   EVAL      §4zcrr = Cau070
046600020412     C                   WHEN      H1NMFL = 'D1CRP'
046700020412     C                   EVAL      §4zcrp = Cau070
046800020527     C                   WHEN      H1NMFL = 'D1CRPA'
046900020527     C                   EVAL      §4zcrpa = Cau070
047000020527     C                   WHEN      H1NMFL = 'D1CRRA'
047100020527     C                   EVAL      §4zcrra = Cau070
047200020527     C                   WHEN      H1NMFL = 'D1CRCA'
047300020527     C                   EVAL      §4zcrca = Cau070
047400020412     C                   ENDSL
047500020412     C                   ENDIF
047600020412     C* imposto pos. del cursore
047700020412     C                   Z-ADD     CURR          H1RIGA
047800020412     C                   Z-ADD     CURC          H1COLO
047900940127     C*
048000940223     C*
048100940223     C* ricerca non abilitata in questa posizione
0482009402231O   C                   OTHER
048300941005     C                   SETON                                        9899
048400941005     C* imposto pos. del cursore
048500941005     C                   Z-ADD     CURR          H1RIGA
048600941005     C                   Z-ADD     CURC          H1COLO
048700940127     C*
0488009401271-   C                   ENDSL
048900940127     C*
049000940506     C                   MOVE      *HIVAL        $LASTV
049100940127     C                   ENDSR
049200940615     C/EJECT
049300940615     C************************************************************
049400940615     C* TRADUZIONI
049500940615     C************************************************************
049600940615     C     LINGUA        BEGSR
049700940615     C*
049800940615     C* determino Riga/Colonna del cursore
049900940615     C                   MOVE      NRG           R$$
050000940615     C                   MOVE      NCL           C$$
050100940615     C                   Z-ADD     RIRI          CURR
050200940615     C                   Z-ADD     COCO          CURC
050300940615     C* FMTD1  - CAMPI DA TRADURRE
050400940615     C* imposto pos. del cursore
050500940615     C                   Z-ADD     CURR          H1RIGA
050600940615     C                   Z-ADD     CURC          H1COLO
050700940621     C                   MOVEL     H1NMFL        W005              5
050800940621     C                   MOVE      W005          W003              3
050900940621     C                   MOVEL     W003          W006              6
051000940620     C                   MOVE      *ZERO         KEY              15
051100980909     C                   MOVEl     D1COD         KEY
051200940615     C                   SELECT
051300940615     C* ricerca non abilitata in questa posizione
0514009406151O   C                   OTHER
051500940615     C                   SETON                                        98
051600940615     C*
0517009406151-   C                   ENDSL
051800940615     C*
051900940615     C                   MOVE      *HIVAL        $LASTV
052000940615     C                   ENDSR
052100940225     C/EJECT
052200940225     C************************************************************
052300940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
052400940225     C************************************************************
052500940225     C     GESPIE        BEGSR
052600940225     C*
052700940225     C                   SELECT
052800940225     C*
052900940506     C     $GEST         WHENEQ    'D1'
053000940506     C     $ULKD1        IFEQ      0
053100940506     C* riempimento opzioni
053200940506     C                   EXSR      F24D1
053300940506     C                   ELSE
053400940614     C                   WRITE     Z1
053500940506     C                   ENDIF
053600940225     C*
053700940510     C*
053800940225     C                   ENDSL
053900940225     C*
054000940225     C                   ENDSR
054100940131     C/EJECT
054200940131     C************************************************************
054300940510     C* GESTIONE VIDEO RECORD D1
054400940131     C************************************************************
054500940127     C     GESD1         BEGSR
054600940225     C* inizializzazione videata
054700940223     C     $INZD1        IFEQ      *ON
054800940127     C                   EXSR      INZD1
054900940223     C                   MOVE      *OFF          $INZD1
055000940117     C                   END
055100940225     C* emissione piede videata se proveniente da altra
055200940225     C* salvataggio valore formato di provenienza
055300940207     C     $LASTV        IFNE      'D1'
055400940614     C                   WRITE     T1
055500940225     C                   EXSR      GESPIE
055600940202     C                   MOVE      $LASTV        $LASTG
055700940202     C                   MOVE      'D1'          $LASTV
055800940202     C                   END
055900940131     C*
056000940203     C                   SELECT
056100940203     C* Annullamento
056200940315    >C     OPZ002        WHENEQ    '04'
056300940224     C* Visualizzazione
056400940315    >C     OPZ002        OREQ      '05'
056500940620     C                   SETON                                        0203
056600940620     C* Manutenzione
056700940620    >C     OPZ002        WHENEQ    '02'
056800940620     C                   SETON                                        02
056900940620     C                   SETOFF                                       03
057000940620     C*
057100940620     C                   OTHER
057200940620     C*
057300940620     C                   SETOFF                                       0203
057400940203     C                   ENDSL
057500940620     C* Immissione/Modifica/Copia
057600940620     C                   EXFMT     D1
057700940506     C* controllo abilitazione tasti funzione
057800940506     C*
057900940506     C                   EXSR      CTRKD1
058000940506     C*
058100940506     C     *IN99         IFEQ      *OFF
058200940131     C*
0583009402031    C                   SELECT
058400940510     c* ROLLUP
058500940131     C* F3=Fine
058600940506     C     $TASTO        WHENEQ    F03
058700940309     C                   EXSR      F03D1
058800940131     C* F4=Ricerca
058900940506     C     $TASTO        WHENEQ    F04
059000940127     C                   EXSR      SEARCH
059100940224     C* F5=Refresh
059200940506     C     $TASTO        WHENEQ    F05
059300940309     C                   EXSR      F05D1
059400940223     C* F8=Successivo
059500940506     C     $TASTO        WHENEQ    F08
059600940309     C                   EXSR      F08D1
059700940131     C* F12=Ritorno
059800940506     C     $TASTO        WHENEQ    F12
059900940309     C                   EXSR      F12D1
060000940315     C* F24=Altri tasti
060100940506     C     $TASTO        WHENEQ    F24
060200940315     C                   EXSR      F24D1
060300940131     C*
0604009402031O   C                   OTHER
060500980910     C                   EXSR      CTRD1
060600940616     C     *IN99         IFEQ      *OFF
060700940616     C                   SELECT
060800940616     C* F7=Traduzione
060900940616     C     $TASTO        WHENEQ    F07
061000940616     C                   EXSR      LINGUA
061100940616     C* F6
0612009406162    C     $TASTO        WHENEQ    F06
061300940224     C* eseguo aggiornamento
061400940315    >C                   EXSR      AGGANA
061500940224     C* eseguo operazioni del dopo-aggiornamento
0616009403183    C     *IN99         IFEQ      *OFF
061700940318     C                   EXSR      GESAGG
0618009403183-   C                   ENDIF
0619009406162-   C                   ENDSL
062000940616     C                   ENDIF
062100940203     C*
0622009402241-   C                   ENDSL
062300940224     C*
0624009405062-   C                   ENDIF
062500940117     C                   ENDSR
062600940506     C/EJECT
062700940506     C************************************************************
062800940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
062900940506     C************************************************************
063000940506     C     CTRKD1        BEGSR
063100940506     C                   SETOFF                                       99
063200940506     C                   MOVEA     $FC1          $FC
063300940506     C                   EXSR      CTRKEY
063400940506     C     *IN99         IFEQ      *ON
063500940506     C                   SETON                                        97
063600940506     C                   ENDIF
063700940506     C                   ENDSR
063800940117      ************************************************************
063900940207      * INIZIALIZZAZIONE VIDEATA DATI
064000940117      ************************************************************
064100940127     C     INZD1         BEGSR
064200940131     C*
064300940615     C                   CLEAR                   D1
064400980909     C                   MOVE      '0'           D1RIS
064500980909     C                   MOVEl     KEY002        D1COD
064600940615     C                   MOVE      AZI002        XTBAZI
064700940615     C                   MOVE      LIN002        XTBLIN
064800980909     C                   MOVE      *blanks       XTBKEY
064900980909     C                   MOVEl     KEY002        XTBKEY
065000970509     C                   MOVE      UNI002        XTBUNT
065100940615$007 C                   MOVE      'Y4Z'         XTBCOD
065101060724     C                   MOVE      *BLANK        opesers           4
065102060724     C                   MOVE      '0'           okmsg             1
065200940225     C*
065300940207     C*
065400940224     C* valorizzo la chiave univoca per reperire i dati
065500940224     C* se non immissione
0656009403151   >C     OPZ002        IFNE      '01'
065700940315    >C*-------------------------------------------------------------
065800940315    >C* impostare i campi chiave ricevuti
065900940315    >C*-------------------------------------------------------------
066000940318     C*
066100940614     C*
066200940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0663009406142   >C     OPZ002        IFEQ      '02'
066400940315    >C     OPZ002        OREQ      '04'
066500940614     C                   MOVE      '1'           XTBALC
066600940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0667009406142E   C                   ELSE
066800940614     C                   MOVE      '0'           XTBALC
0669009406142-   C                   ENDIF
067000940614     C*
067100940616     C                   MOVE      '1'           XTBRIC
067200970214     C                   CALLB     'XATB'
067300940614     C                   PARM                    XATBDS
067400940318     C*
067500940614     C     XTBERR        IFEQ      '0'
067600940614     C                   MOVEL     XTBUNI        EXTDS
067700970512     c                   move      xtbunt        d1uniTA           8
067800941108     C* Giro la data se non blank
067900970214     c                   move      xtbdsc        xtbdscA          10
068000970214     C     XTBDSCa       IFNE      DATA
068100970214     C                   CALLB     'XDT4000'
068200970214     C                   PARM      XTBDSCA       XDTAMG           10
068300940621     C                   PARM      *ZEROS        XDTGMA            6 0
068400940621     C                   PARM      2             XDTSTA            1 0
068500940621     C                   MOVE      XDTGMA        D1DSC
068600940712     C                   MOVE      XTBRIS        D1RIS
068700941108     C                   ELSE
068800941108     C                   MOVE      *ZEROS        D1DSC
068900941108     C                   ENDIF
069000941108     C                   ENDIF
069100940614     C*
069200940224     C*
069300940224     C* se il rcd da gestire non viene trovato
069400940318     C* oppure è allocato da altro pgm
069500940224     C* ritorno al pgm guida avvertendolo dell'errore
0696009406142    C     XTBERR        IFNE      '0'
069700940224     C                   MOVE      *ON           $FINE
069800940315    >C                   MOVE      '1'           ERR002
069900940224     C*
070000940224     C* altrimenti
0701009402242E   C                   ELSE
070200940224     C* riempio la videata
070300940224     C                   EXSR      RIED1
070400940224     C* richiamo routine dei ctrl per decodificarla
070500940614    >C                   EXSR      CTRD1
070600960228     C                   MOVEL     *IN99         SAV99             1
070700960228     C* Se la gestione è più videate
070800960305    >C                   EXSR      CTRD2
070900960305    >C     *IN99         IFEQ      *ON
071000960305    >C                   MOVEL     *IN99         SAV99             1
071100960305    >C                   ENDIF
071200940224     C                   MOVE      *ALL'0'       IN5098
071300940224     C                   MOVEA     IN5098        *IN(50)
071400940224     C* definizione indicatori su video
071500940224     C                   SETOFF                                       02
0716009402243    C                   SELECT
071700940315    >C     OPZ002        WHENEQ    '02'
071800940224     C                   SETON                                        02
071900940315    >C     OPZ002        WHENEQ    '05'
072000940506     C* disabilita tasti
072100940720     C                   MOVE      '0'           $FC(6)
072200940720     C                   MOVE      '0'           $FC(5)
072300940720     C                   MOVE      '0'           $FC(4)
0724009402243-   C                   ENDSL
072500940224     C*
0726009402242-   C                   ENDIF
072700940223     C*
0728009402241-   C                   ENDIF
072900940224     C*
073000940117     C                   ENDSR
073100940207     C/EJECT
073200940207     C************************************************************
073300940510     C* RIEMPIMENTO VIDEATA  D1
073400940207     C************************************************************
073500940207     C     RIED1         BEGSR
073600940207     C*
073700940207     C*
073800940207     C                   ENDSR
073900940309     C/EJECT
074000940309     C************************************************************
074100940309     C* GESTIONE F03 VIDEO D1
074200940309     C************************************************************
074300940309     C     F03D1         BEGSR
074400940309     C*
074500940309     C                   MOVE      *ON           $FINE
074600940315    >C                   MOVE      '1'           RET002
074700940325     C* fine programma
074800940325     C                   EXSR      ENDPGM
074900940325     C*
075000940325     C                   ENDSR
075100940309     C/EJECT
075200940309     C************************************************************
075300940309     C* GESTIONE F05 VIDEO D1
075400940309     C************************************************************
075500940309     C     F05D1         BEGSR
075600940309     C*
075700940309     C                   MOVE      *ON           $INZD1
075800940309     C*
075900940309     C                   ENDSR
076000940309     C/EJECT
076100940309     C************************************************************
076200940309     C* GESTIONE F08 VIDEO D1
076300940309     C************************************************************
076400940309     C     F08D1         BEGSR
076500940309     C*
076600940309     C                   MOVE      *ON           $FINE
076700940315    >C                   MOVE      '0'           RET002
076800940309     C*
076900940309     C                   ENDSR
077000940309     C/EJECT
077100940309     C************************************************************
077200940309     C* GESTIONE F12 VIDEO D1
077300940309     C************************************************************
077400940309     C     F12D1         BEGSR
077500940309     C*
077600940309     C                   MOVE      *ON           $FINE
077700940315    >C                   MOVE      '2'           RET002
077800940309     C*
077900940309     C                   ENDSR
078000940309     C/EJECT
078100940309     C************************************************************
078200940309     C* GESTIONE F24 VIDEO D1
078300940309     C************************************************************
078400940309     C     F24D1         BEGSR
078500940309     C*
078600940506     C*
078700940506     C                   MOVEA     $FX1          $FX
078800940506     C                   MOVEA     $FC1          $FC
078900940506     C                   MOVEA     $FR1          $FR
079000940506     C                   Z-ADD     $ULKD1        $ULK
079100940506     C                   EXSR      RIEKEY
079200941017     C                   MOVEA     $FC           $FC1
079300940506     C                   Z-ADD     $ULK          $ULKD1            3 0
079400940506     C                   MOVEL     $KEY1         Z1KE1
079500940506     C                   MOVEL     $KEY2         Z1KE2
079600940614     C                   WRITE     Z1
079700940506     C*
079800940309     C                   ENDSR
079900940207     C/EJECT
080000940131     C************************************************************
080100940207     C* CONTROLLO VIDEATA
080200940131     C************************************************************
080300940127     C     CTRD1         BEGSR
080400940131     C*
080500940127     C                   SETOFF                                       99
080600940127     C* non imposto pos. del cursore
080700940127     C                   Z-ADD     0             H1RIGA
080800940127     C                   Z-ADD     0             H1COLO
080900940621     C*
081000940621     C* CTRL DATA
081100940621     C*
081200941109     C                   MOVE      DATA          WRKDSC           10
081300940621     C     D1DSC         IFNE      0
081400970214     C                   CALLB     'XDT4000'
081500941109     C                   PARM                    XDTAMG           10
081600940621     C                   PARM      D1DSC         XDTGMA            6 0
081700940621     C                   PARM      3             XDTSTA            1 0
081800940621     C     XDTSTA        IFNE      0
081900980909     C                   SETON                                        7899
082000940621     C                   ELSE
082100941108     C                   MOVE      XDTAMG        WRKDSC
082200940621     C                   ENDIF
082300940621     C                   ENDIF
082400940223     C*
082500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
082600940223     C*
0827009403151   >C     OPZ002        IFNE      '04'
082800971110     C*
082900971110     C* controllo codice vuoto
083000980909$017 C     D1COD         IFEQ      ' '
083100971110     C                   SETON                                        5099
083200971110     C                   ENDIF
083300971110     C*
083400940207     C*
083500940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
083600940315    >C     OPZ002        IFEQ      '01'
083700940315    >C     OPZ002        OREQ      '03'
083800940315    >C*-------------------------------------------------------------
083900940315    >C* impostare i campi chiave e verificare l'esistenza del record
084000940315    >C*-------------------------------------------------------------
084100971110$007 C                   MOVE      'Y4Z'         XTBCOD
084200980909     C                   MOVE      *blanks       XTBKEY
084300980909    >C                   MOVEl     D1COD         XTBKEY
084400970512    >C                   MOVE      D1UNITA       XTBUNT
084500940614     C                   MOVE      '0'           XTBALC
084600940614     C                   MOVE      '1'           XTBRIC
084700970214    >C                   CALLB     'XATB'
084800940614     C                   PARM                    XATBDS
084900980910     C     XTBERR        IFEQ      '0'
085000940223     C                   SETON                                        5999
085100940223     C                   ENDIF
085200940620     C* Controllo se compreso nei range indice
085300940620     C     XTBERR        IFEQ      '3'
085400940620    >C                   SETON                                        5899
085500940620     C                   END
085600940620     C*
085700940620     C* REPERISCO AUTORIZZAZIONI UTENTE
085800940620     C                   CLEAR                   XCHATB
085900941118     C                   MOVE      XSCSOC        XCTSOC
086000941118     C                   MOVE      XSCDUN        XCTUNI
086100940620     C                   MOVEL     KNMUS         XCTPRF
086200940620     C                   MOVE      XTBCOD        XCTCOD
086300940620     C                   CALL      'XCHKATB'
086400940620     C                   PARM                    XCHATB
086500941111     C                   MOVEL     XCTDDS        AUTDS
086600941027     C     XCTRTN        IFEQ      1
086700941027     C                   MOVEL     'N'           MANGRU
086800941027     C                   ENDIF
086900940620     C* Non ammessa immissione di gruppo
087000940620     C     XTBGRU        IFEQ      'G'
087100940620     C     MANGRU        ANDEQ     'N'
087200940620     C                   SETON                                        5799
087300940620     C                   END
087400940620     C* Non ammessa immissione di azienda
087500940620     C     XTBGRU        IFEQ      'A'
087600940620     C     MANAZI        ANDEQ     'N'
087700940620     C                   SETON                                        5699
087800940620     C                   END
087900940712     C* codice riservato non accessibile
088000941006     C     XTBRIS        IFGT      ABIRIS
088100941005    >C     OPZ002        ANDNE     '01'
088200941005    >C     OPZ002        ANDNE     '03'
088300940712     C                   SETON                                        5499
088400940712     C                   END
088500940224     C                   ENDIF
088600940224     C*
088700980909     C*  Controllo codice descrizione tabella
088800980909     C                   if        §4ZDES=*blanks
088900980909     C                   SETON                                        5199
089000980909     C                   END
089100020415     C*  Controllo se sono traini o autotrasportatore
089200020415     C                   if        §4Zta<>'A' and §4zta <> 'T'
089300020415     C                   SETON                                        8399
089400020415     C                   END
089500980909     C*---------------------------------------------------------------*
089600980909     C*  Controllo codice capoconto conto di costo
089700980909     C*  E' impostato ?
089800980909     C                   if        §4ZKCC=*blanks
089900980909     C                   SETON                                        5299
090000980909     C                   Else
090100980909      *
090200980909     C                   CALLb     'XALLINEA'
090300980909     C                   PARM                    §4ZKCC
090400980909     C                   PARM      6             LEN               5 0
090500980909     C                   PARM                    XSCINP
090600980909     C*  Esiste ?
090700980909     C                   move      xscsoc        ksocieta
090800980909     C                   move      §4Zkcc        kkcc
090900020415     C     krco1         chain     anrco99j                           21
091000980909     C                   if        *in21
091100980909     C                   SETON                                        5399
091200980909     C                   else
091300980909     C* non è un capoconto di costo
091400980909     C                   if        rconatura<>'3'
091500980909     C                   SETON                                        5599
091600980909     C                   END
0917009809091-   C                   END
0918009809091-   C                   END
091900980909     C*---------------------------------------------------------------*
092000980909     C*  Controllo codice sottoconto conto di costo
092100980909     C*  E' impostato ?
092200980909     C                   if        §4ZKSC=*blanks
092300980909     C                   SETON                                        6199
092400980909     C                   Else
092500980909     C                   CALLb     'XALLINEA'
092600980909     C                   PARM                    §4ZKSC
092700980909     C                   PARM      8             LEN               5 0
092800980909     C                   PARM                    XSCINP
092900980909     C*  Esiste ?
093000980909     C                   move      xscsoc        ksocieta
093100980909     C                   move      §4Zkcc        kkcc
093200980909     C                   move      §4Zksc        kksc
093300980909     C     krco          chain     anrco99j                           21
093400980909     C                   if        *in21
093500980909     C                   SETON                                        6099
093600980909     C                   else
093700980909     C* non è un conto di costo
093800980909     C                   movel     pdcdesbrev    D1dksc
093900980909     C                   if        rconatura<>'3'
094000980909     C                   SETON                                        5599
094100980909     C                   END
0942009809091-   C                   END
0943009809091-   C                   END
094400980909     C*---------------------------------------------------------------*
094500980909     C*  Controllo la voce del conto di costo
094600980909     C*  E' impostata ?
094601140129     C                   CLEAR                   d1dvoc
094700980909     C                   if        §4ZVOC=*blanks
094800980909     C                   SETON                                        6299
094900980909     C                   Else
095000980909     C*  Esiste ?
095100980909     C                   move      xscsoc        ksocieta
095200980909     C                   move      §4Zvoc        kvoce
095300980909     C     kvci01        chain     anvci01l                           21
095400980909     C                   if        *in21
095500980909     C                   SETON                                        6499
095600140129     C                   else
095700140129     C                   movel     vcidesbrev    d1dvoc
095800140129     C*    kvci02        chain     anvci02l                           21
095900140129     C*                  if        *in21
096000140129     C*                  SETON                                        6399
096100140129     C*                  END
0962009809091-   C                   END
0963009809091-   C                   END
096400980909     C*---------------------------------------------------------------*
096500980909     C*  Controllo il centro di costo Bartolini
096600980909     C*  E' impostato ?
096700020415     C                   if        §4Zta = 'T' and
096800020415     C                             §4ZCDB=*blanks
096900020412     C                   SETON                                        6599
097000020415     c                   end
097100020415     C                   clear                   d1dcdb
097200020415     C                   if        §4zcdb <> *blanks
097300980909     C*  Esiste ?
097400980909     C                   move      'CDC'         ktprifan
097500980909     C                   move      §4Zcdb        kentita
097600980909     C     kent          chain     anent01l                           21
097700980909     C                   if        *in21
097800980909     C                   SETON                                        6699
097900980909     C                   else
098000980909     C                   move      entdesbrev    d1dcdb
098100980909     C                   end
098200980909     C                   end
098300980909     C*---------------------------------------------------------------*
098400980909     C*  Controllo il centro di costo SDI
098500980909     C*  E' impostato ?
098600020415     C                   if        §4Zta = 'T' and
098700020415     C                             §4ZCDB=*blanks
098800980909     C                   SETON                                        6799
098900020415     C                   end
099000980909     C*  Esiste ?
099100020415     C                   clear                   d1dcds
099200020415     C                   if        §4ZCDB<>*blanks
099300980909     C                   move      'CDC'         ktprifan
099400980909     C                   move      §4Zcds        kentita
099500980909     C     kent          chain     anent01l                           21
099600980909     C                   if        *in21
099700980909     C                   SETON                                        6899
099800980909     C                   else
099900980909     C                   move      entdesbrev    d1dcds
100000980909     C                   end
100100980909     C                   end
100200980909     C*---------------------------------------------------------------*
100300980909     C*  Controllo codice capoconto conto IVA
100400980909     C*  E' impostato ?
100500980909     C                   if        §4ZKCI=*blanks
100600980909     C                   SETON                                        6999
100700980909     C                   Else
100800980909     C                   CALLb     'XALLINEA'
100900980909     C                   PARM                    §4ZKCI
101000980909     C                   PARM      6             LEN               5 0
101100980909     C                   PARM                    XSCINP
101200980909     C*  Esiste ?
101300980909     C                   move      xscsoc        ksocieta
101400980909     C                   move      §4Zkci        kkcc
101500020415     C     krco1         chain     anrco99j                           21
101600980909     C                   if        *in21
101700980909     C                   SETON                                        7199
101800980909     C                   else
101900980909     C* non è un capoconto IVA
102000980909     C                   if        rcosnatura<>'3'
102100980909     C                   SETON                                        7099
102200980909     C                   END
1023009809091-   C                   END
1024009809091-   C                   END
102500980909     C*---------------------------------------------------------------*
102600980909     C*  E' impostato ?
102700980909     C                   if        §4ZKSI=*blanks
102800980909     C                   SETON                                        7299
102900980909     C                   Else
103000980909     C                   CALLb     'XALLINEA'
103100980909     C                   PARM                    §4ZKSI
103200980909     C                   PARM      8             LEN               5 0
103300980909     C                   PARM                    XSCINP
103400980909     C*  Esiste ?
103500980909     C                   move      xscsoc        ksocieta
103600980909     C                   move      §4ZkcI        kkcc
103700980909     C                   move      §4ZksI        kksc
103800980909     C     krco          chain     anrco99j                           21
103900980909     C                   if        *in21
104000980909     C                   SETON                                        7399
104100980909     C                   else
104200980909     C* non è un conto di costo
104300020503     C                   movel     pdcdesbrev    D1dksi
104400980909     C                   if        rcosnatura<>'3'
104500980909     C                   SETON                                        7199
104600980909     C                   END
1047009809091-   C                   END
1048009809091-   C                   END
104900980909     C*---------------------------------------------------------------*
105000980909     C*  Controllo codice esenzione IVA
105100980909     C                   if        §4ZIVC>*blanks
105200980909     C*  Esiste ?
105300980910     C                   CLEAR                   X06DS
105400980910     C                   MOVE      XSCSOC        X06SOC
105500980910     C                   MOVE      XSCLIN        X06LIN
105600980910     C                   MOVE      §4ZIVC        X06CIV
105700980910     C                   MOVE      WDATE         X06DAT
105800980910     C                   CALL      'X06G48'
105900980910     C                   PARM                    X06DS
106000980910   B3C     X06ERR        IFEQ      '0'
106100980910     C                   MOVEL     X06UNI        TABG48
106200980910   X3C                   ELSE
106300980910     C                   SETON                                        7499
106400980910   E3C                   ENDIF
106500980909     C                   ENDIF
106600980909     C*---------------------------------------------------------------*
106700980909     C* Devo impostare o il codice di esenzione o l'aliquota IVA
106800980909$017 C     §4ziva        IFgt      *zeros
106900980909     c     §4zivc        andne     *blanks
107000980909$017 C     §4ziva        oreq      *zeros
107100980909     c     §4zivc        andeq     *blanks
107200980909     C                   SETON                                        7599
107300980909     C                   ENDIF
107400980909     C*---------------------------------------------------------------*
107500980909     C* Se impostata aliquota IVA controllo che esista su file associazioni
107600980909     C* aliquote libri-IVA
107700980909     c                   if        §4ziva          <> *zeros
107800980909     C                   move      §4Ziva        kaliq
107900980909     c     k02riv02      chain     anriv02l                           11
108000980909     C                   if        *in11
108100980909     c   11              seton                                        7799
108200980909     c                   endif
108300980909     c                   endif
108400980909     C*
108500980909     C                   endif
108600980911     C*---------------------------------------------------------------*
108700980911     C*  E' impostato ?
108800980911     C                   if        §4ZKCR=*blanks
108900980911     C                   SETON                                        9099
109000980911     C                   Else
109100980911     C                   CALLb     'XALLINEA'
109200980911     C                   PARM                    §4ZKCr
109300980911     C                   PARM      6             LEN               5 0
109400980911     C                   PARM                    XSCINP
109500980911     C*  Esiste ?
109600980911     C                   move      xscsoc        ksocieta
109700980911     C                   move      §4Zkcr        kkcc
109800020415     C     krco1         chain     anrco99j                           21
109900980911     C                   if        *in21
110000980911     C                   SETON                                        9199
1101009809111-   C                   END
1102009809111-   C                   END
110300980911     C*---------------------------------------------------------------*
110400980911     C*  E' impostato ?
110500980911     C                   if        §4ZKSR=*blanks
110600980911     C                   SETON                                        9299
110700980911     C                   Else
110800980911     C                   CALLb     'XALLINEA'
110900980911     C                   PARM                    §4ZKSR
111000980911     C                   PARM      8             LEN               5 0
111100980911     C                   PARM                    XSCINP
111200980911     C*  Esiste ?
111300980911     C                   move      xscsoc        ksocieta
111400980911     C                   move      §4Zkcr        kkcc
111500980911     C                   move      §4Zksr        kksc
111600980911     C     krco          chain     anrco99j                           21
111700980911     C                   if        *in21
111800980911     C                   SETON                                        9399
111900980911     C                   else
112000980911     C* non è un conto di costo
112100980911     C                   movel     pdcdesbrev    D1dksr
1122009809111-   C                   END
1123009809111-   C                   END
112400020412     C*---------------------------------------------------------------*
112500020527     c* CAUSALE REG. ANNO IN CORSO NO AUTOFATTURAZIONE
112600020412     C*  E' impostato ?
112700020527     c                   clear                   d1dcrc
112800020527     C                   if        §4ZCRC=*blanks
112900020527     C                   SETON                                        8099
113000020412     C                   Else
113100020412     C*  Esiste ?
113200020415     C                   EVAL      OpeCausale = §4zcrc
113300020412     C     Kope          CHAIN     AnOpe01L
113400020412     C                   IF        %FOUND(AnOpe01L)
113500020412     C                   EVAL      d1dcrc = OpeDesBrev
113600020412     C                   ELSE
113700020412     C                   seton                                        9980
1138000204121-   C                   END
113900020527     C                   End
114000020412     C*---------------------------------------------------------------*
114100020527     c* CAUSALE REG. ANNO PRECEDENTE NO AUTOFATTURAZIONE
114200020527     c                   clear                   d1dcrr
114300020412     C*  E' impostato ?
114400020527     C                   if        §4ZCRR=*blanks
114500020527     C                   SETON                                        8199
114600020412     C                   Else
114700020412     C*  Esiste ?
114800020415     C                   EVAL      OpeCausale = §4zcrr
114900020412     C     Kope          CHAIN     AnOpe01L
115000020412     C                   IF        %FOUND(AnOpe01L)
115100020412     C                   EVAL      d1dcrr = OpeDesBrev
115200020412     C                   ELSE
115300020412     C                   seton                                        9981
1154000204121-   C                   END
115500020527     C                   End
115600020412     C*---------------------------------------------------------------*
115700020527     c* CAUSALE REG. FATTURE DA RICEVERE NO AUTOFATTURAZIONE
115800020527     c                   clear                   d1dcrp
115900020412     C*  E' impostato ?
116000020527     C                   if        §4ZCRP=*blanks
116100020527     C                   SETON                                        8299
116200020412     C                   Else
116300020412     C*  Esiste ?
116400020415     C                   EVAL      OpeCausale = §4zcrp
116500020412     C     Kope          CHAIN     AnOpe01L
116600020412     C                   IF        %FOUND(AnOpe01L)
116700020412     C                   EVAL      d1dcrp = OpeDesBrev
116800020412     C                   ELSE
116900020412     C                   seton                                        9982
1170000204121-   C                   END
117100020527     C                   End
117200020527     C*---------------------------------------------------------------*
117300060724     C                   if        §4zta = 'A' or §4zta = 'T'
117400020527     c* CAUSALE REG. ANNO IN CORSO AUTOFATTURAZIONE
117500020527     C*  E' impostato ?
117600020527     c                   clear                   d1dcrca
117700020527     C                   if        §4zcrca = *blanks
117800020527     C                   SETON                                        8499
117900020527     C                   Else
118000020527     C*  Esiste ?
118100020527     C                   EVAL      OpeCausale = §4zcrca
118200020527     C     Kope          CHAIN     AnOpe01L
118300020527     C                   IF        %FOUND(AnOpe01L)
118400020527     C                   EVAL      d1dcrca = OpeDesBrev
118401060724     c                   if        §4zta = 'T'
118402060724     c                   eval      opesers = opeser
118403060724     c                   exsr      sropeser
118404060724     c                   end
118500020527     C                   ELSE
118600020527     C                   seton                                        9984
1187000205271-   C                   END
1188000205271-   C                   END
118900020527     C*---------------------------------------------------------------*
119000020527     c* CAUSALE REG. ANNO PRECEDENTE AUTOFATTURAZIONE
119100020527     c                   clear                   d1dcrra
119200020527     C*  E' impostato ?
119300020527     C                   if        §4zcrra=*blanks
119400020527     C                   SETON                                        8599
119500020527     C                   Else
119600020527     C*  Esiste ?
119700020527     C                   EVAL      OpeCausale = §4zcrra
119800020527     C     Kope          CHAIN     AnOpe01L
119900020527     C                   IF        %FOUND(AnOpe01L)
120000020527     C                   EVAL      d1dcrra = OpeDesBrev
120100020527     C                   ELSE
120200020527     C                   seton                                        9985
1203000205271-   C                   END
1204000205271-   C                   END
120500020527     C*---------------------------------------------------------------*
120600020527     c* CAUSALE REG. FATTURE DA RICEVERE AUTOFATTURAZIONE
120700020527     c                   clear                   d1dcrpa
120800020527     C*  E' impostato ?
120900020527     C                   if        §4zcrpa=*blanks
121000020527     C                   SETON                                        8699
121100020527     C                   Else
121200020527     C*  Esiste ?
121300020527     C                   EVAL      OpeCausale = §4zcrpa
121400020527     C     Kope          CHAIN     AnOpe01L
121500020527     C                   IF        %FOUND(AnOpe01L)
121600020527     C                   EVAL      d1dcrpa = OpeDesBrev
121700020527     C                   ELSE
121800020527     C                   seton                                        9986
1219000205271-   C                   END
1220000205271-   C                   END
1221000205271-   C                   END
122200020527     c* controllo la congruenza delle scelte
122300020527     c                   if        (§4zcrc <> *blanks and (§4zcrr=*blanks OR
122400020527     c                             §4ZCRP = *BLANKS)) or
122500020527     c                             (§4zcrr <> *blanks and (§4zcrc=*blanks OR
122600020527     c                             §4ZCRP = *BLANKS)) or
122700020527     c                             (§4zcrp <> *blanks and (§4zcrc=*blanks OR
122800020527     c                             §4ZCRr = *BLANKS))
122900020527     C                   seton                                        998081
123000020527     C                   seton                                        82
123100020527     c                   end
123200020527     c                   if        (§4zcrca <> *blanks and (§4zcrra=*blanks OR
123300020527     c                             §4ZCRPa= *BLANKS)) or
123400020527     c                             (§4zcrra<> *blanks and (§4zcrca=*blanks OR
123500020527     c                             §4ZCRPa= *BLANKS)) or
123600020527     c                             (§4zcrpa<> *blanks and (§4zcrca=*blanks OR
123700020527     c                             §4ZCRra= *BLANKS))
123800020527     C                   seton                                        998485
123900020527     C                   seton                                        86
124000020527     c                   end
124100980909     C*---------------------------------------------------------------*
124200940302     C     *IN99         IFEQ      *ON
124300940510     C     $GEST         ANDEQ     'D1'
124400960228     C     OPZ002        ANDNE     '07'
124500940117     C                   MOVEA     *IN           WIN
124600940128     C                   MOVE      *ALL'0'       IN5098           49
124700940128     C                   MOVEA     IN5098        *IN(50)
124800940614     C                   WRITE     D1
124900940117     C                   MOVEA     WIN           *IN
125000940302     C                   ENDIF
125100940127     C*
125200940117     C                   ENDSR
125300960305     C/EJECT
125400960305     C************************************************************
125500060724     C* CONTROLLO che il registro iva delle causali trazionisti sia lo stesso
125600960305     C************************************************************
125700060724     C     sropeser      BEGSR
125800060724     C* codice di default (A) da tabella TW mando un messaggio di avvertimen
125801060724     C* to nel caso ci siano altri codici con opeser ><
125802060724     c                   if        d1cod = tbluni and okmsg = '0'
125804060724     c     'Y4Z'         setll     anatb02l
125805060724     c                   do        *hival
125806060724     c     'Y4Z'         reade     anatb02l
125807060724     c                   if        %eof(anatb02l)
125808060724     c                   leave
125809060724     c                   end
125810060724     c                   movel(p)  atbuni        angy4zds
125811060724     c                   if        £§4ZTA ='T' and atbkey <> d1cod
125814060725     C                   EVAL      OpeCausale =£§4zcrca
125815060725     C     Kope          CHAIN     AnOpe01L
125816060725     C                   IF        %FOUND(AnOpe01L) and
125825060725     C                             opeSer <> opesers
125826060726     c                   eval      okmsg = '1'
125827060726     c                   seton                                        9987
125828060724     c                   leave
125829060724     c                   end
125830060724     c                   end
125831060724     c                   enddo
125832060724     c                   end
125833060724     c* se codice <> da quello di default se opeser <> errore bloccante
125834060724     c                   if        d1cod <>tbluni
125835060724     c                   eval      atbcod = 'Y4Z'
125836060724     c                   eval      atbkey = 'A'
125837060724     c     katb          chain     anatb02l
125838060724     c                   if        %found(anatb02l)
125839060724     c                   movel(p)  atbuni        angy4zds
125840060725     C                   EVAL      OpeCausale =£§4zcrca
125841060725     C     Kope          CHAIN     AnOpe01L
125842060725     C                   IF        %FOUND(AnOpe01L) and
125843060725     C                             opeSer <> opesers
125851060724     c                   seton                                        9984
125852060724     c                   end
125853060724     c                   end
125855060724     c                   end
125856060724     C*
125900960305     C                   ENDSR
125901060724     C/EJECT
125902060724     C************************************************************
125903060724     C* CONTROLLO VIDEATA D2
125904060724     C************************************************************
125905060724     C     CTRD2         BEGSR
125906060724     C*
125907060724     C                   ENDSR
126000940207     C/EJECT
126100940224     C************************************************************
126200940315    >C* AGGIORNAMENTO ANAGRAFICA
126300940224     C************************************************************
126400940315    >C     AGGANA        BEGSR
126500971110     C*
126600980909$007 C                   MOVE      'Y4Z'         XTBCOD
126700980909     C                   MOVE      *blanks       XTBKEY
126800980909     C                   MOVEl     D1COD         XTBKEY
126900970512     C                   MOVE      D1UNITA       XTBUNT
127000940207     C*
127100940207     C                   SELECT
127200940315    >C     OPZ002        WHENEQ    '01'
127300940315    >C     OPZ002        OREQ      '03'
127400940315    >C                   EXSR      RIEANA
127500950609     C                   MOVE      XSCCUT        XTBUTE
127600940614     C                   MOVE      '5'           XTBRIC
127700970214     C                   CALLB     'XATB'
127800940614    >C                   PARM                    XATBDS
127900940318     C* se rcd non scrivibile attivo errore
128000940615     C     XTBERR        IFEQ      '1'
128100940318    >C                   SETON                                        5999
128200940318     C                   END
128300940315    >C     OPZ002        WHENEQ    '02'
128400940621     C                   MOVE      '1'           XTBRIC
128500940621     C                   MOVE      '1'           XTBALC
128600970214     C                   CALLB     'XATB'
128700940621     C                   PARM                    XATBDS
128800940622    >C                   EXSR      RIEANA
128900950609     C                   MOVE      XSCCUT        XTBUTE
129000940614     C                   MOVE      '4'           XTBRIC
129100970214     C                   CALLB     'XATB'
129200940614    >C                   PARM                    XATBDS
129300940315    >C     OPZ002        WHENEQ    '04'
129400940621     C                   MOVE      '1'           XTBRIC
129500940621     C                   MOVE      '1'           XTBALC
129600970214     C                   CALLB     'XATB'
129700940621     C                   PARM                    XATBDS
129800940614     C                   MOVE      '6'           XTBRIC
129900970214     C                   CALLB     'XATB'
130000940614    >C                   PARM                    XATBDS
130100940621     C* annullo traduzioni
130200940621     C     XTBERR        IFEQ      '0'
130300940621     C                   EXSR      ANNTBT
130400940621     C                   END
130500940621     C*
130600940207     C                   ENDSL
130700940203     C*
130800940203     C                   ENDSR
130900940224     C/EJECT
131000940224     C************************************************************
131100940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
131200940224     C************************************************************
131300940616     C     RIEANA        BEGSR
131400940616     C                   CLEAR                   XTBUNI
131500940621     C                   MOVE      WRKDSC        XTBDSC
131600940712     C                   MOVE      D1RIS         XTBRIS
131700940616     C                   MOVEL     EXTDS         XTBUNI
131800940207     C                   ENDSR
131900940224     C/EJECT
132000940224     C************************************************************
132100940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
132200940224     C************************************************************
132300940224     C     GESAGG        BEGSR
132400940224     C*
132500940224     C* segnalo al pgm chiamante l'avvenuta conferma
132600940315    >C                   MOVE      *ON           OPR002
132700940224     C* segnalo al pgm chiamante l'aver premuto l'F6
132800940315    >C                   MOVE      '0'           RET002
132900940224     C*
1330009402241    C                   SELECT
133100940224     C* nel caso di immissione, ripristino la videata iniziale
133200940315    >C     OPZ002        WHENEQ    '01'
133300940224     C                   MOVE      *ON           $INZD1
133400940224     C* altrimenti ritorno al pgm chiamante
133500940224     C                   OTHER
133600940224     C                   MOVE      *ON           $FINE
1337009402241-   C                   ENDSL
133800940224     C*
133900940224     C                   ENDSR
134000940621     C/EJECT
134100940621     C************************************************************
134200940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
134300940621     C************************************************************
134400940621    >C     ANNTBT        BEGSR
134500940621     C                   MOVE      XTBAZI        TBTAZI
134600940621     C                   MOVE      XTBCOD        TBTCOD
134700980909     C                   MOVE      XTBKEY        TBTKEY
134800940621     C     KTBT          SETLL     ANTBT01L
134900940621     C                   DO        *HIVAL
135000940621     C     KTBT          DELETE    ANTBT01L                           21
135100940621     C  N21              END
135200940621     C*
135300940621     C                   ENDSR
135400940207     C/EJECT
135500940131     C************************************************************
135600940131     C* OPERAZIONI INIZIALI
135700940131     C************************************************************
135800940131     C     *INZSR        BEGSR
135900940131     C*
136000940127     C* Reperimento parametri
136100940127     C*
136200940117     C     *ENTRY        PLIST
136300940117     C                   PARM                    KPJBA
136400980910     C*
136500980910     C* Imposto data
136600980910     C                   CALLb     'XDT4000'
136700980910     C                   PARM                    XDTAMG           10
136800980910     C                   PARM      UDATE         XDTGMA            6 0
136900980910     C                   PARM      4             XDTSTA            1 0
137000980910     C                   MOVEL     XDTAMG        WDATE
137001060724     c* aggancio tabella TW per reperire causale tabella Y4Z di default
137002060724     c                   z-add     1             tblkut
137003060724     c                   eval      tblcod = 'TW'
137004060724     c                   movel(p)  '1'           tblkey
137005060724     c                   clear                   tbluni
137006060724     c     ktab          chain     tabel00f
137100940506     C*
137200940506     C* Reperimento tasti di funzione
137300940506     C*
137400940506     C                   EXSR      REPKEY
137500940621     C*
137600940621     C* KLIST
137601060724     C     Katb          KLIST
137602060724     C                   KFLD                    atbcod
137604060724     C                   KFLD                    atbkey
137605060724     C     Ktab          KLIST
137606060724     C                   KFLD                    tblkut
137607060724     C                   KFLD                    tblcod
137608060724     C                   KFLD                    tblkey
137800940621     C     KTBT          KLIST
137900940621     C                   KFLD                    TBTAZI
138000940621     C                   KFLD                    TBTCOD
138100940621     C                   KFLD                    TBTKEY
138200980909     C     KRCO          KLIST
138300980909     C                   KFLD                    ksocieta
138400980909     C                   KFLD                    kkcc
138500980909     C                   KFLD                    kksc
138600020415     C     KRCO1         KLIST
138700020415     C                   KFLD                    ksocieta
138800020415     C                   KFLD                    kkcc
138900980909     C     KVCI01        KLIST
139000980909     C                   KFLD                    ksocieta
139100980909     C                   KFLD                    kVoce
139200980909     C     KVCI02        KLIST
139300980909     C                   KFLD                    ksocieta
139400980909     C                   KFLD                    kkcc
139500980909     C                   KFLD                    kksc
139600980909     C                   KFLD                    kVoce
139700980909     C     Kent          KLIST
139800980909     C                   KFLD                    ksocieta
139900980909     C                   KFLD                    ktprifan
140000980909     C                   KFLD                    kentita
140100980909     C     K02Riv02      KLIST
140200980909     C                   KFLD                    ksocieta
140300980909     C                   KFLD                    kaliq
140400020412     C     Kope          KLIST
140500020412     C                   KFLD                    OpeSocieta
140600020412     C                   KFLD                    OpeCausale
140700940506     C*
140800980909     C     *LIKE         Define    rcosocieta    ksocieta
140900980909     C     *LIKE         Define    rcokcc        kkcc
141000980909     C     *LIKE         DEfine    rcoksc        kksc
141100980909     C     *LIKE         Define    vcivoce       kVoce
141200980909     C     *LIKe         DEfine    enttprifan    ktprifan
141300980909     C     *LIKE         DEfine    ententita     kentita
141400980909     C     *LIKE         DEfine    rivaliq       kaliq
141500980910     C     *LIKE         DEFINE    X06DAT        WDATE
141600980909     C*
141700940223     C                   ENDSR
141800941103     C/EJECT
141900941103      ************************************************************
142000941103      * Reperimento dati società
142100941103      ************************************************************
142200941103     C     REPSOC        BEGSR
142300941103      *
142400970214     C                   CALLB     'XSOC'
142500941103     C                   PARM                    TIPXSC            6
142600941103     C                   PARM                    SOCXSC            3
142700941103     C                   PARM                    CDSXSC            9 0
142800941103     C                   PARM                    MODXSC            3
142900941103     C                   PARM      *BLANKS       RTNXSC            1
143000941103     C                   PARM                    XSOCDS
143100941103     C                   PARM                    KPJBA
143200941103      *
143300941103     C                   ENDSR
143400940223     C/EJECT
143500940506     C************************************************************
143600940506     C* REPERIMENTO TASTI DI FUNZIONE
143700940506     C************************************************************
143800940506     C     REPKEY        BEGSR
143900940506     C                   MOVEA     $FM1          $FM
144000940506     C                   EXSR      REPKKK
144100940506     C                   MOVEA     $FX           $FX1
144200940614     C                   ENDSR
144300940506     C/EJECT
144400940506     C************************************************************
144500940506     C* REPERIMENTO TASTI DI FUNZIONE
144600940506     C************************************************************
144700940506     C     REPKKK        BEGSR
144800940506     C*
144900940506     C                   CLEAR                   $FX
145000940506     C     1             DO        24            X
145100940506     C*
145200940506     C     $FM(X)        IFNE      *BLANK
145300940506     C*
145400940506     C                   MOVEL     $FM(X)        IDMSG
145500970214     C                   CALLB     'XRTVM'
145600940506     C                   PARM                    IDMSG            27
145700940506     C                   PARM                    TXTMSG          100
145800940506     C                   PARM                    ERRMSG            1
145900940506     C*
146000940506     C     ERRMSG        IFNE      *ON
146100940506     C                   MOVEL     TXTMSG        $FX(X)
146200940506     C                   ELSE
146300940506     C                   MOVEL     *ALL' '       $FX(X)
146400940506     C                   END
146500940506     C*
146600940506     C                   END
146700940506     C*
146800940506     C                   END
146900940506     C*
147000940506     C                   ENDSR
147100940509     C/EJECT
147200940223     C************************************************************
147300940223     C* INIZIALIZZAZIONE VARIABILI
147400940223     C************************************************************
147500940223     C     INZVAR        BEGSR
147600951124     C*
147700951124     C* Chiamata XSOC
147800951124     C*
147900951124     C                   MOVEL     'SOC001'      TIPXSC
148000951124     C                   EXSR      REPSOC
148100951124     C     RTNXSC        IFNE      '1'
148200951124     C                   MOVEL     XSOCDS        SOC001
148300951124     C                   ELSE
148400951124     C                   MOVE      *ON           $FINE
148500951124     C* fine programma
148600951124     C                   EXSR      ENDPGM
148700951124     C                   ENDIF
148800940224     C*
148900940224     C* Dati da PGM guida
149000940224     C*
149100940315    >C                   MOVEL     KPJBU         D002DS
149200940128     C*
149300940223     C* Variabili per gestione videate
149400940128     C*
149500940223     C                   MOVE      'D1'          $GEST
149600940223     C                   MOVE      *OFF          $FINE
149700940223     C                   MOVE      *ON           $INZD1
149800940224     C                   MOVE      *BLANK        $LASTG
149900940224     C                   MOVE      *BLANK        $LASTV
150000940506     C                   Z-ADD     0             $ULKD1            3 0
150100020412     C                   EVAL      OpeSocieta = XscSoc
150200940127     C*
150300940223     C* Variabili appoggio
150400940127     C*
150500940224     C                   Z-ADD     0             CURR
150600940224     C                   Z-ADD     0             CURC
150700940207     C*
150800940207     C* Valorizzazione campi univoci testate
150900940207     C*
151000940614     C                   CLEAR                   T1
151100940614     C                   MOVEL     KNSIF         NOMSIF
151200941109     C                   MOVEL     XSCDSI        NOMDIT
151300941027     C                   MOVEL     DSPGM         NOMPGM
151400940224     C                   Z-ADD     1             X
151500940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
151600940224     C     *IN21         IFEQ      *ON
151700940224     C                   MOVEL     $ID(X)        IDMSG
151800970214     C                   CALLB     'XRTVM'
151900940224     C                   PARM                    IDMSG            27
152000940224     C                   PARM                    TXTMSG          100
152100940224     C                   PARM                    ERRMSG            1
152200940224     C                   PARM      *ON           CTRMSG            1
152300940224     C                   PARM      30            LENMSG            3 0
152400940224     C     ERRMSG        IFNE      *ON
152500940224     C                   MOVEL     TXTMSG        T1OPE
152600940224     C                   ELSE
152700940224     C                   MOVEL     *ALL'?'       T1OPE
152800940224     C                   ENDIF
152900940224     C                   END
153000940224     C*
153100940224     C* Inizializzazione/Decodifica prima videata
153200940224     C*
153300940224     C                   EXSR      INZD1
153400940224     C                   MOVE      *OFF          $INZD1
153500960228     C* Se opzione = '07' richiesti solo controlli
153600960228     C     OPZ002        IFEQ      '07'
153700960228     C                   MOVEL     *IN99         ERR002
153800960228     C                   MOVEL     '0'           RET002
153900960228     C                   MOVEL     *ON           $FINE
154000960228     C                   ENDIF
154100940127     C*
154200940117     C                   ENDSR
154300940128     C************************************************************
154400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
154500940223     C************************************************************
154600940223     C     DEFCAM        BEGSR
154700940223     C*
154800940223     C* klist
154900940207     C*
155000940223     C*
155100940223     C* Variabili per gestione videate
155200940223     C*
155300940223     C                   MOVE      *BLANK        $GEST             2
155400940223     C                   MOVE      *BLANK        $FINE             1
155500940223     C                   MOVE      *BLANK        $INZD1            1
155600940223     C                   MOVE      *BLANK        $LASTG            2
155700940223     C                   MOVE      *BLANK        $LASTV            2
155800940223     C*
155900940223     C* Variabili appoggio
156000940223     C*
156100940223     C                   Z-ADD     0             CURR              2 0
156200940223     C                   Z-ADD     0             CURC              2 0
156300940223     C                   MOVE      *ZEROS        WIN              99
156400940224     C                   MOVE      *BLANK        WTBERR            1
156500940223     C*
156600940223     C* Indici
156700940223     C*
156800940223     C                   Z-ADD     0             X                 3 0
156900940223     C                   Z-ADD     0             Y                 3 0
157000940127     C*
157100940117     C                   ENDSR
157200940128     C************************************************************
157300940509     C/EJECT
157400940509     C************************************************************
157500940509     C* CONTROLLO ABILITAZIONE TASTI
157600940509     C************************************************************
157700940509     C     CTRKEY        BEGSR
157800940509     C                   SELECT
157900940509     C     $TASTO        WHENEQ    F01
158000940720     C     $FC(1)        IFEQ      '0'
158100940509     C                   SETON                                        99
158200940509     C                   ENDIF
158300940509     C     $TASTO        WHENEQ    F02
158400940720     C     $FC(2)        IFEQ      '0'
158500940509     C                   SETON                                        99
158600940509     C                   ENDIF
158700940509     C     $TASTO        WHENEQ    F03
158800940720     C     $FC(3)        IFEQ      '0'
158900940509     C                   SETON                                        99
159000940509     C                   ENDIF
159100940509     C     $TASTO        WHENEQ    F04
159200940720     C     $FC(4)        IFEQ      '0'
159300940509     C                   SETON                                        99
159400940509     C                   ENDIF
159500940509     C     $TASTO        WHENEQ    F05
159600940720     C     $FC(5)        IFEQ      '0'
159700940509     C                   SETON                                        99
159800940509     C                   ENDIF
159900940509     C     $TASTO        WHENEQ    F06
160000940720     C     $FC(6)        IFEQ      '0'
160100940509     C                   SETON                                        99
160200940509     C                   ENDIF
160300940509     C     $TASTO        WHENEQ    F07
160400940720     C     $FC(7)        IFEQ      '0'
160500940509     C                   SETON                                        99
160600940509     C                   ENDIF
160700940509     C     $TASTO        WHENEQ    F08
160800940720     C     $FC(8)        IFEQ      '0'
160900940509     C                   SETON                                        99
161000940509     C                   ENDIF
161100940509     C     $TASTO        WHENEQ    F09
161200940720     C     $FC(09)       IFEQ      '0'
161300940509     C                   SETON                                        99
161400940509     C                   ENDIF
161500940509     C     $TASTO        WHENEQ    F10
161600940720     C     $FC(10)       IFEQ      '0'
161700940509     C                   SETON                                        99
161800940509     C                   ENDIF
161900940509     C     $TASTO        WHENEQ    F11
162000940720     C     $FC(11)       IFEQ      '0'
162100940509     C                   SETON                                        99
162200940509     C                   ENDIF
162300940509     C     $TASTO        WHENEQ    F12
162400940720     C     $FC(12)       IFEQ      '0'
162500940509     C                   SETON                                        99
162600940509     C                   ENDIF
162700940509     C     $TASTO        WHENEQ    F13
162800940720     C     $FC(13)       IFEQ      '0'
162900940509     C                   SETON                                        99
163000940509     C                   ENDIF
163100940509     C     $TASTO        WHENEQ    F14
163200940720     C     $FC(14)       IFEQ      '0'
163300940509     C                   SETON                                        99
163400940509     C                   ENDIF
163500940509     C     $TASTO        WHENEQ    F15
163600940720     C     $FC(15)       IFEQ      '0'
163700940509     C                   SETON                                        99
163800940509     C                   ENDIF
163900940509     C     $TASTO        WHENEQ    F16
164000940720     C     $FC(16)       IFEQ      '0'
164100940509     C                   SETON                                        99
164200940509     C                   ENDIF
164300940509     C     $TASTO        WHENEQ    F17
164400940720     C     $FC(17)       IFEQ      '0'
164500940509     C                   SETON                                        99
164600940509     C                   ENDIF
164700940509     C     $TASTO        WHENEQ    F18
164800940720     C     $FC(18)       IFEQ      '0'
164900940509     C                   SETON                                        99
165000940509     C                   ENDIF
165100940509     C     $TASTO        WHENEQ    F19
165200940720     C     $FC(19)       IFEQ      '0'
165300940509     C                   SETON                                        99
165400940509     C                   ENDIF
165500940509     C     $TASTO        WHENEQ    F20
165600940720     C     $FC(20)       IFEQ      '0'
165700940509     C                   SETON                                        99
165800940509     C                   ENDIF
165900940509     C     $TASTO        WHENEQ    F21
166000940720     C     $FC(21)       IFEQ      '0'
166100940509     C                   SETON                                        99
166200940509     C                   ENDIF
166300940509     C     $TASTO        WHENEQ    F22
166400940720     C     $FC(22)       IFEQ      '0'
166500940509     C                   SETON                                        99
166600940509     C                   ENDIF
166700940509     C     $TASTO        WHENEQ    F23
166800940720     C     $FC(23)       IFEQ      '0'
166900940509     C                   SETON                                        99
167000940509     C                   ENDIF
167100940509     C     $TASTO        WHENEQ    F24
167200940720     C     $FC(24)       IFEQ      '0'
167300940509     C                   SETON                                        99
167400940509     C                   ENDIF
167500940509     C                   ENDSL
167600940509     C                   ENDSR
167700940509     C/EJECT
167800940509     C************************************************************
167900940509     C* RIEMPIMENTO TASTI FUNZIONE
168000940509     C************************************************************
168100940509     C     RIEKEY        BEGSR
168200940509     C*
168300940509     C* Input :
168400940509     C* - $FX = schiera con tutti i testi disponibili
168500940509     C* - $FC = schiere con i flag di validità associati ai testi
168600940509     C* - $FR = schiere con i flag di REVERSE IMAGE
168700940509     C* - $ULK = ultimo testo caricato
168800940509     C* Output :
168900940509     C* - $ULK
169000940509     c* - $KEY1= 1° riga di tasti funzione
169100940509     c* - $KEY2= 2° riga di tasti funzione
169200940509     C*
169300970214     C                   CALLB     'XKEYMSG'
169400940509     C* input
169500940509     C                   PARM                    $FX
169600940509     C                   PARM                    $FC
169700940509     C                   PARM                    $FR
169800940509     C                   PARM                    $ULK              3 0
169900940509     C* output
170000940509     C                   PARM                    $KEY1            79
170100940509     C                   PARM                    $KEY2            79
170200940509     C*
170300940509     C                   ENDSR
170400940615**
17050094022401PROMSG    *LIBL     COS0001    immissione
17060094022402PROMSG    *LIBL     COS0002    modifica
17070094022403PROMSG    *LIBL     COS0003    copia
17080094022404PROMSG    *LIBL     COS0004    annullamento
17090094022405PROMSG    *LIBL     COS0005    visualizzazione
171000940509** TASTI DI FUNZIONE  D1
171100940509PROMSG    *LIBL     KEY0001  01
171200940509PROMSG    *LIBL     KEY0002  02
171300940509PROMSG    *LIBL     KEY0003  03
171400940509PROMSG    *LIBL     KEY0004  04
171500940509PROMSG    *LIBL     KEY0005  05
171600940509PROMSG    *LIBL     KEY0006  06
171700940509PROMSG    *LIBL     KEY0007  07
171800940509PROMSG    *LIBL     KEY0008  08
171900940509PROMSG    *LIBL     KEY0009  09
172000940509PROMSG    *LIBL     KEY0010  10
172100940509PROMSG    *LIBL     KEY0011  11
172200940509PROMSG    *LIBL     KEY0012  12
172300940509PROMSG    *LIBL     KEY0013  13
172400940509PROMSG    *LIBL     KEY0014  14
172500940509PROMSG    *LIBL     KEY0015  15
172600940509PROMSG    *LIBL     KEY0016  16
172700940509PROMSG    *LIBL     KEY0017  17
172800940509PROMSG    *LIBL     KEY0018  18
172900940509PROMSG    *LIBL     KEY0019  19
173000940509PROMSG    *LIBL     KEY0020  20
173100940509PROMSG    *LIBL     KEY0021  21
173200940509PROMSG    *LIBL     KEY0022  22
173300940509PROMSG    *LIBL     KEY0023  23
173400940509PROMSG    *LIBL     KEY0024  24
173500940509**  ABILITAZIONE D1
1736009407200N  01
1737009407200N  02
1738009407201N  03
1739009407201N  04
1740009407201N  05
1741009407201N  06
1742009407201N  07
1743009407201N  08
1744009407200N  09
1745009407200N  10
1746009407200N  11
1747009407201N  12
1748009407200N  13
1749009407200N  14
1750009407200N  15
1751009407200N  16
1752009407200N  17
1753009407200N  18
1754009407200N  19
1755009407200N  20
1756009407200N  21
1757009407200N  22
1758009407200N  23
1759009407200N  24
