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
004500060724     Fanatb02l  IF   E           K DISK
004600060724     Ftabel00f  IF   E           K DISK
004700020412     FANRCO99J  IF   E           K DISK
004800980909     FANent01L  IF   E           K DISK
004900980909     FANVCI01L  IF   E           K DISK
005000140129     F*ANVCI02L  IF   E           K DISK    RENAME(ANVCI000:ANVCI002)
005100980909     FAnRiv02L  IF   E           K DISK
005200940128     D*----------------------------------------------------*
005300940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005400940224     D $ID             S             27    DIM(5) ALT($OZ)
005500940506     D* Schiere per la gestione dei tasti di funzione
005600940506     D $FM             S             27    DIM(24)
005700940506     D $FX             S             50    DIM(24)
005800940506     D $FC             S              1    DIM(24)
005900940506     D $FR             S              1    DIM(24)
006000940506     D* Video D1
006100940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006200940506     D $FX1            S             50    DIM(24) ALT($FM1)
006300940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006400940506     D $FR1            S              1    DIM(24) ALT($FC1)
006500980909$003 D EXTDS         E DS                  EXTNAME(ANGY4ZDS) INZ
006600940127     D* Passaggio Parametri
006700940127     D KPJBA         E DS
006800980910     D* Richiamo a XTAB
006900980910    >D XTABDS        E DS                  INZ
007000980910     D*-------------
007100980910     D* Parametri
007200980910     D X06DS         E DS                  EXTNAME(X06G48DS)
007300980910     D  X06SOC       E                     EXTFLD(X06SOCIETA)
007400980910     D  X06CIV       E                     EXTFLD(X06CDIVA)
007500980910     D  X06DAT       E                     EXTFLD(X06DATA)
007600980910     D  X06LIN       E                     EXTFLD(X06LINGUA)
007700980909     D*-------------
007800980909     D* Ds x ricerche anagrafiche
007900060724     D angy4zds      E DS                  INZ prefix(£)
008000060724     D ana200ds      E DS                  INZ
008100980909     D ana250ds      E DS                  INZ
008200980909     D ana280ds      E DS                  INZ
008300980909     D ana064ds      E DS                  INZ
008400980910     D Ana032ds      E DS                  INZ
008500980910     D TABG48        E DS                  EXTNAME(ANGG48DS) INZ
008600941103     D*-------------
008700941103      * DS Società
008800941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008900960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
009000960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
009100941103     D*-------------
009200941103      * DS Interna per dati utente
009300941103     D XSOCDS          DS          1000
009400941103     D*-------------
009500940616     D XATBDS        E DS                  INZ
009600940318     D*-------------
009700020412     D* DS Esterna causali contabili
009800020412     D ANA070DS      E DS
009900940318     D* DS Esterna gestione messaggi per COMIT
010000940616     D XMSGDS        E DS                  INZ
010100940318     D  STS          E                     EXTFLD(MSGSTS)
010200940318     D                                     DIM(333)
010300940318     D  JBA          E                     EXTFLD(MSGJBA)
010400940318     D                                     DIM(502)
010500940203     D*-------------
010600940620     D* DS Esterna autorizzazioni tabelle
010700940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
010800940620     D*
010900940620     D* DS  restituzione autorizzazioni
011000940620     D* IND=indice
011100940620     D* GRU=man. di gruppo
011200940620     D* AZI=man. azienda
011300940712     D* ABI=abilit. ai riservati
011400941111     D AUTDS           DS
011500940620     D  MANIND                 1      1
011600940620     D  MANGRU                 2      2
011700940620     D  MANAZI                 3      3
011800940712     D  ABIRIS                 4      4
011900940620     D*-------------
012000940207     D* opzione richiesta
012100940614    >D D002DS          DS
012200940614     D  OPZ002                 1      2
012300940614     D* 0 = ritorno normale
012400940614     D* 1 = F3
012500940614     D* 2 = F12
012600940614     D  RET002                 3      3
012700940614     D* operazione eseguite dal pgm chiamato :
012800940614     D* 0 = nessuna operazione
012900940614     D* 1 = eseguito aggiornamento
013000940614     D  OPR002                 4      4
013100940614     D* funzione non eseguibile per errore :
013200940614     D* 0 = nessun errore
013300940614     D* 1 = funzione richiamata chiusa in errore
013400940614     D* Aggiungere eventuali altri codici di errore
013500940614     D  ERR002                 5      5
013600940614     D* Azienda
013700940614     D  AZI002                 6      8
013800940614     D* Lingua
013900940614     D  LIN002                 9      9
014000940614     D* Codice scelto
014100940614     D  KEY002                10     24
014200940620     D* Autorizzazione
014300940620     D  AUT002                25     25
014400970509     D* Unita
014500970509     D  UNI002                26     33
014600940614     D*-------------
014700940201     D*-------------
014800940201     D DSFMT           DS
014900940506     D  $TASTO               369    369
015000940201     D  NRG                  370    370
015100940201     D  NCL                  371    371
015200940201     D*-------------
015300940201     D* posizione cursore
015400940201     D CURSOR          DS
015500940223     D  RIRI                   1      2B 0 INZ
015600940201     D  R$$                    2      2
015700940223     D  COCO                   3      4B 0 INZ
015800940201     D  C$$                    4      4
015900940207     D*-------------
016000940207     D* Reperimento nome PGM
016100940207     D STATUS         SDS           333
016200940207     D  DSPGM            *PROC
016300940225     D*-------------
016400940225     D* COSTANTI
016500940225     D*-------------
016600940506     D*
016700940506     D* Tasti di funzione
016800940506     D F01             C                   CONST(X'31')
016900940506     D F02             C                   CONST(X'32')
017000940506     D F03             C                   CONST(X'33')
017100940506     D F04             C                   CONST(X'34')
017200940506     D F05             C                   CONST(X'35')
017300940506     D F06             C                   CONST(X'36')
017400940506     D F07             C                   CONST(X'37')
017500940506     D F08             C                   CONST(X'38')
017600940506     D F09             C                   CONST(X'39')
017700940506     D F10             C                   CONST(X'3A')
017800940506     D F11             C                   CONST(X'3B')
017900940506     D F12             C                   CONST(X'3C')
018000940506     D F13             C                   CONST(X'B1')
018100940506     D F14             C                   CONST(X'B2')
018200940506     D F15             C                   CONST(X'B3')
018300940506     D F16             C                   CONST(X'B4')
018400940506     D F17             C                   CONST(X'B5')
018500940506     D F18             C                   CONST(X'B6')
018600940506     D F19             C                   CONST(X'B7')
018700940506     D F20             C                   CONST(X'B8')
018800940506     D F21             C                   CONST(X'B9')
018900940506     D F22             C                   CONST(X'BA')
019000940506     D F23             C                   CONST(X'BB')
019100940506     D F24             C                   CONST(X'BC')
019200940506     D ENTER           C                   CONST(X'F1')
019300940506     D ROLDWN          C                   CONST(X'F4')
019400940506     D ROLLUP          C                   CONST(X'F5')
019500940506     D*-------------
019600940506     D*
019700941108     D DATA            C                   CONST('0001-01-01')
019800940128     I*----------------------------------------------------*
019900940615     ID1
020000980909$005 I              D1DES                       §4ZDES
020100980909$005 I              D1KCC                       §4ZKCC
020200980909$005 I              D1KSC                       §4ZKSC
020300980909$005 I              D1VOC                       §4ZVOC
020400980909$005 I              D1CDB                       §4ZCDB
020500980909$005 I              D1CDS                       §4ZCDS
020600980909$005 I              D1KCI                       §4ZKCI
020700980909$005 I              D1KSI                       §4ZKSI
020800980911$005 I              D1KCR                       §4ZKCR
020900980911$005 I              D1KSR                       §4ZKSR
021000980909$005 I              D1IVC                       §4ZIVC
021100980909$005 I              D1IVA                       §4ZIVA
021200020415$005 I              D1crc                       §4Zcrc
021300020415$005 I              D1crp                       §4Zcrp
021400020415$005 I              D1crr                       §4Zcrr
021500020527$005 I              D1crca                      §4Zcrca
021600020527$005 I              D1crpa                      §4Zcrpa
021700020527$005 I              D1crra                      §4Zcrra
021800020415$005 I              D1ta                        §4Zta
021900980909$005 I              D1FLG                       §4ZFLG
022000150116$005 I              D1crce                      §4Zcrce
022100150116$005 I              D1crcae                     §4Zcrcae
022200940127     I*
022300940127     C*----------------------------------------------------*
022400940127     C*                MAIN LINE PROGRAM
022500940127     C*----------------------------------------------------*
022600940127     C*
022700940223     C* inizializzazione variabili
022800940223     C                   EXSR      INZVAR
022900940223     C*
023000940223     C     $FINE         DOWEQ     *OFF
023100940202     C     $GEST         CASEQ     'D1'          GESD1
023200940117     C                   END
023300940117     C                   END
023400940201     C*
023500940325     C* fine programma
023600940325     C                   EXSR      ENDPGM
023700940325     C/EJECT
023800940325     C************************************************************
023900940325     C* RICERCHE
024000940325     C************************************************************
024100940325     C     ENDPGM        BEGSR
024200940325     C*
024300940614     C* disallocazione rcd nel caso di
024400971110$007 C                   MOVE      'Y4Z'         XTBCOD
024500940614    >C                   MOVE      '7'           XTBRIC
024600970214     C                   CALLB     'XATB'
024700940614     C                   PARM                    XATBDS
024800940325     C*
024900940325     C* passaggio dati al pgm chiamante
025000940715     C                   MOVE      *BLANKS       KPJBU
025100940325    >C                   MOVEL     D002DS        KPJBU
025200940325     C*
025300940325     C                   SETON                                            RT
025400940325     C*
025500940325     C                   ENDSR
025600940325     C/EJECT
025700940201     C************************************************************
025800940201     C* RICERCHE
025900940201     C************************************************************
026000940127     C     SEARCH        BEGSR
026100940127     C*
026200940127     C* determino Riga/Colonna del cursore
026300940127     C                   MOVE      NRG           R$$
026400940127     C                   MOVE      NCL           C$$
026500940127     C                   Z-ADD     RIRI          CURR
026600940127     C                   Z-ADD     COCO          CURC
026700940127     C*
026800940224     C* I campi che al momento dell'F4 risultano protetti non devono
026900940224     C* essere abilitati alla funzione di ricerca
027000940224     C*
0271009401271    C                   SELECT
027200980909     C* FMTD1  - capoconto
027300980909    >C     H1NMFL        WHEneq    'D1KCC'
027400980909     C                   RESET                   Ana200DS
027500980909     C                   MOVEL     '01'          OPZ200
027600980909     C                   clear                   KCC200
027700980909     C                   MOVEL     Ana200DS      KPJBU
027800980909     C                   CALL      'ANA200R'
027900980909     C                   PARM                    KPJBA
028000980909     C                   MOVEL     KPJBU         Ana200DS
028100980909     C     KCC200        IFNE      *BLANK
028200980909     C                   MOVEL     KCC200        §4Zkcc
028300980909     C                   ENDIF
028400980909     C* imposto pos. del cursore
028500980909     C                   Z-ADD     CURR          H1RIGA
028600980909     C                   Z-ADD     CURC          H1COLO
028700980909     C*
028800980909     C* Ricerca sottoconti (ANA250R)
028900980909     C     H1NMFL        WHENEQ    'D1KSC'
029000980909     C                   RESET                   ana250DS
029100980909     C                   MOVEL     '01'          OPZ250
029200980909     C                   MOVEL     §4Zkcc        KCC250
029300980909     C                   clear                   KSC250
029400980909     C                   MOVEL     ana250DS      KPJBU
029500980909     C                   CALL      'ANA250R'
029600980909     C                   PARM                    KPJBA
029700980909     C                   MOVEL     KPJBU         ana250DS
029800980909     C     KSC250        IFNE      *BLANK
029900980909     C                   MOVEL     KCC250        §4zkcc
030000980909     C                   MOVEL     KSC250        §4zksc
030100980909     C                   ENDIF
030200980909     C* imposto pos. del cursore
030300980909     C                   Z-ADD     CURR          H1RIGA
030400980909     C                   Z-ADD     CURC          H1COLO
030500980909     C*
030600980909     C* FMTD1  - codice voce
030700980909    >C     H1NMFL        WHEneq    'D1VOC'
030800980909     C                   RESET                   Ana280DS
030900980909     C                   MOVEL     '01'          OPZ280
031000980909     C                   clear                   VOC280
031100980909     C                   MOVEL     Ana280DS      KPJBU
031200980909     C                   CALL      'ANA280R'
031300980909     C                   PARM                    KPJBA
031400980909     C                   MOVEL     KPJBU         Ana280DS
031500980909     C     voc280        IFNE      *BLANK
031600980909     C                   MOVEL     VOC280        §4Zvoc
031700980909     C                   ENDIF
031800980909     C* imposto pos. del cursore
031900980909     C                   Z-ADD     CURR          H1RIGA
032000980909     C                   Z-ADD     CURC          H1COLO
032100980910     C*
032200980910     C* FMTD3  - RIFERIMENTO ENTITA'
032300980910     C     H1NMFL        WHENEQ    'D1CDB'
032400980910     C     H1NMFL        OREQ      'D1CDS'
032500980910    >C                   RESET                   ANA032DS
032600980910    >C                   MOVEL     '01'          OPZ032
032700980910     C                   MOVE      'S'           FSC032
032800980910     C                   MOVE      'CDC'         TPE032
032900980910    >C                   MOVEL     ANA032Ds      KPJBU
033000980910    >C                   CALL      'ANA032R'
033100980910     C                   PARM                    KPJBA
033200980910    >C                   MOVEL     KPJBU         ANA032DS
033300980910     C*
033400980910     C     ENT032        IFNE      *BLANKS
033500980910     C                   SELECT
033600980910     C     H1NMFL        WHENEQ    'D1CDB'
033700980910     C                   MOVEL     ENT032        §4ZCDB
033800980910     C     H1NMFL        WHENEQ    'D1CDS'
033900980910     C                   MOVEL     ENT032        §4ZCDS
034000980910     C                   ENDSL
034100980910     C                   ENDIF
034200980909     C* FMTD1  - codice iva
034300980909    >C     H1NMFL        WHEneq    'D1IVA'
034400980909     C                   RESET                   Ana064DS
034500980909     C                   MOVEL     '2'           TPR064
034600980909     C                   MOVEL     '01'          OPZ064
034700980909     C                   MOVEL     Ana064DS      KPJBU
034800980909     C                   CALL      'ANA064R'
034900980909     C                   PARM                    KPJBA
035000980909     C                   MOVEL     KPJBU         Ana064DS
035100980909     C     ali064        IFNE      *zeros
035200980909     C                   MOVEL     ali064        §4ZIVA
035300980909     C                   ENDIF
035400980909     C* imposto pos. del cursore
035500980909     C                   Z-ADD     CURR          H1RIGA
035600980909     C                   Z-ADD     CURC          H1COLO
035700980909     C*
035800980909     C* FMTD1  - codice esenzione
035900980909    >C     H1NMFL        WHEneq    'D1IVC'
036000980910     C                   RESET                   XTABDS
036100980910     C                   MOVE      XSCSOC        XTAAZI
036200980910     C                   MOVE      XSCLIN        XTALIN
036300980910    >C                   MOVE      *BLANK        WWIVA            13
036400980910     C                   MOVE      WWIVA         XTAKEY
036500980910     C                   MOVE      '01'          XTAOPZ
036600980910     C                   Z-ADD     CURR          XTARIG
036700980910     C                   Z-ADD     CURC          XTACOL
036800980910     C                   CLEAR                   KPJBU
036900980910     C                   MOVEL     XTABDS        KPJBU
037000980910     C                   CALL      'ANGG48R'
037100980910     C                   PARM                    KPJBA
037200980910     C                   MOVEL     KPJBU         XTABDS
037300980910     C     XTAERR        IFNE      '1'
037400980910     C                   MOVE      XTAKEY        WWIVA
037500980910     C                   ELSE
037600980910     C                   MOVE      *BLANK        WWIVA
037700980910     C                   ENDIF
037800980910    >C     WWIVA         IFNE      *BLANK
037900980910    >C                   MOVEL     WWIVA         §4ZIVC
038000980910    >C                   ENDIF
038100980909     C* imposto pos. del cursore
038200980909     C                   Z-ADD     CURR          H1RIGA
038300980909     C                   Z-ADD     CURC          H1COLO
038400980909     C* FMTD1  - capoconto IVA
038500980909    >C     H1NMFL        WHEneq    'D1KCI'
038600980909     C                   RESET                   Ana200DS
038700980909     C                   MOVEL     '01'          OPZ200
038800980909     C                   clear                   KCC200
038900980909     C                   MOVEL     Ana200DS      KPJBU
039000980909     C                   CALL      'ANA200R'
039100980909     C                   PARM                    KPJBA
039200980909     C                   MOVEL     KPJBU         Ana200DS
039300980909     C     KCC200        IFNE      *BLANK
039400980909     C                   MOVEL     KCC200        §4ZKCI
039500980909     C                   ENDIF
039600980909     C* imposto pos. del cursore
039700980909     C                   Z-ADD     CURR          H1RIGA
039800980909     C                   Z-ADD     CURC          H1COLO
039900980909     C*
040000980909     C* Ricerca sottoconti (ANA250R)
040100980909     C     H1NMFL        WHENEQ    'D1KSI'
040200980909     C                   RESET                   ana250DS
040300980909     C                   MOVEL     '01'          OPZ250
040400980909     C                   MOVEL     §4ZKCI        KCC250
040500980909     C                   clear                   KSC250
040600980909     C                   MOVEL     ana250DS      KPJBU
040700980909     C                   CALL      'ANA250R'
040800980909     C                   PARM                    KPJBA
040900980909     C                   MOVEL     KPJBU         ana250DS
041000980909     C     KSC250        IFNE      *BLANK
041100980909     C                   MOVEL     KCC250        §4ZKCI
041200980909     C                   MOVEL     KSC250        §4ZKSI
041300980909     C                   ENDIF
041400980909     C* imposto pos. del cursore
041500980909     C                   Z-ADD     CURR          H1RIGA
041600980909     C                   Z-ADD     CURC          H1COLO
041700980911     C* FMTD1  - capoconto FAtture da ricevere
041800980911    >C     H1NMFL        WHEneq    'D1KCR'
041900980911     C                   RESET                   Ana200DS
042000980911     C                   MOVEL     '01'          OPZ200
042100980911     C                   clear                   KCC200
042200980911     C                   MOVEL     Ana200DS      KPJBU
042300980911     C                   CALL      'ANA200R'
042400980911     C                   PARM                    KPJBA
042500980911     C                   MOVEL     KPJBU         Ana200DS
042600980911     C     KCC200        IFNE      *BLANK
042700980911     C                   MOVEL     KCC200        §4ZKCr
042800980911     C                   ENDIF
042900980911     C* imposto pos. del cursore
043000980911     C                   Z-ADD     CURR          H1RIGA
043100980911     C                   Z-ADD     CURC          H1COLO
043200980911     C*
043300980911     C* Ricerca sottoconti (ANA250R)
043400980911     C     H1NMFL        WHENEQ    'D1KSR'
043500980911     C                   RESET                   ana250DS
043600980911     C                   MOVEL     '01'          OPZ250
043700980911     C                   MOVEL     §4ZKCR        KCC250
043800980911     C                   clear                   KSC250
043900980911     C                   MOVEL     ana250DS      KPJBU
044000980911     C                   CALL      'ANA250R'
044100980911     C                   PARM                    KPJBA
044200980911     C                   MOVEL     KPJBU         ana250DS
044300980911     C     KSC250        IFNE      *BLANK
044400980911     C                   MOVEL     KCC250        §4ZKCR
044500980911     C                   MOVEL     KSC250        §4ZKSR
044600980911     C                   ENDIF
044700020412     C* imposto pos. del cursore
044800020412     C                   Z-ADD     CURR          H1RIGA
044900020412     C                   Z-ADD     CURC          H1COLO
045000020412     C*
045100020412     C* Ricerca causali contabili
045200020412     C                   WHEN      h1nmfl = 'D1CRC'or
045300020412     C                             h1nmfl = 'D1CRR'or
045400020527     C                             h1nmfl = 'D1CRP'or
045500020527     C                             h1nmfl = 'D1CRCA'or
045600020527     C                             h1nmfl = 'D1CRRA'or
045700150116     C                             h1nmfl = 'D1CRPA'or
045800150116     C                             h1nmfl = 'D1CRCE'or
045900150116     C                             h1nmfl = 'D1CRCAE'
046000020412     C                   RESET                   ANA070DS
046100020412     C                   EVAL      OPZ070 = '01'
046200020412     C                   EVAL      Kpjbu = ANA070DS
046300020412     C                   CALL      'ANA070R'
046400020412     C                   PARM                    Kpjba
046500020412     C                   EVAL      ANA070DS = Kpjbu
046600020412      *
046700020412     C                   IF        RET070 = *BLANKS
046800020412     C                   SELECT
046900020412     C                   WHEN      H1NMFL = 'D1CRC'
047000020412     C                   EVAL      §4zcrc  = Cau070
047100020412     C                   WHEN      H1NMFL = 'D1CRR'
047200020412     C                   EVAL      §4zcrr = Cau070
047300020412     C                   WHEN      H1NMFL = 'D1CRP'
047400020412     C                   EVAL      §4zcrp = Cau070
047500020527     C                   WHEN      H1NMFL = 'D1CRPA'
047600020527     C                   EVAL      §4zcrpa = Cau070
047700020527     C                   WHEN      H1NMFL = 'D1CRRA'
047800020527     C                   EVAL      §4zcrra = Cau070
047900020527     C                   WHEN      H1NMFL = 'D1CRCA'
048000020527     C                   EVAL      §4zcrca = Cau070
048100150116     C                   WHEN      H1NMFL = 'D1CRCE'
048200150116     C                   EVAL      §4zcrcE  = Cau070
048300150116     C                   WHEN      H1NMFL = 'D1CRCAE'
048400150116     C                   EVAL      §4zcrcAE= Cau070
048500020412     C                   ENDSL
048600020412     C                   ENDIF
048700020412     C* imposto pos. del cursore
048800020412     C                   Z-ADD     CURR          H1RIGA
048900020412     C                   Z-ADD     CURC          H1COLO
049000940127     C*
049100940223     C*
049200940223     C* ricerca non abilitata in questa posizione
0493009402231O   C                   OTHER
049400941005     C                   SETON                                        9899
049500941005     C* imposto pos. del cursore
049600941005     C                   Z-ADD     CURR          H1RIGA
049700941005     C                   Z-ADD     CURC          H1COLO
049800940127     C*
0499009401271-   C                   ENDSL
050000940127     C*
050100940506     C                   MOVE      *HIVAL        $LASTV
050200940127     C                   ENDSR
050300940615     C/EJECT
050400940615     C************************************************************
050500940615     C* TRADUZIONI
050600940615     C************************************************************
050700940615     C     LINGUA        BEGSR
050800940615     C*
050900940615     C* determino Riga/Colonna del cursore
051000940615     C                   MOVE      NRG           R$$
051100940615     C                   MOVE      NCL           C$$
051200940615     C                   Z-ADD     RIRI          CURR
051300940615     C                   Z-ADD     COCO          CURC
051400940615     C* FMTD1  - CAMPI DA TRADURRE
051500940615     C* imposto pos. del cursore
051600940615     C                   Z-ADD     CURR          H1RIGA
051700940615     C                   Z-ADD     CURC          H1COLO
051800940621     C                   MOVEL     H1NMFL        W005              5
051900940621     C                   MOVE      W005          W003              3
052000940621     C                   MOVEL     W003          W006              6
052100940620     C                   MOVE      *ZERO         KEY              15
052200980909     C                   MOVEl     D1COD         KEY
052300940615     C                   SELECT
052400940615     C* ricerca non abilitata in questa posizione
0525009406151O   C                   OTHER
052600940615     C                   SETON                                        98
052700940615     C*
0528009406151-   C                   ENDSL
052900940615     C*
053000940615     C                   MOVE      *HIVAL        $LASTV
053100940615     C                   ENDSR
053200940225     C/EJECT
053300940225     C************************************************************
053400940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
053500940225     C************************************************************
053600940225     C     GESPIE        BEGSR
053700940225     C*
053800940225     C                   SELECT
053900940225     C*
054000940506     C     $GEST         WHENEQ    'D1'
054100940506     C     $ULKD1        IFEQ      0
054200940506     C* riempimento opzioni
054300940506     C                   EXSR      F24D1
054400940506     C                   ELSE
054500940614     C                   WRITE     Z1
054600940506     C                   ENDIF
054700940225     C*
054800940510     C*
054900940225     C                   ENDSL
055000940225     C*
055100940225     C                   ENDSR
055200940131     C/EJECT
055300940131     C************************************************************
055400940510     C* GESTIONE VIDEO RECORD D1
055500940131     C************************************************************
055600940127     C     GESD1         BEGSR
055700940225     C* inizializzazione videata
055800940223     C     $INZD1        IFEQ      *ON
055900940127     C                   EXSR      INZD1
056000940223     C                   MOVE      *OFF          $INZD1
056100940117     C                   END
056200940225     C* emissione piede videata se proveniente da altra
056300940225     C* salvataggio valore formato di provenienza
056400940207     C     $LASTV        IFNE      'D1'
056500940614     C                   WRITE     T1
056600940225     C                   EXSR      GESPIE
056700940202     C                   MOVE      $LASTV        $LASTG
056800940202     C                   MOVE      'D1'          $LASTV
056900940202     C                   END
057000940131     C*
057100940203     C                   SELECT
057200940203     C* Annullamento
057300940315    >C     OPZ002        WHENEQ    '04'
057400940224     C* Visualizzazione
057500940315    >C     OPZ002        OREQ      '05'
057600940620     C                   SETON                                        0203
057700940620     C* Manutenzione
057800940620    >C     OPZ002        WHENEQ    '02'
057900940620     C                   SETON                                        02
058000940620     C                   SETOFF                                       03
058100940620     C*
058200940620     C                   OTHER
058300940620     C*
058400940620     C                   SETOFF                                       0203
058500940203     C                   ENDSL
058600940620     C* Immissione/Modifica/Copia
058700940620     C                   EXFMT     D1
058800940506     C* controllo abilitazione tasti funzione
058900940506     C*
059000940506     C                   EXSR      CTRKD1
059100940506     C*
059200940506     C     *IN99         IFEQ      *OFF
059300940131     C*
0594009402031    C                   SELECT
059500940510     c* ROLLUP
059600940131     C* F3=Fine
059700940506     C     $TASTO        WHENEQ    F03
059800940309     C                   EXSR      F03D1
059900940131     C* F4=Ricerca
060000940506     C     $TASTO        WHENEQ    F04
060100940127     C                   EXSR      SEARCH
060200940224     C* F5=Refresh
060300940506     C     $TASTO        WHENEQ    F05
060400940309     C                   EXSR      F05D1
060500940223     C* F8=Successivo
060600940506     C     $TASTO        WHENEQ    F08
060700940309     C                   EXSR      F08D1
060800940131     C* F12=Ritorno
060900940506     C     $TASTO        WHENEQ    F12
061000940309     C                   EXSR      F12D1
061100940315     C* F24=Altri tasti
061200940506     C     $TASTO        WHENEQ    F24
061300940315     C                   EXSR      F24D1
061400940131     C*
0615009402031O   C                   OTHER
061600980910     C                   EXSR      CTRD1
061700940616     C     *IN99         IFEQ      *OFF
061800940616     C                   SELECT
061900940616     C* F7=Traduzione
062000940616     C     $TASTO        WHENEQ    F07
062100940616     C                   EXSR      LINGUA
062200940616     C* F6
0623009406162    C     $TASTO        WHENEQ    F06
062400940224     C* eseguo aggiornamento
062500940315    >C                   EXSR      AGGANA
062600940224     C* eseguo operazioni del dopo-aggiornamento
0627009403183    C     *IN99         IFEQ      *OFF
062800940318     C                   EXSR      GESAGG
0629009403183-   C                   ENDIF
0630009406162-   C                   ENDSL
063100940616     C                   ENDIF
063200940203     C*
0633009402241-   C                   ENDSL
063400940224     C*
0635009405062-   C                   ENDIF
063600940117     C                   ENDSR
063700940506     C/EJECT
063800940506     C************************************************************
063900940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
064000940506     C************************************************************
064100940506     C     CTRKD1        BEGSR
064200940506     C                   SETOFF                                       99
064300940506     C                   MOVEA     $FC1          $FC
064400940506     C                   EXSR      CTRKEY
064500940506     C     *IN99         IFEQ      *ON
064600940506     C                   SETON                                        97
064700940506     C                   ENDIF
064800940506     C                   ENDSR
064900940117      ************************************************************
065000940207      * INIZIALIZZAZIONE VIDEATA DATI
065100940117      ************************************************************
065200940127     C     INZD1         BEGSR
065300940131     C*
065400940615     C                   CLEAR                   D1
065500980909     C                   MOVE      '0'           D1RIS
065600980909     C                   MOVEl     KEY002        D1COD
065700940615     C                   MOVE      AZI002        XTBAZI
065800940615     C                   MOVE      LIN002        XTBLIN
065900980909     C                   MOVE      *blanks       XTBKEY
066000980909     C                   MOVEl     KEY002        XTBKEY
066100970509     C                   MOVE      UNI002        XTBUNT
066200940615$007 C                   MOVE      'Y4Z'         XTBCOD
066300060724     C                   MOVE      *BLANK        opesers           4
066400060724     C                   MOVE      '0'           okmsg             1
066500940225     C*
066600940207     C*
066700940224     C* valorizzo la chiave univoca per reperire i dati
066800940224     C* se non immissione
0669009403151   >C     OPZ002        IFNE      '01'
067000940315    >C*-------------------------------------------------------------
067100940315    >C* impostare i campi chiave ricevuti
067200940315    >C*-------------------------------------------------------------
067300940318     C*
067400940614     C*
067500940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0676009406142   >C     OPZ002        IFEQ      '02'
067700940315    >C     OPZ002        OREQ      '04'
067800940614     C                   MOVE      '1'           XTBALC
067900940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0680009406142E   C                   ELSE
068100940614     C                   MOVE      '0'           XTBALC
0682009406142-   C                   ENDIF
068300940614     C*
068400940616     C                   MOVE      '1'           XTBRIC
068500970214     C                   CALLB     'XATB'
068600940614     C                   PARM                    XATBDS
068700940318     C*
068800940614     C     XTBERR        IFEQ      '0'
068900940614     C                   MOVEL     XTBUNI        EXTDS
069000970512     c                   move      xtbunt        d1uniTA           8
069100941108     C* Giro la data se non blank
069200970214     c                   move      xtbdsc        xtbdscA          10
069300970214     C     XTBDSCa       IFNE      DATA
069400970214     C                   CALLB     'XDT4000'
069500970214     C                   PARM      XTBDSCA       XDTAMG           10
069600940621     C                   PARM      *ZEROS        XDTGMA            6 0
069700940621     C                   PARM      2             XDTSTA            1 0
069800940621     C                   MOVE      XDTGMA        D1DSC
069900940712     C                   MOVE      XTBRIS        D1RIS
070000941108     C                   ELSE
070100941108     C                   MOVE      *ZEROS        D1DSC
070200941108     C                   ENDIF
070300941108     C                   ENDIF
070400940614     C*
070500940224     C*
070600940224     C* se il rcd da gestire non viene trovato
070700940318     C* oppure è allocato da altro pgm
070800940224     C* ritorno al pgm guida avvertendolo dell'errore
0709009406142    C     XTBERR        IFNE      '0'
071000940224     C                   MOVE      *ON           $FINE
071100940315    >C                   MOVE      '1'           ERR002
071200940224     C*
071300940224     C* altrimenti
0714009402242E   C                   ELSE
071500940224     C* riempio la videata
071600940224     C                   EXSR      RIED1
071700940224     C* richiamo routine dei ctrl per decodificarla
071800940614    >C                   EXSR      CTRD1
071900960228     C                   MOVEL     *IN99         SAV99             1
072000960228     C* Se la gestione è più videate
072100960305    >C                   EXSR      CTRD2
072200960305    >C     *IN99         IFEQ      *ON
072300960305    >C                   MOVEL     *IN99         SAV99             1
072400960305    >C                   ENDIF
072500940224     C                   MOVE      *ALL'0'       IN5098
072600940224     C                   MOVEA     IN5098        *IN(50)
072700940224     C* definizione indicatori su video
072800940224     C                   SETOFF                                       02
0729009402243    C                   SELECT
073000940315    >C     OPZ002        WHENEQ    '02'
073100940224     C                   SETON                                        02
073200940315    >C     OPZ002        WHENEQ    '05'
073300940506     C* disabilita tasti
073400940720     C                   MOVE      '0'           $FC(6)
073500940720     C                   MOVE      '0'           $FC(5)
073600940720     C                   MOVE      '0'           $FC(4)
0737009402243-   C                   ENDSL
073800940224     C*
0739009402242-   C                   ENDIF
074000940223     C*
0741009402241-   C                   ENDIF
074200940224     C*
074300940117     C                   ENDSR
074400940207     C/EJECT
074500940207     C************************************************************
074600940510     C* RIEMPIMENTO VIDEATA  D1
074700940207     C************************************************************
074800940207     C     RIED1         BEGSR
074900940207     C*
075000940207     C*
075100940207     C                   ENDSR
075200940309     C/EJECT
075300940309     C************************************************************
075400940309     C* GESTIONE F03 VIDEO D1
075500940309     C************************************************************
075600940309     C     F03D1         BEGSR
075700940309     C*
075800940309     C                   MOVE      *ON           $FINE
075900940315    >C                   MOVE      '1'           RET002
076000940325     C* fine programma
076100940325     C                   EXSR      ENDPGM
076200940325     C*
076300940325     C                   ENDSR
076400940309     C/EJECT
076500940309     C************************************************************
076600940309     C* GESTIONE F05 VIDEO D1
076700940309     C************************************************************
076800940309     C     F05D1         BEGSR
076900940309     C*
077000940309     C                   MOVE      *ON           $INZD1
077100940309     C*
077200940309     C                   ENDSR
077300940309     C/EJECT
077400940309     C************************************************************
077500940309     C* GESTIONE F08 VIDEO D1
077600940309     C************************************************************
077700940309     C     F08D1         BEGSR
077800940309     C*
077900940309     C                   MOVE      *ON           $FINE
078000940315    >C                   MOVE      '0'           RET002
078100940309     C*
078200940309     C                   ENDSR
078300940309     C/EJECT
078400940309     C************************************************************
078500940309     C* GESTIONE F12 VIDEO D1
078600940309     C************************************************************
078700940309     C     F12D1         BEGSR
078800940309     C*
078900940309     C                   MOVE      *ON           $FINE
079000940315    >C                   MOVE      '2'           RET002
079100940309     C*
079200940309     C                   ENDSR
079300940309     C/EJECT
079400940309     C************************************************************
079500940309     C* GESTIONE F24 VIDEO D1
079600940309     C************************************************************
079700940309     C     F24D1         BEGSR
079800940309     C*
079900940506     C*
080000940506     C                   MOVEA     $FX1          $FX
080100940506     C                   MOVEA     $FC1          $FC
080200940506     C                   MOVEA     $FR1          $FR
080300940506     C                   Z-ADD     $ULKD1        $ULK
080400940506     C                   EXSR      RIEKEY
080500941017     C                   MOVEA     $FC           $FC1
080600940506     C                   Z-ADD     $ULK          $ULKD1            3 0
080700940506     C                   MOVEL     $KEY1         Z1KE1
080800940506     C                   MOVEL     $KEY2         Z1KE2
080900940614     C                   WRITE     Z1
081000940506     C*
081100940309     C                   ENDSR
081200940207     C/EJECT
081300940131     C************************************************************
081400940207     C* CONTROLLO VIDEATA
081500940131     C************************************************************
081600940127     C     CTRD1         BEGSR
081700940131     C*
081800940127     C                   SETOFF                                       99
081900940127     C* non imposto pos. del cursore
082000940127     C                   Z-ADD     0             H1RIGA
082100940127     C                   Z-ADD     0             H1COLO
082200940621     C*
082300940621     C* CTRL DATA
082400940621     C*
082500941109     C                   MOVE      DATA          WRKDSC           10
082600940621     C     D1DSC         IFNE      0
082700970214     C                   CALLB     'XDT4000'
082800941109     C                   PARM                    XDTAMG           10
082900940621     C                   PARM      D1DSC         XDTGMA            6 0
083000940621     C                   PARM      3             XDTSTA            1 0
083100940621     C     XDTSTA        IFNE      0
083200980909     C                   SETON                                        7899
083300940621     C                   ELSE
083400941108     C                   MOVE      XDTAMG        WRKDSC
083500940621     C                   ENDIF
083600940621     C                   ENDIF
083700940223     C*
083800940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
083900940223     C*
0840009403151   >C     OPZ002        IFNE      '04'
084100971110     C*
084200971110     C* controllo codice vuoto
084300980909$017 C     D1COD         IFEQ      ' '
084400971110     C                   SETON                                        5099
084500971110     C                   ENDIF
084600971110     C*
084700940207     C*
084800940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
084900940315    >C     OPZ002        IFEQ      '01'
085000940315    >C     OPZ002        OREQ      '03'
085100940315    >C*-------------------------------------------------------------
085200940315    >C* impostare i campi chiave e verificare l'esistenza del record
085300940315    >C*-------------------------------------------------------------
085400971110$007 C                   MOVE      'Y4Z'         XTBCOD
085500980909     C                   MOVE      *blanks       XTBKEY
085600980909    >C                   MOVEl     D1COD         XTBKEY
085700970512    >C                   MOVE      D1UNITA       XTBUNT
085800940614     C                   MOVE      '0'           XTBALC
085900940614     C                   MOVE      '1'           XTBRIC
086000970214    >C                   CALLB     'XATB'
086100940614     C                   PARM                    XATBDS
086200980910     C     XTBERR        IFEQ      '0'
086300940223     C                   SETON                                        5999
086400940223     C                   ENDIF
086500940620     C* Controllo se compreso nei range indice
086600940620     C     XTBERR        IFEQ      '3'
086700940620    >C                   SETON                                        5899
086800940620     C                   END
086900940620     C*
087000940620     C* REPERISCO AUTORIZZAZIONI UTENTE
087100940620     C                   CLEAR                   XCHATB
087200941118     C                   MOVE      XSCSOC        XCTSOC
087300941118     C                   MOVE      XSCDUN        XCTUNI
087400940620     C                   MOVEL     KNMUS         XCTPRF
087500940620     C                   MOVE      XTBCOD        XCTCOD
087600940620     C                   CALL      'XCHKATB'
087700940620     C                   PARM                    XCHATB
087800941111     C                   MOVEL     XCTDDS        AUTDS
087900941027     C     XCTRTN        IFEQ      1
088000941027     C                   MOVEL     'N'           MANGRU
088100941027     C                   ENDIF
088200940620     C* Non ammessa immissione di gruppo
088300940620     C     XTBGRU        IFEQ      'G'
088400940620     C     MANGRU        ANDEQ     'N'
088500940620     C                   SETON                                        5799
088600940620     C                   END
088700940620     C* Non ammessa immissione di azienda
088800940620     C     XTBGRU        IFEQ      'A'
088900940620     C     MANAZI        ANDEQ     'N'
089000940620     C                   SETON                                        5699
089100940620     C                   END
089200940712     C* codice riservato non accessibile
089300941006     C     XTBRIS        IFGT      ABIRIS
089400941005    >C     OPZ002        ANDNE     '01'
089500941005    >C     OPZ002        ANDNE     '03'
089600940712     C                   SETON                                        5499
089700940712     C                   END
089800940224     C                   ENDIF
089900940224     C*
090000980909     C*  Controllo codice descrizione tabella
090100980909     C                   if        §4ZDES=*blanks
090200980909     C                   SETON                                        5199
090300980909     C                   END
090400020415     C*  Controllo se sono traini o autotrasportatore
090500020415     C                   if        §4Zta<>'A' and §4zta <> 'T'
090600020415     C                   SETON                                        8399
090700020415     C                   END
090800980909     C*---------------------------------------------------------------*
090900980909     C*  Controllo codice capoconto conto di costo
091000980909     C*  E' impostato ?
091100980909     C                   if        §4ZKCC=*blanks
091200980909     C                   SETON                                        5299
091300980909     C                   Else
091400980909      *
091500980909     C                   CALLb     'XALLINEA'
091600980909     C                   PARM                    §4ZKCC
091700980909     C                   PARM      6             LEN               5 0
091800980909     C                   PARM                    XSCINP
091900980909     C*  Esiste ?
092000980909     C                   move      xscsoc        ksocieta
092100980909     C                   move      §4Zkcc        kkcc
092200020415     C     krco1         chain     anrco99j                           21
092300980909     C                   if        *in21
092400980909     C                   SETON                                        5399
092500980909     C                   else
092600980909     C* non è un capoconto di costo
092700980909     C                   if        rconatura<>'3'
092800980909     C                   SETON                                        5599
092900980909     C                   END
0930009809091-   C                   END
0931009809091-   C                   END
093200980909     C*---------------------------------------------------------------*
093300980909     C*  Controllo codice sottoconto conto di costo
093400980909     C*  E' impostato ?
093500980909     C                   if        §4ZKSC=*blanks
093600980909     C                   SETON                                        6199
093700980909     C                   Else
093800980909     C                   CALLb     'XALLINEA'
093900980909     C                   PARM                    §4ZKSC
094000980909     C                   PARM      8             LEN               5 0
094100980909     C                   PARM                    XSCINP
094200980909     C*  Esiste ?
094300980909     C                   move      xscsoc        ksocieta
094400980909     C                   move      §4Zkcc        kkcc
094500980909     C                   move      §4Zksc        kksc
094600980909     C     krco          chain     anrco99j                           21
094700980909     C                   if        *in21
094800980909     C                   SETON                                        6099
094900980909     C                   else
095000980909     C* non è un conto di costo
095100980909     C                   movel     pdcdesbrev    D1dksc
095200980909     C                   if        rconatura<>'3'
095300980909     C                   SETON                                        5599
095400980909     C                   END
0955009809091-   C                   END
0956009809091-   C                   END
095700980909     C*---------------------------------------------------------------*
095800980909     C*  Controllo la voce del conto di costo
095900980909     C*  E' impostata ?
096000140129     C                   CLEAR                   d1dvoc
096100980909     C                   if        §4ZVOC=*blanks
096200980909     C                   SETON                                        6299
096300980909     C                   Else
096400980909     C*  Esiste ?
096500980909     C                   move      xscsoc        ksocieta
096600980909     C                   move      §4Zvoc        kvoce
096700980909     C     kvci01        chain     anvci01l                           21
096800980909     C                   if        *in21
096900980909     C                   SETON                                        6499
097000140129     C                   else
097100140129     C                   movel     vcidesbrev    d1dvoc
097200140129     C*    kvci02        chain     anvci02l                           21
097300140129     C*                  if        *in21
097400140129     C*                  SETON                                        6399
097500140129     C*                  END
0976009809091-   C                   END
0977009809091-   C                   END
097800980909     C*---------------------------------------------------------------*
097900980909     C*  Controllo il centro di costo Bartolini
098000980909     C*  E' impostato ?
098100020415     C                   if        §4Zta = 'T' and
098200020415     C                             §4ZCDB=*blanks
098300020412     C                   SETON                                        6599
098400020415     c                   end
098500020415     C                   clear                   d1dcdb
098600020415     C                   if        §4zcdb <> *blanks
098700980909     C*  Esiste ?
098800980909     C                   move      'CDC'         ktprifan
098900980909     C                   move      §4Zcdb        kentita
099000980909     C     kent          chain     anent01l                           21
099100980909     C                   if        *in21
099200980909     C                   SETON                                        6699
099300980909     C                   else
099400980909     C                   move      entdesbrev    d1dcdb
099500980909     C                   end
099600980909     C                   end
099700980909     C*---------------------------------------------------------------*
099800980909     C*  Controllo il centro di costo SDI
099900980909     C*  E' impostato ?
100000020415     C                   if        §4Zta = 'T' and
100100020415     C                             §4ZCDB=*blanks
100200980909     C                   SETON                                        6799
100300020415     C                   end
100400980909     C*  Esiste ?
100500020415     C                   clear                   d1dcds
100600020415     C                   if        §4ZCDB<>*blanks
100700980909     C                   move      'CDC'         ktprifan
100800980909     C                   move      §4Zcds        kentita
100900980909     C     kent          chain     anent01l                           21
101000980909     C                   if        *in21
101100980909     C                   SETON                                        6899
101200980909     C                   else
101300980909     C                   move      entdesbrev    d1dcds
101400980909     C                   end
101500980909     C                   end
101600980909     C*---------------------------------------------------------------*
101700980909     C*  Controllo codice capoconto conto IVA
101800980909     C*  E' impostato ?
101900980909     C                   if        §4ZKCI=*blanks
102000980909     C                   SETON                                        6999
102100980909     C                   Else
102200980909     C                   CALLb     'XALLINEA'
102300980909     C                   PARM                    §4ZKCI
102400980909     C                   PARM      6             LEN               5 0
102500980909     C                   PARM                    XSCINP
102600980909     C*  Esiste ?
102700980909     C                   move      xscsoc        ksocieta
102800980909     C                   move      §4Zkci        kkcc
102900020415     C     krco1         chain     anrco99j                           21
103000980909     C                   if        *in21
103100980909     C                   SETON                                        7199
103200980909     C                   else
103300980909     C* non è un capoconto IVA
103400980909     C                   if        rcosnatura<>'3'
103500980909     C                   SETON                                        7099
103600980909     C                   END
1037009809091-   C                   END
1038009809091-   C                   END
103900980909     C*---------------------------------------------------------------*
104000980909     C*  E' impostato ?
104100980909     C                   if        §4ZKSI=*blanks
104200980909     C                   SETON                                        7299
104300980909     C                   Else
104400980909     C                   CALLb     'XALLINEA'
104500980909     C                   PARM                    §4ZKSI
104600980909     C                   PARM      8             LEN               5 0
104700980909     C                   PARM                    XSCINP
104800980909     C*  Esiste ?
104900980909     C                   move      xscsoc        ksocieta
105000980909     C                   move      §4ZkcI        kkcc
105100980909     C                   move      §4ZksI        kksc
105200980909     C     krco          chain     anrco99j                           21
105300980909     C                   if        *in21
105400980909     C                   SETON                                        7399
105500980909     C                   else
105600980909     C* non è un conto di costo
105700020503     C                   movel     pdcdesbrev    D1dksi
105800980909     C                   if        rcosnatura<>'3'
105900980909     C                   SETON                                        7199
106000980909     C                   END
1061009809091-   C                   END
1062009809091-   C                   END
106300980909     C*---------------------------------------------------------------*
106400980909     C*  Controllo codice esenzione IVA
106500980909     C                   if        §4ZIVC>*blanks
106600980909     C*  Esiste ?
106700980910     C                   CLEAR                   X06DS
106800980910     C                   MOVE      XSCSOC        X06SOC
106900980910     C                   MOVE      XSCLIN        X06LIN
107000980910     C                   MOVE      §4ZIVC        X06CIV
107100980910     C                   MOVE      WDATE         X06DAT
107200980910     C                   CALL      'X06G48'
107300980910     C                   PARM                    X06DS
107400980910   B3C     X06ERR        IFEQ      '0'
107500980910     C                   MOVEL     X06UNI        TABG48
107600980910   X3C                   ELSE
107700980910     C                   SETON                                        7499
107800980910   E3C                   ENDIF
107900980909     C                   ENDIF
108000980909     C*---------------------------------------------------------------*
108100980909     C* Devo impostare o il codice di esenzione o l'aliquota IVA
108200980909$017 C     §4ziva        IFgt      *zeros
108300980909     c     §4zivc        andne     *blanks
108400980909$017 C     §4ziva        oreq      *zeros
108500980909     c     §4zivc        andeq     *blanks
108600980909     C                   SETON                                        7599
108700980909     C                   ENDIF
108800980909     C*---------------------------------------------------------------*
108900980909     C* Se impostata aliquota IVA controllo che esista su file associazioni
109000980909     C* aliquote libri-IVA
109100980909     c                   if        §4ziva          <> *zeros
109200980909     C                   move      §4Ziva        kaliq
109300980909     c     k02riv02      chain     anriv02l                           11
109400980909     C                   if        *in11
109500980909     c   11              seton                                        7799
109600980909     c                   endif
109700980909     c                   endif
109800980909     C*
109900980909     C                   endif
110000980911     C*---------------------------------------------------------------*
110100980911     C*  E' impostato ?
110200980911     C                   if        §4ZKCR=*blanks
110300980911     C                   SETON                                        9099
110400980911     C                   Else
110500980911     C                   CALLb     'XALLINEA'
110600980911     C                   PARM                    §4ZKCr
110700980911     C                   PARM      6             LEN               5 0
110800980911     C                   PARM                    XSCINP
110900980911     C*  Esiste ?
111000980911     C                   move      xscsoc        ksocieta
111100980911     C                   move      §4Zkcr        kkcc
111200020415     C     krco1         chain     anrco99j                           21
111300980911     C                   if        *in21
111400980911     C                   SETON                                        9199
1115009809111-   C                   END
1116009809111-   C                   END
111700980911     C*---------------------------------------------------------------*
111800980911     C*  E' impostato ?
111900980911     C                   if        §4ZKSR=*blanks
112000980911     C                   SETON                                        9299
112100980911     C                   Else
112200980911     C                   CALLb     'XALLINEA'
112300980911     C                   PARM                    §4ZKSR
112400980911     C                   PARM      8             LEN               5 0
112500980911     C                   PARM                    XSCINP
112600980911     C*  Esiste ?
112700980911     C                   move      xscsoc        ksocieta
112800980911     C                   move      §4Zkcr        kkcc
112900980911     C                   move      §4Zksr        kksc
113000980911     C     krco          chain     anrco99j                           21
113100980911     C                   if        *in21
113200980911     C                   SETON                                        9399
113300980911     C                   else
113400980911     C* non è un conto di costo
113500980911     C                   movel     pdcdesbrev    D1dksr
1136009809111-   C                   END
1137009809111-   C                   END
113800020412     C*---------------------------------------------------------------*
113900020527     c* CAUSALE REG. ANNO IN CORSO NO AUTOFATTURAZIONE
114000020412     C*  E' impostato ?
114100020527     c                   clear                   d1dcrc
114200020527     C                   if        §4ZCRC=*blanks
114300020527     C                   SETON                                        8099
114400020412     C                   Else
114500020412     C*  Esiste ?
114600020415     C                   EVAL      OpeCausale = §4zcrc
114700020412     C     Kope          CHAIN     AnOpe01L
114800020412     C                   IF        %FOUND(AnOpe01L)
114900020412     C                   EVAL      d1dcrc = OpeDesBrev
115000020412     C                   ELSE
115100020412     C                   seton                                        9980
1152000204121-   C                   END
115300020527     C                   End
115400150116     C*---------------------------------------------------------------*
115500150116     c* CAUSALE REG. ANNO IN CORSO NO AUTOFATTURAZIONE iva esente
115600150116     c                   clear                   d1dcrce
115700150116     c                   setoff                                       40
115800150116     C                   if        §4ZCRCe<>*blanks
115900150116     C                   EVAL      OpeCausale = §4zcrce
116000150116     C     Kope          CHAIN     AnOpe01L
116100150116     C                   IF        %FOUND(AnOpe01L)
116200150116     C                   EVAL      d1dcrce= OpeDesBrev
116300150116     C                   ELSE
116400150116     C                   seton                                        9940
1165001501161-   C                   END
116600150116     C                   End
116700020412     C*---------------------------------------------------------------*
116800020527     c* CAUSALE REG. ANNO PRECEDENTE NO AUTOFATTURAZIONE
116900020527     c                   clear                   d1dcrr
117000020412     C*  E' impostato ?
117100020527     C                   if        §4ZCRR=*blanks
117200020527     C                   SETON                                        8199
117300020412     C                   Else
117400020412     C*  Esiste ?
117500020415     C                   EVAL      OpeCausale = §4zcrr
117600020412     C     Kope          CHAIN     AnOpe01L
117700020412     C                   IF        %FOUND(AnOpe01L)
117800020412     C                   EVAL      d1dcrr = OpeDesBrev
117900020412     C                   ELSE
118000020412     C                   seton                                        9981
1181000204121-   C                   END
118200020527     C                   End
118300020412     C*---------------------------------------------------------------*
118400020527     c* CAUSALE REG. FATTURE DA RICEVERE NO AUTOFATTURAZIONE
118500020527     c                   clear                   d1dcrp
118600020412     C*  E' impostato ?
118700020527     C                   if        §4ZCRP=*blanks
118800020527     C                   SETON                                        8299
118900020412     C                   Else
119000020412     C*  Esiste ?
119100020415     C                   EVAL      OpeCausale = §4zcrp
119200020412     C     Kope          CHAIN     AnOpe01L
119300020412     C                   IF        %FOUND(AnOpe01L)
119400020412     C                   EVAL      d1dcrp = OpeDesBrev
119500020412     C                   ELSE
119600020412     C                   seton                                        9982
1197000204121-   C                   END
119800020527     C                   End
119900020527     C*---------------------------------------------------------------*
120000060724     C                   if        §4zta = 'A' or §4zta = 'T'
120100020527     c* CAUSALE REG. ANNO IN CORSO AUTOFATTURAZIONE
120200020527     C*  E' impostato ?
120300020527     c                   clear                   d1dcrca
120400020527     C                   if        §4zcrca = *blanks
120500020527     C                   SETON                                        8499
120600020527     C                   Else
120700020527     C*  Esiste ?
120800020527     C                   EVAL      OpeCausale = §4zcrca
120900020527     C     Kope          CHAIN     AnOpe01L
121000020527     C                   IF        %FOUND(AnOpe01L)
121100020527     C                   EVAL      d1dcrca = OpeDesBrev
121200060724     c                   if        §4zta = 'T'
121300060724     c                   eval      opesers = opeser
121400060724     c                   exsr      sropeser
121500060724     c                   end
121600020527     C                   ELSE
121700020527     C                   seton                                        9984
1218000205271-   C                   END
1219001501161-   C                   END
122000150116     c* CAUSALE REG. ANNO IN CORSO AUTOFATTURAZIONE per iva esente
122100150116     C*  E' impostato ?
122200150116     c                   clear                   d1dcrcae
122300150116     c                   setoff                                       44
122400150116     C                   if        §4zcrcae<>*blanks
122500150116     C*  Esiste ?
122600150116     C                   EVAL      OpeCausale = §4zcrcae
122700150116     C     Kope          CHAIN     AnOpe01L
122800150116     C                   IF        %FOUND(AnOpe01L)
122900150116     C                   EVAL      d1dcrcae= OpeDesBrev
123000150116     C                   ELSE
123100150116     C                   seton                                        9944
1232001501161-   C                   END
1233001501161-   C                   END
123400020527     C*---------------------------------------------------------------*
123500020527     c* CAUSALE REG. ANNO PRECEDENTE AUTOFATTURAZIONE
123600020527     c                   clear                   d1dcrra
123700020527     C*  E' impostato ?
123800020527     C                   if        §4zcrra=*blanks
123900020527     C                   SETON                                        8599
124000020527     C                   Else
124100020527     C*  Esiste ?
124200020527     C                   EVAL      OpeCausale = §4zcrra
124300020527     C     Kope          CHAIN     AnOpe01L
124400020527     C                   IF        %FOUND(AnOpe01L)
124500020527     C                   EVAL      d1dcrra = OpeDesBrev
124600020527     C                   ELSE
124700020527     C                   seton                                        9985
1248000205271-   C                   END
1249000205271-   C                   END
125000020527     C*---------------------------------------------------------------*
125100020527     c* CAUSALE REG. FATTURE DA RICEVERE AUTOFATTURAZIONE
125200020527     c                   clear                   d1dcrpa
125300020527     C*  E' impostato ?
125400020527     C                   if        §4zcrpa=*blanks
125500020527     C                   SETON                                        8699
125600020527     C                   Else
125700020527     C*  Esiste ?
125800020527     C                   EVAL      OpeCausale = §4zcrpa
125900020527     C     Kope          CHAIN     AnOpe01L
126000020527     C                   IF        %FOUND(AnOpe01L)
126100020527     C                   EVAL      d1dcrpa = OpeDesBrev
126200020527     C                   ELSE
126300020527     C                   seton                                        9986
1264000205271-   C                   END
1265000205271-   C                   END
1266000205271-   C                   END
126700020527     c* controllo la congruenza delle scelte
126800020527     c                   if        (§4zcrc <> *blanks and (§4zcrr=*blanks OR
126900020527     c                             §4ZCRP = *BLANKS)) or
127000020527     c                             (§4zcrr <> *blanks and (§4zcrc=*blanks OR
127100020527     c                             §4ZCRP = *BLANKS)) or
127200020527     c                             (§4zcrp <> *blanks and (§4zcrc=*blanks OR
127300020527     c                             §4ZCRr = *BLANKS))
127400020527     C                   seton                                        998081
127500020527     C                   seton                                        82
127600020527     c                   end
127700020527     c                   if        (§4zcrca <> *blanks and (§4zcrra=*blanks OR
127800020527     c                             §4ZCRPa= *BLANKS)) or
127900020527     c                             (§4zcrra<> *blanks and (§4zcrca=*blanks OR
128000020527     c                             §4ZCRPa= *BLANKS)) or
128100020527     c                             (§4zcrpa<> *blanks and (§4zcrca=*blanks OR
128200020527     c                             §4ZCRra= *BLANKS))
128300020527     C                   seton                                        998485
128400020527     C                   seton                                        86
128500020527     c                   end
128600980909     C*---------------------------------------------------------------*
128700940302     C     *IN99         IFEQ      *ON
128800940510     C     $GEST         ANDEQ     'D1'
128900960228     C     OPZ002        ANDNE     '07'
129000940117     C                   MOVEA     *IN           WIN
129100940128     C                   MOVE      *ALL'0'       IN5098           49
129200940128     C                   MOVEA     IN5098        *IN(50)
129300940614     C                   WRITE     D1
129400940117     C                   MOVEA     WIN           *IN
129500940302     C                   ENDIF
129600940127     C*
129700940117     C                   ENDSR
129800960305     C/EJECT
129900960305     C************************************************************
130000060724     C* CONTROLLO che il registro iva delle causali trazionisti sia lo stesso
130100960305     C************************************************************
130200060724     C     sropeser      BEGSR
130300060724     C* codice di default (A) da tabella TW mando un messaggio di avvertimen
130400060724     C* to nel caso ci siano altri codici con opeser ><
130500060724     c                   if        d1cod = tbluni and okmsg = '0'
130600060724     c     'Y4Z'         setll     anatb02l
130700060724     c                   do        *hival
130800060724     c     'Y4Z'         reade     anatb02l
130900060724     c                   if        %eof(anatb02l)
131000060724     c                   leave
131100060724     c                   end
131200060724     c                   movel(p)  atbuni        angy4zds
131300060724     c                   if        £§4ZTA ='T' and atbkey <> d1cod
131400060725     C                   EVAL      OpeCausale =£§4zcrca
131500060725     C     Kope          CHAIN     AnOpe01L
131600060725     C                   IF        %FOUND(AnOpe01L) and
131700060725     C                             opeSer <> opesers
131800060726     c                   eval      okmsg = '1'
131900060726     c                   seton                                        9987
132000060724     c                   leave
132100060724     c                   end
132200060724     c                   end
132300060724     c                   enddo
132400060724     c                   end
132500060724     c* se codice <> da quello di default se opeser <> errore bloccante
132600060724     c                   if        d1cod <>tbluni
132700060724     c                   eval      atbcod = 'Y4Z'
132800060724     c                   eval      atbkey = 'A'
132900060724     c     katb          chain     anatb02l
133000060724     c                   if        %found(anatb02l)
133100060724     c                   movel(p)  atbuni        angy4zds
133200060725     C                   EVAL      OpeCausale =£§4zcrca
133300060725     C     Kope          CHAIN     AnOpe01L
133400060725     C                   IF        %FOUND(AnOpe01L) and
133500060725     C                             opeSer <> opesers
133600060724     c                   seton                                        9984
133700060724     c                   end
133800060724     c                   end
133900060724     c                   end
134000060724     C*
134100960305     C                   ENDSR
134200060724     C/EJECT
134300060724     C************************************************************
134400060724     C* CONTROLLO VIDEATA D2
134500060724     C************************************************************
134600060724     C     CTRD2         BEGSR
134700060724     C*
134800060724     C                   ENDSR
134900940207     C/EJECT
135000940224     C************************************************************
135100940315    >C* AGGIORNAMENTO ANAGRAFICA
135200940224     C************************************************************
135300940315    >C     AGGANA        BEGSR
135400971110     C*
135500980909$007 C                   MOVE      'Y4Z'         XTBCOD
135600980909     C                   MOVE      *blanks       XTBKEY
135700980909     C                   MOVEl     D1COD         XTBKEY
135800970512     C                   MOVE      D1UNITA       XTBUNT
135900940207     C*
136000940207     C                   SELECT
136100940315    >C     OPZ002        WHENEQ    '01'
136200940315    >C     OPZ002        OREQ      '03'
136300940315    >C                   EXSR      RIEANA
136400950609     C                   MOVE      XSCCUT        XTBUTE
136500940614     C                   MOVE      '5'           XTBRIC
136600970214     C                   CALLB     'XATB'
136700940614    >C                   PARM                    XATBDS
136800940318     C* se rcd non scrivibile attivo errore
136900940615     C     XTBERR        IFEQ      '1'
137000940318    >C                   SETON                                        5999
137100940318     C                   END
137200940315    >C     OPZ002        WHENEQ    '02'
137300940621     C                   MOVE      '1'           XTBRIC
137400940621     C                   MOVE      '1'           XTBALC
137500970214     C                   CALLB     'XATB'
137600940621     C                   PARM                    XATBDS
137700940622    >C                   EXSR      RIEANA
137800950609     C                   MOVE      XSCCUT        XTBUTE
137900940614     C                   MOVE      '4'           XTBRIC
138000970214     C                   CALLB     'XATB'
138100940614    >C                   PARM                    XATBDS
138200940315    >C     OPZ002        WHENEQ    '04'
138300940621     C                   MOVE      '1'           XTBRIC
138400940621     C                   MOVE      '1'           XTBALC
138500970214     C                   CALLB     'XATB'
138600940621     C                   PARM                    XATBDS
138700940614     C                   MOVE      '6'           XTBRIC
138800970214     C                   CALLB     'XATB'
138900940614    >C                   PARM                    XATBDS
139000940621     C* annullo traduzioni
139100940621     C     XTBERR        IFEQ      '0'
139200940621     C                   EXSR      ANNTBT
139300940621     C                   END
139400940621     C*
139500940207     C                   ENDSL
139600940203     C*
139700940203     C                   ENDSR
139800940224     C/EJECT
139900940224     C************************************************************
140000940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
140100940224     C************************************************************
140200940616     C     RIEANA        BEGSR
140300940616     C                   CLEAR                   XTBUNI
140400940621     C                   MOVE      WRKDSC        XTBDSC
140500940712     C                   MOVE      D1RIS         XTBRIS
140600940616     C                   MOVEL     EXTDS         XTBUNI
140700940207     C                   ENDSR
140800940224     C/EJECT
140900940224     C************************************************************
141000940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
141100940224     C************************************************************
141200940224     C     GESAGG        BEGSR
141300940224     C*
141400940224     C* segnalo al pgm chiamante l'avvenuta conferma
141500940315    >C                   MOVE      *ON           OPR002
141600940224     C* segnalo al pgm chiamante l'aver premuto l'F6
141700940315    >C                   MOVE      '0'           RET002
141800940224     C*
1419009402241    C                   SELECT
142000940224     C* nel caso di immissione, ripristino la videata iniziale
142100940315    >C     OPZ002        WHENEQ    '01'
142200940224     C                   MOVE      *ON           $INZD1
142300940224     C* altrimenti ritorno al pgm chiamante
142400940224     C                   OTHER
142500940224     C                   MOVE      *ON           $FINE
1426009402241-   C                   ENDSL
142700940224     C*
142800940224     C                   ENDSR
142900940621     C/EJECT
143000940621     C************************************************************
143100940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
143200940621     C************************************************************
143300940621    >C     ANNTBT        BEGSR
143400940621     C                   MOVE      XTBAZI        TBTAZI
143500940621     C                   MOVE      XTBCOD        TBTCOD
143600980909     C                   MOVE      XTBKEY        TBTKEY
143700940621     C     KTBT          SETLL     ANTBT01L
143800940621     C                   DO        *HIVAL
143900940621     C     KTBT          DELETE    ANTBT01L                           21
144000940621     C  N21              END
144100940621     C*
144200940621     C                   ENDSR
144300940207     C/EJECT
144400940131     C************************************************************
144500940131     C* OPERAZIONI INIZIALI
144600940131     C************************************************************
144700940131     C     *INZSR        BEGSR
144800940131     C*
144900940127     C* Reperimento parametri
145000940127     C*
145100940117     C     *ENTRY        PLIST
145200940117     C                   PARM                    KPJBA
145300980910     C*
145400980910     C* Imposto data
145500980910     C                   CALLb     'XDT4000'
145600980910     C                   PARM                    XDTAMG           10
145700980910     C                   PARM      UDATE         XDTGMA            6 0
145800980910     C                   PARM      4             XDTSTA            1 0
145900980910     C                   MOVEL     XDTAMG        WDATE
146000060724     c* aggancio tabella TW per reperire causale tabella Y4Z di default
146100060724     c                   z-add     1             tblkut
146200060724     c                   eval      tblcod = 'TW'
146300060724     c                   movel(p)  '1'           tblkey
146400060724     c                   clear                   tbluni
146500060724     c     ktab          chain     tabel00f
146600940506     C*
146700940506     C* Reperimento tasti di funzione
146800940506     C*
146900940506     C                   EXSR      REPKEY
147000940621     C*
147100940621     C* KLIST
147200060724     C     Katb          KLIST
147300060724     C                   KFLD                    atbcod
147400060724     C                   KFLD                    atbkey
147500060724     C     Ktab          KLIST
147600060724     C                   KFLD                    tblkut
147700060724     C                   KFLD                    tblcod
147800060724     C                   KFLD                    tblkey
147900940621     C     KTBT          KLIST
148000940621     C                   KFLD                    TBTAZI
148100940621     C                   KFLD                    TBTCOD
148200940621     C                   KFLD                    TBTKEY
148300980909     C     KRCO          KLIST
148400980909     C                   KFLD                    ksocieta
148500980909     C                   KFLD                    kkcc
148600980909     C                   KFLD                    kksc
148700020415     C     KRCO1         KLIST
148800020415     C                   KFLD                    ksocieta
148900020415     C                   KFLD                    kkcc
149000980909     C     KVCI01        KLIST
149100980909     C                   KFLD                    ksocieta
149200980909     C                   KFLD                    kVoce
149300980909     C     KVCI02        KLIST
149400980909     C                   KFLD                    ksocieta
149500980909     C                   KFLD                    kkcc
149600980909     C                   KFLD                    kksc
149700980909     C                   KFLD                    kVoce
149800980909     C     Kent          KLIST
149900980909     C                   KFLD                    ksocieta
150000980909     C                   KFLD                    ktprifan
150100980909     C                   KFLD                    kentita
150200980909     C     K02Riv02      KLIST
150300980909     C                   KFLD                    ksocieta
150400980909     C                   KFLD                    kaliq
150500020412     C     Kope          KLIST
150600020412     C                   KFLD                    OpeSocieta
150700020412     C                   KFLD                    OpeCausale
150800940506     C*
150900980909     C     *LIKE         Define    rcosocieta    ksocieta
151000980909     C     *LIKE         Define    rcokcc        kkcc
151100980909     C     *LIKE         DEfine    rcoksc        kksc
151200980909     C     *LIKE         Define    vcivoce       kVoce
151300980909     C     *LIKe         DEfine    enttprifan    ktprifan
151400980909     C     *LIKE         DEfine    ententita     kentita
151500980909     C     *LIKE         DEfine    rivaliq       kaliq
151600980910     C     *LIKE         DEFINE    X06DAT        WDATE
151700980909     C*
151800940223     C                   ENDSR
151900941103     C/EJECT
152000941103      ************************************************************
152100941103      * Reperimento dati società
152200941103      ************************************************************
152300941103     C     REPSOC        BEGSR
152400941103      *
152500970214     C                   CALLB     'XSOC'
152600941103     C                   PARM                    TIPXSC            6
152700941103     C                   PARM                    SOCXSC            3
152800941103     C                   PARM                    CDSXSC            9 0
152900941103     C                   PARM                    MODXSC            3
153000941103     C                   PARM      *BLANKS       RTNXSC            1
153100941103     C                   PARM                    XSOCDS
153200941103     C                   PARM                    KPJBA
153300941103      *
153400941103     C                   ENDSR
153500940223     C/EJECT
153600940506     C************************************************************
153700940506     C* REPERIMENTO TASTI DI FUNZIONE
153800940506     C************************************************************
153900940506     C     REPKEY        BEGSR
154000940506     C                   MOVEA     $FM1          $FM
154100940506     C                   EXSR      REPKKK
154200940506     C                   MOVEA     $FX           $FX1
154300940614     C                   ENDSR
154400940506     C/EJECT
154500940506     C************************************************************
154600940506     C* REPERIMENTO TASTI DI FUNZIONE
154700940506     C************************************************************
154800940506     C     REPKKK        BEGSR
154900940506     C*
155000940506     C                   CLEAR                   $FX
155100940506     C     1             DO        24            X
155200940506     C*
155300940506     C     $FM(X)        IFNE      *BLANK
155400940506     C*
155500940506     C                   MOVEL     $FM(X)        IDMSG
155600970214     C                   CALLB     'XRTVM'
155700940506     C                   PARM                    IDMSG            27
155800940506     C                   PARM                    TXTMSG          100
155900940506     C                   PARM                    ERRMSG            1
156000940506     C*
156100940506     C     ERRMSG        IFNE      *ON
156200940506     C                   MOVEL     TXTMSG        $FX(X)
156300940506     C                   ELSE
156400940506     C                   MOVEL     *ALL' '       $FX(X)
156500940506     C                   END
156600940506     C*
156700940506     C                   END
156800940506     C*
156900940506     C                   END
157000940506     C*
157100940506     C                   ENDSR
157200940509     C/EJECT
157300940223     C************************************************************
157400940223     C* INIZIALIZZAZIONE VARIABILI
157500940223     C************************************************************
157600940223     C     INZVAR        BEGSR
157700951124     C*
157800951124     C* Chiamata XSOC
157900951124     C*
158000951124     C                   MOVEL     'SOC001'      TIPXSC
158100951124     C                   EXSR      REPSOC
158200951124     C     RTNXSC        IFNE      '1'
158300951124     C                   MOVEL     XSOCDS        SOC001
158400951124     C                   ELSE
158500951124     C                   MOVE      *ON           $FINE
158600951124     C* fine programma
158700951124     C                   EXSR      ENDPGM
158800951124     C                   ENDIF
158900940224     C*
159000940224     C* Dati da PGM guida
159100940224     C*
159200940315    >C                   MOVEL     KPJBU         D002DS
159300940128     C*
159400940223     C* Variabili per gestione videate
159500940128     C*
159600940223     C                   MOVE      'D1'          $GEST
159700940223     C                   MOVE      *OFF          $FINE
159800940223     C                   MOVE      *ON           $INZD1
159900940224     C                   MOVE      *BLANK        $LASTG
160000940224     C                   MOVE      *BLANK        $LASTV
160100940506     C                   Z-ADD     0             $ULKD1            3 0
160200020412     C                   EVAL      OpeSocieta = XscSoc
160300940127     C*
160400940223     C* Variabili appoggio
160500940127     C*
160600940224     C                   Z-ADD     0             CURR
160700940224     C                   Z-ADD     0             CURC
160800940207     C*
160900940207     C* Valorizzazione campi univoci testate
161000940207     C*
161100940614     C                   CLEAR                   T1
161200940614     C                   MOVEL     KNSIF         NOMSIF
161300941109     C                   MOVEL     XSCDSI        NOMDIT
161400941027     C                   MOVEL     DSPGM         NOMPGM
161500940224     C                   Z-ADD     1             X
161600940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
161700940224     C     *IN21         IFEQ      *ON
161800940224     C                   MOVEL     $ID(X)        IDMSG
161900970214     C                   CALLB     'XRTVM'
162000940224     C                   PARM                    IDMSG            27
162100940224     C                   PARM                    TXTMSG          100
162200940224     C                   PARM                    ERRMSG            1
162300940224     C                   PARM      *ON           CTRMSG            1
162400940224     C                   PARM      30            LENMSG            3 0
162500940224     C     ERRMSG        IFNE      *ON
162600940224     C                   MOVEL     TXTMSG        T1OPE
162700940224     C                   ELSE
162800940224     C                   MOVEL     *ALL'?'       T1OPE
162900940224     C                   ENDIF
163000940224     C                   END
163100940224     C*
163200940224     C* Inizializzazione/Decodifica prima videata
163300940224     C*
163400940224     C                   EXSR      INZD1
163500940224     C                   MOVE      *OFF          $INZD1
163600960228     C* Se opzione = '07' richiesti solo controlli
163700960228     C     OPZ002        IFEQ      '07'
163800960228     C                   MOVEL     *IN99         ERR002
163900960228     C                   MOVEL     '0'           RET002
164000960228     C                   MOVEL     *ON           $FINE
164100960228     C                   ENDIF
164200940127     C*
164300940117     C                   ENDSR
164400940128     C************************************************************
164500940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
164600940223     C************************************************************
164700940223     C     DEFCAM        BEGSR
164800940223     C*
164900940223     C* klist
165000940207     C*
165100940223     C*
165200940223     C* Variabili per gestione videate
165300940223     C*
165400940223     C                   MOVE      *BLANK        $GEST             2
165500940223     C                   MOVE      *BLANK        $FINE             1
165600940223     C                   MOVE      *BLANK        $INZD1            1
165700940223     C                   MOVE      *BLANK        $LASTG            2
165800940223     C                   MOVE      *BLANK        $LASTV            2
165900940223     C*
166000940223     C* Variabili appoggio
166100940223     C*
166200940223     C                   Z-ADD     0             CURR              2 0
166300940223     C                   Z-ADD     0             CURC              2 0
166400940223     C                   MOVE      *ZEROS        WIN              99
166500940224     C                   MOVE      *BLANK        WTBERR            1
166600940223     C*
166700940223     C* Indici
166800940223     C*
166900940223     C                   Z-ADD     0             X                 3 0
167000940223     C                   Z-ADD     0             Y                 3 0
167100940127     C*
167200940117     C                   ENDSR
167300940128     C************************************************************
167400940509     C/EJECT
167500940509     C************************************************************
167600940509     C* CONTROLLO ABILITAZIONE TASTI
167700940509     C************************************************************
167800940509     C     CTRKEY        BEGSR
167900940509     C                   SELECT
168000940509     C     $TASTO        WHENEQ    F01
168100940720     C     $FC(1)        IFEQ      '0'
168200940509     C                   SETON                                        99
168300940509     C                   ENDIF
168400940509     C     $TASTO        WHENEQ    F02
168500940720     C     $FC(2)        IFEQ      '0'
168600940509     C                   SETON                                        99
168700940509     C                   ENDIF
168800940509     C     $TASTO        WHENEQ    F03
168900940720     C     $FC(3)        IFEQ      '0'
169000940509     C                   SETON                                        99
169100940509     C                   ENDIF
169200940509     C     $TASTO        WHENEQ    F04
169300940720     C     $FC(4)        IFEQ      '0'
169400940509     C                   SETON                                        99
169500940509     C                   ENDIF
169600940509     C     $TASTO        WHENEQ    F05
169700940720     C     $FC(5)        IFEQ      '0'
169800940509     C                   SETON                                        99
169900940509     C                   ENDIF
170000940509     C     $TASTO        WHENEQ    F06
170100940720     C     $FC(6)        IFEQ      '0'
170200940509     C                   SETON                                        99
170300940509     C                   ENDIF
170400940509     C     $TASTO        WHENEQ    F07
170500940720     C     $FC(7)        IFEQ      '0'
170600940509     C                   SETON                                        99
170700940509     C                   ENDIF
170800940509     C     $TASTO        WHENEQ    F08
170900940720     C     $FC(8)        IFEQ      '0'
171000940509     C                   SETON                                        99
171100940509     C                   ENDIF
171200940509     C     $TASTO        WHENEQ    F09
171300940720     C     $FC(09)       IFEQ      '0'
171400940509     C                   SETON                                        99
171500940509     C                   ENDIF
171600940509     C     $TASTO        WHENEQ    F10
171700940720     C     $FC(10)       IFEQ      '0'
171800940509     C                   SETON                                        99
171900940509     C                   ENDIF
172000940509     C     $TASTO        WHENEQ    F11
172100940720     C     $FC(11)       IFEQ      '0'
172200940509     C                   SETON                                        99
172300940509     C                   ENDIF
172400940509     C     $TASTO        WHENEQ    F12
172500940720     C     $FC(12)       IFEQ      '0'
172600940509     C                   SETON                                        99
172700940509     C                   ENDIF
172800940509     C     $TASTO        WHENEQ    F13
172900940720     C     $FC(13)       IFEQ      '0'
173000940509     C                   SETON                                        99
173100940509     C                   ENDIF
173200940509     C     $TASTO        WHENEQ    F14
173300940720     C     $FC(14)       IFEQ      '0'
173400940509     C                   SETON                                        99
173500940509     C                   ENDIF
173600940509     C     $TASTO        WHENEQ    F15
173700940720     C     $FC(15)       IFEQ      '0'
173800940509     C                   SETON                                        99
173900940509     C                   ENDIF
174000940509     C     $TASTO        WHENEQ    F16
174100940720     C     $FC(16)       IFEQ      '0'
174200940509     C                   SETON                                        99
174300940509     C                   ENDIF
174400940509     C     $TASTO        WHENEQ    F17
174500940720     C     $FC(17)       IFEQ      '0'
174600940509     C                   SETON                                        99
174700940509     C                   ENDIF
174800940509     C     $TASTO        WHENEQ    F18
174900940720     C     $FC(18)       IFEQ      '0'
175000940509     C                   SETON                                        99
175100940509     C                   ENDIF
175200940509     C     $TASTO        WHENEQ    F19
175300940720     C     $FC(19)       IFEQ      '0'
175400940509     C                   SETON                                        99
175500940509     C                   ENDIF
175600940509     C     $TASTO        WHENEQ    F20
175700940720     C     $FC(20)       IFEQ      '0'
175800940509     C                   SETON                                        99
175900940509     C                   ENDIF
176000940509     C     $TASTO        WHENEQ    F21
176100940720     C     $FC(21)       IFEQ      '0'
176200940509     C                   SETON                                        99
176300940509     C                   ENDIF
176400940509     C     $TASTO        WHENEQ    F22
176500940720     C     $FC(22)       IFEQ      '0'
176600940509     C                   SETON                                        99
176700940509     C                   ENDIF
176800940509     C     $TASTO        WHENEQ    F23
176900940720     C     $FC(23)       IFEQ      '0'
177000940509     C                   SETON                                        99
177100940509     C                   ENDIF
177200940509     C     $TASTO        WHENEQ    F24
177300940720     C     $FC(24)       IFEQ      '0'
177400940509     C                   SETON                                        99
177500940509     C                   ENDIF
177600940509     C                   ENDSL
177700940509     C                   ENDSR
177800940509     C/EJECT
177900940509     C************************************************************
178000940509     C* RIEMPIMENTO TASTI FUNZIONE
178100940509     C************************************************************
178200940509     C     RIEKEY        BEGSR
178300940509     C*
178400940509     C* Input :
178500940509     C* - $FX = schiera con tutti i testi disponibili
178600940509     C* - $FC = schiere con i flag di validità associati ai testi
178700940509     C* - $FR = schiere con i flag di REVERSE IMAGE
178800940509     C* - $ULK = ultimo testo caricato
178900940509     C* Output :
179000940509     C* - $ULK
179100940509     c* - $KEY1= 1° riga di tasti funzione
179200940509     c* - $KEY2= 2° riga di tasti funzione
179300940509     C*
179400970214     C                   CALLB     'XKEYMSG'
179500940509     C* input
179600940509     C                   PARM                    $FX
179700940509     C                   PARM                    $FC
179800940509     C                   PARM                    $FR
179900940509     C                   PARM                    $ULK              3 0
180000940509     C* output
180100940509     C                   PARM                    $KEY1            79
180200940509     C                   PARM                    $KEY2            79
180300940509     C*
180400940509     C                   ENDSR
180500940615**
18060094022401PROMSG    *LIBL     COS0001    immissione
18070094022402PROMSG    *LIBL     COS0002    modifica
18080094022403PROMSG    *LIBL     COS0003    copia
18090094022404PROMSG    *LIBL     COS0004    annullamento
18100094022405PROMSG    *LIBL     COS0005    visualizzazione
181100940509** TASTI DI FUNZIONE  D1
181200940509PROMSG    *LIBL     KEY0001  01
181300940509PROMSG    *LIBL     KEY0002  02
181400940509PROMSG    *LIBL     KEY0003  03
181500940509PROMSG    *LIBL     KEY0004  04
181600940509PROMSG    *LIBL     KEY0005  05
181700940509PROMSG    *LIBL     KEY0006  06
181800940509PROMSG    *LIBL     KEY0007  07
181900940509PROMSG    *LIBL     KEY0008  08
182000940509PROMSG    *LIBL     KEY0009  09
182100940509PROMSG    *LIBL     KEY0010  10
182200940509PROMSG    *LIBL     KEY0011  11
182300940509PROMSG    *LIBL     KEY0012  12
182400940509PROMSG    *LIBL     KEY0013  13
182500940509PROMSG    *LIBL     KEY0014  14
182600940509PROMSG    *LIBL     KEY0015  15
182700940509PROMSG    *LIBL     KEY0016  16
182800940509PROMSG    *LIBL     KEY0017  17
182900940509PROMSG    *LIBL     KEY0018  18
183000940509PROMSG    *LIBL     KEY0019  19
183100940509PROMSG    *LIBL     KEY0020  20
183200940509PROMSG    *LIBL     KEY0021  21
183300940509PROMSG    *LIBL     KEY0022  22
183400940509PROMSG    *LIBL     KEY0023  23
183500940509PROMSG    *LIBL     KEY0024  24
183600940509**  ABILITAZIONE D1
1837009407200N  01
1838009407200N  02
1839009407201N  03
1840009407201N  04
1841009407201N  05
1842009407201N  06
1843009407201N  07
1844009407201N  08
1845009407200N  09
1846009407200N  10
1847009407200N  11
1848009407201N  12
1849009407200N  13
1850009407200N  14
1851009407200N  15
1852009407200N  16
1853009407200N  17
1854009407200N  18
1855009407200N  19
1856009407200N  20
1857009407200N  21
1858009407200N  22
1859009407200N  23
1860009407200N  24
