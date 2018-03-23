000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
004100980720$001 FANGYP2D2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
004301980720     Fanban01l  IF   E           K DISK
004302980720     Fanope01l  IF   E           K DISK
004303980720     Fansog01l  IF   E           K DISK
004304980720     Fanrco99j  IF   E           K DISK
004400940128     D*----------------------------------------------------*
004500940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004600940224     D $ID             S             27    DIM(5) ALT($OZ)
004700940506     D* Schiere per la gestione dei tasti di funzione
004800940506     D $FM             S             27    DIM(24)
004900940506     D $FX             S             50    DIM(24)
005000940506     D $FC             S              1    DIM(24)
005100940506     D $FR             S              1    DIM(24)
005200940506     D* Video D1
005300940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005400940506     D $FX1            S             50    DIM(24) ALT($FM1)
005500940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005600940506     D $FR1            S              1    DIM(24) ALT($FC1)
005700940615     D* campi in lingua
005800940622$010 D*LIN             S             06    DIM(0011) CTDATA PERRCD(001)
005900940622$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006000980720$003 D EXTDS         E DS                  EXTNAME(ANGYP2DS) INZ
006100940127     D* Passaggio Parametri
006200940127     D KPJBA         E DS
006300941103     D*-------------
006400941103      * DS Società
006500941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
006600960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
006700960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
006800941103     D*-------------
006900941103      * DS Interna per dati utente
007000941103     D XSOCDS          DS          1000
007100941103     D*-------------
007200940616     D XATBDS        E DS                  INZ
007300940318     D*-------------
007400940318     D* DS Esterna gestione messaggi per COMIT
007500940616     D XMSGDS        E DS                  INZ
007600940318     D  STS          E                     EXTFLD(MSGSTS)
007700940318     D                                     DIM(333)
007800940318     D  JBA          E                     EXTFLD(MSGJBA)
007900940318     D                                     DIM(502)
008000940203     D*-------------
008100940620     D* DS Esterna autorizzazioni tabelle
008200940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
008300940620     D*
008400940620     D* DS  restituzione autorizzazioni
008500940620     D* IND=indice
008600940620     D* GRU=man. di gruppo
008700940620     D* AZI=man. azienda
008800940712     D* ABI=abilit. ai riservati
008900941111     D AUTDS           DS
009000940620     D  MANIND                 1      1
009100940620     D  MANGRU                 2      2
009200940620     D  MANAZI                 3      3
009300940712     D  ABIRIS                 4      4
009400940620     D*-------------
009500940207     D* opzione richiesta
009600940614    >D D002DS          DS
009700940614     D  OPZ002                 1      2
009800940614     D* 0 = ritorno normale
009900940614     D* 1 = F3
010000940614     D* 2 = F12
010100940614     D  RET002                 3      3
010200940614     D* operazione eseguite dal pgm chiamato :
010300940614     D* 0 = nessuna operazione
010400940614     D* 1 = eseguito aggiornamento
010500940614     D  OPR002                 4      4
010600940614     D* funzione non eseguibile per errore :
010700940614     D* 0 = nessun errore
010800940614     D* 1 = funzione richiamata chiusa in errore
010900940614     D* Aggiungere eventuali altri codici di errore
011000940614     D  ERR002                 5      5
011100940614     D* Azienda
011200940614     D  AZI002                 6      8
011300940614     D* Lingua
011400940614     D  LIN002                 9      9
011500940614     D* Codice scelto
011600940614     D  KEY002                10     24
011700940620     D* Autorizzazione
011800940620     D  AUT002                25     25
011801970509     D* Unita
011802970509     D  UNI002                26     33
011900940614     D*-------------
012000940201     D*-------------
012100940201     D DSFMT           DS
012200940506     D  $TASTO               369    369
012300940201     D  NRG                  370    370
012400940201     D  NCL                  371    371
012500940201     D*-------------
012600940201     D* posizione cursore
012700940201     D CURSOR          DS
012800940223     D  RIRI                   1      2B 0 INZ
012900940201     D  R$$                    2      2
013000940223     D  COCO                   3      4B 0 INZ
013100940201     D  C$$                    4      4
013200940207     D*-------------
013300940207     D* Reperimento nome PGM
013400940207     D STATUS         SDS           333
013500940207     D  DSPGM            *PROC
013600940225     D*-------------
013700940225     D* COSTANTI
013800940225     D*-------------
013900940506     D*
014000940506     D* Tasti di funzione
014100940506     D F01             C                   CONST(X'31')
014200940506     D F02             C                   CONST(X'32')
014300940506     D F03             C                   CONST(X'33')
014400940506     D F04             C                   CONST(X'34')
014500940506     D F05             C                   CONST(X'35')
014600940506     D F06             C                   CONST(X'36')
014700940506     D F07             C                   CONST(X'37')
014800940506     D F08             C                   CONST(X'38')
014900940506     D F09             C                   CONST(X'39')
015000940506     D F10             C                   CONST(X'3A')
015100940506     D F11             C                   CONST(X'3B')
015200940506     D F12             C                   CONST(X'3C')
015300940506     D F13             C                   CONST(X'B1')
015400940506     D F14             C                   CONST(X'B2')
015500940506     D F15             C                   CONST(X'B3')
015600940506     D F16             C                   CONST(X'B4')
015700940506     D F17             C                   CONST(X'B5')
015800940506     D F18             C                   CONST(X'B6')
015900940506     D F19             C                   CONST(X'B7')
016000940506     D F20             C                   CONST(X'B8')
016100940506     D F21             C                   CONST(X'B9')
016200940506     D F22             C                   CONST(X'BA')
016300940506     D F23             C                   CONST(X'BB')
016400940506     D F24             C                   CONST(X'BC')
016500940506     D ENTER           C                   CONST(X'F1')
016600940506     D ROLDWN          C                   CONST(X'F4')
016700940506     D ROLLUP          C                   CONST(X'F5')
016800940506     D*-------------
016900940506     D*
017000941108     D DATA            C                   CONST('0001-01-01')
017001980720     D*
017002980720     D*-------------
017003980720     D a250ds        E DS                  extname(ana250ds) inz
017004980720     D a070ds        E DS                  extname(ana070ds) inz
017005980720     D ana200ds      E DS                  inz
017006980720     D ana088ds      E DS                  inz
017007980720     D*
017008980720     D ass1            S              1    DIM(20) CTDATA PERRCD(20)            COMANDI
017009980720     D ass2            S              1    DIM(20) CTDATA PERRCD(20)            COMANDI
017010980720     D ass3            S              1    DIM(20) CTDATA PERRCD(20)            COMANDI
017100940128     I*----------------------------------------------------*
017200940615     ID1
017201980720$005 I              D1KCD                       §P2KCD
017300980720$005 I              D1BAS                       §P2BAS
017400980720$005 I              D1KCS                       §P2KCS
017500980720$005 I              D1KSS                       §P2KSS
017501980720$005 I              D1BANCA                     §P2BANCA
017502980720$005 I              D1LINEA                     §P2LINEA
017503980720$005 I              D1CAS                       §P2CAS
017504980720$005 I              D1KBA                       §P2KBA
017600980720$005 I              D1BAA                       §P2BAA
017700980720$005 I              D1KCA                       §P2KCA
017800980720$005 I              D1KSA                       §P2KSA
017801980720$005 I              D1CAU                       §P2CAU
017900980720$005 I              D1TIP                       §P2TIP
018100980720$005 I              D1AZI                       §P2AZI
018101980720$005 I              D1SIF                       §P2SIF
018500980720$005 I**            D1DCA                       §P2DCA
018800980720     I*
018900940127     C*----------------------------------------------------*
019000940127     C*                MAIN LINE PROGRAM
019100940127     C*----------------------------------------------------*
019200940127     C*
019300940223     C* inizializzazione variabili
019400940223     C                   EXSR      INZVAR
019500940223     C*
019600940223     C     $FINE         DOWEQ     *OFF
019700940202     C     $GEST         CASEQ     'D1'          GESD1
019800940117     C                   END
019900940117     C                   END
020000940201     C*
020100940325     C* fine programma
020200940325     C                   EXSR      ENDPGM
020300940325     C/EJECT
020400940325     C************************************************************
020500940325     C* RICERCHE
020600940325     C************************************************************
020700940325     C     ENDPGM        BEGSR
020800940325     C*
020900940614     C* disallocazione rcd nel caso di
021000971110$007 C                   MOVE      'YP2'         XTBCOD
021100940614    >C                   MOVE      '7'           XTBRIC
021200970214     C                   CALLB     'XATB'
021300940614     C                   PARM                    XATBDS
021400940325     C*
021500940325     C* passaggio dati al pgm chiamante
021600940715     C                   MOVE      *BLANKS       KPJBU
021700940325    >C                   MOVEL     D002DS        KPJBU
021800940325     C*
021900940325     C                   SETON                                            RT
022000940325     C*
022100940325     C                   ENDSR
022200940325     C/EJECT
022300940201     C************************************************************
022400940201     C* RICERCHE
022500940201     C************************************************************
022600940127     C     SEARCH        BEGSR
022700940127     C*
022800940127     C* determino Riga/Colonna del cursore
022900940127     C                   MOVE      NRG           R$$
023000940127     C                   MOVE      NCL           C$$
023100940127     C                   Z-ADD     RIRI          CURR
023200940127     C                   Z-ADD     COCO          CURC
023300940127     C*
023400940224     C* I campi che al momento dell'F4 risultano protetti non devono
023500940224     C* essere abilitati alla funzione di ricerca
023600940224     C*
0237009401271    C                   SELECT
023800940127     C*
023900940315     C* FMTD1  - PACCHETTO
024000940614    >C*          H1NMFL    WHEQ 'D1PKG'
024100940207     C* imposto pos. del cursore
024200940614     C*                    Z-ADDCURR      H1RIGA
024300940614     C*                    Z-ADDCURC      H1COLO
024417980720     C*
024418980720     C* FMTD1  - capoconto
024419980720    >C     H1NMFL        WHEneq    'D1KCD'
024420980720     C                   RESET                   Ana200DS
024421980720     C                   MOVEL     '01'          OPZ200
024422980720     C                   clear                   KCC200
024423980720     C                   MOVEL     Ana200DS      KPJBU
024424980720     C                   CALL      'ANA200R'
024425980720     C                   PARM                    KPJBA
024426980720     C                   MOVEL     KPJBU         Ana200DS
024427980720     C     KCC200        IFNE      *BLANK
024428980720     C                   MOVEL     KCC200        §p2Kcd
024429980720     C                   ENDIF
024430980720     C* imposto pos. del cursore
024431980720     C                   Z-ADD     CURR          H1RIGA
024432980720     C                   Z-ADD     CURC          H1COLO
024433980720     C*
024434980720     C* Ricerca sottoconti (ANA250R)
024435980720     C     H1NMFL        WHENEQ    'D1BAS'
024436980720     C                   clear                   §p2bas
024437980720     C                   RESET                   a250DS
024438980720     C                   MOVEL     '01'          OPZ250
024439981001     C                   MOVE      §p2kcd        KCC250
024440980720     C*                  MOVE      D1KSC         KSC250
024441980720     C                   clear                   KSC250
024442980720     C                   MOVEL     a250DS        KPJBU
024443980720     C                   CALL      'ANA250R'
024444980720     C                   PARM                    KPJBA
024445980720     C                   MOVEL     KPJBU         a250DS
024446980720     C     KSC250        IFNE      *BLANK
024447980720     C                   MOVEL     KCC250        §p2kcd
024448980720     C                   MOVEL     KSC250        §p2bas
024449980720     C                   ENDIF
024450980720     C* imposto pos. del cursore
024451980720     C                   Z-ADD     CURR          H1RIGA
024452980720     C                   Z-ADD     CURC          H1COLO
024453980720     C*
024454980720     C* FMTD1  - capoconto
024455980720    >C     H1NMFL        WHEneq    'D1KCS'
024456980720     C                   RESET                   Ana200DS
024457980720     C                   MOVEL     '01'          OPZ200
024458980720     C                   clear                   KCC200
024459980720     C                   MOVEL     Ana200DS      KPJBU
024460980720     C                   CALL      'ANA200R'
024461980720     C                   PARM                    KPJBA
024462980720     C                   MOVEL     KPJBU         Ana200DS
024463980720     C     KCC200        IFNE      *BLANK
024464980720     C                   MOVEL     KCC200        §p2Kcs
024465980720     C                   ENDIF
024466980720     C* imposto pos. del cursore
024467980720     C                   Z-ADD     CURR          H1RIGA
024468980720     C                   Z-ADD     CURC          H1COLO
024469980720     C*
024470980720     C* Ricerca sottoconti (ANA250R)
024471980720     C     H1NMFL        WHENEQ    'D1KSS'
024472980720     C                   clear                   §p2kss
024473980720     C                   RESET                   a250DS
024474980720     C                   MOVEL     '01'          OPZ250
024475980720     C                   MOVEL     §p2kcs        KCC250
024476980720     C*                  MOVE      D1KSC         KSC250
024477980720     C                   clear                   KSC250
024478980720     C                   MOVEL     a250DS        KPJBU
024479980720     C                   CALL      'ANA250R'
024480980720     C                   PARM                    KPJBA
024481980720     C                   MOVEL     KPJBU         a250DS
024482980720     C     KSC250        IFNE      *BLANK
024483980720     C                   MOVEL     KCC250        §p2kcs
024484980720     C                   MOVEL     KSC250        §p2kss
024485980720     C                   ENDIF
024486980720     C* imposto pos. del cursore
024487980720     C                   Z-ADD     CURR          H1RIGA
024488980720     C                   Z-ADD     CURC          H1COLO
024489980720     C*
024490980720     C* Ricerca  Rapporto Bancario (ANA088R)
024491980720     C     H1NMFL        WHENEQ    'D1BANCA'
024492980720     C     H1NMFL        OREQ      'D1LINEA'
024493980720     C                   RESET                   ANA088DS
024494980720     C                   MOVEL     '01'          OPZ088
024495980720     C                   MOVEL     ANA088DS      KPJBU
024496980720     C                   CALL      'ANA088R'
024497980720     C                   PARM                    KPJBA
024498980720     C                   MOVEL     KPJBU         ANA088DS
024499980720     C     banca088      ifne      *blank
024500980720     C     linea088      orne      *blank
024501980720     C                   MOVEL     BANCA088      §p2banca
024502980720     C                   MOVEL     LINEA088      §p2linea
024503980720     C                   endif
024504980720     C*
024505980720    >C*  FMTD1  - CAUSALE 1
024506980720     C     H1NMFL        WHENEQ    'D1CAS'
024507980720     C                   RESET                   A070DS
024508980720     C                   MOVEL     '01'          OPZ070
024509980720     C                   MOVEL     A070DS        KPJBU
024510980720     C                   CALL      'ANA070R'
024511980720     C                   PARM                    KPJBA
024512980720     C                   MOVEL     KPJBU         A070DS
024513980720     C                   MOVEL     CAU070        WWCAUSC1          4
024514980720     C*
024515980720     C     WWCAUSC1      IFNE      *BLANK
024516980720     C                   MOVEL     WWCAUSC1      §p2cas
024517980720     C                   ENDIF
024518980720     C* imposto pos. del cursore
024519980720     C                   Z-ADD     CURR          H1RIGA
024520980720     C                   Z-ADD     CURC          H1COLO
024521980720     C* FMTD1  - capoconto
024522980720    >C     H1NMFL        WHEneq    'D1KBA'
024523980720     C                   RESET                   Ana200DS
024524980720     C                   MOVEL     '01'          OPZ200
024525980720     C                   clear                   KCC200
024526980720     C                   MOVEL     Ana200DS      KPJBU
024527980720     C                   CALL      'ANA200R'
024528980720     C                   PARM                    KPJBA
024529980720     C                   MOVEL     KPJBU         Ana200DS
024530980720     C     KCC200        IFNE      *BLANK
024531980720     C                   MOVEL     KCC200        §p2KBA
024532980720     C                   ENDIF
024533980720     C* imposto pos. del cursore
024534980720     C                   Z-ADD     CURR          H1RIGA
024535980720     C                   Z-ADD     CURC          H1COLO
024536980720     C*
024537980720     C* Ricerca sottoconti (ANA250R)
024538980720     C     H1NMFL        WHENEQ    'D1BAA'
024539980720     C                   clear                   §p2baa
024540980720     C                   RESET                   a250DS
024541980720     C                   MOVEL     '01'          OPZ250
024542980720     C                   MOVEL     §p2kba        KCC250
024543980720     C*                  MOVE      D1KSC         KSC250
024544980720     C                   clear                   KSC250
024545980720     C                   MOVEL     a250DS        KPJBU
024546980720     C                   CALL      'ANA250R'
024547980720     C                   PARM                    KPJBA
024548980720     C                   MOVEL     KPJBU         a250DS
024549980720     C     KSC250        IFNE      *BLANK
024550980720     C                   MOVEL     KCC250        §p2kba
024551980720     C                   MOVEL     KSC250        §p2baa
024552980720     C                   ENDIF
024553980720     C* imposto pos. del cursore
024554980720     C                   Z-ADD     CURR          H1RIGA
024555980720     C                   Z-ADD     CURC          H1COLO
024556980720     C*
024557980720     C* FMTD1  - capoconto
024558980720    >C     H1NMFL        WHEneq    'D1KCA'
024559980720     C                   RESET                   Ana200DS
024560980720     C                   MOVEL     '01'          OPZ200
024561980720     C                   clear                   KCC200
024562980720     C                   MOVEL     Ana200DS      KPJBU
024563980720     C                   CALL      'ANA200R'
024564980720     C                   PARM                    KPJBA
024565980720     C                   MOVEL     KPJBU         Ana200DS
024566980720     C     KCC200        IFNE      *BLANK
024567980720     C                   MOVEL     KCC200        §p2Kca
024568980720     C                   ENDIF
024569980720     C* imposto pos. del cursore
024570980720     C                   Z-ADD     CURR          H1RIGA
024571980720     C                   Z-ADD     CURC          H1COLO
024572980720     C*
024573980720     C* Ricerca sottoconti (ANA250R)
024574980720     C     H1NMFL        WHENEQ    'D1KSA'
024575980720     C                   clear                   §p2ksa
024576980720     C                   RESET                   a250DS
024577980720     C                   MOVEL     '01'          OPZ250
024578980720     C                   MOVEL     §p2kca        KCC250
024579980720     C*                  MOVE      D1KSC         KSC250
024580980720     C                   clear                   KSC250
024581980720     C                   MOVEL     a250DS        KPJBU
024582980720     C                   CALL      'ANA250R'
024583980720     C                   PARM                    KPJBA
024584980720     C                   MOVEL     KPJBU         a250DS
024585980720     C     KSC250        IFNE      *BLANK
024586980720     C                   MOVEL     KCC250        §p2kca
024587980720     C                   MOVEL     KSC250        §p2ksa
024588980720     C                   ENDIF
024589980720     C* imposto pos. del cursore
024590980720     C                   Z-ADD     CURR          H1RIGA
024591980720     C                   Z-ADD     CURC          H1COLO
024592980720     C*
024593980720    >C*  FMTD1  - CAUSALE 1
024594980720     C     H1NMFL        WHENEQ    'D1CAU'
024595980720     C                   RESET                   A070DS
024596980720     C                   MOVEL     '01'          OPZ070
024597980720     C                   MOVEL     A070DS        KPJBU
024598980720     C                   CALL      'ANA070R'
024599980720     C                   PARM                    KPJBA
024600980720     C                   MOVEL     KPJBU         A070DS
024601980720     C                   MOVEL     CAU070        WWCAUSC1          4
024602980720     C*
024603980720     C     WWCAUSC1      IFNE      *BLANK
024604980720     C                   MOVEL     WWCAUSC1      §p2cau
024605980720     C                   ENDIF
024606980720     C* imposto pos. del cursore
024607980720     C                   Z-ADD     CURR          H1RIGA
024608980720     C                   Z-ADD     CURC          H1COLO
024609940223     C*
024610940223     C* ricerca non abilitata in questa posizione
0247009402231O   C                   OTHER
024800941005     C                   SETON                                        9899
024900941005     C* imposto pos. del cursore
025000941005     C                   Z-ADD     CURR          H1RIGA
025100941005     C                   Z-ADD     CURC          H1COLO
025200940127     C*
0253009401271-   C                   ENDSL
025400940127     C*
025500940506     C                   MOVE      *HIVAL        $LASTV
025600940127     C                   ENDSR
025700940615     C/EJECT
025800940615     C************************************************************
025900940615     C* TRADUZIONI
026000940615     C************************************************************
026100940615     C     LINGUA        BEGSR
026200940615     C*
026300940615     C* determino Riga/Colonna del cursore
026400940615     C                   MOVE      NRG           R$$
026500940615     C                   MOVE      NCL           C$$
026600940615     C                   Z-ADD     RIRI          CURR
026700940615     C                   Z-ADD     COCO          CURC
026800940615     C*
026900940615     C*
027000940615     C*
027100940615     C* FMTD1  - CAMPI DA TRADURRE
027200940615     C* imposto pos. del cursore
027300940615     C                   Z-ADD     CURR          H1RIGA
027400940615     C                   Z-ADD     CURC          H1COLO
027500940621     C                   MOVEL     H1NMFL        W005              5
027600940621     C                   MOVE      W005          W003              3
027700940621     C                   MOVEL     W003          W006              6
027800940620     C                   MOVE      *ZERO         KEY              15
027900980720     C                   MOVEL     D1COD         KEY
028000940615     C                   SELECT
028100940615     C*
028200940615     C*
028300940615     C* ricerca non abilitata in questa posizione
0284009406151O   C                   OTHER
028500940615     C                   SETON                                        98
028600940615     C*
0287009406151-   C                   ENDSL
028800940615     C*
028900940615     C                   MOVE      *HIVAL        $LASTV
029000940615     C                   ENDSR
029100940225     C/EJECT
029200940225     C************************************************************
029300940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
029400940225     C************************************************************
029500940225     C     GESPIE        BEGSR
029600940225     C*
029700940225     C                   SELECT
029800940225     C*
029900940506     C     $GEST         WHENEQ    'D1'
030000940506     C     $ULKD1        IFEQ      0
030100940506     C* riempimento opzioni
030200940506     C                   EXSR      F24D1
030300940506     C                   ELSE
030400940614     C                   WRITE     Z1
030500940506     C                   ENDIF
030600940225     C*
030700940510     C*
030800940225     C                   ENDSL
030900940225     C*
031000940225     C                   ENDSR
031100940131     C/EJECT
031200940131     C************************************************************
031300940510     C* GESTIONE VIDEO RECORD D1
031400940131     C************************************************************
031500940127     C     GESD1         BEGSR
031600940225     C* inizializzazione videata
031700940223     C     $INZD1        IFEQ      *ON
031800940127     C                   EXSR      INZD1
031900940223     C                   MOVE      *OFF          $INZD1
032000940117     C                   END
032100940225     C* emissione piede videata se proveniente da altra
032200940225     C* salvataggio valore formato di provenienza
032300940207     C     $LASTV        IFNE      'D1'
032400940614     C                   WRITE     T1
032500940225     C                   EXSR      GESPIE
032600940202     C                   MOVE      $LASTV        $LASTG
032700940202     C                   MOVE      'D1'          $LASTV
032800940202     C                   END
032900940131     C*
033000940203     C                   SELECT
033100940203     C* Annullamento
033200940315    >C     OPZ002        WHENEQ    '04'
033300940224     C* Visualizzazione
033400940315    >C     OPZ002        OREQ      '05'
033500940620     C                   SETON                                        0203
033600940620     C* Manutenzione
033700940620    >C     OPZ002        WHENEQ    '02'
033800940620     C                   SETON                                        02
033900940620     C                   SETOFF                                       03
034000940620     C*
034100940620     C                   OTHER
034200940620     C*
034300940620     C                   SETOFF                                       0203
034400940203     C                   ENDSL
034500940620     C* Immissione/Modifica/Copia
034600940620     C                   EXFMT     D1
034700940506     C* controllo abilitazione tasti funzione
034800940506     C*
034900940506     C                   EXSR      CTRKD1
035000940506     C*
035100940506     C     *IN99         IFEQ      *OFF
035200940131     C*
0353009402031    C                   SELECT
035400940510     c* ROLLUP
035500940131     C* F3=Fine
035600940506     C     $TASTO        WHENEQ    F03
035700940309     C                   EXSR      F03D1
035800940131     C* F4=Ricerca
035900940506     C     $TASTO        WHENEQ    F04
036000940127     C                   EXSR      SEARCH
036100940224     C* F5=Refresh
036200940506     C     $TASTO        WHENEQ    F05
036300940309     C                   EXSR      F05D1
036400940223     C* F8=Successivo
036500940506     C     $TASTO        WHENEQ    F08
036600940309     C                   EXSR      F08D1
036700940131     C* F12=Ritorno
036800940506     C     $TASTO        WHENEQ    F12
036900940309     C                   EXSR      F12D1
037000940315     C* F24=Altri tasti
037100940506     C     $TASTO        WHENEQ    F24
037200940315     C                   EXSR      F24D1
037300940131     C*
0374009402031O   C                   OTHER
037500940127     C                   EXSR      CTRD1
037600940616     C     *IN99         IFEQ      *OFF
037700940616     C                   SELECT
037800940616     C* F7=Traduzione
037900940616     C     $TASTO        WHENEQ    F07
038000940616     C                   EXSR      LINGUA
038100940616     C* F6
0382009406162    C     $TASTO        WHENEQ    F06
038300940224     C* eseguo aggiornamento
038400940315    >C                   EXSR      AGGANA
038500940224     C* eseguo operazioni del dopo-aggiornamento
0386009403183    C     *IN99         IFEQ      *OFF
038700940318     C                   EXSR      GESAGG
0388009403183-   C                   ENDIF
0389009406162-   C                   ENDSL
039000940616     C                   ENDIF
039100940203     C*
0392009402241-   C                   ENDSL
039300940224     C*
0394009405062-   C                   ENDIF
039500940117     C                   ENDSR
039600940506     C/EJECT
039700940506     C************************************************************
039800940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
039900940506     C************************************************************
040000940506     C     CTRKD1        BEGSR
040100940506     C                   SETOFF                                       99
040200940506     C                   MOVEA     $FC1          $FC
040300940506     C                   EXSR      CTRKEY
040400940506     C     *IN99         IFEQ      *ON
040500940506     C                   SETON                                        97
040600940506     C                   ENDIF
040700940506     C                   ENDSR
040800940117      ************************************************************
040900940207      * INIZIALIZZAZIONE VIDEATA DATI
041000940117      ************************************************************
041100940127     C     INZD1         BEGSR
041200940131     C*
041300940615     C                   CLEAR                   D1
041400941006     C                   MOVE      '0'           D1RIS
041500981001     C                   MOVE      KEY002        D1COD
041600940615     C                   MOVE      AZI002        XTBAZI
041700940615     C                   MOVE      LIN002        XTBLIN
041800940616     C                   MOVE      *ALL'0'       XTBKEY
041900981001     C                   MOVE      KEY002        XTBKEY
042000970509     C                   MOVE      UNI002        XTBUNT
042100940615$007 C                   MOVE      'YP2'         XTBCOD
042200940225     C*
042300940207     C*
042400940224     C* valorizzo la chiave univoca per reperire i dati
042500940224     C* se non immissione
0426009403151   >C     OPZ002        IFNE      '01'
042700940315    >C*-------------------------------------------------------------
042800940315    >C* impostare i campi chiave ricevuti
042900940315    >C*-------------------------------------------------------------
043000940318     C*
043100940614     C*
043200940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0433009406142   >C     OPZ002        IFEQ      '02'
043400940315    >C     OPZ002        OREQ      '04'
043500940614     C                   MOVE      '1'           XTBALC
043600940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0437009406142E   C                   ELSE
043800940614     C                   MOVE      '0'           XTBALC
0439009406142-   C                   ENDIF
044000940614     C*
044100940616     C                   MOVE      '1'           XTBRIC
044200970214     C                   CALLB     'XATB'
044300940614     C                   PARM                    XATBDS
044400940318     C*
044500940614     C     XTBERR        IFEQ      '0'
044600940614     C                   MOVEL     XTBUNI        EXTDS
044700970512     c                   move      xtbunt        d1uniTA           8
044800941108     C* Giro la data se non blank
044900970214     c                   move      xtbdsc        xtbdscA          10
045000970214     C     XTBDSCa       IFNE      DATA
045100970214     C                   CALLB     'XDT4000'
045200970214     C                   PARM      XTBDSCA       XDTAMG           10
045300940621     C                   PARM      *ZEROS        XDTGMA            6 0
045400940621     C                   PARM      2             XDTSTA            1 0
045500940621     C                   MOVE      XDTGMA        D1DSC
045600940712     C                   MOVE      XTBRIS        D1RIS
045700941108     C                   ELSE
045800941108     C                   MOVE      *ZEROS        D1DSC
045900941108     C                   ENDIF
046000941108     C                   ENDIF
046100940614     C*
046200940224     C*
046300940224     C* se il rcd da gestire non viene trovato
046400940318     C* oppure è allocato da altro pgm
046500940224     C* ritorno al pgm guida avvertendolo dell'errore
0466009406142    C     XTBERR        IFNE      '0'
046700940224     C                   MOVE      *ON           $FINE
046800940315    >C                   MOVE      '1'           ERR002
046900940224     C*
047000940224     C* altrimenti
0471009402242E   C                   ELSE
047200940224     C* riempio la videata
047300940224     C                   EXSR      RIED1
047400940224     C* richiamo routine dei ctrl per decodificarla
047500940614    >C                   EXSR      CTRD1
047600960228     C                   MOVEL     *IN99         SAV99             1
047700960228     C* Se la gestione è più videate
047800960305    >C                   EXSR      CTRD2
047900960305    >C     *IN99         IFEQ      *ON
048000960305    >C                   MOVEL     *IN99         SAV99             1
048100960305    >C                   ENDIF
048200940224     C                   MOVE      *ALL'0'       IN5098
048300940224     C                   MOVEA     IN5098        *IN(50)
048400940224     C* definizione indicatori su video
048500940224     C                   SETOFF                                       02
0486009402243    C                   SELECT
048700940315    >C     OPZ002        WHENEQ    '02'
048800940224     C                   SETON                                        02
048900940315    >C     OPZ002        WHENEQ    '05'
049000940506     C* disabilita tasti
049100940720     C                   MOVE      '0'           $FC(6)
049200940720     C                   MOVE      '0'           $FC(5)
049300940720     C                   MOVE      '0'           $FC(4)
0494009402243-   C                   ENDSL
049500940224     C*
0496009402242-   C                   ENDIF
049700940223     C*
0498009402241-   C                   ENDIF
049900940224     C*
050000940117     C                   ENDSR
050100940207     C/EJECT
050200940207     C************************************************************
050300940510     C* RIEMPIMENTO VIDEATA  D1
050400940207     C************************************************************
050500940207     C     RIED1         BEGSR
050600940207     C*
050700940207     C*
050800940207     C                   ENDSR
050900940309     C/EJECT
051000940309     C************************************************************
051100940309     C* GESTIONE F03 VIDEO D1
051200940309     C************************************************************
051300940309     C     F03D1         BEGSR
051400940309     C*
051500940309     C                   MOVE      *ON           $FINE
051600940315    >C                   MOVE      '1'           RET002
051700940325     C* fine programma
051800940325     C                   EXSR      ENDPGM
051900940325     C*
052000940325     C                   ENDSR
052100940309     C/EJECT
052200940309     C************************************************************
052300940309     C* GESTIONE F05 VIDEO D1
052400940309     C************************************************************
052500940309     C     F05D1         BEGSR
052600940309     C*
052700940309     C                   MOVE      *ON           $INZD1
052800940309     C*
052900940309     C                   ENDSR
053000940309     C/EJECT
053100940309     C************************************************************
053200940309     C* GESTIONE F08 VIDEO D1
053300940309     C************************************************************
053400940309     C     F08D1         BEGSR
053500940309     C*
053600940309     C                   MOVE      *ON           $FINE
053700940315    >C                   MOVE      '0'           RET002
053800940309     C*
053900940309     C                   ENDSR
054000940309     C/EJECT
054100940309     C************************************************************
054200940309     C* GESTIONE F12 VIDEO D1
054300940309     C************************************************************
054400940309     C     F12D1         BEGSR
054500940309     C*
054600940309     C                   MOVE      *ON           $FINE
054700940315    >C                   MOVE      '2'           RET002
054800940309     C*
054900940309     C                   ENDSR
055000940309     C/EJECT
055100940309     C************************************************************
055200940309     C* GESTIONE F24 VIDEO D1
055300940309     C************************************************************
055400940309     C     F24D1         BEGSR
055500940309     C*
055600940506     C*
055700940506     C                   MOVEA     $FX1          $FX
055800940506     C                   MOVEA     $FC1          $FC
055900940506     C                   MOVEA     $FR1          $FR
056000940506     C                   Z-ADD     $ULKD1        $ULK
056100940506     C                   EXSR      RIEKEY
056200941017     C                   MOVEA     $FC           $FC1
056300940506     C                   Z-ADD     $ULK          $ULKD1            3 0
056400940506     C                   MOVEL     $KEY1         Z1KE1
056500940506     C                   MOVEL     $KEY2         Z1KE2
056600940614     C                   WRITE     Z1
056700940506     C*
056800940309     C                   ENDSR
056900940207     C/EJECT
057000940131     C************************************************************
057100940207     C* CONTROLLO VIDEATA
057200940131     C************************************************************
057300940127     C     CTRD1         BEGSR
057400940131     C*
057500940127     C                   SETOFF                                       99
057600940127     C* non imposto pos. del cursore
057700940127     C                   Z-ADD     0             H1RIGA
057800940127     C                   Z-ADD     0             H1COLO
057900940621     C*
058000940621     C* CTRL DATA
058100940621     C*
058200941109     C                   MOVE      DATA          WRKDSC           10
058300940621     C     D1DSC         IFNE      0
058400970214     C                   CALLB     'XDT4000'
058500941109     C                   PARM                    XDTAMG           10
058600940621     C                   PARM      D1DSC         XDTGMA            6 0
058700940621     C                   PARM      3             XDTSTA            1 0
058800940621     C     XDTSTA        IFNE      0
058900940621     C                   SETON                                        5599
059000940621     C                   ELSE
059100941108     C                   MOVE      XDTAMG        WRKDSC
059200940621     C                   ENDIF
059300940621     C                   ENDIF
059400940223     C*
059500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
059600940223     C*
0597009403151   >C     OPZ002        IFNE      '04'
059800971110     C*
059900971110     C* controllo codice vuoto
060000971110$017 C     D1COD         IFEQ      *ALL' '
060100971110     C                   SETON                                        5099
060200971110     C                   ENDIF
060300971110     C*
060301980720     C                   clear                   d1basd
060302980720     C*
060303980720     c                   if        §p2kcd        <> *blanks   and
060304980720     c                             §p2bas        <> *blanks
060305981001     c                   move      §p2kcd        vkcc
060306981001     C                   move      §p2bas        KSCA
060307980720     C     KACO          CHAIN     anrco99j                           90
060308980720     C  n90rcosogg       chain     ansog01l                           90
060309980720     C     *IN90         IFEQ      '1'
060310980720     C*                  SETON                                        91
060311980720     C                   clear                   d1basd
060312980720     C                   ELSE
060313980720     C                   MOVEL     sogdes        d1basd
060314980720     C                   END
060315980720     C                   END
060400940207     C*
060403980720     C                   clear                   d1kasd
060404980720     C*
060405980720     c                   if        §p2kcs        <> *blanks   and
060406980720     c                             §p2kss        <> *blanks
060407980720     c                   movel     §p2kcs        vkcc
060408980720     C                   movel     §p2kss        KSCA
060409980720     C     KACO          CHAIN     anrco99j                           90
060410980720     C  n90rcosogg       chain     ansog01l                           90
060411980720     C     *IN90         IFEQ      '1'
060412980720     C*                  SETON                                        91
060413980720     C                   clear                   d1kasd
060414980720     C                   ELSE
060415980720     C                   MOVEL     sogdes        d1kasd
060416980720     C                   END
060417980720     C                   END
060418980720     C*
060421980720     C                   clear                   d1band                         *
060422980720     c*
060423980720     c                   if        §p2banca      <> *blanks and
060424980720     c                             §p2linea      <> *blanks
060425980720     c*
060428980720     C     Kbanca        CHAIN     anban01l                           33        *
060429980720     C     *IN33         IFEQ      '0'                                          *      B002
060430980720     C                   MOVEL     bandesbrev    d1band                         *
060431980720     C                   ELSE                                                   *      X002
060432980720     C                   clear                   d1band                         *
060433980720     C                   END                                                    *      E002
060434980720     C                   END                                                    *      E002
060435980720     C*
060436980720     c                   clear                   d1dcas
060437980720     c*
060438980720     c                   if        §p2cas        <> *blanks
060439980720     c                   movel     §p2cas        kkcau
060440980720     c     k02ope01      chain     anope01l                           55
060441980720     c  n55              movel     opedesbrev    d1dcas
060442980720     c                   endif
060443980720     C*
060444980720     C                   clear                   d1baad
060448980720     C*
060449980720     c                   if        §p2kca        <> *blanks   and
060450980720     c                             §p2ksa        <> *blanks
060451980720     c                   movel     §p2kca        vkcc
060452980720     C                   movel     §p2ksa        KSCA
060453980720     C     KACO          CHAIN     anrco99j                           90
060454980720     C  n90rcosogg       chain     ansog01l                           90
060455980720     C     *IN90         IFEQ      '1'
060456980720     C*                  SETON                                        91
060457980720     C                   clear                   d1baad
060458980720     C                   ELSE
060459980720     C                   MOVEL     sogdes        d1baad
060460980720     C                   END
060461980720     C                   END
060462980720     C*
060463980720     C                   clear                   d1kasd
060464980720     C*
060465980720     c                   if        §p2kcs        <> *blanks   and
060466980720     c                             §p2kss        <> *blanks
060467980720     c                   movel     §p2kcs        vkcc
060468980720     C                   movel     §p2kss        KSCA
060469980720     C     KACO          CHAIN     anrco99j                           90
060470980720     C  n90rcosogg       chain     ansog01l                           90
060471980720     C     *IN90         IFEQ      '1'
060472980720     C*                  SETON                                        91
060473980720     C                   clear                   d1kasd
060474980720     C                   ELSE
060475980720     C                   MOVEL     sogdes        d1kasd
060476980720     C                   END
060477980720     C                   END
060478980720     C*
060494980720     c                   clear                   d1dcau
060495980720     c*
060496980720     c                   if        §p2cau        <> *blanks
060497980720     c                   movel     §p2cau        kkcau
060498980720     c     k02ope01      chain     anope01l                           55
060499980720     c  n55              movel     opedesbrev    d1dcau
060500980720     c                   endif
060501980720     C*
060502980720     c*                            d1tip          d1tipd
060503980720     C*
060504980720     c                   clear                   d1tipd
060505980720     c*
060506980720     C     §P2TIP        IFEQ      'A'
060507980720     C                   movel     ass1(1)       d1tipd
060508980720     C                   endif
060509980720     C     §P2TIP        IFEQ      'T'
060510980720     C                   movel     ass2(1)       d1tipd
060511980720     C                   endif
060512980720     C     §P2TIP        IFEQ      'B'
060513980720     C                   movel     ass3(1)       d1tipd
060514980720     C                   endif
060515980720     C     §P2TIP        IFEQ      *blanks
060516980720     C                   clear                   d1tipd
060517980720     c*                  seton                                        99
060518980720     C                   endif
060528980720      *
060529940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
060600940315    >C     OPZ002        IFEQ      '01'
060700940315    >C     OPZ002        OREQ      '03'
060800940315    >C*-------------------------------------------------------------
060900940315    >C* impostare i campi chiave e verificare l'esistenza del record
061000940315    >C*-------------------------------------------------------------
061100971110$007 C                   MOVE      'YP2'         XTBCOD
061200940614     C                   MOVE      *ALL'0'       XTBKEY
061300981001    >C                   MOVE      D1COD         XTBKEY
061400970512    >C                   MOVE      D1UNITA       XTBUNT
061500940614     C                   MOVE      '0'           XTBALC
061600940614     C                   MOVE      '1'           XTBRIC
061700970214    >C                   CALLB     'XATB'
061800940614     C                   PARM                    XATBDS
061900940614     C     XTBERR        IFEQ      '0'
062000940223     C                   SETON                                        5999
062100940223     C                   ENDIF
062200940620     C* Controllo se compreso nei range indice
062300940620     C     XTBERR        IFEQ      '3'
062400940620    >C                   SETON                                        5899
062500940620     C                   END
062600940620     C*
062700940620     C* REPERISCO AUTORIZZAZIONI UTENTE
062800940620     C                   CLEAR                   XCHATB
062900941118     C                   MOVE      XSCSOC        XCTSOC
063000941118     C                   MOVE      XSCDUN        XCTUNI
063100940620     C                   MOVEL     KNMUS         XCTPRF
063200940620     C                   MOVE      XTBCOD        XCTCOD
063300940620     C                   CALL      'XCHKATB'
063400940620     C                   PARM                    XCHATB
063500941111     C                   MOVEL     XCTDDS        AUTDS
063600941027     C     XCTRTN        IFEQ      1
063700941027     C                   MOVEL     'N'           MANGRU
063800941027     C                   ENDIF
063900940620     C* Non ammessa immissione di gruppo
064000940620     C     XTBGRU        IFEQ      'G'
064100940620     C     MANGRU        ANDEQ     'N'
064200940620     C                   SETON                                        5799
064300940620     C                   END
064400940620     C* Non ammessa immissione di azienda
064500940620     C     XTBGRU        IFEQ      'A'
064600940620     C     MANAZI        ANDEQ     'N'
064700940620     C                   SETON                                        5699
064800940620     C                   END
064900940712     C* codice riservato non accessibile
065000941006     C     XTBRIS        IFGT      ABIRIS
065100941005    >C     OPZ002        ANDNE     '01'
065200941005    >C     OPZ002        ANDNE     '03'
065300940712     C                   SETON                                        5499
065400940712     C                   END
065500940224     C                   ENDIF
065600940224     C*
0657009402241-   C                   END
065800940131     C*
065900940302     C     *IN99         IFEQ      *ON
066000940510     C     $GEST         ANDEQ     'D1'
066100960228     C     OPZ002        ANDNE     '07'
066200940117     C                   MOVEA     *IN           WIN
066300940128     C                   MOVE      *ALL'0'       IN5098           49
066400940128     C                   MOVEA     IN5098        *IN(50)
066500940614     C                   WRITE     D1
066600940117     C                   MOVEA     WIN           *IN
066700940302     C                   ENDIF
066800940127     C*
066900940117     C                   ENDSR
067000960305     C/EJECT
067100960305     C************************************************************
067200960305     C* CONTROLLO VIDEATA D2
067300960305     C************************************************************
067400960305     C     CTRD2         BEGSR
067500960305     C*
067600960305     C                   ENDSR
067700940207     C/EJECT
067800940224     C************************************************************
067900940315    >C* AGGIORNAMENTO ANAGRAFICA
068000940224     C************************************************************
068100940315    >C     AGGANA        BEGSR
068200971110     C*
068300971110$007 C                   MOVE      'YP2'         XTBCOD
068400940614     C                   MOVE      *ALL'0'       XTBKEY
068500981001     C                   MOVE      D1COD         XTBKEY
068600970512     C                   MOVE      D1UNITA       XTBUNT
068700940207     C*
068800940207     C                   SELECT
068900940315    >C     OPZ002        WHENEQ    '01'
069000940315    >C     OPZ002        OREQ      '03'
069100940315    >C                   EXSR      RIEANA
069200950609     C                   MOVE      XSCCUT        XTBUTE
069300940614     C                   MOVE      '5'           XTBRIC
069400970214     C                   CALLB     'XATB'
069500940614    >C                   PARM                    XATBDS
069600940318     C* se rcd non scrivibile attivo errore
069700940615     C     XTBERR        IFEQ      '1'
069800940318    >C                   SETON                                        5999
069900940318     C                   END
070000940315    >C     OPZ002        WHENEQ    '02'
070100940621     C                   MOVE      '1'           XTBRIC
070200940621     C                   MOVE      '1'           XTBALC
070300970214     C                   CALLB     'XATB'
070400940621     C                   PARM                    XATBDS
070500940622    >C                   EXSR      RIEANA
070600950609     C                   MOVE      XSCCUT        XTBUTE
070700940614     C                   MOVE      '4'           XTBRIC
070800970214     C                   CALLB     'XATB'
070900940614    >C                   PARM                    XATBDS
071000940315    >C     OPZ002        WHENEQ    '04'
071100940621     C                   MOVE      '1'           XTBRIC
071200940621     C                   MOVE      '1'           XTBALC
071300970214     C                   CALLB     'XATB'
071400940621     C                   PARM                    XATBDS
071500940614     C                   MOVE      '6'           XTBRIC
071600970214     C                   CALLB     'XATB'
071700940614    >C                   PARM                    XATBDS
071800940621     C* annullo traduzioni
071900940621     C     XTBERR        IFEQ      '0'
072000940621     C                   EXSR      ANNTBT
072100940621     C                   END
072200940621     C*
072300940207     C                   ENDSL
072400940203     C*
072500940203     C                   ENDSR
072600940224     C/EJECT
072700940224     C************************************************************
072800940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
072900940224     C************************************************************
073000940616     C     RIEANA        BEGSR
073100940616     C                   CLEAR                   XTBUNI
073200940621     C                   MOVE      WRKDSC        XTBDSC
073300940712     C                   MOVE      D1RIS         XTBRIS
073400940616     C                   MOVEL     EXTDS         XTBUNI
073500940207     C                   ENDSR
073600940224     C/EJECT
073700940224     C************************************************************
073800940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
073900940224     C************************************************************
074000940224     C     GESAGG        BEGSR
074100940224     C*
074200940224     C* segnalo al pgm chiamante l'avvenuta conferma
074300940315    >C                   MOVE      *ON           OPR002
074400940224     C* segnalo al pgm chiamante l'aver premuto l'F6
074500940315    >C                   MOVE      '0'           RET002
074600940224     C*
0747009402241    C                   SELECT
074800940224     C* nel caso di immissione, ripristino la videata iniziale
074900940315    >C     OPZ002        WHENEQ    '01'
075000940224     C                   MOVE      *ON           $INZD1
075100940224     C* altrimenti ritorno al pgm chiamante
075200940224     C                   OTHER
075300940224     C                   MOVE      *ON           $FINE
0754009402241-   C                   ENDSL
075500940224     C*
075600940224     C                   ENDSR
075700940621     C/EJECT
075800940621     C************************************************************
075900940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
076000940621     C************************************************************
076100940621    >C     ANNTBT        BEGSR
076200940621     C                   MOVE      XTBAZI        TBTAZI
076300940621     C                   MOVE      XTBCOD        TBTCOD
076400981001     C                   MOVE      XTBKEY        TBTKEY
076500940621     C     KTBT          SETLL     ANTBT01L
076600940621     C                   DO        *HIVAL
076700940621     C     KTBT          DELETE    ANTBT01L                           21
076800940621     C  N21              END
076900940621     C*
077000940621     C                   ENDSR
077100940207     C/EJECT
077200940131     C************************************************************
077300940131     C* OPERAZIONI INIZIALI
077400940131     C************************************************************
077500940131     C     *INZSR        BEGSR
077600940131     C*
077700940127     C* Reperimento parametri
077800940127     C*
077900940117     C     *ENTRY        PLIST
078000940117     C                   PARM                    KPJBA
078100940712     C*
078200940712     C* Reperimento Codice utente
078300940712     C*
078400940223     C*
078500940506     C*
078600940506     C* Reperimento tasti di funzione
078700940506     C*
078800940506     C                   EXSR      REPKEY
078900940621     C*
079000940621     C* KLIST
079100940621     C*
079200940621     C     KTBT          KLIST
079300940621     C                   KFLD                    TBTAZI
079400940621     C                   KFLD                    TBTCOD
079500940621     C                   KFLD                    TBTKEY
079600940506     C*
079601980720     C     KACO          KLIST
079602980720     C                   KFLD                    xscsoc
079603980720     C                   KFLD                    VKCC              6
079604980720     C                   KFLD                    KSCA              8
079605980720     C*                                                    *
079606980720     c     K02ope01      klist
079607980720     c                   kfld                    Xscsoc
079608980720     c                   kfld                    kkcau             4
079609980720     C*                                                    *
079610980720     c     Kbanca        klist
079611980720     c                   kfld                    Xscsoc
079612980720     c                   kfld                    §p2banca
079613980720     C*                                                    *
079700940223     C                   ENDSR
079800941103     C/EJECT
079900941103      ************************************************************
080000941103      * Reperimento dati società
080100941103      ************************************************************
080200941103     C     REPSOC        BEGSR
080300941103      *
080400970214     C                   CALLB     'XSOC'
080500941103     C                   PARM                    TIPXSC            6
080600941103     C                   PARM                    SOCXSC            3
080700941103     C                   PARM                    CDSXSC            9 0
080800941103     C                   PARM                    MODXSC            3
080900941103     C                   PARM      *BLANKS       RTNXSC            1
081000941103     C                   PARM                    XSOCDS
081100941103     C                   PARM                    KPJBA
081200941103      *
081300941103     C                   ENDSR
081400940223     C/EJECT
081500940506     C************************************************************
081600940506     C* REPERIMENTO TASTI DI FUNZIONE
081700940506     C************************************************************
081800940506     C     REPKEY        BEGSR
081900940506     C                   MOVEA     $FM1          $FM
082000940506     C                   EXSR      REPKKK
082100940506     C                   MOVEA     $FX           $FX1
082200940614     C                   ENDSR
082300940506     C/EJECT
082400940506     C************************************************************
082500940506     C* REPERIMENTO TASTI DI FUNZIONE
082600940506     C************************************************************
082700940506     C     REPKKK        BEGSR
082800940506     C*
082900940506     C                   CLEAR                   $FX
083000940506     C     1             DO        24            X
083100940506     C*
083200940506     C     $FM(X)        IFNE      *BLANK
083300940506     C*
083400940506     C                   MOVEL     $FM(X)        IDMSG
083500970214     C                   CALLB     'XRTVM'
083600940506     C                   PARM                    IDMSG            27
083700940506     C                   PARM                    TXTMSG          100
083800940506     C                   PARM                    ERRMSG            1
083900940506     C*
084000940506     C     ERRMSG        IFNE      *ON
084100940506     C                   MOVEL     TXTMSG        $FX(X)
084200940506     C                   ELSE
084300940506     C                   MOVEL     *ALL' '       $FX(X)
084400940506     C                   END
084500940506     C*
084600940506     C                   END
084700940506     C*
084800940506     C                   END
084900940506     C*
085000940506     C                   ENDSR
085100940509     C/EJECT
085200940223     C************************************************************
085300940223     C* INIZIALIZZAZIONE VARIABILI
085400940223     C************************************************************
085500940223     C     INZVAR        BEGSR
085600951124     C*
085700951124     C* Chiamata XSOC
085800951124     C*
085900951124     C                   MOVEL     'SOC001'      TIPXSC
086000951124     C                   EXSR      REPSOC
086100951124     C     RTNXSC        IFNE      '1'
086200951124     C                   MOVEL     XSOCDS        SOC001
086300951124     C                   ELSE
086400951124     C                   MOVE      *ON           $FINE
086500951124     C* fine programma
086600951124     C                   EXSR      ENDPGM
086700951124     C                   ENDIF
086800940224     C*
086900940224     C* Dati da PGM guida
087000940224     C*
087100940315    >C                   MOVEL     KPJBU         D002DS
087200940128     C*
087300940223     C* Variabili per gestione videate
087400940128     C*
087500940223     C                   MOVE      'D1'          $GEST
087600940223     C                   MOVE      *OFF          $FINE
087700940223     C                   MOVE      *ON           $INZD1
087800940224     C                   MOVE      *BLANK        $LASTG
087900940224     C                   MOVE      *BLANK        $LASTV
088000940506     C                   Z-ADD     0             $ULKD1            3 0
088100940127     C*
088200940223     C* Variabili appoggio
088300940127     C*
088400940224     C                   Z-ADD     0             CURR
088500940224     C                   Z-ADD     0             CURC
088600940207     C*
088700940207     C* Valorizzazione campi univoci testate
088800940207     C*
088900940614     C                   CLEAR                   T1
089000940614     C                   MOVEL     KNSIF         NOMSIF
089100941109     C                   MOVEL     XSCDSI        NOMDIT
089200941027     C                   MOVEL     DSPGM         NOMPGM
089300940224     C                   Z-ADD     1             X
089400940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
089500940224     C     *IN21         IFEQ      *ON
089600940224     C                   MOVEL     $ID(X)        IDMSG
089700970214     C                   CALLB     'XRTVM'
089800940224     C                   PARM                    IDMSG            27
089900940224     C                   PARM                    TXTMSG          100
090000940224     C                   PARM                    ERRMSG            1
090100940224     C                   PARM      *ON           CTRMSG            1
090200940224     C                   PARM      30            LENMSG            3 0
090300940224     C     ERRMSG        IFNE      *ON
090400940224     C                   MOVEL     TXTMSG        T1OPE
090500940224     C                   ELSE
090600940224     C                   MOVEL     *ALL'?'       T1OPE
090700940224     C                   ENDIF
090800940224     C                   END
090900940224     C*
091000940224     C* Inizializzazione/Decodifica prima videata
091100940224     C*
091200940224     C                   EXSR      INZD1
091300940224     C                   MOVE      *OFF          $INZD1
091400960228     C* Se opzione = '07' richiesti solo controlli
091500960228     C     OPZ002        IFEQ      '07'
091600960228     C                   MOVEL     *IN99         ERR002
091700960228     C                   MOVEL     '0'           RET002
091800960228     C                   MOVEL     *ON           $FINE
091900960228     C                   ENDIF
092000940127     C*
092100940117     C                   ENDSR
092200940128     C************************************************************
092300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
092400940223     C************************************************************
092500940223     C     DEFCAM        BEGSR
092600940223     C*
092700940223     C* klist
092800940207     C*
092900940223     C*
093000940223     C* Variabili per gestione videate
093100940223     C*
093200940223     C                   MOVE      *BLANK        $GEST             2
093300940223     C                   MOVE      *BLANK        $FINE             1
093400940223     C                   MOVE      *BLANK        $INZD1            1
093500940223     C                   MOVE      *BLANK        $LASTG            2
093600940223     C                   MOVE      *BLANK        $LASTV            2
093700940223     C*
093800940223     C* Variabili appoggio
093900940223     C*
094000940223     C                   Z-ADD     0             CURR              2 0
094100940223     C                   Z-ADD     0             CURC              2 0
094200940223     C                   MOVE      *ZEROS        WIN              99
094300940224     C                   MOVE      *BLANK        WTBERR            1
094400940223     C*
094500940223     C* Indici
094600940223     C*
094700940223     C                   Z-ADD     0             X                 3 0
094800940223     C                   Z-ADD     0             Y                 3 0
094900940127     C*
095000940117     C                   ENDSR
095100940128     C************************************************************
095200940509     C/EJECT
095300940509     C************************************************************
095400940509     C* CONTROLLO ABILITAZIONE TASTI
095500940509     C************************************************************
095600940509     C     CTRKEY        BEGSR
095700940509     C                   SELECT
095800940509     C     $TASTO        WHENEQ    F01
095900940720     C     $FC(1)        IFEQ      '0'
096000940509     C                   SETON                                        99
096100940509     C                   ENDIF
096200940509     C     $TASTO        WHENEQ    F02
096300940720     C     $FC(2)        IFEQ      '0'
096400940509     C                   SETON                                        99
096500940509     C                   ENDIF
096600940509     C     $TASTO        WHENEQ    F03
096700940720     C     $FC(3)        IFEQ      '0'
096800940509     C                   SETON                                        99
096900940509     C                   ENDIF
097000940509     C     $TASTO        WHENEQ    F04
097100940720     C     $FC(4)        IFEQ      '0'
097200940509     C                   SETON                                        99
097300940509     C                   ENDIF
097400940509     C     $TASTO        WHENEQ    F05
097500940720     C     $FC(5)        IFEQ      '0'
097600940509     C                   SETON                                        99
097700940509     C                   ENDIF
097800940509     C     $TASTO        WHENEQ    F06
097900940720     C     $FC(6)        IFEQ      '0'
098000940509     C                   SETON                                        99
098100940509     C                   ENDIF
098200940509     C     $TASTO        WHENEQ    F07
098300940720     C     $FC(7)        IFEQ      '0'
098400940509     C                   SETON                                        99
098500940509     C                   ENDIF
098600940509     C     $TASTO        WHENEQ    F08
098700940720     C     $FC(8)        IFEQ      '0'
098800940509     C                   SETON                                        99
098900940509     C                   ENDIF
099000940509     C     $TASTO        WHENEQ    F09
099100940720     C     $FC(09)       IFEQ      '0'
099200940509     C                   SETON                                        99
099300940509     C                   ENDIF
099400940509     C     $TASTO        WHENEQ    F10
099500940720     C     $FC(10)       IFEQ      '0'
099600940509     C                   SETON                                        99
099700940509     C                   ENDIF
099800940509     C     $TASTO        WHENEQ    F11
099900940720     C     $FC(11)       IFEQ      '0'
100000940509     C                   SETON                                        99
100100940509     C                   ENDIF
100200940509     C     $TASTO        WHENEQ    F12
100300940720     C     $FC(12)       IFEQ      '0'
100400940509     C                   SETON                                        99
100500940509     C                   ENDIF
100600940509     C     $TASTO        WHENEQ    F13
100700940720     C     $FC(13)       IFEQ      '0'
100800940509     C                   SETON                                        99
100900940509     C                   ENDIF
101000940509     C     $TASTO        WHENEQ    F14
101100940720     C     $FC(14)       IFEQ      '0'
101200940509     C                   SETON                                        99
101300940509     C                   ENDIF
101400940509     C     $TASTO        WHENEQ    F15
101500940720     C     $FC(15)       IFEQ      '0'
101600940509     C                   SETON                                        99
101700940509     C                   ENDIF
101800940509     C     $TASTO        WHENEQ    F16
101900940720     C     $FC(16)       IFEQ      '0'
102000940509     C                   SETON                                        99
102100940509     C                   ENDIF
102200940509     C     $TASTO        WHENEQ    F17
102300940720     C     $FC(17)       IFEQ      '0'
102400940509     C                   SETON                                        99
102500940509     C                   ENDIF
102600940509     C     $TASTO        WHENEQ    F18
102700940720     C     $FC(18)       IFEQ      '0'
102800940509     C                   SETON                                        99
102900940509     C                   ENDIF
103000940509     C     $TASTO        WHENEQ    F19
103100940720     C     $FC(19)       IFEQ      '0'
103200940509     C                   SETON                                        99
103300940509     C                   ENDIF
103400940509     C     $TASTO        WHENEQ    F20
103500940720     C     $FC(20)       IFEQ      '0'
103600940509     C                   SETON                                        99
103700940509     C                   ENDIF
103800940509     C     $TASTO        WHENEQ    F21
103900940720     C     $FC(21)       IFEQ      '0'
104000940509     C                   SETON                                        99
104100940509     C                   ENDIF
104200940509     C     $TASTO        WHENEQ    F22
104300940720     C     $FC(22)       IFEQ      '0'
104400940509     C                   SETON                                        99
104500940509     C                   ENDIF
104600940509     C     $TASTO        WHENEQ    F23
104700940720     C     $FC(23)       IFEQ      '0'
104800940509     C                   SETON                                        99
104900940509     C                   ENDIF
105000940509     C     $TASTO        WHENEQ    F24
105100940720     C     $FC(24)       IFEQ      '0'
105200940509     C                   SETON                                        99
105300940509     C                   ENDIF
105400940509     C                   ENDSL
105500940509     C                   ENDSR
105600940509     C/EJECT
105700940509     C************************************************************
105800940509     C* RIEMPIMENTO TASTI FUNZIONE
105900940509     C************************************************************
106000940509     C     RIEKEY        BEGSR
106100940509     C*
106200940509     C* Input :
106300940509     C* - $FX = schiera con tutti i testi disponibili
106400940509     C* - $FC = schiere con i flag di validità associati ai testi
106500940509     C* - $FR = schiere con i flag di REVERSE IMAGE
106600940509     C* - $ULK = ultimo testo caricato
106700940509     C* Output :
106800940509     C* - $ULK
106900940509     c* - $KEY1= 1° riga di tasti funzione
107000940509     c* - $KEY2= 2° riga di tasti funzione
107100940509     C*
107200970214     C                   CALLB     'XKEYMSG'
107300940509     C* input
107400940509     C                   PARM                    $FX
107500940509     C                   PARM                    $FC
107600940509     C                   PARM                    $FR
107700940509     C                   PARM                    $ULK              3 0
107800940509     C* output
107900940509     C                   PARM                    $KEY1            79
108000940509     C                   PARM                    $KEY2            79
108100940509     C*
108200940509     C                   ENDSR
108300940615**
10840094022401PROMSG    *LIBL     COS0001    immissione
10850094022402PROMSG    *LIBL     COS0002    modifica
10860094022403PROMSG    *LIBL     COS0003    copia
10870094022404PROMSG    *LIBL     COS0004    annullamento
10880094022405PROMSG    *LIBL     COS0005    visualizzazione
108900940509** TASTI DI FUNZIONE  D1
109000940509PROMSG    *LIBL     KEY0001  01
109100940509PROMSG    *LIBL     KEY0002  02
109200940509PROMSG    *LIBL     KEY0003  03
109300940509PROMSG    *LIBL     KEY0004  04
109400940509PROMSG    *LIBL     KEY0005  05
109500940509PROMSG    *LIBL     KEY0006  06
109600940509PROMSG    *LIBL     KEY0007  07
109700940509PROMSG    *LIBL     KEY0008  08
109800940509PROMSG    *LIBL     KEY0009  09
109900940509PROMSG    *LIBL     KEY0010  10
110000940509PROMSG    *LIBL     KEY0011  11
110100940509PROMSG    *LIBL     KEY0012  12
110200940509PROMSG    *LIBL     KEY0013  13
110300940509PROMSG    *LIBL     KEY0014  14
110400940509PROMSG    *LIBL     KEY0015  15
110500940509PROMSG    *LIBL     KEY0016  16
110600940509PROMSG    *LIBL     KEY0017  17
110700940509PROMSG    *LIBL     KEY0018  18
110800940509PROMSG    *LIBL     KEY0019  19
110900940509PROMSG    *LIBL     KEY0020  20
111000940509PROMSG    *LIBL     KEY0021  21
111100940509PROMSG    *LIBL     KEY0022  22
111200940509PROMSG    *LIBL     KEY0023  23
111300940509PROMSG    *LIBL     KEY0024  24
111400940509**  ABILITAZIONE D1
1115009407200N  01
1116009407200N  02
1117009407201N  03
1118009407201N  04
1119009407201N  05
1120009407201N  06
1121009407201N  07
1122009407201N  08
1123009407200N  09
1124009407200N  10
1125009407200N  11
1126009407201N  12
1127009407200N  13
1128009407200N  14
1129009407200N  15
1130009407200N  16
1131009407200N  17
1132009407200N  18
1133009407200N  19
1134009407200N  20
1135009407200N  21
1136009407200N  22
1137009407200N  23
1138009407200N  24
113900980720**         ASS1
114000980720ASSEGNI
114100980720**         ASS2
114200980720ASSEGNI di TRAENZA
114300980720**         ASS3
114400980720BONIFICI
