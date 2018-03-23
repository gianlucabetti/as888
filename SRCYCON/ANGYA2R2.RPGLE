000100960417     H*PARMS DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200980925     H*PARMS OPTION(*NOXREF) CVTOPT(*NONE)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940117      *
000500980924      *           Attributi contabili - Tipo gestione
000600980924      *         ---------------------------------------
000700980924      *                      Manutenzione
000800940223      *
000900940223      *
001000940615      * DOCUMENTI MICROANALISI :
001100940223      *
001200940223      * NOTE TECNICHE :
001300940223      *
001400940223      *
001500940614      * PERSONALIZZAZIONI
001600940223      *
001700940223      *
001800940223      *
001900940223      *               MAPPA INDICATORI STD
002000940223      *
002100940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002200940620      *  03           PROTECT TUTTI I CAMPI
002300940223      *  21           GENERICO OPERAZIONI I/O
002400940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002500940223      *  30           SFLDSP
002600940223      * N31           SFLCLR
002700940128      *  31           SFLDSPCTL
002800940128      *  32           SFLNXTCHG
002900940128      *  33           SFLEND
003000940128      *  39           OF PRTF
003100940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003200940317    > *  Specificare l'uso dei singoli indicatori
003300940315      *  50 <---> 98  ERRORI SU VIDEO
003400940317    > *  Specificare l'uso dei singoli indicatori
003500940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003800940128      *  99           INDIC. GENERALE DI ERRORE
003900940128     F*----------------------------------------------------*
004000980924     FANGYA2D2  CF   E             WORKSTN
004100940201     F                                     INFDS(DSFMT)
004200940621     FANTBT01L  UF   E           K DISK
004300940128     D*----------------------------------------------------*
004400940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004500940224     D $ID             S             27    DIM(5) ALT($OZ)
004600940506     D* Schiere per la gestione dei tasti di funzione
004700940506     D $FM             S             27    DIM(24)
004800940506     D $FX             S             50    DIM(24)
004900940506     D $FC             S              1    DIM(24)
005000940506     D $FR             S              1    DIM(24)
005100940506     D* Video D1
005200940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005300940506     D $FX1            S             50    DIM(24) ALT($FM1)
005400940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005500940506     D $FR1            S              1    DIM(24) ALT($FC1)
005600940615     D* campi in lingua
005700980924     D LIN             S             06    DIM(0001) CTDATA PERRCD(001)
005800980924     D LIND            S             50    DIM(0001) ALT(LIN)
005900980924     D EXTDS         E DS                  EXTNAME(ANGYA2DS) INZ
006000940127     D* Passaggio Parametri
006100940127     D KPJBA         E DS
006200941103     D*-------------
006300941103      * DS Società
006400941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
006500941103     D*-------------
006600941103      * DS Interna per dati utente
006700941103     D XSOCDS          DS          1000
006800941103     D*-------------
006900940616     D XATBDS        E DS                  INZ
007000980924     D*-------------
007100980924     D* Parametri per ricerca codice ANATB00F
007200980924     D XTABDS          DS                  INZ
007300980924     D* '  ' = Gestione
007400980924     D* '01' = Ricerca
007500980924     D* '02' = Gestione
007600980924     D* '05' = Visualizzazione
007700980924     D* '11' = Abilitazione
007800980924     D  OPZTAB                 1      2
007900980924     D* 0 = ritorno normale
008000980924     D* 1 = F3
008100980924     D* 2 = F12
008200980924     D  RETTAB                 3      3
008300980924     D* operazione eseguite dal pgm chiamato :
008400980924     D* 0 = nessuna operazione
008500980924     D* 1 = eseguito aggiornamento
008600980924     D  OPRTAB                 4      4
008700980924     D* funzione non eseguibile per errore :
008800980924     D* 0 = nessun errore
008900980924     D* 1 = funzione richiamata chiusa in errore
009000980924     D* Aggiungere eventuali altri codici di errore
009100980924     D  ERRTAB                 5      5
009200980924     D* Azienda
009300980924     D  AZITAB                 6      8
009400980924     D* Lingua
009500980924     D  LINTAB                 9      9
009600980924     D* Codice scelto
009700980924     D  KEYTAB                10     24
009800980924     D* riga cursore chiamante
009900980924     D  RIGTAB                25     26  0
010000980924     D* colonna cursore chiamante
010100980924     D  COLTAB                27     28  0
010200980924     D* posizione colonna
010300980924     D  POSTAB                29     30
010400940318     D*-------------
010500980924     D* DS Esterna tabella 0A2
010600980924     D TAB0A2        E DS                  EXTNAME(ANG0A2DS)
010700980924     D*-------------
010800940318     D* DS Esterna gestione messaggi per COMIT
010900940616     D XMSGDS        E DS                  INZ
011000940318     D  STS          E                     EXTFLD(MSGSTS)
011100940318     D                                     DIM(333)                                           bili
011200940318     D  JBA          E                     EXTFLD(MSGJBA)
011300940318     D                                     DIM(502)                                           bili
011400940203     D*-------------
011500940620     D* DS Esterna autorizzazioni tabelle
011600940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
011700940620     D*
011800940620     D* DS  restituzione autorizzazioni
011900940620     D* IND=indice
012000940620     D* GRU=man. di gruppo
012100940620     D* AZI=man. azienda
012200940712     D* ABI=abilit. ai riservati
012300941111     D AUTDS           DS
012400940620     D  MANIND                 1      1
012500940620     D  MANGRU                 2      2
012600940620     D  MANAZI                 3      3
012700940712     D  ABIRIS                 4      4
012800940620     D*-------------
012900940207     D* opzione richiesta
013000940614    >D D002DS          DS
013100940614     D  OPZ002                 1      2
013200940614     D* 0 = ritorno normale
013300940614     D* 1 = F3
013400940614     D* 2 = F12
013500940614     D  RET002                 3      3
013600940614     D* operazione eseguite dal pgm chiamato :
013700940614     D* 0 = nessuna operazione
013800940614     D* 1 = eseguito aggiornamento
013900940614     D  OPR002                 4      4
014000940614     D* funzione non eseguibile per errore :
014100940614     D* 0 = nessun errore
014200940614     D* 1 = funzione richiamata chiusa in errore
014300940614     D* Aggiungere eventuali altri codici di errore
014400940614     D  ERR002                 5      5
014500940614     D* Azienda
014600940614     D  AZI002                 6      8
014700940614     D* Lingua
014800940614     D  LIN002                 9      9
014900940614     D* Codice scelto
015000940614     D  KEY002                10     24
015100940620     D* Autorizzazione
015200940620     D  AUT002                25     25
015300940614     D*-------------
015400940201     D*-------------
015500940201     D DSFMT           DS
015600940506     D  $TASTO               369    369
015700940201     D  NRG                  370    370
015800940201     D  NCL                  371    371
015900940201     D*-------------
016000940201     D* posizione cursore
016100940201     D CURSOR          DS
016200940223     D  RIRI                   1      2B 0 INZ
016300940201     D  R$$                    2      2
016400940223     D  COCO                   3      4B 0 INZ
016500940201     D  C$$                    4      4
016600940207     D*-------------
016700940207     D* Reperimento nome PGM
016800940207     D STATUS         SDS           333
016900940207     D  DSPGM            *PROC
017000940225     D*-------------
017100940225     D* COSTANTI
017200940225     D*-------------
017300940506     D*
017400940506     D* Tasti di funzione
017500940506     D F01             C                   CONST(X'31')
017600940506     D F02             C                   CONST(X'32')
017700940506     D F03             C                   CONST(X'33')
017800940506     D F04             C                   CONST(X'34')
017900940506     D F05             C                   CONST(X'35')
018000940506     D F06             C                   CONST(X'36')
018100940506     D F07             C                   CONST(X'37')
018200940506     D F08             C                   CONST(X'38')
018300940506     D F09             C                   CONST(X'39')
018400940506     D F10             C                   CONST(X'3A')
018500940506     D F11             C                   CONST(X'3B')
018600940506     D F12             C                   CONST(X'3C')
018700940506     D F13             C                   CONST(X'B1')
018800940506     D F14             C                   CONST(X'B2')
018900940506     D F15             C                   CONST(X'B3')
019000940506     D F16             C                   CONST(X'B4')
019100940506     D F17             C                   CONST(X'B5')
019200940506     D F18             C                   CONST(X'B6')
019300940506     D F19             C                   CONST(X'B7')
019400940506     D F20             C                   CONST(X'B8')
019500940506     D F21             C                   CONST(X'B9')
019600940506     D F22             C                   CONST(X'BA')
019700940506     D F23             C                   CONST(X'BB')
019800940506     D F24             C                   CONST(X'BC')
019900940506     D ENTER           C                   CONST(X'F1')
020000940506     D ROLDWN          C                   CONST(X'F4')
020100940506     D ROLLUP          C                   CONST(X'F5')
020200980924     I*---------------------------------------------------*
020300940127     C*----------------------------------------------------*
020400940127     C*                MAIN LINE PROGRAM
020500940127     C*----------------------------------------------------*
020600940127     C*
020700940223     C* inizializzazione variabili
020800940223     C                   EXSR      INZVAR
020900940223     C*
021000940223     C     $FINE         DOWEQ     *OFF
021100940202     C     $GEST         CASEQ     'D1'          GESD1
021200940117     C                   END
021300940117     C                   END
021400940201     C*
021500940325     C* fine programma
021600940325     C                   EXSR      ENDPGM
021700940325     C/EJECT
021800940325     C************************************************************
021900940325     C* RICERCHE
022000940325     C************************************************************
022100940325     C     ENDPGM        BEGSR
022200940325     C*
022300940614     C* disallocazione rcd nel caso di
022400940614    >C                   MOVE      '7'           XTBRIC
022500961004     C                   CALLb     'XATB'
022600940614     C                   PARM                    XATBDS
022700940325     C*
022800940325     C* passaggio dati al pgm chiamante
022900940715     C                   MOVE      *BLANKS       KPJBU
023000940325    >C                   MOVEL     D002DS        KPJBU
023100940325     C*
023200940325     C                   SETON                                            RT
023300940325     C*
023400940325     C                   ENDSR
023500940325     C/EJECT
023600940201     C************************************************************
023700940201     C* RICERCHE
023800940201     C************************************************************
023900940127     C     SEARCH        BEGSR
024000940127     C*
024100940127     C* determino Riga/Colonna del cursore
024200940127     C                   MOVE      NRG           R$$
024300940127     C                   MOVE      NCL           C$$
024400940127     C                   Z-ADD     RIRI          CURR
024500940127     C                   Z-ADD     COCO          CURC
024600940127     C*
024700940224     C* I campi che al momento dell'F4 risultano protetti non devono
024800940224     C* essere abilitati alla funzione di ricerca
024900940224     C*
0250009401271    C                   SELECT
025100940127     C*
025200980924     C* FMTD1  - categorie inserite
025300980924    >C     H1NMFL        WHENEQ    'D1COD'
025400980924     C                   MOVE      '01'          OPZTAB
025500980924     C                   MOVE      *ZEROS        KEYTAB
025600980924     C                   MOVE      XSCSOC        AZITAB
025700980924     C                   MOVE      XSCLIN        LINTAB
025800980924     C                   CLEAR                   KPJBU
025900980924     C                   MOVEL     XTABDS        KPJBU
026000980924     C                   CALL      'ANG0A2R'
026100980924     C                   PARM                    KPJBA
026200980924     C                   MOVEL     KPJBU         XTABDS
026300980924     C                   MOVE      KEYTAB        D1COD
026400980924     C*
026500940223     C* ricerca non abilitata in questa posizione
0266009402231O   C                   OTHER
026700980924     C                   SETON                                          98
026800941005     C* imposto pos. del cursore
026900941005     C                   Z-ADD     CURR          H1RIGA
027000941005     C                   Z-ADD     CURC          H1COLO
027100940127     C*
0272009401271-   C                   ENDSL
027300940127     C*
027400940506     C                   MOVE      *HIVAL        $LASTV
027500940127     C                   ENDSR
027600940615     C/EJECT
027700940615     C************************************************************
027800940615     C* TRADUZIONI
027900940615     C************************************************************
028000940615     C     LINGUA        BEGSR
028100940615     C*
028200940615     C* determino Riga/Colonna del cursore
028300940615     C                   MOVE      NRG           R$$
028400940615     C                   MOVE      NCL           C$$
028500940615     C                   Z-ADD     RIRI          CURR
028600940615     C                   Z-ADD     COCO          CURC
028700940615     C*
028800940615     C* FMTD1  - CAMPI DA TRADURRE
028900940615     C* imposto pos. del cursore
029000940615     C                   Z-ADD     CURR          H1RIGA
029100940615     C                   Z-ADD     CURC          H1COLO
029200940621     C                   MOVEL     H1NMFL        W005              5
029300940621     C                   MOVE      W005          W003              3
029400940621     C                   MOVEL     W003          W006              6
029500940620     C                   MOVE      *ZERO         KEY              15
029600940616     C                   MOVE      D1COD         KEY
029700940615     C                   SELECT
029800940621$014 C     W003          WHENEQ    'DES   '
029900940621$014 C                   Z-ADD     1             I                 3 0
030000940621$014 C     W006          LOOKUP    LIN(I)                                 21
030100940621$014 C                   CALL      'ANA101R'
030200940621$014 C                   PARM      W003          CAMPO            10
030300980924$014 C                   PARM      'YA2'         COD               3
030400940621$014 C                   PARM                    KEY              15
030500940621$014 C                   PARM      030           LUN               3 0
030600940621$014 C                   PARM      001           POS               3 0
030700980924$014 C     DESYA2        PARM      DESYA2        DES              50
030800940621$014 C                   PARM      LIND(I)       FLD              50
030900940621$014 C                   PARM      XTBAZI        AZI               3
031000940621$014 C                   PARM      LIN002        LNG               1
031100940621$014 C                   PARM      OPZ002        OPZ               2
031200940615     C*
031300940615     C* ricerca non abilitata in questa posizione
0314009406151O   C                   OTHER
031500940615     C                   SETON                                        98
031600940615     C*
0317009406151-   C                   ENDSL
031800940615     C*
031900940615     C                   MOVE      *HIVAL        $LASTV
032000940615     C                   ENDSR
032100940225     C/EJECT
032200940225     C************************************************************
032300940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
032400940225     C************************************************************
032500940225     C     GESPIE        BEGSR
032600940225     C*
032700940225     C                   SELECT
032800940225     C*
032900940506     C     $GEST         WHENEQ    'D1'
033000940506     C     $ULKD1        IFEQ      0
033100940506     C* riempimento opzioni
033200940506     C                   EXSR      F24D1
033300940506     C                   ELSE
033400940614     C                   WRITE     Z1
033500940506     C                   ENDIF
033600940225     C*
033700940510     C*
033800940225     C                   ENDSL
033900940225     C*
034000940225     C                   ENDSR
034100940131     C/EJECT
034200940131     C************************************************************
034300940510     C* GESTIONE VIDEO RECORD D1
034400940131     C************************************************************
034500940127     C     GESD1         BEGSR
034600940225     C* inizializzazione videata
034700940223     C     $INZD1        IFEQ      *ON
034800940127     C                   EXSR      INZD1
034900940223     C                   MOVE      *OFF          $INZD1
035000940117     C                   END
035100940225     C* emissione piede videata se proveniente da altra
035200940225     C* salvataggio valore formato di provenienza
035300940207     C     $LASTV        IFNE      'D1'
035400940614     C                   WRITE     T1
035500940225     C                   EXSR      GESPIE
035600940202     C                   MOVE      $LASTV        $LASTG
035700940202     C                   MOVE      'D1'          $LASTV
035800940202     C                   END
035900940131     C*
036000940203     C                   SELECT
036100940203     C* Annullamento
036200940315    >C     OPZ002        WHENEQ    '04'
036300940224     C* Visualizzazione
036400940315    >C     OPZ002        OREQ      '05'
036500940620     C                   SETON                                        0203
036600940620     C* Manutenzione
036700940620    >C     OPZ002        WHENEQ    '02'
036800940620     C                   SETON                                        02
036900940620     C                   SETOFF                                       03
037000940620     C*
037100940620     C                   OTHER
037200940620     C*
037300940620     C                   SETOFF                                       0203
037400940203     C                   ENDSL
037500940620     C* Immissione/Modifica/Copia
037600940620     C                   EXFMT     D1
037700940506     C* controllo abilitazione tasti funzione
037800940506     C*
037900940506     C                   EXSR      CTRKD1
038000940506     C*
038100940506     C     *IN99         IFEQ      *OFF
038200940131     C*
0383009402031    C                   SELECT
038400940510     c* ROLLUP
038500940131     C* F3=Fine
038600940506     C     $TASTO        WHENEQ    F03
038700940309     C                   EXSR      F03D1
038800940131     C* F4=Ricerca
038900940506     C     $TASTO        WHENEQ    F04
039000940127     C                   EXSR      SEARCH
039100940224     C* F5=Refresh
039200940506     C     $TASTO        WHENEQ    F05
039300940309     C                   EXSR      F05D1
039400940223     C* F8=Successivo
039500940506     C     $TASTO        WHENEQ    F08
039600940309     C                   EXSR      F08D1
039700940131     C* F12=Ritorno
039800940506     C     $TASTO        WHENEQ    F12
039900940309     C                   EXSR      F12D1
040000940315     C* F24=Altri tasti
040100940506     C     $TASTO        WHENEQ    F24
040200940315     C                   EXSR      F24D1
040300940131     C*
0404009402031O   C                   OTHER
040500940127     C                   EXSR      CTRD1
040600940616     C     *IN99         IFEQ      *OFF
040700940616     C                   SELECT
040800940616     C* F7=Traduzione
040900940616     C     $TASTO        WHENEQ    F07
041000940616     C                   EXSR      LINGUA
041100940616     C* F6
0412009406162    C     $TASTO        WHENEQ    F06
041300940224     C* eseguo aggiornamento
041400940315    >C                   EXSR      AGGANA
041500940224     C* eseguo operazioni del dopo-aggiornamento
0416009403183    C     *IN99         IFEQ      *OFF
041700940318     C                   EXSR      GESAGG
0418009403183-   C                   ENDIF
0419009406162-   C                   ENDSL
042000940616     C                   ENDIF
042100940203     C*
0422009402241-   C                   ENDSL
042300940224     C*
0424009405062-   C                   ENDIF
042500940117     C                   ENDSR
042600940506     C/EJECT
042700940506     C************************************************************
042800940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
042900940506     C************************************************************
043000940506     C     CTRKD1        BEGSR
043100940506     C                   SETOFF                                       99
043200940506     C                   MOVEA     $FC1          $FC
043300940506     C                   EXSR      CTRKEY
043400940506     C     *IN99         IFEQ      *ON
043500940506     C                   SETON                                        97
043600940506     C                   ENDIF
043700940506     C                   ENDSR
043800940117      ************************************************************
043900940207      * INIZIALIZZAZIONE VIDEATA DATI
044000940117      ************************************************************
044100940127     C     INZD1         BEGSR
044200940131     C*
044300940615     C                   CLEAR                   D1
044400941006     C                   MOVE      '0'           D1RIS
044500940616     C                   MOVE      KEY002        D1COD
044600980925     C                   MOVE      TPGYA2        D1TPG
044700980925     C                   MOVE      DESYA2        D1DES
044800940615     C                   MOVE      AZI002        XTBAZI
044900940615     C                   MOVE      LIN002        XTBLIN
045000940616     C                   MOVE      *ALL'0'       XTBKEY
045100940615     C                   MOVE      KEY002        XTBKEY
045200980924$007 C                   MOVE      'YA2'         XTBCOD
045300940207     C*
045400940224     C* valorizzo la chiave univoca per reperire i dati
045500940224     C* se non immissione
0456009403151   >C     OPZ002        IFNE      '01'
045700940315    >C*-------------------------------------------------------------
045800940315    >C* impostare i campi chiave ricevuti
045900940315    >C*-------------------------------------------------------------
046000940318     C*
046100940614     C*
046200940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0463009406142   >C     OPZ002        IFEQ      '02'
046400940315    >C     OPZ002        OREQ      '04'
046500940614     C                   MOVE      '1'           XTBALC
046600940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0467009406142E   C                   ELSE
046800940614     C                   MOVE      '0'           XTBALC
0469009406142-   C                   ENDIF
047000940614     C*
047100940616     C                   MOVE      '1'           XTBRIC
047200961004     C                   CALLb     'XATB'
047300940614     C                   PARM                    XATBDS
047400940318     C*
047500940614     C     XTBERR        IFEQ      '0'
047600940614     C                   MOVEL     XTBUNI        EXTDS
047700941108     C* Giro la data se non blank
047800980925     c
047900980925     C     XTBDSC        IFNE      *loval
048000980925     C     *dmy          MOVE      XTBDSC        D1DSC
048100940712     C                   MOVE      XTBRIS        D1RIS
048200941108     C                   ELSE
048300941108     C                   MOVE      *ZEROS        D1DSC
048400941108     C                   ENDIF
048500980925     C                   ENDIF
048600940224     C*
048700940224     C* se il rcd da gestire non viene trovato
048800940318     C* oppure è allocato da altro pgm
048900940224     C* ritorno al pgm guida avvertendolo dell'errore
0490009406142    C     XTBERR        IFNE      '0'
049100940224     C                   MOVE      *ON           $FINE
049200940315    >C                   MOVE      '1'           ERR002
049300940224     C*
049400940224     C* altrimenti
0495009402242E   C                   ELSE
049600940224     C* riempio la videata
049700940224     C                   EXSR      RIED1
049800940224     C* richiamo routine dei ctrl per decodificarla
049900940614    >C                   EXSR      CTRD1
050000940224     C                   MOVE      *ALL'0'       IN5098
050100940224     C                   MOVEA     IN5098        *IN(50)
050200940224     C* definizione indicatori su video
050300940224     C                   SETOFF                                       02
0504009402243    C                   SELECT
050500940315    >C     OPZ002        WHENEQ    '02'
050600940224     C                   SETON                                        02        PR campi key
050700940315    >C     OPZ002        WHENEQ    '05'
050800940506     C* disabilita tasti
050900940720     C                   MOVE      '0'           $FC(6)
051000940720     C                   MOVE      '0'           $FC(5)
051100940720     C                   MOVE      '0'           $FC(4)
0512009402243-   C                   ENDSL
051300940224     C*
0514009402242-   C                   ENDIF
051500940223     C*
0516009402241-   C                   ENDIF
051700940224     C*
051800940117     C                   ENDSR
051900940207     C/EJECT
052000940207     C************************************************************
052100940510     C* RIEMPIMENTO VIDEATA  D1
052200940207     C************************************************************
052300940207     C     RIED1         BEGSR
052400940207     C*
052500980925     c                   MOVE      DESYA2        d1des
052600980925     c                   MOVE      TPGYA2        d1tpg
052700940207     C*
052800940207     C                   ENDSR
052900940309     C/EJECT
053000940309     C************************************************************
053100940309     C* GESTIONE F03 VIDEO D1
053200940309     C************************************************************
053300940309     C     F03D1         BEGSR
053400940309     C*
053500940309     C                   MOVE      *ON           $FINE
053600940315    >C                   MOVE      '1'           RET002
053700940325     C* fine programma
053800940325     C                   EXSR      ENDPGM
053900940325     C*
054000940325     C                   ENDSR
054100940309     C/EJECT
054200940309     C************************************************************
054300940309     C* GESTIONE F05 VIDEO D1
054400940309     C************************************************************
054500940309     C     F05D1         BEGSR
054600940309     C*
054700940309     C                   MOVE      *ON           $INZD1
054800940309     C*
054900940309     C                   ENDSR
055000940309     C/EJECT
055100940309     C************************************************************
055200940309     C* GESTIONE F08 VIDEO D1
055300940309     C************************************************************
055400940309     C     F08D1         BEGSR
055500940309     C*
055600940309     C                   MOVE      *ON           $FINE
055700940315    >C                   MOVE      '0'           RET002
055800940309     C*
055900940309     C                   ENDSR
056000940309     C/EJECT
056100940309     C************************************************************
056200940309     C* GESTIONE F12 VIDEO D1
056300940309     C************************************************************
056400940309     C     F12D1         BEGSR
056500940309     C*
056600940309     C                   MOVE      *ON           $FINE
056700940315    >C                   MOVE      '2'           RET002
056800940309     C*
056900940309     C                   ENDSR
057000940309     C/EJECT
057100940309     C************************************************************
057200940309     C* GESTIONE F24 VIDEO D1
057300940309     C************************************************************
057400940309     C     F24D1         BEGSR
057500940309     C*
057600940506     C*
057700940506     C                   MOVEA     $FX1          $FX
057800940506     C                   MOVEA     $FC1          $FC
057900940506     C                   MOVEA     $FR1          $FR
058000940506     C                   Z-ADD     $ULKD1        $ULK
058100940506     C                   EXSR      RIEKEY
058200941017     C                   MOVEA     $FC           $FC1
058300940506     C                   Z-ADD     $ULK          $ULKD1            3 0
058400940506     C                   MOVEL     $KEY1         Z1KE1
058500940506     C                   MOVEL     $KEY2         Z1KE2
058600940614     C                   WRITE     Z1
058700940506     C*
058800940309     C                   ENDSR
058900940207     C/EJECT
059000940131     C************************************************************
059100940207     C* CONTROLLO VIDEATA
059200940131     C************************************************************
059300940127     C     CTRD1         BEGSR
059400940131     C*
059500940127     C                   SETOFF                                       99
059600940127     C* non imposto pos. del cursore
059700940127     C                   Z-ADD     0             H1RIGA
059800940127     C                   Z-ADD     0             H1COLO
059900980924     c*
060000980924     C* controllo codice vuoto
060100980924$017 C     D1COD         IFEQ      *ALL'0'
060200980924     C                   SETON                                        5099
060300980924     C                   ENDIF
060400940621     C*
060500940621     C* CTRL DATA
060600980925     c                   If        d1dsc <>0
060700980925     c     *dmy          test(d)                 d1dsc                  55
060800980925     c                   if        *in55=*on
060900980925     c                   seton                                        99
061000980925     c                   Endif
061100980925     c                   Endif
061200940223     C*
061300940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
061400940223     C*
0615009403151   >C     OPZ002        IFNE      '04'
061600980924     c*
061700980924     C* Controllo esistenza categoria contabile su tab. 0A2
061800980924     c                   IF        D1COD <> 0
061900980924     C                   MOVE      *ZERO         XTBKEY
062000980924     C                   MOVE      D1COD         XTBKEY                         *
062100980924     C                   MOVE      '1'           XTBRIC
062200980924     C                   MOVE      XSCSOC        XTBAZI
062300980924     C                   MOVE      '0'           XTBALC
062400980924     C                   MOVE      '0A2'         XTBCOD
062500980924     C                   CALL      'XATB'
062600980924     C                   PARM                    XATBDS
062700980924     C     XTBERR        IFEQ      *ON
062800980924     C                   SETON                                        6099
062900980925     c                   clear                   d1des
063000980924     C                   ELSE
063100980924     C                   MOVEL     XTBUNI        TAB0A2
063200980924     c                   MOVEL     DES0A2        D1DES
063300980924     C                   ENDIF
063400980924     C                   ENDIF
063500980924     c*
063600980924$009 C     d1tpg         IFEQ      *BLANK
063700980924$009 C                   SETON                                        6399
063800980922$009 C                   ENDIF
063900940207     C*
064000940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
064100940315    >C     OPZ002        IFEQ      '01'
064200940315    >C     OPZ002        OREQ      '03'
064300980924    >C*
064400940614     C                   MOVE      *ALL'0'       XTBKEY
064500940620    >C                   MOVE      D1COD         XTBKEY
064600940614     C                   MOVE      '0'           XTBALC
064700940614     C                   MOVE      '1'           XTBRIC
064800980924     C                   MOVE      'YA2'         XTBCOD
064900961004    >C                   CALLb     'XATB'
065000940614     C                   PARM                    XATBDS
065100940614     C     XTBERR        IFEQ      '0'
065200940223     C                   SETON                                        5999
065300940223     C                   ENDIF
065400940620     C* Controllo se compreso nei range indice
065500940620     C     XTBERR        IFEQ      '3'
065600940620    >C                   SETON                                        5899
065700940620     C                   END
065800940620     C*
065900940620     C* Non ammessa immissione di gruppo
066000940620     C     XTBGRU        IFEQ      'G'
066100940620     C     MANGRU        ANDEQ     'N'
066200940620     C                   SETON                                        5799
066300940620     C                   END
066400940620     C* Non ammessa immissione di azienda
066500940620     C     XTBGRU        IFEQ      'A'
066600940620     C     MANAZI        ANDEQ     'N'
066700940620     C                   SETON                                        5699
066800940620     C                   END
066900940712     C* codice riservato non accessibile
067000941006     C     XTBRIS        IFGT      ABIRIS
067100941005    >C     OPZ002        ANDNE     '01'
067200941005    >C     OPZ002        ANDNE     '03'
067300940712     C                   SETON                                        5499
067400940712     C                   END
067500940224     C                   ENDIF
067600940224     C*
0677009402241-   C                   END
067800940131     C*
067900940302     C     *IN99         IFEQ      *ON
068000940510     C     $GEST         ANDEQ     'D1'
068100940117     C                   MOVEA     *IN           WIN
068200940128     C                   MOVE      *ALL'0'       IN5098           49
068300940128     C                   MOVEA     IN5098        *IN(50)
068400940614     C                   WRITE     D1
068500940117     C                   MOVEA     WIN           *IN
068600940302     C                   ENDIF
068700940127     C*
068800940117     C                   ENDSR
068900940207     C/EJECT
069000940224     C************************************************************
069100940315    >C* AGGIORNAMENTO ANAGRAFICA
069200940224     C************************************************************
069300940315    >C     AGGANA        BEGSR
069400940614     C                   MOVE      *ALL'0'       XTBKEY
069500940614     C                   MOVE      D1COD         XTBKEY
069600940207     C*
069700940207     C                   SELECT
069800980925     c*
069900940315    >C     OPZ002        WHENEQ    '01'
070000940315    >C     OPZ002        OREQ      '03'
070100940315    >C                   EXSR      RIEANA
070200980928     C                   MOVE      'YA2'         XTBCOD
070300980928     C                   MOVE      XSCSOC        XTBAZI
070400940712     C                   MOVE      XSCCUT        XTBUTE
070500940614     C                   MOVE      '5'           XTBRIC
070600980928     C                   MOVE      '1'           XTBALC
070700961004     C                   CALLb     'XATB'
070800940614    >C                   PARM                    XATBDS
070900940318     C* se rcd non scrivibile attivo errore
071000940615     C     XTBERR        IFEQ      '1'
071100940318    >C                   SETON                                        5999
071200940318     C                   END
071300980925     c*
071400940315    >C     OPZ002        WHENEQ    '02'
071500980928     C                   MOVE      XSCSOC        XTBAZI
071600980928     C                   MOVE      'YA2'         XTBCOD
071700940621     C                   MOVE      '1'           XTBRIC
071800940621     C                   MOVE      '1'           XTBALC
071900961004     C                   CALLb     'XATB'
072000940621     C                   PARM                    XATBDS
072100940622    >C                   EXSR      RIEANA
072200940712     C                   MOVE      XSCCUT        XTBUTE
072300940614     C                   MOVE      '4'           XTBRIC
072400980928     C                   MOVE      XSCSOC        XTBAZI
072500980928     C                   MOVE      'YA2'         XTBCOD
072600961004     C                   CALLb     'XATB'
072700940614    >C                   PARM                    XATBDS
072800980925     c*
072900940315    >C     OPZ002        WHENEQ    '04'
073000980928     C                   MOVE      XSCSOC        XTBAZI
073100980928     C                   MOVE      'YA2'         XTBCOD
073200940621     C                   MOVE      '1'           XTBRIC
073300940621     C                   MOVE      '1'           XTBALC
073400961004     C                   CALLb     'XATB'
073500940621     C                   PARM                    XATBDS
073600940614     C                   MOVE      '6'           XTBRIC
073700980928     C                   MOVE      XSCSOC        XTBAZI
073800980928     C                   MOVE      'YA2'         XTBCOD
073900961004     C                   CALLb     'XATB'
074000940614    >C                   PARM                    XATBDS
074100940621     C* annullo traduzioni
074200940621     C     XTBERR        IFEQ      '0'
074300940621     C                   EXSR      ANNTBT
074400940621     C                   END
074500940621     C*
074600940207     C                   ENDSL
074700940203     C*
074800940203     C                   ENDSR
074900940224     C************************************************************
075000940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
075100940224     C************************************************************
075200940616     C     RIEANA        BEGSR
075300940616     C                   CLEAR                   XTBUNI
075400980925     c                   if        d1dsc<>0
075500980925     C     *dmy          MOVE      d1dsc         XTBDSC
075600980925     c                   endif
075700940712     C                   MOVE      D1RIS         XTBRIS
075800980924     C                   MOVE      D1des         desya2
075900980924     C                   MOVE      D1tpg         tpgya2
076000940616     C                   MOVEL     EXTDS         XTBUNI
076100940207     C                   ENDSR
076200940224     C/EJECT
076300940224     C************************************************************
076400940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
076500940224     C************************************************************
076600940224     C     GESAGG        BEGSR
076700940224     C*
076800940224     C* segnalo al pgm chiamante l'avvenuta conferma
076900940315    >C                   MOVE      *ON           OPR002
077000940224     C* segnalo al pgm chiamante l'aver premuto l'F6
077100940315    >C                   MOVE      '0'           RET002
077200940224     C*
0773009402241    C                   SELECT
077400940224     C* nel caso di immissione, ripristino la videata iniziale
077500940315    >C     OPZ002        WHENEQ    '01'
077600940224     C                   MOVE      *ON           $INZD1
077700940224     C* altrimenti ritorno al pgm chiamante
077800940224     C                   OTHER
077900940224     C                   MOVE      *ON           $FINE
0780009402241-   C                   ENDSL
078100940224     C*
078200940224     C                   ENDSR
078300940621     C/EJECT
078400940621     C************************************************************
078500940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
078600940621     C************************************************************
078700940621    >C     ANNTBT        BEGSR
078800940621     C                   MOVE      XTBAZI        TBTAZI
078900940621     C                   MOVE      XTBCOD        TBTCOD
079000940621     C                   MOVE      XTBKEY        TBTKEY
079100940621     C     KTBT          SETLL     ANTBT01L
079200940621     C                   DO        *HIVAL
079300940621     C     KTBT          DELETE    ANTBT01L                           21
079400940621     C  N21              END
079500940621     C*
079600940621     C                   ENDSR
079700940207     C/EJECT
079800940131     C************************************************************
079900940131     C* OPERAZIONI INIZIALI
080000940131     C************************************************************
080100940131     C     *INZSR        BEGSR
080200940131     C*
080300940127     C* Reperimento parametri
080400940127     C*
080500940117     C     *ENTRY        PLIST
080600940117     C                   PARM                    KPJBA
080700940203     C*
080800940712     C*
080900940223     C*
081000940506     C*
081100940506     C* Reperimento tasti di funzione
081200940506     C*
081300940506     C                   EXSR      REPKEY
081400940621     C*
081500940621     C* KLIST
081600940621     C*
081700940621     C     KTBT          KLIST
081800940621     C                   KFLD                    TBTAZI
081900940621     C                   KFLD                    TBTCOD
082000940621     C                   KFLD                    TBTKEY
082100980924     c*
082200940223     C                   ENDSR
082300941103     C/EJECT
082400941103      ************************************************************
082500941103      * Reperimento dati società
082600941103      ************************************************************
082700941103     C     REPSOC        BEGSR
082800941103      *
082900961004     C                   CALLb     'XSOC'
083000941103     C                   PARM                    TIPXSC            6
083100941103     C                   PARM                    SOCXSC            3
083200941103     C                   PARM                    CDSXSC            9 0
083300941103     C                   PARM                    MODXSC            3
083400941103     C                   PARM      *BLANKS       RTNXSC            1
083500941103     C                   PARM                    XSOCDS
083600941103     C                   PARM                    KPJBA
083700941103      *
083800941103     C                   ENDSR
083900940223     C/EJECT
084000940506     C************************************************************
084100940506     C* REPERIMENTO TASTI DI FUNZIONE
084200940506     C************************************************************
084300940506     C     REPKEY        BEGSR
084400940506     C                   MOVEA     $FM1          $FM
084500940506     C                   EXSR      REPKKK
084600940506     C                   MOVEA     $FX           $FX1
084700940614     C                   ENDSR
084800940506     C/EJECT
084900940506     C************************************************************
085000940506     C* REPERIMENTO TASTI DI FUNZIONE
085100940506     C************************************************************
085200940506     C     REPKKK        BEGSR
085300940506     C*
085400940506     C                   CLEAR                   $FX
085500940506     C     1             DO        24            X
085600940506     C*
085700940506     C     $FM(X)        IFNE      *BLANK
085800940506     C*
085900940506     C                   MOVEL     $FM(X)        IDMSG
086000961004     C                   CALLb     'XRTVM'
086100940506     C                   PARM                    IDMSG            27
086200940506     C                   PARM                    TXTMSG          100
086300940506     C                   PARM                    ERRMSG            1
086400940506     C*
086500940506     C     ERRMSG        IFNE      *ON
086600940506     C                   MOVEL     TXTMSG        $FX(X)
086700940506     C                   ELSE
086800940506     C                   MOVEL     *ALL' '       $FX(X)
086900940506     C                   END
087000940506     C*
087100940506     C                   END
087200940506     C*
087300940506     C                   END
087400940506     C*
087500940506     C                   ENDSR
087600940509     C/EJECT
087700940223     C************************************************************
087800940223     C* INIZIALIZZAZIONE VARIABILI
087900940223     C************************************************************
088000940223     C     INZVAR        BEGSR
088100940223     C* Reperimento codice lingua e codice azienda
088200940223     C*
088300940223     C                   MOVEL     'SOC001'      TIPXSC
088400940223     C                   EXSR      REPSOC
088500940223     C     RTNXSC        IFNE      '1'
088600940223     C                   MOVEL     XSOCDS        SOC001
088700940223     C                   ELSE
088800940223     C                   MOVE      *ON           $FINE
088900940223     C* fine programma
089000940223     C                   EXSR      ENDPGM
089100940223     C                   ENDIF
089200940224     C*
089300940224     C* Dati da PGM guida
089400940224     C*
089500940315    >C                   MOVEL     KPJBU         D002DS
089600940128     C*
089700940223     C* Variabili per gestione videate
089800940128     C*
089900940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
090000940223     C                   MOVE      *OFF          $FINE                          fine pgm
090100940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
090200940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
090300940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
090400940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
090500940127     C*
090600940223     C* Variabili appoggio
090700940127     C*
090800940224     C                   Z-ADD     0             CURR
090900940224     C                   Z-ADD     0             CURC
091000940207     C*
091100940207     C* Valorizzazione campi univoci testate
091200940207     C*
091300940614     C                   CLEAR                   T1
091400940614     C                   MOVEL     KNSIF         NOMSIF
091500941109     C                   MOVEL     XSCDSI        NOMDIT
091600941027     C                   MOVEL     DSPGM         NOMPGM
091700940224     C                   Z-ADD     1             X
091800940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
091900940224     C     *IN21         IFEQ      *ON
092000940224     C                   MOVEL     $ID(X)        IDMSG
092100961004     C                   CALLb     'XRTVM'
092200940224     C                   PARM                    IDMSG            27
092300940224     C                   PARM                    TXTMSG          100
092400940224     C                   PARM                    ERRMSG            1
092500940224     C                   PARM      *ON           CTRMSG            1            centratura
092600940224     C                   PARM      30            LENMSG            3 0          lun output.
092700940224     C     ERRMSG        IFNE      *ON
092800940224     C                   MOVEL     TXTMSG        T1OPE
092900940224     C                   ELSE
093000940224     C                   MOVEL     *ALL'?'       T1OPE
093100940224     C                   ENDIF
093200940224     C                   END
093300940224     C*
093400940224     C* Inizializzazione/Decodifica prima videata
093500940224     C*
093600940224     C                   EXSR      INZD1
093700940224     C                   MOVE      *OFF          $INZD1
093800940127     C*
093900940117     C                   ENDSR
094000940128     C************************************************************
094100940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
094200940223     C************************************************************
094300940223     C     DEFCAM        BEGSR
094400940223     C*
094500940223     C* klist
094600940207     C*
094700940223     C*
094800940223     C* Variabili per gestione videate
094900940223     C*
095000940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
095100940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
095200940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
095300940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
095400940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
095500940223     C*
095600940223     C* Variabili appoggio
095700940223     C*
095800940223     C                   Z-ADD     0             CURR              2 0
095900940223     C                   Z-ADD     0             CURC              2 0
096000940223     C                   MOVE      *ZEROS        WIN              99
096100940224     C                   MOVE      *BLANK        WTBERR            1
096200940223     C*
096300940223     C* Indici
096400940223     C*
096500940223     C                   Z-ADD     0             X                 3 0
096600940223     C                   Z-ADD     0             Y                 3 0
096700940127     C*
096800940117     C                   ENDSR
096900940128     C************************************************************
097000940509     C/EJECT
097100940509     C************************************************************
097200940509     C* CONTROLLO ABILITAZIONE TASTI
097300940509     C************************************************************
097400940509     C     CTRKEY        BEGSR
097500940509     C                   SELECT
097600940509     C     $TASTO        WHENEQ    F01
097700940720     C     $FC(1)        IFEQ      '0'
097800940509     C                   SETON                                        99
097900940509     C                   ENDIF
098000940509     C     $TASTO        WHENEQ    F02
098100940720     C     $FC(2)        IFEQ      '0'
098200940509     C                   SETON                                        99
098300940509     C                   ENDIF
098400940509     C     $TASTO        WHENEQ    F03
098500940720     C     $FC(3)        IFEQ      '0'
098600940509     C                   SETON                                        99
098700940509     C                   ENDIF
098800940509     C     $TASTO        WHENEQ    F04
098900940720     C     $FC(4)        IFEQ      '0'
099000940509     C                   SETON                                        99
099100940509     C                   ENDIF
099200940509     C     $TASTO        WHENEQ    F05
099300940720     C     $FC(5)        IFEQ      '0'
099400940509     C                   SETON                                        99
099500940509     C                   ENDIF
099600940509     C     $TASTO        WHENEQ    F06
099700940720     C     $FC(6)        IFEQ      '0'
099800940509     C                   SETON                                        99
099900940509     C                   ENDIF
100000940509     C     $TASTO        WHENEQ    F07
100100940720     C     $FC(7)        IFEQ      '0'
100200940509     C                   SETON                                        99
100300940509     C                   ENDIF
100400940509     C     $TASTO        WHENEQ    F08
100500940720     C     $FC(8)        IFEQ      '0'
100600940509     C                   SETON                                        99
100700940509     C                   ENDIF
100800940509     C     $TASTO        WHENEQ    F09
100900940720     C     $FC(09)       IFEQ      '0'
101000940509     C                   SETON                                        99
101100940509     C                   ENDIF
101200940509     C     $TASTO        WHENEQ    F10
101300940720     C     $FC(10)       IFEQ      '0'
101400940509     C                   SETON                                        99
101500940509     C                   ENDIF
101600940509     C     $TASTO        WHENEQ    F11
101700940720     C     $FC(11)       IFEQ      '0'
101800940509     C                   SETON                                        99
101900940509     C                   ENDIF
102000940509     C     $TASTO        WHENEQ    F12
102100940720     C     $FC(12)       IFEQ      '0'
102200940509     C                   SETON                                        99
102300940509     C                   ENDIF
102400940509     C     $TASTO        WHENEQ    F13
102500940720     C     $FC(13)       IFEQ      '0'
102600940509     C                   SETON                                        99
102700940509     C                   ENDIF
102800940509     C     $TASTO        WHENEQ    F14
102900940720     C     $FC(14)       IFEQ      '0'
103000940509     C                   SETON                                        99
103100940509     C                   ENDIF
103200940509     C     $TASTO        WHENEQ    F15
103300940720     C     $FC(15)       IFEQ      '0'
103400940509     C                   SETON                                        99
103500940509     C                   ENDIF
103600940509     C     $TASTO        WHENEQ    F16
103700940720     C     $FC(16)       IFEQ      '0'
103800940509     C                   SETON                                        99
103900940509     C                   ENDIF
104000940509     C     $TASTO        WHENEQ    F17
104100940720     C     $FC(17)       IFEQ      '0'
104200940509     C                   SETON                                        99
104300940509     C                   ENDIF
104400940509     C     $TASTO        WHENEQ    F18
104500940720     C     $FC(18)       IFEQ      '0'
104600940509     C                   SETON                                        99
104700940509     C                   ENDIF
104800940509     C     $TASTO        WHENEQ    F19
104900940720     C     $FC(19)       IFEQ      '0'
105000940509     C                   SETON                                        99
105100940509     C                   ENDIF
105200940509     C     $TASTO        WHENEQ    F20
105300940720     C     $FC(20)       IFEQ      '0'
105400940509     C                   SETON                                        99
105500940509     C                   ENDIF
105600940509     C     $TASTO        WHENEQ    F21
105700940720     C     $FC(21)       IFEQ      '0'
105800940509     C                   SETON                                        99
105900940509     C                   ENDIF
106000940509     C     $TASTO        WHENEQ    F22
106100940720     C     $FC(22)       IFEQ      '0'
106200940509     C                   SETON                                        99
106300940509     C                   ENDIF
106400940509     C     $TASTO        WHENEQ    F23
106500940720     C     $FC(23)       IFEQ      '0'
106600940509     C                   SETON                                        99
106700940509     C                   ENDIF
106800940509     C     $TASTO        WHENEQ    F24
106900940720     C     $FC(24)       IFEQ      '0'
107000940509     C                   SETON                                        99
107100940509     C                   ENDIF
107200940509     C                   ENDSL
107300940509     C                   ENDSR
107400940509     C/EJECT
107500940509     C************************************************************
107600940509     C* RIEMPIMENTO TASTI FUNZIONE
107700940509     C************************************************************
107800940509     C     RIEKEY        BEGSR
107900940509     C*
108000940509     C* Input :
108100940509     C* - $FX = schiera con tutti i testi disponibili
108200940509     C* - $FC = schiere con i flag di validità associati ai testi
108300940509     C* - $FR = schiere con i flag di REVERSE IMAGE
108400940509     C* - $ULK = ultimo testo caricato
108500940509     C* Output :
108600940509     C* - $ULK
108700940509     c* - $KEY1= 1° riga di tasti funzione
108800940509     c* - $KEY2= 2° riga di tasti funzione
108900940509     C*
109000961004     C                   CALLb     'XKEYMSG'
109100940509     C* input
109200940509     C                   PARM                    $FX
109300940509     C                   PARM                    $FC
109400940509     C                   PARM                    $FR
109500940509     C                   PARM                    $ULK              3 0
109600940509     C* output
109700940509     C                   PARM                    $KEY1            79
109800940509     C                   PARM                    $KEY2            79
109900940509     C*
110000940509     C                   ENDSR
110100940615**
11020094022401PROMSG    *LIBL     COS0001    immissione
11030094022402PROMSG    *LIBL     COS0002    modifica
11040094022403PROMSG    *LIBL     COS0003    copia
11050094022404PROMSG    *LIBL     COS0004    annullamento
11060094022405PROMSG    *LIBL     COS0005    visualizzazione
110700940509** TASTI DI FUNZIONE  D1
110800940509PROMSG    *LIBL     KEY0001  01
110900940509PROMSG    *LIBL     KEY0002  02
111000940509PROMSG    *LIBL     KEY0003  03
111100940509PROMSG    *LIBL     KEY0004  04
111200940509PROMSG    *LIBL     KEY0005  05
111300940509PROMSG    *LIBL     KEY0006  06
111400940509PROMSG    *LIBL     KEY0007  07
111500940509PROMSG    *LIBL     KEY0008  08
111600940509PROMSG    *LIBL     KEY0009  09
111700940509PROMSG    *LIBL     KEY0010  10
111800940509PROMSG    *LIBL     KEY0011  11
111900940509PROMSG    *LIBL     KEY0012  12
112000940509PROMSG    *LIBL     KEY0013  13
112100940509PROMSG    *LIBL     KEY0014  14
112200940509PROMSG    *LIBL     KEY0015  15
112300940509PROMSG    *LIBL     KEY0016  16
112400940509PROMSG    *LIBL     KEY0017  17
112500940509PROMSG    *LIBL     KEY0018  18
112600940509PROMSG    *LIBL     KEY0019  19
112700940509PROMSG    *LIBL     KEY0020  20
112800940509PROMSG    *LIBL     KEY0021  21
112900940509PROMSG    *LIBL     KEY0022  22
113000940509PROMSG    *LIBL     KEY0023  23
113100940509PROMSG    *LIBL     KEY0024  24
113200940509**  ABILITAZIONE D1
1133009407200N  01
1134009407200N  02
1135009407201N  03
1136009407201N  04
1137009407201N  05
1138009407201N  06
1139009407201N  07
1140009407201N  08
1141009407200N  09
1142009407200N  10
1143009407200N  11
1144009407201N  12
1145009407200N  13
1146009407200N  14
1147009407200N  15
1148009407200N  16
1149009407200N  17
1150009407200N  18
1151009407200N  19
1152009407200N  20
1153009407200N  21
1154009407200N  22
1155009407200N  23
1156009407200N  24
115700940615**
115800940615DES   Descrizione ................
