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
004100940614$001 FANGY4MD2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
004400980722     Fanope01l  IF   E           K DISK
004500980722     Fanriv01l  IF   E           K DISK
004600980917     FAnRco99J  IF   E           K DISK
004700940128     D*----------------------------------------------------*
004800940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004900940224     D $ID             S             27    DIM(5) ALT($OZ)
005000940506     D* Schiere per la gestione dei tasti di funzione
005100940506     D $FM             S             27    DIM(24)
005200940506     D $FX             S             50    DIM(24)
005300940506     D $FC             S              1    DIM(24)
005400940506     D $FR             S              1    DIM(24)
005500940506     D* Video D1
005600940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005700940506     D $FX1            S             50    DIM(24) ALT($FM1)
005800940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005900940506     D $FR1            S              1    DIM(24) ALT($FC1)
006000940615     D* campi in lingua
006100940622$010 D*LIN             S             06    DIM(0011) CTDATA PERRCD(001)
006200940622$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006300940614$003 D EXTDS         E DS                  EXTNAME(ANGY4MDS) INZ
006400940127     D* Passaggio Parametri
006500940127     D KPJBA         E DS
006600941103     D*-------------
006700941103      * DS Società
006800941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
006900960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
007000960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
007100980917     D*-------------
007200980917     D ana200ds      E DS                  INZ
007300980917     D ana250ds      E DS                  INZ
007400941103     D*-------------
007500941103      * DS Interna per dati utente
007600941103     D XSOCDS          DS          1000
007700941103     D*-------------
007800940616     D XATBDS        E DS                  INZ
007900940318     D*-------------
008000940318     D* DS Esterna gestione messaggi per COMIT
008100940616     D XMSGDS        E DS                  INZ
008200940318     D  STS          E                     EXTFLD(MSGSTS)
008300940318     D                                     DIM(333)
008400940318     D  JBA          E                     EXTFLD(MSGJBA)
008500940318     D                                     DIM(502)
008600940203     D*-------------
008700940620     D* DS Esterna autorizzazioni tabelle
008800940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
008900940620     D*
009000940620     D* DS  restituzione autorizzazioni
009100940620     D* IND=indice
009200940620     D* GRU=man. di gruppo
009300940620     D* AZI=man. azienda
009400940712     D* ABI=abilit. ai riservati
009500941111     D AUTDS           DS
009600940620     D  MANIND                 1      1
009700940620     D  MANGRU                 2      2
009800940620     D  MANAZI                 3      3
009900940712     D  ABIRIS                 4      4
010000940620     D*-------------
010100940207     D* opzione richiesta
010200940614    >D D002DS          DS
010300940614     D  OPZ002                 1      2
010400940614     D* 0 = ritorno normale
010500940614     D* 1 = F3
010600940614     D* 2 = F12
010700940614     D  RET002                 3      3
010800940614     D* operazione eseguite dal pgm chiamato :
010900940614     D* 0 = nessuna operazione
011000940614     D* 1 = eseguito aggiornamento
011100940614     D  OPR002                 4      4
011200940614     D* funzione non eseguibile per errore :
011300940614     D* 0 = nessun errore
011400940614     D* 1 = funzione richiamata chiusa in errore
011500940614     D* Aggiungere eventuali altri codici di errore
011600940614     D  ERR002                 5      5
011700940614     D* Azienda
011800940614     D  AZI002                 6      8
011900940614     D* Lingua
012000940614     D  LIN002                 9      9
012100940614     D* Codice scelto
012200940614     D  KEY002                10     24
012300940620     D* Autorizzazione
012400940620     D  AUT002                25     25
012500970509     D* Unita
012600970509     D  UNI002                26     33
012700940614     D*-------------
012800940201     D*-------------
012900940201     D DSFMT           DS
013000940506     D  $TASTO               369    369
013100940201     D  NRG                  370    370
013200940201     D  NCL                  371    371
013300940201     D*-------------
013400940201     D* posizione cursore
013500940201     D CURSOR          DS
013600940223     D  RIRI                   1      2B 0 INZ
013700940201     D  R$$                    2      2
013800940223     D  COCO                   3      4B 0 INZ
013900940201     D  C$$                    4      4
014000940207     D*-------------
014100940207     D* Reperimento nome PGM
014200940207     D STATUS         SDS           333
014300940207     D  DSPGM            *PROC
014400940225     D*-------------
014500940225     D* COSTANTI
014600940225     D*-------------
014700940506     D*
014800940506     D* Tasti di funzione
014900940506     D F01             C                   CONST(X'31')
015000940506     D F02             C                   CONST(X'32')
015100940506     D F03             C                   CONST(X'33')
015200940506     D F04             C                   CONST(X'34')
015300940506     D F05             C                   CONST(X'35')
015400940506     D F06             C                   CONST(X'36')
015500940506     D F07             C                   CONST(X'37')
015600940506     D F08             C                   CONST(X'38')
015700940506     D F09             C                   CONST(X'39')
015800940506     D F10             C                   CONST(X'3A')
015900940506     D F11             C                   CONST(X'3B')
016000940506     D F12             C                   CONST(X'3C')
016100940506     D F13             C                   CONST(X'B1')
016200940506     D F14             C                   CONST(X'B2')
016300940506     D F15             C                   CONST(X'B3')
016400940506     D F16             C                   CONST(X'B4')
016500940506     D F17             C                   CONST(X'B5')
016600940506     D F18             C                   CONST(X'B6')
016700940506     D F19             C                   CONST(X'B7')
016800940506     D F20             C                   CONST(X'B8')
016900940506     D F21             C                   CONST(X'B9')
017000940506     D F22             C                   CONST(X'BA')
017100940506     D F23             C                   CONST(X'BB')
017200940506     D F24             C                   CONST(X'BC')
017300940506     D ENTER           C                   CONST(X'F1')
017400940506     D ROLDWN          C                   CONST(X'F4')
017500940506     D ROLLUP          C                   CONST(X'F5')
017600940506     D*-------------
017700940506     D*
017800941108     D DATA            C                   CONST('0001-01-01')
017900980722     D*-------------
018000980722     D a070ds        E DS                  extname(ana070ds) inz
018100980722     D a068ds        E DS                  extname(ana068ds) inz
018200980722     D*-------------
018300980722     D WLBDA8          DS
018400980722     D  G02DAT                 1      8S 0
018500980722     D  G02INV                 9     16S 0
018600980722     D  G02ERR                17     17
018700980722     D  G02TGI                18     22S 0
018800940128     I*----------------------------------------------------*
018900940615     ID1
019000980722$005 I              D1CD1                       §4mCD1
019100070411$005 I*             D1CD2                       §4mCD2
019200980722$005 I              D1CD3                       §4mCD3
019300070411$005 I*             D1IVA                       §4mIVA
019400980722$005 I              D1ESE                       §4mESE
019500980722$005 I              D1GGP                       §4mGGP
019600980917$005 I              D1KCr                       §4mKCr
019700980917$005 I              D1KSr                       §4mKSr
019800940127     I*
019900940127     C*----------------------------------------------------*
020000940127     C*                MAIN LINE PROGRAM
020100940127     C*----------------------------------------------------*
020200940127     C*
020300940223     C* inizializzazione variabili
020400940223     C                   EXSR      INZVAR
020500940223     C*
020600940223     C     $FINE         DOWEQ     *OFF
020700940202     C     $GEST         CASEQ     'D1'          GESD1
020800940117     C                   END
020900940117     C                   END
021000940201     C*
021100940325     C* fine programma
021200940325     C                   EXSR      ENDPGM
021300940325     C/EJECT
021400940325     C************************************************************
021500940325     C* RICERCHE
021600940325     C************************************************************
021700940325     C     ENDPGM        BEGSR
021800940325     C*
021900940614     C* disallocazione rcd nel caso di
022000971110$007 C                   MOVE      'Y4M'         XTBCOD
022100940614    >C                   MOVE      '7'           XTBRIC
022200970214     C                   CALLB     'XATB'
022300940614     C                   PARM                    XATBDS
022400940325     C*
022500940325     C* passaggio dati al pgm chiamante
022600940715     C                   MOVE      *BLANKS       KPJBU
022700940325    >C                   MOVEL     D002DS        KPJBU
022800940325     C*
022900940325     C                   SETON                                            RT
023000940325     C*
023100940325     C                   ENDSR
023200940325     C/EJECT
023300940201     C************************************************************
023400940201     C* RICERCHE
023500940201     C************************************************************
023600940127     C     SEARCH        BEGSR
023700940127     C*
023800940127     C* determino Riga/Colonna del cursore
023900940127     C                   MOVE      NRG           R$$
024000940127     C                   MOVE      NCL           C$$
024100940127     C                   Z-ADD     RIRI          CURR
024200940127     C                   Z-ADD     COCO          CURC
024300940127     C*
024400940224     C* I campi che al momento dell'F4 risultano protetti non devono
024500940224     C* essere abilitati alla funzione di ricerca
024600940224     C*
0247009401271    C                   SELECT
024800940223     C*
024900980722    >C*  FMTD1  - CAUSALE 1
025000980722     C     H1NMFL        WHENEQ    'D1CD1'
025100980722     C                   RESET                   A070DS
025200980722     C                   MOVEL     '01'          OPZ070
025300980722     C                   MOVEL     A070DS        KPJBU
025400980722     C                   CALL      'ANA070R'
025500980722     C                   PARM                    KPJBA
025600980722     C                   MOVEL     KPJBU         A070DS
025700980722     C                   MOVEL     CAU070        WWCAUSC1          4
025800980722     C*
025900980722     C     WWCAUSC1      IFNE      *BLANK
026000980722     C                   MOVEL     WWCAUSC1      §4mcd1
026100980722     C                   movel     DE1070        D1DCD1
026200980722     C                   ENDIF
026300980722     C* imposto pos. del cursore
026400980722     C                   Z-ADD     CURR          H1RIGA
026500980722     C                   Z-ADD     CURC          H1COLO
026600980722     C*
026700980722    >C*  FMTD1  - CAUSALE 2
026800980722     C     H1NMFL        WHENEQ    'D1CD2'
026900980722     C                   RESET                   A070DS
027000980722     C                   MOVEL     '01'          OPZ070
027100980722     C                   MOVEL     A070DS        KPJBU
027200980722     C                   CALL      'ANA070R'
027300980722     C                   PARM                    KPJBA
027400980722     C                   MOVEL     KPJBU         A070DS
027500980722     C                   MOVEL     CAU070        WWCAUSC1          4
027600980722     C*
027700980722     C     WWCAUSC1      IFNE      *BLANK
027800980722     C                   MOVEL     WWCAUSC1      §4mcd2
027900070411     C*                  movel     DE1070        D1DCD2
028000980722     C                   ENDIF
028100980722     C*
028200980722     C* imposto pos. del cursore
028300980722     C                   Z-ADD     CURR          H1RIGA
028400980722     C                   Z-ADD     CURC          H1COLO
028500980722     C*
028600980722     C* CAUSALE 3
028700980722     C     H1NMFL        WHENEQ    'D1CD3'
028800980722     C                   RESET                   A070DS
028900980722     C                   MOVEL     '01'          OPZ070
029000980722     C                   MOVEL     A070DS        KPJBU
029100980722     C                   CALL      'ANA070R'
029200980722     C                   PARM                    KPJBA
029300980722     C                   MOVEL     KPJBU         A070DS
029400980722     C                   MOVEL     CAU070        WWCAUSC1          4
029500980722     C*
029600980722     C     WWCAUSC1      IFNE      *BLANK
029700980722     C                   MOVEL     WWCAUSC1      §4mcd3
029800980722     C                   movel     DE1070        D1DCD3
029900980722     C                   ENDIF
030000980722     C*
030100980722     C* FMTS1  - RIFERIMENTI E ALIQUOTE
030200980722     C     H1NMFL        WHENEQ    'D1IVA'
030300980722     C     H1NMFL        OREQ      'D1ESE'
030400980722     C                   RESET                   A068ds
030500980722     C                   MOVE      '01'          OPZ068
030600980722     C                   MOVE      XscSoc        SOC068
030700980722     C                   MOVE      '2'           TPR068
030800980722     C                   MOVE      '997'         LIB068
030900980722     C                   MOVE      *ZEROS        ALI068
031000980722     C                   MOVE      *BLANK        RIF068
031100980722     C                   MOVEL     A068ds        KPJBU
031200980722     C                   CALL      'ANA068R'
031300980722     C                   PARM                    KPJBA
031400980722     C                   MOVEL     KPJBU         A068ds
031500980722     C*
031600980722     C                   IF        Ali068 <> 0        or
031700980722     C                             Rif068 <> *blank
031800980722     C                   MOVE      ALI068        §4MIVA
031900980722     C                   MOVE      RIF068        §4MESE
032000980722     C                   end
032100980917     C* FMTD1  - capoconto
032200980917    >C     H1NMFL        WHEneq    'D1KCR'
032300980917     C                   RESET                   Ana200DS
032400980917     C                   MOVEL     '01'          OPZ200
032500980917     C                   clear                   KCC200
032600980917     C                   MOVEL     Ana200DS      KPJBU
032700980917     C                   CALL      'ANA200R'
032800980917     C                   PARM                    KPJBA
032900980917     C                   MOVEL     KPJBU         Ana200DS
033000980917     C     KCC200        IFNE      *BLANK
033100980917     C                   MOVEL     KCC200        §4mkcr
033200980917     C                   ENDIF
033300980917     C* imposto pos. del cursore
033400980917     C                   Z-ADD     CURR          H1RIGA
033500980917     C                   Z-ADD     CURC          H1COLO
033600980917     C*
033700980917     C* Ricerca sottoconti (ANA250R)
033800980917     C     H1NMFL        WHENEQ    'D1KSR'
033900980917     C                   RESET                   ana250DS
034000980917     C                   MOVEL     '01'          OPZ250
034100980917     C                   MOVEL     §4mkcr        KCC250
034200980917     C                   clear                   KSC250
034300980917     C                   MOVEL     ana250DS      KPJBU
034400980917     C                   CALL      'ANA250R'
034500980917     C                   PARM                    KPJBA
034600980917     C                   MOVEL     KPJBU         ana250DS
034700980917     C     KSC250        IFNE      *BLANK
034800980917     C                   MOVEL     KCC250        §4mkcr
034900980917     C                   MOVEL     KSC250        §4mksr
035000980917     C                   ENDIF
035100980917     C* imposto pos. del cursore
035200980917     C                   Z-ADD     CURR          H1RIGA
035300980917     C                   Z-ADD     CURC          H1COLO
035400980722     C*
035500980722     C* ricerca non abilitata in questa posizione
0356009807221O   C                   OTHER
035700980722     C                   SETON                                        9899
035800980722     C* imposto pos. del cursore
035900980722     C                   Z-ADD     CURR          H1RIGA
036000980722     C                   Z-ADD     CURC          H1COLO
036100940127     C*
0362009401271-   C                   ENDSL
036300940127     C*
036400940506     C                   MOVE      *HIVAL        $LASTV
036500940127     C                   ENDSR
036600940615     C/EJECT
036700940615     C************************************************************
036800940615     C* TRADUZIONI
036900940615     C************************************************************
037000940615     C     LINGUA        BEGSR
037100940615     C*
037200940615     C* determino Riga/Colonna del cursore
037300940615     C                   MOVE      NRG           R$$
037400940615     C                   MOVE      NCL           C$$
037500940615     C                   Z-ADD     RIRI          CURR
037600940615     C                   Z-ADD     COCO          CURC
037700940615     C*
037800940615     C*
037900940615     C*
038000940615     C* FMTD1  - CAMPI DA TRADURRE
038100940615     C* imposto pos. del cursore
038200940615     C                   Z-ADD     CURR          H1RIGA
038300940615     C                   Z-ADD     CURC          H1COLO
038400940621     C                   MOVEL     H1NMFL        W005              5
038500940621     C                   MOVE      W005          W003              3
038600940621     C                   MOVEL     W003          W006              6
038700980731     C                   MOVEl     *blanks       KEY              15
038800980731     C                   MOVEl     D1COD         KEY
038900940615     C                   SELECT
039000940615     C*
039100940615     C*
039200940615     C* ricerca non abilitata in questa posizione
0393009406151O   C                   OTHER
039400940615     C                   SETON                                        98
039500940615     C*
0396009406151-   C                   ENDSL
039700940615     C*
039800940615     C                   MOVE      *HIVAL        $LASTV
039900940615     C                   ENDSR
040000940225     C/EJECT
040100940225     C************************************************************
040200940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
040300940225     C************************************************************
040400940225     C     GESPIE        BEGSR
040500940225     C*
040600940225     C                   SELECT
040700940225     C*
040800940506     C     $GEST         WHENEQ    'D1'
040900940506     C     $ULKD1        IFEQ      0
041000940506     C* riempimento opzioni
041100940506     C                   EXSR      F24D1
041200940506     C                   ELSE
041300940614     C                   WRITE     Z1
041400940506     C                   ENDIF
041500940225     C*
041600940510     C*
041700940225     C                   ENDSL
041800940225     C*
041900940225     C                   ENDSR
042000940131     C/EJECT
042100940131     C************************************************************
042200940510     C* GESTIONE VIDEO RECORD D1
042300940131     C************************************************************
042400940127     C     GESD1         BEGSR
042500940225     C* inizializzazione videata
042600940223     C     $INZD1        IFEQ      *ON
042700940127     C                   EXSR      INZD1
042800940223     C                   MOVE      *OFF          $INZD1
042900940117     C                   END
043000940225     C* emissione piede videata se proveniente da altra
043100940225     C* salvataggio valore formato di provenienza
043200940207     C     $LASTV        IFNE      'D1'
043300940614     C                   WRITE     T1
043400940225     C                   EXSR      GESPIE
043500940202     C                   MOVE      $LASTV        $LASTG
043600940202     C                   MOVE      'D1'          $LASTV
043700940202     C                   END
043800940131     C*
043900940203     C                   SELECT
044000940203     C* Annullamento
044100980722    >C     OPZ002        WHENEQ    '04'
044200940224     C* Visualizzazione
044300940315    >C     OPZ002        OREQ      '05'
044400940620     C                   SETON                                        0203
044500940620     C* Manutenzione
044600940620    >C     OPZ002        WHENEQ    '02'
044700940620     C                   SETON                                        02
044800940620     C                   SETOFF                                       03
044900940620     C*
045000940620     C                   OTHER
045100940620     C*
045200940620     C                   SETOFF                                       0203
045300940203     C                   ENDSL
045400940620     C* Immissione/Modifica/Copia
045500940620     C                   EXFMT     D1
045600940506     C* controllo abilitazione tasti funzione
045700940506     C*
045800940506     C                   EXSR      CTRKD1
045900940506     C*
046000940506     C     *IN99         IFEQ      *OFF
046100940131     C*
0462009402031    C                   SELECT
046300940510     c* ROLLUP
046400940131     C* F3=Fine
046500940506     C     $TASTO        WHENEQ    F03
046600940309     C                   EXSR      F03D1
046700940131     C* F4=Ricerca
046800940506     C     $TASTO        WHENEQ    F04
046900940127     C                   EXSR      SEARCH
047000940224     C* F5=Refresh
047100940506     C     $TASTO        WHENEQ    F05
047200940309     C                   EXSR      F05D1
047300940223     C* F8=Successivo
047400940506     C     $TASTO        WHENEQ    F08
047500940309     C                   EXSR      F08D1
047600940131     C* F12=Ritorno
047700940506     C     $TASTO        WHENEQ    F12
047800940309     C                   EXSR      F12D1
047900940315     C* F24=Altri tasti
048000940506     C     $TASTO        WHENEQ    F24
048100940315     C                   EXSR      F24D1
048200940131     C*
0483009402031O   C                   OTHER
048400940127     C                   EXSR      CTRD1
048500940616     C     *IN99         IFEQ      *OFF
048600940616     C                   SELECT
048700940616     C* F7=Traduzione
048800940616     C     $TASTO        WHENEQ    F07
048900940616     C                   EXSR      LINGUA
049000940616     C* F6
0491009406162    C     $TASTO        WHENEQ    F06
049200940224     C* eseguo aggiornamento
049300940315    >C                   EXSR      AGGANA
049400940224     C* eseguo operazioni del dopo-aggiornamento
0495009403183    C     *IN99         IFEQ      *OFF
049600940318     C                   EXSR      GESAGG
0497009403183-   C                   ENDIF
0498009406162-   C                   ENDSL
049900940616     C                   ENDIF
050000940203     C*
0501009402241-   C                   ENDSL
050200940224     C*
0503009405062-   C                   ENDIF
050400940117     C                   ENDSR
050500940506     C/EJECT
050600940506     C************************************************************
050700940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
050800940506     C************************************************************
050900940506     C     CTRKD1        BEGSR
051000940506     C                   SETOFF                                       99
051100940506     C                   MOVEA     $FC1          $FC
051200940506     C                   EXSR      CTRKEY
051300940506     C     *IN99         IFEQ      *ON
051400940506     C                   SETON                                        97
051500940506     C                   ENDIF
051600940506     C                   ENDSR
051700940117      ************************************************************
051800940207      * INIZIALIZZAZIONE VIDEATA DATI
051900940117      ************************************************************
052000940127     C     INZD1         BEGSR
052100940131     C*
052200940615     C                   CLEAR                   D1
052300941006     C                   MOVE      '0'           D1RIS
052400980731     C                   MOVEl     KEY002        D1COD
052500940615     C                   MOVE      AZI002        XTBAZI
052600940615     C                   MOVE      LIN002        XTBLIN
052700980731     C                   MOVE      *blanks       XTBKEY
052800980731     C                   MOVEl     KEY002        XTBKEY
052900970509     C                   MOVE      UNI002        XTBUNT
053000940615$007 C                   MOVE      'Y4M'         XTBCOD
053100940225     C*
053200940207     C*
053300940224     C* valorizzo la chiave univoca per reperire i dati
053400940224     C* se non immissione
0535009403151   >C     OPZ002        IFNE      '01'
053600940315    >C*-------------------------------------------------------------
053700940315    >C* impostare i campi chiave ricevuti
053800940315    >C*-------------------------------------------------------------
053900940318     C*
054000940614     C*
054100940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0542009406142   >C     OPZ002        IFEQ      '02'
054300940315    >C     OPZ002        OREQ      '04'
054400940614     C                   MOVE      '1'           XTBALC
054500940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0546009406142E   C                   ELSE
054700940614     C                   MOVE      '0'           XTBALC
0548009406142-   C                   ENDIF
054900940614     C*
055000940616     C                   MOVE      '1'           XTBRIC
055100970214     C                   CALLB     'XATB'
055200940614     C                   PARM                    XATBDS
055300940318     C*
055400940614     C     XTBERR        IFEQ      '0'
055500940614     C                   MOVEL     XTBUNI        EXTDS
055600970512     c                   move      xtbunt        d1uniTA           8
055700941108     C* Giro la data se non blank
055800970214     c                   move      xtbdsc        xtbdscA          10
055900970214     C     XTBDSCa       IFNE      DATA
056000970214     C                   CALLB     'XDT4000'
056100970214     C                   PARM      XTBDSCA       XDTAMG           10
056200940621     C                   PARM      *ZEROS        XDTGMA            6 0
056300940621     C                   PARM      2             XDTSTA            1 0
056400940621     C                   MOVE      XDTGMA        D1DSC
056500940712     C                   MOVE      XTBRIS        D1RIS
056600941108     C                   ELSE
056700941108     C                   MOVE      *ZEROS        D1DSC
056800941108     C                   ENDIF
056900941108     C                   ENDIF
057000980722     C*
057100980722     C* Giro la data attivazione procedura
057200980731     C                   MOVE      §4MDAT        G02INV
057300980722     C                   MOVEL     '3'           G02ERR
057400980722     C                   CALL      'XSRDA8'
057500980722     C                   PARM                    WLBDA8
057600980722     C                   Z-ADD     G02DAT        D1DAT
057700980722     C* Giro la data creazione file statistiche
057800980731     C                   MOVE      §4MDST        G02INV
057900980722     C                   MOVEL     '3'           G02ERR
058000980722     C                   CALL      'XSRDA8'
058100980722     C                   PARM                    WLBDA8
058200980722     C                   Z-ADD     G02DAT        D1DST
058300940614     C*
058400940224     C*
058500940224     C* se il rcd da gestire non viene trovato
058600940318     C* oppure è allocato da altro pgm
058700940224     C* ritorno al pgm guida avvertendolo dell'errore
0588009406142    C     XTBERR        IFNE      '0'
058900940224     C                   MOVE      *ON           $FINE
059000940315    >C                   MOVE      '1'           ERR002
059100940224     C*
059200940224     C* altrimenti
0593009402242E   C                   ELSE
059400940224     C* riempio la videata
059500940224     C                   EXSR      RIED1
059600940224     C* richiamo routine dei ctrl per decodificarla
059700940614    >C                   EXSR      CTRD1
059800960228     C                   MOVEL     *IN99         SAV99             1
059900960228     C* Se la gestione è più videate
060000960305    >C                   EXSR      CTRD2
060100960305    >C     *IN99         IFEQ      *ON
060200960305    >C                   MOVEL     *IN99         SAV99             1
060300960305    >C                   ENDIF
060400940224     C                   MOVE      *ALL'0'       IN5098
060500940224     C                   MOVEA     IN5098        *IN(50)
060600940224     C* definizione indicatori su video
060700940224     C                   SETOFF                                       02
0608009402243    C                   SELECT
060900940315    >C     OPZ002        WHENEQ    '02'
061000940224     C                   SETON                                        02
061100940315    >C     OPZ002        WHENEQ    '05'
061200940506     C* disabilita tasti
061300940720     C                   MOVE      '0'           $FC(6)
061400940720     C                   MOVE      '0'           $FC(5)
061500940720     C                   MOVE      '0'           $FC(4)
0616009402243-   C                   ENDSL
061700940224     C*
0618009402242-   C                   ENDIF
061900940223     C*
0620009402241-   C                   ENDIF
062100940224     C*
062200940117     C                   ENDSR
062300940207     C/EJECT
062400940207     C************************************************************
062500940510     C* RIEMPIMENTO VIDEATA  D1
062600940207     C************************************************************
062700940207     C     RIED1         BEGSR
062800940207     C*
062900940207     C*
063000940207     C                   ENDSR
063100940309     C/EJECT
063200940309     C************************************************************
063300940309     C* GESTIONE F03 VIDEO D1
063400940309     C************************************************************
063500940309     C     F03D1         BEGSR
063600940309     C*
063700940309     C                   MOVE      *ON           $FINE
063800940315    >C                   MOVE      '1'           RET002
063900940325     C* fine programma
064000940325     C                   EXSR      ENDPGM
064100940325     C*
064200940325     C                   ENDSR
064300940309     C/EJECT
064400940309     C************************************************************
064500940309     C* GESTIONE F05 VIDEO D1
064600940309     C************************************************************
064700940309     C     F05D1         BEGSR
064800940309     C*
064900940309     C                   MOVE      *ON           $INZD1
065000940309     C*
065100940309     C                   ENDSR
065200940309     C/EJECT
065300940309     C************************************************************
065400940309     C* GESTIONE F08 VIDEO D1
065500940309     C************************************************************
065600940309     C     F08D1         BEGSR
065700940309     C*
065800940309     C                   MOVE      *ON           $FINE
065900940315    >C                   MOVE      '0'           RET002
066000940309     C*
066100940309     C                   ENDSR
066200940309     C/EJECT
066300940309     C************************************************************
066400940309     C* GESTIONE F12 VIDEO D1
066500940309     C************************************************************
066600940309     C     F12D1         BEGSR
066700940309     C*
066800940309     C                   MOVE      *ON           $FINE
066900940315    >C                   MOVE      '2'           RET002
067000940309     C*
067100940309     C                   ENDSR
067200940309     C/EJECT
067300940309     C************************************************************
067400940309     C* GESTIONE F24 VIDEO D1
067500940309     C************************************************************
067600940309     C     F24D1         BEGSR
067700940309     C*
067800940506     C*
067900940506     C                   MOVEA     $FX1          $FX
068000940506     C                   MOVEA     $FC1          $FC
068100940506     C                   MOVEA     $FR1          $FR
068200940506     C                   Z-ADD     $ULKD1        $ULK
068300940506     C                   EXSR      RIEKEY
068400941017     C                   MOVEA     $FC           $FC1
068500940506     C                   Z-ADD     $ULK          $ULKD1            3 0
068600940506     C                   MOVEL     $KEY1         Z1KE1
068700940506     C                   MOVEL     $KEY2         Z1KE2
068800940614     C                   WRITE     Z1
068900940506     C*
069000940309     C                   ENDSR
069100940207     C/EJECT
069200940131     C************************************************************
069300940207     C* CONTROLLO VIDEATA
069400940131     C************************************************************
069500940127     C     CTRD1         BEGSR
069600940131     C*
069700980722     C                   SETOFF                                       99
069800940127     C* non imposto pos. del cursore
069900940127     C                   Z-ADD     0             H1RIGA
070000940127     C                   Z-ADD     0             H1COLO
070100940621     C*
070200940621     C* CTRL DATA
070300940621     C*
070400941109     C                   MOVE      DATA          WRKDSC           10
070500940621     C     D1DSC         IFNE      0
070600970214     C                   CALLB     'XDT4000'
070700941109     C                   PARM                    XDTAMG           10
070800940621     C                   PARM      D1DSC         XDTGMA            6 0
070900940621     C                   PARM      3             XDTSTA            1 0
071000940621     C     XDTSTA        IFNE      0
071100980722     C                   SETON                                        5599
071200940621     C                   ELSE
071300941108     C                   MOVE      XDTAMG        WRKDSC
071400940621     C                   ENDIF
071500940621     C                   ENDIF
071600940223     C*
071700940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
071800940223     C*
0719009403151   >C     OPZ002        IFNE      '04'
072000971110     C*
072100971110     C* controllo codice vuoto
072200981014$017 C     D1COD         IFEQ      *blanks
072300971110     C                   SETON                                        5099
072400971110     C                   ENDIF
072500971110     C*
072600980722     C* Controllo causale 1
072700980722     c                   clear                   d1dcd1
072800980722     c*
072900980722     c                   if        §4Mcd1        <> *blanks
073000980722     c                   movel     §4mcd1        kkcau
073100980722     c     k02ope01      chain     anope01l                           64
073200980722     c                   if        *in64
073300980722     C                   seton                                        9981
073400980722     C                   else
073500980722     c                   movel     opedesbrev    d1dcd1
073600980722     c                   endif
073700980722     C                   else
073800980722     c                   Seton                                        9984
073900980722     c                   endif
074000980722     C*
074100980722     C* Controllo causale 2
074200070411     c*                  clear                   d1dcd2
074300070411     c*
074400070411     c*                  if        §4Mcd2        <> *blanks
074500070411     c*                  movel     §4mcd2        kkcau
074600070411     c*    k02ope01      chain     anope01l                           64
074700070411     c*                  if        *in64
074800070411     C*                  seton                                        9982
074900070411     C*                  else
075000070411     c*                  movel     opedesbrev    d1dcd2
075100070411     c*                  endif
075200070411     C*                  else
075300070411     c*                  Seton                                        9985
075400070411     c*                  endif
075500980722     C*
075600980722     C* Controllo causale 3
075700980722     c                   clear                   d1dcd3
075800980722     c*
075900980722     c                   if        §4Mcd3        <> *blanks
076000980722     c                   movel     §4mcd3        kkcau
076100980722     c     k02ope01      chain     anope01l                           64
076200980722     c                   if        *in64
076300980722     C                   seton                                        9983
076400980722     C                   else
076500980722     c                   movel     opedesbrev    d1dcd3
076600980722     c                   endif
076700980722     C                   else
076800980722     c                   Seton                                        9986
076900980722     c                   endif
077000980722     C*
077100980722     C* Controllo aliquota/Esenzione
077200980722     c*
077300980722     c                   if        §4Mese<>*blanks and §4Miva<>*zeros or
077400980722     c                             §4Mese=*blanks  and §4Miva=*zeros
077500980722     c                   Seton                                        9987
077600980722     C                   else
077700980722     C* Controllo codice di esenzione
077800980722     c                   if        §4Mese<>*blanks
077900980722     c                   move      xscsoc        ksocieta
078000980722     c                   move      '2'           ktpreg
078100980722     c                   movel     *blanks       kaliq
078200980722     c                   movel     §4Mese        krifiva
078300980722     c     k02riv01      chain     anriv01l                           64
078400980722     c                   if        *in64
078500980722     C                   seton                                        9989
078600980722     c                   endif
078700980722     C* Controllo aliquota
078800980722     C                   else
078900980722     c                   if        §4Miva<>*zeros
079000980722     c                   move      xscsoc        ksocieta
079100980722     c                   move      '2'           ktpreg
079200980722     c                   movel     §4miva        kaliq
079300980722     c                   movel     *blanks       krifiva
079400980722     c     k02riv01      chain     anriv01l                           64
079500980722     c                   if        *in64
079600980722     C                   seton                                        9988
079700980722     c                   endif
079800980722     c                   endif
079900980722     c                   endif
080000980722     c                   endif
080100980722     C*
080200980722     C* CTRL DATA attivazione procedura
080300980722     C     D1DAT         IFNE      0
080400980722     C                   MOVE      D1DAT         G02DAT
080500980722     C                   MOVE      *BLANKS       G02ERR
080600980722     C                   CALL      'XSRDA8'
080700980722     C                   PARM                    WLBDA8
080800980722     C     G02ERR        IFEQ      '1'
080900980722     C                   SETON                                        8099
081000980722     C                   ELSE
081100980722     C                   MOVE      G02INV        §4MDAT
081200980722     C                   MOVE      G02DAT        D1DAT
081300980722     C                   ENDIF
081400980722     C                   ELSE
081500980805     C                   MOVE      *zeros        §4MDAT
081600980722     C                   ENDIF
081700980722     C*
081800980722     C* CTRL DATA attivazione creazione file statische
081900980722     C     D1DST         IFNE      0
082000980722     C                   MOVE      D1DST         G02DAT
082100980722     C                   MOVE      *BLANKS       G02ERR
082200980722     C                   CALL      'XSRDA8'
082300980722     C                   PARM                    WLBDA8
082400980722     C     G02ERR        IFEQ      '1'
082500980722     C                   SETON                                        7999
082600980722     C                   ELSE
082700980722     C                   MOVE      G02INV        §4MDST
082800980722     C                   MOVE      G02DAT        D1DST
082900980722     C                   ENDIF
083000980722     C                   ELSE
083100980805     C                   MOVE      *zeros        §4MDST
083200980722     C                   ENDIF
083300980917     C*
083400980917     c                   clear                   d1dksr
083500980917     c*
083600980917     c                   if        §4mkcr         <> *blanks   and
083700980917     c                             §4mksr         <> *blanks
083800980917     c                   movel     §4mkcr        rcokcc
083900980917     C                   movel     §4mksr        rcoksc
084000980917     C     K03rco99      CHAIN     anrco99j                           90
084100980917     C**n90rcosogg       chain     ansog01l                           90
084200980917     C     *IN90         IFEQ      '1'
084300980917     C                   SETON                                        9968
084400980917     C                   clear                   d1dksr
084500980917     C                   ELSE
084600980917     C                   MOVEL     pdcdesbrev    d1dksr
084700980917     C                   END
084800980917     C                   ELSE
084900980917     C                   SETON                                        9965
085000980917     C                   END
085100940207     C*
085200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
085300940315    >C     OPZ002        IFEQ      '01'
085400940315    >C     OPZ002        OREQ      '03'
085500940315    >C*-------------------------------------------------------------
085600940315    >C* impostare i campi chiave e verificare l'esistenza del record
085700940315    >C*-------------------------------------------------------------
085800971110$007 C                   MOVE      'Y4M'         XTBCOD
085900980731     C                   MOVE      *BLANKS       XTBKEY
086000980731    >C                   MOVEL     D1COD         XTBKEY
086100970512    >C                   MOVE      D1UNITA       XTBUNT
086200940614     C                   MOVE      '0'           XTBALC
086300940614     C                   MOVE      '1'           XTBRIC
086400970214    >C                   CALLB     'XATB'
086500940614     C                   PARM                    XATBDS
086600940614     C     XTBERR        IFEQ      '0'
086700940223     C                   SETON                                        5999
086800940223     C                   ENDIF
086900940620     C* Controllo se compreso nei range indice
087000940620     C     XTBERR        IFEQ      '3'
087100940620    >C                   SETON                                        5899
087200940620     C                   END
087300940620     C*
087400940620     C* REPERISCO AUTORIZZAZIONI UTENTE
087500940620     C                   CLEAR                   XCHATB
087600941118     C                   MOVE      XSCSOC        XCTSOC
087700941118     C                   MOVE      XSCDUN        XCTUNI
087800940620     C                   MOVEL     KNMUS         XCTPRF
087900940620     C                   MOVE      XTBCOD        XCTCOD
088000940620     C                   CALL      'XCHKATB'
088100940620     C                   PARM                    XCHATB
088200941111     C                   MOVEL     XCTDDS        AUTDS
088300941027     C     XCTRTN        IFEQ      1
088400941027     C                   MOVEL     'N'           MANGRU
088500941027     C                   ENDIF
088600940620     C* Non ammessa immissione di gruppo
088700940620     C     XTBGRU        IFEQ      'G'
088800940620     C     MANGRU        ANDEQ     'N'
088900940620     C                   SETON                                        5799
089000940620     C                   END
089100940620     C* Non ammessa immissione di azienda
089200940620     C     XTBGRU        IFEQ      'A'
089300940620     C     MANAZI        ANDEQ     'N'
089400940620     C                   SETON                                        5699
089500940620     C                   END
089600940712     C* codice riservato non accessibile
089700941006     C     XTBRIS        IFGT      ABIRIS
089800941005    >C     OPZ002        ANDNE     '01'
089900941005    >C     OPZ002        ANDNE     '03'
090000980722     C                   SETON                                        6499
090100940712     C                   END
090200940224     C                   ENDIF
090300940224     C*
0904009402241-   C                   END
090500940131     C*
090600940302     C     *IN99         IFEQ      *ON
090700940510     C     $GEST         ANDEQ     'D1'
090800960228     C     OPZ002        ANDNE     '07'
090900940117     C                   MOVEA     *IN           WIN
091000940128     C                   MOVE      *ALL'0'       IN5098           49
091100940128     C                   MOVEA     IN5098        *IN(50)
091200940614     C                   WRITE     D1
091300940117     C                   MOVEA     WIN           *IN
091400940302     C                   ENDIF
091500940127     C*
091600940117     C                   ENDSR
091700960305     C/EJECT
091800960305     C************************************************************
091900960305     C* CONTROLLO VIDEATA D2
092000960305     C************************************************************
092100960305     C     CTRD2         BEGSR
092200960305     C*
092300960305     C                   ENDSR
092400940207     C/EJECT
092500940224     C************************************************************
092600940315    >C* AGGIORNAMENTO ANAGRAFICA
092700940224     C************************************************************
092800940315    >C     AGGANA        BEGSR
092900971110     C*
093000971110$007 C                   MOVE      'Y4M'         XTBCOD
093100980731     C                   MOVE      *blanks       XTBKEY
093200980731     C                   MOVEl     D1COD         XTBKEY
093300970512     C                   MOVE      D1UNITA       XTBUNT
093400940207     C*
093500940207     C                   SELECT
093600940315    >C     OPZ002        WHENEQ    '01'
093700940315    >C     OPZ002        OREQ      '03'
093800940315    >C                   EXSR      RIEANA
093900950609     C                   MOVE      XSCCUT        XTBUTE
094000940614     C                   MOVE      '5'           XTBRIC
094100970214     C                   CALLB     'XATB'
094200940614    >C                   PARM                    XATBDS
094300940318     C* se rcd non scrivibile attivo errore
094400940615     C     XTBERR        IFEQ      '1'
094500940318    >C                   SETON                                        5999
094600940318     C                   END
094700940315    >C     OPZ002        WHENEQ    '02'
094800940621     C                   MOVE      '1'           XTBRIC
094900940621     C                   MOVE      '1'           XTBALC
095000970214     C                   CALLB     'XATB'
095100940621     C                   PARM                    XATBDS
095200940622    >C                   EXSR      RIEANA
095300950609     C                   MOVE      XSCCUT        XTBUTE
095400940614     C                   MOVE      '4'           XTBRIC
095500970214     C                   CALLB     'XATB'
095600940614    >C                   PARM                    XATBDS
095700940315    >C     OPZ002        WHENEQ    '04'
095800940621     C                   MOVE      '1'           XTBRIC
095900940621     C                   MOVE      '1'           XTBALC
096000970214     C                   CALLB     'XATB'
096100940621     C                   PARM                    XATBDS
096200940614     C                   MOVE      '6'           XTBRIC
096300970214     C                   CALLB     'XATB'
096400940614    >C                   PARM                    XATBDS
096500940621     C* annullo traduzioni
096600940621     C     XTBERR        IFEQ      '0'
096700940621     C                   EXSR      ANNTBT
096800940621     C                   END
096900940621     C*
097000940207     C                   ENDSL
097100940203     C*
097200940203     C                   ENDSR
097300940224     C/EJECT
097400940224     C************************************************************
097500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
097600940224     C************************************************************
097700940616     C     RIEANA        BEGSR
097800940616     C                   CLEAR                   XTBUNI
097900940621     C                   MOVE      WRKDSC        XTBDSC
098000940712     C                   MOVE      D1RIS         XTBRIS
098100940616     C                   MOVEL     EXTDS         XTBUNI
098200940207     C                   ENDSR
098300940224     C/EJECT
098400940224     C************************************************************
098500940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
098600940224     C************************************************************
098700940224     C     GESAGG        BEGSR
098800940224     C*
098900940224     C* segnalo al pgm chiamante l'avvenuta conferma
099000940315    >C                   MOVE      *ON           OPR002
099100940224     C* segnalo al pgm chiamante l'aver premuto l'F6
099200940315    >C                   MOVE      '0'           RET002
099300940224     C*
0994009402241    C                   SELECT
099500940224     C* nel caso di immissione, ripristino la videata iniziale
099600940315    >C     OPZ002        WHENEQ    '01'
099700940224     C                   MOVE      *ON           $INZD1
099800940224     C* altrimenti ritorno al pgm chiamante
099900940224     C                   OTHER
100000940224     C                   MOVE      *ON           $FINE
1001009402241-   C                   ENDSL
100200940224     C*
100300940224     C                   ENDSR
100400940621     C/EJECT
100500940621     C************************************************************
100600940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
100700940621     C************************************************************
100800940621    >C     ANNTBT        BEGSR
100900940621     C                   MOVE      XTBAZI        TBTAZI
101000940621     C                   MOVE      XTBCOD        TBTCOD
101100940621     C                   MOVE      XTBKEY        TBTKEY
101200940621     C     KTBT          SETLL     ANTBT01L
101300940621     C                   DO        *HIVAL
101400940621     C     KTBT          DELETE    ANTBT01L                           21
101500940621     C  N21              END
101600940621     C*
101700940621     C                   ENDSR
101800940207     C/EJECT
101900940131     C************************************************************
102000940131     C* OPERAZIONI INIZIALI
102100940131     C************************************************************
102200940131     C     *INZSR        BEGSR
102300940131     C*
102400940127     C* Reperimento parametri
102500940127     C*
102600940117     C     *ENTRY        PLIST
102700940117     C                   PARM                    KPJBA
102800940712     C*
102900940712     C* Reperimento Codice utente
103000940712     C*
103100940223     C*
103200940506     C*
103300940506     C* Reperimento tasti di funzione
103400940506     C*
103500940506     C                   EXSR      REPKEY
103600940621     C*
103700940621     C* KLIST
103800940621     C*
103900980917     C     K03Rco99      KLIST
104000980917     C                   KFLD                    xscsoc
104100980917     C                   KFLD                    rcokcc
104200980917     C                   KFLD                    rcoksc
104300940621     C     KTBT          KLIST
104400940621     C                   KFLD                    TBTAZI
104500940621     C                   KFLD                    TBTCOD
104600940621     C                   KFLD                    TBTKEY
104700980722     C*                                                    *
104800980722     c     K02ope01      klist
104900980722     c                   kfld                    Xscsoc
105000980722     c                   kfld                    kkcau             4
105100980722     C*
105200980722     c     K02riv01      klist
105300980722     c                   kfld                    ksocieta          3
105400980722     c                   kfld                    ktpreg            1
105500980722     c                   kfld                    kaliq             5 2
105600980722     c                   kfld                    krifiva           3
105700940506     C*
105800940223     C                   ENDSR
105900941103     C/EJECT
106000941103      ************************************************************
106100941103      * Reperimento dati società
106200941103      ************************************************************
106300941103     C     REPSOC        BEGSR
106400941103      *
106500970214     C                   CALLB     'XSOC'
106600941103     C                   PARM                    TIPXSC            6
106700941103     C                   PARM                    SOCXSC            3
106800941103     C                   PARM                    CDSXSC            9 0
106900941103     C                   PARM                    MODXSC            3
107000941103     C                   PARM      *BLANKS       RTNXSC            1
107100941103     C                   PARM                    XSOCDS
107200941103     C                   PARM                    KPJBA
107300941103      *
107400941103     C                   ENDSR
107500940223     C/EJECT
107600940506     C************************************************************
107700940506     C* REPERIMENTO TASTI DI FUNZIONE
107800940506     C************************************************************
107900940506     C     REPKEY        BEGSR
108000940506     C                   MOVEA     $FM1          $FM
108100940506     C                   EXSR      REPKKK
108200940506     C                   MOVEA     $FX           $FX1
108300940614     C                   ENDSR
108400940506     C/EJECT
108500940506     C************************************************************
108600940506     C* REPERIMENTO TASTI DI FUNZIONE
108700940506     C************************************************************
108800940506     C     REPKKK        BEGSR
108900940506     C*
109000940506     C                   CLEAR                   $FX
109100940506     C     1             DO        24            X
109200940506     C*
109300940506     C     $FM(X)        IFNE      *BLANK
109400940506     C*
109500940506     C                   MOVEL     $FM(X)        IDMSG
109600970214     C                   CALLB     'XRTVM'
109700940506     C                   PARM                    IDMSG            27
109800940506     C                   PARM                    TXTMSG          100
109900940506     C                   PARM                    ERRMSG            1
110000940506     C*
110100940506     C     ERRMSG        IFNE      *ON
110200940506     C                   MOVEL     TXTMSG        $FX(X)
110300940506     C                   ELSE
110400940506     C                   MOVEL     *ALL' '       $FX(X)
110500940506     C                   END
110600940506     C*
110700940506     C                   END
110800940506     C*
110900940506     C                   END
111000940506     C*
111100940506     C                   ENDSR
111200940509     C/EJECT
111300940223     C************************************************************
111400940223     C* INIZIALIZZAZIONE VARIABILI
111500940223     C************************************************************
111600940223     C     INZVAR        BEGSR
111700951124     C*
111800951124     C* Chiamata XSOC
111900951124     C*
112000951124     C                   MOVEL     'SOC001'      TIPXSC
112100951124     C                   EXSR      REPSOC
112200951124     C     RTNXSC        IFNE      '1'
112300951124     C                   MOVEL     XSOCDS        SOC001
112400951124     C                   ELSE
112500951124     C                   MOVE      *ON           $FINE
112600951124     C* fine programma
112700951124     C                   EXSR      ENDPGM
112800951124     C                   ENDIF
112900940224     C*
113000940224     C* Dati da PGM guida
113100940224     C*
113200940315    >C                   MOVEL     KPJBU         D002DS
113300940128     C*
113400940223     C* Variabili per gestione videate
113500940128     C*
113600940223     C                   MOVE      'D1'          $GEST
113700940223     C                   MOVE      *OFF          $FINE
113800940223     C                   MOVE      *ON           $INZD1
113900940224     C                   MOVE      *BLANK        $LASTG
114000940224     C                   MOVE      *BLANK        $LASTV
114100940506     C                   Z-ADD     0             $ULKD1            3 0
114200940127     C*
114300940223     C* Variabili appoggio
114400940127     C*
114500940224     C                   Z-ADD     0             CURR
114600940224     C                   Z-ADD     0             CURC
114700940207     C*
114800940207     C* Valorizzazione campi univoci testate
114900940207     C*
115000940614     C                   CLEAR                   T1
115100940614     C                   MOVEL     KNSIF         NOMSIF
115200941109     C                   MOVEL     XSCDSI        NOMDIT
115300941027     C                   MOVEL     DSPGM         NOMPGM
115400940224     C                   Z-ADD     1             X
115500940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
115600940224     C     *IN21         IFEQ      *ON
115700940224     C                   MOVEL     $ID(X)        IDMSG
115800970214     C                   CALLB     'XRTVM'
115900940224     C                   PARM                    IDMSG            27
116000940224     C                   PARM                    TXTMSG          100
116100940224     C                   PARM                    ERRMSG            1
116200940224     C                   PARM      *ON           CTRMSG            1
116300940224     C                   PARM      30            LENMSG            3 0
116400940224     C     ERRMSG        IFNE      *ON
116500940224     C                   MOVEL     TXTMSG        T1OPE
116600940224     C                   ELSE
116700940224     C                   MOVEL     *ALL'?'       T1OPE
116800940224     C                   ENDIF
116900940224     C                   END
117000940224     C*
117100940224     C* Inizializzazione/Decodifica prima videata
117200940224     C*
117300940224     C                   EXSR      INZD1
117400940224     C                   MOVE      *OFF          $INZD1
117500960228     C* Se opzione = '07' richiesti solo controlli
117600960228     C     OPZ002        IFEQ      '07'
117700960228     C                   MOVEL     *IN99         ERR002
117800960228     C                   MOVEL     '0'           RET002
117900960228     C                   MOVEL     *ON           $FINE
118000960228     C                   ENDIF
118100940127     C*
118200940117     C                   ENDSR
118300940128     C************************************************************
118400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
118500940223     C************************************************************
118600940223     C     DEFCAM        BEGSR
118700940223     C*
118800940223     C* klist
118900940207     C*
119000940223     C*
119100940223     C* Variabili per gestione videate
119200940223     C*
119300940223     C                   MOVE      *BLANK        $GEST             2
119400940223     C                   MOVE      *BLANK        $FINE             1
119500940223     C                   MOVE      *BLANK        $INZD1            1
119600940223     C                   MOVE      *BLANK        $LASTG            2
119700940223     C                   MOVE      *BLANK        $LASTV            2
119800940223     C*
119900940223     C* Variabili appoggio
120000940223     C*
120100940223     C                   Z-ADD     0             CURR              2 0
120200940223     C                   Z-ADD     0             CURC              2 0
120300940223     C                   MOVE      *ZEROS        WIN              99
120400940224     C                   MOVE      *BLANK        WTBERR            1
120500940223     C*
120600940223     C* Indici
120700940223     C*
120800940223     C                   Z-ADD     0             X                 3 0
120900940223     C                   Z-ADD     0             Y                 3 0
121000940127     C*
121100940117     C                   ENDSR
121200940128     C************************************************************
121300940509     C/EJECT
121400940509     C************************************************************
121500940509     C* CONTROLLO ABILITAZIONE TASTI
121600940509     C************************************************************
121700940509     C     CTRKEY        BEGSR
121800940509     C                   SELECT
121900940509     C     $TASTO        WHENEQ    F01
122000940720     C     $FC(1)        IFEQ      '0'
122100940509     C                   SETON                                        99
122200940509     C                   ENDIF
122300940509     C     $TASTO        WHENEQ    F02
122400940720     C     $FC(2)        IFEQ      '0'
122500940509     C                   SETON                                        99
122600940509     C                   ENDIF
122700940509     C     $TASTO        WHENEQ    F03
122800940720     C     $FC(3)        IFEQ      '0'
122900940509     C                   SETON                                        99
123000940509     C                   ENDIF
123100940509     C     $TASTO        WHENEQ    F04
123200940720     C     $FC(4)        IFEQ      '0'
123300940509     C                   SETON                                        99
123400940509     C                   ENDIF
123500940509     C     $TASTO        WHENEQ    F05
123600940720     C     $FC(5)        IFEQ      '0'
123700940509     C                   SETON                                        99
123800940509     C                   ENDIF
123900940509     C     $TASTO        WHENEQ    F06
124000940720     C     $FC(6)        IFEQ      '0'
124100940509     C                   SETON                                        99
124200940509     C                   ENDIF
124300940509     C     $TASTO        WHENEQ    F07
124400940720     C     $FC(7)        IFEQ      '0'
124500940509     C                   SETON                                        99
124600940509     C                   ENDIF
124700940509     C     $TASTO        WHENEQ    F08
124800940720     C     $FC(8)        IFEQ      '0'
124900940509     C                   SETON                                        99
125000940509     C                   ENDIF
125100940509     C     $TASTO        WHENEQ    F09
125200940720     C     $FC(09)       IFEQ      '0'
125300940509     C                   SETON                                        99
125400940509     C                   ENDIF
125500940509     C     $TASTO        WHENEQ    F10
125600940720     C     $FC(10)       IFEQ      '0'
125700940509     C                   SETON                                        99
125800940509     C                   ENDIF
125900940509     C     $TASTO        WHENEQ    F11
126000940720     C     $FC(11)       IFEQ      '0'
126100940509     C                   SETON                                        99
126200940509     C                   ENDIF
126300940509     C     $TASTO        WHENEQ    F12
126400940720     C     $FC(12)       IFEQ      '0'
126500940509     C                   SETON                                        99
126600940509     C                   ENDIF
126700940509     C     $TASTO        WHENEQ    F13
126800940720     C     $FC(13)       IFEQ      '0'
126900940509     C                   SETON                                        99
127000940509     C                   ENDIF
127100940509     C     $TASTO        WHENEQ    F14
127200940720     C     $FC(14)       IFEQ      '0'
127300940509     C                   SETON                                        99
127400940509     C                   ENDIF
127500940509     C     $TASTO        WHENEQ    F15
127600940720     C     $FC(15)       IFEQ      '0'
127700940509     C                   SETON                                        99
127800940509     C                   ENDIF
127900940509     C     $TASTO        WHENEQ    F16
128000940720     C     $FC(16)       IFEQ      '0'
128100940509     C                   SETON                                        99
128200940509     C                   ENDIF
128300940509     C     $TASTO        WHENEQ    F17
128400940720     C     $FC(17)       IFEQ      '0'
128500940509     C                   SETON                                        99
128600940509     C                   ENDIF
128700940509     C     $TASTO        WHENEQ    F18
128800940720     C     $FC(18)       IFEQ      '0'
128900940509     C                   SETON                                        99
129000940509     C                   ENDIF
129100940509     C     $TASTO        WHENEQ    F19
129200940720     C     $FC(19)       IFEQ      '0'
129300940509     C                   SETON                                        99
129400940509     C                   ENDIF
129500940509     C     $TASTO        WHENEQ    F20
129600940720     C     $FC(20)       IFEQ      '0'
129700940509     C                   SETON                                        99
129800940509     C                   ENDIF
129900940509     C     $TASTO        WHENEQ    F21
130000940720     C     $FC(21)       IFEQ      '0'
130100940509     C                   SETON                                        99
130200940509     C                   ENDIF
130300940509     C     $TASTO        WHENEQ    F22
130400940720     C     $FC(22)       IFEQ      '0'
130500940509     C                   SETON                                        99
130600940509     C                   ENDIF
130700940509     C     $TASTO        WHENEQ    F23
130800940720     C     $FC(23)       IFEQ      '0'
130900940509     C                   SETON                                        99
131000940509     C                   ENDIF
131100940509     C     $TASTO        WHENEQ    F24
131200940720     C     $FC(24)       IFEQ      '0'
131300940509     C                   SETON                                        99
131400940509     C                   ENDIF
131500940509     C                   ENDSL
131600940509     C                   ENDSR
131700940509     C/EJECT
131800940509     C************************************************************
131900940509     C* RIEMPIMENTO TASTI FUNZIONE
132000940509     C************************************************************
132100940509     C     RIEKEY        BEGSR
132200940509     C*
132300940509     C* Input :
132400940509     C* - $FX = schiera con tutti i testi disponibili
132500940509     C* - $FC = schiere con i flag di validità associati ai testi
132600940509     C* - $FR = schiere con i flag di REVERSE IMAGE
132700940509     C* - $ULK = ultimo testo caricato
132800940509     C* Output :
132900940509     C* - $ULK
133000940509     c* - $KEY1= 1° riga di tasti funzione
133100940509     c* - $KEY2= 2° riga di tasti funzione
133200940509     C*
133300970214     C                   CALLB     'XKEYMSG'
133400940509     C* input
133500940509     C                   PARM                    $FX
133600940509     C                   PARM                    $FC
133700940509     C                   PARM                    $FR
133800940509     C                   PARM                    $ULK              3 0
133900940509     C* output
134000940509     C                   PARM                    $KEY1            79
134100940509     C                   PARM                    $KEY2            79
134200940509     C*
134300940509     C                   ENDSR
134400940615**
13450094022401PROMSG    *LIBL     COS0001    immissione
13460094022402PROMSG    *LIBL     COS0002    modifica
13470094022403PROMSG    *LIBL     COS0003    copia
13480094022404PROMSG    *LIBL     COS0004    annullamento
13490094022405PROMSG    *LIBL     COS0005    visualizzazione
135000940509** TASTI DI FUNZIONE  D1
135100940509PROMSG    *LIBL     KEY0001  01
135200940509PROMSG    *LIBL     KEY0002  02
135300940509PROMSG    *LIBL     KEY0003  03
135400940509PROMSG    *LIBL     KEY0004  04
135500940509PROMSG    *LIBL     KEY0005  05
135600940509PROMSG    *LIBL     KEY0006  06
135700940509PROMSG    *LIBL     KEY0007  07
135800940509PROMSG    *LIBL     KEY0008  08
135900940509PROMSG    *LIBL     KEY0009  09
136000940509PROMSG    *LIBL     KEY0010  10
136100940509PROMSG    *LIBL     KEY0011  11
136200940509PROMSG    *LIBL     KEY0012  12
136300940509PROMSG    *LIBL     KEY0013  13
136400940509PROMSG    *LIBL     KEY0014  14
136500940509PROMSG    *LIBL     KEY0015  15
136600940509PROMSG    *LIBL     KEY0016  16
136700940509PROMSG    *LIBL     KEY0017  17
136800940509PROMSG    *LIBL     KEY0018  18
136900940509PROMSG    *LIBL     KEY0019  19
137000940509PROMSG    *LIBL     KEY0020  20
137100940509PROMSG    *LIBL     KEY0021  21
137200940509PROMSG    *LIBL     KEY0022  22
137300940509PROMSG    *LIBL     KEY0023  23
137400940509PROMSG    *LIBL     KEY0024  24
137500940509**  ABILITAZIONE D1
1376009407200N  01
1377009407200N  02
1378009407201N  03
1379009407201N  04
1380009407201N  05
1381009407201N  06
1382009407201N  07
1383009407201N  08
1384009407200N  09
1385009407200N  10
1386009407200N  11
1387009407201N  12
1388009407200N  13
1389009407200N  14
1390009407200N  15
1391009407200N  16
1392009407200N  17
1393009407200N  18
1394009407200N  19
1395009407200N  20
1396009407200N  21
1397009407200N  22
1398009407200N  23
1399009407200N  24
