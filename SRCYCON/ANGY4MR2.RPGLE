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
007400110916     D anope000_cd1    DS                  INZ LIKEREC(anope000)
007500110916     D anope000_cd2    DS                  INZ LIKEREC(anope000)
007600110916     D anope000_cd3    DS                  INZ LIKEREC(anope000)
007700941103     D*-------------
007800941103      * DS Interna per dati utente
007900941103     D XSOCDS          DS          1000
008000941103     D*-------------
008100940616     D XATBDS        E DS                  INZ
008200940318     D*-------------
008300940318     D* DS Esterna gestione messaggi per COMIT
008400940616     D XMSGDS        E DS                  INZ
008500940318     D  STS          E                     EXTFLD(MSGSTS)
008600940318     D                                     DIM(333)
008700940318     D  JBA          E                     EXTFLD(MSGJBA)
008800940318     D                                     DIM(502)
008900940203     D*-------------
009000940620     D* DS Esterna autorizzazioni tabelle
009100940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
009200940620     D*
009300940620     D* DS  restituzione autorizzazioni
009400940620     D* IND=indice
009500940620     D* GRU=man. di gruppo
009600940620     D* AZI=man. azienda
009700940712     D* ABI=abilit. ai riservati
009800941111     D AUTDS           DS
009900940620     D  MANIND                 1      1
010000940620     D  MANGRU                 2      2
010100940620     D  MANAZI                 3      3
010200940712     D  ABIRIS                 4      4
010300940620     D*-------------
010400940207     D* opzione richiesta
010500940614    >D D002DS          DS
010600940614     D  OPZ002                 1      2
010700940614     D* 0 = ritorno normale
010800940614     D* 1 = F3
010900940614     D* 2 = F12
011000940614     D  RET002                 3      3
011100940614     D* operazione eseguite dal pgm chiamato :
011200940614     D* 0 = nessuna operazione
011300940614     D* 1 = eseguito aggiornamento
011400940614     D  OPR002                 4      4
011500940614     D* funzione non eseguibile per errore :
011600940614     D* 0 = nessun errore
011700940614     D* 1 = funzione richiamata chiusa in errore
011800940614     D* Aggiungere eventuali altri codici di errore
011900940614     D  ERR002                 5      5
012000940614     D* Azienda
012100940614     D  AZI002                 6      8
012200940614     D* Lingua
012300940614     D  LIN002                 9      9
012400940614     D* Codice scelto
012500940614     D  KEY002                10     24
012600940620     D* Autorizzazione
012700940620     D  AUT002                25     25
012800970509     D* Unita
012900970509     D  UNI002                26     33
013000940614     D*-------------
013100940201     D*-------------
013200940201     D DSFMT           DS
013300940506     D  $TASTO               369    369
013400940201     D  NRG                  370    370
013500940201     D  NCL                  371    371
013600940201     D*-------------
013700940201     D* posizione cursore
013800940201     D CURSOR          DS
013900940223     D  RIRI                   1      2B 0 INZ
014000940201     D  R$$                    2      2
014100940223     D  COCO                   3      4B 0 INZ
014200940201     D  C$$                    4      4
014300940207     D*-------------
014400940207     D* Reperimento nome PGM
014500940207     D STATUS         SDS           333
014600940207     D  DSPGM            *PROC
014700940225     D*-------------
014800940225     D* COSTANTI
014900940225     D*-------------
015000940506     D*
015100940506     D* Tasti di funzione
015200940506     D F01             C                   CONST(X'31')
015300940506     D F02             C                   CONST(X'32')
015400940506     D F03             C                   CONST(X'33')
015500940506     D F04             C                   CONST(X'34')
015600940506     D F05             C                   CONST(X'35')
015700940506     D F06             C                   CONST(X'36')
015800940506     D F07             C                   CONST(X'37')
015900940506     D F08             C                   CONST(X'38')
016000940506     D F09             C                   CONST(X'39')
016100940506     D F10             C                   CONST(X'3A')
016200940506     D F11             C                   CONST(X'3B')
016300940506     D F12             C                   CONST(X'3C')
016400940506     D F13             C                   CONST(X'B1')
016500940506     D F14             C                   CONST(X'B2')
016600940506     D F15             C                   CONST(X'B3')
016700940506     D F16             C                   CONST(X'B4')
016800940506     D F17             C                   CONST(X'B5')
016900940506     D F18             C                   CONST(X'B6')
017000940506     D F19             C                   CONST(X'B7')
017100940506     D F20             C                   CONST(X'B8')
017200940506     D F21             C                   CONST(X'B9')
017300940506     D F22             C                   CONST(X'BA')
017400940506     D F23             C                   CONST(X'BB')
017500940506     D F24             C                   CONST(X'BC')
017600940506     D ENTER           C                   CONST(X'F1')
017700940506     D ROLDWN          C                   CONST(X'F4')
017800940506     D ROLLUP          C                   CONST(X'F5')
017900940506     D*-------------
018000940506     D*
018100941108     D DATA            C                   CONST('0001-01-01')
018200980722     D*-------------
018300980722     D a070ds        E DS                  extname(ana070ds) inz
018400980722     D a068ds        E DS                  extname(ana068ds) inz
018500980722     D*-------------
018600980722     D WLBDA8          DS
018700980722     D  G02DAT                 1      8S 0
018800980722     D  G02INV                 9     16S 0
018900980722     D  G02ERR                17     17
019000980722     D  G02TGI                18     22S 0
019100940128     I*----------------------------------------------------*
019200940615     ID1
019300980722$005 I              D1CD1                       §4mCD1
019400110916$005 I              D1CD2                       §4mCD2
019500980722$005 I              D1CD3                       §4mCD3
019600070411$005 I*             D1IVA                       §4mIVA
019700980722$005 I              D1ESE                       §4mESE
019800980722$005 I              D1GGP                       §4mGGP
019900980917$005 I              D1KCr                       §4mKCr
020000980917$005 I              D1KSr                       §4mKSr
020100940127     I*
020200940127     C*----------------------------------------------------*
020300940127     C*                MAIN LINE PROGRAM
020400940127     C*----------------------------------------------------*
020500940127     C*
020600940223     C* inizializzazione variabili
020700940223     C                   EXSR      INZVAR
020800940223     C*
020900940223     C     $FINE         DOWEQ     *OFF
021000940202     C     $GEST         CASEQ     'D1'          GESD1
021100940117     C                   END
021200940117     C                   END
021300940201     C*
021400940325     C* fine programma
021500940325     C                   EXSR      ENDPGM
021600940325     C/EJECT
021700940325     C************************************************************
021800940325     C* RICERCHE
021900940325     C************************************************************
022000940325     C     ENDPGM        BEGSR
022100940325     C*
022200940614     C* disallocazione rcd nel caso di
022300971110$007 C                   MOVE      'Y4M'         XTBCOD
022400940614    >C                   MOVE      '7'           XTBRIC
022500970214     C                   CALLB     'XATB'
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
025100940223     C*
025200980722    >C*  FMTD1  - CAUSALE 1
025300980722     C     H1NMFL        WHENEQ    'D1CD1'
025400980722     C                   RESET                   A070DS
025500980722     C                   MOVEL     '01'          OPZ070
025600980722     C                   MOVEL     A070DS        KPJBU
025700980722     C                   CALL      'ANA070R'
025800980722     C                   PARM                    KPJBA
025900980722     C                   MOVEL     KPJBU         A070DS
026000980722     C                   MOVEL     CAU070        WWCAUSC1          4
026100980722     C*
026200980722     C     WWCAUSC1      IFNE      *BLANK
026300980722     C                   MOVEL     WWCAUSC1      §4mcd1
026400980722     C                   movel     DE1070        D1DCD1
026500980722     C                   ENDIF
026600980722     C* imposto pos. del cursore
026700980722     C                   Z-ADD     CURR          H1RIGA
026800980722     C                   Z-ADD     CURC          H1COLO
026900980722     C*
027000980722    >C*  FMTD1  - CAUSALE 2
027100980722     C     H1NMFL        WHENEQ    'D1CD2'
027200980722     C                   RESET                   A070DS
027300980722     C                   MOVEL     '01'          OPZ070
027400980722     C                   MOVEL     A070DS        KPJBU
027500980722     C                   CALL      'ANA070R'
027600980722     C                   PARM                    KPJBA
027700980722     C                   MOVEL     KPJBU         A070DS
027800980722     C                   MOVEL     CAU070        WWCAUSC1          4
027900980722     C*
028000980722     C     WWCAUSC1      IFNE      *BLANK
028100980722     C                   MOVEL     WWCAUSC1      §4mcd2
028200070411     C*                  movel     DE1070        D1DCD2
028300980722     C                   ENDIF
028400980722     C*
028500980722     C* imposto pos. del cursore
028600980722     C                   Z-ADD     CURR          H1RIGA
028700980722     C                   Z-ADD     CURC          H1COLO
028800980722     C*
028900980722     C* CAUSALE 3
029000980722     C     H1NMFL        WHENEQ    'D1CD3'
029100980722     C                   RESET                   A070DS
029200980722     C                   MOVEL     '01'          OPZ070
029300980722     C                   MOVEL     A070DS        KPJBU
029400980722     C                   CALL      'ANA070R'
029500980722     C                   PARM                    KPJBA
029600980722     C                   MOVEL     KPJBU         A070DS
029700980722     C                   MOVEL     CAU070        WWCAUSC1          4
029800980722     C*
029900980722     C     WWCAUSC1      IFNE      *BLANK
030000980722     C                   MOVEL     WWCAUSC1      §4mcd3
030100980722     C                   movel     DE1070        D1DCD3
030200980722     C                   ENDIF
030300980722     C*
030400980722     C* FMTS1  - RIFERIMENTI E ALIQUOTE
030500980722     C     H1NMFL        WHENEQ    'D1IVA'
030600980722     C     H1NMFL        OREQ      'D1ESE'
030700980722     C                   RESET                   A068ds
030800980722     C                   MOVE      '01'          OPZ068
030900980722     C                   MOVE      XscSoc        SOC068
031000980722     C                   MOVE      '2'           TPR068
031100980722     C                   MOVE      '997'         LIB068
031200980722     C                   MOVE      *ZEROS        ALI068
031300980722     C                   MOVE      *BLANK        RIF068
031400980722     C                   MOVEL     A068ds        KPJBU
031500980722     C                   CALL      'ANA068R'
031600980722     C                   PARM                    KPJBA
031700980722     C                   MOVEL     KPJBU         A068ds
031800980722     C*
031900980722     C                   IF        Ali068 <> 0        or
032000980722     C                             Rif068 <> *blank
032100980722     C                   MOVE      ALI068        §4MIVA
032200980722     C                   MOVE      RIF068        §4MESE
032300980722     C                   end
032400980917     C* FMTD1  - capoconto
032500980917    >C     H1NMFL        WHEneq    'D1KCR'
032600980917     C                   RESET                   Ana200DS
032700980917     C                   MOVEL     '01'          OPZ200
032800980917     C                   clear                   KCC200
032900980917     C                   MOVEL     Ana200DS      KPJBU
033000980917     C                   CALL      'ANA200R'
033100980917     C                   PARM                    KPJBA
033200980917     C                   MOVEL     KPJBU         Ana200DS
033300980917     C     KCC200        IFNE      *BLANK
033400980917     C                   MOVEL     KCC200        §4mkcr
033500980917     C                   ENDIF
033600980917     C* imposto pos. del cursore
033700980917     C                   Z-ADD     CURR          H1RIGA
033800980917     C                   Z-ADD     CURC          H1COLO
033900980917     C*
034000980917     C* Ricerca sottoconti (ANA250R)
034100980917     C     H1NMFL        WHENEQ    'D1KSR'
034200980917     C                   RESET                   ana250DS
034300980917     C                   MOVEL     '01'          OPZ250
034400980917     C                   MOVEL     §4mkcr        KCC250
034500980917     C                   clear                   KSC250
034600980917     C                   MOVEL     ana250DS      KPJBU
034700980917     C                   CALL      'ANA250R'
034800980917     C                   PARM                    KPJBA
034900980917     C                   MOVEL     KPJBU         ana250DS
035000980917     C     KSC250        IFNE      *BLANK
035100980917     C                   MOVEL     KCC250        §4mkcr
035200980917     C                   MOVEL     KSC250        §4mksr
035300980917     C                   ENDIF
035400980917     C* imposto pos. del cursore
035500980917     C                   Z-ADD     CURR          H1RIGA
035600980917     C                   Z-ADD     CURC          H1COLO
035700980722     C*
035800980722     C* ricerca non abilitata in questa posizione
0359009807221O   C                   OTHER
036000980722     C                   SETON                                        9899
036100980722     C* imposto pos. del cursore
036200980722     C                   Z-ADD     CURR          H1RIGA
036300980722     C                   Z-ADD     CURC          H1COLO
036400940127     C*
0365009401271-   C                   ENDSL
036600940127     C*
036700940506     C                   MOVE      *HIVAL        $LASTV
036800940127     C                   ENDSR
036900940615     C/EJECT
037000940615     C************************************************************
037100940615     C* TRADUZIONI
037200940615     C************************************************************
037300940615     C     LINGUA        BEGSR
037400940615     C*
037500940615     C* determino Riga/Colonna del cursore
037600940615     C                   MOVE      NRG           R$$
037700940615     C                   MOVE      NCL           C$$
037800940615     C                   Z-ADD     RIRI          CURR
037900940615     C                   Z-ADD     COCO          CURC
038000940615     C*
038100940615     C*
038200940615     C*
038300940615     C* FMTD1  - CAMPI DA TRADURRE
038400940615     C* imposto pos. del cursore
038500940615     C                   Z-ADD     CURR          H1RIGA
038600940615     C                   Z-ADD     CURC          H1COLO
038700940621     C                   MOVEL     H1NMFL        W005              5
038800940621     C                   MOVE      W005          W003              3
038900940621     C                   MOVEL     W003          W006              6
039000980731     C                   MOVEl     *blanks       KEY              15
039100980731     C                   MOVEl     D1COD         KEY
039200940615     C                   SELECT
039300940615     C*
039400940615     C*
039500940615     C* ricerca non abilitata in questa posizione
0396009406151O   C                   OTHER
039700940615     C                   SETON                                        98
039800940615     C*
0399009406151-   C                   ENDSL
040000940615     C*
040100940615     C                   MOVE      *HIVAL        $LASTV
040200940615     C                   ENDSR
040300940225     C/EJECT
040400940225     C************************************************************
040500940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
040600940225     C************************************************************
040700940225     C     GESPIE        BEGSR
040800940225     C*
040900940225     C                   SELECT
041000940225     C*
041100940506     C     $GEST         WHENEQ    'D1'
041200940506     C     $ULKD1        IFEQ      0
041300940506     C* riempimento opzioni
041400940506     C                   EXSR      F24D1
041500940506     C                   ELSE
041600940614     C                   WRITE     Z1
041700940506     C                   ENDIF
041800940225     C*
041900940510     C*
042000940225     C                   ENDSL
042100940225     C*
042200940225     C                   ENDSR
042300940131     C/EJECT
042400940131     C************************************************************
042500940510     C* GESTIONE VIDEO RECORD D1
042600940131     C************************************************************
042700940127     C     GESD1         BEGSR
042800940225     C* inizializzazione videata
042900940223     C     $INZD1        IFEQ      *ON
043000940127     C                   EXSR      INZD1
043100940223     C                   MOVE      *OFF          $INZD1
043200940117     C                   END
043300940225     C* emissione piede videata se proveniente da altra
043400940225     C* salvataggio valore formato di provenienza
043500940207     C     $LASTV        IFNE      'D1'
043600940614     C                   WRITE     T1
043700940225     C                   EXSR      GESPIE
043800940202     C                   MOVE      $LASTV        $LASTG
043900940202     C                   MOVE      'D1'          $LASTV
044000940202     C                   END
044100940131     C*
044200940203     C                   SELECT
044300940203     C* Annullamento
044400980722    >C     OPZ002        WHENEQ    '04'
044500940224     C* Visualizzazione
044600940315    >C     OPZ002        OREQ      '05'
044700940620     C                   SETON                                        0203
044800940620     C* Manutenzione
044900940620    >C     OPZ002        WHENEQ    '02'
045000940620     C                   SETON                                        02
045100940620     C                   SETOFF                                       03
045200940620     C*
045300940620     C                   OTHER
045400940620     C*
045500940620     C                   SETOFF                                       0203
045600940203     C                   ENDSL
045700940620     C* Immissione/Modifica/Copia
045800940620     C                   EXFMT     D1
045900940506     C* controllo abilitazione tasti funzione
046000940506     C*
046100940506     C                   EXSR      CTRKD1
046200940506     C*
046300940506     C     *IN99         IFEQ      *OFF
046400940131     C*
0465009402031    C                   SELECT
046600940510     c* ROLLUP
046700940131     C* F3=Fine
046800940506     C     $TASTO        WHENEQ    F03
046900940309     C                   EXSR      F03D1
047000940131     C* F4=Ricerca
047100940506     C     $TASTO        WHENEQ    F04
047200940127     C                   EXSR      SEARCH
047300940224     C* F5=Refresh
047400940506     C     $TASTO        WHENEQ    F05
047500940309     C                   EXSR      F05D1
047600940223     C* F8=Successivo
047700940506     C     $TASTO        WHENEQ    F08
047800940309     C                   EXSR      F08D1
047900940131     C* F12=Ritorno
048000940506     C     $TASTO        WHENEQ    F12
048100940309     C                   EXSR      F12D1
048200940315     C* F24=Altri tasti
048300940506     C     $TASTO        WHENEQ    F24
048400940315     C                   EXSR      F24D1
048500940131     C*
0486009402031O   C                   OTHER
048700940127     C                   EXSR      CTRD1
048800940616     C     *IN99         IFEQ      *OFF
048900940616     C                   SELECT
049000940616     C* F7=Traduzione
049100940616     C     $TASTO        WHENEQ    F07
049200940616     C                   EXSR      LINGUA
049300940616     C* F6
0494009406162    C     $TASTO        WHENEQ    F06
049500940224     C* eseguo aggiornamento
049600940315    >C                   EXSR      AGGANA
049700940224     C* eseguo operazioni del dopo-aggiornamento
0498009403183    C     *IN99         IFEQ      *OFF
049900940318     C                   EXSR      GESAGG
0500009403183-   C                   ENDIF
0501009406162-   C                   ENDSL
050200940616     C                   ENDIF
050300940203     C*
0504009402241-   C                   ENDSL
050500940224     C*
0506009405062-   C                   ENDIF
050700940117     C                   ENDSR
050800940506     C/EJECT
050900940506     C************************************************************
051000940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
051100940506     C************************************************************
051200940506     C     CTRKD1        BEGSR
051300940506     C                   SETOFF                                       99
051400940506     C                   MOVEA     $FC1          $FC
051500940506     C                   EXSR      CTRKEY
051600940506     C     *IN99         IFEQ      *ON
051700940506     C                   SETON                                        97
051800940506     C                   ENDIF
051900940506     C                   ENDSR
052000940117      ************************************************************
052100940207      * INIZIALIZZAZIONE VIDEATA DATI
052200940117      ************************************************************
052300940127     C     INZD1         BEGSR
052400940131     C*
052500940615     C                   CLEAR                   D1
052600941006     C                   MOVE      '0'           D1RIS
052700980731     C                   MOVEl     KEY002        D1COD
052800940615     C                   MOVE      AZI002        XTBAZI
052900940615     C                   MOVE      LIN002        XTBLIN
053000980731     C                   MOVE      *blanks       XTBKEY
053100980731     C                   MOVEl     KEY002        XTBKEY
053200970509     C                   MOVE      UNI002        XTBUNT
053300940615$007 C                   MOVE      'Y4M'         XTBCOD
053400940225     C*
053500940207     C*
053600940224     C* valorizzo la chiave univoca per reperire i dati
053700940224     C* se non immissione
0538009403151   >C     OPZ002        IFNE      '01'
053900940315    >C*-------------------------------------------------------------
054000940315    >C* impostare i campi chiave ricevuti
054100940315    >C*-------------------------------------------------------------
054200940318     C*
054300940614     C*
054400940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0545009406142   >C     OPZ002        IFEQ      '02'
054600940315    >C     OPZ002        OREQ      '04'
054700940614     C                   MOVE      '1'           XTBALC
054800940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0549009406142E   C                   ELSE
055000940614     C                   MOVE      '0'           XTBALC
0551009406142-   C                   ENDIF
055200940614     C*
055300940616     C                   MOVE      '1'           XTBRIC
055400970214     C                   CALLB     'XATB'
055500940614     C                   PARM                    XATBDS
055600940318     C*
055700940614     C     XTBERR        IFEQ      '0'
055800940614     C                   MOVEL     XTBUNI        EXTDS
055900970512     c                   move      xtbunt        d1uniTA           8
056000941108     C* Giro la data se non blank
056100970214     c                   move      xtbdsc        xtbdscA          10
056200970214     C     XTBDSCa       IFNE      DATA
056300970214     C                   CALLB     'XDT4000'
056400970214     C                   PARM      XTBDSCA       XDTAMG           10
056500940621     C                   PARM      *ZEROS        XDTGMA            6 0
056600940621     C                   PARM      2             XDTSTA            1 0
056700940621     C                   MOVE      XDTGMA        D1DSC
056800940712     C                   MOVE      XTBRIS        D1RIS
056900941108     C                   ELSE
057000941108     C                   MOVE      *ZEROS        D1DSC
057100941108     C                   ENDIF
057200941108     C                   ENDIF
057300980722     C*
057400980722     C* Giro la data attivazione procedura
057500980731     C                   MOVE      §4MDAT        G02INV
057600980722     C                   MOVEL     '3'           G02ERR
057700980722     C                   CALL      'XSRDA8'
057800980722     C                   PARM                    WLBDA8
057900980722     C                   Z-ADD     G02DAT        D1DAT
058000980722     C* Giro la data creazione file statistiche
058100980731     C                   MOVE      §4MDST        G02INV
058200980722     C                   MOVEL     '3'           G02ERR
058300980722     C                   CALL      'XSRDA8'
058400980722     C                   PARM                    WLBDA8
058500980722     C                   Z-ADD     G02DAT        D1DST
058600940614     C*
058700940224     C*
058800940224     C* se il rcd da gestire non viene trovato
058900940318     C* oppure è allocato da altro pgm
059000940224     C* ritorno al pgm guida avvertendolo dell'errore
0591009406142    C     XTBERR        IFNE      '0'
059200940224     C                   MOVE      *ON           $FINE
059300940315    >C                   MOVE      '1'           ERR002
059400940224     C*
059500940224     C* altrimenti
0596009402242E   C                   ELSE
059700940224     C* riempio la videata
059800940224     C                   EXSR      RIED1
059900940224     C* richiamo routine dei ctrl per decodificarla
060000940614    >C                   EXSR      CTRD1
060100960228     C                   MOVEL     *IN99         SAV99             1
060200960228     C* Se la gestione è più videate
060300960305    >C                   EXSR      CTRD2
060400960305    >C     *IN99         IFEQ      *ON
060500960305    >C                   MOVEL     *IN99         SAV99             1
060600960305    >C                   ENDIF
060700940224     C                   MOVE      *ALL'0'       IN5098
060800940224     C                   MOVEA     IN5098        *IN(50)
060900940224     C* definizione indicatori su video
061000940224     C                   SETOFF                                       02
0611009402243    C                   SELECT
061200940315    >C     OPZ002        WHENEQ    '02'
061300940224     C                   SETON                                        02
061400940315    >C     OPZ002        WHENEQ    '05'
061500940506     C* disabilita tasti
061600940720     C                   MOVE      '0'           $FC(6)
061700940720     C                   MOVE      '0'           $FC(5)
061800940720     C                   MOVE      '0'           $FC(4)
0619009402243-   C                   ENDSL
062000940224     C*
0621009402242-   C                   ENDIF
062200940223     C*
0623009402241-   C                   ENDIF
062400940224     C*
062500940117     C                   ENDSR
062600940207     C/EJECT
062700940207     C************************************************************
062800940510     C* RIEMPIMENTO VIDEATA  D1
062900940207     C************************************************************
063000940207     C     RIED1         BEGSR
063100940207     C*
063200940207     C*
063300940207     C                   ENDSR
063400940309     C/EJECT
063500940309     C************************************************************
063600940309     C* GESTIONE F03 VIDEO D1
063700940309     C************************************************************
063800940309     C     F03D1         BEGSR
063900940309     C*
064000940309     C                   MOVE      *ON           $FINE
064100940315    >C                   MOVE      '1'           RET002
064200940325     C* fine programma
064300940325     C                   EXSR      ENDPGM
064400940325     C*
064500940325     C                   ENDSR
064600940309     C/EJECT
064700940309     C************************************************************
064800940309     C* GESTIONE F05 VIDEO D1
064900940309     C************************************************************
065000940309     C     F05D1         BEGSR
065100940309     C*
065200940309     C                   MOVE      *ON           $INZD1
065300940309     C*
065400940309     C                   ENDSR
065500940309     C/EJECT
065600940309     C************************************************************
065700940309     C* GESTIONE F08 VIDEO D1
065800940309     C************************************************************
065900940309     C     F08D1         BEGSR
066000940309     C*
066100940309     C                   MOVE      *ON           $FINE
066200940315    >C                   MOVE      '0'           RET002
066300940309     C*
066400940309     C                   ENDSR
066500940309     C/EJECT
066600940309     C************************************************************
066700940309     C* GESTIONE F12 VIDEO D1
066800940309     C************************************************************
066900940309     C     F12D1         BEGSR
067000940309     C*
067100940309     C                   MOVE      *ON           $FINE
067200940315    >C                   MOVE      '2'           RET002
067300940309     C*
067400940309     C                   ENDSR
067500940309     C/EJECT
067600940309     C************************************************************
067700940309     C* GESTIONE F24 VIDEO D1
067800940309     C************************************************************
067900940309     C     F24D1         BEGSR
068000940309     C*
068100940506     C*
068200940506     C                   MOVEA     $FX1          $FX
068300940506     C                   MOVEA     $FC1          $FC
068400940506     C                   MOVEA     $FR1          $FR
068500940506     C                   Z-ADD     $ULKD1        $ULK
068600940506     C                   EXSR      RIEKEY
068700941017     C                   MOVEA     $FC           $FC1
068800940506     C                   Z-ADD     $ULK          $ULKD1            3 0
068900940506     C                   MOVEL     $KEY1         Z1KE1
069000940506     C                   MOVEL     $KEY2         Z1KE2
069100940614     C                   WRITE     Z1
069200940506     C*
069300940309     C                   ENDSR
069400940207     C/EJECT
069500940131     C************************************************************
069600940207     C* CONTROLLO VIDEATA
069700940131     C************************************************************
069800940127     C     CTRD1         BEGSR
069900940131     C*
070000980722     C                   SETOFF                                       99
070100940127     C* non imposto pos. del cursore
070200940127     C                   Z-ADD     0             H1RIGA
070300940127     C                   Z-ADD     0             H1COLO
070400940621     C*
070500940621     C* CTRL DATA
070600940621     C*
070700941109     C                   MOVE      DATA          WRKDSC           10
070800940621     C     D1DSC         IFNE      0
070900970214     C                   CALLB     'XDT4000'
071000941109     C                   PARM                    XDTAMG           10
071100940621     C                   PARM      D1DSC         XDTGMA            6 0
071200940621     C                   PARM      3             XDTSTA            1 0
071300940621     C     XDTSTA        IFNE      0
071400980722     C                   SETON                                        5599
071500940621     C                   ELSE
071600941108     C                   MOVE      XDTAMG        WRKDSC
071700940621     C                   ENDIF
071800940621     C                   ENDIF
071900940223     C*
072000940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
072100940223     C*
0722009403151   >C     OPZ002        IFNE      '04'
072300971110     C*
072400971110     C* controllo codice vuoto
072500981014$017 C     D1COD         IFEQ      *blanks
072600971110     C                   SETON                                        5099
072700971110     C                   ENDIF
072800971110     C*
072900980722     C* Controllo causale 1
073000980722     c                   clear                   d1dcd1
073100980722     c*
073200980722     c                   if        §4Mcd1        <> *blanks
073300980722     c                   movel     §4mcd1        kkcau
073400110916     c     k02ope01      chain     anope01l      anope000_cd1         64
073500110916     c                   if        *in64
073600980722     C                   seton                                        9981
073700980722     C                   else
073800110916     c                   eval      d1dcd1 = anope000_cd1.opedesbrev
073900110916     c                   if        anope000_cd1.opeTpc <> '1' or
074000110916     c                             anope000_cd1.opeStp <> 'C' or
074100110916     c                             anope000_cd1.opeTpDocI <> '1'
074200110916     c                   seton                                        9981
074300110916     c                   endif
074400980722     c                   endif
074500980722     C                   else
074600980722     c                   Seton                                        9984
074700980722     c                   endif
074800980722     C*
074900980722     C* Controllo causale 2
075000110916     c                   clear                   d1dcd2
075100110916     c
075200110916     c                   if        §4Mcd2        <> *blanks
075300110916     c                   movel     §4mcd2        kkcau
075400110916     c     k02ope01      chain     anope01l      anope000_cd2         64
075500110916     c                   if        *in64
075600110916     C                   seton                                        9982
075700110916     C                   else
075800110916     c                   eval      d1dcd2 = anope000_cd2.opedesbrev
075900110916     c                   if        anope000_cd2.opeTpc <> '1' or
076000110916     c                             anope000_cd2.opeStp <> 'C' or
076100110916     c                             anope000_cd2.opeTpDocI <> '1' or
076200110916     c                             anope000_cd2.opeCtPar = '1' or
076300110916     c                             anope000_cd2.opeLibro <>
076400110916     c                                                     anope000_cd1.opeLibro
076500110916     c                   seton                                        9982
076600110916     c                   endif
076700110916     c                   endif
076800110916     C                   else
076900110916     c                   Seton                                        9985
077000110916     c                   endif
077100110916     C
077200980722     C* Controllo causale 3
077300980722     c                   clear                   d1dcd3
077400980722     c*
077500980722     c                   if        §4Mcd3        <> *blanks
077600980722     c                   movel     §4mcd3        kkcau
077700110916     c     k02ope01      chain     anope01l      anope000_cd3         64
077800980722     c                   if        *in64
077900980722     C                   seton                                        9983
078000980722     C                   else
078100110916     c                   eval      d1dcd3 = anope000_cd3.opedesbrev
078200110916     c                   if        anope000_cd3.opeTpc <> '1' or
078300110916     c                             anope000_cd3.opeStp <> 'C' or
078400110916     c                             anope000_cd3.opeTpDocI <> '1'
078500110916     c                   seton                                        9983
078600110916     c                   endif
078700980722     c                   endif
078800980722     C                   else
078900980722     c                   Seton                                        9986
079000980722     c                   endif
079100980722     C*
079200980722     C* Controllo aliquota/Esenzione
079300980722     c*
079400980722     c                   if        §4Mese<>*blanks and §4Miva<>*zeros or
079500980722     c                             §4Mese=*blanks  and §4Miva=*zeros
079600980722     c                   Seton                                        9987
079700980722     C                   else
079800980722     C* Controllo codice di esenzione
079900980722     c                   if        §4Mese<>*blanks
080000980722     c                   move      xscsoc        ksocieta
080100980722     c                   move      '2'           ktpreg
080200980722     c                   movel     *blanks       kaliq
080300980722     c                   movel     §4Mese        krifiva
080400980722     c     k02riv01      chain     anriv01l                           64
080500980722     c                   if        *in64
080600980722     C                   seton                                        9989
080700980722     c                   endif
080800980722     C* Controllo aliquota
080900980722     C                   else
081000980722     c                   if        §4Miva<>*zeros
081100980722     c                   move      xscsoc        ksocieta
081200980722     c                   move      '2'           ktpreg
081300980722     c                   movel     §4miva        kaliq
081400980722     c                   movel     *blanks       krifiva
081500980722     c     k02riv01      chain     anriv01l                           64
081600980722     c                   if        *in64
081700980722     C                   seton                                        9988
081800980722     c                   endif
081900980722     c                   endif
082000980722     c                   endif
082100980722     c                   endif
082200980722     C*
082300980722     C* CTRL DATA attivazione procedura
082400980722     C     D1DAT         IFNE      0
082500980722     C                   MOVE      D1DAT         G02DAT
082600980722     C                   MOVE      *BLANKS       G02ERR
082700980722     C                   CALL      'XSRDA8'
082800980722     C                   PARM                    WLBDA8
082900980722     C     G02ERR        IFEQ      '1'
083000980722     C                   SETON                                        8099
083100980722     C                   ELSE
083200980722     C                   MOVE      G02INV        §4MDAT
083300980722     C                   MOVE      G02DAT        D1DAT
083400980722     C                   ENDIF
083500980722     C                   ELSE
083600980805     C                   MOVE      *zeros        §4MDAT
083700980722     C                   ENDIF
083800980722     C*
083900980722     C* CTRL DATA attivazione creazione file statische
084000980722     C     D1DST         IFNE      0
084100980722     C                   MOVE      D1DST         G02DAT
084200980722     C                   MOVE      *BLANKS       G02ERR
084300980722     C                   CALL      'XSRDA8'
084400980722     C                   PARM                    WLBDA8
084500980722     C     G02ERR        IFEQ      '1'
084600980722     C                   SETON                                        7999
084700980722     C                   ELSE
084800980722     C                   MOVE      G02INV        §4MDST
084900980722     C                   MOVE      G02DAT        D1DST
085000980722     C                   ENDIF
085100980722     C                   ELSE
085200980805     C                   MOVE      *zeros        §4MDST
085300980722     C                   ENDIF
085400980917     C*
085500980917     c                   clear                   d1dksr
085600980917     c*
085700980917     c                   if        §4mkcr         <> *blanks   and
085800980917     c                             §4mksr         <> *blanks
085900980917     c                   movel     §4mkcr        rcokcc
086000980917     C                   movel     §4mksr        rcoksc
086100980917     C     K03rco99      CHAIN     anrco99j                           90
086200980917     C**n90rcosogg       chain     ansog01l                           90
086300980917     C     *IN90         IFEQ      '1'
086400980917     C                   SETON                                        9968
086500980917     C                   clear                   d1dksr
086600980917     C                   ELSE
086700980917     C                   MOVEL     pdcdesbrev    d1dksr
086800980917     C                   END
086900980917     C                   ELSE
087000980917     C                   SETON                                        9965
087100980917     C                   END
087200940207     C*
087300940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
087400940315    >C     OPZ002        IFEQ      '01'
087500940315    >C     OPZ002        OREQ      '03'
087600940315    >C*-------------------------------------------------------------
087700940315    >C* impostare i campi chiave e verificare l'esistenza del record
087800940315    >C*-------------------------------------------------------------
087900971110$007 C                   MOVE      'Y4M'         XTBCOD
088000980731     C                   MOVE      *BLANKS       XTBKEY
088100980731    >C                   MOVEL     D1COD         XTBKEY
088200970512    >C                   MOVE      D1UNITA       XTBUNT
088300940614     C                   MOVE      '0'           XTBALC
088400940614     C                   MOVE      '1'           XTBRIC
088500970214    >C                   CALLB     'XATB'
088600940614     C                   PARM                    XATBDS
088700940614     C     XTBERR        IFEQ      '0'
088800940223     C                   SETON                                        5999
088900940223     C                   ENDIF
089000940620     C* Controllo se compreso nei range indice
089100940620     C     XTBERR        IFEQ      '3'
089200940620    >C                   SETON                                        5899
089300940620     C                   END
089400940620     C*
089500940620     C* REPERISCO AUTORIZZAZIONI UTENTE
089600940620     C                   CLEAR                   XCHATB
089700941118     C                   MOVE      XSCSOC        XCTSOC
089800941118     C                   MOVE      XSCDUN        XCTUNI
089900940620     C                   MOVEL     KNMUS         XCTPRF
090000940620     C                   MOVE      XTBCOD        XCTCOD
090100940620     C                   CALL      'XCHKATB'
090200940620     C                   PARM                    XCHATB
090300941111     C                   MOVEL     XCTDDS        AUTDS
090400941027     C     XCTRTN        IFEQ      1
090500941027     C                   MOVEL     'N'           MANGRU
090600941027     C                   ENDIF
090700940620     C* Non ammessa immissione di gruppo
090800940620     C     XTBGRU        IFEQ      'G'
090900940620     C     MANGRU        ANDEQ     'N'
091000940620     C                   SETON                                        5799
091100940620     C                   END
091200940620     C* Non ammessa immissione di azienda
091300940620     C     XTBGRU        IFEQ      'A'
091400940620     C     MANAZI        ANDEQ     'N'
091500940620     C                   SETON                                        5699
091600940620     C                   END
091700940712     C* codice riservato non accessibile
091800941006     C     XTBRIS        IFGT      ABIRIS
091900941005    >C     OPZ002        ANDNE     '01'
092000941005    >C     OPZ002        ANDNE     '03'
092100980722     C                   SETON                                        6499
092200940712     C                   END
092300940224     C                   ENDIF
092400940224     C*
0925009402241-   C                   END
092600940131     C*
092700940302     C     *IN99         IFEQ      *ON
092800940510     C     $GEST         ANDEQ     'D1'
092900960228     C     OPZ002        ANDNE     '07'
093000940117     C                   MOVEA     *IN           WIN
093100940128     C                   MOVE      *ALL'0'       IN5098           49
093200940128     C                   MOVEA     IN5098        *IN(50)
093300940614     C                   WRITE     D1
093400940117     C                   MOVEA     WIN           *IN
093500940302     C                   ENDIF
093600940127     C*
093700940117     C                   ENDSR
093800960305     C/EJECT
093900960305     C************************************************************
094000960305     C* CONTROLLO VIDEATA D2
094100960305     C************************************************************
094200960305     C     CTRD2         BEGSR
094300960305     C*
094400960305     C                   ENDSR
094500940207     C/EJECT
094600940224     C************************************************************
094700940315    >C* AGGIORNAMENTO ANAGRAFICA
094800940224     C************************************************************
094900940315    >C     AGGANA        BEGSR
095000971110     C*
095100971110$007 C                   MOVE      'Y4M'         XTBCOD
095200980731     C                   MOVE      *blanks       XTBKEY
095300980731     C                   MOVEl     D1COD         XTBKEY
095400970512     C                   MOVE      D1UNITA       XTBUNT
095500940207     C*
095600940207     C                   SELECT
095700940315    >C     OPZ002        WHENEQ    '01'
095800940315    >C     OPZ002        OREQ      '03'
095900940315    >C                   EXSR      RIEANA
096000950609     C                   MOVE      XSCCUT        XTBUTE
096100940614     C                   MOVE      '5'           XTBRIC
096200970214     C                   CALLB     'XATB'
096300940614    >C                   PARM                    XATBDS
096400940318     C* se rcd non scrivibile attivo errore
096500940615     C     XTBERR        IFEQ      '1'
096600940318    >C                   SETON                                        5999
096700940318     C                   END
096800940315    >C     OPZ002        WHENEQ    '02'
096900940621     C                   MOVE      '1'           XTBRIC
097000940621     C                   MOVE      '1'           XTBALC
097100970214     C                   CALLB     'XATB'
097200940621     C                   PARM                    XATBDS
097300940622    >C                   EXSR      RIEANA
097400950609     C                   MOVE      XSCCUT        XTBUTE
097500940614     C                   MOVE      '4'           XTBRIC
097600970214     C                   CALLB     'XATB'
097700940614    >C                   PARM                    XATBDS
097800940315    >C     OPZ002        WHENEQ    '04'
097900940621     C                   MOVE      '1'           XTBRIC
098000940621     C                   MOVE      '1'           XTBALC
098100970214     C                   CALLB     'XATB'
098200940621     C                   PARM                    XATBDS
098300940614     C                   MOVE      '6'           XTBRIC
098400970214     C                   CALLB     'XATB'
098500940614    >C                   PARM                    XATBDS
098600940621     C* annullo traduzioni
098700940621     C     XTBERR        IFEQ      '0'
098800940621     C                   EXSR      ANNTBT
098900940621     C                   END
099000940621     C*
099100940207     C                   ENDSL
099200940203     C*
099300940203     C                   ENDSR
099400940224     C/EJECT
099500940224     C************************************************************
099600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
099700940224     C************************************************************
099800940616     C     RIEANA        BEGSR
099900940616     C                   CLEAR                   XTBUNI
100000940621     C                   MOVE      WRKDSC        XTBDSC
100100940712     C                   MOVE      D1RIS         XTBRIS
100200940616     C                   MOVEL     EXTDS         XTBUNI
100300940207     C                   ENDSR
100400940224     C/EJECT
100500940224     C************************************************************
100600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
100700940224     C************************************************************
100800940224     C     GESAGG        BEGSR
100900940224     C*
101000940224     C* segnalo al pgm chiamante l'avvenuta conferma
101100940315    >C                   MOVE      *ON           OPR002
101200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
101300940315    >C                   MOVE      '0'           RET002
101400940224     C*
1015009402241    C                   SELECT
101600940224     C* nel caso di immissione, ripristino la videata iniziale
101700940315    >C     OPZ002        WHENEQ    '01'
101800940224     C                   MOVE      *ON           $INZD1
101900940224     C* altrimenti ritorno al pgm chiamante
102000940224     C                   OTHER
102100940224     C                   MOVE      *ON           $FINE
1022009402241-   C                   ENDSL
102300940224     C*
102400940224     C                   ENDSR
102500940621     C/EJECT
102600940621     C************************************************************
102700940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
102800940621     C************************************************************
102900940621    >C     ANNTBT        BEGSR
103000940621     C                   MOVE      XTBAZI        TBTAZI
103100940621     C                   MOVE      XTBCOD        TBTCOD
103200940621     C                   MOVE      XTBKEY        TBTKEY
103300940621     C     KTBT          SETLL     ANTBT01L
103400940621     C                   DO        *HIVAL
103500940621     C     KTBT          DELETE    ANTBT01L                           21
103600940621     C  N21              END
103700940621     C*
103800940621     C                   ENDSR
103900940207     C/EJECT
104000940131     C************************************************************
104100940131     C* OPERAZIONI INIZIALI
104200940131     C************************************************************
104300940131     C     *INZSR        BEGSR
104400940131     C*
104500940127     C* Reperimento parametri
104600940127     C*
104700940117     C     *ENTRY        PLIST
104800940117     C                   PARM                    KPJBA
104900940712     C*
105000940712     C* Reperimento Codice utente
105100940712     C*
105200940223     C*
105300940506     C*
105400940506     C* Reperimento tasti di funzione
105500940506     C*
105600940506     C                   EXSR      REPKEY
105700940621     C*
105800940621     C* KLIST
105900940621     C*
106000980917     C     K03Rco99      KLIST
106100980917     C                   KFLD                    xscsoc
106200980917     C                   KFLD                    rcokcc
106300980917     C                   KFLD                    rcoksc
106400940621     C     KTBT          KLIST
106500940621     C                   KFLD                    TBTAZI
106600940621     C                   KFLD                    TBTCOD
106700940621     C                   KFLD                    TBTKEY
106800980722     C*                                                    *
106900980722     c     K02ope01      klist
107000980722     c                   kfld                    Xscsoc
107100980722     c                   kfld                    kkcau             4
107200980722     C*
107300980722     c     K02riv01      klist
107400980722     c                   kfld                    ksocieta          3
107500980722     c                   kfld                    ktpreg            1
107600980722     c                   kfld                    kaliq             5 2
107700980722     c                   kfld                    krifiva           3
107800940506     C*
107900940223     C                   ENDSR
108000941103     C/EJECT
108100941103      ************************************************************
108200941103      * Reperimento dati società
108300941103      ************************************************************
108400941103     C     REPSOC        BEGSR
108500941103      *
108600970214     C                   CALLB     'XSOC'
108700941103     C                   PARM                    TIPXSC            6
108800941103     C                   PARM                    SOCXSC            3
108900941103     C                   PARM                    CDSXSC            9 0
109000941103     C                   PARM                    MODXSC            3
109100941103     C                   PARM      *BLANKS       RTNXSC            1
109200941103     C                   PARM                    XSOCDS
109300941103     C                   PARM                    KPJBA
109400941103      *
109500941103     C                   ENDSR
109600940223     C/EJECT
109700940506     C************************************************************
109800940506     C* REPERIMENTO TASTI DI FUNZIONE
109900940506     C************************************************************
110000940506     C     REPKEY        BEGSR
110100940506     C                   MOVEA     $FM1          $FM
110200940506     C                   EXSR      REPKKK
110300940506     C                   MOVEA     $FX           $FX1
110400940614     C                   ENDSR
110500940506     C/EJECT
110600940506     C************************************************************
110700940506     C* REPERIMENTO TASTI DI FUNZIONE
110800940506     C************************************************************
110900940506     C     REPKKK        BEGSR
111000940506     C*
111100940506     C                   CLEAR                   $FX
111200940506     C     1             DO        24            X
111300940506     C*
111400940506     C     $FM(X)        IFNE      *BLANK
111500940506     C*
111600940506     C                   MOVEL     $FM(X)        IDMSG
111700970214     C                   CALLB     'XRTVM'
111800940506     C                   PARM                    IDMSG            27
111900940506     C                   PARM                    TXTMSG          100
112000940506     C                   PARM                    ERRMSG            1
112100940506     C*
112200940506     C     ERRMSG        IFNE      *ON
112300940506     C                   MOVEL     TXTMSG        $FX(X)
112400940506     C                   ELSE
112500940506     C                   MOVEL     *ALL' '       $FX(X)
112600940506     C                   END
112700940506     C*
112800940506     C                   END
112900940506     C*
113000940506     C                   END
113100940506     C*
113200940506     C                   ENDSR
113300940509     C/EJECT
113400940223     C************************************************************
113500940223     C* INIZIALIZZAZIONE VARIABILI
113600940223     C************************************************************
113700940223     C     INZVAR        BEGSR
113800951124     C*
113900951124     C* Chiamata XSOC
114000951124     C*
114100951124     C                   MOVEL     'SOC001'      TIPXSC
114200951124     C                   EXSR      REPSOC
114300951124     C     RTNXSC        IFNE      '1'
114400951124     C                   MOVEL     XSOCDS        SOC001
114500951124     C                   ELSE
114600951124     C                   MOVE      *ON           $FINE
114700951124     C* fine programma
114800951124     C                   EXSR      ENDPGM
114900951124     C                   ENDIF
115000940224     C*
115100940224     C* Dati da PGM guida
115200940224     C*
115300940315    >C                   MOVEL     KPJBU         D002DS
115400940128     C*
115500940223     C* Variabili per gestione videate
115600940128     C*
115700940223     C                   MOVE      'D1'          $GEST
115800940223     C                   MOVE      *OFF          $FINE
115900940223     C                   MOVE      *ON           $INZD1
116000940224     C                   MOVE      *BLANK        $LASTG
116100940224     C                   MOVE      *BLANK        $LASTV
116200940506     C                   Z-ADD     0             $ULKD1            3 0
116300940127     C*
116400940223     C* Variabili appoggio
116500940127     C*
116600940224     C                   Z-ADD     0             CURR
116700940224     C                   Z-ADD     0             CURC
116800940207     C*
116900940207     C* Valorizzazione campi univoci testate
117000940207     C*
117100940614     C                   CLEAR                   T1
117200940614     C                   MOVEL     KNSIF         NOMSIF
117300941109     C                   MOVEL     XSCDSI        NOMDIT
117400941027     C                   MOVEL     DSPGM         NOMPGM
117500940224     C                   Z-ADD     1             X
117600940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
117700940224     C     *IN21         IFEQ      *ON
117800940224     C                   MOVEL     $ID(X)        IDMSG
117900970214     C                   CALLB     'XRTVM'
118000940224     C                   PARM                    IDMSG            27
118100940224     C                   PARM                    TXTMSG          100
118200940224     C                   PARM                    ERRMSG            1
118300940224     C                   PARM      *ON           CTRMSG            1
118400940224     C                   PARM      30            LENMSG            3 0
118500940224     C     ERRMSG        IFNE      *ON
118600940224     C                   MOVEL     TXTMSG        T1OPE
118700940224     C                   ELSE
118800940224     C                   MOVEL     *ALL'?'       T1OPE
118900940224     C                   ENDIF
119000940224     C                   END
119100940224     C*
119200940224     C* Inizializzazione/Decodifica prima videata
119300940224     C*
119400940224     C                   EXSR      INZD1
119500940224     C                   MOVE      *OFF          $INZD1
119600960228     C* Se opzione = '07' richiesti solo controlli
119700960228     C     OPZ002        IFEQ      '07'
119800960228     C                   MOVEL     *IN99         ERR002
119900960228     C                   MOVEL     '0'           RET002
120000960228     C                   MOVEL     *ON           $FINE
120100960228     C                   ENDIF
120200940127     C*
120300940117     C                   ENDSR
120400940128     C************************************************************
120500940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
120600940223     C************************************************************
120700940223     C     DEFCAM        BEGSR
120800940223     C*
120900940223     C* klist
121000940207     C*
121100940223     C*
121200940223     C* Variabili per gestione videate
121300940223     C*
121400940223     C                   MOVE      *BLANK        $GEST             2
121500940223     C                   MOVE      *BLANK        $FINE             1
121600940223     C                   MOVE      *BLANK        $INZD1            1
121700940223     C                   MOVE      *BLANK        $LASTG            2
121800940223     C                   MOVE      *BLANK        $LASTV            2
121900940223     C*
122000940223     C* Variabili appoggio
122100940223     C*
122200940223     C                   Z-ADD     0             CURR              2 0
122300940223     C                   Z-ADD     0             CURC              2 0
122400940223     C                   MOVE      *ZEROS        WIN              99
122500940224     C                   MOVE      *BLANK        WTBERR            1
122600940223     C*
122700940223     C* Indici
122800940223     C*
122900940223     C                   Z-ADD     0             X                 3 0
123000940223     C                   Z-ADD     0             Y                 3 0
123100940127     C*
123200940117     C                   ENDSR
123300940128     C************************************************************
123400940509     C/EJECT
123500940509     C************************************************************
123600940509     C* CONTROLLO ABILITAZIONE TASTI
123700940509     C************************************************************
123800940509     C     CTRKEY        BEGSR
123900940509     C                   SELECT
124000940509     C     $TASTO        WHENEQ    F01
124100940720     C     $FC(1)        IFEQ      '0'
124200940509     C                   SETON                                        99
124300940509     C                   ENDIF
124400940509     C     $TASTO        WHENEQ    F02
124500940720     C     $FC(2)        IFEQ      '0'
124600940509     C                   SETON                                        99
124700940509     C                   ENDIF
124800940509     C     $TASTO        WHENEQ    F03
124900940720     C     $FC(3)        IFEQ      '0'
125000940509     C                   SETON                                        99
125100940509     C                   ENDIF
125200940509     C     $TASTO        WHENEQ    F04
125300940720     C     $FC(4)        IFEQ      '0'
125400940509     C                   SETON                                        99
125500940509     C                   ENDIF
125600940509     C     $TASTO        WHENEQ    F05
125700940720     C     $FC(5)        IFEQ      '0'
125800940509     C                   SETON                                        99
125900940509     C                   ENDIF
126000940509     C     $TASTO        WHENEQ    F06
126100940720     C     $FC(6)        IFEQ      '0'
126200940509     C                   SETON                                        99
126300940509     C                   ENDIF
126400940509     C     $TASTO        WHENEQ    F07
126500940720     C     $FC(7)        IFEQ      '0'
126600940509     C                   SETON                                        99
126700940509     C                   ENDIF
126800940509     C     $TASTO        WHENEQ    F08
126900940720     C     $FC(8)        IFEQ      '0'
127000940509     C                   SETON                                        99
127100940509     C                   ENDIF
127200940509     C     $TASTO        WHENEQ    F09
127300940720     C     $FC(09)       IFEQ      '0'
127400940509     C                   SETON                                        99
127500940509     C                   ENDIF
127600940509     C     $TASTO        WHENEQ    F10
127700940720     C     $FC(10)       IFEQ      '0'
127800940509     C                   SETON                                        99
127900940509     C                   ENDIF
128000940509     C     $TASTO        WHENEQ    F11
128100940720     C     $FC(11)       IFEQ      '0'
128200940509     C                   SETON                                        99
128300940509     C                   ENDIF
128400940509     C     $TASTO        WHENEQ    F12
128500940720     C     $FC(12)       IFEQ      '0'
128600940509     C                   SETON                                        99
128700940509     C                   ENDIF
128800940509     C     $TASTO        WHENEQ    F13
128900940720     C     $FC(13)       IFEQ      '0'
129000940509     C                   SETON                                        99
129100940509     C                   ENDIF
129200940509     C     $TASTO        WHENEQ    F14
129300940720     C     $FC(14)       IFEQ      '0'
129400940509     C                   SETON                                        99
129500940509     C                   ENDIF
129600940509     C     $TASTO        WHENEQ    F15
129700940720     C     $FC(15)       IFEQ      '0'
129800940509     C                   SETON                                        99
129900940509     C                   ENDIF
130000940509     C     $TASTO        WHENEQ    F16
130100940720     C     $FC(16)       IFEQ      '0'
130200940509     C                   SETON                                        99
130300940509     C                   ENDIF
130400940509     C     $TASTO        WHENEQ    F17
130500940720     C     $FC(17)       IFEQ      '0'
130600940509     C                   SETON                                        99
130700940509     C                   ENDIF
130800940509     C     $TASTO        WHENEQ    F18
130900940720     C     $FC(18)       IFEQ      '0'
131000940509     C                   SETON                                        99
131100940509     C                   ENDIF
131200940509     C     $TASTO        WHENEQ    F19
131300940720     C     $FC(19)       IFEQ      '0'
131400940509     C                   SETON                                        99
131500940509     C                   ENDIF
131600940509     C     $TASTO        WHENEQ    F20
131700940720     C     $FC(20)       IFEQ      '0'
131800940509     C                   SETON                                        99
131900940509     C                   ENDIF
132000940509     C     $TASTO        WHENEQ    F21
132100940720     C     $FC(21)       IFEQ      '0'
132200940509     C                   SETON                                        99
132300940509     C                   ENDIF
132400940509     C     $TASTO        WHENEQ    F22
132500940720     C     $FC(22)       IFEQ      '0'
132600940509     C                   SETON                                        99
132700940509     C                   ENDIF
132800940509     C     $TASTO        WHENEQ    F23
132900940720     C     $FC(23)       IFEQ      '0'
133000940509     C                   SETON                                        99
133100940509     C                   ENDIF
133200940509     C     $TASTO        WHENEQ    F24
133300940720     C     $FC(24)       IFEQ      '0'
133400940509     C                   SETON                                        99
133500940509     C                   ENDIF
133600940509     C                   ENDSL
133700940509     C                   ENDSR
133800940509     C/EJECT
133900940509     C************************************************************
134000940509     C* RIEMPIMENTO TASTI FUNZIONE
134100940509     C************************************************************
134200940509     C     RIEKEY        BEGSR
134300940509     C*
134400940509     C* Input :
134500940509     C* - $FX = schiera con tutti i testi disponibili
134600940509     C* - $FC = schiere con i flag di validità associati ai testi
134700940509     C* - $FR = schiere con i flag di REVERSE IMAGE
134800940509     C* - $ULK = ultimo testo caricato
134900940509     C* Output :
135000940509     C* - $ULK
135100940509     c* - $KEY1= 1° riga di tasti funzione
135200940509     c* - $KEY2= 2° riga di tasti funzione
135300940509     C*
135400970214     C                   CALLB     'XKEYMSG'
135500940509     C* input
135600940509     C                   PARM                    $FX
135700940509     C                   PARM                    $FC
135800940509     C                   PARM                    $FR
135900940509     C                   PARM                    $ULK              3 0
136000940509     C* output
136100940509     C                   PARM                    $KEY1            79
136200940509     C                   PARM                    $KEY2            79
136300940509     C*
136400940509     C                   ENDSR
136500940615**
13660094022401PROMSG    *LIBL     COS0001    immissione
13670094022402PROMSG    *LIBL     COS0002    modifica
13680094022403PROMSG    *LIBL     COS0003    copia
13690094022404PROMSG    *LIBL     COS0004    annullamento
13700094022405PROMSG    *LIBL     COS0005    visualizzazione
137100940509** TASTI DI FUNZIONE  D1
137200940509PROMSG    *LIBL     KEY0001  01
137300940509PROMSG    *LIBL     KEY0002  02
137400940509PROMSG    *LIBL     KEY0003  03
137500940509PROMSG    *LIBL     KEY0004  04
137600940509PROMSG    *LIBL     KEY0005  05
137700940509PROMSG    *LIBL     KEY0006  06
137800940509PROMSG    *LIBL     KEY0007  07
137900940509PROMSG    *LIBL     KEY0008  08
138000940509PROMSG    *LIBL     KEY0009  09
138100940509PROMSG    *LIBL     KEY0010  10
138200940509PROMSG    *LIBL     KEY0011  11
138300940509PROMSG    *LIBL     KEY0012  12
138400940509PROMSG    *LIBL     KEY0013  13
138500940509PROMSG    *LIBL     KEY0014  14
138600940509PROMSG    *LIBL     KEY0015  15
138700940509PROMSG    *LIBL     KEY0016  16
138800940509PROMSG    *LIBL     KEY0017  17
138900940509PROMSG    *LIBL     KEY0018  18
139000940509PROMSG    *LIBL     KEY0019  19
139100940509PROMSG    *LIBL     KEY0020  20
139200940509PROMSG    *LIBL     KEY0021  21
139300940509PROMSG    *LIBL     KEY0022  22
139400940509PROMSG    *LIBL     KEY0023  23
139500940509PROMSG    *LIBL     KEY0024  24
139600940509**  ABILITAZIONE D1
1397009407200N  01
1398009407200N  02
1399009407201N  03
1400009407201N  04
1401009407201N  05
1402009407201N  06
1403009407201N  07
1404009407201N  08
1405009407200N  09
1406009407200N  10
1407009407200N  11
1408009407201N  12
1409009407200N  13
1410009407200N  14
1411009407200N  15
1412009407200N  16
1413009407200N  17
1414009407200N  18
1415009407200N  19
1416009407200N  20
1417009407200N  21
1418009407200N  22
1419009407200N  23
1420009407200N  24
