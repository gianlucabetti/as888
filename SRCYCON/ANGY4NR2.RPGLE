000100070206     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000200070206     H CVTOPT(*DATETIME)
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
004100940614$001 FANGY4ND2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
004400980722     FAnSog01L  IF   E           K DISK
004500130412     Fanope01l  IF   E           K DISK
004600980722     FAnVci01L  IF   E           K DISK
004700980722     FAnRiv02L  IF   E           K DISK
004800980722     FAnRiv03L  IF   E           K DISK    rename(anriv000:anriv03)
004900980722     FAnRco99J  IF   E           K DISK
005000070118     Fansif01l  IF   E           K DISK    USROPN
005100940128     D*----------------------------------------------------*
005200940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005300940224     D $ID             S             27    DIM(5) ALT($OZ)
005400940506     D* Schiere per la gestione dei tasti di funzione
005500940506     D $FM             S             27    DIM(24)
005600940506     D $FX             S             50    DIM(24)
005700940506     D $FC             S              1    DIM(24)
005800940506     D $FR             S              1    DIM(24)
005900940506     D* Video D1
006000940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006100940506     D $FX1            S             50    DIM(24) ALT($FM1)
006200940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006300940506     D $FR1            S              1    DIM(24) ALT($FC1)
006400940615     D* campi in lingua
006500070206$010 D LIN             S             06    DIM(0011) CTDATA PERRCD(001)
006600070206$010 D LIND            S             50    DIM(0011) ALT(LIN)
006700940614$003 D EXTDS         E DS                  EXTNAME(ANGY4NDS) INZ
006800070206     D* Passaggio Parametri
006900940127     D KPJBA         E DS
007000941103     D*-------------
007100941103      * DS Società
007200941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
007300960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
007400960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
007500941103     D*-------------
007600941103      * DS Interna per dati utente
007700941103     D XSOCDS          DS          1000
007800941103     D*-------------
007900940616     D XATBDS        E DS                  INZ
008000130412     D a070ds        E DS                  extname(ana070ds) inz
008100940318     D*-------------
008200940318     D* DS Esterna gestione messaggi per COMIT
008300940616     D XMSGDS        E DS                  INZ
008400940318     D  STS          E                     EXTFLD(MSGSTS)
008500940318     D                                     DIM(333)
008600940318     D  JBA          E                     EXTFLD(MSGJBA)
008700940318     D                                     DIM(502)
008800940203     D*-------------
008900940620     D* DS Esterna autorizzazioni tabelle
009000940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
009100940620     D*
009200940620     D* DS  restituzione autorizzazioni
009300940620     D* IND=indice
009400940620     D* GRU=man. di gruppo
009500940620     D* AZI=man. azienda
009600940712     D* ABI=abilit. ai riservati
009700941111     D AUTDS           DS
009800940620     D  MANIND                 1      1
009900940620     D  MANGRU                 2      2
010000940620     D  MANAZI                 3      3
010100940712     D  ABIRIS                 4      4
010200940620     D*-------------
010300940207     D* opzione richiesta
010400940614    >D D002DS          DS
010500940614     D  OPZ002                 1      2
010600940614     D* 0 = ritorno normale
010700940614     D* 1 = F3
010800940614     D* 2 = F12
010900940614     D  RET002                 3      3
011000940614     D* operazione eseguite dal pgm chiamato :
011100940614     D* 0 = nessuna operazione
011200940614     D* 1 = eseguito aggiornamento
011300940614     D  OPR002                 4      4
011400940614     D* funzione non eseguibile per errore :
011500940614     D* 0 = nessun errore
011600940614     D* 1 = funzione richiamata chiusa in errore
011700940614     D* Aggiungere eventuali altri codici di errore
011800940614     D  ERR002                 5      5
011900940614     D* Azienda
012000940614     D  AZI002                 6      8
012100940614     D* Lingua
012200940614     D  LIN002                 9      9
012300940614     D* Codice scelto
012400940614     D  KEY002                10     24
012500940620     D* Autorizzazione
012600940620     D  AUT002                25     25
012700970509     D* Unita
012800970509     D  UNI002                26     33
012900940614     D*-------------
013000980722     D ana200ds      E DS                  INZ
013100980722     D ana250ds      E DS                  INZ
013200980722     D ana280ds      E DS                  INZ
013300980722     D ana064ds      E DS                  INZ
013400030828     D ANA964DS      E DS                  INZ
013500030828     D X07ValDS      E DS                  INZ
013600940201     D*-------------
013700940201     D DSFMT           DS
013800940506     D  $TASTO               369    369
013900940201     D  NRG                  370    370
014000940201     D  NCL                  371    371
014100940201     D*-------------
014200940201     D* posizione cursore
014300940201     D CURSOR          DS
014400940223     D  RIRI                   1      2B 0 INZ
014500940201     D  R$$                    2      2
014600940223     D  COCO                   3      4B 0 INZ
014700940201     D  C$$                    4      4
014800940207     D*-------------
014900940207     D* Reperimento nome PGM
015000940207     D STATUS         SDS           333
015100940207     D  DSPGM            *PROC
015200940225     D*-------------
015300940225     D* COSTANTI
015400940225     D*-------------
015500940506     D*
015600940506     D* Tasti di funzione
015700940506     D F01             C                   CONST(X'31')
015800940506     D F02             C                   CONST(X'32')
015900940506     D F03             C                   CONST(X'33')
016000940506     D F04             C                   CONST(X'34')
016100940506     D F05             C                   CONST(X'35')
016200940506     D F06             C                   CONST(X'36')
016300940506     D F07             C                   CONST(X'37')
016400940506     D F08             C                   CONST(X'38')
016500940506     D F09             C                   CONST(X'39')
016600940506     D F10             C                   CONST(X'3A')
016700940506     D F11             C                   CONST(X'3B')
016800940506     D F12             C                   CONST(X'3C')
016900940506     D F13             C                   CONST(X'B1')
017000940506     D F14             C                   CONST(X'B2')
017100940506     D F15             C                   CONST(X'B3')
017200940506     D F16             C                   CONST(X'B4')
017300940506     D F17             C                   CONST(X'B5')
017400940506     D F18             C                   CONST(X'B6')
017500940506     D F19             C                   CONST(X'B7')
017600940506     D F20             C                   CONST(X'B8')
017700940506     D F21             C                   CONST(X'B9')
017800940506     D F22             C                   CONST(X'BA')
017900940506     D F23             C                   CONST(X'BB')
018000940506     D F24             C                   CONST(X'BC')
018100940506     D ENTER           C                   CONST(X'F1')
018200940506     D ROLDWN          C                   CONST(X'F4')
018300940506     D ROLLUP          C                   CONST(X'F5')
018400940506     D*-------------
018500940506     D*
018600941108     D DATA            C                   CONST('0001-01-01')
018700030828     D Ricerca         C                   '01'
018800030828     D BlankValido     C                   '1'
018900030828     D ChkValore       C                   '1'
019000030828     D Eseguito        C                   '0'
019100030828     D Errore          C                   '1'
019200940128     I*----------------------------------------------------*
019300940615     ID1
019400980722$005 I              D1DES                       §4NDES
019500980722$005 I              D1DE1                       §4NDE1
019600980722$005 I              D1IVA                       §4NIVA
019700980722$005 I              D1ESE                       §4NESE
019800980722$005 I              D1CCP                       §4NCCP
019900980722$005 I              D1SCP                       §4NSCP
020000980722$005 I              D1VOC                       §4NVOC
020100070118$005 I*             D1KCC                       §4NKCC
020200070118$005 I*             D1KSC                       §4NKSC
020300940127     I*
020400940127     C*----------------------------------------------------*
020500940127     C*                MAIN LINE PROGRAM
020600940127     C*----------------------------------------------------*
020700940127     C*
020800940223     C* inizializzazione variabili
020900980722     C                   EXSR      INZVAR
021000940223     C*
021100940223     C     $FINE         DOWEQ     *OFF
021200940202     C     $GEST         CASEQ     'D1'          GESD1
021300940117     C                   END
021400940117     C                   END
021500940201     C*
021600940325     C* fine programma
021700940325     C                   EXSR      ENDPGM
021800940325     C/EJECT
021900940325     C************************************************************
022000940325     C* RICERCHE
022100940325     C************************************************************
022200940325     C     ENDPGM        BEGSR
022300940325     C*
022400940614     C* disallocazione rcd nel caso di
022500971110$007 C                   MOVE      'Y4N'         XTBCOD
022600940614    >C                   MOVE      '7'           XTBRIC
022700970214     C                   CALLB     'XATB'
022800940614     C                   PARM                    XATBDS
022900940325     C*
023000940325     C* passaggio dati al pgm chiamante
023100940715     C                   MOVE      *BLANKS       KPJBU
023200940325    >C                   MOVEL     D002DS        KPJBU
023300940325     C*
023400940325     C                   SETON                                            RT
023500940325     C*
023600940325     C                   ENDSR
023700940325     C/EJECT
023800940201     C************************************************************
023900940201     C* RICERCHE
024000940201     C************************************************************
024100940127     C     SEARCH        BEGSR
024200940127     C*
024300940127     C* determino Riga/Colonna del cursore
024400940127     C                   MOVE      NRG           R$$
024500940127     C                   MOVE      NCL           C$$
024600940127     C                   Z-ADD     RIRI          CURR
024700940127     C                   Z-ADD     COCO          CURC
024800940127     C*
024900940224     C* I campi che al momento dell'F4 risultano protetti non devono
025000940224     C* essere abilitati alla funzione di ricerca
025100940224     C*
0252009401271    C                   SELECT
025300980722     C*
025400980722     C* FMTD1  - capoconto
025500980722    >C     H1NMFL        WHEneq    'D1CCP'
025600980722     C                   RESET                   Ana200DS
025700980722     C                   MOVEL     '01'          OPZ200
025800980722     C                   clear                   KCC200
025900980722     C                   MOVEL     Ana200DS      KPJBU
026000980722     C                   CALL      'ANA200R'
026100980722     C                   PARM                    KPJBA
026200980722     C                   MOVEL     KPJBU         Ana200DS
026300980722     C     KCC200        IFNE      *BLANK
026400980722     C                   MOVEL     KCC200        §4nccp
026500980722     C                   ENDIF
026600980722     C* imposto pos. del cursore
026700980722     C                   Z-ADD     CURR          H1RIGA
026800980722     C                   Z-ADD     CURC          H1COLO
026900980722     C*
027000980722     C* Ricerca sottoconti (ANA250R)
027100980722     C     H1NMFL        WHENEQ    'D1SCP'
027200980722     C                   RESET                   ana250DS
027300980722     C                   MOVEL     '01'          OPZ250
027400980722     C                   MOVEL     §4nccp        KCC250
027500980722     C                   clear                   KSC250
027600980722     C                   MOVEL     ana250DS      KPJBU
027700980722     C                   CALL      'ANA250R'
027800980722     C                   PARM                    KPJBA
027900980722     C                   MOVEL     KPJBU         ana250DS
028000980722     C     KSC250        IFNE      *BLANK
028100980722     C                   MOVEL     KCC250        §4nccp
028200980722     C                   MOVEL     KSC250        §4nscp
028300980722     C                   ENDIF
028400980722     C* imposto pos. del cursore
028500980722     C                   Z-ADD     CURR          H1RIGA
028600980722     C                   Z-ADD     CURC          H1COLO
028700940127     C*
028800980722     C* FMTD1  - capoconto
028900980722    >C     H1NMFL        WHEneq    'D1KCC'
029000980722     C                   RESET                   Ana200DS
029100980722     C                   MOVEL     '01'          OPZ200
029200980722     C                   clear                   KCC200
029300980722     C                   MOVEL     Ana200DS      KPJBU
029400980722     C                   CALL      'ANA200R'
029500980722     C                   PARM                    KPJBA
029600980722     C                   MOVEL     KPJBU         Ana200DS
029700980722     C     KCC200        IFNE      *BLANK
029800980722     C                   MOVEL     KCC200        §4nkcc
029900980722     C                   ENDIF
030000980722     C* imposto pos. del cursore
030100980722     C                   Z-ADD     CURR          H1RIGA
030200980722     C                   Z-ADD     CURC          H1COLO
030300980722     C*
030400980722     C* Ricerca sottoconti (ANA250R)
030500980722     C     H1NMFL        WHENEQ    'D1KSC'
030600980722     C                   RESET                   ana250DS
030700980722     C                   MOVEL     '01'          OPZ250
030800980722     C                   MOVEL     §4nkcc        KCC250
030900980722     C                   clear                   KSC250
031000980722     C                   MOVEL     ana250DS      KPJBU
031100980722     C                   CALL      'ANA250R'
031200980722     C                   PARM                    KPJBA
031300980722     C                   MOVEL     KPJBU         ana250DS
031400980722     C     KSC250        IFNE      *BLANK
031500980722     C                   MOVEL     KCC250        §4nkcc
031600980722     C                   MOVEL     KSC250        §4nksc
031700980722     C                   ENDIF
031800980722     C* imposto pos. del cursore
031900980722     C                   Z-ADD     CURR          H1RIGA
032000980722     C                   Z-ADD     CURC          H1COLO
032100980722     C*
032200980722     C* FMTD1  - codice voce
032300980722    >C     H1NMFL        WHEneq    'D1VOC'
032400980722     C                   RESET                   Ana280DS
032500980722     C                   MOVEL     '01'          OPZ280
032600980722     C                   clear                   VOC280
032700980722     C                   MOVEL     Ana280DS      KPJBU
032800980722     C                   CALL      'ANA280R'
032900980722     C                   PARM                    KPJBA
033000980722     C                   MOVEL     KPJBU         Ana280DS
033100980722     C     voc280        IFNE      *BLANK
033200980722     C                   MOVEL     VOC280        §4nvoc
033300980722     C                   ENDIF
033400980722     C* imposto pos. del cursore
033500980722     C                   Z-ADD     CURR          H1RIGA
033600980722     C                   Z-ADD     CURC          H1COLO
033700980722     C*
033800980722     C* FMTD1  - codice iva
033900980722    >C     H1NMFL        WHEneq    'D1IVA'
034000980722     C                   RESET                   Ana064DS
034100980722     C                   MOVEL     '2'           TPR064
034200980722     C                   MOVEL     '01'          OPZ064
034300980722     C                   MOVEL     Ana064DS      KPJBU
034400980722     C                   CALL      'ANA064R'
034500980722     C                   PARM                    KPJBA
034600980722     C                   MOVEL     KPJBU         Ana064DS
034700980722     C     ali064        IFNE      *zeros
034800980722     C                   MOVEL     ali064        §4niva
034900980722     C                   ENDIF
035000980722     C* imposto pos. del cursore
035100980722     C                   Z-ADD     CURR          H1RIGA
035200980722     C                   Z-ADD     CURC          H1COLO
035300980722     C*
035400980722     C* FMTD1  - codice esenzione
035500980722    >C     H1NMFL        WHEneq    'D1ESE'
035600980722     C                   RESET                   Ana064DS
035700980722     C                   MOVEL     '2'           TPR064
035800980722     C                   MOVEL     '01'          OPZ064
035900980722     C                   MOVEL     Ana064DS      KPJBU
036000980722     C                   CALL      'ANA064R'
036100980722     C                   PARM                    KPJBA
036200980722     C                   MOVEL     KPJBU         Ana064DS
036300980722     C     rfi064        IFNE      *BLANK
036400980722     C                   MOVEL     rfi064        §4nese
036500980722     C                   ENDIF
036600980722     C* imposto pos. del cursore
036700980722     C                   Z-ADD     CURR          H1RIGA
036800980722     C                   Z-ADD     CURC          H1COLO
036900130412     C*
037000130423    >C*  CAUSALE testata
037100130418     C     H1NMFL        WHENEQ    '§4PRCONCAU'
037200130412     C                   RESET                   A070DS
037300130412     C                   MOVEL     '01'          OPZ070
037400130412     C                   MOVEL     A070DS        KPJBU
037500130412     C                   CALL      'ANA070R'
037600130412     C                   PARM                    KPJBA
037700130412     C                   MOVEL     KPJBU         A070DS
037800130412     C*
037900130412     C     CAU070        IFNE      *BLANK
038000130418     C                   MOVEL     CAU070        §4PrConCau
038100130418     C                   movel     DE1070        §4PrConCad
038200130412     C                   ENDIF
038300130412     C* imposto pos. del cursore
038400130412     C                   Z-ADD     CURR          H1RIGA
038500130412     C                   Z-ADD     CURC          H1COLO
038600130423    >C*  CAUSALE riga
038700130423     C     H1NMFL        WHENEQ    '§4PRCOCAU2'
038800130423     C                   RESET                   A070DS
038900130423     C                   MOVEL     '01'          OPZ070
039000130423     C                   MOVEL     A070DS        KPJBU
039100130423     C                   CALL      'ANA070R'
039200130423     C                   PARM                    KPJBA
039300130423     C                   MOVEL     KPJBU         A070DS
039400130423     C*
039500130423     C     CAU070        IFNE      *BLANK
039600130423     C                   MOVEL     CAU070        §4PrCoCau2
039700130423     C                   movel     DE1070        §4PrCoCad2
039800130423     C                   ENDIF
039900130423     C* imposto pos. del cursore
040000130423     C                   Z-ADD     CURR          H1RIGA
040100130423     C                   Z-ADD     CURC          H1COLO
040200940127     C*
040300940223     C*
040400940223     C* ricerca non abilitata in questa posizione
0405009402231O   C                   OTHER
040600941005     C                   SETON                                        9899
040700941005     C* imposto pos. del cursore
040800941005     C                   Z-ADD     CURR          H1RIGA
040900941005     C                   Z-ADD     CURC          H1COLO
041000940127     C*
0411009401271-   C                   ENDSL
041200940127     C*
041300940506     C                   MOVE      *HIVAL        $LASTV
041400940127     C                   ENDSR
041500940615     C/EJECT
041600940615     C************************************************************
041700940615     C* TRADUZIONI
041800940615     C************************************************************
041900070206     C     LINGUA        BEGSR
042000070206     C*
042100070206     C* determino Riga/Colonna del cursore
042200070206     C                   MOVE      NRG           R$$
042300070206     C                   MOVE      NCL           C$$
042400070206     C                   Z-ADD     RIRI          CURR
042500070206     C                   Z-ADD     COCO          CURC
042600070206     C*
042700070206     C*
042800070206     C*
042900070206     C* FMTD1  - CAMPI DA TRADURRE
043000070206     C* imposto pos. del cursore
043100070206     C                   Z-ADD     CURR          H1RIGA
043200070206     C                   Z-ADD     CURC          H1COLO
043300070206     x****               MOVEL     H1NMFL        W005              5
043400070206     x****               MOVE      W005          W003              3
043500070206     x****               MOVEL     W003          W006              6
043600070206     C                   MOVEL     H1NMFL        W008              8
043700070206     C                   MOVE      W008          W003              6
043800070206     C                   MOVE      W008          W006              6
043900070207     C                   MOVEL     D1COD         KEY              15
044000070206     C                   SELECT
044100070206$014 C     W003          WHENEQ    'DES'
044200070206$014 C                   Z-ADD     1             I                 3 0
044300070206$014 C     W006          LOOKUP    LIN(I)                                 21
044400070206$014 C                   CALL      'ANA101R '
044500070206$014 C                   PARM      W003          CAMPO            10
044600070206$014 C                   PARM      'Y4N'         COD               3
044700070206$014 C                   PARM                    KEY              15
044800070206$014 C                   PARM      044           LUN               3 0
044900070206$014 C                   PARM      001           POS               3 0
045000070206$014 C     §4NDES        PARM      §4NDES        DES              50
045100070206$014 C                   PARM      LIND(I)       FLD              50
045200070206$014 C                   PARM      XTBAZI        AZI               3
045300070206$014 C                   PARM      LIN002        LNG               1
045400070206$014 C                   PARM      OPZ002        OPZ               2
045500070206     C*
045600070206     C*
045700070206     C* ricerca non abilitata in questa posizione
0458000702061O   C                   OTHER
045900070206     C                   SETON                                        98
046000070206     C*
0461000702061-   C                   ENDSL
046200070206     C*
046300070206     C                   MOVE      *HIVAL        $LASTV
046400070206     C                   ENDSR
046500940225     C/EJECT
046600940225     C************************************************************
046700940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
046800940225     C************************************************************
046900940225     C     GESPIE        BEGSR
047000940225     C*
047100940225     C                   SELECT
047200940225     C*
047300940506     C     $GEST         WHENEQ    'D1'
047400940506     C     $ULKD1        IFEQ      0
047500940506     C* riempimento opzioni
047600940506     C                   EXSR      F24D1
047700940506     C                   ELSE
047800940614     C                   WRITE     Z1
047900940506     C                   ENDIF
048000940225     C*
048100940510     C*
048200940225     C                   ENDSL
048300940225     C*
048400940225     C                   ENDSR
048500940131     C/EJECT
048600940131     C************************************************************
048700940510     C* GESTIONE VIDEO RECORD D1
048800940131     C************************************************************
048900940127     C     GESD1         BEGSR
049000940225     C* inizializzazione videata
049100940223     C     $INZD1        IFEQ      *ON
049200940127     C                   EXSR      INZD1
049300940223     C                   MOVE      *OFF          $INZD1
049400940117     C                   END
049500940225     C* emissione piede videata se proveniente da altra
049600940225     C* salvataggio valore formato di provenienza
049700940207     C     $LASTV        IFNE      'D1'
049800940614     C                   WRITE     T1
049900940225     C                   EXSR      GESPIE
050000940202     C                   MOVE      $LASTV        $LASTG
050100940202     C                   MOVE      'D1'          $LASTV
050200940202     C                   END
050300940131     C*
050400940203     C                   SELECT
050500940203     C* Annullamento
050600940315    >C     OPZ002        WHENEQ    '04'
050700940224     C* Visualizzazione
050800940315    >C     OPZ002        OREQ      '05'
050900940620     C                   SETON                                        0203
051000940620     C* Manutenzione
051100940620    >C     OPZ002        WHENEQ    '02'
051200940620     C                   SETON                                        02
051300940620     C                   SETOFF                                       03
051400940620     C*
051500940620     C                   OTHER
051600940620     C*
051700940620     C                   SETOFF                                       0203
051800940203     C                   ENDSL
051900940620     C* Immissione/Modifica/Copia
052000940620     C                   EXFMT     D1
052100940506     C* controllo abilitazione tasti funzione
052200940506     C*
052300940506     C                   EXSR      CTRKD1
052400940506     C*
052500940506     C     *IN99         IFEQ      *OFF
052600940131     C*
0527009402031    C                   SELECT
052800940510     c* ROLLUP
052900940131     C* F3=Fine
053000940506     C     $TASTO        WHENEQ    F03
053100940309     C                   EXSR      F03D1
053200940131     C* F4=Ricerca
053300940506     C     $TASTO        WHENEQ    F04
053400940127     C                   EXSR      SEARCH
053500940224     C* F5=Refresh
053600940506     C     $TASTO        WHENEQ    F05
053700940309     C                   EXSR      F05D1
053800940223     C* F8=Successivo
053900940506     C     $TASTO        WHENEQ    F08
054000940309     C                   EXSR      F08D1
054100940131     C* F12=Ritorno
054200940506     C     $TASTO        WHENEQ    F12
054300940309     C                   EXSR      F12D1
054400940315     C* F24=Altri tasti
054500940506     C     $TASTO        WHENEQ    F24
054600940315     C                   EXSR      F24D1
054700940131     C*
0548009402031O   C                   OTHER
054900940127     C                   EXSR      CTRD1
055000940616     C     *IN99         IFEQ      *OFF
055100940616     C                   SELECT
055200940616     C* F7=Traduzione
055300940616     C     $TASTO        WHENEQ    F07
055400940616     C                   EXSR      LINGUA
055500940616     C* F6
0556009406162    C     $TASTO        WHENEQ    F06
055700940224     C* eseguo aggiornamento
055800940315    >C                   EXSR      AGGANA
055900940224     C* eseguo operazioni del dopo-aggiornamento
0560009403183    C     *IN99         IFEQ      *OFF
056100940318     C                   EXSR      GESAGG
0562009403183-   C                   ENDIF
0563009406162-   C                   ENDSL
056400940616     C                   ENDIF
056500940203     C*
0566009402241-   C                   ENDSL
056700940224     C*
0568009405062-   C                   ENDIF
056900940117     C                   ENDSR
057000940506     C/EJECT
057100940506     C************************************************************
057200940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
057300940506     C************************************************************
057400940506     C     CTRKD1        BEGSR
057500940506     C                   SETOFF                                       99
057600940506     C                   MOVEA     $FC1          $FC
057700940506     C                   EXSR      CTRKEY
057800940506     C     *IN99         IFEQ      *ON
057900940506     C                   SETON                                        97
058000940506     C                   ENDIF
058100940506     C                   ENDSR
058200940117      ************************************************************
058300940207      * INIZIALIZZAZIONE VIDEATA DATI
058400940117      ************************************************************
058500940127     C     INZD1         BEGSR
058600940131     C*
058700940615     C                   CLEAR                   D1
058800941006     C                   MOVE      '0'           D1RIS
058900980722     C                   MOVEl     KEY002        D1COD
059000940615     C                   MOVE      AZI002        XTBAZI
059100940615     C                   MOVE      LIN002        XTBLIN
059200980722     C                   MOVE      *ALL' '       XTBKEY
059300980722     C                   MOVEl     KEY002        XTBKEY
059400970509     C                   MOVE      UNI002        XTBUNT
059500940615$007 C                   MOVE      'Y4N'         XTBCOD
059600940225     C*
059700940207     C*
059800940224     C* valorizzo la chiave univoca per reperire i dati
059900940224     C* se non immissione
0600009403151   >C     OPZ002        IFNE      '01'
060100940315    >C*-------------------------------------------------------------
060200940315    >C* impostare i campi chiave ricevuti
060300940315    >C*-------------------------------------------------------------
060400940318     C*
060500940614     C*
060600940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0607009406142   >C     OPZ002        IFEQ      '02'
060800940315    >C     OPZ002        OREQ      '04'
060900940614     C                   MOVE      '1'           XTBALC
061000940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0611009406142E   C                   ELSE
061200940614     C                   MOVE      '0'           XTBALC
0613009406142-   C                   ENDIF
061400940614     C*
061500940616     C                   MOVE      '1'           XTBRIC
061600970214     C                   CALLB     'XATB'
061700940614     C                   PARM                    XATBDS
061800940318     C*
061900940614     C     XTBERR        IFEQ      '0'
062000940614     C                   MOVEL     XTBUNI        EXTDS
062100970512     c                   move      xtbunt        d1uniTA           8
062200941108     C* Giro la data se non blank
062300970214     c                   move      xtbdsc        xtbdscA          10
062400970214     C     XTBDSCa       IFNE      DATA
062500970214     C                   CALLB     'XDT4000'
062600970214     C                   PARM      XTBDSCA       XDTAMG           10
062700940621     C                   PARM      *ZEROS        XDTGMA            6 0
062800940621     C                   PARM      2             XDTSTA            1 0
062900940621     C                   MOVE      XDTGMA        D1DSC
063000940712     C                   MOVE      XTBRIS        D1RIS
063100941108     C                   ELSE
063200941108     C                   MOVE      *ZEROS        D1DSC
063300941108     C                   ENDIF
063400941108     C                   ENDIF
063500940614     C*
063600940224     C*
063700940224     C* se il rcd da gestire non viene trovato
063800940318     C* oppure è allocato da altro pgm
063900940224     C* ritorno al pgm guida avvertendolo dell'errore
0640009406142    C     XTBERR        IFNE      '0'
064100940224     C                   MOVE      *ON           $FINE
064200940315    >C                   MOVE      '1'           ERR002
064300940224     C*
064400940224     C* altrimenti
0645009402242E   C                   ELSE
064600940224     C* riempio la videata
064700940224     C                   EXSR      RIED1
064800940224     C* richiamo routine dei ctrl per decodificarla
064900940614    >C                   EXSR      CTRD1
065000960228     C                   MOVEL     *IN99         SAV99             1
065100960228     C* Se la gestione è più videate
065200960305    >C                   EXSR      CTRD2
065300960305    >C     *IN99         IFEQ      *ON
065400960305    >C                   MOVEL     *IN99         SAV99             1
065500960305    >C                   ENDIF
065600940224     C                   MOVE      *ALL'0'       IN5098
065700940224     C                   MOVEA     IN5098        *IN(50)
065800940224     C* definizione indicatori su video
065900940224     C                   SETOFF                                       02
0660009402243    C                   SELECT
066100940315    >C     OPZ002        WHENEQ    '02'
066200940224     C                   SETON                                        02
066300940315    >C     OPZ002        WHENEQ    '05'
066400940506     C* disabilita tasti
066500940720     C                   MOVE      '0'           $FC(6)
066600940720     C                   MOVE      '0'           $FC(5)
066700940720     C                   MOVE      '0'           $FC(4)
0668009402243-   C                   ENDSL
066900940224     C*
0670009402242-   C                   ENDIF
067100940223     C*
0672009402241-   C                   ENDIF
067300940224     C*
067400940117     C                   ENDSR
067500940207     C/EJECT
067600940207     C************************************************************
067700940510     C* RIEMPIMENTO VIDEATA  D1
067800940207     C************************************************************
067900940207     C     RIED1         BEGSR
068000940207     C*
068100030901     C                   IF        §4NIRIFSPE = *ON
068200030901     C                   EVAL      D1IRIFSPE = XscOn
068300030901     C                   ELSE
068400030901     C                   EVAL      D1IRIFSPE = XscOff
068500030901     C                   ENDIF
068600030901
068700030901     C                   IF        §4NIRIFFAT = *ON
068800030901     C                   EVAL      D1IRIFFAT = XscOn
068900030901     C                   ELSE
069000030901     C                   EVAL      D1IRIFFAT = XscOff
069100030901     C                   ENDIF
069200030901
069300030828     C                   IF        §4NISGLVAR = *ON
069400030828     C                   EVAL      D1ISGLVAR = XscOn
069500030828     C                   ELSE
069600030828     C                   EVAL      D1ISGLVAR = XscOff
069700030828     C                   ENDIF
069800030828
069900030828     C                   IF        §4NISPEREC = *ON
070000030828     C                   EVAL      D1ISPEREC = XscOn
070100030828     C                   ELSE
070200030828     C                   EVAL      D1ISPEREC = XscOff
070300030828     C                   ENDIF
070400030828
070500030828     C                   IF        §4NCE = *ON
070600030828     C                   EVAL      D1CE = XscOn
070700030828     C                   ELSE
070800030828     C                   EVAL      D1CE = XscOff
070900030828     C                   ENDIF
071000030828
071100040315     C                   SELECT
071200040315     C                   WHEN      §4NEFATMAN = *ON
071300030828     C                   EVAL      D1EFATMAN = XscOn
071400040315     C                   WHEN      §4NEFATMAN = *OFF
071500030828     C                   EVAL      D1EFATMAN = XscOff
071600040315     C                   OTHER
071700040315     C                   EVAL      D1EFATMAN = §4NEFATMAN
071800040315     C                   ENDSL
071900030828
072000940207     C*
072100940207     C                   ENDSR
072200940309     C/EJECT
072300940309     C************************************************************
072400940309     C* GESTIONE F03 VIDEO D1
072500940309     C************************************************************
072600940309     C     F03D1         BEGSR
072700940309     C*
072800940309     C                   MOVE      *ON           $FINE
072900940315    >C                   MOVE      '1'           RET002
073000940325     C* fine programma
073100940325     C                   EXSR      ENDPGM
073200940325     C*
073300940325     C                   ENDSR
073400940309     C/EJECT
073500940309     C************************************************************
073600940309     C* GESTIONE F05 VIDEO D1
073700940309     C************************************************************
073800940309     C     F05D1         BEGSR
073900940309     C*
074000940309     C                   MOVE      *ON           $INZD1
074100940309     C*
074200940309     C                   ENDSR
074300940309     C/EJECT
074400940309     C************************************************************
074500940309     C* GESTIONE F08 VIDEO D1
074600940309     C************************************************************
074700940309     C     F08D1         BEGSR
074800940309     C*
074900940309     C                   MOVE      *ON           $FINE
075000940315    >C                   MOVE      '0'           RET002
075100940309     C*
075200940309     C                   ENDSR
075300940309     C/EJECT
075400940309     C************************************************************
075500940309     C* GESTIONE F12 VIDEO D1
075600940309     C************************************************************
075700940309     C     F12D1         BEGSR
075800940309     C*
075900940309     C                   MOVE      *ON           $FINE
076000940315    >C                   MOVE      '2'           RET002
076100940309     C*
076200940309     C                   ENDSR
076300940309     C/EJECT
076400940309     C************************************************************
076500940309     C* GESTIONE F24 VIDEO D1
076600940309     C************************************************************
076700940309     C     F24D1         BEGSR
076800940309     C*
076900940506     C*
077000940506     C                   MOVEA     $FX1          $FX
077100940506     C                   MOVEA     $FC1          $FC
077200940506     C                   MOVEA     $FR1          $FR
077300940506     C                   Z-ADD     $ULKD1        $ULK
077400940506     C                   EXSR      RIEKEY
077500941017     C                   MOVEA     $FC           $FC1
077600940506     C                   Z-ADD     $ULK          $ULKD1            3 0
077700940506     C                   MOVEL     $KEY1         Z1KE1
077800940506     C                   MOVEL     $KEY2         Z1KE2
077900940614     C                   WRITE     Z1
078000940506     C*
078100940309     C                   ENDSR
078200940207     C/EJECT
078300940131     C************************************************************
078400940207     C* CONTROLLO VIDEATA
078500940131     C************************************************************
078600940127     C     CTRD1         BEGSR
078700940131     C*
078800940127     C                   SETOFF                                       99
078900940127     C* non imposto pos. del cursore
079000940127     C                   Z-ADD     0             H1RIGA
079100940127     C                   Z-ADD     0             H1COLO
079200940621     C*
079300940621     C* CTRL DATA
079400940621     C*
079500941109     C                   MOVE      DATA          WRKDSC           10
079600940621     C     D1DSC         IFNE      0
079700970214     C                   CALLB     'XDT4000'
079800941109     C                   PARM                    XDTAMG           10
079900940621     C                   PARM      D1DSC         XDTGMA            6 0
080000940621     C                   PARM      3             XDTSTA            1 0
080100940621     C     XDTSTA        IFNE      0
080200940621     C                   SETON                                        5599
080300940621     C                   ELSE
080400941108     C                   MOVE      XDTAMG        WRKDSC
080500940621     C                   ENDIF
080600940621     C                   ENDIF
080700940223     C*
080800940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
080900940223     C*
0810009403151   >C     OPZ002        IFNE      '04'
081100971110     C*
081200971110     C* controllo codice vuoto
081300971110$017 C     D1COD         IFEQ      *ALL' '
081400971110     C                   SETON                                        5099
081500971110     C                   ENDIF
081600971110     C*
081700980722$017 C     §4ndes        IFEQ      *ALL' '
081800980722     C                   SETON                                        6099
081900980722     C                   ENDIF
082000980722     C*
082100980722$017 C     §4nde1        IFEQ      *ALL' '
082200980722     C                   SETON                                        6199
082300980722     C                   ENDIF
082400980722     C*
082500980722$017 C     §4niva        IFgt      *zeros
082600980722     c     §4nese        andne     *blanks
082700980722$017 C     §4niva        oreq      *zeros
082800980722     c     §4nese        andeq     *blanks
082900980722     C                   SETON                                        6299
083000980722     C                   ENDIF
083100980722     c*
083200980722     c                   clear                   d1ivad
083300980722     c*
083400980722     c                   if        §4niva         <> *zeros
083500980722     c     k02riv02      chain     anriv02l                           11
083600980722     c   11              seton                                        6999
083700980722     c  n11              movel     rivdesbrev    d1ivad
083800980722     c                   endif
083900980722     c*
084000980722     c                   clear                   d1esed
084100980722     c*
084200980722     c                   if        §4nese         <> *blanks
084300980722     c     k02riv03      chain     anriv03                            11
084400980722     c   11              seton                                        7099
084500980722     c  n11              movel     rivdesbrev    d1esed
084600980722     c                   endif
084700980722     c*
084800980722     c                   clear                   d1scpd
084900980722     c*
085000070319     c                   if        §4nccp <> *blanks or
085100070319     c                             §4nscp <> *blanks
085200980722     c                   movel     §4nccp        rcokcc
085300980722     C                   movel     §4nscp        rcoksc
085400980722     C     K03rco99      CHAIN     anrco99j                           90
085500980722     C**n90rcosogg       chain     ansog01l                           90
085600980722     C     *IN90         IFEQ      '1'
085700980722     C                   SETON                                        9966
085800980722     C                   clear                   d1scpd
085900980722     C                   ELSE
086000980722     C                   MOVEL     pdcdesbrev    d1scpd
086100980722     C                   END
086200980722     C                   ELSE
086300070319     C*                  SETON                                        9963
086400980722     C                   END
086500980722     C*
086600980722     c                   clear                   d1vocd
086700980722     c*
086800980722     c                   if        §4nvoc         <> *blanks
086900980722     c     k02voc01      chain     AnVci01l                           12
087000980722     c  n12              movel     vcidesbrev    d1vocd
087100980722     c   12              seton                                        6799
087200980722     C                   ELSE
087300070319     C*                  SETON                                        9964
087400980722     c                   endif
087500070118      // Controllo la società di utilizzo.
087600070118     C                   IF        §4nSocUso = *BLANK
087700070118     C                   EVAL      sifDesBrev = 'Tutte'
087800070118     C                   ELSE
087900070118     C                   IF        NOT %OPEN(ansif01l)
088000070118     C                   OPEN      ansif01l
088100070118     C                   ENDIF
088200070118     C     §4nSocUso     CHAIN     ansif01l
088300070118     C                   IF        NOT %FOUND
088400070118     C                   EVAL      *IN71 = *ON
088500070118     C                   EVAL      *IN99 = *ON
088600070118     C                   CLEAR                   sifDesBrev
088700070118     C                   ENDIF
088800070118     C                   ENDIF
088900130412     C*
089000130423     C* Controllo causale testata
089100130418     c                   clear                   §4prConCaD
089200130412     c*
089300130412     c                   select
089400130418     c                   when      §4prcon ='S'
089500130418     c                             and §4prconcau <> *blanks
089600130412     c                   movel     xscsoc        opesocieta
089700130418     c                   movel     §4prconcau    opecausale
089800130412     c     k02ope01      chain     anope01l
089900130412     c                   IF        NOT %FOUND
090000130412     c                   seton                                        9972
090100130412     c                   else
090200130418     c                   eval      §4prconcad = opedesbrev
090300130412     c                   if        opeTpc <> '1' or
090400130412     c                             opeStp <> 'C' or
090500130412     c                             (opeTpDocI <> '1' and
090600130412     c                              opeTpDocI <> '4' and
090700130412     c                              opeTpDocI <> '7' )
090800130412     c                   seton                                        9972
090900130412     c                   endif
091000130412     c                   endif
091100130418     c                   when      §4prcon ='S'
091200130418     c                             and §4prconcau = *blanks
091300130412     c                   Seton                                        9972
091400130418     c                   when      §4prcon <> 'S'
091500130418     c                             and §4prconcau <> *blanks
091600130412     c                   Seton                                        9972
091700130412     c                   endsl
091800130423     C*
091900130423     C* Controllo causale riga
092000130423     c                   clear                   §4prCoCaD2
092100130423     c*
092200130423     c                   select
092300130423     c                   when      §4prcon ='S'
092400130423     c                             and §4prcocau2 <> *blanks
092500130423     c                   movel     xscsoc        opesocieta
092600130423     c                   movel     §4prcocau2    opecausale
092700130423     c     k02ope01      chain     anope01l
092800130423     c                   IF        NOT %FOUND
092900130423     c                   seton                                        9973
093000130423     c                   else
093100130423     c                   eval      §4prcocad2 = opedesbrev
093200130423     c                   if        opeTpc <> '0' or
093300130423     c                             opeTpDocNI <> '9'
093400130423     c                   seton                                        9973
093500130423     c                   endif
093600130423     c                   endif
093700130423     c                   when      §4prcon ='S'
093800130423     c                             and §4prcocau2 = *blanks
093900130423     c                   Seton                                        9973
094000130423     c                   when      §4prcon <> 'S'
094100130423     c                             and §4prcocau2 <> *blanks
094200130423     c                   Seton                                        9973
094300130423     c                   endsl
094400980722     C*
094500070118     c*                  clear                   d1kscd
094600980722     c*
094700070118     c*                  if        §4nkcc         <> *blanks   and
094800070118     c*                            §4nksc         <> *blanks
094900070118     c*                  movel     §4nkcc        rcokcc
095000070118     C*                  movel     §4nksc        rcoksc
095100070118     C*    K03rco99      CHAIN     anrco99j                           90
095200070118     C**n90rcosogg       chain     ansog01l                           90
095300070118     C*    *IN90         IFEQ      '1'
095400070118     C*                  SETON                                        9968
095500070118     C*                  clear                   d1kscd
095600070118     C*                  ELSE
095700070118     C*                  MOVEL     pdcdesbrev    d1kscd
095800070118     C*                  END
095900070118     C*                  ELSE
096000070118     C*                  SETON                                        9965
096100070118     C*                  END
096200030828
096300980722     C*
096400940207     C*
096500940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
096600940315    >C     OPZ002        IFEQ      '01'
096700940315    >C     OPZ002        OREQ      '03'
096800940315    >C*-------------------------------------------------------------
096900940315    >C* impostare i campi chiave e verificare l'esistenza del record
097000940315    >C*-------------------------------------------------------------
097100971110$007 C                   MOVE      'Y4N'         XTBCOD
097200980722     C                   MOVE      *ALL' '       XTBKEY
097300980722    >C                   MOVEl     D1COD         XTBKEY
097400970512    >C                   MOVE      D1UNITA       XTBUNT
097500940614     C                   MOVE      '0'           XTBALC
097600940614     C                   MOVE      '1'           XTBRIC
097700970214    >C                   CALLB     'XATB'
097800940614     C                   PARM                    XATBDS
097900940614     C     XTBERR        IFEQ      '0'
098000940223     C                   SETON                                        5999
098100940223     C                   ENDIF
098200940620     C* Controllo se compreso nei range indice
098300940620     C     XTBERR        IFEQ      '3'
098400940620    >C                   SETON                                        5899
098500940620     C                   END
098600940620     C*
098700940620     C* REPERISCO AUTORIZZAZIONI UTENTE
098800940620     C                   CLEAR                   XCHATB
098900941118     C                   MOVE      XSCSOC        XCTSOC
099000941118     C                   MOVE      XSCDUN        XCTUNI
099100940620     C                   MOVEL     KNMUS         XCTPRF
099200940620     C                   MOVE      XTBCOD        XCTCOD
099300940620     C                   CALL      'XCHKATB'
099400940620     C                   PARM                    XCHATB
099500941111     C                   MOVEL     XCTDDS        AUTDS
099600941027     C     XCTRTN        IFEQ      1
099700941027     C                   MOVEL     'N'           MANGRU
099800941027     C                   ENDIF
099900940620     C* Non ammessa immissione di gruppo
100000940620     C     XTBGRU        IFEQ      'G'
100100940620     C     MANGRU        ANDEQ     'N'
100200940620     C                   SETON                                        5799
100300940620     C                   END
100400940620     C* Non ammessa immissione di azienda
100500940620     C     XTBGRU        IFEQ      'A'
100600940620     C     MANAZI        ANDEQ     'N'
100700940620     C                   SETON                                        5699
100800940620     C                   END
100900940712     C* codice riservato non accessibile
101000941006     C     XTBRIS        IFGT      ABIRIS
101100941005    >C     OPZ002        ANDNE     '01'
101200941005    >C     OPZ002        ANDNE     '03'
101300940712     C                   SETON                                        5499
101400940712     C                   END
101500940224     C                   ENDIF
101600940224     C*
1017009402241-   C                   END
101800940131     C*
101900940302     C     *IN99         IFEQ      *ON
102000940510     C     $GEST         ANDEQ     'D1'
102100960228     C     OPZ002        ANDNE     '07'
102200940117     C                   MOVEA     *IN           WIN
102300940128     C                   MOVE      *ALL'0'       IN5098           49
102400940128     C                   MOVEA     IN5098        *IN(50)
102500940614     C                   WRITE     D1
102600940117     C                   MOVEA     WIN           *IN
102700940302     C                   ENDIF
102800940127     C*
102900940117     C                   ENDSR
103000960305     C/EJECT
103100960305     C************************************************************
103200960305     C* CONTROLLO VIDEATA D2
103300960305     C************************************************************
103400960305     C     CTRD2         BEGSR
103500960305     C*
103600960305     C                   ENDSR
103700940207     C/EJECT
103800940224     C************************************************************
103900940315    >C* AGGIORNAMENTO ANAGRAFICA
104000940224     C************************************************************
104100940315    >C     AGGANA        BEGSR
104200971110     C*
104300971110$007 C                   MOVE      'Y4N'         XTBCOD
104400980722     C                   MOVE      *ALL' '       XTBKEY
104500980722     C                   MOVEl     D1COD         XTBKEY
104600970512     C                   MOVE      D1UNITA       XTBUNT
104700940207     C*
104800940207     C                   SELECT
104900940315    >C     OPZ002        WHENEQ    '01'
105000940315    >C     OPZ002        OREQ      '03'
105100940315    >C                   EXSR      RIEANA
105200950609     C                   MOVE      XSCCUT        XTBUTE
105300940614     C                   MOVE      '5'           XTBRIC
105400970214     C                   CALLB     'XATB'
105500940614    >C                   PARM                    XATBDS
105600940318     C* se rcd non scrivibile attivo errore
105700940615     C     XTBERR        IFEQ      '1'
105800940318    >C                   SETON                                        5999
105900940318     C                   END
106000940315    >C     OPZ002        WHENEQ    '02'
106100940621     C                   MOVE      '1'           XTBRIC
106200940621     C                   MOVE      '1'           XTBALC
106300970214     C                   CALLB     'XATB'
106400940621     C                   PARM                    XATBDS
106500940622    >C                   EXSR      RIEANA
106600950609     C                   MOVE      XSCCUT        XTBUTE
106700940614     C                   MOVE      '4'           XTBRIC
106800970214     C                   CALLB     'XATB'
106900940614    >C                   PARM                    XATBDS
107000940315    >C     OPZ002        WHENEQ    '04'
107100940621     C                   MOVE      '1'           XTBRIC
107200940621     C                   MOVE      '1'           XTBALC
107300970214     C                   CALLB     'XATB'
107400940621     C                   PARM                    XATBDS
107500940614     C                   MOVE      '6'           XTBRIC
107600970214     C                   CALLB     'XATB'
107700940614    >C                   PARM                    XATBDS
107800940621     C* annullo traduzioni
107900940621     C     XTBERR        IFEQ      '0'
108000940621     C                   EXSR      ANNTBT
108100940621     C                   END
108200940621     C*
108300940207     C                   ENDSL
108400940203     C*
108500940203     C                   ENDSR
108600940224     C/EJECT
108700940224     C************************************************************
108800940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
108900940224     C************************************************************
109000940616     C     RIEANA        BEGSR
109100030828
109200030901     C                   IF        D1IRIFSPE = XscOn
109300030901     C                   EVAL      §4NIRIFSPE = *ON
109400030901     C                   ELSE
109500030901     C                   EVAL      §4NIRIFSPE = *OFF
109600030901     C                   ENDIF
109700030901
109800030901     C                   IF        D1IRIFFAT = XscOn
109900030901     C                   EVAL      §4NIRIFFAT = *ON
110000030901     C                   ELSE
110100030901     C                   EVAL      §4NIRIFFAT = *OFF
110200030901     C                   ENDIF
110300030901
110400030828     C                   IF        D1ISGLVAR = XscOn
110500030828     C                   EVAL      §4NISGLVAR = *ON
110600030828     C                   ELSE
110700030828     C                   EVAL      §4NISGLVAR = *OFF
110800030828     C                   ENDIF
110900030828
111000030828     C                   IF        D1ISPEREC = XscOn
111100030828     C                   EVAL      §4NISPEREC = *ON
111200030828     C                   ELSE
111300030828     C                   EVAL      §4NISPEREC = *OFF
111400030828     C                   ENDIF
111500030828
111600030828     C                   IF        D1CE = XscOn
111700030828     C                   EVAL      §4NCE = *ON
111800030828     C                   ELSE
111900030828     C                   EVAL      §4NCE = *OFF
112000030828     C                   ENDIF
112100030828
112200040315     C                   SELECT
112300040315     C                   WHEN      D1EFATMAN = XscOn
112400030828     C                   EVAL      §4NEFATMAN = *ON
112500040315     C                   WHEN      D1EFATMAN = XscOff
112600030828     C                   EVAL      §4NEFATMAN = *OFF
112700040315     C                   OTHER
112800040315     C                   EVAL      §4NEFATMAN = D1EFATMAN
112900040315     C                   ENDSL
113000030828
113100940616     C                   CLEAR                   XTBUNI
113200940621     C                   MOVE      WRKDSC        XTBDSC
113300940712     C                   MOVE      D1RIS         XTBRIS
113400940616     C                   MOVEL     EXTDS         XTBUNI
113500940207     C                   ENDSR
113600940224     C/EJECT
113700940224     C************************************************************
113800940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
113900940224     C************************************************************
114000940224     C     GESAGG        BEGSR
114100940224     C*
114200940224     C* segnalo al pgm chiamante l'avvenuta conferma
114300940315    >C                   MOVE      *ON           OPR002
114400940224     C* segnalo al pgm chiamante l'aver premuto l'F6
114500940315    >C                   MOVE      '0'           RET002
114600940224     C*
1147009402241    C                   SELECT
114800940224     C* nel caso di immissione, ripristino la videata iniziale
114900940315    >C     OPZ002        WHENEQ    '01'
115000940224     C                   MOVE      *ON           $INZD1
115100940224     C* altrimenti ritorno al pgm chiamante
115200940224     C                   OTHER
115300940224     C                   MOVE      *ON           $FINE
1154009402241-   C                   ENDSL
115500940224     C*
115600940224     C                   ENDSR
115700940621     C/EJECT
115800940621     C************************************************************
115900940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
116000940621     C************************************************************
116100940621    >C     ANNTBT        BEGSR
116200940621     C                   MOVE      XTBAZI        TBTAZI
116300940621     C                   MOVE      XTBCOD        TBTCOD
116400980722     C                   MOVEl     XTBKEY        TBTKEY
116500940621     C     KTBT          SETLL     ANTBT01L
116600940621     C                   DO        *HIVAL
116700940621     C     KTBT          DELETE    ANTBT01L                           21
116800940621     C  N21              END
116900940621     C*
117000940621     C                   ENDSR
117100940207     C/EJECT
117200940131     C************************************************************
117300940131     C* OPERAZIONI INIZIALI
117400940131     C************************************************************
117500940131     C     *INZSR        BEGSR
117600940131     C*
117700940127     C* Reperimento parametri
117800940127     C*
117900940117     C     *ENTRY        PLIST
118000940117     C                   PARM                    KPJBA
118100940712     C*
118200940712     C* Reperimento Codice utente
118300940712     C*
118400940223     C*
118500940506     C*
118600940506     C* Reperimento tasti di funzione
118700940506     C*
118800940506     C                   EXSR      REPKEY
118900940621     C*
119000940621     C* KLIST
119100940621     C*
119200130412     c     K02ope01      klist
119300130412     c                   kfld                    opesocieta
119400130412     c                   kfld                    opecausale
119500130412     C*                                                    *
119600940621     C     KTBT          KLIST
119700940621     C                   KFLD                    TBTAZI
119800940621     C                   KFLD                    TBTCOD
119900940621     C                   KFLD                    TBTKEY
120000940506     C*
120100980722     C     K02Riv02      KLIST
120200980722     C                   KFLD                    xscsoc
120300980722     C                   KFLD                    §4niva
120400980722     C*
120500980722     C     K02Riv03      KLIST
120600980722     C                   KFLD                    xscsoc
120700980722     C                   KFLD                    §4nese
120800980722     C*
120900980722     C     K03Rco99      KLIST
121000980722     C                   KFLD                    xscsoc
121100980722     C                   KFLD                    rcokcc
121200980722     C                   KFLD                    rcoksc
121300980722     C*
121400980722     C     K02voc01      KLIST
121500980722     C                   KFLD                    xscsoc
121600980722     C                   KFLD                    §4nvoc
121700980722     C*
121800940223     C                   ENDSR
121900941103     C/EJECT
122000941103      ************************************************************
122100941103      * Reperimento dati società
122200941103      ************************************************************
122300941103     C     REPSOC        BEGSR
122400941103      *
122500970214     C                   CALLB     'XSOC'
122600941103     C                   PARM                    TIPXSC            6
122700941103     C                   PARM                    SOCXSC            3
122800941103     C                   PARM                    CDSXSC            9 0
122900941103     C                   PARM                    MODXSC            3
123000941103     C                   PARM      *BLANKS       RTNXSC            1
123100941103     C                   PARM                    XSOCDS
123200941103     C                   PARM                    KPJBA
123300941103      *
123400941103     C                   ENDSR
123500940223     C/EJECT
123600940506     C************************************************************
123700940506     C* REPERIMENTO TASTI DI FUNZIONE
123800940506     C************************************************************
123900940506     C     REPKEY        BEGSR
124000940506     C                   MOVEA     $FM1          $FM
124100940506     C                   EXSR      REPKKK
124200940506     C                   MOVEA     $FX           $FX1
124300940614     C                   ENDSR
124400940506     C/EJECT
124500940506     C************************************************************
124600940506     C* REPERIMENTO TASTI DI FUNZIONE
124700940506     C************************************************************
124800940506     C     REPKKK        BEGSR
124900940506     C*
125000940506     C                   CLEAR                   $FX
125100940506     C     1             DO        24            X
125200940506     C*
125300940506     C     $FM(X)        IFNE      *BLANK
125400940506     C*
125500940506     C                   MOVEL     $FM(X)        IDMSG
125600970214     C                   CALLB     'XRTVM'
125700940506     C                   PARM                    IDMSG            27
125800940506     C                   PARM                    TXTMSG          100
125900940506     C                   PARM                    ERRMSG            1
126000940506     C*
126100940506     C     ERRMSG        IFNE      *ON
126200940506     C                   MOVEL     TXTMSG        $FX(X)
126300940506     C                   ELSE
126400940506     C                   MOVEL     *ALL' '       $FX(X)
126500940506     C                   END
126600940506     C*
126700940506     C                   END
126800940506     C*
126900940506     C                   END
127000940506     C*
127100940506     C                   ENDSR
127200940509     C/EJECT
127300940223     C************************************************************
127400940223     C* INIZIALIZZAZIONE VARIABILI
127500940223     C************************************************************
127600940223     C     INZVAR        BEGSR
127700951124     C*
127800951124     C* Chiamata XSOC
127900951124     C*
128000951124     C                   MOVEL     'SOC001'      TIPXSC
128100951124     C                   EXSR      REPSOC
128200951124     C     RTNXSC        IFNE      '1'
128300951124     C                   MOVEL     XSOCDS        SOC001
128400951124     C                   ELSE
128500951124     C                   MOVE      *ON           $FINE
128600951124     C* fine programma
128700951124     C                   EXSR      ENDPGM
128800951124     C                   ENDIF
128900940224     C*
129000940224     C* Dati da PGM guida
129100940224     C*
129200940315    >C                   MOVEL     KPJBU         D002DS
129300940128     C*
129400940223     C* Variabili per gestione videate
129500940128     C*
129600940223     C                   MOVE      'D1'          $GEST
129700940223     C                   MOVE      *OFF          $FINE
129800940223     C                   MOVE      *ON           $INZD1
129900940224     C                   MOVE      *BLANK        $LASTG
130000940224     C                   MOVE      *BLANK        $LASTV
130100940506     C                   Z-ADD     0             $ULKD1            3 0
130200940127     C*
130300940223     C* Variabili appoggio
130400940127     C*
130500940224     C                   Z-ADD     0             CURR
130600940224     C                   Z-ADD     0             CURC
130700940207     C*
130800940207     C* Valorizzazione campi univoci testate
130900940207     C*
131000940614     C                   CLEAR                   T1
131100940614     C                   MOVEL     KNSIF         NOMSIF
131200941109     C                   MOVEL     XSCDSI        NOMDIT
131300941027     C                   MOVEL     DSPGM         NOMPGM
131400940224     C                   Z-ADD     1             X
131500940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
131600940224     C     *IN21         IFEQ      *ON
131700940224     C                   MOVEL     $ID(X)        IDMSG
131800970214     C                   CALLB     'XRTVM'
131900940224     C                   PARM                    IDMSG            27
132000940224     C                   PARM                    TXTMSG          100
132100940224     C                   PARM                    ERRMSG            1
132200940224     C                   PARM      *ON           CTRMSG            1
132300940224     C                   PARM      30            LENMSG            3 0
132400940224     C     ERRMSG        IFNE      *ON
132500940224     C                   MOVEL     TXTMSG        T1OPE
132600940224     C                   ELSE
132700940224     C                   MOVEL     *ALL'?'       T1OPE
132800940224     C                   ENDIF
132900940224     C                   END
133000940224     C*
133100940224     C* Inizializzazione/Decodifica prima videata
133200940224     C*
133300940224     C                   EXSR      INZD1
133400940224     C                   MOVE      *OFF          $INZD1
133500960228     C* Se opzione = '07' richiesti solo controlli
133600960228     C     OPZ002        IFEQ      '07'
133700960228     C                   MOVEL     *IN99         ERR002
133800960228     C                   MOVEL     '0'           RET002
133900960228     C                   MOVEL     *ON           $FINE
134000960228     C                   ENDIF
134100940127     C*
134200940117     C                   ENDSR
134300940128     C************************************************************
134400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
134500940223     C************************************************************
134600940223     C     DEFCAM        BEGSR
134700940223     C*
134800940223     C* klist
134900940207     C*
135000940223     C*
135100940223     C* Variabili per gestione videate
135200940223     C*
135300940223     C                   MOVE      *BLANK        $GEST             2
135400940223     C                   MOVE      *BLANK        $FINE             1
135500940223     C                   MOVE      *BLANK        $INZD1            1
135600940223     C                   MOVE      *BLANK        $LASTG            2
135700940223     C                   MOVE      *BLANK        $LASTV            2
135800940223     C*
135900940223     C* Variabili appoggio
136000940223     C*
136100940223     C                   Z-ADD     0             CURR              2 0
136200940223     C                   Z-ADD     0             CURC              2 0
136300940223     C                   MOVE      *ZEROS        WIN              99
136400940224     C                   MOVE      *BLANK        WTBERR            1
136500940223     C*
136600940223     C* Indici
136700940223     C*
136800940223     C                   Z-ADD     0             X                 3 0
136900940223     C                   Z-ADD     0             Y                 3 0
137000940127     C*
137100940117     C                   ENDSR
137200940128     C************************************************************
137300940509     C/EJECT
137400940509     C************************************************************
137500940509     C* CONTROLLO ABILITAZIONE TASTI
137600940509     C************************************************************
137700940509     C     CTRKEY        BEGSR
137800940509     C                   SELECT
137900940509     C     $TASTO        WHENEQ    F01
138000940720     C     $FC(1)        IFEQ      '0'
138100940509     C                   SETON                                        99
138200940509     C                   ENDIF
138300940509     C     $TASTO        WHENEQ    F02
138400940720     C     $FC(2)        IFEQ      '0'
138500940509     C                   SETON                                        99
138600940509     C                   ENDIF
138700940509     C     $TASTO        WHENEQ    F03
138800940720     C     $FC(3)        IFEQ      '0'
138900940509     C                   SETON                                        99
139000940509     C                   ENDIF
139100940509     C     $TASTO        WHENEQ    F04
139200940720     C     $FC(4)        IFEQ      '0'
139300940509     C                   SETON                                        99
139400940509     C                   ENDIF
139500940509     C     $TASTO        WHENEQ    F05
139600940720     C     $FC(5)        IFEQ      '0'
139700940509     C                   SETON                                        99
139800940509     C                   ENDIF
139900940509     C     $TASTO        WHENEQ    F06
140000940720     C     $FC(6)        IFEQ      '0'
140100940509     C                   SETON                                        99
140200940509     C                   ENDIF
140300940509     C     $TASTO        WHENEQ    F07
140400940720     C     $FC(7)        IFEQ      '0'
140500940509     C                   SETON                                        99
140600940509     C                   ENDIF
140700940509     C     $TASTO        WHENEQ    F08
140800940720     C     $FC(8)        IFEQ      '0'
140900940509     C                   SETON                                        99
141000940509     C                   ENDIF
141100940509     C     $TASTO        WHENEQ    F09
141200940720     C     $FC(09)       IFEQ      '0'
141300940509     C                   SETON                                        99
141400940509     C                   ENDIF
141500940509     C     $TASTO        WHENEQ    F10
141600940720     C     $FC(10)       IFEQ      '0'
141700940509     C                   SETON                                        99
141800940509     C                   ENDIF
141900940509     C     $TASTO        WHENEQ    F11
142000940720     C     $FC(11)       IFEQ      '0'
142100940509     C                   SETON                                        99
142200940509     C                   ENDIF
142300940509     C     $TASTO        WHENEQ    F12
142400940720     C     $FC(12)       IFEQ      '0'
142500940509     C                   SETON                                        99
142600940509     C                   ENDIF
142700940509     C     $TASTO        WHENEQ    F13
142800940720     C     $FC(13)       IFEQ      '0'
142900940509     C                   SETON                                        99
143000940509     C                   ENDIF
143100940509     C     $TASTO        WHENEQ    F14
143200940720     C     $FC(14)       IFEQ      '0'
143300940509     C                   SETON                                        99
143400940509     C                   ENDIF
143500940509     C     $TASTO        WHENEQ    F15
143600940720     C     $FC(15)       IFEQ      '0'
143700940509     C                   SETON                                        99
143800940509     C                   ENDIF
143900940509     C     $TASTO        WHENEQ    F16
144000940720     C     $FC(16)       IFEQ      '0'
144100940509     C                   SETON                                        99
144200940509     C                   ENDIF
144300940509     C     $TASTO        WHENEQ    F17
144400940720     C     $FC(17)       IFEQ      '0'
144500940509     C                   SETON                                        99
144600940509     C                   ENDIF
144700940509     C     $TASTO        WHENEQ    F18
144800940720     C     $FC(18)       IFEQ      '0'
144900940509     C                   SETON                                        99
145000940509     C                   ENDIF
145100940509     C     $TASTO        WHENEQ    F19
145200940720     C     $FC(19)       IFEQ      '0'
145300940509     C                   SETON                                        99
145400940509     C                   ENDIF
145500940509     C     $TASTO        WHENEQ    F20
145600940720     C     $FC(20)       IFEQ      '0'
145700940509     C                   SETON                                        99
145800940509     C                   ENDIF
145900940509     C     $TASTO        WHENEQ    F21
146000940720     C     $FC(21)       IFEQ      '0'
146100940509     C                   SETON                                        99
146200940509     C                   ENDIF
146300940509     C     $TASTO        WHENEQ    F22
146400940720     C     $FC(22)       IFEQ      '0'
146500940509     C                   SETON                                        99
146600940509     C                   ENDIF
146700940509     C     $TASTO        WHENEQ    F23
146800940720     C     $FC(23)       IFEQ      '0'
146900940509     C                   SETON                                        99
147000940509     C                   ENDIF
147100940509     C     $TASTO        WHENEQ    F24
147200940720     C     $FC(24)       IFEQ      '0'
147300940509     C                   SETON                                        99
147400940509     C                   ENDIF
147500940509     C                   ENDSL
147600940509     C                   ENDSR
147700940509     C/EJECT
147800940509     C************************************************************
147900940509     C* RIEMPIMENTO TASTI FUNZIONE
148000940509     C************************************************************
148100940509     C     RIEKEY        BEGSR
148200940509     C*
148300940509     C* Input :
148400940509     C* - $FX = schiera con tutti i testi disponibili
148500940509     C* - $FC = schiere con i flag di validità associati ai testi
148600940509     C* - $FR = schiere con i flag di REVERSE IMAGE
148700940509     C* - $ULK = ultimo testo caricato
148800940509     C* Output :
148900940509     C* - $ULK
149000940509     c* - $KEY1= 1° riga di tasti funzione
149100940509     c* - $KEY2= 2° riga di tasti funzione
149200940509     C*
149300970214     C                   CALLB     'XKEYMSG'
149400940509     C* input
149500940509     C                   PARM                    $FX
149600940509     C                   PARM                    $FC
149700940509     C                   PARM                    $FR
149800940509     C                   PARM                    $ULK              3 0
149900940509     C* output
150000940509     C                   PARM                    $KEY1            79
150100940509     C                   PARM                    $KEY2            79
150200940509     C*
150300940509     C                   ENDSR
150400030828
150500030828     ***********************************************************************
150600030828     **
150700030828     ** Chiamata a ANA964R
150800030828     **
150900030828     ***********************************************************************
151000030828     C     CalANA964R    BEGSR
151100030828
151200030828     C                   EVAL      Opz964 = Ricerca
151300030828     C                   EVAL      All964 = BlankValido
151400030828     C                   EVAL      KPJBU = ANA964DS
151500030828
151600030828     C                   CALL      'ANA964R'
151700030828     C                   PARM                    KPJBA
151800030828
151900030828     C                   EVAL      ANA964DS = KPJBU
152000030828
152100030828     C                   ENDSR
152200030828
152300030828     ***********************************************************************
152400030828     **
152500030828     ** Chiamata a X07VALR
152600030828     **
152700030828     ***********************************************************************
152800030828     C     CalX07VALR    BEGSR
152900030828
153000030828     C                   CALLB     'X07VALR'
153100030828     C                   PARM                    X07ValDS
153200030828
153300030828     C                   ENDSR
153400940615**
15350094022401PROMSG    *LIBL     COS0001    immissione
15360094022402PROMSG    *LIBL     COS0002    modifica
15370094022403PROMSG    *LIBL     COS0003    copia
15380094022404PROMSG    *LIBL     COS0004    annullamento
15390094022405PROMSG    *LIBL     COS0005    visualizzazione
154000940509** TASTI DI FUNZIONE  D1
154100940509PROMSG    *LIBL     KEY0001  01
154200940509PROMSG    *LIBL     KEY0002  02
154300940509PROMSG    *LIBL     KEY0003  03
154400940509PROMSG    *LIBL     KEY0004  04
154500940509PROMSG    *LIBL     KEY0005  05
154600940509PROMSG    *LIBL     KEY0006  06
154700940509PROMSG    *LIBL     KEY0007  07
154800940509PROMSG    *LIBL     KEY0008  08
154900940509PROMSG    *LIBL     KEY0009  09
155000940509PROMSG    *LIBL     KEY0010  10
155100940509PROMSG    *LIBL     KEY0011  11
155200940509PROMSG    *LIBL     KEY0012  12
155300940509PROMSG    *LIBL     KEY0013  13
155400940509PROMSG    *LIBL     KEY0014  14
155500940509PROMSG    *LIBL     KEY0015  15
155600940509PROMSG    *LIBL     KEY0016  16
155700940509PROMSG    *LIBL     KEY0017  17
155800940509PROMSG    *LIBL     KEY0018  18
155900940509PROMSG    *LIBL     KEY0019  19
156000940509PROMSG    *LIBL     KEY0020  20
156100940509PROMSG    *LIBL     KEY0021  21
156200940509PROMSG    *LIBL     KEY0022  22
156300940509PROMSG    *LIBL     KEY0023  23
156400940509PROMSG    *LIBL     KEY0024  24
156500940509**  ABILITAZIONE D1
1566009407200N  01
1567009407200N  02
1568009407201N  03
1569009407201N  04
1570009407201N  05
1571009407201N  06
1572009407201N  07
1573009407201N  08
1574009407200N  09
1575009407200N  10
1576009407200N  11
1577009407201N  12
1578009407200N  13
1579009407200N  14
1580009407200N  15
1581009407200N  16
1582009407200N  17
1583009407200N  18
1584009407200N  19
1585009407200N  20
1586009407200N  21
1587009407200N  22
1588009407200N  23
1589009407200N  24
159000070206**
159100070206DES   Descrizione motivazione ....
