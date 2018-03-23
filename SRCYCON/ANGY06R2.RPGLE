000100000113     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
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
004100940614$001 FANGY06D2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
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
006000940614$003 D EXTDS         E DS                  EXTNAME(ANGY06DS) INZ
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
011900970509     D* Unita
012000970509     D  UNI002                26     33
012100940614     D*-------------
012200940201     D*-------------
012300940201     D DSFMT           DS
012400940506     D  $TASTO               369    369
012500940201     D  NRG                  370    370
012600940201     D  NCL                  371    371
012700940201     D*-------------
012800940201     D* posizione cursore
012900940201     D CURSOR          DS
013000940223     D  RIRI                   1      2B 0 INZ
013100940201     D  R$$                    2      2
013200940223     D  COCO                   3      4B 0 INZ
013300940201     D  C$$                    4      4
013400940207     D*-------------
013500940207     D* Reperimento nome PGM
013600940207     D STATUS         SDS           333
013700940207     D  DSPGM            *PROC
013800010406X0113D*-------------
013900010406  "  D* Reperimento titolo tabella
014000010618  "  D  WRKTIT         S             35A
014100010406  "  D  WRKTBCOD       S                   LIKE(XTBCOD)
014200010406  "  D  LENTIT         S              3  0
014300010406X0113D  I              S              3  0
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
017900940128     I*----------------------------------------------------*
018000940615     ID1
018100940614$005 I              D1DES                       DESY06
018100940614$005 I              D1CAUFAT                    CAUFATY06
018100940614$005 I              D1LBRIVA                    LBRIVAY06
018100940614$005 I              D1CAUSFE                    CAUSFEY06
018100940614$005 I              D1KCCFE                     KCCFEY06
018100940614$005 I              D1KSCFE                     KSCFEY06
018100940614$005 I              D1SOC                       SOCY06
018100940614$005 I              D1LIBGD1                    LIBGD1Y06
018100940614$005 I              D1LIBGD2                    LIBGD2Y06
018200940127     I*
018300940127     C*----------------------------------------------------*
018400940127     C*                MAIN LINE PROGRAM
018500940127     C*----------------------------------------------------*
018600940127     C*
018700940223     C* inizializzazione variabili
018800940223     C                   EXSR      INZVAR
018900940223     C*
019000940223     C     $FINE         DOWEQ     *OFF
019100940202     C     $GEST         CASEQ     'D1'          GESD1
019200940117     C                   END
019300940117     C                   END
019400940201     C*
019500940325     C* fine programma
019600940325     C                   EXSR      ENDPGM
019700940325     C/EJECT
019800940325     C************************************************************
019900940325     C* RICERCHE
020000940325     C************************************************************
020100940325     C     ENDPGM        BEGSR
020200940325     C*
020300940614     C* disallocazione rcd nel caso di
020400971110$007 C                   MOVE      'Y06'         XTBCOD
020500940614    >C                   MOVE      '7'           XTBRIC
020600970214     C                   CALLB     'XATB'
020700940614     C                   PARM                    XATBDS
020800940325     C*
020900940325     C* passaggio dati al pgm chiamante
021000940715     C                   MOVE      *BLANKS       KPJBU
021100940325    >C                   MOVEL     D002DS        KPJBU
021200940325     C*
021300940325     C                   SETON                                            RT
021400940325     C*
021500940325     C                   ENDSR
021600940325     C/EJECT
021700940201     C************************************************************
021800940201     C* RICERCHE
021900940201     C************************************************************
022000940127     C     SEARCH        BEGSR
022100940127     C*
022200940127     C* determino Riga/Colonna del cursore
022300940127     C                   MOVE      NRG           R$$
022400940127     C                   MOVE      NCL           C$$
022500940127     C                   Z-ADD     RIRI          CURR
022600940127     C                   Z-ADD     COCO          CURC
022700940127     C*
022800940224     C* I campi che al momento dell'F4 risultano protetti non devono
022900940224     C* essere abilitati alla funzione di ricerca
023000940224     C*
0231009401271    C                   SELECT
023200940127     C*
023300940315     C* FMTD1  - PACCHETTO
023400940614    >C*          H1NMFL    WHEQ 'D1PKG'
023500940207     C* imposto pos. del cursore
023600940614     C*                    Z-ADDCURR      H1RIGA
023700940614     C*                    Z-ADDCURC      H1COLO
023800940127     C*
023900940223     C*
024000940223     C* ricerca non abilitata in questa posizione
0241009402231O   C                   OTHER
024200941005     C                   SETON                                        9899
024300941005     C* imposto pos. del cursore
024400941005     C                   Z-ADD     CURR          H1RIGA
024500941005     C                   Z-ADD     CURC          H1COLO
024600940127     C*
0247009401271-   C                   ENDSL
024800940127     C*
024900940506     C                   MOVE      *HIVAL        $LASTV
025000940127     C                   ENDSR
025100940615     C/EJECT
025200940615     C************************************************************
025300940615     C* TRADUZIONI
025400940615     C************************************************************
025500940615     C     LINGUA        BEGSR
025600940615     C*
025700940615     C* determino Riga/Colonna del cursore
025800940615     C                   MOVE      NRG           R$$
025900940615     C                   MOVE      NCL           C$$
026000940615     C                   Z-ADD     RIRI          CURR
026100940615     C                   Z-ADD     COCO          CURC
026200940615     C*
026300940615     C*
026400940615     C*
026500940615     C* FMTD1  - CAMPI DA TRADURRE
026600940615     C* imposto pos. del cursore
026700940615     C                   Z-ADD     CURR          H1RIGA
026800940615     C                   Z-ADD     CURC          H1COLO
026900030714     x****               MOVEL     H1NMFL        W005              5
027000030714     x****               MOVE      W005          W003              3
027100030714     x****               MOVEL     W003          W006              6
027200030714     C                   MOVEL     H1NMFL        W008              8
027300030714     C                   MOVE      W008          W003              6
027400030714     C                   MOVE      W008          W006              6
027500940620     C                   MOVE      *ZERO         KEY              15
027600940616     C                   MOVE      D1COD         KEY
027700940615     C                   SELECT
029200940615     C*
029300940615     C*
029400940615     C* ricerca non abilitata in questa posizione
0295009406151O   C                   OTHER
029600940615     C                   SETON                                        98
029700940615     C*
0298009406151-   C                   ENDSL
029900940615     C*
030000940615     C                   MOVE      *HIVAL        $LASTV
030100940615     C                   ENDSR
030200940225     C/EJECT
030300940225     C************************************************************
030400940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
030500940225     C************************************************************
030600940225     C     GESPIE        BEGSR
030700940225     C*
030800940225     C                   SELECT
030900940225     C*
031000940506     C     $GEST         WHENEQ    'D1'
031100940506     C     $ULKD1        IFEQ      0
031200940506     C* riempimento opzioni
031300940506     C                   EXSR      F24D1
031400940506     C                   ELSE
031500940614     C                   WRITE     Z1
031600940506     C                   ENDIF
031700940225     C*
031800940510     C*
031900940225     C                   ENDSL
032000940225     C*
032100940225     C                   ENDSR
032200940131     C/EJECT
032300940131     C************************************************************
032400940510     C* GESTIONE VIDEO RECORD D1
032500940131     C************************************************************
032600940127     C     GESD1         BEGSR
032700940225     C* inizializzazione videata
032800940223     C     $INZD1        IFEQ      *ON
032900940127     C                   EXSR      INZD1
033000940223     C                   MOVE      *OFF          $INZD1
033100940117     C                   END
033200940225     C* emissione piede videata se proveniente da altra
033300940225     C* salvataggio valore formato di provenienza
033400940207     C     $LASTV        IFNE      'D1'
033500940614     C                   WRITE     T1
033600940225     C                   EXSR      GESPIE
033700940202     C                   MOVE      $LASTV        $LASTG
033800940202     C                   MOVE      'D1'          $LASTV
033900940202     C                   END
034000940131     C*
034100940203     C                   SELECT
034200940203     C* Annullamento
034300940315    >C     OPZ002        WHENEQ    '04'
034400940224     C* Visualizzazione
034500940315    >C     OPZ002        OREQ      '05'
034600940620     C                   SETON                                        0203
034700940620     C* Manutenzione
034800940620    >C     OPZ002        WHENEQ    '02'
034900940620     C                   SETON                                        02
035000940620     C                   SETOFF                                       03
035100940620     C*
035200940620     C                   OTHER
035300940620     C*
035400940620     C                   SETOFF                                       0203
035500940203     C                   ENDSL
035600940620     C* Immissione/Modifica/Copia
035700940620     C                   EXFMT     D1
035800940506     C* controllo abilitazione tasti funzione
035900940506     C*
036000940506     C                   EXSR      CTRKD1
036100940506     C*
036200940506     C     *IN99         IFEQ      *OFF
036300940131     C*
0364009402031    C                   SELECT
036500940510     c* ROLLUP
036600940131     C* F3=Fine
036700940506     C     $TASTO        WHENEQ    F03
036800940309     C                   EXSR      F03D1
036900940131     C* F4=Ricerca
037000940506     C     $TASTO        WHENEQ    F04
037100940127     C                   EXSR      SEARCH
037200940224     C* F5=Refresh
037300940506     C     $TASTO        WHENEQ    F05
037400940309     C                   EXSR      F05D1
037500940223     C* F8=Successivo
037600940506     C     $TASTO        WHENEQ    F08
037700940309     C                   EXSR      F08D1
037800940131     C* F12=Ritorno
037900940506     C     $TASTO        WHENEQ    F12
038000940309     C                   EXSR      F12D1
038100940315     C* F24=Altri tasti
038200940506     C     $TASTO        WHENEQ    F24
038300940315     C                   EXSR      F24D1
038400940131     C*
0385009402031O   C                   OTHER
038600940127     C                   EXSR      CTRD1
038700940616     C     *IN99         IFEQ      *OFF
038800940616     C                   SELECT
038900940616     C* F7=Traduzione
039000940616     C     $TASTO        WHENEQ    F07
039100940616     C                   EXSR      LINGUA
039200940616     C* F6
0393009406162    C     $TASTO        WHENEQ    F06
039400940224     C* eseguo aggiornamento
039500940315    >C                   EXSR      AGGANA
039600940224     C* eseguo operazioni del dopo-aggiornamento
0397009403183    C     *IN99         IFEQ      *OFF
039800940318     C                   EXSR      GESAGG
0399009403183-   C                   ENDIF
0400009406162-   C                   ENDSL
040100940616     C                   ENDIF
040200940203     C*
0403009402241-   C                   ENDSL
040400940224     C*
0405009405062-   C                   ENDIF
040600940117     C                   ENDSR
040700940506     C/EJECT
040800940506     C************************************************************
040900940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
041000940506     C************************************************************
041100940506     C     CTRKD1        BEGSR
041200940506     C                   SETOFF                                       99
041300940506     C                   MOVEA     $FC1          $FC
041400940506     C                   EXSR      CTRKEY
041500940506     C     *IN99         IFEQ      *ON
041600940506     C                   SETON                                        97
041700940506     C                   ENDIF
041800940506     C                   ENDSR
041900940117      ************************************************************
042000940207      * INIZIALIZZAZIONE VIDEATA DATI
042100940117      ************************************************************
042200940127     C     INZD1         BEGSR
042300940131     C*
042400940615     C                   CLEAR                   D1
042500010406X0113C*
042600010406  "  C* reperimento titolo
042700010406  "  C                   clear                   xatbds
042800010406  "  C                   eval      xtbkey      = *zero
042900010417X0113C* inserita la seguenta riga
043000010417$002 C                   move      'Y06'         xtbkey
043100010417X0113C                   eval      xtbazi      = xscsoc
043200010406  "  C                   eval      xtblin      = xsclin
043300010406  "  C                   eval      xtbcod      = 'AAA'
043400010406  "  C                   eval      xtbric      = '1'
043500010406  "  C                   eval      xtbalc      = '0'
043600010406  "  C                   CALLB     'XATB'
043700010406  "  C                   PARM                    XATBDS
043800010406  "  C                   if        xtberr <> *off
043900010406  "  C                   movel     *all'?'       WrkTit
044000010406  "  C                   else
044100010406  "  C                   movel     xtbuni        WrkTit
044200010618  "  C                   move      '     '       WrkTit
044300010406  "  C                   endif
044400010406  "  C*
044500010406  "  C* centratura
044600010406  "  C                   eval      WrkTit=%trimr(Wrktit)
044700010406  "  C     ' '           CHECKR    Wrktit        I
044800010406  "  C                   call      'X71CENTRA'
044900010406  "  C                   parm                    Wrktit
045000010406  "  C                   parm                    I
045100010406  "  C                   parm                    lentit
045200010406  "  C                   movel     Wrktit        T1tit
045300010406X0113C*
045400990827X0079C* Aggiunta riga seguente
045500990827$028 C
045600941006     C                   MOVE      '0'           D1RIS
045700940616     C                   MOVE      KEY002        D1COD
045800940615     C                   MOVE      AZI002        XTBAZI
045900940615     C                   MOVE      LIN002        XTBLIN
046000940616     C                   MOVE      *ALL'0'       XTBKEY
046100940615     C                   MOVE      KEY002        XTBKEY
046200970509     C                   MOVE      UNI002        XTBUNT
046300940615$007 C                   MOVE      'Y06'         XTBCOD
046400940225     C*
046500940207     C*
046600940224     C* valorizzo la chiave univoca per reperire i dati
046700940224     C* se non immissione
0468009403151   >C     OPZ002        IFNE      '01'
046900940315    >C*-------------------------------------------------------------
047000940315    >C* impostare i campi chiave ricevuti
047100940315    >C*-------------------------------------------------------------
047200940318     C*
047300940614     C*
047400940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0475009406142   >C     OPZ002        IFEQ      '02'
047600940315    >C     OPZ002        OREQ      '04'
047700940614     C                   MOVE      '1'           XTBALC
047800940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0479009406142E   C                   ELSE
048000940614     C                   MOVE      '0'           XTBALC
0481009406142-   C                   ENDIF
048200940614     C*
048300940616     C                   MOVE      '1'           XTBRIC
048400970214     C                   CALLB     'XATB'
048500940614     C                   PARM                    XATBDS
048600940318     C*
048700940614     C     XTBERR        IFEQ      '0'
048800940614     C                   MOVEL     XTBUNI        EXTDS
048900970512     c                   move      xtbunt        d1uniTA           8
049000941108     C* Giro la data se non blank
049100970214     c                   move      xtbdsc        xtbdscA          10
049200030313     C                   MOVE      XTBRIS        D1RIS
049300970214     C     XTBDSCa       IFNE      DATA
049400970214     C                   CALLB     'XDT4000'
049500970214     C                   PARM      XTBDSCA       XDTAMG           10
049600940621     C                   PARM      *ZEROS        XDTGMA            6 0
049700940621     C                   PARM      2             XDTSTA            1 0
049800940621     C                   MOVE      XDTGMA        D1DSC
049900030313     x***************    MOVE      XTBRIS        D1RIS
050000941108     C                   ELSE
050100941108     C                   MOVE      *ZEROS        D1DSC
050200941108     C                   ENDIF
050300941108     C                   ENDIF
050400940614     C*
050500940224     C*
050600940224     C* se il rcd da gestire non viene trovato
050700940318     C* oppure è allocato da altro pgm
050800940224     C* ritorno al pgm guida avvertendolo dell'errore
0509009406142    C     XTBERR        IFNE      '0'
051000940224     C                   MOVE      *ON           $FINE
051100940315    >C                   MOVE      '1'           ERR002
051200940224     C*
051300940224     C* altrimenti
0514009402242E   C                   ELSE
051500940224     C* riempio la videata
051600940224     C                   EXSR      RIED1
051700940224     C* richiamo routine dei ctrl per decodificarla
051800940614    >C                   EXSR      CTRD1
051900960228     C                   MOVEL     *IN99         SAV99             1
052000960228     C* Se la gestione è più videate
052100960305    >C                   EXSR      CTRD2
052200960305    >C     *IN99         IFEQ      *ON
052300960305    >C                   MOVEL     *IN99         SAV99             1
052400960305    >C                   ENDIF
052500940224     C                   MOVE      *ALL'0'       IN5098
052600940224     C                   MOVEA     IN5098        *IN(50)
052700940224     C* definizione indicatori su video
052800940224     C                   SETOFF                                       02
0529009402243    C                   SELECT
053000940315    >C     OPZ002        WHENEQ    '02'
053100940224     C                   SETON                                        02
053200940315    >C     OPZ002        WHENEQ    '05'
053300940506     C* disabilita tasti
053400940720     C                   MOVE      '0'           $FC(6)
053500940720     C                   MOVE      '0'           $FC(5)
053600940720     C                   MOVE      '0'           $FC(4)
0537009402243-   C                   ENDSL
053800940224     C*
0539009402242-   C                   ENDIF
054000940223     C*
0541009402241-   C                   ENDIF
054200940224     C*
054300940117     C                   ENDSR
054400940207     C/EJECT
054500940207     C************************************************************
054600940510     C* RIEMPIMENTO VIDEATA  D1
054700940207     C************************************************************
054800940207     C     RIED1         BEGSR
054900940207     C*
055000990827$025 C
055100940207     C*
055200940207     C                   ENDSR
055300940309     C/EJECT
055400940309     C************************************************************
055500940309     C* GESTIONE F03 VIDEO D1
055600940309     C************************************************************
055700940309     C     F03D1         BEGSR
055800940309     C*
055900940309     C                   MOVE      *ON           $FINE
056000940315    >C                   MOVE      '1'           RET002
056100940325     C* fine programma
056200940325     C                   EXSR      ENDPGM
056300940325     C*
056400940325     C                   ENDSR
056500940309     C/EJECT
056600940309     C************************************************************
056700940309     C* GESTIONE F05 VIDEO D1
056800940309     C************************************************************
056900940309     C     F05D1         BEGSR
057000940309     C*
057100940309     C                   MOVE      *ON           $INZD1
057200940309     C*
057300940309     C                   ENDSR
057400940309     C/EJECT
057500940309     C************************************************************
057600940309     C* GESTIONE F08 VIDEO D1
057700940309     C************************************************************
057800940309     C     F08D1         BEGSR
057900940309     C*
058000940309     C                   MOVE      *ON           $FINE
058100940315    >C                   MOVE      '0'           RET002
058200940309     C*
058300940309     C                   ENDSR
058400940309     C/EJECT
058500940309     C************************************************************
058600940309     C* GESTIONE F12 VIDEO D1
058700940309     C************************************************************
058800940309     C     F12D1         BEGSR
058900940309     C*
059000940309     C                   MOVE      *ON           $FINE
059100940315    >C                   MOVE      '2'           RET002
059200940309     C*
059300940309     C                   ENDSR
059400940309     C/EJECT
059500940309     C************************************************************
059600940309     C* GESTIONE F24 VIDEO D1
059700940309     C************************************************************
059800940309     C     F24D1         BEGSR
059900940309     C*
060000940506     C*
060100940506     C                   MOVEA     $FX1          $FX
060200940506     C                   MOVEA     $FC1          $FC
060300940506     C                   MOVEA     $FR1          $FR
060400940506     C                   Z-ADD     $ULKD1        $ULK
060500940506     C                   EXSR      RIEKEY
060600941017     C                   MOVEA     $FC           $FC1
060700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
060800940506     C                   MOVEL     $KEY1         Z1KE1
060900940506     C                   MOVEL     $KEY2         Z1KE2
061000940614     C                   WRITE     Z1
061100940506     C*
061200940309     C                   ENDSR
061300940207     C/EJECT
061400940131     C************************************************************
061500940207     C* CONTROLLO VIDEATA
061600940131     C************************************************************
061700940127     C     CTRD1         BEGSR
061800940131     C*
061900940127     C                   SETOFF                                       99
062000940127     C* non imposto pos. del cursore
062100940127     C                   Z-ADD     0             H1RIGA
062200940127     C                   Z-ADD     0             H1COLO
062300940621     C*
062400940621     C* CTRL DATA
062500940621     C*
062600941109     C                   MOVE      DATA          WRKDSC           10
062700940621     C     D1DSC         IFNE      0
062800970214     C                   CALLB     'XDT4000'
062900941109     C                   PARM                    XDTAMG           10
063000940621     C                   PARM      D1DSC         XDTGMA            6 0
063100940621     C                   PARM      3             XDTSTA            1 0
063200940621     C     XDTSTA        IFNE      0
063300940621     C                   SETON                                        5599
063400940621     C                   ELSE
063500941108     C                   MOVE      XDTAMG        WRKDSC
063600940621     C                   ENDIF
063700940621     C                   ENDIF
063800940223     C*
063900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
064000940223     C*
0641009403151   >C     OPZ002        IFNE      '04'
064200971110     C*
064300971110     C* controllo codice vuoto
064400971110$017 C     D1COD         IFEQ      *ALL' '
064500971110     C                   SETON                                        5099
064600971110     C                   ENDIF
064700971110     C*
064800940614$009 C     DESY06        IFEQ      *BLANK
064800940614$009 C                   SETON                                        6099
064800940614$009 C                   ENDIF
064800940614$009 C     CAUFATY06     IFEQ      *BLANK
064800940614$009 C                   SETON                                        6199
064800940614$009 C                   ENDIF
064800940614$009 C     SOCY06        IFEQ      *BLANK
064800940614$009 C                   SETON                                        6299
064800940614$009 C                   ENDIF
065200940207     C*
065300940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
065400940315    >C     OPZ002        IFEQ      '01'
065500940315    >C     OPZ002        OREQ      '03'
065600940315    >C*-------------------------------------------------------------
065700940315    >C* impostare i campi chiave e verificare l'esistenza del record
065800940315    >C*-------------------------------------------------------------
065900971110$007 C                   MOVE      'Y06'         XTBCOD
066000940614     C                   MOVE      *ALL'0'       XTBKEY
066100940620    >C                   MOVE      D1COD         XTBKEY
066200970512    >C                   MOVE      D1UNITA       XTBUNT
066300940614     C                   MOVE      '0'           XTBALC
066400940614     C                   MOVE      '1'           XTBRIC
066500970214    >C                   CALLB     'XATB'
066600940614     C                   PARM                    XATBDS
066700940614     C     XTBERR        IFEQ      '0'
066800940223     C                   SETON                                        5999
066900940223     C                   ENDIF
067000940620     C* Controllo se compreso nei range indice
067100940620     C     XTBERR        IFEQ      '3'
067200940620    >C                   SETON                                        5899
067300940620     C                   END
067400940620     C*
067500940620     C* REPERISCO AUTORIZZAZIONI UTENTE
067600940620     C                   CLEAR                   XCHATB
067700941118     C                   MOVE      XSCSOC        XCTSOC
067800941118     C                   MOVE      XSCDUN        XCTUNI
067900940620     C                   MOVEL     KNMUS         XCTPRF
068000940620     C                   MOVE      XTBCOD        XCTCOD
068100940620     C                   CALL      'XCHKATB'
068200940620     C                   PARM                    XCHATB
068300941111     C                   MOVEL     XCTDDS        AUTDS
068400941027     C     XCTRTN        IFEQ      1
068500941027     C                   MOVEL     'N'           MANGRU
068600941027     C                   ENDIF
068700940620     C* Non ammessa immissione di gruppo
068800940620     C     XTBGRU        IFEQ      'G'
068900940620     C     MANGRU        ANDEQ     'N'
069000940620     C                   SETON                                        5799
069100940620     C                   END
069200940620     C* Non ammessa immissione di azienda
069300940620     C     XTBGRU        IFEQ      'A'
069400940620     C     MANAZI        ANDEQ     'N'
069500940620     C                   SETON                                        5699
069600940620     C                   END
069700940712     C* codice riservato non accessibile
069800941006     C     XTBRIS        IFGT      ABIRIS
069900941005    >C     OPZ002        ANDNE     '01'
070000941005    >C     OPZ002        ANDNE     '03'
070100940712     C                   SETON                                        5499
070200940712     C                   END
070300940224     C                   ENDIF
070400940224     C*
0705009402241-   C                   END
070600940131     C*
070700940302     C     *IN99         IFEQ      *ON
070800940510     C     $GEST         ANDEQ     'D1'
070900960228     C     OPZ002        ANDNE     '07'
071000940117     C                   MOVEA     *IN           WIN
071100940128     C                   MOVE      *ALL'0'       IN5098           49
071200940128     C                   MOVEA     IN5098        *IN(50)
071300940614     C                   WRITE     D1
071400940117     C                   MOVEA     WIN           *IN
071500940302     C                   ENDIF
071600940127     C*
071700940117     C                   ENDSR
071800960305     C/EJECT
071900960305     C************************************************************
072000960305     C* CONTROLLO VIDEATA D2
072100960305     C************************************************************
072200960305     C     CTRD2         BEGSR
072300960305     C*
072400960305     C                   ENDSR
072500940207     C/EJECT
072600940224     C************************************************************
072700940315    >C* AGGIORNAMENTO ANAGRAFICA
072800940224     C************************************************************
072900940315    >C     AGGANA        BEGSR
073000971110     C*
073100971110$007 C                   MOVE      'Y06'         XTBCOD
073200940614     C                   MOVE      *ALL'0'       XTBKEY
073300940614     C                   MOVE      D1COD         XTBKEY
073400970512     C                   MOVE      D1UNITA       XTBUNT
073500940207     C*
073600940207     C                   SELECT
073700940315    >C     OPZ002        WHENEQ    '01'
073800940315    >C     OPZ002        OREQ      '03'
073900940315    >C                   EXSR      RIEANA
074000950609     C                   MOVE      XSCCUT        XTBUTE
074100940614     C                   MOVE      '5'           XTBRIC
074200970214     C                   CALLB     'XATB'
074300940614    >C                   PARM                    XATBDS
074400940318     C* se rcd non scrivibile attivo errore
074500940615     C     XTBERR        IFEQ      '1'
074600940318    >C                   SETON                                        5999
074700940318     C                   END
074800940315    >C     OPZ002        WHENEQ    '02'
074900940621     C                   MOVE      '1'           XTBRIC
075000940621     C                   MOVE      '1'           XTBALC
075100970214     C                   CALLB     'XATB'
075200940621     C                   PARM                    XATBDS
075300940622    >C                   EXSR      RIEANA
075400950609     C                   MOVE      XSCCUT        XTBUTE
075500940614     C                   MOVE      '4'           XTBRIC
075600970214     C                   CALLB     'XATB'
075700940614    >C                   PARM                    XATBDS
075800940315    >C     OPZ002        WHENEQ    '04'
075900940621     C                   MOVE      '1'           XTBRIC
076000940621     C                   MOVE      '1'           XTBALC
076100970214     C                   CALLB     'XATB'
076200940621     C                   PARM                    XATBDS
076300940614     C                   MOVE      '6'           XTBRIC
076400970214     C                   CALLB     'XATB'
076500940614    >C                   PARM                    XATBDS
076600940621     C* annullo traduzioni
076700940621     C     XTBERR        IFEQ      '0'
076800940621     C                   EXSR      ANNTBT
076900940621     C                   END
077000940621     C*
077100940207     C                   ENDSL
077200940203     C*
077300940203     C                   ENDSR
077400940224     C/EJECT
077500940224     C************************************************************
077600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
077700940224     C************************************************************
077800940616     C     RIEANA        BEGSR
077900940616     C                   CLEAR                   XTBUNI
078000940621     C                   MOVE      WRKDSC        XTBDSC
078100940712     C                   MOVE      D1RIS         XTBRIS
078200981209$026 C
078300940616     C                   MOVEL     EXTDS         XTBUNI
078400940207     C                   ENDSR
078500940224     C/EJECT
078600940224     C************************************************************
078700940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
078800940224     C************************************************************
078900940224     C     GESAGG        BEGSR
079000940224     C*
079100940224     C* segnalo al pgm chiamante l'avvenuta conferma
079200940315    >C                   MOVE      *ON           OPR002
079300940224     C* segnalo al pgm chiamante l'aver premuto l'F6
079400940315    >C                   MOVE      '0'           RET002
079500940224     C*
0796009402241    C                   SELECT
079700940224     C* nel caso di immissione, ripristino la videata iniziale
079800940315    >C     OPZ002        WHENEQ    '01'
079900940224     C                   MOVE      *ON           $INZD1
080000940224     C* altrimenti ritorno al pgm chiamante
080100940224     C                   OTHER
080200940224     C                   MOVE      *ON           $FINE
0803009402241-   C                   ENDSL
080400940224     C*
080500940224     C                   ENDSR
080600940621     C/EJECT
080700940621     C************************************************************
080800940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
080900940621     C************************************************************
081000940621    >C     ANNTBT        BEGSR
081100940621     C                   MOVE      XTBAZI        TBTAZI
081200940621     C                   MOVE      XTBCOD        TBTCOD
081300940621     C                   MOVE      XTBKEY        TBTKEY
081400940621     C     KTBT          SETLL     ANTBT01L
081500940621     C                   DO        *HIVAL
081600940621     C     KTBT          DELETE    ANTBT01L                           21
081700940621     C  N21              END
081800940621     C*
081900940621     C                   ENDSR
082000940207     C/EJECT
082100940131     C************************************************************
082200940131     C* OPERAZIONI INIZIALI
082300940131     C************************************************************
082400940131     C     *INZSR        BEGSR
082500940131     C*
082600940127     C* Reperimento parametri
082700940127     C*
082800940117     C     *ENTRY        PLIST
082900940117     C                   PARM                    KPJBA
083000940712     C*
083100940712     C* Reperimento Codice utente
083200940712     C*
083300940223     C*
083400940506     C*
083500940506     C* Reperimento tasti di funzione
083600940506     C*
083700940506     C                   EXSR      REPKEY
083800940621     C*
083900940621     C* KLIST
084000940621     C*
084100940621     C     KTBT          KLIST
084200940621     C                   KFLD                    TBTAZI
084300940621     C                   KFLD                    TBTCOD
084400940621     C                   KFLD                    TBTKEY
084500010406X0113C*
084600010406  "  C* reperisco le dimensioni della variabile del titolo
084700010406X0113C                   EVAL      LENTIT = %Size(T1Tit)
084800940506     C*
084900940223     C                   ENDSR
085000941103     C/EJECT
085100941103      ************************************************************
085200941103      * Reperimento dati società
085300941103      ************************************************************
085400941103     C     REPSOC        BEGSR
085500941103      *
085600970214     C                   CALLB     'XSOC'
085700941103     C                   PARM                    TIPXSC            6
085800941103     C                   PARM                    SOCXSC            3
085900941103     C                   PARM                    CDSXSC            9 0
086000941103     C                   PARM                    MODXSC            3
086100941103     C                   PARM      *BLANKS       RTNXSC            1
086200941103     C                   PARM                    XSOCDS
086300941103     C                   PARM                    KPJBA
086400941103      *
086500941103     C                   ENDSR
086600940223     C/EJECT
086700940506     C************************************************************
086800940506     C* REPERIMENTO TASTI DI FUNZIONE
086900940506     C************************************************************
087000940506     C     REPKEY        BEGSR
087100940506     C                   MOVEA     $FM1          $FM
087200940506     C                   EXSR      REPKKK
087300940506     C                   MOVEA     $FX           $FX1
087400940614     C                   ENDSR
087500940506     C/EJECT
087600940506     C************************************************************
087700940506     C* REPERIMENTO TASTI DI FUNZIONE
087800940506     C************************************************************
087900940506     C     REPKKK        BEGSR
088000940506     C*
088100940506     C                   CLEAR                   $FX
088200940506     C     1             DO        24            X
088300940506     C*
088400940506     C     $FM(X)        IFNE      *BLANK
088500940506     C*
088600940506     C                   MOVEL     $FM(X)        IDMSG
088700970214     C                   CALLB     'XRTVM'
088800940506     C                   PARM                    IDMSG            27
088900940506     C                   PARM                    TXTMSG          100
089000940506     C                   PARM                    ERRMSG            1
089100940506     C*
089200940506     C     ERRMSG        IFNE      *ON
089300940506     C                   MOVEL     TXTMSG        $FX(X)
089400940506     C                   ELSE
089500940506     C                   MOVEL     *ALL' '       $FX(X)
089600940506     C                   END
089700940506     C*
089800940506     C                   END
089900940506     C*
090000940506     C                   END
090100940506     C*
090200940506     C                   ENDSR
090300940509     C/EJECT
090400940223     C************************************************************
090500940223     C* INIZIALIZZAZIONE VARIABILI
090600940223     C************************************************************
090700940223     C     INZVAR        BEGSR
090800951124     C*
090900951124     C* Chiamata XSOC
091000951124     C*
091100951124     C                   MOVEL     'SOC001'      TIPXSC
091200951124     C                   EXSR      REPSOC
091300951124     C     RTNXSC        IFNE      '1'
091400951124     C                   MOVEL     XSOCDS        SOC001
091500951124     C                   ELSE
091600951124     C                   MOVE      *ON           $FINE
091700951124     C* fine programma
091800951124     C                   EXSR      ENDPGM
091900951124     C                   ENDIF
092000940224     C*
092100940224     C* Dati da PGM guida
092200940224     C*
092300940315    >C                   MOVEL     KPJBU         D002DS
092400940128     C*
092500940223     C* Variabili per gestione videate
092600940128     C*
092700940223     C                   MOVE      'D1'          $GEST
092800940223     C                   MOVE      *OFF          $FINE
092900940223     C                   MOVE      *ON           $INZD1
093000940224     C                   MOVE      *BLANK        $LASTG
093100940224     C                   MOVE      *BLANK        $LASTV
093200940506     C                   Z-ADD     0             $ULKD1            3 0
093300940127     C*
093400940223     C* Variabili appoggio
093500940127     C*
093600940224     C                   Z-ADD     0             CURR
093700940224     C                   Z-ADD     0             CURC
093800940207     C*
093900940207     C* Valorizzazione campi univoci testate
094000940207     C*
094100940614     C                   CLEAR                   T1
094200940614     C                   MOVEL     KNSIF         NOMSIF
094300941109     C                   MOVEL     XSCDSI        NOMDIT
094400941027     C                   MOVEL     DSPGM         NOMPGM
094500940224     C                   Z-ADD     1             X
094600940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
094700940224     C     *IN21         IFEQ      *ON
094800940224     C                   MOVEL     $ID(X)        IDMSG
094900970214     C                   CALLB     'XRTVM'
095000940224     C                   PARM                    IDMSG            27
095100940224     C                   PARM                    TXTMSG          100
095200940224     C                   PARM                    ERRMSG            1
095300940224     C                   PARM      *ON           CTRMSG            1
095400940224     C                   PARM      30            LENMSG            3 0
095500940224     C     ERRMSG        IFNE      *ON
095600940224     C                   MOVEL     TXTMSG        T1OPE
095700940224     C                   ELSE
095800940224     C                   MOVEL     *ALL'?'       T1OPE
095900940224     C                   ENDIF
096000940224     C                   END
096100940224     C*
096200940224     C* Inizializzazione/Decodifica prima videata
096300940224     C*
096400940224     C                   EXSR      INZD1
096500940224     C                   MOVE      *OFF          $INZD1
096600960228     C* Se opzione = '07' richiesti solo controlli
096700960228     C     OPZ002        IFEQ      '07'
096800960228     C                   MOVEL     *IN99         ERR002
096900960228     C                   MOVEL     '0'           RET002
097000960228     C                   MOVEL     *ON           $FINE
097100960228     C                   ENDIF
097200030714     c*
097300030714     c* a seconda della lingua richiesta o meno, abilito o disablito F7
097400030714$029 C                   eval      $fc1(7)=*off
097500940127     C*
097600940117     C                   ENDSR
097700940128     C************************************************************
097800940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
097900940223     C************************************************************
098000940223     C     DEFCAM        BEGSR
098100940223     C*
098200940223     C* klist
098300940207     C*
098400940223     C*
098500940223     C* Variabili per gestione videate
098600940223     C*
098700940223     C                   MOVE      *BLANK        $GEST             2
098800940223     C                   MOVE      *BLANK        $FINE             1
098900940223     C                   MOVE      *BLANK        $INZD1            1
099000940223     C                   MOVE      *BLANK        $LASTG            2
099100940223     C                   MOVE      *BLANK        $LASTV            2
099200940223     C*
099300940223     C* Variabili appoggio
099400940223     C*
099500940223     C                   Z-ADD     0             CURR              2 0
099600940223     C                   Z-ADD     0             CURC              2 0
099700940223     C                   MOVE      *ZEROS        WIN              99
099800940224     C                   MOVE      *BLANK        WTBERR            1
099900940223     C*
100000940223     C* Indici
100100940223     C*
100200940223     C                   Z-ADD     0             X                 3 0
100300940223     C                   Z-ADD     0             Y                 3 0
100400940127     C*
100500940117     C                   ENDSR
100600940128     C************************************************************
100700940509     C/EJECT
100800940509     C************************************************************
100900940509     C* CONTROLLO ABILITAZIONE TASTI
101000940509     C************************************************************
101100940509     C     CTRKEY        BEGSR
101200940509     C                   SELECT
101300940509     C     $TASTO        WHENEQ    F01
101400940720     C     $FC(1)        IFEQ      '0'
101500940509     C                   SETON                                        99
101600940509     C                   ENDIF
101700940509     C     $TASTO        WHENEQ    F02
101800940720     C     $FC(2)        IFEQ      '0'
101900940509     C                   SETON                                        99
102000940509     C                   ENDIF
102100940509     C     $TASTO        WHENEQ    F03
102200940720     C     $FC(3)        IFEQ      '0'
102300940509     C                   SETON                                        99
102400940509     C                   ENDIF
102500940509     C     $TASTO        WHENEQ    F04
102600940720     C     $FC(4)        IFEQ      '0'
102700940509     C                   SETON                                        99
102800940509     C                   ENDIF
102900940509     C     $TASTO        WHENEQ    F05
103000940720     C     $FC(5)        IFEQ      '0'
103100940509     C                   SETON                                        99
103200940509     C                   ENDIF
103300940509     C     $TASTO        WHENEQ    F06
103400940720     C     $FC(6)        IFEQ      '0'
103500940509     C                   SETON                                        99
103600940509     C                   ENDIF
103700940509     C     $TASTO        WHENEQ    F07
103800940720     C     $FC(7)        IFEQ      '0'
103900940509     C                   SETON                                        99
104000940509     C                   ENDIF
104100940509     C     $TASTO        WHENEQ    F08
104200940720     C     $FC(8)        IFEQ      '0'
104300940509     C                   SETON                                        99
104400940509     C                   ENDIF
104500940509     C     $TASTO        WHENEQ    F09
104600940720     C     $FC(09)       IFEQ      '0'
104700940509     C                   SETON                                        99
104800940509     C                   ENDIF
104900940509     C     $TASTO        WHENEQ    F10
105000940720     C     $FC(10)       IFEQ      '0'
105100940509     C                   SETON                                        99
105200940509     C                   ENDIF
105300940509     C     $TASTO        WHENEQ    F11
105400940720     C     $FC(11)       IFEQ      '0'
105500940509     C                   SETON                                        99
105600940509     C                   ENDIF
105700940509     C     $TASTO        WHENEQ    F12
105800940720     C     $FC(12)       IFEQ      '0'
105900940509     C                   SETON                                        99
106000940509     C                   ENDIF
106100940509     C     $TASTO        WHENEQ    F13
106200940720     C     $FC(13)       IFEQ      '0'
106300940509     C                   SETON                                        99
106400940509     C                   ENDIF
106500940509     C     $TASTO        WHENEQ    F14
106600940720     C     $FC(14)       IFEQ      '0'
106700940509     C                   SETON                                        99
106800940509     C                   ENDIF
106900940509     C     $TASTO        WHENEQ    F15
107000940720     C     $FC(15)       IFEQ      '0'
107100940509     C                   SETON                                        99
107200940509     C                   ENDIF
107300940509     C     $TASTO        WHENEQ    F16
107400940720     C     $FC(16)       IFEQ      '0'
107500940509     C                   SETON                                        99
107600940509     C                   ENDIF
107700940509     C     $TASTO        WHENEQ    F17
107800940720     C     $FC(17)       IFEQ      '0'
107900940509     C                   SETON                                        99
108000940509     C                   ENDIF
108100940509     C     $TASTO        WHENEQ    F18
108200940720     C     $FC(18)       IFEQ      '0'
108300940509     C                   SETON                                        99
108400940509     C                   ENDIF
108500940509     C     $TASTO        WHENEQ    F19
108600940720     C     $FC(19)       IFEQ      '0'
108700940509     C                   SETON                                        99
108800940509     C                   ENDIF
108900940509     C     $TASTO        WHENEQ    F20
109000940720     C     $FC(20)       IFEQ      '0'
109100940509     C                   SETON                                        99
109200940509     C                   ENDIF
109300940509     C     $TASTO        WHENEQ    F21
109400940720     C     $FC(21)       IFEQ      '0'
109500940509     C                   SETON                                        99
109600940509     C                   ENDIF
109700940509     C     $TASTO        WHENEQ    F22
109800940720     C     $FC(22)       IFEQ      '0'
109900940509     C                   SETON                                        99
110000940509     C                   ENDIF
110100940509     C     $TASTO        WHENEQ    F23
110200940720     C     $FC(23)       IFEQ      '0'
110300940509     C                   SETON                                        99
110400940509     C                   ENDIF
110500940509     C     $TASTO        WHENEQ    F24
110600940720     C     $FC(24)       IFEQ      '0'
110700940509     C                   SETON                                        99
110800940509     C                   ENDIF
110900940509     C                   ENDSL
111000940509     C                   ENDSR
111100940509     C/EJECT
111200940509     C************************************************************
111300940509     C* RIEMPIMENTO TASTI FUNZIONE
111400940509     C************************************************************
111500940509     C     RIEKEY        BEGSR
111600940509     C*
111700940509     C* Input :
111800940509     C* - $FX = schiera con tutti i testi disponibili
111900940509     C* - $FC = schiere con i flag di validità associati ai testi
112000940509     C* - $FR = schiere con i flag di REVERSE IMAGE
112100940509     C* - $ULK = ultimo testo caricato
112200940509     C* Output :
112300940509     C* - $ULK
112400940509     c* - $KEY1= 1° riga di tasti funzione
112500940509     c* - $KEY2= 2° riga di tasti funzione
112600940509     C*
112700970214     C                   CALLB     'XKEYMSG'
112800940509     C* input
112900940509     C                   PARM                    $FX
113000940509     C                   PARM                    $FC
113100940509     C                   PARM                    $FR
113200940509     C                   PARM                    $ULK              3 0
113300940509     C* output
113400940509     C                   PARM                    $KEY1            79
113500940509     C                   PARM                    $KEY2            79
113600940509     C*
113700940509     C                   ENDSR
113800940615**
11390094022401PROMSG    *LIBL     COS0001    immissione
11400094022402PROMSG    *LIBL     COS0002    modifica
11410094022403PROMSG    *LIBL     COS0003    copia
11420094022404PROMSG    *LIBL     COS0004    annullamento
11430094022405PROMSG    *LIBL     COS0005    visualizzazione
114400940509** TASTI DI FUNZIONE  D1
114500940509PROMSG    *LIBL     KEY0001  01
114600940509PROMSG    *LIBL     KEY0002  02
114700940509PROMSG    *LIBL     KEY0003  03
114800940509PROMSG    *LIBL     KEY0004  04
114900940509PROMSG    *LIBL     KEY0005  05
115000940509PROMSG    *LIBL     KEY0006  06
115100940509PROMSG    *LIBL     KEY0007  07
115200940509PROMSG    *LIBL     KEY0008  08
115300940509PROMSG    *LIBL     KEY0009  09
115400940509PROMSG    *LIBL     KEY0010  10
115500940509PROMSG    *LIBL     KEY0011  11
115600940509PROMSG    *LIBL     KEY0012  12
115700940509PROMSG    *LIBL     KEY0013  13
115800940509PROMSG    *LIBL     KEY0014  14
115900940509PROMSG    *LIBL     KEY0015  15
116000940509PROMSG    *LIBL     KEY0016  16
116100940509PROMSG    *LIBL     KEY0017  17
116200940509PROMSG    *LIBL     KEY0018  18
116300940509PROMSG    *LIBL     KEY0019  19
116400940509PROMSG    *LIBL     KEY0020  20
116500940509PROMSG    *LIBL     KEY0021  21
116600940509PROMSG    *LIBL     KEY0022  22
116700940509PROMSG    *LIBL     KEY0023  23
116800940509PROMSG    *LIBL     KEY0024  24
116900940509**  ABILITAZIONE D1
1170009407200N  01
1171009407200N  02
1172009407201N  03
1173009407201N  04
1174009407201N  05
1175009407201N  06
1176009407201N  07
1177009407201N  08
1178009407200N  09
1179009407200N  10
1180009407200N  11
1181009407201N  12
1182009407200N  13
1183009407200N  14
1184009407200N  15
1185009407200N  16
1186009407200N  17
1187009407200N  18
1188009407200N  19
1189009407200N  20
1190009407200N  21
1191009407200N  22
1192009407200N  23
1193009407200N  24
