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
004100940614$001 FANGY4FD2  CF   E             WORKSTN
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
006000940614$003 D EXTDS         E DS                  EXTNAME(ANGY4FDS) INZ
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
018100940614$005 I              D1FIRME                     FIRMEY4F
018100940614$005 I              D1LIMITE                    LIMITEY4F
018100940614$005 I              D1FDG                       FDGY4F
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
020400971110$007 C                   MOVE      'Y4F'         XTBCOD
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
026900940621     C                   MOVEL     H1NMFL        W005              5
027000940621     C                   MOVE      W005          W003              3
027100940621     C                   MOVEL     W003          W006              6
027200940620     C                   MOVE      *ZERO         KEY              15
027300940616     C                   MOVE      D1COD         KEY
027400940615     C                   SELECT
028900940615     C*
029000940615     C*
029100940615     C* ricerca non abilitata in questa posizione
0292009406151O   C                   OTHER
029300940615     C                   SETON                                        98
029400940615     C*
0295009406151-   C                   ENDSL
029600940615     C*
029700940615     C                   MOVE      *HIVAL        $LASTV
029800940615     C                   ENDSR
029900940225     C/EJECT
030000940225     C************************************************************
030100940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
030200940225     C************************************************************
030300940225     C     GESPIE        BEGSR
030400940225     C*
030500940225     C                   SELECT
030600940225     C*
030700940506     C     $GEST         WHENEQ    'D1'
030800940506     C     $ULKD1        IFEQ      0
030900940506     C* riempimento opzioni
031000940506     C                   EXSR      F24D1
031100940506     C                   ELSE
031200940614     C                   WRITE     Z1
031300940506     C                   ENDIF
031400940225     C*
031500940510     C*
031600940225     C                   ENDSL
031700940225     C*
031800940225     C                   ENDSR
031900940131     C/EJECT
032000940131     C************************************************************
032100940510     C* GESTIONE VIDEO RECORD D1
032200940131     C************************************************************
032300940127     C     GESD1         BEGSR
032400940225     C* inizializzazione videata
032500940223     C     $INZD1        IFEQ      *ON
032600940127     C                   EXSR      INZD1
032700940223     C                   MOVE      *OFF          $INZD1
032800940117     C                   END
032900940225     C* emissione piede videata se proveniente da altra
033000940225     C* salvataggio valore formato di provenienza
033100940207     C     $LASTV        IFNE      'D1'
033200940614     C                   WRITE     T1
033300940225     C                   EXSR      GESPIE
033400940202     C                   MOVE      $LASTV        $LASTG
033500940202     C                   MOVE      'D1'          $LASTV
033600940202     C                   END
033700940131     C*
033800940203     C                   SELECT
033900940203     C* Annullamento
034000940315    >C     OPZ002        WHENEQ    '04'
034100940224     C* Visualizzazione
034200940315    >C     OPZ002        OREQ      '05'
034300940620     C                   SETON                                        0203
034400940620     C* Manutenzione
034500940620    >C     OPZ002        WHENEQ    '02'
034600940620     C                   SETON                                        02
034700940620     C                   SETOFF                                       03
034800940620     C*
034900940620     C                   OTHER
035000940620     C*
035100940620     C                   SETOFF                                       0203
035200940203     C                   ENDSL
035300940620     C* Immissione/Modifica/Copia
035400940620     C                   EXFMT     D1
035500940506     C* controllo abilitazione tasti funzione
035600940506     C*
035700940506     C                   EXSR      CTRKD1
035800940506     C*
035900940506     C     *IN99         IFEQ      *OFF
036000940131     C*
0361009402031    C                   SELECT
036200940510     c* ROLLUP
036300940131     C* F3=Fine
036400940506     C     $TASTO        WHENEQ    F03
036500940309     C                   EXSR      F03D1
036600940131     C* F4=Ricerca
036700940506     C     $TASTO        WHENEQ    F04
036800940127     C                   EXSR      SEARCH
036900940224     C* F5=Refresh
037000940506     C     $TASTO        WHENEQ    F05
037100940309     C                   EXSR      F05D1
037200940223     C* F8=Successivo
037300940506     C     $TASTO        WHENEQ    F08
037400940309     C                   EXSR      F08D1
037500940131     C* F12=Ritorno
037600940506     C     $TASTO        WHENEQ    F12
037700940309     C                   EXSR      F12D1
037800940315     C* F24=Altri tasti
037900940506     C     $TASTO        WHENEQ    F24
038000940315     C                   EXSR      F24D1
038100940131     C*
0382009402031O   C                   OTHER
038300940127     C                   EXSR      CTRD1
038400940616     C     *IN99         IFEQ      *OFF
038500940616     C                   SELECT
038600940616     C* F7=Traduzione
038700940616     C     $TASTO        WHENEQ    F07
038800940616     C                   EXSR      LINGUA
038900940616     C* F6
0390009406162    C     $TASTO        WHENEQ    F06
039100940224     C* eseguo aggiornamento
039200940315    >C                   EXSR      AGGANA
039300940224     C* eseguo operazioni del dopo-aggiornamento
0394009403183    C     *IN99         IFEQ      *OFF
039500940318     C                   EXSR      GESAGG
0396009403183-   C                   ENDIF
0397009406162-   C                   ENDSL
039800940616     C                   ENDIF
039900940203     C*
0400009402241-   C                   ENDSL
040100940224     C*
0402009405062-   C                   ENDIF
040300940117     C                   ENDSR
040400940506     C/EJECT
040500940506     C************************************************************
040600940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
040700940506     C************************************************************
040800940506     C     CTRKD1        BEGSR
040900940506     C                   SETOFF                                       99
041000940506     C                   MOVEA     $FC1          $FC
041100940506     C                   EXSR      CTRKEY
041200940506     C     *IN99         IFEQ      *ON
041300940506     C                   SETON                                        97
041400940506     C                   ENDIF
041500940506     C                   ENDSR
041600940117      ************************************************************
041700940207      * INIZIALIZZAZIONE VIDEATA DATI
041800940117      ************************************************************
041900940127     C     INZD1         BEGSR
042000940131     C*
042100940615     C                   CLEAR                   D1
042200010406X0113C*
042300010406  "  C* reperimento titolo
042400010406  "  C                   clear                   xatbds
042500010406  "  C                   eval      xtbkey      = *zero
042600010417X0113C* inserita la seguenta riga
042700010417$002 C                   move      'Y4F'         xtbkey
042800010417X0113C                   eval      xtbazi      = xscsoc
042900010406  "  C                   eval      xtblin      = xsclin
043000010406  "  C                   eval      xtbcod      = 'AAA'
043100010406  "  C                   eval      xtbric      = '1'
043200010406  "  C                   eval      xtbalc      = '0'
043300010406  "  C                   CALLB     'XATB'
043400010406  "  C                   PARM                    XATBDS
043500010406  "  C                   if        xtberr <> *off
043600010406  "  C                   movel     *all'?'       WrkTit
043700010406  "  C                   else
043800010406  "  C                   movel     xtbuni        WrkTit
043900010618  "  C                   move      '     '       WrkTit
044000010406  "  C                   endif
044100010406  "  C*
044200010406  "  C* centratura
044300010406  "  C                   eval      WrkTit=%trimr(Wrktit)
044400010406  "  C     ' '           CHECKR    Wrktit        I
044500010406  "  C                   call      'X71CENTRA'
044600010406  "  C                   parm                    Wrktit
044700010406  "  C                   parm                    I
044800010406  "  C                   parm                    lentit
044900010406  "  C                   movel     Wrktit        T1tit
045000010406X0113C*
045100990827X0079C* Aggiunta riga seguente
045200990827$028 C
045300941006     C                   MOVE      '0'           D1RIS
045400940616     C                   MOVE      KEY002        D1COD
045500940615     C                   MOVE      AZI002        XTBAZI
045600940615     C                   MOVE      LIN002        XTBLIN
045700940616     C                   MOVE      *ALL'0'       XTBKEY
045800940615     C                   MOVE      KEY002        XTBKEY
045900970509     C                   MOVE      UNI002        XTBUNT
046000940615$007 C                   MOVE      'Y4F'         XTBCOD
046100940225     C*
046200940207     C*
046300940224     C* valorizzo la chiave univoca per reperire i dati
046400940224     C* se non immissione
0465009403151   >C     OPZ002        IFNE      '01'
046600940315    >C*-------------------------------------------------------------
046700940315    >C* impostare i campi chiave ricevuti
046800940315    >C*-------------------------------------------------------------
046900940318     C*
047000940614     C*
047100940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0472009406142   >C     OPZ002        IFEQ      '02'
047300940315    >C     OPZ002        OREQ      '04'
047400940614     C                   MOVE      '1'           XTBALC
047500940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0476009406142E   C                   ELSE
047700940614     C                   MOVE      '0'           XTBALC
0478009406142-   C                   ENDIF
047900940614     C*
048000940616     C                   MOVE      '1'           XTBRIC
048100970214     C                   CALLB     'XATB'
048200940614     C                   PARM                    XATBDS
048300940318     C*
048400940614     C     XTBERR        IFEQ      '0'
048500940614     C                   MOVEL     XTBUNI        EXTDS
048600970512     c                   move      xtbunt        d1uniTA           8
048700941108     C* Giro la data se non blank
048800970214     c                   move      xtbdsc        xtbdscA          10
048900970214     C     XTBDSCa       IFNE      DATA
049000970214     C                   CALLB     'XDT4000'
049100970214     C                   PARM      XTBDSCA       XDTAMG           10
049200940621     C                   PARM      *ZEROS        XDTGMA            6 0
049300940621     C                   PARM      2             XDTSTA            1 0
049400940621     C                   MOVE      XDTGMA        D1DSC
049500940712     C                   MOVE      XTBRIS        D1RIS
049600941108     C                   ELSE
049700941108     C                   MOVE      *ZEROS        D1DSC
049800941108     C                   ENDIF
049900941108     C                   ENDIF
050000940614     C*
050100940224     C*
050200940224     C* se il rcd da gestire non viene trovato
050300940318     C* oppure è allocato da altro pgm
050400940224     C* ritorno al pgm guida avvertendolo dell'errore
0505009406142    C     XTBERR        IFNE      '0'
050600940224     C                   MOVE      *ON           $FINE
050700940315    >C                   MOVE      '1'           ERR002
050800940224     C*
050900940224     C* altrimenti
0510009402242E   C                   ELSE
051100940224     C* riempio la videata
051200940224     C                   EXSR      RIED1
051300940224     C* richiamo routine dei ctrl per decodificarla
051400940614    >C                   EXSR      CTRD1
051500960228     C                   MOVEL     *IN99         SAV99             1
051600960228     C* Se la gestione è più videate
051700960305    >C                   EXSR      CTRD2
051800960305    >C     *IN99         IFEQ      *ON
051900960305    >C                   MOVEL     *IN99         SAV99             1
052000960305    >C                   ENDIF
052100940224     C                   MOVE      *ALL'0'       IN5098
052200940224     C                   MOVEA     IN5098        *IN(50)
052300940224     C* definizione indicatori su video
052400940224     C                   SETOFF                                       02
0525009402243    C                   SELECT
052600940315    >C     OPZ002        WHENEQ    '02'
052700940224     C                   SETON                                        02
052800940315    >C     OPZ002        WHENEQ    '05'
052900940506     C* disabilita tasti
053000940720     C                   MOVE      '0'           $FC(6)
053100940720     C                   MOVE      '0'           $FC(5)
053200940720     C                   MOVE      '0'           $FC(4)
0533009402243-   C                   ENDSL
053400940224     C*
0535009402242-   C                   ENDIF
053600940223     C*
0537009402241-   C                   ENDIF
053800940224     C*
053900940117     C                   ENDSR
054000940207     C/EJECT
054100940207     C************************************************************
054200940510     C* RIEMPIMENTO VIDEATA  D1
054300940207     C************************************************************
054400940207     C     RIED1         BEGSR
054500940207     C*
054600990827$025 C
054700940207     C*
054800940207     C                   ENDSR
054900940309     C/EJECT
055000940309     C************************************************************
055100940309     C* GESTIONE F03 VIDEO D1
055200940309     C************************************************************
055300940309     C     F03D1         BEGSR
055400940309     C*
055500940309     C                   MOVE      *ON           $FINE
055600940315    >C                   MOVE      '1'           RET002
055700940325     C* fine programma
055800940325     C                   EXSR      ENDPGM
055900940325     C*
056000940325     C                   ENDSR
056100940309     C/EJECT
056200940309     C************************************************************
056300940309     C* GESTIONE F05 VIDEO D1
056400940309     C************************************************************
056500940309     C     F05D1         BEGSR
056600940309     C*
056700940309     C                   MOVE      *ON           $INZD1
056800940309     C*
056900940309     C                   ENDSR
057000940309     C/EJECT
057100940309     C************************************************************
057200940309     C* GESTIONE F08 VIDEO D1
057300940309     C************************************************************
057400940309     C     F08D1         BEGSR
057500940309     C*
057600940309     C                   MOVE      *ON           $FINE
057700940315    >C                   MOVE      '0'           RET002
057800940309     C*
057900940309     C                   ENDSR
058000940309     C/EJECT
058100940309     C************************************************************
058200940309     C* GESTIONE F12 VIDEO D1
058300940309     C************************************************************
058400940309     C     F12D1         BEGSR
058500940309     C*
058600940309     C                   MOVE      *ON           $FINE
058700940315    >C                   MOVE      '2'           RET002
058800940309     C*
058900940309     C                   ENDSR
059000940309     C/EJECT
059100940309     C************************************************************
059200940309     C* GESTIONE F24 VIDEO D1
059300940309     C************************************************************
059400940309     C     F24D1         BEGSR
059500940309     C*
059600940506     C*
059700940506     C                   MOVEA     $FX1          $FX
059800940506     C                   MOVEA     $FC1          $FC
059900940506     C                   MOVEA     $FR1          $FR
060000940506     C                   Z-ADD     $ULKD1        $ULK
060100940506     C                   EXSR      RIEKEY
060200941017     C                   MOVEA     $FC           $FC1
060300940506     C                   Z-ADD     $ULK          $ULKD1            3 0
060400940506     C                   MOVEL     $KEY1         Z1KE1
060500940506     C                   MOVEL     $KEY2         Z1KE2
060600940614     C                   WRITE     Z1
060700940506     C*
060800940309     C                   ENDSR
060900940207     C/EJECT
061000940131     C************************************************************
061100940207     C* CONTROLLO VIDEATA
061200940131     C************************************************************
061300940127     C     CTRD1         BEGSR
061400940131     C*
061500940127     C                   SETOFF                                       99
061600940127     C* non imposto pos. del cursore
061700940127     C                   Z-ADD     0             H1RIGA
061800940127     C                   Z-ADD     0             H1COLO
061900940621     C*
062000940621     C* CTRL DATA
062100940621     C*
062200941109     C                   MOVE      DATA          WRKDSC           10
062300940621     C     D1DSC         IFNE      0
062400970214     C                   CALLB     'XDT4000'
062500941109     C                   PARM                    XDTAMG           10
062600940621     C                   PARM      D1DSC         XDTGMA            6 0
062700940621     C                   PARM      3             XDTSTA            1 0
062800940621     C     XDTSTA        IFNE      0
062900940621     C                   SETON                                        5599
063000940621     C                   ELSE
063100941108     C                   MOVE      XDTAMG        WRKDSC
063200940621     C                   ENDIF
063300940621     C                   ENDIF
063400940223     C*
063500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
063600940223     C*
0637009403151   >C     OPZ002        IFNE      '04'
063800971110     C*
063900971110     C* controllo codice vuoto
064000971110$017 C     D1COD         IFEQ      *ALL'0'
064100971110     C                   SETON                                        5099
064200971110     C                   ENDIF
064300971110     C*
064800940207     C*
064900940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
065000940315    >C     OPZ002        IFEQ      '01'
065100940315    >C     OPZ002        OREQ      '03'
065200940315    >C*-------------------------------------------------------------
065300940315    >C* impostare i campi chiave e verificare l'esistenza del record
065400940315    >C*-------------------------------------------------------------
065500971110$007 C                   MOVE      'Y4F'         XTBCOD
065600940614     C                   MOVE      *ALL'0'       XTBKEY
065700940620    >C                   MOVE      D1COD         XTBKEY
065800970512    >C                   MOVE      D1UNITA       XTBUNT
065900940614     C                   MOVE      '0'           XTBALC
066000940614     C                   MOVE      '1'           XTBRIC
066100970214    >C                   CALLB     'XATB'
066200940614     C                   PARM                    XATBDS
066300940614     C     XTBERR        IFEQ      '0'
066400940223     C                   SETON                                        5999
066500940223     C                   ENDIF
066600940620     C* Controllo se compreso nei range indice
066700940620     C     XTBERR        IFEQ      '3'
066800940620    >C                   SETON                                        5899
066900940620     C                   END
067000940620     C*
067100940620     C* REPERISCO AUTORIZZAZIONI UTENTE
067200940620     C                   CLEAR                   XCHATB
067300941118     C                   MOVE      XSCSOC        XCTSOC
067400941118     C                   MOVE      XSCDUN        XCTUNI
067500940620     C                   MOVEL     KNMUS         XCTPRF
067600940620     C                   MOVE      XTBCOD        XCTCOD
067700940620     C                   CALL      'XCHKATB'
067800940620     C                   PARM                    XCHATB
067900941111     C                   MOVEL     XCTDDS        AUTDS
068000941027     C     XCTRTN        IFEQ      1
068100941027     C                   MOVEL     'N'           MANGRU
068200941027     C                   ENDIF
068300940620     C* Non ammessa immissione di gruppo
068400940620     C     XTBGRU        IFEQ      'G'
068500940620     C     MANGRU        ANDEQ     'N'
068600940620     C                   SETON                                        5799
068700940620     C                   END
068800940620     C* Non ammessa immissione di azienda
068900940620     C     XTBGRU        IFEQ      'A'
069000940620     C     MANAZI        ANDEQ     'N'
069100940620     C                   SETON                                        5699
069200940620     C                   END
069300940712     C* codice riservato non accessibile
069400941006     C     XTBRIS        IFGT      ABIRIS
069500941005    >C     OPZ002        ANDNE     '01'
069600941005    >C     OPZ002        ANDNE     '03'
069700940712     C                   SETON                                        5499
069800940712     C                   END
069900940224     C                   ENDIF
070000940224     C*
0701009402241-   C                   END
070200940131     C*
070300940302     C     *IN99         IFEQ      *ON
070400940510     C     $GEST         ANDEQ     'D1'
070500960228     C     OPZ002        ANDNE     '07'
070600940117     C                   MOVEA     *IN           WIN
070700940128     C                   MOVE      *ALL'0'       IN5098           49
070800940128     C                   MOVEA     IN5098        *IN(50)
070900940614     C                   WRITE     D1
071000940117     C                   MOVEA     WIN           *IN
071100940302     C                   ENDIF
071200940127     C*
071300940117     C                   ENDSR
071400960305     C/EJECT
071500960305     C************************************************************
071600960305     C* CONTROLLO VIDEATA D2
071700960305     C************************************************************
071800960305     C     CTRD2         BEGSR
071900960305     C*
072000960305     C                   ENDSR
072100940207     C/EJECT
072200940224     C************************************************************
072300940315    >C* AGGIORNAMENTO ANAGRAFICA
072400940224     C************************************************************
072500940315    >C     AGGANA        BEGSR
072600971110     C*
072700971110$007 C                   MOVE      'Y4F'         XTBCOD
072800940614     C                   MOVE      *ALL'0'       XTBKEY
072900940614     C                   MOVE      D1COD         XTBKEY
073000970512     C                   MOVE      D1UNITA       XTBUNT
073100940207     C*
073200940207     C                   SELECT
073300940315    >C     OPZ002        WHENEQ    '01'
073400940315    >C     OPZ002        OREQ      '03'
073500940315    >C                   EXSR      RIEANA
073600950609     C                   MOVE      XSCCUT        XTBUTE
073700940614     C                   MOVE      '5'           XTBRIC
073800970214     C                   CALLB     'XATB'
073900940614    >C                   PARM                    XATBDS
074000940318     C* se rcd non scrivibile attivo errore
074100940615     C     XTBERR        IFEQ      '1'
074200940318    >C                   SETON                                        5999
074300940318     C                   END
074400940315    >C     OPZ002        WHENEQ    '02'
074500940621     C                   MOVE      '1'           XTBRIC
074600940621     C                   MOVE      '1'           XTBALC
074700970214     C                   CALLB     'XATB'
074800940621     C                   PARM                    XATBDS
074900940622    >C                   EXSR      RIEANA
075000950609     C                   MOVE      XSCCUT        XTBUTE
075100940614     C                   MOVE      '4'           XTBRIC
075200970214     C                   CALLB     'XATB'
075300940614    >C                   PARM                    XATBDS
075400940315    >C     OPZ002        WHENEQ    '04'
075500940621     C                   MOVE      '1'           XTBRIC
075600940621     C                   MOVE      '1'           XTBALC
075700970214     C                   CALLB     'XATB'
075800940621     C                   PARM                    XATBDS
075900940614     C                   MOVE      '6'           XTBRIC
076000970214     C                   CALLB     'XATB'
076100940614    >C                   PARM                    XATBDS
076200940621     C* annullo traduzioni
076300940621     C     XTBERR        IFEQ      '0'
076400940621     C                   EXSR      ANNTBT
076500940621     C                   END
076600940621     C*
076700940207     C                   ENDSL
076800940203     C*
076900940203     C                   ENDSR
077000940224     C/EJECT
077100940224     C************************************************************
077200940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
077300940224     C************************************************************
077400940616     C     RIEANA        BEGSR
077500940616     C                   CLEAR                   XTBUNI
077600940621     C                   MOVE      WRKDSC        XTBDSC
077700940712     C                   MOVE      D1RIS         XTBRIS
077800981209$026 C
077900940616     C                   MOVEL     EXTDS         XTBUNI
078000940207     C                   ENDSR
078100940224     C/EJECT
078200940224     C************************************************************
078300940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
078400940224     C************************************************************
078500940224     C     GESAGG        BEGSR
078600940224     C*
078700940224     C* segnalo al pgm chiamante l'avvenuta conferma
078800940315    >C                   MOVE      *ON           OPR002
078900940224     C* segnalo al pgm chiamante l'aver premuto l'F6
079000940315    >C                   MOVE      '0'           RET002
079100940224     C*
0792009402241    C                   SELECT
079300940224     C* nel caso di immissione, ripristino la videata iniziale
079400940315    >C     OPZ002        WHENEQ    '01'
079500940224     C                   MOVE      *ON           $INZD1
079600940224     C* altrimenti ritorno al pgm chiamante
079700940224     C                   OTHER
079800940224     C                   MOVE      *ON           $FINE
0799009402241-   C                   ENDSL
080000940224     C*
080100940224     C                   ENDSR
080200940621     C/EJECT
080300940621     C************************************************************
080400940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
080500940621     C************************************************************
080600940621    >C     ANNTBT        BEGSR
080700940621     C                   MOVE      XTBAZI        TBTAZI
080800940621     C                   MOVE      XTBCOD        TBTCOD
080900940621     C                   MOVE      XTBKEY        TBTKEY
081000940621     C     KTBT          SETLL     ANTBT01L
081100940621     C                   DO        *HIVAL
081200940621     C     KTBT          DELETE    ANTBT01L                           21
081300940621     C  N21              END
081400940621     C*
081500940621     C                   ENDSR
081600940207     C/EJECT
081700940131     C************************************************************
081800940131     C* OPERAZIONI INIZIALI
081900940131     C************************************************************
082000940131     C     *INZSR        BEGSR
082100940131     C*
082200940127     C* Reperimento parametri
082300940127     C*
082400940117     C     *ENTRY        PLIST
082500940117     C                   PARM                    KPJBA
082600940712     C*
082700940712     C* Reperimento Codice utente
082800940712     C*
082900940223     C*
083000940506     C*
083100940506     C* Reperimento tasti di funzione
083200940506     C*
083300940506     C                   EXSR      REPKEY
083400940621     C*
083500940621     C* KLIST
083600940621     C*
083700940621     C     KTBT          KLIST
083800940621     C                   KFLD                    TBTAZI
083900940621     C                   KFLD                    TBTCOD
084000940621     C                   KFLD                    TBTKEY
084100010406X0113C*
084200010406  "  C* reperisco le dimensioni della variabile del titolo
084300010406X0113C                   EVAL      LENTIT = %Size(T1Tit)
084400940506     C*
084500940223     C                   ENDSR
084600941103     C/EJECT
084700941103      ************************************************************
084800941103      * Reperimento dati società
084900941103      ************************************************************
085000941103     C     REPSOC        BEGSR
085100941103      *
085200970214     C                   CALLB     'XSOC'
085300941103     C                   PARM                    TIPXSC            6
085400941103     C                   PARM                    SOCXSC            3
085500941103     C                   PARM                    CDSXSC            9 0
085600941103     C                   PARM                    MODXSC            3
085700941103     C                   PARM      *BLANKS       RTNXSC            1
085800941103     C                   PARM                    XSOCDS
085900941103     C                   PARM                    KPJBA
086000941103      *
086100941103     C                   ENDSR
086200940223     C/EJECT
086300940506     C************************************************************
086400940506     C* REPERIMENTO TASTI DI FUNZIONE
086500940506     C************************************************************
086600940506     C     REPKEY        BEGSR
086700940506     C                   MOVEA     $FM1          $FM
086800940506     C                   EXSR      REPKKK
086900940506     C                   MOVEA     $FX           $FX1
087000940614     C                   ENDSR
087100940506     C/EJECT
087200940506     C************************************************************
087300940506     C* REPERIMENTO TASTI DI FUNZIONE
087400940506     C************************************************************
087500940506     C     REPKKK        BEGSR
087600940506     C*
087700940506     C                   CLEAR                   $FX
087800940506     C     1             DO        24            X
087900940506     C*
088000940506     C     $FM(X)        IFNE      *BLANK
088100940506     C*
088200940506     C                   MOVEL     $FM(X)        IDMSG
088300970214     C                   CALLB     'XRTVM'
088400940506     C                   PARM                    IDMSG            27
088500940506     C                   PARM                    TXTMSG          100
088600940506     C                   PARM                    ERRMSG            1
088700940506     C*
088800940506     C     ERRMSG        IFNE      *ON
088900940506     C                   MOVEL     TXTMSG        $FX(X)
089000940506     C                   ELSE
089100940506     C                   MOVEL     *ALL' '       $FX(X)
089200940506     C                   END
089300940506     C*
089400940506     C                   END
089500940506     C*
089600940506     C                   END
089700940506     C*
089800940506     C                   ENDSR
089900940509     C/EJECT
090000940223     C************************************************************
090100940223     C* INIZIALIZZAZIONE VARIABILI
090200940223     C************************************************************
090300940223     C     INZVAR        BEGSR
090400951124     C*
090500951124     C* Chiamata XSOC
090600951124     C*
090700951124     C                   MOVEL     'SOC001'      TIPXSC
090800951124     C                   EXSR      REPSOC
090900951124     C     RTNXSC        IFNE      '1'
091000951124     C                   MOVEL     XSOCDS        SOC001
091100951124     C                   ELSE
091200951124     C                   MOVE      *ON           $FINE
091300951124     C* fine programma
091400951124     C                   EXSR      ENDPGM
091500951124     C                   ENDIF
091600940224     C*
091700940224     C* Dati da PGM guida
091800940224     C*
091900940315    >C                   MOVEL     KPJBU         D002DS
092000940128     C*
092100940223     C* Variabili per gestione videate
092200940128     C*
092300940223     C                   MOVE      'D1'          $GEST
092400940223     C                   MOVE      *OFF          $FINE
092500940223     C                   MOVE      *ON           $INZD1
092600940224     C                   MOVE      *BLANK        $LASTG
092700940224     C                   MOVE      *BLANK        $LASTV
092800940506     C                   Z-ADD     0             $ULKD1            3 0
092900940127     C*
093000940223     C* Variabili appoggio
093100940127     C*
093200940224     C                   Z-ADD     0             CURR
093300940224     C                   Z-ADD     0             CURC
093400940207     C*
093500940207     C* Valorizzazione campi univoci testate
093600940207     C*
093700940614     C                   CLEAR                   T1
093800940614     C                   MOVEL     KNSIF         NOMSIF
093900941109     C                   MOVEL     XSCDSI        NOMDIT
094000941027     C                   MOVEL     DSPGM         NOMPGM
094100940224     C                   Z-ADD     1             X
094200940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
094300940224     C     *IN21         IFEQ      *ON
094400940224     C                   MOVEL     $ID(X)        IDMSG
094500970214     C                   CALLB     'XRTVM'
094600940224     C                   PARM                    IDMSG            27
094700940224     C                   PARM                    TXTMSG          100
094800940224     C                   PARM                    ERRMSG            1
094900940224     C                   PARM      *ON           CTRMSG            1
095000940224     C                   PARM      30            LENMSG            3 0
095100940224     C     ERRMSG        IFNE      *ON
095200940224     C                   MOVEL     TXTMSG        T1OPE
095300940224     C                   ELSE
095400940224     C                   MOVEL     *ALL'?'       T1OPE
095500940224     C                   ENDIF
095600940224     C                   END
095700940224     C*
095800940224     C* Inizializzazione/Decodifica prima videata
095900940224     C*
096000940224     C                   EXSR      INZD1
096100940224     C                   MOVE      *OFF          $INZD1
096200960228     C* Se opzione = '07' richiesti solo controlli
096300960228     C     OPZ002        IFEQ      '07'
096400960228     C                   MOVEL     *IN99         ERR002
096500960228     C                   MOVEL     '0'           RET002
096600960228     C                   MOVEL     *ON           $FINE
096700960228     C                   ENDIF
096800940127     C*
096900940117     C                   ENDSR
097000940128     C************************************************************
097100940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
097200940223     C************************************************************
097300940223     C     DEFCAM        BEGSR
097400940223     C*
097500940223     C* klist
097600940207     C*
097700940223     C*
097800940223     C* Variabili per gestione videate
097900940223     C*
098000940223     C                   MOVE      *BLANK        $GEST             2
098100940223     C                   MOVE      *BLANK        $FINE             1
098200940223     C                   MOVE      *BLANK        $INZD1            1
098300940223     C                   MOVE      *BLANK        $LASTG            2
098400940223     C                   MOVE      *BLANK        $LASTV            2
098500940223     C*
098600940223     C* Variabili appoggio
098700940223     C*
098800940223     C                   Z-ADD     0             CURR              2 0
098900940223     C                   Z-ADD     0             CURC              2 0
099000940223     C                   MOVE      *ZEROS        WIN              99
099100940224     C                   MOVE      *BLANK        WTBERR            1
099200940223     C*
099300940223     C* Indici
099400940223     C*
099500940223     C                   Z-ADD     0             X                 3 0
099600940223     C                   Z-ADD     0             Y                 3 0
099700940127     C*
099800940117     C                   ENDSR
099900940128     C************************************************************
100000940509     C/EJECT
100100940509     C************************************************************
100200940509     C* CONTROLLO ABILITAZIONE TASTI
100300940509     C************************************************************
100400940509     C     CTRKEY        BEGSR
100500940509     C                   SELECT
100600940509     C     $TASTO        WHENEQ    F01
100700940720     C     $FC(1)        IFEQ      '0'
100800940509     C                   SETON                                        99
100900940509     C                   ENDIF
101000940509     C     $TASTO        WHENEQ    F02
101100940720     C     $FC(2)        IFEQ      '0'
101200940509     C                   SETON                                        99
101300940509     C                   ENDIF
101400940509     C     $TASTO        WHENEQ    F03
101500940720     C     $FC(3)        IFEQ      '0'
101600940509     C                   SETON                                        99
101700940509     C                   ENDIF
101800940509     C     $TASTO        WHENEQ    F04
101900940720     C     $FC(4)        IFEQ      '0'
102000940509     C                   SETON                                        99
102100940509     C                   ENDIF
102200940509     C     $TASTO        WHENEQ    F05
102300940720     C     $FC(5)        IFEQ      '0'
102400940509     C                   SETON                                        99
102500940509     C                   ENDIF
102600940509     C     $TASTO        WHENEQ    F06
102700940720     C     $FC(6)        IFEQ      '0'
102800940509     C                   SETON                                        99
102900940509     C                   ENDIF
103000940509     C     $TASTO        WHENEQ    F07
103100940720     C     $FC(7)        IFEQ      '0'
103200940509     C                   SETON                                        99
103300940509     C                   ENDIF
103400940509     C     $TASTO        WHENEQ    F08
103500940720     C     $FC(8)        IFEQ      '0'
103600940509     C                   SETON                                        99
103700940509     C                   ENDIF
103800940509     C     $TASTO        WHENEQ    F09
103900940720     C     $FC(09)       IFEQ      '0'
104000940509     C                   SETON                                        99
104100940509     C                   ENDIF
104200940509     C     $TASTO        WHENEQ    F10
104300940720     C     $FC(10)       IFEQ      '0'
104400940509     C                   SETON                                        99
104500940509     C                   ENDIF
104600940509     C     $TASTO        WHENEQ    F11
104700940720     C     $FC(11)       IFEQ      '0'
104800940509     C                   SETON                                        99
104900940509     C                   ENDIF
105000940509     C     $TASTO        WHENEQ    F12
105100940720     C     $FC(12)       IFEQ      '0'
105200940509     C                   SETON                                        99
105300940509     C                   ENDIF
105400940509     C     $TASTO        WHENEQ    F13
105500940720     C     $FC(13)       IFEQ      '0'
105600940509     C                   SETON                                        99
105700940509     C                   ENDIF
105800940509     C     $TASTO        WHENEQ    F14
105900940720     C     $FC(14)       IFEQ      '0'
106000940509     C                   SETON                                        99
106100940509     C                   ENDIF
106200940509     C     $TASTO        WHENEQ    F15
106300940720     C     $FC(15)       IFEQ      '0'
106400940509     C                   SETON                                        99
106500940509     C                   ENDIF
106600940509     C     $TASTO        WHENEQ    F16
106700940720     C     $FC(16)       IFEQ      '0'
106800940509     C                   SETON                                        99
106900940509     C                   ENDIF
107000940509     C     $TASTO        WHENEQ    F17
107100940720     C     $FC(17)       IFEQ      '0'
107200940509     C                   SETON                                        99
107300940509     C                   ENDIF
107400940509     C     $TASTO        WHENEQ    F18
107500940720     C     $FC(18)       IFEQ      '0'
107600940509     C                   SETON                                        99
107700940509     C                   ENDIF
107800940509     C     $TASTO        WHENEQ    F19
107900940720     C     $FC(19)       IFEQ      '0'
108000940509     C                   SETON                                        99
108100940509     C                   ENDIF
108200940509     C     $TASTO        WHENEQ    F20
108300940720     C     $FC(20)       IFEQ      '0'
108400940509     C                   SETON                                        99
108500940509     C                   ENDIF
108600940509     C     $TASTO        WHENEQ    F21
108700940720     C     $FC(21)       IFEQ      '0'
108800940509     C                   SETON                                        99
108900940509     C                   ENDIF
109000940509     C     $TASTO        WHENEQ    F22
109100940720     C     $FC(22)       IFEQ      '0'
109200940509     C                   SETON                                        99
109300940509     C                   ENDIF
109400940509     C     $TASTO        WHENEQ    F23
109500940720     C     $FC(23)       IFEQ      '0'
109600940509     C                   SETON                                        99
109700940509     C                   ENDIF
109800940509     C     $TASTO        WHENEQ    F24
109900940720     C     $FC(24)       IFEQ      '0'
110000940509     C                   SETON                                        99
110100940509     C                   ENDIF
110200940509     C                   ENDSL
110300940509     C                   ENDSR
110400940509     C/EJECT
110500940509     C************************************************************
110600940509     C* RIEMPIMENTO TASTI FUNZIONE
110700940509     C************************************************************
110800940509     C     RIEKEY        BEGSR
110900940509     C*
111000940509     C* Input :
111100940509     C* - $FX = schiera con tutti i testi disponibili
111200940509     C* - $FC = schiere con i flag di validità associati ai testi
111300940509     C* - $FR = schiere con i flag di REVERSE IMAGE
111400940509     C* - $ULK = ultimo testo caricato
111500940509     C* Output :
111600940509     C* - $ULK
111700940509     c* - $KEY1= 1° riga di tasti funzione
111800940509     c* - $KEY2= 2° riga di tasti funzione
111900940509     C*
112000970214     C                   CALLB     'XKEYMSG'
112100940509     C* input
112200940509     C                   PARM                    $FX
112300940509     C                   PARM                    $FC
112400940509     C                   PARM                    $FR
112500940509     C                   PARM                    $ULK              3 0
112600940509     C* output
112700940509     C                   PARM                    $KEY1            79
112800940509     C                   PARM                    $KEY2            79
112900940509     C*
113000940509     C                   ENDSR
113100940615**
11320094022401PROMSG    *LIBL     COS0001    immissione
11330094022402PROMSG    *LIBL     COS0002    modifica
11340094022403PROMSG    *LIBL     COS0003    copia
11350094022404PROMSG    *LIBL     COS0004    annullamento
11360094022405PROMSG    *LIBL     COS0005    visualizzazione
113700940509** TASTI DI FUNZIONE  D1
113800940509PROMSG    *LIBL     KEY0001  01
113900940509PROMSG    *LIBL     KEY0002  02
114000940509PROMSG    *LIBL     KEY0003  03
114100940509PROMSG    *LIBL     KEY0004  04
114200940509PROMSG    *LIBL     KEY0005  05
114300940509PROMSG    *LIBL     KEY0006  06
114400940509PROMSG    *LIBL     KEY0007  07
114500940509PROMSG    *LIBL     KEY0008  08
114600940509PROMSG    *LIBL     KEY0009  09
114700940509PROMSG    *LIBL     KEY0010  10
114800940509PROMSG    *LIBL     KEY0011  11
114900940509PROMSG    *LIBL     KEY0012  12
115000940509PROMSG    *LIBL     KEY0013  13
115100940509PROMSG    *LIBL     KEY0014  14
115200940509PROMSG    *LIBL     KEY0015  15
115300940509PROMSG    *LIBL     KEY0016  16
115400940509PROMSG    *LIBL     KEY0017  17
115500940509PROMSG    *LIBL     KEY0018  18
115600940509PROMSG    *LIBL     KEY0019  19
115700940509PROMSG    *LIBL     KEY0020  20
115800940509PROMSG    *LIBL     KEY0021  21
115900940509PROMSG    *LIBL     KEY0022  22
116000940509PROMSG    *LIBL     KEY0023  23
116100940509PROMSG    *LIBL     KEY0024  24
116200940509**  ABILITAZIONE D1
1163009407200N  01
1164009407200N  02
1165009407201N  03
1166009407201N  04
1167009407201N  05
1168009407201N  06
1169009407201N  07
1170009407201N  08
1171009407200N  09
1172009407200N  10
1173009407200N  11
1174009407201N  12
1175009407200N  13
1176009407200N  14
1177009407200N  15
1178009407200N  16
1179009407200N  17
1180009407200N  18
1181009407200N  19
1182009407200N  20
1183009407200N  21
1184009407200N  22
1185009407200N  23
1186009407200N  24
