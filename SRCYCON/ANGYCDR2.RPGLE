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
004100980921$001 FangYCDD2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300980921     Fanpdc01L  UF   E           K DISK
004400940621     FANTBT01L  UF   E           K DISK
004500940128     D*----------------------------------------------------*
004600940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004700940224     D $ID             S             27    DIM(5) ALT($OZ)
004800940506     D* Schiere per la gestione dei tasti di funzione
004900940506     D $FM             S             27    DIM(24)
005000940506     D $FX             S             50    DIM(24)
005100940506     D $FC             S              1    DIM(24)
005200940506     D $FR             S              1    DIM(24)
005300940506     D* Video D1
005400940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005500940506     D $FX1            S             50    DIM(24) ALT($FM1)
005600940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005700940506     D $FR1            S              1    DIM(24) ALT($FC1)
005800940615     D* campi in lingua
005900940622$010 D*LIN             S             06    DIM(0011) CTDATA PERRCD(001)
006000940622$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006100980921$003 D EXTDS         E DS                  EXTNAME(angYCDDS) INZ
006200940127     D* Passaggio Parametri
006300940127     D KPJBA         E DS
006400941103     D*-------------
006500941103      * DS Società
006600941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
006700960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
006800960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
006900941103     D*-------------
007000941103      * DS Interna per dati utente
007100941103     D XSOCDS          DS          1000
007200941103     D*-------------
007300940616     D XATBDS        E DS                  INZ
007400940318     D*-------------
007500940318     D* DS Esterna gestione messaggi per COMIT
007600940616     D XMSGDS        E DS                  INZ
007700940318     D  STS          E                     EXTFLD(MSGSTS)
007800940318     D                                     DIM(333)
007900940318     D  JBA          E                     EXTFLD(MSGJBA)
008000940318     D                                     DIM(502)
008100940203     D*-------------
008200940620     D* DS Esterna autorizzazioni tabelle
008300940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
008400940620     D*
008500940620     D* DS  restituzione autorizzazioni
008600940620     D* IND=indice
008700940620     D* GRU=man. di gruppo
008800940620     D* AZI=man. azienda
008900940712     D* ABI=abilit. ai riservati
009000941111     D AUTDS           DS
009100940620     D  MANIND                 1      1
009200940620     D  MANGRU                 2      2
009300940620     D  MANAZI                 3      3
009400940712     D  ABIRIS                 4      4
009500940620     D*-------------
009600940207     D* opzione richiesta
009700940614    >D D002DS          DS
009800940614     D  OPZ002                 1      2
009900940614     D* 0 = ritorno normale
010000940614     D* 1 = F3
010100940614     D* 2 = F12
010200940614     D  RET002                 3      3
010300940614     D* operazione eseguite dal pgm chiamato :
010400940614     D* 0 = nessuna operazione
010500940614     D* 1 = eseguito aggiornamento
010600940614     D  OPR002                 4      4
010700940614     D* funzione non eseguibile per errore :
010800940614     D* 0 = nessun errore
010900940614     D* 1 = funzione richiamata chiusa in errore
011000940614     D* Aggiungere eventuali altri codici di errore
011100940614     D  ERR002                 5      5
011200940614     D* Azienda
011300940614     D  AZI002                 6      8
011400940614     D* Lingua
011500940614     D  LIN002                 9      9
011600940614     D* Codice scelto
011700940614     D  KEY002                10     24
011800940620     D* Autorizzazione
011900940620     D  AUT002                25     25
012000970509     D* Unita
012100970509     D  UNI002                26     33
012200940614     D*-------------
012300940201     D*-------------
012400940201     D DSFMT           DS
012500940506     D  $TASTO               369    369
012600940201     D  NRG                  370    370
012700940201     D  NCL                  371    371
012800940201     D*-------------
012900940201     D* posizione cursore
013000940201     D CURSOR          DS
013100940223     D  RIRI                   1      2B 0 INZ
013200940201     D  R$$                    2      2
013300940223     D  COCO                   3      4B 0 INZ
013400940201     D  C$$                    4      4
013500940207     D*-------------
013600940207     D* Reperimento nome PGM
013700940207     D STATUS         SDS           333
013800940207     D  DSPGM            *PROC
013900940225     D*-------------
014000940225     D* COSTANTI
014100940225     D*-------------
014200940506     D*
014300940506     D* Tasti di funzione
014400940506     D F01             C                   CONST(X'31')
014500940506     D F02             C                   CONST(X'32')
014600940506     D F03             C                   CONST(X'33')
014700940506     D F04             C                   CONST(X'34')
014800940506     D F05             C                   CONST(X'35')
014900940506     D F06             C                   CONST(X'36')
015000940506     D F07             C                   CONST(X'37')
015100940506     D F08             C                   CONST(X'38')
015200940506     D F09             C                   CONST(X'39')
015300940506     D F10             C                   CONST(X'3A')
015400940506     D F11             C                   CONST(X'3B')
015500940506     D F12             C                   CONST(X'3C')
015600940506     D F13             C                   CONST(X'B1')
015700940506     D F14             C                   CONST(X'B2')
015800940506     D F15             C                   CONST(X'B3')
015900940506     D F16             C                   CONST(X'B4')
016000940506     D F17             C                   CONST(X'B5')
016100940506     D F18             C                   CONST(X'B6')
016200940506     D F19             C                   CONST(X'B7')
016300940506     D F20             C                   CONST(X'B8')
016400940506     D F21             C                   CONST(X'B9')
016500940506     D F22             C                   CONST(X'BA')
016600940506     D F23             C                   CONST(X'BB')
016700940506     D F24             C                   CONST(X'BC')
016800940506     D ENTER           C                   CONST(X'F1')
016900940506     D ROLDWN          C                   CONST(X'F4')
017000940506     D ROLLUP          C                   CONST(X'F5')
017100940506     D*-------------
017200940506     D*
017300941108     D DATA            C                   CONST('0001-01-01')
017400940128     I*----------------------------------------------------*
017500940615     ID1
017600980921$005 I              D1KC1                       §CDKC1
017700980921$005 I              D1KS1                       §CDKS1
017800980921$005 I              D1KC2                       §CDKC2
017900980921$005 I              D1KS2                       §CDKS2
018000980921$005 I              D1KC3                       §CDKC3
018100980921$005 I              D1KS3                       §CDKS3
018200980921$005 I              D1KC4                       §CDKC4
018300980921$005 I              D1KS4                       §CDKS4
018400980921$005 I              D1KS5                       §CDKS5
018500980921$005 I              D1KC5                       §CDKC5
018600940127     I*
018700940127     C*----------------------------------------------------*
018800940127     C*                MAIN LINE PROGRAM
018900940127     C*----------------------------------------------------*
019000940127     C*
019100940223     C* inizializzazione variabili
019200940223     C                   EXSR      INZVAR
019300940223     C*
019400940223     C     $FINE         DOWEQ     *OFF
019500940202     C     $GEST         CASEQ     'D1'          GESD1
019600940117     C                   END
019700940117     C                   END
019800940201     C*
019900940325     C* fine programma
020000940325     C                   EXSR      ENDPGM
020100940325     C/EJECT
020200940325     C************************************************************
020300940325     C* RICERCHE
020400940325     C************************************************************
020500940325     C     ENDPGM        BEGSR
020600940325     C*
020700940614     C* disallocazione rcd nel caso di
020800971110$007 C                   MOVE      'YCD'         XTBCOD
020900940614    >C                   MOVE      '7'           XTBRIC
021000970214     C                   CALLB     'XATB'
021100940614     C                   PARM                    XATBDS
021200940325     C*
021300940325     C* passaggio dati al pgm chiamante
021400940715     C                   MOVE      *BLANKS       KPJBU
021500940325    >C                   MOVEL     D002DS        KPJBU
021600940325     C*
021700940325     C                   SETON                                            RT
021800940325     C*
021900940325     C                   ENDSR
022000940325     C/EJECT
022100940201     C************************************************************
022200940201     C* RICERCHE
022300940201     C************************************************************
022400940127     C     SEARCH        BEGSR
022500940127     C*
022600940127     C* determino Riga/Colonna del cursore
022700940127     C                   MOVE      NRG           R$$
022800940127     C                   MOVE      NCL           C$$
022900940127     C                   Z-ADD     RIRI          CURR
023000940127     C                   Z-ADD     COCO          CURC
023100940127     C*
023200940224     C* I campi che al momento dell'F4 risultano protetti non devono
023300940224     C* essere abilitati alla funzione di ricerca
023400940224     C*
0235009401271    C                   SELECT
023600940127     C*
023700940315     C* FMTD1  - PACCHETTO
023800940614    >C*          H1NMFL    WHEQ 'D1PKG'
023900940207     C* imposto pos. del cursore
024000940614     C*                    Z-ADDCURR      H1RIGA
024100940614     C*                    Z-ADDCURC      H1COLO
024200940127     C*
024300940223     C*
024400940223     C* ricerca non abilitata in questa posizione
0245009402231O   C                   OTHER
024600941005     C                   SETON                                        9899
024700941005     C* imposto pos. del cursore
024800941005     C                   Z-ADD     CURR          H1RIGA
024900941005     C                   Z-ADD     CURC          H1COLO
025000940127     C*
0251009401271-   C                   ENDSL
025200940127     C*
025300940506     C                   MOVE      *HIVAL        $LASTV
025400940127     C                   ENDSR
025500940615     C/EJECT
025600940615     C************************************************************
025700940615     C* TRADUZIONI
025800940615     C************************************************************
025900940615     C     LINGUA        BEGSR
026000940615     C*
026100940615     C* determino Riga/Colonna del cursore
026200940615     C                   MOVE      NRG           R$$
026300940615     C                   MOVE      NCL           C$$
026400940615     C                   Z-ADD     RIRI          CURR
026500940615     C                   Z-ADD     COCO          CURC
026600940615     C*
026700940615     C*
026800940615     C*
026900940615     C* FMTD1  - CAMPI DA TRADURRE
027000940615     C* imposto pos. del cursore
027100940615     C                   Z-ADD     CURR          H1RIGA
027200940615     C                   Z-ADD     CURC          H1COLO
027300940621     C                   MOVEL     H1NMFL        W005              5
027400940621     C                   MOVE      W005          W003              3
027500940621     C                   MOVEL     W003          W006              6
027600940620     C                   MOVE      *ZERO         KEY              15
027700940616     C                   MOVE      D1COD         KEY
027800940615     C                   SELECT
027900940615     C*
028000940615     C*
028100940615     C* ricerca non abilitata in questa posizione
0282009406151O   C                   OTHER
028300940615     C                   SETON                                        98
028400940615     C*
0285009406151-   C                   ENDSL
028600940615     C*
028700940615     C                   MOVE      *HIVAL        $LASTV
028800940615     C                   ENDSR
028900940225     C/EJECT
029000940225     C************************************************************
029100940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
029200940225     C************************************************************
029300940225     C     GESPIE        BEGSR
029400940225     C*
029500940225     C                   SELECT
029600940225     C*
029700940506     C     $GEST         WHENEQ    'D1'
029800940506     C     $ULKD1        IFEQ      0
029900940506     C* riempimento opzioni
030000940506     C                   EXSR      F24D1
030100940506     C                   ELSE
030200940614     C                   WRITE     Z1
030300940506     C                   ENDIF
030400940225     C*
030500940510     C*
030600940225     C                   ENDSL
030700940225     C*
030800940225     C                   ENDSR
030900940131     C/EJECT
031000940131     C************************************************************
031100940510     C* GESTIONE VIDEO RECORD D1
031200940131     C************************************************************
031300940127     C     GESD1         BEGSR
031400940225     C* inizializzazione videata
031500940223     C     $INZD1        IFEQ      *ON
031600940127     C                   EXSR      INZD1
031700940223     C                   MOVE      *OFF          $INZD1
031800940117     C                   END
031900940225     C* emissione piede videata se proveniente da altra
032000940225     C* salvataggio valore formato di provenienza
032100940207     C     $LASTV        IFNE      'D1'
032200940614     C                   WRITE     T1
032300940225     C                   EXSR      GESPIE
032400940202     C                   MOVE      $LASTV        $LASTG
032500940202     C                   MOVE      'D1'          $LASTV
032600940202     C                   END
032700940131     C*
032800940203     C                   SELECT
032900940203     C* Annullamento
033000940315    >C     OPZ002        WHENEQ    '04'
033100940224     C* Visualizzazione
033200940315    >C     OPZ002        OREQ      '05'
033300940620     C                   SETON                                        0203
033400940620     C* Manutenzione
033500940620    >C     OPZ002        WHENEQ    '02'
033600940620     C                   SETON                                        02
033700940620     C                   SETOFF                                       03
033800940620     C*
033900940620     C                   OTHER
034000940620     C*
034100940620     C                   SETOFF                                       0203
034200940203     C                   ENDSL
034300940620     C* Immissione/Modifica/Copia
034400940620     C                   EXFMT     D1
034500940506     C* controllo abilitazione tasti funzione
034600940506     C*
034700940506     C                   EXSR      CTRKD1
034800940506     C*
034900940506     C     *IN99         IFEQ      *OFF
035000940131     C*
0351009402031    C                   SELECT
035200940510     c* ROLLUP
035300940131     C* F3=Fine
035400940506     C     $TASTO        WHENEQ    F03
035500940309     C                   EXSR      F03D1
035600940131     C* F4=Ricerca
035700940506     C     $TASTO        WHENEQ    F04
035800940127     C                   EXSR      SEARCH
035900940224     C* F5=Refresh
036000940506     C     $TASTO        WHENEQ    F05
036100940309     C                   EXSR      F05D1
036200940223     C* F8=Successivo
036300940506     C     $TASTO        WHENEQ    F08
036400940309     C                   EXSR      F08D1
036500940131     C* F12=Ritorno
036600940506     C     $TASTO        WHENEQ    F12
036700940309     C                   EXSR      F12D1
036800940315     C* F24=Altri tasti
036900940506     C     $TASTO        WHENEQ    F24
037000940315     C                   EXSR      F24D1
037100940131     C*
0372009402031O   C                   OTHER
037300940127     C                   EXSR      CTRD1
037400940616     C     *IN99         IFEQ      *OFF
037500940616     C                   SELECT
037600940616     C* F7=Traduzione
037700940616     C     $TASTO        WHENEQ    F07
037800940616     C                   EXSR      LINGUA
037900940616     C* F6
0380009406162    C     $TASTO        WHENEQ    F06
038100940224     C* eseguo aggiornamento
038200940315    >C                   EXSR      AGGANA
038300940224     C* eseguo operazioni del dopo-aggiornamento
0384009403183    C     *IN99         IFEQ      *OFF
038500940318     C                   EXSR      GESAGG
0386009403183-   C                   ENDIF
0387009406162-   C                   ENDSL
038800940616     C                   ENDIF
038900940203     C*
0390009402241-   C                   ENDSL
039100940224     C*
0392009405062-   C                   ENDIF
039300940117     C                   ENDSR
039400940506     C/EJECT
039500940506     C************************************************************
039600940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
039700940506     C************************************************************
039800940506     C     CTRKD1        BEGSR
039900940506     C                   SETOFF                                       99
040000940506     C                   MOVEA     $FC1          $FC
040100940506     C                   EXSR      CTRKEY
040200940506     C     *IN99         IFEQ      *ON
040300940506     C                   SETON                                        97
040400940506     C                   ENDIF
040500940506     C                   ENDSR
040600940117      ************************************************************
040700940207      * INIZIALIZZAZIONE VIDEATA DATI
040800940117      ************************************************************
040900940127     C     INZD1         BEGSR
041000940131     C*
041100940615     C                   CLEAR                   D1
041200941006     C                   MOVE      '0'           D1RIS
041300940616     C                   MOVE      KEY002        D1COD
041400940615     C                   MOVE      AZI002        XTBAZI
041500940615     C                   MOVE      LIN002        XTBLIN
041600940616     C                   MOVE      *ALL'0'       XTBKEY
041700940615     C                   MOVE      KEY002        XTBKEY
041800970509     C                   MOVE      UNI002        XTBUNT
041900940615$007 C                   MOVE      'YCD'         XTBCOD
042000940225     C*
042100940207     C*
042200940224     C* valorizzo la chiave univoca per reperire i dati
042300940224     C* se non immissione
0424009403151   >C     OPZ002        IFNE      '01'
042500940315    >C*-------------------------------------------------------------
042600940315    >C* impostare i campi chiave ricevuti
042700940315    >C*-------------------------------------------------------------
042800940318     C*
042900940614     C*
043000940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0431009406142   >C     OPZ002        IFEQ      '02'
043200940315    >C     OPZ002        OREQ      '04'
043300940614     C                   MOVE      '1'           XTBALC
043400940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0435009406142E   C                   ELSE
043600940614     C                   MOVE      '0'           XTBALC
0437009406142-   C                   ENDIF
043800940614     C*
043900940616     C                   MOVE      '1'           XTBRIC
044000970214     C                   CALLB     'XATB'
044100940614     C                   PARM                    XATBDS
044200940318     C*
044300940614     C     XTBERR        IFEQ      '0'
044400940614     C                   MOVEL     XTBUNI        EXTDS
044500970512     c                   move      xtbunt        d1uniTA           8
044600941108     C* Giro la data se non blank
044700970214     c                   move      xtbdsc        xtbdscA          10
044800970214     C     XTBDSCa       IFNE      DATA
044900970214     C                   CALLB     'XDT4000'
045000970214     C                   PARM      XTBDSCA       XDTAMG           10
045100940621     C                   PARM      *ZEROS        XDTGMA            6 0
045200940621     C                   PARM      2             XDTSTA            1 0
045300940621     C                   MOVE      XDTGMA        D1DSC
045400940712     C                   MOVE      XTBRIS        D1RIS
045500941108     C                   ELSE
045600941108     C                   MOVE      *ZEROS        D1DSC
045700941108     C                   ENDIF
045800941108     C                   ENDIF
045900940614     C*
046000940224     C*
046100940224     C* se il rcd da gestire non viene trovato
046200940318     C* oppure è allocato da altro pgm
046300940224     C* ritorno al pgm guida avvertendolo dell'errore
0464009406142    C     XTBERR        IFNE      '0'
046500940224     C                   MOVE      *ON           $FINE
046600940315    >C                   MOVE      '1'           ERR002
046700940224     C*
046800940224     C* altrimenti
0469009402242E   C                   ELSE
047000940224     C* riempio la videata
047100940224     C                   EXSR      RIED1
047200940224     C* richiamo routine dei ctrl per decodificarla
047300940614    >C                   EXSR      CTRD1
047400960228     C                   MOVEL     *IN99         SAV99             1
047500960228     C* Se la gestione è più videate
047600960305    >C                   EXSR      CTRD2
047700960305    >C     *IN99         IFEQ      *ON
047800960305    >C                   MOVEL     *IN99         SAV99             1
047900960305    >C                   ENDIF
048000940224     C                   MOVE      *ALL'0'       IN5098
048100940224     C                   MOVEA     IN5098        *IN(50)
048200940224     C* definizione indicatori su video
048300940224     C                   SETOFF                                       02
0484009402243    C                   SELECT
048500940315    >C     OPZ002        WHENEQ    '02'
048600940224     C                   SETON                                        02
048700940315    >C     OPZ002        WHENEQ    '05'
048800940506     C* disabilita tasti
048900940720     C                   MOVE      '0'           $FC(6)
049000940720     C                   MOVE      '0'           $FC(5)
049100940720     C                   MOVE      '0'           $FC(4)
0492009402243-   C                   ENDSL
049300940224     C*
0494009402242-   C                   ENDIF
049500940223     C*
0496009402241-   C                   ENDIF
049700940224     C*
049800940117     C                   ENDSR
049900940207     C/EJECT
050000940207     C************************************************************
050100940510     C* RIEMPIMENTO VIDEATA  D1
050200940207     C************************************************************
050300940207     C     RIED1         BEGSR
050400940207     C*
050500940207     C*
050600940207     C                   ENDSR
050700940309     C/EJECT
050800940309     C************************************************************
050900940309     C* GESTIONE F03 VIDEO D1
051000940309     C************************************************************
051100940309     C     F03D1         BEGSR
051200940309     C*
051300940309     C                   MOVE      *ON           $FINE
051400940315    >C                   MOVE      '1'           RET002
051500940325     C* fine programma
051600940325     C                   EXSR      ENDPGM
051700940325     C*
051800940325     C                   ENDSR
051900940309     C/EJECT
052000940309     C************************************************************
052100940309     C* GESTIONE F05 VIDEO D1
052200940309     C************************************************************
052300940309     C     F05D1         BEGSR
052400940309     C*
052500940309     C                   MOVE      *ON           $INZD1
052600940309     C*
052700940309     C                   ENDSR
052800940309     C/EJECT
052900940309     C************************************************************
053000940309     C* GESTIONE F08 VIDEO D1
053100940309     C************************************************************
053200940309     C     F08D1         BEGSR
053300940309     C*
053400940309     C                   MOVE      *ON           $FINE
053500940315    >C                   MOVE      '0'           RET002
053600940309     C*
053700940309     C                   ENDSR
053800940309     C/EJECT
053900940309     C************************************************************
054000940309     C* GESTIONE F12 VIDEO D1
054100940309     C************************************************************
054200940309     C     F12D1         BEGSR
054300940309     C*
054400940309     C                   MOVE      *ON           $FINE
054500940315    >C                   MOVE      '2'           RET002
054600940309     C*
054700940309     C                   ENDSR
054800940309     C/EJECT
054900940309     C************************************************************
055000940309     C* GESTIONE F24 VIDEO D1
055100940309     C************************************************************
055200940309     C     F24D1         BEGSR
055300940309     C*
055400940506     C*
055500940506     C                   MOVEA     $FX1          $FX
055600940506     C                   MOVEA     $FC1          $FC
055700940506     C                   MOVEA     $FR1          $FR
055800940506     C                   Z-ADD     $ULKD1        $ULK
055900940506     C                   EXSR      RIEKEY
056000941017     C                   MOVEA     $FC           $FC1
056100940506     C                   Z-ADD     $ULK          $ULKD1            3 0
056200940506     C                   MOVEL     $KEY1         Z1KE1
056300940506     C                   MOVEL     $KEY2         Z1KE2
056400940614     C                   WRITE     Z1
056500940506     C*
056600940309     C                   ENDSR
056700940207     C/EJECT
056800940131     C************************************************************
056900940207     C* CONTROLLO VIDEATA
057000940131     C************************************************************
057100940127     C     CTRD1         BEGSR
057200940131     C*
057300940127     C                   SETOFF                                       99
057400940127     C* non imposto pos. del cursore
057500940127     C                   Z-ADD     0             H1RIGA
057600940127     C                   Z-ADD     0             H1COLO
057700940621     C*
057800940621     C* CTRL DATA
057900940621     C*
058000941109     C                   MOVE      DATA          WRKDSC           10
058100940621     C     D1DSC         IFNE      0
058200970214     C                   CALLB     'XDT4000'
058300941109     C                   PARM                    XDTAMG           10
058400940621     C                   PARM      D1DSC         XDTGMA            6 0
058500940621     C                   PARM      3             XDTSTA            1 0
058600940621     C     XDTSTA        IFNE      0
058700940621     C                   SETON                                        5599
058800940621     C                   ELSE
058900941108     C                   MOVE      XDTAMG        WRKDSC
059000940621     C                   ENDIF
059100940621     C                   ENDIF
059200940223     C*
059300940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
059400940223     C*
0595009403151   >C     OPZ002        IFNE      '04'
059600971110     C*
059700971110     C* controllo codice vuoto
059800980921$017 C     D1COD         IFEQ      *zeros
059900971110     C                   SETON                                        5099
060000971110     C                   ENDIF
060100971110     C*
060200940207     C*
060300940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
060400940315    >C     OPZ002        IFEQ      '01'
060500940315    >C     OPZ002        OREQ      '03'
060600940315    >C*-------------------------------------------------------------
060700940315    >C* impostare i campi chiave e verificare l'esistenza del record
060800940315    >C*-------------------------------------------------------------
060900971110$007 C                   MOVE      'YCD'         XTBCOD
061000940614     C                   MOVE      *ALL'0'       XTBKEY
061100940620    >C                   MOVE      D1COD         XTBKEY
061200970512    >C                   MOVE      D1UNITA       XTBUNT
061300940614     C                   MOVE      '0'           XTBALC
061400940614     C                   MOVE      '1'           XTBRIC
061500970214    >C                   CALLB     'XATB'
061600940614     C                   PARM                    XATBDS
061700940614     C     XTBERR        IFEQ      '0'
061800940223     C                   SETON                                        5999
061900940223     C                   ENDIF
062000940620     C* Controllo se compreso nei range indice
062100940620     C     XTBERR        IFEQ      '3'
062200940620    >C                   SETON                                        5899
062300940620     C                   END
062400940620     C*
062500940620     C* REPERISCO AUTORIZZAZIONI UTENTE
062600940620     C                   CLEAR                   XCHATB
062700941118     C                   MOVE      XSCSOC        XCTSOC
062800941118     C                   MOVE      XSCDUN        XCTUNI
062900940620     C                   MOVEL     KNMUS         XCTPRF
063000940620     C                   MOVE      XTBCOD        XCTCOD
063100940620     C                   CALL      'XCHKATB'
063200940620     C                   PARM                    XCHATB
063300941111     C                   MOVEL     XCTDDS        AUTDS
063400941027     C     XCTRTN        IFEQ      1
063500941027     C                   MOVEL     'N'           MANGRU
063600941027     C                   ENDIF
063700940620     C* Non ammessa immissione di gruppo
063800940620     C     XTBGRU        IFEQ      'G'
063900940620     C     MANGRU        ANDEQ     'N'
064000940620     C                   SETON                                        5799
064100940620     C                   END
064200940620     C* Non ammessa immissione di azienda
064300940620     C     XTBGRU        IFEQ      'A'
064400940620     C     MANAZI        ANDEQ     'N'
064500940620     C                   SETON                                        5699
064600940620     C                   END
064700940712     C* codice riservato non accessibile
064800941006     C     XTBRIS        IFGT      ABIRIS
064900941005    >C     OPZ002        ANDNE     '01'
065000941005    >C     OPZ002        ANDNE     '03'
065100940712     C                   SETON                                        5499
065200940712     C                   END
065300940224     C                   ENDIF
065400980921     C                   if        §cdkc1 <> *blank or §cdks1 <> *blank
065500980921     C                   move      §cdkc1        kcc
065600980921     C                   move      §cdks1        ksc
065700980921     C     kanpdc        chain     anpdc01l                           21
065800980921     C                   if        *in21
065900980921     C                   SETON                                        7099
0660009809211-   C                   END
0661009809211-   C                   end
066200980921     C                   if        §cdkc2 <> *blank or §cdks2 <> *blank
066300980921     C                   move      §cdkc2        kcc
066400980921     C                   move      §cdks2        ksc
066500980921     C     kanpdc        chain     anpdc01l                           21
066600980921     C                   if        *in21
066700980921     C                   SETON                                        7199
0668009809211-   C                   END
0669009809211-   C                   end
067000980921     C                   if        §cdkc3 <> *blank or §cdks3 <> *blank
067100980921     C                   move      §cdkc3        kcc
067200980921     C                   move      §cdks3        ksc
067300980921     C     kanpdc        chain     anpdc01l                           21
067400980921     C                   if        *in21
067500980921     C                   SETON                                        7299
0676009809211-   C                   END
0677009809211-   C                   end
067800980921     C                   if        §cdkc4 <> *blank or §cdks4 <> *blank
067900980921     C                   move      §cdkc4        kcc
068000980921     C                   move      §cdks4        ksc
068100980921     C     kanpdc        chain     anpdc01l                           21
068200980921     C                   if        *in21
068300980921     C                   SETON                                        7399
0684009809211-   C                   END
0685009809211-   C                   end
068600980921     C                   if        §cdkc5 <> *blank or §cdks5 <> *blank
068700980921     C                   move      §cdkc5        kcc
068800980921     C                   move      §cdks5        ksc
068900980921     C     kanpdc        chain     anpdc01l                           21
069000980921     C                   if        *in21
069100980921     C                   SETON                                        7499
0692009809211-   C                   END
0693009809211-   C                   end
069400940224     C*
0695009402241-   C                   END
069600940131     C*
069700940302     C     *IN99         IFEQ      *ON
069800940510     C     $GEST         ANDEQ     'D1'
069900960228     C     OPZ002        ANDNE     '07'
070000940117     C                   MOVEA     *IN           WIN
070100940128     C                   MOVE      *ALL'0'       IN5098           49
070200940128     C                   MOVEA     IN5098        *IN(50)
070300940614     C                   WRITE     D1
070400940117     C                   MOVEA     WIN           *IN
070500940302     C                   ENDIF
070600940127     C*
070700940117     C                   ENDSR
070800960305     C/EJECT
070900960305     C************************************************************
071000960305     C* CONTROLLO VIDEATA D2
071100960305     C************************************************************
071200960305     C     CTRD2         BEGSR
071300960305     C*
071400960305     C                   ENDSR
071500940207     C/EJECT
071600940224     C************************************************************
071700940315    >C* AGGIORNAMENTO ANAGRAFICA
071800940224     C************************************************************
071900940315    >C     AGGANA        BEGSR
072000971110     C*
072100971110$007 C                   MOVE      'YCD'         XTBCOD
072200940614     C                   MOVE      *ALL'0'       XTBKEY
072300940614     C                   MOVE      D1COD         XTBKEY
072400970512     C                   MOVE      D1UNITA       XTBUNT
072500940207     C*
072600940207     C                   SELECT
072700940315    >C     OPZ002        WHENEQ    '01'
072800940315    >C     OPZ002        OREQ      '03'
072900940315    >C                   EXSR      RIEANA
073000950609     C                   MOVE      XSCCUT        XTBUTE
073100940614     C                   MOVE      '5'           XTBRIC
073200970214     C                   CALLB     'XATB'
073300940614    >C                   PARM                    XATBDS
073400940318     C* se rcd non scrivibile attivo errore
073500940615     C     XTBERR        IFEQ      '1'
073600940318    >C                   SETON                                        5999
073700940318     C                   END
073800940315    >C     OPZ002        WHENEQ    '02'
073900940621     C                   MOVE      '1'           XTBRIC
074000940621     C                   MOVE      '1'           XTBALC
074100970214     C                   CALLB     'XATB'
074200940621     C                   PARM                    XATBDS
074300940622    >C                   EXSR      RIEANA
074400950609     C                   MOVE      XSCCUT        XTBUTE
074500940614     C                   MOVE      '4'           XTBRIC
074600970214     C                   CALLB     'XATB'
074700940614    >C                   PARM                    XATBDS
074800940315    >C     OPZ002        WHENEQ    '04'
074900940621     C                   MOVE      '1'           XTBRIC
075000940621     C                   MOVE      '1'           XTBALC
075100970214     C                   CALLB     'XATB'
075200940621     C                   PARM                    XATBDS
075300940614     C                   MOVE      '6'           XTBRIC
075400970214     C                   CALLB     'XATB'
075500940614    >C                   PARM                    XATBDS
075600940621     C* annullo traduzioni
075700940621     C     XTBERR        IFEQ      '0'
075800940621     C                   EXSR      ANNTBT
075900940621     C                   END
076000940621     C*
076100940207     C                   ENDSL
076200940203     C*
076300940203     C                   ENDSR
076400940224     C/EJECT
076500940224     C************************************************************
076600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
076700940224     C************************************************************
076800940616     C     RIEANA        BEGSR
076900940616     C                   CLEAR                   XTBUNI
077000940621     C                   MOVE      WRKDSC        XTBDSC
077100940712     C                   MOVE      D1RIS         XTBRIS
077200940616     C                   MOVEL     EXTDS         XTBUNI
077300940207     C                   ENDSR
077400940224     C/EJECT
077500940224     C************************************************************
077600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
077700940224     C************************************************************
077800940224     C     GESAGG        BEGSR
077900940224     C*
078000940224     C* segnalo al pgm chiamante l'avvenuta conferma
078100940315    >C                   MOVE      *ON           OPR002
078200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
078300940315    >C                   MOVE      '0'           RET002
078400940224     C*
0785009402241    C                   SELECT
078600940224     C* nel caso di immissione, ripristino la videata iniziale
078700940315    >C     OPZ002        WHENEQ    '01'
078800940224     C                   MOVE      *ON           $INZD1
078900940224     C* altrimenti ritorno al pgm chiamante
079000940224     C                   OTHER
079100940224     C                   MOVE      *ON           $FINE
0792009402241-   C                   ENDSL
079300940224     C*
079400940224     C                   ENDSR
079500940621     C/EJECT
079600940621     C************************************************************
079700940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
079800940621     C************************************************************
079900940621    >C     ANNTBT        BEGSR
080000940621     C                   MOVE      XTBAZI        TBTAZI
080100940621     C                   MOVE      XTBCOD        TBTCOD
080200940621     C                   MOVE      XTBKEY        TBTKEY
080300940621     C     KTBT          SETLL     ANTBT01L
080400940621     C                   DO        *HIVAL
080500940621     C     KTBT          DELETE    ANTBT01L                           21
080600940621     C  N21              END
080700940621     C*
080800940621     C                   ENDSR
080900940207     C/EJECT
081000940131     C************************************************************
081100940131     C* OPERAZIONI INIZIALI
081200940131     C************************************************************
081300940131     C     *INZSR        BEGSR
081400940131     C*
081500940127     C* Reperimento parametri
081600940127     C*
081700940117     C     *ENTRY        PLIST
081800940117     C                   PARM                    KPJBA
081900940712     C*
082000940712     C* Reperimento Codice utente
082100940712     C*
082200940223     C*
082300940506     C*
082400940506     C* Reperimento tasti di funzione
082500940506     C*
082600940506     C                   EXSR      REPKEY
082700940621     C*
082800940621     C* KLIST
082900940621     C*
083000940621     C     KTBT          KLIST
083100940621     C                   KFLD                    TBTAZI
083200940621     C                   KFLD                    TBTCOD
083300940621     C                   KFLD                    TBTKEY
083400940506     C*
083500980921     C     Kanpdc        KLIST
083600980921     C                   KFLD                    xscsoc
083700980921     C                   KFLD                    kcc               6
083800980921     C                   KFLD                    ksc               8
083900940223     C                   ENDSR
084000941103     C/EJECT
084100941103      ************************************************************
084200941103      * Reperimento dati società
084300941103      ************************************************************
084400941103     C     REPSOC        BEGSR
084500941103      *
084600970214     C                   CALLB     'XSOC'
084700941103     C                   PARM                    TIPXSC            6
084800941103     C                   PARM                    SOCXSC            3
084900941103     C                   PARM                    CDSXSC            9 0
085000941103     C                   PARM                    MODXSC            3
085100941103     C                   PARM      *BLANKS       RTNXSC            1
085200941103     C                   PARM                    XSOCDS
085300941103     C                   PARM                    KPJBA
085400941103      *
085500941103     C                   ENDSR
085600940223     C/EJECT
085700940506     C************************************************************
085800940506     C* REPERIMENTO TASTI DI FUNZIONE
085900940506     C************************************************************
086000940506     C     REPKEY        BEGSR
086100940506     C                   MOVEA     $FM1          $FM
086200940506     C                   EXSR      REPKKK
086300940506     C                   MOVEA     $FX           $FX1
086400940614     C                   ENDSR
086500940506     C/EJECT
086600940506     C************************************************************
086700940506     C* REPERIMENTO TASTI DI FUNZIONE
086800940506     C************************************************************
086900940506     C     REPKKK        BEGSR
087000940506     C*
087100940506     C                   CLEAR                   $FX
087200940506     C     1             DO        24            X
087300940506     C*
087400940506     C     $FM(X)        IFNE      *BLANK
087500940506     C*
087600940506     C                   MOVEL     $FM(X)        IDMSG
087700970214     C                   CALLB     'XRTVM'
087800940506     C                   PARM                    IDMSG            27
087900940506     C                   PARM                    TXTMSG          100
088000940506     C                   PARM                    ERRMSG            1
088100940506     C*
088200940506     C     ERRMSG        IFNE      *ON
088300940506     C                   MOVEL     TXTMSG        $FX(X)
088400940506     C                   ELSE
088500940506     C                   MOVEL     *ALL' '       $FX(X)
088600940506     C                   END
088700940506     C*
088800940506     C                   END
088900940506     C*
089000940506     C                   END
089100940506     C*
089200940506     C                   ENDSR
089300940509     C/EJECT
089400940223     C************************************************************
089500940223     C* INIZIALIZZAZIONE VARIABILI
089600940223     C************************************************************
089700940223     C     INZVAR        BEGSR
089800951124     C*
089900951124     C* Chiamata XSOC
090000951124     C*
090100951124     C                   MOVEL     'SOC001'      TIPXSC
090200951124     C                   EXSR      REPSOC
090300951124     C     RTNXSC        IFNE      '1'
090400951124     C                   MOVEL     XSOCDS        SOC001
090500951124     C                   ELSE
090600951124     C                   MOVE      *ON           $FINE
090700951124     C* fine programma
090800951124     C                   EXSR      ENDPGM
090900951124     C                   ENDIF
091000940224     C*
091100940224     C* Dati da PGM guida
091200940224     C*
091300940315    >C                   MOVEL     KPJBU         D002DS
091400940128     C*
091500940223     C* Variabili per gestione videate
091600940128     C*
091700940223     C                   MOVE      'D1'          $GEST
091800940223     C                   MOVE      *OFF          $FINE
091900940223     C                   MOVE      *ON           $INZD1
092000940224     C                   MOVE      *BLANK        $LASTG
092100940224     C                   MOVE      *BLANK        $LASTV
092200940506     C                   Z-ADD     0             $ULKD1            3 0
092300940127     C*
092400940223     C* Variabili appoggio
092500940127     C*
092600940224     C                   Z-ADD     0             CURR
092700940224     C                   Z-ADD     0             CURC
092800940207     C*
092900940207     C* Valorizzazione campi univoci testate
093000940207     C*
093100940614     C                   CLEAR                   T1
093200940614     C                   MOVEL     KNSIF         NOMSIF
093300941109     C                   MOVEL     XSCDSI        NOMDIT
093400941027     C                   MOVEL     DSPGM         NOMPGM
093500940224     C                   Z-ADD     1             X
093600940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
093700940224     C     *IN21         IFEQ      *ON
093800940224     C                   MOVEL     $ID(X)        IDMSG
093900970214     C                   CALLB     'XRTVM'
094000940224     C                   PARM                    IDMSG            27
094100940224     C                   PARM                    TXTMSG          100
094200940224     C                   PARM                    ERRMSG            1
094300940224     C                   PARM      *ON           CTRMSG            1
094400940224     C                   PARM      30            LENMSG            3 0
094500940224     C     ERRMSG        IFNE      *ON
094600940224     C                   MOVEL     TXTMSG        T1OPE
094700940224     C                   ELSE
094800940224     C                   MOVEL     *ALL'?'       T1OPE
094900940224     C                   ENDIF
095000940224     C                   END
095100940224     C*
095200940224     C* Inizializzazione/Decodifica prima videata
095300940224     C*
095400940224     C                   EXSR      INZD1
095500940224     C                   MOVE      *OFF          $INZD1
095600960228     C* Se opzione = '07' richiesti solo controlli
095700960228     C     OPZ002        IFEQ      '07'
095800960228     C                   MOVEL     *IN99         ERR002
095900960228     C                   MOVEL     '0'           RET002
096000960228     C                   MOVEL     *ON           $FINE
096100960228     C                   ENDIF
096200940127     C*
096300940117     C                   ENDSR
096400940128     C************************************************************
096500940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
096600940223     C************************************************************
096700940223     C     DEFCAM        BEGSR
096800940223     C*
096900940223     C* klist
097000940207     C*
097100940223     C*
097200940223     C* Variabili per gestione videate
097300940223     C*
097400940223     C                   MOVE      *BLANK        $GEST             2
097500940223     C                   MOVE      *BLANK        $FINE             1
097600940223     C                   MOVE      *BLANK        $INZD1            1
097700940223     C                   MOVE      *BLANK        $LASTG            2
097800940223     C                   MOVE      *BLANK        $LASTV            2
097900940223     C*
098000940223     C* Variabili appoggio
098100940223     C*
098200940223     C                   Z-ADD     0             CURR              2 0
098300940223     C                   Z-ADD     0             CURC              2 0
098400940223     C                   MOVE      *ZEROS        WIN              99
098500940224     C                   MOVE      *BLANK        WTBERR            1
098600940223     C*
098700940223     C* Indici
098800940223     C*
098900940223     C                   Z-ADD     0             X                 3 0
099000940223     C                   Z-ADD     0             Y                 3 0
099100940127     C*
099200940117     C                   ENDSR
099300940128     C************************************************************
099400940509     C/EJECT
099500940509     C************************************************************
099600940509     C* CONTROLLO ABILITAZIONE TASTI
099700940509     C************************************************************
099800940509     C     CTRKEY        BEGSR
099900940509     C                   SELECT
100000940509     C     $TASTO        WHENEQ    F01
100100940720     C     $FC(1)        IFEQ      '0'
100200940509     C                   SETON                                        99
100300940509     C                   ENDIF
100400940509     C     $TASTO        WHENEQ    F02
100500940720     C     $FC(2)        IFEQ      '0'
100600940509     C                   SETON                                        99
100700940509     C                   ENDIF
100800940509     C     $TASTO        WHENEQ    F03
100900940720     C     $FC(3)        IFEQ      '0'
101000940509     C                   SETON                                        99
101100940509     C                   ENDIF
101200940509     C     $TASTO        WHENEQ    F04
101300940720     C     $FC(4)        IFEQ      '0'
101400940509     C                   SETON                                        99
101500940509     C                   ENDIF
101600940509     C     $TASTO        WHENEQ    F05
101700940720     C     $FC(5)        IFEQ      '0'
101800940509     C                   SETON                                        99
101900940509     C                   ENDIF
102000940509     C     $TASTO        WHENEQ    F06
102100940720     C     $FC(6)        IFEQ      '0'
102200940509     C                   SETON                                        99
102300940509     C                   ENDIF
102400940509     C     $TASTO        WHENEQ    F07
102500940720     C     $FC(7)        IFEQ      '0'
102600940509     C                   SETON                                        99
102700940509     C                   ENDIF
102800940509     C     $TASTO        WHENEQ    F08
102900940720     C     $FC(8)        IFEQ      '0'
103000940509     C                   SETON                                        99
103100940509     C                   ENDIF
103200940509     C     $TASTO        WHENEQ    F09
103300940720     C     $FC(09)       IFEQ      '0'
103400940509     C                   SETON                                        99
103500940509     C                   ENDIF
103600940509     C     $TASTO        WHENEQ    F10
103700940720     C     $FC(10)       IFEQ      '0'
103800940509     C                   SETON                                        99
103900940509     C                   ENDIF
104000940509     C     $TASTO        WHENEQ    F11
104100940720     C     $FC(11)       IFEQ      '0'
104200940509     C                   SETON                                        99
104300940509     C                   ENDIF
104400940509     C     $TASTO        WHENEQ    F12
104500940720     C     $FC(12)       IFEQ      '0'
104600940509     C                   SETON                                        99
104700940509     C                   ENDIF
104800940509     C     $TASTO        WHENEQ    F13
104900940720     C     $FC(13)       IFEQ      '0'
105000940509     C                   SETON                                        99
105100940509     C                   ENDIF
105200940509     C     $TASTO        WHENEQ    F14
105300940720     C     $FC(14)       IFEQ      '0'
105400940509     C                   SETON                                        99
105500940509     C                   ENDIF
105600940509     C     $TASTO        WHENEQ    F15
105700940720     C     $FC(15)       IFEQ      '0'
105800940509     C                   SETON                                        99
105900940509     C                   ENDIF
106000940509     C     $TASTO        WHENEQ    F16
106100940720     C     $FC(16)       IFEQ      '0'
106200940509     C                   SETON                                        99
106300940509     C                   ENDIF
106400940509     C     $TASTO        WHENEQ    F17
106500940720     C     $FC(17)       IFEQ      '0'
106600940509     C                   SETON                                        99
106700940509     C                   ENDIF
106800940509     C     $TASTO        WHENEQ    F18
106900940720     C     $FC(18)       IFEQ      '0'
107000940509     C                   SETON                                        99
107100940509     C                   ENDIF
107200940509     C     $TASTO        WHENEQ    F19
107300940720     C     $FC(19)       IFEQ      '0'
107400940509     C                   SETON                                        99
107500940509     C                   ENDIF
107600940509     C     $TASTO        WHENEQ    F20
107700940720     C     $FC(20)       IFEQ      '0'
107800940509     C                   SETON                                        99
107900940509     C                   ENDIF
108000940509     C     $TASTO        WHENEQ    F21
108100940720     C     $FC(21)       IFEQ      '0'
108200940509     C                   SETON                                        99
108300940509     C                   ENDIF
108400940509     C     $TASTO        WHENEQ    F22
108500940720     C     $FC(22)       IFEQ      '0'
108600940509     C                   SETON                                        99
108700940509     C                   ENDIF
108800940509     C     $TASTO        WHENEQ    F23
108900940720     C     $FC(23)       IFEQ      '0'
109000940509     C                   SETON                                        99
109100940509     C                   ENDIF
109200940509     C     $TASTO        WHENEQ    F24
109300940720     C     $FC(24)       IFEQ      '0'
109400940509     C                   SETON                                        99
109500940509     C                   ENDIF
109600940509     C                   ENDSL
109700940509     C                   ENDSR
109800940509     C/EJECT
109900940509     C************************************************************
110000940509     C* RIEMPIMENTO TASTI FUNZIONE
110100940509     C************************************************************
110200940509     C     RIEKEY        BEGSR
110300940509     C*
110400940509     C* Input :
110500940509     C* - $FX = schiera con tutti i testi disponibili
110600940509     C* - $FC = schiere con i flag di validità associati ai testi
110700940509     C* - $FR = schiere con i flag di REVERSE IMAGE
110800940509     C* - $ULK = ultimo testo caricato
110900940509     C* Output :
111000940509     C* - $ULK
111100940509     c* - $KEY1= 1° riga di tasti funzione
111200940509     c* - $KEY2= 2° riga di tasti funzione
111300940509     C*
111400970214     C                   CALLB     'XKEYMSG'
111500940509     C* input
111600940509     C                   PARM                    $FX
111700940509     C                   PARM                    $FC
111800940509     C                   PARM                    $FR
111900940509     C                   PARM                    $ULK              3 0
112000940509     C* output
112100940509     C                   PARM                    $KEY1            79
112200940509     C                   PARM                    $KEY2            79
112300940509     C*
112400940509     C                   ENDSR
112500940615**
11260094022401PROMSG    *LIBL     COS0001    immissione
11270094022402PROMSG    *LIBL     COS0002    modifica
11280094022403PROMSG    *LIBL     COS0003    copia
11290094022404PROMSG    *LIBL     COS0004    annullamento
11300094022405PROMSG    *LIBL     COS0005    visualizzazione
113100940509** TASTI DI FUNZIONE  D1
113200940509PROMSG    *LIBL     KEY0001  01
113300940509PROMSG    *LIBL     KEY0002  02
113400940509PROMSG    *LIBL     KEY0003  03
113500940509PROMSG    *LIBL     KEY0004  04
113600940509PROMSG    *LIBL     KEY0005  05
113700940509PROMSG    *LIBL     KEY0006  06
113800940509PROMSG    *LIBL     KEY0007  07
113900940509PROMSG    *LIBL     KEY0008  08
114000940509PROMSG    *LIBL     KEY0009  09
114100940509PROMSG    *LIBL     KEY0010  10
114200940509PROMSG    *LIBL     KEY0011  11
114300940509PROMSG    *LIBL     KEY0012  12
114400940509PROMSG    *LIBL     KEY0013  13
114500940509PROMSG    *LIBL     KEY0014  14
114600940509PROMSG    *LIBL     KEY0015  15
114700940509PROMSG    *LIBL     KEY0016  16
114800940509PROMSG    *LIBL     KEY0017  17
114900940509PROMSG    *LIBL     KEY0018  18
115000940509PROMSG    *LIBL     KEY0019  19
115100940509PROMSG    *LIBL     KEY0020  20
115200940509PROMSG    *LIBL     KEY0021  21
115300940509PROMSG    *LIBL     KEY0022  22
115400940509PROMSG    *LIBL     KEY0023  23
115500940509PROMSG    *LIBL     KEY0024  24
115600940509**  ABILITAZIONE D1
1157009407200N  01
1158009407200N  02
1159009407201N  03
1160009407201N  04
1161009407201N  05
1162009407201N  06
1163009407201N  07
1164009407201N  08
1165009407200N  09
1166009407200N  10
1167009407200N  11
1168009407201N  12
1169009407200N  13
1170009407200N  14
1171009407200N  15
1172009407200N  16
1173009407200N  17
1174009407200N  18
1175009407200N  19
1176009407200N  20
1177009407200N  21
1178009407200N  22
1179009407200N  23
1180009407200N  24
