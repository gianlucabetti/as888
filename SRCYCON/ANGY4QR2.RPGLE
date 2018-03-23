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
004100940614$001 FANGY4QD2  CF   E             WORKSTN
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
006000940614$003 D EXTDS         E DS                  EXTNAME(ANGY4QDS) INZ
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
013800940225     D*-------------
013900940225     D* COSTANTI
014000940225     D*-------------
014100940506     D*
014200940506     D* Tasti di funzione
014300940506     D F01             C                   CONST(X'31')
014400940506     D F02             C                   CONST(X'32')
014500940506     D F03             C                   CONST(X'33')
014600940506     D F04             C                   CONST(X'34')
014700940506     D F05             C                   CONST(X'35')
014800940506     D F06             C                   CONST(X'36')
014900940506     D F07             C                   CONST(X'37')
015000940506     D F08             C                   CONST(X'38')
015100940506     D F09             C                   CONST(X'39')
015200940506     D F10             C                   CONST(X'3A')
015300940506     D F11             C                   CONST(X'3B')
015400940506     D F12             C                   CONST(X'3C')
015500940506     D F13             C                   CONST(X'B1')
015600940506     D F14             C                   CONST(X'B2')
015700940506     D F15             C                   CONST(X'B3')
015800940506     D F16             C                   CONST(X'B4')
015900940506     D F17             C                   CONST(X'B5')
016000940506     D F18             C                   CONST(X'B6')
016100940506     D F19             C                   CONST(X'B7')
016200940506     D F20             C                   CONST(X'B8')
016300940506     D F21             C                   CONST(X'B9')
016400940506     D F22             C                   CONST(X'BA')
016500940506     D F23             C                   CONST(X'BB')
016600940506     D F24             C                   CONST(X'BC')
016700940506     D ENTER           C                   CONST(X'F1')
016800940506     D ROLDWN          C                   CONST(X'F4')
016900940506     D ROLLUP          C                   CONST(X'F5')
017000940506     D*-------------
017100940506     D*
017200941108     D DATA            C                   CONST('0001-01-01')
017300940128     I*----------------------------------------------------*
017400940615     ID1
017500981013$005 I              D1KSC                       §4qKSC
017600981013$005 I              D1SOC                       §4qSOC
017700981013$005 I              D1CCC                       §4qCCC
017800981013$005 I              D1SCC                       §4qSCC
017900981013$005 I              D1BAN                       §4qBAN
018000981013$005 I              D1LIN                       §4qLIN
018100981013$005 I              D1ABI                       §4qABI
018200981013$005 I              D1CAB                       §4qCAB
018300981013$005 I              D1CCB                       §4qCCB
018400981013$005 I              D1FPC                       §4qFPC
018500981013$005 I              D1DES                       §4qDES
018600981013$005 I              D1CAU                       §4qCAU
018700981013$005 I              D1FIL                       §4qFIL
018800990607$005 I              D1DAT                       §4qDAT
018900940127     I*
019000940127     C*----------------------------------------------------*
019100940127     C*                MAIN LINE PROGRAM
019200940127     C*----------------------------------------------------*
019300940127     C*
019400940223     C* inizializzazione variabili
019500940223     C                   EXSR      INZVAR
019600940223     C*
019700940223     C     $FINE         DOWEQ     *OFF
019800940202     C     $GEST         CASEQ     'D1'          GESD1
019900940117     C                   END
020000940117     C                   END
020100940201     C*
020200940325     C* fine programma
020300940325     C                   EXSR      ENDPGM
020400940325     C/EJECT
020500940325     C************************************************************
020600940325     C* RICERCHE
020700940325     C************************************************************
020800940325     C     ENDPGM        BEGSR
020900940325     C*
021000940614     C* disallocazione rcd nel caso di
021100971110$007 C                   MOVE      'Y4Q'         XTBCOD
021200940614    >C                   MOVE      '7'           XTBRIC
021300970214     C                   CALLB     'XATB'
021400940614     C                   PARM                    XATBDS
021500940325     C*
021600940325     C* passaggio dati al pgm chiamante
021700940715     C                   MOVE      *BLANKS       KPJBU
021800940325    >C                   MOVEL     D002DS        KPJBU
021900940325     C*
022000940325     C                   SETON                                            RT
022100940325     C*
022200940325     C                   ENDSR
022300940325     C/EJECT
022400940201     C************************************************************
022500940201     C* RICERCHE
022600940201     C************************************************************
022700940127     C     SEARCH        BEGSR
022800940127     C*
022900940127     C* determino Riga/Colonna del cursore
023000940127     C                   MOVE      NRG           R$$
023100940127     C                   MOVE      NCL           C$$
023200940127     C                   Z-ADD     RIRI          CURR
023300940127     C                   Z-ADD     COCO          CURC
023400940127     C*
023500940224     C* I campi che al momento dell'F4 risultano protetti non devono
023600940224     C* essere abilitati alla funzione di ricerca
023700940224     C*
0238009401271    C                   SELECT
023900940127     C*
024000940315     C* FMTD1  - PACCHETTO
024100940614    >C*          H1NMFL    WHEQ 'D1PKG'
024200940207     C* imposto pos. del cursore
024300940614     C*                    Z-ADDCURR      H1RIGA
024400940614     C*                    Z-ADDCURC      H1COLO
024500940127     C*
024600940223     C*
024700940223     C* ricerca non abilitata in questa posizione
0248009402231O   C                   OTHER
024900941005     C                   SETON                                        9899
025000941005     C* imposto pos. del cursore
025100941005     C                   Z-ADD     CURR          H1RIGA
025200941005     C                   Z-ADD     CURC          H1COLO
025300940127     C*
0254009401271-   C                   ENDSL
025500940127     C*
025600940506     C                   MOVE      *HIVAL        $LASTV
025700940127     C                   ENDSR
025800940615     C/EJECT
025900940615     C************************************************************
026000940615     C* TRADUZIONI
026100940615     C************************************************************
026200940615     C     LINGUA        BEGSR
026300940615     C*
026400940615     C* determino Riga/Colonna del cursore
026500940615     C                   MOVE      NRG           R$$
026600940615     C                   MOVE      NCL           C$$
026700940615     C                   Z-ADD     RIRI          CURR
026800940615     C                   Z-ADD     COCO          CURC
026900940615     C*
027000940615     C*
027100940615     C*
027200940615     C* FMTD1  - CAMPI DA TRADURRE
027300940615     C* imposto pos. del cursore
027400940615     C                   Z-ADD     CURR          H1RIGA
027500940615     C                   Z-ADD     CURC          H1COLO
027600940621     C                   MOVEL     H1NMFL        W005              5
027700940621     C                   MOVE      W005          W003              3
027800940621     C                   MOVEL     W003          W006              6
027900940620     C                   MOVE      *ZERO         KEY              15
028000940616     C                   MOVE      D1COD         KEY
028100940615     C                   SELECT
028200940615     C*
028300940615     C*
028400940615     C* ricerca non abilitata in questa posizione
0285009406151O   C                   OTHER
028600940615     C                   SETON                                        98
028700940615     C*
0288009406151-   C                   ENDSL
028900940615     C*
029000940615     C                   MOVE      *HIVAL        $LASTV
029100940615     C                   ENDSR
029200940225     C/EJECT
029300940225     C************************************************************
029400940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
029500940225     C************************************************************
029600940225     C     GESPIE        BEGSR
029700940225     C*
029800940225     C                   SELECT
029900940225     C*
030000940506     C     $GEST         WHENEQ    'D1'
030100940506     C     $ULKD1        IFEQ      0
030200940506     C* riempimento opzioni
030300940506     C                   EXSR      F24D1
030400940506     C                   ELSE
030500940614     C                   WRITE     Z1
030600940506     C                   ENDIF
030700940225     C*
030800940510     C*
030900940225     C                   ENDSL
031000940225     C*
031100940225     C                   ENDSR
031200940131     C/EJECT
031300940131     C************************************************************
031400940510     C* GESTIONE VIDEO RECORD D1
031500940131     C************************************************************
031600940127     C     GESD1         BEGSR
031700940225     C* inizializzazione videata
031800940223     C     $INZD1        IFEQ      *ON
031900940127     C                   EXSR      INZD1
032000940223     C                   MOVE      *OFF          $INZD1
032100940117     C                   END
032200940225     C* emissione piede videata se proveniente da altra
032300940225     C* salvataggio valore formato di provenienza
032400940207     C     $LASTV        IFNE      'D1'
032500940614     C                   WRITE     T1
032600940225     C                   EXSR      GESPIE
032700940202     C                   MOVE      $LASTV        $LASTG
032800940202     C                   MOVE      'D1'          $LASTV
032900940202     C                   END
033000940131     C*
033100940203     C                   SELECT
033200940203     C* Annullamento
033300940315    >C     OPZ002        WHENEQ    '04'
033400940224     C* Visualizzazione
033500940315    >C     OPZ002        OREQ      '05'
033600940620     C                   SETON                                        0203
033700940620     C* Manutenzione
033800940620    >C     OPZ002        WHENEQ    '02'
033900940620     C                   SETON                                        02
034000940620     C                   SETOFF                                       03
034100940620     C*
034200940620     C                   OTHER
034300940620     C*
034400940620     C                   SETOFF                                       0203
034500940203     C                   ENDSL
034600940620     C* Immissione/Modifica/Copia
034700940620     C                   EXFMT     D1
034800940506     C* controllo abilitazione tasti funzione
034900940506     C*
035000940506     C                   EXSR      CTRKD1
035100940506     C*
035200940506     C     *IN99         IFEQ      *OFF
035300940131     C*
0354009402031    C                   SELECT
035500940510     c* ROLLUP
035600940131     C* F3=Fine
035700940506     C     $TASTO        WHENEQ    F03
035800940309     C                   EXSR      F03D1
035900940131     C* F4=Ricerca
036000940506     C     $TASTO        WHENEQ    F04
036100940127     C                   EXSR      SEARCH
036200940224     C* F5=Refresh
036300940506     C     $TASTO        WHENEQ    F05
036400940309     C                   EXSR      F05D1
036500940223     C* F8=Successivo
036600940506     C     $TASTO        WHENEQ    F08
036700940309     C                   EXSR      F08D1
036800940131     C* F12=Ritorno
036900940506     C     $TASTO        WHENEQ    F12
037000940309     C                   EXSR      F12D1
037100940315     C* F24=Altri tasti
037200940506     C     $TASTO        WHENEQ    F24
037300940315     C                   EXSR      F24D1
037400940131     C*
0375009402031O   C                   OTHER
037600940127     C                   EXSR      CTRD1
037700940616     C     *IN99         IFEQ      *OFF
037800940616     C                   SELECT
037900940616     C* F7=Traduzione
038000940616     C     $TASTO        WHENEQ    F07
038100940616     C                   EXSR      LINGUA
038200940616     C* F6
0383009406162    C     $TASTO        WHENEQ    F06
038400940224     C* eseguo aggiornamento
038500940315    >C                   EXSR      AGGANA
038600940224     C* eseguo operazioni del dopo-aggiornamento
0387009403183    C     *IN99         IFEQ      *OFF
038800940318     C                   EXSR      GESAGG
0389009403183-   C                   ENDIF
0390009406162-   C                   ENDSL
039100940616     C                   ENDIF
039200940203     C*
0393009402241-   C                   ENDSL
039400940224     C*
0395009405062-   C                   ENDIF
039600940117     C                   ENDSR
039700940506     C/EJECT
039800940506     C************************************************************
039900940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
040000940506     C************************************************************
040100940506     C     CTRKD1        BEGSR
040200940506     C                   SETOFF                                       99
040300940506     C                   MOVEA     $FC1          $FC
040400940506     C                   EXSR      CTRKEY
040500940506     C     *IN99         IFEQ      *ON
040600940506     C                   SETON                                        97
040700940506     C                   ENDIF
040800981217     C* Xallinea x banxa
040900981217     C                   CALLb     'XALLINEA'
041000981217     C                   PARM                    §4Qban
041100981217     C                   PARM      4             LEN               5 0
041200981217     C                   PARM                    XSCINP
041300981217     C
041400940506     C                   ENDSR
041500940117      ************************************************************
041600940207      * INIZIALIZZAZIONE VIDEATA DATI
041700940117      ************************************************************
041800940127     C     INZD1         BEGSR
041900940131     C*
042000940615     C                   CLEAR                   D1
042100941006     C                   MOVE      '0'           D1RIS
042200940616     C                   MOVE      KEY002        D1COD
042300940615     C                   MOVE      AZI002        XTBAZI
042400940615     C                   MOVE      LIN002        XTBLIN
042500940616     C                   MOVE      *ALL'0'       XTBKEY
042600940615     C                   MOVE      KEY002        XTBKEY
042700970509     C                   MOVE      UNI002        XTBUNT
042800940615$007 C                   MOVE      'Y4Q'         XTBCOD
042900940225     C*
043000940207     C*
043100940224     C* valorizzo la chiave univoca per reperire i dati
043200940224     C* se non immissione
0433009403151   >C     OPZ002        IFNE      '01'
043400940315    >C*-------------------------------------------------------------
043500940315    >C* impostare i campi chiave ricevuti
043600940315    >C*-------------------------------------------------------------
043700940318     C*
043800940614     C*
043900940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0440009406142   >C     OPZ002        IFEQ      '02'
044100940315    >C     OPZ002        OREQ      '04'
044200940614     C                   MOVE      '1'           XTBALC
044300940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0444009406142E   C                   ELSE
044500940614     C                   MOVE      '0'           XTBALC
0446009406142-   C                   ENDIF
044700940614     C*
044800940616     C                   MOVE      '1'           XTBRIC
044900970214     C                   CALLB     'XATB'
045000940614     C                   PARM                    XATBDS
045100940318     C*
045200940614     C     XTBERR        IFEQ      '0'
045300940614     C                   MOVEL     XTBUNI        EXTDS
045400970512     c                   move      xtbunt        d1uniTA           8
045500941108     C* Giro la data se non blank
045600970214     c                   move      xtbdsc        xtbdscA          10
045700970214     C     XTBDSCa       IFNE      DATA
045800970214     C                   CALLB     'XDT4000'
045900970214     C                   PARM      XTBDSCA       XDTAMG           10
046000940621     C                   PARM      *ZEROS        XDTGMA            6 0
046100940621     C                   PARM      2             XDTSTA            1 0
046200940621     C                   MOVE      XDTGMA        D1DSC
046300940712     C                   MOVE      XTBRIS        D1RIS
046400941108     C                   ELSE
046500941108     C                   MOVE      *ZEROS        D1DSC
046600941108     C                   ENDIF
046700941108     C                   ENDIF
046800940614     C*
046900940224     C*
047000940224     C* se il rcd da gestire non viene trovato
047100940318     C* oppure è allocato da altro pgm
047200940224     C* ritorno al pgm guida avvertendolo dell'errore
0473009406142    C     XTBERR        IFNE      '0'
047400940224     C                   MOVE      *ON           $FINE
047500940315    >C                   MOVE      '1'           ERR002
047600940224     C*
047700940224     C* altrimenti
0478009402242E   C                   ELSE
047900940224     C* riempio la videata
048000940224     C                   EXSR      RIED1
048100940224     C* richiamo routine dei ctrl per decodificarla
048200940614    >C                   EXSR      CTRD1
048300960228     C                   MOVEL     *IN99         SAV99             1
048400960228     C* Se la gestione è più videate
048500960305    >C                   EXSR      CTRD2
048600960305    >C     *IN99         IFEQ      *ON
048700960305    >C                   MOVEL     *IN99         SAV99             1
048800960305    >C                   ENDIF
048900940224     C                   MOVE      *ALL'0'       IN5098
049000940224     C                   MOVEA     IN5098        *IN(50)
049100940224     C* definizione indicatori su video
049200940224     C                   SETOFF                                       02
0493009402243    C                   SELECT
049400940315    >C     OPZ002        WHENEQ    '02'
049500940224     C                   SETON                                        02
049600940315    >C     OPZ002        WHENEQ    '05'
049700940506     C* disabilita tasti
049800940720     C                   MOVE      '0'           $FC(6)
049900940720     C                   MOVE      '0'           $FC(5)
050000940720     C                   MOVE      '0'           $FC(4)
0501009402243-   C                   ENDSL
050200940224     C*
0503009402242-   C                   ENDIF
050400940223     C*
0505009402241-   C                   ENDIF
050600940224     C*
050700940117     C                   ENDSR
050800940207     C/EJECT
050900940207     C************************************************************
051000940510     C* RIEMPIMENTO VIDEATA  D1
051100940207     C************************************************************
051200940207     C     RIED1         BEGSR
051300940207     C*
051400940207     C*
051500940207     C                   ENDSR
051600940309     C/EJECT
051700940309     C************************************************************
051800940309     C* GESTIONE F03 VIDEO D1
051900940309     C************************************************************
052000940309     C     F03D1         BEGSR
052100940309     C*
052200940309     C                   MOVE      *ON           $FINE
052300940315    >C                   MOVE      '1'           RET002
052400940325     C* fine programma
052500940325     C                   EXSR      ENDPGM
052600940325     C*
052700940325     C                   ENDSR
052800940309     C/EJECT
052900940309     C************************************************************
053000940309     C* GESTIONE F05 VIDEO D1
053100940309     C************************************************************
053200940309     C     F05D1         BEGSR
053300940309     C*
053400940309     C                   MOVE      *ON           $INZD1
053500940309     C*
053600940309     C                   ENDSR
053700940309     C/EJECT
053800940309     C************************************************************
053900940309     C* GESTIONE F08 VIDEO D1
054000940309     C************************************************************
054100940309     C     F08D1         BEGSR
054200940309     C*
054300940309     C                   MOVE      *ON           $FINE
054400940315    >C                   MOVE      '0'           RET002
054500940309     C*
054600940309     C                   ENDSR
054700940309     C/EJECT
054800940309     C************************************************************
054900940309     C* GESTIONE F12 VIDEO D1
055000940309     C************************************************************
055100940309     C     F12D1         BEGSR
055200940309     C*
055300940309     C                   MOVE      *ON           $FINE
055400940315    >C                   MOVE      '2'           RET002
055500940309     C*
055600940309     C                   ENDSR
055700940309     C/EJECT
055800940309     C************************************************************
055900940309     C* GESTIONE F24 VIDEO D1
056000940309     C************************************************************
056100940309     C     F24D1         BEGSR
056200940309     C*
056300940506     C*
056400940506     C                   MOVEA     $FX1          $FX
056500940506     C                   MOVEA     $FC1          $FC
056600940506     C                   MOVEA     $FR1          $FR
056700940506     C                   Z-ADD     $ULKD1        $ULK
056800940506     C                   EXSR      RIEKEY
056900941017     C                   MOVEA     $FC           $FC1
057000940506     C                   Z-ADD     $ULK          $ULKD1            3 0
057100940506     C                   MOVEL     $KEY1         Z1KE1
057200940506     C                   MOVEL     $KEY2         Z1KE2
057300940614     C                   WRITE     Z1
057400940506     C*
057500940309     C                   ENDSR
057600940207     C/EJECT
057700940131     C************************************************************
057800940207     C* CONTROLLO VIDEATA
057900940131     C************************************************************
058000940127     C     CTRD1         BEGSR
058100940131     C*
058200940127     C                   SETOFF                                       99
058300940127     C* non imposto pos. del cursore
058400940127     C                   Z-ADD     0             H1RIGA
058500940127     C                   Z-ADD     0             H1COLO
058600940621     C*
058700940621     C* CTRL DATA
058800940621     C*
058900941109     C                   MOVE      DATA          WRKDSC           10
059000940621     C     D1DSC         IFNE      0
059100970214     C                   CALLB     'XDT4000'
059200941109     C                   PARM                    XDTAMG           10
059300940621     C                   PARM      D1DSC         XDTGMA            6 0
059400940621     C                   PARM      3             XDTSTA            1 0
059500940621     C     XDTSTA        IFNE      0
059600940621     C                   SETON                                        5599
059700940621     C                   ELSE
059800941108     C                   MOVE      XDTAMG        WRKDSC
059900940621     C                   ENDIF
060000940621     C                   ENDIF
060100940223     C*
060200940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
060300940223     C*
0604009403151   >C     OPZ002        IFNE      '04'
060500971110     C*
060600971110     C* controllo codice vuoto
060700971110$017 C     D1COD         IFEQ      *ALL' '
060800971110     C                   SETON                                        5099
060900971110     C                   ENDIF
061000971110     C*
061100940207     C*
061200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
061300940315    >C     OPZ002        IFEQ      '01'
061400940315    >C     OPZ002        OREQ      '03'
061500940315    >C*-------------------------------------------------------------
061600940315    >C* impostare i campi chiave e verificare l'esistenza del record
061700940315    >C*-------------------------------------------------------------
061800971110$007 C                   MOVE      'Y4Q'         XTBCOD
061900940614     C                   MOVE      *ALL'0'       XTBKEY
062000940620    >C                   MOVE      D1COD         XTBKEY
062100970512    >C                   MOVE      D1UNITA       XTBUNT
062200940614     C                   MOVE      '0'           XTBALC
062300940614     C                   MOVE      '1'           XTBRIC
062400970214    >C                   CALLB     'XATB'
062500940614     C                   PARM                    XATBDS
062600940614     C     XTBERR        IFEQ      '0'
062700981210     C***                SETON                                        5999
062800940223     C                   ENDIF
062900940620     C* Controllo se compreso nei range indice
063000940620     C     XTBERR        IFEQ      '3'
063100981210    >C***                SETON                                        5899
063200940620     C                   END
063300940620     C*
063400940620     C* REPERISCO AUTORIZZAZIONI UTENTE
063500940620     C                   CLEAR                   XCHATB
063600941118     C                   MOVE      XSCSOC        XCTSOC
063700941118     C                   MOVE      XSCDUN        XCTUNI
063800940620     C                   MOVEL     KNMUS         XCTPRF
063900940620     C                   MOVE      XTBCOD        XCTCOD
064000940620     C                   CALL      'XCHKATB'
064100940620     C                   PARM                    XCHATB
064200941111     C                   MOVEL     XCTDDS        AUTDS
064300941027     C     XCTRTN        IFEQ      1
064400941027     C                   MOVEL     'N'           MANGRU
064500941027     C                   ENDIF
064600940620     C* Non ammessa immissione di gruppo
064700940620     C     XTBGRU        IFEQ      'G'
064800940620     C     MANGRU        ANDEQ     'N'
064900981207     C***                SETON                                        5799
065000940620     C                   END
065100940620     C* Non ammessa immissione di azienda
065200940620     C     XTBGRU        IFEQ      'A'
065300940620     C     MANAZI        ANDEQ     'N'
065400981207     C***                SETON                                        5699
065500940620     C                   END
065600940712     C* codice riservato non accessibile
065700941006     C     XTBRIS        IFGT      ABIRIS
065800941005    >C     OPZ002        ANDNE     '01'
065900941005    >C     OPZ002        ANDNE     '03'
066000981207     C***                SETON                                        5499
066100940712     C                   END
066200940224     C                   ENDIF
066300940224     C*
0664009402241-   C                   END
066500940131     C*
066600940302     C     *IN99         IFEQ      *ON
066700940510     C     $GEST         ANDEQ     'D1'
066800960228     C     OPZ002        ANDNE     '07'
066900940117     C                   MOVEA     *IN           WIN
067000940128     C                   MOVE      *ALL'0'       IN5098           49
067100940128     C                   MOVEA     IN5098        *IN(50)
067200940614     C                   WRITE     D1
067300940117     C                   MOVEA     WIN           *IN
067400940302     C                   ENDIF
067500940127     C*
067600940117     C                   ENDSR
067700960305     C/EJECT
067800960305     C************************************************************
067900960305     C* CONTROLLO VIDEATA D2
068000960305     C************************************************************
068100960305     C     CTRD2         BEGSR
068200960305     C*
068300960305     C                   ENDSR
068400940207     C/EJECT
068500940224     C************************************************************
068600940315    >C* AGGIORNAMENTO ANAGRAFICA
068700940224     C************************************************************
068800940315    >C     AGGANA        BEGSR
068900971110     C*
069000971110$007 C                   MOVE      'Y4Q'         XTBCOD
069100940614     C                   MOVE      *ALL'0'       XTBKEY
069200940614     C                   MOVE      D1COD         XTBKEY
069300970512     C                   MOVE      D1UNITA       XTBUNT
069400940207     C*
069500940207     C                   SELECT
069600940315    >C     OPZ002        WHENEQ    '01'
069700940315    >C     OPZ002        OREQ      '03'
069800940315    >C                   EXSR      RIEANA
069900950609     C                   MOVE      XSCCUT        XTBUTE
070000940614     C                   MOVE      '5'           XTBRIC
070100970214     C                   CALLB     'XATB'
070200940614    >C                   PARM                    XATBDS
070300940318     C* se rcd non scrivibile attivo errore
070400940615     C     XTBERR        IFEQ      '1'
070500981210    >C***                SETON                                        5999
070600940318     C                   END
070700940315    >C     OPZ002        WHENEQ    '02'
070800940621     C                   MOVE      '1'           XTBRIC
070900940621     C                   MOVE      '1'           XTBALC
071000970214     C                   CALLB     'XATB'
071100940621     C                   PARM                    XATBDS
071200940622    >C                   EXSR      RIEANA
071300950609     C                   MOVE      XSCCUT        XTBUTE
071400940614     C                   MOVE      '4'           XTBRIC
071500970214     C                   CALLB     'XATB'
071600940614    >C                   PARM                    XATBDS
071700940315    >C     OPZ002        WHENEQ    '04'
071800940621     C                   MOVE      '1'           XTBRIC
071900940621     C                   MOVE      '1'           XTBALC
072000970214     C                   CALLB     'XATB'
072100940621     C                   PARM                    XATBDS
072200940614     C                   MOVE      '6'           XTBRIC
072300970214     C                   CALLB     'XATB'
072400940614    >C                   PARM                    XATBDS
072500940621     C* annullo traduzioni
072600940621     C     XTBERR        IFEQ      '0'
072700940621     C                   EXSR      ANNTBT
072800940621     C                   END
072900940621     C*
073000940207     C                   ENDSL
073100940203     C*
073200940203     C                   ENDSR
073300940224     C/EJECT
073400940224     C************************************************************
073500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
073600940224     C************************************************************
073700940616     C     RIEANA        BEGSR
073800940616     C                   CLEAR                   XTBUNI
073900940621     C                   MOVE      WRKDSC        XTBDSC
074000940712     C                   MOVE      D1RIS         XTBRIS
074100940616     C                   MOVEL     EXTDS         XTBUNI
074200940207     C                   ENDSR
074300940224     C/EJECT
074400940224     C************************************************************
074500940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
074600940224     C************************************************************
074700940224     C     GESAGG        BEGSR
074800940224     C*
074900940224     C* segnalo al pgm chiamante l'avvenuta conferma
075000940315    >C                   MOVE      *ON           OPR002
075100940224     C* segnalo al pgm chiamante l'aver premuto l'F6
075200940315    >C                   MOVE      '0'           RET002
075300940224     C*
0754009402241    C                   SELECT
075500940224     C* nel caso di immissione, ripristino la videata iniziale
075600940315    >C     OPZ002        WHENEQ    '01'
075700940224     C                   MOVE      *ON           $INZD1
075800940224     C* altrimenti ritorno al pgm chiamante
075900940224     C                   OTHER
076000940224     C                   MOVE      *ON           $FINE
0761009402241-   C                   ENDSL
076200940224     C*
076300940224     C                   ENDSR
076400940621     C/EJECT
076500940621     C************************************************************
076600940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
076700940621     C************************************************************
076800940621    >C     ANNTBT        BEGSR
076900940621     C                   MOVE      XTBAZI        TBTAZI
077000940621     C                   MOVE      XTBCOD        TBTCOD
077100940621     C                   MOVE      XTBKEY        TBTKEY
077200940621     C     KTBT          SETLL     ANTBT01L
077300940621     C                   DO        *HIVAL
077400940621     C     KTBT          DELETE    ANTBT01L                           21
077500940621     C  N21              END
077600940621     C*
077700940621     C                   ENDSR
077800940207     C/EJECT
077900940131     C************************************************************
078000940131     C* OPERAZIONI INIZIALI
078100940131     C************************************************************
078200940131     C     *INZSR        BEGSR
078300940131     C*
078400940127     C* Reperimento parametri
078500940127     C*
078600940117     C     *ENTRY        PLIST
078700940117     C                   PARM                    KPJBA
078800940712     C*
078900940712     C* Reperimento Codice utente
079000940712     C*
079100940223     C*
079200940506     C*
079300940506     C* Reperimento tasti di funzione
079400940506     C*
079500940506     C                   EXSR      REPKEY
079600940621     C*
079700940621     C* KLIST
079800940621     C*
079900940621     C     KTBT          KLIST
080000940621     C                   KFLD                    TBTAZI
080100940621     C                   KFLD                    TBTCOD
080200940621     C                   KFLD                    TBTKEY
080300940506     C*
080400940223     C                   ENDSR
080500941103     C/EJECT
080600941103      ************************************************************
080700941103      * Reperimento dati società
080800941103      ************************************************************
080900941103     C     REPSOC        BEGSR
081000941103      *
081100970214     C                   CALLB     'XSOC'
081200941103     C                   PARM                    TIPXSC            6
081300941103     C                   PARM                    SOCXSC            3
081400941103     C                   PARM                    CDSXSC            9 0
081500941103     C                   PARM                    MODXSC            3
081600941103     C                   PARM      *BLANKS       RTNXSC            1
081700941103     C                   PARM                    XSOCDS
081800941103     C                   PARM                    KPJBA
081900941103      *
082000941103     C                   ENDSR
082100940223     C/EJECT
082200940506     C************************************************************
082300940506     C* REPERIMENTO TASTI DI FUNZIONE
082400940506     C************************************************************
082500940506     C     REPKEY        BEGSR
082600940506     C                   MOVEA     $FM1          $FM
082700940506     C                   EXSR      REPKKK
082800940506     C                   MOVEA     $FX           $FX1
082900940614     C                   ENDSR
083000940506     C/EJECT
083100940506     C************************************************************
083200940506     C* REPERIMENTO TASTI DI FUNZIONE
083300940506     C************************************************************
083400940506     C     REPKKK        BEGSR
083500940506     C*
083600940506     C                   CLEAR                   $FX
083700940506     C     1             DO        24            X
083800940506     C*
083900940506     C     $FM(X)        IFNE      *BLANK
084000940506     C*
084100940506     C                   MOVEL     $FM(X)        IDMSG
084200970214     C                   CALLB     'XRTVM'
084300940506     C                   PARM                    IDMSG            27
084400940506     C                   PARM                    TXTMSG          100
084500940506     C                   PARM                    ERRMSG            1
084600940506     C*
084700940506     C     ERRMSG        IFNE      *ON
084800940506     C                   MOVEL     TXTMSG        $FX(X)
084900940506     C                   ELSE
085000940506     C                   MOVEL     *ALL' '       $FX(X)
085100940506     C                   END
085200940506     C*
085300940506     C                   END
085400940506     C*
085500940506     C                   END
085600940506     C*
085700940506     C                   ENDSR
085800940509     C/EJECT
085900940223     C************************************************************
086000940223     C* INIZIALIZZAZIONE VARIABILI
086100940223     C************************************************************
086200940223     C     INZVAR        BEGSR
086300951124     C*
086400951124     C* Chiamata XSOC
086500951124     C*
086600951124     C                   MOVEL     'SOC001'      TIPXSC
086700951124     C                   EXSR      REPSOC
086800951124     C     RTNXSC        IFNE      '1'
086900951124     C                   MOVEL     XSOCDS        SOC001
087000951124     C                   ELSE
087100951124     C                   MOVE      *ON           $FINE
087200951124     C* fine programma
087300951124     C                   EXSR      ENDPGM
087400951124     C                   ENDIF
087500940224     C*
087600940224     C* Dati da PGM guida
087700940224     C*
087800940315    >C                   MOVEL     KPJBU         D002DS
087900940128     C*
088000940223     C* Variabili per gestione videate
088100940128     C*
088200940223     C                   MOVE      'D1'          $GEST
088300940223     C                   MOVE      *OFF          $FINE
088400940223     C                   MOVE      *ON           $INZD1
088500940224     C                   MOVE      *BLANK        $LASTG
088600940224     C                   MOVE      *BLANK        $LASTV
088700940506     C                   Z-ADD     0             $ULKD1            3 0
088800940127     C*
088900940223     C* Variabili appoggio
089000940127     C*
089100940224     C                   Z-ADD     0             CURR
089200940224     C                   Z-ADD     0             CURC
089300940207     C*
089400940207     C* Valorizzazione campi univoci testate
089500940207     C*
089600940614     C                   CLEAR                   T1
089700940614     C                   MOVEL     KNSIF         NOMSIF
089800941109     C                   MOVEL     XSCDSI        NOMDIT
089900941027     C                   MOVEL     DSPGM         NOMPGM
090000940224     C                   Z-ADD     1             X
090100940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
090200940224     C     *IN21         IFEQ      *ON
090300940224     C                   MOVEL     $ID(X)        IDMSG
090400970214     C                   CALLB     'XRTVM'
090500940224     C                   PARM                    IDMSG            27
090600940224     C                   PARM                    TXTMSG          100
090700940224     C                   PARM                    ERRMSG            1
090800940224     C                   PARM      *ON           CTRMSG            1
090900940224     C                   PARM      30            LENMSG            3 0
091000940224     C     ERRMSG        IFNE      *ON
091100940224     C                   MOVEL     TXTMSG        T1OPE
091200940224     C                   ELSE
091300940224     C                   MOVEL     *ALL'?'       T1OPE
091400940224     C                   ENDIF
091500940224     C                   END
091600940224     C*
091700940224     C* Inizializzazione/Decodifica prima videata
091800940224     C*
091900940224     C                   EXSR      INZD1
092000940224     C                   MOVE      *OFF          $INZD1
092100960228     C* Se opzione = '07' richiesti solo controlli
092200960228     C     OPZ002        IFEQ      '07'
092300960228     C                   MOVEL     *IN99         ERR002
092400960228     C                   MOVEL     '0'           RET002
092500960228     C                   MOVEL     *ON           $FINE
092600960228     C                   ENDIF
092700940127     C*
092800940117     C                   ENDSR
092900940128     C************************************************************
093000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
093100940223     C************************************************************
093200940223     C     DEFCAM        BEGSR
093300940223     C*
093400940223     C* klist
093500940207     C*
093600940223     C*
093700940223     C* Variabili per gestione videate
093800940223     C*
093900940223     C                   MOVE      *BLANK        $GEST             2
094000940223     C                   MOVE      *BLANK        $FINE             1
094100940223     C                   MOVE      *BLANK        $INZD1            1
094200940223     C                   MOVE      *BLANK        $LASTG            2
094300940223     C                   MOVE      *BLANK        $LASTV            2
094400940223     C*
094500940223     C* Variabili appoggio
094600940223     C*
094700940223     C                   Z-ADD     0             CURR              2 0
094800940223     C                   Z-ADD     0             CURC              2 0
094900940223     C                   MOVE      *ZEROS        WIN              99
095000940224     C                   MOVE      *BLANK        WTBERR            1
095100940223     C*
095200940223     C* Indici
095300940223     C*
095400940223     C                   Z-ADD     0             X                 3 0
095500940223     C                   Z-ADD     0             Y                 3 0
095600940127     C*
095700940117     C                   ENDSR
095800940128     C************************************************************
095900940509     C/EJECT
096000940509     C************************************************************
096100940509     C* CONTROLLO ABILITAZIONE TASTI
096200940509     C************************************************************
096300940509     C     CTRKEY        BEGSR
096400940509     C                   SELECT
096500940509     C     $TASTO        WHENEQ    F01
096600940720     C     $FC(1)        IFEQ      '0'
096700940509     C                   SETON                                        99
096800940509     C                   ENDIF
096900940509     C     $TASTO        WHENEQ    F02
097000940720     C     $FC(2)        IFEQ      '0'
097100940509     C                   SETON                                        99
097200940509     C                   ENDIF
097300940509     C     $TASTO        WHENEQ    F03
097400940720     C     $FC(3)        IFEQ      '0'
097500940509     C                   SETON                                        99
097600940509     C                   ENDIF
097700940509     C     $TASTO        WHENEQ    F04
097800940720     C     $FC(4)        IFEQ      '0'
097900940509     C                   SETON                                        99
098000940509     C                   ENDIF
098100940509     C     $TASTO        WHENEQ    F05
098200940720     C     $FC(5)        IFEQ      '0'
098300940509     C                   SETON                                        99
098400940509     C                   ENDIF
098500940509     C     $TASTO        WHENEQ    F06
098600940720     C     $FC(6)        IFEQ      '0'
098700940509     C                   SETON                                        99
098800940509     C                   ENDIF
098900940509     C     $TASTO        WHENEQ    F07
099000940720     C     $FC(7)        IFEQ      '0'
099100940509     C                   SETON                                        99
099200940509     C                   ENDIF
099300940509     C     $TASTO        WHENEQ    F08
099400940720     C     $FC(8)        IFEQ      '0'
099500940509     C                   SETON                                        99
099600940509     C                   ENDIF
099700940509     C     $TASTO        WHENEQ    F09
099800940720     C     $FC(09)       IFEQ      '0'
099900940509     C                   SETON                                        99
100000940509     C                   ENDIF
100100940509     C     $TASTO        WHENEQ    F10
100200940720     C     $FC(10)       IFEQ      '0'
100300940509     C                   SETON                                        99
100400940509     C                   ENDIF
100500940509     C     $TASTO        WHENEQ    F11
100600940720     C     $FC(11)       IFEQ      '0'
100700940509     C                   SETON                                        99
100800940509     C                   ENDIF
100900940509     C     $TASTO        WHENEQ    F12
101000940720     C     $FC(12)       IFEQ      '0'
101100940509     C                   SETON                                        99
101200940509     C                   ENDIF
101300940509     C     $TASTO        WHENEQ    F13
101400940720     C     $FC(13)       IFEQ      '0'
101500940509     C                   SETON                                        99
101600940509     C                   ENDIF
101700940509     C     $TASTO        WHENEQ    F14
101800940720     C     $FC(14)       IFEQ      '0'
101900940509     C                   SETON                                        99
102000940509     C                   ENDIF
102100940509     C     $TASTO        WHENEQ    F15
102200940720     C     $FC(15)       IFEQ      '0'
102300940509     C                   SETON                                        99
102400940509     C                   ENDIF
102500940509     C     $TASTO        WHENEQ    F16
102600940720     C     $FC(16)       IFEQ      '0'
102700940509     C                   SETON                                        99
102800940509     C                   ENDIF
102900940509     C     $TASTO        WHENEQ    F17
103000940720     C     $FC(17)       IFEQ      '0'
103100940509     C                   SETON                                        99
103200940509     C                   ENDIF
103300940509     C     $TASTO        WHENEQ    F18
103400940720     C     $FC(18)       IFEQ      '0'
103500940509     C                   SETON                                        99
103600940509     C                   ENDIF
103700940509     C     $TASTO        WHENEQ    F19
103800940720     C     $FC(19)       IFEQ      '0'
103900940509     C                   SETON                                        99
104000940509     C                   ENDIF
104100940509     C     $TASTO        WHENEQ    F20
104200940720     C     $FC(20)       IFEQ      '0'
104300940509     C                   SETON                                        99
104400940509     C                   ENDIF
104500940509     C     $TASTO        WHENEQ    F21
104600940720     C     $FC(21)       IFEQ      '0'
104700940509     C                   SETON                                        99
104800940509     C                   ENDIF
104900940509     C     $TASTO        WHENEQ    F22
105000940720     C     $FC(22)       IFEQ      '0'
105100940509     C                   SETON                                        99
105200940509     C                   ENDIF
105300940509     C     $TASTO        WHENEQ    F23
105400940720     C     $FC(23)       IFEQ      '0'
105500940509     C                   SETON                                        99
105600940509     C                   ENDIF
105700940509     C     $TASTO        WHENEQ    F24
105800940720     C     $FC(24)       IFEQ      '0'
105900940509     C                   SETON                                        99
106000940509     C                   ENDIF
106100940509     C                   ENDSL
106200940509     C                   ENDSR
106300940509     C/EJECT
106400940509     C************************************************************
106500940509     C* RIEMPIMENTO TASTI FUNZIONE
106600940509     C************************************************************
106700940509     C     RIEKEY        BEGSR
106800940509     C*
106900940509     C* Input :
107000940509     C* - $FX = schiera con tutti i testi disponibili
107100940509     C* - $FC = schiere con i flag di validità associati ai testi
107200940509     C* - $FR = schiere con i flag di REVERSE IMAGE
107300940509     C* - $ULK = ultimo testo caricato
107400940509     C* Output :
107500940509     C* - $ULK
107600940509     c* - $KEY1= 1° riga di tasti funzione
107700940509     c* - $KEY2= 2° riga di tasti funzione
107800940509     C*
107900970214     C                   CALLB     'XKEYMSG'
108000940509     C* input
108100940509     C                   PARM                    $FX
108200940509     C                   PARM                    $FC
108300940509     C                   PARM                    $FR
108400940509     C                   PARM                    $ULK              3 0
108500940509     C* output
108600940509     C                   PARM                    $KEY1            79
108700940509     C                   PARM                    $KEY2            79
108800940509     C*
108900940509     C                   ENDSR
109000940615**
10910094022401PROMSG    *LIBL     COS0001    immissione
10920094022402PROMSG    *LIBL     COS0002    modifica
10930094022403PROMSG    *LIBL     COS0003    copia
10940094022404PROMSG    *LIBL     COS0004    annullamento
10950094022405PROMSG    *LIBL     COS0005    visualizzazione
109600940509** TASTI DI FUNZIONE  D1
109700940509PROMSG    *LIBL     KEY0001  01
109800940509PROMSG    *LIBL     KEY0002  02
109900940509PROMSG    *LIBL     KEY0003  03
110000940509PROMSG    *LIBL     KEY0004  04
110100940509PROMSG    *LIBL     KEY0005  05
110200940509PROMSG    *LIBL     KEY0006  06
110300940509PROMSG    *LIBL     KEY0007  07
110400940509PROMSG    *LIBL     KEY0008  08
110500940509PROMSG    *LIBL     KEY0009  09
110600940509PROMSG    *LIBL     KEY0010  10
110700940509PROMSG    *LIBL     KEY0011  11
110800940509PROMSG    *LIBL     KEY0012  12
110900940509PROMSG    *LIBL     KEY0013  13
111000940509PROMSG    *LIBL     KEY0014  14
111100940509PROMSG    *LIBL     KEY0015  15
111200940509PROMSG    *LIBL     KEY0016  16
111300940509PROMSG    *LIBL     KEY0017  17
111400940509PROMSG    *LIBL     KEY0018  18
111500940509PROMSG    *LIBL     KEY0019  19
111600940509PROMSG    *LIBL     KEY0020  20
111700940509PROMSG    *LIBL     KEY0021  21
111800940509PROMSG    *LIBL     KEY0022  22
111900940509PROMSG    *LIBL     KEY0023  23
112000940509PROMSG    *LIBL     KEY0024  24
112100940509**  ABILITAZIONE D1
1122009407200N  01
1123009407200N  02
1124009407201N  03
1125009407201N  04
1126009407201N  05
1127009407201N  06
1128009407201N  07
1129009407201N  08
1130009407200N  09
1131009407200N  10
1132009407200N  11
1133009407201N  12
1134009407200N  13
1135009407200N  14
1136009407200N  15
1137009407200N  16
1138009407200N  17
1139009407200N  18
1140009407200N  19
1141009407200N  20
1142009407200N  21
1143009407200N  22
1144009407200N  23
1145009407200N  24
