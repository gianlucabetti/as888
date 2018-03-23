000100021015     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
000200021015     H CVTOPT(*DATETIME)
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
004100990215$001 FANGYP4D2  CF   E             WORKSTN
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
006000990215$003 D EXTDS         E DS                  EXTNAME(ANGYP4DS) INZ
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
017500990215$005 I              D1KCD                       §P4KCD
017600990215$005 I              D1KSD                       §P4KSD
017700990215$005 I              D1KCA                       §P4KCA
017800990215$005 I              D1KSA                       §P4KSA
017900990216$005 I              D1VCD                       §P4VCD
018000990216$005 I              D1VCA                       §P4VCA
018100990215$005 I              D1CDD                       §P4CDD
018200990215$005 I              D1CDA                       §P4CDA
018300990215$005 I              D1CAD                       §P4CAD
018400990215$005 I              D1CAA                       §P4CAA
018500990215$015 I              D1TDD                       §P4TDD
018600990215$015 I              D1TDA                       §P4TDA
018700940127     I*
018800940127     C*----------------------------------------------------*
018900940127     C*                MAIN LINE PROGRAM
019000940127     C*----------------------------------------------------*
019100940127     C*
019200940223     C* inizializzazione variabili
019300940223     C                   EXSR      INZVAR
019400940223     C*
019500940223     C     $FINE         DOWEQ     *OFF
019600940202     C     $GEST         CASEQ     'D1'          GESD1
019700940117     C                   END
019800940117     C                   END
019900940201     C*
020000940325     C* fine programma
020100940325     C                   EXSR      ENDPGM
020200940325     C/EJECT
020300940325     C************************************************************
020400940325     C* RICERCHE
020500940325     C************************************************************
020600940325     C     ENDPGM        BEGSR
020700940325     C*
020800940614     C* disallocazione rcd nel caso di
020900971110$007 C                   MOVE      'YP4'         XTBCOD
021000940614    >C                   MOVE      '7'           XTBRIC
021100970214     C                   CALLB     'XATB'
021200940614     C                   PARM                    XATBDS
021300940325     C*
021400940325     C* passaggio dati al pgm chiamante
021500940715     C                   MOVE      *BLANKS       KPJBU
021600940325    >C                   MOVEL     D002DS        KPJBU
021700940325     C*
021800940325     C                   SETON                                            RT
021900940325     C*
022000940325     C                   ENDSR
022100940325     C/EJECT
022200940201     C************************************************************
022300940201     C* RICERCHE
022400940201     C************************************************************
022500940127     C     SEARCH        BEGSR
022600940127     C*
022700940127     C* determino Riga/Colonna del cursore
022800940127     C                   MOVE      NRG           R$$
022900940127     C                   MOVE      NCL           C$$
023000940127     C                   Z-ADD     RIRI          CURR
023100940127     C                   Z-ADD     COCO          CURC
023200940127     C*
023300940224     C* I campi che al momento dell'F4 risultano protetti non devono
023400940224     C* essere abilitati alla funzione di ricerca
023500940224     C*
0236009401271    C                   SELECT
023700940127     C*
023800940315     C* FMTD1  - PACCHETTO
023900940614    >C*          H1NMFL    WHEQ 'D1PKG'
024000940207     C* imposto pos. del cursore
024100940614     C*                    Z-ADDCURR      H1RIGA
024200940614     C*                    Z-ADDCURC      H1COLO
024300940127     C*
024400940223     C*
024500940223     C* ricerca non abilitata in questa posizione
0246009402231O   C                   OTHER
024700941005     C                   SETON                                        9899
024800941005     C* imposto pos. del cursore
024900941005     C                   Z-ADD     CURR          H1RIGA
025000941005     C                   Z-ADD     CURC          H1COLO
025100940127     C*
0252009401271-   C                   ENDSL
025300940127     C*
025400940506     C                   MOVE      *HIVAL        $LASTV
025500940127     C                   ENDSR
025600940615     C/EJECT
025700940615     C************************************************************
025800940615     C* TRADUZIONI
025900940615     C************************************************************
026000940615     C     LINGUA        BEGSR
026100940615     C*
026200940615     C* determino Riga/Colonna del cursore
026300940615     C                   MOVE      NRG           R$$
026400940615     C                   MOVE      NCL           C$$
026500940615     C                   Z-ADD     RIRI          CURR
026600940615     C                   Z-ADD     COCO          CURC
026700940615     C*
026800940615     C*
026900940615     C*
027000940615     C* FMTD1  - CAMPI DA TRADURRE
027100940615     C* imposto pos. del cursore
027200940615     C                   Z-ADD     CURR          H1RIGA
027300940615     C                   Z-ADD     CURC          H1COLO
027400940621     C                   MOVEL     H1NMFL        W005              5
027500940621     C                   MOVE      W005          W003              3
027600940621     C                   MOVEL     W003          W006              6
027700940620     C                   MOVE      *ZERO         KEY              15
027800940616     C                   MOVE      D1COD         KEY
027900940615     C                   SELECT
028000940615     C*
028100940615     C*
028200940615     C* ricerca non abilitata in questa posizione
0283009406151O   C                   OTHER
028400940615     C                   SETON                                        98
028500940615     C*
0286009406151-   C                   ENDSL
028700940615     C*
028800940615     C                   MOVE      *HIVAL        $LASTV
028900940615     C                   ENDSR
029000940225     C/EJECT
029100940225     C************************************************************
029200940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
029300940225     C************************************************************
029400940225     C     GESPIE        BEGSR
029500940225     C*
029600940225     C                   SELECT
029700940225     C*
029800940506     C     $GEST         WHENEQ    'D1'
029900940506     C     $ULKD1        IFEQ      0
030000940506     C* riempimento opzioni
030100940506     C                   EXSR      F24D1
030200940506     C                   ELSE
030300940614     C                   WRITE     Z1
030400940506     C                   ENDIF
030500940225     C*
030600940510     C*
030700940225     C                   ENDSL
030800940225     C*
030900940225     C                   ENDSR
031000940131     C/EJECT
031100940131     C************************************************************
031200940510     C* GESTIONE VIDEO RECORD D1
031300940131     C************************************************************
031400940127     C     GESD1         BEGSR
031500940225     C* inizializzazione videata
031600940223     C     $INZD1        IFEQ      *ON
031700940127     C                   EXSR      INZD1
031800940223     C                   MOVE      *OFF          $INZD1
031900940117     C                   END
032000940225     C* emissione piede videata se proveniente da altra
032100940225     C* salvataggio valore formato di provenienza
032200940207     C     $LASTV        IFNE      'D1'
032300940614     C                   WRITE     T1
032400940225     C                   EXSR      GESPIE
032500940202     C                   MOVE      $LASTV        $LASTG
032600940202     C                   MOVE      'D1'          $LASTV
032700940202     C                   END
032800940131     C*
032900940203     C                   SELECT
033000940203     C* Annullamento
033100940315    >C     OPZ002        WHENEQ    '04'
033200940224     C* Visualizzazione
033300940315    >C     OPZ002        OREQ      '05'
033400940620     C                   SETON                                        0203
033500940620     C* Manutenzione
033600940620    >C     OPZ002        WHENEQ    '02'
033700940620     C                   SETON                                        02
033800940620     C                   SETOFF                                       03
033900940620     C*
034000940620     C                   OTHER
034100940620     C*
034200940620     C                   SETOFF                                       0203
034300940203     C                   ENDSL
034400940620     C* Immissione/Modifica/Copia
034500940620     C                   EXFMT     D1
034600940506     C* controllo abilitazione tasti funzione
034700940506     C*
034800940506     C                   EXSR      CTRKD1
034900940506     C*
035000940506     C     *IN99         IFEQ      *OFF
035100940131     C*
0352009402031    C                   SELECT
035300940510     c* ROLLUP
035400940131     C* F3=Fine
035500940506     C     $TASTO        WHENEQ    F03
035600940309     C                   EXSR      F03D1
035700940131     C* F4=Ricerca
035800940506     C     $TASTO        WHENEQ    F04
035900940127     C                   EXSR      SEARCH
036000940224     C* F5=Refresh
036100940506     C     $TASTO        WHENEQ    F05
036200940309     C                   EXSR      F05D1
036300940223     C* F8=Successivo
036400940506     C     $TASTO        WHENEQ    F08
036500940309     C                   EXSR      F08D1
036600940131     C* F12=Ritorno
036700940506     C     $TASTO        WHENEQ    F12
036800940309     C                   EXSR      F12D1
036900940315     C* F24=Altri tasti
037000940506     C     $TASTO        WHENEQ    F24
037100940315     C                   EXSR      F24D1
037200940131     C*
0373009402031O   C                   OTHER
037400940127     C                   EXSR      CTRD1
037500940616     C     *IN99         IFEQ      *OFF
037600940616     C                   SELECT
037700940616     C* F7=Traduzione
037800940616     C     $TASTO        WHENEQ    F07
037900940616     C                   EXSR      LINGUA
038000940616     C* F6
0381009406162    C     $TASTO        WHENEQ    F06
038200940224     C* eseguo aggiornamento
038300940315    >C                   EXSR      AGGANA
038400940224     C* eseguo operazioni del dopo-aggiornamento
0385009403183    C     *IN99         IFEQ      *OFF
038600940318     C                   EXSR      GESAGG
0387009403183-   C                   ENDIF
0388009406162-   C                   ENDSL
038900940616     C                   ENDIF
039000940203     C*
0391009402241-   C                   ENDSL
039200940224     C*
0393009405062-   C                   ENDIF
039400940117     C                   ENDSR
039500940506     C/EJECT
039600940506     C************************************************************
039700940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
039800940506     C************************************************************
039900940506     C     CTRKD1        BEGSR
040000940506     C                   SETOFF                                       99
040100940506     C                   MOVEA     $FC1          $FC
040200940506     C                   EXSR      CTRKEY
040300940506     C     *IN99         IFEQ      *ON
040400940506     C                   SETON                                        97
040500940506     C                   ENDIF
040600940506     C                   ENDSR
040700940117      ************************************************************
040800940207      * INIZIALIZZAZIONE VIDEATA DATI
040900940117      ************************************************************
041000940127     C     INZD1         BEGSR
041100940131     C*
041200940615     C                   CLEAR                   D1
041300941006     C                   MOVE      '0'           D1RIS
041400940616     C                   MOVE      KEY002        D1COD
041500940615     C                   MOVE      AZI002        XTBAZI
041600940615     C                   MOVE      LIN002        XTBLIN
041700940616     C                   MOVE      *ALL'0'       XTBKEY
041800940615     C                   MOVE      KEY002        XTBKEY
041900970509     C                   MOVE      UNI002        XTBUNT
042000940615$007 C                   MOVE      'YP4'         XTBCOD
042100940225     C*
042200940207     C*
042300940224     C* valorizzo la chiave univoca per reperire i dati
042400940224     C* se non immissione
0425009403151   >C     OPZ002        IFNE      '01'
042600940315    >C*-------------------------------------------------------------
042700940315    >C* impostare i campi chiave ricevuti
042800940315    >C*-------------------------------------------------------------
042900940318     C*
043000940614     C*
043100940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0432009406142   >C     OPZ002        IFEQ      '02'
043300940315    >C     OPZ002        OREQ      '04'
043400940614     C                   MOVE      '1'           XTBALC
043500940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0436009406142E   C                   ELSE
043700940614     C                   MOVE      '0'           XTBALC
0438009406142-   C                   ENDIF
043900940614     C*
044000940616     C                   MOVE      '1'           XTBRIC
044100970214     C                   CALLB     'XATB'
044200940614     C                   PARM                    XATBDS
044300940318     C*
044400940614     C     XTBERR        IFEQ      '0'
044500940614     C                   MOVEL     XTBUNI        EXTDS
044600970512     c                   move      xtbunt        d1uniTA           8
044700941108     C* Giro la data se non blank
044800970214     c                   move      xtbdsc        xtbdscA          10
044900970214     C     XTBDSCa       IFNE      DATA
045000970214     C                   CALLB     'XDT4000'
045100970214     C                   PARM      XTBDSCA       XDTAMG           10
045200940621     C                   PARM      *ZEROS        XDTGMA            6 0
045300940621     C                   PARM      2             XDTSTA            1 0
045400940621     C                   MOVE      XDTGMA        D1DSC
045500940712     C                   MOVE      XTBRIS        D1RIS
045600941108     C                   ELSE
045700941108     C                   MOVE      *ZEROS        D1DSC
045800941108     C                   ENDIF
045900941108     C                   ENDIF
046000940614     C*
046100940224     C*
046200940224     C* se il rcd da gestire non viene trovato
046300940318     C* oppure è allocato da altro pgm
046400940224     C* ritorno al pgm guida avvertendolo dell'errore
0465009406142    C     XTBERR        IFNE      '0'
046600940224     C                   MOVE      *ON           $FINE
046700940315    >C                   MOVE      '1'           ERR002
046800940224     C*
046900940224     C* altrimenti
0470009402242E   C                   ELSE
047100940224     C* riempio la videata
047200940224     C                   EXSR      RIED1
047300940224     C* richiamo routine dei ctrl per decodificarla
047400940614    >C                   EXSR      CTRD1
047500960228     C                   MOVEL     *IN99         SAV99             1
047600960228     C* Se la gestione è più videate
047700960305    >C                   EXSR      CTRD2
047800960305    >C     *IN99         IFEQ      *ON
047900960305    >C                   MOVEL     *IN99         SAV99             1
048000960305    >C                   ENDIF
048100940224     C                   MOVE      *ALL'0'       IN5098
048200940224     C                   MOVEA     IN5098        *IN(50)
048300940224     C* definizione indicatori su video
048400940224     C                   SETOFF                                       02
0485009402243    C                   SELECT
048600940315    >C     OPZ002        WHENEQ    '02'
048700940224     C                   SETON                                        02
048800940315    >C     OPZ002        WHENEQ    '05'
048900940506     C* disabilita tasti
049000940720     C                   MOVE      '0'           $FC(6)
049100940720     C                   MOVE      '0'           $FC(5)
049200940720     C                   MOVE      '0'           $FC(4)
0493009402243-   C                   ENDSL
049400940224     C*
0495009402242-   C                   ENDIF
049600940223     C*
0497009402241-   C                   ENDIF
049800940224     C*
049900940117     C                   ENDSR
050000940207     C/EJECT
050100940207     C************************************************************
050200940510     C* RIEMPIMENTO VIDEATA  D1
050300940207     C************************************************************
050400940207     C     RIED1         BEGSR
050500940207     C*
050600940207     C*
050700940207     C                   ENDSR
050800940309     C/EJECT
050900940309     C************************************************************
051000940309     C* GESTIONE F03 VIDEO D1
051100940309     C************************************************************
051200940309     C     F03D1         BEGSR
051300940309     C*
051400940309     C                   MOVE      *ON           $FINE
051500940315    >C                   MOVE      '1'           RET002
051600940325     C* fine programma
051700940325     C                   EXSR      ENDPGM
051800940325     C*
051900940325     C                   ENDSR
052000940309     C/EJECT
052100940309     C************************************************************
052200940309     C* GESTIONE F05 VIDEO D1
052300940309     C************************************************************
052400940309     C     F05D1         BEGSR
052500940309     C*
052600940309     C                   MOVE      *ON           $INZD1
052700940309     C*
052800940309     C                   ENDSR
052900940309     C/EJECT
053000940309     C************************************************************
053100940309     C* GESTIONE F08 VIDEO D1
053200940309     C************************************************************
053300940309     C     F08D1         BEGSR
053400940309     C*
053500940309     C                   MOVE      *ON           $FINE
053600940315    >C                   MOVE      '0'           RET002
053700940309     C*
053800940309     C                   ENDSR
053900940309     C/EJECT
054000940309     C************************************************************
054100940309     C* GESTIONE F12 VIDEO D1
054200940309     C************************************************************
054300940309     C     F12D1         BEGSR
054400940309     C*
054500940309     C                   MOVE      *ON           $FINE
054600940315    >C                   MOVE      '2'           RET002
054700940309     C*
054800940309     C                   ENDSR
054900940309     C/EJECT
055000940309     C************************************************************
055100940309     C* GESTIONE F24 VIDEO D1
055200940309     C************************************************************
055300940309     C     F24D1         BEGSR
055400940309     C*
055500940506     C*
055600940506     C                   MOVEA     $FX1          $FX
055700940506     C                   MOVEA     $FC1          $FC
055800940506     C                   MOVEA     $FR1          $FR
055900940506     C                   Z-ADD     $ULKD1        $ULK
056000940506     C                   EXSR      RIEKEY
056100941017     C                   MOVEA     $FC           $FC1
056200940506     C                   Z-ADD     $ULK          $ULKD1            3 0
056300940506     C                   MOVEL     $KEY1         Z1KE1
056400940506     C                   MOVEL     $KEY2         Z1KE2
056500940614     C                   WRITE     Z1
056600940506     C*
056700940309     C                   ENDSR
056800940207     C/EJECT
056900940131     C************************************************************
057000940207     C* CONTROLLO VIDEATA
057100940131     C************************************************************
057200940127     C     CTRD1         BEGSR
057300940131     C*
057400940127     C                   SETOFF                                       99
057500940127     C* non imposto pos. del cursore
057600940127     C                   Z-ADD     0             H1RIGA
057700940127     C                   Z-ADD     0             H1COLO
057800940621     C*
057900940621     C* CTRL DATA
058000940621     C*
058100941109     C                   MOVE      DATA          WRKDSC           10
058200940621     C     D1DSC         IFNE      0
058300970214     C                   CALLB     'XDT4000'
058400941109     C                   PARM                    XDTAMG           10
058500940621     C                   PARM      D1DSC         XDTGMA            6 0
058600940621     C                   PARM      3             XDTSTA            1 0
058700940621     C     XDTSTA        IFNE      0
058800940621     C                   SETON                                        5599
058900940621     C                   ELSE
059000941108     C                   MOVE      XDTAMG        WRKDSC
059100940621     C                   ENDIF
059200940621     C                   ENDIF
059300940223     C*
059400940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
059500940223     C*
0596009403151   >C     OPZ002        IFNE      '04'
059700971110     C*
059800971110     C* controllo codice vuoto
059900971110$017 C     D1COD         IFEQ      *ALL' '
060000971110     C                   SETON                                        5099
060100971110     C                   ENDIF
060200971110     C*
060300940207     C*
060400940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
060500940315    >C     OPZ002        IFEQ      '01'
060600940315    >C     OPZ002        OREQ      '03'
060700940315    >C*-------------------------------------------------------------
060800940315    >C* impostare i campi chiave e verificare l'esistenza del record
060900940315    >C*-------------------------------------------------------------
061000971110$007 C                   MOVE      'YP4'         XTBCOD
061100940614     C                   MOVE      *ALL'0'       XTBKEY
061200940620    >C                   MOVE      D1COD         XTBKEY
061300970512    >C                   MOVE      D1UNITA       XTBUNT
061400940614     C                   MOVE      '0'           XTBALC
061500940614     C                   MOVE      '1'           XTBRIC
061600970214    >C                   CALLB     'XATB'
061700940614     C                   PARM                    XATBDS
061800940614     C     XTBERR        IFEQ      '0'
061900940223     C                   SETON                                        5999
062000940223     C                   ENDIF
062100940620     C* Controllo se compreso nei range indice
062200940620     C     XTBERR        IFEQ      '3'
062300940620    >C                   SETON                                        5899
062400940620     C                   END
062500940620     C*
062600940620     C* REPERISCO AUTORIZZAZIONI UTENTE
062700940620     C                   CLEAR                   XCHATB
062800941118     C                   MOVE      XSCSOC        XCTSOC
062900941118     C                   MOVE      XSCDUN        XCTUNI
063000940620     C                   MOVEL     KNMUS         XCTPRF
063100940620     C                   MOVE      XTBCOD        XCTCOD
063200940620     C                   CALL      'XCHKATB'
063300940620     C                   PARM                    XCHATB
063400941111     C                   MOVEL     XCTDDS        AUTDS
063500941027     C     XCTRTN        IFEQ      1
063600941027     C                   MOVEL     'N'           MANGRU
063700941027     C                   ENDIF
063800940620     C* Non ammessa immissione di gruppo
063900940620     C     XTBGRU        IFEQ      'G'
064000940620     C     MANGRU        ANDEQ     'N'
064100940620     C                   SETON                                        5799
064200940620     C                   END
064300940620     C* Non ammessa immissione di azienda
064400940620     C     XTBGRU        IFEQ      'A'
064500940620     C     MANAZI        ANDEQ     'N'
064600940620     C                   SETON                                        5699
064700940620     C                   END
064800940712     C* codice riservato non accessibile
064900941006     C     XTBRIS        IFGT      ABIRIS
065000941005    >C     OPZ002        ANDNE     '01'
065100941005    >C     OPZ002        ANDNE     '03'
065200940712     C                   SETON                                        5499
065300940712     C                   END
065400940224     C                   ENDIF
065500940224     C*
0656009402241-   C                   END
065700940131     C*
065800940302     C     *IN99         IFEQ      *ON
065900940510     C     $GEST         ANDEQ     'D1'
066000960228     C     OPZ002        ANDNE     '07'
066100940117     C                   MOVEA     *IN           WIN
066200940128     C                   MOVE      *ALL'0'       IN5098           49
066300940128     C                   MOVEA     IN5098        *IN(50)
066400940614     C                   WRITE     D1
066500940117     C                   MOVEA     WIN           *IN
066600940302     C                   ENDIF
066700940127     C*
066800940117     C                   ENDSR
066900960305     C/EJECT
067000960305     C************************************************************
067100960305     C* CONTROLLO VIDEATA D2
067200960305     C************************************************************
067300960305     C     CTRD2         BEGSR
067400960305     C*
067500960305     C                   ENDSR
067600940207     C/EJECT
067700940224     C************************************************************
067800940315    >C* AGGIORNAMENTO ANAGRAFICA
067900940224     C************************************************************
068000940315    >C     AGGANA        BEGSR
068100971110     C*
068200971110$007 C                   MOVE      'YP4'         XTBCOD
068300940614     C                   MOVE      *ALL'0'       XTBKEY
068400940614     C                   MOVE      D1COD         XTBKEY
068500970512     C                   MOVE      D1UNITA       XTBUNT
068600940207     C*
068700940207     C                   SELECT
068800940315    >C     OPZ002        WHENEQ    '01'
068900940315    >C     OPZ002        OREQ      '03'
069000940315    >C                   EXSR      RIEANA
069100950609     C                   MOVE      XSCCUT        XTBUTE
069200940614     C                   MOVE      '5'           XTBRIC
069300970214     C                   CALLB     'XATB'
069400940614    >C                   PARM                    XATBDS
069500940318     C* se rcd non scrivibile attivo errore
069600940615     C     XTBERR        IFEQ      '1'
069700940318    >C                   SETON                                        5999
069800940318     C                   END
069900940315    >C     OPZ002        WHENEQ    '02'
070000940621     C                   MOVE      '1'           XTBRIC
070100940621     C                   MOVE      '1'           XTBALC
070200970214     C                   CALLB     'XATB'
070300940621     C                   PARM                    XATBDS
070400940622    >C                   EXSR      RIEANA
070500950609     C                   MOVE      XSCCUT        XTBUTE
070600940614     C                   MOVE      '4'           XTBRIC
070700970214     C                   CALLB     'XATB'
070800940614    >C                   PARM                    XATBDS
070900940315    >C     OPZ002        WHENEQ    '04'
071000940621     C                   MOVE      '1'           XTBRIC
071100940621     C                   MOVE      '1'           XTBALC
071200970214     C                   CALLB     'XATB'
071300940621     C                   PARM                    XATBDS
071400940614     C                   MOVE      '6'           XTBRIC
071500970214     C                   CALLB     'XATB'
071600940614    >C                   PARM                    XATBDS
071700940621     C* annullo traduzioni
071800940621     C     XTBERR        IFEQ      '0'
071900940621     C                   EXSR      ANNTBT
072000940621     C                   END
072100940621     C*
072200940207     C                   ENDSL
072300940203     C*
072400940203     C                   ENDSR
072500940224     C/EJECT
072600940224     C************************************************************
072700940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
072800940224     C************************************************************
072900940616     C     RIEANA        BEGSR
073000940616     C                   CLEAR                   XTBUNI
073100940621     C                   MOVE      WRKDSC        XTBDSC
073200940712     C                   MOVE      D1RIS         XTBRIS
073300940616     C                   MOVEL     EXTDS         XTBUNI
073400940207     C                   ENDSR
073500940224     C/EJECT
073600940224     C************************************************************
073700940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
073800940224     C************************************************************
073900940224     C     GESAGG        BEGSR
074000940224     C*
074100940224     C* segnalo al pgm chiamante l'avvenuta conferma
074200940315    >C                   MOVE      *ON           OPR002
074300940224     C* segnalo al pgm chiamante l'aver premuto l'F6
074400940315    >C                   MOVE      '0'           RET002
074500940224     C*
0746009402241    C                   SELECT
074700940224     C* nel caso di immissione, ripristino la videata iniziale
074800940315    >C     OPZ002        WHENEQ    '01'
074900940224     C                   MOVE      *ON           $INZD1
075000940224     C* altrimenti ritorno al pgm chiamante
075100940224     C                   OTHER
075200940224     C                   MOVE      *ON           $FINE
0753009402241-   C                   ENDSL
075400940224     C*
075500940224     C                   ENDSR
075600940621     C/EJECT
075700940621     C************************************************************
075800940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
075900940621     C************************************************************
076000940621    >C     ANNTBT        BEGSR
076100940621     C                   MOVE      XTBAZI        TBTAZI
076200940621     C                   MOVE      XTBCOD        TBTCOD
076300940621     C                   MOVE      XTBKEY        TBTKEY
076400940621     C     KTBT          SETLL     ANTBT01L
076500940621     C                   DO        *HIVAL
076600940621     C     KTBT          DELETE    ANTBT01L                           21
076700940621     C  N21              END
076800940621     C*
076900940621     C                   ENDSR
077000940207     C/EJECT
077100940131     C************************************************************
077200940131     C* OPERAZIONI INIZIALI
077300940131     C************************************************************
077400940131     C     *INZSR        BEGSR
077500940131     C*
077600940127     C* Reperimento parametri
077700940127     C*
077800940117     C     *ENTRY        PLIST
077900940117     C                   PARM                    KPJBA
078000940712     C*
078100940712     C* Reperimento Codice utente
078200940712     C*
078300940223     C*
078400940506     C*
078500940506     C* Reperimento tasti di funzione
078600940506     C*
078700940506     C                   EXSR      REPKEY
078800940621     C*
078900940621     C* KLIST
079000940621     C*
079100940621     C     KTBT          KLIST
079200940621     C                   KFLD                    TBTAZI
079300940621     C                   KFLD                    TBTCOD
079400940621     C                   KFLD                    TBTKEY
079500940506     C*
079600940223     C                   ENDSR
079700941103     C/EJECT
079800941103      ************************************************************
079900941103      * Reperimento dati società
080000941103      ************************************************************
080100941103     C     REPSOC        BEGSR
080200941103      *
080300970214     C                   CALLB     'XSOC'
080400941103     C                   PARM                    TIPXSC            6
080500941103     C                   PARM                    SOCXSC            3
080600941103     C                   PARM                    CDSXSC            9 0
080700941103     C                   PARM                    MODXSC            3
080800941103     C                   PARM      *BLANKS       RTNXSC            1
080900941103     C                   PARM                    XSOCDS
081000941103     C                   PARM                    KPJBA
081100941103      *
081200941103     C                   ENDSR
081300940223     C/EJECT
081400940506     C************************************************************
081500940506     C* REPERIMENTO TASTI DI FUNZIONE
081600940506     C************************************************************
081700940506     C     REPKEY        BEGSR
081800940506     C                   MOVEA     $FM1          $FM
081900940506     C                   EXSR      REPKKK
082000940506     C                   MOVEA     $FX           $FX1
082100940614     C                   ENDSR
082200940506     C/EJECT
082300940506     C************************************************************
082400940506     C* REPERIMENTO TASTI DI FUNZIONE
082500940506     C************************************************************
082600940506     C     REPKKK        BEGSR
082700940506     C*
082800940506     C                   CLEAR                   $FX
082900940506     C     1             DO        24            X
083000940506     C*
083100940506     C     $FM(X)        IFNE      *BLANK
083200940506     C*
083300940506     C                   MOVEL     $FM(X)        IDMSG
083400970214     C                   CALLB     'XRTVM'
083500940506     C                   PARM                    IDMSG            27
083600940506     C                   PARM                    TXTMSG          100
083700940506     C                   PARM                    ERRMSG            1
083800940506     C*
083900940506     C     ERRMSG        IFNE      *ON
084000940506     C                   MOVEL     TXTMSG        $FX(X)
084100940506     C                   ELSE
084200940506     C                   MOVEL     *ALL' '       $FX(X)
084300940506     C                   END
084400940506     C*
084500940506     C                   END
084600940506     C*
084700940506     C                   END
084800940506     C*
084900940506     C                   ENDSR
085000940509     C/EJECT
085100940223     C************************************************************
085200940223     C* INIZIALIZZAZIONE VARIABILI
085300940223     C************************************************************
085400940223     C     INZVAR        BEGSR
085500951124     C*
085600951124     C* Chiamata XSOC
085700951124     C*
085800951124     C                   MOVEL     'SOC001'      TIPXSC
085900951124     C                   EXSR      REPSOC
086000951124     C     RTNXSC        IFNE      '1'
086100951124     C                   MOVEL     XSOCDS        SOC001
086200951124     C                   ELSE
086300951124     C                   MOVE      *ON           $FINE
086400951124     C* fine programma
086500951124     C                   EXSR      ENDPGM
086600951124     C                   ENDIF
086700940224     C*
086800940224     C* Dati da PGM guida
086900940224     C*
087000940315    >C                   MOVEL     KPJBU         D002DS
087100940128     C*
087200940223     C* Variabili per gestione videate
087300940128     C*
087400940223     C                   MOVE      'D1'          $GEST
087500940223     C                   MOVE      *OFF          $FINE
087600940223     C                   MOVE      *ON           $INZD1
087700940224     C                   MOVE      *BLANK        $LASTG
087800940224     C                   MOVE      *BLANK        $LASTV
087900940506     C                   Z-ADD     0             $ULKD1            3 0
088000940127     C*
088100940223     C* Variabili appoggio
088200940127     C*
088300940224     C                   Z-ADD     0             CURR
088400940224     C                   Z-ADD     0             CURC
088500940207     C*
088600940207     C* Valorizzazione campi univoci testate
088700940207     C*
088800940614     C                   CLEAR                   T1
088900940614     C                   MOVEL     KNSIF         NOMSIF
089000941109     C                   MOVEL     XSCDSI        NOMDIT
089100941027     C                   MOVEL     DSPGM         NOMPGM
089200940224     C                   Z-ADD     1             X
089300940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
089400940224     C     *IN21         IFEQ      *ON
089500940224     C                   MOVEL     $ID(X)        IDMSG
089600970214     C                   CALLB     'XRTVM'
089700940224     C                   PARM                    IDMSG            27
089800940224     C                   PARM                    TXTMSG          100
089900940224     C                   PARM                    ERRMSG            1
090000940224     C                   PARM      *ON           CTRMSG            1
090100940224     C                   PARM      30            LENMSG            3 0
090200940224     C     ERRMSG        IFNE      *ON
090300940224     C                   MOVEL     TXTMSG        T1OPE
090400940224     C                   ELSE
090500940224     C                   MOVEL     *ALL'?'       T1OPE
090600940224     C                   ENDIF
090700940224     C                   END
090800940224     C*
090900940224     C* Inizializzazione/Decodifica prima videata
091000940224     C*
091100940224     C                   EXSR      INZD1
091200940224     C                   MOVE      *OFF          $INZD1
091300960228     C* Se opzione = '07' richiesti solo controlli
091400960228     C     OPZ002        IFEQ      '07'
091500960228     C                   MOVEL     *IN99         ERR002
091600960228     C                   MOVEL     '0'           RET002
091700960228     C                   MOVEL     *ON           $FINE
091800960228     C                   ENDIF
091900940127     C*
092000940117     C                   ENDSR
092100940128     C************************************************************
092200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
092300940223     C************************************************************
092400940223     C     DEFCAM        BEGSR
092500940223     C*
092600940223     C* klist
092700940207     C*
092800940223     C*
092900940223     C* Variabili per gestione videate
093000940223     C*
093100940223     C                   MOVE      *BLANK        $GEST             2
093200940223     C                   MOVE      *BLANK        $FINE             1
093300940223     C                   MOVE      *BLANK        $INZD1            1
093400940223     C                   MOVE      *BLANK        $LASTG            2
093500940223     C                   MOVE      *BLANK        $LASTV            2
093600940223     C*
093700940223     C* Variabili appoggio
093800940223     C*
093900940223     C                   Z-ADD     0             CURR              2 0
094000940223     C                   Z-ADD     0             CURC              2 0
094100940223     C                   MOVE      *ZEROS        WIN              99
094200940224     C                   MOVE      *BLANK        WTBERR            1
094300940223     C*
094400940223     C* Indici
094500940223     C*
094600940223     C                   Z-ADD     0             X                 3 0
094700940223     C                   Z-ADD     0             Y                 3 0
094800940127     C*
094900940117     C                   ENDSR
095000940128     C************************************************************
095100940509     C/EJECT
095200940509     C************************************************************
095300940509     C* CONTROLLO ABILITAZIONE TASTI
095400940509     C************************************************************
095500940509     C     CTRKEY        BEGSR
095600940509     C                   SELECT
095700940509     C     $TASTO        WHENEQ    F01
095800940720     C     $FC(1)        IFEQ      '0'
095900940509     C                   SETON                                        99
096000940509     C                   ENDIF
096100940509     C     $TASTO        WHENEQ    F02
096200940720     C     $FC(2)        IFEQ      '0'
096300940509     C                   SETON                                        99
096400940509     C                   ENDIF
096500940509     C     $TASTO        WHENEQ    F03
096600940720     C     $FC(3)        IFEQ      '0'
096700940509     C                   SETON                                        99
096800940509     C                   ENDIF
096900940509     C     $TASTO        WHENEQ    F04
097000940720     C     $FC(4)        IFEQ      '0'
097100940509     C                   SETON                                        99
097200940509     C                   ENDIF
097300940509     C     $TASTO        WHENEQ    F05
097400940720     C     $FC(5)        IFEQ      '0'
097500940509     C                   SETON                                        99
097600940509     C                   ENDIF
097700940509     C     $TASTO        WHENEQ    F06
097800940720     C     $FC(6)        IFEQ      '0'
097900940509     C                   SETON                                        99
098000940509     C                   ENDIF
098100940509     C     $TASTO        WHENEQ    F07
098200940720     C     $FC(7)        IFEQ      '0'
098300940509     C                   SETON                                        99
098400940509     C                   ENDIF
098500940509     C     $TASTO        WHENEQ    F08
098600940720     C     $FC(8)        IFEQ      '0'
098700940509     C                   SETON                                        99
098800940509     C                   ENDIF
098900940509     C     $TASTO        WHENEQ    F09
099000940720     C     $FC(09)       IFEQ      '0'
099100940509     C                   SETON                                        99
099200940509     C                   ENDIF
099300940509     C     $TASTO        WHENEQ    F10
099400940720     C     $FC(10)       IFEQ      '0'
099500940509     C                   SETON                                        99
099600940509     C                   ENDIF
099700940509     C     $TASTO        WHENEQ    F11
099800940720     C     $FC(11)       IFEQ      '0'
099900940509     C                   SETON                                        99
100000940509     C                   ENDIF
100100940509     C     $TASTO        WHENEQ    F12
100200940720     C     $FC(12)       IFEQ      '0'
100300940509     C                   SETON                                        99
100400940509     C                   ENDIF
100500940509     C     $TASTO        WHENEQ    F13
100600940720     C     $FC(13)       IFEQ      '0'
100700940509     C                   SETON                                        99
100800940509     C                   ENDIF
100900940509     C     $TASTO        WHENEQ    F14
101000940720     C     $FC(14)       IFEQ      '0'
101100940509     C                   SETON                                        99
101200940509     C                   ENDIF
101300940509     C     $TASTO        WHENEQ    F15
101400940720     C     $FC(15)       IFEQ      '0'
101500940509     C                   SETON                                        99
101600940509     C                   ENDIF
101700940509     C     $TASTO        WHENEQ    F16
101800940720     C     $FC(16)       IFEQ      '0'
101900940509     C                   SETON                                        99
102000940509     C                   ENDIF
102100940509     C     $TASTO        WHENEQ    F17
102200940720     C     $FC(17)       IFEQ      '0'
102300940509     C                   SETON                                        99
102400940509     C                   ENDIF
102500940509     C     $TASTO        WHENEQ    F18
102600940720     C     $FC(18)       IFEQ      '0'
102700940509     C                   SETON                                        99
102800940509     C                   ENDIF
102900940509     C     $TASTO        WHENEQ    F19
103000940720     C     $FC(19)       IFEQ      '0'
103100940509     C                   SETON                                        99
103200940509     C                   ENDIF
103300940509     C     $TASTO        WHENEQ    F20
103400940720     C     $FC(20)       IFEQ      '0'
103500940509     C                   SETON                                        99
103600940509     C                   ENDIF
103700940509     C     $TASTO        WHENEQ    F21
103800940720     C     $FC(21)       IFEQ      '0'
103900940509     C                   SETON                                        99
104000940509     C                   ENDIF
104100940509     C     $TASTO        WHENEQ    F22
104200940720     C     $FC(22)       IFEQ      '0'
104300940509     C                   SETON                                        99
104400940509     C                   ENDIF
104500940509     C     $TASTO        WHENEQ    F23
104600940720     C     $FC(23)       IFEQ      '0'
104700940509     C                   SETON                                        99
104800940509     C                   ENDIF
104900940509     C     $TASTO        WHENEQ    F24
105000940720     C     $FC(24)       IFEQ      '0'
105100940509     C                   SETON                                        99
105200940509     C                   ENDIF
105300940509     C                   ENDSL
105400940509     C                   ENDSR
105500940509     C/EJECT
105600940509     C************************************************************
105700940509     C* RIEMPIMENTO TASTI FUNZIONE
105800940509     C************************************************************
105900940509     C     RIEKEY        BEGSR
106000940509     C*
106100940509     C* Input :
106200940509     C* - $FX = schiera con tutti i testi disponibili
106300940509     C* - $FC = schiere con i flag di validità associati ai testi
106400940509     C* - $FR = schiere con i flag di REVERSE IMAGE
106500940509     C* - $ULK = ultimo testo caricato
106600940509     C* Output :
106700940509     C* - $ULK
106800940509     c* - $KEY1= 1° riga di tasti funzione
106900940509     c* - $KEY2= 2° riga di tasti funzione
107000940509     C*
107100970214     C                   CALLB     'XKEYMSG'
107200940509     C* input
107300940509     C                   PARM                    $FX
107400940509     C                   PARM                    $FC
107500940509     C                   PARM                    $FR
107600940509     C                   PARM                    $ULK              3 0
107700940509     C* output
107800940509     C                   PARM                    $KEY1            79
107900940509     C                   PARM                    $KEY2            79
108000940509     C*
108100940509     C                   ENDSR
108200940615**
10830094022401PROMSG    *LIBL     COS0001    immissione
10840094022402PROMSG    *LIBL     COS0002    modifica
10850094022403PROMSG    *LIBL     COS0003    copia
10860094022404PROMSG    *LIBL     COS0004    annullamento
10870094022405PROMSG    *LIBL     COS0005    visualizzazione
108800940509** TASTI DI FUNZIONE  D1
108900940509PROMSG    *LIBL     KEY0001  01
109000940509PROMSG    *LIBL     KEY0002  02
109100940509PROMSG    *LIBL     KEY0003  03
109200940509PROMSG    *LIBL     KEY0004  04
109300940509PROMSG    *LIBL     KEY0005  05
109400940509PROMSG    *LIBL     KEY0006  06
109500940509PROMSG    *LIBL     KEY0007  07
109600940509PROMSG    *LIBL     KEY0008  08
109700940509PROMSG    *LIBL     KEY0009  09
109800940509PROMSG    *LIBL     KEY0010  10
109900940509PROMSG    *LIBL     KEY0011  11
110000940509PROMSG    *LIBL     KEY0012  12
110100940509PROMSG    *LIBL     KEY0013  13
110200940509PROMSG    *LIBL     KEY0014  14
110300940509PROMSG    *LIBL     KEY0015  15
110400940509PROMSG    *LIBL     KEY0016  16
110500940509PROMSG    *LIBL     KEY0017  17
110600940509PROMSG    *LIBL     KEY0018  18
110700940509PROMSG    *LIBL     KEY0019  19
110800940509PROMSG    *LIBL     KEY0020  20
110900940509PROMSG    *LIBL     KEY0021  21
111000940509PROMSG    *LIBL     KEY0022  22
111100940509PROMSG    *LIBL     KEY0023  23
111200940509PROMSG    *LIBL     KEY0024  24
111300940509**  ABILITAZIONE D1
1114009407200N  01
1115009407200N  02
1116009407201N  03
1117009407201N  04
1118009407201N  05
1119009407201N  06
1120009407201N  07
1121009407201N  08
1122009407200N  09
1123009407200N  10
1124009407200N  11
1125009407201N  12
1126009407200N  13
1127009407200N  14
1128009407200N  15
1129009407200N  16
1130009407200N  17
1131009407200N  18
1132009407200N  19
1133009407200N  20
1134009407200N  21
1135009407200N  22
1136009407200N  23
1137009407200N  24
