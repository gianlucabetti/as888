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
004100940614$001 FANGY01D2  CF   E             WORKSTN
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
006000940614$003 D EXTDS         E DS                  EXTNAME(ANGY01DS) INZ
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
017500940614$005 I              D1IMPMAX                    IMPMAXY01
017600940127     I*
017700940127     C*----------------------------------------------------*
017800940127     C*                MAIN LINE PROGRAM
017900940127     C*----------------------------------------------------*
018000940127     C*
018100940223     C* inizializzazione variabili
018200940223     C                   EXSR      INZVAR
018300940223     C*
018400940223     C     $FINE         DOWEQ     *OFF
018500940202     C     $GEST         CASEQ     'D1'          GESD1
018600940117     C                   END
018700940117     C                   END
018800940201     C*
018900940325     C* fine programma
019000940325     C                   EXSR      ENDPGM
019100940325     C/EJECT
019200940325     C************************************************************
019300940325     C* RICERCHE
019400940325     C************************************************************
019500940325     C     ENDPGM        BEGSR
019600940325     C*
019700940614     C* disallocazione rcd nel caso di
019800971110$007 C                   MOVE      'Y01'         XTBCOD
019900940614    >C                   MOVE      '7'           XTBRIC
020000970214     C                   CALLB     'XATB'
020100940614     C                   PARM                    XATBDS
020200940325     C*
020300940325     C* passaggio dati al pgm chiamante
020400940715     C                   MOVE      *BLANKS       KPJBU
020500940325    >C                   MOVEL     D002DS        KPJBU
020600940325     C*
020700940325     C                   SETON                                            RT
020800940325     C*
020900940325     C                   ENDSR
021000940325     C/EJECT
021100940201     C************************************************************
021200940201     C* RICERCHE
021300940201     C************************************************************
021400940127     C     SEARCH        BEGSR
021500940127     C*
021600940127     C* determino Riga/Colonna del cursore
021700940127     C                   MOVE      NRG           R$$
021800940127     C                   MOVE      NCL           C$$
021900940127     C                   Z-ADD     RIRI          CURR
022000940127     C                   Z-ADD     COCO          CURC
022100940127     C*
022200940224     C* I campi che al momento dell'F4 risultano protetti non devono
022300940224     C* essere abilitati alla funzione di ricerca
022400940224     C*
0225009401271    C                   SELECT
022600940127     C*
022700940315     C* FMTD1  - PACCHETTO
022800940614    >C*          H1NMFL    WHEQ 'D1PKG'
022900940207     C* imposto pos. del cursore
023000940614     C*                    Z-ADDCURR      H1RIGA
023100940614     C*                    Z-ADDCURC      H1COLO
023200940127     C*
023300940223     C*
023400940223     C* ricerca non abilitata in questa posizione
0235009402231O   C                   OTHER
023600941005     C                   SETON                                        9899
023700941005     C* imposto pos. del cursore
023800941005     C                   Z-ADD     CURR          H1RIGA
023900941005     C                   Z-ADD     CURC          H1COLO
024000940127     C*
0241009401271-   C                   ENDSL
024200940127     C*
024300940506     C                   MOVE      *HIVAL        $LASTV
024400940127     C                   ENDSR
024500940615     C/EJECT
024600940615     C************************************************************
024700940615     C* TRADUZIONI
024800940615     C************************************************************
024900940615     C     LINGUA        BEGSR
025000940615     C*
025100940615     C* determino Riga/Colonna del cursore
025200940615     C                   MOVE      NRG           R$$
025300940615     C                   MOVE      NCL           C$$
025400940615     C                   Z-ADD     RIRI          CURR
025500940615     C                   Z-ADD     COCO          CURC
025600940615     C*
025700940615     C*
025800940615     C*
025900940615     C* FMTD1  - CAMPI DA TRADURRE
026000940615     C* imposto pos. del cursore
026100940615     C                   Z-ADD     CURR          H1RIGA
026200940615     C                   Z-ADD     CURC          H1COLO
026300940621     C                   MOVEL     H1NMFL        W005              5
026400940621     C                   MOVE      W005          W003              3
026500940621     C                   MOVEL     W003          W006              6
026600940620     C                   MOVE      *ZERO         KEY              15
026700940616     C                   MOVE      D1COD         KEY
026800940615     C                   SELECT
028300940615     C*
028400940615     C*
028500940615     C* ricerca non abilitata in questa posizione
0286009406151O   C                   OTHER
028700940615     C                   SETON                                        98
028800940615     C*
0289009406151-   C                   ENDSL
029000940615     C*
029100940615     C                   MOVE      *HIVAL        $LASTV
029200940615     C                   ENDSR
029300940225     C/EJECT
029400940225     C************************************************************
029500940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
029600940225     C************************************************************
029700940225     C     GESPIE        BEGSR
029800940225     C*
029900940225     C                   SELECT
030000940225     C*
030100940506     C     $GEST         WHENEQ    'D1'
030200940506     C     $ULKD1        IFEQ      0
030300940506     C* riempimento opzioni
030400940506     C                   EXSR      F24D1
030500940506     C                   ELSE
030600940614     C                   WRITE     Z1
030700940506     C                   ENDIF
030800940225     C*
030900940510     C*
031000940225     C                   ENDSL
031100940225     C*
031200940225     C                   ENDSR
031300940131     C/EJECT
031400940131     C************************************************************
031500940510     C* GESTIONE VIDEO RECORD D1
031600940131     C************************************************************
031700940127     C     GESD1         BEGSR
031800940225     C* inizializzazione videata
031900940223     C     $INZD1        IFEQ      *ON
032000940127     C                   EXSR      INZD1
032100940223     C                   MOVE      *OFF          $INZD1
032200940117     C                   END
032300940225     C* emissione piede videata se proveniente da altra
032400940225     C* salvataggio valore formato di provenienza
032500940207     C     $LASTV        IFNE      'D1'
032600940614     C                   WRITE     T1
032700940225     C                   EXSR      GESPIE
032800940202     C                   MOVE      $LASTV        $LASTG
032900940202     C                   MOVE      'D1'          $LASTV
033000940202     C                   END
033100940131     C*
033200940203     C                   SELECT
033300940203     C* Annullamento
033400940315    >C     OPZ002        WHENEQ    '04'
033500940224     C* Visualizzazione
033600940315    >C     OPZ002        OREQ      '05'
033700940620     C                   SETON                                        0203
033800940620     C* Manutenzione
033900940620    >C     OPZ002        WHENEQ    '02'
034000940620     C                   SETON                                        02
034100940620     C                   SETOFF                                       03
034200940620     C*
034300940620     C                   OTHER
034400940620     C*
034500940620     C                   SETOFF                                       0203
034600940203     C                   ENDSL
034700940620     C* Immissione/Modifica/Copia
034800940620     C                   EXFMT     D1
034900940506     C* controllo abilitazione tasti funzione
035000940506     C*
035100940506     C                   EXSR      CTRKD1
035200940506     C*
035300940506     C     *IN99         IFEQ      *OFF
035400940131     C*
0355009402031    C                   SELECT
035600940510     c* ROLLUP
035700940131     C* F3=Fine
035800940506     C     $TASTO        WHENEQ    F03
035900940309     C                   EXSR      F03D1
036000940131     C* F4=Ricerca
036100940506     C     $TASTO        WHENEQ    F04
036200940127     C                   EXSR      SEARCH
036300940224     C* F5=Refresh
036400940506     C     $TASTO        WHENEQ    F05
036500940309     C                   EXSR      F05D1
036600940223     C* F8=Successivo
036700940506     C     $TASTO        WHENEQ    F08
036800940309     C                   EXSR      F08D1
036900940131     C* F12=Ritorno
037000940506     C     $TASTO        WHENEQ    F12
037100940309     C                   EXSR      F12D1
037200940315     C* F24=Altri tasti
037300940506     C     $TASTO        WHENEQ    F24
037400940315     C                   EXSR      F24D1
037500940131     C*
0376009402031O   C                   OTHER
037700940127     C                   EXSR      CTRD1
037800940616     C     *IN99         IFEQ      *OFF
037900940616     C                   SELECT
038000940616     C* F7=Traduzione
038100940616     C     $TASTO        WHENEQ    F07
038200940616     C                   EXSR      LINGUA
038300940616     C* F6
0384009406162    C     $TASTO        WHENEQ    F06
038500940224     C* eseguo aggiornamento
038600940315    >C                   EXSR      AGGANA
038700940224     C* eseguo operazioni del dopo-aggiornamento
0388009403183    C     *IN99         IFEQ      *OFF
038900940318     C                   EXSR      GESAGG
0390009403183-   C                   ENDIF
0391009406162-   C                   ENDSL
039200940616     C                   ENDIF
039300940203     C*
0394009402241-   C                   ENDSL
039500940224     C*
0396009405062-   C                   ENDIF
039700940117     C                   ENDSR
039800940506     C/EJECT
039900940506     C************************************************************
040000940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
040100940506     C************************************************************
040200940506     C     CTRKD1        BEGSR
040300940506     C                   SETOFF                                       99
040400940506     C                   MOVEA     $FC1          $FC
040500940506     C                   EXSR      CTRKEY
040600940506     C     *IN99         IFEQ      *ON
040700940506     C                   SETON                                        97
040800940506     C                   ENDIF
040900940506     C                   ENDSR
041000940117      ************************************************************
041100940207      * INIZIALIZZAZIONE VIDEATA DATI
041200940117      ************************************************************
041300940127     C     INZD1         BEGSR
041400940131     C*
041500940615     C                   CLEAR                   D1
041600990827X0079C* Aggiunta riga seguente
041700990827$028 C
041800941006     C                   MOVE      '0'           D1RIS
041900940616     C                   MOVE      KEY002        D1COD
042000940615     C                   MOVE      AZI002        XTBAZI
042100940615     C                   MOVE      LIN002        XTBLIN
042200940616     C                   MOVE      *ALL'0'       XTBKEY
042300940615     C                   MOVE      KEY002        XTBKEY
042400970509     C                   MOVE      UNI002        XTBUNT
042500940615$007 C                   MOVE      'Y01'         XTBCOD
042600940225     C*
042700940207     C*
042800940224     C* valorizzo la chiave univoca per reperire i dati
042900940224     C* se non immissione
0430009403151   >C     OPZ002        IFNE      '01'
043100940315    >C*-------------------------------------------------------------
043200940315    >C* impostare i campi chiave ricevuti
043300940315    >C*-------------------------------------------------------------
043400940318     C*
043500940614     C*
043600940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0437009406142   >C     OPZ002        IFEQ      '02'
043800940315    >C     OPZ002        OREQ      '04'
043900940614     C                   MOVE      '1'           XTBALC
044000940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0441009406142E   C                   ELSE
044200940614     C                   MOVE      '0'           XTBALC
0443009406142-   C                   ENDIF
044400940614     C*
044500940616     C                   MOVE      '1'           XTBRIC
044600970214     C                   CALLB     'XATB'
044700940614     C                   PARM                    XATBDS
044800940318     C*
044900940614     C     XTBERR        IFEQ      '0'
045000940614     C                   MOVEL     XTBUNI        EXTDS
045100970512     c                   move      xtbunt        d1uniTA           8
045200941108     C* Giro la data se non blank
045300970214     c                   move      xtbdsc        xtbdscA          10
045400970214     C     XTBDSCa       IFNE      DATA
045500970214     C                   CALLB     'XDT4000'
045600970214     C                   PARM      XTBDSCA       XDTAMG           10
045700940621     C                   PARM      *ZEROS        XDTGMA            6 0
045800940621     C                   PARM      2             XDTSTA            1 0
045900940621     C                   MOVE      XDTGMA        D1DSC
046000940712     C                   MOVE      XTBRIS        D1RIS
046100941108     C                   ELSE
046200941108     C                   MOVE      *ZEROS        D1DSC
046300941108     C                   ENDIF
046400941108     C                   ENDIF
046500940614     C*
046600940224     C*
046700940224     C* se il rcd da gestire non viene trovato
046800940318     C* oppure è allocato da altro pgm
046900940224     C* ritorno al pgm guida avvertendolo dell'errore
0470009406142    C     XTBERR        IFNE      '0'
047100940224     C                   MOVE      *ON           $FINE
047200940315    >C                   MOVE      '1'           ERR002
047300940224     C*
047400940224     C* altrimenti
0475009402242E   C                   ELSE
047600940224     C* riempio la videata
047700940224     C                   EXSR      RIED1
047800940224     C* richiamo routine dei ctrl per decodificarla
047900940614    >C                   EXSR      CTRD1
048000960228     C                   MOVEL     *IN99         SAV99             1
048100960228     C* Se la gestione è più videate
048200960305    >C                   EXSR      CTRD2
048300960305    >C     *IN99         IFEQ      *ON
048400960305    >C                   MOVEL     *IN99         SAV99             1
048500960305    >C                   ENDIF
048600940224     C                   MOVE      *ALL'0'       IN5098
048700940224     C                   MOVEA     IN5098        *IN(50)
048800940224     C* definizione indicatori su video
048900940224     C                   SETOFF                                       02
0490009402243    C                   SELECT
049100940315    >C     OPZ002        WHENEQ    '02'
049200940224     C                   SETON                                        02
049300940315    >C     OPZ002        WHENEQ    '05'
049400940506     C* disabilita tasti
049500940720     C                   MOVE      '0'           $FC(6)
049600940720     C                   MOVE      '0'           $FC(5)
049700940720     C                   MOVE      '0'           $FC(4)
0498009402243-   C                   ENDSL
049900940224     C*
0500009402242-   C                   ENDIF
050100940223     C*
0502009402241-   C                   ENDIF
050300940224     C*
050400940117     C                   ENDSR
050500940207     C/EJECT
050600940207     C************************************************************
050700940510     C* RIEMPIMENTO VIDEATA  D1
050800940207     C************************************************************
050900940207     C     RIED1         BEGSR
051000940207     C*
051100990827$025 C
051200940207     C*
051300940207     C                   ENDSR
051400940309     C/EJECT
051500940309     C************************************************************
051600940309     C* GESTIONE F03 VIDEO D1
051700940309     C************************************************************
051800940309     C     F03D1         BEGSR
051900940309     C*
052000940309     C                   MOVE      *ON           $FINE
052100940315    >C                   MOVE      '1'           RET002
052200940325     C* fine programma
052300940325     C                   EXSR      ENDPGM
052400940325     C*
052500940325     C                   ENDSR
052600940309     C/EJECT
052700940309     C************************************************************
052800940309     C* GESTIONE F05 VIDEO D1
052900940309     C************************************************************
053000940309     C     F05D1         BEGSR
053100940309     C*
053200940309     C                   MOVE      *ON           $INZD1
053300940309     C*
053400940309     C                   ENDSR
053500940309     C/EJECT
053600940309     C************************************************************
053700940309     C* GESTIONE F08 VIDEO D1
053800940309     C************************************************************
053900940309     C     F08D1         BEGSR
054000940309     C*
054100940309     C                   MOVE      *ON           $FINE
054200940315    >C                   MOVE      '0'           RET002
054300940309     C*
054400940309     C                   ENDSR
054500940309     C/EJECT
054600940309     C************************************************************
054700940309     C* GESTIONE F12 VIDEO D1
054800940309     C************************************************************
054900940309     C     F12D1         BEGSR
055000940309     C*
055100940309     C                   MOVE      *ON           $FINE
055200940315    >C                   MOVE      '2'           RET002
055300940309     C*
055400940309     C                   ENDSR
055500940309     C/EJECT
055600940309     C************************************************************
055700940309     C* GESTIONE F24 VIDEO D1
055800940309     C************************************************************
055900940309     C     F24D1         BEGSR
056000940309     C*
056100940506     C*
056200940506     C                   MOVEA     $FX1          $FX
056300940506     C                   MOVEA     $FC1          $FC
056400940506     C                   MOVEA     $FR1          $FR
056500940506     C                   Z-ADD     $ULKD1        $ULK
056600940506     C                   EXSR      RIEKEY
056700941017     C                   MOVEA     $FC           $FC1
056800940506     C                   Z-ADD     $ULK          $ULKD1            3 0
056900940506     C                   MOVEL     $KEY1         Z1KE1
057000940506     C                   MOVEL     $KEY2         Z1KE2
057100940614     C                   WRITE     Z1
057200940506     C*
057300940309     C                   ENDSR
057400940207     C/EJECT
057500940131     C************************************************************
057600940207     C* CONTROLLO VIDEATA
057700940131     C************************************************************
057800940127     C     CTRD1         BEGSR
057900940131     C*
058000940127     C                   SETOFF                                       99
058100940127     C* non imposto pos. del cursore
058200940127     C                   Z-ADD     0             H1RIGA
058300940127     C                   Z-ADD     0             H1COLO
058400940621     C*
058500940621     C* CTRL DATA
058600940621     C*
058700941109     C                   MOVE      DATA          WRKDSC           10
058800940621     C     D1DSC         IFNE      0
058900970214     C                   CALLB     'XDT4000'
059000941109     C                   PARM                    XDTAMG           10
059100940621     C                   PARM      D1DSC         XDTGMA            6 0
059200940621     C                   PARM      3             XDTSTA            1 0
059300940621     C     XDTSTA        IFNE      0
059400940621     C                   SETON                                        5599
059500940621     C                   ELSE
059600941108     C                   MOVE      XDTAMG        WRKDSC
059700940621     C                   ENDIF
059800940621     C                   ENDIF
059900940223     C*
060000940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
060100940223     C*
0602009403151   >C     OPZ002        IFNE      '04'
060300971110     C*
060400971110     C* controllo codice vuoto
060500971110$017 C     D1COD         IFEQ      *ALL' '
060600971110     C                   SETON                                        5099
060700971110     C                   ENDIF
060800971110     C*
060900940614$009 C     IMPMAXY01     IFEQ      *ZERO
060900940614$009 C                   SETON                                        6099
060900940614$009 C                   ENDIF
061300940207     C*
061400940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
061500940315    >C     OPZ002        IFEQ      '01'
061600940315    >C     OPZ002        OREQ      '03'
061700940315    >C*-------------------------------------------------------------
061800940315    >C* impostare i campi chiave e verificare l'esistenza del record
061900940315    >C*-------------------------------------------------------------
062000971110$007 C                   MOVE      'Y01'         XTBCOD
062100940614     C                   MOVE      *ALL'0'       XTBKEY
062200940620    >C                   MOVE      D1COD         XTBKEY
062300970512    >C                   MOVE      D1UNITA       XTBUNT
062400940614     C                   MOVE      '0'           XTBALC
062500940614     C                   MOVE      '1'           XTBRIC
062600970214    >C                   CALLB     'XATB'
062700940614     C                   PARM                    XATBDS
062800940614     C     XTBERR        IFEQ      '0'
062900940223     C                   SETON                                        5999
063000940223     C                   ENDIF
063100940620     C* Controllo se compreso nei range indice
063200940620     C     XTBERR        IFEQ      '3'
063300940620    >C                   SETON                                        5899
063400940620     C                   END
063500940620     C*
063600940620     C* REPERISCO AUTORIZZAZIONI UTENTE
063700940620     C                   CLEAR                   XCHATB
063800941118     C                   MOVE      XSCSOC        XCTSOC
063900941118     C                   MOVE      XSCDUN        XCTUNI
064000940620     C                   MOVEL     KNMUS         XCTPRF
064100940620     C                   MOVE      XTBCOD        XCTCOD
064200940620     C                   CALL      'XCHKATB'
064300940620     C                   PARM                    XCHATB
064400941111     C                   MOVEL     XCTDDS        AUTDS
064500941027     C     XCTRTN        IFEQ      1
064600941027     C                   MOVEL     'N'           MANGRU
064700941027     C                   ENDIF
064800940620     C* Non ammessa immissione di gruppo
064900940620     C     XTBGRU        IFEQ      'G'
065000940620     C     MANGRU        ANDEQ     'N'
065100940620     C                   SETON                                        5799
065200940620     C                   END
065300940620     C* Non ammessa immissione di azienda
065400940620     C     XTBGRU        IFEQ      'A'
065500940620     C     MANAZI        ANDEQ     'N'
065600940620     C                   SETON                                        5699
065700940620     C                   END
065800940712     C* codice riservato non accessibile
065900941006     C     XTBRIS        IFGT      ABIRIS
066000941005    >C     OPZ002        ANDNE     '01'
066100941005    >C     OPZ002        ANDNE     '03'
066200940712     C                   SETON                                        5499
066300940712     C                   END
066400940224     C                   ENDIF
066500940224     C*
0666009402241-   C                   END
066700940131     C*
066800940302     C     *IN99         IFEQ      *ON
066900940510     C     $GEST         ANDEQ     'D1'
067000960228     C     OPZ002        ANDNE     '07'
067100940117     C                   MOVEA     *IN           WIN
067200940128     C                   MOVE      *ALL'0'       IN5098           49
067300940128     C                   MOVEA     IN5098        *IN(50)
067400940614     C                   WRITE     D1
067500940117     C                   MOVEA     WIN           *IN
067600940302     C                   ENDIF
067700940127     C*
067800940117     C                   ENDSR
067900960305     C/EJECT
068000960305     C************************************************************
068100960305     C* CONTROLLO VIDEATA D2
068200960305     C************************************************************
068300960305     C     CTRD2         BEGSR
068400960305     C*
068500960305     C                   ENDSR
068600940207     C/EJECT
068700940224     C************************************************************
068800940315    >C* AGGIORNAMENTO ANAGRAFICA
068900940224     C************************************************************
069000940315    >C     AGGANA        BEGSR
069100971110     C*
069200971110$007 C                   MOVE      'Y01'         XTBCOD
069300940614     C                   MOVE      *ALL'0'       XTBKEY
069400940614     C                   MOVE      D1COD         XTBKEY
069500970512     C                   MOVE      D1UNITA       XTBUNT
069600940207     C*
069700940207     C                   SELECT
069800940315    >C     OPZ002        WHENEQ    '01'
069900940315    >C     OPZ002        OREQ      '03'
070000940315    >C                   EXSR      RIEANA
070100950609     C                   MOVE      XSCCUT        XTBUTE
070200940614     C                   MOVE      '5'           XTBRIC
070300970214     C                   CALLB     'XATB'
070400940614    >C                   PARM                    XATBDS
070500940318     C* se rcd non scrivibile attivo errore
070600940615     C     XTBERR        IFEQ      '1'
070700940318    >C                   SETON                                        5999
070800940318     C                   END
070900940315    >C     OPZ002        WHENEQ    '02'
071000940621     C                   MOVE      '1'           XTBRIC
071100940621     C                   MOVE      '1'           XTBALC
071200970214     C                   CALLB     'XATB'
071300940621     C                   PARM                    XATBDS
071400940622    >C                   EXSR      RIEANA
071500950609     C                   MOVE      XSCCUT        XTBUTE
071600940614     C                   MOVE      '4'           XTBRIC
071700970214     C                   CALLB     'XATB'
071800940614    >C                   PARM                    XATBDS
071900940315    >C     OPZ002        WHENEQ    '04'
072000940621     C                   MOVE      '1'           XTBRIC
072100940621     C                   MOVE      '1'           XTBALC
072200970214     C                   CALLB     'XATB'
072300940621     C                   PARM                    XATBDS
072400940614     C                   MOVE      '6'           XTBRIC
072500970214     C                   CALLB     'XATB'
072600940614    >C                   PARM                    XATBDS
072700940621     C* annullo traduzioni
072800940621     C     XTBERR        IFEQ      '0'
072900940621     C                   EXSR      ANNTBT
073000940621     C                   END
073100940621     C*
073200940207     C                   ENDSL
073300940203     C*
073400940203     C                   ENDSR
073500940224     C/EJECT
073600940224     C************************************************************
073700940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
073800940224     C************************************************************
073900940616     C     RIEANA        BEGSR
074000940616     C                   CLEAR                   XTBUNI
074100940621     C                   MOVE      WRKDSC        XTBDSC
074200940712     C                   MOVE      D1RIS         XTBRIS
074300981209$026 C
074400940616     C                   MOVEL     EXTDS         XTBUNI
074500940207     C                   ENDSR
074600940224     C/EJECT
074700940224     C************************************************************
074800940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
074900940224     C************************************************************
075000940224     C     GESAGG        BEGSR
075100940224     C*
075200940224     C* segnalo al pgm chiamante l'avvenuta conferma
075300940315    >C                   MOVE      *ON           OPR002
075400940224     C* segnalo al pgm chiamante l'aver premuto l'F6
075500940315    >C                   MOVE      '0'           RET002
075600940224     C*
0757009402241    C                   SELECT
075800940224     C* nel caso di immissione, ripristino la videata iniziale
075900940315    >C     OPZ002        WHENEQ    '01'
076000940224     C                   MOVE      *ON           $INZD1
076100940224     C* altrimenti ritorno al pgm chiamante
076200940224     C                   OTHER
076300940224     C                   MOVE      *ON           $FINE
0764009402241-   C                   ENDSL
076500940224     C*
076600940224     C                   ENDSR
076700940621     C/EJECT
076800940621     C************************************************************
076900940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
077000940621     C************************************************************
077100940621    >C     ANNTBT        BEGSR
077200940621     C                   MOVE      XTBAZI        TBTAZI
077300940621     C                   MOVE      XTBCOD        TBTCOD
077400940621     C                   MOVE      XTBKEY        TBTKEY
077500940621     C     KTBT          SETLL     ANTBT01L
077600940621     C                   DO        *HIVAL
077700940621     C     KTBT          DELETE    ANTBT01L                           21
077800940621     C  N21              END
077900940621     C*
078000940621     C                   ENDSR
078100940207     C/EJECT
078200940131     C************************************************************
078300940131     C* OPERAZIONI INIZIALI
078400940131     C************************************************************
078500940131     C     *INZSR        BEGSR
078600940131     C*
078700940127     C* Reperimento parametri
078800940127     C*
078900940117     C     *ENTRY        PLIST
079000940117     C                   PARM                    KPJBA
079100940712     C*
079200940712     C* Reperimento Codice utente
079300940712     C*
079400940223     C*
079500940506     C*
079600940506     C* Reperimento tasti di funzione
079700940506     C*
079800940506     C                   EXSR      REPKEY
079900940621     C*
080000940621     C* KLIST
080100940621     C*
080200940621     C     KTBT          KLIST
080300940621     C                   KFLD                    TBTAZI
080400940621     C                   KFLD                    TBTCOD
080500940621     C                   KFLD                    TBTKEY
080600940506     C*
080700940223     C                   ENDSR
080800941103     C/EJECT
080900941103      ************************************************************
081000941103      * Reperimento dati società
081100941103      ************************************************************
081200941103     C     REPSOC        BEGSR
081300941103      *
081400970214     C                   CALLB     'XSOC'
081500941103     C                   PARM                    TIPXSC            6
081600941103     C                   PARM                    SOCXSC            3
081700941103     C                   PARM                    CDSXSC            9 0
081800941103     C                   PARM                    MODXSC            3
081900941103     C                   PARM      *BLANKS       RTNXSC            1
082000941103     C                   PARM                    XSOCDS
082100941103     C                   PARM                    KPJBA
082200941103      *
082300941103     C                   ENDSR
082400940223     C/EJECT
082500940506     C************************************************************
082600940506     C* REPERIMENTO TASTI DI FUNZIONE
082700940506     C************************************************************
082800940506     C     REPKEY        BEGSR
082900940506     C                   MOVEA     $FM1          $FM
083000940506     C                   EXSR      REPKKK
083100940506     C                   MOVEA     $FX           $FX1
083200940614     C                   ENDSR
083300940506     C/EJECT
083400940506     C************************************************************
083500940506     C* REPERIMENTO TASTI DI FUNZIONE
083600940506     C************************************************************
083700940506     C     REPKKK        BEGSR
083800940506     C*
083900940506     C                   CLEAR                   $FX
084000940506     C     1             DO        24            X
084100940506     C*
084200940506     C     $FM(X)        IFNE      *BLANK
084300940506     C*
084400940506     C                   MOVEL     $FM(X)        IDMSG
084500970214     C                   CALLB     'XRTVM'
084600940506     C                   PARM                    IDMSG            27
084700940506     C                   PARM                    TXTMSG          100
084800940506     C                   PARM                    ERRMSG            1
084900940506     C*
085000940506     C     ERRMSG        IFNE      *ON
085100940506     C                   MOVEL     TXTMSG        $FX(X)
085200940506     C                   ELSE
085300940506     C                   MOVEL     *ALL' '       $FX(X)
085400940506     C                   END
085500940506     C*
085600940506     C                   END
085700940506     C*
085800940506     C                   END
085900940506     C*
086000940506     C                   ENDSR
086100940509     C/EJECT
086200940223     C************************************************************
086300940223     C* INIZIALIZZAZIONE VARIABILI
086400940223     C************************************************************
086500940223     C     INZVAR        BEGSR
086600951124     C*
086700951124     C* Chiamata XSOC
086800951124     C*
086900951124     C                   MOVEL     'SOC001'      TIPXSC
087000951124     C                   EXSR      REPSOC
087100951124     C     RTNXSC        IFNE      '1'
087200951124     C                   MOVEL     XSOCDS        SOC001
087300951124     C                   ELSE
087400951124     C                   MOVE      *ON           $FINE
087500951124     C* fine programma
087600951124     C                   EXSR      ENDPGM
087700951124     C                   ENDIF
087800940224     C*
087900940224     C* Dati da PGM guida
088000940224     C*
088100940315    >C                   MOVEL     KPJBU         D002DS
088200940128     C*
088300940223     C* Variabili per gestione videate
088400940128     C*
088500940223     C                   MOVE      'D1'          $GEST
088600940223     C                   MOVE      *OFF          $FINE
088700940223     C                   MOVE      *ON           $INZD1
088800940224     C                   MOVE      *BLANK        $LASTG
088900940224     C                   MOVE      *BLANK        $LASTV
089000940506     C                   Z-ADD     0             $ULKD1            3 0
089100940127     C*
089200940223     C* Variabili appoggio
089300940127     C*
089400940224     C                   Z-ADD     0             CURR
089500940224     C                   Z-ADD     0             CURC
089600940207     C*
089700940207     C* Valorizzazione campi univoci testate
089800940207     C*
089900940614     C                   CLEAR                   T1
090000940614     C                   MOVEL     KNSIF         NOMSIF
090100941109     C                   MOVEL     XSCDSI        NOMDIT
090200941027     C                   MOVEL     DSPGM         NOMPGM
090300940224     C                   Z-ADD     1             X
090400940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
090500940224     C     *IN21         IFEQ      *ON
090600940224     C                   MOVEL     $ID(X)        IDMSG
090700970214     C                   CALLB     'XRTVM'
090800940224     C                   PARM                    IDMSG            27
090900940224     C                   PARM                    TXTMSG          100
091000940224     C                   PARM                    ERRMSG            1
091100940224     C                   PARM      *ON           CTRMSG            1
091200940224     C                   PARM      30            LENMSG            3 0
091300940224     C     ERRMSG        IFNE      *ON
091400940224     C                   MOVEL     TXTMSG        T1OPE
091500940224     C                   ELSE
091600940224     C                   MOVEL     *ALL'?'       T1OPE
091700940224     C                   ENDIF
091800940224     C                   END
091900940224     C*
092000940224     C* Inizializzazione/Decodifica prima videata
092100940224     C*
092200940224     C                   EXSR      INZD1
092300940224     C                   MOVE      *OFF          $INZD1
092400960228     C* Se opzione = '07' richiesti solo controlli
092500960228     C     OPZ002        IFEQ      '07'
092600960228     C                   MOVEL     *IN99         ERR002
092700960228     C                   MOVEL     '0'           RET002
092800960228     C                   MOVEL     *ON           $FINE
092900960228     C                   ENDIF
093000940127     C*
093100940117     C                   ENDSR
093200940128     C************************************************************
093300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
093400940223     C************************************************************
093500940223     C     DEFCAM        BEGSR
093600940223     C*
093700940223     C* klist
093800940207     C*
093900940223     C*
094000940223     C* Variabili per gestione videate
094100940223     C*
094200940223     C                   MOVE      *BLANK        $GEST             2
094300940223     C                   MOVE      *BLANK        $FINE             1
094400940223     C                   MOVE      *BLANK        $INZD1            1
094500940223     C                   MOVE      *BLANK        $LASTG            2
094600940223     C                   MOVE      *BLANK        $LASTV            2
094700940223     C*
094800940223     C* Variabili appoggio
094900940223     C*
095000940223     C                   Z-ADD     0             CURR              2 0
095100940223     C                   Z-ADD     0             CURC              2 0
095200940223     C                   MOVE      *ZEROS        WIN              99
095300940224     C                   MOVE      *BLANK        WTBERR            1
095400940223     C*
095500940223     C* Indici
095600940223     C*
095700940223     C                   Z-ADD     0             X                 3 0
095800940223     C                   Z-ADD     0             Y                 3 0
095900940127     C*
096000940117     C                   ENDSR
096100940128     C************************************************************
096200940509     C/EJECT
096300940509     C************************************************************
096400940509     C* CONTROLLO ABILITAZIONE TASTI
096500940509     C************************************************************
096600940509     C     CTRKEY        BEGSR
096700940509     C                   SELECT
096800940509     C     $TASTO        WHENEQ    F01
096900940720     C     $FC(1)        IFEQ      '0'
097000940509     C                   SETON                                        99
097100940509     C                   ENDIF
097200940509     C     $TASTO        WHENEQ    F02
097300940720     C     $FC(2)        IFEQ      '0'
097400940509     C                   SETON                                        99
097500940509     C                   ENDIF
097600940509     C     $TASTO        WHENEQ    F03
097700940720     C     $FC(3)        IFEQ      '0'
097800940509     C                   SETON                                        99
097900940509     C                   ENDIF
098000940509     C     $TASTO        WHENEQ    F04
098100940720     C     $FC(4)        IFEQ      '0'
098200940509     C                   SETON                                        99
098300940509     C                   ENDIF
098400940509     C     $TASTO        WHENEQ    F05
098500940720     C     $FC(5)        IFEQ      '0'
098600940509     C                   SETON                                        99
098700940509     C                   ENDIF
098800940509     C     $TASTO        WHENEQ    F06
098900940720     C     $FC(6)        IFEQ      '0'
099000940509     C                   SETON                                        99
099100940509     C                   ENDIF
099200940509     C     $TASTO        WHENEQ    F07
099300940720     C     $FC(7)        IFEQ      '0'
099400940509     C                   SETON                                        99
099500940509     C                   ENDIF
099600940509     C     $TASTO        WHENEQ    F08
099700940720     C     $FC(8)        IFEQ      '0'
099800940509     C                   SETON                                        99
099900940509     C                   ENDIF
100000940509     C     $TASTO        WHENEQ    F09
100100940720     C     $FC(09)       IFEQ      '0'
100200940509     C                   SETON                                        99
100300940509     C                   ENDIF
100400940509     C     $TASTO        WHENEQ    F10
100500940720     C     $FC(10)       IFEQ      '0'
100600940509     C                   SETON                                        99
100700940509     C                   ENDIF
100800940509     C     $TASTO        WHENEQ    F11
100900940720     C     $FC(11)       IFEQ      '0'
101000940509     C                   SETON                                        99
101100940509     C                   ENDIF
101200940509     C     $TASTO        WHENEQ    F12
101300940720     C     $FC(12)       IFEQ      '0'
101400940509     C                   SETON                                        99
101500940509     C                   ENDIF
101600940509     C     $TASTO        WHENEQ    F13
101700940720     C     $FC(13)       IFEQ      '0'
101800940509     C                   SETON                                        99
101900940509     C                   ENDIF
102000940509     C     $TASTO        WHENEQ    F14
102100940720     C     $FC(14)       IFEQ      '0'
102200940509     C                   SETON                                        99
102300940509     C                   ENDIF
102400940509     C     $TASTO        WHENEQ    F15
102500940720     C     $FC(15)       IFEQ      '0'
102600940509     C                   SETON                                        99
102700940509     C                   ENDIF
102800940509     C     $TASTO        WHENEQ    F16
102900940720     C     $FC(16)       IFEQ      '0'
103000940509     C                   SETON                                        99
103100940509     C                   ENDIF
103200940509     C     $TASTO        WHENEQ    F17
103300940720     C     $FC(17)       IFEQ      '0'
103400940509     C                   SETON                                        99
103500940509     C                   ENDIF
103600940509     C     $TASTO        WHENEQ    F18
103700940720     C     $FC(18)       IFEQ      '0'
103800940509     C                   SETON                                        99
103900940509     C                   ENDIF
104000940509     C     $TASTO        WHENEQ    F19
104100940720     C     $FC(19)       IFEQ      '0'
104200940509     C                   SETON                                        99
104300940509     C                   ENDIF
104400940509     C     $TASTO        WHENEQ    F20
104500940720     C     $FC(20)       IFEQ      '0'
104600940509     C                   SETON                                        99
104700940509     C                   ENDIF
104800940509     C     $TASTO        WHENEQ    F21
104900940720     C     $FC(21)       IFEQ      '0'
105000940509     C                   SETON                                        99
105100940509     C                   ENDIF
105200940509     C     $TASTO        WHENEQ    F22
105300940720     C     $FC(22)       IFEQ      '0'
105400940509     C                   SETON                                        99
105500940509     C                   ENDIF
105600940509     C     $TASTO        WHENEQ    F23
105700940720     C     $FC(23)       IFEQ      '0'
105800940509     C                   SETON                                        99
105900940509     C                   ENDIF
106000940509     C     $TASTO        WHENEQ    F24
106100940720     C     $FC(24)       IFEQ      '0'
106200940509     C                   SETON                                        99
106300940509     C                   ENDIF
106400940509     C                   ENDSL
106500940509     C                   ENDSR
106600940509     C/EJECT
106700940509     C************************************************************
106800940509     C* RIEMPIMENTO TASTI FUNZIONE
106900940509     C************************************************************
107000940509     C     RIEKEY        BEGSR
107100940509     C*
107200940509     C* Input :
107300940509     C* - $FX = schiera con tutti i testi disponibili
107400940509     C* - $FC = schiere con i flag di validità associati ai testi
107500940509     C* - $FR = schiere con i flag di REVERSE IMAGE
107600940509     C* - $ULK = ultimo testo caricato
107700940509     C* Output :
107800940509     C* - $ULK
107900940509     c* - $KEY1= 1° riga di tasti funzione
108000940509     c* - $KEY2= 2° riga di tasti funzione
108100940509     C*
108200970214     C                   CALLB     'XKEYMSG'
108300940509     C* input
108400940509     C                   PARM                    $FX
108500940509     C                   PARM                    $FC
108600940509     C                   PARM                    $FR
108700940509     C                   PARM                    $ULK              3 0
108800940509     C* output
108900940509     C                   PARM                    $KEY1            79
109000940509     C                   PARM                    $KEY2            79
109100940509     C*
109200940509     C                   ENDSR
109300940615**
10940094022401PROMSG    *LIBL     COS0001    immissione
10950094022402PROMSG    *LIBL     COS0002    modifica
10960094022403PROMSG    *LIBL     COS0003    copia
10970094022404PROMSG    *LIBL     COS0004    annullamento
10980094022405PROMSG    *LIBL     COS0005    visualizzazione
109900940509** TASTI DI FUNZIONE  D1
110000940509PROMSG    *LIBL     KEY0001  01
110100940509PROMSG    *LIBL     KEY0002  02
110200940509PROMSG    *LIBL     KEY0003  03
110300940509PROMSG    *LIBL     KEY0004  04
110400940509PROMSG    *LIBL     KEY0005  05
110500940509PROMSG    *LIBL     KEY0006  06
110600940509PROMSG    *LIBL     KEY0007  07
110700940509PROMSG    *LIBL     KEY0008  08
110800940509PROMSG    *LIBL     KEY0009  09
110900940509PROMSG    *LIBL     KEY0010  10
111000940509PROMSG    *LIBL     KEY0011  11
111100940509PROMSG    *LIBL     KEY0012  12
111200940509PROMSG    *LIBL     KEY0013  13
111300940509PROMSG    *LIBL     KEY0014  14
111400940509PROMSG    *LIBL     KEY0015  15
111500940509PROMSG    *LIBL     KEY0016  16
111600940509PROMSG    *LIBL     KEY0017  17
111700940509PROMSG    *LIBL     KEY0018  18
111800940509PROMSG    *LIBL     KEY0019  19
111900940509PROMSG    *LIBL     KEY0020  20
112000940509PROMSG    *LIBL     KEY0021  21
112100940509PROMSG    *LIBL     KEY0022  22
112200940509PROMSG    *LIBL     KEY0023  23
112300940509PROMSG    *LIBL     KEY0024  24
112400940509**  ABILITAZIONE D1
1125009407200N  01
1126009407200N  02
1127009407201N  03
1128009407201N  04
1129009407201N  05
1130009407201N  06
1131009407201N  07
1132009407201N  08
1133009407200N  09
1134009407200N  10
1135009407200N  11
1136009407201N  12
1137009407200N  13
1138009407200N  14
1139009407200N  15
1140009407200N  16
1141009407200N  17
1142009407200N  18
1143009407200N  19
1144009407200N  20
1145009407200N  21
1146009407200N  22
1147009407200N  23
1148009407200N  24
