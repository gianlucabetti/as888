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
004100940614$001 FANGY6BD2  CF   E             WORKSTN
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
006000940614$003 D EXTDS         E DS                  EXTNAME(ANGY6BDS) INZ
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
017500981117$005 I              D1PER                       §6BPER
017600981117$005 I              D1FIM                       §6BFIM
017700981117$005 I              D1KCA                       §6BKCA
017800981117$005 I              D1KSA                       §6BKSA
017900981117$005 I              D1VCA                       §6BVCA
018000981117$005 I              D1KCC                       §6BKCC
018100981117$005 I              D1KSC                       §6BKSC
018200981117$005 I              D1VCC                       §6BVCC
018300981117$005 I              D1ENT                       §6BENT
018400981117$005 I              D1CAU                       §6BCOD
018500940127     I*
018600940127     C*----------------------------------------------------*
018700940127     C*                MAIN LINE PROGRAM
018800940127     C*----------------------------------------------------*
018900940127     C*
019000940223     C* inizializzazione variabili
019100940223     C                   EXSR      INZVAR
019200940223     C*
019300940223     C     $FINE         DOWEQ     *OFF
019400940202     C     $GEST         CASEQ     'D1'          GESD1
019500940117     C                   END
019600940117     C                   END
019700940201     C*
019800940325     C* fine programma
019900940325     C                   EXSR      ENDPGM
020000940325     C/EJECT
020100940325     C************************************************************
020200940325     C* RICERCHE
020300940325     C************************************************************
020400940325     C     ENDPGM        BEGSR
020500940325     C*
020600940614     C* disallocazione rcd nel caso di
020700971110$007 C                   MOVE      'Y6B'         XTBCOD
020800940614    >C                   MOVE      '7'           XTBRIC
020900970214     C                   CALLB     'XATB'
021000940614     C                   PARM                    XATBDS
021100940325     C*
021200940325     C* passaggio dati al pgm chiamante
021300940715     C                   MOVE      *BLANKS       KPJBU
021400940325    >C                   MOVEL     D002DS        KPJBU
021500940325     C*
021600940325     C                   SETON                                            RT
021700940325     C*
021800940325     C                   ENDSR
021900940325     C/EJECT
022000940201     C************************************************************
022100940201     C* RICERCHE
022200940201     C************************************************************
022300940127     C     SEARCH        BEGSR
022400940127     C*
022500940127     C* determino Riga/Colonna del cursore
022600940127     C                   MOVE      NRG           R$$
022700940127     C                   MOVE      NCL           C$$
022800940127     C                   Z-ADD     RIRI          CURR
022900940127     C                   Z-ADD     COCO          CURC
023000940127     C*
023100940224     C* I campi che al momento dell'F4 risultano protetti non devono
023200940224     C* essere abilitati alla funzione di ricerca
023300940224     C*
0234009401271    C                   SELECT
023500940127     C*
023600940315     C* FMTD1  - PACCHETTO
023700940614    >C*          H1NMFL    WHEQ 'D1PKG'
023800940207     C* imposto pos. del cursore
023900940614     C*                    Z-ADDCURR      H1RIGA
024000940614     C*                    Z-ADDCURC      H1COLO
024100940127     C*
024200940223     C*
024300940223     C* ricerca non abilitata in questa posizione
0244009402231O   C                   OTHER
024500941005     C                   SETON                                        9899
024600941005     C* imposto pos. del cursore
024700941005     C                   Z-ADD     CURR          H1RIGA
024800941005     C                   Z-ADD     CURC          H1COLO
024900940127     C*
0250009401271-   C                   ENDSL
025100940127     C*
025200940506     C                   MOVE      *HIVAL        $LASTV
025300940127     C                   ENDSR
025400940615     C/EJECT
025500940615     C************************************************************
025600940615     C* TRADUZIONI
025700940615     C************************************************************
025800940615     C     LINGUA        BEGSR
025900940615     C*
026000940615     C* determino Riga/Colonna del cursore
026100940615     C                   MOVE      NRG           R$$
026200940615     C                   MOVE      NCL           C$$
026300940615     C                   Z-ADD     RIRI          CURR
026400940615     C                   Z-ADD     COCO          CURC
026500940615     C*
026600940615     C*
026700940615     C*
026800940615     C* FMTD1  - CAMPI DA TRADURRE
026900940615     C* imposto pos. del cursore
027000940615     C                   Z-ADD     CURR          H1RIGA
027100940615     C                   Z-ADD     CURC          H1COLO
027200940621     C                   MOVEL     H1NMFL        W005              5
027300940621     C                   MOVE      W005          W003              3
027400940621     C                   MOVEL     W003          W006              6
027500940620     C                   MOVE      *ZERO         KEY              15
027600940616     C                   MOVE      D1COD         KEY
027700940615     C                   SELECT
027800940615     C*
027900940615     C*
028000940615     C* ricerca non abilitata in questa posizione
0281009406151O   C                   OTHER
028200940615     C                   SETON                                        98
028300940615     C*
0284009406151-   C                   ENDSL
028500940615     C*
028600940615     C                   MOVE      *HIVAL        $LASTV
028700940615     C                   ENDSR
028800940225     C/EJECT
028900940225     C************************************************************
029000940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
029100940225     C************************************************************
029200940225     C     GESPIE        BEGSR
029300940225     C*
029400940225     C                   SELECT
029500940225     C*
029600940506     C     $GEST         WHENEQ    'D1'
029700940506     C     $ULKD1        IFEQ      0
029800940506     C* riempimento opzioni
029900940506     C                   EXSR      F24D1
030000940506     C                   ELSE
030100940614     C                   WRITE     Z1
030200940506     C                   ENDIF
030300940225     C*
030400940510     C*
030500940225     C                   ENDSL
030600940225     C*
030700940225     C                   ENDSR
030800940131     C/EJECT
030900940131     C************************************************************
031000940510     C* GESTIONE VIDEO RECORD D1
031100940131     C************************************************************
031200940127     C     GESD1         BEGSR
031300940225     C* inizializzazione videata
031400940223     C     $INZD1        IFEQ      *ON
031500940127     C                   EXSR      INZD1
031600940223     C                   MOVE      *OFF          $INZD1
031700940117     C                   END
031800940225     C* emissione piede videata se proveniente da altra
031900940225     C* salvataggio valore formato di provenienza
032000940207     C     $LASTV        IFNE      'D1'
032100940614     C                   WRITE     T1
032200940225     C                   EXSR      GESPIE
032300940202     C                   MOVE      $LASTV        $LASTG
032400940202     C                   MOVE      'D1'          $LASTV
032500940202     C                   END
032600940131     C*
032700940203     C                   SELECT
032800940203     C* Annullamento
032900940315    >C     OPZ002        WHENEQ    '04'
033000940224     C* Visualizzazione
033100940315    >C     OPZ002        OREQ      '05'
033200940620     C                   SETON                                        0203
033300940620     C* Manutenzione
033400940620    >C     OPZ002        WHENEQ    '02'
033500940620     C                   SETON                                        02
033600940620     C                   SETOFF                                       03
033700940620     C*
033800940620     C                   OTHER
033900940620     C*
034000940620     C                   SETOFF                                       0203
034100940203     C                   ENDSL
034200940620     C* Immissione/Modifica/Copia
034300940620     C                   EXFMT     D1
034400940506     C* controllo abilitazione tasti funzione
034500940506     C*
034600940506     C                   EXSR      CTRKD1
034700940506     C*
034800940506     C     *IN99         IFEQ      *OFF
034900940131     C*
0350009402031    C                   SELECT
035100940510     c* ROLLUP
035200940131     C* F3=Fine
035300940506     C     $TASTO        WHENEQ    F03
035400940309     C                   EXSR      F03D1
035500940131     C* F4=Ricerca
035600940506     C     $TASTO        WHENEQ    F04
035700940127     C                   EXSR      SEARCH
035800940224     C* F5=Refresh
035900940506     C     $TASTO        WHENEQ    F05
036000940309     C                   EXSR      F05D1
036100940223     C* F8=Successivo
036200940506     C     $TASTO        WHENEQ    F08
036300940309     C                   EXSR      F08D1
036400940131     C* F12=Ritorno
036500940506     C     $TASTO        WHENEQ    F12
036600940309     C                   EXSR      F12D1
036700940315     C* F24=Altri tasti
036800940506     C     $TASTO        WHENEQ    F24
036900940315     C                   EXSR      F24D1
037000940131     C*
0371009402031O   C                   OTHER
037200940127     C                   EXSR      CTRD1
037300940616     C     *IN99         IFEQ      *OFF
037400940616     C                   SELECT
037500940616     C* F7=Traduzione
037600940616     C     $TASTO        WHENEQ    F07
037700940616     C                   EXSR      LINGUA
037800940616     C* F6
0379009406162    C     $TASTO        WHENEQ    F06
038000940224     C* eseguo aggiornamento
038100940315    >C                   EXSR      AGGANA
038200940224     C* eseguo operazioni del dopo-aggiornamento
0383009403183    C     *IN99         IFEQ      *OFF
038400940318     C                   EXSR      GESAGG
0385009403183-   C                   ENDIF
0386009406162-   C                   ENDSL
038700940616     C                   ENDIF
038800940203     C*
0389009402241-   C                   ENDSL
039000940224     C*
0391009405062-   C                   ENDIF
039200940117     C                   ENDSR
039300940506     C/EJECT
039400940506     C************************************************************
039500940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
039600940506     C************************************************************
039700940506     C     CTRKD1        BEGSR
039800940506     C                   SETOFF                                       99
039900940506     C                   MOVEA     $FC1          $FC
040000940506     C                   EXSR      CTRKEY
040100940506     C     *IN99         IFEQ      *ON
040200940506     C                   SETON                                        97
040300940506     C                   ENDIF
040400940506     C                   ENDSR
040500940117      ************************************************************
040600940207      * INIZIALIZZAZIONE VIDEATA DATI
040700940117      ************************************************************
040800940127     C     INZD1         BEGSR
040900940131     C*
041000940615     C                   CLEAR                   D1
041100941006     C                   MOVE      '0'           D1RIS
041200940616     C                   MOVE      KEY002        D1COD
041300940615     C                   MOVE      AZI002        XTBAZI
041400940615     C                   MOVE      LIN002        XTBLIN
041500940616     C                   MOVE      *ALL'0'       XTBKEY
041600940615     C                   MOVE      KEY002        XTBKEY
041700970509     C                   MOVE      UNI002        XTBUNT
041800940615$007 C                   MOVE      'Y6B'         XTBCOD
041900940225     C*
042000940207     C*
042100940224     C* valorizzo la chiave univoca per reperire i dati
042200940224     C* se non immissione
0423009403151   >C     OPZ002        IFNE      '01'
042400940315    >C*-------------------------------------------------------------
042500940315    >C* impostare i campi chiave ricevuti
042600940315    >C*-------------------------------------------------------------
042700940318     C*
042800940614     C*
042900940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0430009406142   >C     OPZ002        IFEQ      '02'
043100940315    >C     OPZ002        OREQ      '04'
043200940614     C                   MOVE      '1'           XTBALC
043300940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0434009406142E   C                   ELSE
043500940614     C                   MOVE      '0'           XTBALC
0436009406142-   C                   ENDIF
043700940614     C*
043800940616     C                   MOVE      '1'           XTBRIC
043900970214     C                   CALLB     'XATB'
044000940614     C                   PARM                    XATBDS
044100940318     C*
044200940614     C     XTBERR        IFEQ      '0'
044300940614     C                   MOVEL     XTBUNI        EXTDS
044400970512     c                   move      xtbunt        d1uniTA           8
044500941108     C* Giro la data se non blank
044600970214     c                   move      xtbdsc        xtbdscA          10
044700970214     C     XTBDSCa       IFNE      DATA
044800970214     C                   CALLB     'XDT4000'
044900970214     C                   PARM      XTBDSCA       XDTAMG           10
045000940621     C                   PARM      *ZEROS        XDTGMA            6 0
045100940621     C                   PARM      2             XDTSTA            1 0
045200940621     C                   MOVE      XDTGMA        D1DSC
045300940712     C                   MOVE      XTBRIS        D1RIS
045400941108     C                   ELSE
045500941108     C                   MOVE      *ZEROS        D1DSC
045600941108     C                   ENDIF
045700941108     C                   ENDIF
045800940614     C*
045900940224     C*
046000940224     C* se il rcd da gestire non viene trovato
046100940318     C* oppure è allocato da altro pgm
046200940224     C* ritorno al pgm guida avvertendolo dell'errore
0463009406142    C     XTBERR        IFNE      '0'
046400940224     C                   MOVE      *ON           $FINE
046500940315    >C                   MOVE      '1'           ERR002
046600940224     C*
046700940224     C* altrimenti
0468009402242E   C                   ELSE
046900940224     C* riempio la videata
047000940224     C                   EXSR      RIED1
047100940224     C* richiamo routine dei ctrl per decodificarla
047200940614    >C                   EXSR      CTRD1
047300960228     C                   MOVEL     *IN99         SAV99             1
047400960228     C* Se la gestione è più videate
047500960305    >C                   EXSR      CTRD2
047600960305    >C     *IN99         IFEQ      *ON
047700960305    >C                   MOVEL     *IN99         SAV99             1
047800960305    >C                   ENDIF
047900940224     C                   MOVE      *ALL'0'       IN5098
048000940224     C                   MOVEA     IN5098        *IN(50)
048100940224     C* definizione indicatori su video
048200940224     C                   SETOFF                                       02
0483009402243    C                   SELECT
048400940315    >C     OPZ002        WHENEQ    '02'
048500940224     C                   SETON                                        02
048600940315    >C     OPZ002        WHENEQ    '05'
048700940506     C* disabilita tasti
048800940720     C                   MOVE      '0'           $FC(6)
048900940720     C                   MOVE      '0'           $FC(5)
049000940720     C                   MOVE      '0'           $FC(4)
0491009402243-   C                   ENDSL
049200940224     C*
0493009402242-   C                   ENDIF
049400940223     C*
0495009402241-   C                   ENDIF
049600940224     C*
049700940117     C                   ENDSR
049800940207     C/EJECT
049900940207     C************************************************************
050000940510     C* RIEMPIMENTO VIDEATA  D1
050100940207     C************************************************************
050200940207     C     RIED1         BEGSR
050300940207     C*
050400940207     C*
050500940207     C                   ENDSR
050600940309     C/EJECT
050700940309     C************************************************************
050800940309     C* GESTIONE F03 VIDEO D1
050900940309     C************************************************************
051000940309     C     F03D1         BEGSR
051100940309     C*
051200940309     C                   MOVE      *ON           $FINE
051300940315    >C                   MOVE      '1'           RET002
051400940325     C* fine programma
051500940325     C                   EXSR      ENDPGM
051600940325     C*
051700940325     C                   ENDSR
051800940309     C/EJECT
051900940309     C************************************************************
052000940309     C* GESTIONE F05 VIDEO D1
052100940309     C************************************************************
052200940309     C     F05D1         BEGSR
052300940309     C*
052400940309     C                   MOVE      *ON           $INZD1
052500940309     C*
052600940309     C                   ENDSR
052700940309     C/EJECT
052800940309     C************************************************************
052900940309     C* GESTIONE F08 VIDEO D1
053000940309     C************************************************************
053100940309     C     F08D1         BEGSR
053200940309     C*
053300940309     C                   MOVE      *ON           $FINE
053400940315    >C                   MOVE      '0'           RET002
053500940309     C*
053600940309     C                   ENDSR
053700940309     C/EJECT
053800940309     C************************************************************
053900940309     C* GESTIONE F12 VIDEO D1
054000940309     C************************************************************
054100940309     C     F12D1         BEGSR
054200940309     C*
054300940309     C                   MOVE      *ON           $FINE
054400940315    >C                   MOVE      '2'           RET002
054500940309     C*
054600940309     C                   ENDSR
054700940309     C/EJECT
054800940309     C************************************************************
054900940309     C* GESTIONE F24 VIDEO D1
055000940309     C************************************************************
055100940309     C     F24D1         BEGSR
055200940309     C*
055300940506     C*
055400940506     C                   MOVEA     $FX1          $FX
055500940506     C                   MOVEA     $FC1          $FC
055600940506     C                   MOVEA     $FR1          $FR
055700940506     C                   Z-ADD     $ULKD1        $ULK
055800940506     C                   EXSR      RIEKEY
055900941017     C                   MOVEA     $FC           $FC1
056000940506     C                   Z-ADD     $ULK          $ULKD1            3 0
056100940506     C                   MOVEL     $KEY1         Z1KE1
056200940506     C                   MOVEL     $KEY2         Z1KE2
056300940614     C                   WRITE     Z1
056400940506     C*
056500940309     C                   ENDSR
056600940207     C/EJECT
056700940131     C************************************************************
056800940207     C* CONTROLLO VIDEATA
056900940131     C************************************************************
057000940127     C     CTRD1         BEGSR
057100940131     C*
057200940127     C                   SETOFF                                       99
057300940127     C* non imposto pos. del cursore
057400940127     C                   Z-ADD     0             H1RIGA
057500940127     C                   Z-ADD     0             H1COLO
057600940621     C*
057700940621     C* CTRL DATA
057800940621     C*
057900941109     C                   MOVE      DATA          WRKDSC           10
058000940621     C     D1DSC         IFNE      0
058100970214     C                   CALLB     'XDT4000'
058200941109     C                   PARM                    XDTAMG           10
058300940621     C                   PARM      D1DSC         XDTGMA            6 0
058400940621     C                   PARM      3             XDTSTA            1 0
058500940621     C     XDTSTA        IFNE      0
058600940621     C                   SETON                                        5599
058700940621     C                   ELSE
058800941108     C                   MOVE      XDTAMG        WRKDSC
058900940621     C                   ENDIF
059000940621     C                   ENDIF
059100940223     C*
059200940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
059300940223     C*
0594009403151   >C     OPZ002        IFNE      '04'
059500971110     C*
059600971110     C* controllo codice vuoto
059700971110$017 C     D1COD         IFEQ      *ALL' '
059800971110     C                   SETON                                        5099
059900971110     C                   ENDIF
060000971110     C*
060100940207     C*
060200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
060300940315    >C     OPZ002        IFEQ      '01'
060400940315    >C     OPZ002        OREQ      '03'
060500940315    >C*-------------------------------------------------------------
060600940315    >C* impostare i campi chiave e verificare l'esistenza del record
060700940315    >C*-------------------------------------------------------------
060800971110$007 C                   MOVE      'Y6B'         XTBCOD
060900940614     C                   MOVE      *ALL'0'       XTBKEY
061000940620    >C                   MOVE      D1COD         XTBKEY
061100970512    >C                   MOVE      D1UNITA       XTBUNT
061200940614     C                   MOVE      '0'           XTBALC
061300940614     C                   MOVE      '1'           XTBRIC
061400970214    >C                   CALLB     'XATB'
061500940614     C                   PARM                    XATBDS
061600940614     C     XTBERR        IFEQ      '0'
061700940223     C                   SETON                                        5999
061800940223     C                   ENDIF
061900940620     C* Controllo se compreso nei range indice
062000940620     C     XTBERR        IFEQ      '3'
062100940620    >C                   SETON                                        5899
062200940620     C                   END
062300940620     C*
062400940620     C* REPERISCO AUTORIZZAZIONI UTENTE
062500940620     C                   CLEAR                   XCHATB
062600941118     C                   MOVE      XSCSOC        XCTSOC
062700941118     C                   MOVE      XSCDUN        XCTUNI
062800940620     C                   MOVEL     KNMUS         XCTPRF
062900940620     C                   MOVE      XTBCOD        XCTCOD
063000940620     C                   CALL      'XCHKATB'
063100940620     C                   PARM                    XCHATB
063200941111     C                   MOVEL     XCTDDS        AUTDS
063300941027     C     XCTRTN        IFEQ      1
063400941027     C                   MOVEL     'N'           MANGRU
063500941027     C                   ENDIF
063600940620     C* Non ammessa immissione di gruppo
063700940620     C     XTBGRU        IFEQ      'G'
063800940620     C     MANGRU        ANDEQ     'N'
063900940620     C                   SETON                                        5799
064000940620     C                   END
064100940620     C* Non ammessa immissione di azienda
064200940620     C     XTBGRU        IFEQ      'A'
064300940620     C     MANAZI        ANDEQ     'N'
064400940620     C                   SETON                                        5699
064500940620     C                   END
064600940712     C* codice riservato non accessibile
064700941006     C     XTBRIS        IFGT      ABIRIS
064800941005    >C     OPZ002        ANDNE     '01'
064900941005    >C     OPZ002        ANDNE     '03'
065000940712     C                   SETON                                        5499
065100940712     C                   END
065200940224     C                   ENDIF
065300940224     C*
0654009402241-   C                   END
065500940131     C*
065600940302     C     *IN99         IFEQ      *ON
065700940510     C     $GEST         ANDEQ     'D1'
065800960228     C     OPZ002        ANDNE     '07'
065900940117     C                   MOVEA     *IN           WIN
066000940128     C                   MOVE      *ALL'0'       IN5098           49
066100940128     C                   MOVEA     IN5098        *IN(50)
066200940614     C                   WRITE     D1
066300940117     C                   MOVEA     WIN           *IN
066400940302     C                   ENDIF
066500940127     C*
066600940117     C                   ENDSR
066700960305     C/EJECT
066800960305     C************************************************************
066900960305     C* CONTROLLO VIDEATA D2
067000960305     C************************************************************
067100960305     C     CTRD2         BEGSR
067200960305     C*
067300960305     C                   ENDSR
067400940207     C/EJECT
067500940224     C************************************************************
067600940315    >C* AGGIORNAMENTO ANAGRAFICA
067700940224     C************************************************************
067800940315    >C     AGGANA        BEGSR
067900971110     C*
068000971110$007 C                   MOVE      'Y6B'         XTBCOD
068100940614     C                   MOVE      *ALL'0'       XTBKEY
068200940614     C                   MOVE      D1COD         XTBKEY
068300970512     C                   MOVE      D1UNITA       XTBUNT
068400940207     C*
068500940207     C                   SELECT
068600940315    >C     OPZ002        WHENEQ    '01'
068700940315    >C     OPZ002        OREQ      '03'
068800940315    >C                   EXSR      RIEANA
068900950609     C                   MOVE      XSCCUT        XTBUTE
069000940614     C                   MOVE      '5'           XTBRIC
069100970214     C                   CALLB     'XATB'
069200940614    >C                   PARM                    XATBDS
069300940318     C* se rcd non scrivibile attivo errore
069400940615     C     XTBERR        IFEQ      '1'
069500940318    >C                   SETON                                        5999
069600940318     C                   END
069700940315    >C     OPZ002        WHENEQ    '02'
069800940621     C                   MOVE      '1'           XTBRIC
069900940621     C                   MOVE      '1'           XTBALC
070000970214     C                   CALLB     'XATB'
070100940621     C                   PARM                    XATBDS
070200940622    >C                   EXSR      RIEANA
070300950609     C                   MOVE      XSCCUT        XTBUTE
070400940614     C                   MOVE      '4'           XTBRIC
070500970214     C                   CALLB     'XATB'
070600940614    >C                   PARM                    XATBDS
070700940315    >C     OPZ002        WHENEQ    '04'
070800940621     C                   MOVE      '1'           XTBRIC
070900940621     C                   MOVE      '1'           XTBALC
071000970214     C                   CALLB     'XATB'
071100940621     C                   PARM                    XATBDS
071200940614     C                   MOVE      '6'           XTBRIC
071300970214     C                   CALLB     'XATB'
071400940614    >C                   PARM                    XATBDS
071500940621     C* annullo traduzioni
071600940621     C     XTBERR        IFEQ      '0'
071700940621     C                   EXSR      ANNTBT
071800940621     C                   END
071900940621     C*
072000940207     C                   ENDSL
072100940203     C*
072200940203     C                   ENDSR
072300940224     C/EJECT
072400940224     C************************************************************
072500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
072600940224     C************************************************************
072700940616     C     RIEANA        BEGSR
072800940616     C                   CLEAR                   XTBUNI
072900940621     C                   MOVE      WRKDSC        XTBDSC
073000940712     C                   MOVE      D1RIS         XTBRIS
073100940616     C                   MOVEL     EXTDS         XTBUNI
073200940207     C                   ENDSR
073300940224     C/EJECT
073400940224     C************************************************************
073500940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
073600940224     C************************************************************
073700940224     C     GESAGG        BEGSR
073800940224     C*
073900940224     C* segnalo al pgm chiamante l'avvenuta conferma
074000940315    >C                   MOVE      *ON           OPR002
074100940224     C* segnalo al pgm chiamante l'aver premuto l'F6
074200940315    >C                   MOVE      '0'           RET002
074300940224     C*
0744009402241    C                   SELECT
074500940224     C* nel caso di immissione, ripristino la videata iniziale
074600940315    >C     OPZ002        WHENEQ    '01'
074700940224     C                   MOVE      *ON           $INZD1
074800940224     C* altrimenti ritorno al pgm chiamante
074900940224     C                   OTHER
075000940224     C                   MOVE      *ON           $FINE
0751009402241-   C                   ENDSL
075200940224     C*
075300940224     C                   ENDSR
075400940621     C/EJECT
075500940621     C************************************************************
075600940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
075700940621     C************************************************************
075800940621    >C     ANNTBT        BEGSR
075900940621     C                   MOVE      XTBAZI        TBTAZI
076000940621     C                   MOVE      XTBCOD        TBTCOD
076100940621     C                   MOVE      XTBKEY        TBTKEY
076200940621     C     KTBT          SETLL     ANTBT01L
076300940621     C                   DO        *HIVAL
076400940621     C     KTBT          DELETE    ANTBT01L                           21
076500940621     C  N21              END
076600940621     C*
076700940621     C                   ENDSR
076800940207     C/EJECT
076900940131     C************************************************************
077000940131     C* OPERAZIONI INIZIALI
077100940131     C************************************************************
077200940131     C     *INZSR        BEGSR
077300940131     C*
077400940127     C* Reperimento parametri
077500940127     C*
077600940117     C     *ENTRY        PLIST
077700940117     C                   PARM                    KPJBA
077800940712     C*
077900940712     C* Reperimento Codice utente
078000940712     C*
078100940223     C*
078200940506     C*
078300940506     C* Reperimento tasti di funzione
078400940506     C*
078500940506     C                   EXSR      REPKEY
078600940621     C*
078700940621     C* KLIST
078800940621     C*
078900940621     C     KTBT          KLIST
079000940621     C                   KFLD                    TBTAZI
079100940621     C                   KFLD                    TBTCOD
079200940621     C                   KFLD                    TBTKEY
079300940506     C*
079400940223     C                   ENDSR
079500941103     C/EJECT
079600941103      ************************************************************
079700941103      * Reperimento dati società
079800941103      ************************************************************
079900941103     C     REPSOC        BEGSR
080000941103      *
080100970214     C                   CALLB     'XSOC'
080200941103     C                   PARM                    TIPXSC            6
080300941103     C                   PARM                    SOCXSC            3
080400941103     C                   PARM                    CDSXSC            9 0
080500941103     C                   PARM                    MODXSC            3
080600941103     C                   PARM      *BLANKS       RTNXSC            1
080700941103     C                   PARM                    XSOCDS
080800941103     C                   PARM                    KPJBA
080900941103      *
081000941103     C                   ENDSR
081100940223     C/EJECT
081200940506     C************************************************************
081300940506     C* REPERIMENTO TASTI DI FUNZIONE
081400940506     C************************************************************
081500940506     C     REPKEY        BEGSR
081600940506     C                   MOVEA     $FM1          $FM
081700940506     C                   EXSR      REPKKK
081800940506     C                   MOVEA     $FX           $FX1
081900940614     C                   ENDSR
082000940506     C/EJECT
082100940506     C************************************************************
082200940506     C* REPERIMENTO TASTI DI FUNZIONE
082300940506     C************************************************************
082400940506     C     REPKKK        BEGSR
082500940506     C*
082600940506     C                   CLEAR                   $FX
082700940506     C     1             DO        24            X
082800940506     C*
082900940506     C     $FM(X)        IFNE      *BLANK
083000940506     C*
083100940506     C                   MOVEL     $FM(X)        IDMSG
083200970214     C                   CALLB     'XRTVM'
083300940506     C                   PARM                    IDMSG            27
083400940506     C                   PARM                    TXTMSG          100
083500940506     C                   PARM                    ERRMSG            1
083600940506     C*
083700940506     C     ERRMSG        IFNE      *ON
083800940506     C                   MOVEL     TXTMSG        $FX(X)
083900940506     C                   ELSE
084000940506     C                   MOVEL     *ALL' '       $FX(X)
084100940506     C                   END
084200940506     C*
084300940506     C                   END
084400940506     C*
084500940506     C                   END
084600940506     C*
084700940506     C                   ENDSR
084800940509     C/EJECT
084900940223     C************************************************************
085000940223     C* INIZIALIZZAZIONE VARIABILI
085100940223     C************************************************************
085200940223     C     INZVAR        BEGSR
085300951124     C*
085400951124     C* Chiamata XSOC
085500951124     C*
085600951124     C                   MOVEL     'SOC001'      TIPXSC
085700951124     C                   EXSR      REPSOC
085800951124     C     RTNXSC        IFNE      '1'
085900951124     C                   MOVEL     XSOCDS        SOC001
086000951124     C                   ELSE
086100951124     C                   MOVE      *ON           $FINE
086200951124     C* fine programma
086300951124     C                   EXSR      ENDPGM
086400951124     C                   ENDIF
086500940224     C*
086600940224     C* Dati da PGM guida
086700940224     C*
086800940315    >C                   MOVEL     KPJBU         D002DS
086900940128     C*
087000940223     C* Variabili per gestione videate
087100940128     C*
087200940223     C                   MOVE      'D1'          $GEST
087300940223     C                   MOVE      *OFF          $FINE
087400940223     C                   MOVE      *ON           $INZD1
087500940224     C                   MOVE      *BLANK        $LASTG
087600940224     C                   MOVE      *BLANK        $LASTV
087700940506     C                   Z-ADD     0             $ULKD1            3 0
087800940127     C*
087900940223     C* Variabili appoggio
088000940127     C*
088100940224     C                   Z-ADD     0             CURR
088200940224     C                   Z-ADD     0             CURC
088300940207     C*
088400940207     C* Valorizzazione campi univoci testate
088500940207     C*
088600940614     C                   CLEAR                   T1
088700940614     C                   MOVEL     KNSIF         NOMSIF
088800941109     C                   MOVEL     XSCDSI        NOMDIT
088900941027     C                   MOVEL     DSPGM         NOMPGM
089000940224     C                   Z-ADD     1             X
089100940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
089200940224     C     *IN21         IFEQ      *ON
089300940224     C                   MOVEL     $ID(X)        IDMSG
089400970214     C                   CALLB     'XRTVM'
089500940224     C                   PARM                    IDMSG            27
089600940224     C                   PARM                    TXTMSG          100
089700940224     C                   PARM                    ERRMSG            1
089800940224     C                   PARM      *ON           CTRMSG            1
089900940224     C                   PARM      30            LENMSG            3 0
090000940224     C     ERRMSG        IFNE      *ON
090100940224     C                   MOVEL     TXTMSG        T1OPE
090200940224     C                   ELSE
090300940224     C                   MOVEL     *ALL'?'       T1OPE
090400940224     C                   ENDIF
090500940224     C                   END
090600940224     C*
090700940224     C* Inizializzazione/Decodifica prima videata
090800940224     C*
090900940224     C                   EXSR      INZD1
091000940224     C                   MOVE      *OFF          $INZD1
091100960228     C* Se opzione = '07' richiesti solo controlli
091200960228     C     OPZ002        IFEQ      '07'
091300960228     C                   MOVEL     *IN99         ERR002
091400960228     C                   MOVEL     '0'           RET002
091500960228     C                   MOVEL     *ON           $FINE
091600960228     C                   ENDIF
091700940127     C*
091800940117     C                   ENDSR
091900940128     C************************************************************
092000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
092100940223     C************************************************************
092200940223     C     DEFCAM        BEGSR
092300940223     C*
092400940223     C* klist
092500940207     C*
092600940223     C*
092700940223     C* Variabili per gestione videate
092800940223     C*
092900940223     C                   MOVE      *BLANK        $GEST             2
093000940223     C                   MOVE      *BLANK        $FINE             1
093100940223     C                   MOVE      *BLANK        $INZD1            1
093200940223     C                   MOVE      *BLANK        $LASTG            2
093300940223     C                   MOVE      *BLANK        $LASTV            2
093400940223     C*
093500940223     C* Variabili appoggio
093600940223     C*
093700940223     C                   Z-ADD     0             CURR              2 0
093800940223     C                   Z-ADD     0             CURC              2 0
093900940223     C                   MOVE      *ZEROS        WIN              99
094000940224     C                   MOVE      *BLANK        WTBERR            1
094100940223     C*
094200940223     C* Indici
094300940223     C*
094400940223     C                   Z-ADD     0             X                 3 0
094500940223     C                   Z-ADD     0             Y                 3 0
094600940127     C*
094700940117     C                   ENDSR
094800940128     C************************************************************
094900940509     C/EJECT
095000940509     C************************************************************
095100940509     C* CONTROLLO ABILITAZIONE TASTI
095200940509     C************************************************************
095300940509     C     CTRKEY        BEGSR
095400940509     C                   SELECT
095500940509     C     $TASTO        WHENEQ    F01
095600940720     C     $FC(1)        IFEQ      '0'
095700940509     C                   SETON                                        99
095800940509     C                   ENDIF
095900940509     C     $TASTO        WHENEQ    F02
096000940720     C     $FC(2)        IFEQ      '0'
096100940509     C                   SETON                                        99
096200940509     C                   ENDIF
096300940509     C     $TASTO        WHENEQ    F03
096400940720     C     $FC(3)        IFEQ      '0'
096500940509     C                   SETON                                        99
096600940509     C                   ENDIF
096700940509     C     $TASTO        WHENEQ    F04
096800940720     C     $FC(4)        IFEQ      '0'
096900940509     C                   SETON                                        99
097000940509     C                   ENDIF
097100940509     C     $TASTO        WHENEQ    F05
097200940720     C     $FC(5)        IFEQ      '0'
097300940509     C                   SETON                                        99
097400940509     C                   ENDIF
097500940509     C     $TASTO        WHENEQ    F06
097600940720     C     $FC(6)        IFEQ      '0'
097700940509     C                   SETON                                        99
097800940509     C                   ENDIF
097900940509     C     $TASTO        WHENEQ    F07
098000940720     C     $FC(7)        IFEQ      '0'
098100940509     C                   SETON                                        99
098200940509     C                   ENDIF
098300940509     C     $TASTO        WHENEQ    F08
098400940720     C     $FC(8)        IFEQ      '0'
098500940509     C                   SETON                                        99
098600940509     C                   ENDIF
098700940509     C     $TASTO        WHENEQ    F09
098800940720     C     $FC(09)       IFEQ      '0'
098900940509     C                   SETON                                        99
099000940509     C                   ENDIF
099100940509     C     $TASTO        WHENEQ    F10
099200940720     C     $FC(10)       IFEQ      '0'
099300940509     C                   SETON                                        99
099400940509     C                   ENDIF
099500940509     C     $TASTO        WHENEQ    F11
099600940720     C     $FC(11)       IFEQ      '0'
099700940509     C                   SETON                                        99
099800940509     C                   ENDIF
099900940509     C     $TASTO        WHENEQ    F12
100000940720     C     $FC(12)       IFEQ      '0'
100100940509     C                   SETON                                        99
100200940509     C                   ENDIF
100300940509     C     $TASTO        WHENEQ    F13
100400940720     C     $FC(13)       IFEQ      '0'
100500940509     C                   SETON                                        99
100600940509     C                   ENDIF
100700940509     C     $TASTO        WHENEQ    F14
100800940720     C     $FC(14)       IFEQ      '0'
100900940509     C                   SETON                                        99
101000940509     C                   ENDIF
101100940509     C     $TASTO        WHENEQ    F15
101200940720     C     $FC(15)       IFEQ      '0'
101300940509     C                   SETON                                        99
101400940509     C                   ENDIF
101500940509     C     $TASTO        WHENEQ    F16
101600940720     C     $FC(16)       IFEQ      '0'
101700940509     C                   SETON                                        99
101800940509     C                   ENDIF
101900940509     C     $TASTO        WHENEQ    F17
102000940720     C     $FC(17)       IFEQ      '0'
102100940509     C                   SETON                                        99
102200940509     C                   ENDIF
102300940509     C     $TASTO        WHENEQ    F18
102400940720     C     $FC(18)       IFEQ      '0'
102500940509     C                   SETON                                        99
102600940509     C                   ENDIF
102700940509     C     $TASTO        WHENEQ    F19
102800940720     C     $FC(19)       IFEQ      '0'
102900940509     C                   SETON                                        99
103000940509     C                   ENDIF
103100940509     C     $TASTO        WHENEQ    F20
103200940720     C     $FC(20)       IFEQ      '0'
103300940509     C                   SETON                                        99
103400940509     C                   ENDIF
103500940509     C     $TASTO        WHENEQ    F21
103600940720     C     $FC(21)       IFEQ      '0'
103700940509     C                   SETON                                        99
103800940509     C                   ENDIF
103900940509     C     $TASTO        WHENEQ    F22
104000940720     C     $FC(22)       IFEQ      '0'
104100940509     C                   SETON                                        99
104200940509     C                   ENDIF
104300940509     C     $TASTO        WHENEQ    F23
104400940720     C     $FC(23)       IFEQ      '0'
104500940509     C                   SETON                                        99
104600940509     C                   ENDIF
104700940509     C     $TASTO        WHENEQ    F24
104800940720     C     $FC(24)       IFEQ      '0'
104900940509     C                   SETON                                        99
105000940509     C                   ENDIF
105100940509     C                   ENDSL
105200940509     C                   ENDSR
105300940509     C/EJECT
105400940509     C************************************************************
105500940509     C* RIEMPIMENTO TASTI FUNZIONE
105600940509     C************************************************************
105700940509     C     RIEKEY        BEGSR
105800940509     C*
105900940509     C* Input :
106000940509     C* - $FX = schiera con tutti i testi disponibili
106100940509     C* - $FC = schiere con i flag di validità associati ai testi
106200940509     C* - $FR = schiere con i flag di REVERSE IMAGE
106300940509     C* - $ULK = ultimo testo caricato
106400940509     C* Output :
106500940509     C* - $ULK
106600940509     c* - $KEY1= 1° riga di tasti funzione
106700940509     c* - $KEY2= 2° riga di tasti funzione
106800940509     C*
106900970214     C                   CALLB     'XKEYMSG'
107000940509     C* input
107100940509     C                   PARM                    $FX
107200940509     C                   PARM                    $FC
107300940509     C                   PARM                    $FR
107400940509     C                   PARM                    $ULK              3 0
107500940509     C* output
107600940509     C                   PARM                    $KEY1            79
107700940509     C                   PARM                    $KEY2            79
107800940509     C*
107900940509     C                   ENDSR
108000940615**
10810094022401PROMSG    *LIBL     COS0001    immissione
10820094022402PROMSG    *LIBL     COS0002    modifica
10830094022403PROMSG    *LIBL     COS0003    copia
10840094022404PROMSG    *LIBL     COS0004    annullamento
10850094022405PROMSG    *LIBL     COS0005    visualizzazione
108600940509** TASTI DI FUNZIONE  D1
108700940509PROMSG    *LIBL     KEY0001  01
108800940509PROMSG    *LIBL     KEY0002  02
108900940509PROMSG    *LIBL     KEY0003  03
109000940509PROMSG    *LIBL     KEY0004  04
109100940509PROMSG    *LIBL     KEY0005  05
109200940509PROMSG    *LIBL     KEY0006  06
109300940509PROMSG    *LIBL     KEY0007  07
109400940509PROMSG    *LIBL     KEY0008  08
109500940509PROMSG    *LIBL     KEY0009  09
109600940509PROMSG    *LIBL     KEY0010  10
109700940509PROMSG    *LIBL     KEY0011  11
109800940509PROMSG    *LIBL     KEY0012  12
109900940509PROMSG    *LIBL     KEY0013  13
110000940509PROMSG    *LIBL     KEY0014  14
110100940509PROMSG    *LIBL     KEY0015  15
110200940509PROMSG    *LIBL     KEY0016  16
110300940509PROMSG    *LIBL     KEY0017  17
110400940509PROMSG    *LIBL     KEY0018  18
110500940509PROMSG    *LIBL     KEY0019  19
110600940509PROMSG    *LIBL     KEY0020  20
110700940509PROMSG    *LIBL     KEY0021  21
110800940509PROMSG    *LIBL     KEY0022  22
110900940509PROMSG    *LIBL     KEY0023  23
111000940509PROMSG    *LIBL     KEY0024  24
111100940509**  ABILITAZIONE D1
1112009407200N  01
1113009407200N  02
1114009407201N  03
1115009407201N  04
1116009407201N  05
1117009407201N  06
1118009407201N  07
1119009407201N  08
1120009407200N  09
1121009407200N  10
1122009407200N  11
1123009407201N  12
1124009407200N  13
1125009407200N  14
1126009407200N  15
1127009407200N  16
1128009407200N  17
1129009407200N  18
1130009407200N  19
1131009407200N  20
1132009407200N  21
1133009407200N  22
1134009407200N  23
1135009407200N  24
