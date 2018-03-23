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
004100940614$001 FANGY4RD2  CF   E             WORKSTN
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
006000940614$003 D EXTDS         E DS                  EXTNAME(ANGY4RDS) INZ
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
017500980930$005 I              D1DES                       §4RDES
017600980930$005 I              D1CTD                       §4RCTD
017700980930$005 I              D1CTA                       §4RCTA
017800980930$005 I              D1CAD                       §4RCAD
017900980930$005 I              D1CAA                       §4RCAA
018000980930$005 I              D1CCD                       §4RCCD
018100980930$005 I              D1SCD                       §4RSCD
018200980930$005 I              D1CCA                       §4RCCA
018300980930$005 I              D1SCA                       §4RSCA
018400980930$005 I              D1DSD                       §4RDSD
018500980930$005 I              D1DSA                       §4RDSA
018600980930$005 I              D1DPD                       §4RDPD
018700980930$005 I              D1DPA                       §4RDPA
018800980930$005 I              D1DDD                       §4RDDD
018900980930$005 I              D1DDA                       §4RDDA
019000980930$005 I              D1STA                       §4RSTA
019100980930$005 I              D1TRA                       §4RTRA
019200980930$005 I              D1PA1                       §4RPA1
019300980930$005 I              D1PAG                       §4RPAG
019400980930$005 I              D1INC                       §4RINC
019500980930$005 I              D1DTA                       §4RDTA
019600980930$005 I              D1KSC                       §4RKSC
019700980930$005 I              D1CDI                       §4RCDI
019800980930$005 I              D1CEV                       §4RCEV
019900940127     I*
020000940127     C*----------------------------------------------------*
020100940127     C*                MAIN LINE PROGRAM
020200940127     C*----------------------------------------------------*
020300940127     C*
020400940223     C* inizializzazione variabili
020500940223     C                   EXSR      INZVAR
020600940223     C*
020700940223     C     $FINE         DOWEQ     *OFF
020800940202     C     $GEST         CASEQ     'D1'          GESD1
020900940117     C                   END
021000940117     C                   END
021100940201     C*
021200940325     C* fine programma
021300940325     C                   EXSR      ENDPGM
021400940325     C/EJECT
021500940325     C************************************************************
021600940325     C* RICERCHE
021700940325     C************************************************************
021800940325     C     ENDPGM        BEGSR
021900940325     C*
022000940614     C* disallocazione rcd nel caso di
022100971110$007 C                   MOVE      'Y4R'         XTBCOD
022200940614    >C                   MOVE      '7'           XTBRIC
022300970214     C                   CALLB     'XATB'
022400940614     C                   PARM                    XATBDS
022500940325     C*
022600940325     C* passaggio dati al pgm chiamante
022700940715     C                   MOVE      *BLANKS       KPJBU
022800940325    >C                   MOVEL     D002DS        KPJBU
022900940325     C*
023000940325     C                   SETON                                            RT
023100940325     C*
023200940325     C                   ENDSR
023300940325     C/EJECT
023400940201     C************************************************************
023500940201     C* RICERCHE
023600940201     C************************************************************
023700940127     C     SEARCH        BEGSR
023800940127     C*
023900940127     C* determino Riga/Colonna del cursore
024000940127     C                   MOVE      NRG           R$$
024100940127     C                   MOVE      NCL           C$$
024200940127     C                   Z-ADD     RIRI          CURR
024300940127     C                   Z-ADD     COCO          CURC
024400940127     C*
024500940224     C* I campi che al momento dell'F4 risultano protetti non devono
024600940224     C* essere abilitati alla funzione di ricerca
024700940224     C*
0248009401271    C                   SELECT
024900940127     C*
025000940315     C* FMTD1  - PACCHETTO
025100940614    >C*          H1NMFL    WHEQ 'D1PKG'
025200940207     C* imposto pos. del cursore
025300940614     C*                    Z-ADDCURR      H1RIGA
025400940614     C*                    Z-ADDCURC      H1COLO
025500940127     C*
025600940223     C*
025700940223     C* ricerca non abilitata in questa posizione
0258009402231O   C                   OTHER
025900941005     C                   SETON                                        9899
026000941005     C* imposto pos. del cursore
026100941005     C                   Z-ADD     CURR          H1RIGA
026200941005     C                   Z-ADD     CURC          H1COLO
026300940127     C*
0264009401271-   C                   ENDSL
026500940127     C*
026600940506     C                   MOVE      *HIVAL        $LASTV
026700940127     C                   ENDSR
026800940615     C/EJECT
026900940615     C************************************************************
027000940615     C* TRADUZIONI
027100940615     C************************************************************
027200940615     C     LINGUA        BEGSR
027300940615     C*
027400940615     C* determino Riga/Colonna del cursore
027500940615     C                   MOVE      NRG           R$$
027600940615     C                   MOVE      NCL           C$$
027700940615     C                   Z-ADD     RIRI          CURR
027800940615     C                   Z-ADD     COCO          CURC
027900940615     C*
028000940615     C*
028100940615     C*
028200940615     C* FMTD1  - CAMPI DA TRADURRE
028300940615     C* imposto pos. del cursore
028400940615     C                   Z-ADD     CURR          H1RIGA
028500940615     C                   Z-ADD     CURC          H1COLO
028600940621     C                   MOVEL     H1NMFL        W005              5
028700940621     C                   MOVE      W005          W003              3
028800940621     C                   MOVEL     W003          W006              6
028900940620     C                   MOVE      *ZERO         KEY              15
029000940616     C                   MOVE      D1COD         KEY
029100940615     C                   SELECT
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
042500941006     C                   MOVE      '0'           D1RIS
042600940616     C                   MOVE      KEY002        D1COD
042700940615     C                   MOVE      AZI002        XTBAZI
042800940615     C                   MOVE      LIN002        XTBLIN
042900940616     C                   MOVE      *ALL'0'       XTBKEY
043000940615     C                   MOVE      KEY002        XTBKEY
043100970509     C                   MOVE      UNI002        XTBUNT
043200940615$007 C                   MOVE      'Y4R'         XTBCOD
043300940225     C*
043400940207     C*
043500940224     C* valorizzo la chiave univoca per reperire i dati
043600940224     C* se non immissione
0437009403151   >C     OPZ002        IFNE      '01'
043800940315    >C*-------------------------------------------------------------
043900940315    >C* impostare i campi chiave ricevuti
044000940315    >C*-------------------------------------------------------------
044100940318     C*
044200940614     C*
044300940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0444009406142   >C     OPZ002        IFEQ      '02'
044500940315    >C     OPZ002        OREQ      '04'
044600940614     C                   MOVE      '1'           XTBALC
044700940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0448009406142E   C                   ELSE
044900940614     C                   MOVE      '0'           XTBALC
0450009406142-   C                   ENDIF
045100940614     C*
045200940616     C                   MOVE      '1'           XTBRIC
045300970214     C                   CALLB     'XATB'
045400940614     C                   PARM                    XATBDS
045500940318     C*
045600940614     C     XTBERR        IFEQ      '0'
045700940614     C                   MOVEL     XTBUNI        EXTDS
045800970512     c                   move      xtbunt        d1uniTA           8
045900941108     C* Giro la data se non blank
046000970214     c                   move      xtbdsc        xtbdscA          10
046100970214     C     XTBDSCa       IFNE      DATA
046200970214     C                   CALLB     'XDT4000'
046300970214     C                   PARM      XTBDSCA       XDTAMG           10
046400940621     C                   PARM      *ZEROS        XDTGMA            6 0
046500940621     C                   PARM      2             XDTSTA            1 0
046600940621     C                   MOVE      XDTGMA        D1DSC
046700940712     C                   MOVE      XTBRIS        D1RIS
046800941108     C                   ELSE
046900941108     C                   MOVE      *ZEROS        D1DSC
047000941108     C                   ENDIF
047100941108     C                   ENDIF
047200940614     C*
047300940224     C*
047400940224     C* se il rcd da gestire non viene trovato
047500940318     C* oppure è allocato da altro pgm
047600940224     C* ritorno al pgm guida avvertendolo dell'errore
0477009406142    C     XTBERR        IFNE      '0'
047800940224     C                   MOVE      *ON           $FINE
047900940315    >C                   MOVE      '1'           ERR002
048000940224     C*
048100940224     C* altrimenti
0482009402242E   C                   ELSE
048300940224     C* riempio la videata
048400940224     C                   EXSR      RIED1
048500940224     C* richiamo routine dei ctrl per decodificarla
048600940614    >C                   EXSR      CTRD1
048700960228     C                   MOVEL     *IN99         SAV99             1
048800960228     C* Se la gestione è più videate
048900960305    >C                   EXSR      CTRD2
049000960305    >C     *IN99         IFEQ      *ON
049100960305    >C                   MOVEL     *IN99         SAV99             1
049200960305    >C                   ENDIF
049300940224     C                   MOVE      *ALL'0'       IN5098
049400940224     C                   MOVEA     IN5098        *IN(50)
049500940224     C* definizione indicatori su video
049600940224     C                   SETOFF                                       02
0497009402243    C                   SELECT
049800940315    >C     OPZ002        WHENEQ    '02'
049900940224     C                   SETON                                        02
050000940315    >C     OPZ002        WHENEQ    '05'
050100940506     C* disabilita tasti
050200940720     C                   MOVE      '0'           $FC(6)
050300940720     C                   MOVE      '0'           $FC(5)
050400940720     C                   MOVE      '0'           $FC(4)
0505009402243-   C                   ENDSL
050600940224     C*
0507009402242-   C                   ENDIF
050800940223     C*
0509009402241-   C                   ENDIF
051000940224     C*
051100940117     C                   ENDSR
051200940207     C/EJECT
051300940207     C************************************************************
051400940510     C* RIEMPIMENTO VIDEATA  D1
051500940207     C************************************************************
051600940207     C     RIED1         BEGSR
051700940207     C*
051800940207     C*
051900940207     C                   ENDSR
052000940309     C/EJECT
052100940309     C************************************************************
052200940309     C* GESTIONE F03 VIDEO D1
052300940309     C************************************************************
052400940309     C     F03D1         BEGSR
052500940309     C*
052600940309     C                   MOVE      *ON           $FINE
052700940315    >C                   MOVE      '1'           RET002
052800940325     C* fine programma
052900940325     C                   EXSR      ENDPGM
053000940325     C*
053100940325     C                   ENDSR
053200940309     C/EJECT
053300940309     C************************************************************
053400940309     C* GESTIONE F05 VIDEO D1
053500940309     C************************************************************
053600940309     C     F05D1         BEGSR
053700940309     C*
053800940309     C                   MOVE      *ON           $INZD1
053900940309     C*
054000940309     C                   ENDSR
054100940309     C/EJECT
054200940309     C************************************************************
054300940309     C* GESTIONE F08 VIDEO D1
054400940309     C************************************************************
054500940309     C     F08D1         BEGSR
054600940309     C*
054700940309     C                   MOVE      *ON           $FINE
054800940315    >C                   MOVE      '0'           RET002
054900940309     C*
055000940309     C                   ENDSR
055100940309     C/EJECT
055200940309     C************************************************************
055300940309     C* GESTIONE F12 VIDEO D1
055400940309     C************************************************************
055500940309     C     F12D1         BEGSR
055600940309     C*
055700940309     C                   MOVE      *ON           $FINE
055800940315    >C                   MOVE      '2'           RET002
055900940309     C*
056000940309     C                   ENDSR
056100940309     C/EJECT
056200940309     C************************************************************
056300940309     C* GESTIONE F24 VIDEO D1
056400940309     C************************************************************
056500940309     C     F24D1         BEGSR
056600940309     C*
056700940506     C*
056800940506     C                   MOVEA     $FX1          $FX
056900940506     C                   MOVEA     $FC1          $FC
057000940506     C                   MOVEA     $FR1          $FR
057100940506     C                   Z-ADD     $ULKD1        $ULK
057200940506     C                   EXSR      RIEKEY
057300941017     C                   MOVEA     $FC           $FC1
057400940506     C                   Z-ADD     $ULK          $ULKD1            3 0
057500940506     C                   MOVEL     $KEY1         Z1KE1
057600940506     C                   MOVEL     $KEY2         Z1KE2
057700940614     C                   WRITE     Z1
057800940506     C*
057900940309     C                   ENDSR
058000940207     C/EJECT
058100940131     C************************************************************
058200940207     C* CONTROLLO VIDEATA
058300940131     C************************************************************
058400940127     C     CTRD1         BEGSR
058500940131     C*
058600940127     C                   SETOFF                                       99
058700940127     C* non imposto pos. del cursore
058800940127     C                   Z-ADD     0             H1RIGA
058900940127     C                   Z-ADD     0             H1COLO
059000940621     C*
059100940621     C* CTRL DATA
059200940621     C*
059300941109     C                   MOVE      DATA          WRKDSC           10
059400940621     C     D1DSC         IFNE      0
059500970214     C                   CALLB     'XDT4000'
059600941109     C                   PARM                    XDTAMG           10
059700940621     C                   PARM      D1DSC         XDTGMA            6 0
059800940621     C                   PARM      3             XDTSTA            1 0
059900940621     C     XDTSTA        IFNE      0
060000940621     C                   SETON                                        5599
060100940621     C                   ELSE
060200941108     C                   MOVE      XDTAMG        WRKDSC
060300940621     C                   ENDIF
060400940621     C                   ENDIF
060500940223     C*
060600940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
060700940223     C*
0608009403151   >C     OPZ002        IFNE      '04'
060900971110     C*
061000971110     C* controllo codice vuoto
061100971110$017 C     D1COD         IFEQ      *ALL' '
061200971110     C                   SETON                                        5099
061300971110     C                   ENDIF
061400971110     C*
061500940207     C*
061600940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
061700940315    >C     OPZ002        IFEQ      '01'
061800940315    >C     OPZ002        OREQ      '03'
061900940315    >C*-------------------------------------------------------------
062000940315    >C* impostare i campi chiave e verificare l'esistenza del record
062100940315    >C*-------------------------------------------------------------
062200971110$007 C                   MOVE      'Y4R'         XTBCOD
062300940614     C                   MOVE      *ALL'0'       XTBKEY
062400940620    >C                   MOVE      D1COD         XTBKEY
062500970512    >C                   MOVE      D1UNITA       XTBUNT
062600940614     C                   MOVE      '0'           XTBALC
062700940614     C                   MOVE      '1'           XTBRIC
062800970214    >C                   CALLB     'XATB'
062900940614     C                   PARM                    XATBDS
063000940614     C     XTBERR        IFEQ      '0'
063100940223     C                   SETON                                        5999
063200940223     C                   ENDIF
063300940620     C* Controllo se compreso nei range indice
063400940620     C     XTBERR        IFEQ      '3'
063500940620    >C                   SETON                                        5899
063600940620     C                   END
063700940620     C*
063800940620     C* REPERISCO AUTORIZZAZIONI UTENTE
063900940620     C                   CLEAR                   XCHATB
064000941118     C                   MOVE      XSCSOC        XCTSOC
064100941118     C                   MOVE      XSCDUN        XCTUNI
064200940620     C                   MOVEL     KNMUS         XCTPRF
064300940620     C                   MOVE      XTBCOD        XCTCOD
064400940620     C                   CALL      'XCHKATB'
064500940620     C                   PARM                    XCHATB
064600941111     C                   MOVEL     XCTDDS        AUTDS
064700941027     C     XCTRTN        IFEQ      1
064800941027     C                   MOVEL     'N'           MANGRU
064900941027     C                   ENDIF
065000940620     C* Non ammessa immissione di gruppo
065100940620     C     XTBGRU        IFEQ      'G'
065200940620     C     MANGRU        ANDEQ     'N'
065300940620     C                   SETON                                        5799
065400940620     C                   END
065500940620     C* Non ammessa immissione di azienda
065600940620     C     XTBGRU        IFEQ      'A'
065700940620     C     MANAZI        ANDEQ     'N'
065800940620     C                   SETON                                        5699
065900940620     C                   END
066000940712     C* codice riservato non accessibile
066100941006     C     XTBRIS        IFGT      ABIRIS
066200941005    >C     OPZ002        ANDNE     '01'
066300941005    >C     OPZ002        ANDNE     '03'
066400940712     C                   SETON                                        5499
066500940712     C                   END
066600940224     C                   ENDIF
066700940224     C*
0668009402241-   C                   END
066900940131     C*
067000940302     C     *IN99         IFEQ      *ON
067100940510     C     $GEST         ANDEQ     'D1'
067200960228     C     OPZ002        ANDNE     '07'
067300940117     C                   MOVEA     *IN           WIN
067400940128     C                   MOVE      *ALL'0'       IN5098           49
067500940128     C                   MOVEA     IN5098        *IN(50)
067600940614     C                   WRITE     D1
067700940117     C                   MOVEA     WIN           *IN
067800940302     C                   ENDIF
067900940127     C*
068000940117     C                   ENDSR
068100960305     C/EJECT
068200960305     C************************************************************
068300960305     C* CONTROLLO VIDEATA D2
068400960305     C************************************************************
068500960305     C     CTRD2         BEGSR
068600960305     C*
068700960305     C                   ENDSR
068800940207     C/EJECT
068900940224     C************************************************************
069000940315    >C* AGGIORNAMENTO ANAGRAFICA
069100940224     C************************************************************
069200940315    >C     AGGANA        BEGSR
069300971110     C*
069400971110$007 C                   MOVE      'Y4R'         XTBCOD
069500940614     C                   MOVE      *ALL'0'       XTBKEY
069600940614     C                   MOVE      D1COD         XTBKEY
069700970512     C                   MOVE      D1UNITA       XTBUNT
069800940207     C*
069900940207     C                   SELECT
070000940315    >C     OPZ002        WHENEQ    '01'
070100940315    >C     OPZ002        OREQ      '03'
070200940315    >C                   EXSR      RIEANA
070300950609     C                   MOVE      XSCCUT        XTBUTE
070400940614     C                   MOVE      '5'           XTBRIC
070500970214     C                   CALLB     'XATB'
070600940614    >C                   PARM                    XATBDS
070700940318     C* se rcd non scrivibile attivo errore
070800940615     C     XTBERR        IFEQ      '1'
070900940318    >C                   SETON                                        5999
071000940318     C                   END
071100940315    >C     OPZ002        WHENEQ    '02'
071200940621     C                   MOVE      '1'           XTBRIC
071300940621     C                   MOVE      '1'           XTBALC
071400970214     C                   CALLB     'XATB'
071500940621     C                   PARM                    XATBDS
071600940622    >C                   EXSR      RIEANA
071700950609     C                   MOVE      XSCCUT        XTBUTE
071800940614     C                   MOVE      '4'           XTBRIC
071900970214     C                   CALLB     'XATB'
072000940614    >C                   PARM                    XATBDS
072100940315    >C     OPZ002        WHENEQ    '04'
072200940621     C                   MOVE      '1'           XTBRIC
072300940621     C                   MOVE      '1'           XTBALC
072400970214     C                   CALLB     'XATB'
072500940621     C                   PARM                    XATBDS
072600940614     C                   MOVE      '6'           XTBRIC
072700970214     C                   CALLB     'XATB'
072800940614    >C                   PARM                    XATBDS
072900940621     C* annullo traduzioni
073000940621     C     XTBERR        IFEQ      '0'
073100940621     C                   EXSR      ANNTBT
073200940621     C                   END
073300940621     C*
073400940207     C                   ENDSL
073500940203     C*
073600940203     C                   ENDSR
073700940224     C/EJECT
073800940224     C************************************************************
073900940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
074000940224     C************************************************************
074100940616     C     RIEANA        BEGSR
074200940616     C                   CLEAR                   XTBUNI
074300940621     C                   MOVE      WRKDSC        XTBDSC
074400940712     C                   MOVE      D1RIS         XTBRIS
074500940616     C                   MOVEL     EXTDS         XTBUNI
074600940207     C                   ENDSR
074700940224     C/EJECT
074800940224     C************************************************************
074900940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
075000940224     C************************************************************
075100940224     C     GESAGG        BEGSR
075200940224     C*
075300940224     C* segnalo al pgm chiamante l'avvenuta conferma
075400940315    >C                   MOVE      *ON           OPR002
075500940224     C* segnalo al pgm chiamante l'aver premuto l'F6
075600940315    >C                   MOVE      '0'           RET002
075700940224     C*
0758009402241    C                   SELECT
075900940224     C* nel caso di immissione, ripristino la videata iniziale
076000940315    >C     OPZ002        WHENEQ    '01'
076100940224     C                   MOVE      *ON           $INZD1
076200940224     C* altrimenti ritorno al pgm chiamante
076300940224     C                   OTHER
076400940224     C                   MOVE      *ON           $FINE
0765009402241-   C                   ENDSL
076600940224     C*
076700940224     C                   ENDSR
076800940621     C/EJECT
076900940621     C************************************************************
077000940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
077100940621     C************************************************************
077200940621    >C     ANNTBT        BEGSR
077300940621     C                   MOVE      XTBAZI        TBTAZI
077400940621     C                   MOVE      XTBCOD        TBTCOD
077500940621     C                   MOVE      XTBKEY        TBTKEY
077600940621     C     KTBT          SETLL     ANTBT01L
077700940621     C                   DO        *HIVAL
077800940621     C     KTBT          DELETE    ANTBT01L                           21
077900940621     C  N21              END
078000940621     C*
078100940621     C                   ENDSR
078200940207     C/EJECT
078300940131     C************************************************************
078400940131     C* OPERAZIONI INIZIALI
078500940131     C************************************************************
078600940131     C     *INZSR        BEGSR
078700940131     C*
078800940127     C* Reperimento parametri
078900940127     C*
079000940117     C     *ENTRY        PLIST
079100940117     C                   PARM                    KPJBA
079200940712     C*
079300940712     C* Reperimento Codice utente
079400940712     C*
079500940223     C*
079600940506     C*
079700940506     C* Reperimento tasti di funzione
079800940506     C*
079900940506     C                   EXSR      REPKEY
080000940621     C*
080100940621     C* KLIST
080200940621     C*
080300940621     C     KTBT          KLIST
080400940621     C                   KFLD                    TBTAZI
080500940621     C                   KFLD                    TBTCOD
080600940621     C                   KFLD                    TBTKEY
080700940506     C*
080800940223     C                   ENDSR
080900941103     C/EJECT
081000941103      ************************************************************
081100941103      * Reperimento dati società
081200941103      ************************************************************
081300941103     C     REPSOC        BEGSR
081400941103      *
081500970214     C                   CALLB     'XSOC'
081600941103     C                   PARM                    TIPXSC            6
081700941103     C                   PARM                    SOCXSC            3
081800941103     C                   PARM                    CDSXSC            9 0
081900941103     C                   PARM                    MODXSC            3
082000941103     C                   PARM      *BLANKS       RTNXSC            1
082100941103     C                   PARM                    XSOCDS
082200941103     C                   PARM                    KPJBA
082300941103      *
082400941103     C                   ENDSR
082500940223     C/EJECT
082600940506     C************************************************************
082700940506     C* REPERIMENTO TASTI DI FUNZIONE
082800940506     C************************************************************
082900940506     C     REPKEY        BEGSR
083000940506     C                   MOVEA     $FM1          $FM
083100940506     C                   EXSR      REPKKK
083200940506     C                   MOVEA     $FX           $FX1
083300940614     C                   ENDSR
083400940506     C/EJECT
083500940506     C************************************************************
083600940506     C* REPERIMENTO TASTI DI FUNZIONE
083700940506     C************************************************************
083800940506     C     REPKKK        BEGSR
083900940506     C*
084000940506     C                   CLEAR                   $FX
084100940506     C     1             DO        24            X
084200940506     C*
084300940506     C     $FM(X)        IFNE      *BLANK
084400940506     C*
084500940506     C                   MOVEL     $FM(X)        IDMSG
084600970214     C                   CALLB     'XRTVM'
084700940506     C                   PARM                    IDMSG            27
084800940506     C                   PARM                    TXTMSG          100
084900940506     C                   PARM                    ERRMSG            1
085000940506     C*
085100940506     C     ERRMSG        IFNE      *ON
085200940506     C                   MOVEL     TXTMSG        $FX(X)
085300940506     C                   ELSE
085400940506     C                   MOVEL     *ALL' '       $FX(X)
085500940506     C                   END
085600940506     C*
085700940506     C                   END
085800940506     C*
085900940506     C                   END
086000940506     C*
086100940506     C                   ENDSR
086200940509     C/EJECT
086300940223     C************************************************************
086400940223     C* INIZIALIZZAZIONE VARIABILI
086500940223     C************************************************************
086600940223     C     INZVAR        BEGSR
086700951124     C*
086800951124     C* Chiamata XSOC
086900951124     C*
087000951124     C                   MOVEL     'SOC001'      TIPXSC
087100951124     C                   EXSR      REPSOC
087200951124     C     RTNXSC        IFNE      '1'
087300951124     C                   MOVEL     XSOCDS        SOC001
087400951124     C                   ELSE
087500951124     C                   MOVE      *ON           $FINE
087600951124     C* fine programma
087700951124     C                   EXSR      ENDPGM
087800951124     C                   ENDIF
087900940224     C*
088000940224     C* Dati da PGM guida
088100940224     C*
088200940315    >C                   MOVEL     KPJBU         D002DS
088300940128     C*
088400940223     C* Variabili per gestione videate
088500940128     C*
088600940223     C                   MOVE      'D1'          $GEST
088700940223     C                   MOVE      *OFF          $FINE
088800940223     C                   MOVE      *ON           $INZD1
088900940224     C                   MOVE      *BLANK        $LASTG
089000940224     C                   MOVE      *BLANK        $LASTV
089100940506     C                   Z-ADD     0             $ULKD1            3 0
089200940127     C*
089300940223     C* Variabili appoggio
089400940127     C*
089500940224     C                   Z-ADD     0             CURR
089600940224     C                   Z-ADD     0             CURC
089700940207     C*
089800940207     C* Valorizzazione campi univoci testate
089900940207     C*
090000940614     C                   CLEAR                   T1
090100940614     C                   MOVEL     KNSIF         NOMSIF
090200941109     C                   MOVEL     XSCDSI        NOMDIT
090300941027     C                   MOVEL     DSPGM         NOMPGM
090400940224     C                   Z-ADD     1             X
090500940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
090600940224     C     *IN21         IFEQ      *ON
090700940224     C                   MOVEL     $ID(X)        IDMSG
090800970214     C                   CALLB     'XRTVM'
090900940224     C                   PARM                    IDMSG            27
091000940224     C                   PARM                    TXTMSG          100
091100940224     C                   PARM                    ERRMSG            1
091200940224     C                   PARM      *ON           CTRMSG            1
091300940224     C                   PARM      30            LENMSG            3 0
091400940224     C     ERRMSG        IFNE      *ON
091500940224     C                   MOVEL     TXTMSG        T1OPE
091600940224     C                   ELSE
091700940224     C                   MOVEL     *ALL'?'       T1OPE
091800940224     C                   ENDIF
091900940224     C                   END
092000940224     C*
092100940224     C* Inizializzazione/Decodifica prima videata
092200940224     C*
092300940224     C                   EXSR      INZD1
092400940224     C                   MOVE      *OFF          $INZD1
092500960228     C* Se opzione = '07' richiesti solo controlli
092600960228     C     OPZ002        IFEQ      '07'
092700960228     C                   MOVEL     *IN99         ERR002
092800960228     C                   MOVEL     '0'           RET002
092900960228     C                   MOVEL     *ON           $FINE
093000960228     C                   ENDIF
093100940127     C*
093200940117     C                   ENDSR
093300940128     C************************************************************
093400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
093500940223     C************************************************************
093600940223     C     DEFCAM        BEGSR
093700940223     C*
093800940223     C* klist
093900940207     C*
094000940223     C*
094100940223     C* Variabili per gestione videate
094200940223     C*
094300940223     C                   MOVE      *BLANK        $GEST             2
094400940223     C                   MOVE      *BLANK        $FINE             1
094500940223     C                   MOVE      *BLANK        $INZD1            1
094600940223     C                   MOVE      *BLANK        $LASTG            2
094700940223     C                   MOVE      *BLANK        $LASTV            2
094800940223     C*
094900940223     C* Variabili appoggio
095000940223     C*
095100940223     C                   Z-ADD     0             CURR              2 0
095200940223     C                   Z-ADD     0             CURC              2 0
095300940223     C                   MOVE      *ZEROS        WIN              99
095400940224     C                   MOVE      *BLANK        WTBERR            1
095500940223     C*
095600940223     C* Indici
095700940223     C*
095800940223     C                   Z-ADD     0             X                 3 0
095900940223     C                   Z-ADD     0             Y                 3 0
096000940127     C*
096100940117     C                   ENDSR
096200940128     C************************************************************
096300940509     C/EJECT
096400940509     C************************************************************
096500940509     C* CONTROLLO ABILITAZIONE TASTI
096600940509     C************************************************************
096700940509     C     CTRKEY        BEGSR
096800940509     C                   SELECT
096900940509     C     $TASTO        WHENEQ    F01
097000940720     C     $FC(1)        IFEQ      '0'
097100940509     C                   SETON                                        99
097200940509     C                   ENDIF
097300940509     C     $TASTO        WHENEQ    F02
097400940720     C     $FC(2)        IFEQ      '0'
097500940509     C                   SETON                                        99
097600940509     C                   ENDIF
097700940509     C     $TASTO        WHENEQ    F03
097800940720     C     $FC(3)        IFEQ      '0'
097900940509     C                   SETON                                        99
098000940509     C                   ENDIF
098100940509     C     $TASTO        WHENEQ    F04
098200940720     C     $FC(4)        IFEQ      '0'
098300940509     C                   SETON                                        99
098400940509     C                   ENDIF
098500940509     C     $TASTO        WHENEQ    F05
098600940720     C     $FC(5)        IFEQ      '0'
098700940509     C                   SETON                                        99
098800940509     C                   ENDIF
098900940509     C     $TASTO        WHENEQ    F06
099000940720     C     $FC(6)        IFEQ      '0'
099100940509     C                   SETON                                        99
099200940509     C                   ENDIF
099300940509     C     $TASTO        WHENEQ    F07
099400940720     C     $FC(7)        IFEQ      '0'
099500940509     C                   SETON                                        99
099600940509     C                   ENDIF
099700940509     C     $TASTO        WHENEQ    F08
099800940720     C     $FC(8)        IFEQ      '0'
099900940509     C                   SETON                                        99
100000940509     C                   ENDIF
100100940509     C     $TASTO        WHENEQ    F09
100200940720     C     $FC(09)       IFEQ      '0'
100300940509     C                   SETON                                        99
100400940509     C                   ENDIF
100500940509     C     $TASTO        WHENEQ    F10
100600940720     C     $FC(10)       IFEQ      '0'
100700940509     C                   SETON                                        99
100800940509     C                   ENDIF
100900940509     C     $TASTO        WHENEQ    F11
101000940720     C     $FC(11)       IFEQ      '0'
101100940509     C                   SETON                                        99
101200940509     C                   ENDIF
101300940509     C     $TASTO        WHENEQ    F12
101400940720     C     $FC(12)       IFEQ      '0'
101500940509     C                   SETON                                        99
101600940509     C                   ENDIF
101700940509     C     $TASTO        WHENEQ    F13
101800940720     C     $FC(13)       IFEQ      '0'
101900940509     C                   SETON                                        99
102000940509     C                   ENDIF
102100940509     C     $TASTO        WHENEQ    F14
102200940720     C     $FC(14)       IFEQ      '0'
102300940509     C                   SETON                                        99
102400940509     C                   ENDIF
102500940509     C     $TASTO        WHENEQ    F15
102600940720     C     $FC(15)       IFEQ      '0'
102700940509     C                   SETON                                        99
102800940509     C                   ENDIF
102900940509     C     $TASTO        WHENEQ    F16
103000940720     C     $FC(16)       IFEQ      '0'
103100940509     C                   SETON                                        99
103200940509     C                   ENDIF
103300940509     C     $TASTO        WHENEQ    F17
103400940720     C     $FC(17)       IFEQ      '0'
103500940509     C                   SETON                                        99
103600940509     C                   ENDIF
103700940509     C     $TASTO        WHENEQ    F18
103800940720     C     $FC(18)       IFEQ      '0'
103900940509     C                   SETON                                        99
104000940509     C                   ENDIF
104100940509     C     $TASTO        WHENEQ    F19
104200940720     C     $FC(19)       IFEQ      '0'
104300940509     C                   SETON                                        99
104400940509     C                   ENDIF
104500940509     C     $TASTO        WHENEQ    F20
104600940720     C     $FC(20)       IFEQ      '0'
104700940509     C                   SETON                                        99
104800940509     C                   ENDIF
104900940509     C     $TASTO        WHENEQ    F21
105000940720     C     $FC(21)       IFEQ      '0'
105100940509     C                   SETON                                        99
105200940509     C                   ENDIF
105300940509     C     $TASTO        WHENEQ    F22
105400940720     C     $FC(22)       IFEQ      '0'
105500940509     C                   SETON                                        99
105600940509     C                   ENDIF
105700940509     C     $TASTO        WHENEQ    F23
105800940720     C     $FC(23)       IFEQ      '0'
105900940509     C                   SETON                                        99
106000940509     C                   ENDIF
106100940509     C     $TASTO        WHENEQ    F24
106200940720     C     $FC(24)       IFEQ      '0'
106300940509     C                   SETON                                        99
106400940509     C                   ENDIF
106500940509     C                   ENDSL
106600940509     C                   ENDSR
106700940509     C/EJECT
106800940509     C************************************************************
106900940509     C* RIEMPIMENTO TASTI FUNZIONE
107000940509     C************************************************************
107100940509     C     RIEKEY        BEGSR
107200940509     C*
107300940509     C* Input :
107400940509     C* - $FX = schiera con tutti i testi disponibili
107500940509     C* - $FC = schiere con i flag di validità associati ai testi
107600940509     C* - $FR = schiere con i flag di REVERSE IMAGE
107700940509     C* - $ULK = ultimo testo caricato
107800940509     C* Output :
107900940509     C* - $ULK
108000940509     c* - $KEY1= 1° riga di tasti funzione
108100940509     c* - $KEY2= 2° riga di tasti funzione
108200940509     C*
108300970214     C                   CALLB     'XKEYMSG'
108400940509     C* input
108500940509     C                   PARM                    $FX
108600940509     C                   PARM                    $FC
108700940509     C                   PARM                    $FR
108800940509     C                   PARM                    $ULK              3 0
108900940509     C* output
109000940509     C                   PARM                    $KEY1            79
109100940509     C                   PARM                    $KEY2            79
109200940509     C*
109300940509     C                   ENDSR
109400940615**
10950094022401PROMSG    *LIBL     COS0001    immissione
10960094022402PROMSG    *LIBL     COS0002    modifica
10970094022403PROMSG    *LIBL     COS0003    copia
10980094022404PROMSG    *LIBL     COS0004    annullamento
10990094022405PROMSG    *LIBL     COS0005    visualizzazione
110000940509** TASTI DI FUNZIONE  D1
110100940509PROMSG    *LIBL     KEY0001  01
110200940509PROMSG    *LIBL     KEY0002  02
110300940509PROMSG    *LIBL     KEY0003  03
110400940509PROMSG    *LIBL     KEY0004  04
110500940509PROMSG    *LIBL     KEY0005  05
110600940509PROMSG    *LIBL     KEY0006  06
110700940509PROMSG    *LIBL     KEY0007  07
110800940509PROMSG    *LIBL     KEY0008  08
110900940509PROMSG    *LIBL     KEY0009  09
111000940509PROMSG    *LIBL     KEY0010  10
111100940509PROMSG    *LIBL     KEY0011  11
111200940509PROMSG    *LIBL     KEY0012  12
111300940509PROMSG    *LIBL     KEY0013  13
111400940509PROMSG    *LIBL     KEY0014  14
111500940509PROMSG    *LIBL     KEY0015  15
111600940509PROMSG    *LIBL     KEY0016  16
111700940509PROMSG    *LIBL     KEY0017  17
111800940509PROMSG    *LIBL     KEY0018  18
111900940509PROMSG    *LIBL     KEY0019  19
112000940509PROMSG    *LIBL     KEY0020  20
112100940509PROMSG    *LIBL     KEY0021  21
112200940509PROMSG    *LIBL     KEY0022  22
112300940509PROMSG    *LIBL     KEY0023  23
112400940509PROMSG    *LIBL     KEY0024  24
112500940509**  ABILITAZIONE D1
1126009407200N  01
1127009407200N  02
1128009407201N  03
1129009407201N  04
1130009407201N  05
1131009407201N  06
1132009407201N  07
1133009407201N  08
1134009407200N  09
1135009407200N  10
1136009407200N  11
1137009407201N  12
1138009407200N  13
1139009407200N  14
1140009407200N  15
1141009407200N  16
1142009407200N  17
1143009407200N  18
1144009407200N  19
1145009407200N  20
1146009407200N  21
1147009407200N  22
1148009407200N  23
1149009407200N  24
