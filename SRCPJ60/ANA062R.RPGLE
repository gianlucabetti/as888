000100981202     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200941115     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400941115      *
000500950419      *            GUIDA ANAGRAFICA LIBRI IVA
000600950419      *           ----------------------------
000700940128     F*----------------------------------------------------*
000800940909     F*------------
000900950419     FANDLI01L  IF   E           K DISK
001000950419     F                                     RENAME(ANDLI000:ANDLI01)
001100950428     F*------------
001200950428     FANALR02L  IF   E           K DISK
001300940909     F*------------
001400980804     FANDLS01L  IF   E           K DISK
001500980804     F*------------
001600940201     FANMPE02L  UF A E           K DISK
001700020308C1550F*------------
001800020308  "   * Slittamenti iva trasportatori mista
001900020308C1550FANSLI01L  IF   E           K DISK
002000080422C2051F* Associazione p.vendita/reglibro (omette annullati)
002100080418C2051FANPNR02L  IF   E           K DISK
002101180105R275 F*------------
002102180105 "   F* Regole per detrazione IVA a Credito
002103180105 "   F* Key: Soc/Reg/Libro/DtInVal Unique
002104180105R275 FSDGRIC01L IF   E           K DISK
002200940211     F*------------
002300950419     FANA062D   CF   E             WORKSTN
002400950419     F                                     RENAME(A062D1:FMTD1)
002500950419     F                                     RENAME(A062D2:FMTD2)
002600950419     F                                     RENAME(A062D3:FMTD3)
002700950419     F                                     RENAME(A062S1:FMTS1)
002800950419     F                                     RENAME(A062C1:FMTC1)
002900950419     F                                     RENAME(A062T1:FMTT1)
003000950419     F                                     RENAME(A062Z1:FMTZ1)
003100950419     F                                     RENAME(A062Z2:FMTZ2)
003200950419     F                                     RENAME(A062Z3:FMTZ3)
003300940131     F                                     SFILE(FMTS1:S1NRR)
003400940201     F                                     INFDS(DSFMT)
003500940128     D*----------------------------------------------------*
003600940302     D* Schiere per la gestione delle opzioni
003700980715     D $DIMOP          C                   CONST(30)
003800940302     D*
003900980715     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
004000980715     D $TX1            S             50    DIM($dimop) ALT($MS1)
004100980715     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004200980715     D $FL1            S              1    DIM($dimop) ALT($OP1)
004300941214     D $TX             S             50    DIM(50)
004400941214     D $FL             S              1    DIM(50)
004500980715     D* schiera contenente i nomi dei programma richiamati attraverso
004600980715     D* le opzioni personalizzate
004700980715     D $PGO1           S             10    DIM($dimop)
004800980715     D*
004900980715     D* Indice della schiera per le opzioni personalizzate
005000980715     D PosOpz          S              2  0
005100980715     D* numero max opzioni
005200980715     D NumOpz          S              2  0 Inz($DIMOP)
005300980715     D* numero effettive opzioni
005400980715     D OpzLen          S              2  0
005500980715     D*-------------
005600940909     D* Schiere generali per la gestione dei tasti di funzione
005700940506     D $FM             S             27    DIM(24)
005800940506     D $FX             S             50    DIM(24)
005900940506     D $FC             S              1    DIM(24)
006000940506     D $FR             S              1    DIM(24)
006100980715     D* Indice della schiera tasti funzione
006200980715     D PosFun          S              2  0
006300980715     D*-------------
006400940909     D* Schiere per la gestione dei tasti di funzione
006500940506     D* Video D1
006600940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006700940506     D $FX1            S             50    DIM(24) ALT($FM1)
006800940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006900940506     D $FR1            S              1    DIM(24) ALT($FC1)
007000940506     D* Schiere per la gestione dei tasti di funzione
007100940506     D* Video S1
007200940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
007300940506     D $FX2            S             50    DIM(24) ALT($FM2)
007400940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
007500940506     D $FR2            S              1    DIM(24) ALT($FC2)
007600980715     D* Schiera contente i nomi dei programmi da richiamare con i tasti
007700980715     D* funzione personalizzata S1
007800980715     D $PGF2           S             10    DIM(25)
007900940916     D* Schiere per la gestione dei tasti di funzione
008000940916     D* Video D2
008100940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
008200940916     D $FX3            S             50    DIM(24) ALT($FM3)
008300940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
008400940916     D $FR3            S              1    DIM(24) ALT($FC3)
008500940211     D*-------------
008600940207     D* Valori assunti
008700941111     D DSMPE           DS                  INZ
008800940207     D  $LOP                   1      1  0
008900940831     D  $ORD                   2      3  0
009000950427     D  $TPR                   4      4
009100950427     D  $LIB                   5      7
009200941026     D*-------------
009300941026     D* Parametri del PGM di gestione commenti ANA725R
009400950419     D*  se non sono gestiti i commenti bisogna asteriscare
009500950419     D*725DS    EIDSANA725DS
009600950419     D*I            '0'                             RET725
009700950419     D*I            '0'                             OPR725
009800950419     D*I            '0'                             ERR725
009900950419     D*--------------
010000950419     D* Parametri del PGM invio posta ANA727R
010100950301     D A727DS        E DS                  EXTNAME(ANA727DS) INZ
010200950301     D  RET727       E                     INZ('0')
010300950301     D  OPR727       E                     INZ('0')
010400950301     D  ERR727       E                     INZ('0')
010500950420     D*-------------
010600950428     D* Abilitazione da capofila
010700950420     D A066DS        E DS                  EXTNAME(ANA066DS) INZ
010800950428     D  RET066       E                     INZ('0')
010900950428     D  OPR066       E                     INZ('0')
011000950428     D  ERR066       E                     INZ('0')
011100950428     D* Parametri per associazione libri/riferimenti
011200950428     D A068DS        E DS                  EXTNAME(ANA068DS)
011300950428     D  RET068       E                     INZ('0')
011400950428     D  OPR068       E                     INZ('0')
011500950428     D  ERR068       E                     INZ('0')
011600980804     D*-------------
011700980804     D* Parametri per slittamento libri iva
011800980804     D A140DS        E DS                  EXTNAME(ANA140DS)
011900980804     D  RET140       E                     INZ('0')
012000980804     D  OPR140       E                     INZ('0')
012100980804     D  ERR140       E                     INZ('0')
012200070924C2051D*-------------
012300070924  "  D* Parametri per punti vendita
012400080421C2051D ANA159ds1     E DS                  INZ
012500940207     D*-------------
012600940211     D* Passaggio Parametri
012700940211     D KPJBA         E DS
012800940211     D*-------------
012900940225     D* Reperimento Autorizzazioni
013000941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
013100941124     D*-------------
013200950410     D* Passaggio Autorizzazioni
013300950410     D DSAUT           DS           512
013400950410     D*-------------
013500941124     D* Maschera dati autorizzazioni
013600000801C1396X***** GPDCDS        E DS                  EXTNAME(ANGPDCDS)
013700000801C1396D GIVADS        E DS                  EXTNAME(ANGIVADS)
013800941209     D*-------------
013900941209     D* Dati di ambiente ottenuti da XSOC
014000950419     D SOC001        E DS                  EXTNAME(XSOC001DS)
014100020308C1550D    XScIvaRgm                   1    overlay(XscFgo:9)                    Scadenz. retroattivo
014200941209     D*-------------
014300941209     D* DS Interna per dati di output di XSOC
014400941209     D XSOCDS          DS          1000
014500950420     D*-------------
014600950420     D* Parametri gestione anagrafica
014700000802C1396X***** P008DS        E DS                  EXTNAME(ANP008DS)
014800000802C1396D P045DS        E DS                  EXTNAME(ANP045DS)
014900940325     D*-------------
015000940325     D* Parametri in ricezione
015100950419     D A062DS        E DS                  EXTNAME(ANA062DS)
015200940325     D*-------------
015300980715     D* campo di salvataggio per la DS d'ingresso durante la chiamata
015400980715     D* alla funzione personalizzata
015500980715    >D $SaveDS         S                   like(A062DS)
015600980715     D*-------------
015700950419     D* Parametri del PGM di manutenzione ANA063R
015800950419     D A063DS        E DS                  EXTNAME(ANA063DS) INZ
015900950419     D  RET063       E                     INZ('0')
016000950419     D  OPR063       E                     INZ('0')
016100950419     D  ERR063       E                     INZ('0')
016200940211     D*-------------
016300940211     D DSFMT           DS           512
016400940506     D  $TASTO               369    369
016500940211     D  NRG                  370    370
016600940211     D  NCL                  371    371
016700940211     D  SFLNRR               378    379B 0
016800950419     D* Ricerca legende
016900950419     D A964DS        E DS                  EXTNAME(ANA964DS) INZ
017000950419     D* Controlli validità legende
017100950419     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
017200940211     D*-------------
017300940211     D* posizione cursore
017400940211     D CURSOR          DS
017500940211     D  RIRI                   1      2B 0 INZ
017600940211     D  R$$                    2      2
017700940211     D  COCO                   3      4B 0 INZ
017800940211     D  C$$                    4      4
017900940127     D*-------------
018000940127     D* Reperimento nome PGM
018100940127     D STATUS         SDS           333
018200940127     D  DSPGM            *PROC
018300950411     D  PARMS            *PARMS
018400950420     D*-------------
018500950420     D XPAOUT          DS          2000
018600950427     D*-------------
018700950427     D* Costanti
018800950427     D REPTPR          C                   CONST('DLITPREG')
018900940616     D* nr.di righe del sfl (SFLPAG)
019000950421     D SFLPAG          C                   CONST(10)
019100940609     D* dimensione della schiera $MS1
019200941117     D* data a 0
019300941117     D $DT0            C                   CONST('0001-01-01')
019400940616     D*-------------
019500940506     D* Tasti di funzione
019600940506     D F01             C                   CONST(X'31')
019700940506     D F02             C                   CONST(X'32')
019800940506     D F03             C                   CONST(X'33')
019900940506     D F04             C                   CONST(X'34')
020000940506     D F05             C                   CONST(X'35')
020100940506     D F06             C                   CONST(X'36')
020200940506     D F07             C                   CONST(X'37')
020300940506     D F08             C                   CONST(X'38')
020400940506     D F09             C                   CONST(X'39')
020500940506     D F10             C                   CONST(X'3A')
020600940506     D F11             C                   CONST(X'3B')
020700940506     D F12             C                   CONST(X'3C')
020800940506     D F13             C                   CONST(X'B1')
020900940506     D F14             C                   CONST(X'B2')
021000940506     D F15             C                   CONST(X'B3')
021100940506     D F16             C                   CONST(X'B4')
021200940506     D F17             C                   CONST(X'B5')
021300940506     D F18             C                   CONST(X'B6')
021400940506     D F19             C                   CONST(X'B7')
021500940506     D F20             C                   CONST(X'B8')
021600940506     D F21             C                   CONST(X'B9')
021700940506     D F22             C                   CONST(X'BA')
021800940506     D F23             C                   CONST(X'BB')
021900940506     D F24             C                   CONST(X'BC')
022000940506     D ENTER           C                   CONST(X'F1')
022100940506     D ROLDWN          C                   CONST(X'F4')
022200940506     D ROLLUP          C                   CONST(X'F5')
022300950420     D*-------------
022400950420     D* Attributi video
022500950420     D* (definisco come costanti qugli attributi video che potrei
022600950420     D*  testare e quindi preferisco assegnargli un valore "parlante")
022700950420     D*  bottone presente
022800950420     D PRES            C                   CONST(X'A1')
022900950428     D*  cliente abilitato in società di riferimento
023000950428     D ABIL            C                   CONST(X'A2')
023100020308C1550 *----------------------------------------------------*
023200020308  "   * Gestione slittamenti iva trasportatori mista
023300020308C1550D Ana148DS      E DS                  inz
023301180105R275  *-----------
023302180105  "   * Gestione Regole detrazione IVA a Credito
023303180105R275 D SDGSI30DS     E DS                  inz
023400021014A0972D* Attributi video
023500021014A0972D Normale         C                   Const(X'32')
023600021014A0972D Protetto        C                   Const(X'A0')
023700940128     I*----------------------------------------------------*
023800950419     I* Anagrafica libri IVA
023900950419     IANDLI01
024000950419     I              DLISOCIETA                  DLISOC
024100950419     I              DLISERIE                    DLISER
024200950419     I              DLIDESCR                    DLIDES
024300950419     I              DLIDESBREV                  DLIDSB
024400950419     I              DLITPREG                    DLITPR
024500950419     I              DLILIBRO                    DLILIB
024600950419     I              DLIKCCIVAN                  DLICIN
024700950419     I              DLIKSCIVAN                  DLISIN
024800950419     I              DLIKCCIVAI                  DLICII
024900950419     I              DLIKSCIVAI                  DLISII
025000950426     I              DLIIVAIND                   DLITII
025100950419     I              DLIAMBITO                   DLIAMB
025200950419     I              DLITERRITO                  DLITER
025300950419     I              DLISOSPENS                  DLISOS
025400950426     I              DLITPDOCUM                  DLITDC
025500950419     I              DLIVENTILA                  DLIVEN
025600950419     I              DLITPLIBRO                  DLITPL
025700950428     I* Anagrafica associazione libro/registro
025800950428     IANALR000
025900950428     I              ALRSOCIETA                  ALRSOC
026000950428     I              ALRUNITA                    ALRUNI
026100950428     I              ALRDESCR                    ALRDES
026200950428     I              ALRDESBREV                  ALRDSB
026300950428     I              ALRTPREG                    ALRTPR
026400950428     I              ALRLIBRO                    ALRLIB
026500950428     I              ALRALIQ                     ALRALI
026600950428     I              ALRRIFIVA                   ALRRIF
026700950428     I              ALRINDET                    ALRABI
026800951006     I              ALRPROPRA                   ALRPRA
026900951006     I              ALRALLEIVA                  ALRAIV
027000950428     I*-------------
027100980804     I* Anagrafica slittameti libri IVA
027200980804     IANDLS000
027300980804     I              DLSSOCIETA                  DLSSOC
027400980804     I              DLSTPREG                    DLSTPR
027500980804     I              DLSLIBRO                    DLSLIB
027600950331     I*-------------
027700950331     I* Sottotitolo di testata
027800950419     I* se non si hanno sottotitoli, mettere tutta la costante = ' '
027900950419     I*             'PROMSG    *LIBL-     C         SOTTIT
028000950419     I*             '     COSxxxx'
028100940207     I*-------------
028200940127     C*----------------------------------------------------*
028300940127     C*                MAIN LINE PROGRAM
028400940127     C*----------------------------------------------------*
028500940127     C*
028600940223     C* inizializzazione variabili
028700940223     C                   EXSR      INZVAR
028800940223     C*
028900940223     C     $FINE         DOWEQ     *OFF
029000940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
029100940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
029200940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
029300941027     C                   ENDCS
029400941027     C                   ENDDO
029500940325     C* fine programma
029600940325     C                   EXSR      ENDPGM
029700940225     C*
029800940325     C/EJECT
029900940325     C************************************************************
030000940325     C* FINE PROGRAMMA
030100940325     C************************************************************
030200940325     C     ENDPGM        BEGSR
030300940325     C*
030400940325     C* passaggio dati al pgm chiamante
030500950419     C                   MOVEL     WOPR          OPR062                                     ne S1
030600950419     C                   MOVEL     A062DS        KPJBU
030700940325     C*
030800940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
030900950419     C     OPZ062        IFEQ      *BLANK
031000940325     C                   SETON                                            LR
031100940325     C* altrimenti chiudo in RT
031200940325     C                   ELSE
031300940325     C                   SETON                                            RT
031400940325     C                   ENDIF
031500940616     C                   RETURN
031600940325     C*
031700940325     C                   ENDSR
031800940224     C/EJECT
031900940201     C************************************************************
032000940201     C* RICERCHE
032100940201     C************************************************************
032200940127     C     SEARCH        BEGSR
032300940127     C*
032400940127     C* determino Riga/Colonna del cursore
032500940127     C                   MOVE      NRG           R$$
032600940127     C                   MOVE      NCL           C$$
032700940127     C                   Z-ADD     RIRI          CURR
032800940127     C                   Z-ADD     COCO          CURC
032900940127     C*
033000941118   B1C                   SELECT
033100940127     C*
033200950419     C*
033300950419     C* FMTD1  - LEGENDA TIPO REGISTRO
033400950419     C     H1NMFL        WHENEQ    'D1TPR'
033500950419     C                   RESET                   A964DS
033600950419     C                   MOVEL     '01'          OPZ964
033700950419     C                   MOVEL     'ANDLI000'    TRC964
033800950428     C                   MOVEL     REPTPR        CAM964
033900950419     C                   MOVEL     A964DS        KPJBU
034000950419     C                   CALL      'ANA964R'
034100950419     C                   PARM                    KPJBA
034200950419     C                   MOVEL     KPJBU         A964DS
034300950419     C                   MOVEL     VAL964        WWTPR
034400950419     C*
034500950419     C     WWTPR         IFNE      *BLANK
034600950419     C                   MOVEL     WWTPR         D1TPR
034700950419     C                   ENDIF
034800950419     C*
034900950419     C* FMTD2  - LEGENDA TIPO REGISTRO
035000950419     C     H1NMFL        WHENEQ    'D2TPR'
035100950419     C                   RESET                   A964DS
035200950419     C                   MOVEL     '01'          OPZ964
035300950419     C                   MOVEL     'ANDLI000'    TRC964
035400950428     C                   MOVEL     REPTPR        CAM964
035500950419     C                   MOVEL     A964DS        KPJBU
035600950419     C                   CALL      'ANA964R'
035700950419     C                   PARM                    KPJBA
035800950419     C                   MOVEL     KPJBU         A964DS
035900950419     C                   MOVEL     VAL964        WWTPR
036000950419     C*
036100950419     C     WWTPR         IFNE      *BLANK
036200950419     C                   MOVEL     WWTPR         D2TPR
036300950419     C                   ENDIF
036400940127     C* imposto pos. del cursore
036500940127     C                   Z-ADD     CURR          H1RIGA
036600940127     C                   Z-ADD     CURC          H1COLO
036700940223     C*
036800940223     C* ricerca non abilitata in questa posizione
036900941118   X1C                   OTHER
037000940929     C* imposto pos. del cursore
037100940929     C                   Z-ADD     CURR          H1RIGA
037200940929     C                   Z-ADD     CURC          H1COLO
037300940929     C                   SETON                                        9899
037400940127     C*
037500941118   E1C                   ENDSL
037600940127     C*
037700940506     C                   MOVE      *HIVAL        $LASTV
037800940127     C                   ENDSR
037900940131     C/EJECT
038000940224     C************************************************************
038100940131     C* RIEMPIMENTO OPZIONI
038200940224     C************************************************************
038300940131     C     RIEOPZ        BEGSR
038400940223     C*
038500940223     C* Input :
038600940223     C* - $TX = schiera con tutti i testi disponibili
038700940223     C* - $FL = schiere con i flag di validità associati ai testi
038800940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
038900940223     C*          opzione usato
039000940223     C* - WMXO = variabile contenente il numero massimo di testi
039100940223     C*          opzione usabili
039200940721     C* - $L01 = Lunghezza prima riga
039300940721     C* - $L02 = Lunghezza seconda riga
039400940223     C* Output :
039500940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
039600940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
039700941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
039800940223     C*
039900961004     C                   CALLb     'X04OPMSG'
040000940223     C* input
040100940223     C                   PARM                    $TX
040200940223     C                   PARM                    $FL
040300940225     C                   PARM                    WCOZ              3 0
040400940223     C                   PARM                    WMXO              3 0
040500940223     C* output
040600940223     C                   PARM                    WOPZ1            76
040700940223     C                   PARM                    WOPZ2            76
040800941214     C                   PARM                    FF23              1
040900950301     C* input
041000940721    >C                   PARM                    $L01              3 0
041100940721    >C                   PARM                    $L02              3 0
041200940223     C*
041300060224A1148C                   PARM                    $ALLOPT         960
041400060224A1148C*
041500940131     C                   ENDSR
041600940506     C/EJECT
041700940506     C************************************************************
041800940506     C* RIEMPIMENTO TASTI FUNZIONE
041900940506     C************************************************************
042000940506     C     RIEKEY        BEGSR
042100940506     C*
042200940506     C* Input :
042300940506     C* - $FX = schiera con tutti i testi disponibili
042400940506     C* - $FC = schiere con i flag di validità associati ai testi
042500940506     C* - $FR = schiere con i flag di REVERSE IMAGE
042600940506     C* - $ULK = ultimo testo caricato
042700940721     C* - $L01 = Lunghezza prima riga
042800940721     C* - $L02 = Lunghezza seconda riga
042900940506     C* Output :
043000940506     C* - $ULK
043100940609     C* - $KEY1= 1° riga di tasti funzione
043200940609     C* - $KEY2= 2° riga di tasti funzione
043300940506     C*
043400961004     C                   CALLb     'XKEYMSG'
043500940506     C* input
043600940506     C                   PARM                    $FX
043700940506     C                   PARM                    $FC
043800940506     C                   PARM                    $FR
043900940506     C                   PARM                    $ULK              3 0
044000940506     C* output
044100940506     C                   PARM                    $KEY1            79
044200940506     C                   PARM                    $KEY2            79
044300950301     C* input
044400940721    >C                   PARM                    $L01              3 0
044500940721    >C                   PARM                    $L02              3 0
044600940506     C*
044700060224A1148C                   PARM                    $ALLFUNCT       480
044800060224A1148C*
044900940506     C                   ENDSR
045000940224     C/EJECT
045100940224     C************************************************************
045200940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
045300940224     C************************************************************
045400940224     C     GESPIE        BEGSR
045500940224     C*
045600940224     C                   SELECT
045700940224     C* Parzializzazioni
045800940224     C     $GEST         WHENEQ    'D1'
045900940506     C     $ULKD1        IFEQ      0
046000940506     C* riempimento opzioni
046100940506     C                   EXSR      F24D1
046200940506     C                   ELSE
046300940506     C                   WRITE     FMTZ1
046400940506     C                   ENDIF
046500940506     C* Lista
046600940506     C     $GEST         WHENEQ    'S1'
046700940506     C     $ULKS1        IFEQ      0
046800940506     C                   EXSR      F24S1
046900940506     C                   ELSE
047000940506     C                   WRITE     FMTZ2
047100940506     C                   ENDIF
047200940916     C* Modifica valori assunti
047300940916     C     $GEST         WHENEQ    'D2'
047400940916     C     $ULKD2        IFEQ      0
047500940916     C                   EXSR      F24D2
047600940916     C                   ELSE
047700940916     C                   WRITE     FMTZ3
047800940916     C                   ENDIF
047900940506     C                   ENDSL
048000940506     C*
048100940224     C                   ENDSR
048200940224     C/EJECT
048300940224     C************************************************************
048400940224     C* GESTIONE PARZIALIZZAZIONI
048500940224     C************************************************************
048600940224     C     GESD1         BEGSR
048700940224     C* inizializzazione videata
048800940223     C     $INZD1        IFEQ      *ON
048900940127     C                   EXSR      INZD1
049000940223     C                   MOVE      *OFF          $INZD1
049100941027     C                   ENDIF
049200940223     C* emissione piede videata se proveniente da altra
049300940223     C* salvataggio valore formato di provenienza
049400940202     C     $LASTV        IFNE      'D1'
049500940223     C* salvo stato F17 per sapere cosa fare in caso di F12
049600940223     C                   MOVE      *INKR         WINKR
049700940202     C                   WRITE     FMTT1
049800940224     C                   EXSR      GESPIE
049900940202     C                   MOVE      $LASTV        $LASTG
050000940202     C                   MOVE      'D1'          $LASTV
050100940707     C                   ENDIF
050200940707     C*
050300940707     C* se esistono errori sulla videata
050400940707     C* emetto la write del formato a indicatori spenti per vedere
050500940707     C* le eventuali decodifiche
050600940707     C     *IN99         IFEQ      *ON
050700940707     C                   MOVEA     *IN           WIN
050800940707     C                   MOVE      *ALL'0'       IN5098           49
050900940707     C                   MOVEA     IN5098        *IN(50)
051000940707     C                   WRITE     FMTD1
051100940707     C                   MOVEA     WIN           *IN
051200940707     C                   ENDIF
051300940131     C*
051400940131     C                   EXFMT     FMTD1
051500940506     C*
051600941205     C* controllo abilitazione tasti funzione
051700940506     C                   EXSR      CTRKD1
051800940506     C*
051900940506     C     *IN99         IFEQ      *OFF
052000940131     C*
052100940117     C                   SELECT
052200940916     C* F2=Decodifica
052300940916     C     $TASTO        WHENEQ    F02
052400940916     C                   EXSR      F02D1
052500940131     C* F3=Fine
052600940506     C     $TASTO        WHENEQ    F03
052700940307     C                   EXSR      F03D1
052800940131     C* F4=Ricerca
052900940506     C     $TASTO        WHENEQ    F04
053000940127     C                   EXSR      SEARCH
053100940916     C* F5=Refresh
053200940916     C     $TASTO        WHENEQ    F05
053300940916     C                   EXSR      F05D1
053400941206     C* F8=Successivo
053500941206     C     $TASTO        WHENEQ    F08
053600941206     C                   EXSR      F08D1
053700950301     C* F10=Immissione
053800950301     C     $TASTO        WHENEQ    F10
053900950301     C                   EXSR      F10D1
054000940131     C* F12=Ritorno
054100940506     C     $TASTO        WHENEQ    F12
054200940307     C                   EXSR      F12D1
054300940131     C* F18=Modifica valori assunti
054400940506     C     $TASTO        WHENEQ    F18
054500940307     C                   EXSR      F18D1
054600940224     C* F24=Altri tasti
054700940506     C     $TASTO        WHENEQ    F24
054800940309     C                   EXSR      F24D1
054900940131     C*
055000940117     C                   OTHER
055100940127     C                   EXSR      CTRD1
055200940127     C     *IN99         IFEQ      *OFF
055300940131     C* definisco l'ordinamento
055400950410     C                   EXSR      DEFORD
055500950410     C* richiamo la lista
055600940127     C                   MOVE      'S1'          $GEST
055700940223     C                   MOVE      *ON           $INZS1
055800940302     C                   MOVE      *BLANK        W1POS
055900940211     C                   ENDIF
056000940224     C*
056100940117     C                   ENDSL
056200940224     C*
056300940506     C                   ENDIF
056400940117     C                   ENDSR
056500940224     C/EJECT
056600940224     C************************************************************
056700940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
056800940224     C************************************************************
056900940127     C     INZD1         BEGSR
057000940318     C*
057100940318     C                   CLEAR                   FMTD1
057200941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
057300941013     C*    parzializzazioni
057400941104     C                   SELECT
057500941104     C* il pgm è richiamato e
057600941104     C* sono stati passati dei valori di parzializzazione ma non
057700941104     C* la chiave univoca
057800950419     C     OPZ062        WHENNE    *BLANK
057900950403     C* imposto i valori passati sui campi a video
058000950419     C                   MOVE      TPR062        D1TPR
058100950419     C                   MOVE      LIB062        D1LIB
058200950421     C* se passata la richiesta di gestione differenziata di
058300950421     C* società o capofila, imposto il flag a video col valore in
058400950421     C* ingresso
058500961128     C     fsc062        IFNE      *BLANK
058600950426     C                   MOVEL     FSC062        D1FSC
058700950421     C* altrimenti, come default, gestisco la società di riferimento
058800950421     C                   ELSE
058900950421     C                   MOVEL     'S'           D1FSC
059000950421     C                   ENDIF
059100950421     C* il pgm non è richiamato
059200950421     C     OPZ062        WHENEQ    *BLANK
059300950421     C* imposto dei valori di default
059400950421     C                   MOVE      *BLANK        D1TPR
059500950421     C                   MOVE      *BLANK        D1LIB
059600950421     C                   MOVEL     'S'           D1FSC
059700950421     C                   ENDSL
059800950421     C*
059900950421     C* se siamo in logica di gruppo sia a livello aziendale
060000950421     C     XSCSOC        IFNE      XSCCPA
060100950421     C* che per la singola procedura anagrafica
060200000802C1396X*****GSP008        ANDEQ     'G'
060300000802C1396C     GSP045        ANDEQ     'G'
060400950421     C* ma non siamo in ricerca
060500950421     C     OPZ062        ANDNE     '01'
060600950421     C* abilito la scelta alla società capofila
060700950421     C                   SETON                                        09
060800950421     C                   ENDIF
060900950421     C*
061000940224     C*
061100940117     C                   ENDSR
061200940506     C/EJECT
061300940506     C************************************************************
061400940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
061500940609     C* INPUT
061600941205     C* $FC1 = Abilitazioni videata D1
061700940509     C* OUTPUT
061800940509     C* *IN97 *IN99 Errori
061900940506     C************************************************************
062000940506     C     CTRKD1        BEGSR
062100941205     C*
062200940506     C                   SETOFF                                       99
062300940506     C                   MOVEA     $FC1          $FC
062400940506     C                   EXSR      CTRKEY
062500940506     C     *IN99         IFEQ      *ON
062600940506     C                   SETON                                        97
062700940506     C                   ENDIF
062800941205     C*
062900940506     C                   ENDSR
063000940916     C/EJECT
063100940916     C************************************************************
063200940916     C* GESTIONE F02 VIDEO D1
063300940916     C************************************************************
063400940916     C     F02D1         BEGSR
063500940916     C*
063600940916     C* per decodificare la videata richiamo la routine dei ctrl
063700940916     C* e poi disattivo gli eventuali messaggi di errore
063800940916     C                   EXSR      CTRD1
063900940916     C*
064000940916     C                   MOVE      *ALL'0'       IN5098
064100940916     C                   MOVEA     IN5098        *IN(50)
064200940916     C*
064300940916     C                   ENDSR
064400940224     C/EJECT
064500940131     C************************************************************
064600940307     C* GESTIONE F03 VIDEO D1
064700940131     C************************************************************
064800940307     C     F03D1         BEGSR
064900940131     C*
065000940307     C                   MOVE      *ON           $FINE
065100950419     C                   MOVE      '1'           RET062
065200940325     C* fine programma
065300940325     C                   EXSR      ENDPGM
065400940307     C*
065500940307     C                   ENDSR
065600940916     C/EJECT
065700940916     C************************************************************
065800940916     C* GESTIONE F05 VIDEO D1
065900940916     C************************************************************
066000940916     C     F05D1         BEGSR
066100940916     C*
066200940916     C                   MOVE      *ON           $INZD1
066300940916     C*
066400940916     C                   ENDSR
066500941206     C/EJECT
066600941206     C************************************************************
066700941206     C* GESTIONE F08 VIDEO D1
066800941206     C************************************************************
066900941206     C     F08D1         BEGSR
067000941206     C*
067100941206     C                   MOVE      *ON           $FINE
067200950419     C                   MOVE      '0'           RET062
067300941206     C*
067400941206     C                   ENDSR
067500950301     C/EJECT
067600950301     C************************************************************
067700950301     C* GESTIONE F10 VIDEO D1
067800950301     C************************************************************
067900950301     C     F10D1         BEGSR
068000950301     C*
068100950419     C                   RESET                   A063DS
068200950419     C                   MOVEL     '01'          OPZ063
068300950301     C* passare anche le parzializzazioni ricevute relative alla chiave
068400950301     C* univoca : se ce ne sono saranno usate come default d'immissione
068500961128     C                   MOVEL     D1FSC         FSC063
068600950419     C                   MOVEL     TPR062        TPR063
068700950419     C                   MOVEL     LIB062        LIB063
068800950403     C* codice di riferimento per le valorizzazioni di default
068900950421     C                   MOVE      $TPR          DF1063
069000950421     C                   MOVE      $LIB          DF2063
069100950420     C                   MOVEL     A063DS        KPJBU
069200950419     C                   CALL      'ANA063R'
069300950301     C                   PARM                    KPJBA
069400950419     C                   MOVEL     KPJBU         A063DS
069500950301     C* ritorno da PGM gestione
069600950419     C                   EXSR      RTN063
069700950301     C*
069800950301     C                   MOVE      *HIVAL        $LASTV
069900950301     C*
070000950301     C                   ENDSR
070100940307     C/EJECT
070200940307     C************************************************************
070300940307     C* GESTIONE F12 VIDEO D1
070400940307     C************************************************************
070500940307     C     F12D1         BEGSR
070600940307     C*
070700940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
070800940307     C*  per F12 torno alla lista
070900940307     C     $LOP          IFEQ      2
071000940307     C     WINKR         OREQ      *ON
071100940307     C                   MOVE      'S1'          $GEST
071200940307     C*  altrimenti, per F12 torno a menu
071300940307     C                   ELSE
071400940307     C                   MOVE      *ON           $FINE
071500950419    >C                   MOVE      '2'           RET062
071600941027     C                   ENDIF
071700940307     C*
071800940307     C                   ENDSR
071900940307     C/EJECT
072000940307     C************************************************************
072100940307     C* GESTIONE F18 VIDEO D1
072200940307     C************************************************************
072300940307     C     F18D1         BEGSR
072400940307     C*
072500940307     C                   MOVE      'D2'          $GEST
072600940307     C                   MOVEL     *ON           $INZD2
072700940307     C*
072800940307     C                   ENDSR
072900940307     C/EJECT
073000940307     C************************************************************
073100940307     C* GESTIONE F24 VIDEO D1
073200940609     C* input
073300940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
073400940509     C* $FX1 =Testi tasti funzione
073500940509     C* $FC1 = Abilitazioni
073600940509     C* $FR1 = Inversioni di fondo
073700940307     C************************************************************
073800940307     C     F24D1         BEGSR
073900940506     C*
074000940506     C                   MOVEA     $FX1          $FX
074100940506     C                   MOVEA     $FC1          $FC
074200940506     C                   MOVEA     $FR1          $FR
074300940506     C                   Z-ADD     $ULKD1        $ULK
074400941220    >C                   Z-ADD     79            $L01
074500941220    >C                   Z-ADD     79            $L02
074600940506     C                   EXSR      RIEKEY
074700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
074800940720     C                   MOVEA     $FC           $FC1
074900940506     C                   MOVEL     $KEY1         Z1KE1
075000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
075100940506     C                   MOVEL     $KEY2         Z1KE2
075200940506     C                   WRITE     FMTZ1
075300940506     C*
075400940307     C*
075500940307     C                   ENDSR
075600940307     C/EJECT
075700940307     C************************************************************
075800940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
075900940307     C************************************************************
076000940307     C     CTRD1         BEGSR
076100940307     C*
076200940127     C                   SETOFF                                       99
076300940127     C* non imposto pos. del cursore
076400940127     C                   Z-ADD     0             H1RIGA
076500940127     C                   Z-ADD     0             H1COLO
076600940131     C*
076700950419     C*  elencare gli eventuali controlli sui campi di
076800950419     C*  parzializzazione, ricordando che quasi sempre essi
076900950419     C*  non ne avranno perchè determinano solo il range di
077000950419     C*  scrittura della lista e quindi (nel caso di campi
077100950419     C*  alfanumerici) saranno trattati come chiavi parziali
077200950420     C     D1FSC         IFNE      WFSC
077300950419     C*
077400950419     C* Imposto campo WWWSOC
077500950419     C*
077600950419     C     D1FSC         IFEQ      'S'
077700950419     C                   MOVE      XSCSOC        WWWSOC
077800950419     C                   ELSE
077900950419     C                   MOVE      XSCCPA        WWWSOC
078000950419     C                   END
078100950420     C*
078200950420     C* la 1a volta non serve perchè è già stata fatta dalla routine
078300950420     C* INZVAR, le successive invece serve perchè bisogna riportare
078400950420     C* tutte le schiere al loro valore iniziale
078500950420     C     WFSC          IFNE      *BLANK
078600950420     C                   EXSR      DEFOPZ
078700950420     C                   ENDIF
078800950420     C*
078900950420     C* Ulteriore definizione delle opzioni utilizzabili a pgm
079000950420     C*
079100950420     C                   EXSR      ULTOPZ
079200950420     C                   MOVE      D1FSC         WFSC
079300950428     C*
079400950428     C     D1LIB         IFNE      *BLANK
079500961004     C                   CALLb     'XALLINEA'
079600950428     C                   PARM                    D1LIB
079700950428     C                   PARM      3             LEN               5 0
079800950428     C                   PARM                    XSCINP
079900950428     C                   ENDIF
080000950420     C*
080100940131     C*
080200950420     C                   ENDIF
080300940117     C                   ENDSR
080400950410     C/EJECT
080500950410     C************************************************************
080600950410     C* DEFINIZIONE DELL'ORDINAMENTO
080700950410     C************************************************************
080800950410     C     DEFORD        BEGSR
080900950410     C*
081000950410     C* definisco l'ordinamento della lista a seconda dei valori
081100950410     C* impostati nel video di parzializzazione
081200950410     C*
081300950410     C                   SELECT
081400950419     C*  nel caso di più viste logiche e quindi più campi di
081500950419     C*  parzializzazione, qui vanno definiti i criteri per
081600950419     C*  determinare quale VL va letta, a seconda di quali campi
081700950419     C*  sono stati valorizzati
081800950419     C     D1TPR         WHENNE    *BLANK
081900950410     C                   Z-ADD     1             $ORD
082000950419     C     D1LIB         WHENNE    *BLANK
082100950426     C                   Z-ADD     1             $ORD
082200950410     C* imposto l'ordinamento 99 per ottenere in selezione solo il
082300950410     C* rcd con la chiave passata
082400950410     C     $UNIQ         WHENEQ    *ON
082500950410     C                   Z-ADD     99            $ORD
082600950410     C* nel caso non sia stato digitato alcun valore in parzializzazione
082700950410     C* utilizzerò l'ordinamento precedentemente memorizzato
082800950410     C* (temporaneamente o definitivamente) nei valori assunti
082900950410     C                   OTHER
083000950410     C                   ENDSL
083100950410     C                   ENDSR
083200940224     C/EJECT
083300940127     C************************************************************
083400940131     C* GESTIONE LISTA
083500940127     C************************************************************
083600940127     C     GESS1         BEGSR
083700940223     C* inizializzazione videata
083800940223     C     $INZS1        IFEQ      *ON
083900940127     C                   EXSR      INZS1
084000940223     C                   MOVE      *OFF          $INZS1
084100940127     C                   ENDIF
084200940223     C* emissione piede videata se proveniente da altra
084300940223     C* salvataggio valore formato di provenienza
084400940202     C     $LASTV        IFNE      'S1'
084500940202     C                   WRITE     FMTT1
084600940224     C                   EXSR      GESPIE
084700940202     C                   MOVE      $LASTV        $LASTG
084800940202     C                   MOVE      'S1'          $LASTV
084900941027     C                   ENDIF
085000940223     C*
085100940223     C     WMAX          IFEQ      0
085200940211     C                   WRITE     FMTD3                                        NO RCD
085300940211     C                   ENDIF
085400980715     C*
085500980715     C* Inizializzazione indice tasti funzione personalizzati
085600980715     C                   Eval      PosFun = 25
085700980715     C*
085800941026     C*Serve per emissione finestra pgm invio anA727r (per open/close
085900940831     C*window)
086000940831     C                   MOVE      '1'           WIN727            1
086100940127     C*
086200940131     C                   EXFMT     FMTC1
086300940204     C     C1NRR         IFNE      0
086400940204     C                   Z-ADD     C1NRR         WSFL
086500940204     C                   ENDIF
086600940127     C                   Z-ADD     SFLNRR        C1RCD
086700940506     C*
086800940506     C* controllo abilitazione tasti funzione
086900940506     C                   EXSR      CTRKS1
087000941205     C*
087100940506     C     *IN99         IFEQ      *OFF
087200940127     C*
087300941027     C                   SELECT
087400940127     C* F3=Fine
087500940506     C     $TASTO        WHENEQ    F03
087600940309     C                   EXSR      F03S1
087700941206     C* F8=Successivo
087800941206     C     $TASTO        WHENEQ    F08
087900941206     C                   EXSR      F08S1
088000940131     C* F10=Immissione
088100940506     C     $TASTO        WHENEQ    F10
088200940309     C                   EXSR      F10S1
088300940131     C* F12=Ritorno
088400940506     C     $TASTO        WHENEQ    F12
088500940309     C                   EXSR      F12S1
088600940131     C* F17=Parzializzazioni
088700940506     C     $TASTO        WHENEQ    F17
088800940309     C                   EXSR      F17S1
088900940131     C* F18=Modifica valori assunti
089000940506     C     $TASTO        WHENEQ    F18
089100940309     C                   EXSR      F18S1
089200940224     C* F23=Altre opzioni
089300940506     C     $TASTO        WHENEQ    F23
089400940309     C                   EXSR      F23S1
089500940224     C* F24=Altri tasti
089600940506     C     $TASTO        WHENEQ    F24
089700940309     C                   EXSR      F24S1
089800940127     C* ROLL-UP
089900940506     C     $TASTO        WHENEQ    ROLLUP
090000940127     C                   EXSR      ROLS1
090100980715     C*
090200980715     C* Personalizzata
090300980715     C     $PGF2(PosFun) Whenne    *BLANK
090400980715     C                   EXSR      FperS1
090500980715     C*
090600940127     C*
090700941027     C                   OTHER
090800940127     C* CONTROLLO DATI
090900940131     C                   EXSR      CTRC1
091000940201     C     *IN99         IFEQ      *OFF
091100940131     C                   EXSR      CTRS1
091200941027     C                   ENDIF
091300941205     C*
091400941027     C                   ENDSL
091500940127     C*
091600940506     C                   ENDIF
091700941205     C*
091800940127     C                   ENDSR
091900940224     C/EJECT
092000980715     C************************************************************
092100980715     C* Chiamata programma con tasto personalizzato
092200980715     C************************************************************
092300980715     C     FPerS1        BEGSR
092400980715     C*
092500980715     C* salvo la DS di ingresso
092600980715    >C                   MOVEL     A062DS        $SaveDS
092700980715     C*
092800980715    >C                   MOVEL     A062DS        KPJBU
092900980715    >C                   CALL      $PGF2(PosFun)                        22
093000980715     C                   PARM                    KPJBA
093100980715     C                   PARM                    NOMPGM
093200980715     C*
093300980715     C* ripristino la DS di ingresso
093400980715    >C                   MOVEL     $SaveDS       A062DS
093500980715     C*
093600980715     C                   ENDSR
093700980715     C/EJECT
093800940127     C************************************************************
093900940131     C* INIZIALIZZAZIONE LISTA
094000940127     C************************************************************
094100940127     C     INZS1         BEGSR
094200940302     C* pulizia SFL
094300940128     C                   SETOFF                                         3031
094400940131     C                   WRITE     FMTC1
094500940128     C                   SETON                                          31
094600940127     C*
094700020308C1550 *
094800020308  "   * visualizzo intestazione colonna slittamenti iva trasp. mista
094900020308  "   * solo se siamo in regime iva trasportatori mista
095000020308  "  C                   if        XScIvaRgm = '2'
095100020308  "  C                   eval      *in41 = *On
095200020308  "  C                   endif
095300020308C1550 *
095400940128     C* riempimento opzioni
095500940223     C                   MOVEA     $TX1          $TX
095600940223     C                   MOVEA     $FL1          $FL
095700940309     C                   Z-ADD     0             WCOZ
095800940225     C*  equivale al num.di elementi della schiera $TX1
095900980715     C                   Z-ADD     OPZLEN        WMXO
096000941220    >C                   Z-ADD     76            $L01
096100950426    >C                   Z-ADD     76            $L02
096200940128     C                   EXSR      RIEOPZ
096300941214     C                   Z-ADD     23            X
096400941214     C                   MOVE      FF23          $FC2(X)
096500940608     C                   MOVEL     WOPZ1         C1OPZ1
096600060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
096700940608     C                   MOVEL     WOPZ2         C1OPZ2
096800940225     C                   Z-ADD     WCOZ          C1COZ
096900940128     C*
097000940128     C* CARICAMENTO SFL A PAGINE
097100940128     C*
097200940201     C                   Z-ADD     0             S1NRR
097300940128     C                   Z-ADD     0             WMAX
097400940224     C*
097500940224     C* Posizionamento su file pilota
097600940302     C                   CLEAR                   C1POS
097700941214     C                   EXSR      SETANA
097800940302     C* ripristino i valori del posizionamento
097900940302     C                   MOVE      W1POS         C1POS
098000950419    >C* aggiungere gestione altri campi posizionamento se ci sono
098100940131     C*
098200940201     C* Lettura file pilota
098300941214     C                   EXSR      REDANA
098400940131     C*
098500940131     C* Carico 1a pagina
098600940127     C                   EXSR      ROLS1
098700940201     C                   Z-ADD     SFLPAG        WPAG
098800940127     C*
098900940127     C                   ENDSR
099000940127     C/EJECT
099100940127     C************************************************************
099200940131     C* CARICAMENTO PAGINA LISTA
099300940127     C************************************************************
099400940127     C     ROLS1         BEGSR
099500940127     C*
099600940128     C                   SETOFF                                       32
099700940223     C                   Z-ADD     0             Y
099800940127     C                   Z-ADD     WMAX          S1NRR
099900940127     C*
100000940127     C* Leggo dal file anagrafico per caricare la lista
100100940127     C*
100200941027     C     $EFILE        DOWEQ     *OFF
100300940201     C     Y             ANDLT     WPAG
100400940127     C*
100500940131     C                   EXSR      RIES1
100600940127     C*
100700940127     C                   ADD       1             S1NRR
100800940127     C                   ADD       1             Y
100900940127     C*
101000940131     C                   WRITE     FMTS1
101100940131     C*
101200941214     C                   EXSR      REDANA
101300940128     C*
101400941027     C                   ENDDO
101500940127     C*
101600940223     C                   Z-ADD     S1NRR         WMAX                 30
101700940127     C*
101800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
101900940127     C*
102000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
102100940127     C                   MVR                     RESTO             3 0
102200940131     C     PAGINE        MULT      SFLPAG        C1RCD
102300941027     C     RESTO         IFGT      0
102400940127     C                   ADD       1             C1RCD
102500941027     C                   ELSE
102600940407     C                   SUB       SFLPAG        C1RCD
102700940407     C                   ADD       1             C1RCD
102800941027     C                   ENDIF
102900940128     C*
103000940127     C                   ENDSR
103100940224     C/EJECT
103200940224     C************************************************************
103300940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
103400940224     C************************************************************
103500941214     C     SETANA        BEGSR
103600940224     C*
103700940224     C                   SELECT
103800941214     C*
103900941214     C* Gestione posizionamento
104000950301     C* Se il campo W1POS è > *blank imposto il posizionamento con
104100950301     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
104200941214     C*
104300950301    >C* Ord.1 = chiave ordinamento 1
104400940224     C     $ORD          WHENEQ    1
104500940302     C     W1POS         IFNE      *BLANK
104600950428     C                   MOVEL     WWWSOC        DLISOC
104700950426     C*
104800950426     C                   MOVEL     W1POS         DLITPR
104900950426     C                   MOVEL     *BLANK        DLILIB
105000940302     C                   ELSE
105100950426     C                   MOVEL     WWWSOC        DLISOC
105200950426     C                   MOVEL     D1TPR         DLITPR
105300950426     C                   MOVEL     D1LIB         DLILIB
105400941027     C                   ENDIF
105500950503     C     K3DLI1        SETLL     ANDLI01
105600940831     C* Ord.99= Chiave univoca
105700941104     C* (quando viene gestita la chiave univoca NON deve essere gestito
105800941104     C*  il posizionamento C1POS/W1POS)
105900940831     C     $ORD          WHENEQ    99
106000950426     C                   MOVEL     WWWSOC        DLISOC
106100950426     C                   MOVEL     D1TPR         DLITPR
106200950426     C                   MOVEL     D1LIB         DLILIB
106300950503     C     K3DLI1        SETLL     ANDLI01
106400940224     C                   ENDSL
106500940224     C*
106600940224     C                   ENDSR
106700940128     C/EJECT
106800940128     C************************************************************
106900940131     C* LETTURA RCD ARCHIVIO PILOTA
107000940128     C************************************************************
107100941214     C     REDANA        BEGSR
107200940128     C*
107300940131     C                   MOVEL     *OFF          $EFILE
107400940131     C                   MOVEL     *OFF          $RCDOK
107500940131     C*
107600941027     C     $EFILE        DOUEQ     *ON
107700940131     C     $RCDOK        OREQ      *ON
107800940131     C*
107900941027     C                   SELECT
108000940131     C     $ORD          WHENEQ    1
108100950502     C                   MOVEL     WWWSOC        DLISOC
108200950502     C     DLISOC        READE     ANDLI01                                21
108300940831     C     $ORD          WHENEQ    99
108400950502     C                   MOVEL     WWWSOC        DLISOC
108500950502     C     DLISOC        READE     ANDLI01                                21
108600941027     C                   ENDSL
108700940131     C*
108800940131     C                   MOVE      *IN21         $EFILE
108900940131     C*
109000941027     C     $EFILE        IFEQ      *OFF
109100941214     C                   EXSR      SELANA
109200941027     C                   ENDIF
109300940131     C*
109400941027     C                   ENDDO
109500940131     C*
109600940131     C                   MOVE      *IN21         *IN33                          SflEnd
109700940131     C*
109800940131     C                   ENDSR
109900940224     C/EJECT
110000940224     C************************************************************
110100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
110200940224     C************************************************************
110300940224     C     CLCPAG        BEGSR
110400940224     C* Input :
110500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
110600940224     C*          cursore
110700940224     C* - SFLPAG = numero rcd per pagina sfl
110800940224     C* Output :
110900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
111000940224     C*
111100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
111200940224     C                   MVR                     RESTO             3 0
111300940224     C     RESTO         IFGT      0
111400940224     C                   ADD       1             PAGINE
111500940224     C                   ENDIF
111600940224     C     PAGINE        MULT      SFLPAG        WPAG
111700940224     C*
111800940224     C                   ENDSR
111900940131     C/EJECT
112000940131     C************************************************************
112100940131     C* SELEZIONI SULLA RIGA LETTA
112200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
112300940314     C* non rientra nei parametri di parzializzazione
112400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
112500940314     C* superiore stabilito nei parametri
112600940314     C*
112700940131     C************************************************************
112800941214     C     SELANA        BEGSR
112900940131     C*
113000940202     C                   MOVEL     *ON           $RCDOK
113100940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
113200940131A1211C                   MOVEL     *hival        ALL9             15
113300950301     C* creo limite massimo parzializzazioni :
113400950301     C* per ogni valore alfanumerico creo il limite superiore
113500950301     C* per cui caricare, creando una variabile che contiene
113600950301     C* il valore digitato nel campo a video riempito di "9"
113700950419     C     D1LIB         CAT       ALL9:0        LIB999
113800940314     C*
113900950314     C*
114000940314     C                   SELECT
114100950301     C*
114200940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
114300950426     C*
114400950426     C     DLILIB        WHENLT    D1LIB
114500950426     C     DLILIB        ORGT      LIB999
114600950426     C                   MOVEL     *OFF          $RCDOK
114700940202     C*
114800940202     C                   ENDSL
114900950314     C* FINE SELEZIONE
115000950314     C*  nel caso che il dato letto sia > del limite superiore e
115100950314     C*  questo dato è la chiave della VL in lettura, allora per
115200950314     C*  evitare altri accessi inutili salto a fine file
115300950314     C                   SELECT
115400950502     C* ordinamento per registro/libro
115500950502     C     DLILIB        WHENGT    LIB999
115600950502     C     $ORD          ANDEQ     1
115700950502     C                   MOVEL     WWWSOC        DLISOC
115800950503     C     K2DLI1        SETGT     ANDLI01
115900950502     C                   ENDSL
116000950502     C* ordinamento per registro
116100950502     C     D1TPR         IFNE      *BLANK
116200950502     C     DLITPR        IFNE      D1TPR
116300950502     C     $ORD          ANDEQ     1
116400950502     C                   MOVEL     *OFF          $RCDOK
116500950502     C     *HIVAL        SETLL     ANDLI01
116600950502     C                   ENDIF
116700950502     C                   ENDIF
116800950419    >C** ordinamento unique per gestione POSTA
116900950426    >C*          $ORD      WHEQ 99
117000950426    >C*          DLITPR    IFNE D1TPR
117100950426    >C*          DLILIB    ORNE D1LIB
117200950426    >C*                    MOVEL*OFF      $RCDOK
117300950426    >C*          *HIVAL    SETLLANDLI01
117400950426    >C*                    ENDIF
117500950314    >C*
117600940202     C*
117700940202     C                   ENDSR
117800940131     C/EJECT
117900940131     C************************************************************
118000940131     C* RIEMPIMENTO RIGA LISTA
118100940131     C************************************************************
118200940131     C     RIES1         BEGSR
118300940131     C*
118400940131     C                   CLEAR                   FMTS1
118500950301     C*
118600950301     C* riempio i campi del sfl, compresi gli hidden con i valori
118700950301     C* del rcd letto
118800950419     C                   MOVEL     DLITPR        S1TPR
118900950419     C                   MOVEL     DLILIB        S1LIB
119000950419     C                   MOVEL     DLIDES        S1DES
119100950419     C                   MOVEL     DLISER        S1SER
119200950503     C*
119300950503     C* azzero le memorizzazioni degli attributi
119400950503     C* - presenza associazioni
119500950503     C                   MOVEL     *ZERO         H1B01
119600980804     C* - presenza slittamento libro
119700980804     C                   MOVEL     *ZERO         H1B02
119800020308C1550C* - presenza slittamento libro se iva trasportatori mista
119900020308C1550C                   MOVEL     *ZERO         H1B03
119901180105R275 C* - presenza regola per detrazione IVA a Credito
119902180105R275 C                   MOVEL     *ZERO         H1B04
120000070924C2051C* - presenza punti vendita
120100180105C2051C                   MOVEL     *ZERO         H1B14
120200950503     C* - rcd abilitato
120300950503     C                   MOVEL     *ZERO         H1ABIL
120400950420     C*
120500950503     C* reperisco se associazioni presenti
120600950428     C                   MOVE      WWWSOC        ALRSOC
120700950428     C                   MOVE      S1TPR         ALRTPR
120800950428     C                   MOVE      S1LIB         ALRLIB
120900950428     C     K3ALR2        SETLL     ANALR02L                               21
121000950503     C                   MOVE      *IN21         H1B01
121100980804     c*
121200980804     C* reperisco se presente slittamento
121300980804     C                   MOVE      WWWSOC        DLSSOC
121400980804     C                   MOVE      S1TPR         DLSTPR
121500980804     C                   MOVE      S1LIB         DLSLIB
121600980804     C     K3DLS1        SETLL     ANDLS01L                               21
121700980804     C                   MOVE      *IN21         H1B02
121800950503     C*
121900020308C1550C* reperisco se presente slittamento
122000020308  "  C                   move      WWWSoc        SLISocieta
122100020308  "  C                   move      S1TpR         SLITpReg
122200020308  "  C                   move      S1Lib         SLILibro
122300020308  "  C     K03SLI01      SETLL     ANSLI01L                               21
122400020308  "  C                   MOVE      *IN21         H1B03
122500020308C1550C*
122501180105R275 C* reperisco se presente regola per detrazione IVA a Credito
122502180105 "   C                   MOVE      WWWSOC        RICSocieta
122503180105 "   C                   MOVE      S1TPR         RICTpReg
122504180105 "   C                   MOVE      S1LIB         RICLibro
122505180105 "   C     K03RIC01      SETLL     SDGRIC01L                              21
122506180105 "   C                   MOVE      *IN21         H1B04
122507180105R275 C*
122600070924C2051C* reperisco se presente punto vendita
122700080418  "  C                   move      WWWSoc        PnrSocieta
122800080418  "  C                   move      S1TpR         PNrTpReg
122900080418  "  C                   move      S1Lib         PNrLibro
123000080421  "  c* 1 solo punto vendita per tipo/reg
123100080421  "  C     K03Pnr02      chain     ANPNr02L
123200080421  "  c                   if        %found(anpnr02L)
123300180105  "  C                   MOVE      *on           H1B14
123400080421  "  c                   move      pnrNumrea     H1NumREa
123500100202C2051c                   move      pnrPrgrea     H1PrgREa
123600100202C2135c                   move      pnrNumP       H1NumP
123700080421c2051c                   endif
123800950503     C* se stiamo visualizzando i libri della capofila anagrafica
123900950503     C* testo se per il codice recuperato ne esiste uno relativo sulla
124000950503     C* società di appartenenza dell'utente
124100950503     C     D1FSC         IFEQ      'C'
124200950503     C* salvo quei campi che potrebbero sporcarsi
124300950503     C                   MOVE      DLISOC        WSOC
124400950503     C* eseguo il test
124500950503     C                   MOVE      XSCSOC        DLISOC
124600950503     C     K3DLI1        SETLL     ANDLI01                                21
124700950503     C                   MOVEL     *IN21         H1ABIL
124800950503     C* ripristino i campi che si potevano sporcare
124900950503     C                   MOVE      WSOC          DLISOC
125000950503     C* mi riposiziono sul rcd successivo a quello scritto sul sfl
125100950503     C* altrimenti non si potrebbe essere sicuri di cosa leggerà
125200950503     C     K3DLI1        SETGT     ANDLI01
125300950503     C                   ENDIF
125400950301     C*
125500950301     C* imposto quegli attributi del sfl che sono modificabili
125600950301     C* al richiamo del determinato pgm
125700950301     C                   EXSR      ATRS1
125800940131     C*
125900940131     C                   ENDSR
126000950301     C/EJECT
126100950301     C************************************************************
126200950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
126300950301     C************************************************************
126400950301     C     ATRS1         BEGSR
126500950301     C*
126600950419     C* impostare le condizioni per ripristinare gli attributi
126700950419     C* della riga del sfl (se ce ne sono)
126800950420     C* azzero i bottoni
126900950420     C                   MOVEL     X'A0'         S1B01A
127000980804     C                   MOVEL     X'A0'         S1B02A
127100020308C1550C                   MOVEL     X'A0'         S1B03A
127101180105R275 C                   MOVEL     X'A0'         S1B04A
127200180105C2051C                   MOVEL     X'A0'         S1B14A
127300950503     C* azzero gli altri attributi
127400950503     C                   MOVEL     X'A0'         S1TPRA
127500950503     C                   MOVEL     X'A0'         S1LIBA
127600950420     C*
127700950420     C* li valorizzo a seconda dello stato ricevuto o testato
127800950503     C     H1B01         IFEQ      *ON
127900950420     C                   MOVEL     PRES          S1B01A
128000950420     C                   ENDIF
128100980804     c*
128200980804     C     H1B02         IFEQ      *ON
128300980804     C                   MOVEL     PRES          S1B02A
128400980804     C                   ENDIF
128500950428     C*
128600020322C1550 * se iva trasportatori
128700020322  "  C                   if        XScIvaRgm = '2'
128800020322  "   *  evidenzio il flag se presente slittamento
128900020322  "  C                   if        H1B03 = *on
129000020322  "  C                   movel     Pres          S1B03A
129100020322  "  C                   endif
129200020322  "   *  visualizzo l'intestazione 'Sm' della colonna
129300020311  "  C                   eval      *in41 = *On
129400020322  "  C                   endif
129500020322C1550 *
129501180105R275 C     H1B04         IFEQ      *ON
129502180105 "   C                   MOVEL     PRES          S1B04A
129503180105 "   C                   ENDIF
129504180105R275  *
129600070924      *
129700070924c2051 *  evidenzio il flag se presente punto vendita
129800180105  "  C                   if        H1B14 = *on
129900180105  "  C                   movel     Pres          S1B14A
130000070924c2051C                   endif
130100070924      *
130200950503     C     H1ABIL        IFEQ      *ON
130300950428     C                   MOVEL     ABIL          S1TPRA
130400950503     C                   MOVEL     ABIL          S1LIBA
130500950428     C                   ENDIF
130600950301     C*
130700950301     C                   ENDSR
130800941205     C/EJECT
130900941205     C************************************************************
131000941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
131100941205     C* INPUT
131200941205     C* $FC2 = Abilitazioni videata S1
131300941205     C* OUTPUT
131400941205     C* *IN97 *IN99 Errori
131500941205     C************************************************************
131600941205     C     CTRKS1        BEGSR
131700941205     C*
131800941205     C                   SETOFF                                       99
131900941205     C                   MOVEA     $FC2          $FC
132000941205     C                   EXSR      CTRKEY
132100941205     C     *IN99         IFEQ      *ON
132200941205     C                   SETON                                        97
132300941205     C                   ENDIF
132400941205     C*
132500941205     C                   ENDSR
132600941206     C/EJECT
132700941206     C************************************************************
132800941206     C* GESTIONE F08 VIDEO S1
132900941206     C************************************************************
133000941206     C     F08S1         BEGSR
133100941206     C*
133200941206     C                   MOVE      *ON           $FINE
133300950419     C                   MOVE      '0'           RET062
133400941206     C*
133500941206     C                   ENDSR
133600940309     C/EJECT
133700940309     C************************************************************
133800940309     C* GESTIONE F03 VIDEO S1
133900940309     C************************************************************
134000940309     C     F03S1         BEGSR
134100940309     C*
134200940309     C                   MOVE      *ON           $FINE
134300950419    >C                   MOVE      '1'           RET062
134400940325     C* fine programma
134500940325     C                   EXSR      ENDPGM
134600940309     C*
134700940309     C                   ENDSR
134800940309     C/EJECT
134900940309     C************************************************************
135000940309     C* GESTIONE F10 VIDEO S1
135100940609     C* AGGIUNTA RECORD
135200940309     C************************************************************
135300940309     C     F10S1         BEGSR
135400940309     C*
135500950421     C                   RESET                   A063DS
135600950421     C                   MOVEL     '01'          OPZ063
135700950102     C* passare anche le parzializzazioni ricevute relative alla chiave
135800950102     C* univoca : se ce ne sono saranno usate come default d'immissione
135900961128     C                   MOVEL     d1fsc         FSC063
136000950419     C                   MOVEL     TPR062        TPR063
136100950419     C                   MOVEL     LIB062        LIB063
136200950403     C* codice di riferimento per le valorizzazioni di default
136300950427     C                   MOVE      $TPR          DF1063
136400950427     C                   MOVE      $LIB          DF2063
136500950427     C                   MOVEL     A063DS        KPJBU
136600950427     C                   CALL      'ANA063R'
136700940309     C                   PARM                    KPJBA
136800950427     C                   MOVEL     KPJBU         A063DS
136900940309     C* ritorno da PGM gestione
137000950427     C                   EXSR      RTN063
137100940309     C     WINZS1        IFEQ      *ON
137200940309     C                   MOVE      *ON           $INZS1
137300940309     C* se esistevano già righe sul sfl
137400940309     C* calcolo pagina a cui deve posizionarsi
137500940309     C     WSFL          IFGT      0
137600940309     C                   EXSR      CLCPAG
137700940309     C* altrimenti carico solo la 1a pagina
137800940309     C                   ELSE
137900940309     C                   Z-ADD     SFLPAG        WPAG
138000940309     C                   ENDIF
138100940309     C                   ENDIF
138200940309     C*
138300940627     C                   MOVE      *HIVAL        $LASTV
138400940627     C*
138500940309     C                   ENDSR
138600940309     C/EJECT
138700940309     C************************************************************
138800940309     C* GESTIONE F12 VIDEO S1
138900940309     C************************************************************
139000940309     C     F12S1         BEGSR
139100940309     C*
139200940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
139300940309     C     $LOP          IFEQ      1
139400940309     C                   MOVE      'D1'          $GEST
139500940309     C*  se la 1a videata è questa, per F12 torno a menu
139600940309     C                   ELSE
139700940309     C                   MOVE      *ON           $FINE
139800950419    >C                   MOVE      '2'           RET062
139900941027     C                   ENDIF
140000940309     C*
140100940309     C                   ENDSR
140200940309     C/EJECT
140300940309     C************************************************************
140400940309     C* GESTIONE F17 VIDEO S1
140500940309     C************************************************************
140600940309     C     F17S1         BEGSR
140700940309     C*
140800940309     C                   MOVE      'D1'          $GEST
140900941104     C* se il pgm è stato richiamato con chiave univoca
141000941104     C* e dopo è stata richiesta una parzializzazione
141100941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
141200941104     C* valore; propongo come ordinamento il 1°)
141300941213     C     $UNIQ         IFEQ      *ON
141400950331     C                   MOVE      *OFF          $UNIQ
141500941027     C                   ENDIF
141600940831     C     $ORD          IFEQ      99
141700940831     C                   Z-ADD     1             $ORD
141800941104     C                   MOVE      *ON           $INZD1
141900941027     C                   ENDIF
142000940309     C*
142100940309     C                   ENDSR
142200940309     C/EJECT
142300940309     C************************************************************
142400940309     C* GESTIONE F18 VIDEO S1
142500940309     C************************************************************
142600940309     C     F18S1         BEGSR
142700940309     C*
142800940309     C                   MOVE      'D2'          $GEST
142900940309     C                   MOVEL     *ON           $INZD2
143000940831     C     $ORD          IFEQ      99
143100940831     C                   Z-ADD     1             $ORD
143200941027     C                   ENDIF
143300940309     C*
143400940309     C                   ENDSR
143500940309     C/EJECT
143600940309     C************************************************************
143700940309     C* GESTIONE F23 VIDEO S1
143800940309     C************************************************************
143900940309     C     F23S1         BEGSR
144000940309     C*
144100941220    >C                   Z-ADD     76            $L01
144200941220    >C                   Z-ADD     76            $L02
144300940309     C                   EXSR      RIEOPZ
144400941214     C                   Z-ADD     23            X
144500941214     C                   MOVE      FF23          $FC2(X)
144600940608     C                   MOVEL     WOPZ1         C1OPZ1
144700060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
144800940608     C                   MOVEL     WOPZ2         C1OPZ2
144900940309     C                   Z-ADD     WCOZ          C1COZ
145000940309     C*
145100940309     C                   ENDSR
145200940309     C/EJECT
145300940309     C************************************************************
145400940309     C* GESTIONE F24 VIDEO S1
145500940309     C************************************************************
145600940309     C     F24S1         BEGSR
145700940506     C*
145800940506     C                   MOVEA     $FX2          $FX
145900940506     C                   MOVEA     $FC2          $FC
146000940506     C                   MOVEA     $FR2          $FR
146100940506     C                   Z-ADD     $ULKS1        $ULK
146200941220    >C                   Z-ADD     79            $L01
146300941220    >C                   Z-ADD     79            $L02
146400940506     C                   EXSR      RIEKEY
146500940506     C                   Z-ADD     $ULK          $ULKS1            3 0
146600940720     C                   MOVEA     $FC           $FC2
146700940506     C                   MOVEL     $KEY1         Z2KE1
146800060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
146900940506     C                   MOVEL     $KEY2         Z2KE2
147000940506     C                   WRITE     FMTZ2
147100940506     C*
147200940309     C                   ENDSR
147300940128     C/EJECT
147400940128     C************************************************************
147500940131     C* CONTROLLO TESTATA LISTA
147600940128     C************************************************************
147700940131     C     CTRC1         BEGSR
147800940128     C*
147900940201     C                   MOVE      *OFF          *IN99
148000940131     C*
148100940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
148200940202     C* L'INIZIALIZZAZIONE LISTA
148300940202     C*
148400941214     C     C1POS         IFNE      W1POS
148500950419    >C* aggiungere in OR altri campi posizionamento se ci sono
148600940202     C                   EXSR      POSS1
148700941027     C                   ENDIF
148800940202     C*
148900940202     C                   ENDSR
149000940202     C/EJECT
149100940202     C************************************************************
149200940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
149300940202     C************************************************************
149400940202     C     POSS1         BEGSR
149500940202     C*
149600940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
149700940202     C*
149800941118   B1C     WMAX          IFGT      0
149900940202     C                   READC     FMTS1                                  21
150000941118   B2C     *IN21         DOWEQ     *OFF
150100940202     C     *IN99         ANDEQ     *OFF
150200940202     C*
150300941118   B3C     S1OPZ         IFNE      0
150400940202     C                   SETON                                        51  99
150500941118   E3C                   ENDIF
150600940202     C* se rilevata opzione attiva, attivo sflnxtchg
150700941118   B3C     *IN99         IFEQ      *ON
150800940202     C                   MOVE      *ON           *IN32
150900950301     C*
151000950301     C* imposto quegli attributi del sfl che sono modificabili
151100950301     C* al richiamo del determinato pgm
151200941118     C                   EXSR      ATRS1
151300941118     C*
151400940202     C                   UPDATE    FMTS1
151500941118   X3C                   ELSE
151600940202     C                   READC     FMTS1                                  21
151700941118   E3C                   ENDIF
151800940202     C*
151900941118   E2C                   ENDDO
152000940202     C*
152100941118   E1C                   ENDIF
152200940202     C*
152300940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
152400940221     C* AL VALORE DI POSIZIONAMENTO :
152500940221     C* se questo è maggiore del valore del rispettivo campo di
152600940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
152700940221     C* con i valori di parzializzazione.
152800940221     C*
152900941118   B1C     *IN99         IFEQ      *OFF
153000940721    >C                   MOVE      *BLANK        W1POS
153100940202     C*
153200941118   B2C                   SELECT
153300940202     C*
153400940202     C     $ORD          WHENEQ    1
153500940202     C*
153600950420    >C     C1POS         IFGT      D1TPR
153700950406     C                   MOVEL     C1POS         W1POS
153800950419    >C* sostituire C1POS/W1POS con altri campi posizionamento
153900950419    >C* se il campo in parzializzazione ne richiede uno specifico
154000941027     C                   ENDIF
154100940131     C*
154200940202     C                   MOVE      *ON           *IN99
154300940223     C                   MOVE      *ON           $INZS1
154400940831     C*
154500940831     C     $ORD          WHENEQ    99
154600940831     C*
154700940831     C                   MOVE      *ON           *IN99
154800940831     C                   MOVE      *ON           $INZS1
154900940831     C*
155000940202     C*
155100940202     C     $ORD          WHENEQ    2
155200940202     C*
155300950419    >C     C1POS         IFGT      D1LIB
155400941214     C                   MOVE      C1POS         W1POS
155500950419    >C* sostituire C1POS/W1POS con altri campi posizionamento
155600950419    >C* se il campo in parzializzazione ne richiede uno specifico
155700941027     C                   ENDIF
155800940202     C*
155900940202     C                   MOVE      *ON           *IN99
156000940223     C                   MOVE      *ON           $INZS1
156100940202     C*
156200941118   E2C                   ENDSL
156300940202     C*
156400941118   E1C                   ENDIF
156500940202     C*
156600940131     C                   ENDSR
156700940131     C/EJECT
156800940131     C************************************************************
156900940131     C* CONTROLLO OPZIONI LISTA
157000940131     C************************************************************
157100940131     C     CTRS1         BEGSR
157200940131     C*
157300940202     C                   MOVEL     *OFF          $ESCI
157400940201     C                   SETOFF                                       99
157500940131     C                   Z-ADD     0             S1OPZ
157600940131     C*
157700940127     C* Leggo il sfl solo se ci sono rcd
157800941118   B1C     WMAX          IFGT      0
157900940131     C                   READC     FMTS1                                  21
158000940127     C*
158100940131     C* esce se fine sfl o errore che richiede l'uscita
158200941118   B2C     *IN21         DOWEQ     *OFF
158300940131     C     $ESCI         ANDEQ     *OFF
158400940201     C                   Z-ADD     S1NRR         C1RCD
158500940131     C* ctrl su riga
158600940131     C                   EXSR      RECS1
158700940131     C* gestione opzioni
158800941118   B3C     S1OPZ         IFNE      0
158900940201     C     *IN99         ANDEQ     *OFF
159000940131     C                   EXSR      OPZS1
159100941118   E3C                   ENDIF
159200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
159300941118   B3C     *IN99         IFEQ      *ON
159400940131     C                   MOVE      *ON           *IN32
159500941118   E3C                   ENDIF
159600950301     C*
159700950301     C* imposto quegli attributi del sfl che sono modificabili
159800950301     C* al richiamo del determinato pgm
159900941118     C                   EXSR      ATRS1
160000941118     C*
160100940131     C                   Z-ADD     0             S1OPZ
160200940223     C*
160300940127     C                   UPDATE    FMTS1
160400941125     C                   MOVE      *OFF          *IN32
160500940223     C*
160600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
160700941118   B3C     $ESCI         IFEQ      *OFF
160800940131     C                   READC     FMTS1                                  21
160900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
161000941118   B4C     *IN21         IFEQ      *ON
161100940201     C                   MOVE      WINZS1        $INZS1
161200940204     C* calcolo pagina a cui deve posizionarsi
161300940728     C     WSFL          IFGT      0
161400940728     C                   EXSR      CLCPAG
161500940728     C                   ELSE
161600940728     C                   Z-ADD     SFLPAG        WPAG
161700940728     C                   ENDIF
161800941118   E4C                   ENDIF
161900941118   E4C                   ENDIF
162000940131     C*
162100941118   E2C                   ENDDO
162200940127     C*
162300941118   E1C                   ENDIF
162400940131     C*
162500940127     C                   ENDSR
162600940127     C/EJECT
162700940127     C************************************************************
162800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
162900940127     C************************************************************
163000940131     C     RECS1         BEGSR
163100940131     C*
163200940201     C* reset indicatori DSPATR
163300940201     C                   MOVE      *ALL'0'       IN4049           10
163400940201     C                   MOVEA     IN4049        *IN(40)
163500940201     C*
163600940131     C* Controllo se Utente Autorizzato alla scelta
163700940202     C     S1OPZ         IFNE      0
163800940131     C                   Z-ADD     1             X
163900940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
164000980715     C* mi salvo l'indice dell' opzione
164100980715     C   21              Z-add     X             PosOpz            2 0
164200940211     C     *IN21         IFEQ      *OFF
164300940223     C     $FL1(X)       OREQ      'N'
164400940131     C                   MOVE      *ON           $ESCI
164500940201     C                   SETON                                        504099
164600940131     C                   ENDIF
164700940202     C                   ENDIF
164800080617     C*
164900080617c2051C* non ammessa opzione 27 per tipo registro acquisti
165000080617  "  C     S1OPZ         IFeq      27
165100080617  "  C     S1Tpr         andeq     '1'
165200080617  "  C                   SETON                                        554099
165300080617C2051C                   ENDIF
165400940131     C*
165500940131     C                   ENDSR
165600941118     C/EJECT
165700941118     C************************************************************
165800941118     C* GESTIONE OPZIONI LISTA
165900941118     C************************************************************
166000941118     C     OPZS1         BEGSR
166100950301     C*
166200950301     C                   Z-ADD     RIRI          CURR
166300950301     C                   Z-ADD     COCO          CURC
166400941118     C*
166500941027     C                   SELECT
166600940225     C*
166700940225     C* se opz=Selezione passo la chiave al pgm chiamante
166800940225     C     S1OPZ         WHENEQ    1
166900950419    >C                   MOVE      S1OPZ         OPZ062
167000950419     C* passare nei campi appositi della DS A062DS il valore
167100950301     C* della chiave della VL univoca (se c'è)
167200950419    >C                   MOVE      S1TPR         TPR062
167300950419    >C                   MOVE      S1LIB         LIB062
167400950426    >C                   MOVE      D1FSC         FSC062
167500940225     C                   MOVE      *ON           $ESCI
167600940225     C                   MOVE      *ON           $FINE
167700940909     C*
167800940909     C                   MOVE      *HIVAL        $LASTV
167900950428     C* ABILITA
168000950428     C*
168100950428     C     S1OPZ         WHENEQ    11
168200950503     C     H1ABIL        IFNE      *ON
168300950428     C                   RESET                   A066DS
168400950428     C                   MOVE      '11'          OPZ066
168500950428     C                   MOVE      XSCCPA        CAP066
168600950428     C                   MOVE      XSCSOC        SOC066
168700950428     C                   MOVE      S1TPR         TPR066
168800950428     C                   MOVE      S1LIB         LIB066
168900950428     C                   MOVEL     A066DS        KPJBU
169000950428     C                   CALL      'ANA066R'
169100950428     C                   PARM                    KPJBA
169200950428     C                   MOVEL     KPJBU         A066DS
169300950428     C*
169400950503     C* ritorno da PGM abilitazione
169500950428     C                   EXSR      RTN066
169600950428     C*
169700950428     C* se è già abilitato do msg d'errore
169800950428     C                   ELSE
169900950428     C                   SETON                                        5399
170000950428     C                   ENDIF
170100950428     C*
170200950428     C* ASSOCIAZIONI LIBRI/RIFERIMENTI
170300950428     C*
170400950428     C     S1OPZ         WHENEQ    20
170500950428     C                   RESET                   A068DS
170600950428     C                   MOVE      '02'          OPZ068
170700950428     C                   MOVE      WWWSOC        SOC068
170800950428     C                   MOVE      S1TPR         TPR068
170900950428     C                   MOVE      S1LIB         LIB068
171000980804     C                   MOVE      *ZEROS        ALI068
171100980804     C                   MOVE      *BLANK        RIF068
171200950428     C                   MOVEL     A068DS        KPJBU
171300950428     C                   CALL      'ANA068R'
171400950428     C                   PARM                    KPJBA
171500950428     C                   MOVEL     KPJBU         A068DS
171600980804     C*
171700980804     C* ritorno da PGM
171800980804     C                   EXSR      RTN068
171900980804     C*
172000980804     C*
172100980804     C* SLITTAMENTO LIBRI IVA
172200980804     C*
172300980804     C     S1OPZ         WHENEQ    21
172400980804     C                   RESET                   A140DS
172500980804     C                   MOVE      '02'          OPZ140
172600980804     C***                MOVE      WWWSOC        SOC140
172700980804     C                   MOVE      S1TPR         TPR140
172800980804     C                   MOVE      S1LIB         LIB140
172900980804     C                   MOVEL     A140DS        KPJBU
173000980804     C                   CALL      'ANA140R'
173100980804     C                   PARM                    KPJBA
173200980804     C                   MOVEL     KPJBU         A140DS
173300980804     C*
173400980804     C* ritorno da PGM
173500980804     C                   EXSR      RTN140
173600020308C1550 *
173700020308  "   * Gestione slittamenti iva trasportatori mista
173800020308  "  C     S1OPZ         WHENEQ    22
173900020308C1550C                   exsr      OpzS1_22
173901180105R275 C*
173902180105  "  C* Gestione Regole detrazione IVA a Credito
173903180105  "  C     S1OPZ         WHENEQ    23
173904180105R275 C                   exsr      OpzS1_23
174000070924C2051 *
174100080418  "   * Gestione punto vendita
174200070924  "  C     S1OPZ         WHENEQ    27
174300070924C2051C                   exsr      OpzS1_27
174400980804     C*
174500940831     C* se opz=Invia scrive nella posta
174600950419     C*
174700950419     C*          S1OPZ     WHEQ 10
174800950419     C*                    RESETA727DS
174900950419     C*                    MOVE '01'      OPZ727
175000950419    >C*                    MOVEL______    TXT727           Testo
175100950419    >C*                    MOVEL'___'     TPA727
175200950419    >C*                    MOVEL______    KY1727
175300950419    >C*                    MOVEL______    KY2727
175400950419    >C*                    MOVEL______    KY3727
175500950419    >C*                    MOVEL______    KY4727
175600950419     C*                    MOVE 'E'       PGM727
175700950419     C*                    MOVE XSCSOC    SOC727
175800950419     C*                    MOVE WIN727    INZ727
175900950419     C*                    CLEARKPJBU
176000950419     C*                    MOVELA727DS    KPJBU
176100950419     C*                    CALL 'ANA727R'
176200950419     C*                    PARM           KPJBA
176300950419     C*                    MOVELKPJBU     A727DS
176400950419     C*
176500950419     C*                    EXSR RTN727
176600950419     C*
176700950419     C*                    MOVE *HIVAL    $LASTV
176800940831     C*
176900940831     C* se opz=Commenti richiamo pgm gestione commenti
177000950419     C*
177100950419     C*          S1OPZ     WHEQ 14
177200950419     C*                    RESETA725DS
177300950419     C*          OPZ001    IFEQ '04'
177400950419     C*          OPZ001    OREQ '05'
177500950419     C* oppure se l'utente è abilitato per la funzione specifica di
177600950419     C* questa anagrafica solo alla visualizzazione
177700950419     C*                    MOVE '05'      OPZ725
177800950419     C*                    ELSE
177900950419     C*                    MOVE '02'      OPZ725
178000950419     C*                    ENDIF
178100950419    >C*                    MOVE '___'     TPA725
178200950419     C* inserire chiavi commenti
178300950419    >C*                    MOVEL______    KY1725
178400950419    >C*                    MOVEL______    KY2725
178500950419    >C*                    MOVEL______    KY3725
178600950419    >C*                    MOVEL______    KY4725
178700950419     C*                    MOVE *BLANK    KPJBU
178800950419     C*                    MOVELA725DS    KPJBU
178900950419     C*                    CALL 'ANA725R'
179000950419     C*                    PARM           KPJBA
179100950419     C*                    MOVELKPJBU     A725DS
179200950419     C*
179300950419     C* ritorno da PGM commenti
179400950419     C*                    EXSR RTN725
179500950419     C*
179600950419     C*                    MOVE *HIVAL    $LASTV
179700980715     C*
179800980715     C* se selezionata opzione personalizzata, richiamo il programma
179900980715     C* corrispondente
180000980715     C     $PGO1(PosOpz) Whenne    *Blank
180100980715     C                   RESET                   A063DS
180200980715     C* << passare nei campi appositi della DS A043DS il valore
180300980715     C*    della chiave della VL univoca (se c'è) >>
180400980715     C                   MOVEL     S1OPZ         OPZ063
180500980715     C* passare nei campi appositi della DS D002DS il valore
180600980715     C* della chiave della VL univoca
180700980715    >C                   MOVEL     S1TPR         TPR063
180800980715    >C                   MOVEL     S1LIB         LIB063
180900980715    >C                   MOVEL     D1FSC         FSC063
181000980715     C*
181100980715     C                   MOVEL     A063DS        KPJBU
181200980715     C*
181300980715     C                   CALL      $PGO1(PosOpz)                        22
181400980715     C                   PARM                    KPJBA
181500980715     C                   PARM                    NOMPGM
181600980715     C*
181700980715     C                   MOVEL     KPJBU         A063DS
181800980715     C*
181900980715     C* ritorno da PGM
182000980715     C                   EXSR      RTN063
182100980715     C*
182200940225     C*
182300940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
182400941027     C                   OTHER
182500940225     C*
182600950419    >C                   RESET                   A063DS
182700950419    >C                   MOVEL     S1OPZ         OPZ063
182800950419    >C*          CMT001    IFEQ '1'
182900950419    >C*                    MOVEL'1'       CMT002
183000950419     C*                    ELSE
183100950419    >C*                    MOVEL'0'       CMT002
183200950419     C*                    ENDIF
183300950301     C* passare nei campi appositi della DS D002DS il valore
183400950310     C* della chiave della VL univoca
183500950419    >C                   MOVEL     S1TPR         TPR063
183600950419    >C                   MOVEL     S1LIB         LIB063
183700950426    >C                   MOVEL     D1FSC         FSC063
183800940314     C*
183900950419    >C                   MOVEL     A063DS        KPJBU
184000950419    >C                   CALL      'ANA063R'
184100940314     C                   PARM                    KPJBA
184200950420    >C*  se vengono gestite le autorizzazioni
184300950420    >C*  le passo al pgm di manutenzione per evitargli di doversele
184400950420    >C*  riprendere
184500950419    >C                   PARM                    AUTDS
184600950301     C*
184700950419    >C                   MOVEL     KPJBU         A063DS
184800940201     C*
184900940223     C* ritorno da PGM gestione
185000950419    >C                   EXSR      RTN063
185100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
185200941027     C     *IN99         IFEQ      *ON
185300940223     C                   SETON                                        40
185400941027     C                   ENDIF
185500940131     C*
185600940506     C                   MOVE      *HIVAL        $LASTV
185700980715     C*
185800941027     C                   ENDSL
185900940225     C*
186000940131     C                   ENDSR
186100940223     C/EJECT
186200020308C1550 ************************************************************
186300020308  "   * Gestione slittamenti iva trasportatori mista
186400020308  "   ************************************************************
186500020308  "  C     OpzS1_22      Begsr
186600020308  "   *
186700020308  "  C                   reset                   Ana148DS
186800020308  "  C                   move      '02'          Opz148
186900020308  "  C                   move      WWWSoc        Societa148
187000020308  "  C                   move      S1TpR         TpReg148
187100020308  "  C                   move      S1Lib         Libro148
187200020308  "  C                   movel     Ana148DS      Kpjbu
187300020308  "  C                   call      'ANA148R'
187400020308  "  C                   parm                    Kpjba
187500020308  "  C                   movel     Kpjbu         Ana148DS
187600020308  "   *
187700020308  "   * ritorno da PGM
187800020308  "  C                   EXSR      RTN148
187900020308  "   *
188000020308C1550C     XOpzS1_22     Endsr
188100020308C1550 ************************************************************
188200020308  "   * Ritorno Gestione slittamenti iva trasportatori mista
188300020308  "   ************************************************************
188400020308  "  C     Rtn148        Begsr
188500020308  "   *
188600020308  "   * modo di ritorno
188700020308  "  C                   SELECT
188800020502  "  C* 1 = F3
188900020308  "  C     RET148        WHENEQ    '1'
189000020308  "  C                   MOVE      *ON           $ESCI                          sospese READC
189100020308  "  C                   MOVE      *ON           $FINE                          fine pgm
189200020308  "  C                   MOVE      '1'           RET062
189300020308  "  C* 2 = F12
189400020308  "  C     RET148        WHENEQ    '2'
189500020308  "  C                   MOVE      *ON           $ESCI                          sospese READC
189600020502  "   *  ricarico il sfl: potrei essere ritornato con F12 dopo aver
189700020502  "   *  inserito/copiato/cancelalto uno slittamento
189800020502  "  C                   MOVE      *ON           $InzS1                         sospese READC
189900020308  "  C                   ENDSL
190000020308  "  C*
190100020308  "  C* operazione eseguite dal pgm chiamato
190200020308  "  C                   SELECT
190300020308  "  C* 1 =
190400020308  "  C     OPR148        WHENEQ    '1'
190500020502  "   *  ricarico il sfl: potrei aver inserito/copiato/cancellato
190600020502  "   *  uno slittamento che non corrisponde al libro sul quale
190700020502  "   *  ho scelto l'opzione
190800020502  "  C                   MOVE      *ON           $InzS1                         sospese READC
190900020308  "   *
191000020308  "  C                   ENDSL
191100020308  "   *
191200020502  "   * funzione non eseguibile per errore :
191300020308  "  C                   SELECT
191400020502  "   * 1 = funzione richiamata chiusa in errore
191500020308  "  C     ERR148        WHENEQ    '1'
191600020308  "  C                   MOVE      *ON           $ESCI                          sospese READC
191700020308  "  C                   SETON                                        5299      emiss.SFLMSGID
191800020308  "  C                   ENDSL
191900020502  "   *
192000020308  "   *
192100020308C1550C     XRtn148       Endsr
192200070924C2051 ************************************************************
192300070924  "   * Gestione punti vendita
192400080421c2051 ************************************************************
192500080421     C     OpzS1_27      Begsr
192600080421     c*
192700080421    >C                   RESET                   Ana159ds1
192800080421     c                   if        H1Numrea <> *blanks
192900100202C2135c                             or H1NumP   <> *blanks
193000080421    >C                   MOVEL     '02'          OPZ1159
193100080421     c                   move      H1NumREa      NumREa1159
193200080421     c                   move      H1PrgREa      PrgREa1159
193300100202c2135c                   move      H1NumP        NumP1159
193400080421     c                   else
193500080421    >C                   MOVEL     '01'          OPZ1159
193600080421     c                   endif
193700080421     C* passare nei campi appositi della DS A159ds1 il valore
193800080421     C* della chiave della VL univoca
193900080421     C                   MOVE      xscsoc        Societ1159
194000080421     c                   move      s1tpr         TpReg1159
194100080421     c                   move      s1lib         Libro1159
194200080421     C*
194300080421    >C                   MOVEL     Ana159ds1     KPJBU
194400080421    >C                   CALL      'ANA159R1'
194500080421     C                   PARM                    KPJBA
194600080421    >C                   MOVEL     KPJBU         ana159ds1
194700080421     c*
194800080421     c                   exsr      rtn1159
194900080421      *
195000070924C2051C     XOpzS1_27     Endsr
195100070924C2051 ************************************************************
195200070924  "   * Ritorno Gestione punti vendita
195300070924  "   ************************************************************
195400080421  "  C     Rtn1159       Begsr
195500070924  "   *
195600070924  "   * modo di ritorno
195700070924  "  C                   SELECT
195800070924  "  C* 1 = F3
195900080421  "  C     RET1159       WHENEQ    '1'
196000070924  "  C                   MOVE      *ON           $ESCI                          sospese READC
196100070924  "  C                   MOVE      *ON           $FINE                          fine pgm
196200070924  "  C                   MOVE      '1'           RET062
196300070924  "  C* 2 = F12
196400080421  "  C     RET1159       WHENEQ    '2'
196500070924  "  C                   MOVE      *ON           $ESCI                          sospese READC
196600070924  "  C                   ENDSL
196700070924  "  C*
196800070924  "  C* operazione eseguite dal pgm chiamato
196900070924  "  C                   SELECT
197000070924  "  C* 1 =
197100080421  "  C     OPR1159       WHENEQ    '1'
197200070924  "   *  ricarico il sfl: potrei aver inserito/copiato/cancellato
197300080421  "   *  un punto vendita
197400080910  "  C                   MOVE      *ON           $InzS1                         sospese READC
197500080910  "  C                   MOVE      *ON           WInzS1                         sospese READC
197600070924  "   *
197700070924  "  C                   ENDSL
197800070924  "   *
197900070924  "   * funzione non eseguibile per errore :
198000070924  "  C                   SELECT
198100070924  "   * 1 = funzione richiamata chiusa in errore
198200080421  "  C     ERR1159       WHENEQ    '1'
198300070924  "  C                   MOVE      *ON           $ESCI                          sospese READC
198400070924  "  C                   SETON                                        5299      emiss.SFLMSGID
198500070924  "  C                   ENDSL
198600070924  "   *
198700070924  "   *
198800080421C2051C                   Endsr
198801180105R275 C************************************************************
198802180105  "  C* Gestione Regole detrazione IVA a Credito
198803180105  "  C************************************************************
198804180105R275 C     OpzS1_23      BEGSR
198805180105     C*
198806180105     C                   Reset                   SDGSI30DS
198807180105     C                   move      '02'          OpzI30
198808180105     C                   move      S1TpR         TpRegI30
198809180105     C                   move      S1Lib         LibroI30
198810180105     C                   movel     SDGSI30DS     Kpjbu
198811180105     C                   call      'SDGSI30R'
198812180105     C                   parm                    Kpjba
198813180105     C                   movel     Kpjbu         SDGSI30DS
198814180105     C*
198815180105     C* ritorno da PGM
198816180105     C                   EXSR      RTNI30
198817180105     C*
198818180105R275 C                   ENDSR
198819180105R275 C/EJECT
198820180105R2750C************************************************************
198821180105  "  C* Ritorno gestione Regole detrazione IVA a Credito
198822180105  "  C************************************************************
198823180105R275 C     RtnI30        BEGSR
198824180105     C*
198825180105     C* modo di ritorno
198826180105     C                   SELECT
198827180105     C* 1 = F3
198828180105     C     RETI30        WHENEQ    '1'
198829180105     C                   MOVE      *ON           $ESCI                          sospese READC
198830180105     C                   MOVE      *ON           $FINE                          fine pgm
198831180105     C                   MOVE      '1'           RET062
198832180105     C* 2 = F12
198833180105     C     RETI30        WHENEQ    '2'
198834180105     C                   MOVE      *ON           $ESCI                          sospese READC
198835180105     C*  ricarico il sfl: potrei essere ritornato con F12 dopo aver
198836180105     C*  inserito/copiato/cancelalto uno slittamento
198837180105     C********           MOVE      *ON           $InzS1                         sospese READC
198838180105     C                   ENDSL
198839180105     C*
198840180105     C* operazione eseguite dal pgm chiamato
198841180105     C                   SELECT
198842180105     C* 1 =
198843180105     C     OPRI30        WHENEQ    '1'
198844180105     C*  ricarico il sfl: potrei aver inserito/copiato/cancellato
198845180105     C*  uno slittamento che non corrisponde al libro sul quale
198846180105     C*  ho scelto l'opzione
198847180105     C************       MOVE      *ON           $InzS1                         sospese READC
198848180105     C*
198849180105     C* reperisco se presenti regole
198850180105     C                   MOVE      WWWSOC        RICSocieta
198851180105     C                   MOVE      S1TPR         RICTpReg
198852180105     C                   MOVE      S1LIB         RICLibro
198853180105     C     K03RIC01      SETLL     SDGRIC01L                              21
198854180105     C                   MOVE      *IN21         H1B04
198855180105     C*
198856180105     C                   ENDSL
198857180105     C*
198858180105     C* funzione non eseguibile per errore :
198859180105     C                   SELECT
198860180105     C* 1 = funzione richiamata chiusa in errore
198861180105     C     ERRI30        WHENEQ    '1'
198862180105     C                   MOVE      *ON           $ESCI                          sospese READC
198863180105     C                   SETON                                        5299      emiss.SFLMSGID
198864180105     C                   ENDSL
198865180105     C*
198866180105R275 C                   ENDSR
198867180105R275 C/EJECT
198900940223     C************************************************************
199000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
199100940223     C************************************************************
199200950419    >C     RTN063        BEGSR
199300940223     C*
199400940223     C* modo di ritorno
199500940223     C*
199600941027     C                   SELECT
199700950301     C* questi modi di utilizzo dei valori di ritorno dal
199800950301     C* pgm di manutenzione rcd di anagrafica sono delle
199900950301     C* proposte, normalmente sempre valide, ma modificabili
200000950301     C* per situazioni particolari
200100940223     C* 1 = F3
200200950419    >C     RET063        WHENEQ    '1'
200300940224     C                   MOVE      *ON           $ESCI                          sospese READC
200400940223     C                   MOVE      *ON           $FINE                          fine pgm
200500950419     C                   MOVE      '1'           RET062
200600940223     C* 2 = F12
200700950419    >C     RET063        WHENEQ    '2'
200800940223     C                   MOVE      *ON           $ESCI                          sospese READC
200900940223     C*
201000941027     C                   ENDSL
201100940223     C*
201200940223     C* operazione eseguite dal pgm chiamato
201300940223     C*
201400941027     C                   SELECT
201500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
201600950419    >C     OPR063        WHENEQ    '1'
201700950301     C*
201800950301     C* se l'opzione con cui è stato chiamato non è la modifica
201900950301     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
202000950419    >C     OPZ063        IFNE      '02'
202100940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
202200950419    >C                   MOVE      OPR063        WOPR                           avverto pgm chiamant
202300950301     C* se l'opzione con cui è stato chiamato è la modifica
202400950301     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
202500950301     C                   ELSE
202600950301     C                   EXSR      AGGS1
202700950301     C                   ENDIF
202800940223     C*
202900941027     C                   ENDSL
203000940223     C*
203100940223     C* funzione non eseguibile per errore :
203200940223     C*
203300941027     C                   SELECT
203400940223     C* 1 = funzione richiamata chiusa in errore
203500950419    >C     ERR063        WHENEQ    '1'
203600940223     C                   MOVE      *ON           $ESCI                          sospese READC
203700940223     C                   SETON                                        5299      emiss.SFLMSGID
203800950301     C* 2 = funzione richiamata senza la dovuta autorizzazione
203900950419    >C     ERR063        WHENEQ    '2'
204000950301     C                   MOVE      *ON           $ESCI                          sospese READC
204100950301     C                   SETON                                        5499      emiss.SFLMSGID
204200950420    >C*  eventualmente gestire altri codici di errore
204300941027     C*
204400941027     C                   ENDSL
204500940223     C*
204600940223     C                   ENDSR
204700950301     C/EJECT
204800950428     C************************************************************
204900950428     C* GESTIONE RITORNO DA PGM DI ABILITAZIONE
205000950428     C************************************************************
205100950428     C     RTN066        BEGSR
205200950428     C*
205300950428     C* modo di ritorno
205400950428     C*
205500950428     C                   SELECT
205600950428     C* questi modi di utilizzo dei valori di ritorno dal
205700950428     C* pgm di manutenzione rcd di anagrafica sono delle
205800950428     C* proposte, normalmente sempre valide, ma modificabili
205900950428     C* per situazioni particolari
206000950428     C* 1 =
206100950428     C     RET066        WHENEQ    '1'
206200950428     C*
206300950428     C                   ENDSL
206400950428     C*
206500950428     C* operazione eseguite dal pgm chiamato
206600950428     C*
206700950428     C                   SELECT
206800950503     C* 1 = Abilitato
206900950428     C     OPR066        WHENEQ    '1'
207000950428     C*
207100950503     C                   MOVEL     *ON           H1ABIL
207200950428     C*
207300950428     C                   ENDSL
207400950428     C*
207500950428     C* funzione non eseguibile per errore :
207600950428     C*
207700950428     C                   SELECT
207800950428     C* 1 = funzione richiamata chiusa in errore
207900950428     C     ERR066        WHENEQ    '1'
208000950428     C                   MOVE      *ON           $ESCI                          sospese READC
208100950428     C                   SETON                                        5299      emiss.SFLMSGID
208200950428     C*
208300950428     C                   ENDSL
208400950428     C*
208500950428     C                   ENDSR
208600950428     C/EJECT
208700950428     C************************************************************
208800950428     C* GESTIONE RITORNO DA PGM DI ASSOCIAZIONE LIBRI/RIFERIMENTI
208900950428     C************************************************************
209000950428     C     RTN068        BEGSR
209100950428     C*
209200950428     C* modo di ritorno
209300950428     C*
209400950428     C                   SELECT
209500950428     C* questi modi di utilizzo dei valori di ritorno dal
209600950428     C* pgm di manutenzione rcd di anagrafica sono delle
209700950428     C* proposte, normalmente sempre valide, ma modificabili
209800950428     C* per situazioni particolari
209900950428     C* 1 =
210000950428     C     RET068        WHENEQ    '1'
210100951220     C                   MOVE      *ON           $ESCI                          sospese READC
210200951220     C                   MOVE      *ON           $FINE                          fine pgm
210300951220     C                   MOVE      '1'           RET062
210400951220     C* 2 = F12
210500951220    >C     RET068        WHENEQ    '2'
210600951220     C                   MOVE      *ON           $ESCI                          sospese READC
210700950428     C                   ENDSL
210800950428     C*
210900950428     C* operazione eseguite dal pgm chiamato
211000950428     C*
211100950428     C                   SELECT
211200950428     C* 1 =
211300950428     C     OPR068        WHENEQ    '1'
211400950503     C*
211500950503     C* reperisco se associazioni presenti
211600950503     C                   MOVE      WWWSOC        ALRSOC
211700950503     C                   MOVE      S1TPR         ALRTPR
211800950503     C                   MOVE      S1LIB         ALRLIB
211900950503     C     K3ALR2        SETLL     ANALR02L                               21
212000950503     C                   MOVE      *IN21         H1B01
212100980804     c*
212200950428     C                   ENDSL
212300950428     C*
212400950428     C* funzione non eseguibile per errore :
212500950428     C*
212600950428     C                   SELECT
212700950428     C* 1 = funzione richiamata chiusa in errore
212800950428     C     ERR068        WHENEQ    '1'
212900950428     C                   MOVE      *ON           $ESCI                          sospese READC
213000950428     C                   SETON                                        5299      emiss.SFLMSGID
213100950428     C*
213200950428     C                   ENDSL
213300950428     C*
213400950428     C                   ENDSR
213500950428     C/EJECT
213600980804     C************************************************************
213700980804     C* GESTIONE RITORNO DA PGM DI SLITTAMENTO LIBRI IVA
213800980804     C************************************************************
213900980804     C     RTN140        BEGSR
214000980804     C*
214100980804     C* modo di ritorno
214200980804     C*
214300980804     C                   SELECT
214400980804     C* questi modi di utilizzo dei valori di ritorno dal
214500980804     C* pgm di manutenzione rcd di anagrafica sono delle
214600980804     C* proposte, normalmente sempre valide, ma modificabili
214700980804     C* per situazioni particolari
214800980804     C* 1 =
214900980804     C     RET140        WHENEQ    '1'
215000980804     C                   MOVE      *ON           $ESCI                          sospese READC
215100980804     C                   MOVE      *ON           $FINE                          fine pgm
215200980804     C                   MOVE      '1'           RET062
215300980804     C* 2 = F12
215400980804    >C     RET140        WHENEQ    '2'
215500980804     C                   MOVE      *ON           $ESCI                          sospese READC
215600980804     C                   ENDSL
215700980804     C*
215800980804     C* operazione eseguite dal pgm chiamato
215900980804     C*
216000980804     C                   SELECT
216100980804     C* 1 =
216200980804     C     OPR140        WHENEQ    '1'
216300980804     C*
216400980804     C* reperisco se presenti slittamenti libri
216500980804     C                   MOVE      WWWSOC        DLSSOC
216600980804     C                   MOVE      S1TPR         DLSTPR
216700980804     C                   MOVE      S1LIB         DLSLIB
216800980804     C     K3DLS1        SETLL     ANDLS01L                               21
216900980804     C                   MOVE      *IN21         H1B02
217000980804     c*
217100980804     C                   ENDSL
217200980804     C*
217300980804     C* funzione non eseguibile per errore :
217400980804     C*
217500980804     C                   SELECT
217600980804     C* 1 = funzione richiamata chiusa in errore
217700980804     C     ERR140        WHENEQ    '1'
217800980804     C                   MOVE      *ON           $ESCI                          sospese READC
217900980804     C                   SETON                                        5299      emiss.SFLMSGID
218000980804     C*
218100980804     C                   ENDSL
218200980804     C*
218300980804     C                   ENDSR
218400980804     C/EJECT
218500950301     C************************************************************
218600950301     C* AGGIORNAMENTO RIGA LISTA
218700950301     C************************************************************
218800950301     C     AGGS1         BEGSR
218900950301     C*
219000950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
219100950301     C* di manutenzione dopo una opzione 02
219200950301     C*
219300950419     C                   MOVEL     TPR063        S1TPR
219400950419     C                   MOVEL     LIB063        S1LIB
219500950420     C                   MOVEL     DES063        S1DES
219600950420     C                   MOVEL     SER063        S1SER
219700950301     C*
219800950301     C                   ENDSR
219900941124     C/EJECT
220000941124     C************************************************************
220100941124     C* GESTIONE RITORNO DA PGM DEI COMMENTI
220200941124     C************************************************************
220300950419     C*          RTN725    BEGSR
220400941124     C*
220500941124     C* modo di ritorno
220600941124     C*
220700950419     C*                    SELEC
220800950419     C* questi modi di utilizzo dei valori di ritorno dal
220900950419     C* pgm di manutenzione rcd di anagrafica sono delle
221000950419     C* proposte, normalmente sempre valide, ma modificabili
221100950419     C* per situazioni particolari
221200950419     C* 1 = F3
221300950419     C*          RET725    WHEQ '1'
221400950419     C*                    MOVE *ON       $ESCI            sospese READC
221500950419     C*                    MOVE *ON       $FINE            fine pgm
221600950419     C* 2 = F12
221700950419     C*          RET725    WHEQ '2'
221800950419     C*                    MOVE *ON       $ESCI            sospese READC
221900950419     C*
222000950419     C*                    ENDSL
222100950419     C*
222200950419     C* operazione eseguite dal pgm chiamato
222300950419     C*
222400950419     C*                    SELEC
222500950419     C* 1 = eseguita scrittura o modifica commento
222600950419     C*          OPR725    WHEQ '1'
222700950419    >C* segnalare su sfl guida l'avvenuto aggiornamento
222800950419     C*
222900950419     C*                    ENDSL
223000950419     C*
223100950419     C* funzione non eseguibile per errore :
223200950419     C*
223300950419     C*                    SELEC
223400950419     C* 1 = funzione richiamata chiusa in errore
223500950419     C*          ERR725    WHEQ '1'
223600950419     C*                    MOVE *ON       $ESCI            sospese READC
223700950419     C*                    SETON                     5299  emiss.SFLMSGID
223800950419     C* 2 = funzione richiamata senza la dovuta autorizzazione
223900950419     C*          ERR725    WHEQ '2'
224000950419     C*                    MOVE *ON       $ESCI            sospese READC
224100950419     C*                    SETON                     5499  emiss.SFLMSGID
224200950419    >C*  eventualmente gestire altri codici di errore
224300950419     C*
224400950419     C*                    ENDSL
224500950419     C*
224600950419     C*                    ENDSR
224700950331     C/EJECT
224800950331     C************************************************************
224900950331     C* GESTIONE RITORNO DA PGM INVIO
225000950331     C************************************************************
225100950419     C*          RTN727    BEGSR
225200950331     C*
225300950331     C* modo di ritorno
225400950331     C*
225500950419     C*                    SELEC
225600950419     C* questi modi di utilizzo dei valori di ritorno dal
225700950419     C* pgm di manutenzione rcd di anagrafica sono delle
225800950419     C* proposte, normalmente sempre valide, ma modificabili
225900950419     C* per situazioni particolari
226000950419     C* 1 = F3
226100950419     C*          RET727    WHEQ '1'
226200950419     C*                    MOVE *ON       $ESCI            sospese READC
226300950419     C*                    MOVE *ON       $FINE            fine pgm
226400950419     C* 2 = F12
226500950419     C*          RET727    WHEQ '2'
226600950419     C*                    MOVE *ON       $ESCI            sospese READC
226700950419     C*
226800950419     C*                    ENDSL
226900950419     C*
227000950419     C* operazione eseguite dal pgm chiamato
227100950419     C*
227200950419     C*                    SELEC
227300950419     C* 1 = eseguito aggiornamento
227400950419     C*          OPR727    WHEQ '1'
227500950419     C*
227600950419     C*                    ENDSL
227700950419     C*
227800950419     C* funzione non eseguibile per errore :
227900950419     C*
228000950419     C*                    SELEC
228100950419     C* 1 = funzione richiamata chiusa in errore
228200950419     C*          ERR727    WHEQ '1'
228300950419     C*                    MOVE *ON       $ESCI            sospese READC
228400950419     C*                    SETON                     5299  emiss.SFLMSGID
228500950419     C* 2 = funzione richiamata senza la dovuta autorizzazione
228600950419     C*          ERR727    WHEQ '2'
228700950419     C*                    MOVE *ON       $ESCI            sospese READC
228800950419     C*                    SETON                     5499  emiss.SFLMSGID
228900950419     C*
229000950419     C*                    ENDSL
229100950419     C*
229200950419     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
229300950419     C* di sola finestra
229400950419     C*                    MOVEL'0'       WIN727
229500950419     C*
229600950419     C*                    ENDSR
229700940223     C/EJECT
229800940131     C************************************************************
229900940131     C* GESTIONE VALORI ASSUNTI
230000940131     C************************************************************
230100940131     C     GESD2         BEGSR
230200940131     C*
230300941118   B1C     $INZD2        IFEQ      *ON
230400940131     C                   EXSR      INZD2
230500940223     C                   MOVE      *OFF          $INZD2
230600941118   E1C                   ENDIF
230700940202     C*
230800941118   B1C     $LASTV        IFNE      'D2'
230900940923     C                   EXSR      GESPIE
231000940202     C                   MOVE      $LASTV        $LASTG
231100940202     C                   MOVE      'D2'          $LASTV
231200941118   E1C                   ENDIF
231300940131     C*
231400940707     C* se esistono errori sulla videata
231500940707     C* emetto la write del formato a indicatori spenti per vedere
231600940707     C* le eventuali decodifiche
231700941118   B1C     *IN99         IFEQ      *ON
231800940707     C                   MOVEA     *IN           WIN
231900940707     C                   MOVE      *ALL'0'       IN5098           49
232000940707     C                   MOVEA     IN5098        *IN(50)
232100940707     C                   WRITE     FMTD2
232200940707     C                   MOVEA     WIN           *IN
232300941118   E1C                   ENDIF
232400940707     C*
232500940131     C                   EXFMT     FMTD2
232600941205     C*
232700941205     C* controllo abilitazione tasti funzione
232800941205     C                   EXSR      CTRKD2
232900941205     C*
233000941205   B1C     *IN99         IFEQ      *OFF
233100941205     C*
233200941205   B2C                   SELECT
233300940131     C* F3=Fine
233400940506     C     $TASTO        WHENEQ    F03
233500940309     C                   EXSR      F03D2
233600940131     C* F12=Ritorno
233700940506     C     $TASTO        WHENEQ    F12
233800940309     C                   EXSR      F12D2
233900940916     C* F24=Altri tasti
234000940916     C     $TASTO        WHENEQ    F24
234100940916     C                   EXSR      F24D2
234200940131     C*
234300940131     C                   OTHER
234400940131     C                   EXSR      CTRD2
234500941205   B3C     *IN99         IFEQ      *OFF
234600940211     C                   MOVE      $LASTG        $GEST
234700941205   B4C                   SELECT
234800940202     C     $LASTG        WHENEQ    'S1'
234900940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
235000941205   B5C     $ORD          IFNE      D2ORD
235100940223     C                   MOVE      *ON           $INZS1
235200940302     C                   MOVE      *BLANK        W1POS
235300941205   E5C                   ENDIF
235400940202     C     $LASTG        WHENEQ    'D1'
235500940318     C* per ora niente
235600941205   E4C                   ENDSL
235700940131     C* definisco i nuovi valori di default
235800940201     C                   Z-ADD     D2LOP         $LOP
235900940131     C                   Z-ADD     D2ORD         $ORD
236000950420    >C                   MOVE      D2TPR         $TPR
236100950420    >C                   MOVE      D2LIB         $LIB
236200950420    >C*  elenco altri dati default specifici dell'anagrafica
236300950322     C*
236400950322     C* aggiorno valori assunti
236500950328     C* se richiesto il salvataggio
236600950328   B4C     D2BSV         IFEQ      1
236700950322     C                   EXSR      AGGMPE
236800950322   E4C                   ENDIF
236900950322     C*
237000941205   E3C                   ENDIF
237100940131     C*
237200941205   E2C                   ENDSL
237300940131     C*
237400941205   E1C                   ENDIF
237500941205     C*
237600940131     C                   ENDSR
237700940223     C/EJECT
237800940131     C************************************************************
237900940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
238000940131     C************************************************************
238100940201     C     INZD2         BEGSR
238200940131     C*
238300940131     C                   CLEAR                   FMTD2
238400021014A0972C                   EVAL      D2BSVA = Normale
238500950328     C* se sono entrato con chiave univoca
238600950328     C* impedisco il salvataggio valori assunti
238700950328     C     $UNIQ         IFEQ      *ON
238800021014A0972X***                Z-ADD     2             D2BSV
238900021014A0972C                   EVAL      D2BSVA = Protetto
239000950328     C                   ENDIF
239100940201     C                   MOVEL     $LOP          D2LOP
239200940831     C                   MOVE      $ORD          D2ORD
239300950421     C                   MOVE      $TPR          D2TPR
239400950421     C                   MOVE      $LIB          D2LIB
239500940131     C*
239600940131     C                   ENDSR
239700941205     C/EJECT
239800941205     C************************************************************
239900941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
240000941205     C* INPUT
240100941205     C* $FC3 = Abilitazioni videata D2
240200941205     C* OUTPUT
240300941205     C* *IN97 *IN99 Errori
240400941205     C************************************************************
240500941205     C     CTRKD2        BEGSR
240600941205     C*
240700941205     C                   SETOFF                                       99
240800941205     C                   MOVEA     $FC3          $FC
240900941205     C                   EXSR      CTRKEY
241000941205     C     *IN99         IFEQ      *ON
241100941205     C                   SETON                                        97
241200941205     C                   ENDIF
241300941205     C*
241400941205     C                   ENDSR
241500940309     C/EJECT
241600940309     C************************************************************
241700940309     C* GESTIONE F03 VIDEO D2
241800940309     C************************************************************
241900940309     C     F03D2         BEGSR
242000940309     C*
242100940309     C                   MOVE      *ON           $FINE
242200940325     C* fine programma
242300940325     C                   EXSR      ENDPGM
242400940309     C*
242500940309     C                   ENDSR
242600940309     C/EJECT
242700940309     C************************************************************
242800940309     C* GESTIONE F12 VIDEO D2
242900940309     C************************************************************
243000940309     C     F12D2         BEGSR
243100940309     C*
243200940309     C                   MOVE      $LASTG        $GEST
243300940309     C*
243400940309     C                   ENDSR
243500940916     C/EJECT
243600940916     C************************************************************
243700940916     C* GESTIONE F24 VIDEO D2
243800940916     C************************************************************
243900940916     C     F24D2         BEGSR
244000940916     C*
244100940916     C                   MOVEA     $FX3          $FX
244200940916     C                   MOVEA     $FC3          $FC
244300940916     C                   MOVEA     $FR3          $FR
244400940916     C                   Z-ADD     $ULKD2        $ULK
244500950403    >C                   Z-ADD     44            $L01
244600950328    >C                   Z-ADD     0             $L02
244700940916     C                   EXSR      RIEKEY
244800940916     C                   Z-ADD     $ULK          $ULKD2            3 0
244900940916     C                   MOVEA     $FC           $FC3
245000940916     C                   MOVEL     $KEY1         Z3KE1
245100060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
245200940916     C                   WRITE     FMTZ3
245300940916     C*
245400940916     C                   ENDSR
245500940223     C/EJECT
245600940131     C************************************************************
245700940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
245800940131     C************************************************************
245900940131     C     CTRD2         BEGSR
246000940131     C*
246100940131     C                   SETOFF                                       99
246200940131     C* non imposto pos. del cursore
246300940131     C                   Z-ADD     0             H1RIGA
246400940131     C                   Z-ADD     0             H1COLO
246500940131     C*
246600940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
246700940201     C     D2LOP         IFEQ      0
246800940131     C                   SETON                                          5099
246900941027     C                   ENDIF
247000940131     C*
247100940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
247200940131     C     D2ORD         IFEQ      0
247300940131     C                   SETON                                          5199
247400941027     C                   ENDIF
247500950403     C*
247600950427     C* CTRL VALIDITA' TIPO REGISTRO
247700950427     C     D2TPR         IFNE      *BLANK
247800950427     C                   RESET                   X07DS
247900950427     C                   MOVE      '1'           X07RIC
248000950427     C                   MOVEL     'ANDLI000'    X07TRC
248100950427     C                   MOVEL     REPTPR        X07CAM
248200950427     C                   MOVEL     D2TPR         X07VAL
248300961004     C                   CALLb     'X07VALR'
248400950427     C                   PARM                    X07DS
248500950427   B3C     X07ERR        IFNE      '0'
248600950427     C                   SETON                                          5299
248700950427   E3C                   ENDIF
248800950427   E3C                   ENDIF
248900950428     C     D2LIB         IFEQ      *BLANK
249000961004     C                   CALLb     'XALLINEA'
249100950428     C                   PARM                    D2LIB
249200950428     C                   PARM      3             LEN               5 0
249300950428     C                   PARM                    XSCINP
249400950428     C                   ENDIF
249500950403     C* CONTROLLO ESISTENZA CODICE PER I DEFAULT
249600950427     C     D2TPR         IFNE      *BLANK
249700950427     C     D2LIB         ORNE      *BLANK
249800950421     C                   MOVEL     WWWSOC        DLISOC
249900950421     C                   MOVEL     D2TPR         DLITPR
250000950421     C                   MOVEL     D2LIB         DLILIB
250100950503     C     K3DLI1        SETLL     ANDLI01                                21
250200950421     C     *IN21         IFEQ      *OFF
250300950421     C                   SETON                                        5699
250400950421     C                   ENDIF
250500950403     C                   ENDIF
250600950420    >C*  elenco controlli altri dati default specifici dell'anagrafica
250700940131     C*
250800940131     C                   ENDSR
250900941205     C/EJECT
251000941205     C************************************************************
251100941205     C* CONTROLLO ABILITAZIONE TASTI
251200941205     C* INPUT
251300941205     C* $TASTO =Tasto funzione premuto
251400941205     C* $FC = Tasti abilitati
251500941205     C* OUTPUT
251600941205     C* IN99 = ERrore
251700941205     C************************************************************
251800941205     C     CTRKEY        BEGSR
251900941205     C*
252000941205     C                   SELECT
252100941205     C     $TASTO        WHENEQ    F01
252200980715     C                   z-add     01            posfun
252300941205     C     $FC(1)        IFEQ      '0'
252400941205     C                   SETON                                        99
252500941205     C                   ENDIF
252600941205     C     $TASTO        WHENEQ    F02
252700980715     C                   z-add     02            posfun
252800941205     C     $FC(2)        IFEQ      '0'
252900941205     C                   SETON                                        99
253000941205     C                   ENDIF
253100941205     C     $TASTO        WHENEQ    F03
253200980715     C                   z-add     03            posfun
253300941205     C     $FC(3)        IFEQ      '0'
253400941205     C                   SETON                                        99
253500941205     C                   ENDIF
253600941205     C     $TASTO        WHENEQ    F04
253700980715     C                   z-add     04            posfun
253800941205     C     $FC(4)        IFEQ      '0'
253900941205     C                   SETON                                        99
254000941205     C                   ENDIF
254100941205     C     $TASTO        WHENEQ    F05
254200980715     C                   z-add     05            posfun
254300941205     C     $FC(5)        IFEQ      '0'
254400941205     C                   SETON                                        99
254500941205     C                   ENDIF
254600941205     C     $TASTO        WHENEQ    F06
254700980715     C                   z-add     06            posfun
254800941205     C     $FC(6)        IFEQ      '0'
254900941205     C                   SETON                                        99
255000941205     C                   ENDIF
255100941205     C     $TASTO        WHENEQ    F07
255200980715     C                   z-add     07            posfun
255300941205     C     $FC(7)        IFEQ      '0'
255400941205     C                   SETON                                        99
255500941205     C                   ENDIF
255600941205     C     $TASTO        WHENEQ    F08
255700980715     C                   z-add     08            posfun
255800941205     C     $FC(8)        IFEQ      '0'
255900941205     C                   SETON                                        99
256000941205     C                   ENDIF
256100941205     C     $TASTO        WHENEQ    F09
256200980715     C                   z-add     09            posfun
256300941205     C     $FC(09)       IFEQ      '0'
256400941205     C                   SETON                                        99
256500941205     C                   ENDIF
256600941205     C     $TASTO        WHENEQ    F10
256700980715     C                   z-add     10            posfun
256800941205     C     $FC(10)       IFEQ      '0'
256900941205     C                   SETON                                        99
257000941205     C                   ENDIF
257100941205     C     $TASTO        WHENEQ    F11
257200980715     C                   z-add     11            posfun
257300941205     C     $FC(11)       IFEQ      '0'
257400941205     C                   SETON                                        99
257500941205     C                   ENDIF
257600941205     C     $TASTO        WHENEQ    F12
257700980715     C                   z-add     12            posfun
257800941205     C     $FC(12)       IFEQ      '0'
257900941205     C                   SETON                                        99
258000941205     C                   ENDIF
258100941205     C     $TASTO        WHENEQ    F13
258200980715     C                   z-add     13            posfun
258300941205     C     $FC(13)       IFEQ      '0'
258400941205     C                   SETON                                        99
258500941205     C                   ENDIF
258600941205     C     $TASTO        WHENEQ    F14
258700980715     C                   z-add     14            posfun
258800941205     C     $FC(14)       IFEQ      '0'
258900941205     C                   SETON                                        99
259000941205     C                   ENDIF
259100941205     C     $TASTO        WHENEQ    F15
259200980715     C                   z-add     15            posfun
259300941205     C     $FC(15)       IFEQ      '0'
259400941205     C                   SETON                                        99
259500941205     C                   ENDIF
259600941205     C     $TASTO        WHENEQ    F16
259700980715     C                   z-add     16            posfun
259800941205     C     $FC(16)       IFEQ      '0'
259900941205     C                   SETON                                        99
260000941205     C                   ENDIF
260100941205     C     $TASTO        WHENEQ    F17
260200980715     C                   z-add     17            posfun
260300941205     C     $FC(17)       IFEQ      '0'
260400941205     C                   SETON                                        99
260500941205     C                   ENDIF
260600941205     C     $TASTO        WHENEQ    F18
260700980715     C                   z-add     18            posfun
260800941205     C     $FC(18)       IFEQ      '0'
260900941205     C                   SETON                                        99
261000941205     C                   ENDIF
261100941205     C     $TASTO        WHENEQ    F19
261200980715     C                   z-add     19            posfun
261300941205     C     $FC(19)       IFEQ      '0'
261400941205     C                   SETON                                        99
261500941205     C                   ENDIF
261600941205     C     $TASTO        WHENEQ    F20
261700980715     C                   z-add     20            posfun
261800941205     C     $FC(20)       IFEQ      '0'
261900941205     C                   SETON                                        99
262000941205     C                   ENDIF
262100941205     C     $TASTO        WHENEQ    F21
262200980715     C                   z-add     21            posfun
262300941205     C     $FC(21)       IFEQ      '0'
262400941205     C                   SETON                                        99
262500941205     C                   ENDIF
262600941205     C     $TASTO        WHENEQ    F22
262700980715     C                   z-add     22            posfun
262800941205     C     $FC(22)       IFEQ      '0'
262900941205     C                   SETON                                        99
263000941205     C                   ENDIF
263100941205     C     $TASTO        WHENEQ    F23
263200980715     C                   z-add     23            posfun
263300941205     C     $FC(23)       IFEQ      '0'
263400941205     C                   SETON                                        99
263500941205     C                   ENDIF
263600941205     C     $TASTO        WHENEQ    F24
263700980715     C                   z-add     24            posfun
263800941205     C     $FC(24)       IFEQ      '0'
263900941205     C                   SETON                                        99
264000941205     C                   ENDIF
264100941205     C                   ENDSL
264200941205     C*
264300941205     C                   ENDSR
264400940223     C/EJECT
264500940201     C************************************************************
264600940201     C* AGGIORNAMENTO VALORI ASSUNTI
264700940201     C************************************************************
264800940201     C     AGGMPE        BEGSR
264900940201     C*
265000940201     C                   CLEAR                   ANMPE000
265100941118     C                   MOVEL     XSCSOC        MPESOC
265200941118     C                   MOVEL     DSPGM         MPEPGM
265300940201     C                   MOVEL     KNMUS         MPEPRF
265400941013     C*
265500950322     C* Si memorizzano i dati di default a seconda delle modalità con
265600950322     C* cui il pgm è usato
265700950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
265800950322     C* MPETIP = 'PV'  pgm richiamato da menù
265900950322     C* MPETIP = 'PB'  x lancio Batch
266000950322     C*
266100941013     C                   SELECT
266200950420     C     OPZ062        WHENEQ    '01'
266300941013     C                   MOVEL     'PX'          MPETIP
266400941013     C                   OTHER
266500941013     C                   MOVEL     'PV'          MPETIP
266600941013     C                   ENDSL
266700941118     C     K4MPE2        CHAIN     ANMPE02L                           21
266800940201     C                   MOVEL     DSMPE         MPEPAR
266900940201     C     *IN21         IFEQ      *ON
267000940201     C                   WRITE     ANMPE000
267100940201     C                   ELSE
267200940201     C                   UPDATE    ANMPE000
267300941027     C                   ENDIF
267400940201     C*
267500940201     C                   ENDSR
267600940223     C/EJECT
267700940131     C************************************************************
267800940131     C* OPERAZIONI INIZIALI
267900940131     C************************************************************
268000940131     C     *INZSR        BEGSR
268100940131     C*
268200940127     C* Reperimento parametri
268300940127     C*
268400940117     C     *ENTRY        PLIST
268500940117     C                   PARM                    KPJBA
268600950410     C                   PARM                    DSAUT
268700950410     C*
268800950410     C     PARMS         IFLT      2
268900950410     C                   MOVE      *BLANK        AUTDS
269000950410     C                   ELSE
269100950410     C                   MOVEL     DSAUT         AUTDS
269200950410     C                   ENDIF
269300940225     C*
269400940223     C* Reperimento opzioni
269500940127     C*
269600940128     C                   EXSR      REPOPZ
269700950406     C*
269800980721      *
269900980721      * Reperimento dati società
270000980721      *
270100980721     C                   MOVEL     'SOC001'      TIPXSC
270200980721     C                   MOVEL     *BLANK        SOCXSC
270300980721     C                   EXSR      REPSOC
270400980721     C     RTNXSC        IFNE      '1'
270500980721     C                   MOVEL     XSOCDS        SOC001
270600980721     C                   ELSE
270700980721     C                   MOVE      *ON           $FINE
270800980721     C                   EXSR      ENDPGM
270900980721     C                   ENDIF
271000940506     C* Reperimento tasti di funzione
271100940506     C*
271200940506     C                   EXSR      REPKEY
271300980721     C* Chiamata a routine per il reperimento delle opzioni
271400980721     C* personalizzate
271500980721     C                   CAllb     'X46USREXT'
271600980721     C                   PARM                    $OP1
271700980721     C                   PARM                    $TX1
271800980721     C                   PARM                    $FL1
271900980721     C                   PARM                    $PGO1
272000980721     C                   PARM      $dimOp        NumOpz
272100980721     C                   PARM                    $FX2
272200980721     C                   PARM                    $FC2
272300980721     C                   PARM                    $PGF2
272400980721     C                   PARM                    DsPgm
272500980721     C                   PARM                    xsccut
272600980721     C                   PARM                    xscgrp
272700980721     C*
272800980721     C                   Z-add     1             OPZLEN
272900980721     C     *blank        Lookup    $TX1(opzlen)                           21
273000980721     C                   if        *in21 = *on
273100980721     C                   sub       1             OPZLEN
273200980721     C                   else
273300980721     C                   Z-add     $DimOp        OPZLEN
273400980721     C                   endif
273500980721     C*
273600980721      *
273700980721      *
273800940127     C*
273900940117     C                   ENDSR
274000940223     C/EJECT
274100940223     C************************************************************
274200940223     C* INIZIALIZZAZIONE VARIABILI
274300940223     C************************************************************
274400940223     C     INZVAR        BEGSR
274500941215     C*
274600941215     C* Dati da PGM chiamante
274700941215     C*
274800950420    >C                   MOVEL     KPJBU         A062DS
274900941201     C*
275000950420     C* Reperimento parametri di gestione
275100950420    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
275200950420    >C*   asteriscare la chiamata alla subroutine REPPAR
275300950420     C*
275400950420     C                   MOVEL     *BLANK        XPARIC
275500950420     C                   MOVEL     XSCSOC        XPASOC
275600950420    >C                   MOVEL     'IVA     '    XPAPAR
275700950420     C                   MOVEL     *BLANK        XPAOUT
275800950420     C                   EXSR      REPPAR
275900950420     C     XPAERR        IFEQ      '0'
276000000802C1396X*****              MOVEL     XPAOUT        P008DS
276100000802C1396C                   MOVEL     XPAOUT        P045DS
276200950420     C                   ELSE
276300950420     C                   RETURN
276400950420     C                   ENDIF
276500950420     C*
276600941124     C* Reperimento autorizzazioni
276700941124     C*
276800950420    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
276900950420    >C*   asteriscare la chiamata alla subroutine REPAUT
277000941124    >C                   EXSR      REPAUT
277100941026     C*
277200941026     C* Reperimento valori default utente
277300941026     C*
277400941026     C                   EXSR      REPDFT
277500940506     C*
277600940506     C* Definizione delle opzioni utilizzabili a pgm
277700940506     C*
277800940506     C                   EXSR      DEFOPZ
277900950330     C*
278000940223     C* Variabili per gestione videate
278100940223     C*
278200940223     C                   MOVE      *OFF          $FINE                          fine pgm
278300940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
278400940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
278500940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
278600940223     C                   MOVE      *OFF          $EFILE                         memo fine file
278700940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
278800940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
278900940509     C                   MOVE      *HIVAL        $LASTV
279000940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
279100940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
279200940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
279300940223     C*
279400940318     C                   MOVE      *ON           $INZD1
279500940318     C                   MOVE      *ON           $INZS1
279600940831     C*
279700940831     C* se pgm richiamato con chiave univoca impostata,
279800940831     C* forzo la emissione della lista e imposto l'ordinamento
279900950419    >C*  in questo esempio la chiave univoca per la posta è
280000950419    >C*   OBJ/TYP/PRG ma basta testare il primo campo :
280100950419    >C*    se è valorizzata la chiave primaria lo sarà anche la
280200950419    >C*    secondaria se obbligatoria
280300950330     C*
280400950330     C* Variabili appoggio
280500950330     C*
280600950330     C                   Z-ADD     SFLPAG        WPAG
280700950420     C                   MOVE      *BLANK        WFSC
280800950330     C                   MOVEL     *OFF          WOPR
280900941104     C*
281000941104     C* definisco qual'è la prima videata
281100950410     C* (se la prima videata risulta la lista, devo comunque caricare
281200950410     C*  la videata di parzializzazione e gestire la definizione
281300950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
281400950410     C*  passati dei dati, può variare il caricamento)
281500940223     C                   SELECT
281600940223     C     $LOP          WHENEQ    1
281700940223     C                   MOVE      'D1'          $GEST
281800940223     C     $LOP          WHENEQ    2
281900940223     C                   MOVE      'S1'          $GEST
282000941104     C                   EXSR      INZD1
282100941104     C                   MOVE      *OFF          $INZD1
282200950410     C                   EXSR      DEFORD
282300941104     C                   ENDSL
282400950420     C* Esegue sempre inizializzazione videata parzializzazioni
282500950420     C* perchè controlla nella ULTOPZ il campo D1FSC
282600950420     C                   EXSR      INZD1
282700950420     C                   MOVE      *OFF          $INZD1
282800950420     C*
282900950420     C* per decodificare la videata richiamo la routine dei ctrl
283000950420     C* e poi disattivo gli eventuali messaggi di errore
283100950420     C                   EXSR      CTRD1
283200950420     C*
283300950420     C                   MOVE      *ALL'0'       IN5098
283400950420     C                   MOVEA     IN5098        *IN(50)
283500950420     C*
283600950420     C* Imposto campo WWWSOC
283700950420     C*
283800950420     C     D1FSC         IFEQ      'C'
283900950420     C                   MOVE      XSCCPA        WWWSOC
284000950420     C                   ELSE
284100950420     C                   MOVE      XSCSOC        WWWSOC
284200950420     C                   END
284300950420     C* Ulteriore definizione delle opzioni utilizzabili a pgm
284400950420     C*
284500950420     C                   EXSR      ULTOPZ
284600950420     C*
284700950420     C                   MOVE      D1FSC         WFSC
284800950420     C*
284900950420     C*
285000940223     C*
285100940223     C* Valorizzazione campi univoci testate
285200940223     C*
285300950427     C                   CLEAR                   FMTT1
285400950427     C                   MOVEL     KNSIF         NOMSIF
285500950427     C                   MOVEL     XSCDSI        NOMDIT
285600950427     C                   MOVEL     DSPGM         NOMPGM
285700950331     C* recupero il sottotitolo (se c'è)
285800950427     C*                    MOVEL*BLANK    T1TIT
285900950420     C***        SOTTIT    IFNE *BLANK
286000950420     C*                    MOVELSOTTIT    IDMSG
286100950420     C*                    MOVEL*BLANK    IDMSG
286200950420     C*                    CALL 'XRTVM'
286300950420     C*                    PARM           IDMSG  27
286400950420     C*                    PARM           TXTMSG100
286500950420     C*                    PARM           ERRMSG  1
286600950420     C*                    PARM *ON       CTRMSG  1        centratura
286700950420     C*                    PARM 30        LENMSG  30       lun output.
286800950420     C*          ERRMSG    IFNE *ON
286900950420     C*                    MOVELTXTMSG    T1TIT
287000950420     C*                    ELSE
287100950420     C*                    MOVEL*ALL'?'   T1TIT
287200950420     C*                    ENDIF
287300940223     C*
287400940223     C                   ENDSR
287500941209     C/EJECT
287600941209     C************************************************************
287700941209     C* REPERIMENTO DATI SOCIETA'
287800941209     C************************************************************
287900941209     C     REPSOC        BEGSR
288000941209     C*
288100961004     C                   CALLb     'XSOC'
288200941209     C                   PARM                    TIPXSC            6
288300941209     C                   PARM                    SOCXSC            3
288400941209     C                   PARM                    CDSXSC            9 0
288500941209     C                   PARM                    MODXSC            3
288600941209     C                   PARM      *BLANKS       RTNXSC            1
288700941209     C                   PARM                    XSOCDS
288800941209     C                   PARM                    KPJBA
288900941209     C*
289000941209     C                   ENDSR
289100941124     C/EJECT
289200941124     C************************************************************
289300941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
289400941124     C************************************************************
289500941124     C     REPAUT        BEGSR
289600941124     C*
289700941124     C                   CLEAR                   AUTDS
289800941124     C*
289900941124     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
290000941124     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
290100941124     C                   Z-ADD     0             XCAGRP                         GRUPPO
290200941124     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
290300000801C1396X*****              MOVEL     'ANGPDC'      XCAFNC                         FUNZIONE
290400000801C1396C                   MOVEL     'ANGIVA'      XCAFNC                         FUNZIONE
290500941124    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
290600941124    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
290700941124    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
290800941124     C*
290900961004     C                   CALLb     'XCHKAUT'
291000941124     C                   PARM                    AUTDS
291100941124     C*
291200941124     C* errori/non abilitazione
291300941124     C     XCARTN        IFNE      0
291400941124     C                   EXSR      ENDPGM
291500941124     C                   ELSE
291600000801C1396X*****              MOVEL     XCADDS        GPDCDS
291700000801C1396C                   MOVEL     XCADDS        GIVADS
291800941124     C                   ENDIF
291900941124     C*
292000941124     C                   ENDSR
292100940223     C/EJECT
292200940127     C************************************************************
292300940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
292400940127     C************************************************************
292500940131     C     REPDFT        BEGSR
292600940127     C*
292700941118     C                   MOVEL     XSCSOC        MPESOC
292800940201     C                   MOVEL     DSPGM         MPEPGM
292900940131     C                   MOVEL     KNMUS         MPEPRF
293000941027     C*Si reperiscono i dati di default a seconda delle modalità con
293100941027     C*cui il pgm è usato
293200941027     C*MPETIP = 'PX'  pgm richiamato come lista (CMD4)
293300941027     C*MPETIP = 'PV'  pgm richiamato da menù
293400941027     C*MPETIP = 'PB'  x lancio Batch
293500941013     C                   SELECT
293600950420     C     OPZ062        WHENEQ    '01'
293700941013     C                   MOVEL     'PX'          MPETIP
293800941013     C                   OTHER
293900941013     C                   MOVEL     'PV'          MPETIP
294000941013     C                   ENDSL
294100941118     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
294200940131     C     *IN21         IFEQ      *OFF
294300940131     C                   MOVEL     MPEPAR        DSMPE
294400940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
294500940131     C* i suoi valori di default vengono decisi a pgm
294600940131     C                   ELSE
294700940201     C                   Z-ADD     1             $LOP
294800940131     C                   Z-ADD     1             $ORD
294900950421     C                   MOVEL     *BLANK        $TPR
295000950421     C                   MOVEL     *BLANK        $LIB
295100950420    >C*  elenco altri dati default specifici dell'anagrafica
295200941027     C                   ENDIF
295300940131     C*
295400940131     C                   ENDSR
295500940223     C/EJECT
295600950420     C************************************************************
295700950420     C* REPERIMENTO PARAMETRI GESTIONE
295800950420     C************************************************************
295900950420     C     REPPAR        BEGSR
296000950420     C*
296100961004     C                   CALLb     'XPAR'
296200950420     C                   PARM                    XPASOC            3
296300950420     C                   PARM                    XPAPAR            8
296400950420     C                   PARM                    XPAOUT
296500950420     C                   PARM                    XPAERR            1
296600950420     C                   PARM                    XPARIC            1
296700950420     C*
296800950420     C                   ENDSR
296900950420     C/EJECT
297000940131     C************************************************************
297100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
297200940131     C************************************************************
297300940225     C     DEFOPZ        BEGSR
297400940131     C*
297500940225     C* Questa routine deve valorizzare la schiera $FL1
297600940225     C* con S o N a seconda della utilizzabilità o meno
297700940225     C* dell'opzione correlata
297800940225     C*
297900950301     C* Inizialmente si considerano tutte le opzioni come valide
298000940317     C     1             DO        $DIMOP        X
298100940225     C                   MOVE      'S'           $FL1(X)
298200941027     C                   ENDDO
298300940715     C*
298400940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
298500941018     C* con 1 o 0 a seconda della utilizzabilità o meno
298600940715     C* del tasto funzione correlato
298700940715     C*
298800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
298900940715     C* di compilazione
299000940715     C                   RESET                   $FC1
299100940715     C                   RESET                   $FR1
299200940715     C                   RESET                   $FC2
299300940715     C                   RESET                   $FR2
299400940919     C                   RESET                   $FC3
299500940919     C                   RESET                   $FR3
299600020308C1550 *
299700020308  "   * disattivo opz. 22=slittamenti iva trasportatori mista
299800020308  "   * se sull'anagrafica società non è definito regime iva tr.mista
299900020308  "  C                   Z-ADD     1             X
300000020308  "  C     22            LOOKUP    $OP1(X)                                21
300100020308  "  C                   if         *in21 = *on and XScIvaRgm <> '2'
300200020308  "  C                   MOVE      'N'           $FL1(X)
300300020308C1550C                   endif
300400940225     C*
300500940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
300600940225     C* invalido alcune opzioni
300700940225     C                   SELECT
300800940225     C* Se pgm richiamato in ricerca disattivo :
300900950419    >C     OPZ062        WHENEQ    '01'
301000940603     C* F3=Fine
301100940603     C                   Z-ADD     3             X
301200941216     C                   MOVE      '0'           $FC1(X)
301300941216     C                   MOVE      '0'           $FC2(X)
301400941216     C                   MOVE      '0'           $FC3(X)
301500950116     C*
301600950420    >C*  aggiungere nollo stesso modo le altre opzioni da
301700950420    >C*  disattivare (quelle specifiche per ogni pgm)
301800950301     C*
301900940225     C* Se pgm richiamato in interrogazione disattivo :
302000950419    >C     OPZ062        WHENEQ    '05'
302100940307     C* F10=Immissione
302200940615     C                   Z-ADD     10            X
302300950301     C                   MOVE      '0'           $FC1(X)
302400950301     C                   MOVE      '0'           $FC2(X)
302500940225     C* 1=Selezione
302600940225     C                   Z-ADD     1             X
302700940225     C     01            LOOKUP    $OP1(X)                                21
302800940225     C     *IN21         IFEQ      *ON
302900950426     C                   MOVE      'N'           $FL1(X)
303000940225     C                   ENDIF
303100940225     C* 2=Modifica
303200940225     C                   Z-ADD     1             X
303300940225     C     02            LOOKUP    $OP1(X)                                21
303400940225     C     *IN21         IFEQ      *ON
303500940225     C                   MOVE      'N'           $FL1(X)
303600940225     C                   ENDIF
303700940225     C* 3=Copia
303800940225     C                   Z-ADD     1             X
303900940225     C     03            LOOKUP    $OP1(X)                                21
304000940225     C     *IN21         IFEQ      *ON
304100940225     C                   MOVE      'N'           $FL1(X)
304200940225     C                   ENDIF
304300940225     C* 4=Annullamento
304400940225     C                   Z-ADD     1             X
304500940225     C     04            LOOKUP    $OP1(X)                                21
304600940225     C     *IN21         IFEQ      *ON
304700940225     C                   MOVE      'N'           $FL1(X)
304800940225     C                   ENDIF
304900950426     C*11=Abilita
305000950426     C                   Z-ADD     1             X
305100950426     C     11            LOOKUP    $OP1(X)                                21
305200950426     C     *IN21         IFEQ      *ON
305300950426     C                   MOVE      'N'           $FL1(X)
305400950426     C                   ENDIF
305500940225     C*
305600950426     C*20=Associazione libri/riferimenti:
305700950426     C                   Z-ADD     1             X
305800950426     C     20            LOOKUP    $OP1(X)                                21
305900950426     C     *IN21         IFEQ      *ON
306000950426     C                   MOVE      'N'           $FL1(X)
306100950426     C                   ENDIF
306200980804     c*
306300980804     C*21=Slittamenti libri iva:
306400980804     C                   Z-ADD     1             X
306500980804     C     21            LOOKUP    $OP1(X)                                21
306600980804     C     *IN21         IFEQ      *ON
306700980804     C                   MOVE      'N'           $FL1(X)
306800980804     C                   ENDIF
306900950426     C*
307000020308C1550C*22=Slittamenti iva trasportatori mista
307100020308  "  C                   Z-ADD     1             X
307200020308  "  C     22            LOOKUP    $OP1(X)                                21
307300020308  "  C     *IN21         IFEQ      *ON
307400020308  "  C                   MOVE      'N'           $FL1(X)
307500020308  "  C                   ENDIF
307600020308C1550C*
307700950419     C     OPZ062        WHENEQ    *BLANKS
307800940225     C* Se pgm non richiamato, disattivo subito :
307900941124     C* F8=Selezione seguente
308000941124     C                   Z-ADD     8             X
308100941206     C                   MOVE      '0'           $FC1(X)
308200941206     C                   MOVE      '0'           $FC2(X)
308300940225     C* 1=Selezione
308400940225     C                   Z-ADD     1             X
308500940225     C     01            LOOKUP    $OP1(X)                                21
308600940225     C     *IN21         IFEQ      *ON
308700940225     C                   MOVE      'N'           $FL1(X)
308800940225     C                   ENDIF
308900940225     C*
309000950301     C* In tutti gli altri casi, disattivo subito :
309100941228     C                   OTHER
309200941228     C* 1=Selezione
309300941228     C                   Z-ADD     1             X
309400941228     C     01            LOOKUP    $OP1(X)                                21
309500941228     C     *IN21         IFEQ      *ON
309600941228     C                   MOVE      'N'           $FL1(X)
309700941228     C                   ENDIF
309800941228     C*
309900940225     C                   ENDSL
310000941118     C*
310100950420    >C*  non testare le autorizzazioni perchè vengono spostate nella
310200950420    >C*  routine ULTOPZ
310300941118     C* poi, disattivo a seconda delle autorizzazioni :
310400950420    >C*          MANPDC    IFEQ 'N'
310500950420    >C*                    Z-ADD1         X
310600950420    >C*          02        LOKUP$OP1,X                   21
310700950420    >C*          *IN21     IFEQ *ON
310800950420    >C*                    MOVE 'N'       $FL1,X
310900950420    >C*                    ENDIF
311000950420    >C*                    Z-ADD1         X
311100950420    >C*          03        LOKUP$OP1,X                   21
311200950420    >C*          *IN21     IFEQ *ON
311300950420    >C*                    MOVE 'N'       $FL1,X
311400950420    >C*                    ENDIF
311500950420    >C*                    Z-ADD1         X
311600950420    >C*          04        LOKUP$OP1,X                   21
311700950420    >C*          *IN21     IFEQ *ON
311800950420    >C*                    MOVE 'N'       $FL1,X
311900950420    >C*                    ENDIF
312000950420     C*                    Z-ADD10        X
312100950420     C*                    MOVE '0'       $FC1,X
312200950420     C*                    MOVE '0'       $FC2,X
312300950420    >C*                    ENDIF
312400950420     C*- visualizzazione
312500950420    >C*          VISPDC    IFEQ 'N'
312600950420    >C*                    Z-ADD1         X
312700950420    >C*          05        LOKUP$OP1,X                   21
312800950420    >C*          *IN21     IFEQ *ON
312900950420    >C*                    MOVE 'N'       $FL1,X
313000950420    >C*                    ENDIF
313100950420    >C*                    ENDIF
313200940225     C*
313300940225     C                   ENDSR
313400940225     C/EJECT
313500950420     C************************************************************
313600950420     C* ULTERIORI DEFINIZIONE OPZIONI UTILIZZABILI A PGM
313700950420     C************************************************************
313800950420     C     ULTOPZ        BEGSR
313900950420     C*
314000950420     C* Questa routine deve eseguire una ulteriore definizione
314100950420     C* delle opzioni a seconda se siamo in logica di gruppo o meno
314200950420     C* e, se lo siamo, a seconda della società richiesta (capofila
314300950420     C* oppure di riferimento)
314400950420     C*
314500950420     C                   SELECT
314600950420     C*
314700950420     C* se siamo in logica di gruppo per società disattivo :
314800950420     C*
314900950420     C     XSCSOC        WHENNE    XSCCPA
315000000802C1396X*****GSP008        ANDEQ     'G'
315100000802C1396C     GSP045        ANDEQ     'G'
315200950420     C     D1FSC         ANDEQ     'S'
315300950420     C* F10=Immissione
315400950420     C                   Z-ADD     10            X
315500950420     C                   MOVE      '0'           $FC1(X)
315600950420     C                   MOVE      '0'           $FC2(X)
315700950420     C* 3=Copia
315800950420     C                   Z-ADD     1             X
315900950420     C     03            LOOKUP    $OP1(X)                                21
316000950420     C     *IN21         IFEQ      *ON
316100950426     C                   MOVE      'N'           $FL1(X)
316200950420     C                   ENDIF
316300950420     C* 11=Abilita
316400950420     C                   Z-ADD     1             X
316500950420     C     11            LOOKUP    $OP1(X)                                21
316600950420     C     *IN21         IFEQ      *ON
316700950426     C                   MOVE      'N'           $FL1(X)
316800950420     C                   ENDIF
316900950420     C* a seconda delle autorizzazioni
317000950420     C* - il ctrl sull'autorizzazione alla manutenzione viene
317100950420     C*   demandato al pgm chiamato
317200950420     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
317300950420     C*   si è abilitati ad altre funzioni
317400950420     C* - annullamento
317500000801C1396X*****ANNPDC        IFEQ      *OFF
317600000801C1396C     ANNDLI        IFEQ      *OFF
317700950420     C* 4=Annullamento
317800950420     C                   Z-ADD     1             X
317900950420     C     04            LOOKUP    $OP1(X)                                21
318000950420     C     *IN21         IFEQ      *ON
318100950420     C                   MOVE      'N'           $FL1(X)
318200950420     C                   ENDIF
318300950420     C                   ENDIF
318400950420     C* - il ctrl sull'autorizzazione alla visualizzazione viene
318500950420     C*   demandato al pgm chiamato
318600950420     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
318700950420     C*   si è abilitati ad altre funzioni
318800950420     C*
318900950420     C* se siamo in logica di gruppo per capofila disattivo :
319000950420     C*
319100950420     C     XSCSOC        WHENNE    XSCCPA
319200000802C1396X*****GSP008        ANDEQ     'G'
319300000802C1396C     GSP045        ANDEQ     'G'
319400950420     C     D1FSC         ANDEQ     'C'
319500950420     C* F10=Immissione
319600950420     C                   Z-ADD     10            X
319700950420     C                   MOVE      '0'           $FC1(X)
319800950420     C                   MOVE      '0'           $FC2(X)
319900950420     C* tutte le opzioni
320000950420     C                   DO        $DIMOP        X
320100950420     C                   MOVE      'N'           $FL1(X)
320200950420     C                   ENDDO
320300950420     C* tranne
320400950420     C* 11=Abilita (a seconda dell'autorizzazione)
320500000801C1396X*****ABIPDC        IFEQ      *ON
320600000801C1396C     ABIDLI        IFEQ      *ON
320700950420     C                   Z-ADD     1             X
320800950420     C     11            LOOKUP    $OP1(X)                                21
320900950420     C     *IN21         IFEQ      *ON
321000950420     C                   MOVE      'S'           $FL1(X)
321100950420     C                   ENDIF
321200950420     C                   ENDIF
321300950420     C* - il ctrl sull'autorizzazione alla visualizzazione viene
321400950420     C*   demandato al pgm chiamato
321500950420     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
321600950420     C*   si è abilitati ad altre funzioni
321700950420     C*   quindi l'abilito comunque
321800950420     C                   Z-ADD     1             X
321900950420     C     05            LOOKUP    $OP1(X)                                21
322000950420     C     *IN21         IFEQ      *ON
322100950420     C                   MOVE      'S'           $FL1(X)
322200950420     C                   ENDIF
322300950420     C*
322400950420     C* se siamo in logica di società disattivo :
322500950420     C*
322600950420     C     XSCSOC        WHENEQ    XSCCPA
322700000802C1396X*****GSP008        ORNE      'G'
322800000802C1396C     GSP045        ORNE      'G'
322900950420     C* 11=Abilita
323000950420     C                   Z-ADD     1             X
323100950420     C     11            LOOKUP    $OP1(X)                                21
323200950420     C     *IN21         IFEQ      *ON
323300950420     C                   MOVE      'N'           $FL1(X)
323400950420     C                   ENDIF
323500950420     C* e, a seconda delle autorizzazioni
323600950420     C* - immissione/copia
323700000801C1396X*****IMMPDC        IFEQ      *OFF
323800000801C1396C     IMMDLI        IFEQ      *OFF
323900950420     C* F10=Immissione
324000950420     C                   Z-ADD     10            X
324100950420     C                   MOVE      '0'           $FC1(X)
324200950420     C                   MOVE      '0'           $FC2(X)
324300950420     C* 3=Copia
324400950420     C                   Z-ADD     1             X
324500950420     C     03            LOOKUP    $OP1(X)                                21
324600950420     C     *IN21         IFEQ      *ON
324700950420     C                   MOVE      'N'           $FL1(X)
324800950420     C                   ENDIF
324900950420     C                   ENDIF
325000950420     C* - il ctrl sull'autorizzazione alla manutenzione viene
325100950420     C*   demandato al pgm chiamato
325200950420     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
325300950420     C*   si è abilitati ad altre funzioni
325400950420     C* - annullamento
325500000801C1396X*****ANNPDC        IFEQ      *OFF
325600000801C1396C     ANNDLI        IFEQ      *OFF
325700950420     C* 4=Annullamento
325800950420     C                   Z-ADD     1             X
325900950420     C     04            LOOKUP    $OP1(X)                                21
326000950420     C     *IN21         IFEQ      *ON
326100950420     C                   MOVE      'N'           $FL1(X)
326200950420     C                   ENDIF
326300950420     C                   ENDIF
326400950420     C* 20=Associazione libri/riferimenti
326500950420     C                   Z-ADD     1             X
326600950420     C     20            LOOKUP    $OP1(X)                                21
326700950420     C     *IN21         IFEQ      *ON
326800950420     C                   MOVE      'S'           $FL1(X)
326900950420     C                   ENDIF
327000980804     C* 21=Slittamento libri IVA
327100980804     C                   Z-ADD     1             X
327200980804     C     21            LOOKUP    $OP1(X)                                21
327300980804     C     *IN21         IFEQ      *ON
327400980804     C                   MOVE      'S'           $FL1(X)
327500980804     C                   ENDIF
327600020308C1550 * 22=Slittamento iva trasportatori mista
327700020308  "  C                   if        XScIvaRgm = '2'
327800020308  "  C                   Z-ADD     1             X
327900020308  "  C     22            LOOKUP    $OP1(X)                                21
328000020308  "  C     *IN21         IFEQ      *ON
328100020308  "  C                   MOVE      'S'           $FL1(X)
328200020308  "  C                   ENDIF
328300020308C1550C                   endif
328400950420     C* - il ctrl sull'autorizzazione alla visualizzazione viene
328500950420     C*   demandato al pgm chiamato
328600950420     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
328700950420     C*   si è abilitati ad altre funzioni
328800950420     C*
328900950420     C                   ENDSL
329000950420     C*
329100950420     C                   ENDSR
329200950420     C/EJECT
329300940225     C************************************************************
329400940225     C* REPERIMENTO OPZIONI
329500940225     C************************************************************
329600940225     C     REPOPZ        BEGSR
329700940225     C*
329800940314     C     1             DO        $DIMOP        X
329900940128     C*
330000940223     C     $MS1(X)       IFNE      *BLANK
330100940128     C*
330200940223     C                   MOVEL     $MS1(X)       IDMSG
330300961004     C                   CALLb     'XRTVM'
330400940201     C                   PARM                    IDMSG            27
330500940201     C                   PARM                    TXTMSG          100
330600940201     C                   PARM                    ERRMSG            1
330700940128     C*
330800940128     C     ERRMSG        IFNE      *ON
330900940223     C                   MOVEL     TXTMSG        $TX1(X)
331000940128     C                   ELSE
331100940223     C                   MOVEL     *ALL'?'       $TX1(X)
331200941027     C                   ENDIF
331300940128     C*
331400941027     C                   ENDIF
331500940128     C*
331600941027     C                   ENDDO
331700940128     C*
331800940128     C                   ENDSR
331900940506     C/EJECT
332000940506     C************************************************************
332100940506     C* REPERIMENTO TASTI DI FUNZIONE
332200940506     C************************************************************
332300940506     C     REPKEY        BEGSR
332400940506     C                   MOVEA     $FM1          $FM
332500950906     C                   MOVEA     $FC1          $FC
332600940506     C                   EXSR      REPKKK
332700940506     C                   MOVEA     $FX           $FX1
332800940506     C*
332900940506     C                   MOVEA     $FM2          $FM
333000950906     C                   MOVEA     $FC2          $FC
333100940506     C                   EXSR      REPKKK
333200940506     C                   MOVEA     $FX           $FX2
333300940919     C*
333400940919     C                   MOVEA     $FM3          $FM
333500950906     C                   MOVEA     $FC3          $FC
333600940919     C                   EXSR      REPKKK
333700940919     C                   MOVEA     $FX           $FX3
333800940919     C                   ENDSR
333900940506     C/EJECT
334000940506     C************************************************************
334100940506     C* REPERIMENTO TASTI DI FUNZIONE
334200940506     C************************************************************
334300940506     C     REPKKK        BEGSR
334400940506     C*
334500940506     C                   CLEAR                   $FX
334600940506     C     1             DO        24            X
334700940506     C*
334800940506     C     $FM(X)        IFNE      *BLANK
334900950906     C     $FC(X)        ANDNE     '0'
335000940506     C*
335100940506     C                   MOVEL     $FM(X)        IDMSG
335200961004     C                   CALLb     'XRTVM'
335300940506     C                   PARM                    IDMSG            27
335400940506     C                   PARM                    TXTMSG          100
335500940506     C                   PARM                    ERRMSG            1
335600940506     C*
335700940506     C     ERRMSG        IFNE      *ON
335800940506     C                   MOVEL     TXTMSG        $FX(X)
335900940506     C                   ELSE
336000940506     C                   MOVEL     *ALL' '       $FX(X)
336100941027     C                   ENDIF
336200940506     C*
336300941027     C                   ENDIF
336400940506     C*
336500941027     C                   ENDDO
336600940506     C*
336700940506     C                   ENDSR
336800940223     C/EJECT
336900940128     C************************************************************
337000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
337100940128     C************************************************************
337200940223     C     DEFCAM        BEGSR
337300940128     C*
337400940223     C* klist
337500940223     C*
337600950427     C* ANDLI01L
337700950503     C     K3DLI1        KLIST
337800950427     C                   KFLD                    DLISOC
337900950427     C                   KFLD                    DLITPR
338000950427     C                   KFLD                    DLILIB
338100950503     C     K2DLI1        KLIST
338200950427     C                   KFLD                    DLISOC
338300950427     C                   KFLD                    DLITPR
338400950428     C* ANALR02L
338500950428     C     K3ALR2        KLIST
338600950428     C                   KFLD                    ALRSOC
338700950428     C                   KFLD                    ALRTPR
338800950428     C                   KFLD                    ALRLIB
338900980804     C* ANDLS01L
339000980804     C     K3DLS1        KLIST
339100980804     C                   KFLD                    DLSSOC
339200980804     C                   KFLD                    DLSTPR
339300980804     C                   KFLD                    DLSLIB
339400940128     C* ANMPE02L
339500941118     C     K4MPE2        KLIST
339600941118     C                   KFLD                    MPESOC
339700941118     C                   KFLD                    MPEPGM
339800940128     C                   KFLD                    MPETIP
339900940128     C                   KFLD                    MPEPRF
340000020308C1550C* ANSLI01L
340100020308  "  C     K03SLI01      klist
340200020308  "  C                   kfld                    SLISocieta
340300020308  "  C                   kfld                    SLITpReg
340400020308C1550C                   kfld                    SLILibro
340401180105R275 C*
340402180105 "   C* SDGRIC01L
340403180105 "   C     K03RIC01      klist
340404180105 "   C                   kfld                    RICSocieta
340405180105 "   C                   kfld                    RICTpReg
340406180105R275 C                   kfld                    RICLibro
340500080418C2051C* ANpnr02L
340600080418  "  C     K03Pnr02      klist
340700080418  "  C                   kfld                    PNrSocieta
340800080418  "  C                   kfld                    PNrTpReg
340900080418C2051C                   kfld                    PNrLibro
341000940223     C*
341100940223     C* Variabili per gestione videate
341200940223     C*
341300940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
341400940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
341500940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
341600940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
341700940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
341800940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
341900940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
342000940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
342100940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
342200940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
342300940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
342400940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
342500941213     C                   MOVE      *BLANK        $UNIQ             1            gest.POSTA
342600940223     C*
342700940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
342800940223     C*
342900950420     C     *LIKE         DEFINE    XSCSOC        WWWSOC
343000940223     C     *LIKE         DEFINE    C1RCD         S1NRR
343100940223     C     *LIKE         DEFINE    C1NRR         WSFL
343200940223     C     *LIKE         DEFINE    C1RCD         WMAX
343300940223     C     *LIKE         DEFINE    C1RCD         WPAG
343400940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
343500940608     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
343600950426     C     *LIKE         DEFINE    OPR062        WOPR                                       ne S1
343700950426     C     *LIKE         DEFINE    DLITPR        WWTPR                                      ne S1
343800950426     C     *LIKE         DEFINE    D1FSC         WFSC                                       ne S1
343900950426     C     *LIKE         DEFINE    D1LIB         LIB999                                     ne S1
344000940223     C                   MOVE      *BLANK        WWGRP             3
344100940223     C                   Z-ADD     0             CURR              2 0
344200940223     C                   Z-ADD     0             CURC              2 0
344300940223     C                   MOVE      *BLANK        WIN              99
344400940314     C     *LIKE         DEFINE    C1POS         W1POS
344500950503     C     *LIKE         DEFINE    DLISOC        WSOC
344600950419    >C* aggiungere definizione altri campi posizionamento se ci sono
344700940223     C*
344800940223     C* Indici
344900940223     C*
345000940223     C                   Z-ADD     0             X                 3 0
345100940223     C                   Z-ADD     0             Y                 3 0
345200940127     C*
345300940117     C                   ENDSR
345400940223     C/EJECT
345401180105R275 C* Aggiunto msg SDO0061 "23=Regole detrazione IVA a Credito"
345500950906**
345600940718PROMSG    *LIBL     OPZ0001
345700940225PROMSG    *LIBL     OPZ0002
345800940131PROMSG    *LIBL     OPZ0003
345900940131PROMSG    *LIBL     OPZ0004
346000940131PROMSG    *LIBL     OPZ0005
346100950420PROMSG    *LIBL     OPZ0013
346200950420PROMSG    *LIBL     OPZ0101
346300980804PROMSG    *LIBL     OPZ0303
346400020308PROMSG    *LIBL     OPZ0456                      ### C1550 ###
346401180105SDGMSG    *LIBL     SDO0061                      ### R275  ###
346500070924PROMSG    *LIBL     OPZ0601                      ### C2051 ###
346600940201**
34670094022501S      <Selezione
34680094022502S      <Modifica
34690094022503S      <Copia
34700094022504S      <Annullamento
34710094022505S      <Visualizzazione
34720095041911S      <Abilita
34730095041920S      <Associaz. lib/rif
34740098080721S      <Regole slittam.
34750002030822S      <Slittamenti iva trasportatori mista    ### C1550 ###
34750118010523S      <Regole detrazione IVA a Credito        ### R275  ###
34760007092427S      <Punti vendita                          ### C2051 ###
347700940506** TASTI DI FUNZIONE  D1
347800940506PROMSG    *LIBL     KEY0001  01
347900940506PROMSG    *LIBL     KEY0002  02
348000940506PROMSG    *LIBL     KEY0003  03
348100940506PROMSG    *LIBL     KEY0004  04
348200940506PROMSG    *LIBL     KEY0005  05
348300940506PROMSG    *LIBL     KEY0006  06
348400940506PROMSG    *LIBL     KEY0007  07
348500940506PROMSG    *LIBL     KEY0008  08
348600940506PROMSG    *LIBL     KEY0009  09
348700940506PROMSG    *LIBL     KEY0010  10
348800940506PROMSG    *LIBL     KEY0011  11
348900940506PROMSG    *LIBL     KEY0012  12
349000940506PROMSG    *LIBL     KEY0013  13
349100940506PROMSG    *LIBL     KEY0014  14
349200940506PROMSG    *LIBL     KEY0015  15
349300940506PROMSG    *LIBL     KEY0016  16
349400940506PROMSG    *LIBL     KEY0017  17
349500940506PROMSG    *LIBL     KEY0018  18
349600940506PROMSG    *LIBL     KEY0019  19
349700940506PROMSG    *LIBL     KEY0020  20
349800940506PROMSG    *LIBL     KEY0021  21
349900940506PROMSG    *LIBL     KEY0022  22
350000940506PROMSG    *LIBL     KEY0023  23
350100940506PROMSG    *LIBL     KEY0024  24
350200940506**  ABILITAZIONE D1
3503009407200N  01
3504009407200N  02
3505009407201N  03
3506009407201N  04
3507009407200N  05
3508009407200N  06
3509009407200N  07
3510009412061N  08
3511009407200N  09
3512009503011N  10
3513009407200N  11
3514009407201N  12
3515009407200N  13
3516009407200N  14
3517009407200N  15
3518009407200N  16
3519009407200N  17
3520009407201N  18
3521009407200N  19
3522009407200N  20
3523009407200N  21
3524009407200N  22
3525009509060N  23   Impostare a '1' se si utilizzano opzioni
3526009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
352700940916** TASTI DI FUNZIONE  S1
352800940916PROMSG    *LIBL     KEY0001  01
352900940916PROMSG    *LIBL     KEY0002  02
353000940916PROMSG    *LIBL     KEY0003  03
353100940916PROMSG    *LIBL     KEY0004  04
353200940916PROMSG    *LIBL     KEY0005  05
353300940916PROMSG    *LIBL     KEY0006  06
353400940916PROMSG    *LIBL     KEY0007  07
353500940916PROMSG    *LIBL     KEY0008  08
353600940916PROMSG    *LIBL     KEY0009  09
353700940916PROMSG    *LIBL     KEY0010  10
353800940916PROMSG    *LIBL     KEY0011  11
353900940916PROMSG    *LIBL     KEY0012  12
354000940916PROMSG    *LIBL     KEY0013  13
354100940916PROMSG    *LIBL     KEY0014  14
354200940916PROMSG    *LIBL     KEY0015  15
354300940916PROMSG    *LIBL     KEY0016  16
354400940916PROMSG    *LIBL     KEY0017  17
354500940916PROMSG    *LIBL     KEY0018  18
354600940916PROMSG    *LIBL     KEY0019  19
354700940916PROMSG    *LIBL     KEY0020  20
354800940916PROMSG    *LIBL     KEY0021  21
354900940916PROMSG    *LIBL     KEY0022  22
355000940916PROMSG    *LIBL     KEY0023  23
355100940916PROMSG    *LIBL     KEY0024  24
355200940916**  ABILITAZIONE S1
3553009409160N  01
3554009409160N  02
3555009409161N  03
3556009409160N  04
3557009409160N  05
3558009409160N  06
3559009409160N  07
3560009411241N  08
3561009409160N  09
3562009504261N  10
3563009504260N  11
3564009409161N  12
3565009409160N  13
3566009409160N  14
3567009409160N  15
3568009409160N  16
3569009409161N  17
3570009409161N  18
3571009409160N  19
3572009504260N  20
3573009409160N  21
3574009409160N  22
3575009509061N  23   Impostare a '1' se si utilizzano opzioni
3576009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
357700940916** TASTI DI FUNZIONE  D3
357800940506PROMSG    *LIBL     KEY0001  01
357900940506PROMSG    *LIBL     KEY0002  02
358000940506PROMSG    *LIBL     KEY0003  03
358100940506PROMSG    *LIBL     KEY0004  04
358200940506PROMSG    *LIBL     KEY0005  05
358300940506PROMSG    *LIBL     KEY0006  06
358400940506PROMSG    *LIBL     KEY0007  07
358500940506PROMSG    *LIBL     KEY0008  08
358600940506PROMSG    *LIBL     KEY0009  09
358700940506PROMSG    *LIBL     KEY0010  10
358800940506PROMSG    *LIBL     KEY0011  11
358900940506PROMSG    *LIBL     KEY0012  12
359000940506PROMSG    *LIBL     KEY0013  13
359100940506PROMSG    *LIBL     KEY0014  14
359200940506PROMSG    *LIBL     KEY0015  15
359300940506PROMSG    *LIBL     KEY0016  16
359400940506PROMSG    *LIBL     KEY0017  17
359500940506PROMSG    *LIBL     KEY0018  18
359600940506PROMSG    *LIBL     KEY0019  19
359700940506PROMSG    *LIBL     KEY0020  20
359800940506PROMSG    *LIBL     KEY0021  21
359900940506PROMSG    *LIBL     KEY0022  22
360000940506PROMSG    *LIBL     KEY0023  23
360100940506PROMSG    *LIBL     KEY0024  24
360200940916**  ABILITAZIONE D3
3603009407200N  01
3604009407200N  02
3605009407201N  03
3606009407200N  04
3607009407200N  05
3608009407200N  06
3609009407200N  07
3610009407200N  08
3611009407200N  09
3612009409160N  10
3613009407200N  11
3614009407201N  12
3615009407200N  13
3616009407200N  14
3617009407200N  15
3618009407200N  16
3619009409160N  17
3620009409160N  18
3621009407200N  19
3622009407200N  20
3623009407200N  21
3624009407200N  22
3625009509060N  23   Impostare a '1' se si utilizzano opzioni
3626009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
