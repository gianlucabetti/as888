000100990422     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200990422     H*PARMS BNDDIR(PJXBND PJCBND)
000300990422     H*PARMS CVTOPT(*NONE)
000400990422     H DECEDIT('0,') DATEDIT(*DMY.)
000500990422      *
000600170125R250 X***          GUIDA manutenzione mod.770 percipienti
000601170125R250  *            GUIDA Manutenzione CU percipienti
000700990422      *           ----------------------------------------
000800990422      *
000900990422      *
001000990422      * DOCUMENTI MICROANALISI :
001100990422      *
001200990422      * NOTE TECNICHE :
001300990422      *
001400990422      *
001500990422      * PERSONALIZZAZIONI
001600990422      *
001700990422      *
001800990422      *
001900990422      *               MAPPA INDICATORI
002000990422      *
002100990422      *  21           GENERICO OPERAZIONI I/O
002200990422      *  22           GENERICO ERRORE OPERAZIONI I/O
002300990422      *  30           SFLDSP
002400990422      * N31           SFLCLR
002500990422      *  31           SFLDSPCTL
002600990422      *  32           SFLNXTCHG
002700990422      *  33           SFLEND
002800990422      *  39           OF PRTF
002900990422      *  40 <---> 49  DSPATR ERRORI SU SFL
003000990422      *  50 <---> 89  ERROSI SU VIDEO
003100990422      *  elenco errori speciali:
003200990422      *  90           REGISTRAZIONE GIA' ESISTENTE PER IMMISSIONE O COPIA
003300990422      *  91           OPZIONE NON VALIDA
003400990422      *  92           IMPOSSIBILE RIPOSIZIONAMENTO CON OPZIONI PRESENTI SU LISTA
003500990422      *  93           LA FUNZIONE RICHIAMATA SI E' CHIUSA IN ERRORE
003600990422      *  95           L'OPERAZIONE DI SCRITTURA NON E' ANDATA A BUON FINE
003700990422      *  94           UTENTE NON AUTORIZZATO ALLA FUNZIONE NEL MODO DESIDERATO
003800990422      *  96           ESISTONO ERRORI IN ALTRI SCHERMI
003900990422      *  97           TASTO   NON ABILITATO
004000990422      *  98           RICERCA NON ABIL. NELLA POSIZ.
004100990422      *  99           INDIC. GENERALE DI ERRORE
004101170125R250  *----------------------------------------------------*
004102170125R250  *  R250: Modificata anche dicitura del sorgente, togliendo
004103170125R250  *  il riferimento al 770.
004200990422     F*----------------------------------------------------*
004300990422    >F*----------------------------------------------------------------
004400990422    >F* Viste logiche utilizzate per i diversi ordinamenti
004500990422    >F*----------------------------------------------------------------
004600990422    >F* eventuali altre viste logiche a seconda delle parzializzazioni
004700990422     F*------------
004800990422    >FANAPE01L  IF   E           K DISK
004900990422    >F                                     RENAME(anape000:anape01)
005000990422     F*------------
005100990422     FND77005L  IF   E           K DISK
005200990422    >F                                     RENAME(ND770000:ND77005)
005300990422     F*------------
005400990422    >FNDCR75D   CF   E             WORKSTN
005500990422    >F                                     RENAME(CR75D1:FMTD1)
005600990422    >F                                     RENAME(CR75D2:FMTD2)
005700990422    >F                                     RENAME(CR75D3:FMTD3)
005800990422    >F                                     RENAME(CR75S1:FMTS1)
005900990422    >F                                     RENAME(CR75C1:FMTC1)
006000990422    >F                                     RENAME(CR75T1:FMTT1)
006100990422    >F                                     RENAME(CR75Z1:FMTZ1)
006200990422    >F                                     RENAME(CR75Z2:FMTZ2)
006300990422    >F                                     RENAME(CR75Z3:FMTZ3)
006400990422     F                                     SFILE(FMTS1:S1NRR)
006500990422     F                                     INFDS(DSFMT)
006600990422     D*----------------------------------------------------*
006700990422     D* Schiere per la gestione delle opzioni
006800990422     D*
006900990422     D* dimensione della schiera $MS1, da non cambiare
007000990422     D $DIMOP          C                   CONST(30)
007100990422     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
007200990422     D $TX1            S             50    DIM($dimop) ALT($MS1)
007300990422     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
007400990422     D $FL1            S              1    DIM($dimop) ALT($OP1)
007500990422     D* schiera contenente i nomi dei programma richiamati attraverso
007600990422     D* le opzioni personalizzate
007700990422     D $PGO1           S             10    DIM($dimop)
007800990422     D $TX             S             50    DIM(50)
007900990422     D $FL             S              1    DIM(50)
008000990422     D* Schiere generali per la gestione dei tasti di funzione
008100990422     D $FM             S             27    DIM(24)
008200990422     D $FX             S             50    DIM(24)
008300990422     D $FC             S              1    DIM(24)
008400990422     D $FR             S              1    DIM(24)
008500990422     D* Schiere per la gestione dei tasti di funzione
008600990422     D* Video D1
008700990422     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008800990422     D $FX1            S             50    DIM(24) ALT($FM1)
008900990422     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
009000990422     D $FR1            S              1    DIM(24) ALT($FC1)
009100990422     D* Schiera contente i nomi dei programmi da richiamare con i tasti
009200990422     D* funzione personalizzata S1
009300990422     D $PGF2           S             10    DIM(25)
009400990422     D* Schiere per la gestione dei tasti di funzione
009500990422     D* Video S1
009600990422     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
009700990422     D $FX2            S             50    DIM(24) ALT($FM2)
009800990422     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009900990422     D $FR2            S              1    DIM(24) ALT($FC2)
010000990422     D* Schiere per la gestione dei tasti di funzione
010100990422     D* Video D2
010200990422     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
010300990422     D $FX3            S             50    DIM(24) ALT($FM3)
010400990422     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
010500990422     D $FR3            S              1    DIM(24) ALT($FC3)
010600990422     D*
010700990422     D $D1L01          S              3  0
010800990422     D $D1L02          S              3  0
010900990422     D $S1L01          S              3  0
011000990422     D $S1L02          S              3  0
011100990422     D $C1Op1          S              3  0
011200990422     D $C1Op2          S              3  0
011300990422     D $D2L01          S              3  0
011400990422     D $D2L02          S              3  0
011500990422    >D* Inserire qui la definizione di altre variabili per reperire
011600990422    >D* la lunghezza di altre videate che usano un piede diverso da
011700990422    >D* STD
011800990422     D*
011900990422     D*-------------
012000990422     D* Indice della schiera tasti funzione
012100990422     D PosFun          S              2  0
012200990422     D/EJECT
012300990422     D*----------------------------------------------------*
012400990422     D*-------------
012500990422     D* Valori assunti
012600990422     D XMPEDS        E DS                  INZ
012700990422     D XMPEDS1       E DS                  INZ
012800990422     D DSMPE           DS                  INZ
012900990422     D  $LOP                   1      1  0
013000990422     D  $ORD                   2      3  0
013100990422    >D**$DFn                   4     xx
013200990422     D*-------------
013300990422     D* Parametri del PGM di gestione commenti ANA725R
013400990422    >D*  se non sono gestiti i commenti bisogna asteriscare
013500990422     D*ANA725DS      E DS                  INZ
013600990422     D* RET725       E                     INZ('0')
013700990422     D* OPR725       E                     INZ('0')
013800990422     D* ERR725       E                     INZ('0')
013900990422     D*-------------
014000990422     D* Parametri del PGM di test esistenza commenti ANA724R
014100990422    >D*  se non sono gestiti i commenti bisogna asteriscare
014200990422     D*ANA724DS      E DS                  INZ
014300990422     D* EXI724       E                     INZ('0')
014400990422     D* RIS724       E                     INZ('0')
014500990422     D* CMT724       E                     INZ('0')
014600990422     D*-------------
014700990422     D* Parametri del PGM invio posta ANA727R
014800990422    >D*  se non è gestita la posta bisogna asteriscare
014900990422     D*ANA727DS      E DS                  INZ
015000990422     D* RET727       E                     INZ('0')
015100990422     D* OPR727       E                     INZ('0')
015200990422     D* ERR727       E                     INZ('0')
015300990422     D*-------------
015400990422     D* Passaggio Parametri
015500990422     D KPJBA         E DS
015600990422     D*-------------
015700990422     D* Reperimento Autorizzazioni
015800990422     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015900990422     D*-------------
016000990422     D* Passaggio Autorizzazioni
016100990422     D DSAUT           DS           512
016200990422     D*-------------
016300990422     D* Dati di ambiente ottenuti da XSOC
016400990422     D SOC001        E DS                  EXTNAME(XSOC001DS)
016500990422     D*-------------
016600990422     D* DS Interna per dati di output di XSOC
016700990422     D XSOCDS          DS          1000
016800990422     D*-------------
016900990422     D* Parametri del PGM di manutenzione NDCR75R
017000990422    >D NDCR75DS      E DS                  inz
017100990422    >D  RETR75       E                     INZ('0')
017200990422    >D  OPRR75       E                     INZ('0')
017300990422    >D  ERRR75       E                     INZ('0')
017400990422     D*-------------
017500990422     D* campo di salvataggio per la DS d'ingresso durante la chiamata
017600990422     D* alla funzione personalizzata
017700990422    >D $SaveDS         S                   like(NDCr75DS)
017800990422     D*-------------
017900990422     D DSFMT           DS           512
018000990422     D  $TASTO               369    369
018100990422     D  NRG                  370    370
018200990422     D  NCL                  371    371
018300990422     D  SFLNRR               378    379B 0
018400990422     D*-------------
018500990422     D* posizione cursore
018600990422     D CURSOR          DS
018700990422     D  RIRI                   1      2B 0 INZ
018800990422     D  R$$                    2      2
018900990422     D  COCO                   3      4B 0 INZ
019000990422     D  C$$                    4      4
019100990422     D*-------------
019200990422     D* Reperimento nome PGM
019300990422     D STATUS         SDS           333
019400990422     D  DSPGM            *PROC
019500990422     D  PARMS            *PARMS
019600990422     D*-------------
019700990422     D* Costanti
019800990422     D* nr.di righe del sfl (SFLPAG)
019900990924    >D SFLPAG          C                   CONST(11)
020000990422     D*-------------
020100990422     D* Tasti di funzione
020200990422     D F01             C                   CONST(X'31')
020300990422     D F02             C                   CONST(X'32')
020400990422     D F03             C                   CONST(X'33')
020500990422     D F04             C                   CONST(X'34')
020600990422     D F05             C                   CONST(X'35')
020700990422     D F06             C                   CONST(X'36')
020800990422     D F07             C                   CONST(X'37')
020900990422     D F08             C                   CONST(X'38')
021000990422     D F09             C                   CONST(X'39')
021100990422     D F10             C                   CONST(X'3A')
021200990422     D F11             C                   CONST(X'3B')
021300990422     D F12             C                   CONST(X'3C')
021400990422     D F13             C                   CONST(X'B1')
021500990422     D F14             C                   CONST(X'B2')
021600990422     D F15             C                   CONST(X'B3')
021700990422     D F16             C                   CONST(X'B4')
021800990422     D F17             C                   CONST(X'B5')
021900990422     D F18             C                   CONST(X'B6')
022000990422     D F19             C                   CONST(X'B7')
022100990422     D F20             C                   CONST(X'B8')
022200990422     D F21             C                   CONST(X'B9')
022300990422     D F22             C                   CONST(X'BA')
022400990422     D F23             C                   CONST(X'BB')
022500990422     D F24             C                   CONST(X'BC')
022600990422     D ENTER           C                   CONST(X'F1')
022700990422     D ROLDWN          C                   CONST(X'F4')
022800990422     D ROLLUP          C                   CONST(X'F5')
022900990422     D*-------------
023000990422     D* Sottotitolo di testata
023100990422    >D* se non si hanno sottotitoli, mettere tutta la costante = ' '
023200990422     D SOTTIT          C                   CONST('               -
023300990422     D                                                 ')
023400990422     D*-------------
023500990422     D* Variabili per gestione videate
023600990422     D $Gest           S              2A
023700990422     D $Fine           S              1A
023800990422     D $InzD1          S              1A
023900990422     D $InzS1          S              1A
024000990422     D $InzD2          S              1A
024100990422     D $LastG          S              2A
024200990422     D $LastV          S              2A
024300990422     D $EndFile        S              1A
024400990422     D $Esci           S              1A
024500990422     D $RcdOK          S              1A
024600990422     D $Unique         S              1A
024700990422     D*-------------
024800990422     D* Variabili appoggio sempre presenti in tutte le anagrafiche
024900990422     D S1Nrr           S                   like(C1Rcd)
025000990422     D WSfl            S                   like(C1Nrr)
025100990422     D WMax            S                   like(C1Rcd)
025200990422     D WPag            S                   like(C1Rcd)
025300990422     D WInzS1          S                   like($InzS1)
025400990422    >D WOpr            S                   like(OprR75)
025500990422     D CurR            S              2S 0
025600990422     D CurC            S              2S 0
025700990422     D Win             S             99A
025800990422     D W1Pos           S                   like(C1Pos)
025900990422     D WINKR           S              1A
026000990422     D WExf            S              1A
026100990422     D*-------------
026200990422     D* Variabili appoggio specifiche della singola anagrafica
026300990422     D*  campi per parzializzazioni su rcd (solo alfanumerici)
026400990422    >D Anno999         S                   like(AnnoR75)
026500990422    >D Qua999          S                   like(QuaR75)
026600990422    >D*
026700990422     D*  Indici
026800990422     D X               S              3S 0
026900990422     D Y               S              3S 0
027000990422     D* Indice della schiera per le opzioni personalizzate
027100990422     D PosOpz          S              2  0
027200990422     D* numero max opzioni
027300990422     D NumOpz          S              2  0 Inz($DIMOP)
027400990422     D* numero effettive opzioni
027500990422     D OpzLen          S              2  0
027600020415     D*-------------
027700020415     D* Ricerca capoconti
027800020415     D A200DS        E DS                  EXTNAME(ANA200DS) INZ
027900020415     D* Ricerca sottoconti
028000020415     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
028100020415     D*-------------
028200020415     D* DS esterna per dati fornitori
028300020415     D DVAFOR        E DS                  INZ
028400020415     D* DS per controllo conto
028500020415     D ND002DS       E DS                  INZ
028600020415     D* Definizioni parametri driver anagrafici
028700020415     D Dvaric          S              1
028800020415     D DvaSocieta      S              3
028900020415     D DvaUnita        S              8
029000020415     D DvaStrutt       S             10
029100020415     D DvaDtRif        S               D
029200020415     D DvaLenOut       S              9B 0
029300020415     D DvaSnatura      S              1
029400020415     D DvaCodice       S              8
029500020415     D DvaLineav       S              3
029600020415     D DvaFiliale      S              3  0
029700020415     D DvaSogg         S              8
029800020415     D DvaTpInd        S              2
029900020415     D DvaCdInd        S              3
030000020415     D DvaErrore       S              1
030100020415     D DvaOutput       S           4000
030200020415     D*-------------
030300020415     D* Definizioni parametri standard chiamate moduli
030400020415     D Esito           S              1
030500020415     D GesMsg          S              1
030600020415     D LenOut          S              9B 0
030700020415     D Struttura       S             10
030800020415     D Data            S               D
030900020415     D*-------------
031000020415     D $Kcc            S                   LIKE(N77Kcc)
031100020415     D $Ksc            S                   LIKE(N77Ksc)
031200020415     D*-------------
031300021014C1595D* Attributi video
031400021014C1595D Normale         C                   Const(X'32')
031500021014C1595D Protetto        C                   Const(X'A0')
031600990422     C*----------------------------------------------------*
031700990422     C*                MAIN LINE PROGRAM
031800990422     C*----------------------------------------------------*
031900990422     C*
032000990422     C* inizializzazione variabili
032100990422     C                   EXSR      INZVAR
032200990422     C*
032300990422     C     $FINE         DOWEQ     *OFF
032400990422     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
032500990422     C     $GEST         CASEQ     'S1'          GESS1                          Lista
032600990422     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
032700990422     C                   ENDCS
032800990422     C                   ENDDO
032900990422     C* fine programma
033000990422     C                   EXSR      ENDPGM
033100990422     C*
033200990422     C/EJECT
033300990422     C************************************************************
033400990422     C* FINE PROGRAMMA
033500990422     C************************************************************
033600990422     C     ENDPGM        BEGSR
033700990422     C*
033800990422     C* passaggio dati al pgm chiamante
033900990422    >C                   MOVEL     WOPR          OPRR75                                     ne S1
034000990422    >C                   MOVEL     NDCr75DS      KPJBU
034100990422     C*
034200990422     C* Se pgm Guida non è stato richiamato posso chiudere in LR
034300990422    >C     OPZR75        IFEQ      *BLANK
034400990422     C                   SETON                                            LR
034500990422     C* altrimenti chiudo in RT
034600990422     C                   ELSE
034700990422     C                   SETON                                            RT
034800990422     C                   ENDIF
034900990422     C                   RETURN
035000990422     C*
035100990422     C                   ENDSR
035200990422     C/EJECT
035300990422     C************************************************************
035400990422     C* RICERCHE
035500990422     C************************************************************
035600990422     C     SEARCH        BEGSR
035700990422     C*
035800990422     C* determino Riga/Colonna del cursore
035900990422     C                   MOVE      NRG           R$$
036000990422     C                   MOVE      NCL           C$$
036100990422     C                   Z-ADD     RIRI          CURR
036200990422     C                   Z-ADD     COCO          CURC
036300020415      *
036400020415     C     D1kcc         IFNE      *BLANKS
036500020415     C     D1kcc         ANDNE     'C     '
036600020415     C     D1kcc         ANDNE     'F     '
036700020415     C                   CALLB     'XALLINEA'
036800020415     C                   PARM                    D1kcc
036900020415     C                   PARM      6             LEN               5 0
037000020415     C                   PARM                    XSCINP
037100020415     C                   ENDIF
037200020415      *
037300020415     C     D1ksc         IFNE      *BLANKS
037400020415     C                   CALLB     'XALLINEA'
037500020415     C                   PARM                    D1ksc
037600020415     C                   PARM      8             LEN               5 0
037700020415     C                   PARM                    XSCINP
037800020415     C                   ENDIF
037900990422     C*
038000990422   B1C                   SELECT
038100990422     C*
038200990422    >C*--------------------------------------------------------------
038300990422    >C* inserire le routine di ricerca
038400990422    >C* RICORDATE: per tutti i campi che in certe condizioni risultano PR,
038500990422    >C* NON deve essere possibile richiamare i rispettivi pgm in ricerca,
038600990422    >C* SEMMAI richiamarli in visualizzazione (opzione=05)
038700990422    >C*--------------------------------------------------------------
038800990422     C*
038900990422    >C** FMTD1
039000990422    >C*    H1NMFL        WHENEQ    'D1BANCA'
039100990422    >C*                  RESET                   ANA084DS
039200990422    >C*                  MOVEL     '01'          OPZ084
039300990422    >C*                  MOVEL     ANA084DS      KPJBU
039400990422    >C*                  CALL      'ANA084R'
039500990422    >C*                  PARM                    KPJBA
039600990422    >C*                  MOVEL     KPJBU         ANA084DS
039700990422    >C**
039800990422    >C*    Banca084      IFNE      *BLANK
039900990422    >C*                  MOVEL     Banca084      D1Banca
040000990422    >C*                  ENDIF
040100020415      *
040200020415      * FMTD1
040300020415     C     H1NMFL        WHENEQ    'D1KCC'
040400020415     C                   RESET                   A200DS
040500020415     C                   MOVEL     '01'          OPZ200
040600020415     C                   MOVE      D1KCC         KCC200
040700020415     C                   MOVEL     A200DS        KPJBU
040800020415     C                   CALL      'ANA200R'
040900020415     C                   PARM                    KPJBA
041000020415     C                   MOVEL     KPJBU         A200DS
041100020415      *
041200020415     C     KCC200        IFNE      *BLANK
041300020415     C                   MOVEL     KCC200        D1KCC
041400020415     C                   MOVEL     *blank        D1KSC
041500020415     C                   ENDIF
041600020415      *
041700020415      * FMTD1  - SOTTOCONTO
041800020415     C     H1NMFL        WHENEQ    'D1KSC'
041900020415     C                   RESET                   A201DS
042000020415     C                   MOVE      XSCSOC        SOC201
042100020415     C                   MOVE      D1KCC         KCC201
042200020415     C                   MOVE      D1KSC         KSC201
042300020415     C                   MOVEL     A201DS        KPJBU
042400020415     C                   CALL      'ANA201R'
042500020415     C                   PARM                    KPJBA
042600020415     C                   MOVEL     KPJBU         A201DS
042700020415      *
042800020415     C     KSC201        IFNE      *BLANK
042900020415     C                   MOVEL     KCC201        D1KCC
043000020415     C                   MOVEL     KSC201        D1KSC
043100020415     C                   ENDIF
043200990422    >C*
043300990422     C* ricerca non abilitata in questa posizione
043400990422   X1C                   OTHER
043500990422     C                   SETON                                        9899
043600990422     C*
043700990422   E1C                   ENDSL
043800990422     C*
043900990422     C* imposto pos. del cursore
044000990422     C                   Z-ADD     CURR          H1RIGA
044100990422     C                   Z-ADD     CURC          H1COLO
044200990422     C*
044300990422     C                   MOVE      *HIVAL        $LASTV
044400990422     C*
044500990422     C                   ENDSR
044600990422     C/EJECT
044700990422     C************************************************************
044800990422     C* RIEMPIMENTO OPZIONI
044900990422     C************************************************************
045000990422     C     RIEOPZ        BEGSR
045100990422     C*
045200990422     C* Input :
045300990422     C* - $TX = schiera con tutti i testi disponibili
045400990422     C* - $FL = schiere con i flag di validità associati ai testi
045500990422     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
045600990422     C*          opzione usato
045700990422     C* - WMXO = variabile contenente il numero massimo di testi
045800990422     C*          opzione usabili
045900990422     C* - $L01 = Lunghezza prima riga
046000990422     C* - $L02 = Lunghezza seconda riga
046100990422     C* Output :
046200990422     C* - WOPZ1 = variabile contenente la prima riga opzioni
046300990422     C* - WOPZ2 = variabile contenente la seconda riga opzioni
046400990422     C* - FF23  = flag che indica se c'è bisogno del tasto F23
046500990422     C*
046600990422     C                   CALLB     'X04OPMSG'
046700990422     C* input
046800990422     C                   PARM                    $TX
046900990422     C                   PARM                    $FL
047000990422     C                   PARM                    WCOZ              3 0
047100990422     C                   PARM                    WMXO              3 0
047200990422     C* output
047300990422     C                   PARM                    WOPZ1            76
047400990422     C                   PARM                    WOPZ2            76
047500990422     C                   PARM                    FF23              1
047600990422     C* input
047700990422     C                   PARM                    $L01              3 0
047800990422     C                   PARM                    $L02              3 0
047900990422     C*
048000060224C1913C                   PARM                    $ALLOPT         960
048100060224C1913C*
048200990422     C                   ENDSR
048300990422     C/EJECT
048400990422     C************************************************************
048500990422     C* RIEMPIMENTO TASTI FUNZIONE
048600990422     C************************************************************
048700990422     C     RIEKEY        BEGSR
048800990422     C*
048900990422     C* Input :
049000990422     C* - $FX = schiera con tutti i testi disponibili
049100990422     C* - $FC = schiere con i flag di validità associati ai testi
049200990422     C* - $FR = schiere con i flag di REVERSE IMAGE
049300990422     C* - $ULK = ultimo testo caricato
049400990422     C* - $L01 = Lunghezza prima riga
049500990422     C* - $L02 = Lunghezza seconda riga
049600990422     C* Output :
049700990422     C* - $ULK
049800990422     C* - $KEY1= 1° riga di tasti funzione
049900990422     C* - $KEY2= 2° riga di tasti funzione
050000990422     C*
050100990422     C                   CALLB     'XKEYMSG'
050200990422     C* input
050300990422     C                   PARM                    $FX
050400990422     C                   PARM                    $FC
050500990422     C                   PARM                    $FR
050600990422     C                   PARM                    $ULK              3 0
050700990422     C* output
050800990422     C                   PARM                    $KEY1            79
050900990422     C                   PARM                    $KEY2            79
051000990422     C* input
051100990422     C                   PARM                    $L01              3 0
051200990422     C                   PARM                    $L02              3 0
051300990422     C*
051400060224C1913C                   PARM                    $ALLFUNCT       480
051500060224C1913C*
051600990422     C                   ENDSR
051700990422     C/EJECT
051800990422     C************************************************************
051900990422     C* GESTIONE EMISSIONE TASTI FUNZIONE
052000990422     C************************************************************
052100990422     C     GESPIE        BEGSR
052200990422     C*
052300990422     C                   SELECT
052400990422     C* Parzializzazioni
052500990422     C     $GEST         WHENEQ    'D1'
052600990422     C     $ULKD1        IFEQ      0
052700990422     C* riempimento opzioni
052800990422     C                   EXSR      F24D1
052900990422     C                   ELSE
053000990422     C                   WRITE     FMTZ1
053100990422     C                   ENDIF
053200990422     C* Lista
053300990422     C     $GEST         WHENEQ    'S1'
053400990422     C     $ULKS1        IFEQ      0
053500990422     C                   EXSR      F24S1
053600990422     C                   ELSE
053700990422     C                   WRITE     FMTZ2
053800990422     C                   ENDIF
053900990422     C* Modifica valori assunti
054000990422     C     $GEST         WHENEQ    'D2'
054100990422     C     $ULKD2        IFEQ      0
054200990422     C                   EXSR      F24D2
054300990422     C                   ELSE
054400990422     C                   WRITE     FMTZ3
054500990422     C                   ENDIF
054600990422     C                   ENDSL
054700990422     C*
054800990422     C                   ENDSR
054900990422     C/EJECT
055000990422     C************************************************************
055100990422     C* GESTIONE PARZIALIZZAZIONI
055200990422     C************************************************************
055300990422     C     GESD1         BEGSR
055400990422     C* inizializzazione videata
055500990422     C     $INZD1        IFEQ      *ON
055600990422     C                   EXSR      INZD1
055700990422     C                   MOVE      *OFF          $INZD1
055800990422     C                   ENDIF
055900990422     C* emissione piede videata se proveniente da altra
056000990422     C* salvataggio valore formato di provenienza
056100990422     C     $LASTV        IFNE      'D1'
056200990422     C* salvo stato F17 per sapere cosa fare in caso di F12
056300990422     C                   MOVE      *INKR         WINKR
056400990422     C                   WRITE     FMTT1
056500990422     C                   EXSR      GESPIE
056600990422     C                   MOVE      $LASTV        $LASTG
056700990422     C                   MOVE      'D1'          $LASTV
056800990422     C                   ENDIF
056900990422     C*
057000990422     C* se esistono errori sulla videata
057100990422     C* emetto la write del formato a indicatori spenti per vedere
057200990422     C* le eventuali decodifiche
057300990422     C     *IN99         IFEQ      *ON
057400990422     C                   MOVEA     *IN           WIN
057500990422     C                   MOVE      *ALL'0'       IN5098           49
057600990422     C                   MOVEA     IN5098        *IN(50)
057700990422     C                   WRITE     FMTD1
057800990422     C                   MOVEA     WIN           *IN
057900990422     C                   ENDIF
058000990422     C*
058100990422     C                   EXFMT     FMTD1
058200990422     C*
058300990422     C* azzero la posizione del cursore
058400990422     C                   Z-ADD     0             H1RIGA
058500990422     C                   Z-ADD     0             H1COLO
058600990422     C*
058700990422     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
058800990422     C* INPUT
058900990422     C* $FC1 = Abilitazioni videata D1
059000990422     C* OUTPUT
059100990422     C* *IN97 + *IN99 Errori
059200990422     C                   MOVEA     $FC1          $FC
059300990422     C                   EXSR      CTRKEY
059400990422     C*
059500990422     C     *IN99         IFEQ      *OFF
059600990422     C*
059700990422     C                   SELECT
059800990422     C* F2=Decodifica
059900990422     C     $TASTO        WHENEQ    F02
060000990422     C                   EXSR      F02D1
060100990422     C* F3=Fine
060200990422     C     $TASTO        WHENEQ    F03
060300990422     C                   EXSR      F03D1
060400990422     C* F4=Ricerca
060500990422     C     $TASTO        WHENEQ    F04
060600990422     C                   EXSR      SEARCH
060700990422     C* F5=Refresh
060800990422     C     $TASTO        WHENEQ    F05
060900990422     C                   EXSR      F05D1
061000990422     C* F8=Successivo
061100990422     C     $TASTO        WHENEQ    F08
061200990422     C                   EXSR      F08D1
061300990422     C* F10=Immissione
061400990422     C     $TASTO        WHENEQ    F10
061500990422     C                   EXSR      F10D1
061600990422     C* F12=Ritorno
061700990422     C     $TASTO        WHENEQ    F12
061800990422     C                   EXSR      F12D1
061900990422     C* F18=Modifica valori assunti
062000990422     C     $TASTO        WHENEQ    F18
062100990422     C                   EXSR      F18D1
062200990422     C* F24=Altri tasti
062300990422     C     $TASTO        WHENEQ    F24
062400990422     C                   EXSR      F24D1
062500990422     C*
062600990422     C                   OTHER
062700990422     C                   EXSR      CTRD1
062800990422     C     *IN99         IFEQ      *OFF
062900990422     C* definisco l'ordinamento
063000990422     C                   EXSR      DEFORD
063100990422     C* richiamo la lista
063200990422     C                   MOVE      'S1'          $GEST
063300990422     C                   MOVE      *ON           $INZS1
063400990422     C                   MOVE      *blanks       W1POS
063500990422     C                   ENDIF
063600990422     C*
063700990422     C                   ENDSL
063800990422     C*
063900990422     C                   ENDIF
064000990422     C*
064100990422     C                   ENDSR
064200990422     C/EJECT
064300990422     C************************************************************
064400990422     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
064500990422     C************************************************************
064600990422     C     INZD1         BEGSR
064700990422     C*
064800990422     C                   CLEAR                   FMTD1
064900990422     C* Se pgm  richiamato con parametri forniti , li imposto nelle
065000990422     C*    parzializzazioni
065100990422     C                   SELECT
065200990422     C* il pgm è richiamato con chiave univoca per gestione POSTA
065300990422    >C*  nell'esempio la chiave univoca è formata dai campi
065400990422    >C*  Societa/Codice
065500990422     C*    $Unique       WHENEQ    *ON
065600990422    >C*                  MOVE      UQ1001        D1Anno
065700990422     C* il pgm è richiamato e
065800990422     C* sono stati passati dei valori di parzializzazione ma non
065900990422     C* la chiave univoca
066000990422    >C     OPZR75        WHENNE    *BLANK
066100990422     C* imposto i valori passati sui campi a video
066200990422    >C                   MOVE      AnnoR75       D1Anno
066300990422     C* il pgm non è richiamato
066400990422    >C*    OPZR75        WHENEQ    *BLANK
066500990422     C* imposto dei valori di default
066600990422    >C*                  MOVE      '____'        D1______
066700990422     C                   ENDSL
066800990422     C*
066900990422     C                   ENDSR
067000990422     C/EJECT
067100990422     C************************************************************
067200990422     C* GESTIONE F02 VIDEO D1
067300990422     C************************************************************
067400990422     C     F02D1         BEGSR
067500990422     C*
067600990422     C* per decodificare la videata richiamo la routine dei ctrl
067700990422     C* e poi disattivo gli eventuali messaggi di errore
067800990422     C                   EXSR      CTRD1
067900990422     C*
068000990422     C                   MOVE      *ALL'0'       IN5098
068100990422     C                   MOVEA     IN5098        *IN(50)
068200990422     C*
068300990422     C                   ENDSR
068400990422     C/EJECT
068500990422     C************************************************************
068600990422     C* GESTIONE F03 VIDEO D1
068700990422     C************************************************************
068800990422     C     F03D1         BEGSR
068900990422     C*
069000990422     C                   MOVE      *ON           $FINE
069100990422    >C                   MOVE      '1'           RETR75
069200990422     C* fine programma
069300990422     C                   EXSR      ENDPGM
069400990422     C*
069500990422     C                   ENDSR
069600990422     C/EJECT
069700990422     C************************************************************
069800990422     C* GESTIONE F05 VIDEO D1
069900990422     C************************************************************
070000990422     C     F05D1         BEGSR
070100990422     C*
070200990422     C                   MOVE      *ON           $INZD1
070300990422     C*
070400990422     C                   ENDSR
070500990422     C/EJECT
070600990422     C************************************************************
070700990422     C* GESTIONE F08 VIDEO D1
070800990422     C************************************************************
070900990422     C     F08D1         BEGSR
071000990422     C*
071100990422     C                   MOVE      *ON           $FINE
071200990422    >C                   MOVE      '0'           RETR75
071300990422     C*
071400990422     C                   ENDSR
071500990422     C/EJECT
071600990422     C************************************************************
071700990422     C* GESTIONE F10 VIDEO D1
071800990422     C************************************************************
071900990422     C     F10D1         BEGSR
072000990422     C*
072100990422    >C                   RESET                   NDCR75DS
072200990422    >C                   MOVE      xscsoc        SOCIETAR75
072300990422    >C                   MOVEL     '01'          OPZR75
072400990422    >C     CMTR75        IFEQ      '1'
072500990422    >C                   MOVEL     '1'           CMTR75
072600990422     C                   ELSE
072700990422    >C                   MOVEL     '0'           CMTR75
072800990422     C                   ENDIF
072900990422     C* passare anche le parzializzazioni ricevute relative alla chiave
073000990422     C* univoca :
073100990422     C*  se ce ne sono, potranno essere usate come dft d'immissione
073200990422    >C                   IF        OPZR75 <> *Blank
073300990422    >C                   MOVEL     XscSoc        SocietaR75
073400990423    >C                   MOVEL     D1Anno        AnnoR75
073500020419    >C                   MOVEL     'AU'          QuaR75
073600990423    >C                   MOVEL     S1Kcc         KccR75
073700990423    >C                   MOVEL     S1Ksc         KscR75
073800990423    >C                   MOVEL     H1Rgt         RgtR75
073900990423    >C                   MOVEL     H1Prg         PrgR75
074000150626R186 C                   MoveL     H1C77         C77R75
074100990507    >C                   MOVEL     H1Cog         COGR75
074200990507    >C                   MOVEL     H1Nom         NOMR75
074300150626R186 C                   MoveL     H1CDF         CDFR75
074400990422     C                   ENDIF
074500990422     C* codice di riferimento per le valorizzazioni di default
074600990422    >C***??*****         MOVE      $DFn          DF1002
074700990422    >C                   MOVEL     NDCR75DS      KPJBU
074800990422    >C                   CALL      'NDCR75R1'
074900990422     C                   PARM                    KPJBA
075000990422    >C                   MOVEL     KPJBU         NDCR75DS
075100990422     C* ritorno da PGM gestione
075200990422    >C                   EXSR      RTN002
075300990422     C*
075400990422     C                   MOVE      *HIVAL        $LASTV
075500990422     C*
075600990422     C                   ENDSR
075700990422     C/EJECT
075800990422     C************************************************************
075900990422     C* GESTIONE F12 VIDEO D1
076000990422     C************************************************************
076100990422     C     F12D1         BEGSR
076200990422     C*
076300990422     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
076400990422     C*  per F12 torno alla lista
076500990422     C     $LOP          IFEQ      2
076600990422     C     WINKR         OREQ      *ON
076700990422     C                   MOVE      'S1'          $GEST
076800990422     C*  altrimenti, per F12 torno a menu
076900990422     C                   ELSE
077000990422     C                   MOVE      *ON           $FINE
077100990422    >C                   MOVE      '2'           RETR75
077200990422     C                   ENDIF
077300990422     C*
077400990422     C                   ENDSR
077500990422     C/EJECT
077600990422     C************************************************************
077700990422     C* GESTIONE F18 VIDEO D1
077800990422     C************************************************************
077900990422     C     F18D1         BEGSR
078000990422     C*
078100990422     C                   MOVE      'D2'          $GEST
078200990422     C                   MOVEL     *ON           $INZD2
078300990422     C*
078400990422     C                   ENDSR
078500990422     C/EJECT
078600990422     C************************************************************
078700990422     C* GESTIONE F24 VIDEO D1
078800990422     C* input
078900990422     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
079000990422     C* $FX1 =Testi tasti funzione
079100990422     C* $FC1 = Abilitazioni
079200990422     C* $FR1 = Inversioni di fondo
079300990422     C************************************************************
079400990422     C     F24D1         BEGSR
079500990422     C*
079600990422     C                   MOVEA     $FX1          $FX
079700990422     C                   MOVEA     $FC1          $FC
079800990422     C                   MOVEA     $FR1          $FR
079900990422     C                   Z-ADD     $ULKD1        $ULK
080000990422     C                   Z-ADD     $D1L01        $L01
080100990422     C                   Z-ADD     $D1L02        $L02
080200990422     C                   EXSR      RIEKEY
080300990422     C                   Z-ADD     $ULK          $ULKD1            3 0
080400990422     C                   MOVEA     $FC           $FC1
080500990422     C                   MOVEL     $KEY1         Z1KE1
080600060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
080700990422     C                   MOVEL     $KEY2         Z1KE2
080800990422     C                   WRITE     FMTZ1
080900990422     C*
081000990422     C*
081100990422     C                   ENDSR
081200990422     C/EJECT
081300990422     C************************************************************
081400990422     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
081500990422     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
081600990422     C* 93 94 96 97 98
081700990422     C************************************************************
081800990422     C     CTRD1         BEGSR
081900990422     C*
082000990422     C                   SETOFF                                       99
082100990422     C*
082200990422     C* Anno
082300990422$009 C     D1Anno        IFeq      0
082400990422     C                   SetOn                                        5199
082500990422     C                   EndIf
082600990422     C*
082700020415     C* Conto
082800020415     C                   CLEAR                   D1KScd
082900150206A0   C     D1KCC         IFNE      *BLANKS
083000020415     C     D1KSC         ORNE      *BLANKS
083100020415     C*
083200020415     C* xallinea
083300020415     C     D1KCC         IFNE      'C     '
083400020415     C     D1KCC         andne     'F     '
083500020415     C     D1KCC         ANDNE     *blanks
083600020415     C                   CALLB     'XALLINEA'
083700020415     C                   PARM                    D1KCC
083800020415     C                   PARM      6             LEN               5 0
083900020415     C                   PARM                    XSCINP
084000020415     C                   ENDIF
084100020415     C     D1KSC         IFNE      *BLANKS
084200020415     C                   CALLB     'XALLINEA'
084300020415     C                   PARM                    D1KSC
084400020415     C                   PARM      8             LEN               5 0
084500020415     C                   PARM                    XSCINP
084600020415     C                   Endif
084700020415     C*
084800020415     C* xcapclifor
084900020415     C                   MOVE      D1KCC         $kcc
085000020415     C                   MOVE      D1KSC         $ksc
085100020415     C     D1KCC         IFEQ      'C       '
085200020415     C     D1KCC         OREQ      'F       '
085300020415     C                   CALLB     'XCAPCLIFOR'
085400020415     C                   PARM                    XSCSOC
085500020415     C                   PARM                    $kcc
085600020415     C                   PARM                    $ksc
085700020415     C                   MOVE      $kcc          D1KCC
085800020415     C                   ENDIF
085900020415     C*
086000020415     C* Richiamo routine per controllare conto
086100020415     C                   Clear                   ND002DS
086200020415     C                   EVAL      LenOut = %Size(ND002DS)
086300020415     C                   CallB     'NDMVC002'
086400020415     C                   PARM                    XSCSOC
086500020415     C                   PARM                    $Kcc
086600020415     C                   PARM                    $Ksc
086700020415     C                   PARM      *Loval        Data
086800020415     C                   PARM      *OFF          GesMsg
086900020415     C                   PARM      *OFF          Esito
087000020415     C                   PARM      'ND002DS'     Struttura
087100020415     C                   PARM                    ND002DS
087200020415     C                   PARM                    LenOut
087300020415     C*
087400150206A1   C     Esito         IFEQ      *ON
087500020415     C                   SETON                                        5299
087600150206x1   C                   ELSE
087700020415     C*
087800020415     C                   MOVEL     DESBREV002    D1KSCD
087900020415     C*
088000020415     C* Se è c'è anche il sottoconto, deve essere percipiente
088100150206a2   C                   if        D1Ksc <> *Blanks
088200020415     C*
088300020415     C* Controllo esistenza in dvasog come fornitore percipiente
088400150206a3   C                   if        snatura002='F'
088500020415     C*
088600020415     C                   MOVE      '1'           DVARIC
088700020415     C                   MOVE      xscsoc        DVASOCIETA
088800020415     C                   MOVE      snatura002    DVASNATURA
088900020415     C                   MOVE      tpind002      DVATPIND
089000020415     C                   MOVE      cdind002      DVACDIND
089100020415     C                   MOVE      d1ksc         DVAcodice
089200020415     C                   MOVEL     'DVAFOR'      DVAstrutt
089300020415     C                   EVAL      DVALenOut = %Size(DVAFOR)
089400020415     C*
089500020415     C                   CALLB     'NDDVASOG'    PDVASOG
089600020415     C*
089700150206a4   C                   if        DVAerrore <> *ON
089800020415     C                   eval      %subst(DVAFOR:1:DVALENOUT)
089900020415     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
090000150206a5   C                   if        DVfkcca = *blank
090100020415     C* il fornitore non è un percipiente
090200020415     C                   seton                                        5499
090300150206e5   C                   endif
090400150206x4   C                   else
090500020415     C* conto inesistente
090600020415     C                   seton                                        5299
090700150206e4   C                   endif
090800020415     C*
090900150206x3   C                   else
091000020415     C*
091100020415     C* codice fornitore inesistente
091200020415     C                   seton                                        5599
091300020415     C*
091400150206e3   C                   endif
091500020415     C*
091600150206e2   C                   endif
091700020415     C*
091800150206e1   C                   endif
091900020415     C*
092000150206e0   C                   endif
092100990422     C*
092200150206R165 C                   If        D1Kcc         <> *BLANKS   and
092300150206 "   C                             D1Ksc         <> *BLANKS   and
092400150206 "   C                             D1CDF         <> *BLANKS
092500150206 "   C                   Seton                                        5699
092600150206 "   C                   EndiF
092700150206R165 C*
092800990422     C                   ENDSR
092900990422     C/EJECT
093000990422     C************************************************************
093100990422     C* DEFINIZIONE DELL'ORDINAMENTO
093200990422     C************************************************************
093300990422     C     DEFORD        BEGSR
093400990422     C*
093500990422     C* definisco l'ordinamento della lista a seconda dei valori
093600990422     C* impostati nel video di parzializzazione
093700990422     C*
093800990422     C                   SELECT
093900990422    >C*  nel caso di più viste logiche e quindi più campi di
094000990422    >C*  parzializzazione, qui vanno definiti i criteri per
094100990422    >C*  determinare quale VL va letta, a seconda di quali campi
094200990422    >C*  sono stati valorizzati
094300990422    >C     D1Anno        WHENNE    0
094400990422     C                   Z-ADD     1             $ORD
094500990422    >C*    D1Descr       WHENNE    *BLANK
094600990422     C*                  Z-ADD     2             $ORD
094700990422     C* imposto l'ordinamento 99 per ottenere in selezione solo il
094800990422     C* rcd con la chiave passata
094900990422     C     $Unique       WHENEQ    *ON
095000990422     C                   Z-ADD     99            $ORD
095100990422     C* nel caso non sia stato digitato alcun valore in parzializzazione
095200990422     C* utilizzerò l'ordinamento precedentemente memorizzato
095300990422     C* (temporaneamente o definitivamente) nei valori assunti
095400990422     C                   OTHER
095500990422     C                   ENDSL
095600990422     C                   ENDSR
095700990422     C/EJECT
095800990422     C************************************************************
095900990422     C* Chiamata programma con tasto personalizzato
096000990422     C************************************************************
096100990422     C     FPerS1        BEGSR
096200990422     C*
096300990422     C* salvo la DS di ingresso
096400990422    >C                   MOVEL     NDCr75DS      $SaveDS
096500990422     C*
096600990422    >C                   MOVEL     NDCr75DS      KPJBU
096700990422     C                   CALL      $PGF2(PosFun)                        22
096800990422     C                   PARM                    KPJBA
096900990422     C                   PARM                    NOMPGM
097000990422     C*
097100990422     C* ripristino la DS di ingresso
097200990422    >C                   MOVEL     $SaveDS       NDCr75DS
097300990422     C*
097400990422     C                   ENDSR
097500990422     C/EJECT
097600990422     C************************************************************
097700990422     C* GESTIONE LISTA
097800990422     C************************************************************
097900990422     C     GESS1         BEGSR
098000990422     C* inizializzazione videata
098100990422     C     $INZS1        IFEQ      *ON
098200990422     C                   EXSR      INZS1
098300990422     C                   MOVE      *OFF          $INZS1
098400990422     C                   ENDIF
098500990422     C* emissione piede videata se proveniente da altra
098600990422     C* salvataggio valore formato di provenienza
098700990422     C     $LASTV        IFNE      'S1'
098800990422     C                   WRITE     FMTT1
098900990422     C                   EXSR      GESPIE
099000990422     C                   MOVE      $LASTV        $LASTG
099100990422     C                   MOVE      'S1'          $LASTV
099200990422     C                   ENDIF
099300990422     C*
099400990422     C     WMAX          IFEQ      0
099500990422     C                   WRITE     FMTD3                                        NO RCD
099600990422     C                   ENDIF
099700990422     C* Inizializzazione indice tasti funzione personalizzati
099800990422     C                   Eval      PosFun = 25
099900990422     C*
100000990422     C                   EXFMT     FMTC1
100100990422     C*
100200990422     C* azzero la posizione del cursore
100300990422     C                   Z-ADD     0             H1RIGA
100400990422     C                   Z-ADD     0             H1COLO
100500990422     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
100600990422     C* di refresh dello sfondo
100700990422     C                   MOVE      '1'           WExf
100800990422     C*
100900990422     C     C1NRR         IFNE      0
101000990422     C                   Z-ADD     C1NRR         WSFL
101100990422     C                   ENDIF
101200990422     C                   IF        SFLNRR > 0
101300990422     C                   Z-ADD     SFLNRR        C1RCD
101400990422     C                   ENDIF
101500990422     C* inizializzazione indice opzioni personalizzate
101600990422     C                   Eval      PosOpz = 0
101700990422     C* CONTROLLO ABILITAZIONE TASTI
101800990422     C* INPUT
101900990422     C* $FC2 = Abilitazioni videata S1
102000990422     C* OUTPUT
102100990422     C* *IN97 + *IN99 Errori
102200990422     C                   MOVEA     $FC2          $FC
102300990422     C                   EXSR      CTRKEY
102400990422     C*
102500990422     C     *IN99         IFEQ      *OFF
102600990422     C*
102700990422     C                   SELECT
102800990422     C* F3=Fine
102900990422     C     $TASTO        WHENEQ    F03
103000990422     C                   EXSR      F03S1
103100990422     C* F8=Successivo
103200990422     C     $TASTO        WHENEQ    F08
103300990422     C                   EXSR      F08S1
103400990422     C* F10=Immissione
103500990422     C     $TASTO        WHENEQ    F10
103600990422     C                   EXSR      F10S1
103700990422     C* F12=Ritorno
103800990422     C     $TASTO        WHENEQ    F12
103900990422     C                   EXSR      F12S1
104000990422     C* F17=Parzializzazioni
104100990422     C     $TASTO        WHENEQ    F17
104200990422     C                   EXSR      F17S1
104300990422     C* F18=Modifica valori assunti
104400990422     C     $TASTO        WHENEQ    F18
104500990422     C                   EXSR      F18S1
104600990422     C* F23=Altre opzioni
104700990422     C     $TASTO        WHENEQ    F23
104800990422     C                   EXSR      F23S1
104900990422     C* F24=Altri tasti
105000990422     C     $TASTO        WHENEQ    F24
105100990422     C                   EXSR      F24S1
105200990422     C* ROLL-UP
105300990422     C     $TASTO        WHENEQ    ROLLUP
105400990422     C                   EXSR      ROLS1
105500990422     C* Personalizzata
105600990422     C     $PGF2(PosFun) Whenne    *BLANK
105700990422     C                   EXSR      FperS1
105800990422     C*
105900990422     C                   OTHER
106000990422     C* CONTROLLO DATI
106100990422     C                   EXSR      CTRC1
106200990422     C     *IN99         IFEQ      *OFF
106300990422     C                   EXSR      CTRS1
106400990422     C                   ENDIF
106500990422     C*
106600990422     C                   ENDSL
106700990422     C*
106800990422     C                   ENDIF
106900990422     C*
107000990422     C                   ENDSR
107100990422     C/EJECT
107200990422     C************************************************************
107300990422     C* INIZIALIZZAZIONE LISTA
107400990422     C************************************************************
107500990422     C     INZS1         BEGSR
107600990422     C* pulizia SFL
107700990422     C                   SETOFF                                         3031
107800990422     C                   WRITE     FMTC1
107900990422     C                   SETON                                          31
108000990422     C*
108100990422     C* riempimento opzioni
108200990422     C                   MOVEA     $TX1          $TX
108300990422     C                   MOVEA     $FL1          $FL
108400990422     C                   Z-ADD     0             WCOZ
108500990422     C                   Z-ADD     0             WMXO
108600990422     C*  equivale al num.di elementi della schiera $TX1
108700990422     C                   Z-add     OpzLen        WMXO
108800990422     C                   Z-ADD     $C1Op1        $L01
108900990422     C                   Z-ADD     $C1Op2        $L02
109000990422     C                   EXSR      RIEOPZ
109100990422     C                   Z-ADD     23            X
109200990422     C                   MOVE      FF23          $FC2(X)
109300990422     C                   MOVEL     WOPZ1         C1OPZ1
109400060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
109500990422     C                   MOVEL     WOPZ2         C1OPZ2
109600990422     C                   Z-ADD     WCOZ          C1COZ
109700990422     C*
109800990422     C* CARICAMENTO SFL A PAGINE
109900990422     C*
110000990422     C                   Z-ADD     0             S1NRR
110100990422     C                   Z-ADD     0             WMAX
110200990422     C*
110300990422     C* Posizionamento su file pilota
110400990422     C                   CLEAR                   C1POS
110500990422     C                   EXSR      SETANA
110600990422     C* ripristino i valori del posizionamento
110700990422     C                   MOVE      W1POS         C1POS
110800990422    >C* aggiungere gestione altri campi posizionamento se ci sono
110900990422     C*
111000990422     C* Lettura file pilota
111100990422     C                   EXSR      REDANA
111200990422     C*
111300990422     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
111400990422     C                   IF        $LastV = 'D1'
111500990422     C                   Z-ADD     SFLPAG        WPAG
111600990422     C                   ENDIF
111700990422     C                   EXSR      ROLS1
111800990422     C* imposto il caricamento sfl del numero massimo di righe per pagina
111900990422     C                   Z-ADD     SFLPAG        WPAG
112000990422     C*
112100990422     C                   ENDSR
112200990422     C/EJECT
112300990422     C************************************************************
112400990422     C* CARICAMENTO PAGINA LISTA
112500990422     C************************************************************
112600990422     C     ROLS1         BEGSR
112700990422     C*
112800990422     C                   SETOFF                                       32
112900990422     C                   Z-ADD     0             Y
113000990422     C                   Z-ADD     WMAX          S1NRR
113100990422     C*
113200990422     C* Leggo dal file anagrafico per caricare la lista
113300990422     C*
113400990422     C     $EndFile      DOWEQ     *OFF
113500990422     C     Y             ANDLT     WPAG
113600990422     C*
113700990422     C                   EXSR      RIES1
113800990422     C*
113900990422     C                   ADD       1             S1NRR
114000990422     C                   ADD       1             Y
114100990422     C*
114200990422     C                   WRITE     FMTS1
114300990422     C*
114400990422     C                   EXSR      REDANA
114500990422     C*
114600990422     C                   ENDDO
114700990422     C*
114800990422     C                   Z-ADD     S1NRR         WMAX                 30
114900990422     C*
115000990422     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
115100990422     C*
115200990422     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
115300990422     C                   MVR                     RESTO             3 0
115400990422     C     PAGINE        MULT      SFLPAG        C1RCD
115500990422     C     RESTO         IFGT      0
115600990422     C                   ADD       1             C1RCD
115700990422     C                   ELSE
115800990422     C                   SUB       SFLPAG        C1RCD
115900990422     C                   ADD       1             C1RCD
116000990422     C                   ENDIF
116100990422     C*
116200990422     C                   ENDSR
116300990422     C/EJECT
116400990422     C************************************************************
116500990422     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
116600990422     C************************************************************
116700990422     C     SETANA        BEGSR
116800990422     C*
116900990422    >C* se tutte le VL hanno la società in chiave (ed è la principale)
117000990422    >C* si può impostare una volta per tutte al di fuori dei test
117100990422    >C                   MOVE      XscSoc        N77Societa
117200990422     C*
117300990422     C                   SELECT
117400990422     C*
117500990422     C* Gestione posizionamento
117600990422     C* Se il campo W1POS è > *blank imposto il posizionamento con
117700990422     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
117800990422     C*
117900990422    >C* Ord.1 = chiave ordinamento 1
118000990422     C     $ORD          WHENEQ    1
118100990422     C     W1POS         IFNE      *blanks
118200990422    >C                   MOVEL     W1POS         N77Cog
118300990422     C                   ELSE
118400990422    >C                   MOVEL     *blanks       N77Cog
118500990422     C                   ENDIF
118600990423    >C                   z-add     D1Anno        N77ACF
118700020415     C                   MOVEL     'AU'          N77Qua
118800990507    >C     K04N7705      SETLL     ND77005
118900990422     c*
119000990422     c*
119100990422     c*
119200990422    >C* Ord.3 = chiave ordinamento 3
119300990422     C*    $ORD          WHENEQ    3
119400990422     C*    W1POS         IFNE      *BLANK
119500990422    >C*                  MOVEL     W1POS         ___Banca
119600990422     C*                  ELSE
119700990422    >C*                  MOVEL     D1Banca       ___Banca
119800990422    >C*                  MOVEL     D1Linea       ___Linea
119900990422     C*                  ENDIF
120000990422    >C*    K03___03      SETLL     AN___02
120100990422     C* Ord.99= Chiave univoca per la POSTA
120200990422     C* (quando viene gestita la chiave univoca NON deve essere gestito
120300990422     C*  il posizionamento C1POS/W1POS)
120400990422     C                   ENDSL
120500990422     C*
120600990422     C                   ENDSR
120700990422     C/EJECT
120800990422     C************************************************************
120900990422     C* LETTURA RCD ARCHIVIO PILOTA
121000990422     C************************************************************
121100990422     C     REDANA        BEGSR
121200990422     C*
121300990422     C                   MOVEL     *OFF          $EndFile
121400990422     C                   MOVEL     *OFF          $RCDOK
121500990422     C*
121600990422     C     $EndFile      DOUEQ     *ON
121700990422     C     $RCDOK        OREQ      *ON
121800990422     C*
121900990422     C                   SELECT
122000990422     C*
122100990422     C     $ORD          WHENEQ    1
122200990422    >C* per quei campi che usano XALLINEA e/o che sono a riempimento
122300990422    >C* completo (che, quindi, non hanno la gestione del range di lettura)
122400990422    >C* si può usare la READE con la chiave completa (che se è UNIQUE darà
122500990422    >C* ovviamente un rcd solo)
122600990422    >C                   MOVEL     XscSoc        N77Societa
122700990423    >C                   z-add     D1Anno        N77ACF
122800020415     C                   MOVEL     'AU'          N77Qua
122900990422    >C     K03N7705      READE     ND77005                                21
123000990422     C*
123100990422     C*    $ORD          WHENEQ    2
123200990422    >C* se esiste la società come chiave primaria e il resto della chiave
123300990422    >C* viene elaborata per il range di lettura, viene eseguita la READE
123400990422    >C* solo con la società
123500990422    >C*    XscSoc        READE     AN___02                                21
123600990422     C*
123700990422     C*    $ORD          WHENEQ    3
123800990422    >C* nel caso di più campi chiave per questa VL, presenti contemporanea_
123900990422    >C* mente in parzializzazione, i quali gestiscono tutti il range di
124000990422    >C* lettura, non varia il modo di reperire il rcd
124100990422    >C*    XscSoc        READE     AN___03                                21
124200990422     C*
124300990422     C*    $ORD          WHENEQ    99
124400990422    >C* nel caso di lettura dati POSTA, basta leggere in READE con la
124500990422    >C* chiave precedentemente riempita nella routine SetAna
124600990422    >C*    K02___99      READE     AN___99                                21
124700990422     C*
124800990422    >C*  questo è lo schema generico da usare
124900990422    >C*    $ORD          WHENEQ    __
125000990422    >C*                  READ      AN___nn                                21
125100990422     C                   ENDSL
125200990422     C*
125300990422     C                   MOVE      *IN21         $EndFile
125400990422     C*
125500990422     C     $EndFile      IFEQ      *OFF
125600990422     C                   EXSR      SELANA
125700990422     C                   ENDIF
125800990422     C*
125900990422     C                   ENDDO
126000990422     C*
126100990422     C                   MOVE      *IN21         *IN33                          SflEnd
126200990422     C*
126300990422     C                   ENDSR
126400990422     C/EJECT
126500990422     C************************************************************
126600990422     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
126700990422     C************************************************************
126800990422     C     CLCPAG        BEGSR
126900990422     C* Input :
127000990422     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
127100990422     C*          cursore
127200990422     C* - SFLPAG = numero rcd per pagina sfl
127300990422     C* Output :
127400990422     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
127500990422     C*
127600990422     C     WSFL          DIV       SFLPAG        PAGINE            4 0
127700990422     C                   MVR                     RESTO             3 0
127800990422     C     RESTO         IFGT      0
127900990422     C                   ADD       1             PAGINE
128000990422     C                   ENDIF
128100990422     C     PAGINE        MULT      SFLPAG        WPAG
128200990422     C*
128300990422     C                   ENDSR
128400990422     C/EJECT
128500990422     C************************************************************
128600990422     C* SELEZIONI SULLA RIGA LETTA
128700990422     C* impostare a *OFF il campo $RCDOK per scartare un record che
128800990422     C* non rientra nei parametri di parzializzazione
128900990422     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
129000990422     C* superiore stabilito nei parametri
129100990422     C*
129200990422     C************************************************************
129300990422     C     SELANA        BEGSR
129400990422     C*
129500990422     C                   MOVEL     *ON           $RCDOK
129600990422     C***                MOVEL     *ALL'9'       ALL9             15
129700990422    >C***                Move      D1Anno        W1Anno            4
129800990422     C* creo limite massimo parzializzazioni :
129900990422     C* per ogni valore alfanumerico creo il limite superiore
130000990422     C* per cui caricare, creando una variabile che contiene
130100990422     C* il valore digitato nel campo a video riempito di "9"
130200990422    >C***  D1Anno        CAT       ALL9:0        Anno999
130300990422    >C***  D1Qua         CAT       ALL9:0        Qua999
130400990422     C*
130500150206R165 C                   Clear                   WrkCDF           20
130600150206R165 C                   If        D1CDF <> *blanks
130700150206R165 C                   Select
130800150206R165 C                   When      N77CDF        <> *blanks
130900150206R165 C                   MOVEL     N77CDF        WrkCDF
131000150211R165 C                   When      N77CDE        <> *blanks
131100150211R165 C                   MOVEL     N77CDE        WrkCDF
131200150206R165 C                   EndSl
131300150206R165 C                   EndIf
131400990422     C*
131500990422     C                   SELECT
131600990422     C*
131700990422     C* SELEZIONI RISPETTO AI VALORI RCD
131800990422     C*
131900990422     C* Scartare i record che non rispondono alle selezioni effettuate
132000990422     C* esempio : esclusione degli annullati.
132100990422     C*
132200990422    >C*    ___ANN        WHENEQ    'A'
132300990422    >C*                  MOVEL     *OFF          $RCDOK
132400990422     C*
132500990422     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
132600990422     C*
132700990422    >C** descrizione non compreso nella scelta
132800990422    >C***  N77ACF        WHENLT    D1Anno
132900990422    >C***  N77ACF        ORGT      Anno999
133000990422    >C***                MOVEL     *OFF          $RCDOK
133100990422    >C
133200990422     C***  N77Qua        WhenLt    D1Qua
133300990422     C***  N77Qua        orgt      Qua999
133400990422    >C***                MOVEL     *OFF          $RCDOK
133500020415     C
133600020415     C     N77Kcc        WhenNE    D1Kcc
133700020415     C     D1Kcc         AndNE     *Blanks
133800020415     C                   MOVEL     *OFF          $RCDOK
133900020415     C
134000020415     C     N77Ksc        WhenNE    D1Ksc
134100020415     C     D1Ksc         AndNE     *Blanks
134200020415     C                   MOVEL     *OFF          $RCDOK
134300150206R165 C
134400150206R165 C     WrkCDF        WhenNE    D1CDF
134500150206R165 C     D1CDF         AndNE     *Blanks
134600150206R165 C                   MOVEL     *OFF          $RCDOK
134700990422      *
134800990422     C*
134900990422    >C*  questo è lo schema generico da usare
135000990422    >C*    xxxxxx        WHENLT    D1xxx
135100990422    >C*    xxxxxx        ORGT      xxx999
135200990422    >C*                  MOVEL     *OFF          $RCDOK
135300990422     C*
135400990422     C                   ENDSL
135500990422     C*
135600990422     C* FACILITAZIONE NEGLI ACCESSI
135700990422     C*
135800990422     C* nel caso che uno dei dati letti sia > del limite superiore
135900990422     C* oppure < del limite inferiore è necessario fare dei
136000990422     C* ragionamenti onde evitare altri accessi inutili;
136100990422     C* questi ragionamenti dovranno tenere conto dell'ordinamento
136200990422     C* attualmente vigente, per cui si potranno verificare i seguenti
136300990422     C* casi :
136400990422     C* 1. un campo è > del limite superiore ed è la 1a chiave della
136500990422     C*    klist dell'ordinamento vigente :
136600990422     C*    nessun altro rcd sarà valido per cui bisogna andare a fine
136700990422     C*    file mediante un *HIVAL SETLL;
136800990422     C* 2. un campo è > del limite superiore e NON è la 1a chiave della
136900990422     C*    klist dell'ordinamento vigente :
137000990422     C*    nessun rcd che verrà letto con gli stessi valori per la
137100990422     C*    precedente porzione di klist sarà valido, per cui
137200990422     C*    conviene saltarli facendo una SETGT apunto con la klist che
137300990422     C*    esclude il campo in esame;
137400990422     C* 3. un campo è < del limite inferiore e NON è la 1a chiave della
137500990422     C*    klist dell'ordinamento vigente :
137600990422     C*    bisogna arrivare al più presto al limite inferiore per cui
137700990422     C*    conviene fare una SETLL con una klist avente come ultimo
137800990422     C*    campo quello in esame, lasciando per gli altri campi gli
137900990422     C*    stessi valori letti ma sostituendo per quello in esame
138000990422     C*    il limite inferiore;
138100990422     C* non è possibile che un campo che è anche chiave primaria
138200990422     C* dell'ordinamento vigente sia < del limite inferiore perchè
138300990422     C* nella routine SETANA la setll viene appunto fatta con il
138400990422     C* limite inferiore;
138500990422     C* non si deve prendere in considerazione il caso che un campo
138600990422     C* non presente nella chiave dell'ordinamento vigente sia
138700990422     C* esterno ai limiti perchè non genererebbe migliorie negli
138800990422     C* accessi;
138900990422     C*
139000990422     C*********          IF        $RcdOk = *off
139100990422     C*
139200990422     C***                SELECT
139300990422    >c**
139400990422    >c** descrizione > del limite superiore
139500990422    >C*    ___Descr      WHENGT    Descr999
139600990422    >C*                  SELECT
139700990422    >c** ordinamento per descrizione
139800990422    >C*    $ORD          WHENEQ    2
139900990422    >C*    *HIVAL        SETLL     AN___02
140000990422    >C** altri ordinamenti
140100990422    >C*    $ORD          WHENEQ    _
140200990422    >C*   non ci interessano perchè non hanno questo campo in chiave
140300990422    >C*                  ENDSL
140400990422    >C**
140500990422    >C** descrizione < del limite inferiore
140600990422    >C*    ___Descr      WHENLT    D1Descr
140700990422    >C*   non è possibile perchè la descrizione è presente in una unica
140800990422    >C*   VL ma come chiave primaria
140900990422    >C**
141000990422    >C** banca > del limite superiore
141100990422    >C*    ___Banca      WHENGT    Banca999
141200990422    >C*                  SELECT
141300990422    >C** ordinamento per banca/linea
141400990422    >C*    $ORD          WHENEQ    3
141500990422    >C*    *HIVAL        SETLL     AN___03
141600990422    >C** altri ordinamenti
141700990422    >C*    $ORD          WHENEQ    _
141800990422    >C*   non ci interessano perchè non hanno questo campo in chiave
141900990422    >C*                  ENDSL
142000990422    >C**
142100990422    >C** banca < del limite inferiore
142200990422    >C*    ___Banca      WHENLT    D1Banca
142300990422    >C*   non è possibile perchè la banca è presente in una unica
142400990422    >C*   VL ma come chiave primaria
142500990422    >C**
142600990422    >C** Linea > del limite superiore
142700990422    >C*    ___Linea      WHENGT    Linea999
142800990422    >C*                  SELECT
142900990422    >C** ordinamento per banca/linea
143000990422    >C*    $ORD          WHENEQ    3
143100990422    >C*    ___Banca      SETGT     AN___03
143200990422    >C** altri ordinamenti
143300990422    >C*    $ORD          WHENEQ    _
143400990422    >C*   non ci interessano perchè non hanno questo campo in chiave
143500990422    >C*                  ENDSL
143600990422    >C**
143700990422    >C** Linea < del limite inferiore
143800990422    >C*    ___Linea      WHENLT    D1Linea
143900990422    >C*                  SELECT
144000990422    >C** ordinamento per banca/linea
144100990422    >C*    $ORD          WHENEQ    3
144200990422    >C*                  MOVE      D1Linea       ___Linea
144300990422    >C*    K03___03      SETLL     AN___03
144400990422    >C** altri ordinamenti
144500990422    >C*    $ORD          WHENEQ    _
144600990422    >C*   non ci interessano perchè non hanno questo campo in chiave
144700990422    >C*                  ENDSL
144800990422    >C*
144900990422    >C*--------------------------------------------------------------
145000990422     C*                  ENDSL
145100990422     C*
145200990422     C*                  ENDIF
145300990422     C*
145400990422     C                   ENDSR
145500990422     C/EJECT
145600990422     C************************************************************
145700990422     C* RIEMPIMENTO RIGA LISTA
145800990422     C************************************************************
145900990422     C     RIES1         BEGSR
146000990422     C*
146100990422     C                   CLEAR                   FMTS1
146200990422     C*
146300990422     C* riempio i campi del sfl, compresi gli hidden con i valori
146400990422     C* del rcd letto
146500990422    >C     N77Cog        Cat       N77Nom:1      S1Descr
146600990422    >C                   MOVEL     N77KCC        S1KCC
146700990422    >C                   MOVEL     N77KSC        S1KSC
146800990423    >C                   MOVEL     N77Qua        S1Qua
146900990422    >C                   MOVEL     N77RGT        H1Rgt
147000990423    >C                   MOVEL     N77PRG        H1Prg
147100150626R186 C                   MoveL     N77C77        H1C77
147200990507    >C                   MOVEL     N77COG        H1COG
147300990507    >C                   MOVEL     N77NOM        H1NOM
147400150206R165 C                   If        N77CDF        <> *blanks
147500150206R165 C                   MOVEL     N77CDF        S1CDF
147600150630R186 C                   MOVEL     N77CDF        H1CDF
147700150630R165 C                   EndIf
147800150211R165 C                   If        N77CDE        <> *blanks
147900150211R165 C                   MOVEL     N77CDE        S1CDF
147901160202R211 C                   MOVEL     N77CDF        H1CDF
148000160202R211 X*** ex R186        Clear                   H1CDF
148100150206R165 C                   EndIf
148200990422     C*
148300990422     C* test esistenza commenti
148400990422    >C*  se non sono gestiti i commenti bisogna asteriscare
148500990422     C*                  EXSR      CTR724
148600990422     C*
148700990422     C* imposto quegli attributi del sfl che sono modificabili
148800990422     C* al richiamo del determinato pgm
148900990422     C                   EXSR      ATRS1
149000990422     C*
149100990422     C                   ENDSR
149200990422     C/EJECT
149300990422     C************************************************************
149400990422     C* REPERIMENTO COMMENTI SE PRESENTI
149500990422     C************************************************************
149600990422    >C*  se non sono gestiti i commenti bisogna asteriscare
149700990422     C*    CTR724        BEGSR
149800990422     C*
149900990422     C*                  RESET                   ANA724DS
150000990422     C*                  MOVEL     '01'          OPZ724
150100990422     C*                  MOVEL     XSCSOC        SOC724
150200990422    >C*                  MOVEL     *BLANK        UNI724
150300990422    >C*                  MOVEL     '___'         TPA724
150400990422     C* inserire chiavi commenti
150500990422    >C*                  MOVEL     _______       KY1724
150600990422    >C*                  MOVEL     _______       KY2724
150700990422    >C*                  MOVEL     _______       KY3724
150800990422    >C*                  MOVEL     _______       KY4724
150900990422    >C* a seconda dell'analisi inserire una data differente da oggi
151000990422    >C* per il confronto con le date scadenza dei commenti
151100990422     C*                  MOVEL     UDate         DTS724
151200990422     C*                  MOVEL     ANA724DS      KPJBU
151300990422     C*                  CALL      'ANA724R'
151400990422     C*                  PARM                    KPJBA
151500990422     C*                  MOVEL     KPJBU         ANA724DS
151600990422     C*
151700990422     C* memorizzo la presenza o meno di commenti
151800990422     C*                  MOVE      EXI724        H1Commenti
151900990422     C*
152000990422     C*                  ENDSR
152100990422     C************************************************************
152200990422     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
152300990422     C************************************************************
152400990422     C     ATRS1         BEGSR
152500990422     C*
152600990422     C* in questa routine bisogna attivare indicatori o campi programma
152700990422     C* che gestiscono gli attributi di uno o più campi di questa videata;
152800990422     C* attenzione : non è questo il posto in cui eseguire controlli,
152900990422     C*  dovrebbero già arrivare dei flag
153000990422     C*
153100990422     C* presenza commento
153200990422     C*                  IF        H1Commenti = *on
153300990422     C*                  SETON                                        41
153400990422     C*                  ELSE
153500990422     C*                  SETOFF                                       41
153600990422     C*                  ENDIF
153700990422     C*
153800990422     C                   ENDSR
153900990422     C/EJECT
154000990422     C************************************************************
154100990422     C* GESTIONE F08 VIDEO S1
154200990422     C************************************************************
154300990422     C     F08S1         BEGSR
154400990422     C*
154500990422     C                   MOVE      *ON           $FINE
154600990422    >C                   MOVE      '0'           RETR75
154700990422     C*
154800990422     C                   ENDSR
154900990422     C/EJECT
155000990422     C************************************************************
155100990422     C* GESTIONE F03 VIDEO S1
155200990422     C************************************************************
155300990422     C     F03S1         BEGSR
155400990422     C*
155500990422     C                   MOVE      *ON           $FINE
155600990422    >C                   MOVE      '1'           RETR75
155700990422     C* fine programma
155800990422     C                   EXSR      ENDPGM
155900990422     C*
156000990422     C                   ENDSR
156100990422     C/EJECT
156200990422     C************************************************************
156300990422     C* GESTIONE F10 VIDEO S1
156400990422     C* AGGIUNTA RECORD
156500990422     C************************************************************
156600990422     C     F10S1         BEGSR
156700990422     C*
156800990422    >C                   RESET                   NDCR75DS
156900990422    >C                   MOVE      xscsoc        SocietaR75
157000990422    >C                   MOVEL     '01'          OPZR75
157100990422    >C     CMTR75        IFEQ      '1'
157200990422    >C                   MOVEL     '1'           CMTR75
157300990422     C                   ELSE
157400990422    >C                   MOVEL     '0'           CMTR75
157500990422     C                   ENDIF
157600990422     C* passare anche le parzializzazioni ricevute relative alla chiave
157700990422     C* univoca :
157800990422     C*  se ce ne sono, potranno essere usate come dft d'immissione
157900990422    >C                   IF        OPZR75 <> *Blank
158000990422    >C                   MOVEL     D1anno        AnnoR75
158100020419     C                   MOVEL     'AU'          QuaR75
158200990423    >C                   MOVEL     S1Kcc         KccR75
158300990423    >C                   MOVEL     S1Ksc         KscR75
158400990423    >C                   MOVEL     H1Rgt         RgtR75
158500990423    >C                   MOVEL     H1PRG         PrgR75
158600150626R186 C                   MoveL     H1C77         C77R75
158700990507    >C                   MOVEL     H1COG         CogR75
158800990507    >C                   MOVEL     H1NOM         NomR75
158900150626R186 C                   MoveL     S1CDF         CDFR75
159000990422     C                   ENDIF
159100990422     C* codice di riferimento per le valorizzazioni di default
159200990422    >C*****              MOVE      $DFn          DF1002
159300990422    >C                   MOVEL     NDCR75DS      KPJBU
159400990423    >C                   CALL      'NDCR75R1'
159500990422     C                   PARM                    KPJBA
159600990422    >C                   MOVEL     KPJBU         NDCR75DS
159700990422     C* ritorno da PGM gestione
159800990422    >C                   EXSR      RTN002
159900990422     C     WINZS1        IFEQ      *ON
160000990422     C                   MOVE      *ON           $INZS1
160100990422     C* se esistevano già righe sul sfl
160200990422     C* calcolo pagina a cui deve posizionarsi
160300990422     C     WSFL          IFGT      0
160400990422     C                   EXSR      CLCPAG
160500990422     C* altrimenti carico solo la 1a pagina
160600990422     C                   ELSE
160700990422     C                   Z-ADD     SFLPAG        WPAG
160800990422     C                   ENDIF
160900990422     C                   ENDIF
161000990422     C*
161100990422     C                   MOVE      *HIVAL        $LASTV
161200990422     C*
161300990422     C                   ENDSR
161400990422     C/EJECT
161500990422     C************************************************************
161600990422     C* GESTIONE F12 VIDEO S1
161700990422     C************************************************************
161800990422     C     F12S1         BEGSR
161900990422     C*
162000990422     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
162100990422     C     $LOP          IFEQ      1
162200990422     C                   MOVE      'D1'          $GEST
162300990422     C*  se la 1a videata è questa, per F12 torno a menu
162400990422     C                   ELSE
162500990422     C                   MOVE      *ON           $FINE
162600990422    >C                   MOVE      '2'           RETR75
162700990422     C                   ENDIF
162800990422     C*
162900990422     C                   ENDSR
163000990422     C/EJECT
163100990422     C************************************************************
163200990422     C* GESTIONE F17 VIDEO S1
163300990422     C************************************************************
163400990422     C     F17S1         BEGSR
163500990422     C*
163600990422     C                   MOVE      'D1'          $GEST
163700990422     C* se il pgm è stato richiamato con chiave univoca
163800990422     C* e dopo è stata richiesta una parzializzazione
163900990422     C* tolgo la gestione della chiave univoca (azzere/abblenco il
164000990422     C* valore; propongo come ordinamento il 1°)
164100990422     C     $Unique       IFEQ      *ON
164200990422     C                   MOVE      *OFF          $Unique
164300990422    >C***                MOVE      *BLANK        UQ1001
164400990422     C                   ENDIF
164500990422     C     $ORD          IFEQ      99
164600990422     C                   Z-ADD     1             $ORD
164700990422     C                   MOVE      *ON           $INZD1
164800990422     C                   ENDIF
164900990422     C*
165000990422     C                   ENDSR
165100990422     C/EJECT
165200990422     C************************************************************
165300990422     C* GESTIONE F18 VIDEO S1
165400990422     C************************************************************
165500990422     C     F18S1         BEGSR
165600990422     C*
165700990422     C                   MOVE      'D2'          $GEST
165800990422     C                   MOVEL     *ON           $INZD2
165900990422     C     $ORD          IFEQ      99
166000990422     C                   Z-ADD     1             $ORD
166100990422     C                   ENDIF
166200990422     C*
166300990422     C                   ENDSR
166400990422     C/EJECT
166500990422     C************************************************************
166600990422     C* GESTIONE F23 VIDEO S1
166700990422     C************************************************************
166800990422     C     F23S1         BEGSR
166900990422     C*
167000990422     C                   Z-ADD     $C1Op1        $L01
167100990422     C                   Z-ADD     $C1Op2        $L02
167200990422     C                   EXSR      RIEOPZ
167300990422     C                   Z-ADD     23            X
167400990422     C                   MOVE      FF23          $FC2(X)
167500990422     C                   MOVEL     WOPZ1         C1OPZ1
167600060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
167700990422     C                   MOVEL     WOPZ2         C1OPZ2
167800990422     C                   Z-ADD     WCOZ          C1COZ
167900990422     C*
168000990422     C                   ENDSR
168100990422     C/EJECT
168200990422     C************************************************************
168300990422     C* GESTIONE F24 VIDEO S1
168400990422     C************************************************************
168500990422     C     F24S1         BEGSR
168600990422     C*
168700990422     C                   MOVEA     $FX2          $FX
168800990422     C                   MOVEA     $FC2          $FC
168900990422     C                   MOVEA     $FR2          $FR
169000990422     C                   Z-ADD     $ULKS1        $ULK
169100990422     C                   Z-ADD     $S1L01        $L01
169200990422     C                   Z-ADD     $S1L02        $L02
169300990422     C                   EXSR      RIEKEY
169400990422     C                   Z-ADD     $ULK          $ULKS1            3 0
169500990422     C                   MOVEA     $FC           $FC2
169600990422     C                   MOVEL     $KEY1         Z2KE1
169700060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
169800990422     C                   MOVEL     $KEY2         Z2KE2
169900990422     C                   WRITE     FMTZ2
170000990422     C*
170100990422     C                   ENDSR
170200990422     C/EJECT
170300990422     C************************************************************
170400990422     C* CONTROLLO TESTATA LISTA
170500990422     C************************************************************
170600990422     C     CTRC1         BEGSR
170700990422     C*
170800990422     C                   MOVE      *OFF          *IN99
170900990422     C*
171000990422     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
171100990422     C* L'INIZIALIZZAZIONE LISTA
171200990422     C*
171300990422     C     C1POS         IFNE      W1POS
171400990422    >C* aggiungere in OR altri campi posizionamento se ci sono
171500990422     C                   EXSR      POSS1
171600990422     C                   ENDIF
171700990422     C*
171800990422     C                   ENDSR
171900990422     C/EJECT
172000990422     C************************************************************
172100990422     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
172200990422     C************************************************************
172300990422     C     POSS1         BEGSR
172400990422     C*
172500990422     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
172600990422     C*
172700990422   B1C     WMAX          IFGT      0
172800990422     C                   READC     FMTS1                                  21
172900990422   B2C     *IN21         DOWEQ     *OFF
173000990422     C     *IN99         ANDEQ     *OFF
173100990422     C*
173200990422   B3C     S1OPZ         IFNE      0
173300990422     C                   SETON                                        92  99
173400990422   E3C                   ENDIF
173500990422     C* se rilevata opzione attiva, attivo sflnxtchg
173600990422   B3C     *IN99         IFEQ      *ON
173700990422     C                   MOVE      *ON           *IN32
173800990422     C*
173900990422     C* imposto quegli attributi del sfl che sono modificabili
174000990422     C* al richiamo del determinato pgm
174100990422     C                   EXSR      ATRS1
174200990422     C*
174300990422     C                   UPDATE    FMTS1
174400990422   X3C                   ELSE
174500990422     C                   READC     FMTS1                                  21
174600990422   E3C                   ENDIF
174700990422     C*
174800990422   E2C                   ENDDO
174900990422     C*
175000990422   E1C                   ENDIF
175100990422     C*
175200990422     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
175300990422     C* AL VALORE DI POSIZIONAMENTO :
175400990422     C* se questo è maggiore del valore del rispettivo campo di
175500990422     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
175600990422     C* con i valori di parzializzazione.
175700990422     C*
175800990422   B1C     *IN99         IFEQ      *OFF
175900990422    >C                   MOVE      *blanks       W1POS
176000990422     C*
176100990422     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
176200990422     C* è maggiore del relativo valore di parzializzazione, nel qual caso
176300990422     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
176400990422     C* richiedo comunque un ricaricamento del sfl che, se non si è
176500990422     C* verificata la condizione suddetta avverrà con posizionamento a
176600990422     C* partire dal valore di parzializzazione
176700990422     C*
176800990422   B2C                   SELECT
176900990422    >C* Attenzione : se un ordinamento ha un campo di posizionamento
177000990422    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
177100990422    >C* dei relativi campi
177200990422     C*
177300990422     C     $ORD          WHENEQ    1
177400990422    >C     C1POS         IFGT      *blanks
177500990422    >C                   MOVEL     C1POS         W1POS
177600990422    >C* se uno di questi codici prevede l'uso di XALLINEA
177700990422    >C* bisogna metterlo qui
177800990422    >C*                  CALLB     'XALLINEA'
177900990422    >C*                  PARM                    W1Pos
178000990422    >C*                  PARM      8             LEN               5 0
178100990422    >C*                  PARM                    XSCINP
178200990422     C                   ENDIF
178300990422     C*
178400990422    >C*    $ORD          WHENEQ    2
178500990422    >C*    C1POS         IFGT      D1Descr
178600990422    >C*                  MOVE      C1POS         W1POS
178700990422    >C*                  ENDIF
178800990422     C*
178900990422    >C*    $ORD          WHENEQ    3
179000990422    >C*    C1POS         IFGT      D1Banca
179100990422    >C*                  MOVE      C1POS         W1POS
179200990422    >C*                  ENDIF
179300990422     C*
179400990422     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
179500990422     C*
179600990422   E2C                   ENDSL
179700990422     C*
179800990422     C* richiedo l'inizializzazione sfl
179900990422     C                   MOVE      *ON           $INZS1
180000990422     C*
180100990422   E1C                   ENDIF
180200990422     C*
180300990422     C                   ENDSR
180400990422     C/EJECT
180500990422     C************************************************************
180600990422     C* CONTROLLO OPZIONI LISTA
180700990422     C************************************************************
180800990422     C     CTRS1         BEGSR
180900990422     C*
181000990422     C                   MOVEL     *OFF          $ESCI
181100990422     C                   SETOFF                                       99
181200990422     C                   Z-ADD     0             S1OPZ
181300990422     C*
181400990422     C* Leggo il sfl solo se ci sono rcd
181500990422   B1C     WMAX          IFGT      0
181600990422     C                   READC     FMTS1                                  21
181700990422     C*
181800990422     C* esce se fine sfl o errore che richiede l'uscita
181900990422   B2C     *IN21         DOWEQ     *OFF
182000990422     C     $ESCI         ANDEQ     *OFF
182100990422     C                   Z-ADD     S1NRR         C1RCD
182200990422     C* ctrl su riga
182300990422     C                   EXSR      RECS1
182400990422     C* gestione opzioni
182500990422   B3C     S1OPZ         IFNE      0
182600990422     C     *IN99         ANDEQ     *OFF
182700990422     C                   EXSR      OPZS1
182800990422   E3C                   ENDIF
182900990422     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
183000990422   B3C     *IN99         IFEQ      *ON
183100990422     C                   MOVE      *ON           *IN32
183200990422   E3C                   ENDIF
183300990422     C*
183400990422     C* imposto quegli attributi del sfl che sono modificabili
183500990422     C* al richiamo del determinato pgm
183600990422     C                   EXSR      ATRS1
183700990422     C*
183800990422     C                   Z-ADD     0             S1OPZ
183900990422     C*
184000990422     C                   UPDATE    FMTS1
184100990422     C                   MOVE      *OFF          *IN32
184200990422     C*
184300990422     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
184400990422   B3C     $ESCI         IFEQ      *OFF
184500990422     C                   READC     FMTS1                                  21
184600990422     C* a fine ciclo ripristino il flag richiesta iniz.sfl
184700990422   B4C     *IN21         IFEQ      *ON
184800990422     C                   MOVE      WINZS1        $INZS1
184900990422     C* calcolo pagina a cui deve posizionarsi
185000990422     C     WSFL          IFGT      0
185100990422     C                   EXSR      CLCPAG
185200990422     C                   ELSE
185300990422     C                   Z-ADD     SFLPAG        WPAG
185400990422     C                   ENDIF
185500990422   E4C                   ENDIF
185600990422   E4C                   ENDIF
185700990422     C*
185800990422   E2C                   ENDDO
185900990422     C*
186000990422   E1C                   ENDIF
186100990422     C*
186200990422     C                   ENDSR
186300990422     C/EJECT
186400990422     C************************************************************
186500990422     C* CONTROLLO CAMPI I/O RIGA LISTA
186600990422     C************************************************************
186700990422     C     RECS1         BEGSR
186800990422     C*
186900990422     C* reset indicatori DSPATR
187000990422     C                   MOVE      *ALL'0'       IN4049           10
187100990422     C                   MOVEA     IN4049        *IN(40)
187200990422     C*
187300990422     C* Controllo se Utente Autorizzato alla scelta
187400990422     C     S1OPZ         IFNE      0
187500990422     C                   Z-ADD     1             X
187600990422     C     S1OPZ         LOOKUP    $OP1(X)                                21
187700990422     C* mi salvo l'indice dell' opzione
187800990422     C   21              Z-add     X             PosOpz            2 0
187900990422     C     *IN21         IFEQ      *OFF
188000990422     C     $FL1(X)       OREQ      'N'
188100990422     C                   MOVE      *ON           $ESCI
188200990422     C                   SETON                                        409199
188300990422     C                   ENDIF
188400990422     C                   ENDIF
188500990422     C*
188600990422     C                   ENDSR
188700990422     C/EJECT
188800990422     C************************************************************
188900990422     C* GESTIONE OPZIONI LISTA
189000990422     C************************************************************
189100990422     C     OPZS1         BEGSR
189200990422     C*
189300990422     C                   Z-ADD     RIRI          CURR
189400990422     C                   Z-ADD     COCO          CURC
189500990422     C*
189600990422     C                   SELECT
189700990422     C*
189800990422     C* se opz=Selezione
189900990422     C* passo la chiave al pgm chiamante
190000990422     C     S1OPZ         WHENEQ    1
190100990422    >C                   MOVE      S1OPZ         OPZR75
190200990422     C* passare nei campi appositi della DS NDCr75DS il valore
190300990422     C* della chiave della VL univoca (se c'è)
190400990422    >C                   MOVE      D1Anno        AnnoR75
190500020419    >C                   MOVE      'AU'          QuaR75
190600990422     C                   MOVE      *ON           $ESCI
190700990422     C                   MOVE      *ON           $FINE
190800990422     C*
190900990422     C                   MOVE      *HIVAL        $LASTV
191000990422     C* se opz=Invia
191100990422     C* scrive nella posta
191200990422     C*    S1OPZ         WHENEQ    10
191300990422     C*                  RESET                   ANA727DS
191400990422     C*                  MOVE      '01'          OPZ727
191500990422    >C*                  MOVEL     ______        TXT727                         Testo
191600990422    >C*                  MOVEL     '___'         TPA727
191700990422    >C*                  MOVEL     ______        KY1727
191800990422    >C*                  MOVEL     ______        KY2727
191900990422    >C*                  MOVEL     ______        KY3727
192000990422    >C*                  MOVEL     ______        KY4727
192100990422     C*                  MOVE      'E'           PGM727
192200990422     C*                  MOVE      XSCSOC        SOC727
192300990422     C*                  MOVE      WExf          INZ727
192400990422     C*                  CLEAR                   KPJBU
192500990422     C*                  MOVEL     ANA727DS      KPJBU
192600990422     C*                  CALL      'ANA727R'
192700990422     C*                  PARM                    KPJBA
192800990422     C*                  MOVEL     KPJBU         ANA727DS
192900990422     C*
193000990422     C* Spengo il flag che mi avverte dell'EXFMT fatta dal video di questo
193100990422     C* pgm, in modo che, se non viene eseguita e si richiama il pgm di
193200990422     C* sola finestra, questo non esegua il refresh dello sfondo
193300990422     C*                  MOVE      '0'           WExf
193400990422     C*
193500990422     c*                  EXSR      RTN727
193600990422     C*
193700990422     C*                  MOVE      *HIVAL        $LASTV
193800990422     C*
193900990422     C* se opz=Commenti
194000990422     C* richiamo il pgm di gestione
194100990422    >c*  se non sono gestiti i commenti bisogna asteriscare
194200990422     C*    S1OPZ         WHENEQ    14
194300990422     C*                  RESET                   ANA725DS
194400990422    >C*    OPZR75        IFEQ      '04'
194500990422    >C*    OPZR75        OREQ      '05'
194600990422    >C* oppure se l'utente è abilitato per la funzione specifica di
194700990422    >C* questa anagrafica solo alla visualizzazione
194800990422     C*                  MOVE      '05'          OPZ725
194900990422     C*                  ELSE
195000990422     C*                  MOVE      '02'          OPZ725
195100990422     C*                  ENDIF
195200990422    >C*                  MOVE      '___'         TPA725
195300990422     C* inserire chiavi commenti
195400990422    >C*                  MOVEL     ______        KY1725
195500990422    >C*                  MOVEL     ______        KY2725
195600990422    >C*                  MOVEL     ______        KY3725
195700990422    >C*                  MOVEL     ______        KY4725
195800990422     C*                  MOVE      *BLANK        KPJBU
195900990422     C*                  MOVEL     ANA725DS      KPJBU
196000990422     C*                  CALL      'ANA725R'
196100990422     C*                  PARM                    KPJBA
196200990422     C*                  MOVEL     KPJBU         ANA725DS
196300990422     C*
196400990422     C* ritorno da PGM commenti
196500990422     C*                  EXSR      RTN725
196600990422     C*
196700990422     C*                  MOVE      *HIVAL        $LASTV
196800990422     C*
196900990422     C* se selezionata opzione personalizzata, richiamo il programma
197000990422     C* corrispondente
197100990422     C     $PGO1(PosOpz) Whenne    *Blank
197200990422    >C                   RESET                   NDCR75DS
197300990422    >C                   MOVE      xscsoc        SocietaR75
197400990422    >C                   MOVEL     S1OPZ         OPZR75
197500990422    >C     CMTR75        IFEQ      '1'
197600990422    >C                   MOVEL     '1'           CMTR75
197700990422     C                   ELSE
197800990422    >C                   MOVEL     '0'           CMTR75
197900990422     C                   ENDIF
198000990422     C* passare nei campi appositi della DS NDCR75DS il valore
198100990422     C* della chiave della VL univoca
198200990422    >C                   MOVEL     D1anno        AnnoR75
198300020419    >C                   MOVEL     'AU'          QuaR75
198400990422     C*
198500990422    >C                   MOVEL     NDCR75DS      KPJBU
198600990422     C                   CALL      $PGO1(PosOpz)                        22
198700990422     C                   PARM                    KPJBA
198800990422     C                   PARM                    NOMPGM
198900990422     C*
199000990422    >C                   MOVEL     KPJBU         NDCR75DS
199100990422     C* ritorno da PGM
199200990422     C                   EXSR      RTN002
199300990422      *
199400990422      *
199500990422      * se altre opzioni
199600990422      * richiamo pgm gestione anagrafica
199700990422     C                   OTHER
199800990422     C*
199900990422    >C                   RESET                   NDCR75DS
200000990422    >C                   MOVE      xscsoc        SocietaR75
200100990422    >C                   MOVEL     S1OPZ         OPZR75
200200990422    >C     CMTR75        IFEQ      '1'
200300990422    >C                   MOVEL     '1'           CMTR75
200400990422     C                   ELSE
200500990422    >C                   MOVEL     '0'           CMTR75
200600990422     C                   ENDIF
200700990422     C* passare nei campi appositi della DS NDCR75DS il valore
200800990422     C* della chiave della VL univoca
200900990422    >C                   MOVEL     D1Anno        AnnoR75
201000020419    >C                   MOVEL     'AU'          QuaR75
201100990422    >C                   MOVEL     S1Kcc         KccR75
201200990422    >C                   MOVEL     S1Ksc         KscR75
201300990422    >C                   MOVEL     H1RGT         RGTR75
201400990423    >C                   MOVEL     H1PRG         PRGR75
201500150626R186 C                   MoveL     H1C77         C77R75
201600990507    >C                   MOVEL     H1Cog         COGR75
201700990507    >C                   MOVEL     H1Nom         NOMR75
201800150630R186 C                   MoveL     H1CDF         CDFR75
201900990422     C*
202000990422    >C                   MOVEL     NDCR75DS      KPJBU
202100990423    >C                   CALL      'NDCR75R1'
202200990422     C                   PARM                    KPJBA
202300990422    >C** se vengono gestite le autorizzazioni
202400990422    >C** le passo al pgm di manutenzione per evitargli di doversele
202500990422    >C** riprendere
202600990422    >C*                  PARM                    AUTDS
202700990422     C*
202800990422    >C                   MOVEL     KPJBU         NDCR75DS
202900990422     C*
203000990422     C* ritorno da PGM gestione
203100990422    >C                   EXSR      RTN002
203200990422     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
203300990422     C     *IN99         IFEQ      *ON
203400990422     C                   SETON                                        40
203500990422     C                   ENDIF
203600990422     C*
203700990422     C                   MOVE      *HIVAL        $LASTV
203800990422     C                   ENDSL
203900990422     C*
204000990422     C                   ENDSR
204100990422     C/EJECT
204200990422     C************************************************************
204300990422     C* GESTIONE RITORNO DA trasmisisone anagrafica factor/debitore
204400990422     C************************************************************
204500990422    >C     RTNU30        BEGSR
204600990422     C*
204700990422     C* modo di ritorno
204800990422     C*
204900990422     C                   SELECT
205000990422     C* 1 = errori riscontrati durante l'estrazione debitori
205100990422    >C     ERRR75        WHENEQ    '1'
205200990422     C                   SETON                                        5399      emiss.SFLMSGID
205300990422     C* 0 = funzione richiamata andata a buon fine
205400990422    >C     ERRR75        WHENEQ    '0'
205500990422    >C*  eventualmente gestire altri codici di errore
205600990422     C*
205700990422     C                   ENDSL
205800990422     C*
205900990422     C                   ENDSR
206000990422     C************************************************************
206100990422     C* GESTIONE RITORNO DA PGM DI GESTIONE
206200990422     C************************************************************
206300990422    >C     RTN002        BEGSR
206400990422     C*
206500990422     C* modo di ritorno
206600990422     C*
206700990422     C                   SELECT
206800990422     C* questi modi di utilizzo dei valori di ritorno dal
206900990422     C* pgm di manutenzione rcd di anagrafica sono delle
207000990422     C* proposte, normalmente sempre valide, ma modificabili
207100990422     C* per situazioni particolari
207200990422     C* 1 = F3
207300990422    >C     RETR75        WHENEQ    '1'
207400990422     C                   MOVE      *ON           $ESCI                          sospese READC
207500990422     C                   MOVE      *ON           $FINE                          fine pgm
207600990422     C                   MOVE      '1'           RETR75
207700990422     C* 2 = F12
207800990422    >C     RETR75        WHENEQ    '2'
207900990422     C* se il pgm di manutenzione ha prima una guida, è necessario anche
208000990422     c* questo test, altrimenti prevale il ret002 e il sfl non viene
208100990422     C* ricaricato
208200990422     C     OPRR75        ANDNE     '1'
208300990422     C                   MOVE      *ON           $ESCI                          sospese READC
208400990422     C*
208500990422     C                   ENDSL
208600990422     C*
208700990422     C* operazione eseguite dal pgm chiamato
208800990422     C*
208900990422    >C                   MOVE      OPRR75        WOPR                           avverto pgm chiamant
209000990422     C                   SELECT
209100990422     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
209200990422    >C     OPRR75        WHENEQ    '1'
209300990422     C*
209400990422     C* se l'opzione con cui è stato chiamato non è la modifica
209500990422     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
209600990422    >C     OPZR75        IFNE      '02'
209700990422     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
209800990422     C* se l'opzione con cui è stato chiamato è la modifica
209900990422     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
210000990422     C                   ELSE
210100990422     C                   EXSR      AGGS1
210200990422     C                   ENDIF
210300990422     C*
210400990422     C                   ENDSL
210500990422     C*
210600990422     C* funzione non eseguibile per errore :
210700990422     C*
210800990422     C                   SELECT
210900990422     C* 1 = funzione richiamata chiusa in errore
211000990422    >C     ERRR75        WHENEQ    '1'
211100990422     C                   MOVE      *ON           $ESCI                          sospese READC
211200990422     C                   SETON                                        9399      emiss.SFLMSGID
211300990422     C* 2 = funzione richiamata senza la dovuta autorizzazione
211400990422    >C     ERRR75        WHENEQ    '2'
211500990422     C                   MOVE      *ON           $ESCI                          sospese READC
211600990422     C                   SETON                                        9499      emiss.SFLMSGID
211700990422    >C*  eventualmente gestire altri codici di errore
211800990422     C*
211900990422     C                   ENDSL
212000990422     C*
212100990422     C                   ENDSR
212200990422     C/EJECT
212300990422     C************************************************************
212400990422     C* AGGIORNAMENTO RIGA LISTA
212500990422     C************************************************************
212600990422     C     AGGS1         BEGSR
212700990422     C*
212800990422     C* riempio i campi del sfl che sono stati ripassati dal pgm
212900990422     C* di manutenzione dopo una opzione 02
213000990422     C*
213100990422    >C****               MOVEL     AnnoR75       S1Anno
213200990422    >C                   MOVEL     QuaR75        S1Qua
213300990422    >C                   MOVEL     KCCR75        S1KCC
213400990422    >C                   MOVEL     KSCR75        S1KSC
213500990422    >C                   MOVEL     RGTR75        H1Rgt
213600990423    >C                   MOVEL     PRGR75        H1PRG
213700150626R186 C                   MoveL     C77R75        H1C77
213800990507    >C                   MOVEL     COGR75        H1COG
213900990507    >C                   MOVEL     NOMR75        H1NOM
214000150630R186 C                   MoveL     CDFR75        H1CDF
214100150630R186 C                   MoveL     CDFR75        S1CDF
214200150611R186 C     H1Cog         Cat       H1Nom:1       S1Descr
214300990422     C*
214400990422     C                   ENDSR
214500990422     C/EJECT
214600990422     C************************************************************
214700990422     C* GESTIONE RITORNO DA PGM DEI COMMENTI
214800990422     C************************************************************
214900990422    >C*  se non sono gestiti i commenti bisogna asteriscare
215000990422     C*    RTN725        BEGSR
215100990422     C*
215200990422     C* modo di ritorno
215300990422     C*
215400990422     C*                  SELECT
215500990422     C* questi modi di utilizzo dei valori di ritorno dal
215600990422     C* pgm di manutenzione rcd di anagrafica sono delle
215700990422     C* proposte, normalmente sempre valide, ma modificabili
215800990422     C* per situazioni particolari
215900990422     C* 1 = F3
216000990422     C*    RET725        WHENEQ    '1'
216100990422     C*                  MOVE      *ON           $ESCI                          sospese READC
216200990422     C* 2 = F12
216300990422     C*    RET725        WHENEQ    '2'
216400990422     c*    OPR725        ANDNE     '1'
216500990422     C*                  MOVE      *ON           $ESCI                          sospese READC
216600990422     C*
216700990422     C*                  ENDSL
216800990422     C*
216900990422     C* operazione eseguite dal pgm chiamato
217000990422     C*
217100990422     C*                  SELECT
217200990422     C* 1 = eseguita scrittura o modifica commento
217300990422     C*    OPR725        WHENEQ    '1'
217400990422     C* test esistenza commenti
217500990422    >C*  se non sono gestiti i commenti bisogna asteriscare
217600990422     C*                  EXSR      CTR724
217700990422     C*
217800990422     C*                  ENDSL
217900990422     C*
218000990422     C* funzione non eseguibile per errore :
218100990422     C*
218200990422     C*                  SELECT
218300990422     C* 1 = funzione richiamata chiusa in errore
218400990422     C*    ERR725        WHENEQ    '1'
218500990422     C*                  MOVE      *ON           $ESCI                          sospese READC
218600990422     C*                  SETON                                        9399      emiss.SFLMSGID
218700990422     C* 2 = funzione richiamata senza la dovuta autorizzazione
218800990422     C*    ERR725        WHENEQ    '2'
218900990422     C*                  MOVE      *ON           $ESCI                          sospese READC
219000990422     C*                  SETON                                        9499      emiss.SFLMSGID
219100990422    >C*  eventualmente gestire altri codici di errore
219200990422     C*
219300990422     C*                  ENDSL
219400990422     C*
219500990422     C*                  ENDSR
219600990422     C/EJECT
219700990422     C************************************************************
219800990422     C* GESTIONE RITORNO DA PGM INVIO
219900990422     C************************************************************
220000990422     C*    RTN727        BEGSR
220100990422     C*
220200990422     C* modo di ritorno
220300990422     C*
220400990422     C*                  SELECT
220500990422     C* questi modi di utilizzo dei valori di ritorno dal
220600990422     C* pgm di manutenzione rcd di anagrafica sono delle
220700990422     C* proposte, normalmente sempre valide, ma modificabili
220800990422     C* per situazioni particolari
220900990422     C* 1 = F3
221000990422     C*    RET727        WHENEQ    '1'
221100990422     C*                  MOVE      *ON           $ESCI                          sospese READC
221200990422     C* 2 = F12
221300990422     C*    RET727        WHENEQ    '2'
221400990422     c*    OPR727        ANDNE     '1'
221500990422     C*                  MOVE      *ON           $ESCI                          sospese READC
221600990422     C*
221700990422     C*                  ENDSL
221800990422     C*
221900990422     C* operazione eseguite dal pgm chiamato
222000990422     C*
222100990422     C*                  SELECT
222200990422     C* 1 = eseguito aggiornamento
222300990422     C*    OPR727        WHENEQ    '1'
222400990422     C*
222500990422     C*                  ENDSL
222600990422     C*
222700990422     C* funzione non eseguibile per errore :
222800990422     C*
222900990422     C*                  SELECT
223000990422     C* 1 = funzione richiamata chiusa in errore
223100990422     C*    ERR727        WHENEQ    '1'
223200990422     C*                  MOVE      *ON           $ESCI                          sospese READC
223300990422     C*                  SETON                                        9399      emiss.SFLMSGID
223400990422     C* 2 = funzione richiamata senza la dovuta autorizzazione
223500990422     C*    ERR727        WHENEQ    '2'
223600990422     C*                  MOVE      *ON           $ESCI                          sospese READC
223700990422     C*                  SETON                                        9499      emiss.SFLMSGID
223800990422     C*
223900990422     C*                  ENDSL
224000990422     C*
224100990422     C*                  ENDSR
224200990422     C/EJECT
224300990422     C************************************************************
224400990422     C* GESTIONE VALORI ASSUNTI
224500990422     C************************************************************
224600990422     C     GESD2         BEGSR
224700990422     C*
224800990422   B1C     $INZD2        IFEQ      *ON
224900990422     C                   EXSR      INZD2
225000990422     C                   MOVE      *OFF          $INZD2
225100990422   E1C                   ENDIF
225200990422     C*
225300990422   B1C     $LASTV        IFNE      'D2'
225400990422     C                   EXSR      GESPIE
225500990422   B1C     $LASTV        IFNE      *HIVAL
225600990422     C                   MOVE      $LASTV        $LASTG
225700990422     C                   ENDIF
225800990422     C                   MOVE      'D2'          $LASTV
225900990422   E1C                   ENDIF
226000990422     C*
226100990422     C* se esistono errori sulla videata
226200990422     C* emetto la write del formato a indicatori spenti per vedere
226300990422     C* le eventuali decodifiche
226400990422   B1C     *IN99         IFEQ      *ON
226500990422     C                   MOVEA     *IN           WIN
226600990422     C                   MOVE      *ALL'0'       IN5098           49
226700990422     C                   MOVEA     IN5098        *IN(50)
226800990422     C                   WRITE     FMTD2
226900990422     C                   MOVEA     WIN           *IN
227000990422   E1C                   ENDIF
227100990422     C*
227200990422     C                   EXFMT     FMTD2
227300990422     C*
227400990422     C* azzero la posizione del cursore
227500990422     C                   Z-ADD     0             H1RIGA
227600990422     C                   Z-ADD     0             H1COLO
227700990422     C*
227800990422     C* CONTROLLO ABILITAZIONE TASTI
227900990422     C* INPUT
228000990422     C* $FC3 = Abilitazioni videata D2
228100990422     C* OUTPUT
228200990422     C* *IN97 + *IN99 Errori
228300990422     C                   MOVEA     $FC3          $FC
228400990422     C                   EXSR      CTRKEY
228500990422     C*
228600990422   B1C     *IN99         IFEQ      *OFF
228700990422     C*
228800990422   B2C                   SELECT
228900990422     C* F3=Fine
229000990422     C     $TASTO        WHENEQ    F03
229100990422     C                   EXSR      F03D2
229200990422     C* F12=Ritorno
229300990422     C     $TASTO        WHENEQ    F12
229400990422     C                   EXSR      F12D2
229500990422     C* F24=Altri tasti
229600990422     C     $TASTO        WHENEQ    F24
229700990422     C                   EXSR      F24D2
229800990422     C*
229900990422     C                   OTHER
230000990422     C                   EXSR      CTRD2
230100990422   B3C     *IN99         IFEQ      *OFF
230200990422     C                   MOVE      $LASTG        $GEST
230300990422   B4C                   SELECT
230400990422     C     $LASTG        WHENEQ    'S1'
230500990422     C* se è stato modificato l'ordinamento di default, ricarico la lista
230600990422   B5C     $ORD          IFNE      D2ORD
230700990422     C                   MOVE      *ON           $INZS1
230800990422     C                   MOVE      *blanks       W1POS
230900990422   E5C                   ENDIF
231000990422     C     $LASTG        WHENEQ    'D1'
231100990422     C* per ora niente
231200990422   E4C                   ENDSL
231300990422     C* definisco i nuovi valori di default
231400990422     C                   Z-ADD     D2LOP         $LOP
231500990422     C                   Z-ADD     D2ORD         $ORD
231600990422    >C*****              MOVE      D2DFn         $DFn
231700990422    >C*  elenco altri dati default specifici dell'anagrafica
231800990422     C*
231900990422     C* aggiorno valori assunti
232000990422     C* se richiesto il salvataggio
232100990422   B4C     D2BSV         IFEQ      1
232200990422     C                   EXSR      AGGMPE
232300990422   E4C                   ENDIF
232400990422     C*
232500990422   E3C                   ENDIF
232600990422     C*
232700990422   E2C                   ENDSL
232800990422     C*
232900990422   E1C                   ENDIF
233000990422     C*
233100990422     C                   ENDSR
233200990422     C/EJECT
233300990422     C************************************************************
233400990422     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
233500990422     C************************************************************
233600990422     C     INZD2         BEGSR
233700990422     C*
233800990422     C                   CLEAR                   FMTD2
233900021014C1595C                   EVAL      D2BSVA = Normale
234000990422     C* se sono entrato con chiave univoca
234100990422     C* impedisco il salvataggio valori assunti
234200990422     C     $Unique       IFEQ      *ON
234300021014C1595X***                Z-ADD     2             D2BSV
234400021014C1595C                   EVAL      D2BSVA = Protetto
234500990422     C                   ENDIF
234600990422     C                   MOVEL     $LOP          D2LOP
234700990422     C                   MOVE      $ORD          D2ORD
234800990422    >C****               MOVE      $DFn          D2DFn
234900990422    >c*  elenco altri dati default specifici dell'anagrafica
235000990422     C*
235100990422     C                   ENDSR
235200990422     C/EJECT
235300990422     C************************************************************
235400990422     C* GESTIONE F03 VIDEO D2
235500990422     C************************************************************
235600990422     C     F03D2         BEGSR
235700990422     C*
235800990422     C                   MOVE      *ON           $FINE
235900990422     C* fine programma
236000990422     C                   EXSR      ENDPGM
236100990422     C*
236200990422     C                   ENDSR
236300990422     C/EJECT
236400990422     C************************************************************
236500990422     C* GESTIONE F12 VIDEO D2
236600990422     C************************************************************
236700990422     C     F12D2         BEGSR
236800990422     C*
236900990422     C                   MOVE      $LASTG        $GEST
237000990422     C*
237100990422     C                   ENDSR
237200990422     C/EJECT
237300990422     C************************************************************
237400990422     C* GESTIONE F24 VIDEO D2
237500990422     C************************************************************
237600990422     C     F24D2         BEGSR
237700990422     C*
237800990422     C                   MOVEA     $FX3          $FX
237900990422     C                   MOVEA     $FC3          $FC
238000990422     C                   MOVEA     $FR3          $FR
238100990422     C                   Z-ADD     $ULKD2        $ULK
238200990422     C                   Z-ADD     $D2L01        $L01
238300990422     C                   Z-ADD     $D2L02        $L02
238400990422     C                   EXSR      RIEKEY
238500990422     C                   Z-ADD     $ULK          $ULKD2            3 0
238600990422     C                   MOVEA     $FC           $FC3
238700990422     C                   MOVEL     $KEY1         Z3KE1
238800060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
238900990422     C                   WRITE     FMTZ3
239000990422     C*
239100990422     C                   ENDSR
239200990422     C/EJECT
239300990422     C************************************************************
239400990422     C* CONTROLLO VIDEATA VALORI ASSUNTI
239500990422     C************************************************************
239600990422     C     CTRD2         BEGSR
239700990422     C*
239800990422     C                   SETOFF                                       99
239900990422     C*
240000990422     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
240100990422     C     D2LOP         IFEQ      0
240200990422     C                   SETON                                          5099
240300990422     C                   ENDIF
240400990422     C*
240500990422     C* OBBLIGATORIO VALORE PER ORDINAMENTO
240600990422     C     D2ORD         IFEQ      0
240700990422     C                   SETON                                          5199
240800990422     C                   ENDIF
240900990422     C*
241000990422     C* CONTROLLO ESISTENZA CODICE PER I DEFAULT
241100990422     C***  D2DF1         IFNE      *BLANK
241200990422    >C*    D2DF1         SETLL     AN___01                                21
241300990422     C*    *IN21         IFEQ      *OFF
241400990422     C*                  SETON                                        5699
241500990422     C*                  ENDIF
241600990422    >C*  ATTENZIONE : se il file del codice per i default può essere
241700990422    >C*   lo stesso da cui viene caricato il sfl e, contemporaneamente,
241800990422    >C*   è la stessa vista logica usata nell'attuale ordinamento,
241900990422    >C*   bisogna riposizionarsi se no, al primo roll-up sbaglia il
242000990422    >C*   caricamento
242100990422     C* se ordinamento vigente è lo stesso della v.l. in uso per
242200990422     C* il ctrl del cd riferimento per i default
242300990422    >C*    $ORD          IFEQ      1
242400990422     C* e c'è almeno un rcd per il prossimo caricamento
242500990422     C*    $EndFile      ANDEQ     *OFF
242600990422     C* mi riposiziono
242700990422    >C*    K__ANA01      SETLL     AN___01                                21
242800990422     C*                  ENDIF
242900990422     C*                  ENDIF
243000990422    >C*  elenco controlli altri dati default specifici dell'anagrafica
243100990422     C*
243200990422     C                   ENDSR
243300990422     C/EJECT
243400990422     C************************************************************
243500990422     C* CONTROLLO ABILITAZIONE TASTI
243600990422     C* INPUT
243700990422     C* $TASTO =Tasto funzione premuto
243800990422     C* $FC = Tasti abilitati
243900990422     C* OUTPUT
244000990422     C* IN99 = ERrore
244100990422     C************************************************************
244200990422     C     CTRKEY        BEGSR
244300990422     C*
244400990422     C                   SETOFF                                       99
244500990422     C*
244600990422     C                   SELECT
244700990422     C     $TASTO        WHENEQ    F01
244800990422     C                   z-add     01            posfun
244900990422     C     $FC(1)        IFEQ      '0'
245000990422     C                   SETON                                        9997
245100990422     C                   ENDIF
245200990422     C     $TASTO        WHENEQ    F02
245300990422     C                   z-add     02            posfun
245400990422     C     $FC(2)        IFEQ      '0'
245500990422     C                   SETON                                        9997
245600990422     C                   ENDIF
245700990422     C     $TASTO        WHENEQ    F03
245800990422     C                   z-add     03            posfun
245900990422     C     $FC(3)        IFEQ      '0'
246000990422     C                   SETON                                        9997
246100990422     C                   ENDIF
246200990422     C     $TASTO        WHENEQ    F04
246300990422     C                   z-add     04            posfun
246400990422     C     $FC(4)        IFEQ      '0'
246500990422     C                   SETON                                        9997
246600990422     C                   ENDIF
246700990422     C     $TASTO        WHENEQ    F05
246800990422     C                   z-add     05            posfun
246900990422     C     $FC(5)        IFEQ      '0'
247000990422     C                   SETON                                        9997
247100990422     C                   ENDIF
247200990422     C     $TASTO        WHENEQ    F06
247300990422     C                   z-add     06            posfun
247400990422     C     $FC(6)        IFEQ      '0'
247500990422     C                   SETON                                        9997
247600990422     C                   ENDIF
247700990422     C     $TASTO        WHENEQ    F07
247800990422     C                   z-add     07            posfun
247900990422     C     $FC(7)        IFEQ      '0'
248000990422     C                   SETON                                        9997
248100990422     C                   ENDIF
248200990422     C     $TASTO        WHENEQ    F08
248300990422     C                   z-add     08            posfun
248400990422     C     $FC(8)        IFEQ      '0'
248500990422     C                   SETON                                        9997
248600990422     C                   ENDIF
248700990422     C     $TASTO        WHENEQ    F09
248800990422     C                   z-add     09            posfun
248900990422     C     $FC(09)       IFEQ      '0'
249000990422     C                   SETON                                        9997
249100990422     C                   ENDIF
249200990422     C     $TASTO        WHENEQ    F10
249300990422     C                   z-add     10            posfun
249400990422     C     $FC(10)       IFEQ      '0'
249500990422     C                   SETON                                        9997
249600990422     C                   ENDIF
249700990422     C     $TASTO        WHENEQ    F11
249800990422     C                   z-add     11            posfun
249900990422     C     $FC(11)       IFEQ      '0'
250000990422     C                   SETON                                        9997
250100990422     C                   ENDIF
250200990422     C     $TASTO        WHENEQ    F12
250300990422     C                   z-add     12            posfun
250400990422     C     $FC(12)       IFEQ      '0'
250500990422     C                   SETON                                        9997
250600990422     C                   ENDIF
250700990422     C     $TASTO        WHENEQ    F13
250800990422     C                   z-add     13            posfun
250900990422     C     $FC(13)       IFEQ      '0'
251000990422     C                   SETON                                        9997
251100990422     C                   ENDIF
251200990422     C     $TASTO        WHENEQ    F14
251300990422     C                   z-add     14            posfun
251400990422     C     $FC(14)       IFEQ      '0'
251500990422     C                   SETON                                        9997
251600990422     C                   ENDIF
251700990422     C     $TASTO        WHENEQ    F15
251800990422     C                   z-add     15            posfun
251900990422     C     $FC(15)       IFEQ      '0'
252000990422     C                   SETON                                        9997
252100990422     C                   ENDIF
252200990422     C     $TASTO        WHENEQ    F16
252300990422     C                   z-add     16            posfun
252400990422     C     $FC(16)       IFEQ      '0'
252500990422     C                   SETON                                        9997
252600990422     C                   ENDIF
252700990422     C     $TASTO        WHENEQ    F17
252800990422     C                   z-add     17            posfun
252900990422     C     $FC(17)       IFEQ      '0'
253000990422     C                   SETON                                        9997
253100990422     C                   ENDIF
253200990422     C     $TASTO        WHENEQ    F18
253300990422     C                   z-add     18            posfun
253400990422     C     $FC(18)       IFEQ      '0'
253500990422     C                   SETON                                        9997
253600990422     C                   ENDIF
253700990422     C     $TASTO        WHENEQ    F19
253800990422     C                   z-add     19            posfun
253900990422     C     $FC(19)       IFEQ      '0'
254000990422     C                   SETON                                        9997
254100990422     C                   ENDIF
254200990422     C     $TASTO        WHENEQ    F20
254300990422     C                   z-add     20            posfun
254400990422     C     $FC(20)       IFEQ      '0'
254500990422     C                   SETON                                        9997
254600990422     C                   ENDIF
254700990422     C     $TASTO        WHENEQ    F21
254800990422     C                   z-add     21            posfun
254900990422     C     $FC(21)       IFEQ      '0'
255000990422     C                   SETON                                        9997
255100990422     C                   ENDIF
255200990422     C     $TASTO        WHENEQ    F22
255300990422     C                   z-add     22            posfun
255400990422     C     $FC(22)       IFEQ      '0'
255500990422     C                   SETON                                        9997
255600990422     C                   ENDIF
255700990422     C     $TASTO        WHENEQ    F23
255800990422     C                   z-add     23            posfun
255900990422     C     $FC(23)       IFEQ      '0'
256000990422     C                   SETON                                        9997
256100990422     C                   ENDIF
256200990422     C     $TASTO        WHENEQ    F24
256300990422     C                   z-add     24            posfun
256400990422     C     $FC(24)       IFEQ      '0'
256500990422     C                   SETON                                        9997
256600990422     C                   ENDIF
256700990422     C                   ENDSL
256800990422     C*
256900990422     C                   ENDSR
257000990422     C/EJECT
257100990422     C************************************************************
257200990422     C* AGGIORNAMENTO VALORI ASSUNTI
257300990422     C************************************************************
257400990422     C     AGGMPE        BEGSR
257500990422     C*
257600990422     C* preparo i parametri di chiamata al driver
257700990422     C                   CLEAR                   XMPEDS
257800990422     C                   EVAL      TpChiamata = '03'
257900990422     C                   EVAL      NChiavi = 5
258000990422     C                   EVAL      VLogica = 2
258100990422     C*
258200990422     C* preparo le chiavi per la chiamata al driver
258300990422     C                   CLEAR                   XMPEDS1
258400990422     C                   MOVEL     XSCSOC        DSMPESOC
258500990422     C                   MOVEL     DSPGM         DSMPEPGM
258600990422     C                   MOVEL     KNMUS         DSMPEPRF
258700990422     C*
258800990422     C* Si memorizzano i dati di default a seconda delle modalità con
258900990422     C* cui il pgm è usato
259000990422     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
259100990422     C* MPETIP = 'PV'  pgm richiamato da menù
259200990422     C* MPETIP = 'PB'  x lancio Batch
259300990422     C*
259400990422     C                   SELECT
259500990422    >C     OPZR75        WHENEQ    '01'
259600990422     C                   MOVEL     'PX'          DSMPETIP
259700990422     C                   OTHER
259800990422     C                   MOVEL     'PV'          DSMPETIP
259900990422     C                   ENDSL
260000990422     C* dati
260100990422     C                   MOVEL     DSMPE         DSMPEPAR
260200990422     C*
260300990422     C                   CALLB     'XMPE'
260400990422     C                   PARM                    XMPEDS
260500990422     C                   PARM                    XMPEDS1
260600990422     C*
260700990422     C                   ENDSR
260800990422     C/EJECT
260900990422     C************************************************************
261000990422     C* OPERAZIONI INIZIALI
261100990422     C************************************************************
261200990422     C     *INZSR        BEGSR
261300990422     C*
261400990422     C* Reperimento parametri
261500990422     C*
261600990422     C     *ENTRY        PLIST
261700990422     C                   PARM                    KPJBA
261800990422     C                   PARM                    DSAUT
261900990422     C*
262000990422     C* testo se sono stati passati i dati di autorizzazione
262100990422     C*
262200990422     C     PARMS         IFLT      2
262300990422     C                   MOVE      *BLANK        AUTDS
262400990422     C                   ELSE
262500990422     C                   MOVEL     DSAUT         AUTDS
262600990422     C                   ENDIF
262700990422     C*
262800990422     C* Reperimento dati società
262900990422     C*
263000990422     C                   MOVEL     'SOC001'      TIPXSC
263100990422     C                   MOVEL     *BLANK        SOCXSC
263200990422     C                   EXSR      REPSOC
263300990422     C*
263400990422     C* Controlli dati società
263500990422     C*
263600990422     C     RTNXSC        IFNE      '1'
263700990422     C                   MOVEL     XSOCDS        SOC001
263800990422     C                   ELSE
263900990422     C                   MOVE      *ON           $FINE
264000990422    >C                   MOVE      '1'           ERRR75
264100990422     C                   EXSR      ENDPGM
264200990422     C                   ENDIF
264300990422     C*
264400990422     C* reperisco le dimensioni delle variabili che conterranno i tasti
264500990422     C                   EVAL      $D1L01 = %Size(Z1Ke1)
264600990422     C                   EVAL      $D1L02 = %Size(Z1Ke2)
264700990422     C                   EVAL      $S1L01 = %Size(Z2Ke1)
264800990422     C                   EVAL      $S1L02 = %Size(Z2Ke2)
264900990422     C                   EVAL      $C1Op1 = %Size(C1Opz1)
265000990422     C                   EVAL      $C1Op2 = %Size(C1Opz2)
265100990422     C                   EVAL      $D2L01 = %Size(Z3Ke1)
265200990422     C                   EVAL      $D2L02 = 0
265300990422    >C* aggiungere altre variabili nel caso di videate indipendenti
265400990422    >C* ulteriori a quelle std
265500990422     C*
265600990422     C* Reperimento opzioni
265700990422     C*
265800990422     C                   EXSR      REPOPZ
265900990422     C*
266000990422     C* Reperimento tasti di funzione
266100990422     C*
266200990422     C                   EXSR      REPKEY
266300990422     C* Chiamata a routine per il reperimento delle opzioni
266400990422     C* personalizzate
266500990422    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
266600990422    >C* ripeterla passando le schiere dei tasti funzione
266700990422     C                   CAllb     'X46USREXT'
266800990422     C                   PARM                    $OP1
266900990422     C                   PARM                    $TX1
267000990422     C                   PARM                    $FL1
267100990422     C                   PARM                    $PGO1
267200990422     C                   PARM      $dimOp        NumOpz
267300990422     C                   PARM                    $FX2
267400990422     C                   PARM                    $FC2
267500990422     C                   PARM                    $PGF2
267600990422     C                   PARM                    DsPgm
267700990422     C                   PARM                    xsccut
267800990422     C                   PARM                    xscgrp
267900990422     C*
268000990422     C                   Z-add     1             OPZLEN
268100990422     C     *blank        Lookup    $TX1(opzlen)                           21
268200990422     C                   if        *in21 = *on
268300990422     C                   sub       1             OPZLEN
268400990422     C                   else
268500990422     C                   Z-add     $DimOp        OPZLEN
268600990422     C                   endif
268700990422     C*
268800990422     C                   ENDSR
268900990422     C/EJECT
269000990422     C************************************************************
269100990422     C* INIZIALIZZAZIONE VARIABILI
269200990422     C************************************************************
269300990422     C     INZVAR        BEGSR
269400990422     C*
269500990422     C* Dati da PGM chiamante
269600990422     C*
269700990422     C                   MOVEL     KPJBU         NDCr75DS
269800990422     C*
269900990422     C* Se società diversa da quella impostata nella *inzsr, rieseguo
270000990422     C* reperimento dati società
270100990422    >C                   IF        SocietaR75 <> xscsoc  and
270200990422    >C                             OPZR75 <>*blank
270300990422     C                   MOVEL     'SOC001'      TIPXSC
270400990422    >C                   MOVEL     SocietaR75    SOCXSC
270500990422     C                   EXSR      REPSOC
270600990422     C     RTNXSC        IFNE      '1'
270700990422     C                   MOVEL     XSOCDS        SOC001
270800990422     C                   ELSE
270900990422     C                   MOVE      *ON           $FINE
271000990422    >C                   MOVE      '1'           ERRR75
271100990422     C                   EXSR      ENDPGM
271200990422     C                   ENDIF
271300990422     C                   ENDIF
271400990422     C*
271500990422     C* Reperimento autorizzazioni
271600990422     C*
271700990422    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
271800990422    >C*   asteriscare la chiamata alla subroutine REPAUT
271900990422     C* le autorizzazioni potrebbero essere già state passate,
272000990422     C* nel qual caso non vanno reperite
272100990422    >C     AUTDS         IFEQ      *BLANK
272200990422    >C****               EXSR      REPAUT
272300990422    >C                   ELSE
272400990422    >C*****              MOVEL     XCADDS
272500990422    >C                   ENDIF
272600990422    >C*                  ENDIF
272700990422     C* Reperimento valori default utente
272800990422     C*
272900990422     C                   EXSR      REPDFT
273000990422     C*
273100990422     C* Definizione delle opzioni utilizzabili a pgm
273200990422     C*
273300990422     C                   EXSR      DEFOPZ
273400990422     C*
273500990422     C* Variabili per gestione videate
273600990422     C*
273700990422     C                   MOVE      *OFF          $FINE                          fine pgm
273800990422     C                   MOVE      *OFF          $INZD1                         inizializzione D1
273900990422     C                   MOVE      *OFF          $INZS1                         inizializzione S1
274000990422     C                   MOVE      *OFF          $INZD2                         inizializzione D2
274100990422     C                   MOVE      *OFF          $EndFile                       memo fine file
274200990422     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
274300990422     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
274400990422     C                   MOVE      *HIVAL        $LASTV
274500990422     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
274600990422     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
274700990422     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
274800990422     C* azzero la posizione del cursore
274900990422     C                   Z-ADD     0             H1RIGA
275000990422     C                   Z-ADD     0             H1COLO
275100990422     C*
275200990422     C                   MOVE      *ON           $INZD1
275300990422     C                   MOVE      *ON           $INZS1
275400990422     C*
275500990422     C* se pgm richiamato con chiave univoca impostata,
275600990422     C* forzo la emissione della lista e imposto l'ordinamento
275700990422    >C*  in questo esempio la chiave univoca per la posta è Codice
275800990422    >C*  ma qualora ci fosse più di un campo basterebbe testare quello più
275900990422    >C*  significativo (o meglio, quello che si sa essere obbligatorio
276000990422    >C*  ricevere pieno)
276100990422    >C***  OPZR75        IFNE      *BLANK
276200990422    >C***  UQ1001        IFNE      *BLANK
276300990422     C***                MOVE      *ON           $Unique
276400990422     C***                Z-ADD     2             $LOP
276500990422     C***                Z-ADD     99            $ORD
276600990422     C***                ELSE
276700990422     C***                MOVE      *OFF          $Unique
276800990422     C***                ENDIF
276900990422     C***                ENDIF
277000990422     C*
277100990422     C* Variabili appoggio
277200990422     C*
277300990422     C                   Z-ADD     SFLPAG        WPAG
277400990422     C                   MOVEL     *OFF          WOPR
277500990422     C                   CLEAR                   W1POS
277600990422     C*
277700990422     C* definisco qual'è la prima videata
277800990422     C* (se la prima videata risulta la lista, devo comunque caricare
277900990422     C*  la videata di parzializzazione e gestire la definizione
278000990422     C*  dell'ordinamento, perchè, dai default oppure se sono stati
278100990422     C*  passati dei dati, può variare il caricamento)
278200990422     C                   SELECT
278300990422     C     $LOP          WHENEQ    1
278400990422     C                   MOVE      'D1'          $GEST
278500990422     C     $LOP          WHENEQ    2
278600990422     C                   MOVE      'S1'          $GEST
278700990422     C                   EXSR      INZD1
278800990422     C                   MOVE      *OFF          $INZD1
278900990422     C                   EXSR      DEFORD
279000990422     C                   ENDSL
279100990422     C*
279200990422     C* Valorizzazione campi univoci testate
279300990422     C*
279400990422     C                   CLEAR                   FMTT1
279500990422     C                   MOVEL     KNSIF         NOMSIF
279600990422     C                   MOVEL     XSCDSI        NOMDIT
279700990422     C                   MOVEL     DSPGM         NOMPGM
279800990422     C* recupero il sottotitolo (se c'è)
279900990422     C                   MOVEL     *BLANK        T1TIT
280000990422     C     SOTTIT        IFNE      *BLANK
280100990422     C                   MOVEL     SOTTIT        IDMSG
280200990422     C                   CALLB     'XRTVM'
280300990422     C                   PARM                    IDMSG            27
280400990422     C                   PARM                    TXTMSG          100
280500990422     C                   PARM                    ERRMSG            1
280600990422     C                   PARM      *ON           CTRMSG            1            centratura
280700990422     C                   PARM      30            LENMSG            3 0          lun output.
280800990422     C     ERRMSG        IFNE      *ON
280900990422     C                   MOVEL     TXTMSG        T1TIT
281000990422     C                   ELSE
281100990422     C                   MOVEL     *ALL'?'       T1TIT
281200990422     C                   ENDIF
281300990422     C                   ENDIF
281400990422     C*
281500990422     C                   ENDSR
281600990422     C/EJECT
281700990422     C************************************************************
281800990422     C* REPERIMENTO DATI SOCIETA'
281900990422     C************************************************************
282000990422     C     REPSOC        BEGSR
282100990422     C*
282200990422     C                   CALLB     'XSOC'
282300990422     C                   PARM                    TIPXSC            6
282400990422     C                   PARM                    SOCXSC            3
282500990422     C                   PARM                    CDSXSC            9 0
282600990422     C                   PARM                    MODXSC            3
282700990422     C                   PARM      *BLANKS       RTNXSC            1
282800990422     C                   PARM                    XSOCDS
282900990422     C                   PARM                    KPJBA
283000990422     C*
283100990422     C                   ENDSR
283200990422     C/EJECT
283300990422     C************************************************************
283400990422     C* REPERIMENTO AUTORIZZAZIONI UTENTE
283500990422     C************************************************************
283600990422     C     REPAUT        BEGSR
283700990422     C*
283800990422     C                   CLEAR                   AUTDS
283900990422     C*
284000990422     C*
284100990422     C                   ENDSR
284200990422     C/EJECT
284300990422     C************************************************************
284400990422     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
284500990422     C************************************************************
284600990422     C     REPDFT        BEGSR
284700990422     C*
284800990422     C* preparo i parametri di chiamata al driver
284900990422     C                   CLEAR                   XMPEDS
285000990422     C                   EVAL      TpChiamata = '01'
285100990422     C                   EVAL      NChiavi = 5
285200990422     C                   EVAL      VLogica = 2
285300990422     C*
285400990422     C* preparo le chiavi per la chiamata al driver
285500990422     C                   CLEAR                   XMPEDS1
285600990422     C                   MOVEL     XSCSOC        DSMPESOC
285700990422     C                   MOVEL     DSPGM         DSMPEPGM
285800990422     C                   MOVEL     KNMUS         DSMPEPRF
285900990422     C* Si reperiscono i dati di default a seconda delle modalità con
286000990422     C* cui il pgm è usato
286100990422     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
286200990422     C* MPETIP = 'PV'  pgm richiamato da menù
286300990422     C* MPETIP = 'PB'  x lancio Batch
286400990422     C                   SELECT
286500990422    >C     OPZR75        WHENEQ    '01'
286600990422     C                   MOVEL     'PX'          DSMPETIP
286700990422     C                   OTHER
286800990422     C                   MOVEL     'PV'          DSMPETIP
286900990422     C                   ENDSL
287000990422     C*
287100990422     C                   CALLB     'XMPE'
287200990422     C                   PARM                    XMPEDS
287300990422     C                   PARM                    XMPEDS1
287400990422     C*
287500990422     C     Risultato     IFEQ      '0'
287600990422     C                   MOVEL     DSMPEPAR      DSMPE
287700990422     C* nel caso sia la prima volta che l'utente apre questa anagrafica
287800990422     C* i suoi valori di default vengono decisi a pgm
287900990422     C                   ELSE
288000990422     C                   Z-ADD     1             $LOP
288100990422     C                   Z-ADD     1             $ORD
288200990422    >C*  elenco altri dati default specifici dell'anagrafica
288300990422     C                   ENDIF
288400990422     C*
288500990422     C                   ENDSR
288600990422     C/EJECT
288700990422     C************************************************************
288800990422     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
288900990422     C************************************************************
289000990422     C     DEFOPZ        BEGSR
289100990422     C*
289200990422     C* Questa routine deve valorizzare la schiera $FL1
289300990422     C* con S o N a seconda della utilizzabilità o meno
289400990422     C* dell'opzione correlata
289500990422     C*
289600990422     C* Inizialmente si considerano tutte le opzioni come valide
289700990422     C     1             DO        $DIMOP        X
289800990422     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
289900990422     C                   If        $TX1(X)  <> *Blank
290000990422     C                   MOVE      'S'           $FL1(X)
290100990422     C                   Else
290200990422     C                   Leave
290300990422     C                   EndIf
290400990422     C                   ENDDO
290500990422     C*
290600990422     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
290700990422     C* con 1 o 0 a seconda della utilizzabilità o meno
290800990422     C* del tasto funzione correlato
290900990422     C*
291000990422     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
291100990422     C* di compilazione
291200990422     C                   RESET                   $FC1
291300990422     C                   RESET                   $FR1
291400990422     C                   RESET                   $FC2
291500990422     C                   RESET                   $FR2
291600990422     C                   RESET                   $FC3
291700990422     C                   RESET                   $FR3
291800990422     C*
291900990422     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
292000990422     C* invalido alcune opzioni
292100990422     C                   SELECT
292200990422     C* Se pgm richiamato in ricerca disattivo :
292300990422    >C     OPZR75        WHENEQ    '01'
292400990422     C* F3=Fine
292500990422     C                   Z-ADD     3             X
292600990422     C                   MOVE      '0'           $FC1(X)
292700990422     C                   MOVE      '0'           $FC2(X)
292800990422     C                   MOVE      '0'           $FC3(X)
292900990422     C*
293000990422    >C*  aggiungere nello stesso modo le altre opzioni da
293100990422    >C*  disattivare (quelle specifiche per ogni pgm)
293200990422     C*
293300990422     C* Se pgm richiamato in interrogazione disattivo :
293400990422    >C     OPZR75        WHENEQ    '05'
293500990422     C* F10=Immissione
293600990422     C                   Z-ADD     10            X
293700990422     C                   MOVE      '0'           $FC1(X)
293800990422     C                   MOVE      '0'           $FC2(X)
293900990422     C* 1=Selezione
294000990422     C                   Z-ADD     1             X
294100990422     C     01            LOOKUP    $OP1(X)                                21
294200990422     C     *IN21         IFEQ      *ON
294300990422     C                   MOVE      'N'           $FL1(X)
294400990422     C                   ENDIF
294500990422     C* 2=Modifica
294600990422     C                   Z-ADD     1             X
294700990422     C     02            LOOKUP    $OP1(X)                                21
294800990422     C     *IN21         IFEQ      *ON
294900990422     C                   MOVE      'N'           $FL1(X)
295000990422     C                   ENDIF
295100990422     C* 3=Copia
295200990422     C                   Z-ADD     1             X
295300990422     C     03            LOOKUP    $OP1(X)                                21
295400990422     C     *IN21         IFEQ      *ON
295500990422     C                   MOVE      'N'           $FL1(X)
295600990422     C                   ENDIF
295700990422     C* 4=Annullamento
295800990422     C                   Z-ADD     1             X
295900990422     C     04            LOOKUP    $OP1(X)                                21
296000990422     C     *IN21         IFEQ      *ON
296100990422     C                   MOVE      'N'           $FL1(X)
296200990422     C                   ENDIF
296300990422     C*
296400990422    >C     OPZR75        WHENEQ    *BLANKS
296500990422     C* Se pgm non richiamato, disattivo subito :
296600990422     C* F8=Selezione seguente
296700990422     C                   Z-ADD     8             X
296800990422     C                   MOVE      '0'           $FC1(X)
296900990422     C                   MOVE      '0'           $FC2(X)
297000990422     C* 1=Selezione
297100990422     C                   Z-ADD     1             X
297200990422     C     01            LOOKUP    $OP1(X)                                21
297300990422     C     *IN21         IFEQ      *ON
297400990422     C                   MOVE      'N'           $FL1(X)
297500990422     C                   ENDIF
297600990422     C*
297700990422     C* In tutti gli altri casi, disattivo subito :
297800990422     C                   OTHER
297900990422     C* 1=Selezione
298000990422     C                   Z-ADD     1             X
298100990422     C     01            LOOKUP    $OP1(X)                                21
298200990422     C     *IN21         IFEQ      *ON
298300990422     C                   MOVE      'N'           $FL1(X)
298400990422     C                   ENDIF
298500990422     C*
298600990422     C                   ENDSL
298700990422     C***
298800990422     C* poi, disattivo a seconda delle autorizzazioni :
298900990422    >C***  ImmAAF        IFEQ      *Off
299000990422     C* F10=Immissione
299100990423     C***                Z-ADD     10            X
299200990423     C***                MOVE      '0'           $FC1(X)
299300990423     C***                MOVE      '0'           $FC2(X)
299400990422     C* 3=Copia
299500990423     C***                Z-ADD     1             X
299600990423     C***  03            LOOKUP    $OP1(X)                                21
299700990423     C***  *IN21         IFEQ      *ON
299800990423     C***                MOVE      'N'           $FL1(X)
299900990423     C***                ENDIF
300000990422     C***                ENDIF
300100990422     C*
300200990422     C                   ENDSR
300300990422     C/EJECT
300400990422     C************************************************************
300500990422     C* REPERIMENTO OPZIONI
300600990422     C************************************************************
300700990422     C     REPOPZ        BEGSR
300800990422     C*
300900990422     C     1             DO        $DIMOP        X
301000990422     C*
301100990422     C     $MS1(X)       IFNE      *BLANK
301200990422     C*
301300990422     C                   MOVEL     $MS1(X)       IDMSG
301400990422     C                   CALLB     'XRTVM'
301500990422     C                   PARM                    IDMSG            27
301600990422     C                   PARM                    TXTMSG          100
301700990422     C                   PARM                    ERRMSG            1
301800990422     C*
301900990422     C     ERRMSG        IFNE      *ON
302000990422     C                   MOVEL     TXTMSG        $TX1(X)
302100990422     C                   ELSE
302200990422     C                   MOVEL     *ALL'?'       $TX1(X)
302300990422     C                   ENDIF
302400990422     C*
302500990422     C                   ENDIF
302600990422     C*
302700990422     C                   ENDDO
302800990422     C*
302900990422     C                   ENDSR
303000990422     C/EJECT
303100990422     C************************************************************
303200990422     C* REPERIMENTO TASTI DI FUNZIONE
303300990422     C************************************************************
303400990422     C     REPKEY        BEGSR
303500990422     C                   MOVEA     $FM1          $FM
303600990422     C                   MOVEA     $FC1          $FC
303700990422     C                   EXSR      REPKKK
303800990422     C                   MOVEA     $FX           $FX1
303900990422     C*
304000990422     C                   MOVEA     $FM2          $FM
304100990422     C                   MOVEA     $FC2          $FC
304200990422     C                   EXSR      REPKKK
304300990422     C                   MOVEA     $FX           $FX2
304400990422     C*
304500990422     C                   MOVEA     $FM3          $FM
304600990422     C                   MOVEA     $FC3          $FC
304700990422     C                   EXSR      REPKKK
304800990422     C                   MOVEA     $FX           $FX3
304900990422     C                   ENDSR
305000990422     C/EJECT
305100990422     C************************************************************
305200990422     C* REPERIMENTO TASTI DI FUNZIONE
305300990422     C************************************************************
305400990422     C     REPKKK        BEGSR
305500990422     C*
305600990422     C                   CLEAR                   $FX
305700990422     C     1             DO        24            X
305800990422     C*
305900990422     C     $FM(X)        IFNE      *BLANK
306000990422     C     $Fc(X)        andne     '0'
306100990422     C*
306200990422     C                   MOVEL     $FM(X)        IDMSG
306300990422     C                   CALLB     'XRTVM'
306400990422     C                   PARM                    IDMSG            27
306500990422     C                   PARM                    TXTMSG          100
306600990422     C                   PARM                    ERRMSG            1
306700990422     C*
306800990422     C     ERRMSG        IFNE      *ON
306900990422     C                   MOVEL     TXTMSG        $FX(X)
307000990422     C                   ELSE
307100990422     C                   MOVEL     *ALL' '       $FX(X)
307200990422     C                   ENDIF
307300990422     C*
307400990422     C                   ENDIF
307500990422     C*
307600990422     C                   ENDDO
307700990422     C*
307800990422     C                   ENDSR
307900990422     C/EJECT
308000990422     C************************************************************
308100990422     C* DEFINIZIONE KLIST
308200990422     C************************************************************
308300990422     C     DefKlist      BEGSR
308400990422     C*
308500990422     C* klist
308600990422     C*
308700990422    >C* anape01L
308800990422    >C     K03APE01      KLIST
308900990422    >C                   KFLD                    APESOCIETA
309000990422    >C                   KFLD                    APEKcc
309100990422    >C                   KFLD                    APEKsc
309200990422     C*
309300990422    >C* ND77005L
309400990422    >C     K03N7705      KLIST
309500990422    >C                   KFLD                    N77SOCIETA
309600990422    >C                   KFLD                    N77ACF
309700990422    >C                   KFLD                    N77Qua
309800990422     C*
309900990422    >C* ND77005L
310000990422    >C     K04N7705      KLIST
310100990422    >C                   KFLD                    N77SOCIETA
310200990422    >C                   KFLD                    N77ACF
310300990422    >C                   KFLD                    N77Qua
310400990422    >C                   KFLD                    N77Cog
310500020411      *
310600020411     C* PLIST per NDVASOG
310700020411     C     PDVASOG       PLIST
310800020411     C                   PARM                    DVARIC
310900020411     C                   PARM                    DVASOCIETA
311000020411     C                   PARM                    DVAUNITA
311100020411     C                   PARM                    DVASTRUTT
311200020411     C                   PARM                    DVADTRIF
311300020411     C                   PARM                    DVALENOUT
311400020411     C* Se si accede con il codice cli/for impostare i campi seguenti
311500020411     C* se si accede con il soggetto ometterli e impostare gli altri
311600020411     C                   PARM                    DVASNATURA
311700020411     C                   PARM                    DVACODICE
311800020411     C                   PARM                    DVALINEAV
311900020411     C                   PARM                    DVAFILIALE
312000020411     C                   PARM                    *omit
312100020411     C*                  PARM                    *omit
312200020411     C*                  PARM                    *omit
312300020411     C*                  PARM                    DVSSOGG
312400020411     C                   PARM                    DVATPIND
312500020411     C                   PARM                    DVACDIND
312600020411     C                   PARM                    DVAERRORE
312700020411     C                   PARM                    DVAOUTPUT
312800990422     C*
312900990422     C                   ENDSR
313000990422     C/EJECT
313100990422**
313200990422PROMSG    *LIBL     OPZ0001
313300990422PROMSG    *LIBL     OPZ0002
313400990422PROMSG    *LIBL     OPZ0003
313500990422PROMSG    *LIBL     OPZ0004
313600990422PROMSG    *LIBL     OPZ0005
313700990423
313800990422**
31390099042201S      <Selezione
31400099042202S      <Modifica
31410099042203S      <Copia
31420099042204S      <Annullamento
31430099042205S      <Visualizzazione
31440099042239S      <Estraz. debitori
314500990422** TASTI DI FUNZIONE  D1
314600990422PROMSG    *LIBL     KEY0001  01
314700990422PROMSG    *LIBL     KEY0002  02
314800990422PROMSG    *LIBL     KEY0003  03
314900990422PROMSG    *LIBL     KEY0004  04
315000990422PROMSG    *LIBL     KEY0005  05
315100990422PROMSG    *LIBL     KEY0006  06
315200990422PROMSG    *LIBL     KEY0007  07
315300990422PROMSG    *LIBL     KEY0008  08
315400990422PROMSG    *LIBL     KEY0009  09
315500990422PROMSG    *LIBL     KEY0010  10
315600990422PROMSG    *LIBL     KEY0011  11
315700990422PROMSG    *LIBL     KEY0012  12
315800990422PROMSG    *LIBL     KEY0013  13
315900990422PROMSG    *LIBL     KEY0014  14
316000990422PROMSG    *LIBL     KEY0015  15
316100990422PROMSG    *LIBL     KEY0016  16
316200990422PROMSG    *LIBL     KEY0017  17
316300990422PROMSG    *LIBL     KEY0018  18
316400990422PROMSG    *LIBL     KEY0019  19
316500990422PROMSG    *LIBL     KEY0020  20
316600990422PROMSG    *LIBL     KEY0021  21
316700990422PROMSG    *LIBL     KEY0022  22
316800990422PROMSG    *LIBL     KEY0023  23
316900990422PROMSG    *LIBL     KEY0024  24
317000990422**  ABILITAZIONE D1
3171009904220N  01
3172000204241N  02
3173009904221N  03
3174000204151N  04
3175009904220N  05
3176009904220N  06
3177009904220N  07
3178009904220N  08
3179009904220N  09
3180009904220N  10
3181009904220N  11
3182009904221N  12
3183009904220N  13
3184009904220N  14
3185009904220N  15
3186009904220N  16
3187009904220N  17
3188009904221N  18
3189009904220N  19
3190009904220N  20
3191009904220N  21
3192009904220N  22
3193009904220N  23   Impostare a '1' se si utilizzano opzioni
3194009904221N  24   Lasciare sempre impostata a '1' il valore della riga 24
319500990422** TASTI DI FUNZIONE  S1
319600990422PROMSG    *LIBL     KEY0001  01
319700990422PROMSG    *LIBL     KEY0002  02
319800990422PROMSG    *LIBL     KEY0003  03
319900990422PROMSG    *LIBL     KEY0004  04
320000990422PROMSG    *LIBL     KEY0005  05
320100990422PROMSG    *LIBL     KEY0006  06
320200990422PROMSG    *LIBL     KEY0007  07
320300990422PROMSG    *LIBL     KEY0008  08
320400990422PROMSG    *LIBL     KEY0009  09
320500990422PROMSG    *LIBL     KEY0010  10
320600990422PROMSG    *LIBL     KEY0011  11
320700990422PROMSG    *LIBL     KEY0012  12
320800990422PROMSG    *LIBL     KEY0013  13
320900990422PROMSG    *LIBL     KEY0014  14
321000990422PROMSG    *LIBL     KEY0015  15
321100990422PROMSG    *LIBL     KEY0016  16
321200990422PROMSG    *LIBL     KEY0017  17
321300990422PROMSG    *LIBL     KEY0018  18
321400990422PROMSG    *LIBL     KEY0019  19
321500990422PROMSG    *LIBL     KEY0020  20
321600990422PROMSG    *LIBL     KEY0021  21
321700990422PROMSG    *LIBL     KEY0022  22
321800990422PROMSG    *LIBL     KEY0023  23
321900990422PROMSG    *LIBL     KEY0024  24
322000990422**  ABILITAZIONE S1
3221009904220N  01
3222000204240N  02
3223009904221N  03
3224009904220N  04
3225009904220N  05
3226009904220N  06
3227009904220N  07
3228009904220N  08
3229009904220N  09
3230009904221N  10
3231009904220N  11
3232009904221N  12
3233009904220N  13
3234009904220N  14
3235009904220N  15
3236009904220N  16
3237009904221N  17
3238009904221N  18
3239009904220N  19
3240009904220N  20
3241009904220N  21
3242009904220N  22
3243009904221N  23   Impostare a '1' se si utilizzano opzioni
3244009904221N  24   Lasciare sempre impostata a '1' il valore della riga 24
324500990422** TASTI DI FUNZIONE  D3
324600990422PROMSG    *LIBL     KEY0001  01
324700990422PROMSG    *LIBL     KEY0002  02
324800990422PROMSG    *LIBL     KEY0003  03
324900990422PROMSG    *LIBL     KEY0004  04
325000990422PROMSG    *LIBL     KEY0005  05
325100990422PROMSG    *LIBL     KEY0006  06
325200990422PROMSG    *LIBL     KEY0007  07
325300990422PROMSG    *LIBL     KEY0008  08
325400990422PROMSG    *LIBL     KEY0009  09
325500990422PROMSG    *LIBL     KEY0010  10
325600990422PROMSG    *LIBL     KEY0011  11
325700990422PROMSG    *LIBL     KEY0012  12
325800990422PROMSG    *LIBL     KEY0013  13
325900990422PROMSG    *LIBL     KEY0014  14
326000990422PROMSG    *LIBL     KEY0015  15
326100990422PROMSG    *LIBL     KEY0016  16
326200990422PROMSG    *LIBL     KEY0017  17
326300990422PROMSG    *LIBL     KEY0018  18
326400990422PROMSG    *LIBL     KEY0019  19
326500990422PROMSG    *LIBL     KEY0020  20
326600990422PROMSG    *LIBL     KEY0021  21
326700990422PROMSG    *LIBL     KEY0022  22
326800990422PROMSG    *LIBL     KEY0023  23
326900990422PROMSG    *LIBL     KEY0024  24
327000990422**  ABILITAZIONE D3
3271009904220N  01
3272009904220N  02
3273009904221N  03
3274009904220N  04
3275009904220N  05
3276009904220N  06
3277009904220N  07
3278009904220N  08
3279009904220N  09
3280009904220N  10
3281009904220N  11
3282009904221N  12
3283009904220N  13
3284009904220N  14
3285009904220N  15
3286009904220N  16
3287009904220N  17
3288009904220N  18
3289009904220N  19
3290009904220N  20
3291009904220N  21
3292009904220N  22
3293009904220N  23   Impostare a '1' se si utilizzano opzioni
3294009904221N  24   Lasciare sempre impostata a '1' il valore della riga 24
