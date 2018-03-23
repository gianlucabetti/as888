000100120502     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200120502     H*PARMS BNDDIR(PJXBND PJCBND)
000300120502     H*PARMS CVTOPT(*datetime)
000400990422     H DECEDIT('0,') DATEDIT(*DMY.)
000500990422      *
000600990422      *
000700170216R250 X***          Manutenzione mod.770 percipienti
000800170216R250  *             Manutenzione CU percipienti
000900990423      *           ----------------------------------
001000990422      *
001100990422      *
001200990422      * PERSONALIZZAZIONI
001300990422      *
001400990422      *
001500990422      *
001600990422      *               MAPPA INDICATORI STD
001700990422      *
001800990422      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
001900990422      *  21           GENERICO OPERAZIONI I/O
002000990422      *  22           GENERICO ERRORE OPERAZIONI I/O
002100990422      *  30           SFLDSP
002200990422      * N31           SFLCLR
002300990422      *  31           SFLDSPCTL
002400990422      *  32           SFLNXTCHG
002500990422      *  33           SFLEND
002600990422      *  39           OF PRTF
002700990422      *  40 <---> 49  DSPATR ERRORI SU SFL
002800990422    > *  Specificare l'uso dei singoli indicatori
002900990422      *  50 <---> 89  ERRORI SU VIDEO
003000990422      *  elenco errori speciali:
003100990422      *  90           REGISTRAZIONE GIA' ESISTENTE PER IMMISSIONE O COPIA
003200990422      *  91           OPZIONE NON VALIDA
003300990422      *  92           IMPOSSIBILE RIPOSIZIONAMENTO CON OPZIONI PRESENTI SU LISTA
003400990422      *  93           LA FUNZIONE RICHIAMATA SI E' CHIUSA IN ERRORE
003500990422      *  95           L'OPERAZIONE DI SCRITTURA NON E' ANDATA A BUON FINE
003600990422      *  94           UTENTE NON AUTORIZZATO ALLA FUNZIONE NEL MODO DESIDERATO
003700990422      *  96           ESISTONO ERRORI IN ALTRI SCHERMI
003800990422      *  97           TASTO   NON ABILITATO
003900990422      *  98           RICERCA NON ABIL. NELLA POSIZ.
004000990422      *  99           INDIC. GENERALE DI ERRORE
004100170216R250  *----------------------------------------------------*
004200170216R250  *  R250: Modificata anche dicitura del sorgente, togliendo
004300170216R250  *  il riferimento al 770.
004400990422     F*----------------------------------------------------*
004500990422     F*------------
004600990422    >FANAPE01L  IF   E           K DISK
004700990422    >F                                     RENAME(anape000:anape01)
004800990422     F*------------
004900150626R186>X*** ND77006L  UF A E           K DISK    USROPN
005000150626R186>X***                                  RENAME(Nd770000:ND77006)
005100150626R186>FND77020L  UF A E           K DISK    USROPN
005200150626R186>F                                     RENAME(Nd770000:ND77020)
005300990422     F*------------
005400990422    >FNDCR75D1  CF   E             WORKSTN
005500990422     F                                     RENAME(CR75D1:FMTD1)
005600990422     F                                     RENAME(CR75T1:FMTT1)
005700990422     F                                     RENAME(CR75Z1:FMTZ1)
005800990422    >F* eventuali schermi supplementari
005900990422    >F                                     RENAME(CR75D2:FMTD2)
006000010328    >F                                     RENAME(CR75D3:FMTD3)
006100100514R024 F                                     RENAME(CR75D4:FMTD4)
006200150305R172 F                                     RENAME(CR75D5:FMTD5)
006300160128R211 F                                     RENAME(CR75D6:FMTD6)
006400160128  "  F                                     RENAME(CR75D7:FMTD7)
006500160128R211 F                                     RENAME(CR75D8:FMTD8)
006600990422     F                                     INFDS(DSFMT)
006700990422     F*------------
006800990422     D*----------------------------------------------------*
006900990422     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
007000990422     D $ID             S             27    DIM(5) ALT($OZ)
007100990422     D* Schiere per la gestione dei tasti di funzione
007200990422     D $FM             S             27    DIM(24)
007300990422     D $FX             S             50    DIM(24)
007400990422     D $FC             S              1    DIM(24)
007500990422     D $FR             S              1    DIM(24)
007600990422     D* Video D1
007700990422     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
007800990422     D $FX1            S             50    DIM(24) ALT($FM1)
007900990422     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008000990422     D $FR1            S              1    DIM(24) ALT($FC1)
008100990422     D* Schiera contente i nomi dei programmi da richiamare con i tasti
008200990422     D* funzione personalizzati
008300990422     D $PGF1           S             10    DIM(25)
008400990422     D*----------------------------------------------------*
008500990422     D $D1L01          S              3  0
008600990422     D $D1L02          S              3  0
008700990422     D*-------------
008800990422     D* Passaggio Parametri
008900990422     D KPJBA         E DS
009000990422     D*-------------
009100990422     D* DS Esterna gestione messaggi
009200990422     D XMSGDS        E DS
009300990422     D  STS          E                     EXTFLD(MSGSTS)
009400990422     D                                     DIM(333)
009500990422     D  JBA          E                     EXTFLD(MSGJBA)
009600990422     D                                     DIM(502)
009700990422     D*-------------
009800990422     D* Reperimento Autorizzazioni
009900990422     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010000990422     D*-------------
010100990422     D* Passaggio Autorizzazioni
010200990422     D DSAUT           DS           512
010300990422     D*-------------
010400990422     D* Reperimento nome PGM
010500990422     D STATUS         SDS           333
010600990422     D  DSPGM            *PROC
010700120502     D  ParmS            *ParmS
010800990422     D*-------------
010900990422     D* Ricerche su repository
011000990422     D Ana964DS      E DS                  Inz
011100990422     D*-------------
011200990422     D* Controlli validità legende
011300990422     D X07DS         E DS                  EXTNAME(X07VALDS)
011400990422     D*-------------
011500990422     D* Dati di ambiente ottenuti da XSOC
011600990422     D SOC001        E DS                  EXTNAME(XSOC001DS)
011700990422     D    FgO1                        1    overlay(XscFgO)
011800990422     D*-------------
011900990422     D* DS Interna per dati di output di XSOC
012000990422     D XSOCDS          DS          1000
012100990422     D*-------------
012200990422     D* Passaggio Parametri al PGM di manutenzione
012300990422    >D NDCR75DS      E DS
012400990422     D*-------------
012500990422     D* campo di salvataggio per la DS d'ingresso durante la chiamata
012600990422     D* alla funzione personalizzata
012700990422    >D $SaveDS         S                   like(NDCR75Ds)
012800990422     D*-------------
012900990422     D* Ricerca capoconti
013000990422     D Ana200Ds      E DS                  Inz
013100990422    >D  Ret200       E                     Inz('0')
013200990422    >D  Opr200       E                     Inz('0')
013300990422    >D  Err200       E                     Inz('0')
013400990422    >D  Cmt200       E                     Inz('0')
013500990423     D*-------------
013600990423     D* Ricerca sottoconti
013700990423     D Ana201DS      E DS                  Inz
013800990423    >D  Err201       E                     Inz('0')
013900990422     D*-------------
014000990422     D* Parametri per reperimento decodifica e soggetto relativi al conto
014100990422     D Nd002DS       E DS
014200020807D1183D*------------
014300020807  "  D* Richiamo a XATB
014400020807  "  D XATBDS        E DS
014500020807D1183D TABG16        E DS                  EXTNAME(ANGG16DS) INZ
014600990422     D*-------------
014700990422     D* Definizioni parametri decodifica conto
014800990422     D $Esito          S              1
014900990422     D $GesMsg         S              1
015000990422     D $LenOut         S              9B 0
015100990422     D $Struttura      S             10
015200990422     D $Kcc            S              6
015300990422     D $Ksc            S              8
015400990422     D $Soc            S              3
015500990422     D*-------------
015600990423     D* Definizioni parametri driver soggetti
015700990423     D DvaRic          S              1
015800990423     D DvaSocieta      S              3
015900990423     D DvaUnita        S              8
016000990423     D DvaStrutt       S             10
016100990423     D DvaDtRif        S               D
016200990423     D DvaLenOut       S              9B 0
016300990423     D DvaCodice       S              8
016400990423     D DvaLineav       S              3
016500990423     D DvaSogg         S              8
016600990423     D DvaTpInd        S              2
016700990423     D DvaCdInd        S              3
016800990423     D DvaOutput       S           4000
016900990423     D DvaErrore       S              1
017000990423     d*-------------
017100990423     D* DS esterna per dati soggetti
017200990423     D DVASOG        E DS                  INZ
017300990423     d*-------------
017400990422     D DSFMT           DS
017500990422     D  $TASTO               369    369
017600990422     D  NRG                  370    370
017700990422     D  NCL                  371    371
017800990422     D*-------------
017900990422     D* DS per UserExit
018000990422     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
018100990422     D*-------------
018200990422     D* posizione cursore
018300990422     D CURSOR          DS
018400990422     D  RIRI                   1      2B 0 INZ
018500990422     D  R$$                    2      2
018600990422     D  COCO                   3      4B 0 INZ
018700990422     D  C$$                    4      4
018800990423     D*-------------
018900990423     D* Variabili appoggio specifiche della singola anagrafica
019000990423     D*  Indici
019100990423     D X               S              3S 0
019200990423     D Y               S              3S 0
019300990422     D*-------------
019400990422     D* Costanti
019500990422     D*-------------
019600990422     D* Tasti di funzione
019700990422     D F01             C                   CONST(X'31')
019800990422     D F02             C                   CONST(X'32')
019900990422     D F03             C                   CONST(X'33')
020000990422     D F04             C                   CONST(X'34')
020100990422     D F05             C                   CONST(X'35')
020200990422     D F06             C                   CONST(X'36')
020300990422     D F07             C                   CONST(X'37')
020400990422     D F08             C                   CONST(X'38')
020500990422     D F09             C                   CONST(X'39')
020600990422     D F10             C                   CONST(X'3A')
020700990422     D F11             C                   CONST(X'3B')
020800990422     D F12             C                   CONST(X'3C')
020900990422     D F13             C                   CONST(X'B1')
021000990422     D F14             C                   CONST(X'B2')
021100990422     D F15             C                   CONST(X'B3')
021200990422     D F16             C                   CONST(X'B4')
021300990422     D F17             C                   CONST(X'B5')
021400990422     D F18             C                   CONST(X'B6')
021500990422     D F19             C                   CONST(X'B7')
021600990422     D F20             C                   CONST(X'B8')
021700990422     D F21             C                   CONST(X'B9')
021800990422     D F22             C                   CONST(X'BA')
021900990422     D F23             C                   CONST(X'BB')
022000990422     D F24             C                   CONST(X'BC')
022100990422     D ENTER           C                   CONST(X'F1')
022200990422     D ROLDWN          C                   CONST(X'F4')
022300990422     D ROLLUP          C                   CONST(X'F5')
022400990422     D*-------------
022500990422     D* Sottotitolo di testata
022600160203    >D* se non si h$nno sottotitoli, mettere tutta la costante = ' '
022700990422     D SOTTIT          C                   CONST('               -
022800990422     D                                                 ')
022900990422     D*-------------
023000990422     D* Variabili per gestione videate
023100990422     D $Gest           S              2A
023200990422     D $Fine           S              1A
023300990422     D $InzD1          S              1A
023400990422     D $LastG          S              2A
023500990422     D $LastV          S              2A
023600990422     D $FlgDef         S              1A
023700990422     D*-------------
023800110413R055 D $COD_S          S             10    varying
023900110413R055 D*-------------
024000990422     D* Variabili appoggio sempre presenti in tutte le anagrafiche
024100990422     D CurR            S              2S 0
024200990422     D CurC            S              2S 0
024300990422     D Win             S             99A
024400990608     D DataISO         S               D
024500990608     D* data a 0
024600990608     D $DT0            C                   CONST('0001-01-01')
024700990422     D* Indice della schiera tasti funzione
024800990422     D PosFun          S              2  0
024900990422     D*-------------
025000990422     C*----------------------------------------------------*
025100990422     C*                MAIN LINE PROGRAM
025200990422     C*----------------------------------------------------*
025300990422     C*
025400990422     C* inizializzazione variabili
025500120502     C                   Exsr      INZVAR
025600990422     C*
025700990422     C     $FINE         DOWEQ     *OFF
025800990422     C     $GEST         CASEQ     'D1'          GESD1
025900990422    >C     $GEST         CASEQ     'D2'          GESD2
026000010328C1487C     $GEST         CASEQ     'D3'          GESD3
026100150305R172 C     $GEST         CASEQ     'D5'          GESD5
026200160127R211 C     $GEST         CASEQ     'D6'          GESD6
026300160127  "  C     $GEST         CASEQ     'D7'          GESD7
026400160127R211 C     $GEST         CASEQ     'D8'          GESD8
026500990422     C                   ENDCS
026600990422     C                   ENDDO
026700990422     C*
026800990422     C* fine programma
026900120502     C                   Exsr      ENDPGM
027000990422     C/EJECT
027100990422     C************************************************************
027200990422     C* Fine Programma
027300990422     C************************************************************
027400120502     C     ENDPGM        BegSr
027500990422     C*
027600990422     C* se non esco per errore
027700120502     C     ErrR75        IfEq      '0'
027800990422     C*
027900120502     C                   Select
028000990422     C*
028100120502     C                   EndSl
028200990422     C*
028300120502     C                   EndIf
028400990422     C*
028500990422     C* se ho confermato l'uscita proseguo
028600120502     C     $FINE         IfEq      *ON
028700990422     C*
028800040305     C* disallocazione rcd nel caso di modifica/annullamento
028900120502     C     OPZR75        IfEq      '02'
029000040305     C     OPZR75        OREQ      '04'
029100150626R186 X***                UNLOCK    Nd77006L                             22
029200150626R186 C                   UNLOCK    Nd77020L                             22
029300120502     C                   EndIf
029400040305     C*
029500040305     C* passaggio dati al pgm chiamante
029600120502     C                   MoveL     NDCR75Ds      KPJBU
029700040305     C*
029800120502     C                   SetOn                                            RT
029900040305     C                   RETURN
030000040305     C*
030100120502     C                   EndIf
030200990422     C*
030300120502     C                   EndSr
030400990422     C/EJECT
030500990422     C************************************************************
030600990422     C* RICERCHE
030700990422     C************************************************************
030800120502     C     SEARCH        BegSr
030900990422     C*
031000990422     C* determino Riga/Colonna del cursore
031100120502     C                   Move      NRG           R$$
031200120502     C                   Move      NCL           C$$
031300120502     C                   Z-Add     RIRI          CURR
031400120502     C                   Z-Add     COCO          CURC
031500990422     C*
031600990422     C* I campi che al momento dell'F4 risultano protetti non devono
031700990422     C* essere abilitati alla funzione di ricerca
031800990422     C*
031900120502     C                   Select
032000990422     C*
032100990422     C* FMTD1 - Capoconto
032200120502     C     H1NMFL        WhenEq    'KCC'
032300120502     C                   Reset                   ANA200DS
032400120502     C                   Move      '01'          OPZ200
032500120502     C                   Clear                   KPJBU
032600120502     C                   MoveL     ANA200DS      KPJBU
032700120502     C                   Call      'ANA200R'
032800120502     C                   Parm                    KPJBA
032900120502     C                   MoveL     KPJBU         ANA200DS
033000120502     C     Kcc200        IfNE      *BLANK
033100120502     C                   MoveL     Kcc200        Kcc
033200120502     C                   EndIf
033300990422     C*
033400990422     C* FMTD1  - Sottoconto factor
033500120502     C     H1NMFL        WhenEq    'KSC'
033600990422     C* allineo kcc conto
033700120502     C     Kcc           IfNE      *BLANKS
033800120502     C     Kcc           AndNE     'F     '
033900120502     C     Kcc           AndNE     'C     '
034000120502     C                   CallB     'XALLINEA'
034100120502     C                   Parm                    Kcc
034200120502     C                   Parm      6             LEN               5 0
034300120502     C                   Parm                    XSCINP
034400120502     C                   EndIf
034500990422     C*
034600120502     C                   Reset                   ANA201DS
034700120502     C                   Move      XscSoc        SOC201
034800120502     C                   Move      Kcc           KCC201
034900120502     C                   Move      Ksc           Ksc201
035000120502     C                   Clear                   KPJBU
035100120502     C                   MoveL     ANA201DS      KPJBU
035200120502     C                   Call      'ANA201R'
035300120502     C                   Parm                    KPJBA
035400120502     C                   MoveL     KPJBU         ANA201DS
035500120502     C     Ksc201        IfNE      *BLANK
035600120502     C                   MoveL     Kcc201        Kcc
035700120502     C                   MoveL     Ksc201        Ksc
035800120502     C                   EndIf
035900040305     C*
036000040305     C* ricerca non abilitata in questa posizione
036100040305     C                   OTHER
036200120502     C                   SetOn                                        9899
036300040305     C*
036400120502     C                   EndSl
036500040305     C*
036600040305     C* imposto pos. del cursore
036700120502     C                   Z-Add     CURR          H1RIGA
036800120502     C                   Z-Add     CURC          H1COLO
036900040305     C*
037000120502     C                   Move      *HIVAL        $LASTV
037100990422     C*
037200120502     C                   EndSr
037300990422     C/EJECT
037400990422     C************************************************************
037500990422     C* GESTIONE EMISSIONE TASTI FUNZIONE
037600990422     C************************************************************
037700120502     C     GESPIE        BegSr
037800990422     C*
037900120502     C                   Select
038000990422     C*
038100120502     C     $GEST         WhenEq    'D1'
038200120502     C     $ULKD1        IfEq      0
038300990422     C* riempimento opzioni
038400120502     C                   Exsr      F24D1
038500990422     C                   ELSE
038600990422     C                   WRITE     FMTZ1
038700120502     C                   EndIf
038800990422     C*
038900120502     C     $GEST         WhenEq    'D2'
039000120502     C     $ULKD1        IfEq      0
039100990422     C* riempimento opzioni
039200120502     C                   Exsr      F24D2
039300990422     C                   ELSE
039400990422     C                   WRITE     FMTZ1
039500120502     C                   EndIf
039600010328     C*
039700120502     C     $GEST         WhenEq    'D3'
039800120502     C     $ULKD1        IfEq      0
039900010328     C* riempimento opzioni
040000120502     C                   Exsr      F24D3
040100010328     C                   ELSE
040200010328     C                   WRITE     FMTZ1
040300120502     C                   EndIf
040400150305R172 C*
040500150305  "  C     $GEST         WhenEq    'D5'
040600150305  "  C     $ULKD1        IfEq      0
040700150305  "  C* riempimento opzioni
040800150305  "  C                   Exsr      F24D5
040900150305  "  C                   ELSE
041000150305  "  C                   WRITE     FMTZ1
041100150305R172 C                   EndIf
041200160128R122 C*
041300160128  "  C     $GEST         WhenEq    'D6'
041400160128  "  C     $ULKD1        IfEq      0
041500160128  "  C* riempimento opzioni
041600160128  "  C                   Exsr      F24D6
041700160128  "  C                   ELSE
041800160128  "  C                   WRITE     FMTZ1
041900160128  "  C                   EndIf
042000160128  "  C*
042100160128  "  C     $GEST         WhenEq    'D7'
042200160128  "  C     $ULKD1        IfEq      0
042300160128  "  C* riempimento opzioni
042400160128  "  C                   Exsr      F24D6
042500160128  "  C                   ELSE
042600160128  "  C                   WRITE     FMTZ1
042700160128  "  C                   EndIf
042800160128  "  C*
042900160128  "  C     $GEST         WhenEq    'D8'
043000160128  "  C     $ULKD1        IfEq      0
043100160128  "  C* riempimento opzioni
043200160128  "  C                   Exsr      F24D6
043300160128  "  C                   ELSE
043400160128  "  C                   WRITE     FMTZ1
043500160128R211 C                   EndIf
043600010328     C*
043700120502     C                   EndSl
043800990422     C*
043900120502     C                   EndSr
044000990422     C/EJECT
044100990422     C************************************************************
044200990422     C* Chiamata programma con tasto personalizzato
044300990422     C************************************************************
044400120502     C     FPerD1        BegSr
044500990422     C*
044600990422     C* salvo la DS di ingresso
044700120502    >C                   MoveL     NDCR75DS      $SaveDS
044800990422     C*
044900120502    >C                   Move      xscsoc        SocietaR75
045000990422     C* passare nei campi appositi della DS NDCR75Ds il valore
045100990422     C* della chiave della VL univoca
045200120502    >C                   Z-Add     Vacf          AnnoR75
045300990422     C*
045400120502    >C                   MoveL     NDCR75Ds      KPJBU
045500120502    >C                   Call      $PGF1(PosFun)                        22
045600120502     C                   Parm                    KPJBA
045700120502     C                   Parm                    NOMPGM
045800040305     C*
045900040305     C* ripristino la DS di ingresso
046000120502     C                   MoveL     $SaveDS       NDCR75Ds
046100040305     C*
046200120502     C                   EndSr
046300040305     C/EJECT
046400040305     C************************************************************
046500040305     C* GESTIONE VIDEO RECORD D1
046600040305     C************************************************************
046700120502     C     GESD1         BegSr
046800040305     C* inizializzazione videata
046900120502     C     $INZD1        IfEq      *ON
047000120502     C                   Exsr      INZD1
047100120502     C                   Move      *OFF          $INZD1
047200120502     C                   EndIf
047300040305     C* emissione piede videata se proveniente da altra
047400040305     C* salvataggio valore formato di provenienza
047500120502     C     $LASTV        IfNE      'D1'
047600040305     C                   WRITE     FMTT1
047700120502     C                   Exsr      GESPIE
047800120502     C                   Move      $LASTV        $LASTG
047900120502     C                   Move      'D1'          $LASTV
048000120502     C                   EndIf
048100040305     C*
048200040305     C* se esistono errori sulla videata
048300040305     C* emetto la write del formato a indicatori spenti per vedere
048400040305     C* le eventuali decodifiche
048500120502     C     *IN99         IfEq      *ON
048600040305     C     $GEST         ANDEQ     'D1'
048700120502     C                   MoveA     *IN           WIN
048800120502     C                   Move      *ALL'0'       IN5098           49
048900120502     C                   MoveA     IN5098        *IN(50)
049000990422     C                   WRITE     FMTD1
049100120502     C                   MoveA     WIN           *IN
049200120502     C                   EndIf
049300990422     C* Inizializzazione indice tasti funzione personalizzati
049400990422     C                   Eval      PosFun = 25
049500990422     C*
049600060622C1943X*ex C1560
049700060622  "  X*ex C1560 visualizzo sempre tutti i dati
049800120502  "  X*ex C1560          Move      *on           *in78
049900060622C1943X*ex C1560
050000120502     C                   Select
050100040305     C* Annullamento
050200120502     C     OPZR75        WhenEq    '04'
050300040305     C* Visualizzazione
050400040305     C     OPZR75        OREQ      '05'
050500040305     C                   WRITE     FMTD1
050600040305     C                   EXFMT     PROTECT
050700040305     C* Immissione/Modifica/Copia
050800040305     C                   OTHER
050900040305     C                   EXFMT     FMTD1
051000120502     C                   EndSl
051100040305     C*
051200040305     C* azzero la posizione del cursore
051300120502     C                   Z-Add     0             H1RIGA
051400120502     C                   Z-Add     0             H1COLO
051500040305     C*
051600040305     C* controllo abilitazione tasti funzione
051700120502     C                   Exsr      CTRKD1
051800040305     C*
051900120502     C     *IN99         IfEq      *OFF
052000040305     C*
052100120502     C                   Select
052200040305     C* ROLLUP
052300120502     C     $TASTO        WhenEq    ROLLUP
052400160128R211 X***                Move      'D2'          $GEST
052500160128R211 C                   Move      'D6'          $GEST
052600990422     C* F2=Decodifica
052700120502     C     $TASTO        WhenEq    F02
052800120502     C                   Exsr      F02D1
052900990422     C* F3=Fine
053000120502     C     $TASTO        WhenEq    F03
053100120502     C                   Exsr      F03D1
053200990422     C* F4=Ricerca
053300120502     C     $TASTO        WhenEq    F04
053400120502     C                   Exsr      SEARCH
053500990422     C* F5=Refresh
053600120502     C     $TASTO        WhenEq    F05
053700120502     C                   Exsr      F05D1
053800990422     C* F8=Successivo
053900120502     C     $TASTO        WhenEq    F08
054000120502     C                   Exsr      F08D1
054100990422     C* F12=Ritorno
054200120502     C     $TASTO        WhenEq    F12
054300120502     C                   Exsr      F12D1
054400990422     C* F24=Altri tasti
054500120502     C     $TASTO        WhenEq    F24
054600120502     C                   Exsr      F24D1
054700990422     C* Personalizzata
054800990422     C     $PGF1(PosFun) WHENNE    *BLANK
054900120502     C                   Exsr      FPerD1
055000040305     C*
055100040305     C                   OTHER
055200120502     C                   Exsr      CTRD1
055300990422     C* aggiungere le chiamate ai controlli delle videate supplementari
055400160127R211 C     *IN99         IfEq      *OFF
055500160127R211 C                   Exsr      CTRD6
055600120502     C     *IN99         IfEq      *OFF
055700120502     C                   Exsr      CTRD2
055800120502C1943C     *IN99         IfEq      *Off
055900120502C1478C                   Exsr      CTRD3
056000160127R211 C     *IN99         IfEq      *Off
056100160127  "  C                   Exsr      CTRD5
056200160127  "  C     *IN99         IfEq      *Off
056300160127  "  C                   Exsr      CTRD7
056400160127  "  C     *IN99         IfEq      *Off
056500160127  "  C                   Exsr      CTRD8
056600160127  "  C                   Endif
056700160127  "  C                   Endif
056800160127  "  C                   Endif
056900160127R211 C                   Endif
057000120502C1943C                   EndIf
057100120502  "  C                   EndIf
057200060622C1943C*
057300120502     C     *IN99         IfEq      *ON
057400040305     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
057500040305     C* indicatori uguali in videate differenti, genererei conflitti
057600120502     C                   Move      *ALL'0'       IN5098           49
057700120502     C                   MoveA     IN5098        *IN(50)
057800120502     C                   SetOn                                        9996                       96
057900120502     C                   EndIf
058000120502C1943X****               EndIf
058100040305     C*
058200120502     C     $TASTO        IfEq      F06
058300990422     C     *IN99         ANDEQ     *OFF
058400040305     C* eseguo aggiornamento
058500120502     C                   Exsr      AGGANA
058600040305     C* eseguo operazioni del dopo-aggiornamento
058700120502     C     *IN99         IfEq      *OFF
058800120502     C                   Exsr      GESAGG
058900120502     C                   EndIf
059000120502     C                   EndIf
059100040305     C*
059200120502     C                   EndSl
059300040305     C*
059400120502     C                   EndIf
059500040305     C*
059600120502     C                   EndSr
059700990422     C/EJECT
059800990422     C************************************************************
059900990422     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
060000990422     C************************************************************
060100120502     C     CTRKD1        BegSr
060200990422     C*
060300990422     C                   SETOFF                                       99
060400120502     C                   MoveA     $FC1          $FC
060500120502     C                   Exsr      CTRKEY
060600120502     C     *IN99         IfEq      *ON
060700120502     C                   SetOn                                        97
060800120502     C                   EndIf
060900990422     C*
061000120502     C                   EndSr
061100990422     C/EJECT
061200990422     C************************************************************
061300990422     C* INIZIALIZZAZIONE VIDEATA DATI
061400990422     C************************************************************
061500120502     C     INZD1         BegSr
061600990422     C*
061700120502     C                   Clear                   FMTD1
061800120502     C                   Clear                   FMTD2
061900120502     C                   Clear                   FMTD3
062000150305R172 C                   Clear                   FMTD5
062100160201R211 C                   Clear                   FMTD6
062200160201R211 C                   Clear                   FMTD7
062300160201R211 C                   Clear                   FMTD8
062400040305     C*
062500120502     C     OPZR75        IfNE      '01'
062600120502     C                   MoveL     XscSoc        N77Societa
062700120502     C                   Z-Add     AnnoR75       N77ACF
062800120502     C                   MoveL     QuaR75        N77Qua
062900120502     C                   MoveL     KccR75        N77Kcc
063000120502     C                   MoveL     KscR75        N77Ksc
063100120502     C                   MoveL     RgtR75        N77Rgt
063200120502     C                   MoveL     PrgR75        N77Prg
063300150626R186 C                   MoveL     C77R75        N77C77
063400120502     C                   MoveL     CogR75        N77Cog
063500120502     C                   MoveL     NomR75        N77Nom
063600150626R186 C                   MOVEL     CdfR75        N77Cdf
063700040305     C*
063800040305     C     *IN22         DOUEQ     *OFF
063900040305     C     MSGRTN        ORNE      'R'
064000040305     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
064100120502     C     OPZR75        IfEq      '02'
064200040305     C     OPZR75        OREQ      '04'
064300150626R186 X***  K09N7706      CHAIN     Nd77006                            2122
064400150626R186 C     K11N7720      CHAIN     Nd77020                            2122
064500040305     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
064600040305     C                   ELSE
064700150626R186 X***  K09N7706      CHAIN(N)  Nd77006                            2122
064800150626R186 C     K11N7720      CHAIN(N)  Nd77020                            2122
064900120502     C                   EndIf
065000040305     C*
065100040305     C* Gestione RCD allocato
065200120502     C     *IN22         IfEq      *ON
065300120502     C                   MoveL     'ND77000F'    MSGFIL
065400120502     C                   Exsr      MSGPGM
065500120502     C                   EndIf
065600040305     C                   ENDDO
065700040305     C*
065800040305     C* se il rcd da gestire non viene trovato
065900040305     C* oppure è allocato da altro pgm
066000040305     C* ritorno al pgm guida avvertendolo dell'errore
066100120502     C     *IN21         IfEq      *ON
066200040305     C     *IN22         OREQ      *ON
066300120502     C                   Move      *ON           $FINE
066400120502     C                   Move      '1'           ERRR75
066500040305     C*
066600040305     C* altrimenti
066700040305     C                   ELSE
066800990422     C* riempio la videata
066900120502     C                   Exsr      RIED1
067000040305     C* Aggiungere le routine di riempimento dei record succcessivi
067100120502     C                   Exsr      RIED2
067200010409C1478C* Aggiungere le routine di riempimento dei record succcessivi
067300120502C1478C                   Exsr      RIED3
067400150305R172 C*
067500150305R172 C                   Exsr      RIED5
067600160127R211 C*
067700160127R211 C                   Exsr      RIED6
067800160127R211 C*
067900160127R211 C                   Exsr      RIED7
068000160127R211 C*
068100160127R211 C                   Exsr      RIED8
068200990422     C* richiamo routine dei ctrl per decodificarla
068300120502     C                   Exsr      CTRD1
068400040305     C* Aggiungere le routine di controllo dei record succcessivi
068500120502     C                   Exsr      CTRD2
068600120502C1478C                   Exsr      CTRD3
068700160127R211 C                   Exsr      CTRD5
068800160127R211 C                   Exsr      CTRD6
068900160127R211 C                   Exsr      CTRD7
069000160127R211 C                   Exsr      CTRD8
069100120502     C                   Move      *ALL'0'       IN5098
069200120502     C                   MoveA     IN5098        *IN(50)
069300990422     C*
069400990422     C* imposto quegli attributi del sfl che sono modificabili
069500990422     C* al richiamo del determinato pgm
069600120502     C                   Exsr      ATRD1
069700120502     C                   EndIf
069800040305     C*
069900040305     C                   ELSE
070000990422     C*
070100990422     C* riempio con i default di immissione  (se reperiti)
070200990422     C                   IF        $FlgDef = *on
070300120502     C                   Exsr      RIEDEF
070400120502     C                   EndIf
070500990422     C*
070600990422     C* imposto i campi chiave ricevuti anche se si è in immissione
070700040305     C* perchè potrebbero essere stati passati dei default di chiave
070800120502     C                   Z-Add     AnnoR75       Vacf
070900120502     C                   MoveL     QuaR75        Qua
071000040305     C                   If        OpzR75   <> '01'
071100120502     C                   MoveL     KccR75        Kcc
071200120502     C                   MoveL     KscR75        Ksc
071300120502     C                   MoveL     RGTR75        Rgt
071400120502     C                   MoveL     PrgR75        Prg
071500150626R186 C                   MoveL     C77R75        C77
071600120502     C                   EndIf
071700040305     C*
071800120502     C                   Exsr      ATRD1
071900040305     C*
072000120502     C                   EndIf
072100040305     C*
072200120502     C                   EndSr
072300040305     C/EJECT
072400040305     C************************************************************
072500990422     C* RIEMPIMENTO VIDEATA  D1
072600990422     C************************************************************
072700120502     C     RIED1         BegSr
072800990422      *
072900990422     C* impostare i campi della videata dai campi del file
073000990422     C*
073100120502     C     OPZR75        IfEq      '01'
073200120502     c                   Z-Add     N77Acf        Vacf
073300120502     C                   MoveL     N77QUA        QUA
073400040305     C                   ElSe
073500120502     c                   Z-Add     N77Acf        Vacf
073600120502     C                   MoveL     N77QUA        QUA
073700120502     C                   MoveL     N77Kcc        Kcc
073800120502     C                   MoveL     N77Ksc        Ksc
073900120502     C                   MoveL     N77RGT        RGT
074000120502     C                   MoveL     N77PRG        PRG
074100120502     C                   MoveL     N77COG        COG
074200120502     C                   MoveL     N77CDF        CDF
074300120502     C                   MoveL     N77PFG        PFG
074400120502     C                   MoveL     N77CIT        CIT
074500120502     C                   MoveL     N77PRV        PRV
074600120502     C                   MoveL     N77VIA        VIA
074700160127R211 X****               MoveL     N77SER        SER
074800160203  "  X****               MoveL     N77CDE        CDE
074900160203  "  X****               MoveL     N77NUM        NUM
075000160203R211 X****               MoveL     N77CSE        CSE
075100160203     C                   MoveL     N77NOM        NOM
075200160203     C                   MoveL     N77SES        SES
075300160127     C                   MoveL     N77C77        C77
075400160203     C                   MoveL     N77CtgPrt     CTGPRT
075500160127R165 c                   if        N77Escludi = *On
075600150212 "   C                   MOVE      XscOn         ESCLUDI
075700150212 "   c                   else
075800150212 "   c                   move      XscOff        ESCLUDI
075900150212R165 c                   endif
076000160203R211 C                   MoveL     N77LNC        LNC
076100040305C1478x****               if        N77DNC        > *loval
076200160203     C                   MoveL     N77DNC        DataISO
076300160203A0684C                   IF        DataISO = *LOVAL
076400160203A0684C                   EVAL      DNC   = *ZEROS
076500160203A0684C                   ELSE
076600160203     C     *eur          MoveL     DataISO       DNC
076700160203     C                   EndIf
076800160203C1478x****               EndIf
076900160203     C                   MoveL     N77PNC        PNC
077000160203     C                   EndIf
077100120502C1478C                   MoveL     N77REG        Reg
077200160203C1478C                   MoveL     N77Tit        Titolo
077300160203C1478C                   MoveL     N77Anno       Anno
077400020408C1560C* utilizzati campi rappresentante (non più in uso) per estero
077500160127R211 X*** ex C1560       MoveL     N77RCIT       CitEst
077600160127R211 X*** ex C1560       MoveL     N77RVIA       ViaEst
077700160127R211 X*** ex C2014       MoveL     N77Erede      Erede
077800160127R211 X*** ex C2014       MoveL     N77Eventi     Eventi
077900160127R211 X*** ex C2014       Z-Add     N77IMPPrFa    ImpPrFa
078000160127R211 X*** ex c2014       Z-Add     N77IMPCuCo    ImpCuCo
078100160216R215 C                   Z-Add     N77IMPPrFa    ImpPrFa
078200160216R215 C                   Z-Add     N77IMPCuCo    ImpCuCo
078300990422     C*
078400140430R148 C                   MoveL     N77COM        COMUNE
078500140430R148 C*
078600170216R250 C                   MoveL     N77FUSCOM     FUSCOM
078700120502     C                   EndSr
078800990422     C************************************************************
078900990422     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
079000990422     C************************************************************
079100120502     C     ATRD1         BegSr
079200040305     C*
079300040305     C* in questa routine bisogna attivare indicatori o campi programma
079400040305     C* che gestiscono gli attributi di uno più campi di questa videata;
079500040305     C* attenzione : non è questo il posto in cui eseguire controlli,
079600040305     C*  dovrebbero già arrivare dei flag
079700040305     C*
079800040305     C                   SETOFF                                       02
079900040305     C* in caso di manutenzione
080000120502     C     OPZR75        IfEq      '02'
080100040305     C* proteggo i campi chiave
080200120502     C                   SetOn                                        02        PR campi key
080300120502     C                   EndIf
080400040305     C*
080500040305     x***                IF        OPZR75    = '01'  or
080600040305     x***                          OPZR75    = '03'
080700040305     x***                SETOFF                                       31
080800040305     x***                ELSE
080900120502     x***                SetOn                                        31
081000120502     x***                EndIf
081100990423     C*
081200120502     C                   EndSr
081300990422     C/EJECT
081400990422     C************************************************************
081500990422     C* RIEMPIMENTO CAMPI DAI DEFAULT
081600990422     C************************************************************
081700120502     C     RIEDEF        BegSr
081800990422     C*
081900040305     C* riempire i campi con i default recuperati dal codice di
082000040305     C* riferimento
082100990422     C*
082200120502     C                   EndSr
082300990422     C/EJECT
082400990422     C************************************************************
082500990422     C* GESTIONE F02 VIDEO D1
082600990422     C************************************************************
082700120502     C     F02D1         BegSr
082800990422     C*
082900990422     C* per decodificare la videata richiamo la routine dei ctrl
083000990422     C* e poi disattivo gli eventuali messaggi di errore
083100120502     C                   Exsr      CTRD1
083200990422     C*
083300120502     C                   Move      *ALL'0'       IN5098
083400120502     C                   MoveA     IN5098        *IN(50)
083500990422     C*
083600120502     C                   EndSr
083700990422     C/EJECT
083800990422     C************************************************************
083900990422     C* GESTIONE F03 VIDEO D1
084000990422     C************************************************************
084100120502     C     F03D1         BegSr
084200990422     C*
084300120502     C                   Move      *ON           $FINE
084400120502     C                   Move      '1'           RETR75
084500990422     C* fine programma
084600120502     C                   Exsr      ENDPGM
084700990422     C*
084800120502     C                   EndSr
084900990422     C/EJECT
085000990422     C************************************************************
085100990422     C* GESTIONE F05 VIDEO D1
085200990422     C************************************************************
085300120502     C     F05D1         BegSr
085400990422     C*
085500120502     C                   Move      *ON           $INZD1
085600990422     C*
085700120502     C                   EndSr
085800990422     C/EJECT
085900990422     C************************************************************
086000990422     C* GESTIONE F08 VIDEO D1
086100990422     C************************************************************
086200120502     C     F08D1         BegSr
086300990422     C*
086400120502     C                   Move      *ON           $FINE
086500120502     C                   Move      '0'           RETR75
086600990422     C*
086700120502     C                   EndSr
086800990422     C/EJECT
086900990422     C************************************************************
087000990422     C* GESTIONE F12 VIDEO D1
087100990422     C************************************************************
087200120502     C     F12D1         BegSr
087300990422     C*
087400120502     C                   Move      *ON           $FINE
087500120502     C                   Move      '2'           RETR75
087600990422     C*
087700120502     C                   EndSr
087800990422     C/EJECT
087900990422     C************************************************************
088000990422     C* GESTIONE F24 VIDEO D1
088100990422     C************************************************************
088200120502     C     F24D1         BegSr
088300990422     C*
088400120502     C                   MoveA     $FX1          $FX
088500120502     C                   MoveA     $FC1          $FC
088600120502     C                   MoveA     $FR1          $FR
088700120502     C                   Z-Add     $ULKD1        $ULK
088800120502     C                   Z-Add     $D1L01        $L01
088900120502     C                   Z-Add     $D1L02        $L02
089000120502     C                   Exsr      RIEKEY
089100120502     C                   Z-Add     $ULK          $ULKD1            3 0
089200120502     C                   MoveA     $FC           $FC1
089300120502     C                   MoveL     $KEY1         Z1KE1
089400120502C1913C                   MoveL     $ALLFUNCT     H1ALLFUNCT
089500120502     C                   MoveL     $KEY2         Z1KE2
089600990422     C                   WRITE     FMTZ1
089700990422     C*
089800120502     C                   EndSr
089900990422     C/EJECT
090000990422     C************************************************************
090100990422     C* CONTROLLO VIDEATA
090200990422     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
090300990422     C* 95 96 97 98
090400990422     C************************************************************
090500120502     C     CTRD1         BegSr
090600990422     C*
090700990422     C                   SETOFF                                       99
090800990423     C*
090900120502     C     OPZR75        IfNE      '04'
091000120502     C     OPZR75        AndNE     '05'
091100990422     C*
091200160203     C* allineo il codice conto
091300160203     C                   If        *in02 = *off
091400160203     C     Kcc           IfNE      'C'
091500160203     C     Kcc           AndNE     'F'
091600160203     C                   IF        Kcc   <>   *Blank
091700160203     C                   CallB     'XALLINEA'
091800160203     C                   Parm                    Kcc
091900160203     C                   Parm      6             LEN               5 0
092000160203     C                   Parm                    XSCINP
092100160203     C                   EndIf
092200160203     C                   EndIf
092300160203     C*
092400160203     C                   IF        Ksc  <>   *Blank
092500160203     C                   CallB     'XALLINEA'
092600160203     C                   Parm                    Ksc
092700160203     C                   Parm      8             LEN               5 0
092800160203     C                   Parm                    XSCINP
092900160203     C                   EndIf
093000160203     C                   EndIf
093100160203     C*
093200160203R024 C* Recupero capoconto
093300160203  "  C     Kcc           IfEq      'C       '
093400160203  "  C     Kcc           OREQ      'F       '
093500160203  "  C                   CallB     'XCAPCLIFOR'
093600160203  "  C                   Parm                    xscsoc
093700160203  "  C     Kcc           Parm                    Kcc
093800160203  "  C                   Parm                    Ksc
093900160203  "  C                   EndIf
094000160203R024 C*
094100160203R165 C*
094200160203R165 C                   If        Kcc       <>   *blanks  and
094300160203R165 C                             Ksc       <>   *blanks
094400160203     C                   Eval      $Kcc      =   Kcc
094500160203     C                   Eval      $Ksc      =   Ksc
094600160203     C                   Exsr      CallMVC002
094700160203     C                   IF        $Esito    =   *Off
094800160203     C                   MoveL     DesBrev002    D1KscDd
094900160203     C                   If        Sogg002   =   *Blank
095000160203      *  Conto non gestito a soggetti
095100160203R165 C**N02              SetOn                                        6099
095200160203R165 C  N02              SetOn                                        4099
095300160203     C                   ElSe
095400160203     C     NOM           IfNE      *BLANK
095500160203     C     DNC           ORNE      0
095600160203     C     SES           ORNE      *BLANK
095700160203     C     LNC           ORNE      *BLANK
095800160203     C     PNC           ORNE      *BLANK
095900160203     C                   Exsr      CallDvaSog
096000160203     C                   If        DvaErrore =  *Off
096100160203     C                   Eval      %subst(DVASOG:1:DVALENOUT)
096200160203     C                             = %subst(DVAOUTPUT:1:DVALENOUT)
096300160203D1183X****               If        DVSNatGiur    <> '99'
096400160203D1183C                   Move      '1'           XTBRIC
096500160203  "  C                   Move      XscSoc        XTBAZI
096600160203  "  C                   Move      *ZERO         XTBKEY
096700160203  "  C                   Move      'G16'         XTBCOD
096800160203  "  C                   Move      DVSNATGIUR    XTBKEY
096900160203  "  C                   Callb     'XATB'
097000160203  "  C                   Parm                    XATBDS
097100160203  "  C     XTBERR        IfEq      '0'
097200160203  "  C                   MoveL     XTBUNI        TABG16
097300160203D1183C                   IF        TPNG16 <> '1'
097400160203     C                   SetOn                                        33
097500160203D1183C                   EndIf
097600160203     C                   EndIf
097700160203     C                   EndIf
097800160203     C                   EndIf
097900160203     C                   EndIf
098000160203     C                   ElSe
098100160203R165 C**n02              SetOn                                        6299
098200160203R165 C  n02              SetOn                                        4299
098300160203     C                   EndIf
098400160203     C*
098500160203R165 C                   EndIf
098600160203R165 C*
098700160203     C     NOM           IfNE      *BLANK
098800160203     C     DNC           ORNE      0
098900160203     C     SES           ORNE      *BLANK
099000160203     C     LNC           ORNE      *BLANK
099100160203     C     PNC           ORNE      *BLANK
099200160203     C     *eur          Test(D)                 DNC                    21
099300160203     C                   if        *In21     =   *On
099400160203     C* data errata
099500160203     C                   SetOn                                        3199
099600160203     C                   EndIf
099700160203     C                   EndIf
099800160203     C*
099900160203R211 X*** Ex C2078 C* Casella EREDE valori ammessi "/" o " "
100000160128  "  X*** Ex C2078       IF        Erede  <> '/' and
100100160128  "  X*** Ex C2078                 Erede  <> ' '
100200160128  "  X*** Ex C2078       SetOn                                        3299
100300160203  "  X*** Ex C2078       EndIf
100400160203  "  X*** Ex C2078 C* Casella Anticipazione valori ammessi "/" o " "
100500160203  "  X*** Ex C2078       IF        Titolo  <> '/' and
100600160203  "  X*** Ex C2078                 Titolo  <> ' '
100700160203  "  X*** Ex C2078       SetOn                                        3499
100800170216R211 X*** Ex C2078       EndIf
100900150619R186 C* Controllo congruenza causale / comune
101000150616  "  C                   IF        C77     = 'N '    and
101100150616  "  C                             Comune  =  *blanks
101200150616  "  C                   SetOn                                        3799
101300150619R186 C                   EndIf
101400160128R211 X*** ex R186* Casella Anticipazione valori ammessi "/" o " "
101500160128R211 C* Esclusione dalla certificazione
101600160128R186 C                   IF        ESCLUDI <> XscOn   and
101700150212  "  C                             Escludi <> XscOff
101800150616  "  C                   SetOn                                        3599
101900150212  "  C                   EndIf
102000150212  "  C*
102100150212  "  C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
102200150212  "  C     OPZR75        IFEQ      '01'
102300150212  "  C     OPZR75        OREQ      '03'
102400150212  "  C                   MoveL     XscSoc        N77Societa
102500150212  "  C                   Z-Add     VACF          N77ACF
102600150212  "  C                   MoveL     Qua           N77Qua
102700150212  "  C                   MoveL     Kcc           N77Kcc
102800150212  "  C                   MoveL     Ksc           N77Ksc
102900150212  "  C                   MoveL     Rgt           N77Rgt
103000150626R165 C                   MoveL     Prg           N77Prg
103100150626R186 C                   MoveL     C77           N77C77
103200150626R165 C                   MoveL     Cog           N77Cog
103300150626R165 C                   MoveL     Nom           N77Nom
103400150626R186 X***  K09N7706      SETLL     ND77006L                               21
103500150626R186 C     K11N7720      SETLL     ND77020L                               21
103600150626R165 C     *IN21         IFEQ      '1'
103700150212  "  C                   SETON                                        3699
103800150212  "  C                   ENDIF
103900150212  "  C                   ENDIF
104000150212R165 C*
104100080201     C*
104200120502     C                   EndIf
104300990422     C*
104400120502     C                   EndSr
104500990422     C/EJECT
104600990423      ************************************************************
104700990423      * Chiamata al driver DVASOG per reperimento indirizzo     *
104800990423      ************************************************************
104900120502     C     CallDvaSog    BegSr
105000990423      *
105100120502     C                   Move      '1'           DvaRic
105200120502     C                   Move      XScSoc        DvaSocieta
105300120502     C                   Move      *Blank        DvaUnita
105400120502     C                   MoveL     'DVASOG'      DVAstrutt
105500990423     C                   EVAL      DvaLenOut = %Size(DVASOG)
105600120502     C                   Move      Sogg002       DvaSogg
105700120502     C                   Move      *blanks       DvaTpInd
105800120502     C                   Move      *blanks       DvaCdInd
105900990423      *
106000120502     C                   CallB     'NDDVASOG'    PDVASOG
106100990423      *
106200120502     C     XCalDvaSog    EndSr
106300990423     C/EJECT
106400990423     C************************************************************
106500990422     C* GESTIONE VIDEO RECORD  D2
106600990422     C************************************************************
106700120502    >C     GESD2         BegSr
106800990422     C* emissione piede videata se proveniente da altra
106900990422     C* salvataggio valore formato di provenienza
107000160128R211 X***  $LASTV        IfNE      'D3'
107100160128R211 C     $LASTV        IfNE      'D2'
107200990422     C                   WRITE     FMTT1
107300120502     C                   Exsr      GESPIE
107400120502     C                   Move      $LASTV        $LASTG
107500160128R211 X***                Move      'D3'          $LASTV
107600160128R211 C                   Move      'D2'          $LASTV
107700120502     C                   EndIf
107800040305     C*
107900040305     C* se esistono errori sulla videata
108000040305     C* emetto la write del formato a indicatori spenti per vedere
108100040305     C* le eventuali decodifiche
108200120502     C     *IN99         IfEq      *ON
108300040305     C     $GEST         ANDEQ     'D2'
108400120502     C                   MoveA     *IN           WIN
108500120502     C                   Move      *ALL'0'       IN5098           49
108600120502     C                   MoveA     IN5098        *IN(50)
108700040305     C                   WRITE     FMTD2
108800120502     C                   MoveA     WIN           *IN
108900120502     C                   EndIf
109000040305     C* Inizializzazione indice tasti funzione personalizzati
109100040305     C                   Eval      PosFun = 25
109200990422     C*
109300060622C1943X*ex C1560
109400060622  "  X*ex C1560 visualizzo sempre tutti i dati
109500120502  "  X*ex C1560          Move      *on           *in78
109600060622C1943X*ex C1560
109700120502     C                   Select
109800990422     C* Annullamento
109900120502     C     OPZR75        WhenEq    '04'
110000990422     C* Visualizzazione
110100990422     C     OPZR75        OREQ      '05'
110200990422     C                   WRITE     FMTD2
110300990422     C                   EXFMT     PROTECT
110400990422     C* Immissione/Modifica/Copia
110500040305     C                   OTHER
110600990422     C                   EXFMT     FMTD2
110700120502     C                   EndSl
110800990422     C*
110900990422     C* azzero la posizione del cursore
111000120502     C                   Z-Add     0             H1RIGA
111100120502     C                   Z-Add     0             H1COLO
111200990422     C*
111300990422     C* controllo abilitazione tasti funzione
111400120502     C                   Exsr      CTRKD1
111500990422     C*
111600120502     C     *IN99         IfEq      *OFF
111700990422     C*
111800120502     C                   Select
111900010328C1478C* ROLLUP
112000120502  "  C     $TASTO        WhenEq    ROLLUP
112100120502C1478C                   Move      'D3'          $GEST
112200990422     C* ROLDOWN
112300120502     C     $TASTO        WhenEq    ROLDWN
112400160128R211 X***                Move      'D1'          $GEST
112500160128R211 C                   Move      'D6'          $GEST
112600990422     C* F3=Fine
112700120502     C     $TASTO        WhenEq    F03
112800120502     C                   Exsr      F03D1
112900990422     C* F4=Ricerca
113000120502     C     $TASTO        WhenEq    F04
113100120502     C                   Exsr      SEARCH
113200990422     C* F5=Refresh
113300120502     C     $TASTO        WhenEq    F05
113400120502     C                   Exsr      F05D2
113500990422     C* F8=Successivo
113600120502     C     $TASTO        WhenEq    F08
113700120502     C                   Exsr      F08D1
113800990422     C* F12=Ritorno
113900120502     C     $TASTO        WhenEq    F12
114000120502     C                   Exsr      F12D1
114100990422     C* F24=Altri tasti
114200120502     C     $TASTO        WhenEq    F24
114300120502     C                   Exsr      F24D2
114400990422     C* Personalizzata
114500990422     C     $PGF1(PosFun) Whenne    *BLANK
114600120502     C                   Exsr      FperD1
114700990422     C*
114800040305     C                   OTHER
114900120502     C                   Exsr      CTRD2
115000120502     C     *IN99         IfEq      *OFF
115100120502     C                   Exsr      CTRD1
115200160128R211 C     *IN99         IfEq      *OFF
115300160128  "  C                   Exsr      CTRD5
115400160128  "  C     *IN99         IfEq      *OFF
115500160128  "  C                   Exsr      CTRD6
115600160128  "  C     *IN99         IfEq      *OFF
115700160128  "  C                   Exsr      CTRD7
115800160128  "  C     *IN99         IfEq      *OFF
115900160128R211 C                   Exsr      CTRD8
116000120502C1943C     *IN99         IfEq      *Off
116100120502C1478C                   Exsr      CTRD3
116200120502C1943C                   EndIf
116300120502  "  C                   EndIf
116400060622C1943C*
116500160128R211 C                   Endif
116600160128  "  C                   Endif
116700160128  "  C                   Endif
116800160128  "  C                   Endif
116900160128R211 C*
117000120502     C     *IN99         IfEq      *ON
117100990422     C* Spengo gli indicatori di errore per il controllo su altri schermi
117200990422     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
117300990422     C* videate
117400120502     C                   Move      *ALL'0'       IN5098           49
117500120502     C                   MoveA     IN5098        *IN(50)
117600120502     C                   SetOn                                        9996                       96
117700120502     C                   EndIf
117800120502C1943X****               EndIf
117900990422     C*
118000990422     C*
118100120502     C     $TASTO        IfEq      F06
118200990422     C     *IN99         ANDEQ     *OFF
118300990422     C* eseguo aggiornamento
118400120502     C                   Exsr      AGGANA
118500990422     C* eseguo operazioni del dopo-aggiornamento
118600120502     C     *IN99         IfEq      *OFF
118700120502     C                   Exsr      GESAGG
118800120502     C                   EndIf
118900120502     C                   EndIf
119000040305     C*
119100120502     C                   EndSl
119200040305     C*
119300120502     C                   EndIf
119400040305     C*
119500120502     C                   EndSr
119600040305     C/EJECT
119700040305     C************************************************************
119800990422     C* RIEMPIMENTO VIDEATA D2
119900990422     C************************************************************
120000120502     C     RIED2         BegSr
120100990422     C*
120200120502C1478x****               Z-Add     N77PRA        PRA
120300120502     C                   Z-Add     N77ITC        ITC
120400120502     C                   Z-Add     N77SNS        SNS
120500120502     C                   Z-Add     N77IMN        IMN
120600120502     C                   Z-Add     N77IMS        IMS
120700120502     C                   Z-Add     N77RAC        RAC
120800120502C1770C                   Z-Add     N77RIM        RIM
120900120502     C                   Z-Add     N77NET        NET
121000120502C1478C                   Z-Add     N77ARG        ARG
121100120502C1770C                   Z-Add     N77ARI        ARI
121200120502C1478C                   Z-Add     N77ARS        ARS
121300120502C1478C                   Z-Add     N77IMSAP      IMSAP
121400120502C1478C                   Z-Add     N77RACAP      RACAP
121500160202R211 X****               Z-Add     N77CTBDL      CTBDL
121600160202R211 X****               Z-Add     N77CTBLV      CTBLV
121700120502C1943C                   Z-Add     N77SpeRimb    SpeRimb
121800120502C1943C                   Z-Add     N77RitRimb    RitRimb
121900130206R110 C                   Move      N77Codice     Codice
122000140508R148 C*
122100140508R148 C* --- Add.Comunali: dati relativi alle somme erogate
122200140508R148 C*
122300140507R148 C* Add.Comunale
122400140507R148 C                   Z-Add     N77ACG        ACG
122500140507R148 C* Add.Comunale a titolo d'acconto
122600140507R148 C                   Z-Add     N77ACTA       ACTA
122700140507R148 C* Add.Comunale sospesa
122800140507R148 C                   Z-Add     N77ACS        ACS
122900140508     C*
123000120502     C                   EndSr
123100990422     C/EJECT
123200990422     C************************************************************
123300990422     C* GESTIONE F05 VIDEO D2
123400990422     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
123500990422     C* e il riposizionamento alla prima
123600990422     C************************************************************
123700120502     C     F05D2         BegSr
123800990422     C*
123900120502     C                   Move      *ON           $INZD1
124000120502     C                   Move      'D1'          $GEST
124100990422     C*
124200120502     C                   EndSr
124300990422     C************************************************************
124400990422     C* GESTIONE F24 VIDEO D2
124500990422     C************************************************************
124600120502     C     F24D2         BegSr
124700990422     C*
124800120502     C                   MoveA     $FX1          $FX
124900120502     C                   MoveA     $FC1          $FC
125000120502     C                   MoveA     $FR1          $FR
125100120502     C                   Z-Add     $ULKD1        $ULK
125200120502     C                   Z-Add     $D1L01        $L01
125300120502     C                   Z-Add     $D1L02        $L02
125400120502     C                   Exsr      RIEKEY
125500120502     C                   Z-Add     $ULK          $ULKD1            3 0
125600120502     C                   MoveA     $FC           $FC1
125700120502     C                   MoveL     $KEY1         Z1KE1
125800120502C1913C                   MoveL     $ALLFUNCT     H1ALLFUNCT
125900120502     C                   MoveL     $KEY2         Z1KE2
126000990422     C                   WRITE     FMTZ1
126100990422     C*
126200120502     C                   EndSr
126300990422     C************************************************************
126400990422     C* CONTROLLO VIDEATA  D2
126500990422     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
126600990422     C* 95 96 97 98
126700990422     C************************************************************
126800120502     C     CTRD2         BegSr
126900990422     C*
127000990422     C                   SETOFF                                       99
127100990422     C*
127200990422     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
127300990422     C*
127400120502     C     OPZR75        IfNE      '04'
127500990422     C*
127600170216R250 C* Controllo importi negativi
127700170216  "  C     ITC           comp      0                                    54
127800170216  "  C     SNS           comp      0                                    55
127900170216  "  C     IMN           comp      0                                    56
128000170216  "  C     IMS           comp      0                                    58
128100170216R250 C     RAC           comp      0                                    59
128200990422     C*
128300120502     C                   EndIf
128400990422     C*
128500120502     C                   EndSr
128600990422     C/EJECT
128700010328C1478C************************************************************
128800010328     C* GESTIONE VIDEO RECORD  D3
128900010328     C************************************************************
129000120502     C     GESD3         BegSr
129100010328     C* emissione piede videata se proveniente da altra
129200010328     C* salvataggio valore formato di provenienza
129300120502     C     $LASTV        IfNE      'D3'
129400040305     C                   WRITE     FMTT1
129500120502     C                   Exsr      GESPIE
129600120502     C                   Move      $LASTV        $LASTG
129700120502     C                   Move      'D3'          $LASTV
129800120502     C                   EndIf
129900040305     C*
130000040305     C* se esistono errori sulla videata
130100040305     C* emetto la write del formato a indicatori spenti per vedere
130200040305     C* le eventuali decodifiche
130300120502     C     *IN99         IfEq      *ON
130400040305     C     $GEST         ANDEQ     'D3'
130500120502     C                   MoveA     *IN           WIN
130600120502     C                   Move      *ALL'0'       IN5098           49
130700120502     C                   MoveA     IN5098        *IN(50)
130800010328     C                   WRITE     FMTD3
130900120502     C                   MoveA     WIN           *IN
131000120502     C                   EndIf
131100040305     C* Inizializzazione indice tasti funzione personalizzati
131200040305     C                   Eval      PosFun = 25
131300040305     C*
131400120502     C                   Select
131500040305     C* Annullamento
131600120502     C     OPZR75        WhenEq    '04'
131700040305     C* Visualizzazione
131800040305     C     OPZR75        OREQ      '05'
131900040305     C                   WRITE     FMTD3
132000040305     C                   EXFMT     PROTECT
132100040305     C* Immissione/Modifica/Copia
132200040305     C                   OTHER
132300040305     C                   EXFMT     FMTD3
132400120502     C                   EndSl
132500040305     C*
132600040305     C* azzero la posizione del cursore
132700120502     C                   Z-Add     0             H1RIGA
132800120502     C                   Z-Add     0             H1COLO
132900040305     C*
133000040305     C* controllo abilitazione tasti funzione
133100120502     C                   Exsr      CTRKD1
133200040305     C*
133300120502     C     *IN99         IfEq      *OFF
133400040305     C*
133500120502     C                   Select
133600040305     C* ROLDOWN
133700120502     C     $TASTO        WhenEq    ROLDWN
133800160201     C                   Move      'D2'          $GEST
133900150305R172 C* ROLLUP
134000150305R172 C     $TASTO        WhenEq    ROLLUP
134100160128R211 X**** ex R172       Move      'D5'          $GEST
134200160128R211 C                   Move      'D7'          $GEST
134300010328     C* F3=Fine
134400120502     C     $TASTO        WhenEq    F03
134500120502     C                   Exsr      F03D1
134600010328     C* F4=Ricerca
134700120502     C     $TASTO        WhenEq    F04
134800120502     C                   Exsr      SEARCH
134900010328     C* F5=Refresh
135000120502     C     $TASTO        WhenEq    F05
135100120502     C                   Exsr      F05D3
135200010328     C* F8=Successivo
135300120502     C     $TASTO        WhenEq    F08
135400120502     C                   Exsr      F08D1
135500010328     C* F12=Ritorno
135600120502     C     $TASTO        WhenEq    F12
135700120502     C                   Exsr      F12D1
135800010328     C* F24=Altri tasti
135900120502     C     $TASTO        WhenEq    F24
136000120502     C                   Exsr      F24D3
136100010328     C* Personalizzata
136200010328     C     $PGF1(PosFun) Whenne    *BLANK
136300120502     C                   Exsr      FperD1
136400040305     C*
136500040305     C                   OTHER
136600120502     C                   Exsr      CTRD3
136700120502     C     *IN99         IfEq      *OFF
136800120502     C                   Exsr      CTRD1
136900160128R211 C     *IN99         IfEq      *OFF
137000160128  "  C                   Exsr      CTRD5
137100160128  "  C     *IN99         IfEq      *OFF
137200160128  "  C                   Exsr      CTRD6
137300160128  "  C     *IN99         IfEq      *OFF
137400160128  "  C                   Exsr      CTRD7
137500160128  "  C     *IN99         IfEq      *OFF
137600160128  "  C                   Exsr      CTRD8
137700120502C1943C     *IN99         IfEq      *OFF
137800120502     C                   Exsr      CTRD2
137900120502C1943C                   EndIf
138000120502  "  C                   EndIf
138100060622C1943C*
138200160128R211 C                   EndIf
138300160128  "  C                   EndIf
138400160128  "  C                   EndIf
138500160128R211 C                   EndIf
138600120502     C     *IN99         IfEq      *ON
138700010328     C* Spengo gli indicatori di errore per il controllo su altri schermi
138800010328     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
138900010328     C* videate
139000120502     C                   Move      *ALL'0'       IN5098           49
139100120502     C                   MoveA     IN5098        *IN(50)
139200120502     C                   SetOn                                        9996                       96
139300120502     C                   EndIf
139400120502C1943X****               EndIf
139500010328     C*
139600010328     C*
139700120502     C     $TASTO        IfEq      F06
139800040305     C     *IN99         ANDEQ     *OFF
139900040305     C* eseguo aggiornamento
140000120502     C                   Exsr      AGGANA
140100040305     C* eseguo operazioni del dopo-aggiornamento
140200120502     C     *IN99         IfEq      *OFF
140300120502     C                   Exsr      GESAGG
140400120502     C                   EndIf
140500120502     C                   EndIf
140600040305     C*
140700120502     C                   EndSl
140800040305     C*
140900120502     C                   EndIf
141000040305     C*
141100120502     C                   EndSr
141200040305     C/EJECT
141300100514R024 C************************************************************
141400010328     C* RIEMPIMENTO VIDEATA D3
141500100514R024 C************************************************************
141600120502 "   C     RIED3         BegSr
141700100514 "   C*
141800120502R024 C                   MoveL     N77ASCDF      R3CDF
141900120502R055 C*** ex R024        Z-Add     N77ASIMP      R3IMP
142000120502R055 C                   Movel     N77CauAS      R3Causale
142100120502R024 C                   Z-Add     N77ASIMS      R3IMS
142200120502 "   C                   Z-Add     N77ASRAC      R3RAC
142300120502 "   C                   Z-Add     N77ASRIM      R3RIM
142400120502 "   C                   Z-Add     N77ASRSO      R3RSO
142500120502 "   C                   Z-Add     N77ASARA      R3ARA
142600120502 "   C                   Z-Add     N77ASARI      R3ARI
142700140508R024 C                   Z-Add     N77ASARS      R3ARS
142800140508R148 C*
142900140508R148 C* --- Add.Comunali: redditi erogati da altri soggetti
143000140508R148 C*
143100140508R148 C* Add.Comunale
143200140508R148 C                   Z-Add     N77ASACA      R3ACA
143300140508R148 C* Add.Comunale a titolo d'acconto
143400140508R148 C                   Z-Add     N77ASACI      R3ACI
143500140508R148 C* Add.Comunale sospesa
143600140508R148 C                   Z-Add     N77ASACS      R3ACS
143700140508R024 C*
143800120502R024 C                   EndSr
143900100514     C************************************************************
144000100514     C* RIEMPIMENTO VIDEATA D4 (ex D3)
144100100514     C************************************************************
144200120502R024 C**** RIED3         BegSr
144300120502R024 C     RIED4         BegSr
144400100514     C*
144500120502C1478C                   Move      N77PIn        PIn
144600120502  "  C                   MoveL     N77DtIn       GioI
144700120502  "  C                   Move      N77DtIn       MeseI
144800120502  "  C                   MoveL     N77DtFi       GioF
144900140507c1478C                   Move      N77DtFi       MeseF
145000140507R148 C**** Ex c1478      Move      N77COM        Com
145100140507c1478C                   MoveL     N77RCDF       RCDF
145200120502  "  C                   MoveL     N77RCSE       RCSE
145300120502  "  C                   MoveL     N77RCOG       RCOG
145400120502  "  C                   MoveL     N77RNOM       RNOM
145500120502  "  C                   MoveL     N77RSES       RSES
145600120502  "  C                   MoveL     N77RLNC       RLNC
145700100514  "  x****               If        N77RDNC        > *loval
145800120502c1478C                   MoveL     N77RDNC       DataISO
145900100514A0684C                   IF        DataISO = *LOVAL
146000100514A0684C                   EVAL      RDNC   = *ZEROS
146100100514  "  C                   Else
146200120502  "  C     *eur          MoveL     DataISO       RDNC
146300120502a0684C                   EndIf
146400120502C1478C                   MoveL     N77RPNC       RPNC
146500120502C1478C                   MoveL     N77RPRV       RPRV
146600100514C1478C*
146700120502     C                   EndSr
146800100514     C/EJECT
146900010328     C************************************************************
147000010328     C* GESTIONE F05 VIDEO D3
147100040305     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
147200040305     C* e il riposizionamento alla prima
147300040305     C************************************************************
147400120502     C     F05D3         BegSr
147500040305     C*
147600120502     C                   Move      *ON           $INZD1
147700120502     C                   Move      'D1'          $GEST
147800040305     C*
147900120502     C                   EndSr
148000040305     C************************************************************
148100040305     C* GESTIONE F24 VIDEO D3
148200040305     C************************************************************
148300120502     C     F24D3         BegSr
148400010328     C*
148500120502     C                   MoveA     $FX1          $FX
148600120502     C                   MoveA     $FC1          $FC
148700120502     C                   MoveA     $FR1          $FR
148800120502     C                   Z-Add     $ULKD1        $ULK
148900120502     C                   Z-Add     $D1L01        $L01
149000120502     C                   Z-Add     $D1L02        $L02
149100120502     C                   Exsr      RIEKEY
149200120502     C                   Z-Add     $ULK          $ULKD1            3 0
149300120502     C                   MoveA     $FC           $FC1
149400120502     C                   MoveL     $KEY1         Z1KE1
149500120502C1913C                   MoveL     $ALLFUNCT     H1ALLFUNCT
149600120502     C                   MoveL     $KEY2         Z1KE2
149700010328     C                   WRITE     FMTZ1
149800010328     C*
149900120502     C                   EndSr
150000100514R024 C************************************************************
150100100514R024 C* CONTROLLO VIDEATA  D3
150200100514  "  C* non usare i seguenti indicatori (perchè relativi a D1MSG):
150300100514  "  C* 95 96 97 98
150400100514  "  C************************************************************
150500120502  "  C     CTRD3         BegSr
150600100514  "  C*
150700100514  "  C*
150800120502R024 C                   EndSr
150900100514C1478C************************************************************
151000100514C1478C* CONTROLLO VIDEATA  D4 (ex D3)
151100100514     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
151200100514     C* 95 96 97 98
151300100514     C************************************************************
151400120502R024 C***  CTRD3         BegSr
151500120502R024 C     CTRD4         BegSr
151600100514     C*
151700010328     C                   SETOFF                                       99
151800010328     C*
151900010328     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
152000010328     C*
152100120502     C     OPZR75        IfNE      '04'
152200010328     C*
152300010404     C                   if        MeseI <> 0 or
152400010404     C                             MeseF <> 0 or
152500010404     C                             GioI  <> 0 or
152600010404     C                             GioF  <> 0
152700010328     C* Periodo inizio
152800010404     C                   if        MeseI < 1 or meseI > 12
152900120502     C                   SetOn                                        5099
153000120502     C                   EndIf
153100010404     C                   if        GioI = 0
153200120502     C                   SetOn                                        5199
153300120502     C                   EndIf
153400010328     C*
153500010328     C* Periodo fine
153600010404     C                   if        MeseF < 1 or MeseF > 12
153700120502     C                   SetOn                                        5299
153800120502     C                   EndIf
153900010404     C                   if        GioF = 0
154000120502     C                   SetOn                                        5399
154100120502     C                   EndIf
154200010328     C*
154300010328     C*
154400010328     C* Congruenza periodi inizio/fine
154500120502     C                   Move      GioI          periodoi          6 0
154600120502     C                   MoveL     MeseI         periodoi
154700120502     C                   Move      GioF          periodof          6 0
154800120502     C                   MoveL     Mesef         periodof
154900010328     C                   if        periodoi > periodof
155000120502     C                   SetOn                                        6599
155100120502     C                   EndIf
155200010328     C*
155300120502     C                   EndIf
155400010328     C*
155500120502     C                   EndIf
155600010328     C*
155700120502     C                   EndSr
155800010328C1478C/EJECT
155900150305R172 C************************************************************
156000150305  "  C* GESTIONE VIDEO RECORD  D5
156100150305  "  C************************************************************
156200150305R172 C     GESD5         BegSr
156300150305     C* emissione piede videata se proveniente da altra
156400150305     C* salvataggio valore formato di provenienza
156500150305     C     $LASTV        IfNE      'D5'
156600150305     C                   WRITE     FMTT1
156700150305     C                   Exsr      GESPIE
156800150305     C                   Move      $LASTV        $LASTG
156900150305     C                   Move      'D5'          $LASTV
157000150305     C                   EndIf
157100150305     C*
157200150305     C* se esistono errori sulla videata
157300150305     C* emetto la write del formato a indicatori spenti per vedere
157400150305     C* le eventuali decodifiche
157500150305     C     *IN99         IfEq      *ON
157600150305     C     $GEST         ANDEQ     'D5'
157700150305     C                   MoveA     *IN           WIN
157800150305     C                   Move      *ALL'0'       IN5098           49
157900150305     C                   MoveA     IN5098        *IN(50)
158000150305     C                   WRITE     FMTD5
158100150305     C                   MoveA     WIN           *IN
158200150305     C                   EndIf
158300150305     C* Inizializzazione indice tasti funzione personalizzati
158400150305     C                   Eval      PosFun = 25
158500150305     C*
158600150305     C                   Select
158700150305     C* Annullamento
158800150305     C     OPZR75        WhenEq    '04'
158900150305     C* Visualizzazione
159000150305     C     OPZR75        OREQ      '05'
159100150305     C                   WRITE     FMTD5
159200150305     C                   EXFMT     PROTECT
159300150305     C* Immissione/Modifica/Copia
159400150305     C                   OTHER
159500150305     C                   EXFMT     FMTD5
159600150305     C                   EndSl
159700150305     C*
159800150305     C* azzero la posizione del cursore
159900150305     C                   Z-Add     0             H1RIGA
160000150305     C                   Z-Add     0             H1COLO
160100150305     C*
160200150305     C* controllo abilitazione tasti funzione
160300150305     C                   Exsr      CTRKD1
160400150305     C*
160500150305     C     *IN99         IfEq      *OFF
160600150305     C*
160700150305     C                   Select
160800150305     C* ROLDOWN
160900150305     C     $TASTO        WhenEq    ROLDWN
161000160128R211 X***                Move      'D3'          $GEST
161100160128R211 C                   Move      'D8'          $GEST
161200150305     C* F3=Fine
161300150305     C     $TASTO        WhenEq    F03
161400150305     C                   Exsr      F03D1
161500150305     C* F4=Ricerca
161600150305     C     $TASTO        WhenEq    F04
161700150305     C                   Exsr      SEARCH
161800150305     C* F5=Refresh
161900150305     C     $TASTO        WhenEq    F05
162000150305     C                   Exsr      F05D5
162100150305     C* F8=Successivo
162200150305     C     $TASTO        WhenEq    F08
162300150305     C                   Exsr      F08D1
162400150305     C* F12=Ritorno
162500150305     C     $TASTO        WhenEq    F12
162600150305     C                   Exsr      F12D1
162700150305     C* F24=Altri tasti
162800150305     C     $TASTO        WhenEq    F24
162900150305     C                   Exsr      F24D5
163000150305     C* Personalizzata
163100150305     C     $PGF1(PosFun) Whenne    *BLANK
163200150305     C                   Exsr      FperD1
163300150305     C*
163400150305     C                   OTHER
163500150305     C                   Exsr      CTRD5
163600150305     C     *IN99         IfEq      *OFF
163700150305     C                   Exsr      CTRD1
163800150305     C     *IN99         IfEq      *OFF
163900150305     C                   Exsr      CTRD2
164000150305     C     *IN99         IfEq      *OFF
164100150305     C                   Exsr      CTRD3
164200160128R211 C     *IN99         IfEq      *OFF
164300160128  "  C                   Exsr      CTRD6
164400160128  "  C     *IN99         IfEq      *OFF
164500160128  "  C                   Exsr      CTRD7
164600160128  "  C     *IN99         IfEq      *OFF
164700160128R211 C                   Exsr      CTRD8
164800150305     C                   EndIf
164900150305     C                   EndIf
165000150305     C                   EndIf
165100160128R211 C                   EndIf
165200160128  "  C                   EndIf
165300160128R211 C                   EndIf
165400150305     C*
165500150305     C     *IN99         IfEq      *ON
165600150305     C* Spengo gli indicatori di errore per il controllo su altri schermi
165700150305     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
165800150305     C* videate
165900150305     C                   Move      *ALL'0'       IN5098           49
166000150305     C                   MoveA     IN5098        *IN(50)
166100150305     C                   SetOn                                        9996                       96
166200150305     C                   EndIf
166300150305     C*
166400150305     C*
166500150305     C     $TASTO        IfEq      F06
166600150305     C     *IN99         ANDEQ     *OFF
166700150305     C* eseguo aggiornamento
166800150305     C                   Exsr      AGGANA
166900150305     C* eseguo operazioni del dopo-aggiornamento
167000150305     C     *IN99         IfEq      *OFF
167100150305     C                   Exsr      GESAGG
167200150305     C                   EndIf
167300150305     C                   EndIf
167400150305     C*
167500150305     C                   EndSl
167600150305     C*
167700150305     C                   EndIf
167800150305     C*
167900150305R172 C                   EndSr
168000150305  "  C************************************************************
168100150305  "  C* RIEMPIMENTO VIDEATA D5
168200150305  "  C************************************************************
168300150305R172 C     RIED5         BegSr
168400150305     C*
168500150305     C                   Z-add     N77PROTCEI    PROTCEI
168600150305     C                   Z-add     N77PROTOC     PROTOC
168700150305     C                   Z-add     N77PROTCEA    PROTCEA
168800150305     C*
168900150305R172 C                   Endsr
169000150305  "  C/EJECT
169100150305  "  C************************************************************
169200150305  "  C* GESTIONE F05 VIDEO D5
169300150305  "  C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
169400150305  "  C* e il riposizionamento alla prima
169500150305  "  C************************************************************
169600150305R172 C     F05D5         BegSr
169700150305     C*
169800150305     C                   Move      *ON           $INZD1
169900150305     C                   Move      'D1'          $GEST
170000150305     C*
170100150312R172 C/EJECT
170200150312  "  C                   EndSr
170300150305  "  C************************************************************
170400150305  "  C* GESTIONE F24 VIDEO D5
170500150305  "  C************************************************************
170600150305R172 C     F24D5         BegSr
170700150305     C*
170800150305     C                   MoveA     $FX1          $FX
170900150305     C                   MoveA     $FC1          $FC
171000150305     C                   MoveA     $FR1          $FR
171100150305     C                   Z-Add     $ULKD1        $ULK
171200150305     C                   Z-Add     $D1L01        $L01
171300150305     C                   Z-Add     $D1L02        $L02
171400150305     C                   Exsr      RIEKEY
171500150305     C                   Z-Add     $ULK          $ULKD1            3 0
171600150305     C                   MoveA     $FC           $FC1
171700150305     C                   MoveL     $KEY1         Z1KE1
171800150305     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
171900150305     C                   MoveL     $KEY2         Z1KE2
172000150305     C                   WRITE     FMTZ1
172100150305     C*
172200150305R172 C                   EndSr
172300150305  "  C************************************************************
172400150305  "  C* CONTROLLO VIDEATA  D5
172500150305  "  C* non usare i seguenti indicatori (perchè relativi a D1MSG):
172600150305  "  C* 95 96 97 98
172700150305  "  C************************************************************
172800150305R172 C     CTRD5         BegSr
172900150305     C*
173000150305     C*
173100150305R172 C                   EndSr
173200160127R211 C/EJECT
173300160127  "  C************************************************************
173400160127  "  C* GESTIONE VIDEO RECORD  D6
173500160127  "  C************************************************************
173600160127R211 C     GESD6         BegSr
173700160127     C* emissione piede videata se proveniente da altra
173800160127     C* salvataggio valore formato di provenienza
173900160127     C     $LASTV        IfNE      'D6'
174000160127     C                   WRITE     FMTT1
174100160127     C                   Exsr      GESPIE
174200160127     C                   Move      $LASTV        $LASTG
174300160127     C                   Move      'D6'          $LASTV
174400160127     C                   EndIf
174500160127     C*
174600160127     C* se esistono errori sulla videata
174700160127     C* emetto la write del formato a indicatori spenti per vedere
174800160127     C* le eventuali decodifiche
174900160127     C     *IN99         IfEq      *ON
175000160127     C     $GEST         ANDEQ     'D6'
175100160127     C                   MoveA     *IN           WIN
175200160127     C                   Move      *ALL'0'       IN5098           49
175300160127     C                   MoveA     IN5098        *IN(50)
175400160127     C                   WRITE     FMTD6
175500160127     C                   MoveA     WIN           *IN
175600160127     C                   EndIf
175700160127     C* Inizializzazione indice tasti funzione personalizzati
175800160127     C                   Eval      PosFun = 25
175900160127     C*
176000160127     C                   Select
176100160127     C* Annullamento
176200160127     C     OPZR75        WhenEq    '04'
176300160127     C* Visualizzazione
176400160127     C     OPZR75        OREQ      '05'
176500160127     C                   WRITE     FMTD6
176600160127     C                   EXFMT     PROTECT
176700160127     C* Immissione/Modifica/Copia
176800160127     C                   OTHER
176900160127     C                   EXFMT     FMTD6
177000160127     C                   EndSl
177100160127     C*
177200160127     C* azzero la posizione del cursore
177300160127     C                   Z-Add     0             H1RIGA
177400160127     C                   Z-Add     0             H1COLO
177500160127     C*
177600160127     C* controllo abilitazione tasti funzione
177700160127     C                   Exsr      CTRKD1
177800160127     C*
177900160127     C     *IN99         IfEq      *OFF
178000160127     C*
178100160127     C                   Select
178200160127     C* ROLDOWN
178300160127     C     $TASTO        WhenEq    ROLDWN
178400160127     C                   Move      'D1'          $GEST
178500160127     C* ROLLUP
178600160127     C     $TASTO        WhenEq    ROLLUP
178700160201     C                   Move      'D2'          $GEST
178800160127     C* F3=Fine
178900160127     C     $TASTO        WhenEq    F03
179000160127     C                   Exsr      F03D1
179100160127     C* F4=Ricerca
179200160127     C     $TASTO        WhenEq    F04
179300160127     C                   Exsr      SEARCH
179400160127     C* F5=Refresh
179500160127     C     $TASTO        WhenEq    F05
179600160127     C                   Exsr      F05D6
179700160127     C* F8=Successivo
179800160127     C     $TASTO        WhenEq    F08
179900160127     C                   Exsr      F08D1
180000160127     C* F12=Ritorno
180100160127     C     $TASTO        WhenEq    F12
180200160127     C                   Exsr      F12D1
180300160127     C* F24=Altri tasti
180400160127     C     $TASTO        WhenEq    F24
180500160127     C                   Exsr      F24D6
180600160127     C* Personalizzata
180700160127     C     $PGF1(PosFun) Whenne    *BLANK
180800160127     C                   Exsr      FperD1
180900160127     C*
181000160127     C                   OTHER
181100160127     C                   Exsr      CTRD6
181200160127     C     *IN99         IfEq      *OFF
181300160127     C                   Exsr      CTRD1
181400160127     C     *IN99         IfEq      *OFF
181500160127     C                   Exsr      CTRD2
181600160127     C     *IN99         IfEq      *OFF
181700160127     C                   Exsr      CTRD3
181800160127     C     *IN99         IfEq      *OFF
181900160127     C                   Exsr      CTRD5
182000160128     C     *IN99         IfEq      *OFF
182100160128     C                   Exsr      CTRD7
182200160128     C     *IN99         IfEq      *OFF
182300160128     C                   Exsr      CTRD8
182400160127     C                   EndIf
182500160127     C                   EndIf
182600160127     C                   EndIf
182700160127     C                   EndIf
182800160128     C                   EndIf
182900160128     C                   EndIf
183000160127     C*
183100160127     C     *IN99         IfEq      *ON
183200160127     C* Spengo gli indicatori di errore per il controllo su altri schermi
183300160127     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
183400160127     C* videate
183500160127     C                   Move      *ALL'0'       IN5098           49
183600160127     C                   MoveA     IN5098        *IN(50)
183700160127     C                   SetOn                                        9996                       96
183800160127     C                   EndIf
183900160127     C*
184000160127     C*
184100160127     C     $TASTO        IfEq      F06
184200160127     C     *IN99         ANDEQ     *OFF
184300160127     C* eseguo aggiornamento
184400160127     C                   Exsr      AGGANA
184500160127     C* eseguo operazioni del dopo-aggiornamento
184600160127     C     *IN99         IfEq      *OFF
184700160127     C                   Exsr      GESAGG
184800160127     C                   EndIf
184900160127     C                   EndIf
185000160127     C*
185100160127     C                   EndSl
185200160127     C*
185300160127     C                   EndIf
185400160127     C*
185500160127R211 C                   EndSr
185600160127  "  C************************************************************
185700160127  "  C* RIEMPIMENTO VIDEATA D6
185800160127  "  C************************************************************
185900160127R211 C     RIED6         BegSr
186000160127     C*
186100160127     C     OPZR75        IfNe      '01'
186200160203     C
186300170216R250 X***                MoveL     N77Eventi     Eventi
186400160203     C*
186500160203     C                   Movel(p)  N77CITAP      CitAp
186600160203     C                   Movel(p)  N77PRVAP      PrvAp
186700160203     C                   Movel(p)  N77COMAP      ComAp
186800180123R279 C                   MoveL(p)  N77FUSCOMP    FusComAp
186900160127     C*
187000160128     C                   MoveL(P)  N77Ser        Ser
187100160128     C                   MoveL(P)  N77Cde        Cde
187200160127     C* campi utilizzati per estero
187300160202     C                   MoveL(P)  N77RCit       CitEst
187400160202     C                   MoveL(P)  N77RVia       ViaEst
187500160127     C*
187600160202     C                   MoveL(P)  N77Cse        Cse
187700160202     C                   MoveL(P)  N77Num        Num
187800160202     C                   MoveL(P)  N77NResS      NRess
187900160127     C                   Endif
188000160127     C*
188100160127R211 C                   Endsr
188200160127  "  C/EJECT
188300160127  "  C************************************************************
188400160127  "  C* GESTIONE F05 VIDEO D6
188500160127  "  C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
188600160127  "  C* e il riposizionamento alla prima
188700160127  "  C************************************************************
188800160127R211 C     F05D6         BegSr
188900160127     C*
189000160127     C                   Move      *ON           $INZD1
189100160127     C                   Move      'D1'          $GEST
189200160127     C*
189300160127R211 C/EJECT
189400160127  "  C                   EndSr
189500160127  "  C************************************************************
189600160127  "  C* GESTIONE F24 VIDEO D6
189700160127  "  C************************************************************
189800160127R211 C     F24D6         BegSr
189900160127     C*
190000160127     C                   MoveA     $FX1          $FX
190100160127     C                   MoveA     $FC1          $FC
190200160127     C                   MoveA     $FR1          $FR
190300160127     C                   Z-Add     $ULKD1        $ULK
190400160127     C                   Z-Add     $D1L01        $L01
190500160127     C                   Z-Add     $D1L02        $L02
190600160127     C                   Exsr      RIEKEY
190700160127     C                   Z-Add     $ULK          $ULKD1            3 0
190800160127     C                   MoveA     $FC           $FC1
190900160127     C                   MoveL     $KEY1         Z1KE1
191000160127     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
191100160127     C                   MoveL     $KEY2         Z1KE2
191200160127     C                   WRITE     FMTZ1
191300160127     C*
191400160127R211 C                   EndSr
191500160127  "  C************************************************************
191600160128  "  C* CONTROLLO VIDEATA  D6
191700160127  "  C* non usare i seguenti indicatori (perchè relativi a D1MSG):
191800160127  "  C* 95 96 97 98
191900160127  "  C************************************************************
192000160127R211 C     CTRD6         BegSr
192100160127     C*
192200160127     C     OPZR75        IfNE      '04'
192300160127     C     OPZR75        AndNE     '05'
192400160203     C*
192500160203     C* Il domicilio fiscale a gennaio dell'anno precedente è obbligatorio
192600160203     C* nel caso la causale di pagamento sia uguale ad 'N'
192700160203     C                   If        C77 = 'N '
192800160203     C                   If        CitAp = *blanks  or
192900160203     C                             PrvAp = *blanks  or
193000160203     C                             ComAp = *blanks
193100160203     C                   Seton                                        4999
193200160203     C                   Endif
193300160203     C                   Endif
193400160203     C*
193500160127     C* Casella non residenti Shumacker
193600160128     C                   IF        NRess  <> '/'       and
193700160128     C                             NRess  <> ' '
193800160127     C                   SetOn                                        4399
193900160127     C                   EndIf
194000160127     C*
194100160203     C* Casella Anticipazione valori ammessi "/" o " "
194200160203     C                   IF        Titolo  <> '/' and
194300160203     C                             Titolo  <> ' '
194400160203     C                   SetOn                                        3499
194500160203     C                   EndIf
194600160203     C*
194700160127     C                   EndIf
194800160127     C*
194900160127R211 C                   EndSr
195000160127R211 C/EJECT
195100160127  "  C************************************************************
195200160127  "  C* GESTIONE VIDEO RECORD  D7
195300160127  "  C************************************************************
195400160127R211 C     GESD7         BegSr
195500160127     C* emissione piede videata se proveniente da altra
195600160127     C* salvataggio valore formato di provenienza
195700160127     C     $LASTV        IfNE      'D7'
195800160127     C                   WRITE     FMTT1
195900160127     C                   Exsr      GESPIE
196000160127     C                   Move      $LASTV        $LASTG
196100160127     C                   Move      'D7'          $LASTV
196200160127     C                   EndIf
196300160127     C*
196400160127     C* se esistono errori sulla videata
196500160127     C* emetto la write del formato a indicatori spenti per vedere
196600160127     C* le eventuali decodifiche
196700160127     C     *IN99         IfEq      *ON
196800160127     C     $GEST         ANDEQ     'D7'
196900160127     C                   MoveA     *IN           WIN
197000160127     C                   Move      *ALL'0'       IN5098           49
197100160127     C                   MoveA     IN5098        *IN(50)
197200160127     C                   WRITE     FMTD7
197300160127     C                   MoveA     WIN           *IN
197400160127     C                   EndIf
197500160127     C* Inizializzazione indice tasti funzione personalizzati
197600160127     C                   Eval      PosFun = 25
197700160127     C*
197800160127     C                   Select
197900160127     C* Annullamento
198000160127     C     OPZR75        WhenEq    '04'
198100160127     C* Visualizzazione
198200160127     C     OPZR75        OREQ      '05'
198300160127     C                   WRITE     FMTD7
198400160127     C                   EXFMT     PROTECT
198500160127     C* Immissione/Modifica/Copia
198600160127     C                   OTHER
198700160127     C                   EXFMT     FMTD7
198800160127     C                   EndSl
198900160127     C*
199000160127     C* azzero la posizione del cursore
199100160127     C                   Z-Add     0             H1RIGA
199200160127     C                   Z-Add     0             H1COLO
199300160127     C*
199400160127     C* controllo abilitazione tasti funzione
199500160127     C                   Exsr      CTRKD1
199600160127     C*
199700160127     C     *IN99         IfEq      *OFF
199800160127     C*
199900160127     C                   Select
200000160128     C* ROLLUP
200100160128     C     $TASTO        WhenEq    ROLLUP
200200160128     C                   Move      'D8'          $GEST
200300160127     C* ROLDOWN
200400160127     C     $TASTO        WhenEq    ROLDWN
200500160127     C                   Move      'D3'          $GEST
200600160127     C* F3=Fine
200700160127     C     $TASTO        WhenEq    F03
200800160127     C                   Exsr      F03D1
200900160127     C* F4=Ricerca
201000160127     C     $TASTO        WhenEq    F04
201100160127     C                   Exsr      SEARCH
201200160127     C* F5=Refresh
201300160127     C     $TASTO        WhenEq    F05
201400160127     C                   Exsr      F05D7
201500160127     C* F8=Successivo
201600160127     C     $TASTO        WhenEq    F08
201700160127     C                   Exsr      F08D1
201800160127     C* F12=Ritorno
201900160127     C     $TASTO        WhenEq    F12
202000160127     C                   Exsr      F12D1
202100160127     C* F24=Altri tasti
202200160127     C     $TASTO        WhenEq    F24
202300160127     C                   Exsr      F24D7
202400160127     C* Personalizzata
202500160127     C     $PGF1(PosFun) Whenne    *BLANK
202600160127     C                   Exsr      FperD1
202700160127     C*
202800160127     C                   OTHER
202900160127     C                   Exsr      CTRD7
203000160127     C     *IN99         IfEq      *OFF
203100160127     C                   Exsr      CTRD1
203200160127     C     *IN99         IfEq      *OFF
203300160127     C                   Exsr      CTRD6
203400160127     C     *IN99         IfEq      *OFF
203500160127     C                   Exsr      CTRD2
203600160127     C     *IN99         IfEq      *OFF
203700160127     C                   Exsr      CTRD3
203800160127     C     *IN99         IfEq      *OFF
203900160127     C                   Exsr      CTRD8
204000160127     C     *IN99         IfEq      *OFF
204100160127     C                   Exsr      CTRD5
204200160127     C                   EndIf
204300160127     C                   EndIf
204400160127     C                   EndIf
204500160127     C                   EndIf
204600160127     C                   EndIf
204700160127     C                   EndIf
204800160127     C*
204900160127     C     *IN99         IfEq      *ON
205000160127     C* Spengo gli indicatori di errore per il controllo su altri schermi
205100160127     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
205200160127     C* videate
205300160127     C                   Move      *ALL'0'       IN5098           49
205400160127     C                   MoveA     IN5098        *IN(50)
205500160127     C                   SetOn                                        9996                       96
205600160127     C                   EndIf
205700160127     C*
205800160127     C*
205900160127     C     $TASTO        IfEq      F06
206000160127     C     *IN99         ANDEQ     *OFF
206100160127     C* eseguo aggiornamento
206200160127     C                   Exsr      AGGANA
206300160127     C* eseguo operazioni del dopo-aggiornamento
206400160127     C     *IN99         IfEq      *OFF
206500160127     C                   Exsr      GESAGG
206600160127     C                   EndIf
206700160127     C                   EndIf
206800160127     C*
206900160127     C                   EndSl
207000160127     C*
207100160127     C                   EndIf
207200160127     C*
207300160127R211 C                   EndSr
207400160127  "  C************************************************************
207500160127  "  C* RIEMPIMENTO VIDEATA D7
207600160127  "  C************************************************************
207700160127R211 C     RIED7         BegSr
207800160127     C*
207900160127     C     OPZR75        IfNe      '01'
208000160127     C*
208100160202     C                   MoveL(P)  N77DPCDF      DPCDF
208200160127     C                   MoveL(P)  N77DPDEN      DPDEN
208300170216R250 X***                MoveL(P)  N77DPCENP     DPCENP
208400160202     C                   MoveL(P)  N77DPCAZI     DPCAZI
208500160202     C                   Movel(P)  N77DPCATE     DPCATE
208600160202     C                   Z-add     N77CTBDL      CTBDL
208700160202     C                   Z-add     N77CTBLV      CTBLV
208800160202     C                   Z-add     N77DPIMPA     DPIMPA
208900160202     C                   Movel(P)  N77DPACON     DPACON
209000160202     C                   Z-add     N77DPCODU     DPCODU
209100160202     C                   Z-add     N77DPCOVE     DPCOVE
209200160202     C                   Movel(P)  N77OSCDF1     OSCDF1
209300160202     C                   Movel(P)  N77OSCDF2     OSCDF2
209400160202     C                   Movel(P)  N77OSCDF3     OSCDF3
209500160127     C                   Endif
209600160127     C*
209700160127R211 C                   Endsr
209800160127  "  C/EJECT
209900160127  "  C************************************************************
210000160128  "  C* GESTIONE F05 VIDEO D7
210100160127  "  C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
210200160127  "  C* e il riposizionamento alla prima
210300160127  "  C************************************************************
210400160127R211 C     F05D7         BegSr
210500160127     C*
210600160127     C                   Move      *ON           $INZD1
210700160127     C                   Move      'D1'          $GEST
210800160127     C*
210900160127R211 C/EJECT
211000160127  "  C                   EndSr
211100160127  "  C************************************************************
211200160128  "  C* GESTIONE F24 VIDEO D7
211300160127  "  C************************************************************
211400160127R211 C     F24D7         BegSr
211500160127     C*
211600160127     C                   MoveA     $FX1          $FX
211700160127     C                   MoveA     $FC1          $FC
211800160127     C                   MoveA     $FR1          $FR
211900160127     C                   Z-Add     $ULKD1        $ULK
212000160127     C                   Z-Add     $D1L01        $L01
212100160127     C                   Z-Add     $D1L02        $L02
212200160127     C                   Exsr      RIEKEY
212300160127     C                   Z-Add     $ULK          $ULKD1            3 0
212400160127     C                   MoveA     $FC           $FC1
212500160127     C                   MoveL     $KEY1         Z1KE1
212600160127     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
212700160127     C                   MoveL     $KEY2         Z1KE2
212800160127     C                   WRITE     FMTZ1
212900160127     C*
213000160127R211 C                   EndSr
213100160127  "  C************************************************************
213200160127  "  C* CONTROLLO VIDEATA  D5
213300160127  "  C* non usare i seguenti indicatori (perchè relativi a D1MSG):
213400160127  "  C* 95 96 97 98
213500160127  "  C************************************************************
213600160127R211 C     CTRD7         BegSr
213700160127     C*
213800160127     C     OPZR75        IfNE      '04'
213900160127     C     OPZR75        AndNE     '05'
214000160128     C* Altri contributi
214100160128     C                   IF        DPAcon <> '/' and
214200160128     C                             DPAcon <> ' '
214300160128     C                   SetOn                                        4899
214400160128     C                   EndIf
214500160127     C*
214600160127     C* Codice ente previdenziale
214700160127     C*
214800160127     C* Categoria
214900160127     C*
215000160127     C                   EndIf
215100160127     C*
215200160127R211 C                   EndSr
215300160127R211 C/EJECT
215400160127  "  C************************************************************
215500160127  "  C* GESTIONE VIDEO RECORD  D8
215600160127  "  C************************************************************
215700160127R211 C     GESD8         BegSr
215800160127     C* emissione piede videata se proveniente da altra
215900160127     C* salvataggio valore formato di provenienza
216000160127     C     $LASTV        IfNE      'D8'
216100160127     C                   WRITE     FMTT1
216200160127     C                   Exsr      GESPIE
216300160127     C                   Move      $LASTV        $LASTG
216400160127     C                   Move      'D8'          $LASTV
216500160127     C                   EndIf
216600160127     C*
216700160127     C* se esistono errori sulla videata
216800160127     C* emetto la write del formato a indicatori spenti per vedere
216900160127     C* le eventuali decodifiche
217000160127     C     *IN99         IfEq      *ON
217100160127     C     $GEST         ANDEQ     'D8'
217200160127     C                   MoveA     *IN           WIN
217300160127     C                   Move      *ALL'0'       IN5098           49
217400160127     C                   MoveA     IN5098        *IN(50)
217500160127     C                   WRITE     FMTD8
217600160127     C                   MoveA     WIN           *IN
217700160127     C                   EndIf
217800160127     C* Inizializzazione indice tasti funzione personalizzati
217900160127     C                   Eval      PosFun = 25
218000160127     C*
218100160127     C                   Select
218200160127     C* Annullamento
218300160127     C     OPZR75        WhenEq    '04'
218400160127     C* Visualizzazione
218500160127     C     OPZR75        OREQ      '05'
218600160127     C                   WRITE     FMTD8
218700160127     C                   EXFMT     PROTECT
218800160127     C* Immissione/Modifica/Copia
218900160127     C                   OTHER
219000160127     C                   EXFMT     FMTD8
219100160127     C                   EndSl
219200160127     C*
219300160127     C* azzero la posizione del cursore
219400160127     C                   Z-Add     0             H1RIGA
219500160127     C                   Z-Add     0             H1COLO
219600160127     C*
219700160127     C* controllo abilitazione tasti funzione
219800160127     C                   Exsr      CTRKD1
219900160127     C*
220000160127     C     *IN99         IfEq      *OFF
220100160127     C*
220200160127     C                   Select
220300160127     C* ROLLUP
220400160127     C     $TASTO        WhenEq    ROLLUP
220500160127     C                   Move      'D5'          $GEST
220600160128     C* ROLDOWN
220700160128     C     $TASTO        WhenEq    ROLDWN
220800160128     C                   Move      'D7'          $GEST
220900160127     C* F3=Fine
221000160127     C     $TASTO        WhenEq    F03
221100160127     C                   Exsr      F03D1
221200160127     C* F4=Ricerca
221300160127     C     $TASTO        WhenEq    F04
221400160127     C                   Exsr      SEARCH
221500160127     C* F5=Refresh
221600160127     C     $TASTO        WhenEq    F05
221700160127     C                   Exsr      F05D8
221800160127     C* F8=Successivo
221900160127     C     $TASTO        WhenEq    F08
222000160127     C                   Exsr      F08D1
222100160127     C* F12=Ritorno
222200160127     C     $TASTO        WhenEq    F12
222300160127     C                   Exsr      F12D1
222400160127     C* F24=Altri tasti
222500160127     C     $TASTO        WhenEq    F24
222600160127     C                   Exsr      F24D8
222700160127     C* Personalizzata
222800160127     C     $PGF1(PosFun) Whenne    *BLANK
222900160127     C                   Exsr      FperD1
223000160127     C*
223100160127     C                   OTHER
223200160127     C                   Exsr      CTRD8
223300160127     C     *IN99         IfEq      *OFF
223400160127     C                   Exsr      CTRD1
223500160127     C     *IN99         IfEq      *OFF
223600160127     C                   Exsr      CTRD6
223700160127     C     *IN99         IfEq      *OFF
223800160127     C                   Exsr      CTRD2
223900160127     C     *IN99         IfEq      *OFF
224000160127     C                   Exsr      CTRD3
224100160127     C     *IN99         IfEq      *OFF
224200160127     C                   Exsr      CTRD7
224300160127     C     *IN99         IfEq      *OFF
224400160127     C                   Exsr      CTRD5
224500160127     C                   EndIf
224600160127     C                   EndIf
224700160127     C                   EndIf
224800160127     C                   EndIf
224900160127     C                   EndIf
225000160127     C                   EndIf
225100160127     C*
225200160127     C     *IN99         IfEq      *ON
225300160127     C* Spengo gli indicatori di errore per il controllo su altri schermi
225400160127     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
225500160127     C* videate
225600160127     C                   Move      *ALL'0'       IN5098           49
225700160127     C                   MoveA     IN5098        *IN(50)
225800160127     C                   SetOn                                        9996                       96
225900160127     C                   EndIf
226000160127     C*
226100160127     C*
226200160127     C     $TASTO        IfEq      F06
226300160127     C     *IN99         ANDEQ     *OFF
226400160127     C* eseguo aggiornamento
226500160127     C                   Exsr      AGGANA
226600160127     C* eseguo operazioni del dopo-aggiornamento
226700160127     C     *IN99         IfEq      *OFF
226800160127     C                   Exsr      GESAGG
226900160127     C                   EndIf
227000160127     C                   EndIf
227100160127     C*
227200160127     C                   EndSl
227300160127     C*
227400160127     C                   EndIf
227500160127     C*
227600160127R211 C                   EndSr
227700160127  "  C************************************************************
227800160127  "  C* RIEMPIMENTO VIDEATA D8
227900160127  "  C************************************************************
228000160127R211 C     RIED8         BegSr
228100160127     C*
228200160127     C     OPZR75        IfNe      '01'
228300160127     C*
228400160127     C                   MoveL(P)  N77F2CDF      F2CDF
228500160128     C                   Z-add     N77F2Ime      F2Ime
228600160128     C                   Z-add     N77F2Rio      F2Rio
228700170216R250 X***                Movel     N77F2Rino     F2Rino
228800160127     C                   MoveL(P)  N77F2CDF2     F2CDF2
228900160128     C                   Z-add     N77F2Ime2     F2Ime2
229000160128     C                   Z-add     N77F2Rio2     F2Rio2
229100170216R250 X***                MoveL(P)  N77F2Rino2    F2Rino2
229200170216R250 C                   Z-add     N77F2Imnt     F2Imnt
229300170216R250 C                   Z-add     N77F2Imn2     F2Imn2
229400160127     C                   Z-add     N77F3Ine      F3Ine
229500160127     C                   Z-add     N77F3Rie      F3Rie
229600160127     C                   Z-add     N77F3Ain      F3Ain
229700160127     C                   Z-add     N77F3Ari      F3Ari
229800160127     C                   Z-add     N77F3Ine2     F3Ine2
229900160127     C                   Z-add     N77F3Rie2     F3Rie2
230000160127     C                   Z-add     N77F3Ain2     F3Ain2
230100160127     C                   Z-add     N77F3Ari2     F3Ari2
230200160127     C                   Endif
230300160127     C*
230400160127R211 C                   Endsr
230500160127  "  C/EJECT
230600160127  "  C************************************************************
230700160127  "  C* GESTIONE F05 VIDEO D8
230800160127  "  C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
230900160127  "  C* e il riposizionamento alla prima
231000160127  "  C************************************************************
231100160127R211 C     F05D8         BegSr
231200160127     C*
231300160127     C                   Move      *ON           $INZD1
231400160127     C                   Move      'D1'          $GEST
231500160127     C*
231600160127R211 C/EJECT
231700160127  "  C                   EndSr
231800160127  "  C************************************************************
231900160127  "  C* GESTIONE F24 VIDEO D8
232000160127  "  C************************************************************
232100160127R211 C     F24D8         BegSr
232200160127     C*
232300160127     C                   MoveA     $FX1          $FX
232400160127     C                   MoveA     $FC1          $FC
232500160127     C                   MoveA     $FR1          $FR
232600160127     C                   Z-Add     $ULKD1        $ULK
232700160127     C                   Z-Add     $D1L01        $L01
232800160127     C                   Z-Add     $D1L02        $L02
232900160127     C                   Exsr      RIEKEY
233000160127     C                   Z-Add     $ULK          $ULKD1            3 0
233100160127     C                   MoveA     $FC           $FC1
233200160127     C                   MoveL     $KEY1         Z1KE1
233300160127     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
233400160127     C                   MoveL     $KEY2         Z1KE2
233500160127     C                   WRITE     FMTZ1
233600160127     C*
233700160127R211 C                   EndSr
233800160127  "  C************************************************************
233900160127  "  C* CONTROLLO VIDEATA  D8
234000160127  "  C* non usare i seguenti indicatori (perchè relativi a D1MSG):
234100160127  "  C* 95 96 97 98
234200160127  "  C************************************************************
234300160127R211 C     CTRD8         BegSr
234400160127     C*
234500160127     C     OPZR75        IfNE      '04'
234600160127     C     OPZR75        AndNE     '05'
234700160127     C*
234800170216R250 X**** ex R211 Ritenute non operate
234900170216  "  X***  ex R211       IF        F2Rino  <> '/'     and
235000170216  "  X***  ex R211                 F2Rino  <> ' '
235100170216  "  X***  ex R211       SetOn                                        4699
235200170216  "  X***  ex R211       EndIf
235300170216  "  X***  ex R211
235400170216  "  X**** ex R211 Ritenute non operate 2
235500170216  "  X***  ex R211       IF        F2Rino2 <> '/'     and
235600170216  "  X***  ex R211                 F2Rino2 <> ' '
235700170216  "  X***  ex R211       SetOn                                        4799
235800170216R250 X***  ex R211       EndIf
235900160128     C                   EndIf
236000160127     C*
236100160127R211 C                   EndSr
236200990422     C************************************************************
236300990422     C* AGGIORNAMENTO ANAGRAFICA
236400990422     C************************************************************
236500120502     C     AGGANA        BegSr
236600990422     C*
236700120502     C                   Select
236800120502    >C     OPZR75        WhenEq    '01'
236900040305     C     OPZR75        OREQ      '03'
237000120502     C                   Exsr      RIEANA
237100150626R186 X***                WRITE     ND77006                              22
237200150626R186 C                   WRITE     ND77020                              22
237300040305     C* se rcd non scrivibile attivo errore
237400120502     C     *IN22         IfEq      *ON
237500120502     C                   SetOn                                        9599
237600120502     C                   EndIf
237700120502     C     OPZR75        WhenEq    '02'
237800120502     C                   Exsr      RIEANA
237900150626R186 X***                UPDATE    ND77006                              22
238000150626R186 C                   UPDATE    ND77020                              22
238100120502     C     OPZR75        WhenEq    '04'
238200150626R186 X***                DELETE    ND77006
238300150626R186 C                   DELETE    ND77020
238400120502     C                   EndSl
238500990422     C*
238600990422     C* richiama pgm User Exit
238700990422     c*
238800120502     C     USRERR        IfEq      *BLANK
238900040305     C* imposta chiave univoca  per user exit
239000120502     C                   Move      XSCSOC        XUKSOC
239100120502     C                   Move      XSCDUN        XUKDUN
239200120502     C                   MoveL     DSPGM         XUKPGM
239300120502     x***                Z-Add     N77ACF        XUKFL1
239400040305     C* ho fino a 6 campi chiave passabili
239500120502     C                   MoveL     XUKDS         KPJBU
239600040305     C     DSPGM         CAT       'U':0         USREXT           10
239700120502     C                   Call      USREXT                               21
239800120502     C                   Parm                    KPJBA
239900120502     C     *IN21         IfEq      *ON
240000120502     C                   Move      *ON           USRERR            1
240100120502     C                   EndIf
240200120502     C                   EndIf
240300120502     C                   EndSr
240400990422     C/EJECT
240500990422     C************************************************************
240600990422     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
240700990422     C************************************************************
240800120502     C     RIEANA        BegSr
240900990422     C*
241000990422     C                   IF        OpzR75     =  '01'    or
241100990422     C                             OpzR75     =  '03'
241200990423     C*
241300990423     C* se inserimento
241400150626R186 X***                Clear                   Nd77006
241500150626R186 C                   Clear                   Nd77020
241600120502     C                   EndIf
241700040305     C*
241800120502     C                   MoveL     SocietaR75    N77Societa
241900120502     C                   Z-Add     VACF          N77ACF
242000120502     C                   MoveL     QUA           N77QUA
242100120502     C                   MoveL     Kcc           N77Kcc
242200120502     C                   MoveL     Ksc           N77Ksc
242300120502     C                   MoveL     RGT           N77RGT
242400120502     C                   MoveL     PRG           N77Prg
242500150716R192 X***                MoveL     COG           N77COG
242600150716R192 C                   MoveL(P)  COG           N77COG
242700150716R192 X***                MoveL     CDF           N77CDF
242800150716R192 C                   MoveL(P)  CDF           N77CDF
242900120502     C                   MoveL     PFG           N77PFG
243000150716R192 X***                MoveL     CIT           N77CIT
243100150716R192 C                   MoveL(P)  CIT           N77CIT
243200120502     C                   MoveL     PRV           N77PRV
243300150716R192 X***                MoveL(P)  VIA           N77VIA
243400150716R192 C                   MoveL     VIA           N77VIA
243500150716R192 X***                MoveL     SER           N77SER
243600150716R192 C                   MoveL(P)  SER           N77SER
243700120502     C                   MoveL     CDE           N77CDE
243800120502     C                   MoveL     NUM           N77NUM
243900150716R192 X***                MoveL     CSE           N77CSE
244000150716R192 C                   MoveL(P)  CSE           N77CSE
244100150716R192 X***                MoveL     NOM           N77NOM
244200150716R192 C                   MoveL(P)  NOM           N77NOM
244300120502     C                   MoveL     SES           N77SES
244400150716R192 X***                MoveL     LNC           N77LNC
244500150716R192 C                   MoveL(P)  LNC           N77LNC
244600120502     C                   MoveL     C77           N77C77
244700150212R165 C                   MoveL     CTGPRT        N77CtgPrt
244800150212  "  C                   if        Escludi  = XSCON
244900150212  "  C                   eval      N77Escludi =  *On
245000150212  "  C                   else
245100150212  "  C                   eval      N77Escludi =  *Off
245200150212  "  C                   endif
245300150212R165 C*
245400120502     C                   MoveL     $DT0          N77DNC
245500120502     C     DNC           IfNE      0
245600120502     C     *eur          MoveL     DNC           DataISO
245700120502     C                   MoveL     DataIso       N77DNC
245800120502     C                   EndIf
245900120502     C                   MoveL     PNC           N77PNC
246000120502C1478x*****              Z-Add     PRA           N77PRA
246100120502     C                   Z-Add     ITC           N77ITC
246200120502     C                   Z-Add     SNS           N77SNS
246300120502     C                   Z-Add     IMN           N77IMN
246400120502     C                   Z-Add     IMS           N77IMS
246500120502     C                   Z-Add     RAC           N77RAC
246600120502C1770C                   Z-Add     RIM           N77RIM
246700120502     C                   Z-Add     NET           N77NET
246800020415     C* utilizzati campi rappresentante (non più in uso) per estero
246900150716R192 X***                MoveL     CitEst        N77RCIT
247000150716R192 C                   MoveL(p)  CitEst        N77RCIT
247100150716R192 X***                MoveL     ViaEst        N77RVIA
247200150716R192 C                   MoveL(p)  ViaEst        N77RVIA
247300120502C1478C                   MoveL     Titolo        N77Tit
247400120502C1478C                   MoveL     Reg           N77Reg
247500120502C1478C                   MoveL     Anno          N77Anno
247600120502C1478C                   Z-Add     ARG           N77ARG
247700120502C1770C                   Z-Add     ARI           N77ARI
247800120502C1478C                   Z-Add     ARS           N77ARS
247900120502C1478C                   Z-Add     IMSAP         N77IMSAP
248000120502C1478C                   Z-Add     RACAP         N77RACAP
248100120502C1846C                   Z-Add     CTBDL         N77CTBDL
248200120502C1846C                   Z-Add     CTBLV         N77CTBLV
248300120502C1943C                   Z-Add     SpeRimb       N77SpeRimb
248400120502C1943C                   Z-Add     RitRimb       N77RitRimb
248500150716R192 X*EX C1478          Move      PIn           N77PIn
248600150716R192 C                   Move(P)   PIn           N77PIn
248700120502C1478C                   Movel     GioI          N77DtIn
248800120502C1478C                   Move      MeseI         N77DtIn
248900120502C1478C                   Movel     GioF          N77DtFi
249000120502C1478C                   Move      MeseF         N77DtFi
249100140507R148 C**** ex C1478      Move      COM           N77Com
249200150716R192 X*EX C1478          MoveL     RCDF          N77RCDF
249300150716R192 C                   MoveL(P)  RCDF          N77RCDF
249400150716C1478C                   MoveL     RCSE          N77RCSE
249500150716R192 X*EX C1478          MoveL     RCOG          N77RCOG
249600150716R192 C                   MoveL(P)  RCOG          N77RCOG
249700150716R192 X*EX C1478          MoveL     RNOM          N77RNOM
249800150716R192 C                   MoveL(P)  RNOM          N77RNOM
249900150716C1478C                   MoveL     RSES          N77RSES
250000150716R192 X*EX C1478          MoveL     RLNC          N77RLNC
250100150716R192 C                   MoveL(P)  RLNC          N77RLNC
250200150716C1478C                   MoveL     $DT0          N77RDNC
250300120502  "  C     RDNC          IfNE      0
250400120502  "  C     *eur          MoveL     RDNC          DataISO
250500120502  "  C                   MoveL     DataIso       N77RDNC
250600120502  "  C                   EndIf
250700120502C1478C                   MoveL     RPNC          N77RPNC
250800120502C1478C                   MoveL     RPRV          N77RPRV
250900160203R211 X*** ex c2014       MoveL     Erede         N77Erede
251000170216R250 X*** ex C2014       MoveL     Eventi        N77Eventi
251100160203C2014c                   Z-Add     ImpPrFa       N77ImpPrFa
251200120502C2014c                   Z-Add     ImpCuCo       N77ImpCuCo
251300100514R024 c*
251400150716R192 X*** ex R024        MoveL     R3CDF         N77ASCDF
251500150716R192 C                   MoveL(P)  R3CDF         N77ASCDF
251600120502R055 C*** ex R024        Z-Add     R3IMP         N77ASIMP
251700120502R055 C                   MoveL     R3Causale     N77CauAS
251800110531R055 C****               Eval      $cod_s = %TrimR(R3CAUSALE)
251900120502R055 C****               Clear                   R3CAUSALE
252000120502R055 C****               Move      $cod_s        R3CAUSALE
252100120502R055 C****               Move      R3Causale     N77CauAS
252200120502R024 C                   Z-Add     R3IMS         N77ASIMS
252300120502 "   C                   Z-Add     R3RAC         N77ASRAC
252400120502 "   C                   Z-Add     R3RIM         N77ASRIM
252500120502 "   C                   Z-Add     R3RSO         N77ASRSO
252600120502 "   C                   Z-Add     R3ARA         N77ASARA
252700120502 "   C                   Z-Add     R3ARI         N77ASARI
252800120502 "   C                   Z-Add     R3ARS         N77ASARS
252900100514R024 c*
253000130206R110 C                   Move      Codice        N77Codice
253100170216R250 C                   Move      FusCom        N77FuscOm
253200140507R148 C                   Move      Comune        N77Com
253300140508R148 C*
253400140508R148 C* --- Add.Comunali: dati relativi alle somme erogate
253500140508R148 C*
253600140507R148 C* Add.Comunale
253700140507R148 C                   Z-Add     ACG           N77ACG
253800140507R148 C* Add.Comunale a titolo d'acconto
253900140507R148 C                   Z-Add     ACTA          N77ACTA
254000140507R148 C* Add.Comunale sospesa
254100140507R148 C                   Z-Add     ACS           N77ACS
254200140508R148 C*
254300140508R148 C* --- Add.Comunali: redditi erogati da altri soggetti
254400140508R148 C*
254500140508R148 C* Add.Comunale
254600140508R148 C                   Z-Add     R3ACA         N77ASACA
254700140508R148 C* Add.Comunale a titolo d'acconto
254800140508R148 C                   Z-Add     R3ACI         N77ASACI
254900140508R148 C* Add.Comunale sospesa
255000140508R148 C                   Z-Add     R3ACS         N77ASACS
255100150312R172 C* Progressivo certificazione inviata
255200150312  "  C                   Z-Add     PROTCEI       N77PROTCEI
255300150312  "  C* Protocollo invio
255400150312  "  C                   Z-Add     PROTOC        N77PROTOC
255500150312  "  C* Progressivo certificazione accettata
255600150312R172 C                   Z-Add     PROTCEA       N77PROTCEA
255700160128R211 C* Non residenti Shumacker
255800160128  "  C                   Movel     NRESS         N77NRESS
255900160128R211 C* Dati previdenziali
256000160128  "  C                   Movel     DPCdf         N77DPCdf
256100170216R211 C                   Movel     DPDen         N77DPDen
256200170216R250 X**                 Movel     DPCenp        N77DPCenP
256300170216R211 C                   Movel     DPCazi        N77DPCazi
256400160128  "  C                   Movel     DPCate        N77DPCate
256500160128  "  C                   Movel     DPAcon        N77DPAcon
256600160128  "  C                   z-add     DPImpa        N77DPImpa
256700160128  "  C                   z-add     DPCodu        N77DPCodu
256800160128  "  C                   z-add     DPCove        N77DPCove
256900160128  "  A*---   (Casi Particolari, operazioni straordinarie)
257000160128  "  C                   Movel(P)  OScdf1        N77OScdf1
257100160128  "  C                   Movel(P)  OScdf2        N77OScdf2
257200160128  "  C                   Movel(P)  OScdf3        N77OScdf3
257300160128  "  A*---   (Somme liquidate a seguito di pignoramento verso terzi)
257400160128  "  C                   Movel(P)  F2Cdf         N77F2Cdf
257500160128  "  C                   z-add     F2Ime         N77F2Ime
257600170216R211 C                   z-add     F2Rio         N77F2Rio
257700170216R250 C                   z-add     F2Imnt        N77F2Imnt
257800170216R250 X***                Movel(P)  F2Rino        N77F2Rino
257900170216R211 C                   Movel(P)  F2Cdf2        N77F2Cdf2
258000160128  "  C                   z-add     F2Ime2        N77F2Ime2
258100170216R211 C                   z-add     F2Rio2        N77F2Rio2
258200170216R250 C                   z-add     F2Imn2        N77F2Imn2
258300170216R250 X***                Movel(P)  F2Rino2       N77F2Rino2
258400170216R211 A*---   (Somme corrisposte a titolo di indennità di esproprio)
258500160128  "  C                   z-add     F3Ine         N77F3Ine
258600160128  "  C                   z-add     F3Rie         N77F3Rie
258700160128  "  C                   z-add     F3Ain         N77F3Ain
258800160128  "  C                   z-add     F3Ari         N77F3Ari
258900160128  "  C                   z-add     F3Ine2        N77F3Ine2
259000160128  "  C                   z-add     F3Rie2        N77F3Rie2
259100160128  "  C                   z-add     F3Ain2        N77F3Ain2
259200160128  "  C                   z-add     F3Ari2        N77F3Ari2
259300160203  "  C                   Movel(P)  CitAp         N77CitAp
259400160203  "  C                   Movel(P)  PrvAp         N77PrvAp
259500160203  "  C                   Movel(P)  ComAP         N77ComAp
259600160128R211 C*
259700180123R279 C                   Move      FusComAp      N77FusComP
259800020408C1478C*
259900120502  "  C                   EndSr
260000010417C1478C/EJECT
260100990422     C************************************************************
260200990422     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
260300990422     C************************************************************
260400120502     C     GESAGG        BegSr
260500990422     C*
260600990422     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
260700120502     C     CMTR75        IfEq      '0'
260800040305     x****               COMMIT
260900120502     C                   EndIf
261000990422     C*
261100990422     C* segnalo al pgm chiamante l'avvenuta conferma
261200120502     C                   Move      *ON           OPRR75
261300990422     C* segnalo al pgm chiamante l'aver premuto l'F6
261400120502     C                   Move      '0'           RETR75
261500990422     C*
261600120502     C                   Select
261700990422     C*
261800990422     C* nel caso di immissione
261900120502     C     OPZR75        WhenEq    '01'
262000990422     C* ripristino la videata iniziale
262100120502     C                   Move      *ON           $INZD1
262200120502     C                   Move      'D1'          $GEST
262300990422     C*
262400990422     C* nel caso di modifica
262500120502     C     OPZR75        WhenEq    '02'
262600990422     C* ripasso i campi che devono essere reimpostati sul sfl
262700120502     C                   Movel     Kcc           KccR75
262800120502     C                   Movel     Ksc           KscR75
262900120502     C                   Movel     RGT           RGTR75
263000120502     C                   Movel     PRG           PRGR75
263100150626R186 C                   MoveL     C77           C77R75
263200120502     C                   Z-Add     VACF          AnnoR75
263300120502     C                   Movel     Qua           QuaR75
263400120502     C                   Movel     Xscsoc        SocietaR75
263500150611R186 C                   Movel     Cog           CogR75
263600150611R186 C                   Movel     Nom           NomR75
263700160205R211 C                   Movel     Cdf           CdfR75
263800040305     C* e ritorno al pgm chiamante
263900120502     C                   Move      *ON           $FINE
264000990422     C*
264100990422     C* per tutte le altre opzioni
264200990422     C                   OTHER
264300990422     C* ritorno al pgm chiamante
264400120502     C                   Move      *ON           $FINE
264500120502     C                   EndSl
264600990422     C*
264700120502     C                   EndSr
264800990422     C/EJECT
264900990422     C************************************************************
265000990422     C* CONTROLLO ABILITAZIONE TASTI
265100990422     C************************************************************
265200120502     C     CTRKEY        BegSr
265300990422     C*
265400120502     C                   Select
265500120502     C     $TASTO        WhenEq    F01
265600120502     C                   Z-Add     01            PosFun
265700120502     C     $FC(1)        IfEq      '0'
265800120502     C                   SetOn                                        99
265900120502     C                   EndIf
266000120502     C     $TASTO        WhenEq    F02
266100120502     C                   Z-Add     02            PosFun
266200120502     C     $FC(2)        IfEq      '0'
266300120502     C                   SetOn                                        99
266400120502     C                   EndIf
266500120502     C     $TASTO        WhenEq    F03
266600120502     C                   Z-Add     03            PosFun
266700120502     C     $FC(3)        IfEq      '0'
266800120502     C                   SetOn                                        99
266900120502     C                   EndIf
267000120502     C     $TASTO        WhenEq    F04
267100120502     C                   Z-Add     04            PosFun
267200120502     C     $FC(4)        IfEq      '0'
267300120502     C                   SetOn                                        99
267400120502     C                   EndIf
267500120502     C     $TASTO        WhenEq    F05
267600120502     C                   Z-Add     05            PosFun
267700120502     C     $FC(5)        IfEq      '0'
267800120502     C                   SetOn                                        99
267900120502     C                   EndIf
268000120502     C     $TASTO        WhenEq    F06
268100120502     C                   Z-Add     06            PosFun
268200120502     C     $FC(6)        IfEq      '0'
268300120502     C                   SetOn                                        99
268400120502     C                   EndIf
268500120502     C     $TASTO        WhenEq    F07
268600120502     C                   Z-Add     07            PosFun
268700120502     C     $FC(7)        IfEq      '0'
268800120502     C                   SetOn                                        99
268900120502     C                   EndIf
269000120502     C     $TASTO        WhenEq    F08
269100120502     C                   Z-Add     08            PosFun
269200120502     C     $FC(8)        IfEq      '0'
269300120502     C                   SetOn                                        99
269400120502     C                   EndIf
269500120502     C     $TASTO        WhenEq    F09
269600120502     C                   Z-Add     09            PosFun
269700120502     C     $FC(09)       IfEq      '0'
269800120502     C                   SetOn                                        99
269900120502     C                   EndIf
270000120502     C     $TASTO        WhenEq    F10
270100120502     C                   Z-Add     10            PosFun
270200120502     C     $FC(10)       IfEq      '0'
270300120502     C                   SetOn                                        99
270400120502     C                   EndIf
270500120502     C     $TASTO        WhenEq    F11
270600120502     C                   Z-Add     11            PosFun
270700120502     C     $FC(11)       IfEq      '0'
270800120502     C                   SetOn                                        99
270900120502     C                   EndIf
271000120502     C     $TASTO        WhenEq    F12
271100120502     C                   Z-Add     12            PosFun
271200120502     C     $FC(12)       IfEq      '0'
271300120502     C                   SetOn                                        99
271400120502     C                   EndIf
271500120502     C     $TASTO        WhenEq    F13
271600120502     C                   Z-Add     13            PosFun
271700120502     C     $FC(13)       IfEq      '0'
271800120502     C                   SetOn                                        99
271900120502     C                   EndIf
272000120502     C     $TASTO        WhenEq    F14
272100120502     C                   Z-Add     14            PosFun
272200120502     C     $FC(14)       IfEq      '0'
272300120502     C                   SetOn                                        99
272400120502     C                   EndIf
272500120502     C     $TASTO        WhenEq    F15
272600120502     C                   Z-Add     15            PosFun
272700120502     C     $FC(15)       IfEq      '0'
272800120502     C                   SetOn                                        99
272900120502     C                   EndIf
273000120502     C     $TASTO        WhenEq    F16
273100120502     C                   Z-Add     16            PosFun
273200120502     C     $FC(16)       IfEq      '0'
273300120502     C                   SetOn                                        99
273400120502     C                   EndIf
273500120502     C     $TASTO        WhenEq    F17
273600120502     C                   Z-Add     17            PosFun
273700120502     C     $FC(17)       IfEq      '0'
273800120502     C                   SetOn                                        99
273900120502     C                   EndIf
274000120502     C     $TASTO        WhenEq    F18
274100120502     C                   Z-Add     18            PosFun
274200120502     C     $FC(18)       IfEq      '0'
274300120502     C                   SetOn                                        99
274400120502     C                   EndIf
274500120502     C     $TASTO        WhenEq    F19
274600120502     C                   Z-Add     19            PosFun
274700120502     C     $FC(19)       IfEq      '0'
274800120502     C                   SetOn                                        99
274900120502     C                   EndIf
275000120502     C     $TASTO        WhenEq    F20
275100120502     C                   Z-Add     20            PosFun
275200120502     C     $FC(20)       IfEq      '0'
275300120502     C                   SetOn                                        99
275400120502     C                   EndIf
275500120502     C     $TASTO        WhenEq    F21
275600120502     C                   Z-Add     21            PosFun
275700120502     C     $FC(21)       IfEq      '0'
275800120502     C                   SetOn                                        99
275900120502     C                   EndIf
276000120502     C     $TASTO        WhenEq    F22
276100120502     C                   Z-Add     22            PosFun
276200120502     C     $FC(22)       IfEq      '0'
276300120502     C                   SetOn                                        99
276400120502     C                   EndIf
276500120502     C     $TASTO        WhenEq    F23
276600120502     C                   Z-Add     23            PosFun
276700120502     C     $FC(23)       IfEq      '0'
276800120502     C                   SetOn                                        99
276900120502     C                   EndIf
277000120502     C     $TASTO        WhenEq    F24
277100120502     C                   Z-Add     24            PosFun
277200120502     C     $FC(24)       IfEq      '0'
277300120502     C                   SetOn                                        99
277400120502     C                   EndIf
277500120502     C                   EndSl
277600990422     C*
277700120502     C                   EndSr
277800990422     C/EJECT
277900990422     C******************************************************
278000990422     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
278100990422     C******************************************************
278200120502     C     MSGPGM        BegSr
278300990422     C*
278400120502     C                   Z-Add     2             MSGMSG
278500120502     C                   MoveL     'S'           MSGRSP
278600120502     C                   MoveL     'CR'          MSGTPR
278700120502     C                   MoveL     'S'           MSGEMV
278800120502     C                   Z-Add     10            MSGRGP
278900120502     C                   MoveL     'S'           MSGLCK
279000120502     C                   MoveA     STATUS        STS
279100120502     C                   MoveL     'N'           MSGCNL
279200120502     C                   MoveL     'N'           MSGVID
279300120502     C                   MoveL     'N'           MSGOPE
279400120502     C                   MoveL     'N'           MSGSTP
279500120502     C                   Call      'XMSGR'
279600120502     C                   Parm                    XMSGDS
279700990422     C*
279800120502     C                   EndSr
279900990422     C/EJECT
280000990422     C************************************************************
280100990422     C* OPERAZIONI INIZIALI
280200990422     C************************************************************
280300120502     C     *INZSR        BegSr
280400990422     C*
280500990422     C* Reperimento parametri
280600990422     C*
280700990422     C     *ENTRY        PLIST
280800120502     C                   Parm                    KPJBA
280900120502     C                   Parm                    DSAUT
281000990422     C*
281100990422     C* Reperimento dati società
281200990422     C*
281300120502    >C                   MoveL     'SOC001'      TIPXSC
281400120502     C                   MoveL     *BLANK        SOCXSC
281500120502     C                   Exsr      REPSOC
281600990422     C*
281700990422     C* Controlli dati società
281800040305     C*
281900120502     C     RTNXSC        IfNE      '1'
282000120502     C                   MoveL     XSOCDS        SOC001
282100040305     C                   ELSE
282200120502     C                   Move      '1'           ErrR75
282300120502     C                   Move      *on           $Fine
282400120502     C                   Exsr      EndPgm
282500120502     C                   EndIf
282600040305     C*
282700040305     C* testo se sono stati passati i dati di autorizzazione
282800040305     C*
282900120502     C     ParmS         IFLT      2
283000120502     C                   Move      *BLANK        AUTDS
283100990422     C                   ELSE
283200120502     C                   MoveL     DSAUT         AUTDS
283300120502     C                   EndIf
283400990422     C*
283500990422     C* reperisco le dimensioni delle variabili che conterranno i tasti
283600990422     C                   EVAL      $D1L01 = %Size(Z1Ke1)
283700990422     C                   EVAL      $D1L02 = %Size(Z1Ke2)
283800040305     x****               EVAL      $D2L01 = %Size(Z3Ke1)
283900040305     x****               EVAL      $D2L02 = %Size(Z3Ke2)
284000990422     C*
284100990422     C* Reperimento tasti di funzione
284200990422     C*
284300120502     C                   Exsr      REPKEY
284400990422     C* Richiamo routine per aggiunta tasti funzione personalizzati
284500120502     C                   Callb     'X46USREXT'
284600120502     C                   Parm                    *omit
284700120502     C                   Parm                    *omit
284800120502     C                   Parm                    *omit
284900120502     C                   Parm                    *omit
285000120502     C                   Parm                    *omit
285100120502     C                   Parm                    $FX1
285200120502     C                   Parm                    $FC1
285300120502     C                   Parm                    $PGF1
285400120502     C                   Parm                    DsPgm
285500120502     C                   Parm                    xsccut
285600120502     C                   Parm                    xscgrp
285700990422     C*
285800990422     C* Attiva Commitment control se necessario
285900990422     C*
286000120502     x*****              CallB     'XSTRCMT'
286100150626R186 X***                OPEN      ND77006L
286200150626R186 C                   OPEN      ND77020L
286300120502     C                   EndSr
286400990422     C/EJECT
286500990422     C************************************************************
286600990422     C* REPERIMENTO TASTI DI FUNZIONE
286700990422     C************************************************************
286800120502     C     REPKEY        BegSr
286900120502     C                   MoveA     $FM1          $FM
287000120502     C                   MoveA     $Fc1          $Fc
287100120502     C                   Exsr      REPKKK
287200120502     C                   MoveA     $FX           $FX1
287300990422     C*
287400120502     C                   EndSr
287500990422     C/EJECT
287600990422     C************************************************************
287700990422     C* REPERIMENTO TASTI DI FUNZIONE
287800990422     C************************************************************
287900120502     C     REPKKK        BegSr
288000990422     C*
288100120502     C                   Clear                   $FX
288200990422     C     1             DO        24            X
288300990422     C*
288400120502     C     $FM(X)        IfNE      *BLANK
288500120502     C     $Fc(X)        AndNE     '0'
288600990422     C*
288700120502     C                   MoveL     $FM(X)        IDMSG
288800120502     C                   CallB     'XRTVM'
288900120502     C                   Parm                    IDMSG            27
289000120502     C                   Parm                    TXTMSG          100
289100120502     C                   Parm                    ERRMSG            1
289200990422     C*
289300120502     C     ERRMSG        IfNE      *ON
289400120502     C                   MoveL     TXTMSG        $FX(X)
289500990422     C                   ELSE
289600120502     C                   MoveL     *ALL' '       $FX(X)
289700120502     C                   EndIf
289800990422     C*
289900120502     C                   EndIf
290000990422     C*
290100990422     C                   ENDDO
290200990422     C*
290300120502     C                   EndSr
290400990422     C/EJECT
290500990423     C************************************************************
290600990423     C* Decodifica conto
290700990423     C************************************************************
290800120502     C     CallMvc002    BegSr
290900990423     C*
291000120502     C                   Clear                   ND002DS
291100990423     C                   EVAL      $LenOut = %Size(ND002DS)
291200120502     C                   CallB     'NDMVC002'
291300120502     C                   Parm      XScSoc        $Soc
291400120502     C                   Parm                    $Kcc
291500120502     C                   Parm                    $Ksc
291600120502     C                   Parm                    *Omit
291700120502     C                   Parm      *OFF          $GesMsg
291800120502     C                   Parm      *OFF          $Esito
291900120502     C                   Parm      'ND002DS'     $Struttura
292000120502     C                   Parm                    ND002DS
292100120502     C                   Parm                    $LenOut
292200990423     C*
292300120502     C                   EndSr
292400990423     C/EJECT
292500990422     C************************************************************
292600990422     C* INIZIALIZZAZIONE VARIABILI
292700990422     C************************************************************
292800120502     C     INZVAR        BegSr
292900990422     C*
293000990422     C* Dati da PGM guida
293100990422     C*
293200120502    >C                   MoveL     KPJBU         NDCR75Ds
293300990422     C*
293400990422     C* Se società diversa da quella impostata nella *inzsr, rieseguo
293500990422     C* reperimento dati società
293600990422    >C                   If        SocietaR75 <> xscsoc    and
293700990422    >C                             OpzR75 <> *blank
293800120502     C                   MoveL     'SOC001'      TIPXSC
293900120502    >C                   MoveL     SocietaR75    SOCXSC
294000120502     C                   Exsr      REPSOC
294100120502     C     RTNXSC        IfNE      '1'
294200120502     C                   MoveL     XSOCDS        SOC001
294300990422     C                   ELSE
294400120502    >C                   Move      '1'           ErrR75
294500120502     C                   Move      *on           $Fine
294600120502     C                   Exsr      EndPgm
294700120502     C                   EndIf
294800120502     C                   EndIf
294900990422     C*
295000990422     C* Reperimento autorizzazioni
295100990422     C*
295200990422    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
295300990422    >C*   asteriscare la chiamata alla subroutine REPAUT
295400990422     C* le autorizzazioni potrebbero essere già state passate,
295500990422     C* nel qual caso non vanno reperite
295600120502    >C     AUTDS         IfEq      *BLANK
295700120502    >C***                Exsr      REPAUT
295800990422    >C                   ELSE
295900120502    >C***                MoveL     XCADDS        CONSCRDS
296000120502    >C                   EndIf
296100990422     C*
296200990422     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
296300990422     C* le autorizzazione
296400990422     C*
296500120502    >C******             Exsr      TSTAUT
296600990422     C*
296700990422     C*
296800990422     C* Definizione dei tasti funzione utilizzabili a pgm
296900990422     C*
297000120502     C                   Exsr      DEFOPZ
297100990422     C*
297200990422     C* Variabili per gestione videate
297300990422     C*
297400120502     C                   Move      'D1'          $GEST                          fmt in gestione
297500120502     C                   Move      *OFF          $FINE                          fine pgm
297600120502     C                   Move      *OFF          $FlgDef                        fine pgm
297700120502     C                   Move      *ON           $INZD1                         inizializzione D1
297800120502     C                   Move      *BLANK        $LASTG                         fmt di provenienza
297900120502     C                   Move      *BLANK        $LASTV                         ultima emissione
298000120502     C                   Z-Add     0             $ULKD1            3 0          piede da emett. D1
298100990422     C* azzero la posizione del cursore
298200120502     C                   Z-Add     0             H1RIGA
298300120502     C                   Z-Add     0             H1COLO
298400990422     C*
298500990422     C* Variabili appoggio
298600990422     C*
298700120502     C                   Z-Add     0             CURR
298800120502     C                   Z-Add     0             CURC
298900990422     C*
299000990422     C* Valorizzazione campi univoci testate
299100990422     C*
299200120502     C                   Clear                   FMTT1
299300120502     C                   MoveL     KNSIF         NOMSIF
299400120502     C                   MoveL     XSCDSI        NOMDIT
299500120502     C                   MoveL     DSPGM         NOMPGM
299600990422     C* recupero il sottotitolo (se c'è)
299700120502     C                   MoveL     *BLANK        T1TIT
299800120502     C     SOTTIT        IfNE      *BLANK
299900120502     C                   MoveL     SOTTIT        IDMSG
300000120502     C                   CallB     'XRTVM'
300100120502     C                   Parm                    IDMSG            27
300200120502     C                   Parm                    TXTMSG          100
300300120502     C                   Parm                    ERRMSG            1
300400120502     C                   Parm      *ON           CTRMSG            1            centratura
300500120502     C                   Parm      30            LENMSG            3 0          lun output.
300600120502     C     ERRMSG        IfNE      *ON
300700120502     C                   MoveL     TXTMSG        T1TIT
300800990422     C                   ELSE
300900120502     C                   MoveL     *ALL'?'       T1TIT
301000120502     C                   EndIf
301100120502     C                   EndIf
301200990422     C* recupero il tipo operazione
301300120502     C                   Z-Add     1             X
301400990422    >C     OPZR75        LOOKUP    $OZ(X)                                 21
301500120502     C     *IN21         IfEq      *ON
301600120502     C                   MoveL     $ID(X)        IDMSG
301700120502     C                   CallB     'XRTVM'
301800120502     C                   Parm                    IDMSG            27
301900120502     C                   Parm                    TXTMSG          100
302000120502     C                   Parm                    ERRMSG            1
302100120502     C                   Parm      *ON           CTRMSG            1            centratura
302200120502     C                   Parm      30            LENMSG            3 0          lun output.
302300120502     C     ERRMSG        IfNE      *ON
302400120502     C                   MoveL     TXTMSG        T1OPE
302500990422     C                   ELSE
302600120502     C                   MoveL     *ALL'?'       T1OPE
302700120502     C                   EndIf
302800120502     C                   EndIf
302900990422     C*
303000990422     C* Reperisco e memorizzo default in caso di immissione
303100990422     C*
303200990422    >C* azzero tutti i campi di appoggio default
303300120502    >C     OPZR75        IfEq      '01'
303400990422    >C* costruisco la chiave univoca per il reperimento dei default
303500120502    >C***                Move      WWWSOC        ___Societa
303600120502    >C***                Move      DF1002        ___Codice
303700120502    >C***                Move      DFn002        ___xxxxxxx
303800990422    >C***  K02___01      CHAIN     AN___01L                           21
303900120502     C     *IN21         IfEq      *OFF
304000990422     C                   EVAL      $FlgDef = *on
304100120502     C                   Exsr      REPDEF
304200120502     C                   EndIf
304300120502     C                   EndIf
304400990422     C*
304500990422     C* Inizializzazione/Decodifica prima videata
304600990422     C*
304700120502     C                   Exsr      INZD1
304800120502     C                   Move      *OFF          $INZD1
304900990422     C*
305000120502     C                   EndSr
305100990422     C/EJECT
305200990422     C************************************************************
305300990422     C* REPERIMENTO DATI SOCIETA'
305400990422     C************************************************************
305500120502     C     REPSOC        BegSr
305600990422     C*
305700120502     C                   CallB     'XSOC'
305800120502     C                   Parm                    TIPXSC            6
305900120502     C                   Parm                    SOCXSC            3
306000120502     C                   Parm                    CDSXSC            9 0
306100120502     C                   Parm                    MODXSC            3
306200120502     C                   Parm      *BLANKS       RTNXSC            1
306300120502     C                   Parm                    XSOCDS
306400120502     C                   Parm                    KPJBA
306500990422     C*
306600120502     C                   EndSr
306700990422     C/EJECT
306800990422     C************************************************************
306900990422     C* REPERIMENTO AUTORIZZAZIONI UTENTE
307000990422     C************************************************************
307100120502     C     REPAUT        BegSr
307200990422     C*
307300120502     C                   Clear                   AUTDS
307400990422     C*
307500990422     C*
307600120502     C                   EndSr
307700990422     C/EJECT
307800990422     C************************************************************
307900990422     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
308000990422    >C* in questo esempio, l'anagrafica possiede un flag di
308100990422    >C* autorizzazione per ognuna delle opzioni std
308200990422     C************************************************************
308300120502     C     TSTAUT        BegSr
308400990422     C*
308500120502     C                   Select
308600990422     C*
308700990422     C*
308800120502     C                   EndSl
308900990422     C*
309000120502     C                   EndSr
309100990422     C/EJECT
309200990422     C************************************************************
309300990422     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
309400990422     C************************************************************
309500120502     C     DEFOPZ        BegSr
309600990422     C*
309700990422     C* Questa routine deve valorizzare la schiera $FC1/$FR1
309800990422     C* con S o N a seconda della utilizzabilità o meno
309900990422     C* del tasto funzione correlato
310000990422     C*
310100990422     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
310200990422     C* di compilazione
310300120502     C                   Reset                   $FC1
310400120502     C                   Reset                   $FR1
310500990422     C*
310600990422     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
310700990422     C* invalido alcune opzioni
310800120502     C                   Select
310900990422     C* Se pgm richiamato in interrogazione disattivo :
311000120502    >C     OPZR75        WhenEq    '05'
311100990422     C* F4=Ricerca
311200120502     C                   Z-Add     4             X
311300120502     C                   Move      '0'           $FC1(X)
311400990422     C* F5=Ripristino
311500120502     C                   Z-Add     5             X
311600120502     C                   Move      '0'           $FC1(X)
311700990422     C* F6=Conferma
311800120502     C                   Z-Add     06            X
311900120502     C                   Move      '0'           $FC1(X)
312000990422     C* Se pgm richiamato in annullamento disattivo :
312100120502    >C     OPZR75        WhenEq    '04'
312200990422     C* F4=Ricerca
312300120502     C                   Z-Add     4             X
312400120502     C                   Move      '0'           $FC1(X)
312500990422     C* F5=Ripristino
312600120502     C                   Z-Add     5             X
312700120502     C                   Move      '0'           $FC1(X)
312800990422     C*
312900990422     C                   OTHER
313000990422     C*
313100120502     C                   EndSl
313200990422     C*
313300120502     C                   EndSr
313400990422     C/EJECT
313500990422     C************************************************************
313600990422     C* REPERIMENTO CAMPI DI DEFAULT
313700990422     C************************************************************
313800120502     C     REPDEF        BegSr
313900990422     C*
314000990422    >C* riempire dei campi di appoggio con tutti quei valori
314100990422    >C* che possono essere usati come default
314200990422     C*
314300120502     C                   EndSr
314400990528     C************************************************************
314500990528     C** Xdata  : controllo/inversione data                      *
314600990528     C************************************************************
314700120502     C     Xdata         BegSr
314800990528     C*
314900120502     C                   Callb     'XDT4000'
315000120502     C                   Parm                    XDTAMG           10
315100120502     C                   Parm                    XDTGMA            6 0
315200120502     C                   Parm                    XDTSTA            1 0
315300990528     C*
315400120502     C                   EndSr
315500990422     C************************************************************
315600990422     C* DEFINIZIONE KLIST
315700990422     C************************************************************
315800120502     C     DefKlist      BegSr
315900990422     C*
316000990422     C* klist
316100040305     C*
316200150626R186 X*** ND77006L
316300150626R186 X***  K09N7706      KLIST
316400150626R186 C* ND77020L
316500150626R186 C     K11N7720      KLIST
316600040305     C                   KFLD                    N77SOCIETA
316700040305     C                   KFLD                    N77ACF
316800040305     C                   KFLD                    N77Qua
316900040305     C                   KFLD                    N77Kcc
317000040305     C                   KFLD                    N77Ksc
317100040305     C                   KFLD                    N77RGT
317200040305     C                   KFLD                    N77PRG
317300150626R186 C                   KFLD                    N77C77
317400040305     C                   KFLD                    N77COG
317500040305     C                   KFLD                    N77NOM
317600150626R186 C                   KFLD                    N77CDF
317700040305     C*
317800040305     C     K03APE01      KLIST
317900040305     C                   KFLD                    APESOCIETA
318000040305     C                   KFLD                    APEKcc
318100040305     C                   KFLD                    APEKsc
318200990423     C*
318300990423     C* per soggetti
318400990423     C     PdvaSog       Plist
318500120502     C                   Parm                    DvaRic
318600120502     C                   Parm                    DvaSocieta
318700120502     C                   Parm                    DvaUnita
318800120502     C                   Parm                    DvaStrutt
318900120502     C                   Parm                    DvaDtRif
319000120502     C                   Parm                    DvaLenOut
319100120502     C                   Parm                    *Omit
319200120502     C                   Parm                    *Omit
319300120502     C                   Parm                    *Omit
319400120502     C                   Parm                    *Omit
319500120502     C                   Parm                    DvaSogg
319600120502     C                   Parm                    DvaTpInd
319700120502     C                   Parm                    DvaCdInd
319800120502     C                   Parm                    DvaErrore
319900120502     C                   Parm                    DvaOutPut
320000990423     C*
320100990422     C*
320200120502     C                   EndSr
320300990422     C/EJECT
320400990422     C************************************************************
320500990422     C* RIEMPIMENTO TASTI FUNZIONE
320600990422     C************************************************************
320700120502     C     RIEKEY        BegSr
320800990422     C*
320900990422     C* Input :
321000990422     C* - $FX = schiera con tutti i testi disponibili
321100990422     C* - $FC = schiere con i flag di validità associati ai testi
321200990422     C* - $FR = schiere con i flag di REVERSE IMAGE
321300990422     C* - $ULK = ultimo testo caricato
321400990422     C* - $L01 = Lunghezza prima riga
321500990422     C* - $L02 = Lunghezza seconda riga
321600990422     C* Output :
321700990422     C* - $ULK
321800990422     C* - $KEY1= 1° riga di tasti funzione
321900990422     C* - $KEY2= 2° riga di tasti funzione
322000990422     C*
322100120502     C                   CallB     'XKEYMSG'
322200990422     C* input
322300120502     C                   Parm                    $FX
322400120502     C                   Parm                    $FC
322500120502     C                   Parm                    $FR
322600120502     C                   Parm                    $ULK              3 0
322700990422     C* output
322800120502     C                   Parm                    $KEY1            79
322900120502     C                   Parm                    $KEY2            79
323000990422     C* input
323100120502     C                   Parm                    $L01              3 0
323200120502     C                   Parm                    $L02              3 0
323300990422     C*
323400120502C1913C                   Parm                    $ALLFUNCT       480
323500060224C1913C*
323600120502     C                   EndSr
323700990422**
32380099042201PROMSG    *LIBL     COS0001
32390099042202PROMSG    *LIBL     COS0002
32400099042203PROMSG    *LIBL     COS0003
32410099042204PROMSG    *LIBL     COS0004
32420099042205PROMSG    *LIBL     COS0005
324300990422** TASTI DI FUNZIONE  D1
324400990422PROMSG    *LIBL     KEY0001  01
324500990422PROMSG    *LIBL     KEY0002  02
324600990422PROMSG    *LIBL     KEY0003  03
324700990422PROMSG    *LIBL     KEY0004  04
324800990422PROMSG    *LIBL     KEY0005  05
324900990422PROMSG    *LIBL     KEY0006  06
325000990422PROMSG    *LIBL     KEY0007  07
325100990422PROMSG    *LIBL     KEY0008  08
325200990422PROMSG    *LIBL     KEY0009  09
325300990422PROMSG    *LIBL     KEY0010  10
325400990422PROMSG    *LIBL     KEY0011  11
325500990422PROMSG    *LIBL     KEY0012  12
325600990422PROMSG    *LIBL     KEY0013  13
325700990422PROMSG    *LIBL     KEY0014  14
325800990422PROMSG    *LIBL     KEY0015  15
325900990422PROMSG    *LIBL     KEY0016  16
326000990422PROMSG    *LIBL     KEY0017  17
326100990422PROMSG    *LIBL     KEY0018  18
326200990422PROMSG    *LIBL     KEY0019  19
326300990422PROMSG    *LIBL     KEY0020  20
326400990422PROMSG    *LIBL     KEY0021  21
326500990422PROMSG    *LIBL     KEY0022  22
326600990422PROMSG    *LIBL     KEY0023  23
326700990422PROMSG    *LIBL     KEY0024  24
326800990422**  ABILITAZIONE D1
3269009904220N  01
3270009904220N  02
3271009904221N  03
3272009904221N  04
3273009904221N  05
3274009904221N  06
3275009904220N  07
3276009904221N  08
3277009904220N  09
3278009904220N  10
3279009904220N  11
3280009904221N  12
3281009904220N  13
3282009904220N  14
3283009904220N  15
3284009904220N  16
3285009904220N  17
3286009904220N  18
3287009904220N  19
3288009904220N  20
3289009904220N  21
3290009904220N  22
3291009904220N  23   Impostare a '1' se si utilizzano opzioni
3292009904221N  24   Lasciare sempre impostata a '1' il valore della riga 24
