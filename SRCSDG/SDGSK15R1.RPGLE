000100160617     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200160617     H*PARMS BNDDIR(PJXBND PJCBND)
000300160617     H*PARMS CVTOPT(*datetime)
000400160617     H DECEDIT('0,') DATEDIT(*DMY.)
000500160617      *
000600160617      *
000700160617      *            Manutenzione mod.770 percipienti esteri
000800160617      *           -----------------------------------------
000900160617      *
001000160620R218 A* Questo pgm è stato creato nuovo con il task R218
001100160617      *
001200160617     F*----------------------------------------------------*
001300160617     F*------------
001400160617    >FANAPE01L  IF   E           K DISK
001500160617    >F                                     RENAME(anape000:anape01)
001600160617     F*------------
001700160617     FSDGN7720L UF A E           K DISK    USROPN
001800160617     F*------------
001900160617    >FSDGSK15D1 CF   E             WORKSTN
002000160617     F                                     RENAME(SK15D1:FMTD1)
002100160617     F                                     RENAME(SK15T1:FMTT1)
002200160617     F                                     RENAME(SK15Z1:FMTZ1)
002300160617    >F* eventuali schermi supplementari
002400160617    >F                                     RENAME(SK15D2:FMTD2)
002500160617     F                                     INFDS(DSFMT)
002600160617     F*------------
002700160617     D*----------------------------------------------------*
002800160617     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002900160617     D $ID             S             27    DIM(5) ALT($OZ)
003000160617     D* Schiere per la gestione dei tasti di funzione
003100160617     D $FM             S             27    DIM(24)
003200160617     D $FX             S             50    DIM(24)
003300160617     D $FC             S              1    DIM(24)
003400160617     D $FR             S              1    DIM(24)
003500160617     D* Video D1
003600160617     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003700160617     D $FX1            S             50    DIM(24) ALT($FM1)
003800160617     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003900160617     D $FR1            S              1    DIM(24) ALT($FC1)
004000160617     D* Schiera contente i nomi dei programmi da richiamare con i tasti
004100160617     D* funzione personalizzati
004200160617     D $PGF1           S             10    DIM(25)
004300160617     D*----------------------------------------------------*
004400160617     D $D1L01          S              3  0
004500160617     D $D1L02          S              3  0
004600160617     D*-------------
004700160617     D* Passaggio Parametri
004800160617     D KPJBA         E DS
004900160617     D*-------------
005000160617     D* DS Esterna gestione messaggi
005100160617     D SDGMSGDS      E DS                  INZ
005200160617     D  STS          E                     EXTFLD(MSGSTS)
005300160617     D                                     DIM(333)
005400160617     D  JBA          E                     EXTFLD(MSGJBA)
005500160617     D                                     DIM(502)
005600160617     D*-------------
005700160617     D* Reperimento Autorizzazioni
005800160617     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005900160617     D*-------------
006000160617     D* Passaggio Autorizzazioni
006100160617     D DSAUT           DS           512
006200160617     D*-------------
006300160617     D* Reperimento nome PGM
006400160617     D STATUS         SDS           333
006500160617     D  DSPGM            *PROC
006600160617     D  ParmS            *ParmS
006700160617     D*-------------
006800160617     D* Dati di ambiente ottenuti da SDGSOCDR
006900160617     D SOC001        E DS                  EXTNAME(SDGSOCDS )
007000160617     D*-------------
007100160617     D* DS Interna per dati di output di SDGSOCDR
007200160617     D XSOCDS          DS          1000
007300160617     D*-------------
007400160617     D* Passaggio Parametri al PGM di manutenzione
007500160617    >D SDGSK15DS     E DS
007600160617     D*-------------
007700160617     D* campo di salvataggio per la DS d'ingresso durante la chiamata
007800160617     D* alla funzione personalizzata
007900160617    >D $SaveDS         S                   like(SDGSk15DS)
008000160617     D*-------------
008100160617     D* Ricerca capoconti
008200160617     D Ana200Ds      E DS                  Inz
008300160617    >D  Ret200       E                     Inz('0')
008400160617    >D  Opr200       E                     Inz('0')
008500160617    >D  Err200       E                     Inz('0')
008600160617    >D  Cmt200       E                     Inz('0')
008700160617     D*-------------
008800160617     D* Ricerca sottoconti
008900160617     D Ana201DS      E DS                  Inz
009000160617    >D  Err201       E                     Inz('0')
009100160617     D*-------------
009200160617     D* Parametri per reperimento decodifica e soggetto relativi al conto
009300160617     D Nd002DS       E DS
009400160617     D*-------------
009500160617     D* Definizioni parametri decodifica conto
009600160617     D $Esito          S              1
009700160617     D $GesMsg         S              1
009800160617     D $LenOut         S              9B 0
009900160617     D $Struttura      S             10
010000160617     D $Kcc            S              6
010100160617     D $Ksc            S              8
010200160617     D $Soc            S              3
010300160617     d*-------------
010400160617     D DSFMT           DS
010500160617     D  $TASTO               369    369
010600160617     D  NRG                  370    370
010700160617     D  NCL                  371    371
010800160617     D*-------------
010900160617     D* DS per UserExit
011000160617     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
011100160617     D*-------------
011200160617     D* posizione cursore
011300160617     D CURSOR          DS
011400160617     D  RIRI                   1      2B 0 INZ
011500160617     D  R$$                    2      2
011600160617     D  COCO                   3      4B 0 INZ
011700160617     D  C$$                    4      4
011800160617     D*-------------
011900160617     D* Variabili appoggio specifiche della singola anagrafica
012000160617     D*  Indici
012100160617     D X               S              3S 0
012200160617     D Y               S              3S 0
012300160617     D*-------------
012400160617     D* Costanti
012500160617     D*-------------
012600160617     D* Tasti di funzione
012700160617     D F01             C                   CONST(X'31')
012800160617     D F02             C                   CONST(X'32')
012900160617     D F03             C                   CONST(X'33')
013000160617     D F04             C                   CONST(X'34')
013100160617     D F05             C                   CONST(X'35')
013200160617     D F06             C                   CONST(X'36')
013300160617     D F07             C                   CONST(X'37')
013400160617     D F08             C                   CONST(X'38')
013500160617     D F09             C                   CONST(X'39')
013600160617     D F10             C                   CONST(X'3A')
013700160617     D F11             C                   CONST(X'3B')
013800160617     D F12             C                   CONST(X'3C')
013900160617     D F13             C                   CONST(X'B1')
014000160617     D F14             C                   CONST(X'B2')
014100160617     D F15             C                   CONST(X'B3')
014200160617     D F16             C                   CONST(X'B4')
014300160617     D F17             C                   CONST(X'B5')
014400160617     D F18             C                   CONST(X'B6')
014500160617     D F19             C                   CONST(X'B7')
014600160617     D F20             C                   CONST(X'B8')
014700160617     D F21             C                   CONST(X'B9')
014800160617     D F22             C                   CONST(X'BA')
014900160617     D F23             C                   CONST(X'BB')
015000160617     D F24             C                   CONST(X'BC')
015100160617     D ENTER           C                   CONST(X'F1')
015200160617     D ROLDWN          C                   CONST(X'F4')
015300160617     D ROLLUP          C                   CONST(X'F5')
015400160617     D*-------------
015500160617     D* Sottotitolo di testata
015600160617    >D* se non si h$nno sottotitoli, mettere tutta la costante = ' '
015700160617     D SOTTIT          C                   CONST('               -
015800160617     D                                                 ')
015900160617     D*-------------
016000160617     D* Variabili per gestione videate
016100160617     D $Gest           S              2A
016200160617     D $Fine           S              1A
016300160617     D $InzD1          S              1A
016400160617     D $LastG          S              2A
016500160617     D $LastV          S              2A
016600160617     D $FlgDef         S              1A
016700160617     D*-------------
016800160617     D $COD_S          S             10    varying
016900160617     D*-------------
017000160617     D* Variabili appoggio sempre presenti in tutte le anagrafiche
017100160617     D CurR            S              2S 0
017200160617     D CurC            S              2S 0
017300160617     D Win             S             99A
017400160617     D DataISO         S               D
017500160617     D* data a 0
017600160617     D $DT0            C                   CONST('0001-01-01')
017700160617     D* Indice della schiera tasti funzione
017800160617     D PosFun          S              2  0
017900160617     D*-------------
018000160617     C*----------------------------------------------------*
018100160617     C*                MAIN LINE PROGRAM
018200160617     C*----------------------------------------------------*
018300160617     C*
018400160617     C* inizializzazione variabili
018500160617     C                   EXSR      INZVAR
018600160617     C*
018700160617     C     $FINE         DOWEQ     *OFF
018800160617     C     $GEST         CASEQ     'D1'          GESD1
018900160617     C     $GEST         CASEQ     'D2'          GESD2
019000160617     C                   ENDCS
019100160617     C                   ENDDO
019200160617     C*
019300160617     C* fine programma
019400160617     C                   EXSR      ENDPGM
019500160617     C/EJECT
019600160617     C************************************************************
019700160617     C* Fine Programma
019800160617     C************************************************************
019900160617     C     ENDPGM        BEGSR
020000160617     C*
020100160617     C* se non esco per errore
020200160617     C     ERRK15        IfEq      '0'
020300160617     C*
020400160617     C                   EndIf
020500160617     C*
020600160617     C* se ho confermato l'uscita proseguo
020700160617     C     $FINE         IfEq      *ON
020800160617     C*
020900160617     C* disallocazione rcd nel caso di modifica/annullamento
021000160617     C     OPZK15        IfEq      '02'
021100160617     C     OPZK15        OREQ      '04'
021200160617     C                   UNLOCK    SDGN7720L                            22
021300160617     C                   EndIf
021400160617     C*
021500160617     C* passaggio dati al pgm chiamante
021600160617     C                   MoveL     SDGSK15DS     KPJBU
021700160617     C*
021800160617     C                   SetOn                                            RT
021900160617     C                   RETURN
022000160617     C*
022100160617     C                   EndIf
022200160617     C*
022300160617     C                   ENDSR
022400160617     C/EJECT
022500160617     C************************************************************
022600160617     C* RICERCHE
022700160617     C************************************************************
022800160617     C     SEARCH        BEGSR
022900160617     C*
023000160617     C* determino Riga/Colonna del cursore
023100160617     C                   Move      NRG           R$$
023200160617     C                   Move      NCL           C$$
023300160617     C                   Z-Add     RIRI          CURR
023400160617     C                   Z-Add     COCO          CURC
023500160617     C*
023600160617     C* I campi che al momento dell'F4 risultano protetti non devono
023700160617     C* essere abilitati alla funzione di ricerca
023800160617     C*
023900160617     C                   Select
024000160617     C*
024100160617     C* FMTD1 - Capoconto
024200160617     C     H1NMFL        WhenEq    'KCC'
024300160617     C                   Reset                   ANA200DS
024400160617     C                   Move      '01'          OPZ200
024500160617     C                   Clear                   KPJBU
024600160617     C                   MoveL     ANA200DS      KPJBU
024700160617     C                   Call      'ANA200R'
024800160617     C                   Parm                    KPJBA
024900160617     C                   MoveL     KPJBU         ANA200DS
025000160617     C     Kcc200        IfNE      *BLANK
025100160617     C                   MoveL     Kcc200        Kcc
025200160617     C                   EndIf
025300160617     C*
025400160617     C* FMTD1  - Sottoconto
025500160617     C     H1NMFL        WhenEq    'KSC'
025600160617     C* allineo kcc conto
025700160617     C     Kcc           IfNE      *BLANKS
025800160617     C     Kcc           AndNE     'F     '
025900160617     C     Kcc           AndNE     'C     '
026000160617     C                   CallB     'XALLINEA'
026100160617     C                   Parm                    Kcc
026200160617     C                   Parm      6             LEN               5 0
026300160617     C                   Parm                    XSCINP
026400160617     C                   EndIf
026500160617     C*
026600160617     C                   Reset                   ANA201DS
026700160617     C                   Move      XscSoc        SOC201
026800160617     C                   Move      Kcc           KCC201
026900160617     C                   Move      Ksc           Ksc201
027000160617     C                   Clear                   KPJBU
027100160617     C                   MoveL     ANA201DS      KPJBU
027200160617     C                   Call      'ANA201R'
027300160617     C                   Parm                    KPJBA
027400160617     C                   MoveL     KPJBU         ANA201DS
027500160617     C     Ksc201        IfNE      *BLANK
027600160617     C                   MoveL     Kcc201        Kcc
027700160617     C                   MoveL     Ksc201        Ksc
027800160617     C                   EndIf
027900160617     C*
028000160617     C* ricerca non abilitata in questa posizione
028100160617     C                   OTHER
028200160617     C                   SetOn                                        9899
028300160617     C*
028400160617     C                   EndSl
028500160617     C*
028600160617     C* imposto pos. del cursore
028700160617     C                   Z-Add     CURR          H1RIGA
028800160617     C                   Z-Add     CURC          H1COLO
028900160617     C*
029000160617     C                   Move      *HIVAL        $LASTV
029100160617     C*
029200160617     C                   ENDSR
029300160617     C/EJECT
029400160617     C************************************************************
029500160617     C* GESTIONE EMISSIONE TASTI FUNZIONE
029600160617     C************************************************************
029700160617     C     GESPIE        BEGSR
029800160617     C*
029900160617     C                   Select
030000160617     C*
030100160617     C     $GEST         WhenEq    'D1'
030200160617     C     $ULKD1        IfEq      0
030300160617     C* riempimento opzioni
030400160617     C                   EXSR      F24D1
030500160617     C                   ELSE
030600160617     C                   WRITE     FMTZ1
030700160617     C                   EndIf
030800160617     C*
030900160617     C     $GEST         WhenEq    'D2'
031000160617     C     $ULKD1        IfEq      0
031100160617     C* riempimento opzioni
031200160617     C                   EXSR      F24D2
031300160617     C                   ELSE
031400160617     C                   WRITE     FMTZ1
031500160617     C                   EndIf
031600160617     C*
031700160617     C                   EndSl
031800160617     C*
031900160617     C                   ENDSR
032000160617     C/EJECT
032100160617     C************************************************************
032200160617     C* Chiamata programma con tasto personalizzato
032300160617     C************************************************************
032400160617     C     FPerD1        BEGSR
032500160617     C*
032600160617     C* salvo la DS di ingresso
032700160617    >C                   MoveL     SDGSK15DS     $SaveDS
032800160617     C*
032900160617    >C                   Move      xscsoc        SocietaK15
033000160617     C* passare nei campi appositi della DS SDGSK15DS il valore
033100160617     C* della chiave della VL univoca
033200160617    >C                   Z-Add     Vacf          AnnoK15
033300160617     C*
033400160617    >C                   MoveL     SDGSK15DS     KPJBU
033500160617    >C                   Call      $PGF1(PosFun)                        22
033600160617     C                   Parm                    KPJBA
033700160617     C*****              Parm                    NOMPGM
033800160617     C*
033900160617     C* ripristino la DS di ingresso
034000160617     C                   MoveL     $SaveDS       SDGSK15DS
034100160617     C*
034200160617     C                   ENDSR
034300160617     C/EJECT
034400160617     C************************************************************
034500160617     C* GESTIONE VIDEO RECORD D1
034600160617     C************************************************************
034700160617     C     GESD1         BEGSR
034800160617     C* inizializzazione videata
034900160617     C     $INZD1        IfEq      *ON
035000160617     C                   EXSR      INZD1
035100160617     C                   Move      *OFF          $INZD1
035200160617     C                   EndIf
035300160617     C* emissione piede videata se proveniente da altra
035400160617     C* salvataggio valore formato di provenienza
035500160617     C     $LASTV        IfNE      'D1'
035600160617     C                   WRITE     FMTT1
035700160617     C                   EXSR      GESPIE
035800160617     C                   Move      $LASTV        $LASTG
035900160617     C                   Move      'D1'          $LASTV
036000160617     C                   EndIf
036100160617     C*
036200160617     C* se esistono errori sulla videata
036300160617     C* emetto la write del formato a indicatori spenti per vedere
036400160617     C* le eventuali decodifiche
036500160617     C     *IN99         IfEq      *ON
036600160617     C     $GEST         ANDEQ     'D1'
036700160617     C                   MoveA     *IN           WIN
036800160617     C                   Move      *ALL'0'       IN5098           49
036900160617     C                   MoveA     IN5098        *IN(50)
037000160617     C                   WRITE     FMTD1
037100160617     C                   MoveA     WIN           *IN
037200160617     C                   EndIf
037300160617     C* Inizializzazione indice tasti funzione personalizzati
037400160617     C                   Eval      PosFun = 25
037500160617     C*
037600160617     C                   Select
037700160617     C* Annullamento
037800160617     C     OPZK15        WhenEq    '04'
037900160617     C* Visualizzazione
038000160617     C     OPZK15        OREQ      '05'
038100160617     C                   WRITE     FMTD1
038200160617     C                   EXFMT     PROTECT
038300160617     C* Immissione/Modifica/Copia
038400160617     C                   OTHER
038500160617     C                   EXFMT     FMTD1
038600160617     C                   EndSl
038700160617     C*
038800160617     C* azzero la posizione del cursore
038900160617     C                   Z-Add     0             H1RIGA
039000160617     C                   Z-Add     0             H1COLO
039100160617     C*
039200160617     C* controllo abilitazione tasti funzione
039300160617     C                   EXSR      CTRKD1
039400160617     C*
039500160617     C     *IN99         IfEq      *OFF
039600160617     C*
039700160617     C                   Select
039800160617     C* ROLLUP
039900160617     C     $TASTO        WhenEq    ROLLUP
040000160617     C                   Move      'D2'          $GEST
040100160617     C* F2=Decodifica
040200160617     C     $TASTO        WhenEq    F02
040300160617     C                   EXSR      F02D1
040400160617     C* F3=Fine
040500160617     C     $TASTO        WhenEq    F03
040600160617     C                   EXSR      F03D1
040700160617     C* F4=Ricerca
040800160617     C     $TASTO        WhenEq    F04
040900160617     C                   EXSR      SEARCH
041000160617     C* F5=Refresh
041100160617     C     $TASTO        WhenEq    F05
041200160617     C                   EXSR      F05D1
041300160617     C* F8=Successivo
041400160617     C     $TASTO        WhenEq    F08
041500160617     C                   EXSR      F08D1
041600160617     C* F12=Ritorno
041700160617     C     $TASTO        WhenEq    F12
041800160617     C                   EXSR      F12D1
041900160617     C* F24=Altri tasti
042000160617     C     $TASTO        WhenEq    F24
042100160617     C                   EXSR      F24D1
042200160617     C* Personalizzata
042300160617     C     $PGF1(PosFun) WHENNE    *BLANK
042400160617     C                   EXSR      FPerD1
042500160617     C*
042600160617     C                   OTHER
042700160617     C                   EXSR      CTRD1
042800160617     C* aggiungere le chiamate ai controlli delle videate supplementari
042900160617     C     *IN99         IfEq      *OFF
043000160617     C*
043100160617     C                   EXSR      CTRD2
043200160617     C     *IN99         IfEq      *ON
043300160617     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
043400160617     C* indicatori uguali in videate differenti, genererei conflitti
043500160617     C                   Move      *ALL'0'       IN5098           49
043600160617     C                   MoveA     IN5098        *IN(50)
043700160617     C                   SetOn                                        9699
043800160617     C                   EndIf
043900160617     C*
044000160617     C                   EndIf
044100160617     C*
044200160617     C     $TASTO        IfEq      F06
044300160617     C     *IN99         ANDEQ     *OFF
044400160617     C* eseguo aggiornamento
044500160617     C                   EXSR      AGGANA
044600160617     C* eseguo operazioni del dopo-aggiornamento
044700160617     C     *IN99         IfEq      *OFF
044800160617     C                   EXSR      GESAGG
044900160617     C                   EndIf
045000160617     C                   EndIf
045100160617     C*
045200160617     C                   EndSl
045300160617     C*
045400160617     C                   EndIf
045500160617     C*
045600160617     C                   ENDSR
045700160617     C/EJECT
045800160617     C************************************************************
045900160617     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
046000160617     C************************************************************
046100160617     C     CTRKD1        BEGSR
046200160617     C*
046300160617     C                   SETOFF                                       99
046400160617     C                   MoveA     $FC1          $FC
046500160617     C                   EXSR      CTRKEY
046600160617     C     *IN99         IfEq      *ON
046700160617     C                   SetOn                                        97
046800160617     C                   EndIf
046900160617     C*
047000160617     C                   ENDSR
047100160617     C/EJECT
047200160617     C************************************************************
047300160617     C* INIZIALIZZAZIONE VIDEATA DATI
047400160617     C************************************************************
047500160617     C     INZD1         BEGSR
047600160617     C*
047700160617     C                   Clear                   FMTD1
047800160617     C                   Clear                   FMTD2
047900160617     C*
048000160617     C     OPZK15        IfNE      '01'
048100160617     C                   MoveL     XscSoc        N77Societa
048200160617     C                   Z-Add     AnnoK15       N77ACF
048300160617     C                   MoveL     QuaK15        N77Qua
048400160617     C                   MoveL     KccK15        N77Kcc
048500160617     C                   MoveL     KscK15        N77Ksc
048600160617     C                   MoveL     RgtK15        N77Rgt
048700160617     C                   MoveL     PrgK15        N77Prg
048800160617     C                   MoveL     C77K15        N77C77
048900160617     C                   MoveL     CogK15        N77Cog
049000160617     C                   MoveL     NomK15        N77Nom
049100160617     C                   MOVEL     CDEK15        N77CDE
049200160617     C*
049300160617     C     *IN22         DOUEQ     *OFF
049400160617     C     MSGRTN        ORNE      'R'
049500160617     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
049600160617     C     OPZK15        IfEq      '02'
049700160617     C     OPZK15        OREQ      '04'
049800160617     C     K11N7720      CHAIN     SDGN7720L                          2122
049900160617     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
050000160617     C                   ELSE
050100160617     C     K11N7720      CHAIN(N)  SDGN7720L                          2122
050200160617     C                   EndIf
050300160617     C*
050400160617     C* Gestione RCD allocato
050500160617     C     *IN22         IfEq      *ON
050600160617     C                   MoveL     'SDGN7700F'   MSGFIL
050700160617     C                   EXSR      MSGPGM
050800160617     C                   EndIf
050900160617     C                   ENDDO
051000160617     C*
051100160617     C* se il rcd da gestire non viene trovato
051200160617     C* oppure è allocato da altro pgm
051300160617     C* ritorno al pgm guida avvertendolo dell'errore
051400160617     C     *IN21         IfEq      *ON
051500160617     C     *IN22         OREQ      *ON
051600160617     C                   Move      *ON           $FINE
051700160617     C                   Move      '1'           ERRK15
051800160617     C*
051900160617     C* altrimenti
052000160617     C                   ELSE
052100160617     C* riempio la videata
052200160617     C                   EXSR      RIED1
052300160617     C* Aggiungere le routine di riempimento dei record succcessivi
052400160617     C                   EXSR      RIED2
052500160617     C* richiamo routine dei ctrl per decodificarla
052600160617     C                   EXSR      CTRD1
052700160617     C* Aggiungere le routine di controllo dei record succcessivi
052800160617     C                   EXSR      CTRD2
052900160617     C                   Move      *ALL'0'       IN5098
053000160617     C                   MoveA     IN5098        *IN(50)
053100160617     C*
053200160617     C* imposto quegli attributi del sfl che sono modificabili
053300160617     C* al richiamo del determinato pgm
053400160617     C                   EXSR      ATRD1
053500160617     C                   EndIf
053600160617     C*
053700160617     C                   ELSE
053800160617     C*
053900160617     C* riempio con i default di immissione  (se reperiti)
054000160617     C                   IF        $FlgDef = *on
054100160617     C                   EXSR      RIEDEF
054200160617     C                   EndIf
054300160620     C*
054400160620     C                   Eval      Escludi     = XScOff
054500160617     C*
054600160617     C* imposto i campi chiave ricevuti anche se si è in immissione
054700160617     C* perchè potrebbero essere stati passati dei default di chiave
054800160617     C                   Z-Add     AnnoK15       Vacf
054900160617     C                   MoveL     QuaK15        Qua
055000160617     C                   If        OpzK15   <> '01'
055100160617     C                   MoveL     KccK15        Kcc
055200160617     C                   MoveL     KscK15        Ksc
055300160617     C                   MoveL     RGTK15        Rgt
055400160617     C                   MoveL     PrgK15        Prg
055500160617     C                   MoveL     C77K15        C77
055600160617     C                   EndIf
055700160617     C*
055800160617     C                   EXSR      ATRD1
055900160617     C*
056000160617     C                   EndIf
056100160617     C*
056200160617     C                   ENDSR
056300160617     C/EJECT
056400160617     C************************************************************
056500160617     C* RIEMPIMENTO VIDEATA  D1
056600160617     C************************************************************
056700160617     C     RIED1         BEGSR
056800160617     C*
056900160617     C* impostare i campi della videata dai campi del file
057000160617     C*
057100160617     C     OPZK15        IfEq      '01'
057200160617     c                   Z-Add     N77Acf        Vacf
057300160617     C                   MoveL     N77QUA        QUA
057400160617     C*
057500160617     C                   else
057600160617     C*
057700160617     c                   Z-Add     N77Acf        Vacf
057800160617     C                   MoveL     N77QUA        QUA
057900160617     C                   MoveL     N77Kcc        Kcc
058000160617     C                   MoveL     N77Ksc        Ksc
058100160617     C                   MoveL     N77RGT        RGT
058200160617     C                   MoveL     N77PRG        PRG
058300160617     C*
058400160617     C                   MoveL     N77Eventi     Eventi
058500160620     C                   MoveL     N77CdFS       CdFS
058600160620     C*
058700160620     C                   if        N77Escludi = *On
058800160620     C                   Move      XScOn         ESCLUDI
058900160620     C                   else
059000160620     C                   Move      XScOff        ESCLUDI
059100160620     C                   endif
059200160617     C*
059300160617     C                   MoveL     N77PFG        PFG
059400160617     C                   MoveL     N77COG        COG
059500160617     C                   MoveL     N77NOM        NOM
059600160617     C                   MoveL     N77SES        SES
059700160617     C*
059800160617     C                   MoveL     N77DNC        DataISO
059900160617     C                   IF        DataISO = *LOVAL
060000160617     C                   EVAL      DNC   = *ZEROS
060100160617     C                   ELSE
060200160617     C     *eur          MoveL     DataISO       DNC
060300160617     C                   EndIf
060400160617     C*
060500160617     C                   MoveL     N77CDE        CDE
060600160617     C                   MoveL     N77CIT        CIT
060700160617     C                   MoveL     N77VIA        VIA
060800160617     C                   MoveL(P)  N77Cse        Cse
060900160617     C*
061000160617     C                   endif
061100160617     C*
061200160617     C                   ENDSR
061300160617     C************************************************************
061400160617     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
061500160617     C************************************************************
061600160617     C     ATRD1         BEGSR
061700160617     C*
061800160617     C* in questa routine bisogna attivare indicatori o campi programma
061900160617     C* che gestiscono gli attributi di uno più campi di questa videata;
062000160617     C* attenzione : non è questo il posto in cui eseguire controlli,
062100160617     C*  dovrebbero già arrivare dei flag
062200160617     C*
062300160617     C                   SETOFF                                       02
062400160617     C* in caso di manutenzione
062500160617     C     OPZK15        IfEq      '02'
062600160617     C* proteggo i campi chiave
062700160617     C                   SetOn                                        02        PR campi key
062800160617     C                   EndIf
062900160617     C*
063000160617     C                   ENDSR
063100160617     C/EJECT
063200160617     C************************************************************
063300160617     C* RIEMPIMENTO CAMPI DAI DEFAULT
063400160617     C************************************************************
063500160617     C     RIEDEF        BEGSR
063600160617     C*
063700160617     C* riempire i campi con i default recuperati dal codice di
063800160617     C* riferimento
063900160617     C*
064000160617     C                   ENDSR
064100160617     C/EJECT
064200160617     C************************************************************
064300160617     C* GESTIONE F02 VIDEO D1
064400160617     C************************************************************
064500160617     C     F02D1         BEGSR
064600160617     C*
064700160617     C* per decodificare la videata richiamo la routine dei ctrl
064800160617     C* e poi disattivo gli eventuali messaggi di errore
064900160617     C                   EXSR      CTRD1
065000160617     C*
065100160617     C                   Move      *ALL'0'       IN5098
065200160617     C                   MoveA     IN5098        *IN(50)
065300160617     C*
065400160617     C                   ENDSR
065500160617     C/EJECT
065600160617     C************************************************************
065700160617     C* GESTIONE F03 VIDEO D1
065800160617     C************************************************************
065900160617     C     F03D1         BEGSR
066000160617     C*
066100160617     C                   Move      *ON           $FINE
066200160617     C                   Move      '1'           RETK15
066300160617     C* fine programma
066400160617     C                   EXSR      ENDPGM
066500160617     C*
066600160617     C                   ENDSR
066700160617     C/EJECT
066800160617     C************************************************************
066900160617     C* GESTIONE F05 VIDEO D1
067000160617     C************************************************************
067100160617     C     F05D1         BEGSR
067200160617     C*
067300160617     C                   Move      *ON           $INZD1
067400160617     C*
067500160617     C                   ENDSR
067600160617     C/EJECT
067700160617     C************************************************************
067800160617     C* GESTIONE F08 VIDEO D1
067900160617     C************************************************************
068000160617     C     F08D1         BEGSR
068100160617     C*
068200160617     C                   Move      *ON           $FINE
068300160617     C                   Move      '0'           RETK15
068400160617     C*
068500160617     C                   ENDSR
068600160617     C/EJECT
068700160617     C************************************************************
068800160617     C* GESTIONE F12 VIDEO D1
068900160617     C************************************************************
069000160617     C     F12D1         BEGSR
069100160617     C*
069200160617     C                   Move      *ON           $FINE
069300160617     C                   Move      '2'           RETK15
069400160617     C*
069500160617     C                   ENDSR
069600160617     C/EJECT
069700160617     C************************************************************
069800160617     C* GESTIONE F24 VIDEO D1
069900160617     C************************************************************
070000160617     C     F24D1         BEGSR
070100160617     C*
070200160617     C                   MoveA     $FX1          $FX
070300160617     C                   MoveA     $FC1          $FC
070400160617     C                   MoveA     $FR1          $FR
070500160617     C                   Z-Add     $ULKD1        $ULK
070600160617     C                   Z-Add     $D1L01        $L01
070700160617     C                   Z-Add     $D1L02        $L02
070800160617     C                   EXSR      RIEKEY
070900160617     C                   Z-Add     $ULK          $ULKD1            3 0
071000160617     C                   MoveA     $FC           $FC1
071100160617     C                   MoveL     $KEY1         Z1KE1
071200160617     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
071300160617     C                   MoveL     $KEY2         Z1KE2
071400160617     C                   WRITE     FMTZ1
071500160617     C*
071600160617     C                   ENDSR
071700160617     C/EJECT
071800160617     C************************************************************
071900160617     C* CONTROLLO VIDEATA
072000160617     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
072100160617     C* 95 96 97 98
072200160617     C************************************************************
072300160617     C     CTRD1         BEGSR
072400160617     C*
072500160617     C                   SETOFF                                       99
072600160621     C*
072700160621     C                   If        Eventi <> *blanks
072800160621     C                   CALL      'XALLINEA'
072900160621     C                   Parm                    Eventi
073000160621     C                   Parm      2             $Len              5 0
073100160621     C                   Parm      '2'           Tipo              1
073200160621     C                   Endif
073300160617     C*
073400160620     C                   if        OPZK15     <> '04'
073500160617     C*
073600160620     C                   EXSR      CtrConto
073700160620     C*
073800160620     C                   EXSR      CtrEventi
073900160620     C*
074000160620     C                   EXSR      CtrEscludi
074100160620     C*
074200160620     C                   EXSR      CtrNatGiu
074300160620     C*
074400160620     C                   EXSR      CtrCognome
074500160620     C*
074600160620     C                   EXSR      CtrPersFis
074700160620     C*
074800160620     C                   EXSR      CtrCodStatoE
074900160617     C*
075000160617     C*
075100160617     C*
075200160617     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
075300160617     C     OPZK15        IFEQ      '01'
075400160617     C     OPZK15        OREQ      '03'
075500160617     C                   MoveL     XscSoc        N77Societa
075600160617     C                   Z-Add     VACF          N77ACF
075700160617     C                   MoveL     Qua           N77Qua
075800160617     C                   MoveL     Kcc           N77Kcc
075900160617     C                   MoveL     Ksc           N77Ksc
076000160617     C                   MoveL     Rgt           N77Rgt
076100160617     C                   MoveL     Prg           N77Prg
076200160617     C                   MoveL     C77           N77C77
076300160617     C                   MoveL     Cog           N77Cog
076400160617     C                   MoveL     Nom           N77Nom
076500160617     C     K11N7720      SETLL     SDGN7720L                              21
076600160617     C     *IN21         IFEQ      '1'
076700160617     C                   SETON                                        5099
076800160617     C                   ENDIF
076900160617     C                   ENDIF
077000160617     C*
077100160620     C                   endif
077200160617     C*
077300160617     C                   ENDSR
077400160617     C/EJECT
077500160620     C************************************************************
077600160620     C* Controllo conto
077700160620     C************************************************************
077800160620     C     CtrConto      BEGSR
077900160620     C*
078000160620     C                   Clear                   D1KscDd
078100160620     C*
078200160620     C     Kcc           IfNE      'C'
078300160620     C     Kcc           AndNE     'F'
078400160620     C                   IF        Kcc        <> *Blank
078500160620     C                   CallB     'XALLINEA'
078600160620     C                   Parm                    Kcc
078700160620     C                   Parm      6             LEN               5 0
078800160620     C                   Parm                    XSCINP
078900160620     C                   EndIf
079000160620     C                   EndIf
079100160620     C*
079200160620     C                   IF        Ksc  <>   *Blank
079300160620     C                   CallB     'XALLINEA'
079400160620     C                   Parm                    Ksc
079500160620     C                   Parm      8             LEN               5 0
079600160620     C                   Parm                    XSCINP
079700160620     C                   EndIf
079800160620     C*
079900160620     C* Recupero capoconto
080000160620     C     Kcc           IfEq      'C       '
080100160620     C     Kcc           OREQ      'F       '
080200160620     C                   CallB     'XCAPCLIFOR'
080300160620     C                   Parm                    xscsoc
080400160620     C     Kcc           Parm                    Kcc
080500160620     C                   Parm                    Ksc
080600160620     C                   EndIf
080700160620     C*
080800160620     C*
080900160620     C                   If        Kcc       <>  *Blanks  and
081000160620     C                             Ksc       <>  *Blanks
081100160620     C*
081200160620     C                   Eval      $Kcc      =   Kcc
081300160620     C                   Eval      $Ksc      =   Ksc
081400160620     C                   EXSR      CallMVC002
081500160620     C                   if        $Esito    =   *Off
081600160620     C                   MoveL     DesBrev002    D1KscDd
081700160620     C                   else
081800160620     C                   SetOn                                        5299
081900160620     C                   endif
082000160620     C*
082100160620     C                   endif
082200160620     C*
082300160620     C                   ENDSR
082400160620     C/EJECT
082500160620     C************************************************************
082600160620     C* Decodifica conto
082700160620     C************************************************************
082800160620     C     CallMvc002    BEGSR
082900160620     C*
083000160620     C                   Clear                   ND002DS
083100160620     C                   EVAL      $LenOut = %Size(ND002DS)
083200160620     C                   CallB     'NDMVC002'
083300160620     C                   Parm      XScSoc        $Soc
083400160620     C                   Parm                    $Kcc
083500160620     C                   Parm                    $Ksc
083600160620     C                   Parm                    *Omit
083700160620     C                   Parm      *OFF          $GesMsg
083800160620     C                   Parm      *OFF          $Esito
083900160620     C                   Parm      'ND002DS'     $Struttura
084000160620     C                   Parm                    ND002DS
084100160620     C                   Parm                    $LenOut
084200160620     C*
084300160620     C                   ENDSR
084400160620     C/EJECT
084500160620     C************************************************************
084600160620     C* Controllo congruenza dati per eventi eccezionali
084700160620     C************************************************************
084800160620     C     CtrEventi     BEGSR
084900160620     C*
085000160620     C* entrambi o nessuno dei 2
085100160620     C                   if        (Eventi   = *Blank   and
085200160620     C                              CdFS     = *Blank       )
085300160620     C                             or
085400160620     C                             (Eventi  <> *Blank   and
085500160620     C                              CdFS    <> *Blank       )
085600160620     C                   else
085700160620     C                   SetOn                                        5799
085800160620     C                   endif
085900160620     C*
086000160620     C                   ENDSR
086100160620     C/EJECT
086200160620     C************************************************************
086300160620     C* Controllo esclusione da 770
086400160620     C************************************************************
086500160620     C     CtrEscludi    BEGSR
086600160620     C*
086700160620     C                   iF        Escludi <> XScOn   and
086800160620     C                             Escludi <> XScOff
086900160620     C                   SetOn                                        5399
087000160620     C                   endif
087100160620     C*
087200160620     C                   ENDSR
087300160620     C/EJECT
087400160620     C************************************************************
087500160620     C* Controllo natura giuridica
087600160620     C************************************************************
087700160620     C     CtrNatGiu     BEGSR
087800160620     C*
087900160620     C                   if        PFG     <> 'F'   and
088000160620     C                             PFG     <> 'G'
088100160620     C                   SetOn                                        5899
088200160620     C                   endif
088300160620     C*
088400160620     C                   ENDSR
088500160620     C/EJECT
088600160620     C************************************************************
088700160620     C* Controllo cognome / denominazione
088800160620     C************************************************************
088900160620     C     CtrCognome    BEGSR
089000160620     C*
089100160620     C                   if        COG      = *Blank
089200160620     C                   SetOn                                        6199
089300160620     C                   endif
089400160620     C*
089500160620     C                   ENDSR
089600160620     C/EJECT
089700160620     C************************************************************
089800160620     C* Controllo dati su persona fisica
089900160620     C************************************************************
090000160620     C     CtrPersFis    BEGSR
090100160620     C*
090200160620     C* se la natura giuridica è di persona fisica
090300160620     C                   IF        PFG         = 'F'         and
090400160620     C* nome, DNC e SES sono obbligatori
090500160620     C                             (NOM        = *Blank   or
090600160620     C                              DNC        = *Zero    or
090700160620     C                              SES        = *Blank     )
090800160620     C                   SetOn                                        5699
090900160620     C                   endif
091000160620     C*
091100160620     C*
091200160620     C* se la natura giuridica è di persona giuridica
091300160620     C                   IF        PFG         = 'G'         and
091400160620     C* nome, DNC e SES non devono essere impostati
091500160620     C                             (NOM       <> *Blank   or
091600160620     C                              DNC       <> *Zero    or
091700160620     C                              SES       <> *Blank     )
091800160620     C                   SetOn                                        5499
091900160620     C                   endif
092000160620     C*
092100160620     C*
092200160620     C                   if        DNC        <> 0
092300160620     C     *eur          Test(D)                 DNC                    21
092400160620     C                   if        *In21     =   *On
092500160620     C* data errata
092600160620     C                   SetOn                                        5599
092700160620     C                   endIf
092800160620     C                   endIf
092900160620     C*
093000160620     C                   ENDSR
093100160620     C/EJECT
093200160620     C************************************************************
093300160620     C* Controllo codice Stato Estero
093400160620     C************************************************************
093500160620     C     CtrCodStatoE  BEGSR
093600160620     C*
093700160620     C                   if        CSE      = *Blank
093800160620     C                   SetOn                                        6299
093900160620     C                   endif
094000160620     C*
094100160620     C                   ENDSR
094200160620     C/EJECT
094300160617     C************************************************************
094400160617     C* GESTIONE VIDEO RECORD  D2
094500160617     C************************************************************
094600160617    >C     GESD2         BEGSR
094700160617     C* emissione piede videata se proveniente da altra
094800160617     C* salvataggio valore formato di provenienza
094900160617     C     $LASTV        IfNE      'D2'
095000160617     C                   WRITE     FMTT1
095100160617     C                   EXSR      GESPIE
095200160617     C                   Move      $LASTV        $LASTG
095300160617     C                   Move      'D2'          $LASTV
095400160617     C                   EndIf
095500160617     C*
095600160617     C* se esistono errori sulla videata
095700160617     C* emetto la write del formato a indicatori spenti per vedere
095800160617     C* le eventuali decodifiche
095900160617     C     *IN99         IfEq      *ON
096000160617     C     $GEST         ANDEQ     'D2'
096100160617     C                   MoveA     *IN           WIN
096200160617     C                   Move      *ALL'0'       IN5098           49
096300160617     C                   MoveA     IN5098        *IN(50)
096400160617     C                   WRITE     FMTD2
096500160617     C                   MoveA     WIN           *IN
096600160617     C                   EndIf
096700160617     C* Inizializzazione indice tasti funzione personalizzati
096800160617     C                   Eval      PosFun = 25
096900160617     C*
097000160617     C                   Select
097100160617     C* Annullamento
097200160617     C     OPZK15        WhenEq    '04'
097300160617     C* Visualizzazione
097400160617     C     OPZK15        OREQ      '05'
097500160617     C                   WRITE     FMTD2
097600160617     C                   EXFMT     PROTECT
097700160617     C* Immissione/Modifica/Copia
097800160617     C                   OTHER
097900160617     C                   EXFMT     FMTD2
098000160617     C                   EndSl
098100160617     C*
098200160617     C* azzero la posizione del cursore
098300160617     C                   Z-Add     0             H1RIGA
098400160617     C                   Z-Add     0             H1COLO
098500160617     C*
098600160617     C* controllo abilitazione tasti funzione
098700160617     C                   EXSR      CTRKD1
098800160617     C*
098900160617     C     *IN99         IfEq      *OFF
099000160617     C*
099100160617     C                   Select
099200160617     C* ROLDOWN
099300160617     C     $TASTO        WhenEq    ROLDWN
099400160617     C                   Move      'D1'          $GEST
099500160617     C* F3=Fine
099600160617     C     $TASTO        WhenEq    F03
099700160617     C                   EXSR      F03D1
099800160617     C* F4=Ricerca
099900160617     C     $TASTO        WhenEq    F04
100000160617     C                   EXSR      SEARCH
100100160617     C* F5=Refresh
100200160617     C     $TASTO        WhenEq    F05
100300160617     C                   EXSR      F05D2
100400160617     C* F8=Successivo
100500160617     C     $TASTO        WhenEq    F08
100600160617     C                   EXSR      F08D1
100700160617     C* F12=Ritorno
100800160617     C     $TASTO        WhenEq    F12
100900160617     C                   EXSR      F12D1
101000160617     C* F24=Altri tasti
101100160617     C     $TASTO        WhenEq    F24
101200160617     C                   EXSR      F24D2
101300160617     C* Personalizzata
101400160617     C     $PGF1(PosFun) Whenne    *BLANK
101500160617     C                   EXSR      FperD1
101600160617     C*
101700160617     C                   OTHER
101800160617     C                   EXSR      CTRD2
101900160617     C     *IN99         IfEq      *OFF
102000160617     C*
102100160617     C                   EXSR      CTRD1
102200160617     C     *IN99         IfEq      *On
102300160617     C* Spengo gli indicatori di errore per il controllo su altri schermi
102400160617     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
102500160617     C* videate
102600160617     C                   Move      *ALL'0'       IN5098           49
102700160617     C                   MoveA     IN5098        *IN(50)
102800160617     C                   SetOn                                        9699
102900160617     C                   EndIf
103000160617     C*
103100160617     C                   Endif
103200160617     C*
103300160617     C*
103400160617     C     $TASTO        IfEq      F06
103500160617     C     *IN99         ANDEQ     *OFF
103600160617     C* eseguo aggiornamento
103700160617     C                   EXSR      AGGANA
103800160617     C* eseguo operazioni del dopo-aggiornamento
103900160617     C     *IN99         IfEq      *OFF
104000160617     C                   EXSR      GESAGG
104100160617     C                   EndIf
104200160617     C                   EndIf
104300160617     C*
104400160617     C                   EndSl
104500160617     C*
104600160617     C                   EndIf
104700160617     C*
104800160617     C                   ENDSR
104900160617     C/EJECT
105000160617     C************************************************************
105100160617     C* RIEMPIMENTO VIDEATA D2
105200160617     C************************************************************
105300160617     C     RIED2         BEGSR
105400160617     C*
105500160617     C*
105600160617     C                   MoveL     N77C77        C77
105700160617     C*
105800160617     C                   Z-Add     N77ITC        ITC
105900160617     C                   Z-Add     N77SNS        SNS
106000160617     C                   Z-Add     N77IMN        IMN
106100160617     C                   Z-Add     N77IMS        IMS
106200160617     C                   Z-Add     N77RIM        RIM
106300160617     C                   Z-Add     N77NET        NET
106400160617     C*
106500160617     C                   ENDSR
106600160617     C/EJECT
106700160617     C************************************************************
106800160617     C* GESTIONE F05 VIDEO D2
106900160617     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
107000160617     C* e il riposizionamento alla prima
107100160617     C************************************************************
107200160617     C     F05D2         BEGSR
107300160617     C*
107400160617     C                   Move      *ON           $INZD1
107500160617     C                   Move      'D1'          $GEST
107600160617     C*
107700160617     C                   ENDSR
107800160617     C************************************************************
107900160617     C* GESTIONE F24 VIDEO D2
108000160617     C************************************************************
108100160617     C     F24D2         BEGSR
108200160617     C*
108300160617     C                   MoveA     $FX1          $FX
108400160617     C                   MoveA     $FC1          $FC
108500160617     C                   MoveA     $FR1          $FR
108600160617     C                   Z-Add     $ULKD1        $ULK
108700160617     C                   Z-Add     $D1L01        $L01
108800160617     C                   Z-Add     $D1L02        $L02
108900160617     C                   EXSR      RIEKEY
109000160617     C                   Z-Add     $ULK          $ULKD1            3 0
109100160617     C                   MoveA     $FC           $FC1
109200160617     C                   MoveL     $KEY1         Z1KE1
109300160617     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
109400160617     C                   MoveL     $KEY2         Z1KE2
109500160617     C                   WRITE     FMTZ1
109600160617     C*
109700160617     C                   ENDSR
109800160617     C************************************************************
109900160617     C* CONTROLLO VIDEATA  D2
110000160617     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
110100160617     C* 95 96 97 98
110200160617     C************************************************************
110300160617     C     CTRD2         BEGSR
110400160617     C*
110500160617     C                   SETOFF                                       99
110600160617     C*
110700160617     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
110800160617     C*
110900160617     C     OPZK15        IfNE      '04'
111000160617     C*
111100160620     C                   EXSR      CtrCausale
111200160620     C                   EXSR      CtrLordo
111300160620     C*
111400160617     C                   EndIf
111500160617     C*
111600160617     C                   ENDSR
111700160617     C/EJECT
111800160617     C************************************************************
111900160620     C* Controllo causale
112000160617     C************************************************************
112100160620     C     CtrCausale    BEGSR
112200160617     C*
112300160620     C                   if        C77      = *Blank
112400160620     C                   SetOn                                        6399
112500160620     C                   endif
112600160620     C*
112700160620     C                   ENDSR
112800160620     C/EJECT
112900160620     C************************************************************
113000160620     C* Controllo ammontare lordo corrisposto
113100160620     C************************************************************
113200160620     C     CtrLordo      BEGSR
113300160620     C*
113400160620     C                   if        ITC      = *Zero
113500160620     C                   SetOn                                        6499
113600160620     C                   endif
113700160620     C*
113800160620     C                   ENDSR
113900160620     C/EJECT
114000160620     C************************************************************
114100160620     C* AGGIORNAMENTO ANAGRAFICA
114200160620     C************************************************************
114300160620     C     AGGANA        BEGSR
114400160620     C*
114500160617     C                   Select
114600160617    >C     OPZK15        WhenEq    '01'
114700160617     C     OPZK15        OREQ      '03'
114800160617     C                   EXSR      RIEANA
114900160617     C                   WRITE     SDGN77000                            22
115000160617     C* se rcd non scrivibile attivo errore
115100160617     C     *IN22         IfEq      *ON
115200160617     C                   SetOn                                        9599
115300160617     C                   EndIf
115400160617     C     OPZK15        WhenEq    '02'
115500160617     C                   EXSR      RIEANA
115600160617     C                   UPDATE    SDGN77000                            22
115700160617     C     OPZK15        WhenEq    '04'
115800160617     C                   DELETE    SDGN77000
115900160617     C                   EndSl
116000160617     C*
116100160617     C* richiama pgm User Exit
116200160617     c*
116300160617     C     USRERR        IfEq      *BLANK
116400160617     C* imposta chiave univoca  per user exit
116500160617     C                   Move      XSCSOC        XUKSOC
116600160617     C                   Move      XSCDUN        XUKDUN
116700160617     C                   MoveL     DSPGM         XUKPGM
116800160617     C* ho fino a 6 campi chiave passabili
116900160617     C                   MoveL     XUKDS         KPJBU
117000160617     C     DSPGM         CAT       'U':0         USREXT           10
117100160617     C                   Call      USREXT                               21
117200160617     C                   Parm                    KPJBA
117300160617     C     *IN21         IfEq      *ON
117400160617     C                   Move      *ON           USRERR            1
117500160617     C                   EndIf
117600160617     C                   EndIf
117700160617     C                   ENDSR
117800160617     C/EJECT
117900160617     C************************************************************
118000160617     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
118100160617     C************************************************************
118200160617     C     RIEANA        BEGSR
118300160617     C*
118400160617     C                   IF        OpzK15     =  '01'    or
118500160617     C                             OpzK15     =  '03'
118600160617     C*
118700160617     C* se inserimento
118800160617     C                   Clear                   SDGN77000
118900160617     C                   EndIf
119000160617     C*
119100160617     C                   MoveL     SocietaK15    N77Societa
119200160617     C                   Z-Add     VACF          N77ACF
119300160617     C                   MoveL     QUA           N77QUA
119400160617     C                   MoveL     Kcc           N77Kcc
119500160617     C                   MoveL     Ksc           N77Ksc
119600160617     C                   MoveL     RGT           N77RGT
119700160617     C                   MoveL     PRG           N77Prg
119800160617     C                   MoveL     Eventi        N77Eventi
119900160620     C                   MoveL     CdFS          N77CdFS
120000160620     C*
120100160620     C                   if        Escludi     = XSCON
120200160620     C                   eval      N77Escludi  = *On
120300160617     C                   else
120400160620     C                   eval      N77Escludi  = *Off
120500160617     C                   endif
120600160617     C*
120700160617     C                   MoveL     PFG           N77PFG
120800160617     C                   MoveL(P)  COG           N77COG
120900160617     C                   MoveL(P)  NOM           N77NOM
121000160617     C                   MoveL     SES           N77SES
121100160617     C*
121200160617     C                   MoveL     $DT0          N77DNC
121300160617     C     DNC           IfNE      0
121400160617     C     *eur          MoveL     DNC           DataISO
121500160617     C                   MoveL     DataIso       N77DNC
121600160617     C                   EndIf
121700160617     C*
121800160617     C                   MoveL(P)  CDE           N77CDE
121900160617     C                   MoveL(P)  CIT           N77CIT
122000160617     C                   MoveL     VIA           N77VIA
122100160617     C                   MoveL(P)  CSE           N77CSE
122200160617     C*
122300160617     C                   MoveL     C77           N77C77
122400160617     C*
122500160617     C                   Z-Add     ITC           N77ITC
122600160617     C                   Z-Add     SNS           N77SNS
122700160617     C                   Z-Add     IMN           N77IMN
122800160617     C                   Z-Add     IMS           N77IMS
122900160617     C                   Z-Add     RIM           N77RIM
123000160617     C                   Z-Add     NET           N77NET
123100160617     C*
123200160617     C                   ENDSR
123300160617     C/EJECT
123400160617     C************************************************************
123500160617     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
123600160617     C************************************************************
123700160617     C     GESAGG        BEGSR
123800160617     C*
123900160617     C* segnalo al pgm chiamante l'avvenuta conferma
124000160617     C                   Move      *ON           OPRK15
124100160617     C* segnalo al pgm chiamante l'aver premuto l'F6
124200160617     C                   Move      '0'           RETK15
124300160617     C*
124400160617     C                   Select
124500160617     C*
124600160617     C* nel caso di immissione
124700160617     C     OPZK15        WhenEq    '01'
124800160617     C* ripristino la videata iniziale
124900160617     C                   Move      *ON           $INZD1
125000160617     C                   Move      'D1'          $GEST
125100160617     C*
125200160617     C* nel caso di modifica
125300160617     C     OPZK15        WhenEq    '02'
125400160617     C* ripasso i campi che devono essere reimpostati sul sfl
125500160617     C                   Movel     Kcc           KccK15
125600160617     C                   Movel     Ksc           KscK15
125700160617     C                   Movel     RGT           RGTK15
125800160617     C                   Movel     PRG           PRGK15
125900160617     C                   MoveL     C77           C77K15
126000160617     C                   Z-Add     VACF          AnnoK15
126100160617     C                   Movel     Qua           QuaK15
126200160617     C                   Movel     Xscsoc        SocietaK15
126300160617     C                   Movel     Cog           CogK15
126400160617     C                   Movel     Nom           NomK15
126500160617     C                   Movel     CDE           CDEK15
126600160617     C* e ritorno al pgm chiamante
126700160617     C                   Move      *ON           $FINE
126800160617     C*
126900160617     C* per tutte le altre opzioni
127000160617     C                   OTHER
127100160617     C* ritorno al pgm chiamante
127200160617     C                   Move      *ON           $FINE
127300160617     C                   EndSl
127400160617     C*
127500160617     C                   ENDSR
127600160617     C/EJECT
127700160617     C************************************************************
127800160617     C* CONTROLLO ABILITAZIONE TASTI
127900160617     C************************************************************
128000160617     C     CTRKEY        BEGSR
128100160617     C*
128200160617     C                   Select
128300160617     C     $TASTO        WhenEq    F01
128400160617     C                   Z-Add     01            PosFun
128500160617     C     $FC(1)        IfEq      '0'
128600160617     C                   SetOn                                        99
128700160617     C                   EndIf
128800160617     C     $TASTO        WhenEq    F02
128900160617     C                   Z-Add     02            PosFun
129000160617     C     $FC(2)        IfEq      '0'
129100160617     C                   SetOn                                        99
129200160617     C                   EndIf
129300160617     C     $TASTO        WhenEq    F03
129400160617     C                   Z-Add     03            PosFun
129500160617     C     $FC(3)        IfEq      '0'
129600160617     C                   SetOn                                        99
129700160617     C                   EndIf
129800160617     C     $TASTO        WhenEq    F04
129900160617     C                   Z-Add     04            PosFun
130000160617     C     $FC(4)        IfEq      '0'
130100160617     C                   SetOn                                        99
130200160617     C                   EndIf
130300160617     C     $TASTO        WhenEq    F05
130400160617     C                   Z-Add     05            PosFun
130500160617     C     $FC(5)        IfEq      '0'
130600160617     C                   SetOn                                        99
130700160617     C                   EndIf
130800160617     C     $TASTO        WhenEq    F06
130900160617     C                   Z-Add     06            PosFun
131000160617     C     $FC(6)        IfEq      '0'
131100160617     C                   SetOn                                        99
131200160617     C                   EndIf
131300160617     C     $TASTO        WhenEq    F07
131400160617     C                   Z-Add     07            PosFun
131500160617     C     $FC(7)        IfEq      '0'
131600160617     C                   SetOn                                        99
131700160617     C                   EndIf
131800160617     C     $TASTO        WhenEq    F08
131900160617     C                   Z-Add     08            PosFun
132000160617     C     $FC(8)        IfEq      '0'
132100160617     C                   SetOn                                        99
132200160617     C                   EndIf
132300160617     C     $TASTO        WhenEq    F09
132400160617     C                   Z-Add     09            PosFun
132500160617     C     $FC(09)       IfEq      '0'
132600160617     C                   SetOn                                        99
132700160617     C                   EndIf
132800160617     C     $TASTO        WhenEq    F10
132900160617     C                   Z-Add     10            PosFun
133000160617     C     $FC(10)       IfEq      '0'
133100160617     C                   SetOn                                        99
133200160617     C                   EndIf
133300160617     C     $TASTO        WhenEq    F11
133400160617     C                   Z-Add     11            PosFun
133500160617     C     $FC(11)       IfEq      '0'
133600160617     C                   SetOn                                        99
133700160617     C                   EndIf
133800160617     C     $TASTO        WhenEq    F12
133900160617     C                   Z-Add     12            PosFun
134000160617     C     $FC(12)       IfEq      '0'
134100160617     C                   SetOn                                        99
134200160617     C                   EndIf
134300160617     C     $TASTO        WhenEq    F13
134400160617     C                   Z-Add     13            PosFun
134500160617     C     $FC(13)       IfEq      '0'
134600160617     C                   SetOn                                        99
134700160617     C                   EndIf
134800160617     C     $TASTO        WhenEq    F14
134900160617     C                   Z-Add     14            PosFun
135000160617     C     $FC(14)       IfEq      '0'
135100160617     C                   SetOn                                        99
135200160617     C                   EndIf
135300160617     C     $TASTO        WhenEq    F15
135400160617     C                   Z-Add     15            PosFun
135500160617     C     $FC(15)       IfEq      '0'
135600160617     C                   SetOn                                        99
135700160617     C                   EndIf
135800160617     C     $TASTO        WhenEq    F16
135900160617     C                   Z-Add     16            PosFun
136000160617     C     $FC(16)       IfEq      '0'
136100160617     C                   SetOn                                        99
136200160617     C                   EndIf
136300160617     C     $TASTO        WhenEq    F17
136400160617     C                   Z-Add     17            PosFun
136500160617     C     $FC(17)       IfEq      '0'
136600160617     C                   SetOn                                        99
136700160617     C                   EndIf
136800160617     C     $TASTO        WhenEq    F18
136900160617     C                   Z-Add     18            PosFun
137000160617     C     $FC(18)       IfEq      '0'
137100160617     C                   SetOn                                        99
137200160617     C                   EndIf
137300160617     C     $TASTO        WhenEq    F19
137400160617     C                   Z-Add     19            PosFun
137500160617     C     $FC(19)       IfEq      '0'
137600160617     C                   SetOn                                        99
137700160617     C                   EndIf
137800160617     C     $TASTO        WhenEq    F20
137900160617     C                   Z-Add     20            PosFun
138000160617     C     $FC(20)       IfEq      '0'
138100160617     C                   SetOn                                        99
138200160617     C                   EndIf
138300160617     C     $TASTO        WhenEq    F21
138400160617     C                   Z-Add     21            PosFun
138500160617     C     $FC(21)       IfEq      '0'
138600160617     C                   SetOn                                        99
138700160617     C                   EndIf
138800160617     C     $TASTO        WhenEq    F22
138900160617     C                   Z-Add     22            PosFun
139000160617     C     $FC(22)       IfEq      '0'
139100160617     C                   SetOn                                        99
139200160617     C                   EndIf
139300160617     C     $TASTO        WhenEq    F23
139400160617     C                   Z-Add     23            PosFun
139500160617     C     $FC(23)       IfEq      '0'
139600160617     C                   SetOn                                        99
139700160617     C                   EndIf
139800160617     C     $TASTO        WhenEq    F24
139900160617     C                   Z-Add     24            PosFun
140000160617     C     $FC(24)       IfEq      '0'
140100160617     C                   SetOn                                        99
140200160617     C                   EndIf
140300160617     C                   EndSl
140400160617     C*
140500160617     C                   ENDSR
140600160617     C/EJECT
140700160617     C******************************************************
140800160617     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
140900160617     C******************************************************
141000160617     C     MSGPGM        BEGSR
141100160617     C*
141200160617     C                   Z-Add     2             MSGMSG
141300160617     C                   MoveL     'S'           MSGRSP
141400160617     C                   MoveL     'CR'          MSGTPR
141500160617     C                   MoveL     'S'           MSGEMV
141600160617     C                   Z-Add     10            MSGRGP
141700160617     C                   MoveL     'S'           MSGLCK
141800160617     C                   MoveA     STATUS        STS
141900160617     C                   MoveL     'N'           MSGCNL
142000160617     C                   MoveL     'N'           MSGVID
142100160617     C                   MoveL     'N'           MSGOPE
142200160617     C                   MoveL     'N'           MSGSTP
142300160617     C                   CALL      'SDGMSGR'
142400160617     C                   Parm                    SDGMSGDS
142500160617     C*
142600160617     C                   ENDSR
142700160617     C/EJECT
142800160617     C************************************************************
142900160617     C* OPERAZIONI INIZIALI
143000160617     C************************************************************
143100160617     C     *INZSR        BEGSR
143200160617     C*
143300160617     C* Reperimento parametri
143400160617     C*
143500160617     C     *ENTRY        PLIST
143600160617     C                   Parm                    KPJBA
143700160617     C                   Parm                    DSAUT
143800160617     C*
143900160617     C* Reperimento dati società
144000160617     C*
144100160617    >C                   MoveL     'SOC001'      TIPXSC
144200160617     C                   MoveL     *BLANK        SOCXSC
144300160617     C                   EXSR      REPSOC
144400160617     C*
144500160617     C* Controlli dati società
144600160617     C*
144700160617     C     RTNXSC        IfNE      '1'
144800160617     C                   MoveL     XSOCDS        SOC001
144900160617     C                   ELSE
145000160617     C                   Move      '1'           ErrK15
145100160617     C                   Move      *on           $Fine
145200160617     C                   EXSR      EndPgm
145300160617     C                   EndIf
145400160617     C*
145500160617     C* testo se sono stati passati i dati di autorizzazione
145600160617     C*
145700160617     C     ParmS         IFLT      2
145800160617     C                   Move      *BLANK        AUTDS
145900160617     C                   ELSE
146000160617     C                   MoveL     DSAUT         AUTDS
146100160617     C                   EndIf
146200160617     C*
146300160617     C* reperisco le dimensioni delle variabili che conterranno i tasti
146400160617     C                   EVAL      $D1L01 = %Size(Z1Ke1)
146500160617     C                   EVAL      $D1L02 = %Size(Z1Ke2)
146600160617     C*
146700160617     C* Reperimento tasti di funzione
146800160617     C*
146900160617     C                   EXSR      REPKEY
147000160617     C* Richiamo routine per aggiunta tasti funzione personalizzati
147100160617     C                   Callb     'X46USREXT'
147200160617     C                   Parm                    *omit
147300160617     C                   Parm                    *omit
147400160617     C                   Parm                    *omit
147500160617     C                   Parm                    *omit
147600160617     C                   Parm                    *omit
147700160617     C                   Parm                    $FX1
147800160617     C                   Parm                    $FC1
147900160617     C                   Parm                    $PGF1
148000160617     C                   Parm                    DsPgm
148100160617     C                   Parm                    xsccut
148200160617     C                   Parm                    xscgrp
148300160617     C*
148400160617     C                   OPEN      SDGN7720L
148500160617     C*
148600160617     C                   ENDSR
148700160617     C/EJECT
148800160617     C************************************************************
148900160617     C* REPERIMENTO TASTI DI FUNZIONE
149000160617     C************************************************************
149100160617     C     REPKEY        BEGSR
149200160617     C                   MoveA     $FM1          $FM
149300160617     C                   MoveA     $Fc1          $Fc
149400160617     C                   EXSR      REPKKK
149500160617     C                   MoveA     $FX           $FX1
149600160617     C*
149700160617     C                   ENDSR
149800160617     C/EJECT
149900160617     C************************************************************
150000160617     C* REPERIMENTO TASTI DI FUNZIONE
150100160617     C************************************************************
150200160617     C     REPKKK        BEGSR
150300160617     C*
150400160617     C                   Clear                   $FX
150500160617     C     1             DO        24            X
150600160617     C*
150700160617     C     $FM(X)        IfNE      *BLANK
150800160617     C     $Fc(X)        AndNE     '0'
150900160617     C*
151000160617     C                   MoveL     $FM(X)        IDMSG
151100160617     C                   CallB     'XRTVM'
151200160617     C                   Parm                    IDMSG            27
151300160617     C                   Parm                    TXTMSG          100
151400160617     C                   Parm                    ERRMSG            1
151500160617     C*
151600160617     C     ERRMSG        IfNE      *ON
151700160617     C                   MoveL     TXTMSG        $FX(X)
151800160617     C                   ELSE
151900160617     C                   MoveL     *ALL' '       $FX(X)
152000160617     C                   EndIf
152100160617     C*
152200160617     C                   EndIf
152300160617     C*
152400160617     C                   ENDDO
152500160617     C*
152600160617     C                   ENDSR
152700160617     C/EJECT
152800160617     C************************************************************
152900160617     C* INIZIALIZZAZIONE VARIABILI
153000160617     C************************************************************
153100160617     C     INZVAR        BEGSR
153200160617     C*
153300160617     C* Dati da PGM guida
153400160617     C*
153500160617    >C                   MoveL     KPJBU         SDGSK15DS
153600160617     C*
153700160617     C* Se società diversa da quella impostata nella *inzsr, rieseguo
153800160617     C* reperimento dati società
153900160617    >C                   If        SocietaK15 <> xscsoc    and
154000160617    >C                             OpzK15 <> *blank
154100160617     C                   MoveL     'SOC001'      TIPXSC
154200160617    >C                   MoveL     SocietaK15    SOCXSC
154300160617     C                   EXSR      REPSOC
154400160617     C     RTNXSC        IfNE      '1'
154500160617     C                   MoveL     XSOCDS        SOC001
154600160617     C                   ELSE
154700160617    >C                   Move      '1'           ErrK15
154800160617     C                   Move      *on           $Fine
154900160617     C                   EXSR      EndPgm
155000160617     C                   EndIf
155100160617     C                   EndIf
155200160617     C*
155300160617     C*
155400160617     C* Definizione dei tasti funzione utilizzabili a pgm
155500160617     C*
155600160617     C                   EXSR      DEFOPZ
155700160617     C*
155800160617     C* Variabili per gestione videate
155900160617     C*
156000160617     C                   Move      'D1'          $GEST                          fmt in gestione
156100160617     C                   Move      *OFF          $FINE                          fine pgm
156200160617     C                   Move      *OFF          $FlgDef                        fine pgm
156300160617     C                   Move      *ON           $INZD1                         inizializzione D1
156400160617     C                   Move      *BLANK        $LASTG                         fmt di provenienza
156500160617     C                   Move      *BLANK        $LASTV                         ultima emissione
156600160617     C                   Z-Add     0             $ULKD1            3 0          piede da emett. D1
156700160617     C* azzero la posizione del cursore
156800160617     C                   Z-Add     0             H1RIGA
156900160617     C                   Z-Add     0             H1COLO
157000160617     C*
157100160617     C* Variabili appoggio
157200160617     C*
157300160617     C                   Z-Add     0             CURR
157400160617     C                   Z-Add     0             CURC
157500160617     C*
157600160617     C* Valorizzazione campi univoci testate
157700160617     C*
157800160617     C                   Clear                   FMTT1
157900160617     C                   MoveL     KNSIF         NOMSIF
158000160617     C                   MoveL     XSCDSI        NOMDIT
158100160617     C                   MOVEL     KCDAZ         NOMAZN
158200160617     C****               MOVEL     DSPGM         NOMPGM
158300160617     C* recupero il sottotitolo (se c'è)
158400160617     C                   MoveL     *BLANK        T1TIT
158500160617     C     SOTTIT        IfNE      *BLANK
158600160617     C                   MoveL     SOTTIT        IDMSG
158700160617     C                   CallB     'XRTVM'
158800160617     C                   Parm                    IDMSG            27
158900160617     C                   Parm                    TXTMSG          100
159000160617     C                   Parm                    ERRMSG            1
159100160617     C                   Parm      *ON           CTRMSG            1            centratura
159200160617     C                   Parm      30            LENMSG            3 0          lun output.
159300160617     C     ERRMSG        IfNE      *ON
159400160617     C                   MoveL     TXTMSG        T1TIT
159500160617     C                   ELSE
159600160617     C                   MoveL     *ALL'?'       T1TIT
159700160617     C                   EndIf
159800160617     C                   EndIf
159900160617     C* recupero il tipo operazione
160000160617     C                   Z-Add     1             X
160100160617    >C     OPZK15        LOOKUP    $OZ(X)                                 21
160200160617     C     *IN21         IfEq      *ON
160300160617     C                   MoveL     $ID(X)        IDMSG
160400160617     C                   CallB     'XRTVM'
160500160617     C                   Parm                    IDMSG            27
160600160617     C                   Parm                    TXTMSG          100
160700160617     C                   Parm                    ERRMSG            1
160800160617     C                   Parm      *ON           CTRMSG            1            centratura
160900160617     C                   Parm      30            LENMSG            3 0          lun output.
161000160617     C     ERRMSG        IfNE      *ON
161100160617     C                   MoveL     TXTMSG        T1OPE
161200160617     C                   ELSE
161300160617     C                   MoveL     *ALL'?'       T1OPE
161400160617     C                   EndIf
161500160617     C                   EndIf
161600160617     C*
161700160617     C* Reperisco e memorizzo default in caso di immissione
161800160617     C*
161900160617    >C* azzero tutti i campi di appoggio default
162000160617    >C     OPZK15        IfEq      '01'
162100160617     C                   EXSR      REPDEF
162200160617     C                   EndIf
162300160617     C*
162400160617     C* Inizializzazione/Decodifica prima videata
162500160617     C*
162600160617     C                   EXSR      INZD1
162700160617     C                   Move      *OFF          $INZD1
162800160617     C*
162900160617     C                   ENDSR
163000160617     C/EJECT
163100160617     C************************************************************
163200160617     C* REPERIMENTO DATI SOCIETA'
163300160617     C************************************************************
163400160617     C     REPSOC        BEGSR
163500160617     C*
163600160617     C                   CALL      'SDGSOCDR'
163700160617     C                   Parm                    TIPXSC            6
163800160617     C                   Parm                    SOCXSC            3
163900160617     C                   Parm                    CDSXSC            9 0
164000160617     C                   Parm                    MODXSC            3
164100160617     C                   Parm      *BLANKS       RTNXSC            1
164200160617     C                   Parm                    XSOCDS
164300160617     C                   Parm                    KPJBA
164400160617     C*
164500160617     C                   ENDSR
164600160617     C/EJECT
164700160617     C************************************************************
164800160617     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
164900160617     C************************************************************
165000160617     C     DEFOPZ        BEGSR
165100160617     C*
165200160617     C* Questa routine deve valorizzare la schiera $FC1/$FR1
165300160617     C* con S o N a seconda della utilizzabilità o meno
165400160617     C* del tasto funzione correlato
165500160617     C*
165600160617     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
165700160617     C* di compilazione
165800160617     C                   Reset                   $FC1
165900160617     C                   Reset                   $FR1
166000160617     C*
166100160617     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
166200160617     C* invalido alcune opzioni
166300160617     C                   Select
166400160617     C* Se pgm richiamato in interrogazione disattivo :
166500160617    >C     OPZK15        WhenEq    '05'
166600160617     C* F4=Ricerca
166700160617     C                   Z-Add     4             X
166800160617     C                   Move      '0'           $FC1(X)
166900160617     C* F5=Ripristino
167000160617     C                   Z-Add     5             X
167100160617     C                   Move      '0'           $FC1(X)
167200160617     C* F6=Conferma
167300160617     C                   Z-Add     06            X
167400160617     C                   Move      '0'           $FC1(X)
167500160617     C* Se pgm richiamato in annullamento disattivo :
167600160617    >C     OPZK15        WhenEq    '04'
167700160617     C* F4=Ricerca
167800160617     C                   Z-Add     4             X
167900160617     C                   Move      '0'           $FC1(X)
168000160617     C* F5=Ripristino
168100160617     C                   Z-Add     5             X
168200160617     C                   Move      '0'           $FC1(X)
168300160617     C*
168400160617     C                   OTHER
168500160617     C*
168600160617     C                   EndSl
168700160617     C*
168800160617     C                   ENDSR
168900160617     C/EJECT
169000160617     C************************************************************
169100160617     C* REPERIMENTO CAMPI DI DEFAULT
169200160617     C************************************************************
169300160617     C     REPDEF        BEGSR
169400160617     C*
169500160620     C                   EVAL      $FlgDef = *on
169600160620     C*
169700160617    >C* riempire dei campi di appoggio con tutti quei valori
169800160617    >C* che possono essere usati come default
169900160617     C*
170000160617     C                   ENDSR
170100160617     C************************************************************
170200160617     C* DEFINIZIONE KLIST
170300160617     C************************************************************
170400160617     C     DefKlist      BEGSR
170500160617     C*
170600160617     C* klist
170700160617     C*
170800160617     C* SDGN7720L
170900160617     C     K11N7720      KLIST
171000160617     C                   KFLD                    N77SOCIETA
171100160617     C                   KFLD                    N77ACF
171200160617     C                   KFLD                    N77Qua
171300160617     C                   KFLD                    N77Kcc
171400160617     C                   KFLD                    N77Ksc
171500160617     C                   KFLD                    N77RGT
171600160617     C                   KFLD                    N77PRG
171700160617     C                   KFLD                    N77C77
171800160617     C                   KFLD                    N77COG
171900160617     C                   KFLD                    N77NOM
172000160617     C                   KFLD                    N77CDE
172100160617     C*
172200160617     C     K03APE01      KLIST
172300160617     C                   KFLD                    APESOCIETA
172400160617     C                   KFLD                    APEKcc
172500160617     C                   KFLD                    APEKsc
172600160617     C*
172700160617     C                   ENDSR
172800160617     C/EJECT
172900160617     C************************************************************
173000160617     C* RIEMPIMENTO TASTI FUNZIONE
173100160617     C************************************************************
173200160617     C     RIEKEY        BEGSR
173300160617     C*
173400160617     C* Input :
173500160617     C* - $FX = schiera con tutti i testi disponibili
173600160617     C* - $FC = schiere con i flag di validità associati ai testi
173700160617     C* - $FR = schiere con i flag di REVERSE IMAGE
173800160617     C* - $ULK = ultimo testo caricato
173900160617     C* - $L01 = Lunghezza prima riga
174000160617     C* - $L02 = Lunghezza seconda riga
174100160617     C* Output :
174200160617     C* - $ULK
174300160617     C* - $KEY1= 1° riga di tasti funzione
174400160617     C* - $KEY2= 2° riga di tasti funzione
174500160617     C*
174600160617     C                   CallB     'XKEYMSG'
174700160617     C* input
174800160617     C                   Parm                    $FX
174900160617     C                   Parm                    $FC
175000160617     C                   Parm                    $FR
175100160617     C                   Parm                    $ULK              3 0
175200160617     C* output
175300160617     C                   Parm                    $KEY1            79
175400160617     C                   Parm                    $KEY2            79
175500160617     C* input
175600160617     C                   Parm                    $L01              3 0
175700160617     C                   Parm                    $L02              3 0
175800160617     C*
175900160617     C                   Parm                    $ALLFUNCT       480
176000160617     C*
176100160617     C                   ENDSR
176200160617**
17630016061701PROMSG    *LIBL     COS0001
17640016061702PROMSG    *LIBL     COS0002
17650016061703PROMSG    *LIBL     COS0003
17660016061704PROMSG    *LIBL     COS0004
17670016061705PROMSG    *LIBL     COS0005
176800160617** TASTI DI FUNZIONE  D1
176900160617PROMSG    *LIBL     KEY0001  01
177000160617PROMSG    *LIBL     KEY0002  02
177100160617PROMSG    *LIBL     KEY0003  03
177200160617PROMSG    *LIBL     KEY0004  04
177300160617PROMSG    *LIBL     KEY0005  05
177400160617PROMSG    *LIBL     KEY0006  06
177500160617PROMSG    *LIBL     KEY0007  07
177600160617PROMSG    *LIBL     KEY0008  08
177700160617PROMSG    *LIBL     KEY0009  09
177800160617PROMSG    *LIBL     KEY0010  10
177900160617PROMSG    *LIBL     KEY0011  11
178000160617PROMSG    *LIBL     KEY0012  12
178100160617PROMSG    *LIBL     KEY0013  13
178200160617PROMSG    *LIBL     KEY0014  14
178300160617PROMSG    *LIBL     KEY0015  15
178400160617PROMSG    *LIBL     KEY0016  16
178500160617PROMSG    *LIBL     KEY0017  17
178600160617PROMSG    *LIBL     KEY0018  18
178700160617PROMSG    *LIBL     KEY0019  19
178800160617PROMSG    *LIBL     KEY0020  20
178900160617PROMSG    *LIBL     KEY0021  21
179000160617PROMSG    *LIBL     KEY0022  22
179100160617PROMSG    *LIBL     KEY0023  23
179200160617PROMSG    *LIBL     KEY0024  24
179300160617**  ABILITAZIONE D1
1794001606170N  01
1795001606170N  02
1796001606171N  03
1797001606171N  04
1798001606171N  05
1799001606171N  06
1800001606170N  07
1801001606171N  08
1802001606170N  09
1803001606170N  10
1804001606170N  11
1805001606171N  12
1806001606170N  13
1807001606170N  14
1808001606170N  15
1809001606170N  16
1810001606170N  17
1811001606170N  18
1812001606170N  19
1813001606170N  20
1814001606170N  21
1815001606170N  22
1816001606170N  23   Impostare a '1' se si utilizzano opzioni
1817001606171N  24   Lasciare sempre impostata a '1' il valore della riga 24
