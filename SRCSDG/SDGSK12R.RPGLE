000100150511     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200150511     H*PARMS CVTOPT(*NONE)
000300150511     H DECEDIT('0,') DATEDIT(*DMY.)
000400150511      *
000401150512      *
000402160728      *           ------------------------------------------------
000404160728      *                Storico percipienti a regime agevolato
000406150512      *                     Manutenzione
000407160728      *           ------------------------------------------------
000700150511      *
000701161020R235  * Pgm creato con il task R235 CU Percipiente in regime agevolato
000702161020R235  *
004400150511     F*----------------------------------------------------*
004401160728     FANTRI01L  IF   E           K DISK
004402160728     F*------------
004701160728     F* Storico percipienti a regime agevolato
004702160728     F* key: Soc/Kcc/Ksc/DtIni(desc)    UNIQUE
004703160728     FSDGSPA01L UF A E           K DISK
004800150511     F*------------
004900160728    >FSDGSK12D  CF   E             WORKSTN
005000160728    >F                                     RENAME(SK12D1:FMTD1)
005100160728    >F                                     RENAME(SK12T1:FMTT1)
005200160728    >F                                     RENAME(SK12Z1:FMTZ1)
005300150511    >F* eventuali schermi supplementari
005400160728    >F                                     RENAME(SK12D2:FMTD2)
005500150511     F                                     INFDS(DSFMT)
005600150511     F*------------
005700150511     D*----------------------------------------------------*
005800150511     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005900150511     D $ID             S             27    DIM(5) ALT($OZ)
006000150511     D* Schiere per la gestione dei tasti di funzione
006100150511     D $FM             S             27    DIM(24)
006200150511     D $FX             S             50    DIM(24)
006300150511     D $FC             S              1    DIM(24)
006400150511     D $FR             S              1    DIM(24)
006500150511     D* Video D1
006600150511     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006700150511     D $FX1            S             50    DIM(24) ALT($FM1)
006800150511     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006900150511     D $FR1            S              1    DIM(24) ALT($FC1)
007000150511     D* Schiera contente i nomi dei programmi da richiamare con i tasti
007100150511     D* funzione personalizzati
007200150511     D $PGF1           S             10    DIM(25)
007300150511     D*----------------------------------------------------*
007400150511     D $D1L01          S              3  0
007500150511     D $D1L02          S              3  0
007600150511     D*-------------
007700150511     D* Passaggio Parametri
007800150511     D KPJBA         E DS
007900150511     D*-------------
008000150511     D* DS Esterna gestione messaggi
008001150512     D SDGMSGDS      E DS
008200150511     D  STS          E                     EXTFLD(MSGSTS)
008300150511     D                                     DIM(333)
008400150511     D  JBA          E                     EXTFLD(MSGJBA)
008500150511     D                                     DIM(502)
008600150511     D*-------------
008700150511     D* Reperimento Autorizzazioni
008800150511     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
008900150511     D*-------------
009000150511     D* Passaggio Autorizzazioni
009100150511     D DSAUT           DS           512
009200150511     D*-------------
009300150511     D* Reperimento nome PGM
009400150511     D STATUS         SDS           333
009500150511     D  DSPGM            *PROC
009600150511     D  PARMS            *PARMS
009700150511     D*-------------
009701150512     D* Dati di ambiente ottenuti da SDGSOCDR
009702150512     D SOC001        E DS                  EXTNAME(SDGSOCDS )
009703150512     D*-------------
009704150512     D* DS Interna per dati di output di SDGSOCDR
009705150512     D XSOCDS          DS          1000
010300150511     D*-------------
010400150511     D* Passaggio Parametri al PGM di manutenzione
010500160728    >D SDGSK12DS     E DS
010600150511     D*-------------
010700150511     D* Passaggio Parametri al pgm di controllo sovrapposizione
010800160728    >D SDGSK13DS     E DS
010900150511     D*-------------
011000150511     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011100150511     D* alla funzione personalizzata
011200160728    >D $SaveDS         S                   like(SDGSK12DS)
011201160728     D*-------------
011202160728     D* Passaggio Parametri al PGM capoconti
011203160728     D ANA200DS      E DS
011204160728     D  CMT200       E                     INZ('0')
011205160728     D  RET200       E                     INZ('0')
011206160728     D  OPR200       E                     INZ('0')
011207160728     D  ERR200       E                     INZ('0')
011208160728     D  FSC200       E                     INZ('S')
011209160728     D*-------------
011210160728     D* Ricerca sottoconti
011211160728     D ANA201DS      E DS                  INZ
011212160728     D*-------------
011213160728     D* Controllo conti
011214160728     D ND002DS       E DS                  INZ
011215160728     D*-------------
011216160728     D* Definizioni parametri standard chiamate moduli
011217160728     D Esito           S              1
011218160728     D GesMsg          S              1
011219160728     D LenOut          S              9B 0
011220160728     D Data            S             10
011221160728     D StrutturaO      S             10
011222160728     D*-------------
011223160728     D* Passaggio Parametri
011224160728     D a095ds        E DS                  EXTNAME(ANA095DS)
013600150511     D*-------------
013700150511     D DSFMT           DS
013800150511     D  $TASTO               369    369
013900150511     D  NRG                  370    370
014000150511     D  NCL                  371    371
014100150511     D*-------------
014200150511     D* DS per UserExit
014300150511     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
014400150511     D*-------------
014500150511     D* posizione cursore
014600150511     D CURSOR          DS
014700150511     D  RIRI                   1      2B 0 INZ
014800150511     D  R$$                    2      2
014900150511     D  COCO                   3      4B 0 INZ
015000150511     D  C$$                    4      4
015100150511     D*-------------
015200150511     D* Costanti
015300150511     D*-------------
015400150511     D* Tasti di funzione
015500150511     D F01             C                   CONST(X'31')
015600150511     D F02             C                   CONST(X'32')
015700150511     D F03             C                   CONST(X'33')
015800150511     D F04             C                   CONST(X'34')
015900150511     D F05             C                   CONST(X'35')
016000150511     D F06             C                   CONST(X'36')
016100150511     D F07             C                   CONST(X'37')
016200150511     D F08             C                   CONST(X'38')
016300150511     D F09             C                   CONST(X'39')
016400150511     D F10             C                   CONST(X'3A')
016500150511     D F11             C                   CONST(X'3B')
016600150511     D F12             C                   CONST(X'3C')
016700150511     D F13             C                   CONST(X'B1')
016800150511     D F14             C                   CONST(X'B2')
016900150511     D F15             C                   CONST(X'B3')
017000150511     D F16             C                   CONST(X'B4')
017100150511     D F17             C                   CONST(X'B5')
017200150511     D F18             C                   CONST(X'B6')
017300150511     D F19             C                   CONST(X'B7')
017400150511     D F20             C                   CONST(X'B8')
017500150511     D F21             C                   CONST(X'B9')
017600150511     D F22             C                   CONST(X'BA')
017700150511     D F23             C                   CONST(X'BB')
017800150511     D F24             C                   CONST(X'BC')
017900150511     D ENTER           C                   CONST(X'F1')
018000150511     D ROLDWN          C                   CONST(X'F4')
018100150511     D ROLLUP          C                   CONST(X'F5')
018200150511     D*-------------
018300150511     D* Sottotitolo di testata
018400150511     D SOTTIT          C                   CONST(' ')
018500150511     D*-------------
018600150511     D* Variabili per gestione videate
018700150511     D $Gest           S              2A
018800150511     D $Fine           S              1A
018900150511     D $InzD1          S              1A
019000150511     D $LastG          S              2A
019100150511     D $LastV          S              2A
019200150511     D $FlgDef         S              1A
019300150511     D*-------------
019400150511     D* Variabili appoggio sempre presenti in tutte le anagrafiche
019500150511     D CurR            S              2S 0
019600150511     D CurC            S              2S 0
019700150511     D Win             S             99A
019800150511     D*-------------
019900150511     D* Variabili appoggio specifiche della singola anagrafica
020000150511     D $DtIni          S               D
020100150511     D $DtFin          S               D
020200150511     D*  Indici
020300150511     D X               S              3S 0
020400150511     D Y               S              3S 0
020500150511     D* Indice della schiera tasti funzione
020600150511     D PosFun          S              2  0
020601150514     D*-------------
020602150514     D* Campi di lavoro
020603160728     D WKcc            S                   Like(SPAKcc )
020604160728     D WKsc            S                   Like(SPAKsc )
020800150511     C*----------------------------------------------------*
020900150511     C*                MAIN LINE PROGRAM
021000150511     C*----------------------------------------------------*
021100150511     C*
021200150511     C* inizializzazione variabili
021300150511     C                   EXSR      INZVAR
021400150511     C*
021500150511     C     $FINE         DOWEQ     *OFF
021600150511     C     $GEST         CASEQ     'D1'          GESD1
021700150511    >C     $GEST         CASEQ     'D2'          GESD2
021800150511     C                   ENDCS
021900150511     C                   ENDDO
022000150511     C*
022100150511     C* fine programma
022200150512     C                   EXSR      ENDPGM
022300150511     C/EJECT
022400150511     C************************************************************
022500150511     C* Fine Programma
022600150511     C************************************************************
022700150512     C     ENDPGM        BEGSR
022800150511     C*
022900150511     C* se non esco per errore
023000160728    >C     ErrK12        IFEQ      '0'
025200150511     C*
025300150511     C                   ENDIF
025400150511     C*
025500150511     C* se ho confermato l'uscita proseguo
025600150511     C     $FINE         IFEQ      *ON
025700150511     C*
025800150511     C* disallocazione rcd nel caso di modifica/annullamento
025900160728    >C     OPZK12        IFEQ      '02'
026000160728    >C     OPZK12        OREQ      '04'
026100160728    >C                   UNLOCK    SDGSPA01L                            22
026200150511     C                   ENDIF
026300150511     C*
026400150511     C* passaggio dati al pgm chiamante
026500160728    >C                   MOVEL     SDGSK12DS     KPJBU
026600150511     C*
026700150511     C                   SETON                                            RT
026800150511     C                   RETURN
026900150511     C*
027000150511     C                   ENDIF
027100150511     C*
027200150511     C                   ENDSR
027300150511     C/EJECT
027400150511     C************************************************************
027500150511     C* RICERCHE
027600150511     C************************************************************
027700150511     C     SEARCH        BEGSR
027800150511     C*
027900150511     C* determino Riga/Colonna del cursore
028000150511     C                   MOVE      NRG           R$$
028100150511     C                   MOVE      NCL           C$$
028200150511     C                   Z-ADD     RIRI          CURR
028300150511     C                   Z-ADD     COCO          CURC
028301150512     C*
028303160728     C     D1KCC         IFNE      'C'
028304160728     C     D1KCC         ANDNE     'F'
028305160728     C                   CALLB     'XALLINEA'
028306160728     C                   PARM                    D1KCC
028307160728     C                   PARM      6             LEN               5 0
028308160728     C                   PARM                    XSCINP
028309160728     C                   ENDIF
028310160728     C*
028311160728     C                   if        D1Ksc      <> *Blank
028312160728     C                   CALLB     'XALLINEA'
028313160728     C                   PARM                    D1Ksc
028314160728     C                   PARM      8             LEN               5 0
028315160728     C                   PARM                    XSCINP
028316160728     C*
028317160728     C     D1KCC         IFEQ      'C       '
028318160728     C     D1KCC         OREQ      'F       '
028319160728     C                   CALLB     'XCAPCLIFOR'
028320160728     C                   PARM                    XScSoc
028321160728     C                   PARM      D1KCC         D1Kcc
028322160728     C                   PARM                    D1Ksc
028323160728     C                   ENDIF
028324160728     C*
028325160728     C                   endif
028403160728     C*
028404160728   B1C                   SELECT
028405160728     C*
028406160728     C* capoconto
028407160728     C     H1NMFL        WHENEQ    'D1KCC'
028408160728     C                   EXSR      F4_Kcc
028409160728     C*
028410160728     C* sottoconto
028411160728     C     H1NMFL        WHENEQ    'D1KSC'
028412160728     C                   EXSR      F4_Ksc
028413160728     C*
028414160728     C* tributo
028415160728     C     H1NMFL        WHENEQ    'D1TRIBUTO'
028416160728     C                   EXSR      F4_Tributo
032900150511     C*
033000150511     C* ricerca non abilitata in questa posizione
033100150511     C                   OTHER
033200150511     C                   SETON                                        9899
033300150511     C*
033400150511     C                   ENDSL
033500150511     C*
033600150511     C* imposto pos. del cursore
033700150511     C                   Z-ADD     CURR          H1RIGA
033800150511     C                   Z-ADD     CURC          H1COLO
033900150511     C*
034000150511     C                   MOVE      *HIVAL        $LASTV
034100150511     C*
034200150511     C                   ENDSR
034300150511     C/EJECT
034301160728     C************************************************************
034302160728     C* Ricerca capoconto
034303160728     C************************************************************
034304160728     C     F4_Kcc        BEGSR
034305160728     C*
034306160728     C                   Reset                   ANA200DS
034307160728     C*
034308160728     C                   MoveL     '01'          OPZ200
034309160728     C                   MoveL     D1Kcc         KCC200
034310160728     C                   MoveL     ANA200DS      KPJBU
034311160728     C                   CALL      'ANA200R'                            21
034312160728     C                   PARM                    KPJBA
034313160728     C                   MoveL     KPJBU         ANA200DS
034314160728     C*
034315160728     C     KCC200        IFNE      *BLANK
034316160728     C                   MOVE      KCC200        D1Kcc
034317160728     C                   ENDIF
034318160728     C*
034319160728     C                   ENDSR
034320160728     C/EJECT
034321160728     C************************************************************
034322160728     C* Ricerca conto
034323160728     C************************************************************
034324160728     C     F4_Ksc        BEGSR
034325160728     C*
034326160728     C                   Reset                   ANA201DS
034327160728     C                   MOVE      XSCSOC        SOC201
034328160728     C                   MOVE      D1KCC         KCC201
034329160728     C                   MOVE      D1Ksc         KSC201
034330160728     C                   MoveL     '01'          OPZ201
034331160728     C                   MoveL     ANA201DS      KPJBU
034332160728     C                   CALL      'ANA201R'
034333160728     C                   PARM                    KPJBA
034334160728     C                   MoveL     KPJBU         ANA201DS
034335160728     C*
034336160728     C     KSC201        IFNE      *BLANK
034337160728     C                   MoveL     KCC201        D1KCC
034338160728     C                   MoveL     KSC201        D1KSC
034339160728     C                   ENDIF
034340160728     C*
034341160728     C                   ENDSR
034342160728     C/EJECT
034343160728     C************************************************************
034344160728     C* Ricerca tributo
034345160728     C************************************************************
034346160728     C     F4_Tributo    BEGSR
034347160728     C*
034348160728    >C                   RESET                   A095ds
034349160728    >C                   MOVE      '01'          OPZ095
034350160728    >C                   CLEAR                   KPJBU
034351160728    >C                   MOVEL     D1Tributo     TRIBUTO095
034352160729    >C                   MOVEL     D1TriProg     TRIPROG095
034353160728    >C                   MOVEL     A095DS        KPJBU
034354160728    >C                   CALL      'ANA095R'
034355160728    >C                   PARM                    KPJBA
034356160728    >C                   MOVEL     KPJBU         A095DS
034357160728     C*
034358160728    >C     TRIbuto095    IFNE      *BLANK
034359160728    >C                   MOVEL     TRIbuto095    D1Tributo
034360160729    >C                   MOVE      triprog095    D1TriProg
034361160728    >C                   ENDIF
034362160728     C*
034363160728     C                   ENDSR
034364160728     C/EJECT
034400150511     C************************************************************
034500150511     C* GESTIONE EMISSIONE TASTI FUNZIONE
034600150511     C************************************************************
034700150511     C     GESPIE        BEGSR
034800150511     C*
034900150511     C                   SELECT
035000150511     C*
035100150511     C     $GEST         WHENEQ    'D1'
035200150511     C     $ULKD1        IFEQ      0
035300150511     C* riempimento opzioni
035400150511     C                   EXSR      F24D1
035500150511     C                   ELSE
035600150511     C                   WRITE     FMTZ1
035700150511     C                   ENDIF
035800150511     C*
035900150511     C     $GEST         WHENEQ    'D2'
036000150511     C     $ULKD1        IFEQ      0
036100150511     C* riempimento opzioni
036200150511     C                   EXSR      F24D2
036300150511     C                   ELSE
036400150511     C                   WRITE     FMTZ1
036500150511     C                   ENDIF
036600150511     C                   ENDSL
036700150511     C*
036800150511     C                   ENDSR
036900150511     C/EJECT
037000150511     C************************************************************
037100150511     C* Chiamata programma con tasto personalizzato
037200150511     C************************************************************
037300150511     C     FPerD1        BEGSR
037400150511     C*
037500150511     C* salvo la DS di ingresso
037600160728    >C                   MOVEL     SDGSK12DS     $SaveDS
037700150511     C*
037800160728    >C                   MOVE      xscsoc        SocietaK12
037900160728     C* passare nei campi appositi della DS SDGSK12DS il valore
038000150511     C* della chiave della VL univoca
038100160728    >C                   MOVEL     XScSoc        SocietaK12
038200160728    >C                   MOVEL     D1Kcc         KccK12
038201160728    >C                   MOVEL     D1Ksc         KscK12
038400150511    >C* siccome potrei non essere passato dalla ctrd1 (capita
038500150511    >C* in caso di opzione '4'), $dtini potrebbe non essere
038600150511    >C* ancora valorizzato per cui trasformo anche qui la data in iso
038700160728     C                   Move      *Loval        DtIniK12
038800150511     C     D1DtIni       IfNe      0
038900150511     C     *JobRun       Test(D)                 D1DtIni                21
039000150511     C     *In21         IfEq      *Off
039100160728     C     *JobRun       Move      D1DtIni       DtIniK12
039200150511     C                   EndIf
039300150511     C                   EndIf
039400150511     C*
039500160728    >C                   MOVEL     SDGSK12DS     KPJBU
039600150511     C                   CALL      $PGF1(PosFun)                        22
039700150511     C                   PARM                    KPJBA
039800150511     C                   PARM                    NOMPGM
039900150511     C*
040000150511     C* ripristino la DS di ingresso
040100160728    >C                   MOVEL     $SaveDS       SDGSK12DS
040200150511     C*
040300150511     C                   ENDSR
040400150511     C/EJECT
040500150511     C************************************************************
040600150511     C* GESTIONE VIDEO RECORD D1
040700150511     C************************************************************
040800150511     C     GESD1         BEGSR
040900150511     C* inizializzazione videata
041000150511   B1C     $INZD1        IFEQ      *ON
041100150511     C                   EXSR      INZD1
041200150511     C                   MOVE      *OFF          $INZD1
041300150511   E1C                   ENDIF
041400150511     C* emissione piede videata se proveniente da altra
041500150511     C* salvataggio valore formato di provenienza
041600150511   B1C     $LASTV        IFNE      'D1'
041700150511     C                   WRITE     FMTT1
041800150511     C                   EXSR      GESPIE
041900150511     C                   MOVE      $LASTV        $LASTG
042000150511     C                   MOVE      'D1'          $LASTV
042100150511   E1C                   ENDIF
042200150511     C*
042300150511     C* se esistono errori sulla videata
042400150511     C* emetto la write del formato a indicatori spenti per vedere
042500150511     C* le eventuali decodifiche
042600150511   B1C     *IN99         IFEQ      *ON
042700150511     C     $GEST         ANDEQ     'D1'
042800150511     C                   MOVEA     *IN           WIN
042900150511     C                   MOVE      *ALL'0'       IN5098           49
043000150511     C                   MOVEA     IN5098        *IN(50)
043100150511     C                   WRITE     FMTD1
043200150511     C                   MOVEA     WIN           *IN
043300150511   E1C                   ENDIF
043400150511     C* Inizializzazione indice tasti funzione personalizzati
043500150511     C                   Eval      PosFun = 25
043600150511     C*
043700150511   B1C                   SELECT
043800150511     C* Annullamento
043900160728    >C     OPZK12        WHENEQ    '04'
044000150511     C* Visualizzazione
044100160728    >C     OPZK12        OREQ      '05'
044200150511     C                   WRITE     FMTD1
044300150511     C                   EXFMT     PROTECT
044400150511     C* Immissione/Modifica/Copia
044500150511   X1C                   OTHER
044600150511     C                   EXFMT     FMTD1
044700150511   E1C                   ENDSL
044800150511     C*
044900150511     C* azzero la posizione del cursore
045000150511     C                   Z-ADD     0             H1RIGA
045100150511     C                   Z-ADD     0             H1COLO
045200150511     C*
045300150511     C* controllo abilitazione tasti funzione
045400150511     C                   EXSR      CTRKD1
045500150511     C*
045600150511   B1C     *IN99         IFEQ      *OFF
045700150511     C*
045800150511   B2C                   SELECT
045900150511     C* ROLLUP
046000150511    >C     $TASTO        WHENEQ    ROLLUP
046100150511    >C                   MOVE      'D2'          $GEST
046200150511     C* F2=Decodifica
046300150511     C     $TASTO        WHENEQ    F02
046400150511     C                   EXSR      F02D1
046500150511     C* F3=Fine
046600150511     C     $TASTO        WHENEQ    F03
046700150511     C                   EXSR      F03D1
046800150511     C* F4=Ricerca
046900150511     C     $TASTO        WHENEQ    F04
047000150511     C                   EXSR      SEARCH
047100150511     C* F5=Refresh
047200150511     C     $TASTO        WHENEQ    F05
047300150511     C                   EXSR      F05D1
047400150511     C* F8=Successivo
047500150511     C     $TASTO        WHENEQ    F08
047600150511     C                   EXSR      F08D1
047700150511     C* F12=Ritorno
047800150511     C     $TASTO        WHENEQ    F12
047900150511     C                   EXSR      F12D1
048000150511     C* F24=Altri tasti
048100150511     C     $TASTO        WHENEQ    F24
048200150511     C                   EXSR      F24D1
048300150511     C* Personalizzata
048400150511     C     $PGF1(PosFun) WHENNE    *BLANK
048500150511     C                   EXSR      FPerD1
048600150511     C*
048700150511   X2C                   OTHER
048800150511     C                   EXSR      CTRD1
048900150511     C* aggiungere le chiamate ai controlli delle videate supplementari
049000150511    >C     *IN99         IFEQ      *OFF
049100150511    >C                   EXSR      CTRD2
049200150511    >C     *IN99         IFEQ      *ON
049300150511    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
049400150511    >C* indicatori uguali in videate differenti, genererei conflitti
049500150511    >C                   MOVE      *ALL'0'       IN5098           49
049600150511    >C                   MOVEA     IN5098        *IN(50)
049700150511    >C                   SETON                                        9996                       96
049800150511    >C                   ENDIF
049900150511    >C                   ENDIF
050000150511     C*
050100150511   B3C     $TASTO        IFEQ      F06
050200150511     C     *IN99         ANDEQ     *OFF
050300150511     C* eseguo aggiornamento
050400150511     C                   EXSR      AGGANA
050500150511     C* eseguo operazioni del dopo-aggiornamento
050600150511   B4C     *IN99         IFEQ      *OFF
050700150511     C                   EXSR      GESAGG
050800150511   E4C                   ENDIF
050900150511   E3C                   ENDIF
051000150511     C*
051100150511   E2C                   ENDSL
051200150511     C*
051300150511   E1C                   ENDIF
051400150511     C*
051500150511     C                   ENDSR
051600150511     C/EJECT
051700150511     C************************************************************
051800150511     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
051900150511     C************************************************************
052000150511     C     CTRKD1        BEGSR
052100150511     C*
052200150511     C                   SETOFF                                       99
052300150511     C                   MOVEA     $FC1          $FC
052400150511     C                   EXSR      CTRKEY
052500150511     C     *IN99         IFEQ      *ON
052600150511     C                   SETON                                        97
052700150511     C                   ENDIF
052800150511     C*
052900150511     C                   ENDSR
053000150511     C/EJECT
053100150511     C************************************************************
053200150511     C* INIZIALIZZAZIONE VIDEATA DATI
053300150511     C************************************************************
053400150511     C     INZD1         BEGSR
053500150511     C*
053600150511     C                   CLEAR                   FMTD1
053700150511    >C                   CLEAR                   FMTD2
053800150511     C*
053900150511     C* per tutte le opzioni NON immissione
054000160728   B>C     OPZK12        IFNE      '01'
054100160728    >C                   MOVEL     XscSoc        SPASocieta
054200160728    >C                   MOVEL     KccK12        SPAKcc
054201160728    >C                   MOVEL     KscK12        SPAKsc
054400160728    >C                   MOVEL     DtIniK12      SPADtIni
054500150511     C*
054600150511   B2C     *IN22         DOUEQ     *OFF
054700150511     C     MSGRTN        ORNE      'R'
054800150511     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
054900160728   B>C     OPZK12        IFEQ      '02'
055000160728    >C     OPZK12        OREQ      '04'
055100160728    >C     K04SPA01      CHAIN     SDGSPA01L                          2122
055200150511     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
055300150511   X3C                   ELSE
055400160728    >C     K04SPA01      CHAIN(N)  SDGSPA01L                          2122
055500150511   E3C                   ENDIF
055600150511     C*
055700150511     C* Gestione RCD allocato
055800150511   B3C     *IN22         IFEQ      *ON
055900160728    >C                   MOVEL     'SDGSPA00F'   MSGFIL
056000150511     C                   EXSR      MSGPGM
056100150511   E3C                   ENDIF
056200150511   E2C                   ENDDO
056300150511     C*
056400150511     C* se il rcd da gestire non viene trovato
056500150511     C* oppure è allocato da altro pgm
056600150511     C* ritorno al pgm guida avvertendolo dell'errore
056700150511   B2C     *IN21         IFEQ      *ON
056800150511     C     *IN22         OREQ      *ON
056900150511     C                   MOVE      *ON           $FINE
057000160728    >C                   MOVE      '1'           ERRK12
057100150511     C*
057200150511     C* altrimenti
057300150511   X2C                   ELSE
057400150511     C* riempio la videata
057500150511     C                   EXSR      RIED1
057600150511    >C                   EXSR      RIED2
057700150511     C* richiamo routine dei ctrl per decodificarla
057800150511     C                   EXSR      CTRD1
057900150511    >C                   EXSR      CTRD2
058000150511     C                   MOVE      *ALL'0'       IN5098
058100150511     C                   MOVEA     IN5098        *IN(50)
058200150511   E2C                   ENDIF
058300150511     C*
058400150511     C* per immissione
058500150511   X1C                   ELSE
058600150511     C*
058700150511     C* riempio con i default di immissione  (se reperiti)
058800150511     C                   IF        $FlgDef = *on
058900150511     C                   EXSR      RIEDEF
059000150511     C                   ENDIF
059100150511     C*
059200150511     C* imposto i campi chiave ricevuti anche se si è in immissione
059300150511     C* perchè potrebbero essere stati passati dei default di chiave
059400160728     C                   MOVEL     KccK12        D1Kcc
059401160728     C                   MOVEL     KscK12        D1Ksc
059600150511   E1C                   ENDIF
059700150511     C*
059800150511     C* imposto quegli attributi della videata che sono modificabili
059900150511     C* al richiamo del determinato pgm
060000150511     C                   EXSR      ATRD1
060100150511     C*
060200150511     C                   ENDSR
060300150511     C/EJECT
060400150511     C************************************************************
060500150511     C* RIEMPIMENTO VIDEATA  D1
060600150511     C************************************************************
060700150511     C     RIED1         BEGSR
060800150511     C*
060900150511     C* impostare i campi della videata dai campi del file
061000150511     C*
061100160728     C                   MoveL     SPAKcc        D1Kcc
061101160728     C                   MoveL     SPAKsc        D1Ksc
061500150511     C*
061600160728     C                   if        SPADtIni   <> *Loval
061700160728     C     *JobRun       MoveL     SPADtIni      D1DtIni
061800150514     C                   else
061900150511     C                   Z-Add     0             D1DtIni
062000150514     C                   endif
062001150512     C*
062002160729     C                   if        SPADtFin   <> *Hival
062200160728     C     *JobRun       MoveL     SPADtFin      D1DtFin
062300150514     C                   else
062400150511     C                   Z-Add     0             D1DtFin
062500150514     C                   endif
062501150514     C*
062503160728     C                   MoveL     SPATributo    D1Tributo
062504160729     C                   MoveL     SPATriProg    D1TriProg
062600150511     C*
062700150511     C                   ENDSR
062701150514     C/EJECT
062800150511     C************************************************************
062900150511     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
063000150511     C************************************************************
063100150511     C     ATRD1         BEGSR
063200150511     C*
063300150511     C                   SETOFF                                       02
063400150511     C*
063500150511     C* in caso di manutenzione
063600160728    >C     OPZK12        IFEQ      '02'
063700150511     C* proteggo i campi chiave
063800150511     C                   SETON                                        02        PR campi key
063900150511   E3C                   ENDIF
064000150511     C*
064100150511     C                   ENDSR
064200150511     C/EJECT
064300150511     C************************************************************
064400150511     C* RIEMPIMENTO CAMPI DAI DEFAULT
064500150511     C************************************************************
064600150511     C     RIEDEF        BEGSR
064700150511     C*
064800150511     C*
064900150511     C                   ENDSR
065000150511     C/EJECT
065100150511     C************************************************************
065200150511     C* GESTIONE F02 VIDEO D1
065300150511     C************************************************************
065400150511     C     F02D1         BEGSR
065500150511     C*
065600150511     C* per decodificare la videata richiamo la routine dei ctrl
065700150511     C* e poi disattivo gli eventuali messaggi di errore
065800150511     C                   EXSR      CTRD1
065900150511     C*
066000150511     C                   MOVE      *ALL'0'       IN5098
066100150511     C                   MOVEA     IN5098        *IN(50)
066200150511     C*
066300150511     C                   ENDSR
066400150511     C/EJECT
066500150511     C************************************************************
066600150511     C* GESTIONE F03 VIDEO D1
066700150511     C************************************************************
066800150511     C     F03D1         BEGSR
066900150511     C*
067000150511     C                   MOVE      *ON           $FINE
067100160728    >C                   MOVE      '1'           RETK12
067200150511     C* fine programma
067300150512     C                   EXSR      ENDPGM
067400150511     C*
067500150511     C                   ENDSR
067600150511     C/EJECT
067700150511     C************************************************************
067800150511     C* GESTIONE F05 VIDEO D1
067900150511     C************************************************************
068000150511     C     F05D1         BEGSR
068100150511     C*
068200150511     C                   MOVE      *ON           $INZD1
068300150511     C*
068400150511     C                   ENDSR
068500150511     C/EJECT
068600150511     C************************************************************
068700150511     C* GESTIONE F08 VIDEO D1
068800150511     C************************************************************
068900150511     C     F08D1         BEGSR
069000150511     C*
069100150511     C                   MOVE      *ON           $FINE
069200160728    >C                   MOVE      '0'           RETK12
069300150511     C*
069400150511     C                   ENDSR
069500150511     C/EJECT
069600150511     C************************************************************
069700150511     C* GESTIONE F12 VIDEO D1
069800150511     C************************************************************
069900150511     C     F12D1         BEGSR
070000150511     C*
070100150511     C                   MOVE      *ON           $FINE
070200160728    >C                   MOVE      '2'           RETK12
070300150511     C*
070400150511     C                   ENDSR
070500150511     C/EJECT
070600150511     C************************************************************
070700150511     C* GESTIONE F24 VIDEO D1
070800150511     C************************************************************
070900150511     C     F24D1         BEGSR
071000150511     C*
071100150511     C                   MOVEA     $FX1          $FX
071200150511     C                   MOVEA     $FC1          $FC
071300150511     C                   MOVEA     $FR1          $FR
071400150511     C                   Z-ADD     $ULKD1        $ULK
071500150511     C                   Z-ADD     $D1L01        $L01
071600150511     C                   Z-ADD     $D1L02        $L02
071700150511     C                   EXSR      RIEKEY
071800150511     C                   Z-ADD     $ULK          $ULKD1            3 0
071900150511     C                   MOVEA     $FC           $FC1
072000150511     C                   MOVEL     $KEY1         Z1KE1
072100150512     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
072200150511     C                   MOVEL     $KEY2         Z1KE2
072300150511     C                   WRITE     FMTZ1
072400150511     C*
072500150511     C                   ENDSR
072600150511     C/EJECT
072700150511     C************************************************************
072800150511     C* CONTROLLO VIDEATA
072900150511     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
073000150511     C* 95 96 97 98
073100150511     C************************************************************
073200150511     C     CTRD1         BEGSR
073300150511     C*
073400150511     C                   SETOFF                                       99
073500150511     C*
073600150511     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
073700150511     C*
073800160728    >C     OPZK12        IFNE      '04'
073801150512     C*
073803160728     C                   EXSR      CtrPercip
073804160728     C*
073805160728     C                   EXSR      CtrDate
073807150512     C*
073809160728     C                   EXSR      CtrTributo
077700150511     C*
077701150514     C* se periodo non è sovrapposto, controllo se già inserito
077702150514     C
083700150511     C*
083800150511     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
083900160728    >C     OPZK12        IFEQ      '01'
084000160728    >C     OPZK12        OREQ      '03'
084100150511     C*
084200150511     C* imposto i campi chiave e verifico l'esistenza del record
084300150511     C*
084400160728    >C                   MOVEL     XScSoc        SPASocieta
084500160728    >C                   MOVEL     D1Kcc         SPAKcc
084501160728    >C                   MOVEL     D1Ksc         SPAKsc
084700160728    >C                   MOVEL     $DtIni        SPADtIni
084800160728    >C     K04SPA01      SETLL     SDGSPA01L                              21
084900150511     C     *IN21         IFEQ      '1'
085000150511     C                   SETON                                        9099
085100150511     C                   ENDIF
085200150511     C                   ENDIF
085300150511     C*
085400150511     C                   ENDIF
085500150511     C*
085600150511     C                   ENDSR
085700150511     C/EJECT
085701160728     C************************************************************
085702160728     C* Controllo esistenza conto percipiente
085703160728     C************************************************************
085704160728     C     CtrPercip     BEGSR
085705160728     C*
085706160728     C                   Clear                   WKcc
085707160728     C                   Clear                   WKsc
085708160728     C*
085709160728     C                   Clear                   D1KSCD
085710160728     C*
085711160728     C     D1KCC         IFNE      'C'
085712160728     C     D1KCC         ANDNE     'F'
085713160728     C                   CALLB     'XALLINEA'
085714160728     C                   PARM                    D1KCC
085715160728     C                   PARM      6             LEN               5 0
085716160728     C                   PARM                    XSCINP
085717160728     C                   ENDIF
085718160728     C*
085719160728     C                   if        D1Ksc      <> *Blank
085720160728     C*
085721160728     C     D1KSC         IFNE      *Blanks
085722160728     C                   CALLB     'XALLINEA'
085723160728     C                   PARM                    D1KSC
085724160728     C                   PARM      8             LEN               5 0
085725160728     C                   PARM                    XSCINP
085726160728     C                   ENDIF
085727160728     C*
085728160728     C     D1KCC         IFEQ      'C       '
085729160728     C     D1KCC         OREQ      'F       '
085730160728     C                   CALLB     'XCAPCLIFOR'
085731160728     C                   PARM                    XScSoc
085732160728     C                   PARM      D1KCC         D1Kcc
085733160728     C                   PARM                    D1Ksc
085734160728   E1C                   ENDIF
085735160728     C*
085736160728     C                   Eval      WKsc        = D1Ksc
085737160728     C                   Eval      WKcc        = D1Kcc
085738160728     C                   EXSR      CALLMVC002
085739160728     C                   if        Esito       = *On
085740160728     C                   else
085741160728     C                   MoveL     DesBrev002    D1KSCD
085742160728     C*
085743160728     C                   if        Percipi002 <> *On
085744160728     C                   SetOn                                        5099
085745160728     C                   endif
085746160728     C*
085747160728     C                   endif
085748160728     C*
085749160728     C                   endif
085750160728     C*
085751160728     C                   ENDSR
085752160728     C/EJECT
085753160728     C***********************************************************
085754160728     C* Driver per controllo conto
085755160728     C***********************************************************
085756160728     C     CALLMvc002    BEGSR
085757160728     C*
085758160728     C                   Clear                   ND002DS
085759160728     C*
085760160728     C                   Eval      LenOut = %Size(ND002DS)
085761160728     C*
085762160728     C                   CALLB     'NDMVC002'
085763160728     C                   PARM                    XScSoc
085764160728     C                   PARM                    WKCC
085765160728     C                   PARM                    WKSC
085766160728     C                   PARM                    Data
085767160728     C                   PARM      *Off          GesMsg
085768160728     C                   PARM      *Off          Esito
085769160728     C                   PARM      'ND002DS'     StrutturaO
085770160728     C                   PARM                    ND002DS
085771160728     C                   PARM                    LenOut
085772160728     C*
085773160728     C                   ENDSR
085774160728     C/EJECT
088415150512     C************************************************************
088416160728     C* Controllo tributo
088417150512     C************************************************************
088418160728    >C     CtrTributo    BEGSR
088419150512     C*
088421160729     C                   Clear                   D1TriProgD
088422160729     C                   if        D1Tributo  <> *Blank     or
088423160729     C                             D1TriProg  <> *Blank
088424160728     C                   Eval      TRISocieta  = XScSoc
088425160728     C                   Eval      TRITributo  = D1Tributo
088426160729     C                   Eval      TRITriProg  = D1TriProg
088427160728     C     K03TRI01      CHAIN     ANTRI01L                           21
088428160728     C     *In21         IFEQ      '0'
088429160729     C                   MoveL     TRIDescr      D1TriProgD
088430160728     C                   else
088431160728     C                   seton                                        6199
088432160728     C                   ENDIF
088433160729     C                   endif
088434160728     C*
088435160728     C                   ENDSR
088436160728     C/EJECT
088437160728     C************************************************************
088438160728     C* Controllo date
088439160728     C************************************************************
088440160728    >C     CtrDate       BEGSR
088441160728     C*
088442150512     C* controllo validità data iniziale
088443150512     C                   Move      *Loval        $DtIni
088444150512     C                   if        D1DtIni    <> 0
088445150512     C     *JobRun       Test(D)                 D1DtIni                21
088446150512     C                   if        *In21       = *Off
088447150512     C     *JobRun       Move      D1DtIni       $DtIni
088448150514     C                   else
088449150512     C                   SetOn                                        5399
088450150512     C                   endif
088451150512     C*
088452150512     C                   else
088453150512     C* data iniziale obbligatoria
088454150512     C                   SetOn                                        5299
088455150512     C                   endif
088456150512     C*
088457150514     C*
088458150512     C* controllo validità data finale
088459160729     C                   Move      *Hival        $DtFin
088460150512     C                   if        D1DtFin    <> 0
088461150512     C     *JobRun       Test(D)                 D1DtFin                21
088462150512     C                   if        *In21       = *Off
088463150512     C     *JobRun       Move      D1DtFin       $DtFin
088464150512     C                   else
088465150512     C                   SetOn                                        5499
088466150512     C                   endif
088467150512     C                   else
088468150512     C* data finale obbligatoria
088469160729     C***********        SetOn                                        5799
088470150512     C                   endif
088471150512     C*
088472150512     C* controllo coerenza date
088473150512     C                   if        $DtIni      > $DtFin     and
088474150512     C                             $DtFin     <> *Loval
088475150512     C                   SetOn                                        5599
088476150512     C                   endif
088477150512     C*
088478150512     C* se inserita almeno la data iniziale, controllo che il
088479160728     C* periodo non sia sovrapposto ad un periodo già esistente
088480150512     C                   if        $DtIni     <> *Loval
088481160728     C                   Clear                   SDGSK13DS
088482160728     C                   Movel     OpzK12        OpzK13
088483160728     C                   Movel     XScSoc        SocietaK13
088484160728     C                   Movel     D1Kcc         KccK13
088485160728     C                   Movel     D1Ksc         KscK13
088486160728     C                   Move      $DtIni        DtIniK13
088487160728     C                   Move      $DtFin        DtFinK13
088488160728     C                   Move      *Off          SovraK13
088489160728     C                   Movel     SDGSK13DS     Kpjbu
088490160728     C                   Call      'SDGSK13R'
088491150512     C                   Parm                    Kpjba
088492160728     C                   Movel     Kpjbu         SDGSK13DS
088493160728     C                   if        SovraK13    = *On
088494150512     C                   SetOn                                        5699
088495150512     C                   endif
088496150512     C                   endif
088497150512     C*
088498150512     C                   ENDSR
088499150512     C/EJECT
088500150512     C************************************************************
088501150512     C* GESTIONE VIDEO RECORD  D2
088502150512     C************************************************************
088503150512    >C     GESD2         BEGSR
088504150511     C* emissione piede videata se proveniente da altra
088600150511     C* salvataggio valore formato di provenienza
088700150511   B1C     $LASTV        IFNE      'D2'
088800150511     C                   WRITE     FMTT1
088900150511     C                   EXSR      GESPIE
089000150511     C                   MOVE      $LASTV        $LASTG
089100150511     C                   MOVE      'D2'          $LASTV
089200150511   E1C                   ENDIF
089300150511     C*
089400150511     C* se esistono errori sulla videata
089500150511     C* emetto la write del formato a indicatori spenti per vedere
089600150511     C* le eventuali decodifiche
089700150511   B1C     *IN99         IFEQ      *ON
089800150511     C     $GEST         ANDEQ     'D2'
089900150511     C                   MOVEA     *IN           WIN
090000150511     C                   MOVE      *ALL'0'       IN5098           49
090100150511     C                   MOVEA     IN5098        *IN(50)
090200150511     C                   WRITE     FMTD2
090300150511     C                   MOVEA     WIN           *IN
090400150511   E1C                   ENDIF
090500150511     C* Inizializzazione indice tasti funzione personalizzati
090600150511     C                   Eval      PosFun = 25
090700150511     C*
090800150511   B1C                   SELECT
090900150511     C* Annullamento
091000160728    >C     OPZK12        WHENEQ    '04'
091100150511     C* Visualizzazione
091200160728    >C     OPZK12        OREQ      '05'
091300150511     C                   WRITE     FMTD2
091400150511     C                   EXFMT     PROTECT
091500150511     C* Immissione/Modifica/Copia
091600150511   X1C                   OTHER
091700150511     C                   EXFMT     FMTD2
091800150511   E1C                   ENDSL
091900150511     C*
092000150511     C* azzero la posizione del cursore
092100150511     C                   Z-ADD     0             H1RIGA
092200150511     C                   Z-ADD     0             H1COLO
092300150511     C*
092400150511     C* controllo abilitazione tasti funzione
092500150511     C                   EXSR      CTRKD1
092600150511     C*
092700150511   B1C     *IN99         IFEQ      *OFF
092800150511     C*
092900150511   B2C                   SELECT
093000150511     C* ROLDOWN
093100150511     C     $TASTO        WHENEQ    ROLDWN
093200150511     C                   MOVE      'D1'          $GEST
093300150511     C* F3=Fine
093400150511     C     $TASTO        WHENEQ    F03
093500150511     C                   EXSR      F03D1
093600150511     C* F4=Ricerca
093700150511     C     $TASTO        WHENEQ    F04
093800150511     C                   EXSR      SEARCH
093900150511     C* F5=Refresh
094000150511     C     $TASTO        WHENEQ    F05
094100150511     C                   EXSR      F05D2
094200150511     C* F8=Successivo
094300150511     C     $TASTO        WHENEQ    F08
094400150511     C                   EXSR      F08D1
094500150511     C* F12=Ritorno
094600150511     C     $TASTO        WHENEQ    F12
094700150511     C                   EXSR      F12D1
094800150511     C* F24=Altri tasti
094900150511     C     $TASTO        WHENEQ    F24
095000150511     C                   EXSR      F24D2
095100150511     C* Personalizzata
095200150511     C     $PGF1(PosFun) Whenne    *BLANK
095300150511     C                   EXSR      FperD1
095400150511     C*
095500150511   X2C                   OTHER
095600150511     C                   EXSR      CTRD2
095700150511   B3C     *IN99         IFEQ      *OFF
095800150511     C                   EXSR      CTRD1
095900150511     C     *IN99         IFEQ      *ON
096000150511     C* Spengo gli indicatori di errore per il controllo su altri schermi
096100150511     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
096200150511     C* videate
096300150511     C                   MOVE      *ALL'0'       IN5098           49
096400150511     C                   MOVEA     IN5098        *IN(50)
096500150511     C                   SETON                                        9996                       96
096600150511     C                   ENDIF
096700150511     C                   ENDIF
096800150511     C*
096900150511     C*
097000150511   B3C     $TASTO        IFEQ      F06
097100150511     C     *IN99         ANDEQ     *OFF
097200150511     C* eseguo aggiornamento
097300150511     C                   EXSR      AGGANA
097400150511     C* eseguo operazioni del dopo-aggiornamento
097500150511   B4C     *IN99         IFEQ      *OFF
097600150511     C                   EXSR      GESAGG
097700150511   E4C                   ENDIF
097800150511   E3C                   ENDIF
097900150511     C*
098000150511   E2C                   ENDSL
098100150511     C*
098200150511   E1C                   ENDIF
098300150511     C*
098400150511    >C                   ENDSR
098500150511     C/EJECT
098600150511     C************************************************************
098700150511     C* RIEMPIMENTO VIDEATA D2
098800150511     C************************************************************
098900150511    >C     RIED2         BEGSR
099000150511     C*
099100150511     C*
099200150511    >C                   ENDSR
099300150511     C/EJECT
099400150511     C************************************************************
099500150511     C* GESTIONE F05 VIDEO D2
099600150511     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
099700150511     C* e il riposizionamento alla prima
099800150511     C************************************************************
099900150511    >C     F05D2         BEGSR
100000150511     C*
100100150511     C                   MOVE      *ON           $INZD1
100200150511     C                   MOVE      'D1'          $GEST
100300150511     C*
100400150511    >C                   ENDSR
100401150514     C/EJECT
100500150511     C************************************************************
100600150511     C* GESTIONE F24 VIDEO D2
100700150511     C************************************************************
100800150511    >C     F24D2         BEGSR
100900150511     C*
101000150511     C                   MOVEA     $FX1          $FX
101100150511     C                   MOVEA     $FC1          $FC
101200150511     C                   MOVEA     $FR1          $FR
101300150511     C                   Z-ADD     $ULKD1        $ULK
101400150511     C                   Z-ADD     $D1L01        $L01
101500150511     C                   Z-ADD     $D1L02        $L02
101600150511     C                   EXSR      RIEKEY
101700150511     C                   Z-ADD     $ULK          $ULKD1            3 0
101800150511     C                   MOVEA     $FC           $FC1
101900150511     C                   MOVEL     $KEY1         Z1KE1
102000150512     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
102100150511     C                   MOVEL     $KEY2         Z1KE2
102200150511     C                   WRITE     FMTZ1
102300150511     C*
102400150511    >C                   ENDSR
102401150514     C/EJECT
102500150511     C************************************************************
102600150511     C* CONTROLLO VIDEATA  D2
102700150511     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
102800150511     C* 95 96 97 98
102900150511     C************************************************************
103000150511    >C     CTRD2         BEGSR
103100150511     C*
103200150511     C                   SETOFF                                       99
103300150511     C*
103400150511     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
103500150511     C*
103600160728    >C     OPZK12        IFNE      '04'
103700150511     C*
103800150511    >C* INSERIRE I CONTROLLI SPECIFICI DELLA VIDEATA
103900150511     C* utilizzare indicatori diversi nelle diverse videate
104000150511     C*
104100150511     C*
104200150511     C                   ENDIF
104300150511     C*
104400150511     C                   ENDSR
104500150511     C/EJECT
104600150511     C************************************************************
104700150511     C* AGGIORNAMENTO ANAGRAFICA
104800150511     C************************************************************
104900150511     C     AGGANA        BEGSR
105000150511     C*
105100150511     C                   SELECT
105200160728    >C     OPZK12        WHENEQ    '01'
105300160728    >C     OPZK12        OREQ      '03'
105400150511     C                   EXSR      RIEANA
105500160728    >C                   WRITE     SDGSPA000                            22
105600150511     C* se rcd non scrivibile attivo errore
105700150511     C     *IN22         IFEQ      *ON
105800150511     C                   SETON                                        9599
105900150511     C                   ENDIF
106000160728    >C     OPZK12        WHENEQ    '02'
106100150511     C                   EXSR      RIEANA
106200160728    >C                   UPDATE    SDGSPA000
106300160728    >C     OPZK12        WHENEQ    '04'
106400160728    >C                   DELETE    SDGSPA000
106500150511     C                   ENDSL
106600150511     C*
106700150511     C* richiama pgm User Exit
106800150511     c*
106900150511     C     USRERR        IFEQ      *BLANK
107000150511     C* passa opzione ricevuta a user exit
107100160728    >C                   MOVEL     OPZK12        XUKOPZ
107200150511     C* imposta chiave univoca  per user exit
107300150511     C                   MOVE      XSCSOC        XUKSOC
107400150511     C                   MOVE      XSCDUN        XUKDUN
107500150511     C                   MOVEL     DSPGM         XUKPGM
107600150511    >C                   MOVEL     XScSoc        XUKFL1
107700160728    >C                   MOVEL     D1Kcc         XUKFL2
107701160728    >C                   MOVEL     D1Ksc         XUKFL3
107900150511    >C                   MOVEL     $DtIni        XUKFL4
108000150511     C                   MOVEL     XUKDS         KPJBU
108100150511     C     DSPGM         CAT       'U':0         USREXT           10
108200150511     C                   CALL      USREXT                               21
108300150511     C                   PARM                    KPJBA
108400150511     C     *IN21         IFEQ      *ON
108500150511     C                   MOVE      *ON           USRERR            1
108600150511     C                   ENDIF
108700150511     C                   ENDIF
108800150511     C*
108900150511     C                   ENDSR
109000150511     C/EJECT
109100150511     C************************************************************
109200150511     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
109300150511     C************************************************************
109400150511     C     RIEANA        BEGSR
109500150511     C*
109600160728    >C                   CLEAR                   SDGSPA000
109700150511     C*
109800150511     C* imposto i campi del file da quelli video
109900150511     C*
110000160728    >C                   MoveL     XscSoc        SPASocieta
110100160728    >C                   MoveL     D1Kcc         SPAKcc
110101160728    >C                   MoveL     D1Ksc         SPAKsc
110500160728    >C                   MoveL     $DtIni        SPADtIni
110600160728    >C                   MoveL     $DtFin        SPADtFin
110601160728    >C                   MoveL     D1Tributo     SPATributo
110602160729    >C                   MoveL     D1TriProg     SPATriProg
110700150511     C*
110800150511     C                   ENDSR
110900150511     C/EJECT
111000150511     C************************************************************
111100150511     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
111200150511     C************************************************************
111300150511     C     GESAGG        BEGSR
111900150511     C*
112000150511     C* segnalo al pgm chiamante l'avvenuta conferma
112100160728    >C                   MOVE      *ON           OPRK12
112200150511     C* segnalo al pgm chiamante l'aver premuto l'F6
112300160728    >C                   MOVE      '0'           RETK12
112400150511     C*
112500150511     C                   SELECT
112600150511     C*
112700150511     C* nel caso di immissione
112800160728    >C     OPZK12        WHENEQ    '01'
112900150511     C* ripristino la videata iniziale
113000150511     C                   MOVE      *ON           $INZD1
113100150511     C                   MOVE      'D1'          $GEST
113200150511     C*
113300150511     C* nel caso di modifica
113400160728    >C     OPZK12        WHENEQ    '02'
113500150511     C* ripasso i campi che devono essere reimpostati sul sfl
113600160728    >C                   MOVEL     SPAKcc        KccK12
113601160728    >C                   MOVEL     SPAKsc        KscK12
114000160728    >C                   MOVEL     SPADtIni      DtIniK12
114100160728    >C                   MOVEL     SPADtFin      DtFinK12
114101160728    >C                   MOVEL     SPATributo    TributoK12
114102160729    >C                   MOVEL     SPATriProg    TriProgK12
114200150511     C* e ritorno al pgm chiamante
114300150511     C                   MOVE      *ON           $FINE
114400150511     C*
114500150511     C* per tutte le altre opzioni
114600150511     C                   OTHER
114700150511     C* ritorno al pgm chiamante
114800150511     C                   MOVE      *ON           $FINE
114900150511     C                   ENDSL
115000150511     C*
115100150511     C                   ENDSR
115200150511     C/EJECT
115300150511     C************************************************************
115400150511     C* CONTROLLO ABILITAZIONE TASTI
115500150511     C************************************************************
115600150511     C     CTRKEY        BEGSR
115700150511     C*
115800150511     C                   SELECT
115900150511     C     $TASTO        WHENEQ    F01
116000150511     C                   Z-ADD     01            PosFun
116100150511     C     $FC(1)        IFEQ      '0'
116200150511     C                   SETON                                        99
116300150511     C                   ENDIF
116400150511     C     $TASTO        WHENEQ    F02
116500150511     C                   Z-ADD     02            PosFun
116600150511     C     $FC(2)        IFEQ      '0'
116700150511     C                   SETON                                        99
116800150511     C                   ENDIF
116900150511     C     $TASTO        WHENEQ    F03
117000150511     C                   Z-ADD     03            PosFun
117100150511     C     $FC(3)        IFEQ      '0'
117200150511     C                   SETON                                        99
117300150511     C                   ENDIF
117400150511     C     $TASTO        WHENEQ    F04
117500150511     C                   Z-ADD     04            PosFun
117600150511     C     $FC(4)        IFEQ      '0'
117700150511     C                   SETON                                        99
117800150511     C                   ENDIF
117900150511     C     $TASTO        WHENEQ    F05
118000150511     C                   Z-ADD     05            PosFun
118100150511     C     $FC(5)        IFEQ      '0'
118200150511     C                   SETON                                        99
118300150511     C                   ENDIF
118400150511     C     $TASTO        WHENEQ    F06
118500150511     C                   Z-ADD     06            PosFun
118600150511     C     $FC(6)        IFEQ      '0'
118700150511     C                   SETON                                        99
118800150511     C                   ENDIF
118900150511     C     $TASTO        WHENEQ    F07
119000150511     C                   Z-ADD     07            PosFun
119100150511     C     $FC(7)        IFEQ      '0'
119200150511     C                   SETON                                        99
119300150511     C                   ENDIF
119400150511     C     $TASTO        WHENEQ    F08
119500150511     C                   Z-ADD     08            PosFun
119600150511     C     $FC(8)        IFEQ      '0'
119700150511     C                   SETON                                        99
119800150511     C                   ENDIF
119900150511     C     $TASTO        WHENEQ    F09
120000150511     C                   Z-ADD     09            PosFun
120100150511     C     $FC(09)       IFEQ      '0'
120200150511     C                   SETON                                        99
120300150511     C                   ENDIF
120400150511     C     $TASTO        WHENEQ    F10
120500150511     C                   Z-ADD     10            PosFun
120600150511     C     $FC(10)       IFEQ      '0'
120700150511     C                   SETON                                        99
120800150511     C                   ENDIF
120900150511     C     $TASTO        WHENEQ    F11
121000150511     C                   Z-ADD     11            PosFun
121100150511     C     $FC(11)       IFEQ      '0'
121200150511     C                   SETON                                        99
121300150511     C                   ENDIF
121400150511     C     $TASTO        WHENEQ    F12
121500150511     C                   Z-ADD     12            PosFun
121600150511     C     $FC(12)       IFEQ      '0'
121700150511     C                   SETON                                        99
121800150511     C                   ENDIF
121900150511     C     $TASTO        WHENEQ    F13
122000150511     C                   Z-ADD     13            PosFun
122100150511     C     $FC(13)       IFEQ      '0'
122200150511     C                   SETON                                        99
122300150511     C                   ENDIF
122400150511     C     $TASTO        WHENEQ    F14
122500150511     C                   Z-ADD     14            PosFun
122600150511     C     $FC(14)       IFEQ      '0'
122700150511     C                   SETON                                        99
122800150511     C                   ENDIF
122900150511     C     $TASTO        WHENEQ    F15
123000150511     C                   Z-ADD     15            PosFun
123100150511     C     $FC(15)       IFEQ      '0'
123200150511     C                   SETON                                        99
123300150511     C                   ENDIF
123400150511     C     $TASTO        WHENEQ    F16
123500150511     C                   Z-ADD     16            PosFun
123600150511     C     $FC(16)       IFEQ      '0'
123700150511     C                   SETON                                        99
123800150511     C                   ENDIF
123900150511     C     $TASTO        WHENEQ    F17
124000150511     C                   Z-ADD     17            PosFun
124100150511     C     $FC(17)       IFEQ      '0'
124200150511     C                   SETON                                        99
124300150511     C                   ENDIF
124400150511     C     $TASTO        WHENEQ    F18
124500150511     C                   Z-ADD     18            PosFun
124600150511     C     $FC(18)       IFEQ      '0'
124700150511     C                   SETON                                        99
124800150511     C                   ENDIF
124900150511     C     $TASTO        WHENEQ    F19
125000150511     C                   Z-ADD     19            PosFun
125100150511     C     $FC(19)       IFEQ      '0'
125200150511     C                   SETON                                        99
125300150511     C                   ENDIF
125400150511     C     $TASTO        WHENEQ    F20
125500150511     C                   Z-ADD     20            PosFun
125600150511     C     $FC(20)       IFEQ      '0'
125700150511     C                   SETON                                        99
125800150511     C                   ENDIF
125900150511     C     $TASTO        WHENEQ    F21
126000150511     C                   Z-ADD     21            PosFun
126100150511     C     $FC(21)       IFEQ      '0'
126200150511     C                   SETON                                        99
126300150511     C                   ENDIF
126400150511     C     $TASTO        WHENEQ    F22
126500150511     C                   Z-ADD     22            PosFun
126600150511     C     $FC(22)       IFEQ      '0'
126700150511     C                   SETON                                        99
126800150511     C                   ENDIF
126900150511     C     $TASTO        WHENEQ    F23
127000150511     C                   Z-ADD     23            PosFun
127100150511     C     $FC(23)       IFEQ      '0'
127200150511     C                   SETON                                        99
127300150511     C                   ENDIF
127400150511     C     $TASTO        WHENEQ    F24
127500150511     C                   Z-ADD     24            PosFun
127600150511     C     $FC(24)       IFEQ      '0'
127700150511     C                   SETON                                        99
127800150511     C                   ENDIF
127900150511     C                   ENDSL
128000150511     C*
128100150511     C                   ENDSR
128200150511     C/EJECT
128300150511     C******************************************************
128400150511     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
128500150511     C******************************************************
128600150511     C     MSGPGM        BEGSR
128700150511     C*
128800150511     C                   Z-ADD     2             MSGMSG
128900150511     C                   MOVEL     'S'           MSGRSP
129000150511     C                   MOVEL     'CR'          MSGTPR
129100150511     C                   MOVEL     'S'           MSGEMV
129200150511     C                   Z-ADD     10            MSGRGP
129300150511     C                   MOVEL     'S'           MSGLCK
129400150511     C                   MOVEA     STATUS        STS
129500150511     C                   MOVEL     'N'           MSGCNL
129600150511     C                   MOVEL     'N'           MSGVID
129700150511     C                   MOVEL     'N'           MSGOPE
129800150511     C                   MOVEL     'N'           MSGSTP
129801150512     C                   CALL      'SDGMSGR'
129802150512     C                   PARM                    SDGMSGDS
130100150511     C*
130200150511     C                   ENDSR
130300150511     C/EJECT
130400150511     C************************************************************
130500150511     C* OPERAZIONI INIZIALI
130600150511     C************************************************************
130700150511     C     *INZSR        BEGSR
130800150511     C*
130900150511     C* Reperimento parametri
131100150511     C     *ENTRY        PLIST
131200150511     C                   PARM                    KPJBA
131300150511     C*
131400150511     C* Reperimento dati società
131500150511     C*
131600150511    >C                   MOVEL     'SOC001'      TIPXSC
131700150511     C                   MOVEL     *BLANK        SOCXSC
131800150511     C                   EXSR      REPSOC
131900150511     C*
132000150511     C* Controlli dati società
132100150511     C*
132200150511     C     RTNXSC        IFNE      '1'
132300150511     C                   MOVEL     XSOCDS        SOC001
132400150511     C                   ELSE
132500160728    >C                   MOVE      '1'           ErrK12
132600150511     C                   MOVE      *on           $Fine
132700150512     C                   EXSR      ENDPGM
132800150511     C                   ENDIF
132900150511     C*
133000150511     C* testo se sono stati passati i dati di autorizzazione
133100150511     C*
133200150511     C     PARMS         IFLT      2
133300150511     C                   MOVE      *BLANK        AUTDS
133400150511     C                   ELSE
133500150511     C                   MOVEL     DSAUT         AUTDS
133600150511     C                   ENDIF
133700150511     C*
133800150511     C* reperisco le dimensioni delle variabili che conterranno i tasti
133900150511     C                   EVAL      $D1L01 = %Size(Z1Ke1)
134000150511     C                   EVAL      $D1L02 = %Size(Z1Ke2)
134100150511     C*
134200150511     C* Reperimento tasti di funzione
134300150511     C*
134400150511     C                   EXSR      REPKEY
134500150511     C*
134600150511     C* Richiamo routine per aggiunta tasti funzione personalizzati
134700150511     C*
134800150511     C                   CAllb     'X46USREXT'
134900150511     C                   PARM                    *omit
135000150511     C                   PARM                    *omit
135100150511     C                   PARM                    *omit
135200150511     C                   PARM                    *omit
135300150511     C                   PARM                    *omit
135400150511     C                   PARM                    $FX1
135500150511     C                   PARM                    $FC1
135600150511     C                   PARM                    $PGF1
135700150511     C                   PARM                    DsPgm
135800150511     C                   PARM                    xsccut
135900150511     C                   PARM                    xscgrp
136500150511     C*
136600150511     C                   ENDSR
136700150511     C/EJECT
136800150511     C************************************************************
136900150511     C* REPERIMENTO TASTI DI FUNZIONE
137000150511     C************************************************************
137100150511     C     REPKEY        BEGSR
137200150511     C*
137300150511     C                   MOVEA     $FM1          $FM
137400150511     C                   MOVEA     $FC1          $FC
137500150511     C                   EXSR      REPKKK
137600150511     C                   MOVEA     $FX           $FX1
137700150511     C*
137800150511     C                   ENDSR
137900150511     C/EJECT
138000150511     C************************************************************
138100150511     C* REPERIMENTO TASTI DI FUNZIONE
138200150511     C************************************************************
138300150511     C     REPKKK        BEGSR
138400150511     C*
138500150511     C                   CLEAR                   $FX
138600150511     C     1             DO        24            X
138700150511     C*
138800150511     C     $FM(X)        IFNE      *BLANK
138900150511     C     $FC(X)        ANDNE     '0'
139000150511     C*
139100150511     C                   MOVEL     $FM(X)        IDMSG
139200150511     C                   CALLB     'XRTVM'
139300150511     C                   PARM                    IDMSG            27
139400150511     C                   PARM                    TXTMSG          100
139500150511     C                   PARM                    ERRMSG            1
139600150511     C*
139700150511     C     ERRMSG        IFNE      *ON
139800150511     C                   MOVEL     TXTMSG        $FX(X)
139900150511     C                   ELSE
140000150511     C                   MOVEL     *ALL' '       $FX(X)
140100150511     C                   ENDIF
140200150511     C*
140300150511     C                   ENDIF
140400150511     C*
140500150511     C                   ENDDO
140600150511     C*
140700150511     C                   ENDSR
140800150511     C/EJECT
140900150511     C************************************************************
141000150511     C* INIZIALIZZAZIONE VARIABILI
141100150511     C************************************************************
141200150511     C     INZVAR        BEGSR
141300150511     C*
141400150511     C* Dati da PGM guida
141500150511     C*
141600160728    >C                   MOVEL     KPJBU         SDGSK12DS
141700150511     C*
141800150511     C* Se società diversa da quella impostata nella *inzsr, rieseguo
141900150511     C* reperimento dati società
142000160728    >C                   If        SocietaK12 <> xscsoc    and
142100160728    >C                             OpzK12 <> *blank
142200150511     C                   MOVEL     'SOC001'      TIPXSC
142300160728    >C                   MOVEL     SocietaK12    SOCXSC
142400150511     C                   EXSR      REPSOC
142500150511     C     RTNXSC        IFNE      '1'
142600150511     C                   MOVEL     XSOCDS        SOC001
142700150511     C                   ELSE
142800160728    >C                   MOVE      '1'           ErrK12
142900150512     C                   MOVE      *On           $Fine
143000150512     C                   EXSR      ENDPGM
143100150511     C                   ENDIF
143200150511     C                   endif
143300150511     C*
143400150511     C* Definizione dei tasti funzione utilizzabili a pgm
143500150511     C*
143600150511     C                   EXSR      DEFOPZ
143700150511     C*
143800150511     C* Variabili per gestione videate
143900150511     C*
144000150511     C                   MOVE      'D1'          $GEST                          fmt in gestione
144100150511     C                   MOVE      *OFF          $FINE                          fine pgm
144200150511     C                   MOVE      *OFF          $FlgDef                        fine pgm
144300150511     C                   MOVE      *ON           $INZD1                         inizializzione D1
144400150511     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
144500150511     C                   MOVE      *BLANK        $LASTV                         ultima emissione
144600150511     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
144700150511     C* azzero la posizione del cursore
144800150511     C                   Z-ADD     0             H1RIGA
144900150511     C                   Z-ADD     0             H1COLO
145000150511     C*
145100150511     C* Variabili appoggio
145200150511     C*
145300150511     C                   Z-ADD     0             CURR
145400150511     C                   Z-ADD     0             CURC
145500150511     C*
145600150511     C* Valorizzazione campi univoci testate
145700150511     C*
145800150511     C                   CLEAR                   FMTT1
145900150511     C                   MOVEL     KNSIF         NOMSIF
146000150511     C                   MOVEL     XSCDSI        NOMDIT
146100150514     C                   MOVEL     DSPGM         NOMPGM           10
146101150514     C                   MoveL     KCDAZ         NOMAZN
146200150511     C* recupero il sottotitolo (se c'è)
146300150511     C                   MOVEL     *BLANK        T1TIT
146400150511     C     SOTTIT        IFNE      *BLANK
146500150511     C                   MOVEL     SOTTIT        IDMSG
146600150511     C                   CALLB     'XRTVM'
146700150511     C                   PARM                    IDMSG            27
146800150511     C                   PARM                    TXTMSG          100
146900150511     C                   PARM                    ERRMSG            1
147000150511     C                   PARM      *ON           CTRMSG            1            centratura
147100150511     C                   PARM      30            LENMSG            3 0          lun output.
147200150511     C     ERRMSG        IFNE      *ON
147300150511     C                   MOVEL     TXTMSG        T1TIT
147400150511     C                   ELSE
147500150511     C                   MOVEL     *ALL'?'       T1TIT
147600150511     C                   ENDIF
147700150511     C                   ENDIF
147800150511     C* recupero il tipo operazione
147900150511     C                   Z-ADD     1             X
148000160728    >C     OPZK12        LOOKUP    $OZ(X)                                 21
148100150511     C     *IN21         IFEQ      *ON
148200150511     C                   MOVEL     $ID(X)        IDMSG
148300150511     C                   CALLB     'XRTVM'
148400150511     C                   PARM                    IDMSG            27
148500150511     C                   PARM                    TXTMSG          100
148600150511     C                   PARM                    ERRMSG            1
148700150514     C                   PARM      *Off          CTRMSG            1            centratura
148800150511     C                   PARM      30            LENMSG            3 0          lun output.
148900150511     C     ERRMSG        IFNE      *ON
149000150511     C                   MOVEL     TXTMSG        T1OPE
149100150511     C                   ELSE
149200150511     C                   MOVEL     *ALL'?'       T1OPE
149300150511     C                   ENDIF
149400150511     C                   ENDIF
150800150511     C*
150900150511     C* Inizializzazione/Decodifica prima videata
151000150511     C*
151100150511     C                   EXSR      INZD1
151200150511     C                   MOVE      *OFF          $INZD1
151300150511     C*
151400150511     C                   ENDSR
151500150511     C/EJECT
151600150511     C************************************************************
151700150511     C* REPERIMENTO DATI SOCIETA'
151800150511     C************************************************************
151900150511     C     REPSOC        BEGSR
152000150511     C*
152001150512     C                   CALL      'SDGSOCDR'
152200150511     C                   PARM                    TIPXSC            6
152300150511     C                   PARM                    SOCXSC            3
152400150511     C                   PARM                    CDSXSC            9 0
152500150511     C                   PARM                    MODXSC            3
152600150511     C                   PARM      *BLANKS       RTNXSC            1
152700150511     C                   PARM                    XSOCDS
152800150511     C                   PARM                    KPJBA
152900150511     C*
153000150511     C                   ENDSR
153100150511     C/EJECT
153200150511     C************************************************************
153300150511     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
153400150511     C************************************************************
153500150511     C     DEFOPZ        BEGSR
153600150511     C*
153700150511     C* Questa routine deve valorizzare la schiera $FC1/$FR1
153800150511     C* con S o N a seconda della utilizzabilità o meno
153900150511     C* del tasto funzione correlato
154000150511     C*
154100150511     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
154200150511     C* di compilazione
154300150511     C                   RESET                   $FC1
154400150511     C                   RESET                   $FR1
154500150511     C*
154600150511     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
154700150511     C* invalido alcune opzioni
154800150511     C                   SELECT
154900150511     C* Se pgm richiamato in interrogazione disattivo :
155000160728    >C     OPZK12        WHENEQ    '05'
155100150511     C* F2=Decodifica
155200150511     C                   Z-ADD     2             X
155300150511     C                   MOVE      '0'           $FC1(X)
155400150511     C* F4=Ricerca
155500150511     C                   Z-ADD     4             X
155600150511     C                   MOVE      '0'           $FC1(X)
155700150511     C* F5=Ripristino
155800150511     C                   Z-ADD     5             X
155900150511     C                   MOVE      '0'           $FC1(X)
156000150511     C* F6=Conferma
156100150511     C                   Z-ADD     06            X
156200150511     C                   MOVE      '0'           $FC1(X)
156300150511     C* Se pgm richiamato in annullamento disattivo :
156400160728    >C     OPZK12        WHENEQ    '04'
156500150511     C* F2=Decodifica
156600150511     C                   Z-ADD     2             X
156700150511     C                   MOVE      '0'           $FC1(X)
156800150511     C* F4=Ricerca
156900150511     C                   Z-ADD     4             X
157000150511     C                   MOVE      '0'           $FC1(X)
157100150511     C* F5=Ripristino
157200150511     C                   Z-ADD     5             X
157300150511     C                   MOVE      '0'           $FC1(X)
157400150511     C*
157500150511     C                   OTHER
157600150511     C*
157700150511     C                   ENDSL
157800150511     C*
157900150511     C                   ENDSR
158000150511     C/EJECT
158100150511     C************************************************************
158200150511     C* REPERIMENTO CAMPI DI DEFAULT
158300150511     C************************************************************
158400150511     C     REPDEF        BEGSR
158500150511     C*
158600150511     C*
158700150511     C                   ENDSR
158800150511     C************************************************************
158900150511     C* DEFINIZIONE KLIST
159000150511     C************************************************************
159100150511     C     DefKlist      BEGSR
159200150511     C*
159300150511     C* klist
159400150511     C*
159500160728    >C* SDGSPA01L
159600160728    >C     K04SPA01      KLIST
159700160728    >C                   KFLD                    SPASocieta
159800160728    >C                   KFLD                    SPAKcc
159900160728    >C                   KFLD                    SPAKsc
160000160728    >C                   KFLD                    SPADtIni
160100150511     C*
160101160728     C* ANTRI01L
160102160728     C     K03TRI01      KLIST
160103160728     C                   KFLD                    TRISocieta
160104160728     C                   KFLD                    TRITributo
160105160728     C                   KFLD                    TRITriProg
160106160728     C*
160200150511     C                   ENDSR
160300150511     C/EJECT
160400150511     C************************************************************
160500150511     C* RIEMPIMENTO TASTI FUNZIONE
160600150511     C************************************************************
160700150511     C     RIEKEY        BEGSR
160800150511     C*
160900150511     C* Input :
161000150511     C* - $FX = schiera con tutti i testi disponibili
161100150511     C* - $FC = schiere con i flag di validità associati ai testi
161200150511     C* - $FR = schiere con i flag di REVERSE IMAGE
161300150511     C* - $ULK = ultimo testo caricato
161400150511     C* - $L01 = Lunghezza prima riga
161500150511     C* - $L02 = Lunghezza seconda riga
161600150511     C* Output :
161700150511     C* - $ULK
161800150511     C* - $KEY1= 1° riga di tasti funzione
161900150511     C* - $KEY2= 2° riga di tasti funzione
162000150511     C*
162100150511     C                   CALLB     'XKEYMSG'
162200150511     C* input
162300150511     C                   PARM                    $FX
162400150511     C                   PARM                    $FC
162500150511     C                   PARM                    $FR
162600150511     C                   PARM                    $ULK              3 0
162700150511     C* output
162800150511     C                   PARM                    $KEY1            79
162900150511     C                   PARM                    $KEY2            79
163000150511     C* input
163100150511     C                   PARM                    $L01              3 0
163200150511     C                   PARM                    $L02              3 0
163300150511     C*
163400150512     C                   PARM                    $ALLFUNCT       480
163500150512     C*
163600150511     C                   ENDSR
163700150511** $OZ / $ID
16370115051201SDGMSG    *LIBL     SDC0002
16390015051102PROMSG    *LIBL     COS0002
16400015051103PROMSG    *LIBL     COS0003
16410015051104PROMSG    *LIBL     COS0004
16420015051105PROMSG    *LIBL     COS0005
164300150511** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
164400150511PROMSG    *LIBL     KEY0001  01
164500150511PROMSG    *LIBL     KEY0002  02
164600150511PROMSG    *LIBL     KEY0003  03
164700150511PROMSG    *LIBL     KEY0004  04
164800150511PROMSG    *LIBL     KEY0005  05
164900150511PROMSG    *LIBL     KEY0006  06
165000150511PROMSG    *LIBL     KEY0007  07
165100150511PROMSG    *LIBL     KEY0008  08
165200150511PROMSG    *LIBL     KEY0009  09
165300150511PROMSG    *LIBL     KEY0010  10
165400150511PROMSG    *LIBL     KEY0011  11
165500150511PROMSG    *LIBL     KEY0012  12
165600150511PROMSG    *LIBL     KEY0013  13
165700150511PROMSG    *LIBL     KEY0014  14
165800150511PROMSG    *LIBL     KEY0015  15
165900150511PROMSG    *LIBL     KEY0016  16
166000150511PROMSG    *LIBL     KEY0017  17
166100150511PROMSG    *LIBL     KEY0018  18
166200150511PROMSG    *LIBL     KEY0019  19
166300150511PROMSG    *LIBL     KEY0020  20
166400150511PROMSG    *LIBL     KEY0021  21
166500150511PROMSG    *LIBL     KEY0022  22
166600150511PROMSG    *LIBL     KEY0023  23
166700150511PROMSG    *LIBL     KEY0024  24
166800150511**  ABILITAZIONE D1 : $FC1 / $FR1
1669001505110N  01
1670001505110N  02
1671001505111N  03
1672001505111N  04
1673001505111N  05
1674001505111N  06
1675001505110N  07
1676001505111N  08
1677001505110N  09
1678001505110N  10
1679001505110N  11
1680001505111N  12
1681001505110N  13
1682001505110N  14
1683001505110N  15
1684001505110N  16
1685001505110N  17
1686001505110N  18
1687001505110N  19
1688001505110N  20
1689001505110N  21
1690001505110N  22
1691001505110N  23   Impostare a '1' se si utilizzano opzioni
1692001505111N  24   Lasciare sempre impostata a '1' il valore della riga 24
