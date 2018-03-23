000100120306     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200120306     H*PARMS CVTOPT(*NONE)
000300120306     H DECEDIT('0,') DATEDIT(*DMY.)
000400120306      *
000500120306      *
000600120306      *            Black list : importi soglia
000700120306      *                      manutenzione
000800120306      *
000900120306      *
001000120306     F*----------------------------------------------------*
001100120306     FSDGBLS01L UF A E           K DISK
001200120306     F*------------
001300120306     FSDGSD21D  CF   E             WORKSTN
001400120306     F                                     RENAME(SD21D1:FMTD1)
001500120306     F                                     RENAME(SD21T1:FMTT1)
001600120306     F                                     RENAME(SD21Z1:FMTZ1)
001700120306     F* eventuali schermi supplementari
001800120306     F                                     RENAME(SD21D2:FMTD2)
001900120306     F                                     INFDS(DSFMT)
002000120306     F*------------
002100120306     D* Prototipazione procedure
002200120306     D/COPY *LIBL/SRCCPY,PJX002PR
002300120306     D*----------------------------------------------------*
002400120306     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002500120306     D $ID             S             27    DIM(5) ALT($OZ)
002600120306     D* Schiere per la gestione dei tasti di funzione
002700120306     D $FM             S             27    DIM(24)
002800120306     D $FX             S             50    DIM(24)
002900120306     D $FC             S              1    DIM(24)
003000120306     D $FR             S              1    DIM(24)
003100120306     D* Video D1
003200120306     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003300120306     D $FX1            S             50    DIM(24) ALT($FM1)
003400120306     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003500120306     D $FR1            S              1    DIM(24) ALT($FC1)
003600120306     D* Schiera contente i nomi dei programmi da richiamare con i tasti
003700120306     D* funzione personalizzati
003800120306     D $PGF1           S             10    DIM(25)
003900120306     D*----------------------------------------------------*
004000120306     D $D1L01          S              3  0
004100120306     D $D1L02          S              3  0
004200120306      * Inserire qui la definizione di altre variabili per reperire
004300120306      * la lunghezza di altre videate che usano un piede diverso da
004400120306      * STD
004500120306     D*-------------
004600120306     D* Passaggio Parametri
004700120306     D KPJBA         E DS
004800120306     D*-------------
004900120306     D* DS Esterna gestione messaggi
005000120306     D SDGMSGDS      E DS
005100120306     D  STS          E                     EXTFLD(MSGSTS)
005200120306     D                                     DIM(333)
005300120306     D  JBA          E                     EXTFLD(MSGJBA)
005400120306     D                                     DIM(502)
005500120306     D*-------------
005600120306     D* Reperimento Autorizzazioni
005700120306     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005800120306     D*-------------
005900120306     D* Passaggio Autorizzazioni
006000120306     D DSAUT           DS           512
006100120306     D*-------------
006200120306     D* Maschera dati autorizzazioni
006300120306     D CONCIVDS      E DS
006400120306     D*-------------
006500120306     D* Reperimento nome PGM
006600120306     D STATUS         SDS           333
006700120306     D  DSPGM            *PROC
006800120306     D  PARMS            *PARMS
006900120306     D*-------------
007000120306     D* Dati di ambiente ottenuti da XSOC
007100120306     D SOC001        E DS                  EXTNAME(XSOC001DS)
007200120306     D*-------------
007300120306     D* DS Interna per dati di output di XSOC
007400120306     D XSOCDS          DS          1000
007500120306     D*-------------
007600120306     D* Passaggio Parametri al PGM di manutenzione
007700120306     D SDGSD21DS     E DS
007800120306     D*-------------
007900120306     D* campo di salvataggio per la DS d'ingresso durante la chiamata
008000120306     D* alla funzione personalizzata
008100120306     D $SaveDS         S                   like(SDGSD21DS)
008200120306     D*-------------
008300120306     D* inserire tutte le altre DS di passaggio parametri
008400120306     D*-------------
008500120306     D DSFMT           DS
008600120306     D  $TASTO               369    369
008700120306     D  NRG                  370    370
008800120306     D  NCL                  371    371
008900120306     D*-------------
009000120306     D* ds per gestione periodi a video
009100120306     D d1ini           DS            48
009200120306     D inf                            4  0 dim(12)
009300120306     D d1sup           DS            48
009400120306     D sup                            4  0 dim(12)
009500120306     D*-------------
009600120306     D* DS per UserExit
009700120306     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
009800120306     D*-------------
009900120306     D* posizione cursore
010000120306     D CURSOR          DS
010100120306     D  RIRI                   1      2B 0 INZ
010200120306     D  R$$                    2      2
010300120306     D  COCO                   3      4B 0 INZ
010400120306     D  C$$                    4      4
010500120306     D*-------------
010600120306     D* Costanti
010601151117R195 D  Data2015       S             10A   inz('2015-01-01')
010700120306     D*-------------
010800120306     D* Tasti di funzione
010900120306     D F01             C                   CONST(X'31')
011000120306     D F02             C                   CONST(X'32')
011100120306     D F03             C                   CONST(X'33')
011200120306     D F04             C                   CONST(X'34')
011300120306     D F05             C                   CONST(X'35')
011400120306     D F06             C                   CONST(X'36')
011500120306     D F07             C                   CONST(X'37')
011600120306     D F08             C                   CONST(X'38')
011700120306     D F09             C                   CONST(X'39')
011800120306     D F10             C                   CONST(X'3A')
011900120306     D F11             C                   CONST(X'3B')
012000120306     D F12             C                   CONST(X'3C')
012100120306     D F13             C                   CONST(X'B1')
012200120306     D F14             C                   CONST(X'B2')
012300120306     D F15             C                   CONST(X'B3')
012400120306     D F16             C                   CONST(X'B4')
012500120306     D F17             C                   CONST(X'B5')
012600120306     D F18             C                   CONST(X'B6')
012700120306     D F19             C                   CONST(X'B7')
012800120306     D F20             C                   CONST(X'B8')
012900120306     D F21             C                   CONST(X'B9')
013000120306     D F22             C                   CONST(X'BA')
013100120306     D F23             C                   CONST(X'BB')
013200120306     D F24             C                   CONST(X'BC')
013300120306     D ENTER           C                   CONST(X'F1')
013400120306     D ROLDWN          C                   CONST(X'F4')
013500120306     D ROLLUP          C                   CONST(X'F5')
013600120306     D*-------------
013700120306     D* Sottotitolo di testata
013800120306     D SottitT1        C                   CONST('SDGMSG    *LIBL-
013900120306     D                                          SDC0070')
014000120306     D*-------------
014100120306     D* Variabili per gestione videate
014200120306     D $Gest           S              2A
014300120306     D $Fine           S              1A
014400120306     D $InzD1          S              1A
014500120306     D $LastG          S              2A
014600120306     D $LastV          S              2A
014700120306     D $FlgDef         S              1A
014800120306     D*-------------
014900120306     D* Variabili appoggio sempre presenti in tutte le anagrafiche
015000120306     D CurR            S              2S 0
015100120306     D CurC            S              2S 0
015200120306     D Win             S             99A
015300120306     D*-------------
015400120306     D* Variabili appoggio specifiche della singola anagrafica
015500120306     D* elenco tutti i campi di appoggio default
015600120306     D*  Indici
015700120306     D X               S              3S 0
015800120306     D Y               S              3S 0
015900120306     D* Indice della schiera tasti funzione
016000120306     D PosFun          S              2  0
016100120306     D*-------------
016200120306     D* Variabili appoggio per le date
016300120306     D WDtIni          S               D
016400120306     D WDtFin          S               D
016500120306     D $DtIni2         S               D
016600120306     D $DtFin2         S               D
016700120306     D NewDtIni        s               d
016800120306     D NewDtFin        s               d
016900120306     D OldDtIni        s               d
017000120306     D OldDtFin        s               d
017100120306     D SavDtIni        S               D
017200120306     D SavDtFin        S               D
017300120306     D $Data           S               D
017301151117R195 D $Data2015       S               D
017400120306     C*----------------------------------------------------*
017500120306     C*                MAIN LINE PROGRAM
017600120306     C*----------------------------------------------------*
017700120306     C*
017800120306     C* inizializzazione variabili
017900120306     C                   EXSR      INZVAR
018000120306     C*
018100120306     C     $FINE         DOWEQ     *OFF
018200120306     C     $GEST         CASEQ     'D1'          GESD1
018300120306     C                   ENDCS
018400120306     C                   ENDDO
018500120306     C*
018600120306     C                   EXSR      ENDPGM
018700120306     C/EJECT
018800120306     C************************************************************
018900120306     C* Fine Programma
019000120306     C************************************************************
019100120306     C     ENDPGM        BEGSR
019200120306     C*
019300120306     C* se ho confermato l'uscita proseguo
019400120306     C     $FINE         IFEQ      *ON
019500120306     C*
019600120306     C* disallocazione rcd nel caso di modifica/annullamento
019700120306     C     OPZD21        IFEQ      '02'
019800120306     C     OPZD21        OREQ      '04'
019900120306     C                   UNLOCK    SDGBLS01L                            22
020000120306     C                   ENDIF
020100120306     C*
020200120306     C* passaggio dati al pgm chiamante
020300120306     C                   MOVEL     SDGSD21DS     KPJBU
020400120306     C*
020500120306     C                   SETON                                            RT
020600120306     C                   RETURN
020700120306     C*
020800120306     C                   ENDIF
020900120306     C*
021000120306     C                   ENDSR
021100120306     C/EJECT
021200120306     ************************************************************
021300120306     C* RICERCHE
021400120306     C************************************************************
021500120306     C     SEARCH        BEGSR
021600120306     C*
021700120306     C* determino Riga/Colonna del cursore
021800120306     C                   MOVE      NRG           R$$
021900120306     C                   MOVE      NCL           C$$
022000120306     C                   Z-ADD     RIRI          CURR
022100120306     C                   Z-ADD     COCO          CURC
022200120306     C*
022300120306     C                   SELECT
022400120306     C*
022500120306     C* ricerca non abilitata in questa posizione
022600120306     C                   OTHER
022700120306     C                   SETON                                        9899
022800120306     C*
022900120306     C                   ENDSL
023000120306     C*
023100120306     C* imposto pos. del cursore
023200120306     C                   Z-ADD     CURR          H1RIGA
023300120306     C                   Z-ADD     CURC          H1COLO
023400120306     C*
023500120306     C                   MOVE      *HIVAL        $LASTV
023600120306     C*
023700120306     C                   ENDSR
023800120306     C/EJECT
023900120306     C************************************************************
024000120306     C* GESTIONE EMISSIONE TASTI FUNZIONE
024100120306     C************************************************************
024200120306     C     GESPIE        BEGSR
024300120306     C*
024400120306     C                   SELECT
024500120306     C*
024600120306     C     $GEST         WHENEQ    'D1'
024700120306     C     $ULKD1        IFEQ      0
024800120306     C* riempimento opzioni
024900120306     C                   EXSR      F24D1
025000120306     C                   ELSE
025100120306     C                   WRITE     FMTZ1
025200120306     C                   ENDIF
025300120306     C*
025400120306     C                   ENDSL
025500120306     C*
025600120306     C                   ENDSR
025700120306     C/EJECT
025800120306     C************************************************************
025900120306     C* Chiamata programma con tasto personalizzato
026000120306     C************************************************************
026100120306     C     FPerD1        BEGSR
026200120306     C*
026300120306     C* salvo la DS di ingresso
026400120306     C                   MOVEL     SDGSD21DS     $SaveDS
026500120306     C*
026600120306     C                   MOVE      xscsoc        SocietaD21
026700120306     C* passare nei campi appositi della DS SDGSD21DS il valore
026800120306     C* della chiave della VL univoca
026900120306     C*                  MOVEL     D1Codice      Codice002
027000120306     C*
027100120306     C                   MOVEL     SDGSD21DS     KPJBU
027200120306     C                   CALL      $PGF1(PosFun)                        22
027300120306     C                   PARM                    KPJBA
027400120306     C                   PARM                    NOMPGM
027500120306     C*
027600120306     C* ripristino la DS di ingresso
027700120306     C                   MOVEL     $SaveDS       SDGSD21DS
027800120306     C*
027900120306     C                   ENDSR
028000120306     C/EJECT
028100120306     C************************************************************
028200120306     C* GESTIONE VIDEO RECORD D1
028300120306     C************************************************************
028400120306     C     GESD1         BEGSR
028500120306     C* inizializzazione videata
028600120306   B1C     $INZD1        IFEQ      *ON
028700120306     C                   EXSR      INZD1
028800120306     C                   MOVE      *OFF          $INZD1
028900120306   E1C                   ENDIF
029000120306     C* emissione piede videata se proveniente da altra
029100120306     C* salvataggio valore formato di provenienza
029200120306   B1C     $LASTV        IFNE      'D1'
029300120306     C                   WRITE     FMTT1
029400120306     C                   EXSR      GESPIE
029500120306     C                   MOVE      $LASTV        $LASTG
029600120306     C                   MOVE      'D1'          $LASTV
029700120306   E1C                   ENDIF
029800120306     C*
029900120306     C* se esistono errori sulla videata
030000120306     C* emetto la write del formato a indicatori spenti per vedere
030100120306     C* le eventuali decodifiche
030200120306   B1C     *IN99         IFEQ      *ON
030300120306     C     $GEST         ANDEQ     'D1'
030400120306     C                   MOVEA     *IN           WIN
030500120306     C                   MOVE      *ALL'0'       IN5098           49
030600120306     C                   MOVEA     IN5098        *IN(50)
030700120306     C                   WRITE     FMTD1
030800120306     C                   MOVEA     WIN           *IN
030900120306   E1C                   ENDIF
031000120306     C* Inizializzazione indice tasti funzione personalizzati
031100120306     C                   Eval      PosFun = 25
031200120306     C*
031300120306   B1C                   SELECT
031400120306     C* Annullamento
031500120306     C     OPZD21        WHENEQ    '04'
031600120306     C* Visualizzazione
031700120306     C     OPZD21        OREQ      '05'
031800120306     C                   WRITE     FMTD1
031900120306     C                   EXFMT     PROTECT
032000120306     C* Immissione/Modifica/Copia
032100120306   X1C                   OTHER
032200120306     C                   EXFMT     FMTD1
032300120306   E1C                   ENDSL
032400120306     C*
032500120306     C* azzero la posizione del cursore
032600120306     C                   Z-ADD     0             H1RIGA
032700120306     C                   Z-ADD     0             H1COLO
032800120306     C*
032900120306     C* controllo abilitazione tasti funzione
033000120306     C                   EXSR      CTRKD1
033100120306     C*
033200120306   B1C     *IN99         IFEQ      *OFF
033300120306     C*
033400120306   B2C                   SELECT
033500120306     C* F2=Decodifica
033600120306     C*    $TASTO        WHENEQ    F02
033700120306     C*                  EXSR      F02D1
033800120306     C* F3=Fine
033900120306     C     $TASTO        WHENEQ    F03
034000120306     C                   EXSR      F03D1
034100120306     C* F4=Ricerca
034200120306     C     $TASTO        WHENEQ    F04
034300120306     C                   EXSR      SEARCH
034400120306     C* F5=Refresh
034500120306     C     $TASTO        WHENEQ    F05
034600120306     C                   EXSR      F05D1
034700120306     C* F8=Successivo
034800120306     C     $TASTO        WHENEQ    F08
034900120306     C                   EXSR      F08D1
035000120306     C* F12=Ritorno
035100120306     C     $TASTO        WHENEQ    F12
035200120306     C                   EXSR      F12D1
035300120306     C* F24=Altri tasti
035400120306     C     $TASTO        WHENEQ    F24
035500120306     C                   EXSR      F24D1
035600120306     C* Personalizzata
035700120306     C     $PGF1(PosFun) WHENNE    *BLANK
035800120306     C                   EXSR      FPerD1
035900120306     C*
036000120306   X2C                   OTHER
036100120306     C                   EXSR      CTRD1
036200120306     C* aggiungere le chiamate ai controlli delle videate supplementari
036300120306     C*
036400120306   B3C     $TASTO        IFEQ      F06
036500120306     C     *IN99         ANDEQ     *OFF
036600120306     C* eseguo aggiornamento
036700120306     C                   EXSR      AGGANA
036800120306     C* eseguo operazioni del dopo-aggiornamento
036900120306   B4C     *IN99         IFEQ      *OFF
037000120306     C                   EXSR      GESAGG
037100120306   E4C                   ENDIF
037200120306   E3C                   ENDIF
037300120306     C*
037400120306   E2C                   ENDSL
037500120306     C*
037600120306   E1C                   ENDIF
037700120306     C*
037800120306     C                   ENDSR
037900120306     C/EJECT
038000120306     C************************************************************
038100120306     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
038200120306     C************************************************************
038300120306     C     CTRKD1        BEGSR
038400120306     C*
038500120306     C                   SETOFF                                       99
038600120306     C                   MOVEA     $FC1          $FC
038700120306     C                   EXSR      CTRKEY
038800120306     C     *IN99         IFEQ      *ON
038900120306     C                   SETON                                        97
039000120306     C                   ENDIF
039100120306     C*
039200120306     C                   ENDSR
039300120306     C/EJECT
039400120306     C************************************************************
039500120306     C* INIZIALIZZAZIONE VIDEATA DATI
039600120306     C************************************************************
039700120306     C     INZD1         BEGSR
039800120306     C*
039900120306     C                   CLEAR                   FMTD1
040000120306     C*
040100120312     C                   Eval      D1On1       = XScOn
040200120312     C                   Eval      D1Off1      = XScOff
040300120312     C*
040400120306     C                   Clear                   WDtIni
040500120306     C                   Clear                   WDtFin
040600120306     C                   Clear                   $DtIni2
040700120306     C                   Clear                   $DtFin2
040800120306     C*
040801151117R195 C     *Iso          Movel     Data2015      $data2015
040900120306     C* per tutte le opzioni NON immissione
041000120306   B>C     OPZD21        IFNE      '01'
041100120306     C*
041200120306     C                   MoveL     SocietaD21    BLSSocieta
041300120306     C                   MoveL     DtIniD21      BLSDtIni
041400120306     C*
041500120306   B2C     *IN22         DOUEQ     *OFF
041600120306     C     MSGRTN        ORNE      'R'
041700120306     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
041800120306   B C     OPZD21        IFEQ      '02'
041900120306     C     OPZD21        OREQ      '04'
042000120306     C     K02BLS01      CHAIN     SDGBLS01L                          2122
042100120306     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
042200120306   X3C                   ELSE
042300120306     C     K02BLS01      CHAIN(N)  SDGBLS01L                          2122
042400120306   E3C                   ENDIF
042500120306     C*
042600120306     C* Gestione RCD allocato
042700120306   B3C     *IN22         IFEQ      *ON
042800120306     C                   MOVEL     'SDGBLS00F'   MSGFIL
042900120306     C                   EXSR      MSGPGM
043000120306   E3C                   ENDIF
043100120306   E2C                   ENDDO
043200120306     C*
043300120306     C* se il rcd da gestire non viene trovato
043400120306     C* oppure è allocato da altro pgm
043500120306     C* ritorno al pgm guida avvertendolo dell'errore
043600120306   B2C     *IN21         IFEQ      *ON
043700120306     C     *IN22         OREQ      *ON
043800120306     C                   MOVE      *ON           $FINE
043900120306    >C                   MOVE      '1'           ERRD21
044000120306     C*
044100120306     C* altrimenti
044200120306   X2C                   ELSE
044300120306     C* riempio la videata
044400120306     C                   EXSR      RIED1
044500120306      ** Aggiungere le routine di riempimento dei record succcessivi
044600120306     C*
044700120306     C* richiamo routine dei ctrl per decodificarla
044800120306     C                   EXSR      CTRD1
044900120306      ** Aggiungere le routine di controllo dei record succcessivi
045000120306     C*
045100120306     C                   MOVE      *ALL'0'       IN5098
045200120306     C                   MOVEA     IN5098        *IN(50)
045300120306   E2C                   ENDIF
045400120306     C*
045500120306     C* per immissione
045600120306   X1C                   ELSE
045700120306     C* carico la divisa a video
045800120306     C                   move      xscdiv        d1divisa
045900120306     C*
046000120306     C* riempio con i default di immissione  (se reperiti)
046100120306     C                   IF        $FlgDef = *on
046200120306     C                   EXSR      RIEDEF
046300120306     C                   ENDIF
046301151117R195 C*
046302151117R195 C                   Movel     XscOff        D1Note
046400120306     C*
046500120306   E1C                   ENDIF
046600120306     C*
046700120306     C* imposto quegli attributi della videata che sono modificabili
046800120306     C* al richiamo del determinato pgm
046900120306     C                   EXSR      ATRD1
047000120306     C*
047100120306     C* codice divisa
047200120306     C                   Eval      D1DivisaD   = *Blank
047300120306     C                   IF        D1Divisa   <> *Blank
047400120306     C                   IF        OpzD21     <> '01'
047500120306     C     *Jobrun       Move      D1DtFin       $Data
047600120306     C                   ELSE
047700120306     C     *Dmy          Move      UDate         $Data
047800120306     C                   endif
047900120306     C                   IF        X0201RpDVS(D1Divisa :$Data:
048000120306     C                                    ANDVS00F:%size(ANDVS00F):
048100120306     C                                    ANDVX00F:%size(ANDVX00F)) =0
048200120306     C                   MOVEL     DVSDesBrev    D1DivisaD
048300120306     C                   ENDIF
048400120306     C                   ENDIF
048500120306     C*
048600120306     C                   ENDSR
048700120306     C/EJECT
048800120306     C************************************************************
048900120306     C* RIEMPIMENTO VIDEATA  D1
049000120306     C************************************************************
049100120306     C     RIED1         BEGSR
049200120306     C*
049300120306     C* impostare i campi della videata dai campi del file
049400120306     C*
049500120306     C                   if        BLSDtIni   <> *Loval
049600120306     C     *Jobrun       Move      BLSDtIni      D1DtIni
049700120306     C                   endif
049800120306     C                   if        BLSDtFin   <> *Loval
049900120306     C     *Jobrun       Move      BLSDtFin      D1DtFin
050000120306     C                   endif
050100120306     C*
050200120306     C* salvo le date per i controlli
050300120306     C                   Move      BLSDtIni      $DtIni2
050400120306     C                   Move      BLSDtFin      $DtFin2
050500120306     C*
050600120306     C                   if        OpzD21      = '1'
050700120306     C                   MoveL     XScDiv        D1Divisa
050800120306     C                   else
050900120306     C                   MoveL     BLSDivisa     D1Divisa
051000120306     C                   endif
051100120306     C                   Z-ADD     BLSImpSogl    D1ImpSogl
051200120312     C*
051300120312     C                   if        BLSNote     = *On
051400120312     C                   Eval      D1Note      = XScOn
051500120312     C                   else
051600120312     C                   Eval      D1Note      = XScOff
051700120312     C                   endif
051800120306     C*
051900120306     C                   ENDSR
052000120306     C************************************************************
052100120306     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
052200120306     C************************************************************
052300120306     C     ATRD1         BEGSR
052400120306     C*
052500120306     C                   SETOFF                                       02
052600120306     C*
052700120306     C* in caso di manutenzione
052800120306     C     OPZD21        IFEQ      '02'
052900120306     C* proteggo i campi chiave
053000120306     C                   SETON                                        02        PR campi key
053100120306   E3C                   ENDIF
053101151117     C*
053102151117R195 C* in caso di anno >= al 2015 oppure inserimento
053103151117  "  C     DTINID21      IFGE      $data2015
053104151117  "  C     DTINID21      OREQ      *loval
053105151117  "  C*  proteggo il campo di gestione soglia nelle note
053106151117  "  C                   SETON                                        06
053109151117R195 C                   ENDIF
053200120306     C*
053300120306     C                   ENDSR
053400120306     C/EJECT
053500120306     C************************************************************
053600120306     C* RIEMPIMENTO CAMPI DAI DEFAULT
053700120306     C************************************************************
053800120306     C     RIEDEF        BEGSR
053900120306     C*
054000120306      * riempire i campi con i default recuperati dal codice di
054100120306      * riferimento
054200120306     C*
054300120306     C                   ENDSR
054400120306     C/EJECT
054500120306     C************************************************************
054600120306     C* GESTIONE F03 VIDEO D1
054700120306     C************************************************************
054800120306     C     F03D1         BEGSR
054900120306     C*
055000120306     C                   MOVE      *ON           $FINE
055100120306     C                   MOVE      '1'           RETD21
055200120306     C* fine programma
055300120306     C                   EXSR      ENDPGM
055400120306     C*
055500120306     C                   ENDSR
055600120306     C/EJECT
055700120306     C************************************************************
055800120306     C* GESTIONE F05 VIDEO D1
055900120306     C************************************************************
056000120306     C     F05D1         BEGSR
056100120306     C*
056200120306     C                   MOVE      *ON           $INZD1
056300120306     C*
056400120306     C                   ENDSR
056500120306     C/EJECT
056600120306     C************************************************************
056700120306     C* GESTIONE F08 VIDEO D1
056800120306     C************************************************************
056900120306     C     F08D1         BEGSR
057000120306     C*
057100120306     C                   MOVE      *ON           $FINE
057200120306     C                   MOVE      '0'           RETD21
057300120306     C*
057400120306     C                   ENDSR
057500120306     C/EJECT
057600120306     C************************************************************
057700120306     C* GESTIONE F12 VIDEO D1
057800120306     C************************************************************
057900120306     C     F12D1         BEGSR
058000120306     C*
058100120306     C                   MOVE      *ON           $FINE
058200120306     C                   MOVE      '2'           RETD21
058300120306     C*
058400120306     C                   ENDSR
058500120306     C/EJECT
058600120306     C************************************************************
058700120306     C* GESTIONE F24 VIDEO D1
058800120306     C************************************************************
058900120306     C     F24D1         BEGSR
059000120306     C*
059100120306     C                   MOVEA     $FX1          $FX
059200120306     C                   MOVEA     $FC1          $FC
059300120306     C                   MOVEA     $FR1          $FR
059400120306     C                   Z-ADD     $ULKD1        $ULK
059500120306     C                   Z-ADD     $D1L01        $L01
059600120306     C                   Z-ADD     $D1L02        $L02
059700120306     C                   EXSR      RIEKEY
059800120306     C                   Z-ADD     $ULK          $ULKD1            3 0
059900120306     C                   MOVEA     $FC           $FC1
060000120306     C                   MOVEL     $KEY1         Z1KE1
060100120306     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
060200120306     C                   MOVEL     $KEY2         Z1KE2
060300120306     C                   WRITE     FMTZ1
060400120306     C*
060500120306     C                   ENDSR
060600120306     C/EJECT
060700120306     C************************************************************
060800120306     C* CONTROLLO VIDEATA
060900120306     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
061000120306     C* 95 96 97 98
061100120306     C************************************************************
061200120306     C     CTRD1         BEGSR
061300120306     C*
061400120306     C                   SETOFF                                       99
061500120306     C*
061600120306     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
061700120306     C*
061800120306   f1C     OPZD21        IFNE      '04'
061900120306     C*
062000120306     C                   EXSR      CtrDtValid
062100120306     C*
062200120306     C                   if        *In99      =  *Off
062300120306     C                   EXSR      CtrPeriodo
062400120306   f2C                   endif
062500120312     C*
062600120312     C                   EXSR      CtrNote
062700120306     C*
062800120306     C*
062900120306     C                   if        *In99       = *Off
063000120306     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
063100120306     C                   if        OpzD21      = '03'         or
063200120306     C                             OpzD21      = '01'
063300120306     C*
063400120306     C                   MoveL     XScSoc        BLSSocieta
063500120306     C     *JobRun       MoveL     D1DtIni       BLSDtIni
063600120306     C     K02BLS01      SETLL     SDGBLS01L                              21
063700120306     C*
063800120306     C                   if        *In21       = '1'
063900120306     C                   SetOn                                        9099
064000120306     C                   endif
064100120306     C*
064200120306     C                   ENDIF
064300120306     C*
064400120306     C                   endif
064500120306     C*
064600120306     C                   ENDIF
064700120306     C*
064800120306     C                   ENDSR
064900120306     C/EJECT
065000120306     C************************************************************
065100120306     C* AGGIORNAMENTO ANAGRAFICA
065200120306     C************************************************************
065300120306     C     CtrDtValid    BEGSR
065400120306     C*
065500120306     C* obbligo data inizio validità
065600120306     C                   if        D1DtIni     = *Zeros
065700120306     C                   SetOn                                        5099
065800120306     C                   else
065900120306     C     *Jobrun       Test(D)                 D1DtIni                21
066000120306     C                   if        *In21       = *On
066100120306     C                   SetOn                                        5199
066200120306     C                   endif
066300120306     C                   endif
066400120306     C*
066500120306     C* obbligo data fine validità
066600120306     C                   if        D1DtFin     = *Zeros
066700120306     C                   SetOn                                        5299
066800120306     C                   else
066900120306     C     *Jobrun       Test(D)                 D1DtFin                21
067000120306     C                   if        *In21       = *On
067100120306     C                   SetOn                                        5399
067200120306     C                   endif
067300120306     C                   endif
067400120306     C*
067500120306     C* controllo che data fine >= data inizio
067600120306     C                   if        *In99      =  *Off
067700120306     C                   if        D1DtIni   <>  *Loval
067800120306     C     *JobRun       Move      D1DtIni       WDtIni
067900120306     C                   endif
068000120306     C                   if        D1DtFin   <>  *Loval
068100120306     C     *JobRun       Move      D1DtFin       WDtFin
068200120306     C                   endif
068300120306     C                   if        WDtFin     <  WDtIni
068400120306     C                   SetOn                                        5499
068500120306     C                   endif
068600120306     C                   endif
068700120306     C*
068800120306     C                   ENDSR
068900120306     C/EJECT
069000120306     C************************************************************
069100120306     C* Controllo sovrapposizione periodo di validità
069200120306     C************************************************************
069300120306     C     CtrPeriodo    BEGSR
069400120306     C*
069500120306     C*
069600120306     C* muovo la data che c'era sui campi a video prima della modifica
069700120306     C                   Move      $DtFin2       SavDtFin
069800120306     C                   Move      $DtIni2       SavDtIni
069900120306     C*
070000120306     C* muovo la data che c'é sui campi video dopo la modifica
070100120306     C                   if        D1DtIni   <>  *Loval
070200120306     C     *JobRun       Move      D1DtIni       WDtIni
070300120306     C                   endif
070400120306     C                   if        D1DtFin   <>  *Loval
070500120306     C     *JobRun       Move      D1DtFin       WDtFin
070600120306     C                   endif
070700120306     C*
070800120306     C                   MoveL     XScSoc        BLSSocieta
070900120306     C     *JobRun       Move      D1DtIni       BLSDtIni
071000120306     C*
071100120306     C     K02BLS01      SETLL     SDGBLS01L
071200120306     C     LEGGI         TAG
071300120306     C     BLSSocieta    READE     SDGBLS01L                              21
071400120306     C*
071500120306     C                   if        *In21       = *Off
071600120306     C                   Move      BLSDtIni      NewDtIni
071700120306     C                   MOVE      BLSDtFin      NewDtFin
071800120306     C*
071900120306   f4C                   if        SavDtIni    = NewDtIni     and
072000120306     C                             SavDtFin    = NewDtFin
072100120306     C*
072200120306     C                   if        OpzD21      = '03'         or
072300120306     C                             OpzD21      = '01'
072400120306     C*
072500120306     C                   Select
072600120306     C                   When      WDtIni     >= SavDtIni     and
072700120306     C                             WDtFin     <= SavDtFin
072800120306     C                   SetOn                                        569957
072900120306     C*
073000120306     C                   When      WDtFin      = SavDtFin
073100120306     C                   SetOn                                        569957
073200120306     C                   endSl
073300120306     C*
073400120306     C                   else
073500120306     C                   GOTO      LEGGI
073600120306   f5C                   endif
073700120306     C*                  else
073800120306     C*                  GOTO      LEGGI
073900120306   f4C                   endif
074000120306     C*
074100120306     C                   if        WDtIni     <= NewDtFin       and
074200120306     C                             WDtIni     >= NewDtIni
074300120306     C                   SETON                                        569957
074400120306     C                   endif
074500120306     C*
074600120306     C                   if        WDtFin     >= NewDtIni
074700120306     C                   SetOn                                        569957
074800120306     C                   endif
074900120306     C*
075000120306     C                   endif
075100120306     C*
075200120306   f6C     K02BLS01      SETLL     SDGBLS01L
075300120306     C     LEGGI2        TAG
075400120306     C     BLSSocieta    READPE    SDGBLS01L                              21
075500120306     C                   if        *IN21       = *Off
075600120306     C                   Move      BLSDtIni      OldDtIni
075700120306     C                   Move      BLSDtFin      OldDtFin
075800120306     C*
075900120306     C                   if        SavDtIni    = OldDtIni     and
076000120306     C                             SavDtFin    = OldDtFin
076100120306     C*
076200120306     C                   if        OpzD21      = '03'         or
076300120306     C                             OpzD21      = '01'
076400120306     C*
076500120306     C                   Select
076600120306     C                   When      WDtIni     >= SavDtIni     and
076700120306     C                             WDtFin     <= SavDtFin
076800120306     C                   SETON                                        569957
076900120306     C*
077000120306     C                   When      WDtFin      = SavDtFin
077100120306     C                   SETON                                        569957
077200120306     C                   endSl
077300120306     C*
077400120306     C                   else
077500120306     C                   GOTO      LEGGI2
077600120306   f8C                   endif
077700120306     C*                  else
077800120306     C*                  GOTO      LEGGI2
077900120306   f7C                   endif
078000120306     C*
078100120306     C                   if        WDtFin     >= OldDtIni    and
078200120306     C                             WDtFin     <= OldDtFin
078300120306     C                   Seton                                        569957
078400120306     C                   endif
078500120306     C*
078600120306     C                   if        WDtIni     <= OldDtFin
078700120306     C                   SetOn                                        569957
078800120306     C                   endif
078900120306   f6C                   endif
079000120306     C*
079100120306     C                   ENDSR
079200120306     C/EJECT
079300120306     C************************************************************
079400120312     C* Controllo flag per applicare soglia alle note
079500120306     C************************************************************
079600120312     C     CtrNote       BEGSR
079700120306     C*
079800120312     C                   if        D1Note    <>   XScOn   and
079900120312     C                             D1Note    <>   XScOff
080000120312     C                   SetOn                                        5899
080100120312     C                   endif
080200120312     C*
080300120312     C                   ENDSR
080400120312     C/EJECT
080500120312     C************************************************************
080600120312     C* AGGIORNAMENTO ANAGRAFICA
080700120312     C************************************************************
080800120312     C     AGGANA        BEGSR
080900120312     C*
081000120306     C                   SELECT
081100120306     C     OPZD21        WHENEQ    '01'
081200120306     C     OPZD21        OREQ      '03'
081300120306     C                   EXSR      RIEANA
081400120306     C                   WRITE     SDGBLS000                            22
081500120306     C* se rcd non scrivibile attivo errore
081600120306     C     *IN22         IFEQ      *ON
081700120306     C                   SETON                                        9599
081800120306     C                   ENDIF
081900120306     C     OPZD21        WHENEQ    '02'
082000120306     C*
082100120306     C* rileggo il record
082200120306     C                   MoveL     SocietaD21    BLSSocieta
082300120306     C                   MoveL     DtIniD21      BLSDtIni
082400120306     C     K02BLS01      CHAIN     SDGBLS01L                          21
082500120306     C                   EXSR      RIEANA
082600120306     C                   UPDATE    SDGBLS000
082700120306     C     OPZD21        WHENEQ    '04'
082800120306     C                   DELETE    SDGBLS000
082900120306     C                   ENDSL
083000120306     C*
083100120306     C                   ENDSR
083200120306     C/EJECT
083300120306     C************************************************************
083400120306     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
083500120306     C************************************************************
083600120306     C     RIEANA        BEGSR
083700120306     C*
083800120306     C                   CLEAR                   SDGBLS000
083900120306     C*
084000120306     C* imposto i campi del file da quelli video
084100120306     C                   MoveL     XScSoc        BLSSocieta
084200120306     C     *Jobrun       Move      D1DtIni       BLSDtIni
084300120306     C     *Jobrun       Move      D1DtFin       BLSDtFin
084400120306     C                   MoveL     D1Divisa      BLSDivisa
084500120306     C                   Z-ADD     D1ImpSogl     BLSImpSogl
084600120312     C                   if        D1Note      = XScOn
084700120312     C                   Eval      BLSNote     = *On
084800120312     C                   else
084900120312     C                   Eval      BLSNote     = *Off
085000120312     C                   endif
085100120306     C*
085200120306     C                   ENDSR
085300120306     C/EJECT
085400120306     C************************************************************
085500120306     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
085600120306     C************************************************************
085700120306     C     GESAGG        BEGSR
085800120306     C*
085900120306     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
086000120306     C*    CMT002        IFEQ      '0'
086100120306     C*                  COMMIT
086200120306     C*                  ENDIF
086300120306     C*
086400120306     C* segnalo al pgm chiamante l'avvenuta conferma
086500120306     C                   MOVE      *ON           OPRD21
086600120306     C* segnalo al pgm chiamante l'aver premuto l'F6
086700120306     C                   MOVE      '0'           RETD21
086800120306     C*
086900120306     C                   SELECT
087000120306     C*
087100120306     C* nel caso di immissione
087200120306     C     OPZD21        WHENEQ    '01'
087300120306     C* ripristino la videata iniziale
087400120306     C                   MOVE      *ON           $INZD1
087500120306     C                   MOVE      'D1'          $GEST
087600120306     C*
087700120306     C* nel caso di modifica
087800120306     C     OPZD21        WHENEQ    '02'
087900120306     C* ripasso i campi che devono essere reimpostati sul sfl
088000120306     C                   if        D1DtIni    <> 0
088100120306     C     *JobRun       Move      D1DtIni       DtIniD21
088200120306     C                   endif
088300120306     C                   if        D1DtFin    <> 0
088400120306     C     *JobRun       Move      D1DtFin       DtFinD21
088500120306     C                   endif
088600120306     C                   Z-ADD     D1ImpSogl     ImpSoglD21
088700120306     C* e ritorno al pgm chiamante
088800120306     C                   MOVE      *ON           $FINE
088900120306     C*
089000120306     C* per tutte le altre opzioni
089100120306     C                   OTHER
089200120306     C* ritorno al pgm chiamante
089300120306     C                   MOVE      *ON           $FINE
089400120306     C                   ENDSL
089500120306     C*
089600120306     C                   ENDSR
089700120306     C/EJECT
089800120306     C************************************************************
089900120306     C* CONTROLLO ABILITAZIONE TASTI
090000120306     C************************************************************
090100120306     C     CTRKEY        BEGSR
090200120306     C*
090300120306     C                   SELECT
090400120306     C     $TASTO        WHENEQ    F01
090500120306     C                   Z-ADD     01            PosFun
090600120306     C     $FC(1)        IFEQ      '0'
090700120306     C                   SETON                                        99
090800120306     C                   ENDIF
090900120306     C     $TASTO        WHENEQ    F02
091000120306     C                   Z-ADD     02            PosFun
091100120306     C     $FC(2)        IFEQ      '0'
091200120306     C                   SETON                                        99
091300120306     C                   ENDIF
091400120306     C     $TASTO        WHENEQ    F03
091500120306     C                   Z-ADD     03            PosFun
091600120306     C     $FC(3)        IFEQ      '0'
091700120306     C                   SETON                                        99
091800120306     C                   ENDIF
091900120306     C     $TASTO        WHENEQ    F04
092000120306     C                   Z-ADD     04            PosFun
092100120306     C     $FC(4)        IFEQ      '0'
092200120306     C                   SETON                                        99
092300120306     C                   ENDIF
092400120306     C     $TASTO        WHENEQ    F05
092500120306     C                   Z-ADD     05            PosFun
092600120306     C     $FC(5)        IFEQ      '0'
092700120306     C                   SETON                                        99
092800120306     C                   ENDIF
092900120306     C     $TASTO        WHENEQ    F06
093000120306     C                   Z-ADD     06            PosFun
093100120306     C     $FC(6)        IFEQ      '0'
093200120306     C                   SETON                                        99
093300120306     C                   ENDIF
093400120306     C     $TASTO        WHENEQ    F07
093500120306     C                   Z-ADD     07            PosFun
093600120306     C     $FC(7)        IFEQ      '0'
093700120306     C                   SETON                                        99
093800120306     C                   ENDIF
093900120306     C     $TASTO        WHENEQ    F08
094000120306     C                   Z-ADD     08            PosFun
094100120306     C     $FC(8)        IFEQ      '0'
094200120306     C                   SETON                                        99
094300120306     C                   ENDIF
094400120306     C     $TASTO        WHENEQ    F09
094500120306     C                   Z-ADD     09            PosFun
094600120306     C     $FC(09)       IFEQ      '0'
094700120306     C                   SETON                                        99
094800120306     C                   ENDIF
094900120306     C     $TASTO        WHENEQ    F10
095000120306     C                   Z-ADD     10            PosFun
095100120306     C     $FC(10)       IFEQ      '0'
095200120306     C                   SETON                                        99
095300120306     C                   ENDIF
095400120306     C     $TASTO        WHENEQ    F11
095500120306     C                   Z-ADD     11            PosFun
095600120306     C     $FC(11)       IFEQ      '0'
095700120306     C                   SETON                                        99
095800120306     C                   ENDIF
095900120306     C     $TASTO        WHENEQ    F12
096000120306     C                   Z-ADD     12            PosFun
096100120306     C     $FC(12)       IFEQ      '0'
096200120306     C                   SETON                                        99
096300120306     C                   ENDIF
096400120306     C     $TASTO        WHENEQ    F13
096500120306     C                   Z-ADD     13            PosFun
096600120306     C     $FC(13)       IFEQ      '0'
096700120306     C                   SETON                                        99
096800120306     C                   ENDIF
096900120306     C     $TASTO        WHENEQ    F14
097000120306     C                   Z-ADD     14            PosFun
097100120306     C     $FC(14)       IFEQ      '0'
097200120306     C                   SETON                                        99
097300120306     C                   ENDIF
097400120306     C     $TASTO        WHENEQ    F15
097500120306     C                   Z-ADD     15            PosFun
097600120306     C     $FC(15)       IFEQ      '0'
097700120306     C                   SETON                                        99
097800120306     C                   ENDIF
097900120306     C     $TASTO        WHENEQ    F16
098000120306     C                   Z-ADD     16            PosFun
098100120306     C     $FC(16)       IFEQ      '0'
098200120306     C                   SETON                                        99
098300120306     C                   ENDIF
098400120306     C     $TASTO        WHENEQ    F17
098500120306     C                   Z-ADD     17            PosFun
098600120306     C     $FC(17)       IFEQ      '0'
098700120306     C                   SETON                                        99
098800120306     C                   ENDIF
098900120306     C     $TASTO        WHENEQ    F18
099000120306     C                   Z-ADD     18            PosFun
099100120306     C     $FC(18)       IFEQ      '0'
099200120306     C                   SETON                                        99
099300120306     C                   ENDIF
099400120306     C     $TASTO        WHENEQ    F19
099500120306     C                   Z-ADD     19            PosFun
099600120306     C     $FC(19)       IFEQ      '0'
099700120306     C                   SETON                                        99
099800120306     C                   ENDIF
099900120306     C     $TASTO        WHENEQ    F20
100000120306     C                   Z-ADD     20            PosFun
100100120306     C     $FC(20)       IFEQ      '0'
100200120306     C                   SETON                                        99
100300120306     C                   ENDIF
100400120306     C     $TASTO        WHENEQ    F21
100500120306     C                   Z-ADD     21            PosFun
100600120306     C     $FC(21)       IFEQ      '0'
100700120306     C                   SETON                                        99
100800120306     C                   ENDIF
100900120306     C     $TASTO        WHENEQ    F22
101000120306     C                   Z-ADD     22            PosFun
101100120306     C     $FC(22)       IFEQ      '0'
101200120306     C                   SETON                                        99
101300120306     C                   ENDIF
101400120306     C     $TASTO        WHENEQ    F23
101500120306     C                   Z-ADD     23            PosFun
101600120306     C     $FC(23)       IFEQ      '0'
101700120306     C                   SETON                                        99
101800120306     C                   ENDIF
101900120306     C     $TASTO        WHENEQ    F24
102000120306     C                   Z-ADD     24            PosFun
102100120306     C     $FC(24)       IFEQ      '0'
102200120306     C                   SETON                                        99
102300120306     C                   ENDIF
102400120306     C                   ENDSL
102500120306     C*
102600120306     C                   ENDSR
102700120306     C/EJECT
102800120306     C******************************************************
102900120306     C* PGM PER GESTIONE MESSAGGI ERRORE
103000120306     C******************************************************
103100120306     C     MSGPGM        BEGSR
103200120306     C*
103300120306     C                   Z-ADD     2             MSGMSG
103400120306     C                   MOVEL     'S'           MSGRSP
103500120306     C                   MOVEL     'CR'          MSGTPR
103600120306     C                   MOVEL     'S'           MSGEMV
103700120306     C                   Z-ADD     10            MSGRGP
103800120306     C                   MOVEL     'S'           MSGLCK
103900120306     C                   MOVEA     STATUS        STS
104000120306     C                   MOVEL     'N'           MSGCNL
104100120306     C                   MOVEL     'N'           MSGVID
104200120306     C                   MOVEL     'N'           MSGOPE
104300120306     C                   MOVEL     'N'           MSGSTP
104400120306     C                   CALL      'SDGMSGR'
104500120306     C                   PARM                    SDGMSGDS
104600120306     C*
104700120306     C                   ENDSR
104800120306     C/EJECT
104900120306     C************************************************************
105000120306     C* OPERAZIONI INIZIALI
105100120306     C************************************************************
105200120306     C     *INZSR        BEGSR
105300120306     C*
105400120306     C* Reperimento parametri
105500120306     C*
105600120306     C     *ENTRY        PLIST
105700120306     C                   PARM                    KPJBA
105800120306     C                   PARM                    DSAUT
105900120306     C*
106000120306     C* Reperimento dati società
106100120306     C*
106200120306     C                   MOVEL     'SOC001'      TIPXSC
106300120306     C                   MOVEL     *BLANK        SOCXSC
106400120306     C                   EXSR      REPSOC
106500120306     C*
106600120306     C* Controlli dati società
106700120306     C*
106800120306     C     RTNXSC        IFNE      '1'
106900120306     C                   MOVEL     XSOCDS        SOC001
107000120306     C                   ELSE
107100120306     C                   MOVE      '1'           ErrD21
107200120306     C                   MOVE      *on           $Fine
107300120306     C                   EXSR      EndPgm
107400120306     C                   ENDIF
107500120306     C*
107600120306     C* testo se sono stati passati i dati di autorizzazione
107700120306     C*
107800120306     C     PARMS         IFLT      2
107900120306     C                   MOVE      *BLANK        AUTDS
108000120306     C                   ELSE
108100120306     C                   MOVEL     DSAUT         AUTDS
108200120306     C                   ENDIF
108300120306     C*
108400120306     C* reperisco le dimensioni delle variabili che conterranno i tasti
108500120306     C                   EVAL      $D1L01 = %Size(Z1Ke1)
108600120306     C                   EVAL      $D1L02 = %Size(Z1Ke2)
108700120306      * aggiungere altre variabili nel caso di videate indipendenti
108800120306      * ulteriori a quelle std
108900120306     C*
109000120306     C* Reperimento tasti di funzione
109100120306     C*
109200120306     C                   EXSR      REPKEY
109300120306     C*
109400120306     C* Richiamo routine per aggiunta tasti funzione personalizzati
109500120306     C*
109600120306     C                   CAllB     'X46USREXT'
109700120306     C                   PARM                    *omit
109800120306     C                   PARM                    *omit
109900120306     C                   PARM                    *omit
110000120306     C                   PARM                    *omit
110100120306     C                   PARM                    *omit
110200120306     C                   PARM                    $FX1
110300120306     C                   PARM                    $FC1
110400120306     C                   PARM                    $PGF1
110500120306     C                   PARM                    DsPgm
110600120306     C                   PARM                    xsccut
110700120306     C                   PARM                    xscgrp
110800120306     C*
110900120306     C                   ENDSR
111000120306     C/EJECT
111100120306     C************************************************************
111200120306     C* REPERIMENTO TASTI DI FUNZIONE
111300120306     C************************************************************
111400120306     C     REPKEY        BEGSR
111500120306     C*
111600120306     C                   MOVEA     $FM1          $FM
111700120306     C                   MOVEA     $FC1          $FC
111800120306     C                   EXSR      REPKKK
111900120306     C                   MOVEA     $FX           $FX1
112000120306     C*
112100120306     C                   ENDSR
112200120306     C/EJECT
112300120306     C************************************************************
112400120306     C* REPERIMENTO TASTI DI FUNZIONE
112500120306     C************************************************************
112600120306     C     REPKKK        BEGSR
112700120306     C*
112800120306     C                   CLEAR                   $FX
112900120306     C     1             DO        24            X
113000120306     C*
113100120306     C     $FM(X)        IFNE      *BLANK
113200120306     C     $FC(X)        ANDNE     '0'
113300120306     C*
113400120306     C                   MOVEL     $FM(X)        IDMSG
113500120306     C                   CALLB     'XRTVM'
113600120306     C                   PARM                    IDMSG            27
113700120306     C                   PARM                    TXTMSG          100
113800120306     C                   PARM                    ERRMSG            1
113900120306     C*
114000120306     C     ERRMSG        IFNE      *ON
114100120306     C                   MOVEL     TXTMSG        $FX(X)
114200120306     C                   ELSE
114300120306     C                   MOVEL     *ALL' '       $FX(X)
114400120306     C                   ENDIF
114500120306     C*
114600120306     C                   ENDIF
114700120306     C*
114800120306     C                   ENDDO
114900120306     C*
115000120306     C                   ENDSR
115100120306     C/EJECT
115200120306     C************************************************************
115300120306     C* INIZIALIZZAZIONE VARIABILI
115400120306     C************************************************************
115500120306     C     INZVAR        BEGSR
115600120306     C*
115700120306     C                   MOVEL     KPJBU         SDGSD21DS
115800120306     C*
115900120306     C* Se società diversa da quella impostata nella *inzsr, rieseguo
116000120306     C* reperimento dati società
116100120306     C                   If        SocietaD21 <> xscsoc    and
116200120306     C                             OpzD21 <> *blank
116300120306     C                   MOVEL     'SOC001'      TIPXSC
116400120306     C                   MOVEL     SocietaD21    SOCXSC
116500120306     C                   EXSR      REPSOC
116600120306     C     RTNXSC        IFNE      '1'
116700120306     C                   MOVEL     XSOCDS        SOC001
116800120306     C                   ELSE
116900120306     C                   MOVE      '1'           ErrD21
117000120306     C                   MOVE      *on           $Fine
117100120306     C                   EXSR      EndPgm
117200120306     C                   ENDIF
117300120306     C                   endif
117400120306     C*
117500120306     C*
117600120306     C* Reperimento autorizzazioni
117700120306     C*
117800120306      *  nel caso l'anagrafica non gestisse le autorizzazioni
117900120306      *   asteriscare la chiamata alla subroutine REPAUT
118000120306     C* le autorizzazioni potrebbero essere già state passate,
118100120306     C* nel qual caso non vanno reperite
118200120306     C     AUTDS         IFEQ      *BLANK
118300120306     C                   EXSR      REPAUT
118400120306     C                   ELSE
118500120306     C                   MOVEL     XCADDS        CONCIVDS
118600120306     C                   ENDIF
118700120306     C*
118800120306     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
118900120306     C* le autorizzazione
119000120306     C*
119100120306     C*  nel caso l'anagrafica non gestisse le autorizzazioni
119200120306     C*   asteriscare la chiamata alla subroutine TSTAUT
119300120306     C                   EXSR      TSTAUT
119400120306     C*
119500120306     C* Definizione dei tasti funzione utilizzabili a pgm
119600120306     C*
119700120306     C                   EXSR      DEFOPZ
119800120306     C*
119900120306     C* Variabili per gestione videate
120000120306     C*
120100120306     C                   MOVE      'D1'          $GEST                          fmt in gestione
120200120306     C                   MOVE      *OFF          $FINE                          fine pgm
120300120306     C                   MOVE      *OFF          $FlgDef                        fine pgm
120400120306     C                   MOVE      *ON           $INZD1                         inizializzione D1
120500120306     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
120600120306     C                   MOVE      *BLANK        $LASTV                         ultima emissione
120700120306     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
120800120306     C* azzero la posizione del cursore
120900120306     C                   Z-ADD     0             H1RIGA
121000120306     C                   Z-ADD     0             H1COLO
121100120306     C*
121200120306     C* Variabili appoggio
121300120306     C*
121400120306     C                   Z-ADD     0             CURR
121500120306     C                   Z-ADD     0             CURC
121600120306     C*
121700120306     C* Valorizzazione campi univoci testate
121800120306     C*
121900120306     C                   CLEAR                   FMTT1
122000120306     C                   MOVEL     KNSIF         NOMSIF
122100120306     C                   MOVEL     XSCDSI        NOMDIT
122200120306     C                   MOVEL     DSPGM         NOMPGM           10
122300120306     C                   MOVEL     KCDAZ         NOMAZN
122400120306     C* recupero il sottotitolo (se c'è)
122500120306     C                   MOVEL     *BLANK        T1TIT
122600120306     C                   if        SottitT1   <> *Blank
122700120306     C                   MOVEL     SottitT1      IDMSG
122800120306     C                   CALLB     'XRTVM'
122900120306     C                   PARM                    IDMSG            27
123000120306     C                   PARM                    TXTMSG          100
123100120306     C                   PARM                    ERRMSG            1
123200120306     C                   PARM      *ON           CTRMSG            1            centratura
123300120306     C                   PARM      30            LENMSG            3 0          lun output.
123400120306     C     ERRMSG        IFNE      *ON
123500120306     C                   MOVEL     TXTMSG        T1TIT
123600120306     C                   ELSE
123700120306     C                   MOVEL     *ALL'?'       T1TIT
123800120306     C                   ENDIF
123900120306     C                   ENDIF
124000120306     C* recupero il tipo operazione
124100120306     C                   Z-ADD     1             X
124200120306     C     OPZD21        LOOKUP    $OZ(X)                                 21
124300120306     C     *IN21         IFEQ      *ON
124400120306     C                   MOVEL     $ID(X)        IDMSG
124500120306     C                   CALLB     'XRTVM'
124600120306     C                   PARM                    IDMSG            27
124700120306     C                   PARM                    TXTMSG          100
124800120306     C                   PARM                    ERRMSG            1
124900120306     C                   PARM      *Off          CTRMSG            1            centratura
125000120306     C                   PARM      30            LENMSG            3 0          lun output.
125100120306     C     ERRMSG        IFNE      *ON
125200120306     C                   MOVEL     TXTMSG        T1OPE
125300120306     C                   ELSE
125400120306     C                   MOVEL     *ALL'?'       T1OPE
125500120306     C                   ENDIF
125600120306     C                   ENDIF
125700120306     C*
125800120306     C* Inizializzazione/Decodifica prima videata
125900120306     C*
126000120306     C                   EXSR      INZD1
126100120306     C                   MOVE      *OFF          $INZD1
126200120306     C*
126300120306     C                   ENDSR
126400120306     C/EJECT
126500120306     C************************************************************
126600120306     C* REPERIMENTO DATI SOCIETA'
126700120306     C************************************************************
126800120306     C     REPSOC        BEGSR
126900120306     C*
127000120306     C                   CALLB     'XSOC'
127100120306     C                   PARM                    TIPXSC            6
127200120306     C                   PARM                    SOCXSC            3
127300120306     C                   PARM                    CDSXSC            9 0
127400120306     C                   PARM                    MODXSC            3
127500120306     C                   PARM      *BLANKS       RTNXSC            1
127600120306     C                   PARM                    XSOCDS
127700120306     C                   PARM                    KPJBA
127800120306     C*
127900120306     C                   ENDSR
128000120306     C/EJECT
128100120306     C************************************************************
128200120306     C* REPERIMENTO AUTORIZZAZIONI UTENTE
128300120306     C************************************************************
128400120306     C     REPAUT        BEGSR
128500120306     C*
128600120306     C                   CLEAR                   AUTDS
128700120306     C*
128800120306     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
128900120306     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
129000120306     C                   Z-ADD     0             XCAGRP                         GRUPPO
129100120306     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
129200120306     C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
129300120306     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
129400120306     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
129500120306     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
129600120306     C*
129700120306     C                   CALLB     'XCHKAUT'
129800120306     C                   PARM                    AUTDS
129900120306     C*
130000120306     C* errori/non abilitazione
130100120306     C     XCARTN        IFNE      0
130200120306     C                   MOVE      '2'           ErrD21
130300120306     C                   MOVE      *ON           $FINE
130400120306     C                   EXSR      ENDPGM
130500120306     C                   ELSE
130600120306     C                   MOVEL     XCADDS        CONCIVDS
130700120306     C                   ENDIF
130800120306     C*
130900120306     C                   ENDSR
131000120306     C/EJECT
131100120306     C************************************************************
131200120306     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
131300120306      * in questo esempio, l'anagrafica possiede un flag di
131400120306      * autorizzazione per ognuna delle opzioni std
131500120306     C************************************************************
131600120306     C     TSTAUT        BEGSR
131700120306     C*
131800120306     C                   SELECT
131900120306     C*
132000120306     C* se richiamato in immissione
132100120306     C     OPZD21        WHENEQ    '01'
132200120306     C* ma l'utente non è abilitato
132300120306     C     IMMALL        ANDEQ     *OFF
132400120306     C* esco da pgm avvertendolo
132500120306     C                   MOVE      '2'           ERRD21
132600120306     C                   MOVE      *ON           $FINE
132700120306     C                   EXSR      ENDPGM
132800120306     C*
132900120306     C* se richiamato in modifica
133000120306     C     OPZD21        WHENEQ    '02'
133100120306     C* ma l'utente non è abilitato
133200120306     C     MANALL        ANDEQ     *OFF
133300120306     C* testo se l'utente è abilitato almeno alla visualizzazione
133400120306     C     VISALL        IFEQ      *ON
133500120306     C                   MOVE      '05'          OPZD21
133600120306     C                   ELSE
133700120306     C* altrimenti esco da pgm avvertendolo
133800120306     C                   MOVE      '2'           ERRD21
133900120306     C                   MOVE      *ON           $FINE
134000120306     C                   EXSR      ENDPGM
134100120306     C                   ENDIF
134200120306     C*
134300120306     C* se richiamato in copia
134400120306     C     OPZD21        WHENEQ    '03'
134500120306     C* ma l'utente non è abilitato
134600120306     C     IMMALL        ANDEQ     *OFF
134700120306     C* esco da pgm avvertendolo
134800120306     C                   MOVE      '2'           ERRD21
134900120306     C                   MOVE      *ON           $FINE
135000120306     C                   EXSR      ENDPGM
135100120306     C*
135200120306     C* se richiamato in annullamento
135300120306     C     OPZD21        WHENEQ    '04'
135400120306     C* ma l'utente non è abilitato
135500120306     C     ANNALL        ANDEQ     *OFF
135600120306     C* esco da pgm avvertendolo
135700120306     C                   MOVE      '2'           ERRD21
135800120306     C                   MOVE      *ON           $FINE
135900120306     C                   EXSR      ENDPGM
136000120306     C*
136100120306     C* se richiamato in visualizzazione
136200120306     C     OPZD21        WHENEQ    '05'
136300120306     C* ma l'utente non è abilitato
136400120306     C     VISALL        ANDEQ     *OFF
136500120306     C* esco da pgm avvertendolo
136600120306     C                   MOVE      '2'           ERRD21
136700120306     C                   MOVE      *ON           $FINE
136800120306     C                   EXSR      ENDPGM
136900120306     C*
137000120306     C                   ENDSL
137100120306     C*
137200120306     C                   ENDSR
137300120306     C/EJECT
137400120306     C************************************************************
137500120306     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
137600120306     C************************************************************
137700120306     C     DEFOPZ        BEGSR
137800120306     C*
137900120306     C* Questa routine deve valorizzare la schiera $FC1/$FR1
138000120306     C* con S o N a seconda della utilizzabilità o meno
138100120306     C* del tasto funzione correlato
138200120306     C*
138300120306     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
138400120306     C* di compilazione
138500120306     C                   RESET                   $FC1
138600120306     C                   RESET                   $FR1
138700120306     C*
138800120306     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
138900120306     C* invalido alcune opzioni
139000120306     C                   SELECT
139100120306     C* Se pgm richiamato in interrogazione disattivo :
139200120306     C     OPZD21        WHENEQ    '05'
139300120306     C* F2=Decodifica
139400120306     C                   Z-ADD     2             X
139500120306     C                   MOVE      '0'           $FC1(X)
139600120306     C* F4=Ricerca
139700120306     C                   Z-ADD     4             X
139800120306     C                   MOVE      '0'           $FC1(X)
139900120306     C* F5=Ripristino
140000120306     C                   Z-ADD     5             X
140100120306     C                   MOVE      '0'           $FC1(X)
140200120306     C* F6=Conferma
140300120306     C                   Z-ADD     06            X
140400120306     C                   MOVE      '0'           $FC1(X)
140500120306     C* Se pgm richiamato in annullamento disattivo :
140600120306     C     OPZD21        WHENEQ    '04'
140700120306     C* F2=Decodifica
140800120306     C                   Z-ADD     2             X
140900120306     C                   MOVE      '0'           $FC1(X)
141000120306     C* F4=Ricerca
141100120306     C                   Z-ADD     4             X
141200120306     C                   MOVE      '0'           $FC1(X)
141300120306     C* F5=Ripristino
141400120306     C                   Z-ADD     5             X
141500120306     C                   MOVE      '0'           $FC1(X)
141600120306     C*
141700120306     C                   OTHER
141800120306     C*
141900120306     C                   ENDSL
142000120306     C*
142100120306     C                   ENDSR
142200120306     C/EJECT
142300120306     C************************************************************
142400120306     C* REPERIMENTO CAMPI DI DEFAULT
142500120306     C************************************************************
142600120306     C     REPDEF        BEGSR
142700120306     C*
142800120306      * riempire dei campi di appoggio con tutti quei valori
142900120306      * che possono essere usati come default
143000120306     C*
143100120306     C                   ENDSR
143200120306     C************************************************************
143300120306     C* DEFINIZIONE KLIST
143400120306     C************************************************************
143500120306     C     DefKlist      BEGSR
143600120306     C*
143700120306     C* SDGBLS01L
143800120306     C     K02BLS01      KLIST
143900120306     C                   KFLD                    BLSSOCIETA
144000120306     C                   KFLD                    BLSDTINI
144100120306     C*
144200120306     C                   ENDSR
144300120306     C/EJECT
144400120306     C************************************************************
144500120306     C* RIEMPIMENTO TASTI FUNZIONE
144600120306     C************************************************************
144700120306     C     RIEKEY        BEGSR
144800120306     C*
144900120306     C* Input :
145000120306     C* - $FX = schiera con tutti i testi disponibili
145100120306     C* - $FC = schiere con i flag di validità associati ai testi
145200120306     C* - $FR = schiere con i flag di REVERSE IMAGE
145300120306     C* - $ULK = ultimo testo caricato
145400120306     C* - $L01 = Lunghezza prima riga
145500120306     C* - $L02 = Lunghezza seconda riga
145600120306     C* Output :
145700120306     C* - $ULK
145800120306     C* - $KEY1= 1° riga di tasti funzione
145900120306     C* - $KEY2= 2° riga di tasti funzione
146000120306     C*
146100120306     C                   CALLB     'XKEYMSG'
146200120306     C* input
146300120306     C                   PARM                    $FX
146400120306     C                   PARM                    $FC
146500120306     C                   PARM                    $FR
146600120306     C                   PARM                    $ULK              3 0
146700120306     C* output
146800120306     C                   PARM                    $KEY1            79
146900120306     C                   PARM                    $KEY2            79
147000120306     C* input
147100120306     C                   PARM                    $L01              3 0
147200120306     C                   PARM                    $L02              3 0
147300120306     C*
147400120306     C                   PARM                    $ALLFUNCT       480
147500120306     C*
147600120306     C                   ENDSR
147700120306** $OZ / $ID
14780012030601SDGMSG    *LIBL     SDC0002
14790012030602PROMSG    *LIBL     COS0002
14800012030603PROMSG    *LIBL     COS0003
14810012030604PROMSG    *LIBL     COS0004
14820012030605PROMSG    *LIBL     COS0005
148300120306** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
148400120306PROMSG    *LIBL     KEY0001  01
148500120306PROMSG    *LIBL     KEY0002  02
148600120306PROMSG    *LIBL     KEY0003  03
148700120306PROMSG    *LIBL     KEY0004  04
148800120306PROMSG    *LIBL     KEY0005  05
148900120306PROMSG    *LIBL     KEY0006  06
149000120306PROMSG    *LIBL     KEY0007  07
149100120306PROMSG    *LIBL     KEY0008  08
149200120306PROMSG    *LIBL     KEY0009  09
149300120306PROMSG    *LIBL     KEY0010  10
149400120306PROMSG    *LIBL     KEY0011  11
149500120306PROMSG    *LIBL     KEY0012  12
149600120306PROMSG    *LIBL     KEY0013  13
149700120306PROMSG    *LIBL     KEY0014  14
149800120306PROMSG    *LIBL     KEY0015  15
149900120306PROMSG    *LIBL     KEY0016  16
150000120306PROMSG    *LIBL     KEY0017  17
150100120306PROMSG    *LIBL     KEY0018  18
150200120306PROMSG    *LIBL     KEY0019  19
150300120306PROMSG    *LIBL     KEY0020  20
150400120306PROMSG    *LIBL     KEY0021  21
150500120306PROMSG    *LIBL     KEY0022  22
150600120306PROMSG    *LIBL     KEY0023  23
150700120306PROMSG    *LIBL     KEY0024  24
150800120306**  ABILITAZIONE D1 : $FC1 / $FR1
1509001203060N  01
1510001203060N  02
1511001203061N  03
1512001203061N  04
1513001203061N  05
1514001203061N  06
1515001203060N  07
1516001203061N  08
1517001203060N  09
1518001203060N  10
1519001203060N  11
1520001203061N  12
1521001203060N  13
1522001203060N  14
1523001203060N  15
1524001203060N  16
1525001203060N  17
1526001203060N  18
1527001203060N  19
1528001203060N  20
1529001203060N  21
1530001203060N  22
1531001203060N  23   Impostare a '1' se si utilizzano opzioni
1532001203061N  24   Lasciare sempre impostata a '1' il valore della riga 24
