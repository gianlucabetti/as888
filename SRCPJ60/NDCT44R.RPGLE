000100981016     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(QILE)
000200981016     H*PARMS OPTION(*NOXREF) CVTOPT(*NONE)
000300970623     H DECEDIT('0,') DATEDIT(*DMY.)
000400970623      *
000500970623    > *            LANCIO RICONCILIAZIONE AUTOMATICA
000600970623      *           --------------------------------------
000700970623      *
000800970623      *
000900970623      * DOCUMENTI MICROANALISI :
001000970623      *
001100970623      *               MAPPA INDICATORI STD
001200970623      *
001300970623      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
001400970623      *  21           GENERICO OPERAZIONI I/O
001500970623      *  22           GENERICO ERRORE OPERAZIONI I/O
001600970623      *  30           SFLDSP
001700970623      * N31           SFLCLR
001800970623      *  31           SFLDSPCTL
001900970623      *  32           SFLNXTCHG
002000970623      *  33           SFLEND
002100970623      *  39           OF PRTF
002200970623      *  40 <---> 49  DSPATR ERRORI SU SFL
002300970623    > *  Specificare l'uso dei singoli indicatori
002400970623      *  50 <---> 98  ERRORI SU VIDEO
002500970623    > *  Specificare l'uso dei singoli indicatori
002600970623      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002700970623      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002800970623      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002900970623      *  99           INDIC. GENERALE DI ERRORE
003000970623     F*----------------------------------------------------*
003100970623    >FNDCT44D   CF   E             WORKSTN
003200970623     F                                     RENAME(CT44D1:FMTD1)
003300970623     F                                     RENAME(CT44T1:FMTT1)
003400970623     F                                     RENAME(CT44Z1:FMTZ1)
003500030403C1630F* modifica valori assunti
003600030403  "  F                                     RENAME(CT44D3:FMTD3)
003700030403C1630F                                     RENAME(CT44Z3:FMTZ3)
003800970623     F                                     INFDS(DSFMT)
003900970623     F*------------
004000970623    >FNdbmb03L  IF   E           K DISK
004100970623     D*----------------------------------------------------*
004200970623     D* Schiere per la gestione dei tasti di funzione
004300970623     D $FM             S             27    DIM(24)
004400970623     D $FX             S             50    DIM(24)
004500970623     D $FC             S              1    DIM(24)
004600970623     D $FR             S              1    DIM(24)
004700970623     D* Video D1/D2
004800970623     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004900970623     D $FX1            S             50    DIM(24) ALT($FM1)
005000970623     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005100970623     D $FR1            S              1    DIM(24) ALT($FC1)
005200030403C1630D* Video D3 Valori assunti
005300030403  "  D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
005400030403  "  D $FX3            S             50    DIM(24) ALT($FM3)
005500030403  "  D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
005600030403C1630D $FR3            S              1    DIM(24) ALT($FC3)
005700970623     D*----------------------------------------------------*
005800970623     D*-------------
005900970623     D* Passaggio Parametri
006000970623     D KPJBA         E DS
006100970623     D*-------------
006200970623    >D* Reperimento Autorizzazioni
006300970623    >D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006400970623     D*-------------
006500970623     D* Dati per pgm XNOAUT
006600970623     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
006700970623     D*-------------
006800970623     D* Passaggio Autorizzazioni
006900970623     D DSAUT           DS           512
007000970623     D*-------------
007100970623     D* Maschera dati autorizzazioni
007200970623    >Dcontesds       E DS
007300970623     D*-------------
007400970623     D* Reperimento nome PGM
007500970623     D STATUS         SDS           333
007600970623     D  DSPGM            *PROC
007700970623     D  PARMS            *PARMS
007800970623     D*-------------
007900970623     D* Dati di ambiente ottenuti da XSOC
008000970623    >D SOC001        E DS                  EXTNAME(XSOC001DS)
008100970623     D*-------------
008200970623     D* DS Interna per dati di output di XSOC
008300970623     D XSOCDS          DS          1000
008400030403C1630D*-------------
008500030403  "  D* Valori assunti
008600030403  "  D XMPEDS        E DS                  Inz
008700030403  "  D XMPEDS1       E DS                  Inz
008800030403  "  D DSMPE           DS                  Inz
008900030403  "  D  $VAL                          1
009000030617  "  D  $Banca                             Like(D1Banca)
009100030617  "  D  $Linea                             Like(D1Linea)
009200030617  "  D  $DtOpA                             Like(D1DtOpA)
009300030403  "  D  $Interatt                          Like(D1Interatt)
009400030403  "  D  $Giorni                            Like(D1Giorni)
009500030403C1630D  $RicNN                             Like(D1RicNN)
009600970623     D*-------------
009700970623     D* Passaggio dati in entrata
009800970623    >D NDCT44DS      E DS
009900970623     D*-------------
010000970623     D* Passaggio dati al pgm di elaborazione
010100970701    >D NDCT45DS      E DS                  inz
010200970623     D*-----------
010300970623     D* Passaggio dati per interrogazione sintetica
010400970701    >D NDCT46DS      E DS                  inz
010500970623     D*-----------
010600970623     D* DS controllo e decodifica Banca + Linea
010700970623     D Nd016Ds       E DS                  Inz
010800970623     D*-----------
010900970623     D* Definizioni parametri standard chiamate moduli
011000970623     D Esito           S              1
011100970623     D LenOut          S              9B 0
011200970623     D OutPut          S           4000
011300970623     D StrutturaO      S             10
011400970623     D Operazione      S              2
011500970623     D Societa         S              3
011600970623     D DtopDa          S               D
011700970623     D DtopA           S               D
011800970623     D*-------------
011900970623     D DSFMT           DS
012000970623     D  $TASTO               369    369
012100970623     D  NRG                  370    370
012200970623     D  NCL                  371    371
012300970623     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
012400970623     D*-------------
012500970623     D* posizione cursore
012600970623     D CURSOR          DS
012700970623     D  RIRI                   1      2B 0 INZ
012800970623     D  R$$                    2      2
012900970623     D  COCO                   3      4B 0 INZ
013000970623     D  C$$                    4      4
013100970623     D*-------------
013200970623     D* Tasti di funzione
013300970623     D F01             C                   CONST(X'31')
013400970623     D F02             C                   CONST(X'32')
013500970623     D F03             C                   CONST(X'33')
013600970623     D F04             C                   CONST(X'34')
013700970623     D F05             C                   CONST(X'35')
013800970623     D F06             C                   CONST(X'36')
013900970623     D F07             C                   CONST(X'37')
014000970623     D F08             C                   CONST(X'38')
014100970623     D F09             C                   CONST(X'39')
014200970623     D F10             C                   CONST(X'3A')
014300970623     D F11             C                   CONST(X'3B')
014400970623     D F12             C                   CONST(X'3C')
014500970623     D F13             C                   CONST(X'B1')
014600970623     D F14             C                   CONST(X'B2')
014700970623     D F15             C                   CONST(X'B3')
014800970623     D F16             C                   CONST(X'B4')
014900970623     D F17             C                   CONST(X'B5')
015000970623     D F18             C                   CONST(X'B6')
015100970623     D F19             C                   CONST(X'B7')
015200970623     D F20             C                   CONST(X'B8')
015300970623     D F21             C                   CONST(X'B9')
015400970623     D F22             C                   CONST(X'BA')
015500970623     D F23             C                   CONST(X'BB')
015600970623     D F24             C                   CONST(X'BC')
015700970623     D ENTER           C                   CONST(X'F1')
015800970623     D ROLDWN          C                   CONST(X'F4')
015900970623     D ROLLUP          C                   CONST(X'F5')
016000970721     D* ------ Definizioni per Gestione messaggi
016100970721     DLENDTA           S              9B 0
016200970721     DCALLSTACK        S              9B 0
016300970721     DERRCOD           DS
016400970721     D  ERRLEN                        9B 0 INZ(16)
016500970721     D  LENRET                        9B 0 INZ(16)
016600970721     D  ERRID                         7
016700970721     DMSGFIL           S             20    INZ('PROMSG    *LIBL     ')
016800970721    >D PROMSG          C                    Const('PROMSG    *LIBL     ')
016900970721     DEsical           S              1A   INZ(*OFF)
017000970721     DDtamsg           S            256A   INZ(*blank)
017100970721     D*-------------
017200970721     D* Sottotitolo di testata
017300970623     D SOTTIT          C                   CONST('                            ')
017400970721     D*-------------
017500000117C1309D ZDateISO        S               D
017600970623     C*----------------------------------------------------*
017700970623     C*                MAIN LINE PROGRAM
017800970623     C*----------------------------------------------------*
017900970623     C*
018000970623     C* inizializzazione variabili
018100970623     C                   EXSR      INZVAR
018200970623     C*
018300970623     C     $FINE         DOWEQ     *OFF
018400970623     C     $GEST         CASEQ     'D1'          GESD1
018500030403C1630C     $GEST         CASEQ     'D3'          GESD3
018600970623     C                   ENDCS
018700970623     C                   ENDDO
018800970623     C*
018900970623     C* fine programma
019000970623     C                   EXSR      ENDPGM
019100970623     C/EJECT
019200970623     C************************************************************
019300970623     C* RICERCHE
019400970623     C************************************************************
019500970623     C     ENDPGM        BEGSR
019600970623     C*
019700970721     c                   movel     ndcT44ds      kpjbu
019800970623     C*
019900970623     C* se ho confermato l'uscita proseguo
020000970623     C     $FINE         IFEQ      *ON
020100970623     C*
020200970623     C                   SETON                                            LR
020300970623     C                   RETURN
020400970623     C*
020500970623     C                   ENDIF
020600970623     C*
020700970623     C                   ENDSR
020800970623     C/EJECT
020900970623     C************************************************************
021000970623     C* RICERCHE
021100970623     C************************************************************
021200970623     C     SEARCH        BEGSR
021300970623     C*
021400970623     C* determino Riga/Colonna del cursore
021500970623     C                   MOVE      NRG           R$$
021600970623     C                   MOVE      NCL           C$$
021700970623     C                   Z-ADD     RIRI          CURR
021800970623     C                   Z-ADD     COCO          CURC
021900970623     C*
022000970623     C* I campi che al momento dell'F4 risultano protetti non devono
022100970623     C* essere abilitati alla funzione di ricerca
022200970623     C*
022300970623     C                   SELECT
022400970623     C*
022500970623    > * FMTD1 - Banca / linea
022600970623    >C     H1NMFL        WHENEQ    'D1BANCA'
022700970623     C     H1NMFL        OREQ      'D1LINEA'
022800970623     C                   RESET                   ndcT46Ds
022900970623     C                   MOVE      '01'          OpzT46
023000970623     C                   MOVEL     D1Banca       BancaT46
023100970623     C                   MOVEL     D1Linea       LineaT46
023200970623     c                   if        d1dtopa <> 0
023300000117C1309c******dmy          move      d1dtopa       dopaT46
023400000117C1309c     *JobRun       move      d1dtopa       dopaT46
023500970623     c                   endif
023600970623     c                   eval      riconcT46 = '2'
023700970623     C                   Clear                   KPJBU
023800970623     C                   MOVEL     ndcT46Ds      KPJBU
023900970623     C                   CALL      'NDCT46R'                            21
024000970623     C                   PARM                    KPJBA
024100970623     C                   MOVEL     KPJBU         ndcT46Ds
024200970623     c                   if        BancaT46 <> *blanks
024300970623     C                   MOVEL     BancaT46      D1Banca
024400970623     c                   endif
024500970623     c                   if        LineaT46 <> *blanks
024600970623     C                   MOVEL     LineaT46      D1Linea
024700970623     c                   endif
024800970623     C*
024900970623     C* ricerca non abilitata in questa posizione
025000970623     C                   OTHER
025100970623     C* imposto pos. del cursore
025200970623     C                   Z-ADD     CURR          H1RIGA
025300970623     C                   Z-ADD     CURC          H1COLO
025400970623     C                   SETON                                        9899
025500970623     C*
025600970623     C                   ENDSL
025700970623     C*
025800970623     C                   MOVE      *HIVAL        $LASTV
025900970623     C                   ENDSR
026000970623     C/EJECT
026100970623     C************************************************************
026200970623     C* GESTIONE EMISSIONE TASTI FUNZIONE
026300970623     C************************************************************
026400970623     C     GESPIE        BEGSR
026500970623     C*
026600970623     C                   SELECT
026700970623     C*
026800970623     C     $GEST         WHENEQ    'D1'
026900970623     C     $ULKD1        IFEQ      0
027000970623     C* riempimento opzioni
027100970623     C                   EXSR      F24D1
027200970623     C                   ELSE
027300970623     C                   WRITE     FMTZ1
027400970623     C                   ENDIF
027500030403C1630c*
027600030403  "  C* Modifica valori assunti
027700030403  "  C     $GEST         WHENEQ    'D3'
027800030403  "  C     $ULKD3        IFEQ      0
027900030403  "  C                   EXSR      F24D3
028000030403  "  C                   ELSE
028100030403  "  C                   WRITE     FMTZ3
028200030403C1630C                   ENDIF
028300970623     C*
028400970623     C                   ENDSL
028500970623     C*
028600970623     C                   ENDSR
028700970623     C/EJECT
028800970623     C************************************************************
028900970623     C* GESTIONE VIDEO RECORD D1
029000970623     C************************************************************
029100970623     C     GESD1         BEGSR
029200970623     C* inizializzazione videata
029300970623   B1C     $INZD1        IFEQ      *ON
029400970623     C                   EXSR      INZD1
029500970623     C                   MOVE      *OFF          $INZD1
029600970623   E1C                   ENDIF
029700970623     C* emissione piede videata se proveniente da altra
029800970623     C* salvataggio valore formato di provenienza
029900970623   B1C     $LASTV        IFNE      'D1'
030000970623     C                   WRITE     FMTT1
030100970623     C                   EXSR      GESPIE
030200970623     C                   MOVE      $LASTV        $LASTG
030300970623     C                   MOVE      'D1'          $LASTV
030400970623   E1C                   ENDIF
030500970623     C*
030600970623     C* se esistono errori sulla videata
030700970623     C* emetto la write del formato a indicatori spenti per vedere
030800970623     C* le eventuali decodifiche
030900970623   B1C     *IN99         IFEQ      *ON
031000970623     C     $GEST         ANDEQ     'D1'
031100970623     C                   MOVEA     *IN           WIN
031200970623     C                   MOVE      *ALL'0'       IN5098           49
031300970623     C                   MOVEA     IN5098        *IN(50)
031400970623     C                   WRITE     FMTD1
031500970623     C                   MOVEA     WIN           *IN
031600970623   E1C                   ENDIF
031700970623     C*
031800970623     C                   EXFMT     FMTD1
031900970623     C* controllo abilitazione tasti funzione
032000970623     C*
032100970623     C                   EXSR      CTRKD1
032200970623     C*
032300970623   B1C     *IN99         IFEQ      *OFF
032400970623     C*
032500970623   B2C                   SELECT
032600970623     C* F2=Decodifica
032700970623     C     $TASTO        WHENEQ    F02
032800970623     C                   EXSR      F02D1
032900970623     C* F3=Fine
033000970623     C     $TASTO        WHENEQ    F03
033100970623     C                   EXSR      F03D1
033200970623     C* F4=Ricerca
033300970623     C     $TASTO        WHENEQ    F04
033400970623     C                   EXSR      SEARCH
033500970623     C* F5=Refresh
033600970623     C     $TASTO        WHENEQ    F05
033700970623     C                   EXSR      F05D1
033800970623     C* F8=Successivo
033900970623     C     $TASTO        WHENEQ    F08
034000970623     C                   EXSR      F08D1
034100970623     C* F12=Ritorno
034200970623     C     $TASTO        WHENEQ    F12
034300970623     C                   EXSR      F12D1
034400970623     C* F18=Valori assunti
034500970623     C     $TASTO        WHENEQ    F18
034600970623     C                   EXSR      F18D1
034700970623     C* F21=Pers. batch
034800970623     C     $TASTO        WHENEQ    F21
034900970623     C                   EXSR      F21D1
035000970623     C* F24=Altri tasti
035100970623     C     $TASTO        WHENEQ    F24
035200970623     C                   EXSR      F24D1
035300970623     C*
035400970623   X2C                   OTHER
035500970623     C                   EXSR      CTRD1
035600970623     C*
035700970623   B3C     $TASTO        IFEQ      F06
035800970623     C     *IN99         ANDEQ     *OFF
035900970623     C* eseguo lancio
036000970623     C                   EXSR      EXELAN
036100970623     C                   EXSR      Endpgm
036200970623   E3C                   ENDIF
036300970623     C*
036400970623   E2C                   ENDSL
036500970623     C*
036600970623   E1C                   ENDIF
036700970623     C*
036800970623     C                   ENDSR
036900970623     C/EJECT
037000970623     C************************************************************
037100970623     C* INIZIALIZZAZIONE VIDEATA DATI
037200970623     C************************************************************
037300970623     C     INZD1         BEGSR
037400970623     C*
037500970623     C                   CLEAR                   FMTD1
037600970623     C                   MOVE      XSCON         D1ON1
037700970623     C                   MOVE      XSCOFF        D1OFF1
037800030303C1630C                   MOVE      XSCON         D1ON2
037900030303C1630C                   MOVE      XSCOFF        D1OFF2
038000970623     C*
038100970623     C                   Z-ADD     1             $PIED1
038200970623     C* Se richiamato, inposto la ds                                    da anmpe
038300970623     C     opzT44        IFne      *blanks
038400970623     C* riempio la videata da parametri memorizzati
038500970623     C                   EXSR      RIED1
038600030403C1630C* se il pgm NON è stato richiamato
038700030403  "  C                   else
038800030403  "  C* se richiesta impostazione parametri da ultima elaborazione,
038900030403  "  C* li reperisco da AnMpe
039000030403  "  C                   if        $Val     =     *On
039100030403  "  C                   EXSR      RIED1MPE
039200030403  "  C*
039300030403C1630C* altrimenti propongo i valori di default
039400970623     C                   ELSE
039500970623     C* Immettere i valodi di default a video
039600130618>>>>>C                   MOVE      XSCOFF        D1interatt
039700130618>>>>>C                   MOVE      XSCON         D1ricnn
039800000117C1309c*****              move      udate         d1dtopa
039900000117C1309c                   move      ZDate         d1dtopa
040000030403C1630C                   ENDIF
040100970623     C                   ENDIF
040200970623     C*
040300970623     C* richiamo routine dei ctrl per decodificarla
040400970623     C                   EXSR      CTRD1
040500970623     C                   MOVE      *ALL'0'       IN5098
040600970623     C                   MOVEA     IN5098        *IN(50)
040700970623     C*
040800970623     C                   ENDSR
040900970623     C/EJECT
041000970623     C************************************************************
041100970623     C* RIEMPIMENTO VIDEATA  D1
041200970623     C************************************************************
041300970623     C     RIED1         BEGSR
041400970623     C*
041500970623     C* impostare i campi della videata dai parametri memorizzati su anmpe
041600970623     C*
041700970623     c                   if        interatT44 = *on
041800970623     C                   MOVE      xscon         D1interatt
041900970623     c                   else
042000970623     C                   MOVE      xscoff        D1interatt
042100970623     c                   endif
042200030303C1630C                   if        ricnnT44 = *on
042300030303  "  C                   MOVE      xscon         D1ricnn
042400030303  "  C                   else
042500030303  "  C                   MOVE      xscoff        D1ricnn
042600030303  "  C                   endif
042700030303C1630C                   Z-ADD     GiorniT44     D1Giorni
042800970623     C                   MOVE      BancaT44      D1banca
042900970623     C                   MOVE      LineaT44      D1linea
043000970625     c                   if        dtopaT44  <> *hival
043100970625     c                             and dtopat44 <> *loval
043200000117C1309C******DMY          move      dtopaT44      d1dtopa
043300000117C1309C     *JobRun       move      dtopaT44      d1dtopa
043400970625     c                   else
043500000117C1309c*****              move      udate         d1dtopa
043600000117C1309c                   move      ZDate         d1dtopa
043700970623     c                   endif
043800970623     C*
043900970623     C                   ENDSR
044000970623     C/EJECT
044100030403C1630C************************************************************
044200030403  "  C* RIEMPIMENTO VIDEATA  D1 DA VALORI ASSUNTI
044300030403  "  C************************************************************
044400030403C1630C     RIED1MPE      BEGSR
044500030403     C*
044600030403     C* imposto i campi della videata dai parametri memorizzati su AnMPE
044700030403     C*
044800030617     C                   Movel     $Banca        D1Banca
044900030617     C                   Movel     $Linea        D1Linea
045000030617     C                   Movel     $DtOpA        D1DtOpA
045100030403     C     $Interatt     IfEq      *On
045200030403     C                   Move      XScOn         D1Interatt
045300030403     C                   Else
045400030403     C                   Move      XScOff        D1Interatt
045500030403     C                   EndIf
045600030403     C                   Z-Add     $Giorni       D1Giorni
045700030403     C     $RicNN        IfEq      *On
045800030403     C                   Move      XScOn         D1RicNN
045900030403     C                   Else
046000030403     C                   Move      XScOff        D1RicNN
046100030403     C                   EndIf
046200030403     C*
046300030403C1630C                   ENDSR
046400030403C1630C/EJECT
046500970623     C************************************************************
046600970623     C* GESTIONE F02 VIDEO D1
046700970623     C************************************************************
046800970623     C     F02D1         BEGSR
046900970623     C*
047000970623     C* per decodificare la videata richiamo la routine dei ctrl
047100970623     C* e poi disattivo gli eventuali messaggi di errore
047200970623     C                   EXSR      CTRD1
047300970623     C*
047400970623     C                   MOVE      *ALL'0'       IN5098
047500970623     C                   MOVEA     IN5098        *IN(50)
047600970623     C*
047700970623     C                   ENDSR
047800970623     C/EJECT
047900970623     C************************************************************
048000970623     C* GESTIONE F03 VIDEO D1
048100970623     C************************************************************
048200970623     C     F03D1         BEGSR
048300970623     C*
048400970623     C                   MOVE      *ON           $FINE
048500970623    >C                   MOVE      '1'           RETT44
048600970623     C* fine programma
048700970623     C                   EXSR      ENDPGM
048800970623     C*
048900970623     C                   ENDSR
049000970623     C/EJECT
049100970623     C************************************************************
049200970623     C* GESTIONE F05 VIDEO D1
049300970623     C************************************************************
049400970623     C     F05D1         BEGSR
049500970623     C*
049600970623     C                   MOVE      *ON           $INZD1
049700970623     C*
049800970623     C                   ENDSR
049900970623     C/EJECT
050000970623     C************************************************************
050100970623     C* GESTIONE F08 VIDEO D1
050200970623     C************************************************************
050300970623     C     F08D1         BEGSR
050400970623     C*
050500970623     C                   MOVE      *ON           $FINE
050600970623    >C                   MOVE      '0'           RETT44
050700970623     C*
050800970623     C                   ENDSR
050900970623     C/EJECT
051000970623     C************************************************************
051100970623     C* GESTIONE F12 VIDEO D1
051200970623     C************************************************************
051300970623     C     F12D1         BEGSR
051400970623     C*
051500970623     C                   MOVE      *ON           $FINE
051600970623    >C                   MOVE      '2'           RETT44
051700970623     C*
051800970623     C                   ENDSR
051900970623     C/EJECT
052000970623     C************************************************************
052100970623     C* GESTIONE F18 VIDEO D1
052200970623     C************************************************************
052300970623     C     F18D1         BEGSR
052400970623     C*
052500970623     C                   MOVE      'D3'          $GEST
052600970623     C                   MOVE      *ON           $INZD3
052700970623     C*
052800970623     C                   ENDSR
052900970623     C/EJECT
053000970623     C************************************************************
053100970623     C* GESTIONE F21 VIDEO D1
053200970623     C************************************************************
053300970623     C     F21D1         BEGSR
053400970623     C*
053500970623     C                   MOVE      'CT45'        KCOAZ
053600970623     C                   CALL      'BCH09'
053700970623     C                   PARM                    KPJBA
053800970623     C*
053900970623     C                   ENDSR
054000970623     C/EJECT
054100970623     C************************************************************
054200970623     C* GESTIONE F24 VIDEO D1
054300970623     C************************************************************
054400970623     C     F24D1         BEGSR
054500970623     C*
054600970623     C                   MOVEA     $FX1          $FX
054700970623     C                   MOVEA     $FC1          $FC
054800970623     C                   MOVEA     $FR1          $FR
054900970623     C                   Z-ADD     $ULKD1        $ULK
055000970623     C                   Z-ADD     79            $L01
055100970623     C                   Z-ADD     79            $L02
055200970623     C                   EXSR      RIEKEY
055300970623     C                   Z-ADD     $ULK          $ULKD1            3 0
055400970623     C                   MOVEA     $FC           $FC1
055500970623     C                   MOVEL     $KEY1         Z1KE1
055600060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
055700970623     C                   MOVEL     $KEY2         Z1KE2
055800970623     C                   WRITE     FMTZ1
055900970623     C*
056000970623     C                   ENDSR
056100970623     C/EJECT
056200030403C1630C************************************************************
056300030403  "  C* GESTIONE F12 VIDEO D3
056400030403  "  C************************************************************
056500030403C1630C     F12D3         BEGSR
056600030403     C*
056700030403     C                   MOVE      $LASTG        $GEST
056800030403     C*
056900030403C1630C                   ENDSR
057000030403C1630C/EJECT
057100030403C1630C************************************************************
057200030403  "  C* GESTIONE F24 VIDEO D3
057300030403  "  C************************************************************
057400030403C1630C     F24D3         BEGSR
057500030403     C*
057600030403     C                   MOVEA     $FX3          $FX
057700030403     C                   MOVEA     $FC3          $FC
057800030403     C                   MOVEA     $FR3          $FR
057900030403     C                   Z-ADD     $ULKD3        $ULK
058000030403     C                   Z-ADD     44            $L01
058100030403     C                   Z-ADD     0             $L02
058200030403     C                   EXSR      RIEKEY
058300030403     C                   Z-ADD     $ULK          $ULKD3            3 0
058400030403     C                   MOVEA     $FC           $FC3
058500030403     C                   MOVEL     $KEY1         Z3KE1
058600060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
058700030403     C                   WRITE     FMTZ3
058800030403     C*
058900030403C1630C                   ENDSR
059000030403C1630C/EJECT
059100970623     C************************************************************
059200970623     C* CONTROLLO VIDEATA
059300970623     C************************************************************
059400970623     C     CTRD1         BEGSR
059500970623     C*
059600970623     C                   SETOFF                                       99
059700970623     C* non imposto pos. del cursore
059800970623     C                   Z-ADD     0             H1RIGA
059900970623     C                   Z-ADD     0             H1COLO
060000970623     c* data operazione a
060100970623     C                   Move      *hival        DtopA
060200970623   1bC                   If        D1DtOpA  <> 0
060300000117C1309c******dmy          test(D)                 d1dtopa                21
060400000117C1309c     *JobRun       test(D)                 d1dtopa                21
060500970623     c                   if        *in21= *on
060600970623     C                   SETON                                        5299
060700970623     c                   else
060800000117C1309c******dmy          move      d1Dtopa       dtOpA
060900000117C1309c     *JobRun       move      d1Dtopa       dtOpA
061000970623  2e C                   Endif
061100970623  1e C                   Endif
061200970623     c
061300970623     C* Cerco banca linea sulla vista  logica che omette quelli ricon
061400970623     c* ciliati
061500970623     c                   clear                   d1bancad
061600970623     c                   move      *off          $ok               1
061700970623     c                   eval      Bmbsocieta = xscsoc
061800970623     c                   eval      BmbBanca = D1banca
061900970623     c                   eval      bmbLinea = D1linea
062000970911     c                   eval      BMBDtOpe = DtOpA
062100970911     c     K04BMB03      setll     NdBmb03l
062200970623     c                   do        *hival
062300970911     c     k03Bmb03      READPE    NDBMB03L                               21
062400101111D2643c* se ci sono solo record con la data richiesta, non funziona con
062500101111  "  c* setll e readpe, per cui eseguo chain con la data
062600101111  "  c                   if        *in21=*on
062700101111  "  c     K04BMB03      chain     NdBmb03l                           21
062800101111D2643c                   endif
062900970623     c   21              leave
063000970911     C** se la data operazione è maggiore della data a scarto
063100970911     c**                 if        Bmbdtope > DtopA
063200970911     c**                 iter
063300970911     c**                 endif
063400970911     c* se c'è già uno stato <> da '1'
063500970915     c****************** if        Bmbstatus <> '1'
063600970911     c* e il movimento è contabilizzato (???)
063700970911     c******************           and BmbNrasReg <> 0
063800970915     C* il primo movimento letto è valido perchè la VL già omette quelli
063900970915     C* riconciliati
064000970915     c                   move      *on           $ok               1
064100970915     C                   leave
064200970915     c****************** endif
064300970623     c*
064400970623     c                   enddo
064500970623     c*
064600970623     c* se non ce ne è neanche uno da passare in riconciliazione
064700970623     c                   if        $ok = *off
064800970623     c                   seton                                        5099
064900970710     c                   endif
065000970623     c* decodifica
065100970623     C* chiamo il driver
065200970623     C                   EXSR      CallDVABRA
065300970623   2bC                   If        Esito = *Off
065400970623     C                   movel     De2Ban016     D1BancaD
065500991102     C                   cat       De2Lin016:1   D1BancaD
065600970623   2ec                   endif
065700970623     C*
065800970623     c* Controllo flag
065900970623     C     D1interatt    IFNE      XSCON
066000970623     C     D1interatt    ANDNE     XSCOFF
066100970623     C                   SETON                                        5499
066200970623     C                   ENDIF
066300030303C1630C*
066400030303  "  c* Controllo flag
066500030303  "  C     D1ricnn       IFNE      XSCON
066600030303  "  C     D1ricnn       ANDNE     XSCOFF
066700030303  "  C                   SETON                                        5699
066800030303C1630C                   ENDIF
066900030303C1630C*
067000030303  "  c* Se richiesta riconciliazione n a n, non è possibile eseguire
067100030303  "  c* la riconciliazione in interattivo
067200030303  "  C     D1ricnn       IFEQ      XSCON
067300030303  "  C     D1interatt    ANDEQ     XSCON
067400030303  "  C                   SETON                                        5799
067500030303C1630C                   ENDIF
067600970623     C*
067700970623     C                   ENDSR
067800970623     C/EJECT
067900030403C1630C************************************************************
068000030403  "  C* GESTIONE VALORI ASSUNTI
068100030403  "  C************************************************************
068200030403C1630C     GESD3         BEGSR
068300030403     C*
068400030403   B1C     $INZD3        IFEQ      *ON
068500030403     C                   EXSR      INZD3
068600030403     C                   MOVE      *OFF          $INZD3
068700030403   E1C                   ENDIF
068800030403     C*
068900030403   B1C     $LASTV        IFNE      'D3'
069000030403     C                   EXSR      GESPIE
069100030403     C                   MOVE      $LASTV        $LASTG
069200030403     C                   MOVE      'D3'          $LASTV
069300030403   E1C                   ENDIF
069400030403     C*
069500030403     C* se esistono errori sulla videata
069600030403     C* emetto la write del formato a indicatori spenti per vedere
069700030403     C* le eventuali decodifiche
069800030403   B1C     *IN99         IFEQ      *ON
069900030403     C                   MOVEA     *IN           WIN
070000030403     C                   MOVE      *ALL'0'       IN5098           49
070100030403     C                   MOVEA     IN5098        *IN(50)
070200030403     C                   WRITE     FMTD3
070300030403     C                   MOVEA     WIN           *IN
070400030403   E1C                   ENDIF
070500030403     C*
070600030403     C                   EXFMT     FMTD3
070700030403     C*
070800030403     C* controllo abilitazione tasti funzione
070900030403     C                   EXSR      CTRKD3
071000030403     C*
071100030403   B1C     *IN99         IFEQ      *OFF
071200030403     C*
071300030403   B2C                   SELECT
071400030403     C* F3=Fine
071500030403     C     $TASTO        WHENEQ    F03
071600030403     C                   EXSR      F03D1
071700030403     C* F12=Ritorno
071800030403     C     $TASTO        WHENEQ    F12
071900030403     C                   EXSR      F12D3
072000030403     C* F24=Altri tasti
072100030403     C     $TASTO        WHENEQ    F24
072200030403     C                   EXSR      F24D3
072300030403     C*
072400030403     C                   OTHER
072500030403     C                   EXSR      CTRD3
072600030403     C     *IN99         IFEQ      *OFF
072700030403     C                   MOVE      $LASTG        $GEST
072800030403     C                   ENDIF
072900030403     C*
073000030403   E2C                   ENDSL
073100030403     C*
073200030403   E1C                   ENDIF
073300030403     C*
073400030403C1630C                   ENDSR
073500030403C1630C/EJECT
073600030403C1630C************************************************************
073700030403  "  C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
073800030403  "  C************************************************************
073900030403C1630C     INZD3         BEGSR
074000030403     C*
074100030403     C                   CLEAR                   FMTD3
074200030403     C     $VAL          IFEQ      *ON
074300030403     C                   MOVE      XSCON         D3VAL
074400030403     C                   ELSE
074500030403     C                   MOVE      XSCOFF        D3VAL
074600030403     C                   ENDIF
074700030403     C                   MOVE      XSCON         D3ON
074800030403     C                   MOVE      XSCOFF        D3OF
074900030403     C*
075000030403C1630C                   ENDSR
075100030403C1630C/EJECT
075200030403C1630C************************************************************
075300030403  "  C* CONTROLLO VIDEATA  D3
075400030403  "  C************************************************************
075500030403C1630C     CTRD3         BEGSR
075600030403     C*
075700030403     C                   SETOFF                                       99
075800030403     C* non imposto pos. del cursore
075900030403     C                   Z-ADD     0             H1RIGA
076000030403     C                   Z-ADD     0             H1COLO
076100030403     C*
076200030403     c* Controllo VALORE
076300030403     C     D3VAL         IFNE      XSCON
076400030403     C     D3VAL         ANDNE     XSCOFF
076500030403     C                   SETON                                        5099
076600030403     C                   ENDIF
076700030403     C*
076800030403     C     D3VAL         IFEQ      XSCON
076900030403     C                   MOVE      *ON           $VAL
077000030403     C                   ELSE
077100030403     C                   MOVE      *OFF          $val
077200030403     c                   endif
077300030403     C*
077400030403C1630C                   ENDSR
077500030403C1630C/EJECT
077600970623     C************************************************************
077700970623     C* ESECUZIONE LANCIO
077800970623     C************************************************************
077900970623     C     EXELAN        BEGSR
078000970721     C*
078100970721     C* Emetto messaggio a video di esecuzione riconciliazione automatica
078200970721     C*
078300970721     C                   EVAL      MsgId  = 'NDC0319'
078400970721     C                   EVAL      MsgFil = Promsg
078500970721     C                   EXSR      InvMsg
078600970623     C*
078700970623     C                   RESET                   ndcT45ds
078800970623    >C                   MOVE      xscsoc        SocietaT45
078900970623    >C                   MOVE      D1Banca       BancaT45
079000970623    >C                   MOVE      D1Linea       LineaT45
079100970623     c                   move      DtopA         DtopAT45
079200020214D1072c                   eval      giorniT45 = d1giorni
079300030303C1630C                   if        D1ricnn = xscon
079400030303  "  c                   move      *on           RicNNT45
079500030303  "  C                   else
079600030303  "  c                   move      *off          RicNNT45
079700030303C1630C                   endif
079800970623     C                   MOVEL     ndcT45ds      KPJBU
079900970623    >C                   MOVE      'CT45'        KCOAZ
080000970623     C                   IF        D1interatt = xscoff
080100970623    >C                   CALL      'BCH10'
080200970623     C                   PARM                    KPJBA
080300970623     c                   else
080400970623    >C                   CALL      'NDCT45R'
080500970623     C                   PARM                    KPJBA
080600970623     C                   ENDIF
080700030403C1630C* aggiorno valori assunti
080800030403C1630C                   EXSR      AGGMPE
080900970623     C* vado a fine
081000970721     c                   move      *on           oprt44
081100970623     C                   MOVEL     *on           $Fine
081200970623     C                   ENDSR
081300970623     C/EJECT
081400970623     C************************************************************
081500970623     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
081600970623     C************************************************************
081700970623     C     CTRKD1        BEGSR
081800970623     C*
081900970623     C                   SETOFF                                       99
082000970623     C                   MOVEA     $FC1          $FC
082100970623     C                   EXSR      CTRKEY
082200970623     C     *IN99         IFEQ      *ON
082300970623     C                   SETON                                        97
082400970623     C                   ENDIF
082500970623     C*
082600970623     C                   ENDSR
082700970623     C/EJECT
082800030403C1630C************************************************************
082900030403  "  C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
083000030403  "  C* INPUT
083100030403  "  C* $FC3 = Abilitazioni videata D3
083200030403  "  C* OUTPUT
083300030403  "  C* *IN97 *IN99 Errori
083400030403  "  C************************************************************
083500030403C1630C     CTRKD3        BEGSR
083600030403     C*
083700030403     C                   SETOFF                                       99
083800030403     C                   MOVEA     $FC3          $FC
083900030403     C                   EXSR      CTRKEY
084000030403     C     *IN99         IFEQ      *ON
084100030403     C                   SETON                                        97
084200030403     C                   ENDIF
084300030403     C*
084400030403C1630C                   ENDSR
084500030403C1630C/EJECT
084600970623     C************************************************************
084700970623     C* CONTROLLO ABILITAZIONE TASTI
084800970623     C************************************************************
084900970623     C     CTRKEY        BEGSR
085000970623     C*
085100970623     C                   SELECT
085200970623     C     $TASTO        WHENEQ    F01
085300970623     C     $FC(1)        IFEQ      '0'
085400970623     C                   SETON                                        99
085500970623     C                   ENDIF
085600970623     C     $TASTO        WHENEQ    F02
085700970623     C     $FC(2)        IFEQ      '0'
085800970623     C                   SETON                                        99
085900970623     C                   ENDIF
086000970623     C     $TASTO        WHENEQ    F03
086100970623     C     $FC(3)        IFEQ      '0'
086200970623     C                   SETON                                        99
086300970623     C                   ENDIF
086400970623     C     $TASTO        WHENEQ    F04
086500970623     C     $FC(4)        IFEQ      '0'
086600970623     C                   SETON                                        99
086700970623     C                   ENDIF
086800970623     C     $TASTO        WHENEQ    F05
086900970623     C     $FC(5)        IFEQ      '0'
087000970623     C                   SETON                                        99
087100970623     C                   ENDIF
087200970623     C     $TASTO        WHENEQ    F06
087300970623     C     $FC(6)        IFEQ      '0'
087400970623     C                   SETON                                        99
087500970623     C                   ENDIF
087600970623     C     $TASTO        WHENEQ    F07
087700970623     C     $FC(7)        IFEQ      '0'
087800970623     C                   SETON                                        99
087900970623     C                   ENDIF
088000970623     C     $TASTO        WHENEQ    F08
088100970623     C     $FC(8)        IFEQ      '0'
088200970623     C                   SETON                                        99
088300970623     C                   ENDIF
088400970623     C     $TASTO        WHENEQ    F09
088500970623     C     $FC(09)       IFEQ      '0'
088600970623     C                   SETON                                        99
088700970623     C                   ENDIF
088800970623     C     $TASTO        WHENEQ    F10
088900970623     C     $FC(10)       IFEQ      '0'
089000970623     C                   SETON                                        99
089100970623     C                   ENDIF
089200970623     C     $TASTO        WHENEQ    F11
089300970623     C     $FC(11)       IFEQ      '0'
089400970623     C                   SETON                                        99
089500970623     C                   ENDIF
089600970623     C     $TASTO        WHENEQ    F12
089700970623     C     $FC(12)       IFEQ      '0'
089800970623     C                   SETON                                        99
089900970623     C                   ENDIF
090000970623     C     $TASTO        WHENEQ    F13
090100970623     C     $FC(13)       IFEQ      '0'
090200970623     C                   SETON                                        99
090300970623     C                   ENDIF
090400970623     C     $TASTO        WHENEQ    F14
090500970623     C     $FC(14)       IFEQ      '0'
090600970623     C                   SETON                                        99
090700970623     C                   ENDIF
090800970623     C     $TASTO        WHENEQ    F15
090900970623     C     $FC(15)       IFEQ      '0'
091000970623     C                   SETON                                        99
091100970623     C                   ENDIF
091200970623     C     $TASTO        WHENEQ    F16
091300970623     C     $FC(16)       IFEQ      '0'
091400970623     C                   SETON                                        99
091500970623     C                   ENDIF
091600970623     C     $TASTO        WHENEQ    F17
091700970623     C     $FC(17)       IFEQ      '0'
091800970623     C                   SETON                                        99
091900970623     C                   ENDIF
092000970623     C     $TASTO        WHENEQ    F18
092100970623     C     $FC(18)       IFEQ      '0'
092200970623     C                   SETON                                        99
092300970623     C                   ENDIF
092400970623     C     $TASTO        WHENEQ    F19
092500970623     C     $FC(19)       IFEQ      '0'
092600970623     C                   SETON                                        99
092700970623     C                   ENDIF
092800970623     C     $TASTO        WHENEQ    F20
092900970623     C     $FC(20)       IFEQ      '0'
093000970623     C                   SETON                                        99
093100970623     C                   ENDIF
093200970623     C     $TASTO        WHENEQ    F21
093300970623     C     $FC(21)       IFEQ      '0'
093400970623     C                   SETON                                        99
093500970623     C                   ENDIF
093600970623     C     $TASTO        WHENEQ    F22
093700970623     C     $FC(22)       IFEQ      '0'
093800970623     C                   SETON                                        99
093900970623     C                   ENDIF
094000970623     C     $TASTO        WHENEQ    F23
094100970623     C     $FC(23)       IFEQ      '0'
094200970623     C                   SETON                                        99
094300970623     C                   ENDIF
094400970623     C     $TASTO        WHENEQ    F24
094500970623     C     $FC(24)       IFEQ      '0'
094600970623     C                   SETON                                        99
094700970623     C                   ENDIF
094800970623     C                   ENDSL
094900970623     C*
095000970623     C                   ENDSR
095100970623     C/EJECT
095200970623     C************************************************************
095300970623     C* OPERAZIONI INIZIALI
095400970623     C************************************************************
095500970623     C     *INZSR        BEGSR
095600000117C1309C     *DMY          Move      Udate         ZDateISO
095700000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
095800970623     C*
095900970623     C* Reperimento parametri
096000970623     C*
096100970623     C     *ENTRY        PLIST
096200970623     C                   PARM                    KPJBA
096300970623      *
096400970623     C*
096500970623     C* Reperimento tasti di funzione
096600970623     C*
096700970623     C                   EXSR      REPKEY
096800970623     C*
096900970623     C                   ENDSR
097000970623     C/EJECT
097100970623     C************************************************************
097200970623     C* REPERIMENTO TASTI DI FUNZIONE
097300970623     C************************************************************
097400970623     C     REPKEY        BEGSR
097500970623     C                   MOVEA     $FM1          $FM
097600970623     C                   MOVEA     $Fc1          $Fc
097700970623     C                   EXSR      REPKKK
097800970623     C                   MOVEA     $FX           $FX1
097900030403     C*
098000030403C1630C                   MOVEA     $FM3          $FM
098100030403  "  C                   MOVEA     $Fc3          $FM
098200030403  "  C                   EXSR      REPKKK
098300030403C1630C                   MOVEA     $FX           $FX3
098400970623     C*
098500970623     C                   ENDSR
098600970623     C/EJECT
098700970623     C************************************************************
098800970623     C* REPERIMENTO TASTI DI FUNZIONE
098900970623     C************************************************************
099000970623     C     REPKKK        BEGSR
099100970623     C*
099200970623     C                   CLEAR                   $FX
099300970623     C     1             DO        24            X
099400970623     C*
099500970623     C     $FM(X)        IFNE      *BLANK
099600970623     C     $fc(X)        andne     '0'
099700970623     C*
099800970623     C                   MOVEL     $FM(X)        IDMSG
099900970623     C                   CALLB     'XRTVM'
100000970623     C                   PARM                    IDMSG            27
100100970623     C                   PARM                    TXTMSG          100
100200970623     C                   PARM                    ERRMSG            1
100300970623     C*
100400970623     C     ERRMSG        IFNE      *ON
100500970623     C                   MOVEL     TXTMSG        $FX(X)
100600970623     C                   ELSE
100700970623     C                   MOVEL     *ALL' '       $FX(X)
100800970623     C                   ENDIF
100900970623     C*
101000970623     C                   ENDIF
101100970623     C*
101200970623     C                   ENDDO
101300970623     C*
101400970623     C                   ENDSR
101500970623     C/EJECT
101600970623     C************************************************************
101700970623     C* INIZIALIZZAZIONE VARIABILI
101800970623     C************************************************************
101900970623     C     INZVAR        BEGSR
102000970623     C*
102100970623     c                   movel     kpjbu         ndcT44ds
102200970623     C* Reperimento dati società
102300970623     C*
102400970623    >C                   MOVEL     'SOC001'      TIPXSC
102500970623     C                   MOVEL     societaT44    SOCXSC
102600970623     C                   EXSR      REPSOC
102700970623     C     RTNXSC        IFNE      '1'
102800970623     C                   MOVEL     XSOCDS        SOC001
102900970623     C                   ELSE
103000970623     C                   RETURN
103100970623     C                   ENDIF
103200970623     C* Asteriscare se non presenti
103300970623     C* Reperimento autorizzazioni
103400970623     C*
103500970623    >C                   EXSR      REPAUT
103600030403C1630C*
103700030403  "  C* Reperimento valori default utente
103800030403  "  C*
103900030403C1630C                   EXSR      REPDFT
104000970623     C*
104100970623     C* Definizione dei tasti funzione utilizzabili a pgm
104200970623     C*
104300970623     C                   EXSR      DEFOPZ
104400970623     C*
104500970623     C* Variabili per gestione videate
104600970623     C*
104700970623     C                   MOVE      'D1'          $GEST                          fmt in gestione
104800970623     C                   MOVE      *OFF          $FINE                          fine pgm
104900970623     C                   MOVE      *ON           $INZD1                         inizializzione D1
105000970623     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
105100970623     C                   MOVE      *BLANK        $LASTV                         ultima emissione
105200970623     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
105300970623     C                   Z-ADD     0             $ULKD3            3 0          piede da emett. D3
105400970623     C*
105500970623     C* Variabili appoggio
105600970623     C*
105700970623     C                   Z-ADD     0             CURR
105800970623     C                   Z-ADD     0             CURC
105900970623     C*
106000970623     C* Valorizzazione campi univoci testate
106100970623     C*
106200970623     C                   CLEAR                   FMTT1
106300970623     C                   MOVEL     KNSIF         NOMSIF
106400970623     C                   MOVEL     XSCDSI        NOMDIT
106500970623     C                   MOVEL     DSPGM         NOMPGM
106600970623     C*
106700970623     C* Inizializzazione/Decodifica prima videata
106800970623     C*
106900970623     C                   EXSR      INZD1
107000970623     C                   MOVE      *OFF          $INZD1
107100970623     C*
107200970623     C                   ENDSR
107300970623     C/EJECT
107400970623     C************************************************************
107500970623     C* REPERIMENTO DATI SOCIETA'
107600970623     C************************************************************
107700970623     C     REPSOC        BEGSR
107800970623     C*
107900970623     C                   CALLB     'XSOC'
108000970623     C                   PARM                    TIPXSC            6
108100970623     C                   PARM                    SOCXSC            3
108200970623     C                   PARM                    CDSXSC            9 0
108300970623     C                   PARM                    MODXSC            3
108400970623     C                   PARM      *BLANKS       RTNXSC            1
108500970623     C                   PARM                    XSOCDS
108600970623     C                   PARM                    KPJBA
108700970623     C*
108800970623     C                   ENDSR
108900970623     C/EJECT
109000970623     C************************************************************
109100970623     C* REPERIMENTO AUTORIZZAZIONI UTENTE
109200970623     C************************************************************
109300970623     C     REPAUT        BEGSR
109400970623     C*
109500970623     C                   CLEAR                   AUTDS
109600970623     C*
109700970623     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
109800970623     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
109900970623     C                   Z-ADD     0             XCAGRP                         GRUPPO
110000970623     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
110100970623    >C                   MOVEL     'CONTES'      XCAFNC                         FUNZIONE
110200970623    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
110300970623    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
110400970623    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
110500970623     C*
110600970623     C                   CALLB     'XCHKAUT'
110700970623     C                   PARM                    AUTDS
110800970623     C*
110900970623     C* errori/non abilitazione
111000970623     C     XCARTN        IFNE      0
111100970623     C                   MOVE      *ON           $fine
111200970623     C                   EXSR      ENDPGM
111300970623     C                   ELSE
111400970623    >C                   MOVEL     XCADDS        CONTESDS
111500970623     C* Se non autorizzati alla funzione oppure                              ed esco
111600991103    >C     TesRicRc      IFNE      *ON
111700970623     C                   MOVE      XCAFNC        XNOFNC
111800970623     C                   MOVE      XCAPRF        XNOPRF
111900970623     C                   MOVE      XCACUT        XNOCUT
112000970623     C                   MOVE      XCATCK        XNOTCK
112100970623     C                   MOVE      '1'           XNOERR                         nessuna abilitazione
112200970623     C                   CALL      'XNOAUTR'
112300970623     C                   PARM                    XNOADS
112400970623     C                   MOVE      *ON           $fine
112500970623     C                   EXSR      ENDPGM
112600970623     C                   ENDIF
112700970623     C                   ENDIF
112800970623     C*
112900970623     C                   ENDSR
113000970623     C/EJECT
113100030403C1630C************************************************************
113200030403  "  C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
113300030403  "  C************************************************************
113400030403C1630C     REPDFT        BEGSR
113500030403     C*
113600030403     C* preparo i parametri di chiamata al driver
113700030403     C                   CLEAR                   XMPEDS
113800030403     C                   EVAL      TpChiamata = '01'
113900030403     C                   EVAL      NChiavi = 5
114000030403     C                   EVAL      VLogica = 2
114100030403     C*
114200030403     C* preparo le chiavi per la chiamata al driver
114300030403     C                   CLEAR                   XMPEDS1
114400030403     C                   MOVEL     XSCSOC        DSMPESOC
114500030403     C                   MOVEL     DSPGM         DSMPEPGM
114600030403     C                   MOVEL     KNMUS         DSMPEPRF
114700030403     C* Si reperiscono i dati di default a seconda delle modalità con
114800030403     C* cui il pgm è usato
114900030403     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
115000030403     C* MPETIP = 'PV'  pgm richiamato da menù
115100030403     C* MPETIP = 'PB'  x lancio Batch
115200030403     C                   MOVEL     'PB'          DSMPETIP
115300030403     C*
115400030403     C                   CALLB     'XMPE'
115500030403     C                   PARM                    XMPEDS
115600030403     C                   PARM                    XMPEDS1
115700030403     C*
115800030403     C     Risultato     IFEQ      '0'
115900030403     C                   MOVEL     DSMPEPAR      DSMPE
116000030403     C* nel caso sia la prima volta che l'utente apre questa anagrafica
116100030403     C* i suoi valori di default vengono decisi a pgm
116200030403     C                   ELSE
116300030403     C                   MOVE      *OFF          $VAL
116400030403     C                   ENDIF
116500030403     C*
116600030403C1630C                   ENDSR
116700030403C1630C/EJECT
116800030403C1630C************************************************************
116900030403  "  C* AGGIORNAMENTO VALORI ASSUNTI
117000030403  "  C************************************************************
117100030403C1630C     AGGMPE        BEGSR
117200030403     C*
117300030403     C* preparo i parametri di chiamata al driver
117400030403     C                   CLEAR                   XMPEDS
117500030403     C                   EVAL      TpChiamata = '03'
117600030403     C                   EVAL      NChiavi = 5
117700030403     C                   EVAL      VLogica = 2
117800030403     C*
117900030403     C* preparo le chiavi per la chiamata al driver
118000030403     C                   CLEAR                   XMPEDS1
118100030403     C                   MOVEL     XSCSOC        DSMPESOC
118200030403     C                   MOVEL     DSPGM         DSMPEPGM
118300030403     C                   MOVEL     KNMUS         DSMPEPRF
118400030403     C*
118500030403     C* Si memorizzano i dati di default a seconda delle modalità con
118600030403     C* cui il pgm è usato
118700030403     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
118800030403     C* MPETIP = 'PV'  pgm richiamato da menù
118900030403     C* MPETIP = 'PB'  x lancio Batch
119000030403     C*
119100030403     C                   MOVEL     'PB'          DSMPETIP
119200030403     C* dati
119300030403     C*
119400030617     C                   Movel     D1Banca       $Banca
119500030617     C                   Movel     D1Linea       $Linea
119600030617     C                   Movel     D1DtOpA       $DtOpA
119700030403     C     D1Interatt    IfEq      XScOn
119800030403     C                   Move      *On           $Interatt
119900030403     C                   Else
120000030403     C                   Move      *Off          $Interatt
120100030403     C                   EndIf
120200030403     C                   Z-Add     D1Giorni      $Giorni
120300030403     C     D1RicNN       IfEq      XScOn
120400030403     C                   Move      *On           $RicNN
120500030403     C                   Else
120600030403     C                   Move      *Off          $RicNN
120700030403     C                   EndIf
120800030403     C                   MOVEL     DSMPE         DSMPEPAR
120900030403     C*
121000030403     C                   CALLB     'XMPE'
121100030403     C                   PARM                    XMPEDS
121200030403     C                   PARM                    XMPEDS1
121300030403     C*
121400030403C1630C                   ENDSR
121500030403C1630C/EJECT
121600970623     C************************************************************
121700970623     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
121800970623     C************************************************************
121900970623     C     DEFOPZ        BEGSR
122000970623     C*
122100970623     C* Questa routine deve valorizzare la schiera $FC1/$FR1
122200970623     C* con S o N a seconda della utilizzabilità o meno
122300970623     C* del tasto funzione correlato
122400970623     C*
122500970623     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
122600970623     C* di compilazione
122700970623     C                   RESET                   $FC1
122800970623     C                   RESET                   $FR1
122900970623     C*
123000970623     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
123100970623     C* invalido alcune opzioni
123200970623     C                   SELECT
123300970623     C*
123400970623     C     OPZT44        WHENEQ    *BLANKS
123500970623     C* Se pgm non richiamato, disattivo subito :
123600970623     C* F8=Selezione seguente
123700970623     C                   Z-ADD     8             X
123800970623     C                   MOVE      '0'           $FC1(X)
123900970623     c*
124000970623     C     OPZT44        WHENEQ    '02'
124100970623     C* Se pgm richiamato, disattivo
124200970623     C* F3=Fine
124300970623     C                   Z-ADD     3             X
124400970623     C                   MOVE      '0'           $FC1(X)
124500970623     C*
124600970623     C* In tutti gli altri casi, disattivo subito :
124700970623     C                   OTHER
124800970623     C*
124900970623     C                   ENDSL
125000970623     C*
125100970623     C                   ENDSR
125200970623     C/EJECT
125300970623     C************************************************************
125400970623     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
125500970623     C************************************************************
125600970623     C     DEFCAM        BEGSR
125700970623     C*
125800970623     C* klist
125900970911     c     K04BMB03      klist
126000970623     c                   kfld                    BmbSocieta
126100970623     c                   kfld                    BmbBanca
126200970623     c                   kfld                    BmbLinea
126300970911     c                   kfld                    BMBDtOpe
126400970623     C*
126500970911     c     K03Bmb03      klist
126600970911     c                   kfld                    BmbSocieta
126700970911     c                   kfld                    BmbBanca
126800970911     c                   kfld                    BmbLinea
126900970911     C*
127000970623     C*
127100970623     C* Variabili per gestione videate
127200970623     C*
127300970623     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
127400970623     C                   MOVE      *BLANK        $FINE             1            fine pgm
127500970623     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
127600970623     C                   MOVE      *BLANK        $INZD3            1            inizializzione D3
127700970623     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
127800970623     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
127900970623     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
128000970623     C*
128100970623     C* Variabili appoggio
128200970623     C*
128300970623     C                   Z-ADD     0             CURR              2 0
128400970623     C                   Z-ADD     0             CURC              2 0
128500970623     C                   MOVE      *ZEROS        WIN              99
128600970623     C*
128700970623     C* Indici
128800970623     C*
128900970623     C                   Z-ADD     0             X                 3 0
129000970623     C                   Z-ADD     0             Y                 3 0
129100970623     C*
129200970623     C                   ENDSR
129300970623     C/EJECT
129400970623     C************************************************************
129500970623     C* Chiamata al driver NdDvaBra
129600970623     C************************************************************
129700970623     C     CallDvaBra    BEGSR
129800970623     C*
129900970623     C                   CLEAR                   Nd016Ds
130000970623     C                   Eval      LenOut    = %Size(ND016DS)
130100970623     C                   CALLB     'NDDVABRA'
130200970623     C                   PARM      '01'          Operazione
130300970623     C                   PARM                    xscsoc
130400970623     C                   PARM                    d1banca
130500970623     C                   PARM                    d1linea
130600970623     C                   PARM                    *Omit
130700970623     C                   PARM                    *Omit
130800970623     C                   PARM      'ND016DS'     StrutturaO
130900970623     C                   PARM                    LenOut
131000970623     C                   PARM      *Off          Esito
131100970623     C     Nd016Ds       PARM      *Blanks       OutPut
131200970623     C*
131300970623     C                   ENDSR
131400970623     C/EJECT
131500970623     C************************************************************
131600970623     C* RIEMPIMENTO TASTI FUNZIONE
131700970623     C************************************************************
131800970623     C     RIEKEY        BEGSR
131900970623     C*
132000970623     C* Input :
132100970623     C* - $FX = schiera con tutti i testi disponibili
132200970623     C* - $FC = schiere con i flag di validità associati ai testi
132300970623     C* - $FR = schiere con i flag di REVERSE IMAGE
132400970623     C* - $ULK = ultimo testo caricato
132500970623     C* - $L01 = Lunghezza prima riga
132600970623     C* - $L02 = Lunghezza seconda riga
132700970623     C* Output :
132800970623     C* - $ULK
132900970623     C* - $KEY1= 1° riga di tasti funzione
133000970623     C* - $KEY2= 2° riga di tasti funzione
133100970623     C*
133200970623     C                   CALLB     'XKEYMSG'
133300970623     C* input
133400970623     C                   PARM                    $FX
133500970623     C                   PARM                    $FC
133600970623     C                   PARM                    $FR
133700970623     C                   PARM                    $ULK              3 0
133800970623     C* output
133900970623     C                   PARM                    $KEY1            79
134000970623     C                   PARM                    $KEY2            79
134100970623    >C* input
134200970623    >C                   PARM                    $L01              3 0
134300970623    >C                   PARM                    $L02              3 0
134400970623     C*
134500060224C1913C                   PARM                    $ALLFUNCT       480
134600060224C1913C*
134700970623     C                   ENDSR
134800970721     C*----------------------------------------------------------
134900970721     C* Invia messaggi di errore
135000970721     C* Input: ID messaggio, Tipo messaggio
135100970721     C*----------------------------------------------------------
135200970721     C     INVMSG        BEGSR
135300970721     C                   call      'QMHSNDPM'
135400970721     C                   PARM                    MSGID             7
135500970721     C                   PARM                    MSGFIL
135600970721     C                   PARM      Dtamsg        MSGDTA          256
135700970721     C                   PARM      256           LENDTA
135800970721     C                   PARM      '*STATUS'     TYPE             10
135900970721     C                   PARM      '*EXT'        MSGQ             10
136000970721     C                   PARM      0             CALLSTACK
136100970721     C                   PARM                    MSGK              4
136200970721     C                   PARM                    ERRCOD
136300970721     c*
136400970721     c*
136500970721     C                   ENDSR
136600030403C1630 * abilitato tasto F18 su D1/D2
136700030403C1630 * aggiunti tasti funzione ed abilitazione a video D3
136800970623** TASTI DI FUNZIONE  D1
136900970623PROMSG    *LIBL     KEY0001  01
137000970623PROMSG    *LIBL     KEY0002  02
137100970623PROMSG    *LIBL     KEY0003  03
137200970623PROMSG    *LIBL     KEY0004  04
137300970623PROMSG    *LIBL     KEY0005  05
137400970623PROMSG    *LIBL     KEY0006  06
137500970623PROMSG    *LIBL     KEY0007  07
137600970623PROMSG    *LIBL     KEY0008  08
137700970623PROMSG    *LIBL     KEY0009  09
137800970623PROMSG    *LIBL     KEY0010  10
137900970623PROMSG    *LIBL     KEY0011  11
138000970623PROMSG    *LIBL     KEY0012  12
138100970623PROMSG    *LIBL     KEY0013  13
138200970623PROMSG    *LIBL     KEY0014  14
138300970623PROMSG    *LIBL     KEY0015  15
138400970623PROMSG    *LIBL     KEY0016  16
138500970623PROMSG    *LIBL     KEY0017  17
138600970623PROMSG    *LIBL     KEY0018  18
138700970623PROMSG    *LIBL     KEY0019  19
138800970623PROMSG    *LIBL     KEY0020  20
138900970623PROMSG    *LIBL     KEY0021  21
139000970623PROMSG    *LIBL     KEY0022  22
139100970623PROMSG    *LIBL     KEY0023  23
139200970623PROMSG    *LIBL     KEY0024  24
139300970623**  ABILITAZIONE D1 /D2
1394009706230N  01
1395009706230N  02
1396009706231N  03
1397009706231N  04
1398009706231N  05
1399009706231N  06
1400009706230N  07
1401009706231N  08
1402009706230N  09
1403009706230N  10
1404009706230N  11
1405009706231N  12
1406009706230N  13
1407009706230N  14
1408009706230N  15
1409009706230N  16
1410009706230N  17
1411000304031N  18
1412009706230N  19
1413009706230N  20
1414009706231N  21
1415009706230N  22
1416009706230N  23   Impostare a '1' se si usano le opzioni
1417009706231N  24   Lasciare sempre impostata a '1' il valore della riga 24
141800030403** TASTI DI FUNZIONE  D3
141900030403PROMSG    *LIBL     KEY0001  01
142000030403PROMSG    *LIBL     KEY0002  02
142100030403PROMSG    *LIBL     KEY0003  03
142200030403PROMSG    *LIBL     KEY0004  04
142300030403PROMSG    *LIBL     KEY0005  05
142400030403PROMSG    *LIBL     KEY0006  06
142500030403PROMSG    *LIBL     KEY0007  07
142600030403PROMSG    *LIBL     KEY0008  08
142700030403PROMSG    *LIBL     KEY0009  09
142800030403PROMSG    *LIBL     KEY0010  10
142900030403PROMSG    *LIBL     KEY0011  11
143000030403PROMSG    *LIBL     KEY0012  12
143100030403PROMSG    *LIBL     KEY0013  13
143200030403PROMSG    *LIBL     KEY0014  14
143300030403PROMSG    *LIBL     KEY0015  15
143400030403PROMSG    *LIBL     KEY0016  16
143500030403PROMSG    *LIBL     KEY0017  17
143600030403PROMSG    *LIBL     KEY0018  18
143700030403PROMSG    *LIBL     KEY0019  19
143800030403PROMSG    *LIBL     KEY0020  20
143900030403PROMSG    *LIBL     KEY0021  21
144000030403PROMSG    *LIBL     KEY0022  22
144100030403PROMSG    *LIBL     KEY0023  23
144200030403PROMSG    *LIBL     KEY0024  24
144300030403**  ABILITAZIONE D3
1444000304030N  01
1445000304030N  02
1446000304031N  03
1447000304030N  04
1448000304030N  05
1449000304030N  06
1450000304030N  07
1451000304030N  08
1452000304030N  09
1453000304030N  10
1454000304030N  11
1455000304031N  12
1456000304030N  13
1457000304030N  14
1458000304030N  15
1459000304030N  16
1460000304030N  17
1461000304030N  18
1462000304030N  19
1463000304030N  20
1464000304030N  21
1465000304030N  22
1466000304030N  23   Impostare a '1' se si usano le opzioni
1467000304031N  24   Lasciare sempre impostata a '1' il valore della riga 24
