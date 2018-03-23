000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970620     H*PARMS CVTOPT(*NONE)
000300970620     H DECEDIT('0,') DATEDIT(*DMY.)
000400970620      *
000500170731      *         ------------------------------------------------
000600170731      *                      Spesometro Trimestrale
000700170731      *                 Elenco file spesometro trimestrale
000800170731      *         ------------------------------------------------
000900970620      *
001000170731R258  * Questo PGM è stato creato con il task R258
001100970620      *
001200970620     F*----------------------------------------------------*
001300170801    >FSDGCSF02L UF A E           K DISK
001400970717     F*------------
001500170731    >FSDGSI83D  CF   E             WORKSTN
001600170731     F                                     RENAME(SI83D1:FMTD1)
001700170731     F                                     RENAME(SI83T1:FMTT1)
001800170731     F                                     RENAME(SI83Z1:FMTZ1)
001900970620     F                                     INFDS(DSFMT)
002000970620     D*----------------------------------------------------*
002100970813     D $OZ             S              2    DIM(6) CTDATA PERRCD(1)
002200970813     D $ID             S             27    DIM(6) ALT($OZ)
002300970620     D* Schiere per la gestione dei tasti di funzione
002400970620     D $FM             S             27    DIM(24)
002500970620     D $FX             S             50    DIM(24)
002600970620     D $FC             S              1    DIM(24)
002700970620     D $FR             S              1    DIM(24)
002800970620     D* Video D1
002900970620     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003000970620     D $FX1            S             50    DIM(24) ALT($FM1)
003100970620     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003200970620     D $FR1            S              1    DIM(24) ALT($FC1)
003300970620     D* Schiera contente i nomi dei programmi da richiamare con i tasti
003400970620     D* funzione personalizzati
003500970620     D $PGF1           S             10    DIM(25)
003600970620     D*----------------------------------------------------*
003700970620     D $D1L01          S              3  0
003800970620     D $D1L02          S              3  0
003900970623     D $D2L01          S              3  0
004000970623     D $D2L02          S              3  0
004100970620     D*-------------
004200970620     D* Passaggio Parametri
004300970620     D KPJBA         E DS
004400970620     D*-------------
004500970620     D* DS Esterna gestione messaggi
004600970620     D XMSGDS        E DS
004700970620     D  STS          E                     EXTFLD(MSGSTS)
004800970620     D                                     DIM(333)
004900970620     D  JBA          E                     EXTFLD(MSGJBA)
005000970620     D                                     DIM(502)
005100970620     D*-------------
005200970620     D* Reperimento Autorizzazioni
005300970620     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005400970620     D*-------------
005500970620     D* Passaggio Autorizzazioni
005600970620     D DSAUT           DS           512
005700970620     D*-------------
005800970620     D* Maschera dati autorizzazioni
005900981026    >D ANGCFIDS      E DS
006000970620     D*-------------
006100970620     D* Reperimento nome PGM
006200970620     D STATUS         SDS           333
006300970620     D  DSPGM            *PROC
006400970620     D  PARMS            *PARMS
006500970620     D*-------------
006600170731     D* Dati di ambiente ottenuti da SDGSOCDR
006700170731     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006800970620     D*-------------
006900970620     D* DS Interna per dati di output di XSOC
007000970620     D XSOCDS          DS          1000
007100970620     D*-------------
007200970701     D* Passaggio Parametri al PGM
007300170731    >D SDGSI83DS     E DS
007400970701     D*-------------
007500970620     D* campo di salvataggio per la DS d'ingresso durante la chiamata
007600970620     D* alla funzione personalizzata
007700170731    >D $SaveDS         S                   like(SDGSI83DS)
007800970620     D*-------------
007900981026     D* variabili di appoggio alla chiamata di xchkc
008000981026     D $File           S             10A
008100981026     D $Lib            S             10A
008200981026     D $Tipo           S              7A
008300981026     D $Errore         S              1A
008400981026     D $Membro         S             10A
008500981026     D*-------------
008600970620     D DSFMT           DS
008700970620     D  $TASTO               369    369
008800970620     D  NRG                  370    370
008900970620     D  NCL                  371    371
009000970620     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
009100970620     D*-------------
009200970620     D* posizione cursore
009300970620     D CURSOR          DS
009400970620     D  RIRI                   1      2B 0 INZ
009500970620     D  R$$                    2      2
009600970620     D  COCO                   3      4B 0 INZ
009700970620     D  C$$                    4      4
009800970620     D*-------------
009900970620     D* Costanti
010000970620     D*-------------
010100970620     D* Tasti di funzione
010200970620     D F01             C                   CONST(X'31')
010300970620     D F02             C                   CONST(X'32')
010400970620     D F03             C                   CONST(X'33')
010500970620     D F04             C                   CONST(X'34')
010600970620     D F05             C                   CONST(X'35')
010700970620     D F06             C                   CONST(X'36')
010800970620     D F07             C                   CONST(X'37')
010900970620     D F08             C                   CONST(X'38')
011000970620     D F09             C                   CONST(X'39')
011100970620     D F10             C                   CONST(X'3A')
011200970620     D F11             C                   CONST(X'3B')
011300970620     D F12             C                   CONST(X'3C')
011400970620     D F13             C                   CONST(X'B1')
011500970620     D F14             C                   CONST(X'B2')
011600970620     D F15             C                   CONST(X'B3')
011700970620     D F16             C                   CONST(X'B4')
011800970620     D F17             C                   CONST(X'B5')
011900970620     D F18             C                   CONST(X'B6')
012000970620     D F19             C                   CONST(X'B7')
012100970620     D F20             C                   CONST(X'B8')
012200970620     D F21             C                   CONST(X'B9')
012300970620     D F22             C                   CONST(X'BA')
012400970620     D F23             C                   CONST(X'BB')
012500970620     D F24             C                   CONST(X'BC')
012600970620     D ENTER           C                   CONST(X'F1')
012700970620     D ROLDWN          C                   CONST(X'F4')
012800970620     D ROLLUP          C                   CONST(X'F5')
012900970620     D*-------------
013000970620     D* Sottotitolo di testata
013100170731     D SOTTIT          C                   CONST('SDGMSG    *LIBL-
013200170731     D                                          SDC0115')
013300970620     D*-------------
013400970620     D* Variabili per gestione videate
013500970620     D $Gest           S              2A
013600970620     D $Fine           S              1A
013700970620     D $InzD1          S              1A
013800970623     D $InzD2          S              1A
013900970620     D $LastG          S              2A
014000970620     D $LastV          S              2A
014100970620     D*-------------
014200970620     D* Variabili appoggio sempre presenti in tutte le anagrafiche
014300970620     D CurR            S              2S 0
014400970620     D CurC            S              2S 0
014500970620     D Win             S             99A
014600970620     D*-------------
014700970620     D* Variabili appoggio specifiche della singola anagrafica
014800970620     D*  Indici
014900970620     D X               S              3S 0
015000970620     D Y               S              3S 0
015100970620     D* Indice della schiera tasti funzione
015200970620     D PosFun          S              2  0
015300970701     D*-------------
015400970701     D* Controlli validità legende
015500170731     D X07VALDS      E DS                  Inz
015600970701     D*-------------
015700970701     D* Ricerca legende
015800170801     D Ana964DS      E DS
015900971016     D*-------------
016000170731     D ZDateISO        S               D
016100170801     D WDtInvio        S               D
016200170801     D WDtXml          S               D
016300970620     C*----------------------------------------------------*
016400970620     C*                MAIN LINE PROGRAM
016500970620     C*----------------------------------------------------*
016600970620     C*
016700970620     C* inizializzazione variabili
016800970620     C                   EXSR      INZVAR
016900970620     C*
017000970620     C     $FINE         DOWEQ     *OFF
017100970620     C     $GEST         CASEQ     'D1'          GESD1
017200970620     C                   ENDCS
017300970620     C                   ENDDO
017400970620     C*
017500970620     C* fine programma
017600970620     C                   EXSR      ENDPGM
017700970620     C/EJECT
017800970620     C************************************************************
017900970620     C* Fine Programma
018000970620     C************************************************************
018100970620     C     ENDPGM        BEGSR
018200970620     C*
018300970620     C* se non esco per errore
018400170731    >C     ErrI83        IFEQ      '0'
018500970620     C*
018600970620     C* se il COMMIT non è demandato al pgm chiamante
018700170731    >C     CMTI83        IFEQ      '0'
018800970620     C*
018900970620     C* test se esistono transazioni attive ed eventualmente cosa fare
019000970620     C                   Z-ADD     0             EXECOD
019100970620     C                   CALL      'XRCMTI'
019200970620     C                   PARM                    EXECOD            1 0
019300970620     C*
019400970620     C                   SELECT
019500970620     C* Annulla richiesta
019600970620     C     EXECOD        WHENEQ    1
019700970620     C                   MOVEL     *OFF          $FINE
019800970620     C                   MOVEL     *HIVAL        $LASTV
019900970620     C* Conferma uscita
020000970620     C     EXECOD        WHENEQ    2
020100970620     C*****              ROLBK
020200970620     C*
020300970620     C                   ENDSL
020400970620     C*
020500970620     C                   ENDIF
020600970620     C*
020700970620     C                   ENDIF
020800970620     C*
020900970620     C* se ho confermato l'uscita proseguo
021000970620     C     $FINE         IFEQ      *ON
021100970620     C*
021200170731     C* disallocazione rcd nel caso di modifica/annullam.
021300170731    >C     OPZI83        IFEQ      '02'
021400170731    >C     OPZI83        OREQ      '04'
021500170801    >C                   UNLOCK    SDGCSF02L                            22
021600970620     C                   ENDIF
021700970620     C*
021800970620     C* passaggio dati al pgm chiamante
021900170731    >C                   MOVEL     SDGSI83DS     KPJBU
022000971016     C*
022100971016     C                   SETON                                            RT
022200970620     C                   RETURN
022300970620     C*
022400970620     C                   ENDIF
022500970620     C*
022600970620     C                   ENDSR
022700970620     C/EJECT
022800970620     C************************************************************
022900970620     C* RICERCHE
023000970620     C************************************************************
023100970620     C     SEARCH        BEGSR
023200970620     C*
023300970620     C* determino Riga/Colonna del cursore
023400970620     C                   MOVE      NRG           R$$
023500970620     C                   MOVE      NCL           C$$
023600970620     C                   Z-ADD     RIRI          CURR
023700970620     C                   Z-ADD     COCO          CURC
023800970620     C*
023900970620     C* I campi che al momento dell'F4 risultano protetti non devono
024000970620     C* essere abilitati alla funzione di ricerca
024100970620     C*
024200970620     C                   SELECT
024300170801     C*
024400170801     C     H1NMFL        WHENEQ    'D1TRIM'
024500170801     C                   EXSR      F4_Trim
024600170801     C*
024700170801     C*  Natura soggetto
024800170801     C     H1NMFL        WHENEQ    'D1CF'
024900170801     C                   EXSR      F4_CF
025000970620     C*
025100970620     C* ricerca non abilitata in questa posizione
025200970620     C                   OTHER
025300970620     C                   SETON                                        9899
025400970620     C*
025500970620     C                   ENDSL
025600970620     C*
025700970620     C* imposto pos. del cursore
025800970620     C                   Z-ADD     CURR          H1RIGA
025900970620     C                   Z-ADD     CURC          H1COLO
026000970620     C*
026100970620     C                   MOVE      *HIVAL        $LASTV
026200970620     C*
026300970620     C                   ENDSR
026400970620     C/EJECT
026500170801     C************************************************************
026600170801     C* Ricerca trimestre di imposta
026700170801     C************************************************************
026800170801     C     F4_Trim       BEGSR
026900170801     C*
027000170801     C                   Reset                   ANA964DS
027100170801     C*
027200170801     C                   MoveL     '01'          Opz964
027300170801     C                   MoveL     'SDGCST000 '  Trc964
027400170801     C                   MoveL     'CSTTRIM   '  Cam964
027500170801     C                   MoveL     *On           All964
027600170801     C*
027700170801     C                   MoveL     ANA964DS      KPJBU
027800170801     C                   CALL      'ANA964R'
027900170801     C                   PARM                    KPJBA
028000170801     C                   MoveL     KPJBU         ANA964DS
028100170801     C*
028200170801     C                   if        Ret964     <> '2'
028300170801     C                   MoveL     Val964        D1Trim
028400170801     C                   endif
028500170801     C*
028600170801     C                   ENDSR
028700170801     C/EJECT
028800170801     C************************************************************
028900170801     C* Ricerca natura del conto
029000170801     C************************************************************
029100170801     C     F4_CF         BEGSR
029200170801     C*
029300170801     C                   Reset                   ANA964DS
029400170801     C*
029500170801     C                   MoveL     '01'          Opz964
029600170801     C                   MoveL     'ANDIZ000'    Trc964
029700170801     C                   MoveL     'DSPCLIFOR'   Cam964
029800170801     C                   MoveL     *Off          All964
029900170801     C*
030000170801     C                   MoveL     ANA964DS      KPJBU
030100170801     C                   CALL      'ANA964R'
030200170801     C                   PARM                    KPJBA
030300170801     C                   MoveL     KPJBU         ANA964DS
030400170801     C*
030500170801     C                   if        Ret964     <> '2'
030600170801     C                   MoveL     Val964        D1CF
030700170801     C                   endif
030800170801     C*
030900170801     C                   ENDSR
031000170801      /EJECT
031100970620     C************************************************************
031200970620     C* GESTIONE EMISSIONE TASTI FUNZIONE
031300970620     C************************************************************
031400970620     C     GESPIE        BEGSR
031500970620     C*
031600970620     C                   SELECT
031700970620     C*
031800970620     C     $GEST         WHENEQ    'D1'
031900970620     C     $ULKD1        IFEQ      0
032000970620     C* riempimento opzioni
032100970620     C                   EXSR      F24D1
032200970620     C                   ELSE
032300970620     C                   WRITE     FMTZ1
032400970620     C                   ENDIF
032500970620     C*
032600970620     C                   ENDSL
032700970620     C*
032800970620     C                   ENDSR
032900970620     C/EJECT
033000970620     C************************************************************
033100970620     C* Chiamata programma con tasto personalizzato
033200970620     C************************************************************
033300970620     C     FPerD1        BEGSR
033400970620     C*
033500970620     C* salvo la DS di ingresso
033600170731    >C                   MOVEL     SDGSI83DS     $SaveDS
033700970620     C*
033800170731    >C                   MOVE      xscsoc        SocietaI83
033900170731     C* passare nei campi appositi della DS SDGSI83DS il valore
034000970620     C* della chiave della VL univoca
034100170731    >C                   MOVEL     D1File        FileI83
034200970620     C*
034300170731    >C                   MOVEL     SDGSI83DS     KPJBU
034400970620    >C                   CALL      $PGF1(PosFun)                        22
034500970620     C                   PARM                    KPJBA
034600170731     C                   PARM                    NOMPGM           10
034700970620     C*
034800970620     C* ripristino la DS di ingresso
034900170731    >C                   MOVEL     $SaveDS       SDGSI83DS
035000970620     C*
035100970620     C                   ENDSR
035200970620     C/EJECT
035300970620     C************************************************************
035400970620     C* GESTIONE VIDEO RECORD D1
035500970620     C************************************************************
035600970620     C     GESD1         BEGSR
035700970620     C* inizializzazione videata
035800970620   B1C     $INZD1        IFEQ      *ON
035900970620     C                   EXSR      INZD1
036000970620     C                   MOVE      *OFF          $INZD1
036100970620   E1C                   ENDIF
036200970620     C* emissione piede videata se proveniente da altra
036300970620     C* salvataggio valore formato di provenienza
036400970620   B1C     $LASTV        IFNE      'D1'
036500970620     C                   WRITE     FMTT1
036600970620     C                   EXSR      GESPIE
036700970620     C                   MOVE      $LASTV        $LASTG
036800970620     C                   MOVE      'D1'          $LASTV
036900970620   E1C                   ENDIF
037000970620     C*
037100970620     C* se esistono errori sulla videata
037200970620     C* emetto la write del formato a indicatori spenti per vedere
037300970620     C* le eventuali decodifiche
037400970620   B1C     *IN99         IFEQ      *ON
037500970620     C     $GEST         ANDEQ     'D1'
037600970620     C                   MOVEA     *IN           WIN
037700970620     C                   MOVE      *ALL'0'       IN5098           49
037800970620     C                   MOVEA     IN5098        *IN(50)
037900970620     C                   WRITE     FMTD1
038000970620     C                   MOVEA     WIN           *IN
038100970620   E1C                   ENDIF
038200970620     C* Inizializzazione indice tasti funzione personalizzati
038300970620     C                   Eval      PosFun = 25
038400970620     C*
038500970620   B1C                   SELECT
038600970620     C* Annullamento
038700170731    >C     OPZI83        WHENEQ    '04'
038800970620     C* Visualizzazione
038900170731    >C     OPZI83        OREQ      '05'
039000970813     C* Cancellaz. file
039100170731    >C     OPZI83        OREQ      '29'
039200970620     C                   WRITE     FMTD1
039300970620     C                   EXFMT     PROTECT
039400970620     C* Immissione/Modifica/Copia
039500970620   X1C                   OTHER
039600970620     C                   EXFMT     FMTD1
039700970620   E1C                   ENDSL
039800970620     C*
039900970620     C* azzero la posizione del cursore
040000970620     C                   Z-ADD     0             H1RIGA
040100970620     C                   Z-ADD     0             H1COLO
040200970620     C*
040300970620     C* controllo abilitazione tasti funzione
040400970620     C                   EXSR      CTRKD1
040500970620     C*
040600970620   B1C     *IN99         IFEQ      *OFF
040700970620     C*
040800970620   B2C                   SELECT
040900970620     C* F2=Decodifica
041000970620     C     $TASTO        WHENEQ    F02
041100970620     C                   EXSR      F02D1
041200970620     C* F3=Fine
041300970620     C     $TASTO        WHENEQ    F03
041400970620     C                   EXSR      F03D1
041500970620     C* F4=Ricerca
041600970620     C     $TASTO        WHENEQ    F04
041700970620     C                   EXSR      SEARCH
041800970620     C* F5=Refresh
041900970620     C     $TASTO        WHENEQ    F05
042000970620     C                   EXSR      F05D1
042100970620     C* F8=Successivo
042200970620     C     $TASTO        WHENEQ    F08
042300970620     C                   EXSR      F08D1
042400970620     C* F12=Ritorno
042500970620     C     $TASTO        WHENEQ    F12
042600970620     C                   EXSR      F12D1
042700970620     C* F24=Altri tasti
042800970620     C     $TASTO        WHENEQ    F24
042900970620     C                   EXSR      F24D1
043000970620     C* Personalizzata
043100970620     C     $PGF1(PosFun) WHENNE    *BLANK
043200970620     C                   EXSR      FPerD1
043300970620     C*
043400970620   X2C                   OTHER
043500970620     C                   EXSR      CTRD1
043600970620     C*
043700970620   B3C     $TASTO        IFEQ      F06
043800970620     C     *IN99         ANDEQ     *OFF
043900970620     C* eseguo aggiornamento
044000970620     C                   EXSR      AGGANA
044100970620     C* eseguo operazioni del dopo-aggiornamento
044200970620   B4C     *IN99         IFEQ      *OFF
044300970620     C                   EXSR      GESAGG
044400970620   E4C                   ENDIF
044500970620   E3C                   ENDIF
044600970620     C*
044700970620   E2C                   ENDSL
044800970620     C*
044900970620   E1C                   ENDIF
045000970620     C*
045100970620     C                   ENDSR
045200970620     C/EJECT
045300970620     C************************************************************
045400970620     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
045500970620     C************************************************************
045600970620     C     CTRKD1        BEGSR
045700970620     C*
045800970620     C                   SETOFF                                       99
045900970620     C                   MOVEA     $FC1          $FC
046000970620     C                   EXSR      CTRKEY
046100970620     C     *IN99         IFEQ      *ON
046200970620     C                   SETON                                        97
046300970620     C                   ENDIF
046400970620     C*
046500970620     C                   ENDSR
046600970620     C/EJECT
046700970620     C************************************************************
046800970620     C* INIZIALIZZAZIONE VIDEATA DATI
046900970620     C************************************************************
047000970620     C     INZD1         BEGSR
047100970620     C*
047200970620     C                   CLEAR                   FMTD1
047300970620     C*
047400170731   B>C     OPZI83        IFNE      '01'
047500170731    >C                   MOVEL     SocietaI83    CSFSocieta
047600170731    >C                   MOVEL     FileI83       CSFFile
047700970620     C*
047800970620   B2C     *IN22         DOUEQ     *OFF
047900970620     C     MSGRTN        ORNE      'R'
048000970813     C* se Modifica/Annullamento/Cancellaz., il rcd che leggo viene bloccato
048100170731   B>C     OPZI83        IFEQ      '02'
048200170731    >C     OPZI83        OREQ      '04'
048300170731    >C     OPZI83        OREQ      '29'
048400170801    >C     K02Csf02      CHAIN     SDGCSF02L                          2122
048500970620     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
048600970620   X3C                   ELSE
048700170801    >C     K02Csf02      CHAIN(N)  SDGCSF02L                          2122
048800970620   E3C                   ENDIF
048900970620     C*
049000970620     C* Gestione RCD allocato
049100970620   B3C     *IN22         IFEQ      *ON
049200170731    >C                   MOVEL     'SDGCSF00F'   MSGFIL
049300970620     C                   EXSR      MSGPGM
049400970620   E3C                   ENDIF
049500970620   E2C                   ENDDO
049600970620     C*
049700970620     C* se il rcd da gestire non viene trovato
049800970620     C* oppure è allocato da altro pgm
049900970620     C* ritorno al pgm guida avvertendolo dell'errore
050000970620   B2C     *IN21         IFEQ      *ON
050100970620     C     *IN22         OREQ      *ON
050200970620     C                   MOVE      *ON           $FINE
050300170731    >C                   MOVE      '1'           ERRI83
050400970620     C*
050500970620     C* altrimenti
050600970620   X2C                   ELSE
050700970620     C* riempio la videata
050800970620     C                   EXSR      RIED1
050900970620     C* richiamo routine dei ctrl per decodificarla
051000970620     C                   EXSR      CTRD1
051100970620     C                   MOVE      *ALL'0'       IN5098
051200970620     C                   MOVEA     IN5098        *IN(50)
051300970620     C* definizione indicatori su video
051400970620     C                   SETOFF                                       02
051500970620   B3C                   SELECT
051600170731    >C     OPZI83        WHENEQ    '02'
051700970620     C                   SETON                                        02        PR campi key
051800170731    >C     OPZI83        WHENEQ    '05'
051900970620   E3C                   ENDSL
052000970620     C*
052100970620   E2C                   ENDIF
052200970620     C*
052300970620   X1C                   ELSE
052400970620     C*
052500970620     C* imposto i campi chiave ricevuti anche se si è in immissione
052600970620     C* perchè potrebbero essere stati passati dei default di chiave
052700170731    >C                   MOVEL     FileI83       D1File
052800970620   E1C                   ENDIF
052900970620     C*
053000970620     C                   ENDSR
053100970620     C/EJECT
053200970620     C************************************************************
053300970620     C* RIEMPIMENTO VIDEATA  D1
053400970620     C************************************************************
053500970620     C     RIED1         BEGSR
053600970620     C*
053700970620     C* impostare i campi della videata dai campi del file
053800970620     C*
053900170731     C                   eval      D1TpRec    = CsfTpRec
054000170731     C                   eval      D1Anno     = CsfAnno
054100170731     C                   eval      D1Trim     = CsfTrim
054200170801     C                   eval      D1Cf       = CsfCf
054300170731     C                   eval      D1File     = CsfFile
054400170810     C                   eval      D1FileRgp  = CsfFileRgp
054500170731     C                   eval      D1Note     = CsfNote
054600170731     C                   eval      D1IdFile   = CsfIdFile
054700170731     C*
054800170731     C                   if        CSFDtInvio <> *loval
054900170731     C     *JobRun       move      CSFDtInvio    D1DtInvio
055000170731     C                   endif
055100170731     C*
055200170801     C                   if        CSFDtXml  <> *loval
055300170801     C     *JobRun       move      CSFDtXml      D1DtXml
055400170731     C                   endif
055500170731     C*
055600170731     C                   exsr      RepProfilo
055700170801     C                   eval      D1UteXml   =   XcaPrf
055800970620     C*
055900970620     C                   ENDSR
056000970620     C/EJECT
056100170731     C************************************************************
056200170731     C* REPERIMENTO profilo
056300170731     C************************************************************
056400170731     C     RepProfilo    BEGSR
056500170731     C*
056600170731     C                   CLEAR                   AUTDS
056700170731     C*
056800170731     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
056900170731     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
057000170731     C                   Z-ADD     0             XCAGRP                         GRUPPO
057100170731     C                   MOVE      'CU'          XCATCK                         GRUPPO
057200170731     C                   clear                   XCAPRF                         PROFILO
057300170731     C                   clear                   XCAFNC                         FUNZIONE
057400170801     C                   movel     CsfUteXml     XCAcut                         FUNZIONE
057500170731     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
057600170731     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
057700170731     C*
057800170731     C                   CALLB     'XCHKAUT'
057900170731     C                   PARM                    AUTDS
058000170731     C*
058100170731    6C                   ENDSR
058200970620     C************************************************************
058300970620     C* GESTIONE F02 VIDEO D1
058400970620     C************************************************************
058500970620     C     F02D1         BEGSR
058600970620     C*
058700970620     C* per decodificare la videata richiamo la routine dei ctrl
058800970620     C* e poi disattivo gli eventuali messaggi di errore
058900970620     C                   EXSR      CTRD1
059000970620     C*
059100970620     C                   MOVE      *ALL'0'       IN5098
059200970620     C                   MOVEA     IN5098        *IN(50)
059300970620     C*
059400970620     C                   ENDSR
059500970620     C/EJECT
059600970620     C************************************************************
059700970620     C* GESTIONE F03 VIDEO D1
059800970620     C************************************************************
059900970620     C     F03D1         BEGSR
060000970620     C*
060100970620     C                   MOVE      *ON           $FINE
060200170731    >C                   MOVE      '1'           RETI83
060300970620     C* fine programma
060400970620     C                   EXSR      ENDPGM
060500970620     C*
060600970620     C                   ENDSR
060700970620     C/EJECT
060800970620     C************************************************************
060900970620     C* GESTIONE F05 VIDEO D1
061000970620     C************************************************************
061100970620     C     F05D1         BEGSR
061200970620     C*
061300970620     C                   MOVE      *ON           $INZD1
061400970620     C*
061500970620     C                   ENDSR
061600970620     C/EJECT
061700970620     C************************************************************
061800970620     C* GESTIONE F08 VIDEO D1
061900970620     C************************************************************
062000970620     C     F08D1         BEGSR
062100970620     C*
062200970620     C                   MOVE      *ON           $FINE
062300170731    >C                   MOVE      '0'           RETI83
062400970620     C*
062500970620     C                   ENDSR
062600970620     C/EJECT
062700970620     C************************************************************
062800970620     C* GESTIONE F12 VIDEO D1
062900970620     C************************************************************
063000970620     C     F12D1         BEGSR
063100970620     C*
063200970620     C                   MOVE      *ON           $FINE
063300170731    >C                   MOVE      '2'           RETI83
063400970620     C*
063500970620     C                   ENDSR
063600970620     C/EJECT
063700970620     C************************************************************
063800970620     C* GESTIONE F24 VIDEO D1
063900970620     C************************************************************
064000970620     C     F24D1         BEGSR
064100970620     C*
064200970620     C                   MOVEA     $FX1          $FX
064300970620     C                   MOVEA     $FC1          $FC
064400970620     C                   MOVEA     $FR1          $FR
064500970620     C                   Z-ADD     $ULKD1        $ULK
064600970620     C                   Z-ADD     $D1L01        $L01
064700970620     C                   Z-ADD     $D1L02        $L02
064800970620     C                   EXSR      RIEKEY
064900970620     C                   Z-ADD     $ULK          $ULKD1            3 0
065000970620     C                   MOVEA     $FC           $FC1
065100970620     C                   MOVEL     $KEY1         Z1KE1
065200170731     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
065300970620     C                   MOVEL     $KEY2         Z1KE2
065400970620     C                   WRITE     FMTZ1
065500970620     C*
065600970620     C                   ENDSR
065700970620     C/EJECT
065800970620     C************************************************************
065900970620     C* CONTROLLO VIDEATA
066000970620     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
066100970620     C* 95 96 97 98
066200970620     C************************************************************
066300970620     C     CTRD1         BEGSR
066400970620     C*
066500970620     C                   SETOFF                                       99
066600970620     C*
066700970620     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
066800970620     C*
066900170731    >C     OPZI83        IFNE      '04'
067000970623     C*
067100970623     C* File
067200970623    >C     D1File        IFEQ      *BLANK
067300970623    >C                   SETON                                        5199
067400970623    >C                   ENDIF
067500170810     C*
067600170810     C* File raggruppante
067700170810    >C     D1FileRgp     IFEQ      *BLANK
067800170810    >C                   SETON                                        5299
067900170810    >C                   ENDIF
068000170731     C*
068100170731     C                   EXSR      CtrTpRec
068200170731     C*
068300170731     C                   EXSR      CtrTrim
068400170801     C*
068500170801     C                   EXSR      CtrCF
068600170801     C*
068700170801     C                   EXSR      CtrData
068800970620     C*
068900170807     C                   if        D1DtInvio  = 0 and
069000170807     C                             D1IdFile  <> *blank
069100170807     C                   seton                                        5899
069200170807     C                   endif
069300170807     C*
069400170807     C                   if        D1DtInvio <> 0 and
069500170807     C                             D1IdFile  = *blank
069600170807     C                   seton                                        5999
069700170807     C                   endif
069800170807     C*
069900970620     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
070000170731    >C     OPZI83        IFEQ      '01'
070100170731    >C     OPZI83        OREQ      '03'
070200970620     C*
070300170731    >C                   MOVEL     SocietaI83    CSFSocieta
070400170731    >C                   MOVEL     D1File        CSFFile
070500170801    >C     K02CSF02      SETLL     SDGCSF02L                              21
070600970620     C     *IN21         IFEQ      '1'
070700970620     C                   SETON                                        9099
070800970620     C                   ENDIF
070900970620     C                   ENDIF
071000970620     C*
071100970620     C                   ENDIF
071200970620     C*
071300970620     C                   ENDSR
071400970620     C/EJECT
071500170731     C***********************************************************
071600170731     C* Controllo tipo record
071700170731     C***********************************************************
071800170731     C     CtrTpRec      BEGSR
071900170731     C*
072000170731     C                   Clear                   D1TpRecD
072100170731     C*
072200170731     C                   Clear                   X07ValDs
072300170731     C*
072400170731     C                   Move      '1'           X07Ric
072500170731     C                   Move      *On           X07All
072600170731     C                   Movel     'SDGCST000'   X07TRc
072700170731     C                   Movel     'CSTTPREC '   X07Cam
072800170731     C                   Movel     D1TpRec       X07VAL
072900170731     C*
073000170731     C                   Callb     'X07VALR'
073100170731     C                   Parm                    X07ValDs
073200170731     C*
073300170731     C     X07ERR        IFEQ      *Off
073400170731     C                   MOVEL     X07DES        D1TpRecD
073500170731     C                   else
073600170801     C                   seton                                        5599
073700170731     C                   endif
073800170731     C*
073900170731     C                   ENDSR
074000170731     C/EJECT
074100170731     C***********************************************************
074200170731     C* Controllo trimestre
074300170731     C***********************************************************
074400170731     C     CtrTrim       BEGSR
074500170731     C*
074600170731     C                   Clear                   X07ValDs
074700170731     C*
074800170731     C                   Move      '1'           X07Ric
074900170731     C                   Move      *On           X07All
075000170731     C                   Movel     'SDGCST000'   X07TRc
075100170731     C                   Movel     'CSTTRIM   '  X07Cam
075200170731     C                   Movel     D1Trim        X07VAL
075300170731     C*
075400170731     C                   Callb     'X07VALR'
075500170731     C                   Parm                    X07ValDs
075600170731     C*
075700170731     C     X07ERR        IFEQ      *Off
075800170731     C                   else
075900170801     C                   seton                                        5799
076000170731     C                   endif
076100170731     C*
076200170731     C                   ENDSR
076300170731     C/EJECT
076400170801     C***********************************************************
076500170801     C* Controllo tipo soggetto
076600170801     C***********************************************************
076700170801     C     CtrCF         BEGSR
076800170801     C*
076900170801     C                   Clear                   D1CFD
077000170801     C*
077100170801     C                   Clear                   X07ValDs
077200170801     C*
077300170801     C                   Move      '1'           X07Ric
077400170801     C                   Move      *Off          X07All
077500170801     C                   Movel     'ANDIZ000'    X07TRc
077600170801     C                   Movel     'DSPCLIFOR'   X07Cam
077700170801     C                   Movel     D1CF          X07VAL
077800170801     C*
077900170801     C                   Callb     'X07VALR'
078000170801     C                   Parm                    X07ValDs
078100170801     C*
078200170801     C     X07ERR        IFEQ      *Off
078300170801     C                   MOVEL     X07DES        D1CFD
078400170801     C                   else
078500170801     C                   SetOn                                        5499
078600170801     C                   endif
078700170801     C*
078800170801     C                   ENDSR
078900170801     C/EJECT
079000170801     C************************************************************
079100170801     C* Controllo data invio
079200170801     C************************************************************
079300170801     C     CtrData       BEGSR
079400170801     C*
079500170801     C                   Clear                   WDtInvio
079600170801     C*
079700170801     C                   if        D1DtInvio  <> 0
079800170801     C     *JobRun       Test(d)                 D1DtInvio              52
079900170801     C* data corretta
080000170801     C                   if        *In52       = *Off
080100170801     C     *JobRun       Move      D1DtInvio     WDtInvio
080200170801     C*
080300170801     C                   else
080400170801     C*
080500170801     C                   SetOn                                          99
080600170801     C                   endif
080700170801     C*
080800170801     C                   endif
080900170801     C*
081000170801     C                   ENDSR
081100170801     C/EJECT
081200970623     C************************************************************
081300970623     C* ESEGUE SCARICO SU SUPPORTO MAGNETICO
081400970623     C************************************************************
081500970623     C     EXESCA        BEGSR
081600970623     C*
081700971016     C*
081800970623     C                   ENDSR
081900970623     C/EJECT
082000970620     C************************************************************
082100970620     C* AGGIORNAMENTO ANAGRAFICA
082200970620     C************************************************************
082300970620     C     AGGANA        BEGSR
082400970620     C*
082500970620     C                   SELECT
082600170731    >C     OPZI83        WHENEQ    '01'
082700170731    >C     OPZI83        OREQ      '03'
082800970620     C                   EXSR      RIEANA
082900170731    >C                   WRITE     SDGCSF000                            22
083000970620     C* se rcd non scrivibile attivo errore
083100970620     C     *IN22         IFEQ      *ON
083200970620     C                   SETON                                        9599
083300970620     C                   ENDIF
083400170731    >C     OPZI83        WHENEQ    '02'
083500970620     C                   EXSR      RIEANA
083600170731    >C                   UPDATE    SDGCSF000
083700170731    >C     OPZI83        WHENEQ    '04'
083800170731    >C                   DELETE    SDGCSF000
083900970620     C                   ENDSL
084000970620     C*
084100970620     C* richiama pgm User Exit
084200970620     c*
084300970620     C     USRERR        IFEQ      *BLANK
084400970620     C* imposta chiave univoca  per user exit
084500970620     C                   MOVE      XSCSOC        XUKSOC
084600970620     C                   MOVE      XSCDUN        XUKDUN
084700970620     C                   MOVEL     DSPGM         XUKPGM
084800170731    >C                   MOVEL     CSFSocieta    XUKFL1
084900170731    >C                   MOVEL     CSFFile       XUKFL2
085000970620     C                   MOVEL     XUKDS         KPJBU
085100970620     C     DSPGM         CAT       'U':0         USREXT           10
085200970620     C                   CALL      USREXT                               21
085300970620     C                   PARM                    KPJBA
085400970620     C     *IN21         IFEQ      *ON
085500970620     C                   MOVE      *ON           USRERR            1
085600970620     C                   ENDIF
085700970620     C                   ENDIF
085800970620     C*
085900970620     C                   ENDSR
086000970620     C/EJECT
086100970620     C************************************************************
086200970620     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
086300970620     C************************************************************
086400970620     C     RIEANA        BEGSR
086500970620     C*
086600170801     C                   if        OpzI83      = '01'   or
086700170801     C                             OpzI83      = '03'
086800170801     C                   clear                   SdgCsf000
086900170801     C                   eval      CSFDtImm    = ZDateIso
087000170801     C                   eval      CSFUteImm   = XscCut
087100170801     C                   endif
087200970620     C*
087300170801     C                   eval      CsfOrigine  = 'M'
087400170801     C                   eval      CsfSocieta  = XscSoc
087500170801     C                   eval      CsfTpCom    = 'T'
087600170801     C                   eval      CsfTpRec    = D1TpRec
087700170801     C                   eval      CsfCf       = D1Cf
087800170801     C                   eval      CsfAnno     = D1Anno
087900170801     C                   eval      CsfTrim     = D1Trim
088000170801     C                   eval      CsfFile     = D1File
088100170810     C                   eval      CsfFileRgp  = D1FileRgp
088200170801     C                   eval      CsfNote     = D1Note
088300170801     C                   eval      CsfIdFile   = D1IdFile
088400170801     C                   eval      CsfDtInvio  = WDtInvio
088500170801     C*
088600170801     C                   eval      CSFDtUlMo   = ZDateIso
088700170801     C                   eval      CSFUteUlMo  = XscCut
088800970620     C*
088900970620     C                   ENDSR
089000970620     C/EJECT
089100970620     C************************************************************
089200970620     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
089300970620     C************************************************************
089400970620     C     GESAGG        BEGSR
089500970620     C*
089600970620     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
089700170731    >C     CMTI83        IFEQ      '0'
089800970707     C*****              COMMIT
089900970620     C                   ENDIF
090000970620     C*
090100970620     C* segnalo al pgm chiamante l'avvenuta conferma
090200170731    >C                   MOVE      *ON           OPRI83
090300970620     C* segnalo al pgm chiamante l'aver premuto l'F6
090400170731    >C                   MOVE      '0'           RETI83
090500970620     C*
090600970620     C                   SELECT
090700970620     C*
090800970620     C* nel caso di immissione
090900170731    >C     OPZI83        WHENEQ    '01'
091000970620     C* ripristino la videata iniziale
091100970620     C                   MOVE      *ON           $INZD1
091200970620     C                   MOVE      'D1'          $GEST
091300970620     C*
091400970620     C* nel caso di modifica
091500170731    >C     OPZI83        WHENEQ    '02'
091600970620     C* ripasso i campi che devono essere reimpostati sul sfl
091700170731    >C*****              MOVE      ___DES        DESI83
091800970620     C* e ritorno al pgm chiamante
091900970620     C                   MOVE      *ON           $FINE
092000970620     C*
092100970620     C* per tutte le altre opzioni
092200970620     C                   OTHER
092300970620     C* ritorno al pgm chiamante
092400970620     C                   MOVE      *ON           $FINE
092500970620     C                   ENDSL
092600970620     C*
092700970620     C                   ENDSR
092800970620     C/EJECT
092900970620     C************************************************************
093000970620     C* CONTROLLO ABILITAZIONE TASTI
093100970620     C************************************************************
093200970620     C     CTRKEY        BEGSR
093300970620     C*
093400970620     C                   SELECT
093500970620     C     $TASTO        WHENEQ    F01
093600970620     C                   z-add     01            posfun
093700970620     C     $FC(1)        IFEQ      '0'
093800970620     C                   SETON                                        99
093900970620     C                   ENDIF
094000970620     C     $TASTO        WHENEQ    F02
094100970620     C                   z-add     02            posfun
094200970620     C     $FC(2)        IFEQ      '0'
094300970620     C                   SETON                                        99
094400970620     C                   ENDIF
094500970620     C     $TASTO        WHENEQ    F03
094600970620     C                   z-add     03            posfun
094700970620     C     $FC(3)        IFEQ      '0'
094800970620     C                   SETON                                        99
094900970620     C                   ENDIF
095000970620     C     $TASTO        WHENEQ    F04
095100970620     C                   z-add     04            posfun
095200970620     C     $FC(4)        IFEQ      '0'
095300970620     C                   SETON                                        99
095400970620     C                   ENDIF
095500970620     C     $TASTO        WHENEQ    F05
095600970620     C                   z-add     05            posfun
095700970620     C     $FC(5)        IFEQ      '0'
095800970620     C                   SETON                                        99
095900970620     C                   ENDIF
096000970620     C     $TASTO        WHENEQ    F06
096100970620     C                   z-add     06            posfun
096200970620     C     $FC(6)        IFEQ      '0'
096300970620     C                   SETON                                        99
096400970620     C                   ENDIF
096500970620     C     $TASTO        WHENEQ    F07
096600970620     C                   z-add     07            posfun
096700970620     C     $FC(7)        IFEQ      '0'
096800970620     C                   SETON                                        99
096900970620     C                   ENDIF
097000970620     C     $TASTO        WHENEQ    F08
097100970620     C                   z-add     08            posfun
097200970620     C     $FC(8)        IFEQ      '0'
097300970620     C                   SETON                                        99
097400970620     C                   ENDIF
097500970620     C     $TASTO        WHENEQ    F09
097600970620     C                   z-add     09            posfun
097700970620     C     $FC(09)       IFEQ      '0'
097800970620     C                   SETON                                        99
097900970620     C                   ENDIF
098000970620     C     $TASTO        WHENEQ    F10
098100970620     C                   z-add     10            posfun
098200970620     C     $FC(10)       IFEQ      '0'
098300970620     C                   SETON                                        99
098400970620     C                   ENDIF
098500970620     C     $TASTO        WHENEQ    F11
098600970620     C                   z-add     11            posfun
098700970620     C     $FC(11)       IFEQ      '0'
098800970620     C                   SETON                                        99
098900970620     C                   ENDIF
099000970620     C     $TASTO        WHENEQ    F12
099100970620     C                   z-add     12            posfun
099200970620     C     $FC(12)       IFEQ      '0'
099300970620     C                   SETON                                        99
099400970620     C                   ENDIF
099500970620     C     $TASTO        WHENEQ    F13
099600970620     C                   z-add     13            posfun
099700970620     C     $FC(13)       IFEQ      '0'
099800970620     C                   SETON                                        99
099900970620     C                   ENDIF
100000970620     C     $TASTO        WHENEQ    F14
100100970620     C                   z-add     14            posfun
100200970620     C     $FC(14)       IFEQ      '0'
100300970620     C                   SETON                                        99
100400970620     C                   ENDIF
100500970620     C     $TASTO        WHENEQ    F15
100600970620     C                   z-add     15            posfun
100700970620     C     $FC(15)       IFEQ      '0'
100800970620     C                   SETON                                        99
100900970620     C                   ENDIF
101000970620     C     $TASTO        WHENEQ    F16
101100970620     C                   z-add     16            posfun
101200970620     C     $FC(16)       IFEQ      '0'
101300970620     C                   SETON                                        99
101400970620     C                   ENDIF
101500970620     C     $TASTO        WHENEQ    F17
101600970620     C                   z-add     17            posfun
101700970620     C     $FC(17)       IFEQ      '0'
101800970620     C                   SETON                                        99
101900970620     C                   ENDIF
102000970620     C     $TASTO        WHENEQ    F18
102100970620     C                   z-add     18            posfun
102200970620     C     $FC(18)       IFEQ      '0'
102300970620     C                   SETON                                        99
102400970620     C                   ENDIF
102500970620     C     $TASTO        WHENEQ    F19
102600970620     C                   z-add     19            posfun
102700970620     C     $FC(19)       IFEQ      '0'
102800970620     C                   SETON                                        99
102900970620     C                   ENDIF
103000970620     C     $TASTO        WHENEQ    F20
103100970620     C                   z-add     20            posfun
103200970620     C     $FC(20)       IFEQ      '0'
103300970620     C                   SETON                                        99
103400970620     C                   ENDIF
103500970620     C     $TASTO        WHENEQ    F21
103600970620     C                   z-add     21            posfun
103700970620     C     $FC(21)       IFEQ      '0'
103800970620     C                   SETON                                        99
103900970620     C                   ENDIF
104000970620     C     $TASTO        WHENEQ    F22
104100970620     C                   z-add     22            posfun
104200970620     C     $FC(22)       IFEQ      '0'
104300970620     C                   SETON                                        99
104400970620     C                   ENDIF
104500970620     C     $TASTO        WHENEQ    F23
104600970620     C                   z-add     23            posfun
104700970620     C     $FC(23)       IFEQ      '0'
104800970620     C                   SETON                                        99
104900970620     C                   ENDIF
105000970620     C     $TASTO        WHENEQ    F24
105100970620     C                   z-add     24            posfun
105200970620     C     $FC(24)       IFEQ      '0'
105300970620     C                   SETON                                        99
105400970620     C                   ENDIF
105500970620     C                   ENDSL
105600970620     C*
105700970620     C                   ENDSR
105800970620     C/EJECT
105900970620     C******************************************************
106000970620     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
106100970620     C******************************************************
106200970620     C     MSGPGM        BEGSR
106300970620     C*
106400970620     C                   Z-ADD     2             MSGMSG
106500970620     C                   MOVEL     'S'           MSGRSP
106600970620     C                   MOVEL     'CR'          MSGTPR
106700970620     C                   MOVEL     'S'           MSGEMV
106800970620     C                   Z-ADD     10            MSGRGP
106900970620     C                   MOVEL     'S'           MSGLCK
107000970620     C                   MOVEA     STATUS        STS
107100970620     C                   MOVEL     'N'           MSGCNL
107200970620     C                   MOVEL     'N'           MSGVID
107300970620     C                   MOVEL     'N'           MSGOPE
107400970620     C                   MOVEL     'N'           MSGSTP
107500970620     C                   CALL      'XMSGR'
107600970620     C                   PARM                    XMSGDS
107700970620     C*
107800970620     C                   ENDSR
107900970620     C/EJECT
108000970620     C************************************************************
108100970620     C* OPERAZIONI INIZIALI
108200970620     C************************************************************
108300970620     C     *INZSR        BEGSR
108400170731     C     *DMY          Move      Udate         ZDateISO
108500170731     C     *JobRun       Move      ZDateISO      ZDate             6 0
108600970620     C*
108700970620     C* Reperimento parametri
108800970620     C*
108900970620     C     *ENTRY        PLIST
109000970620     C                   PARM                    KPJBA
109100970620     C                   PARM                    DSAUT
109200970620     C*
109300970620     C* Reperimento dati società
109400970620     C*
109500970620    >C                   MOVEL     'SOC001'      TIPXSC
109600970620     C                   MOVEL     *BLANK        SOCXSC
109700970620     C                   EXSR      REPSOC
109800970620     C*
109900970620     C* Controlli dati società
110000970620     C*
110100970620     C     RTNXSC        IFNE      '1'
110200970620     C                   MOVEL     XSOCDS        SOC001
110300970620     C                   ELSE
110400170731    >C                   MOVE      '1'           ErrI83
110500970620     C                   MOVE      *on           $Fine
110600970620     C                   EXSR      EndPgm
110700970620     C                   ENDIF
110800970620     C*
110900970620     C* testo se sono stati passati i dati di autorizzazione
111000970620     C*
111100970620     C     PARMS         IFLT      2
111200970620     C                   MOVE      *BLANK        AUTDS
111300970620     C                   ELSE
111400970620     C                   MOVEL     DSAUT         AUTDS
111500970620     C                   ENDIF
111600970620     C*
111700970620     C* reperisco le dimensioni delle variabili che conterranno i tasti
111800970620     C                   EVAL      $D1L01 = %Size(Z1Ke1)
111900970620     C                   EVAL      $D1L02 = %Size(Z1Ke2)
112000170731     C                   EVAL      $D2L01 = 0
112100970623     C                   EVAL      $D2L02 = 0
112200970620     C*
112300970620     C* Reperimento tasti di funzione
112400970620     C*
112500970620     C                   EXSR      REPKEY
112600970620     C* Richiamo routine per aggiunta tasti funzione personalizzati
112700970620     C                   CAllb     'X46USREXT'
112800970620     C                   PARM                    *omit
112900970620     C                   PARM                    *omit
113000970620     C                   PARM                    *omit
113100970620     C                   PARM                    *omit
113200970620     C                   PARM                    *omit
113300970620     C                   PARM                    $FX1
113400970620     C                   PARM                    $FC1
113500970620     C                   PARM                    $PGF1
113600970620     C                   PARM                    DsPgm
113700970620     C                   PARM                    xsccut
113800970620     C                   PARM                    xscgrp
113900970620     C*
114000970620     C* Attiva Commitment control se necessario
114100970620     C*
114200970620    >C*****              CALLB     'XSTRCMT'
114300970620    >C*****              OPEN      AN___01L
114400970620     C*
114500970620     C                   ENDSR
114600970620     C/EJECT
114700970620     C************************************************************
114800970620     C* REPERIMENTO TASTI DI FUNZIONE
114900970620     C************************************************************
115000970620     C     REPKEY        BEGSR
115100970701     C*
115200970620     C                   MOVEA     $FM1          $FM
115300970701     C                   MOVEA     $FC1          $FC
115400970620     C                   EXSR      REPKKK
115500970620     C                   MOVEA     $FX           $FX1
115600970620     C*
115700970620     C                   ENDSR
115800970620     C/EJECT
115900970620     C************************************************************
116000970620     C* REPERIMENTO TASTI DI FUNZIONE
116100970620     C************************************************************
116200970620     C     REPKKK        BEGSR
116300970620     C*
116400970620     C                   CLEAR                   $FX
116500970620     C     1             DO        24            X
116600970620     C*
116700970620     C     $FM(X)        IFNE      *BLANK
116800970620     C     $Fc(X)        andne     '0'
116900970620     C*
117000970620     C                   MOVEL     $FM(X)        IDMSG
117100970620     C                   CALLB     'XRTVM'
117200970620     C                   PARM                    IDMSG            27
117300970620     C                   PARM                    TXTMSG          100
117400970620     C                   PARM                    ERRMSG            1
117500970620     C*
117600970620     C     ERRMSG        IFNE      *ON
117700970620     C                   MOVEL     TXTMSG        $FX(X)
117800970620     C                   ELSE
117900970620     C                   MOVEL     *ALL' '       $FX(X)
118000970620     C                   ENDIF
118100970620     C*
118200970620     C                   ENDIF
118300970620     C*
118400970620     C                   ENDDO
118500970620     C*
118600970620     C                   ENDSR
118700970620     C/EJECT
118800970620     C************************************************************
118900970620     C* INIZIALIZZAZIONE VARIABILI
119000970620     C************************************************************
119100970620     C     INZVAR        BEGSR
119200970620     C*
119300170731    >C                   MOVEL     KPJBU         SDGSI83DS
119400970620     C*
119500970620     C* Se società diversa da quella impostata nella *inzsr, rieseguo
119600970620     C* reperimento dati società
119700170731    >C                   If        SocietaI83 <> xscsoc    and
119800170731    >C                             OpzI83 <> *blank
119900970620     C                   MOVEL     'SOC001'      TIPXSC
120000170731    >C                   MOVEL     SocietaI83    SOCXSC
120100970620     C                   EXSR      REPSOC
120200970620     C     RTNXSC        IFNE      '1'
120300970620     C                   MOVEL     XSOCDS        SOC001
120400970620     C                   ELSE
120500170731    >C                   MOVE      '1'           ErrI83
120600970620     C                   MOVE      *on           $Fine
120700970620     C                   EXSR      EndPgm
120800970620     C                   ENDIF
120900970620     C                   endif
121000970829     C*
121100970829     C* Nel caso che, tra la precedente chiamata a questo pgm e questa,
121200970829     C* ci sia stata una emissione a video del pgm chiamante
121300970829     C* chiudo e riapro il DSPF contenente la finestra
121400970829     C* per evitare il permanere del primo sfondo sotto la finestra.
121500970829     C* Nel caso contrario non eseguo la chiusura cosicchè rimane
121600970829     C* l'ultimo sfondo visualizzato
121700170731     C     ExfI83        IFEQ      '1'
121800170731     C                   CLOSE     SDGSI83D
121900170731     C                   OPEN      SDGSI83D
122000970829     C                   ENDIF
122100970620     C*
122200970620     C* Reperimento autorizzazioni
122300970620     C*
122400970620    >C     AUTDS         IFEQ      *BLANK
122500981026    >C                   EXSR      REPAUT
122600970620    >C                   ELSE
122700981026    >C                   MOVEL     XCADDS        ANGCFIDS
122800970620    >C                   ENDIF
122900970620     C*
123000970620     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
123100970620     C* le autorizzazione
123200970620     C*
123300981026    >C                   EXSR      TSTAUT
123400970620     C*
123500970620     C* Definizione dei tasti funzione utilizzabili a pgm
123600970620     C*
123700970620     C                   EXSR      DEFOPZ
123800970620     C*
123900970620     C* Variabili per gestione videate
124000970620     C*
124100970620     C                   MOVE      *OFF          $FINE                          fine pgm
124200970623     C                   MOVE      *OFF          $INZD1                         inizializzione D1
124300970623     C                   MOVE      *OFF          $INZD2                         inizializzione D2
124400970620     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
124500970620     C                   MOVE      *BLANK        $LASTV                         ultima emissione
124600970620     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
124700970623     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
124800970620     C* azzero la posizione del cursore
124900970620     C                   Z-ADD     0             H1RIGA
125000970620     C                   Z-ADD     0             H1COLO
125100970620     C*
125200970620     C* Variabili appoggio
125300970620     C*
125400970620     C                   Z-ADD     0             CURR
125500970620     C                   Z-ADD     0             CURC
125600970620     C*
125700970620     C* Valorizzazione campi univoci testate
125800970620     C*
125900970620     C                   CLEAR                   FMTT1
126000970620     C                   MOVEL     KNSIF         NOMSIF
126100970620     C                   MOVEL     XSCDSI        NOMDIT
126200970620     C                   MOVEL     DSPGM         NOMPGM
126300170731     C                   MOVEL     KCDAZ         NOMAZN
126400970620     C* recupero il sottotitolo (se c'è)
126500970620     C                   MOVEL     *BLANK        T1TIT
126600970620     C     SOTTIT        IFNE      *BLANK
126700970620     C                   MOVEL     SOTTIT        IDMSG
126800970620     C                   CALLB     'XRTVM'
126900970620     C                   PARM                    IDMSG            27
127000970620     C                   PARM                    TXTMSG          100
127100970620     C                   PARM                    ERRMSG            1
127200970620     C                   PARM      *ON           CTRMSG            1            centratura
127300970620     C                   PARM      30            LENMSG            3 0          lun output.
127400970620     C     ERRMSG        IFNE      *ON
127500970620     C                   MOVEL     TXTMSG        T1TIT
127600970620     C                   ELSE
127700970620     C                   MOVEL     *ALL'?'       T1TIT
127800970620     C                   ENDIF
127900970620     C                   ENDIF
128000970620     C*
128100970620     C* Inizializzazione/Decodifica prima videata
128200970620     C*
128300970623     C                   MOVE      'D1'          $GEST                          fmt in gestione
128400970620     C                   EXSR      INZD1
128500970620     C                   MOVE      *OFF          $INZD1
128600970620     C*
128700970620     C                   ENDSR
128800970620     C/EJECT
128900970620     C************************************************************
129000970620     C* REPERIMENTO DATI SOCIETA'
129100970620     C************************************************************
129200970620     C     REPSOC        BEGSR
129300970620     C*
129400170731     C                   CALL      'SDGSOCDR'
129500970620     C                   PARM                    TIPXSC            6
129600970620     C                   PARM                    SOCXSC            3
129700970620     C                   PARM                    CDSXSC            9 0
129800970620     C                   PARM                    MODXSC            3
129900970620     C                   PARM      *BLANKS       RTNXSC            1
130000970620     C                   PARM                    XSOCDS
130100970620     C                   PARM                    KPJBA
130200970620     C*
130300970620     C                   ENDSR
130400970620     C/EJECT
130500970620     C************************************************************
130600970620     C* REPERIMENTO AUTORIZZAZIONI UTENTE
130700970620     C************************************************************
130800970620     C     REPAUT        BEGSR
130900970620     C*
131000970620     C                   CLEAR                   AUTDS
131100970620     C*
131200970620     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
131300970620     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
131400970620     C                   Z-ADD     0             XCAGRP                         GRUPPO
131500970620     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
131600981026    >C                   MOVEL     'ANGCFI'      XCAFNC                         FUNZIONE
131700970620    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
131800970620    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
131900970620    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
132000970620     C*
132100970620     C                   CALLB     'XCHKAUT'
132200970620     C                   PARM                    AUTDS
132300970620     C*
132400970620     C* errori/non abilitazione
132500970620     C     XCARTN        IFNE      0
132600170731     C                   MOVE      '2'           ErrI83
132700970620     C                   EXSR      ENDPGM
132800970620     C                   ELSE
132900981026    >C                   MOVEL     XCADDS        ANGCFIDS
133000970620     C                   ENDIF
133100970620     C*
133200970620     C                   ENDSR
133300970620     C/EJECT
133400981026     C************************************************************
133500981026     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
133600981026     C************************************************************
133700981026     C     TSTAUT        BEGSR
133800981026     C*
133900981026     C                   SELECT
134000981026     C*
134100981026     C* se richiamato in visualizzazione
134200170731    >C                   WHEN      OPZI83 = '05' and
134300981026     C* ma l'utente non è abilitato
134400981028    >C                             VISSSM = *OFF
134500981026     C* esco da pgm avvertendolo
134600170731    >C                   MOVE      '2'           ERRI83
134700981026     C                   MOVE      *ON           $FINE
134800981026     C                   EXSR      ENDPGM
134900981026     C*
135000981026     C                   ENDSL
135100981026     C*
135200981026     C                   ENDSR
135300981026     C/EJECT
135400970620     C************************************************************
135500970620     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
135600970620     C************************************************************
135700970620     C     DEFOPZ        BEGSR
135800970620     C*
135900970620     C* Questa routine deve valorizzare la schiera $FC1/$FR1
136000970620     C* con S o N a seconda della utilizzabilità o meno
136100970620     C* del tasto funzione correlato
136200970620     C*
136300970620     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
136400970620     C* di compilazione
136500970620     C                   RESET                   $FC1
136600970620     C                   RESET                   $FR1
136700970620     C*
136800970620     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
136900970620     C* invalido alcune opzioni
137000970620     C                   SELECT
137100970620     C* Se pgm richiamato in interrogazione disattivo :
137200170731    >C     OPZI83        WHENEQ    '05'
137300970620     C* F4=Ricerca
137400970620     C                   Z-ADD     4             X
137500970620     C                   MOVE      '0'           $FC1(X)
137600970620     C* F5=Ripristino
137700970620     C                   Z-ADD     5             X
137800970620     C                   MOVE      '0'           $FC1(X)
137900970620     C* F6=Conferma
138000970620     C                   Z-ADD     06            X
138100970620     C                   MOVE      '0'           $FC1(X)
138200170731     C* Se pgm richiamato in annullamento disattivo :
138300170731    >C     OPZI83        WHENEQ    '04'
138400970620     C* F4=Ricerca
138500970620     C                   Z-ADD     4             X
138600970620     C                   MOVE      '0'           $FC1(X)
138700970620     C* F5=Ripristino
138800970620     C                   Z-ADD     5             X
138900970620     C                   MOVE      '0'           $FC1(X)
139000970620     C*
139100970620     C                   OTHER
139200970620     C*
139300970620     C                   ENDSL
139400970620     C*
139500970620     C                   ENDSR
139600970620     C/EJECT
139700970620     C************************************************************
139800970620     C* REPERIMENTO CAMPI DI DEFAULT
139900970620     C************************************************************
140000970620     C     REPDEF        BEGSR
140100970620     C*
140200970620     C*
140300970620     C                   ENDSR
140400970620     C************************************************************
140500970620     C* DEFINIZIONE KLIST
140600970620     C************************************************************
140700970620     C     DefKlist      BEGSR
140800970620     C*
140900970620     C* klist
141000970620     C*
141100170801    >C* SDGCSF02L
141200170801    >C     K02Csf02      KLIST
141300170731    >C                   KFLD                    CSFSocieta
141400170731    >C                   KFLD                    CSFFile
141500970620     C*
141600970620     C                   ENDSR
141700970620     C/EJECT
141800970620     C************************************************************
141900970620     C* RIEMPIMENTO TASTI FUNZIONE
142000970620     C************************************************************
142100970620     C     RIEKEY        BEGSR
142200970620     C*
142300970620     C* Input :
142400970620     C* - $FX = schiera con tutti i testi disponibili
142500970620     C* - $FC = schiere con i flag di validità associati ai testi
142600970620     C* - $FR = schiere con i flag di REVERSE IMAGE
142700970620     C* - $ULK = ultimo testo caricato
142800970620     C* - $L01 = Lunghezza prima riga
142900970620     C* - $L02 = Lunghezza seconda riga
143000970620     C* Output :
143100970620     C* - $ULK
143200970620     C* - $KEY1= 1° riga di tasti funzione
143300970620     C* - $KEY2= 2° riga di tasti funzione
143400970620     C*
143500970620     C                   CALLB     'XKEYMSG'
143600970620     C* input
143700970620     C                   PARM                    $FX
143800970620     C                   PARM                    $FC
143900970620     C                   PARM                    $FR
144000970620     C                   PARM                    $ULK              3 0
144100970620     C* output
144200970620     C                   PARM                    $KEY1            79
144300970620     C                   PARM                    $KEY2            79
144400970620     C* input
144500970620     C                   PARM                    $L01              3 0
144600970620     C                   PARM                    $L02              3 0
144700970620     C*
144800170731     C                   PARM                    $ALLFUNCT       480
144900170731     C*
145000970620     C                   ENDSR
145100970620**
14520097062001PROMSG    *LIBL     COS0001
14530097062002PROMSG    *LIBL     COS0002
14540097062003PROMSG    *LIBL     COS0003
14550097062004PROMSG    *LIBL     COS0004
14560097062005PROMSG    *LIBL     COS0005
145700970620** TASTI DI FUNZIONE  D1
145800970620PROMSG    *LIBL     KEY0001  01
145900970620PROMSG    *LIBL     KEY0002  02
146000970620PROMSG    *LIBL     KEY0003  03
146100970620PROMSG    *LIBL     KEY0004  04
146200970620PROMSG    *LIBL     KEY0005  05
146300970620PROMSG    *LIBL     KEY0006  06
146400970620PROMSG    *LIBL     KEY0007  07
146500970620PROMSG    *LIBL     KEY0008  08
146600970620PROMSG    *LIBL     KEY0009  09
146700970620PROMSG    *LIBL     KEY0010  10
146800970620PROMSG    *LIBL     KEY0011  11
146900970620PROMSG    *LIBL     KEY0012  12
147000970620PROMSG    *LIBL     KEY0013  13
147100970620PROMSG    *LIBL     KEY0014  14
147200970620PROMSG    *LIBL     KEY0015  15
147300970620PROMSG    *LIBL     KEY0016  16
147400970620PROMSG    *LIBL     KEY0017  17
147500970620PROMSG    *LIBL     KEY0018  18
147600970620PROMSG    *LIBL     KEY0019  19
147700970620PROMSG    *LIBL     KEY0020  20
147800970620PROMSG    *LIBL     KEY0021  21
147900970620PROMSG    *LIBL     KEY0022  22
148000970620PROMSG    *LIBL     KEY0023  23
148100970620PROMSG    *LIBL     KEY0024  24
148200970620**  ABILITAZIONE D1
1483009706200N  01
1484009706200N  02
1485009706201N  03
1486001708011N  04
1487009706201N  05
1488009706201N  06
1489009706200N  07
1490009706201N  08
1491009706200N  09
1492009706200N  10
1493009706200N  11
1494009706201N  12
1495009706200N  13
1496009706200N  14
1497009706200N  15
1498009706200N  16
1499009706200N  17
1500009706200N  18
1501009706200N  19
1502009706200N  20
1503009706200N  21
1504009706200N  22
1505009706200N  23   Impostare a '1' se si utilizzano opzioni
1506009706201N  24   Lasciare sempre impostata a '1' il valore della riga 24
